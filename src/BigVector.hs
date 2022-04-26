{-# LANGUAGE DeriveGeneric #-}

module BigVector (
    BigVector,
    vlist,
    vseq,
    vsize,
    vlength,
    vEqualSize,
    vIsNull,
    vgetInt,
    vget,
    vplus,
    vminus,
    smultv,
    vmults,
    vscale,
    vdot,
    vcross,
    vdivs,
    vneg,
    vabs,
    vnorm,
    vdist,
    vangle,
    asList,
    asSeq
) where
    import qualified GHC.Generics as G
    import qualified Text.Printf as TP
    import qualified Data.Maybe as M
    import qualified Data.Sequence as S
    import qualified Data.Foldable as F
    import qualified Data.Hashable as H
    import qualified Data.HashSet as HS
    import qualified MathInfo as MI
    import qualified Debug as DS
    import qualified BigScalar as BS
    
    newtype BigVector = BigVector {
        _pos :: S.Seq BS.BigScalar
    } deriving (G.Generic, Eq)

    instance H.Hashable BigVector where
        hashWithSalt salt (BigVector pos) = H.hashWithSalt salt (F.toList pos)

    instance Show BigVector where
        show vec = TP.printf "<%s>" str
            where str = _str vec show

    instance DS.DebugString BigVector where
        stringify vec = TP.printf "Vector[%s]" str
            where str = _str vec DS.stringify

    _str :: BigVector -> (BS.BigScalar -> String) -> String
    _str (BigVector pos) converter = F.foldl' (++) "" commaSeparated
        where stringList = fmap converter pos
              commaSeparated = S.intersperse "," stringList

    vlist :: [BS.BigScalar] -> MI.Result BigVector
    vlist list = vseq $ S.fromList list

    vseq :: S.Seq BS.BigScalar -> MI.Result BigVector
    vseq vals
        | F.any BS.isExactQuaternion vals = MI.withError MI.InvalidValue
        | otherwise = MI.withValue $ BigVector vals

    vsize :: BigVector -> BS.BigScalar
    vsize = BS.integral . vlength

    vlength :: BigVector -> Int
    vlength (BigVector pos) = S.length pos

    vEqualSize :: BigVector -> BigVector -> Bool
    vEqualSize left right = (vlength left) == (vlength right)

    vIsNull :: BigVector -> Bool
    vIsNull (BigVector pos) = F.all (==BS.zero) pos

    vgetInt :: BigVector -> Int -> MI.Result BS.BigScalar
    vgetInt vec@(BigVector pos) index
        | index < 0 || index >= size = MI.withError MI.InvalidValue
        | otherwise = MI.withValue $ S.index pos index
        where size = vlength vec

    vget :: BigVector -> BS.BigScalar -> MI.Result BS.BigScalar
    vget vec index
        | not $ BS.isExactInteger index = MI.withError MI.InvalidValue
        | otherwise = vgetInt vec intIndex
        where intIndex = BS.asBuiltInInt index

    vplus :: BigVector -> BigVector -> MI.Result BigVector
    vplus = _binaryOperation BS.splus

    vminus :: BigVector -> BigVector -> MI.Result BigVector
    vminus = _binaryOperation BS.sminus

    _binaryOperation :: BS.BinaryScalarAction -> BigVector -> BigVector -> MI.Result BigVector
    _binaryOperation operation left@(BigVector leftPos) right@(BigVector rightPos)
        | not $ vEqualSize left right = MI.withError MI.InvalidValue
        | otherwise = (MI.withValue . BigVector) $ S.zipWith operation leftPos rightPos

    smultv :: BS.BigScalar -> BigVector -> MI.Result BigVector
    smultv left right@(BigVector rightPos)
        | BS.isExactQuaternion left = MI.withError MI.InvalidValue
        | otherwise = (MI.withValue . BigVector) $ fmap (BS.smult left) rightPos

    vmults :: BigVector -> BS.BigScalar -> MI.Result BigVector
    vmults = flip smultv

    vscale :: BigVector -> BigVector -> MI.Result BigVector
    vscale = _binaryOperation BS.smult

    vdot :: BigVector -> BigVector -> MI.Result BS.BigScalar
    vdot left@(BigVector leftPos) right@(BigVector rightPos)
        | not $ vEqualSize left right = MI.withError MI.InvalidValue
        | otherwise = (MI.withValue . _sum) $ S.zipWith BS.smult leftPos rightPos

    _sum :: S.Seq BS.BigScalar -> BS.BigScalar
    _sum = F.foldr BS.splus BS.zero

    vcross :: BigVector -> BigVector -> MI.Result BigVector
    vcross left right
        | 3 == leftSize && leftSize == rightSize = vlist [resultXPos, resultYPos, resultZPos]
        | otherwise = MI.withError MI.InvalidValue
        where leftSize = vlength left
              rightSize = vlength right
              leftXPos = MI.value $ vgetInt left 0
              leftYPos = MI.value $ vgetInt left 1
              leftZPos = MI.value $ vgetInt left 2
              rightXPos = MI.value $ vgetInt right 0
              rightYPos = MI.value $ vgetInt right 1
              rightZPos = MI.value $ vgetInt right 2
              resultXPos = BS.sminus (BS.smult leftYPos rightZPos) (BS.smult leftZPos rightYPos)
              resultYPos = BS.sminus (BS.smult leftZPos rightXPos) (BS.smult leftXPos rightZPos)
              resultZPos = BS.sminus (BS.smult leftXPos rightYPos) (BS.smult leftYPos rightXPos)

    vdivs :: BigVector -> BS.BigScalar -> MI.Result BigVector
    vdivs left right
        | BS.zero == right = MI.withError MI.InvalidValue
        | otherwise = vmults left rightInv
        where rightInv = MI.value $ BS.sinv right

    vneg :: BigVector -> BigVector
    vneg = MI.value . (smultv BS.negOne)

    vabs :: BigVector -> BS.BigScalar
    vabs vec = (BS.ssqrt . MI.value) $ vdot vec vec

    vnorm :: BigVector -> MI.Result BigVector
    vnorm vec
        | vIsNull vec = MI.withError MI.InvalidValue
        | otherwise = vdivs vec (vabs vec)

    vdist :: BigVector -> BigVector -> MI.Result BS.BigScalar
    vdist left right
        | MI.isFailure subtResult = MI.convert subtResult
        | otherwise = (MI.withValue . BS.ssqrt . _sum) $ fmap square (_pos subtValue)
        where subtResult = vminus left right
              subtValue = MI.value subtResult
              square = MI.value . ((flip BS.spow) BS.two)

    vangle :: BigVector -> BigVector -> MI.Result BS.BigScalar
    vangle left right
        | (not $ vEqualSize left right) || (vIsNull left) || (vIsNull right) = MI.withError MI.InvalidValue
        | otherwise = (BS.sacos . MI.value) $ BS.sdiv dotProd absProd
        where dotProd = MI.value $ vdot left right
              absProd = BS.smult (vabs left) (vabs right)

    asList :: BigVector -> [BS.BigScalar]
    asList (BigVector pos) = F.toList pos
    
    asSeq :: BigVector -> S.Seq BS.BigScalar
    asSeq (BigVector pos) = pos

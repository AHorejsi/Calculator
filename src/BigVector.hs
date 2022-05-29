{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Move brackets to avoid $" #-}

module BigVector (
    BigVector,
    vlist,
    vseq,
    vIsNull,
    vdot,
    vcross,
    vdivs,
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
    import qualified CalcSettings as CS
    import qualified Actions as A
    import qualified Stringify as Str
    import qualified MathInfo as MI
    import qualified BigScalar as BS
    
    newtype BigVector = BigVector {
        _pos :: S.Seq BS.BigScalar
    } deriving (G.Generic, Eq)

    instance H.Hashable BigVector where
        hashWithSalt salt (BigVector pos) = H.hashWithSalt salt (F.toList pos)

    instance Show BigVector where
        show vec = _str "Vector[%s]" vec show

    instance Str.Stringifier BigVector where
        stringify sets vec = _str "<%s>" vec (Str.stringify sets)

    _str :: String -> BigVector -> MI.UnaryAction BS.BigScalar String -> String
    _str format vec converter = TP.printf format strVal
        where strVal = _strHelper vec converter

    _strHelper :: BigVector -> MI.UnaryAction BS.BigScalar String -> String
    _strHelper (BigVector pos) converter = concat commaSeparated
        where stringList = fmap converter pos
              commaSeparated = S.intersperse "," stringList

    vlist :: [BS.BigScalar] -> MI.ComputationResult BigVector
    vlist list = vseq $ S.fromList list

    vseq :: S.Seq BS.BigScalar -> MI.ComputationResult BigVector
    vseq vals
        | F.any BS.isExactQuaternion vals = MI.withError MI.InvalidValue
        | otherwise = MI.withValue $ BigVector vals

    instance BS.Sized BigVector where
        intSize (BigVector pos) = S.length pos
        getInt vec@(BigVector pos) index
            | index < 0 || index >= size = MI.withError MI.InvalidValue
            | otherwise = MI.withValue $ S.index pos index
            where size = BS.intSize vec

    vIsNull :: BigVector -> Bool
    vIsNull (BigVector pos) = F.all (==BS.zero) pos

    instance A.Addable BigVector where
        plus = _binaryAction A.unsafePlus

    instance A.Subtractable BigVector where
        minus = _binaryAction A.unsafeMinus

    instance BS.ScalarMultipliable BigVector where
        rightScalarMult left right@(BigVector rightPos)
            | BS.isExactQuaternion left = MI.withError MI.InvalidValue
            | otherwise = (MI.withValue . BigVector) $ fmap (A.unsafeMult left) rightPos
        leftScalarMult = flip BS.rightScalarMult

    instance A.Scalable BigVector where
        scale = _binaryAction A.unsafeMult

    _binaryAction :: BS.BinaryScalarAction -> BigVector -> BigVector -> MI.ComputationResult BigVector
    _binaryAction action left@(BigVector leftPos) right@(BigVector rightPos)
        | not $ BS.equalSize left right = MI.withError MI.InvalidValue
        | otherwise = (MI.withValue . BigVector) $ S.zipWith action leftPos rightPos

    vdot :: BigVector -> BigVector -> MI.ComputationResult BS.BigScalar
    vdot left@(BigVector leftPos) right@(BigVector rightPos)
        | not $ BS.equalSize left right = MI.withError MI.InvalidValue
        | otherwise = (MI.withValue . _sum) $ S.zipWith A.unsafeMult leftPos rightPos

    _sum :: S.Seq BS.BigScalar -> BS.BigScalar
    _sum = F.foldr A.unsafePlus BS.zero

    vcross :: BigVector -> BigVector -> MI.ComputationResult BigVector
    vcross left right
        | 3 == leftSize && leftSize == rightSize = vlist [resultXPos, resultYPos, resultZPos]
        | otherwise = MI.withError MI.InvalidValue
        where leftSize = BS.intSize left
              rightSize = BS.intSize right
              leftXPos = MI.value $ BS.getInt left 0
              leftYPos = MI.value $ BS.getInt left 1
              leftZPos = MI.value $ BS.getInt left 2
              rightXPos = MI.value $ BS.getInt right 0
              rightYPos = MI.value $ BS.getInt right 1
              rightZPos = MI.value $ BS.getInt right 2
              resultXPos = A.unsafeMinus (A.unsafeMult leftYPos rightZPos) (A.unsafeMult leftZPos rightYPos)
              resultYPos = A.unsafeMinus (A.unsafeMult leftZPos rightXPos) (A.unsafeMult leftXPos rightZPos)
              resultZPos = A.unsafeMinus (A.unsafeMult leftXPos rightYPos) (A.unsafeMult leftYPos rightXPos)

    vdivs :: BigVector -> BS.BigScalar -> MI.ComputationResult BigVector
    vdivs left right
        | BS.zero == right = MI.withError MI.InvalidValue
        | otherwise = BS.leftScalarMult left rightInv
        where rightInv = MI.value $ A.inv right

    instance A.Negatable BigVector where
        neg = BS.rightScalarMult BS.negOne

    instance BS.Graphable BigVector where
        absol = MI.withValue . BS.unsafeAbsol
        unsafeAbsol vec = (A.unsafeSqrt . MI.value) $ vdot vec vec
        norm vec
            | vIsNull vec = MI.withError MI.InvalidValue
            | otherwise = vdivs vec (BS.unsafeAbsol vec)

    vdist :: BigVector -> BigVector -> MI.ComputationResult BS.BigScalar
    vdist left right
        | not $ BS.equalSize left right = MI.withError MI.InvalidValue
        | otherwise = (MI.withValue . A.unsafeSqrt . _sum) $ fmap square (_pos subtValue)
        where subtValue = A.unsafeMinus left right
              square = MI.value . ((flip A.pow) BS.two)

    vangle :: BigVector -> BigVector -> MI.ComputationResult BS.BigScalar
    vangle left right
        | (not $ BS.equalSize left right) || (vIsNull left) || (vIsNull right) = MI.withError MI.InvalidValue
        | otherwise = BS.sacos $ A.unsafeDiv dotProd absProd
        where dotProd = MI.value $ vdot left right
              absProd = A.unsafeMult (BS.unsafeAbsol left) (BS.unsafeAbsol right)

    asList :: BigVector -> [BS.BigScalar]
    asList (BigVector pos) = F.toList pos
    
    asSeq :: BigVector -> S.Seq BS.BigScalar
    asSeq (BigVector pos) = pos

{-# LANGUAGE MultiParamTypeClasses #-}

module Actions (
    Addable,
    Subtractable,
    Multipliable,
    Scalable,
    Divisible,
    Power,
    Negatable,
    Comparable,
    plus,
    unsafePlus,
    minus,
    unsafeMinus,
    mult,
    unsafeMult,
    scale,
    unsafeScale,
    div,
    inv,
    unsafeDiv,
    unsafeInv,
    pow,
    sqrt,
    log,
    logBase,
    exp,
    unsafePow,
    unsafeSqrt,
    unsafeExp,
    unsafeLog,
    unsafeLogBase,
    neg,
    unsafeNeg,
    min,
    max,
    less,
    greater,
    lessEqual,
    greaterEqual,
    unsafeMin,
    unsafeMax,
    unsafeLess,
    unsafeGreater,
    unsafeLessEqual,
    unsafeGreaterEqual
) where
    import Prelude hiding (div, pow, exp, log, logBase, sqrt, min, max, mod, rem)
    import qualified MathInfo as MI

    class Addable a where
        plus :: a -> a -> MI.ComputationResult a
        unsafePlus :: a -> a -> a
        unsafePlus = MI.binaryNonerror plus

    class Subtractable a where
        minus :: a -> a -> MI.ComputationResult a
        unsafeMinus :: a -> a -> a
        unsafeMinus = MI.binaryNonerror minus

    class Multipliable a where
        mult :: a -> a -> MI.ComputationResult a
        unsafeMult :: a -> a -> a
        unsafeMult = MI.binaryNonerror mult

    class Scalable a where
        scale :: a -> a -> MI.ComputationResult a
        unsafeScale :: a -> a -> a
        unsafeScale = MI.binaryNonerror scale

    class Divisible a where
        div :: a -> a -> MI.ComputationResult a
        inv :: a -> MI.ComputationResult a
        unsafeDiv :: a -> a -> a
        unsafeDiv = MI.binaryNonerror div
        unsafeInv :: a -> a
        unsafeInv = MI.unaryNonerror inv

    class Power a where
        pow :: a -> a -> MI.ComputationResult a
        sqrt :: a -> MI.ComputationResult a
        exp :: a -> MI.ComputationResult a
        log :: a -> MI.ComputationResult a
        logBase :: a -> a -> MI.ComputationResult a
        unsafePow :: a -> a -> a
        unsafePow = MI.binaryNonerror pow
        unsafeSqrt :: a -> a
        unsafeSqrt = MI.unaryNonerror sqrt
        unsafeExp :: a -> a
        unsafeExp = MI.unaryNonerror exp
        unsafeLog :: a -> a
        unsafeLog = MI.unaryNonerror log
        unsafeLogBase :: a -> a -> a
        unsafeLogBase = MI.binaryNonerror logBase

    class Negatable a where
        neg :: a -> MI.ComputationResult a
        unsafeNeg :: a -> a
        unsafeNeg = MI.unaryNonerror neg

    class Comparable a where
        min :: a -> a -> MI.ComputationResult a
        max :: a -> a -> MI.ComputationResult a
        less :: a -> a -> MI.ComputationResult Bool
        greater :: a -> a -> MI.ComputationResult Bool
        greater left right = less right left
        lessEqual :: a -> a -> MI.ComputationResult Bool
        lessEqual left right = MI.unResolve greaterResult not
            where greaterResult = greater left right
        greaterEqual :: a -> a -> MI.ComputationResult Bool
        greaterEqual left right = MI.unResolve lessResult not
            where lessResult = less left right
        unsafeMin :: a -> a -> a
        unsafeMin = MI.binaryNonerror min
        unsafeMax :: a -> a -> a
        unsafeMax = MI.binaryNonerror max
        unsafeLess :: a -> a -> Bool
        unsafeLess = MI.binaryNonerror less
        unsafeGreater :: a -> a -> Bool
        unsafeGreater = MI.binaryNonerror greater
        unsafeLessEqual :: a -> a -> Bool
        unsafeLessEqual = MI.binaryNonerror lessEqual
        unsafeGreaterEqual :: a -> a -> Bool
        unsafeGreaterEqual = MI.binaryNonerror greaterEqual

module Settings (
    RealForm(
        Decimal,
        Rational
    ),
    ComplexForm(
        Rectangular,
        Polar,
        Exponential
    ),
    AngleUnit(
        Radian,
        Degree
    ),
    Settings,
    decimals,
    realForm,
    complexForm,
    angleUnit,
    (==),
    (/=),
    show
) where
    data RealForm =
        Decimal |
        Rational
        deriving (Enum, Eq, Show)

    data ComplexForm =
        Rectangular |
        Polar |
        Exponential
        deriving (Enum, Eq, Show)

    data AngleUnit =
        Radian |
        Degree
        deriving (Enum, Eq, Show)

    data Settings = Settings {
        decimals :: Int,
        realForm :: RealForm,
        complexForm :: ComplexForm,
        angleUnit :: AngleUnit
    } deriving (Show)

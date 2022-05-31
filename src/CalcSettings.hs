module CalcSettings (
    StringMode(
        Round,
        Sigfig,
        Simple
    ),
    NotationMode(
        Standard,
        Scientific
    ),
    AngleMode(
        Degree,
        Radian
    ),
    ImagRepresentation(
        Rectangular,
        Polar,
        Exponential
    ),
    PrintSettings,
    GeneralSettings,
    prints,
    notation,
    angle,
    imagRep,
    settings,
    string,
) where

    data StringMode = Round {
        _roundCount :: Int
    } | Sigfig {
        _sigfigCount :: Int
    } | Simple deriving (Eq, Show)

    data NotationMode = Standard | Scientific deriving (Enum, Eq, Show)

    data AngleMode = Degree | Radian deriving (Enum, Eq, Show)

    data ImagRepresentation = Rectangular | Polar | Exponential deriving (Enum, Eq, Show)

    data PrintSettings = PrintSettings {
        string :: StringMode,
        notation :: NotationMode
    } deriving (Show)

    data GeneralSettings = GeneralSettings {
        prints :: PrintSettings,
        angle :: AngleMode,
        imagRep :: ImagRepresentation
    } deriving (Show)

    settings :: GeneralSettings
    settings = GeneralSettings (PrintSettings (Sigfig 15) Standard) Radian Rectangular

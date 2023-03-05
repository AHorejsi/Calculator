module CalcSettings (
    AngleMode(
        Degree,
        Radian
    ),
    ImagRep(
        Rectangular,
        Polar,
        Exponential
    ),
    GeneralSettings,
    angle,
    imagRep,
    pretty,
    settings,
) where

    data AngleMode = Degree | Radian

    data ImagRep = Rectangular | Polar | Exponential

    data GeneralSettings = GeneralSettings {
        angle :: AngleMode,
        imagRep :: ImagRep,
        pretty :: Bool
    }

    settings :: GeneralSettings
    settings = GeneralSettings Radian Rectangular False

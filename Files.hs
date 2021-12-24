module Files(
    setsHandle,
    constsHandle,
    varsHandle
) where
    import System.IO

    setsHandle :: IO Handle
    setsHandle = openFile "data/settings.json" ReadWriteMode

    constsHandle :: IO Handle
    constsHandle = openFile "data/const.json" ReadMode

    varsHandle :: IO Handle
    varsHandle = openFile "data/var.json" ReadWriteMode

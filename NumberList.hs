module NumberList (

) where
    import Data.List
    import Imaginary

    newtype RealList a = RealList { rnums :: [a] } deriving (Show, Eq)
    newtype ComplexList a = ComplexList { cnums :: [Complex a] } deriving (Show, Eq)
    newtype QuaternionList a = QuaternionList { qnums :: [Quaternion a] } deriving (Show, Eq)

    rlsize :: RealList a -> Int
    rlsize (RealList nums) = length nums

    clsize :: ComplexList a -> Int
    clsize (ComplexList nums) = length nums

    qlsize :: QuaternionList a -> Int
    qlsize (QuaternionList nums) = length nums

    rplusrl :: (Num a) => a -> RealList a -> RealList a
    rplusrl leftReal (RealList rightNums) = RealList $ map (+leftReal) rightNums

    rlplusr :: (Num a) => RealList a -> a -> RealList a
    rlplusr left right = rplusrl right left

    cplusrl :: (Num a) => Complex a -> RealList a -> ComplexList a
    cplusrl leftCom (RealList rightNums) = ComplexList $ map (cplusr leftCom) rightNums

    rlplusc :: (Num a) => RealList a -> Complex a -> ComplexList a
    rlplusc left right = cplusrl right left

    qplusrl :: (Num a) => Quaternion a -> RealList a -> QuaternionList a
    qplusrl leftQuat (RealList rightNums) = QuaternionList $ map (qplusr leftQuat) rightNums

    rlplusq :: (Num a) => RealList a -> Quaternion a -> QuaternionList a
    rlplusq left right = qplusrl right left

    rpluscl :: (Num a) => a -> ComplexList a -> ComplexList a
    rpluscl leftReal (ComplexList rightNums) = ComplexList $ map (rplusc leftReal) rightNums

    clplusr :: (Num a) => ComplexList a -> a -> ComplexList a
    clplusr left right = rpluscl right left

    cpluscl :: (Num a) => Complex a -> ComplexList a -> ComplexList a
    cpluscl leftCom (ComplexList rightNums) = ComplexList $ map (cplusc leftCom) rightNums

    clplusc :: (Num a) => ComplexList a -> Complex a -> ComplexList a
    clplusc left right = cpluscl right left

    qpluscl :: (Num a) => Quaternion a -> ComplexList a -> QuaternionList a
    qpluscl leftQuat (ComplexList rightNums) = QuaternionList $ map (qplusc leftQuat) rightNums

    clplusq :: (Num a) => ComplexList a -> Quaternion a -> QuaternionList a
    clplusq left right = qpluscl right left

    rplusql :: (Num a) => a -> QuaternionList a -> QuaternionList a
    rplusql leftReal (QuaternionList rightNums) = QuaternionList $ map (rplusq leftReal) rightNums

    qlplusr :: (Num a) => QuaternionList a -> a -> QuaternionList a
    qlplusr left right = rplusql right left

    cplusql :: (Num a) => Complex a -> QuaternionList a -> QuaternionList a
    cplusql leftCom (QuaternionList rightNums) = QuaternionList $ map (cplusq leftCom) rightNums

    qlplusc :: (Num a) => QuaternionList a -> Complex a -> QuaternionList a
    qlplusc left right = cplusql right left

    qplusql :: (Num a) => Quaternion a -> QuaternionList a -> QuaternionList a
    qplusql leftQuat (QuaternionList rightNums) = QuaternionList $ map (qplusq leftQuat) rightNums

    qlplusq :: (Num a) => QuaternionList a -> Quaternion a -> QuaternionList a
    qlplusq left right = qplusql right left

    rlplusrl :: (Num a) => RealList a -> RealList a -> RealList a
    rlplusrl leftRealList rightRealList
        | (rlsize leftRealList) /= (rlsize rightRealList) = error "Lists must be of equal sizes"
        | otherwise = RealList $ zipWith (+) leftNums rightNums
        where leftNums = rnums leftRealList
              rightNums = rnums rightRealList

    rlpluscl :: (Num a) => RealList a -> ComplexList a -> ComplexList a
    rlpluscl leftRealList rightComList
        | (rlsize leftRealList) /= (clsize rightComList) = error "Lists must be of equal sizes"
        | otherwise = ComplexList $ zipWith rplusc leftNums rightNums
        where leftNums = rnums leftRealList
              rightNums = cnums rightComList

    clplusrl :: (Num a) => ComplexList a -> RealList a -> ComplexList a
    clplusrl left right = rlpluscl right left

    clpluscl :: (Num a) => ComplexList a -> ComplexList a -> ComplexList a
    clpluscl leftComList rightComList
        | (clsize leftComList) /= (clsize rightComList) = error "Lists must be of equal sizes"
        | otherwise = ComplexList $ zipWith cplusc leftNums rightNums
        where leftNums = cnums leftComList
              rightNums = cnums rightComList

    rlplusql :: (Num a) => RealList a -> QuaternionList a -> QuaternionList a
    rlplusql leftRealList rightQuatList
        | (rlsize leftRealList) /= (qlsize rightQuatList) = error "Lists must be of equal sizes"
        | otherwise = QuaternionList $ zipWith rplusq leftNums rightNums
        where leftNums = rnums leftRealList
              rightNums = qnums rightQuatList

    qlplusrl :: (Num a) => QuaternionList a -> RealList a -> QuaternionList a
    qlplusrl left right = rlplusql right left

    clplusql :: (Num a) => ComplexList a -> QuaternionList a -> QuaternionList a
    clplusql leftComList rightQuatList
        | (clsize leftComList) /= (qlsize rightQuatList) = error "Lists must be of equal sizes"
        | otherwise = QuaternionList $ zipWith cplusq leftNums rightNums
        where leftNums = cnums leftComList
              rightNums = qnums rightQuatList

    qlplusql :: (Num a) => QuaternionList a -> QuaternionList a -> QuaternionList a
    qlplusql leftQuatList rightQuatList
        | (qlsize leftQuatList) /= (qlsize rightQuatList) = error "Lists must be of equal sizes"
        | otherwise = QuaternionList $ zipWith qplusq leftNums rightNums
        where leftNums = qnums leftQuatList
              rightNums = qnums rightQuatList

    rminusrl :: (Num a) => a -> RealList a -> RealList a
    rminusrl leftReal (RealList rightNums) = RealList $ map (leftReal-) rightNums

    rlminusr :: (Num a) => RealList a -> a -> RealList a
    rlminusr (RealList leftNums) rightReal = RealList $ map (subtract rightReal) leftNums

    cminusrl :: (Num a) => Complex a -> RealList a -> ComplexList a
    cminusrl leftCom (RealList rightNums) = ComplexList $ map (cminusr leftCom) rightNums

    rlminusc :: (Num a) => RealList a -> Complex a -> ComplexList a
    rlminusc (RealList leftNums) rightCom = ComplexList $ map ((flip rminusc) rightCom) leftNums

    qminusrl :: (Num a) => Quaternion a -> RealList a -> QuaternionList a
    qminusrl leftQuat (RealList rightNums) = QuaternionList $ map (qminusr leftQuat) rightNums

    rlminusq :: (Num a) => RealList a -> Quaternion a -> QuaternionList a
    rlminusq (RealList leftNums) rightQuat = QuaternionList $ map ((flip rminusq) rightQuat) leftNums

    rminuscl :: (Num a) => a -> ComplexList a -> ComplexList a
    rminuscl leftReal (ComplexList rightNums) = ComplexList $ map (rminusc leftReal) rightNums

    clminusr :: (Num a) => ComplexList a -> a -> ComplexList a
    clminusr (ComplexList leftNums) rightReal = ComplexList $ map ((flip cminusr) rightReal) leftNums

    cminuscl :: (Num a) => Complex a -> ComplexList a -> ComplexList a
    cminuscl leftCom (ComplexList rightNums) = ComplexList $ map (cminusc leftCom) rightNums

    clminusc :: (Num a) => ComplexList a -> Complex a -> ComplexList a
    clminusc (ComplexList leftNums) rightCom = ComplexList $ map ((flip cminusc) rightCom) leftNums

    qminuscl :: (Num a) => Quaternion a -> ComplexList a -> QuaternionList a
    qminuscl leftQuat (ComplexList rightList) = QuaternionList $ map (qminusc leftQuat) rightList

    clminusq :: (Num a) => ComplexList a -> Quaternion a -> QuaternionList a
    clminusq (ComplexList leftList) rightQuat = QuaternionList $ map ((flip cminusq) rightQuat) leftList

    rminusql :: (Num a) => a -> QuaternionList a -> QuaternionList a
    rminusql leftReal (QuaternionList rightNums) = QuaternionList $ map (rminusq leftReal) rightNums

    qlminusr :: (Num a) => QuaternionList a -> a -> QuaternionList a
    qlminusr (QuaternionList leftNums) rightReal = QuaternionList $ map ((flip qminusr) rightReal) leftNums

    cminusql :: (Num a) => Complex a -> QuaternionList a -> QuaternionList a
    cminusql leftCom (QuaternionList rightNums) = QuaternionList $ map (cminusq leftCom) rightNums

    qlminusc :: (Num a) => QuaternionList a -> Complex a -> QuaternionList a
    qlminusc (QuaternionList leftNums) rightCom = QuaternionList $ map ((flip qminusc) rightCom) leftNums

    qminusql :: (Num a) => Quaternion a -> QuaternionList a -> QuaternionList a
    qminusql leftQuat (QuaternionList rightNums) = QuaternionList $ map (qminusq leftQuat) rightNums

    qlminusq :: (Num a) => QuaternionList a -> Quaternion a -> QuaternionList a
    qlminusq (QuaternionList leftNums) rightQuat = QuaternionList $ map ((flip qminusq) rightQuat) leftNums

    rlminusrl :: (Num a) => RealList a -> RealList a -> RealList a
    rlminusrl leftRealList rightRealList
        | (rlsize leftRealList) /= (rlsize rightRealList) = error "Lists must be of equal sizes"
        | otherwise = RealList $ zipWith (-) leftNums rightNums
        where leftNums = rnums leftRealList
              rightNums = rnums rightRealList

    rlminuscl :: (Num a) => RealList a -> ComplexList a -> ComplexList a
    rlminuscl leftRealList rightComList
        | (rlsize leftRealList) /= (clsize rightComList) = error "Lists must be of equal sizes"
        | otherwise = ComplexList $ zipWith rminusc leftNums rightNums
        where leftNums = rnums leftRealList
              rightNums = cnums rightComList

    clminusrl :: (Num a) => ComplexList a -> RealList a -> ComplexList a
    clminusrl leftComList rightRealList
        | (clsize leftComList) /= (rlsize rightRealList) = error "Lists must be of equal sizes"
        | otherwise = ComplexList $ zipWith cminusr leftNums rightNums
        where leftNums = cnums leftComList
              rightNums = rnums rightRealList

    clminuscl :: (Num a) => ComplexList a -> ComplexList a -> ComplexList a
    clminuscl leftComList rightComList
        | (clsize leftComList) /= (clsize rightComList) = error "Lists must be of equal sizes"
        | otherwise = ComplexList $ zipWith cminusc leftNums rightNums
        where leftNums = cnums leftComList
              rightNums = cnums rightComList

    rlminusql :: (Num a) => RealList a -> QuaternionList a -> QuaternionList a
    rlminusql leftRealList rightQuatList
        | (rlsize leftRealList) /= (qlsize rightQuatList) = error "Lists must be of equal sizes"
        | otherwise = QuaternionList $ zipWith rminusq leftNums rightNums
        where leftNums = rnums leftRealList
              rightNums = qnums rightQuatList

    qlminusrl :: (Num a) => QuaternionList a -> RealList a -> QuaternionList a
    qlminusrl leftQuatList rightRealList
        | (qlsize leftQuatList) /= (rlsize rightRealList) = error "Lists must of equal sizes"
        | otherwise = QuaternionList $ zipWith qminusr leftNums rightNums
        where leftNums = qnums leftQuatList
              rightNums = rnums rightRealList

    clminusql :: (Num a) => ComplexList a -> QuaternionList a -> QuaternionList a
    clminusql leftComList rightQuatList
        | (clsize leftComList) /= (qlsize rightQuatList) = error "Lists must be of equal sizes"
        | otherwise = QuaternionList $ zipWith cminusq leftNums rightNums
        where leftNums = cnums leftComList
              rightNums = qnums rightQuatList

    qlminuscl :: (Num a) => QuaternionList a -> ComplexList a -> QuaternionList a
    qlminuscl leftQuatList rightComList
        | (qlsize leftQuatList) /= (clsize rightComList) = error "Lists must be of equal sizes"
        | otherwise = QuaternionList $ zipWith qminusc leftNums rightNums
        where leftNums = qnums leftQuatList
              rightNums = cnums rightComList
    
    qlminusql :: (Num a) => QuaternionList a -> QuaternionList a -> QuaternionList a
    qlminusql leftQuatList rightQuatList
        | (qlsize leftQuatList) /= (qlsize rightQuatList) = error "Lists must be of equal sizes"
        | otherwise = QuaternionList $ zipWith qminusq leftNums rightNums
        where leftNums = qnums leftQuatList
              rightNums = qnums rightQuatList

    rmultrl :: (Num a) => a -> RealList a -> RealList a
    rmultrl leftReal (RealList rightNums) = RealList $ map (*leftReal) rightNums

    rlmultr :: (Num a) => RealList a -> a -> RealList a
    rlmultr left right = rmultrl right left

    cmultrl :: (Num a) => Complex a -> RealList a -> ComplexList a
    cmultrl leftCom (RealList rightNums) = ComplexList $ map (cmultr leftCom) rightNums

    rlmultc :: (Num a) => RealList a -> Complex a -> ComplexList a
    rlmultc left right = cmultrl right left

    qmultrl :: (Num a) => Quaternion a -> RealList a -> QuaternionList a
    qmultrl leftQuat (RealList rightNums) = QuaternionList $ map (qmultr leftQuat) rightNums

    rlmultq :: (Num a) => RealList a -> Quaternion a -> QuaternionList a
    rlmultq left right = qmultrl right left

    rmultcl :: (Num a) => a -> ComplexList a -> ComplexList a
    rmultcl leftReal (ComplexList rightNums) = ComplexList $ map (rmultc leftReal) rightNums

    clmultr :: (Num a) => ComplexList a -> a -> ComplexList a
    clmultr left right = rmultcl right left

    cmultcl :: (Num a) => Complex a -> ComplexList a -> ComplexList a
    cmultcl leftCom (ComplexList rightNums) = ComplexList $ map (cmultc leftCom) rightNums

    clmultc :: (Num a) => ComplexList a -> Complex a -> ComplexList a
    clmultc left right = cmultcl right left

    qmultcl :: (Num a) => Quaternion a -> ComplexList a -> QuaternionList a
    qmultcl leftQuat (ComplexList rightNums) = QuaternionList $ map (qmultc leftQuat) rightNums

    clmultq :: (Num a) => ComplexList a -> Quaternion a -> QuaternionList a
    clmultq (ComplexList leftNums) rightQuat = QuaternionList $ map ((flip cmultq) rightQuat) leftNums

    rmultql :: (Num a) => a -> QuaternionList a -> QuaternionList a
    rmultql leftReal (QuaternionList rightNums) = QuaternionList $ map (rmultq leftReal) rightNums

    qlmultr :: (Num a) => QuaternionList a -> a -> QuaternionList a
    qlmultr left right = rmultql right left

    cmultql :: (Num a) => Complex a -> QuaternionList a -> QuaternionList a
    cmultql leftCom (QuaternionList rightNums) = QuaternionList $ map (cmultq leftCom) rightNums

    qlmultc :: (Num a) => QuaternionList a -> Complex a -> QuaternionList a
    qlmultc (QuaternionList leftNums) rightCom = QuaternionList $ map ((flip qmultc) rightCom) leftNums

    qmultql :: (Num a) => Quaternion a -> QuaternionList a -> QuaternionList a
    qmultql leftQuat (QuaternionList rightNums) = QuaternionList $ map (qmultq leftQuat) rightNums

    qlmultq :: (Num a) => QuaternionList a -> Quaternion a -> QuaternionList a
    qlmultq (QuaternionList leftNums) rightQuat = QuaternionList $ map ((flip qmultq) rightQuat) leftNums

    rlmultrl :: (Num a) => RealList a -> RealList a -> RealList a
    rlmultrl leftRealList rightRealList
        | (rlsize leftRealList) /= (rlsize rightRealList) = error "Lists must be of equal sizes"
        | otherwise = RealList $ zipWith (*) leftNums rightNums
        where leftNums = rnums leftRealList
              rightNums = rnums rightRealList

    rlmultcl :: (Num a) => RealList a -> ComplexList a -> ComplexList a
    rlmultcl leftRealList rightComList
        | (rlsize leftRealList) /= (clsize rightComList) = error "Lists must be of equal sizes"
        | otherwise = ComplexList $ zipWith rmultc leftNums rightNums
        where leftNums = rnums leftRealList
              rightNums = cnums rightComList

    clmultrl :: (Num a) => ComplexList a -> RealList a -> ComplexList a
    clmultrl left right = rlmultcl right left

    clmultcl :: (Num a) => ComplexList a -> ComplexList a -> ComplexList a
    clmultcl leftComList rightComList
        | (clsize leftComList) /= (clsize rightComList) = error "Lists must be of equal sizes"
        | otherwise = ComplexList $ zipWith cmultc leftNums rightNums
        where leftNums = cnums leftComList
              rightNums = cnums rightComList

    rlmultql :: (Num a) => RealList a -> QuaternionList a -> QuaternionList a
    rlmultql leftRealList rightQuatList
        | (rlsize leftRealList) /= (qlsize rightQuatList) = error "Lists must be of equal sizes"
        | otherwise = QuaternionList $ zipWith rmultq leftNums rightNums
        where leftNums = rnums leftRealList
              rightNums = qnums rightQuatList

    qlmultrl :: (Num a) => QuaternionList a -> RealList a -> QuaternionList a
    qlmultrl left right = rlmultql right left

    clmultql :: (Num a) => ComplexList a -> QuaternionList a -> QuaternionList a
    clmultql leftComList rightQuatList
        | (clsize leftComList) /= (qlsize rightQuatList) = error "Lists must be of equal sizes"
        | otherwise = QuaternionList $ zipWith cmultq leftNums rightNums
        where leftNums = cnums leftComList
              rightNums = qnums rightQuatList

    qlmultcl :: (Num a) => QuaternionList a -> ComplexList a -> QuaternionList a
    qlmultcl leftQuatList rightComList
        | (qlsize leftQuatList) /= (clsize rightComList) = error "Lists must be of equal sizes"
        | otherwise = QuaternionList $ zipWith qmultc leftNums rightNums
        where leftNums = qnums leftQuatList
              rightNums = cnums rightComList

    qlmultql :: (Num a) => QuaternionList a -> QuaternionList a -> QuaternionList a
    qlmultql leftQuatList rightQuatList
        | (qlsize leftQuatList) /= (qlsize rightQuatList) = error "Lists must be of equal sizes"
        | otherwise = QuaternionList $ zipWith qmultq leftNums rightNums
        where leftNums = qnums leftQuatList
              rightNums = qnums rightQuatList

    rdivrl :: (Fractional a) => a -> RealList a -> RealList a
    rdivrl leftReal (RealList rightNums) = RealList $ map (leftReal/) rightNums

    rldivr :: (Fractional a) => RealList a -> a -> RealList a
    rldivr (RealList leftNums) rightReal = RealList $ map (/rightReal) leftNums

    cdivrl :: (Fractional a) => Complex a -> RealList a -> ComplexList a
    cdivrl leftCom (RealList rightNums) = ComplexList $ map (cdivr leftCom) rightNums

    rldivc :: (Fractional a) => RealList a -> Complex a -> ComplexList a
    rldivc (RealList leftNums) rightCom = ComplexList $ map ((flip rdivc) rightCom) leftNums

    qdivrl :: (Fractional a) => Quaternion a -> RealList a -> QuaternionList a
    qdivrl leftQuat (RealList rightNums) = QuaternionList $ map (qdivr leftQuat) rightNums

    rldivq :: (Fractional a) => RealList a -> Quaternion a -> QuaternionList a
    rldivq (RealList leftNums) rightQuat = QuaternionList $ map ((flip rdivq) rightQuat) leftNums

    rdivcl :: (Fractional a) => a -> ComplexList a -> ComplexList a
    rdivcl leftReal (ComplexList rightNums) = ComplexList $ map (rdivc leftReal) rightNums

    cldivr :: (Fractional a) => ComplexList a -> a -> ComplexList a
    cldivr (ComplexList leftNums) rightReal = ComplexList $ map ((flip cdivr) rightReal) leftNums

    cdivcl :: (Fractional a) => Complex a -> ComplexList a -> ComplexList a
    cdivcl leftCom (ComplexList rightNums) = ComplexList $ map (cdivc leftCom) rightNums

    cldivc :: (Fractional a) => ComplexList a -> Complex a -> ComplexList a
    cldivc (ComplexList leftNums) rightCom = ComplexList $ map ((flip cdivc) rightCom) leftNums

    qdivcl :: (Fractional a) => Quaternion a -> ComplexList a -> QuaternionList a
    qdivcl leftQuat (ComplexList rightNums) = QuaternionList $ map (qdivc leftQuat) rightNums

    cldivq :: (Fractional a) => ComplexList a -> Quaternion a -> QuaternionList a
    cldivq (ComplexList leftNums) rightQuat = QuaternionList $ map ((flip cdivq) rightQuat) leftNums

    rdivql :: (Fractional a) => a -> QuaternionList a -> QuaternionList a
    rdivql leftReal (QuaternionList rightNums) = QuaternionList $ map (rdivq leftReal) rightNums

    qldivr :: (Fractional a) => QuaternionList a -> a -> QuaternionList a
    qldivr (QuaternionList leftNums) rightReal = QuaternionList $ map ((flip qdivr) rightReal) leftNums

    cdivql :: (Fractional a) => Complex a -> QuaternionList a -> QuaternionList a
    cdivql leftCom (QuaternionList rightNums) = QuaternionList $ map (cdivq leftCom) rightNums

    qldivc :: (Fractional a) => QuaternionList a -> Complex a -> QuaternionList a
    qldivc (QuaternionList leftNums) rightCom = QuaternionList $ map ((flip qdivc) rightCom) leftNums

    qdivql :: (Fractional a) => Quaternion a -> QuaternionList a -> QuaternionList a
    qdivql leftQuat (QuaternionList rightNums) = QuaternionList $ map (qdivq leftQuat) rightNums

    qldivq :: (Fractional a) => QuaternionList a -> Quaternion a -> QuaternionList a
    qldivq (QuaternionList leftNums) rightQuat = QuaternionList $ map ((flip qdivq) rightQuat) leftNums

    rldivrl :: (Fractional a) => RealList a -> RealList a -> RealList a
    rldivrl leftRealList rightRealList
        | (rlsize leftRealList) /= (rlsize rightRealList) = error "Lists must be of equal sizes"
        | otherwise = RealList $ zipWith (/) leftNums rightNums
        where leftNums = rnums leftRealList
              rightNums = rnums rightRealList

    rldivcl :: (Fractional a) => RealList a -> ComplexList a -> ComplexList a
    rldivcl leftRealList rightComList
        | (rlsize leftRealList) /= (clsize rightComList) = error "Lists must be of equal sizes"
        | otherwise = ComplexList $ zipWith rdivc leftNums rightNums
        where leftNums = rnums leftRealList
              rightNums = cnums rightComList

    cldivrl :: (Fractional a) => ComplexList a -> RealList a -> ComplexList a
    cldivrl leftComList rightRealList
        | (clsize leftComList) /= (rlsize rightRealList) = error "Lists must be of equal sizes"
        | otherwise = ComplexList $ zipWith cdivr leftNums rightNums
        where leftNums = cnums leftComList
              rightNums = rnums rightRealList

    cldivcl :: (Fractional a) => ComplexList a -> ComplexList a -> ComplexList a
    cldivcl leftComList rightComList
        | (clsize leftComList) /= (clsize rightComList) = error "Lists must be of equal sizes"
        | otherwise = ComplexList $ zipWith cdivc leftNums rightNums
        where leftNums = cnums leftComList
              rightNums = cnums rightComList

    rldivql :: (Fractional a) => RealList a -> QuaternionList a -> QuaternionList a
    rldivql leftRealList rightQuatList
        | (rlsize leftRealList) /= (qlsize rightQuatList) = error "Lists must be of equal sizes"
        | otherwise = QuaternionList $ zipWith rdivq leftNums rightNums
        where leftNums = rnums leftRealList
              rightNums = qnums rightQuatList

    qldivrl :: (Fractional a) => QuaternionList a -> RealList a -> QuaternionList a
    qldivrl leftQuatList rightRealList
        | (qlsize leftQuatList) /= (rlsize rightRealList) = error "Lists must be of equal sizes"
        | otherwise = QuaternionList $ zipWith qdivr leftNums rightNums
        where leftNums = qnums leftQuatList
              rightNums = rnums rightRealList

    cldivql :: (Fractional a) => ComplexList a -> QuaternionList a -> QuaternionList a
    cldivql leftComList rightQuatList
        | (clsize leftComList) /= (qlsize rightQuatList) = error "Lists must be of equal sizes"
        | otherwise = QuaternionList $ zipWith cdivq leftNums rightNums
        where leftNums = cnums leftComList
              rightNums = qnums rightQuatList

    qldivcl :: (Fractional a) => QuaternionList a -> ComplexList a -> QuaternionList a
    qldivcl leftQuatList rightComList
        | (qlsize leftQuatList) /= (clsize rightComList) = error "Lists must be of equal sizes"
        | otherwise = QuaternionList $ zipWith qdivc leftNums rightNums
        where leftNums = qnums leftQuatList
              rightNums = cnums rightComList

    qldivql :: (Fractional a) => QuaternionList a -> QuaternionList a -> QuaternionList a
    qldivql leftQuatList rightQuatList
        | (qlsize leftQuatList) /= (qlsize rightQuatList) = error "Lists must be of equal sizes"
        | otherwise = QuaternionList $ zipWith qdivq leftNums rightNums
        where leftNums = qnums leftQuatList
              rightNums = qnums rightQuatList

    rlneg :: (Num a) => RealList a -> RealList a
    rlneg (RealList nums) = RealList $ map negate nums

    clneg :: (Num a) => ComplexList a -> ComplexList a
    clneg (ComplexList nums) = ComplexList $ map cneg nums

    qlneg :: (Num a) => QuaternionList a -> QuaternionList a
    qlneg (QuaternionList nums) = QuaternionList $ map qneg nums

    rpowrl :: (RealFloat a) => a -> RealList a -> RealList a
    rpowrl leftReal (RealList rightNums) = RealList $ map (leftReal**) rightNums

    rlpowr :: (RealFloat a) => RealList a -> a -> RealList a
    rlpowr (RealList leftNums) rightReal = RealList $ map ((flip (**)) rightReal) leftNums

    cpowrl :: (RealFloat a) => Complex a -> RealList a -> ComplexList a
    cpowrl leftCom (RealList rightNums) = ComplexList $ map (cpowr leftCom) rightNums

    rlpowc :: (RealFloat a) => RealList a -> Complex a -> ComplexList a
    rlpowc (RealList leftNums) rightCom = ComplexList $ map ((flip rpowc) rightCom) leftNums

    qpowrl :: (RealFloat a) => Quaternion a -> RealList a -> QuaternionList a
    qpowrl leftQuat (RealList rightNums) = QuaternionList $ map (qpowr leftQuat) rightNums

    rlpowq :: (RealFloat a) => RealList a -> Quaternion a -> QuaternionList a
    rlpowq (RealList leftNums) rightQuat = QuaternionList $ map ((flip rpowq) rightQuat) leftNums

    rpowcl :: (RealFloat a) => a -> ComplexList a -> ComplexList a
    rpowcl leftReal (ComplexList rightNums) = ComplexList $ map (rpowc leftReal) rightNums

    clpowr :: (RealFloat a) => ComplexList a -> a -> ComplexList a
    clpowr (ComplexList leftNums) rightReal = ComplexList $ map ((flip cpowr) rightReal) leftNums

    cpowcl :: (RealFloat a) => Complex a -> ComplexList a -> ComplexList a
    cpowcl leftCom (ComplexList rightNums) = ComplexList $ map (cpowc leftCom) rightNums

    clpowc :: (RealFloat a) => ComplexList a -> Complex a -> ComplexList a
    clpowc (ComplexList leftNums) rightCom = ComplexList $ map ((flip cpowc) rightCom) leftNums

    qpowcl :: (RealFloat a) => Quaternion a -> ComplexList a -> QuaternionList a
    qpowcl leftQuat (ComplexList rightNums) = QuaternionList $ map (qpowc leftQuat) rightNums

    clpowq :: (RealFloat a) => ComplexList a -> Quaternion a -> QuaternionList a
    clpowq (ComplexList leftNums) rightQuat = QuaternionList $ map ((flip cpowq) rightQuat) leftNums

    rpowql :: (RealFloat a) => a -> QuaternionList a -> QuaternionList a
    rpowql leftReal (QuaternionList rightNums) = QuaternionList $ map (rpowq leftReal) rightNums

    qlpowr :: (RealFloat a) => QuaternionList a -> a -> QuaternionList a
    qlpowr (QuaternionList leftNums) rightReal = QuaternionList $ map ((flip qpowr) rightReal) leftNums

    cpowql :: (RealFloat a) => Complex a -> QuaternionList a -> QuaternionList a
    cpowql leftCom (QuaternionList rightNums) = QuaternionList $ map (cpowq leftCom) rightNums

    qlpowc :: (RealFloat a) => QuaternionList a -> Complex a -> QuaternionList a
    qlpowc (QuaternionList leftNums) rightCom = QuaternionList $ map ((flip qpowc) rightCom) leftNums

    qpowql :: (RealFloat a) => Quaternion a -> QuaternionList a -> QuaternionList a
    qpowql leftQuat (QuaternionList rightNums) = QuaternionList $ map (qpowq leftQuat) rightNums

    qlpowq :: (RealFloat a) => QuaternionList a -> Quaternion a -> QuaternionList a
    qlpowq (QuaternionList leftNums) rightQuat = QuaternionList $ map ((flip qpowq) rightQuat) leftNums

    rlpowrl :: (RealFloat a) => RealList a -> RealList a -> RealList a
    rlpowrl leftRealList rightRealList
        | (rlsize leftRealList) /= (rlsize rightRealList) = error "Lists must be of equal sizes"
        | otherwise = RealList $ zipWith (**) leftNums rightNums
        where leftNums = rnums leftRealList
              rightNums = rnums rightRealList

    rlpowcl :: (RealFloat a) => RealList a -> ComplexList a -> ComplexList a
    rlpowcl leftRealList rightComList
        | (rlsize leftRealList) /= (clsize rightComList) = error "Lists must be of equal sizes"
        | otherwise = ComplexList $ zipWith rpowc leftNums rightNums
        where leftNums = rnums leftRealList
              rightNums = cnums rightComList

    clpowrl :: (RealFloat a) => ComplexList a -> RealList a -> ComplexList a
    clpowrl leftComList rightRealList
        | (clsize leftComList) /= (rlsize rightRealList) = error "Lists must be of equal sizes"
        | otherwise = ComplexList $ zipWith cpowr leftNums rightNums
        where leftNums = cnums leftComList
              rightNums = rnums rightRealList

    clpowcl :: (RealFloat a) => ComplexList a -> ComplexList a -> ComplexList a
    clpowcl leftComList rightComList 
        | (clsize leftComList) /= (clsize rightComList) = error "Lists must be of equal sizes"
        | otherwise = ComplexList $ zipWith cpowc leftNums rightNums
        where leftNums = cnums leftComList
              rightNums = cnums rightComList

    rlpowql :: (RealFloat a) => RealList a -> QuaternionList a -> QuaternionList a
    rlpowql leftRealList rightQuatList
        | (rlsize leftRealList) /= (qlsize rightQuatList) = error "Lists must be of equal sizes"
        | otherwise = QuaternionList $ zipWith rpowq leftNums rightNums
        where leftNums = rnums leftRealList
              rightNums = qnums rightQuatList

    qlpowrl :: (RealFloat a) => QuaternionList a -> RealList a -> QuaternionList a
    qlpowrl leftQuatList rightRealList
        | (qlsize leftQuatList) /= (rlsize rightRealList) = error "Lists must be of equal sizes"
        | otherwise = QuaternionList $ zipWith qpowr leftNums rightNums
        where leftNums = qnums leftQuatList
              rightNums = rnums rightRealList

    clpowql :: (RealFloat a) => ComplexList a -> QuaternionList a -> QuaternionList a
    clpowql leftComList rightQuatList
        | (clsize leftComList) /= (qlsize rightQuatList) = error "Lists must be of equal sizes"
        | otherwise = QuaternionList $ zipWith cpowq leftNums rightNums
        where leftNums = cnums leftComList
              rightNums = qnums rightQuatList

    qlpowcl :: (RealFloat a) => QuaternionList a -> ComplexList a -> QuaternionList a
    qlpowcl leftQuatList rightComList
        | (qlsize leftQuatList) /= (clsize rightComList) = error "Lists must be of equal sizes"
        | otherwise = QuaternionList $ zipWith qpowc leftNums rightNums
        where leftNums = qnums leftQuatList
              rightNums = cnums rightComList

    qlpowql :: (RealFloat a) => QuaternionList a -> QuaternionList a -> QuaternionList a
    qlpowql leftQuatList rightQuatList
        | (qlsize leftQuatList) /= (qlsize rightQuatList) = error "Lists must be of equal sizes"
        | otherwise = QuaternionList $ zipWith qpowq leftNums rightNums
        where leftNums = qnums leftQuatList
              rightNums = qnums rightQuatList

    rlmin :: (Num a, Ord a) => RealList a -> a
    rlmin (RealList nums) = _findMin nums (head nums)

    _findMin :: (Num a, Ord a) => [a] -> a -> a
    _findMin [] _ = error "Empty List"
    _findMin [val] minVal = _minValue val minVal
    _findMin (val:vals) minVal = _findMin vals (_minValue val minVal)

    _minValue :: (Num a, Ord a) => a -> a -> a
    _minValue first second = if first < second then first else second

    rlmax :: (Num a, Ord a) => RealList a -> a
    rlmax (RealList nums) = _findMax nums (head nums)

    _findMax :: (Num a, Ord a) => [a] -> a -> a
    _findMax [] _ = error "Empty List"
    _findMax [val] maxVal = _maxValue val maxVal
    _findMax (val:vals) maxVal = _findMax vals (_maxValue val maxVal)

    _maxValue :: (Num a, Ord a) => a -> a -> a
    _maxValue first second = if first > second then first else second

    rlsum :: (Num a) => RealList a -> a
    rlsum (RealList []) = error "Empty List"
    rlsum (RealList nums) = _combineReal (+) nums (head nums)

    clsum :: (Num a) => ComplexList a -> Complex a
    clsum (ComplexList []) = error "Empty List"
    clsum (ComplexList nums) = _combineComplex cplusc nums (head nums)

    qlsum :: (Num a) => QuaternionList a -> Quaternion a
    qlsum (QuaternionList []) = error "Empty List"
    qlsum (QuaternionList nums) = _combineQuaternion qplusq nums (head nums)

    rlprod :: (Num a) => RealList a -> a
    rlprod (RealList []) = error "Empty List"
    rlprod (RealList nums) = _combineReal (*) nums (head nums)

    clprod :: (Num a) => ComplexList a -> Complex a
    clprod (ComplexList []) = error "Empty List"
    clprod (ComplexList nums) = _combineComplex cmultc nums (head nums)

    qlprod :: (Num a) => QuaternionList a -> Quaternion a
    qlprod (QuaternionList []) = error "Empty List"
    qlprod (QuaternionList nums) = _combineQuaternion qmultq nums (head nums)

    _combineReal :: (Num a) => (a -> a -> a) -> [a] -> a -> a
    _combineReal _ [] current = current
    _combineReal accumulator (val:vals) current = _combineReal accumulator vals (accumulator val current)

    _combineComplex :: (Num a) => (Complex a -> Complex a -> Complex a) -> [Complex a] -> Complex a -> Complex a
    _combineComplex _ [] current = current
    _combineComplex accumulator (val:vals) current = _combineComplex accumulator vals (accumulator val current)

    _combineQuaternion :: (Num a) => (Quaternion a -> Quaternion a -> Quaternion a) -> [Quaternion a] -> Quaternion a -> Quaternion a
    _combineQuaternion _ [] current = current
    _combineQuaternion accumulator (val:vals) current = _combineQuaternion accumulator vals (accumulator val current)

    rlcumsum :: (Num a) => RealList a -> RealList a
    rlcumsum (RealList []) = error "Empty List"
    rlcumsum (RealList nums) = RealList $ _aggregateReal (+) nums 0

    clcumsum :: (Num a) => ComplexList a -> ComplexList a
    clcumsum (ComplexList []) = error "Empty List"
    clcumsum (ComplexList nums) = ComplexList $ _aggregateComplex cplusc nums c0

    qlcumsum :: (Num a) => QuaternionList a -> QuaternionList a
    qlcumsum (QuaternionList []) = error "Empty List"
    qlcumsum (QuaternionList nums) = QuaternionList $ _aggregateQuaternion qplusq nums q0

    rlcumprod :: (Num a) => RealList a -> RealList a
    rlcumprod (RealList []) = error "Empty List"
    rlcumprod (RealList nums) = RealList $ _aggregateReal (*) nums 1

    clcumprod :: (Num a) => ComplexList a -> ComplexList a
    clcumprod (ComplexList []) = error "Empty List"
    clcumprod (ComplexList nums) = ComplexList $ _aggregateComplex cmultc nums c1

    qlcumprod :: (Num a) => QuaternionList a -> QuaternionList a
    qlcumprod (QuaternionList []) = error "Empty List"
    qlcumprod (QuaternionList nums) = QuaternionList $ _aggregateQuaternion qmultq nums q1

    _aggregateReal :: (Num a) => (a -> a -> a) -> [a] -> a -> [a]
    _aggregateReal _ [] _ = []
    _aggregateReal accumulator (val:vals) current = result : _aggregateReal accumulator vals result
        where result = accumulator val current

    _aggregateComplex :: (Num a) => (Complex a -> Complex a -> Complex a) -> [Complex a] -> Complex a -> [Complex a]
    _aggregateComplex _ [] _ = []
    _aggregateComplex accumulator (val:vals) current = result : _aggregateComplex accumulator vals result
        where result = accumulator val current

    _aggregateQuaternion :: (Num a) => (Quaternion a -> Quaternion a -> Quaternion a) -> [Quaternion a] -> Quaternion a -> [Quaternion a]
    _aggregateQuaternion _ [] _ = []
    _aggregateQuaternion accumulator (val:vals) current = result : _aggregateQuaternion accumulator vals result
        where result = accumulator val current

    rlmean :: (Fractional a) => RealList a -> a
    rlmean realList = (rlsum realList) / (fromIntegral $ rlsize realList)

    clmean :: (Fractional a) => ComplexList a -> Complex a
    clmean comList = cdivr (clsum comList) (fromIntegral $ clsize comList)

    qlmean :: (Fractional a) => QuaternionList a -> Quaternion a
    qlmean quatList = qdivr (qlsum quatList) (fromIntegral $ qlsize quatList)

    rlgmean :: (RealFloat a) => RealList a -> a
    rlgmean realList = (rlprod realList) ** (recip $ fromIntegral $ rlsize realList)

    clgmean :: (RealFloat a) => ComplexList a -> Complex a
    clgmean comList = cpowr (clprod comList) (recip $ fromIntegral $ clsize comList)

    qlgmean :: (RealFloat a) => QuaternionList a -> Quaternion a
    qlgmean quatList = qpowr (qlprod quatList) (recip $ fromIntegral $ qlsize quatList)

    rlhmean :: (Fractional a) => RealList a -> a
    rlhmean realList = (fromIntegral $ rlsize realList) / (rlsum invs)
        where nums = rnums realList
              invs = RealList $ map recip nums

    clhmean :: (Fractional a) => ComplexList a -> Complex a
    clhmean comList = rdivc (fromIntegral $ clsize comList) (clsum invs)
        where nums = cnums comList
              invs = ComplexList $ map cinv nums

    qlhmean :: (Fractional a) => QuaternionList a -> Quaternion a
    qlhmean quatList = rdivq (fromIntegral $ qlsize quatList) (qlsum invs)
        where nums = qnums quatList
              invs = QuaternionList $ map qinv nums

    rlmedian :: (Num a, Ord a) => RealList a -> a
    rlmedian realList
        | (mod count 2) == 0 = ((sorted !! (halfCount - 1)) + (sorted !! halfCount)) / 2
        | otherwise = sorted !! halfCount
        where nums = rnums realList
              count = rlsize realList
              halfCount = count / 2
              sorted = sort nums

    rlrange :: (Num a, Ord a) => RealList a -> a
    rlrange realList = (rlmax realList) - (rlmin realList)

    rlmidrange :: (Fractional a, Ord a) => RealList a -> a
    rlmidrange realList = ((rlmin realList) + (rlmax realList)) / 2

    rlvar :: (Fractional a) => RealList a -> a
    rlvar realList = (rlsum b) / (fromIntegral $ (rlsize realList) - 1)
        where meanVal = rlmean realList
              a = rlminusr realList meanVal
              b = rlplusrl a a

    clvar :: (Fractional a) => ComplexList a -> Complex a
    clvar comList = cdivr (clsum b) (fromIntegral $ (clsize comList) - 1)
        where meanVal = clmean comList
              a = clminusc comList meanVal
              b = clpluscl a a

    qlvar :: (Fractional a) => QuaternionList a -> Quaternion a
    qlvar quatList = qdivr (qlsum b) (fromIntegral $ (qlsize quatList) - 1)
        where meanVal = qlmean quatList
              a = qlminusq quatList meanVal
              b = qlplusql a a

    rlstddev :: (Floating a) => RealList a -> a
    rlstddev realList = sqrt $ rlvar realList

    clstddev :: (RealFloat a) => ComplexList a -> Complex a
    clstddev comList = csqrt $ clvar comList

    qlstddev :: (RealFloat a) => QuaternionList a -> Quaternion a
    qlstddev quatList = qsqrt $ qlvar quatList

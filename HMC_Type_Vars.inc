! HMC VARIABLE(S) DEFINITION

TYPE HMC_Type_Vars
    
    ! HMC STATIC VARIABLE(S) - SCALAR
    ! Land variable(s)
    integer(kind = 4)                   :: iDomainPixels
    real(kind = 4)                      :: dDxM, dDyM, dDEMMax, dDEMMin, dDEMStepMean
    real(kind = 4)                      :: dDomainArea
    
    ! Integration variable(s)
    integer(kind = 4)                   :: iDtIntegr, iDtIntegrPStep
    
    ! Logical variable(s)
    logical                             :: bLogLAI, bLogAlbedo, bInitLAI      
    
    ! HMC STATIC VARIABLE(S) - 1D, 2D
    ! Land variable(s)
    integer(kind = 4),      allocatable, dimension (:, :)       :: a2iMask, a2iPNT, a2iChoice
    integer(kind = 4),      allocatable, dimension (:, :)       :: a2iNature
    integer(kind = 4),      allocatable, dimension (:, :)       :: a2iXIndex, a2iYIndex, a2iArea
    real(kind = 4),         allocatable, dimension (:)          :: a1dFCN
    real(kind = 4),         allocatable, dimension (:, :)       :: a2dLon, a2dLat
    real(kind = 4),         allocatable, dimension (:, :)       :: a2dDem, a2dS, a2dCN, a2dAreaCell
    real(kind = 4),         allocatable, dimension (:, :)       :: a2dC1, a2dF2, a2dCostF, a2dCostK, a2dCostF1, a2dCostChFix
    real(kind = 4),         allocatable, dimension (:, :)       :: a2dAlpha, a2dBeta, a2dWTable, a2dWTableMax
    real(kind = 4),         allocatable, dimension (:, :)       :: a2dCt, a2dCf, a2dUc, a2dUh
    real(kind = 4),         allocatable, dimension (:, :)       :: a2dCtWP
    real(kind = 4),         allocatable, dimension (:, :)       :: a2dKb1, a2dKc1, a2dKb2, a2dKc2 
    real(kind = 4),         allocatable, dimension (:, :)       :: a2dCoeffResol
    real(kind = 4),         allocatable, dimension (:, :)       :: a2dExpRhoLow, a2dExpRhoHigh, a2dArctUp
    real(kind = 4),         allocatable, dimension (:, :)       :: a2dCoeffWS, a2dCoeffWDL
    
    ! Section variable(s)
    integer(kind = 4),      allocatable, dimension (:, :)       :: a2iXYSection
    character(len = 500),   allocatable, dimension (:)          :: a1sNameSection
    
    ! Lake variable(s)
    integer(kind = 4),      allocatable, dimension (:, :)       :: a2iXYLake
    integer(kind = 4),      allocatable, dimension (:)          :: a1iNCellLake
    real(kind = 4),         allocatable, dimension (:)          :: a1dCodeLake, a1dVMinLake, a1dVLake, a1dCostLake
    
    ! Joint variable(s)
    integer(kind = 4),      allocatable, dimension (:, :)       :: a2iXYJoint, a2iXYInJoint, a2iXYOutJoint
    real(kind = 4),         allocatable, dimension (:)          :: a1dThrLevelJoint
    
    ! Dam variable(s)
    integer(kind = 4),      allocatable, dimension (:, :)       :: a2iXYDam
    integer(kind = 4),      allocatable, dimension (:)          :: a1iNCellDam
    real(kind = 4),         allocatable, dimension (:)          :: a1dCodeDam, a1dVMaxDam
    real(kind = 4),         allocatable, dimension (:)          :: a1dQcSLDam, a1dHMaxDam, a1dCoeffDam
    real(kind = 4),         allocatable, dimension (:, :)       :: a2dVDam, a2dLDam
  
    ! Plant variable(s)
    integer(kind = 4),      allocatable, dimension (:, :)       :: a2iXYPlant
    integer(kind = 4),      allocatable, dimension (:)          :: a1iFlagDamPlant
    real(kind = 4),         allocatable, dimension (:)          :: a1dQMaxPlant, a1dTcPlant
    character(len = 500),   allocatable, dimension (:)          :: a1sNamePlant
    
    ! Catch and release variable(s)
    integer(kind = 4),      allocatable, dimension (:, :)       :: a2iXYCatch
    character(len = 500),   allocatable, dimension (:)          :: a1sNameCatch
    real(kind = 4),         allocatable, dimension (:)          :: a1dWeigthCatch, a1dTCorrCatch
    real(kind = 4),         allocatable, dimension (:)          :: a1dQMinCatch
    integer(kind = 4),      allocatable, dimension (:, :)       :: a2iXYRelease
    character(len = 500),   allocatable, dimension (:)          :: a1sNameRelease
    real(kind = 4),         allocatable, dimension (:)          :: a1dQMaxRelease
    
    ! HMC DYNAMIC VARIABLE(S) - SCALAR 
    ! Time variable(s)
    integer(kind = 4)                   :: iTime
    character(len = 19)                 :: sTimeStep, sTimeMaxLAI, sTimeMaxFC
    
    ! Volume variable(s)
    real(kind = 4)                      :: dVErr, dVarETTot
    
    ! HMC DYNAMIC VARIABLE(S) - 1D, 2D, 3D
    ! Forcing variable(s)
    real(kind = 4), allocatable,  	dimension(:, :)         :: a2dRain, a2dTa, a2dK, a2dW, a2dRHum
    real(kind = 4), allocatable,  	dimension(:, :)         :: a2dPres, a2dLAI, a2dFC, a2dAlbedo
    real(kind = 4), allocatable,        dimension(:, :)         :: a2dSHeight, a2dSKernel

    ! Updating variable(s)
    real(kind = 4), allocatable,        dimension(:, :)         :: a2dSCA, a2dSQA
    real(kind = 4), allocatable,        dimension(:, :)         :: a2dSMStar, a2dSMGain
    
    ! S3M variable(s)
    integer(kind = 4), allocatable,     dimension(:, :)         :: a2iAge
    real(kind = 4), allocatable,  	dimension(:, :)         :: a2dSWE, a2dRhoS, a2dRhoS0, a2dAlbedo_Snow
    real(kind = 4), allocatable,        dimension(:, :)         :: a2dSnowFall
    ! real(kind = 4), allocatable,        dimension(:, :)         :: a2dSnowFallDayCum
    ! real(kind = 4), allocatable,        dimension(:, :)         :: a2dMeltingDayCum
    real(kind = 4), allocatable,        dimension(:, :)         :: a2dMelting, a2dMeltingSc
    real(kind = 4), allocatable,        dimension(:, :)         :: a2dMaskS
    real(kind = 4), allocatable,        dimension(:, :, :)      :: a3dTaC_Days1, a3dTaC_Days5
    real(kind = 4), allocatable,        dimension(:, :, :)      :: a3dMelting, a3dSnowFall
     
    ! LSM variable(s)
    real(kind = 4), allocatable,  	dimension(:, :)         :: a2dLST
    real(kind = 4), allocatable,        dimension(:, :)         :: a2dRn, a2dH, a2dLE, a2dG, a2dEF
    real(kind = 4), allocatable,  	dimension(:, :, :)      :: a3dTaKMarked, a3dTaK24 
    
    ! ET variable(s)
    real(kind = 4), allocatable,  	dimension(:, :)         :: a2dET, a2dAE, a2dETpot, a2dAEpot
    real(kind = 4), allocatable,  	dimension(:, :, :)      :: a3dAE, a3dAEpot 
    
    ! Convolution variable(s) for channel network
    real(kind = 4), allocatable,  	dimension(:, :)         :: a2dHydro, a2dHydroPrev, a2dRouting, a2dDarcy
    real(kind = 4), allocatable,  	dimension(:, :)         :: a2dQDisOut, a2dQVolOut, a2dQTot, a2dQout
    real(kind = 4), allocatable,  	dimension(:, :)         :: a2dIntensity
    real(kind = 4), allocatable,        dimension(:, :)         :: a2dFlowDeep, a2dFlowExf
    
    real(kind = 4), allocatable,        dimension(:, :)         :: a2dUcAct, a2dUDt

    ! Water source variable(s)
    real(kind = 4), allocatable,        dimension(:,:)          :: a2dWSRunoff, a2dWDL
    
    ! Volume variable(s)
    real(kind = 4), allocatable,  	dimension(:, :)         :: a2dVTot, a2dVRet, a2dVSub, a2dVLoss, a2dVExf, a2dVErr
    real(kind = 4), allocatable,        dimension(:, :)         :: a2dVWT, a2dVWTMax
    
    ! Section, plant, catch and release variable(s)
    real(kind = 4), allocatable,        dimension (:)           :: a1dQoutSection
    real(kind = 4), allocatable,        dimension (:)           :: a1dQoutLake
    
    real(kind = 4), allocatable,        dimension (:, :)        :: a2dHydroPlant
    real(kind = 4), allocatable,        dimension (:, :)        :: a2dHydroCatch
    real(kind = 4), allocatable,        dimension (:, :)        :: a2dHydroRelease
    
    ! Dam variable(s)
    real(kind = 4), allocatable,        dimension (:)           :: a1dVDam, a1dLDam, a1dHDam
    real(kind = 4), allocatable,        dimension (:)           :: a1dQoutDam
    real(kind = 4), allocatable,        dimension (:)           :: a1dVDamObs
    
    ! Dynamic Vegetation module variable(s)
    real(kind = 4), allocatable,  	dimension(:, :)         :: a2dGd, a2dRSmin, a2dHveg, a2dBareSoil
    real(kind = 4), allocatable,  	dimension(:, :)         :: a2dVTotWP

    ! Time-Series variable(s)
    logical                             :: bFileForcingTimeSeries
    integer(kind = 4)                   :: iFileUnitTSQ, iFileUnitTSVDam
    logical                             :: bFileUnitTSQ, bFileUnitTSVDam
    
    ! Convolution variable(s) for channel fraction
    real(kind = 4), allocatable,        dimension (:, :)        :: a2dQC, a2dQH, a2dHydroC, a2dHydroH
    real(kind = 4), allocatable,        dimension (:, :)        :: a2dRunoffC, a2dRunoffH, a2dQup
    real(kind = 4), allocatable,        dimension (:, :)        :: a2dWidthC, a2dWidthH
     
    ! Fracturation variable(s)
    real(kind = 4), allocatable,        dimension (:, :)        :: a2dFrac
    
    ! Flooding variable (s)
    real(kind = 4), allocatable,        dimension (:, :)        :: a2dLevBankL, a2dLevBankR, a2dFirst
    real(kind = 4), allocatable,        dimension (:, :)        :: a2dQfloodIL, a2dQfloodCL, a2dQfloodIR, a2dQfloodCR

END TYPE HMC_Type_Vars

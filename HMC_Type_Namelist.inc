TYPE HMC_Type_Namelist
  
    ! Init
    integer(kind = 4)       :: iFlagDebugSet, iFlagDebugLevel
    
    integer(kind = 4)       :: iFlagOs
    
    integer(kind = 4)       :: iFlagTypeData_Static
    integer(kind = 4)       :: iFlagTypeData_Forcing_Gridded, iFlagTypeData_Forcing_Point, iFlagTypeData_Forcing_TimeSeries
    integer(kind = 4)       :: iFlagTypeData_Updating_Gridded
    integer(kind = 4)       :: iFlagTypeData_Output_Gridded, iFlagTypeData_Output_Point, iFlagTypeData_Output_TimeSeries
    integer(kind = 4)       :: iFlagTypeData_State_Gridded, iFlagTypeData_State_Point
    integer(kind = 4)       :: iFlagTypeData_Restart_Gridded, iFlagTypeData_Restart_Point
    integer(kind = 4)       :: iFlagRestart, iFlagFlowDeep
    integer(kind = 4)       :: iFlagVarDtPhysConv
    integer(kind = 4)       :: iFlagReleaseMass
    integer(kind = 4)       :: iFlagLAI, iFlagAlbedo, iFlagCH
    integer(kind = 4)       :: iFlagSnow, iFlagSnowAssim, iFlagSMAssim
    integer(kind = 4)       :: iFlagCoeffRes, iFlagWS
    integer(kind = 4)       :: iFlagGrid
    integer(kind = 4)       :: iFlagCType
    integer(kind = 4)       :: iFlagFrac
    integer(kind = 4)       :: iFlagDynVeg
    integer(kind = 4)       :: iFlagFlood

    logical                 :: bGridCheck

    integer(kind = 4)       :: iSimLength, iDtModel 
    integer(kind = 4)       :: iDtData_Forcing
    integer(kind = 4)       :: iDtData_Updating
    integer(kind = 4)       :: iDtData_Output_Gridded, iDtData_Output_Point
    integer(kind = 4)       :: iDtData_State_Gridded, iDtData_State_Point 
    
    integer(kind = 4)       :: iDtPhysConv, iDtPhysConvPrevious
    integer(kind = 4)       :: iDtPhysMethod

    integer(kind = 4)       :: iScaleFactor, iTcMax, iTc, iTVeg
    
    integer(kind = 4)       :: iRowsL, iColsL
    real(kind = 4)          :: dXLLCornerL, dYLLCornerL, dXCellSizeL, dYCellSizeL, dNoDataL
    
    integer(kind = 4)       :: iRowsF, iColsF 
    real(kind = 4)          :: dXLLCornerF, dYLLCornerF, dXCellSizeF, dYCellSizeF, dNoDataF
    
    integer(kind = 4)       :: iNSection
    integer(kind = 4)       :: iNLake, iNDam, iNPlant, iNJoint, iNCatch, iNRelease
    integer(kind = 4)       :: iDaySteps, iTMarkedSteps
    
    integer(kind = 4)       :: iGlacierValue
    real(kind = 4)          :: dRhoSnowMax, dRhoSnowFresh, dSnowQualityThr
    real(kind = 4)          :: dMeltingTRef
    
    integer(kind = 4)       :: iTdeepShift, iNTime, iEtime
    integer(kind = 4)       :: iNData
    real(kind = 4)          :: dRateMin, dBc
    real(kind = 4)          :: dTRef, dEpsS, dSigma, dBFMin, dBFMax
    real(kind = 4)          :: dZRef, dG, dCp, dRd, dRhoS, dRhoW, dCpS, dCpW
    real(kind = 4)          :: dKq, dKw, dKo, dPorS, dFqS 
    
    real(kind = 4)          :: dWTableHMin, dWTableHUSoil, dWTableHUChannel, dWTableSlopeBM, dWTableHOBedRock
    
    real(kind = 4)          :: dTV, dDamSpillH
    real(kind = 4)          :: dSMGain
    
    character(len = 19)     :: sTimeStart
    character(len = 19)     :: sTimeRestart
    !character(len = 19)     :: sTimeStatus
    
    integer(kind = 4), allocatable,         dimension(:) :: a1iDimsForcing
    real(kind = 4), allocatable,            dimension(:) :: a1dGeoForcing
    real(kind = 4), allocatable,            dimension(:) :: a1dResForcing
    
    real(kind = 4), allocatable,            dimension(:) :: a1dArctUp
    real(kind = 4), allocatable,            dimension(:) :: a1dExpRhoLow
    real(kind = 4), allocatable,            dimension(:) :: a1dExpRhoHigh
    real(kind = 4), allocatable,            dimension(:) :: a1dAltRange
                                        
    real(kind = 4), allocatable,            dimension(:) :: a1dAlbedoMonthly
    real(kind = 4), allocatable,            dimension(:) :: a1dLAIMonthly
    real(kind = 4), allocatable,            dimension(:) :: a1dCHMonthly

    real(kind = 4), allocatable,            dimension(:) :: a1dDemStep
    real(kind = 4), allocatable,            dimension(:) :: a1dIntStep
    real(kind = 4), allocatable,            dimension(:) :: a1dDtStep
    real(kind = 4), allocatable,            dimension(:) :: a1dDtRatioStep
    
    character(len = 256)    :: sPathData_Static_Gridded 
    character(len = 256)    :: sPathData_Static_Point
    character(len = 256)    :: sPathData_Forcing_Gridded
    character(len = 256)    :: sPathData_Forcing_Point
    character(len = 256)    :: sPathData_Forcing_TimeSeries
    character(len = 256)    :: sPathData_Updating_Gridded
    character(len = 256)    :: sPathData_Output_Gridded
    character(len = 256)    :: sPathData_Output_Point
    character(len = 256)    :: sPathData_Output_TimeSeries
    character(len = 256)    :: sPathData_State_Gridded
    character(len = 256)    :: sPathData_State_Point 
    character(len = 256)    :: sPathData_Restart_Gridded 
    character(len = 256)    :: sPathData_Restart_Point 
        
    character(len = 1)      :: sPathBar
    character(len = 700)    :: sCommandUnzipFile
    character(len = 700)    :: sCommandZipFile
    character(len = 700)    :: sCommandRemoveFile
    character(len = 700)    :: sCommandCreateFolder
    
    character(len = 10)     :: sReleaseDate
    character(len = 700)    :: sAuthorNames
    character(len = 5)      :: sReleaseVersion

    real(kind = 4)          :: dUc, dUh, dCt, dCf, dCPI, dWTableHbr, dKSatRatio, dSlopeMax
    character(len = 256)    :: sDomainName
   
END TYPE HMC_Type_Namelist

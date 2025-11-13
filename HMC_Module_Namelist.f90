!--------------------------------------------------------------------------------  
! File:   HMC_Module_Namelist.f90
! Author(s): Fabio Delogu, Francesco Silvestro, Simone Gabellani
! Created on May, 20 2014, 9:57 AM
!
! Module to allocate and read namelist
!--------------------------------------------------------------------------------

!--------------------------------------------------------------------------------
! Module Header
module HMC_Module_Namelist
    
    !--------------------------------------------------------------------------------
    ! External module(s) and implicit none
    use HMC_Module_Tools_Debug          ! to import global variable(s) declaration

    implicit none
    
    include "HMC_Type_Namelist.inc"
    integer, parameter :: iMaxDomain = 1
    type(HMC_Type_Namelist), dimension(iMaxDomain) :: oHMC_Namelist
    save oHMC_Namelist
    !--------------------------------------------------------------------------------
    
contains 
    
    !--------------------------------------------------------------------------------
    ! Subroutine to read namelist
    subroutine HMC_Namelist_Read(dUc, dUh, dCt, dCf, dCPI, dWTableHbr, dKSatRatio, dSlopeMax, &
                                 dCN, dFrac, dWS, dWDL, &
                                 sDomainName, & 
                                 sFileInfo, iArgsType, &
                                 oHMC_Namelist_Init) 
        
        !--------------------------------------------------------------------------------
        ! External module(s) and implicit none
        implicit none

        type(HMC_Type_Namelist) oHMC_Namelist_Init
        !--------------------------------------------------------------------------------
        
        !--------------------------------------------------------------------------------
        ! Variable(s) declaration    
        integer(kind = 4)       :: iErr
        
        integer(kind = 4)       :: iArgsType
        character(len = 256)    :: sFileInfo
        logical                 :: bFileExist
        
        integer(kind = 4)       :: iFlagDebugSet, iFlagDebugLevel, iDebugUnitInit
        
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
        integer(kind = 4)       :: iFlagGrid
        integer(kind = 4)       :: iFlagCoeffRes
        integer(kind = 4)       :: iFlagWS, iFlagWDL
        integer(kind = 4)       :: iFlagCType
        integer(kind = 4)       :: iFlagFrac
        integer(kind = 4)       :: iFlagDynVeg
        integer(kind = 4)       :: iFlagFlood
        integer(kind = 4)       :: iFlagEnergyBalance
        integer(kind = 4)       :: iFlagSoilParamsType
        integer(kind = 4)       :: iFlagInfiltRateVariable
        integer(kind = 4)       :: iFlagBetaET
        
        logical                 :: bGridCheck

        integer(kind = 4)       :: iSimLength, iDtModel, iDtPhysConv
        integer(kind = 4)       :: iDtData_Forcing
        integer(kind = 4)       :: iDtData_Updating
        integer(kind = 4)       :: iDtData_Output_Gridded, iDtData_Output_Point
        integer(kind = 4)       :: iDtData_State_Gridded, iDtData_State_Point
        
        integer(kind = 4)       :: iActiveData_Output_Generic, iActiveData_Output_Flooding, iActiveData_Output_Snow
        integer(kind = 4)       :: iAccumData_Output_Hour
        
        integer(kind = 4)       :: iDtPhysPrev
        integer(kind = 4)       :: iDtPhysMethod
        
        integer(kind = 4)       :: iScaleFactor, iTcMax, iTc, iTVeg

        integer(kind = 4)       :: iRowsL, iColsL
        real(kind = 4)          :: dXLLCornerL, dYLLCornerL, dXCellSizeL, dYCellSizeL, dNoDataL
        
        integer(kind = 4)       :: iRowsF, iColsF
        real(kind = 4)          :: dXLLCornerF, dYLLCornerF, dXCellSizeF, dYCellSizeF, dNoDataF
        
        integer(kind = 4)       :: iNSection
        integer(kind = 4)       :: iNLake, iNDam, iNPlant, iNJoint, iNCatch, iNRelease
        integer(kind = 4)       :: iDaySteps, iTMarkedSteps
        
        integer(kind = 4)       :: iTdeepShift, iNTime, iETime
        integer(kind = 4)       :: iNData
        real(kind = 4)          :: dWTableHMin, dWTableHUSoil, dWTableHUChannel, dWTableSlopeBM, dWTableHOBedRock
        real(kind = 4)          :: dRateMin, dBc, dWTLossMax
        real(kind = 4)          :: dRateRescaling
        real(kind = 4)          :: dPowVarInfiltRate
        real(kind = 4)          :: dTRef, dEpsS, dSigma, dBFMin, dBFMax
        real(kind = 4)          :: dZRef, dG, dCp, dRd, dRhoS, dRhoW, dCpS, dCpW
        real(kind = 4)          :: dKq, dKw, dKo, dPorS, dFqS 
        real(kind = 4)          :: dLSTDeltaMax
        
        real(kind = 4)          :: dTV, dDamSpillH
        real(kind = 4)          :: dSMGain
        
        integer(kind = 4)       :: iGlacierValue
        real(kind = 4)          :: dRhoSnowMax, dRhoSnowFresh, dSnowQualityThr
        real(kind = 4)          :: dMeltingTRef
        
        integer(kind = 4), target, dimension(2)     :: a1iDimsForcing
        real(kind = 4), target, dimension(2)        :: a1dGeoForcing
        real(kind = 4), target, dimension(2)        :: a1dResForcing
        
        real(kind = 4), target, dimension(4)        :: a1dArctUp
        real(kind = 4), target, dimension(4)        :: a1dExpRhoLow
        real(kind = 4), target, dimension(4)        :: a1dExpRhoHigh
        real(kind = 4), target, dimension(4)        :: a1dAltRange
                                        
        real(kind = 4), target, dimension(12)       :: a1dAlbedoMonthly
        real(kind = 4), target, dimension(12)       :: a1dLAIMonthly
        real(kind = 4), target, dimension(12)       :: a1dCHMonthly
        
        real(kind = 4), target, dimension(4)        :: a1dDemStep
        real(kind = 4), target, dimension(4)        :: a1dIntStep
        real(kind = 4), target, dimension(4)        :: a1dDtStep
        real(kind = 4), target, dimension(4)        :: a1dDtRatioStep
        
        character(len = 12)     :: sTimeStart
        character(len = 12)     :: sTimeRestart

        character(len = 19)     :: sTimeStartLong
        character(len = 19)     :: sTimeRestartLong
 
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
        real(kind = 4)          :: dCN, dWS, dWDL, dFrac
        real(kind = 4)          :: dSoil_ksat_infilt, dSoil_vmax, dSoil_ksat_drain !nuova definizione parametri per suolo giulia
        real(kind = 4)          :: dWTable_ksath !nuova definizione parametri per falda giulia
        real(kind = 4)          :: dWTable_init !inizializzazione falda se WTmax raster
        character(len = 256)    :: sDomainName
        
        character(len = 256)    :: sStrCf, sStrCt, sStrUh, sStrUc
        character(len = 256)    :: sStrVmax, sStrKsat_infilt, sStrKsat_drain, sStrKsat_wtable
        !--------------------------------------------------------------------------------

        !--------------------------------------------------------------------------------
        ! Read namelist(s)
        namelist /HMC_Parameters/       dUc, dUh, dCt, dCf, dCPI, dWTableHbr, dKSatRatio, dSlopeMax, &
                                        dCN, dWS, dWDL, dFrac, &
                                        dSoil_ksat_infilt, dSoil_vmax, dSoil_ksat_drain, & 
                                        dWTable_ksath, dWTable_init, &
                                        sDomainName
        
        namelist /HMC_Namelist/         iFlagTypeData_Static, &
                                        iFlagTypeData_Forcing_Gridded, iFlagTypeData_Forcing_Point, & 
                                        iFlagTypeData_Forcing_TimeSeries, &
                                        iFlagTypeData_Updating_Gridded, &
                                        iFlagTypeData_Output_Gridded, iFlagTypeData_Output_Point, & 
                                        iFlagTypeData_Output_TimeSeries, &
                                        iFlagTypeData_State_Gridded, iFlagTypeData_State_Point, &
                                        iFlagTypeData_Restart_Gridded, iFlagTypeData_Restart_Point, &
                                        iFlagDebugSet, iFlagDebugLevel, &
                                        iFlagOs, iFlagFlowDeep, iFlagRestart, &
                                        iFlagVarDtPhysConv, &
                                        iFlagReleaseMass, &
                                        iFlagLAI, iFlagAlbedo, iFlagCH, &
                                        iFlagSnow, iFlagSnowAssim, iFlagSMAssim, &
                                        iFlagCoeffRes, iFlagWS, iFlagWDL, &
                                        iFlagCType, &
                                        iFlagFrac, &
                                        iFlagDynVeg, &
                                        iFlagFlood, iFlagEnergyBalance, & 
                                        iFlagSoilParamsType, &
                                        iFlagInfiltRateVariable, &
                                        iFlagBetaET, &
                                        a1dGeoForcing, a1dResForcing, a1iDimsForcing, &
                                        iScaleFactor, iTcMax, iTVeg, &
                                        iSimLength, iDtModel, &
                                        iDtPhysMethod, iDtPhysConv, &
                                        a1dDemStep, a1dIntStep, a1dDtStep, a1dDtRatioStep, &
                                        iDtData_Forcing, &
                                        iDtData_Updating, &
                                        iDtData_Output_Gridded, iDtData_Output_Point, &
                                        iDtData_State_Gridded, iDtData_State_Point, &
                                        iActiveData_Output_Generic, iActiveData_Output_Flooding, iActiveData_Output_Snow, &
                                        iAccumData_Output_Hour, &
                                        sTimeStart, sTimeRestart, &
                                        sPathData_Static_Gridded, sPathData_Static_Point, &
                                        sPathData_Forcing_Gridded, sPathData_Forcing_Point, sPathData_Forcing_TimeSeries, &
                                        sPathData_Updating_Gridded, &
                                        sPathData_Output_Gridded, sPathData_Output_Point, sPathData_Output_TimeSeries, &
                                        sPathData_State_Gridded, sPathData_State_Point, &
                                        sPathData_Restart_Gridded, sPathData_Restart_Point
                                        
        namelist /HMC_Snow/             a1dArctUp, a1dExpRhoLow, a1dExpRhoHigh, a1dAltRange, &
                                        iGlacierValue, dRhoSnowFresh, dRhoSnowMax, dSnowQualityThr, &
                                        dMeltingTRef
                                        
        namelist /HMC_Constants/        a1dAlbedoMonthly, a1dLAIMonthly, a1dCHMonthly, &
                                        dWTableHMin, dWTableHUSoil, dWTableHUChannel, dWTableSlopeBM, dWTableHOBedRock, &
                                        dRateMin, dBc, dWTLossMax, &
                                        dRateRescaling, &
                                        dPowVarInfiltRate, & 
                                        dTRef, iTdeepShift, dEpsS, dSigma, dBFMin, dBFMax, &
                                        dZRef, dG, dCp, dRd, dRhoS, dRhoW, dCpS, dCpW, & 
                                        dKq, dKw, dKo, dPorS, dFqS, &
                                        dLSTDeltaMax, &
                                        dTV, dDamSpillH, &
                                        dSMGain
                                        
        namelist /HMC_Command/          sCommandZipFile, &
                                        sCommandUnzipFile, &
                                        sCommandRemoveFile, &
                                        sCommandCreateFolder
        
        namelist /HMC_Info/             sReleaseDate, &
                                        sAuthorNames, &
                                        sReleaseVersion
        !--------------------------------------------------------------------------------
        
        !--------------------------------------------------------------------------------
        ! Set defaults for namelist variables
        iFlagTypeData_Static = -9999; 
        iFlagTypeData_Forcing_Gridded = -9999; iFlagTypeData_Forcing_Point = -9999; iFlagTypeData_Forcing_TimeSeries = -9999;
        iFlagTypeData_Updating_Gridded = -9999; 
        iFlagTypeData_Output_Gridded = -9999; iFlagTypeData_Output_Point = -9999; iFlagTypeData_Output_TimeSeries = -9999;
        iFlagTypeData_State_Gridded = -9999; iFlagTypeData_State_Point = -9999; 
        iFlagTypeData_Restart_Gridded = -9999; iFlagTypeData_Restart_Point = -9999; 
        iFlagDebugSet = -9999; iFlagDebugLevel = -9999;
        iFlagOs = -9999; iFlagFlowDeep = -9999; iFlagRestart = -9999;
        iFlagVarDtPhysConv = -9999; iFlagReleaseMass = -9999; 
        iFlagLAI = -9999; iFlagAlbedo = -9999; iFlagCH = -9999; 
        iFlagSnow = -9999; iFlagSnowAssim = -9999; iFlagSMAssim = -9999;
        iFlagCoeffRes = -9999; iFlagWS = -9999; iFlagWDL = -9999;
        iFlagCType = -9999;
        iFlagFrac = -9999;
        iFlagDynVeg = -9999;
        iFlagFlood = -9999; iFlagEnergyBalance = -9999;
        iFlagSoilParamsType = -9999;
        iFlagInfiltRateVariable = -9999;
        iFlagBetaET = 1;
        a1dGeoForcing = -9999.0; a1dResForcing = -9999.0; a1iDimsForcing = -9999; 
        iScaleFactor = -9999; iTcMax = -9999; iTVeg = -9999; iTc = -9999; 
        iSimLength = -9999; iDtModel = -9999; 
        iDtPhysMethod = -9999; iDtPhysConv = -9999; 
        a1dDemStep = -9999.0; a1dIntStep = -9999.0; a1dDtStep = -9999.0; a1dDtRatioStep = -9999.0;
        iDtData_Forcing = -9999; 
        iDtData_Updating = -9999; 
        iDtData_Output_Gridded = -9999; iDtData_Output_Point = -9999; 
        iDtData_State_Gridded = -9999; iDtData_State_Point = -9999;
        
        iActiveData_Output_Generic = -1; iActiveData_Output_Flooding = -1; iActiveData_Output_Snow = -1; 
        iAccumData_Output_Hour = -1
        
        sTimeStart = ""; sTimeRestart = ""; 
        sPathData_Static_Gridded = ""; sPathData_Static_Point = ""; 
        sPathData_Forcing_Gridded = ""; sPathData_Forcing_Point = ""; sPathData_Forcing_TimeSeries = "";
        sPathData_Updating_Gridded = "";
        sPathData_Output_Gridded = ""; sPathData_Output_Gridded = ""; sPathData_Output_TimeSeries = "";
        sPathData_State_Gridded = ""; sPathData_State_Gridded = ""; 
        sPathData_Restart_Gridded = ""; sPathData_Restart_Point = ""; 
        
        iNTime = 0; iNData = 0; iETime = 0;
        
        a1dArctUp = -9999.0; a1dExpRhoLow = -9999.0; a1dExpRhoHigh = -9999.0; a1dAltRange = -9999.0;
        iGlacierValue = -9999; dRhoSnowFresh = -9999.0; dRhoSnowMax = -9999.0; dSnowQualityThr = -9999.0; 
        dMeltingTRef = -9999.0;
        
        a1dAlbedoMonthly = -9999.0; a1dLAIMonthly = -9999.0; a1dCHMonthly = -9999.0
        dWTableHMin = -9999.0; dWTableHUSoil = -9999.0; dWTableHUChannel = -9999.0; 
        dWTableSlopeBM = -9999.0; dWTableHOBedRock = -9999.0;
        dRateMin = -9999.0; dBc = -9999.0; dWTLossMax = -9999.0;
        dRateRescaling = -9999.0;
        dPowVarInfiltRate = -9999.0;
        dTRef = -9999.0; iTdeepShift = -9999; dEpsS = -9999.0; 
        dSigma = -9999.0; dBFMin = -9999.0; dBFMax = -9999.0
        dZRef = -9999.0; dG = -9999.0; dCp = -9999.0; dRd = -9999.0; dRhoS = -9999.0; 
        dRhoW = -9999.0; dCpS = -9999.0; dCpW = -9999.0; 
        dKq = -9999.0; dKw = -9999.0; dKo = -9999.0; dPorS = -9999.0; dFqS = -9999.0;
        dLSTDeltaMax = -9999.0
        dTV = -9999.0; dDamSpillH = -9999.0
        dSMGain = -9999.0;
        
        sCommandZipFile = ""; sCommandUnzipFile = ""; sCommandRemoveFile = ""; sCommandCreateFolder = ""
        
        sReleaseDate = ""; sAuthorNames = ""; sReleaseVersion = "";
        
        dSoil_ksat_infilt = -9999.0; dSoil_vmax = -9999.0; dSoil_ksat_drain = -9999.0;
        dWTable_ksath = -9999.0; dWTable_init = -9999.0;
        !--------------------------------------------------------------------------------  
        
        !--------------------------------------------------------------------------------
        ! Checking information file availability
        !sFileName = trim(sDomainName)//'.info.txt' 
        inquire (file = trim(sFileInfo), exist = bFileExist)
        if ( .not. bFileExist ) then
            write(6, *) "No file info found ", trim(sFileInfo)
            stop "Stopped"
        else
            ! Read namelist section(s)
            open(20,file = trim(sFileInfo))
            read(20, HMC_Namelist, iostat=iErr); rewind(20)
            read(20, HMC_Snow, iostat=iErr); rewind(20)
            read(20, HMC_Constants, iostat=iErr); rewind(20)
            read(20, HMC_Command, iostat=iErr); rewind(20)
            read(20, HMC_Info, iostat=iErr); rewind(20)
            
            ! According to line argument(s) definition
            if (iArgsType == 2) then
                read(20, HMC_Parameters, iostat=iErr); rewind(20)
            endif
            
            close(20) 
        endif
        !--------------------------------------------------------------------------------
        
        !--------------------------------------------------------------------------------
        ! Set debug level
        call HMC_Tools_Debug_SetLevel(iFlagDebugSet, iFlagDebugLevel)
        ! Set file unit debug
        call HMC_Tools_Debug_SetUnit(80, 100, iDebugUnitInit)
        !--------------------------------------------------------------------------------

        !-----------------------------------------------------------------------------------
        ! HMC Version
        call mprintf(.true., iINFO_Basic, '************************************************************************')
        call mprintf(.true., iINFO_Basic, ' Hydrological Model Continuum (Version: '//trim(sReleaseVersion)//')')
        call mprintf(.true., iINFO_Basic, ' Author(s): '//trim(sAuthorNames))
        call mprintf(.true., iINFO_Basic, ' Release Date: '//trim(sReleaseDate))
        call mprintf(.true., iINFO_Basic, '************************************************************************')
        !-----------------------------------------------------------------------------------        
        
        !--------------------------------------------------------------------------------
        ! Time long definition
        sTimeStartLong = sTimeStart(1:4)//"-"//sTimeStart(5:6)//"-"//sTimeStart(7:8)//"_" &
                         //sTimeStart(9:10)//":"//sTimeStart(11:12)//":"//"00"
        
        sTimeRestartLong = sTimeRestart(1:4)//"-"//sTimeRestart(5:6)//"-"//sTimeRestart(7:8)//"_" &
                           //sTimeRestart(9:10)//":"//sTimeRestart(11:12)//":"//"00"
        !--------------------------------------------------------------------------------
                                   
        !--------------------------------------------------------------------------------
        ! Define OS features
        if (iFlagOS.eq.10) then
            call mprintf(.true., iINFO_Main, ' Operative System: GNU/Linux')
            sPathBar = '/'
        elseif (iFlagOS.eq.0) then
            call mprintf(.true., iINFO_Main, ' Operative System: WinOS')
            sPathBar = '\'
        else
            call mprintf(.true., iERROR, ' Incorrect OS definition (Allowed values: GNU/Linux = 10, WinOS = 0)')
        endif
        !--------------------------------------------------------------------------------               
        
        !------------------------------------------------------------------------------------------
        ! Compute simulation time length
        iNTime = (iSimLength)*3600./nint(real(iDtModel))
        ! Compute data time steps
        iNData = int((iNTime*3600))/int(iDtData_Forcing)
        !------------------------------------------------------------------------------------------
        
        !--------------------------------------------------------------------------------  
        ! Check snow physics flag(s)
        if (iFlagSnow.eq.0) iFlagSnowAssim = 0;
        ! Check slope value under a minimum value
        if (dSlopeMax.lt.0.01) dSlopeMax = 0.01 ! Default value
        !--------------------------------------------------------------------------------  
        
        !--------------------------------------------------------------------------------
        ! Namelist definition
        call mprintf(.true., iINFO_Main, ' Read Namelist ... ')
        
        ! Model dims (initialization)
        oHMC_Namelist_Init%iRowsL = 0
        oHMC_Namelist_Init%iColsL = 0
        oHMC_Namelist_Init%iRowsF = 0
        oHMC_Namelist_Init%iColsF = 0
        
        oHMC_Namelist_Init%iNSection = 0
        
        oHMC_Namelist_Init%iNLake = 0
        oHMC_Namelist_Init%iNDam = 0
        oHMC_Namelist_Init%iNPlant = 0
        oHMC_Namelist_Init%iNJoint = 0
        oHMC_Namelist_Init%iNCatch = 0
        oHMC_Namelist_Init%iNRelease = 0
        oHMC_Namelist_Init%iDaySteps = 0
        oHMC_Namelist_Init%iTMarkedSteps = 0
        
        ! Flag(s) info
        oHMC_Namelist_Init%iFlagTypeData_Static = iFlagTypeData_Static
        oHMC_Namelist_Init%iFlagTypeData_Forcing_Gridded = iFlagTypeData_Forcing_Gridded
        oHMC_Namelist_Init%iFlagTypeData_Forcing_Point = iFlagTypeData_Forcing_Point
        if (iFlagTypeData_Forcing_TimeSeries .eq. -9999) then   ! backward compatibility with older version of info file (without forcing time-series)
            oHMC_Namelist_Init%iFlagTypeData_Forcing_TimeSeries = iFlagTypeData_Forcing_Point
        else
            oHMC_Namelist_Init%iFlagTypeData_Forcing_TimeSeries = iFlagTypeData_Forcing_TimeSeries
        endif
        
        if (iFlagTypeData_Updating_Gridded .eq. -9999) then      ! backward compatibility with older version of info file (without updating gridded)
            oHMC_Namelist_Init%iFlagTypeData_Updating_Gridded = iFlagTypeData_Forcing_Gridded
        else
            oHMC_Namelist_Init%iFlagTypeData_Updating_Gridded = iFlagTypeData_Updating_Gridded
        endif
        
        oHMC_Namelist_Init%iFlagTypeData_Output_Gridded = iFlagTypeData_Output_Gridded
        oHMC_Namelist_Init%iFlagTypeData_Output_Point = iFlagTypeData_Output_Point
        if (iFlagTypeData_Output_TimeSeries .eq. -9999) then    ! backward compatibility with older version of info file (without output time-series)
            oHMC_Namelist_Init%iFlagTypeData_Output_TimeSeries = iFlagTypeData_Output_Point
        else
            oHMC_Namelist_Init%iFlagTypeData_Output_TimeSeries = iFlagTypeData_Output_TimeSeries
        endif
        oHMC_Namelist_Init%iFlagTypeData_State_Gridded = iFlagTypeData_State_Gridded
        oHMC_Namelist_Init%iFlagTypeData_State_Point = iFlagTypeData_State_Point
        oHMC_Namelist_Init%iFlagTypeData_Restart_Gridded = iFlagTypeData_Restart_Gridded
        oHMC_Namelist_Init%iFlagTypeData_Restart_Point = iFlagTypeData_Restart_Point
        
        oHMC_Namelist_Init%iFlagDebugLevel = iFlagDebugLevel
        oHMC_Namelist_Init%iFlagOs = iFlagOs
        oHMC_Namelist_Init%iFlagFlowDeep = iFlagFlowDeep
        oHMC_Namelist_Init%iFlagRestart = iFlagRestart
        oHMC_Namelist_Init%iFlagVarDtPhysConv = iFlagVarDtPhysConv
        
        if (iFlagReleaseMass .eq. -9999) then  ! backward compatibility with older version of info file (without release mass flag)
            oHMC_Namelist_Init%iFlagReleaseMass = 1     ! default mode with update release(s) mass balance
        else
            oHMC_Namelist_Init%iFlagReleaseMass = iFlagReleaseMass
        endif

        oHMC_Namelist_Init%iFlagLAI = iFlagLAI
        oHMC_Namelist_Init%iFlagAlbedo = iFlagAlbedo
        oHMC_Namelist_Init%iFlagCH = iFlagCH
        oHMC_Namelist_Init%iFlagSnow = iFlagSnow
        oHMC_Namelist_Init%iFlagSnowAssim = iFlagSnowAssim
        if (iFlagSMAssim .eq. -9999) then  ! backward compatibility with older version of info file (without sm assimilation flag)
            oHMC_Namelist_Init%iFlagSMAssim = 0               ! default mode without soil moisture assimilation
        else
            oHMC_Namelist_Init%iFlagSMAssim = iFlagSMAssim
        endif
        oHMC_Namelist_Init%iFlagGrid = -9999
        
        if (iFlagCoeffRes .eq. -9999) then  ! backward compatibility with older version of info file (without coeff resolution flag)
            oHMC_Namelist_Init%iFlagCoeffRes = 1                ! default mode with empiric relationship
        else
            oHMC_Namelist_Init%iFlagCoeffRes = iFlagCoeffRes    ! to avoid coeff resolution map set flag to zero (vda old cases)
        endif
        
        if (iFlagWS .eq. -9999) then  ! backward compatibility with older version of info file (without ws flag)
            oHMC_Namelist_Init%iFlagWS = 0          ! default mode without watertable sources dynamic
        else
            oHMC_Namelist_Init%iFlagWS = iFlagWS 
        endif
        
        if (iFlagWDL .eq. -9999) then  ! backward compatibility with older version of info file (without wdl flag)
            oHMC_Namelist_Init%iFlagWDL = 0          ! default mode without watertable deep losses dynamic
        else
            oHMC_Namelist_Init%iFlagWDL = iFlagWDL 
        endif
        
        if (iFlagFrac .eq. -9999) then  ! backward compatibility with older version of info file (without frac flag)
            oHMC_Namelist_Init%iFlagFrac = 0              ! deactivate groundwater bedrock fracturation
        else
            oHMC_Namelist_Init%iFlagFrac = iFlagFrac      ! activate/deactivate groundwater bedrock fracturation
        endif
        
        if (iFlagCType .eq. -9999) then  ! backward compatibility with older version of info file (without ctype flag)
            oHMC_Namelist_Init%iFlagCType = 1               ! channel network
        else
            oHMC_Namelist_Init%iFlagCType = iFlagCType      ! channel network/fraction
        endif

        if (iFlagDynVeg .eq. -9999) then  ! backward compatibility with older version of info file (without Vegtype flag)
            oHMC_Namelist_Init%iFlagDynVeg = 0               ! dynamic vegetation module not activated
        else
            oHMC_Namelist_Init%iFlagDynVeg = iFlagDynVeg     ! dynamic vegetation module
        endif

        if (iFlagFlood .eq. -9999) then  ! backward compatibility with older version of info file (without Flooding flag)
            oHMC_Namelist_Init%iFlagFlood = 0               ! flooding module not activated
        else
            oHMC_Namelist_Init%iFlagFlood = iFlagFlood      ! flooding vegetation module
        endif
        
        if (iFlagEnergyBalance .eq. -9999) then  ! backward compatibility with older version of info file (without EnergyBalance flag)
            oHMC_Namelist_Init%iFlagEnergyBalance = 1               ! energy-balance module activated
        else
            oHMC_Namelist_Init%iFlagEnergyBalance = iFlagEnergyBalance      
        endif
        
        if (iFlagSoilParamsType .eq. -9999) then  ! backward compatibility with older version of info file (without soil params type flag)
            oHMC_Namelist_Init%iFlagSoilParamsType = 1 ! default is classical parametrization (through CN)
            call mprintf(.true., iWARN, ' Soil parametrization type is NOT set explicitly - ' // &
                                        'The classical approach through CN will be used ')
            iFlagInfiltRateVariable = -9999.0
        else
            oHMC_Namelist_Init%iFlagSoilParamsType = iFlagSoilParamsType      
            if (iFlagSoilParamsType .eq. 1) then
                call mprintf(.true., iINFO_Basic, ' Classical soil parametrization is selected (iFlagSoilParamsType=1) ' // &
                                            '--> CN will be used')
                call mprintf(.true., iINFO_Basic, ' IMPORTANT - when classical soil parametrization is selected, ' // &
                                            'iFlagInfiltRateVariable is not active')    
                iFlagInfiltRateVariable = -9999.0
            else if (iFlagSoilParamsType .eq. 2) then
                call mprintf(.true., iINFO_Basic, ' Alternative soil parametrization is selected (iFlagSoilParamsType=2) ' // &
                                            '--> vmax, ksat_infilt, ksat_drain will be used')
            endif
        endif
        
        ! Soil infiltration capacity variability with soil saturation degree
        if (iFlagInfiltRateVariable .eq. 0) then
                call mprintf(.true., iINFO_Basic, ' Soil infiltration capacity is not set to change with soil saturation ' // &
                                            '(iFlagInfiltRateVariable=0)')
        elseif (iFlagInfiltRateVariable .eq. 1) then
                call mprintf(.true., iINFO_Basic, ' Soil infiltration capacity is set to change linearly with soil ' // &
                                            'saturation (iFlagInfiltRateVariable=1)')
        elseif (iFlagInfiltRateVariable .eq. 2) then
                call mprintf(.true., iINFO_Basic, ' Soil infiltration capacity is set to change NON-linearly with soil ' // &
                                            'saturation (iFlagInfiltRateVariable=2) ' // &
                                            '(2020 - Yang et al. - Improving the Horton infiltration equation ...)')
                if (dPowVarInfiltRate .eq. -9999) then
                    call mprintf(.true., iERROR,'dPowVarInfiltRate must be defined if iFlagInfiltRateVariable=2. ' // &
                                        ' (Only positive values are appropriate) Program stopped!')                
                endif
        endif
            
        oHMC_Namelist_Init%dPowVarInfiltRate = dPowVarInfiltRate
        oHMC_Namelist_Init%iFlagInfiltRateVariable = iFlagInfiltRateVariable !remains -9999 when not defined (not used for classical soil)
        
        if (iFlagBetaET .eq. 0) then
            call mprintf(.true., iINFO_Basic, ' IMPORTANT: Beta function for ET reduction with water stress is deactivated ' // &
                                            '(iFlagBetaET=0)')
        elseif (iFlagBetaET .eq. 2) then
            call mprintf(.true., iINFO_Basic, ' IMPORTANT: iFlagBetaET=2 is not official!!! Please use with caution! ' // &
                                              '(2=classical BF employed also for soil)')
        endif
        oHMC_Namelist_Init%iFlagBetaET = iFlagBetaET !if not defined in the namelist, remains 1 (implicit backward compatibility)
        
        
        ! Geographical land and forcing info
        oHMC_Namelist_Init%bGridCheck = .false.
        
        oHMC_Namelist_Init%dXLLCornerL = 0.0
        oHMC_Namelist_Init%dYLLCornerL = 0.0
        oHMC_Namelist_Init%dXCellSizeL = 0.0
        oHMC_Namelist_Init%dYCellSizeL = 0.0
        oHMC_Namelist_Init%dNoDataL = 0.0
        oHMC_Namelist_Init%dXLLCornerF = 0.0
        oHMC_Namelist_Init%dYLLCornerF = 0.0
        oHMC_Namelist_Init%dXCellSizeF = 0.0
        oHMC_Namelist_Init%dYCellSizeF = 0.0
        oHMC_Namelist_Init%dNoDataF = 0.0
        
        oHMC_Namelist_Init%a1dGeoForcing = a1dGeoForcing
        oHMC_Namelist_Init%a1dResForcing = a1dResForcing
        oHMC_Namelist_Init%a1iDimsForcing = a1iDimsForcing
        
        ! S3M parameter(s) and constant(s)
        oHMC_Namelist_Init%a1dArctUp = a1dArctUp
        oHMC_Namelist_Init%a1dExpRhoLow = a1dExpRhoLow
        oHMC_Namelist_Init%a1dExpRhoHigh = a1dExpRhoHigh
        oHMC_Namelist_Init%a1dAltRange = a1dAltRange
        oHMC_Namelist_Init%iGlacierValue = iGlacierValue 
        if (dRhoSnowFresh .eq. -9999) then                  ! backward compatibility with older version of info file (without fresh snow density reference)
            oHMC_Namelist_Init%dRhoSnowFresh = 100
        else
            oHMC_Namelist_Init%dRhoSnowFresh = dRhoSnowFresh
        endif   
        oHMC_Namelist_Init%dRhoSnowMax = dRhoSnowMax
        oHMC_Namelist_Init%dSnowQualityThr = dSnowQualityThr
        if (dMeltingTRef .eq. -9999) then                   ! backward compatibility with older version of info file (without melting t reference)
            oHMC_Namelist_Init%dMeltingTRef = 1
        else
            oHMC_Namelist_Init%dMeltingTRef = dMeltingTRef
        endif
  
        ! Time, dt and step(s) info
        oHMC_Namelist_Init%iNTime = iNTime
        oHMC_Namelist_Init%iETime = 0
        oHMC_Namelist_Init%iTcMax = iTcMax
        oHMC_Namelist_Init%iTc = 0
        oHMC_Namelist_Init%iSimLength = iSimLength
        
        if (iTVeg .le. 0) then                      
            oHMC_Namelist_Init%iTVeg = 1 !minimum duration (in model time step) of observed LAI
        else
            oHMC_Namelist_Init%iTVeg = iTVeg
        endif        
        
        oHMC_Namelist_Init%iDtModel = iDtModel
        oHMC_Namelist_Init%iDtPhysConv = iDtPhysConv
        oHMC_Namelist_Init%iDtPhysConvPrevious = iDtPhysConv
        
        if (iDtPhysMethod .eq. -9999) then                      ! backward compatibility with older version of info file (without integration step scalar or linear)
            oHMC_Namelist_Init%iDtPhysMethod = 1
        else
            oHMC_Namelist_Init%iDtPhysMethod = iDtPhysMethod
        endif
        
        if (all(a1dDemStep .eq. -9999.0)) then                 ! backward compatibility with older version of info file (without declaration of arrays to dynamically compute integration step)
            oHMC_Namelist_Init%a1dDemStep = (/ 1, 10, 100, 1000 /)
        else
            oHMC_Namelist_Init%a1dDemStep = a1dDemStep
        endif
        if (all(a1dIntStep .eq. -9999.0)) then                 ! backward compatibility with older version of info file (without declaration of arrays to dynamically compute integration step)
            oHMC_Namelist_Init%a1dIntStep = (/ 1, 5, 25, 600/)
        else
            oHMC_Namelist_Init%a1dIntStep = a1dIntStep
        endif
        if (all(a1dDtStep .eq. -9999.0)) then                  ! backward compatibility with older version of info file (without declaration of arrays to dynamically compute integration step) 
            oHMC_Namelist_Init%a1dDtStep = (/1, 6, 6, 60 /)
        else
            oHMC_Namelist_Init%a1dDtStep = a1dDtStep
        endif
        if (all(a1dDtRatioStep .eq. -9999.0)) then             ! backward compatibility with older version of info file (without declaration of arrays to dynamically compute integration step)
            oHMC_Namelist_Init%a1dDtRatioStep = (/3, 3, 3, 2/)
        else
            oHMC_Namelist_Init%a1dDtRatioStep = a1dDtRatioStep
        endif
        
        oHMC_Namelist_Init%iDtData_Forcing = iDtData_Forcing
        if (iDtData_Updating .eq. -9999) then                   ! backward compatibility with older version of info file (without declaratiom of data updating dt)
            oHMC_Namelist_Init%iDtData_Updating = iDtData_Forcing
        else
            oHMC_Namelist_Init%iDtData_Updating = iDtData_Updating
        endif
        oHMC_Namelist_Init%iDtData_Output_Gridded = iDtData_Output_Gridded 
        oHMC_Namelist_Init%iDtData_Output_Point = iDtData_Output_Point                            
        oHMC_Namelist_Init%iDtData_State_Gridded = iDtData_State_Gridded
        oHMC_Namelist_Init%iDtData_State_Point = iDtData_State_Point
        
        if (iActiveData_Output_Generic .eq. -1) then  
            oHMC_Namelist_Init%iActiveData_Output_Generic = 2
        else
            oHMC_Namelist_Init%iActiveData_Output_Generic = iActiveData_Output_Generic
        endif
        if (iActiveData_Output_Flooding .eq. -1) then 
            oHMC_Namelist_Init%iActiveData_Output_Flooding = 0
        else
            oHMC_Namelist_Init%iActiveData_Output_Flooding = iActiveData_Output_Flooding
        endif
        if (iActiveData_Output_Snow .eq. -1) then 
            oHMC_Namelist_Init%iActiveData_Output_Snow = 0
        else
            oHMC_Namelist_Init%iActiveData_Output_Snow = iActiveData_Output_Snow
        endif
        
        if (iAccumData_Output_Hour .eq. -1) then 
            oHMC_Namelist_Init%iAccumData_Output_Hour = 23
        else
            oHMC_Namelist_Init%iAccumData_Output_Hour = iAccumData_Output_Hour
        endif
        
        ! Time reference info
        oHMC_Namelist_Init%sTimeStart = sTimeStartLong
        oHMC_Namelist_Init%sTimeRestart = sTimeRestartLong
        
        ! Data info
        oHMC_Namelist_Init%iNData = iNData
        oHMC_Namelist_Init%iScaleFactor = iScaleFactor
        
        ! Path(s) info
        oHMC_Namelist_Init%sPathData_Static_Gridded = sPathData_Static_Gridded
        oHMC_Namelist_Init%sPathData_Static_Point = sPathData_Static_Point
        oHMC_Namelist_Init%sPathData_Forcing_Gridded = sPathData_Forcing_Gridded
        oHMC_Namelist_Init%sPathData_Forcing_Point = sPathData_Forcing_Point
        if (sPathData_Forcing_TimeSeries == "") then    ! backward compatibility with older version of info file (without forcing time-series)
            oHMC_Namelist_Init%sPathData_Forcing_TimeSeries = sPathData_Forcing_Point
        else
            oHMC_Namelist_Init%sPathData_Forcing_TimeSeries = sPathData_Forcing_TimeSeries
        endif
        
        if (sPathData_Updating_Gridded == "") then      ! backward compatibility with older version of info file (without updating gridded)
            oHMC_Namelist_Init%sPathData_Updating_Gridded = sPathData_Forcing_Gridded
        else
            oHMC_Namelist_Init%sPathData_Updating_Gridded = sPathData_Updating_Gridded
        endif
        
        oHMC_Namelist_Init%sPathData_Output_Gridded = sPathData_Output_Gridded
        oHMC_Namelist_Init%sPathData_Output_Point = sPathData_Output_Point
        if (sPathData_Output_TimeSeries == "") then     ! backward compatibility with older version of info file (without output time-series)
            oHMC_Namelist_Init%sPathData_Output_TimeSeries = sPathData_State_Point
        else
            oHMC_Namelist_Init%sPathData_Output_TimeSeries = sPathData_Output_TimeSeries
        endif
        oHMC_Namelist_Init%sPathData_State_Gridded = sPathData_State_Gridded
        oHMC_Namelist_Init%sPathData_State_Point = sPathData_State_Point
        oHMC_Namelist_Init%sPathData_Restart_Gridded = sPathData_Restart_Gridded
        oHMC_Namelist_Init%sPathData_Restart_Point = sPathData_Restart_Point
        
        ! Monthly value(s)
        oHMC_Namelist_Init%a1dAlbedoMonthly = a1dAlbedoMonthly
        oHMC_Namelist_Init%a1dLAIMonthly = a1dLAIMonthly
        oHMC_Namelist_Init%a1dCHMonthly = a1dCHMonthly
        
        ! Command line
        oHMC_Namelist_Init%sPathBar = sPathBar
        oHMC_Namelist_Init%sCommandZipFile = sCommandZipFile
        oHMC_Namelist_Init%sCommandUnzipFile = sCommandUnzipFile
        oHMC_Namelist_Init%sCommandRemoveFile = sCommandRemoveFile
        oHMC_Namelist_Init%sCommandCreateFolder = sCommandCreateFolder
        
        ! Command line argument(s) or namelist parameter(s)   
        oHMC_Namelist_Init%dUc = dUc
        oHMC_Namelist_Init%dUh = dUh
        oHMC_Namelist_Init%dCt = dCt
        oHMC_Namelist_Init%dCf = dCf
        oHMC_Namelist_Init%dCPI = dCPI
        oHMC_Namelist_Init%dWTableHbr = dWTableHbr
        oHMC_Namelist_Init%dKSatRatio = dKSatRatio
        oHMC_Namelist_Init%dSlopeMax = dSlopeMax
        oHMC_Namelist_Init%sDomainName = sDomainName
        
        if (dCN .eq. -9999) then  ! backward compatibility with older version of info file (without curve number value)
            oHMC_Namelist_Init%dCN = -9999.0               
        else
            oHMC_Namelist_Init%dCN = dCN
        endif
        
        if (dWS .eq. -9999) then  ! backward compatibility with older version of info file (without water sources value)
            oHMC_Namelist_Init%dWS = -9999.0               
        else
            oHMC_Namelist_Init%dWS = dWS
        endif
        
        if (dWDL .eq. -9999) then  ! backward compatibility with older version of info file (without water sources value)
            oHMC_Namelist_Init%dWDL = -9999.0               
        else
            oHMC_Namelist_Init%dWDL = dWDL
        endif
        
        if (dFrac .eq. -9999) then  ! backward compatibility with older version of info file (without fracturation value)
            oHMC_Namelist_Init%dFrac = -9999.0               
        else
            oHMC_Namelist_Init%dFrac = dFrac
        endif
        
        ! Water-table constant(s)
        oHMC_Namelist_Init%dWTableHMin = dWTableHMin
        oHMC_Namelist_Init%dWTableHUSoil = dWTableHUSoil
        oHMC_Namelist_Init%dWTableHUChannel = dWTableHUChannel
        oHMC_Namelist_Init%dWTableSlopeBM = dWTableSlopeBM
        oHMC_Namelist_Init%dWTableHOBedRock = dWTableHOBedRock
        
        ! New soil parameters
        oHMC_Namelist_Init%dSoil_ksat_infilt = dSoil_ksat_infilt
        oHMC_Namelist_Init%dSoil_vmax = dSoil_vmax
        oHMC_Namelist_Init%dSoil_ksat_drain = dSoil_ksat_drain
        
        ! Aquifer horizontal hydraulic conductivity
        oHMC_Namelist_Init%dWTable_ksath = dWTable_ksath
        ! Water table initial relative level
        oHMC_Namelist_Init%dWTable_init = dWTable_init 
        
        ! checking if dSoil_ksat_infilt & dSoil_vmax & dSoil_ksat_drain are defined in case of iFlagSoilParamsType=2  
        ! & also dWTable_ksath (no need for specific flag --> over-flexibility)
        if ( (iFlagSoilParamsType .eq. 2) .and. (dSoil_ksat_infilt .eq. -9999.0) ) then
                call mprintf(.true., iERROR,'dSoil_ksat_infilt must be defined if iFlagSoilParamsType=2. ' // &
                                        ' Program stopped!')   
        elseif ( (iFlagSoilParamsType .eq. 2) .and. (dSoil_ksat_infilt .le. 0.0) ) then
                call mprintf(.true., iERROR,'dSoil_ksat_infilt must be positive! ' // &
                                        ' Program stopped!')  
        endif        
        ! checking if dSoil_ksat_infilt & dSoil_vmax & dSoil_ksat_drain are defined in case of iFlagSoilParamsType=2  
        ! & also dWTable_ksath (no need for specific flag --> over-flexibility)
        if ( (iFlagSoilParamsType .eq. 2) .and. (dSoil_vmax .eq. -9999.0) ) then
            call mprintf(.true., iERROR,'dSoil_vmax must be defined if iFlagSoilParamsType=2.' // &
                                        ' Program stopped!')        
        elseif ( (iFlagSoilParamsType .eq. 2) .and. (dSoil_vmax .le. 0.0) ) then
            call mprintf(.true., iERROR,'dSoil_vmax must be positive!' // &
                                        ' Program stopped!')  
        endif
        ! checking if dSoil_ksat_infilt & dSoil_vmax & dSoil_ksat_drain are defined in case of iFlagSoilParamsType=2  
        ! & also dWTable_ksath (no need for specific flag --> over-flexibility)
        if ( (iFlagSoilParamsType .eq. 2) .and. (dSoil_ksat_drain .eq. -9999.0) ) then
            call mprintf(.true., iERROR,'dSoil_ksat_drain must be defined if iFlagSoilParamsType=2.' // &
                                        ' Program stopped!')    
        elseif ( (iFlagSoilParamsType .eq. 2) .and. (dSoil_ksat_drain .le. 0.0) ) then
            call mprintf(.true., iERROR,'dSoil_ksat_drain must be positive!' // &
                                        ' Program stopped!')
        endif
        ! checking if dSoil_ksat_infilt & dSoil_vmax & dSoil_ksat_drain are defined in case of iFlagSoilParamsType=2  
        ! & also dWTable_ksath (no need for specific flag --> over-flexibility)
        if ( (iFlagSoilParamsType .eq. 2) .and. (dWTable_ksath .eq. -9999.0) ) then
            call mprintf(.true., iERROR,'dWTable_ksath must be defined if iFlagSoilParamsType=2.' // &
                                        ' Program stopped!')    
        elseif ( (iFlagSoilParamsType .eq. 2) .and. (dWTable_ksath .le. 0.0) ) then
            call mprintf(.true., iERROR,'dWTable_ksath must be positive!' // &
                                        ' Program stopped!')   
        endif
        ! checking if iFlagInfiltRateVariable is defined in case of iFlagSoilParamsType=2  
        ! & also dWTable_ksath (no need for specific flag --> over-flexibility)
        if ( (iFlagSoilParamsType .eq. 2) .and. ( (iFlagInfiltRateVariable.ne.0) .and. &
                                                  (iFlagInfiltRateVariable.ne.1) .and. & 
                                                  (iFlagInfiltRateVariable.ne.2) )) then
                call mprintf(.true., iERROR,'iFlagInfiltRateVariable must be defined if iFlagSoilParamsType=2. ' // &
                                        ' (Allowed values are 0-1-2) Program stopped!')        
        endif
 
        ! Convolution constant(s)
        oHMC_Namelist_Init%dRateMin = dRateMin
        oHMC_Namelist_Init%dBc = dBc
        
        ! dRateRescaling ~ rescaling factor for hypodermic flow fraction dRate
        if (dRateRescaling .eq. -9999) then  ! backward compatibility with older version of info file 
            oHMC_Namelist_Init%dRateRescaling = 1.0               
        else
            oHMC_Namelist_Init%dRateRescaling = dRateRescaling
            if (dRateRescaling .lt. dRateMin) then
                call mprintf(.true., iERROR,'Minimum allowed value for dRateRescaling is dRateMin.' // &
                                        ' Program stopped!')   
            elseif (dRateRescaling .gt. 1.0) then
                call mprintf(.true., iERROR,'Maximum allowed value for dRateRescaling is 1.' // &
                                        ' Program stopped!') 
            endif
        endif
        
        if (dWTLossMax .eq. -9999) then  ! backward compatibility with older version of info file (without watertable deep losses value)
            oHMC_Namelist_Init%dWTLossMax = -9999.0               
        else
            oHMC_Namelist_Init%dWTLossMax = dWTLossMax
        endif
        
        ! LSM constant(s)
        oHMC_Namelist_Init%dTRef = dTRef
        oHMC_Namelist_Init%iTdeepShift = iTdeepShift
        oHMC_Namelist_Init%dEpsS = dEpsS
        oHMC_Namelist_Init%dSigma = dSigma
        oHMC_Namelist_Init%dBFMin = dBFMin
        oHMC_Namelist_Init%dBFMax = dBFMax
        oHMC_Namelist_Init%dZRef = dZRef
        oHMC_Namelist_Init%dG = dG
        oHMC_Namelist_Init%dCp = dCp
        oHMC_Namelist_Init%dRd = dRd
        oHMC_Namelist_Init%dRhoS = dRhoS
        oHMC_Namelist_Init%dRhoW = dRhoW
        oHMC_Namelist_Init%dCpS = dCpS
        oHMC_Namelist_Init%dCpW = dCpW
        oHMC_Namelist_Init%dKq = dKq
        oHMC_Namelist_Init%dKw = dKw
        oHMC_Namelist_Init%dKo = dKo
        oHMC_Namelist_Init%dPorS = dPorS
        oHMC_Namelist_Init%dFqS = dFqS
        
        if (dLSTDeltaMax .eq. -9999) then  ! backward compatibility with older version of info file (without lst delta max value)
            oHMC_Namelist_Init%dLSTDeltaMax = 20               ! default mode without lst delta max value definition
        else
            oHMC_Namelist_Init%dLSTDeltaMax = dLSTDeltaMax
        endif
        
        oHMC_Namelist_Init%dTV = dTV
        oHMC_Namelist_Init%dDamSpillH = dDamSpillH
        
        if (dSMGain .eq. -9999) then  ! backward compatibility with older version of info file (without sm gain value)
            oHMC_Namelist_Init%dSMGain = 0               ! default mode without soil moisture gain definition
        else
            oHMC_Namelist_Init%dSMGain = dSMGain
        endif
        
        ! Model info
        oHMC_Namelist_Init%sReleaseDate = sReleaseDate
        oHMC_Namelist_Init%sAuthorNames = sAuthorNames
        oHMC_Namelist_Init%sReleaseVersion = sReleaseVersion
        
        ! Check of dumping flags for generic and flooding variable(s))
        if ( oHMC_Namelist_Init%iActiveData_Output_Generic .eq. 1 ) then
            call mprintf(.true., iINFO_Main, ' Activation of generic outcome dumping: BASIC DATASETS ')
        else if ( oHMC_Namelist_Init%iActiveData_Output_Generic .eq. 2 ) then
            call mprintf(.true., iINFO_Main, ' Activation of generic outcome dumping: EXTENDED DATASETS')
        else
            call mprintf(.true., iERROR, ' Activation of generic outcome dumping: NULL (1 = BASIN, 2 = EXTENDED)')
        endif
        
        if ( oHMC_Namelist_Init%iActiveData_Output_Flooding .eq. 0 ) then
            call mprintf(.true., iINFO_Main, ' Activation of flooding outcome dumping: NOT ACTIVE ')
        else if ( oHMC_Namelist_Init%iActiveData_Output_Flooding .eq. 1 ) then
            call mprintf(.true., iINFO_Main, ' Activation of flooding outcome dumping: ACTIVE')
        else
            call mprintf(.true., iERROR, ' Activation of flooding outcome dumping: NULL (0 = NOT ACTIVE, 1 = ACTIVE)')
        endif
        
        if ( oHMC_Namelist_Init%iActiveData_Output_Snow .eq. 0 ) then
            call mprintf(.true., iINFO_Main, ' Activation of snow outcome dumping: NOT ACTIVE ')
        else if ( oHMC_Namelist_Init%iActiveData_Output_Snow .eq. 1 ) then
            call mprintf(.true., iINFO_Main, ' Activation of snow outcome dumping: ACTIVE')
        else
            call mprintf(.true., iERROR, ' Activation of flooding outcome dumping: NULL (0 = NOT ACTIVE, 1 = ACTIVE)')
        endif
        
        if (( oHMC_Namelist_Init%iFlagFlood .eq. 0 ) .and. ( oHMC_Namelist_Init%iActiveData_Output_Flooding .eq. 1 )) then
            call mprintf(.true., iWARN, ' Flooding is not activated; deactivate the dumping of flooding output ')
            oHMC_Namelist_Init%iActiveData_Output_Flooding = 0
        endif
        
        if (( oHMC_Namelist_Init%iFlagSnow .eq. 0 ) .and. ( oHMC_Namelist_Init%iActiveData_Output_Snow .eq. 1 )) then
            call mprintf(.true., iWARN, ' Snow is not activated; deactivate the dumping of snow output ')
            oHMC_Namelist_Init%iActiveData_Output_Snow = 0
        endif
        
        ! Info model
        write(sStrUc, *) dUc; write(sStrUh, *) dUh; write(sStrCt, *) dCt; write(sStrCf, *) dCf;
        call mprintf(.true., iINFO_Basic, ' PARAMETER(S) DEFAULT INFO --- dUc: '//trim(sStrUc)//' - dUh: '//trim(sStrUh)// &
                    ' dCt: '//trim(sStrCt)//' dCf: '//trim(sStrCf) )     
        ! Alternative soil parametrization
        if (iFlagSoilParamsType .eq. 2) then
            write(sStrVmax, *) dSoil_vmax; write(sStrKsat_infilt, *) dSoil_ksat_infilt; write(sStrKsat_drain, *) dSoil_ksat_drain;
            call mprintf(.true., iINFO_Basic, ' SOIL PARAMETERS from Namelist --- Vmax: '//trim(sStrVmax)//' - ksat_infilt: '// &
                         trim(sStrKsat_infilt)//' ksat_drain: '//trim(sStrKsat_drain) )    
            write(sStrKsat_wtable, *) dWTable_ksath;
            call mprintf(.true., iINFO_Basic, ' AQUIFER HORIZONTAL HYDRAULIC CONDUCTIVITY from Namelist --- WTable_ksath: ' & 
                         //trim(sStrKsat_wtable) )
        endif
        ! Info
        call mprintf(.true., iINFO_Main, ' Read Namelist ... OK')
        ! Separation Line
        call mprintf(.true., iINFO_Basic, '************************************************************************')
        !--------------------------------------------------------------------------------
        
    end subroutine HMC_Namelist_Read
    !--------------------------------------------------------------------------------

end module HMC_Module_Namelist
!--------------------------------------------------------------------------------

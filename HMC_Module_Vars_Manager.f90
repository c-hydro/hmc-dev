!------------------------------------------------------------------------------------
! File:   HMC_Module_Vars_Manager.f90
! Author(s): Fabio Delogu, Francesco Silvestro, Simone Gabellani
! Created on February 11 2015, 9:57 AM
!
! Module to allocate and initialize global variable(s)
!------------------------------------------------------------------------------------

!------------------------------------------------------------------------------------
! Module Header
module HMC_Module_Vars_Manager
    
    !------------------------------------------------------------------------------------
    ! External module(s) and implicit none
    use HMC_Module_Namelist,    only: oHMC_Namelist
    use HMC_Module_Vars_Loader,  only: oHMC_Vars
    
    use HMC_Module_Tools_Debug
    
    implicit none
    !------------------------------------------------------------------------------------
    
contains
    
    !------------------------------------------------------------------------------------
    ! Subroutine for allocating HMC static variable(s)
    subroutine HMC_Vars_Allocate(iID, &
                                 iRows, iCols, & 
                                 iNSection, &
                                 iNLake, iNDam, iNPlant, iNJoint, iNCatch, iNRelease, &
                                 iDaySteps, iTMarkedSteps, &
                                 iNData, &
                                 iETime)
        
        !------------------------------------------------------------------------------------ 
        ! Variable(s)
        integer(kind = 4)    :: iID
        integer(kind = 4)    :: iRows, iCols
        integer(kind = 4)    :: iNSection
        integer(kind = 4)    :: iNLake, iNDam, iNPlant, iNJoint, iNCatch, iNRelease
        integer(kind = 4)    :: iDaySteps, iTMarkedSteps 
        integer(kind = 4)    :: iNData
        integer(kind = 4)    :: iETime
        !------------------------------------------------------------------------------------ 
        
        !------------------------------------------------------------------------------------ 
        ! Start to allocate static variable(s)
        call mprintf(.true., iINFO_Main, ' Allocating static variable(s) ... ')

        ! Static Land variable(s)
        allocate( oHMC_Vars(iID)%a2dLon             (iRows, iCols) )
        allocate( oHMC_Vars(iID)%a2dLat             (iRows, iCols) )
        allocate( oHMC_Vars(iID)%a2dDEM             (iRows, iCols) )
        allocate( oHMC_Vars(iID)%a2dS               (iRows, iCols) )
        allocate( oHMC_Vars(iID)%a2dCN              (iRows, iCols) )
        allocate( oHMC_Vars(iID)%a1dFCN             (100)   )
        allocate( oHMC_Vars(iID)%a2iPNT             (iRows, iCols) )
        allocate( oHMC_Vars(iID)%a2iChoice          (iRows, iCols) )
        allocate( oHMC_Vars(iID)%a2iMask            (iRows, iCols) )
        allocate( oHMC_Vars(iID)%a2dAreaCell        (iRows, iCols) )
        allocate( oHMC_Vars(iID)%a2iNature          (iRows, iCols) )
        
        allocate( oHMC_Vars(iID)%a2dC1              (iRows, iCols) )
        allocate( oHMC_Vars(iID)%a2dF2              (iRows, iCols) )
        allocate( oHMC_Vars(iID)%a2dCostF           (iRows, iCols) )
        allocate( oHMC_Vars(iID)%a2dCostK           (iRows, iCols) )
        allocate( oHMC_Vars(iID)%a2dCostF1          (iRows, iCols) )
        allocate( oHMC_Vars(iID)%a2dCostChFix       (iRows, iCols) )
        
        allocate( oHMC_Vars(iID)%a2dAlpha           (iRows, iCols) )
        allocate( oHMC_Vars(iID)%a2dBeta            (iRows, iCols) )
        allocate( oHMC_Vars(iID)%a2dWTable          (iRows, iCols) )
        allocate( oHMC_Vars(iID)%a2dWTableMax       (iRows, iCols) )
        
        allocate( oHMC_Vars(iID)%a2dExpRhoLow       (iRows, iCols) )
        allocate( oHMC_Vars(iID)%a2dExpRhoHigh      (iRows, iCols) )
        allocate( oHMC_Vars(iID)%a2dArctUp          (iRows, iCols) )
        
        allocate( oHMC_Vars(iID)%a2dCoeffWS         (iRows, iCols) )
        
        allocate( oHMC_Vars(iID)%a2dCt              (iRows, iCols) )
        allocate( oHMC_Vars(iID)%a2dCf              (iRows, iCols) )
        allocate( oHMC_Vars(iID)%a2dUc              (iRows, iCols) )
        allocate( oHMC_Vars(iID)%a2dUh              (iRows, iCols) )
        
        allocate( oHMC_Vars(iID)%a2dCtWP            (iRows, iCols) )
        allocate( oHMC_Vars(iID)%a2dKb1             (iRows, iCols) )
        allocate( oHMC_Vars(iID)%a2dKc1             (iRows, iCols) )
        allocate( oHMC_Vars(iID)%a2dKb2             (iRows, iCols) )
        allocate( oHMC_Vars(iID)%a2dKc2             (iRows, iCols) )
        
        allocate( oHMC_Vars(iID)%a2dCoeffResol      (iRows, iCols) )
        allocate( oHMC_Vars(iID)%a2iXIndex          (iRows, iCols) )
        allocate( oHMC_Vars(iID)%a2iYIndex          (iRows, iCols) )
        
        ! Section array(s)
        allocate( oHMC_Vars(iID)%a2iXYSection       (iNSection, 2) )
        allocate( oHMC_Vars(iID)%a1sNameSection     (iNSection) )
        
        ! Lake array(s)
        allocate( oHMC_Vars(iID)%a2iXYLake          (iNLake, 2) )
        allocate( oHMC_Vars(iID)%a1dCodeLake        (iNLake) )
        allocate( oHMC_Vars(iID)%a1iNCellLake       (iNLake) )
        allocate( oHMC_Vars(iID)%a1dVMinLake        (iNLake) )
        allocate( oHMC_Vars(iID)%a1dCostLake        (iNLake) )
        
        ! Joint array(s)
        allocate( oHMC_Vars(iID)%a2iXYJoint         (iNJoint, 2) )
        allocate( oHMC_Vars(iID)%a2iXYInJoint       (iNJoint, 2) )
        allocate( oHMC_Vars(iID)%a2iXYOutJoint      (iNJoint, 2) )
        allocate( oHMC_Vars(iID)%a1dThrLevelJoint   (iNJoint) )
        
        ! Dam array(s)
        allocate( oHMC_Vars(iID)%a2iXYDam           (iNDam, 2) )
        allocate( oHMC_Vars(iID)%a1dCodeDam         (iNDam) )
        allocate( oHMC_Vars(iID)%a1iNCellDam        (iNDam) )
        allocate( oHMC_Vars(iID)%a1dVMaxDam         (iNDam) )
        allocate( oHMC_Vars(iID)%a1dQcSLDam         (iNDam) )
        allocate( oHMC_Vars(iID)%a1dHMaxDam         (iNDam) )
        allocate( oHMC_Vars(iID)%a1dCoeffDam        (iNDam) )
        allocate( oHMC_Vars(iID)%a2dVDam            (iNDam, 800) ) 
        allocate( oHMC_Vars(iID)%a2dLDam            (iNDam, 800) )   
        
        ! Plant array(s)
        allocate( oHMC_Vars(iID)%a2iXYPlant         (iNPlant, 2) )
        allocate( oHMC_Vars(iID)%a1iFlagDamPlant    (iNPlant) )
        allocate( oHMC_Vars(iID)%a1dQMaxPlant       (iNPlant) )
        allocate( oHMC_Vars(iID)%a1dTcPlant         (iNPlant) )
        allocate( oHMC_Vars(iID)%a1sNamePlant       (iNPlant) )
        
        ! Catch arrays(s)
        allocate( oHMC_Vars(iID)%a2iXYCatch         (iNCatch, 2) )
        allocate( oHMC_Vars(iID)%a1sNameCatch       (iNCatch) )
        allocate( oHMC_Vars(iID)%a1dWeigthCatch     (iNCatch) )
        allocate( oHMC_Vars(iID)%a1dTCorrCatch      (iNCatch) )
        allocate( oHMC_Vars(iID)%a1dQMinCatch       (iNCatch) )

        ! Release array(s)
        allocate( oHMC_Vars(iID)%a2iXYRelease       (iNRelease, 2) )
        allocate( oHMC_Vars(iID)%a1sNameRelease     (iNRelease) )
        allocate( oHMC_Vars(iID)%a1dQMaxRelease     (iNRelease) ) 
        
        ! Finish to allocate static variable(s)
        call mprintf(.true., iINFO_Main, ' Allocating static variable(s) ... OK')
        !------------------------------------------------------------------------------------ 

        !------------------------------------------------------------------------------------ 
        ! Start to allocate dynamic variable(s)
        call mprintf(.true., iINFO_Main, ' Allocating dynamic variable(s) ... ')

        ! Dynamic forcing variable(s)
        allocate( oHMC_Vars(iID)%a2dRain            (iRows, iCols) )
        allocate( oHMC_Vars(iID)%a2dTa              (iRows, iCols) )
        allocate( oHMC_Vars(iID)%a2dK               (iRows, iCols) )
        allocate( oHMC_Vars(iID)%a2dW               (iRows, iCols) )
        allocate( oHMC_Vars(iID)%a2dRHum            (iRows, iCols) )
        allocate( oHMC_Vars(iID)%a2dPres            (iRows, iCols) )
        allocate( oHMC_Vars(iID)%a2dSHeight         (iRows, iCols) )
        allocate( oHMC_Vars(iID)%a2dSKernel         (iRows, iCols) )
        ! Dynamic updating variable(s)
        allocate( oHMC_Vars(iID)%a2dSCA             (iRows, iCols) )
        allocate( oHMC_Vars(iID)%a2dSQA             (iRows, iCols) )
        allocate( oHMC_Vars(iID)%a2dSMStar          (iRows, iCols) )
        allocate( oHMC_Vars(iID)%a2dSMGain          (iRows, iCols) )
        
        ! Dynamic volume variable(s)
        allocate( oHMC_Vars(iID)%a2dVTot            (iRows, iCols) )
        allocate( oHMC_Vars(iID)%a2dVRet            (iRows, iCols) )
        allocate( oHMC_Vars(iID)%a2dVSub            (iRows, iCols) )
        allocate( oHMC_Vars(iID)%a2dVLoss           (iRows, iCols) )
        allocate( oHMC_Vars(iID)%a2dVExf            (iRows, iCols) )
        allocate( oHMC_Vars(iID)%a2dVErr            (iRows, iCols) )
                
        ! Dynamic (monthly) Vegetation variable(s)
        allocate( oHMC_Vars(iID)%a2dLAI             (iRows, iCols) )
        allocate( oHMC_Vars(iID)%a2dAlbedo          (iRows, iCols) )
        
        ! Dynamic snow variable(s)
        allocate( oHMC_Vars(iID)%a2iAge             (iRows, iCols) )
        allocate( oHMC_Vars(iID)%a2dSWE             (iRows, iCols) )
        allocate( oHMC_Vars(iID)%a2dRhoS            (iRows, iCols) )
        allocate( oHMC_Vars(iID)%a2dRhoS0           (iRows, iCols) )
        allocate( oHMC_Vars(iID)%a2dAlbedo_Snow     (iRows, iCols) )
        allocate( oHMC_Vars(iID)%a3dTaC_Days1       (iRows, iCols, iDaySteps) )
        allocate( oHMC_Vars(iID)%a3dTaC_Days5       (iRows, iCols, iDaySteps*5) )
        allocate( oHMC_Vars(iID)%a2dMelting         (iRows, iCols) )  
        allocate( oHMC_Vars(iID)%a2dMeltingSc       (iRows, iCols) ) 
        allocate( oHMC_Vars(iID)%a2dMeltingDayCum   (iRows, iCols) )
        allocate( oHMC_Vars(iID)%a2dSnowFall        (iRows, iCols) )  
        allocate( oHMC_Vars(iID)%a2dSnowFallDayCum  (iRows, iCols) )
        allocate( oHMC_Vars(iID)%a2dMaskS           (iRows, iCols) ) 
        
        ! Dynamic LSM variable(s)
        allocate( oHMC_Vars(iID)%a2dLST             (iRows, iCols) )
        allocate( oHMC_Vars(iID)%a3dTaKMarked       (iRows, iCols, iTMarkedSteps) )
        allocate( oHMC_Vars(iID)%a3dTaK24           (iRows, iCols, iDaySteps) )
        allocate( oHMC_Vars(iID)%a2dRn              (iRows, iCols) )
        allocate( oHMC_Vars(iID)%a2dH               (iRows, iCols) )
        allocate( oHMC_Vars(iID)%a2dLE              (iRows, iCols) )
        allocate( oHMC_Vars(iID)%a2dG               (iRows, iCols) )
        allocate( oHMC_Vars(iID)%a2dEF              (iRows, iCols) )
        
        ! Dynamic ET variable(s)
        allocate( oHMC_Vars(iID)%a2dAE              (iRows, iCols) ) 
        allocate( oHMC_Vars(iID)%a2dET              (iRows, iCols) )
        allocate( oHMC_Vars(iID)%a2dETCum           (iRows, iCols) )
        
        ! Dynamic Convolution variable(s)
        allocate( oHMC_Vars(iID)%a2dHydro           (iRows, iCols) )
        allocate( oHMC_Vars(iID)%a2dHydroPrev       (iRows, iCols) )
        allocate( oHMC_Vars(iID)%a2dRouting         (iRows, iCols) )
        allocate( oHMC_Vars(iID)%a2dDarcy           (iRows, iCols) )
        allocate( oHMC_Vars(iID)%a2dQout            (iRows, iCols) )
        allocate( oHMC_Vars(iID)%a2dQDisOut         (iRows, iCols) )
        allocate( oHMC_Vars(iID)%a2dQVolOut         (iRows, iCols) )
        allocate( oHMC_Vars(iID)%a2dQTot            (iRows, iCols) )
        allocate( oHMC_Vars(iID)%a2dIntensity       (iRows, iCols) )
        allocate( oHMC_Vars(iID)%a2dFlowDeep        (iRows, iCols) )
        allocate( oHMC_Vars(iID)%a2dFlowExf         (iRows, iCols) )
        allocate( oHMC_Vars(iID)%a2dUcAct           (iRows, iCols) )
        allocate( oHMC_Vars(iID)%a2dUDt             (iRows, iCols) )
        
        ! Dynamic water sources variable(s)
        allocate( oHMC_Vars(iID)%a2dWSRunoff        (iRows, iCols) )
        
        ! Dynamic section variable(s)
        allocate( oHMC_Vars(iID)%a1dQoutSection     (iNSection) )
        
        ! Dynamic lake variable(s)
        allocate( oHMC_Vars(iID)%a1dQoutLake        (iNLake) )
        allocate( oHMC_Vars(iID)%a1dVLake           (iNLake) )
        
        ! Dynamic dam variable(s)
        allocate( oHMC_Vars(iID)%a1dQoutDam         (iNDam) )
        allocate( oHMC_Vars(iID)%a1dVDam            (iNDam) ) 
        allocate( oHMC_Vars(iID)%a1dLDam            (iNDam) ) 
        allocate( oHMC_Vars(iID)%a1dHDam            (iNDam) ) 
        allocate( oHMC_Vars(iID)%a1dVDamObs         (iNDam) )
        
        ! Dynamic plant variable(s)
        !allocate( oHMC_Vars(iID)%a2dHydroPlant      (iNPlant, iNData) )
        allocate( oHMC_Vars(iID)%a2dHydroPlant      (iNPlant, iETime) )
        
        ! Dynamic catch variable(s)
        !allocate( oHMC_Vars(iID)%a2dHydroCatch      (iNCatch, iNData) )
        allocate( oHMC_Vars(iID)%a2dHydroCatch      (iNCatch, iETime) )
        
        ! Dynamic release variable(s)
        !allocate( oHMC_Vars(iID)%a2dHydroRelease    (iNRelease, iNData) )
        allocate( oHMC_Vars(iID)%a2dHydroRelease    (iNRelease, iETime) )
        
        ! Finish to allocate dynamic variable(s)
        call mprintf(.true., iINFO_Main, ' Allocating dynamic variable(s) ... OK')
        !------------------------------------------------------------------------------------ 
        
    end subroutine HMC_Vars_Allocate
    !------------------------------------------------------------------------------------
        
    !------------------------------------------------------------------------------------
    ! Subroutine for initializing var(s) with default value
    subroutine HMC_Vars_InitDefault(iID)

        !------------------------------------------------------------------------------------ 
        ! Variable(s)
        integer(kind = 4) :: iID
        !------------------------------------------------------------------------------------
	
        !------------------------------------------------------------------------------------
        ! Start to initialize scalar variable(s)
        call mprintf(.true., iINFO_Main, ' Initialize scalar variable(s) ... ')
        oHMC_Vars(iID)%bFileForcingTimeSeries = .false.
        
        oHMC_Vars(iID)%iDtIntegr = oHMC_Namelist(iID)%iDtPhysConv
        oHMC_Vars(iID)%iDtIntegrPStep = oHMC_Namelist(iID)%iDtPhysConv   
        oHMC_Vars(iID)%dVErr = 0.0
        oHMC_Vars(iID)%dVarETTot = 0.0
        
        oHMC_Vars(iID)%iFileUnitTSQ = -9999
        oHMC_Vars(iID)%bFileUnitTSQ = .false.
        oHMC_Vars(iID)%iFileUnitTSVDam = -9999
        oHMC_Vars(iID)%bFileUnitTSVDam = .false.
        
        ! Finist to initialize scalar variable(s)
        call mprintf(.true., iINFO_Main, ' Initialize scalar variable(s) ... OK ')
        !------------------------------------------------------------------------------------ 

        !------------------------------------------------------------------------------------
        ! Start to initialize static variable(s)
        call mprintf(.true., iINFO_Main, ' Initialize static variable(s) ... ')

        ! Static land-data variable(s)
        oHMC_Vars(iID)%a2dLon = 0.0
        oHMC_Vars(iID)%a2dLat = 0.0
        oHMC_Vars(iID)%a2iMask = 0.0
        oHMC_Vars(iID)%a2iChoice = 0
        oHMC_Vars(iID)%a2dDEM = 0.0
        oHMC_Vars(iID)%a2dCN = 0.0
        oHMC_Vars(iID)%a2iPNT = 0
        oHMC_Vars(iID)%a1dFCN = 0.0
        oHMC_Vars(iID)%a2dS = 500.0 
        oHMC_Vars(iID)%a2dAreaCell = 0.0
        oHMC_Vars(iID)%a2iNature = 0
        
        ! Static infiltration (horton) variable(s)
        oHMC_Vars(iID)%a2dC1 = 0.0
        oHMC_Vars(iID)%a2dF2 = 0.0
        oHMC_Vars(iID)%a2dCostF1 = 0.0
        oHMC_Vars(iID)%a2dCostF = 0.0
        oHMC_Vars(iID)%a2dCostChFix = 0.0

        ! Static water-table variable(s)
        oHMC_Vars(iID)%a2dAlpha = 0.0
        oHMC_Vars(iID)%a2dBeta = 0.0
        oHMC_Vars(iID)%a2dWTable = 0.0
        oHMC_Vars(iID)%a2dWTableMax = 0.0
        
        ! Static model parameter(s) variable(s)
        oHMC_Vars(iID)%a2dCt = oHMC_Namelist(iID)%dCt
        oHMC_Vars(iID)%a2dCf = oHMC_Namelist(iID)%dCf
        oHMC_Vars(iID)%a2dUc = oHMC_Namelist(iID)%dUc
        oHMC_Vars(iID)%a2dUh = oHMC_Namelist(iID)%dUh
        
        ! Static Water sources parameter variable(s)
        oHMC_Vars(iID)%a2dCoeffWS = 0.0
        
        ! Static energy balance parameter(s)
        oHMC_Vars(iID)%a2dCtWP = 0.0
        oHMC_Vars(iID)%a2dKb1 = 0.0
        oHMC_Vars(iID)%a2dKc1 = 0.0
        oHMC_Vars(iID)%a2dKb2 = 0.0
        oHMC_Vars(iID)%a2dKc2 = 0.0
        
        ! Static coefficient resolution variable
        oHMC_Vars(iID)%a2dCoeffResol = 1.0
        
        ! Static snow variable(s)
        oHMC_Vars(iID)%a2dExpRhoLow = 0.0
        oHMC_Vars(iID)%a2dExpRhoHigh = 0.0
        oHMC_Vars(iID)%a2dArctUp = 0.0
        
        ! Static indexes grids (land-forcing)
        oHMC_Vars(iID)%a2iXIndex = 0
        oHMC_Vars(iID)%a2iYIndex = 0
        
        ! Lake variable(s)
        oHMC_Vars(iID)%a2iXYLake = 0.0
        oHMC_Vars(iID)%a1dCodeLake = 0.0
        oHMC_Vars(iID)%a1iNCellLake = 0.0  ! Number of lake cell(s)
        oHMC_Vars(iID)%a1dVMinLake = 0.0
        oHMC_Vars(iID)%a1dVLake = 0.0
        oHMC_Vars(iID)%a1dCostLake = 0.0

        ! Joint variable(s)
        oHMC_Vars(iID)%a2iXYJoint = 0
        oHMC_Vars(iID)%a2iXYInJoint = 0
        oHMC_Vars(iID)%a2iXYOutJoint = 0
        oHMC_Vars(iID)%a1dThrLevelJoint = 0.0
        
        ! Dam array(s)
        oHMC_Vars(iID)%a2iXYDam  = 0
        oHMC_Vars(iID)%a1dCodeDam = 0.0
        oHMC_Vars(iID)%a1iNCellDam = 0.0        ! Number of lake cell(s) before outlet dam section
        oHMC_Vars(iID)%a1dVMaxDam = 0.0
        oHMC_Vars(iID)%a1dQcSLDam = 0.0
        oHMC_Vars(iID)%a1dHMaxDam = 0.0
        oHMC_Vars(iID)%a1dCoeffDam = 0.0
        oHMC_Vars(iID)%a2dVDam = -9999.0        ! to get dam storage curve (dam volume)
        oHMC_Vars(iID)%a2dLDam = -9999.0        ! to get dam storage curve (height of water into dam)
        
        ! Plant array(s)
        oHMC_Vars(iID)%a2iXYPlant = 0
        oHMC_Vars(iID)%a1iFlagDamPlant = 0
        oHMC_Vars(iID)%a1dQMaxPlant = 0.0
        oHMC_Vars(iID)%a1dTcPlant = 0.0
        oHMC_Vars(iID)%a1sNamePlant = ""
        
        ! Catch array(s)
        oHMC_Vars(iID)%a2iXYCatch = 0
        oHMC_Vars(iID)%a1dWeigthCatch = 0.0
        oHMC_Vars(iID)%a1dTCorrCatch = 0.0
        oHMC_Vars(iID)%a1dQMinCatch = 0.0
        oHMC_Vars(iID)%a1sNameCatch = ""

        ! Release array(s)
        oHMC_Vars(iID)%a2iXYRelease = 0
        oHMC_Vars(iID)%a1sNameRelease = ""
        oHMC_Vars(iID)%a1dQMaxRelease = 0.0
        
        ! Finish to initialize static variable(s)
        call mprintf(.true., iINFO_Main, ' Initialize static variable(s) ... OK ')
        !------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------
        ! Start to initialize dynamic model variable(s)
        call mprintf(.true., iINFO_Main, ' Initialize dynamic variable(s) ... ')
        
        ! Time information
        oHMC_Vars(iID)%sTimeStep = ''
        oHMC_Vars(iID)%iTime = -1
        
        ! Dynamic forcing variable(s)
        oHMC_Vars(iID)%a2dRain = 0.0            ! Rain                  [mm]        --> Forcing
        oHMC_Vars(iID)%a2dTa = 0.0              ! Air Temperature       [C]         --> Forcing
        oHMC_Vars(iID)%a2dK = 0.0               ! Incoming radiation    [W/m^2]     --> Forcing
        oHMC_Vars(iID)%a2dW = 0.0               ! Wind speed            [m/s]       --> Forcing
        oHMC_Vars(iID)%a2dRHum = 0.0            ! Relative humidity     [%]         --> Forcing
        oHMC_Vars(iID)%a2dPres = 0.0            ! Air Pressure          [kPa]       --> Forcing
        oHMC_Vars(iID)%a2dSHeight = -9999.0     ! Snow Height           [cm]        --> Forcing for snow physics
        oHMC_Vars(iID)%a2dSKernel = -9999.0     ! Snow Kernel           [0, 1]      --> Forcing for snow physics
        
        ! Dynamic updating variable(s)
        oHMC_Vars(iID)%a2dSCA = -9999.0         ! Snow Cover Area       [-1, -3]    --> Updating for snow physics
        oHMC_Vars(iID)%a2dSQA = -9999.0         ! Snow Quality          [0, -1]     --> Updating for snow physics
        oHMC_Vars(iID)%a2dSMStar = -9999.0      ! Soil Moisture Value   [0, 1]      --> Updating for soil moisture assimilation
        oHMC_Vars(iID)%a2dSMGain = -9999.0      ! Soil Moisture Gain    [0, 1]      --> Updating for soil moisture assimilation
 
        ! Dynamic volume variable(s)
        oHMC_Vars(iID)%a2dVTot = 0.0
        oHMC_Vars(iID)%a2dVRet = 0.001
        oHMC_Vars(iID)%a2dVSub = 0.0
        oHMC_Vars(iID)%a2dVLoss = 0.0
        oHMC_Vars(iID)%a2dVExf = 0.0
        oHMC_Vars(iID)%a2dVErr = 0.0
        
        ! Dynamic (monthly) vegetation and albedo variable(s)
        oHMC_Vars(iID)%a2dLAI = 0.0
        oHMC_Vars(iID)%a2dAlbedo = 0.0
        
        ! Dynamic snow variable(s)
        oHMC_Vars(iID)%a2iAge = 0           
        oHMC_Vars(iID)%a2dSWE = 0.0   
        oHMC_Vars(iID)%a2dRhoS = 0.0
        oHMC_Vars(iID)%a2dRhoS0 = 0.0
        oHMC_Vars(iID)%a2dAlbedo_Snow = 0.0
        oHMC_Vars(iID)%a2dMelting = 0.0
        oHMC_Vars(iID)%a2dMeltingSc = 0.0
        oHMC_Vars(iID)%a2dMeltingDayCum = 0.0
        oHMC_Vars(iID)%a3dTaC_Days1 = 0.0
        oHMC_Vars(iID)%a3dTaC_Days5 = 0.0
        oHMC_Vars(iID)%a2dSnowFall = 0.0
        oHMC_Vars(iID)%a2dSnowFallDayCum = 0.0
        oHMC_Vars(iID)%a2dMaskS = 0.0
        
        ! Dynamic LSM variable(s)
        oHMC_Vars(iID)%a2dLST = 0.0
        oHMC_Vars(iID)%a3dTaKMarked = 0.0
        oHMC_Vars(iID)%a3dTaK24 = 0.0
        oHMC_Vars(iID)%a2dRn = 0.0
        oHMC_Vars(iID)%a2dH = 0.0
        oHMC_Vars(iID)%a2dLE = 0.0
        oHMC_Vars(iID)%a2dG = 0.0
        oHMC_Vars(iID)%a2dEF = 0.0
        
        ! Dynamic ET variable(s)
        oHMC_Vars(iID)%a2dET = 0.0
        oHMC_Vars(iID)%a2dAE = 0.0
        oHMC_Vars(iID)%a2dETCum = 0.0
        
        ! Dynamic convolution variable(s)
        oHMC_Vars(iID)%a2dHydro = 0.000001    
        oHMC_Vars(iID)%a2dHydroPrev = 0.000001
        oHMC_Vars(iID)%a2dRouting = 0.0     
        oHMC_Vars(iID)%a2dDarcy = 0.0     
        oHMC_Vars(iID)%a2dQout = 0.0     
        oHMC_Vars(iID)%a2dQDisOut = 0.0     
        oHMC_Vars(iID)%a2dQVolOut = 0.0     
        oHMC_Vars(iID)%a2dQTot = 0.0     
        oHMC_Vars(iID)%a2dIntensity = 0.0     
        oHMC_Vars(iID)%a2dFlowDeep= 0.0     
        oHMC_Vars(iID)%a2dFlowExf = 0.0     
        oHMC_Vars(iID)%a2dUcAct = 0.0
        oHMC_Vars(iID)%a2dUDt = 0.0
        
        ! Dynamic water sources variable(s)
        oHMC_Vars(iID)%a2dWSRunoff = 0.0
        
        ! Dynamic section variable(s)
        oHMC_Vars(iID)%a1dQoutSection = 0.0
        
        ! Dynamic lake variable(s)
        oHMC_Vars(iID)%a1dQoutLake = 0.0
        oHMC_Vars(iID)%a1dVLake = 0.0
        
        ! Dynamic dam variable(s)
        oHMC_Vars(iID)%a1dQoutDam = 0.0
        oHMC_Vars(iID)%a1dVDam = 0.0            ! to store dynamic volume dam 
        oHMC_Vars(iID)%a1dLDam = 0.0            ! to store dynamic length dam
        oHMC_Vars(iID)%a1dHDam = 0.0            ! to store dynamic water heigth dam
        oHMC_Vars(iID)%a1dVDamObs  = -9999.0    ! to store dynamic volume dam observation(s)
        
        ! Dynamic plant variable(s)
        oHMC_Vars(iID)%a2dHydroPlant = 0.0
        
        ! Dynamic catch variable(s)
        oHMC_Vars(iID)%a2dHydroCatch = 0.0

        ! Dynamic release variable(s)
        oHMC_Vars(iID)%a2dHydroRelease = 0.0
        
        ! Finish to initialize dynamic variable(s)
        call mprintf(.true., iINFO_Main, ' Initialize dynamic variable(s) ... OK ')
        !------------------------------------------------------------------------------------ 
        
    end subroutine HMC_Vars_InitDefault
    !------------------------------------------------------------------------------------ 
    
end module HMC_Module_Vars_Manager
!------------------------------------------------------------------------------------

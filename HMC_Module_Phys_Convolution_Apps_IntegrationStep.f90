!------------------------------------------------------------------------------------
! File:   HMC_Module_Phys_Convolution_Apps_IntegrationStep.f90
!
! Author(s):    Fabio Delogu, Francesco Silvestro, Simone Gabellani
! Date:         20190410
!
! Convolution Apps IntegrationStep subroutine(s) for HMC model
!------------------------------------------------------------------------------------

!------------------------------------------------------------------------------------
! Module Header
module HMC_Module_Phys_Convolution_Apps_IntegrationStep

    !------------------------------------------------------------------------------------
    ! External module(s) 
    use HMC_Module_Namelist,                only: oHMC_Namelist
    use HMC_Module_Vars_Loader,             only: oHMC_Vars
    
    use HMC_Module_Tools_Debug
    
    use HMC_Module_Phys_HydraulicStructure, only: HMC_Phys_Dam_Spilling, &
                                                  HMC_Phys_Dam_Discharge, &
                                                  HMC_Phys_Lake_Tank
                                                  
    use HMC_Module_Tools_Generic,           only: getIntValue, getIntRange
    
    ! Implicit none for all subroutines in this module
    implicit none
    !------------------------------------------------------------------------------------------

contains 

    !------------------------------------------------------------------------------------------
    ! Subroutine to calculate dynamic integration step channel network
    subroutine HMC_Phys_Convolution_Apps_IntegrationStep_ChannelNetwork(iID, iRows, iCols, dDtDataForcing, dDtIntegrAct)
	
        !------------------------------------------------------------------------------------------
        ! Variable(s) declaration
        integer(kind = 4)               :: iID, iRows, iCols
        
        real(kind = 4)                  :: dDtDataForcing
        real(kind = 4)                  :: dDtIntegr, dDtIntegrPStep, dDtPhysConv
        real(kind = 4), intent(out)     :: dDtIntegrAct
        
        integer(kind = 4)               :: iFlagVarDtPhysConv
        real(kind = 4)                  :: dBc
        real(kind = 4)                  :: dDtRef, dDtRefRatio, dDtIntegrMin
        real(kind = 4)                  :: dDEMStepMean, dVarRainMax, dVarUDtMax
        real(kind = 4)                  :: dVarHydroMax
        
        real(kind = 4), dimension (iRows, iCols)            :: a2dVarUc, a2dVarUh
        
        real(kind = 4), dimension (iRows, iCols)            :: a2dVarHydro, a2dVarRain, a2dVarRouting
        
        real(kind = 4), dimension (iRows, iCols)            :: a2dVarUcAct
        real(kind = 4), dimension (iRows, iCols)            :: a2dVarUDt
        
        character(len = 10)                                 :: sDtIntegrStep, sVarUDtMax, sVarRainMax
        character(len = 10), parameter                      :: sFMTDtIntegrStep = "(I4)"
        character(len = 10), parameter                      :: sFMTVarUDtMax = "(F8.5)"
        character(len = 10), parameter                      :: sFMTVarRainMax = "(F8.5)"
        
        integer(kind = 4)                                   :: iDtPhysMethod
        real(kind = 4), dimension(4)                        :: a1dDemStep, a1dIntegStep, a1dDtRef, a1dDtRefRatio
        real(kind = 4)                                      :: dDemDelta
        integer(kind = 4)                                   :: iIndexEnd, iIndexStart
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Variable(s) initialization
        dDtIntegrAct = 0.0; dDtIntegrPStep = 0.0; dDtIntegr = 0.0;
        dVarRainMax = 0.0; dDEMStepMean = 0.0;
        dDtRef = 0.0; dDtRefRatio = 0.0; dDtIntegrMin = 0.0;
       
        a2dVarHydro = 0.0; 
        
        a2dVarUc = 0.0; a2dVarUh = 0.0;
        a2dVarUcAct = 0.0; 
        a2dVarUDt = 0.0; 
        
        iDtPhysMethod = 0;
        a1dDemStep = -9999.0; a1dIntegStep = -9999.0; a1dDtRef = -9999.0; a1dDtRefRatio  = -9999.0; 
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Temporal integration at previous step
        dDtIntegr = real(oHMC_Vars(iID)%iDtIntegr)
        dDtIntegrPStep = real(oHMC_Vars(iID)%iDtIntegrPStep)
        
        ! Dynamic temporal integration step
        iFlagVarDtPhysConv = oHMC_Namelist(iID)%iFlagVarDtPhysConv
        
        dDtPhysConv = real(oHMC_Namelist(iID)%iDtPhysConv)
        iDtPhysMethod = oHMC_Namelist(iID)%iDtPhysMethod
        
        a1dDemStep = oHMC_Namelist(iID)%a1dDemStep
        a1dIntegStep = oHMC_Namelist(iID)%a1dIntStep
        a1dDtRef = oHMC_Namelist(iID)%a1dDtStep
        a1dDtRefRatio = oHMC_Namelist(iID)%a1dDtRatioStep
        
        ! Exponent of dUcAct formula
        dBc = oHMC_Namelist(iID)%dBc
        ! DEM mean step
        dDEMStepMean = oHMC_Vars(iID)%dDEMStepMean
        
        ! Channel and hill surface velocity
        a2dVarUc = oHMC_Vars(iID)%a2dUc
        a2dVarUh = oHMC_Vars(iID)%a2dUh
        
        ! Dynamic variable(s)
        a2dVarHydro = oHMC_Vars(iID)%a2dHydro
        a2dVarRain = oHMC_Vars(iID)%a2dRain
        a2dVarRouting = oHMC_Vars(iID)%a2dRouting
        a2dVarUcAct = oHMC_Vars(iID)%a2dUcAct
        a2dVarUDt = oHMC_Vars(iID)%a2dUDt
        
        ! Info start
        if (iDEBUG.gt.0) then
            call mprintf(.true., iINFO_Extra, ' Phys :: Convolution :: IntegrationStep ... ' )
        endif
        !------------------------------------------------------------------------------------------
         
        !------------------------------------------------------------------------------------------
        ! Dynamic temporal integration step flag
        if (iFlagVarDtPhysConv.eq.1 ) then
            
            !------------------------------------------------------------------------------------------
            ! Actual temporal integration Step
            dDtIntegrAct = dDtIntegr
            !------------------------------------------------------------------------------------------
            
            !------------------------------------------------------------------------------------------
            ! Definition of temporal integration parameter(s)
            if (iDtPhysMethod .eq. 1) then
                
                !------------------------------------------------------------------------------------------
                ! Get integration parameter(s) using scalar method
                dDtRef = a1dDtRef(3);           ! seconds
                dDtIntegrMin = a1dIntegStep(3); ! seconds
                dDtRefRatio = a1dDtRefRatio(3)  ! seconds

                !if ( (dDEMStepMean.ge.10) .and. (dDEMStepMean.lt.150) ) then ! NB: for vda integration step was too little
                if ( (dDEMStepMean .ge. a1dDemStep(2)) .and. (dDEMStepMean .lt. a1dDemStep(3)) ) then 

                    dDtRef = a1dDtRef(2);           ! seconds
                    dDtIntegrMin = a1dIntegStep(2); ! seconds
                    dDtRefRatio = a1dDtRefRatio(2)  ! seconds

                elseif ( dDEMStepMean .lt. a1dDemStep(2) ) then 

                    dDtRef = a1dDtRef(1);           ! seconds
                    dDtIntegrMin = a1dIntegStep(1); ! seconds
                    dDtRefRatio = a1dDtRefRatio(1); ! seconds

                elseif (dDEMStepMean .gt. a1dDemStep(4) ) then

                    dDtRef = a1dDtRef(4);           ! seconds
                    dDtIntegrMin = a1dIntegStep(4); ! seconds
                    dDtRefRatio = a1dDtRefRatio(4); ! seconds

                else

                    dDtRef = a1dDtRef(3);           ! seconds
                    dDtIntegrMin = a1dIntegStep(3); ! seconds
                    dDtRefRatio = a1dDtRefRatio(3); ! seconds

                endif
                !------------------------------------------------------------------------------------------
            
            elseif (iDtPhysMethod .eq. 2) then
                
                !------------------------------------------------------------------------------------------
                ! Get integration parameter(s) using linear method
                call getIntRange(a1dDemStep, a1dIntegStep, dDEMStepMean, dDemDelta, iIndexStart, iIndexEnd)
                call getIntValue(a1dDemStep, a1dIntegStep, a1dDtRef, a1dDtRefRatio, &
                    dDEMStepMean, dDemDelta, iIndexStart, iIndexEnd, &
                    dDtIntegrMin, dDtRef, dDtRefRatio)
                !------------------------------------------------------------------------------------------
                    
            endif
            !------------------------------------------------------------------------------------------
                
            !------------------------------------------------------------------------------------------
            ! Dynamic integration step evaluation
            where( (oHMC_Vars(iID)%a2iChoice.eq.1) .and. (oHMC_Vars(iID)%a2dDem.gt.0.0) )
                a2dVarUcAct = a2dVarUc*(tan(oHMC_Vars(iID)%a2dBeta)**0.5)*a2dVarHydro**dBc
                a2dVarUDt = a2dVarHydro*a2dVarUcAct/(1000*3600)*oHMC_Vars(iID)%a2dAreaCell !m^3/s
            endwhere

            where( (oHMC_Vars(iID)%a2iChoice.eq.0) .and. (oHMC_Vars(iID)%a2dDem.gt.0.0) ) 
                a2dVarUDt = a2dVarHydro*a2dVarUh/(1000*3600)*oHMC_Vars(iID)%a2dAreaCell !m^3/s
            endwhere

            ! Checking waterlevel and updating 
            where( (a2dVarHydro.gt.0.0) .and. (oHMC_Vars(iID)%a2dDem.gt.0.0) )
                a2dVarUDt = a2dVarUDt/(a2dVarHydro/1000*dDEMStepMean) !m/s
            elsewhere
                a2dVarUDt = 0.0
            endwhere
            !------------------------------------------------------------------------------------------
            
            !------------------------------------------------------------------------------------------
            ! Rain intensity max value
            dVarRainMax = maxval(maxval(a2dVarRain,DIM = 1),DIM = 1)*3600.0/dDtDataForcing
            ! Velocity max value
            dVarUDtMax = maxval(maxval(a2dVarUDt,DIM = 1),DIM = 1)
            if (dVarUDtMax.le.0.1) dVarUDtMax = 0.1
            ! Hydro max value
            dVarHydroMax = maxval(maxval(a2dVarHydro,DIM = 1),DIM = 1)
            !------------------------------------------------------------------------------------------

            !------------------------------------------------------------------------------------------
            ! First estimation of integration step
            dDtIntegrAct = dDEMStepMean/dVarUDtMax*0.6

            ! Checking Integration step value
            if (dDtIntegrAct.gt.dDtDataForcing/dDtRefRatio) then
                dDtIntegrAct = dDtDataForcing/dDtRefRatio
            endif
            if ( (dVarRainMax.gt.1.0) .and. (dDtIntegrAct.gt.dDtPhysConv) ) then
                dDtIntegrAct = dDtPhysConv*exp(-dVarRainMax/3.0) + dDtPhysConv
            endif
            if (dDtIntegrAct.lt.dDtIntegrMin) then
                dDtIntegrAct = dDtIntegrMin
            endif

            dDtIntegrAct = dDtDataForcing/int(dDtDataForcing/dDtIntegrAct)
            dDtIntegrAct = int(dDtIntegrAct/dDtRef)*dDtRef
            !------------------------------------------------------------------------------------------

            !------------------------------------------------------------------------------------------
            ! Checking if dt integration is too big of previous dt integration
            if(dDtIntegrAct.gt.dDtIntegrPStep + dDtRef) then
                dDtIntegrAct = dDtIntegrPStep + dDtRef
            endif
            !------------------------------------------------------------------------------------------

            !------------------------------------------------------------------------------------------
            ! Updating routing variable; intensity must be the same between steps
            a2dVarRouting = a2dVarRouting/dDtIntegrPStep*dDtIntegrAct
            !------------------------------------------------------------------------------------------

        else
            
            !------------------------------------------------------------------------------------------
            ! Actual temporal integration Step
            dDtIntegrAct = dDtIntegr
            !------------------------------------------------------------------------------------------
            
        endif
        !------------------------------------------------------------------------------------------
      
        !------------------------------------------------------------------------------------------
        ! Temporal integration step info
        write(sDtIntegrStep, sFMTDtIntegrStep) int(dDtIntegrAct)
        write(sVarUDtMax, sFMTVarUDtMax) dVarUDtMax
        write(sVarRainMax, sFMTVarRainMax) dVarRainMax
        call mprintf(.true., iINFO_Basic, ' Phys :: Convolution :: IntegrationStep :: '//sDtIntegrStep//' [s]')
        call mprintf(.true., iINFO_Basic, ' Phys :: Convolution :: MaxValue :: '// &
                                          ' Uc: '//sVarUDtMax//' [m/s] '// &
                                          ' Rain: '//sVarRainMax//' [mm]')

        !------------------------------------------------------------------------------------------
                                        
        !------------------------------------------------------------------------------------------
        ! Passing variable(s) to global declaration
        oHMC_Vars(iID)%iDtIntegr = int(dDtIntegrAct)
        oHMC_Vars(iID)%iDtIntegrPStep = int(dDtIntegrAct)
        oHMC_Vars(iID)%a2dRouting = a2dVarRouting
        oHMC_Vars(iID)%a2dUcAct = a2dVarUcAct
        oHMC_Vars(iID)%a2dUDt = a2dVarUDt
        
        ! Info end
        if (iDEBUG.gt.0) then
            call mprintf(.true., iINFO_Extra, ' Phys :: Convolution :: IntegrationStep ... OK' )
        endif
        !------------------------------------------------------------------------------------------
        
    end subroutine HMC_Phys_Convolution_Apps_IntegrationStep_ChannelNetwork
    !------------------------------------------------------------------------------------
    
    !------------------------------------------------------------------------------------------
    ! Subroutine to calculate dynamic integration step channel fraction
    subroutine HMC_Phys_Convolution_Apps_IntegrationStep_ChannelFraction(iID, iRows, iCols, dDtDataForcing, dDtIntegrAct)
	
        !------------------------------------------------------------------------------------------
        ! Variable(s) declaration
        integer(kind = 4)               :: iID, iRows, iCols
        
        real(kind = 4)                  :: dDtDataForcing
        real(kind = 4)                  :: dDtIntegr, dDtIntegrPStep, dDtPhysConv
        real(kind = 4), intent(out)     :: dDtIntegrAct
        
        integer(kind = 4)               :: iFlagVarDtPhysConv
        real(kind = 4)                  :: dBc
        real(kind = 4)                  :: dDtRef, dDtRefRatio, dDtIntegrMin
        real(kind = 4)                  :: dDEMStepMean, dVarRainMax, dVarUDtMax
        real(kind = 4)                  :: dVarHydroMax
        
        real(kind = 4), dimension (iRows, iCols)            :: a2dVarUc, a2dVarUh
        
        real(kind = 4), dimension (iRows, iCols)            :: a2dVarHydro, a2dVarRain, a2dVarRouting
        
        real(kind = 4), dimension (iRows, iCols)            :: a2dVarUcAct
        real(kind = 4), dimension (iRows, iCols)            :: a2dVarUDt
        
        character(len = 10)                                 :: sDtIntegrStep, sVarUDtMax, sVarRainMax
        character(len = 10), parameter                      :: sFMTDtIntegrStep = "(I4)"
        character(len = 10), parameter                      :: sFMTVarUDtMax = "(F8.5)"
        character(len = 10), parameter                      :: sFMTVarRainMax = "(F8.5)"
        
        integer(kind = 4)                                   :: iDtPhysMethod
        real(kind = 4), dimension(4)                        :: a1dDemStep, a1dIntegStep, a1dDtRef, a1dDtRefRatio
        real(kind = 4)                                      :: dDemDelta
        integer(kind = 4)                                   :: iIndexEnd, iIndexStart
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Variable(s) initialization
        dDtIntegrAct = 0.0; dDtIntegrPStep = 0.0; dDtIntegr = 0.0;
        dVarRainMax = 0.0; dDEMStepMean = 0.0;
        dDtRef = 0.0; dDtRefRatio = 0.0; dDtIntegrMin = 0.0;
       
        a2dVarHydro = 0.0; 
        
        a2dVarUc = 0.0; a2dVarUh = 0.0;
        a2dVarUcAct = 0.0; 
        a2dVarUDt = 0.0; 
        
        iDtPhysMethod = 0;
        a1dDemStep = -9999.0; a1dIntegStep = -9999.0; a1dDtRef = -9999.0; a1dDtRefRatio  = -9999.0; 
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Temporal integration at previous step
        dDtIntegr = real(oHMC_Vars(iID)%iDtIntegr)
        dDtIntegrPStep = real(oHMC_Vars(iID)%iDtIntegrPStep)
        
        ! Dynamic temporal integration step
        iFlagVarDtPhysConv = oHMC_Namelist(iID)%iFlagVarDtPhysConv
        
        dDtPhysConv = real(oHMC_Namelist(iID)%iDtPhysConv)
        iDtPhysMethod = oHMC_Namelist(iID)%iDtPhysMethod
        
        a1dDemStep = oHMC_Namelist(iID)%a1dDemStep
        a1dIntegStep = oHMC_Namelist(iID)%a1dIntStep
        a1dDtRef = oHMC_Namelist(iID)%a1dDtStep
        a1dDtRefRatio = oHMC_Namelist(iID)%a1dDtRatioStep
        
        ! Exponent of dUcAct formula
        dBc = oHMC_Namelist(iID)%dBc
        ! DEM mean step
        dDEMStepMean = oHMC_Vars(iID)%dDEMStepMean
        
        ! Channel and hill surface velocity
        a2dVarUc = oHMC_Vars(iID)%a2dUc
        a2dVarUh = oHMC_Vars(iID)%a2dUh
        
        ! Dynamic variable(s)
        a2dVarRain = oHMC_Vars(iID)%a2dRain
        a2dVarRouting = oHMC_Vars(iID)%a2dRouting
        a2dVarUcAct = oHMC_Vars(iID)%a2dUcAct
        a2dVarUDt = oHMC_Vars(iID)%a2dUDt
        
        ! Info start
        if (iDEBUG.gt.0) then
            call mprintf(.true., iINFO_Extra, ' Phys :: Convolution :: IntegrationStep ... ' )
        endif
        !------------------------------------------------------------------------------------------
         
        !------------------------------------------------------------------------------------------
        ! Dynamic temporal integration step flag
        if (iFlagVarDtPhysConv.eq.1 ) then
            
            !------------------------------------------------------------------------------------------
            ! Actual temporal integration Step
            dDtIntegrAct = dDtIntegr
            !------------------------------------------------------------------------------------------
            
            !------------------------------------------------------------------------------------------
            ! Definition of temporal integration parameter(s)
            if (iDtPhysMethod .eq. 1) then
                
                !------------------------------------------------------------------------------------------
                ! Get integration parameter(s) using scalar method
                dDtRef = a1dDtRef(3);           ! seconds
                dDtIntegrMin = a1dIntegStep(3); ! seconds
                dDtRefRatio = a1dDtRefRatio(3)  ! seconds

                !if ( (dDEMStepMean.ge.10) .and. (dDEMStepMean.lt.150) ) then ! NB: for vda integration step was too little
                if ( (dDEMStepMean .ge. a1dDemStep(2)) .and. (dDEMStepMean .lt. a1dDemStep(3)) ) then 

                    dDtRef = a1dDtRef(2);           ! seconds
                    dDtIntegrMin = a1dIntegStep(2); ! seconds
                    dDtRefRatio = a1dDtRefRatio(2)  ! seconds

                elseif ( dDEMStepMean .lt. a1dDemStep(2) ) then 

                    dDtRef = a1dDtRef(1);           ! seconds
                    dDtIntegrMin = a1dIntegStep(1); ! seconds
                    dDtRefRatio = a1dDtRefRatio(1); ! seconds

                elseif (dDEMStepMean .gt. a1dDemStep(4) ) then

                    dDtRef = a1dDtRef(4);           ! seconds
                    dDtIntegrMin = a1dIntegStep(4); ! seconds
                    dDtRefRatio = a1dDtRefRatio(4); ! seconds

                else

                    dDtRef = a1dDtRef(3);           ! seconds
                    dDtIntegrMin = a1dIntegStep(3); ! seconds
                    dDtRefRatio = a1dDtRefRatio(3); ! seconds

                endif
                !------------------------------------------------------------------------------------------
            
            elseif (iDtPhysMethod .eq. 2) then
                
                !------------------------------------------------------------------------------------------
                ! Get integration parameter(s) using linear method
                call getIntRange(a1dDemStep, a1dIntegStep, dDEMStepMean, dDemDelta, iIndexStart, iIndexEnd)
                call getIntValue(a1dDemStep, a1dIntegStep, a1dDtRef, a1dDtRefRatio, &
                    dDEMStepMean, dDemDelta, iIndexStart, iIndexEnd, &
                    dDtIntegrMin, dDtRef, dDtRefRatio)
                !------------------------------------------------------------------------------------------
                    
            endif
            !------------------------------------------------------------------------------------------
                
            !------------------------------------------------------------------------------------------
            ! Dynamic integration step evaluation
            where( (oHMC_Vars(iID)%a2iChoice.eq.1) .and. (oHMC_Vars(iID)%a2dDem.gt.0.0) .and. (oHMC_Vars(iID)%a2dQC.gt.0.0) )
                a2dVarUcAct = oHMC_Vars(iID)%a2dQC/(oHMC_Vars(iID)%a2dWidthC*oHMC_Vars(iID)%a2dHydroC) !m/s
                a2dVarUDt = oHMC_Vars(iID)%a2dQC/(oHMC_Vars(iID)%a2dWidthC*oHMC_Vars(iID)%a2dHydroC) !m3/s
            elsewhere
                a2dVarUDt = 0.0
            endwhere
            !------------------------------------------------------------------------------------------
               
            !------------------------------------------------------------------------------------------
            ! Rain intensity max value
            dVarRainMax = maxval(maxval(a2dVarRain,DIM = 1),DIM = 1)*3600.0/dDtDataForcing
            ! Velocity max value
            dVarUDtMax = maxval(maxval(a2dVarUDt,DIM = 1),DIM = 1)
            if (dVarUDtMax.le.0.1) dVarUDtMax = 0.1
            ! Hydro max value
            dVarHydroMax = maxval(maxval(oHMC_Vars(iID)%a2dHydroC,DIM = 1),DIM = 1)
            !------------------------------------------------------------------------------------------

            !------------------------------------------------------------------------------------------
            ! First estimation of integration step
            dDtIntegrAct = dDEMStepMean/dVarUDtMax*0.6

            ! Checking Integration step value
            if (dDtIntegrAct.gt.dDtDataForcing/dDtRefRatio) then
                dDtIntegrAct = dDtDataForcing/dDtRefRatio
            endif
            if ( (dVarRainMax.gt.1.0) .and. (dDtIntegrAct.gt.dDtPhysConv) ) then
                dDtIntegrAct = dDtPhysConv*exp(-dVarRainMax/3.0) + dDtPhysConv
            endif
            if (dDtIntegrAct.lt.dDtIntegrMin) then
                dDtIntegrAct = dDtIntegrMin
            endif

            dDtIntegrAct = dDtDataForcing/int(dDtDataForcing/dDtIntegrAct)
            dDtIntegrAct = int(dDtIntegrAct/dDtRef)*dDtRef
            !------------------------------------------------------------------------------------------

            !------------------------------------------------------------------------------------------
            ! Checking if dt integration is too big of previous dt integration
            if(dDtIntegrAct.gt.dDtIntegrPStep + dDtRef) then
                dDtIntegrAct = dDtIntegrPStep + dDtRef
            endif
            !------------------------------------------------------------------------------------------

            !------------------------------------------------------------------------------------------
            ! Updating routing variable; intensity must be the same between steps
            a2dVarRouting = a2dVarRouting/dDtIntegrPStep*dDtIntegrAct
            !------------------------------------------------------------------------------------------

        else
            
            !------------------------------------------------------------------------------------------
            ! Actual temporal integration Step
            dDtIntegrAct = dDtIntegr
            !------------------------------------------------------------------------------------------
            
        endif
        !------------------------------------------------------------------------------------------
        
        !  dDtIntegrAct = 30 !!!DA LEVARE
        
        !------------------------------------------------------------------------------------------
        ! Temporal integration step info
        write(sDtIntegrStep, sFMTDtIntegrStep) int(dDtIntegrAct)
        write(sVarUDtMax, sFMTVarUDtMax) dVarUDtMax
        write(sVarRainMax, sFMTVarRainMax) dVarRainMax
        call mprintf(.true., iINFO_Basic, ' Phys :: Convolution :: IntegrationStep :: '//sDtIntegrStep//' [s]')
        call mprintf(.true., iINFO_Basic, ' Phys :: Convolution :: MaxValue :: '// &
                                          ' Uc: '//sVarUDtMax//' [m/s] '// &
                                          ' Rain: '//sVarRainMax//' [mm]')

        !------------------------------------------------------------------------------------------
                                        
        !------------------------------------------------------------------------------------------
        ! Passing variable(s) to global declaration
        oHMC_Vars(iID)%iDtIntegr = int(dDtIntegrAct)
        oHMC_Vars(iID)%iDtIntegrPStep = int(dDtIntegrAct)
        oHMC_Vars(iID)%a2dRouting = a2dVarRouting
        oHMC_Vars(iID)%a2dUcAct = a2dVarUcAct
        oHMC_Vars(iID)%a2dUDt = a2dVarUDt
        
        ! Info end
        if (iDEBUG.gt.0) then
            call mprintf(.true., iINFO_Extra, ' Phys :: Convolution :: IntegrationStep ... OK' )
        endif
        !------------------------------------------------------------------------------------------
        
    end subroutine HMC_Phys_Convolution_Apps_IntegrationStep_ChannelFraction
    !------------------------------------------------------------------------------------
        
end module HMC_Module_Phys_Convolution_Apps_IntegrationStep
!------------------------------------------------------------------------------------------
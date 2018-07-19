!------------------------------------------------------------------------------------
! File:   HMC_Module_Phys_Convolution_Apps.f90
!
! Author(s):    Fabio Delogu, Francesco Silvestro, Simone Gabellani
! Date:         20150212
!
! Convolution Apps subroutine(s) for HMC model
!------------------------------------------------------------------------------------

!------------------------------------------------------------------------------------
! Module Header
module HMC_Module_Phys_Convolution_Apps

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
    ! Subroutine to calculate dynamic integration step
    subroutine HMC_Phys_Convolution_Apps_IntegrationStep(iID, iRows, iCols, dDtDataForcing, dDtIntegrAct)
	
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
        ! Passing varible(s) to global declaration
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
        
    end subroutine HMC_Phys_Convolution_Apps_IntegrationStep
    !------------------------------------------------------------------------------------
    
    !------------------------------------------------------------------------------------------
    ! Subroutine for calculating convolution
    subroutine HMC_Phys_Convolution_Apps_DeepFlow(iID, iRows, iCols, dDtDataForcing)
        
        !------------------------------------------------------------------------------------------
        ! Variable(s) declaration
        integer(kind = 4)           :: iID
        integer(kind = 4)           :: iRows, iCols
        
        integer(kind = 4)           :: iI, iII, iIII, iJ, iJJ, iJJJ
        integer(kind = 4)           :: iFlagFlowDeep, iNgr
        real(kind = 4)              :: dHt, dHm
        real(kind = 4)              :: dDtDataForcing
        
        real(kind = 4), dimension (iRows, iCols)    :: a2dVarVTot, a2dVarVLoss, a2dVarFlowDeep
        real(kind = 4), dimension (iRows, iCols)    :: a2dVarDarcy, a2dVarHydro
        real(kind = 4), dimension (iRows, iCols)    :: a2dVarWSRunoff
        
        real(kind = 4), dimension (iRows, iCols)    :: a2dVarWTable, a2dVarWTableStep
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Initialization variable(s)
        a2dVarVTot = 0.0; a2dVarVLoss = 0.0; a2dVarFlowDeep = 0.0;
        a2dVarDarcy = 0.0; a2dVarHydro = 0.0;
        a2dVarWSRunoff = 0.0
        
        a2dVarWTable = 0.0; a2dVarWTableStep = 0.0; 
        
        iFlagFlowDeep = 0;
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Flow deep flag
        iFlagFlowDeep = oHMC_Namelist(iID)%iFlagFlowDeep 
                    
        ! Info start
        if (iDEBUG.gt.0) then
            call mprintf(.true., iINFO_Extra, ' Phys :: Convolution :: DeepFlow ... ' )
        endif
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Condition for activating flow deep process
        if (iFlagFlowDeep.eq.1) then
            
            !------------------------------------------------------------------------------------------
            ! Re-initializing flow deep 
            oHMC_Vars(iID)%a2dFlowDeep = 0.0
            oHMC_Vars(iID)%a2dDarcy = 0.0
            !------------------------------------------------------------------------------------------
            
            !------------------------------------------------------------------------------------------
            ! Temporal step of model and forcing data
            !iT = oHMC_Vars(iID)%iTime
            !dDt = real(oHMC_Namelist(iID)%iDtModel)
            !------------------------------------------------------------------------------------------

            !------------------------------------------------------------------------------------------
            ! Land data from global declaration
            a2dVarWTable = oHMC_Vars(iID)%a2dWTable
            !a2dVarHydro = oHMC_Vars(iID)%a2dHydro
            a2dVarVTot = oHMC_Vars(iID)%a2dVTot
            a2dVarVLoss = oHMC_Vars(iID)%a2dVLoss
            a2dVarWSRunoff = oHMC_Vars(iID)%a2dWSRunoff
            !------------------------------------------------------------------------------------------
            
            !-----------------------------------------------------------------------------------------
            ! Debug
            if (iDEBUG.gt.0) then
                call mprintf(.true., iINFO_Extra, ' ========= DEEPFLOW START =========== ')  
                call mprintf(.true., iINFO_Extra, checkvar(a2dVarFlowDeep, oHMC_Vars(iID)%a2iMask, 'FLOWDEEP START ') )
                call mprintf(.true., iINFO_Extra, checkvar(a2dVarVTot, oHMC_Vars(iID)%a2iMask, 'VTOT START ') )
                call mprintf(.true., iINFO_Extra, checkvar(a2dVarWTable, oHMC_Vars(iID)%a2iMask, 'WTABLE START ') )
                !call mprintf(.true., iINFO_Extra, checkvar(a2dVarHydro, oHMC_Vars(iID)%a2iMask, 'HYDRO START ') )
                call mprintf(.true., iINFO_Extra, ' ') 
            endif
            !-----------------------------------------------------------------------------------------
            
            !------------------------------------------------------------------------------------------
            ! Update WTable
            where( oHMC_Vars(iID)%a2dDem.gt.0.0 )
                a2dVarWTableStep = a2dVarWTable + a2dVarVLoss/1000
            endwhere
            !------------------------------------------------------------------------------------------

            !------------------------------------------------------------------------------------------
            ! Compute deep flow
            do iJ = 3, iCols - 1
                do iI = 3, iRows - 1
                    
                    ! Initialization cycle variable(s)
                    iNgr = 0
                    dHt = 0.0
                    dHm = 0.0
                    
                    ! Defining flow directions
                    iII = int((oHMC_Vars(iID)%a2iPNT(iI,iJ)  - 1)/3) - 1
                    iJJ = oHMC_Vars(iID)%a2iPNT(iI,iJ) - 5 - 3*iII
                    iIII = iI + iII
                    iJJJ = iJ + iJJ
                    
                    !write(*,*) 'PNT: ',iVarPNT   
                    !write(*,*) 'iJ: ',iJ, ' iI: ',iI, ' iJJ: ',iJJ, ' iII: ',iII , ' iJJJ: ',iJJJ, ' iIII: ',iIII
                    !------------------------------------------------------------------------------------------

                    !------------------------------------------------------------------------------------------
                    ! Terrain condition for i,j and iii,jjj 
                    if ( (oHMC_Vars(iID)%a2dDem(iI,iJ).gt.0.0 ) .and. (oHMC_Vars(iID)%a2dDem(iIII,iJJJ).gt.0.0) ) then

                        !------------------------------------------------------------------------------------------
                        ! Cycle(s) on buffer area
                        do iII = iI - 1, iI + 1
                            do iJJ = iJ - 1, iJ + 1

                                if ( (oHMC_Vars(iID)%a2dDem(iII,iJJ).gt.0.0 ) .and. ((iII.ne.iI).and.(iJJ.ne.iJ)) ) then

                                    if ( (a2dVarWTable(iI, iJ) - a2dVarWTable(iII,iJJ)).gt.0.0 ) then

                                        dHt = dHt + (a2dVarWTable(iI, iJ) - a2dVarWTable(iII, iJJ))
                                        iNgr = iNgr + 1

                                    endif

                                endif

                            enddo
                        enddo
                        !------------------------------------------------------------------------------------------
                        
                        !------------------------------------------------------------------------------------------
                        if (iNgr.gt.0) then

                            !------------------------------------------------------------------------------------------
                            dHm = dHt/iNgr
                            
                            a2dVarDarcy(iI,iJ) = dHm/sqrt(oHMC_Vars(iID)%a2dAreaCell(iI,iJ)) * &
                                                 oHMC_Vars(iID)%a2dCostF1(iI,iJ) * &
                                                 dDtDataForcing/3600*oHMC_Namelist(iID)%dKSatRatio
                                               
                            if ( a2dVarDarcy(iI,iJ) .gt. ( a2dVarWTable(iI,iJ) - oHMC_Vars(iID)%a2dWTableMax(iI,iJ))*1000 ) then
                                a2dVarDarcy(iI,iJ) = (a2dVarWTable(iI,iJ) - oHMC_Vars(iID)%a2dWTableMax(iI,iJ))*1000
                            endif
                            !------------------------------------------------------------------------------------------
                            
                            !------------------------------------------------------------------------------------------
                            do iII = iI - 1, iI + 1
                                do iJJ = iJ - 1, iJ + 1

                                    if ( (oHMC_Vars(iID)%a2dDem(iII, iJJ).gt.0.0) .and. ((iII.ne.iI).and.(iJJ.ne.iJ)) ) then
                                        if ( (a2dVarWTable(iI,iJ) - a2dVarWTable(iII,iJJ)).gt.0.0 ) then

                                            a2dVarWTableStep(iII, iJJ) = a2dVarWTableStep(iII, iJJ) + & 
                                                                         a2dVarDarcy(iI,iJ)*(a2dVarWTable(iI, iJ) - &
                                                                         a2dVarWTable(iII,iJJ))/(dHt*1000)
                                            dHm = dHm

                                        endif
                                    endif
                                    
                                enddo
                            enddo
                            !------------------------------------------------------------------------------------------
                            
                        endif
                        !------------------------------------------------------------------------------------------

                    endif
                    !------------------------------------------------------------------------------------------
                    
                    !------------------------------------------------------------------------------------------
                    ! Outlet cell
                    if ( (oHMC_Vars(iID)%a2dDem(iI,iJ).gt.0.0) .and. (oHMC_Vars(iID)%a2dDem(iIII, iJJJ).lt.0.0) ) then
                        
                        a2dVarDarcy(iI, iJ) = oHMC_Vars(iID)%a2dAlpha(iI,iJ)*oHMC_Vars(iID)%a2dCostF1(iI,iJ)* &
                                              dDtDataForcing/(3600*1000)*oHMC_Namelist(iID)%dKSatRatio

                        if ( a2dVarDarcy(iI,iJ) .gt. (a2dVarWTable(iI,iJ) - oHMC_Vars(iID)%a2dWTableMax(iI,iJ)) ) then
                            a2dVarDarcy(iI,iJ) = (a2dVarWTable(iI,iJ) - oHMC_Vars(iID)%a2dWTableMax(iI,iJ))
                        endif

                    endif
                    !------------------------------------------------------------------------------------------

                enddo
            enddo
            !------------------------------------------------------------------------------------------

            !------------------------------------------------------------------------------------------
            where( oHMC_Vars(iID)%a2dDem.gt.0.0 )
                a2dVarWTableStep = a2dVarWTableStep - a2dVarDarcy/1000
            endwhere
            !------------------------------------------------------------------------------------------
            
            !------------------------------------------------------------------------------------------
            ! Flow deep - Interaction between watertable and surface
            where( (oHMC_Vars(iID)%a2dDem.gt.0.0) .and. (a2dVarWTableStep.gt.oHMC_Vars(iID)%a2dDem) )
                a2dVarFlowDeep = (a2dVarWTableStep - oHMC_Vars(iID)%a2dDem)*dDtDataForcing/3600*1000
                a2dVarWTableStep = oHMC_Vars(iID)%a2dDem
            endwhere

            ! Updating watertable
            where( oHMC_Vars(iID)%a2dDem.gt.0.0 )
                a2dVarWTable = a2dVarWTableStep
            endwhere
            
            a2dVarWSRunoff = 0.0
            where( oHMC_Vars(iID)%a2dDem.gt.0.0 )
                a2dVarWSRunoff = (a2dVarWTable - oHMC_Vars(iID)%a2dWTableMax)*oHMC_Vars(iID)%a2dCoeffWS*oHMC_Vars(iID)%a2dAreaCell ! m^3/s
                a2dVarWTable = a2dVarWTable - (a2dVarWTable - oHMC_Vars(iID)%a2dWTableMax)*oHMC_Vars(iID)%a2dCoeffWS*dDtDataForcing ! m
            endwhere
            
            ! Updating VTot
            where( oHMC_Vars(iID)%a2dDem.gt.0.0 )
                a2dVarVTot = a2dVarVTot + a2dVarFlowDeep
            endwhere
            
            ! Updating flow deep and vtot where vtot >= vmax
            a2dVarFlowDeep = 0.0 
            where( (oHMC_Vars(iID)%a2dDem.gt.0.0) .and. (a2dVarVTot.gt.oHMC_Vars(iID)%a2dS) )
                a2dVarFlowDeep = a2dVarVTot - oHMC_Vars(iID)%a2dS
                a2dVarVTot = oHMC_Vars(iID)%a2dS
            endwhere
            !------------------------------------------------------------------------------------------
            
            !-----------------------------------------------------------------------------------------
            ! Debug
            if (iDEBUG.gt.0) then
                call mprintf(.true., iINFO_Extra, ' ') 
                call mprintf(.true., iINFO_Extra, checkvar(a2dVarFlowDeep, oHMC_Vars(iID)%a2iMask, 'FLOWDEEP END ') )
                call mprintf(.true., iINFO_Extra, checkvar(a2dVarVTot, oHMC_Vars(iID)%a2iMask, 'VTOT END ') )
                call mprintf(.true., iINFO_Extra, checkvar(a2dVarWTable, oHMC_Vars(iID)%a2iMask, 'WTABLE END ') )
                !call mprintf(.true., iINFO_Extra, checkvar(a2dVarHydro, oHMC_Vars(iID)%a2iMask, 'HYDRO END ') )
                call mprintf(.true., iINFO_Extra, ' ========= DEEPFLOW END =========== ') 
            endif
            !-----------------------------------------------------------------------------------------
            
        else
            
            !-----------------------------------------------------------------------------------------
            ! Deep flow process not activated
            if (iDEBUG.gt.0) then
                call mprintf(.true., iINFO_Extra, ' Phys :: Convolution :: DeepFlow ... Skipped!' )
            endif
            a2dVarVTot = oHMC_Vars(iID)%a2dVTot
            a2dVarFlowDeep = 0.0
            a2dVarWSRunoff = 0.0
            !-----------------------------------------------------------------------------------------
            
        endif
        !-----------------------------------------------------------------------------------------

        !-----------------------------------------------------------------------------------------
        ! Updating model global variable(s)
        oHMC_Vars(iID)%a2dFlowDeep = a2dVarFlowDeep
        oHMC_Vars(iID)%a2dVTot = a2dVarVTot
        oHMC_Vars(iID)%a2dWTable = a2dVarWTable
        oHMC_Vars(iID)%a2dWSRunoff = a2dVarWSRunoff
        
        !oHMC_Vars(iID)%a2dHydro = a2dVarHydro

        ! Info end
        if (iDEBUG.gt.0) then
            call mprintf(.true., iINFO_Extra, ' Phys :: Convolution :: DeepFlow ... OK' )
        endif
        !-----------------------------------------------------------------------------------------

    end subroutine HMC_Phys_Convolution_Apps_DeepFlow
    !------------------------------------------------------------------------------------------

    !------------------------------------------------------------------------------------
    ! Subroutine for calculating discharge
    subroutine HMC_Phys_Convolution_Apps_Discharge(iID, iRows, iCols, iNSection, &
                                                   dDtDataForcing, dDtAct, iNTime, iTq, dDtMax)
        
        !------------------------------------------------------------------------------------------
        ! Variable(s) declaration
        integer(kind = 4)   :: iID, iRows, iCols, iTAct
        real(kind = 4)      :: dDtDataForcing, dDtMax, dDtAct, dDtDischarge
        
        integer(kind = 4), intent(inout)    :: iNTime, iTq  
        integer(kind = 4)                   :: iI, iJ, iT, iS, iD
        integer(kind = 4)                   :: iNSection
        
        !integer(kind = 4)   :: iFlagVarUc
        real(kind = 4)      :: dRate
        
        !real(kind = 4), dimension (iRows, iCols)          :: a2dVarHydroPrev, a2dVarHydro, a2dVarIntensity
        real(kind = 4), dimension (iRows, iCols)          :: a2dVarQDisOut, a2dVarQVolOut, a2dVarQTot, a2dVarQout

        real(kind = 4), dimension (iNSection)             :: a1dVarQoutSection
        
        character(len = 10)                               :: sVarQoutSectionFirst, sVarQoutSectionMax
        character(len = 10), parameter                    :: sFMTVarQoutSection = "(F10.2)"
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Variable(s) initialization 
        a2dVarQDisOut = 0.0; a2dVarQVolOut = 0.0; a2dVarQTot = 0.0; a2dVarQout = 0.0; 

        a1dVarQoutSection = 0.0; 
        
        ! Re-initializing Q distributed discharge matrix (in global memory)
        oHMC_Vars(iID)%a2dQout = 0.0;           ! Distributed discharge
        oHMC_Vars(iID)%a1dQoutSection = 0.0;    ! Section discharge OUT
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Flags
        !iFlagVarUc = oHMC_Namelist(iID)%iFlagVarUc

        ! Temporal model step
        iT = int(oHMC_Vars(iID)%iTime)
        
        ! Variable(s) time dependent from global declaration
        a2dVarQDisOut = oHMC_Vars(iID)%a2dQDisOut ! Qout
        a2dVarQVolOut = oHMC_Vars(iID)%a2dQVolOut ! Qtmp
        a2dVarQTot = oHMC_Vars(iID)%a2dQTot
        a2dVarQout = oHMC_Vars(iID)%a2dQout

        a1dVarQoutSection = oHMC_Vars(iID)%a1dQoutSection
        
        ! Info start
        if (iDEBUG.gt.0) then
            call mprintf(.true., iINFO_Extra, ' Phys :: Convolution :: Discharge ... ' )
        endif
        !------------------------------------------------------------------------------------------
       
        !------------------------------------------------------------------------------------------
        ! Debug
        if (iDEBUG.gt.0) then
            call mprintf(.true., iINFO_Extra, ' ========= DISCHARGE START =========== ') 
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarQDisOut, oHMC_Vars(iID)%a2iMask, 'QDIS OUT START (Qout)') )
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarQVolOut, oHMC_Vars(iID)%a2iMask, 'QVOL OUT START (Qtmp)') )
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarQTot, oHMC_Vars(iID)%a2iMask, 'QTOT START (Q)') ) 
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarQout, oHMC_Vars(iID)%a2iMask, 'QOUT START (Qmap)') )  
            call mprintf(.true., iINFO_Extra, checkvar(oHMC_Vars(iID)%a2dUc, oHMC_Vars(iID)%a2iMask, 'UC ') )
            call mprintf(.true., iINFO_Extra, ' ') 
        endif
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Integrating step (SurfaceFlow)
        dDtDischarge = dDtAct
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Discharge in channel cells [mm/s]
        where( (oHMC_Vars(iID)%a2iChoice.eq.1) .and. (oHMC_Vars(iID)%a2dDEM.gt.0.0) )
            a2dVarQTot = a2dVarQTot + a2dVarQVolOut
        endwhere

        ! Discharge in hills cells [mm/s]
        where( (oHMC_Vars(iID)%a2iChoice.eq.0) .and. (oHMC_Vars(iID)%a2dDEM.gt.0.0) )
            a2dVarQTot = a2dVarQTot + a2dVarQVolOut
        endwhere
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Compute discharge from mm/s to m^3/s (constant is in 1/h)
        if( (real(iTq)*dDtDischarge) .ge. (dDtDataForcing - dDtMax*1.001) ) then ! 1.001 for numerical approx
            
            !------------------------------------------------------------------------------------------
            ! Compute distributed discharge (total and for each step)
            where( oHMC_Vars(iID)%a2dDEM.gt.0.0 )
                a2dVarQTot = a2dVarQTot/(real(iTq)*1000)*oHMC_Vars(iID)%a2dAreaCell
                a2dVarQout = a2dVarQTot                                
            endwhere
            !------------------------------------------------------------------------------------------
            
            !------------------------------------------------------------------------------------------
            ! Calculating discharge in selected outlet(s)
            a1dVarQoutSection = 0.0
            do iS = 1, iNSection
                
                ! Extracting outlet(s) indexes
                iI = 0; iJ = 0;
                iI = oHMC_Vars(iID)%a2iXYSection(iS, 2); iJ = oHMC_Vars(iID)%a2iXYSection(iS, 1)
                
                ! Get section discharge
                a1dVarQoutSection(iS) =  a2dVarQTot(iI, iJ)
               
                ! Subsurface flux
                dRate = sin(oHMC_Vars(iID)%a2dBeta(iI, iJ))
                
                if (dRate.gt.1) dRate = 0.99
                if (dRate.lt.0) dRate = 0.1
                
                if (oHMC_Vars(iID)%a2dWTableMax(iI,iJ).eq.0.0) dRate = 1.0 !Celle dove non ho falda
        
            enddo
                        
            ! Re-initialize qtot array and q counter
            !write(*,*) a1dVarQoutSection; write(*,*) a1dVarQinDam
            a2dVarQTot = 0.0 
            iTq = 0
            !------------------------------------------------------------------------------------------
            
            !------------------------------------------------------------------------------------------
            ! Discharge information time step
            write(sVarQoutSectionFirst, sFMTVarQoutSection) a1dVarQoutSection(1)
            write(sVarQoutSectionMax, sFMTVarQoutSection) maxval(a1dVarQoutSection)
            call mprintf(.true., iINFO_Basic, ' Phys :: Convolution :: Discharge ::'// &
                                  ' Q SecFirst: '//sVarQoutSectionFirst//' [m^3/s]'// &
                                  ' Q SecMax: '//sVarQoutSectionMax//' [m^3/s]')
            !------------------------------------------------------------------------------------------
                    
        endif
        !------------------------------------------------------------------------------------------
               
        !------------------------------------------------------------------------------------------
        ! Debug
        if (iDEBUG.gt.0) then
            call mprintf(.true., iINFO_Extra, ' ') 
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarQDisOut, oHMC_Vars(iID)%a2iMask, 'QDIS OUT END (Qout)') )
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarQVolOut, oHMC_Vars(iID)%a2iMask, 'QVOL OUT END (Qtmp)') )
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarQTot, oHMC_Vars(iID)%a2iMask, 'QTOT END (Q)') )  
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarQout, oHMC_Vars(iID)%a2iMask, 'QOUT END (Qmap)') )  
            call mprintf(.true., iINFO_Extra, ' ========= DISCHARGE END =========== ') 
        endif
        !------------------------------------------------------------------------------------------
     
        !------------------------------------------------------------------------------------------
        ! Updating model global variable(s)
        oHMC_Vars(iID)%a2dQTot = a2dVarQTot    
        oHMC_Vars(iID)%a2dQout = a2dVarQout   ! Distributed discharge
        oHMC_Vars(iID)%a1dQoutSection = a1dVarQoutSection   ! Section discharge OUT
        
        ! Info end
        if (iDEBUG.gt.0) then
            call mprintf(.true., iINFO_Extra, ' Phys :: Convolution :: Discharge ... OK' )
        endif
        !------------------------------------------------------------------------------------------
        
    end subroutine HMC_Phys_Convolution_Apps_Discharge
    !------------------------------------------------------------------------------------------
        
    !------------------------------------------------------------------------------------------
    ! Subroutine for calculating surface flow
    subroutine HMC_Phys_Convolution_Apps_SurfaceFlow(iID, iRows, iCols, &
                                                     dDtDataForcing, dDtAct, iTAct, iTq, iDtMax, &
                                                     iNData, iNDam, iNLake, & 
                                                     iNPlant, iNCatch, iNRelease, iNJoint, &
                                                     iTime, iNTime, iETime)
        
        !------------------------------------------------------------------------------------------
        ! Variable(s) declaration
        integer(kind = 4)   :: iID, iRows, iCols, iTAct, iTq, iDtMax
        integer(kind = 4)   :: iNData, iNDam, iNLake, iNPlant, iNCatch, iNRelease, iNJoint
        integer(kind = 4)   :: iShift, iRank
        real(kind = 4)      :: dDtDataForcing, dDtAct
        
        integer(kind = 4)   :: iTTemp
        integer(kind = 4)   :: iI, iII, iIII, iJ, iJJ, iJJJ, iP, iR, iD, iL, iC
        integer(kind = 4)   :: iIm, iJm, iIin, iJin, iIout, iJout
        real(kind = 4)      :: dHm, dHin
        real(kind = 4)      :: dDtSurfaceFlow
        
        integer(kind = 4)   :: iFlagReleaseMass
        integer(kind = 4)   :: iVarPNT
        real(kind = 4)      :: dUMax
        real(kind = 4)      :: dRm, dBc
        real(kind = 4)      :: dVLake, dDh, dDhPrev, dRoutPrev
        real(kind = 4)      :: dQt, dHinFD
        real(kind = 4)      :: dVarAreaCell, dVarTcCatch, dVarQMinCatch
        
        real(kind = 4), dimension (iRows, iCols)        :: a2dVarIntensityPrev, a2dVarIntensityUpd
        real(kind = 4), dimension (iRows, iCols)        :: a2dVarHydroPrev, a2dVarHydroUpd
        
        real(kind = 4), dimension (iRows, iCols)        :: a2dVarFlowExf
        real(kind = 4), dimension (iRows, iCols)        :: a2dVarUcAct, a2dVarUhAct
        real(kind = 4), dimension (iRows, iCols)        :: a2dVarQDisOut, a2dVarQVolOut
        real(kind = 4), dimension (iRows, iCols)        :: a2dVarRouting
        
        real(kind = 4), dimension (iNDam)               :: a1dVarVDam, a1dVarHDam, a1dVarLDam, a1dVarCoeffDam, a1dVarQoutDam
      
        real(kind = 4), dimension (iNLake)              :: a1dVarVLake

        real(kind = 4), dimension (iNPlant)             :: a1dVarQPlant
           
        real(kind = 4), dimension (iNPlant, iETime)     :: a2dVarHydroPlant
        real(kind = 4), dimension (iNCatch, iETime)     :: a2dVarHydroCatch
        real(kind = 4), dimension (iNRelease, iETime)   :: a2dVarHydroRelease
        
        integer(kind=4) :: iTime, iNTime, iETime
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Variable(s) initialization 
        a2dVarIntensityPrev = 0.0; a2dVarIntensityUpd = 0.0;
        
        a2dVarHydroPrev = 0.0; a2dVarHydroUpd = 0.0;

        a2dVarUcAct = 0.0; a2dVarUhAct = 0.0; 
        a2dVarFlowExf = 0.0; a2dVarRouting = 0.0;
        
        a2dVarQDisOut = 0.0 ! Outgoing discharge in m^3/s from each cell
        a2dVarQVolOut = 0.0 ! Outgoing discharge in volume from each cell
        
        a1dVarVDam = 0.0; a1dVarHDam = 0.0; a1dVarLDam = 0.0; a1dVarCoeffDam = 0.0; a1dVarQoutDam = 0.0
        
        a1dVarVLake = 0.0;
        
        a1dVarQPlant = 0.0; 

        a2dVarHydroPlant = 0.0;
        a2dVarHydroCatch = 0.0;
        a2dVarHydroRelease = 0.0;

        ! Null global variable(s)
        oHMC_Vars(iID)%a2dQVolOut = 0.0 ! Initialize each step (Portata in volume in uscita da una cella == Qtmp)
        oHMC_Vars(iID)%a2dQDisOut = 0.0 ! Initialize each step (Portata in uscita da una cella == Qout)
        
        oHMC_Vars(iID)%a1dQoutDam = 0.0
        
        !oHMC_Vars(iID)%a2dHydroPrev = 0.0 ! Initialize each step
        dDh = 0.0
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Temporal step 
        iTTemp = int(iTAct)
        
        ! Integrating step (SurfaceFlow)
        dDtSurfaceflow = dDtAct
        
        ! Variable(s) time dependent from global declaration
        a2dVarFlowExf = oHMC_Vars(iID)%a2dFlowExf        
        a2dVarHydroPrev = oHMC_Vars(iID)%a2dHydro
        a2dVarIntensityPrev = oHMC_Vars(iID)%a2dIntensity
        a2dVarRouting = oHMC_Vars(iID)%a2dRouting

        ! Dam variable(s)
        a1dVarVDam = oHMC_Vars(iID)%a1dVDam
        a1dVarHDam = oHMC_Vars(iID)%a1dHDam
        a1dVarLDam = oHMC_Vars(iID)%a1dLDam
        a1dVarCoeffDam = oHMC_Vars(iID)%a1dCoeffDam

        ! Lake variable(s)
        a1dVarVLake = oHMC_Vars(iID)%a1dVLake
        
        ! Plant data variable(s)
        a2dVarHydroPlant = oHMC_Vars(iID)%a2dHydroPlant
        ! Catch data variable(s)
        a2dVarHydroCatch = oHMC_Vars(iID)%a2dHydroCatch
        ! Release data variable(s)
        a2dVarHydroRelease = oHMC_Vars(iID)%a2dHydroRelease
        
        ! Exponent of dUcAct formula
        dBc = oHMC_Namelist(iID)%dBc
        
        ! Activate/Deactivate release mass update
        iFlagReleaseMass = oHMC_Namelist(iID)%iFlagReleaseMass
        
        ! Info start
        if (iDEBUG.gt.0) then
            call mprintf(.true., iINFO_Extra, ' Phys :: Convolution :: SurfaceFlow ... ' )
        endif
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Debug
        if (iDEBUG.gt.0) then
            call mprintf(.true., iINFO_Extra, ' ========= SURFACE FLOW START ========= ') 
            call mprintf(.true., iINFO_Extra, checkvar(oHMC_Vars(iID)%a2dAreaCell, oHMC_Vars(iID)%a2iMask, 'AREACELL') )
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarHydroUpd, oHMC_Vars(iID)%a2iMask, 'HYDRO UPD START') )
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarHydroPrev, oHMC_Vars(iID)%a2iMask, 'HYDRO PREV START') )
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarQDisOut, oHMC_Vars(iID)%a2iMask, 'QDIS OUT START (Qout)') )
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarQVolOut, oHMC_Vars(iID)%a2iMask, 'QVOL OUT START (Qtmp)') )
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarRouting, oHMC_Vars(iID)%a2iMask, 'ROUTING START') )
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarIntensityPrev, oHMC_Vars(iID)%a2iMask, 'INTENSITY START') )
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarFlowExf, oHMC_Vars(iID)%a2iMask, 'EXFILTRATION START') )
            call mprintf(.true., iINFO_Extra, checkarray(a2dVarHydroPlant(:,2), 'HYDRO PLANT START') )
            call mprintf(.true., iINFO_Extra, checkarray(a2dVarHydroCatch(:,2), 'HYDRO CATCH START') )
            call mprintf(.true., iINFO_Extra, checkarray(a2dVarHydroRelease(:,2), 'HYDRO RELEASE START') )
            call mprintf(.true., iINFO_Extra, ' ') 
        endif
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Channel max surface velocity (UcMax)
        dUMax = 3600.0/dDtSurfaceflow*0.5
        
        ! Hill overland equation 
        a2dVarUhAct = oHMC_Vars(iID)%a2dUh
        where ( (oHMC_Vars(iID)%a2iChoice.eq.0) .and. (a2dVarUhact.gt.dUMax))  ! numerical check
            a2dVarUhAct = dUMax
        endwhere
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Hydro variable (previous and update array)
        where (a2dVarHydroPrev .lt. 0.0)
            a2dVarHydroPrev = 0.0000001
        endwhere        
        where (a2dVarHydroPrev .gt. 100000.0)
            a2dVarHydroPrev = 0.0000001
        endwhere
        
        ! Updating hydro variable (using previous step and checking values under zero) --> WaterLevel (tirante)
        a2dVarHydroUpd = a2dVarHydroPrev
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Routing variable
        where( a2dVarRouting .lt. 0.0)
            a2dVarRouting = 0.0
        endwhere
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Intensity variable (previous and update) 
        where(a2dVarIntensityPrev.lt.0.0)
            a2dVarIntensityPrev = 0.0
        endwhere
        ! Updating variables surface cell input (exfiltration + runoff [mm/h]) --> CHECKING CONVERSION
        where(oHMC_Vars(iID)%a2dDEM.gt.0.0)
            a2dVarIntensityUpd = a2dVarIntensityPrev + a2dVarFlowExf*1000.0*3600.0 + &
                                 (1 - oHMC_Vars(iID)%a2dCoeffResol)*a2dVarRouting/dDtSurfaceflow*3600.0 + &
                                 !(1 - oHMC_Vars(iID)%a2dCoeffResol)*oHMC_Vars(iID)%a2dFlowDeep + &      ! Tevere settings ( --- development mode ---)
                                 (oHMC_Vars(iID)%a2dWSRunoff*1000.0*3600.0)/oHMC_Vars(iID)%a2dAreaCell   ! WSRunoff from m^3/s to mm/h            
        endwhere
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Check plant(s) availability
        if (iNPlant .gt. 0) then
            
            !------------------------------------------------------------------------------------------
            ! Cycle on plant(s)
            iP = 0;
            do iP = 1, iNPlant
                
                !------------------------------------------------------------------------------------------
                ! Get plant information
                iI = 0; iJ = 0;
                iI = oHMC_Vars(iID)%a2iXYPlant(iP, 2); 
                iJ = oHMC_Vars(iID)%a2iXYPlant(iP, 1);

                dVarAreaCell = oHMC_Vars(iID)%a2dAreaCell(iI, iJ)
               
                ! Check area cell
                if (dVarAreaCell.lt.0) then
                    dVarAreaCell = oHMC_Vars(iID)%dDxM*oHMC_Vars(iID)%dDyM
                endif
                !------------------------------------------------------------------------------------------
                
                !------------------------------------------------------------------------------------------
                ! Add plant data to intensity and updata dam volume
                if (a2dVarHydroPlant(iP, iTTemp + 1) .ge. 0) then !Se ho le turbinate (si presume che i tempi siano relativi all'immissione)
                    
                    !------------------------------------------------------------------------------------------
                    ! Update intensity
                    a2dVarIntensityUpd(iI, iJ) = a2dVarIntensityUpd(iI, iJ) + a2dVarHydroPlant(iP, iTTemp + 1)

                    ! Update dam volume
                    a1dVarVDam(oHMC_Vars(iID)%a1iFlagDamPlant(iP)) = a1dVarVDam(oHMC_Vars(iID)%a1iFlagDamPlant(iP)) - &
                        a2dVarHydroPlant(iP, iTTemp + 1)*(dVarAreaCell)/(1000*3600)*dDtSurfaceflow !m^3
                    !------------------------------------------------------------------------------------------
                        
                else
                    
                    !------------------------------------------------------------------------------------------
                    ! Compute plant max discharge (transform to mm/h)
                    if ( a1dVarVDam(oHMC_Vars(iID)%a1iFlagDamPlant(iP)) .lt. &
                         oHMC_Vars(iID)%a1dVMaxDam(oHMC_Vars(iID)%a1iFlagDamPlant(iP)) )then
                        
                        ! VDam < VDamMax --> Q**6;
                        a1dVarQPlant(iP) = oHMC_Vars(iID)%a1dQMaxPlant(iP)* &
                                           (a1dVarVDam(oHMC_Vars(iID)%a1iFlagDamPlant(iP))/ &
                                            oHMC_Vars(iID)%a1dVMaxDam(oHMC_Vars(iID)%a1iFlagDamPlant(iP)))**6
                                            
                    else
                        ! VDam => VDamMax --> QMax
                        a1dVarQPlant(iP) = oHMC_Vars(iID)%a1dQMaxPlant(iP)
                    endif

                    ! Check plant discharge
                    if (a1dVarQPlant(iP) .lt. 0.0) a1dVarQPlant(iP) = 0.0
                    !------------------------------------------------------------------------------------------
                    
                    !------------------------------------------------------------------------------------------
                    ! Update intensity
                    a2dVarIntensityUpd(iI, iJ) = a2dVarIntensityUpd(iI, iJ) + &
                                                 a1dVarQPlant(iP)*1000*3600/(dVarAreaCell)

                    ! Update dam volume
                    a1dVarVDam(oHMC_Vars(iID)%a1iFlagDamPlant(iP)) = a1dVarVDam(oHMC_Vars(iID)%a1iFlagDamPlant(iP)) - &
                                                                     a1dVarQPlant(iP)*dDtSurfaceflow
                    
                    !write(*,*) a1dVarVDam(oHMC_Vars(iID)%a1iFlagDamPlant(iP)), a1dVarQPlant(iP)
                    !------------------------------------------------------------------------------------------
                                                                     
                endif
                !------------------------------------------------------------------------------------------
                
                !------------------------------------------------------------------------------------------
                ! Check dam volume
                if(a1dVarVDam(oHMC_Vars(iID)%a1iFlagDamPlant(iP)) .lt. 0.0) a1dVarVDam(oHMC_Vars(iID)%a1iFlagDamPlant(iP)) = 0.0
                !------------------------------------------------------------------------------------------
                
            enddo 
            !------------------------------------------------------------------------------------------
        
        endif
        !------------------------------------------------------------------------------------------ 
        
        !------------------------------------------------------------------------------------------ 
        ! Check release(s) availability
        if (iNRelease .gt. 0) then
            
            !------------------------------------------------------------------------------------------
            ! Cycle on release(s)
            iR = 0;
            do iR = 1, iNRelease
                
                !------------------------------------------------------------------------------------------
                ! Get plant information
                iI = 0; iJ = 0;
                iI = oHMC_Vars(iID)%a2iXYRelease(iR, 2); iJ = oHMC_Vars(iID)%a2iXYRelease(iR, 1);
                !------------------------------------------------------------------------------------------
                
                !------------------------------------------------------------------------------------------
                ! Check hydro release(s) data 
                if (a2dVarHydroRelease(iR,iTTemp + 1) .lt. 0.0) a2dVarHydroRelease(iR,iTTemp + 1) = 0.0
                ! Update intensity using hydro release
                a2dVarIntensityUpd(iI, iJ) = a2dVarIntensityUpd(iI, iJ) + a2dVarHydroRelease(iR,iTTemp + 1)
                !------------------------------------------------------------------------------------------
                
            enddo
            !------------------------------------------------------------------------------------------
            
        endif
        !------------------------------------------------------------------------------------------ 
        
        !------------------------------------------------------------------------------------------
        ! Surface Routing
        
        ! HILLS
        ! Surface equation for hills (direct euler's method)
        where ( (oHMC_Vars(iID)%a2iChoice.eq.0.0) .and. (oHMC_Vars(iID)%a2dDEM.gt.0.0) ) 
            a2dVarHydroUpd = a2dVarHydroUpd + a2dVarIntensityUpd*dDtSurfaceflow/3600 - &
                             a2dVarHydroUpd*a2dVarUhAct*dDtSurfaceflow/3600.0
            a2dVarQDisOut = a2dVarHydroPrev*a2dVarUhAct*dDtSurfaceflow/3600.0
        endwhere
        
        ! CHANNELS
        ! Surface equation for channels (direct euler's method)
        where ( (oHMC_Vars(iID)%a2iChoice.eq.1.0) .and. (oHMC_Vars(iID)%a2dDEM.gt.0.0) ) 

            a2dVarUcAct = oHMC_Vars(iID)%a2dUc*(tan(oHMC_Vars(iID)%a2dBeta)**0.5)*a2dVarHydroUpd**dBc ! FPI settings
            
            where (a2dVarUcAct.gt.dUMax)
                a2dVarUcAct = dUMax
            endwhere

            a2dVarQDisOut = a2dVarHydroUpd*a2dVarUcAct*dDtSurfaceflow/3600.0

            ! Surface tank equation (runoff with routing + exfiltration) 
            a2dVarHydroUpd = a2dVarHydroUpd + a2dVarIntensityUpd*dDtSurfaceflow/3600 - &
                          a2dVarHydroUpd*a2dVarUcAct*dDtSurfaceflow/3600.0

        endwhere
        
        where ( (oHMC_Vars(iID)%a2iChoice.eq.1.0) .and. (oHMC_Vars(iID)%a2dDEM.gt.0.0) ) 

            a2dVarQDisOut = oHMC_Vars(iID)%a2dUc*(tan(oHMC_Vars(iID)%a2dBeta)**0.5)*(0.5*a2dVarHydroPrev**(1 + dBc) + &
                            0.5*a2dVarHydroUpd**(1 + dBc))*dDtSurfaceflow/3600
                          
            where (a2dVarQDisOut .gt. (a2dVarHydroPrev + a2dVarIntensityUpd*dDtSurfaceflow/3600)*0.7)
                a2dVarQDisOut = (a2dVarHydroPrev + a2dVarIntensityUpd*dDtSurfaceflow/3600)*0.7
            endwhere

            a2dVarHydroUpd = a2dVarHydroPrev + a2dVarIntensityUpd*dDtSurfaceflow/3600 - a2dVarQDisOut
           
        endwhere
        !------------------------------------------------------------------------------------------

        !------------------------------------------------------------------------------------------
        ! Check dam availability
        if (iNDam .gt. 0) then
            ! Cycle on dam(s)
            iD = 0;
            do iD = 1, iNDam
                
                iI = 0; iJ = 0;
                iI = oHMC_Vars(iID)%a2iXYDam(iD, 2); iJ = oHMC_Vars(iID)%a2iXYDam(iD, 1);
                
                ! Lake condition
                if (oHMC_Vars(iID)%a1dCodeDam(iD) .gt. 0.0) then
                    ! Distributed lake
                    dVLake = 0.0;
                    dVLake = sum(sum(a2dVarIntensityUpd, dim=1, mask=oHMC_Vars(iID)%a2iChoice.eq.oHMC_Vars(iID)%a1dCodeDam(iD)))
                    a1dVarVDam(iD) = a1dVarVDam(iD) + dVLake*dDtSurfaceflow/(3600*1000)*(oHMC_Vars(iID)%a2dAreaCell(iI, iJ)) ! in m^3
                else
                    ! Punctual lake
                    a1dVarVDam(iD) = a1dVarVDam(iD) + a2dVarQDisOut(iI, iJ)/1000*(oHMC_Vars(iID)%a2dAreaCell(iI, iJ))
                endif
                
                a2dVarQDisOut(iI, iJ) = 0.0
                
            enddo
        endif
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Check lake availability
        if (iNLake .gt. 0) then
            ! Cycle on lake
            iL = 0;
            do iL = 1, iNLake
                iI = 0; iJ = 0;
                iI = oHMC_Vars(iID)%a2iXYLake(iL, 2); iJ = oHMC_Vars(iID)%a2iXYLake(iL, 1);
                
                if (oHMC_Vars(iID)%a1dCodeLake(iL) .gt. 0) then
                    ! Distributed lake
                    dVLake = 0.0;
                    dVLake = sum(sum(a2dVarIntensityUpd, dim=1, mask=oHMC_Vars(iID)%a2iChoice.eq.oHMC_Vars(iID)%a1dCodeLake(iL)))
                    a1dVarVLake(iL) = a1dVarVLake(iL) + dVLake*dDtSurfaceflow/(3600*1000)*(oHMC_Vars(iID)%a2dAreaCell(iI, iJ)) ! in m^3
                else
                    ! Punctual lake
                    a1dVarVLake(iL) = a1dVarVLake(iL) + a2dVarQDisOut(iI, iJ)/1000*(oHMC_Vars(iID)%a2dAreaCell(iI, iJ))
                endif
		
                a2dVarQDisOut(iI, iJ) = 0.0
		
            enddo
        endif
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Flow part for following cell 
        ! In Horton this flow will add at rain rate in the following step

        ! Checking hydro variable
        where(a2dVarHydroUpd.lt.0.0)
            a2dVarHydroUpd = 0.0000001
        endwhere

        ! Calculating flow 
        ! Calcolo la porzione di acqua che va nella cella successiva
        ! Essa verr� sommata alla pioggia nella subroutine di Horton
        ! l'istante successivo
        a2dVarRouting = 0.0
        do iI = 1, iRows
            do iJ = 1, iCols 
                
                ! DEM condition
                if (oHMC_Vars(iID)%a2dDEM(iI,iJ).gt.0.0) then
                    
                    ! Rate and pointers definition
                    !iVarPNT = 0
                    !iVarPNT = int(oHMC_Vars(iID)%a2iPNT(iI,iJ))
                    
                    ! Defining flow directions
                    iII = int((int(oHMC_Vars(iID)%a2iPNT(iI,iJ))  - 1)/3) - 1
                    iJJ = int(oHMC_Vars(iID)%a2iPNT(iI,iJ)) - 5 - 3*iII
                    iIII = iI + iII
                    iJJJ = iJ + iJJ
                    
                    if ( (iIII.ge.1) .and. (iJJJ.ge.1) ) then
                        
                        ! Integrazione del routing in mm/passo_integrazione_del_routing
                        ! L'acqua viene mandata nella cella successiva e utilizzata nella Subrotine
                        ! Horton
                        dRm = 0.0;
                        dRm = a2dVarQDisOut(iI, iJ) ! Trapezi
                        

                        a2dVarRouting(iIII, iJJJ) = a2dVarRouting(iIII, iJJJ) + dRm  ![mm]
                        a2dVarQVolOut(iI, iJ) = dRm/dDtSurfaceflow	
                        
                    endif 
                endif   
            enddo
        enddo				
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Check catch availability
        if (iNCatch .gt. 0) then
            ! Cycle on catch(es) ---> subtract turbinate(s) from routing
            iC = 0;
            do iC = 1, iNCatch
                
                iI = 0; iJ = 0; dDh = 0.0
                iI = oHMC_Vars(iID)%a2iXYCatch(iC, 2); iJ = oHMC_Vars(iID)%a2iXYCatch(iC, 1);
                dVarTcCatch = oHMC_Vars(iID)%a1dTCorrCatch(iC)
                dVarQminCatch = oHMC_Vars(iID)%a1dQMinCatch(iC)
                
                dVarAreaCell = oHMC_Vars(iID)%a2dAreaCell(iI, iJ)
                
                ! Check area cell
                if (dVarAreaCell.lt.0) then
                    dVarAreaCell = oHMC_Vars(iID)%dDxM*oHMC_Vars(iID)%dDyM
                endif
                
                ! Compute minimum discharge
                dVarQminCatch = oHMC_Vars(iID)%a1dQMinCatch(iC)/dVarAreaCell*dDtSurfaceflow*1000 ! [mm]

                ! Compute h
                dDh = a2dVarHydroCatch(iC, iTTemp + 1)/dVarAreaCell*dDtSurfaceflow*1000 ! [mm]

                ! Defining flow directions
                iII = int((int(oHMC_Vars(iID)%a2iPNT(iI,iJ))  - 1)/3) - 1
                iJJ = int(oHMC_Vars(iID)%a2iPNT(iI,iJ)) - 5 - 3*iII
                iIII = iI + iII
                iJJJ = iJ + iJJ
                
                ! Index(es) not allowed
                dRoutPrev = 0.0
                if( (iIII.ge.1) .and. (iJJJ.ge.1) ) then
                    
                    dRoutPrev = a2dVarRouting(iIII, iJJJ) 
                    
                    ! Q less than QMV
                    if(a2dVarRouting(iIII, iJJJ).le.dVarQminCatch)then
                        dDh=0.0
                    ! Q larger than QMV
                    else
                        !Q-QMV < Qplant
                        if((a2dVarRouting(iIII, iJJJ)-dDh).lt.dVarQminCatch) then
                                dDh=a2dVarRouting(iIII, iJJJ)-dVarQminCatch				
                        endif
                    endif

                    !Q - QplantCorrected
                    a2dVarRouting(iIII, iJJJ)=a2dVarRouting(iIII, iJJJ)-dDh
                    
                    ! Update data release(s)
                    if (iFlagReleaseMass .eq. 1) then   ! activate/deactivate release mass update
                        if (iNCatch .eq. iNRelease) then
                            iShift = nint(dVarTcCatch*60/dDtDataForcing)
                            iRank = size(a2dVarHydroRelease, dim = 2)

                            if (iTTemp + iShift .lt. iRank ) then
                                a2dVarHydroRelease(iC, iTTemp + 1) = dDh/dDtSurfaceflow*3600
                            endif

                        endif
                    endif

                    if(a2dVarRouting(iIII, iJJJ).lt.0.0)then
                        a2dVarRouting(iIII, iJJJ) = 0.0
                    endif

                endif

            enddo
        endif
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Check joint availability
        if (iNJoint .gt. 0) then
            ! Cycle on joint(s) 
            iJ = 0;
            do iJ = 1, iNJoint
                iIm = 0; iJm = 0; iIin = 0; iJin = 0;
                iIm = oHMC_Vars(iID)%a2iXYJoint(iJ,2); iJm = oHMC_Vars(iID)%a2iXYJoint(iJ,1);
                iIin = oHMC_Vars(iID)%a2iXYInJoint(iJ,2); iJin = oHMC_Vars(iID)%a2iXYInJoint(iJ,1);
                
                dHm = 0.0; dHin = 0.0;
                dHm = a2dVarHydroUpd(iIm, iJm) !dH2
                dHin = a2dVarHydroUpd(iIin,iJin) !dH1
                
                if ( (dHin.lt.dHm) .and. (dHm.gt.0.0) ) then
                    dQt = 0.0; dHinFD = 0.0;
                    dQt = a2dVarQVolOut(iIin, iJin)/1000 + oHMC_Vars(iID)%a2dAreaCell(iIm, iJm) ! in [m^3/s]
                    dHinFD = sqrt(dHin**2 + 1000*1000*2*(dQt**2)/(oHMC_Vars(iID)%a2dAreaCell(iIm, iJm)*9.8*dHin/1000)) ! derivata eq delle spinte
                    
                    if (dHinFD/dHm .lt. oHMC_Vars(iID)%a1dThrLevelJoint(iJ)) then
                        
                        iIout = 0; iJout = 0;
                        iIout = oHMC_Vars(iID)%a2iXYOutJoint(iJ,2); iJout = oHMC_Vars(iID)%a2iXYOutJoint(iJ,1);
                        
                        ! Main channel
                        a2dVarRouting(iIout,iJout) = a2dVarRouting(iIout,iJout) + a2dVarRouting(iIm,iJm)*(1 - dHinFD/dHm) ! Routing dove immetto la derivazione del Master 
                        a2dVarRouting(iIm,iJm) = a2dVarRouting(iIm,iJm)*dHinFD/dHm ! Routing in main channel
                        
                        ! Tributary channel
			a2dVarRouting(iIout,iJout) = a2dVarRouting(iIout,iJout) + a2dVarRouting(iIin,iJin)*(1 - dHinFD/dHm) ! Routing dove immetto la derivazione dell'immissario
			a2dVarRouting(iIin,iJin) = a2dVarRouting(iIin,iJin)*dHinFD/dHm ! Routing in main channel
                        
                    endif
                endif
            enddo    
        endif
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Compute dam spilling
        call HMC_Phys_Dam_Spilling(iID, iNDam, dDtSurfaceflow, &
                                                     a1dVarVDam, a1dVarQoutDam, a1dVarCoeffDam)
        !------------------------------------------------------------------------------------------
                                     
        !------------------------------------------------------------------------------------------
        ! Check dam availability
        if (iNDam .gt. 0) then
            ! Cycle on dam
            iD = 0
            do iD = 1, iNDam
            
                iI = 0; iJ = 0;
                iI = oHMC_Vars(iID)%a2iXYDam(iD, 2); iJ = oHMC_Vars(iID)%a2iXYDam(iD, 1);
                
                ! Pointers definition
                !iVarPNT = 0
                !iVarPNT = int(oHMC_Vars(iID)%a2iPNT(iI,iJ))
                
                ! Defining flow directions
                iII = int((int(oHMC_Vars(iID)%a2iPNT(iI,iJ))  - 1)/3) - 1
                iJJ = int(oHMC_Vars(iID)%a2iPNT(iI,iJ)) - 5 - 3*iII
                iIII = iI + iII
                iJJJ = iJ + iJJ
                
                ! Update routing 
                a2dVarRouting(iIII, iJJJ) = a2dVarRouting(iIII, iJJJ) + a1dVarQoutDam(iD)
                
                ! Volume to mean dam level [mm]
                where( oHMC_Vars(iID)%a2iChoice.eq.oHMC_Vars(iID)%a1dCodeDam(iD) .and. (oHMC_Vars(iID)%a2dDem.gt.0.0) ) 
                    a2dVarHydroUpd = a1dVarVDam(iD)/(oHMC_Vars(iID)%a1iNCellDam(iD)*oHMC_Vars(iID)%a2dAreaCell(iI,iJ))*1000 ! [mm]
                endwhere
                    
            enddo
            
        endif
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Debug
        if (iDEBUG.gt.0) then
            call mprintf(.true., iINFO_Extra, ' ') 
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarHydroUpd, oHMC_Vars(iID)%a2iMask, 'HYDRO UPD END') )
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarHydroPrev, oHMC_Vars(iID)%a2iMask, 'HYDRO PREV END') )
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarQDisOut, oHMC_Vars(iID)%a2iMask, 'QDIS OUT END (Qout)') )
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarQVolOut, oHMC_Vars(iID)%a2iMask, 'QVOL OUT END (Qtmp)') )
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarRouting, oHMC_Vars(iID)%a2iMask, 'ROUTING END') )
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarIntensityUpd, oHMC_Vars(iID)%a2iMask, 'INTENSITY END') )
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarFlowExf, oHMC_Vars(iID)%a2iMask, 'EXFILTRATION END') )     
            call mprintf(.true., iINFO_Extra, checkarray(a2dVarHydroPlant(:,2), 'HYDRO PLANT END') )
            call mprintf(.true., iINFO_Extra, checkarray(a2dVarHydroCatch(:,2), 'HYDRO CATCH END') )
            call mprintf(.true., iINFO_Extra, checkarray(a2dVarHydroRelease(:,2), 'HYDRO RELEASE END') )
            call mprintf(.true., iINFO_Extra, ' ========= SURFACE FLOW END =========== ') 
        endif
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Updating model global variable(s)
	oHMC_Vars(iID)%a2dQVolOut = a2dVarQVolOut
        oHMC_Vars(iID)%a2dQDisOut = a2dVarQDisOut
        
        oHMC_Vars(iID)%a2dHydro = a2dVarHydroUpd
        oHMC_Vars(iID)%a2dHydroPrev = a2dVarHydroPrev
        !oHMC_Vars(iID)%a2dIntensity = a2dVarIntensityUpd
        
        oHMC_Vars(iID)%a2dRouting = a2dVarRouting   ! Compute to use in horton and after set to zero
        
        oHMC_Vars(iID)%a1dVDam = a1dVarVDam
        oHMC_Vars(iID)%a1dHDam = a1dVarHDam 
        oHMC_Vars(iID)%a1dLDam = a1dVarLDam
        oHMC_Vars(iID)%a1dCoeffDam = a1dVarCoeffDam 
        oHMC_Vars(iID)%a1dQoutDam = a1dVarQoutDam
        
        oHMC_Vars(iID)%a1dVLake = a1dVarVLake
        
        oHMC_Vars(iID)%a2dHydroPlant = a2dVarHydroPlant
        oHMC_Vars(iID)%a2dHydroCatch = a2dVarHydroCatch
        oHMC_Vars(iID)%a2dHydroRelease = a2dVarHydroRelease
        
        ! Info end
        if (iDEBUG.gt.0) then
            call mprintf(.true., iINFO_Extra, ' Phys :: Convolution :: SurfaceFlow ... OK' )
        endif
        !------------------------------------------------------------------------------------------
           
    end subroutine HMC_Phys_Convolution_Apps_SurfaceFlow
    !------------------------------------------------------------------------------------------

    !------------------------------------------------------------------------------------------
    ! Subroutine for calculating hypodermic flow
    subroutine HMC_Phys_Convolution_Apps_SubFlow(iID, iRows, iCols, dDtDataForcing, dDtAct, iNDam)
        
        !------------------------------------------------------------------------------------------
        ! Variable(s) declaration
        integer(kind = 4)   :: iID, iRows, iCols
        integer(kind = 4)   :: iNDam
        real(kind = 4)      :: dDtDataForcing, dDtAct 
        
        integer(kind = 4)   :: iI, iII, iIII, iJ, iJJ, iJJJ, iD
        !integer(kind = 4)   :: iVarPNT
        
        integer(kind = 4)   :: iFlagFlowDeep
        real(kind = 4)      :: dDtSubflow
        real(kind = 4)      :: dRate, dRateMin
        
        real(kind = 4), dimension (iRows, iCols)            :: a2dVarVTot, a2dVarVTotStep, a2dVarVLoss

        real(kind = 4), dimension (iRows, iCols)            :: a2dVarFlowExf
        !real(kind = 4), dimension (iRows, iCols)            :: a2dVarVSub

        real(kind = 4), dimension (iNDam)                   :: a1dVarVDam
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Initialize variable(s)
        dDtSubflow = 0.0; dRate = 0.0;  dRateMin = 0.0; iFlagFlowDeep = 0;
        
        a2dVarVTot = 0.0; a2dVarVTotStep = 0.0;  a2dVarVLoss = 0.0; 
        a2dVarFlowExf = 0.0
        
        a1dVarVDam = 0.0
        
        oHMC_Vars(iID)%a2dFlowExf = 0.0
        
        !iVarPNT = 0; a2dVarVSub = 0.0;
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Integrating step (Subflow)
        dDtSubflow = dDtAct
        
        ! Hypodermic flow minimum rate
        dRateMin = oHMC_Namelist(iID)%dRateMin
        iFlagFlowDeep = oHMC_Namelist(iID)%iFlagFlowDeep

        ! Dam(s) data from global declaration
        a1dVarVDam = oHMC_Vars(iID)%a1dVDam
 
        ! Variable(s) from global declaration
        a2dVarVTot = oHMC_Vars(iID)%a2dVTot
        !a2dVarVSub = oHMC_Vars(iID)%a2dVSub
        a2dVarVLoss = oHMC_Vars(iID)%a2dVLoss
        
        a2dVarFlowExf = oHMC_Vars(iID)%a2dFlowExf
        
        ! Info start
        if (iDEBUG.gt.0) then
            call mprintf(.true., iINFO_Extra, ' Phys :: Convolution :: SubFlow ... ' )
        endif
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Debug
        if (iDEBUG.gt.0) then
            call mprintf(.true., iINFO_Extra, ' ========= SUBFLOW START ========= ') 
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarVTot, oHMC_Vars(iID)%a2iMask, 'VTOT START ') )
            !call mprintf(.true., iINFO_Extra, checkvar(a2dVarVSub, oHMC_Vars(iID)%a2iMask, 'VSUB START ') )
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarVLoss, oHMC_Vars(iID)%a2iMask, 'VLOSS START ') )
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarFlowExf, oHMC_Vars(iID)%a2iMask, 'FLOWEXF START ') )        
            call mprintf(.true., iINFO_Extra, '')
        endif
        !------------------------------------------------------------------------------------------
         
        !------------------------------------------------------------------------------------------
        ! Conditions on total and loss volume
        where( (oHMC_Vars(iID)%a2dDEM.gt.0.0).and.(a2dVarVTot.lt.0.0) ) a2dVarVTot = 0.0
        where( (oHMC_Vars(iID)%a2dDEM.gt.0.0).and.(a2dVarVLoss.lt.0.0) ) a2dVarVLoss = 0.0
        !------------------------------------------------------------------------------------------
         
        !------------------------------------------------------------------------------------------
        ! Check dam availability
        if (iNDam .gt. 0) then
            ! Set VTot equal zero into Dam(s) cell(s)
            do iD = 1,iNDam
                iI = 0; iJ = 0;
                iI = oHMC_Vars(iID)%a2iXYDam(iD, 2); iJ = oHMC_Vars(iID)%a2iXYDam(iD, 1)
                !a1dVarVDam(iD) = a1dVarVDam(iD) + a2dVarVTot(iI,iJ)*(oHMC_Vars(iID)%a2dAreaCell(iI,iJ))/1000
                a2dVarVTot(iI,iJ) = 0.0
            enddo
        endif
        !------------------------------------------------------------------------------------------
            
        !------------------------------------------------------------------------------------------
        ! Cycling on each pixels
        ! Calcola il volume di uscita dalla cella nei due casi: a2dV > o < di a2dS;
        ! Qsup � la portata che esce dalla parte superiore della cella e si aggiunge al deflusso superficiale               
        ! il contatore punta alla cella successiva(controllare se vale per l'ultima cella)
        iI = 0; iJ = 0;
        do iJ = 1, iCols
            do iI = 1, iRows
                
                ! DEM condition
                if (oHMC_Vars(iID)%a2dDEM(iI,iJ).gt.0.0) then
                    
                    ! Pointers definition
  
                    !iVarPNT = 0
                    !iVarPNT = int(oHMC_Vars(iID)%a2iPNT(iI,iJ))
                    
                    ! Defining flow directions
                    iII = int((int(oHMC_Vars(iID)%a2iPNT(iI,iJ))  - 1)/3) - 1
                    iJJ = int(oHMC_Vars(iID)%a2iPNT(iI,iJ)) - 5 - 3*iII
                    iIII = iI + iII
                    iJJJ = iJ + iJJ
                    
                    ! Debugging ndexes
                    !write(*,*) 'Rate: ',dRate,' iPNT: ',iVarPNT
                    !write(*,*) 'iJ: ',iJ, ' iI: ',iI, ' iJJ: ',iJJ, ' iII: ',iII , ' iJJJ: ',iJJJ, ' iIII: ',iIII
                    
                    ! Calculating VTot and VLoss using flowdeep condition
                    if (iFlagFlowDeep.eq.0) then
                        
                        ! VTot (Vloss == 0)
                        if(iIII.ge.1.and.iJJJ.ge.1) then
                            a2dVarVTotStep(iIII,iJJJ) = a2dVarVTotStep(iIII,iJJJ) + oHMC_Vars(iID)%a2dVSub(iI, iJ)
                        endif
                        
                    else
                        
                        ! Rate definition
                        dRate = 0.0; 
                        dRate = sin(oHMC_Vars(iID)%a2dBeta(iI,iJ))
                        
                        ! Checking rate value
                        if(dRate.gt.1.0)        dRate = 0.99
                        if(dRate.lt.dRateMin)   dRate = dRateMin

                        ! VTot
                        if(iIII.ge.1.and.iJJJ.ge.1) then
                            a2dVarVTotStep(iIII,iJJJ) = a2dVarVTotStep(iIII,iJJJ) + oHMC_Vars(iID)%a2dVSub(iI, iJ)*dRate
                        endif
                        ! Vloss
                        if(iIII.ge.1.and.iJJJ.ge.1) then 
                            a2dVarVLoss(iIII,iJJJ) = a2dVarVLoss(iIII,iJJJ) + oHMC_Vars(iID)%a2dVSub(iI, iJ)*(1 - dRate)
                        endif
                    
                    endif
                    
                endif
              
            enddo
        enddo
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Updating total volume 
        a2dVarVTot = a2dVarVTot + a2dVarVTotStep
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Checking total volume and calculating exfiltration volume
	where ( (oHMC_Vars(iID)%a2dDEM.gt.0.0) .and. (a2dVarVTot.gt.oHMC_Vars(iID)%a2dS) )
            ! Calculating esfiltration flow [m/seconds]
            a2dVarFlowExf = (a2dVarVTot - oHMC_Vars(iID)%a2dS)/(1000.0*dDtSubflow) !in m/sec
            ! Updating total volume information
            a2dVarVTot = 1.0*oHMC_Vars(iID)%a2dS 
        endwhere
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Hypodermic flow to fill cell(s)-lake of the dam
        if (iNDam .gt. 0) then
            do iD = 1,iNdam
                iI = 0; iJ = 0;
                iI = oHMC_Vars(iID)%a2iXYDam(iD,2); iJ = oHMC_Vars(iID)%a2iXYDam(iD,1)
                ! Amount of upstream volume at dam section. V set zero at the subroutine begin
                a1dVarVDam(iD) = a1dVarVDam(iD) + a2dVarVTot(iI,iJ)*(oHMC_Vars(iID)%a2dAreaCell(iI,iJ))/1000
            enddo
        endif
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Debug
        if (iDEBUG.gt.0) then
            call mprintf(.true., iINFO_Extra, '')
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarVTot, oHMC_Vars(iID)%a2iMask, 'VTOT END ') )
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarVTotStep, oHMC_Vars(iID)%a2iMask, 'VTOTSTEP START ') )
            !call mprintf(.true., iINFO_Extra, checkvar(a2dVarVSub, oHMC_Vars(iID)%a2iMask, 'VSUB END ') )
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarVLoss, oHMC_Vars(iID)%a2iMask, 'VLOSS END ') )
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarFlowExf, oHMC_Vars(iID)%a2iMask, 'FLOWEXF END ') ) 
            call mprintf(.true., iINFO_Extra, ' ========= SUBFLOW END ========= ') 
        endif
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Updating variable(s)
        oHMC_Vars(iID)%a2dVTot = a2dVarVTot
        oHMC_Vars(iID)%a2dVLoss = a2dVarVLoss
        
        oHMC_Vars(iID)%a2dFlowExf = a2dVarFlowExf
        
        oHMC_Vars(iID)%a1dVDam = a1dVarVDam
        
        ! Info end
        if (iDEBUG.gt.0) then
            call mprintf(.true., iINFO_Extra, ' Phys :: Convolution :: SubFlow ... OK' )
        endif
        !------------------------------------------------------------------------------------------

    end subroutine HMC_Phys_Convolution_Apps_SubFlow
    !------------------------------------------------------------------------------------------

    !------------------------------------------------------------------------------------------
    ! Subroutine for calculating infiltration/runoff (using horton modified method)
    subroutine HMC_Phys_Convolution_Apps_Horton(iID, iRows, iCols, dDtDataForcing, dDtAct, iTq, iTime, iNTime)
        
        !------------------------------------------------------------------------------------------
        ! Variable(s) declaration
        integer(kind = 4) :: iID, iRows, iCols
        real(kind = 4) :: dDtDataForcing, dDtAct
        
        real(kind = 4) :: dVarVErr
        real(kind = 4) :: dDtHorton
        real(kind = 4) :: dDomainArea

        real(kind = 4), dimension (iRows, iCols)         :: a2dVarVTot, a2dVarVTotPStep
        real(kind = 4), dimension (iRows, iCols)         :: a2dVarVSub
        real(kind = 4), dimension (iRows, iCols)         :: a2dVarRain, a2dVarIntensity
        
        !real(kind = 4), dimension (iRows, iCols)         :: a2dVarB, a2dVarCh
        real(kind = 4), dimension (iRows, iCols)         :: a2dVarG, a2dVarVErr
        
        real(kind = 4), dimension (iRows, iCols)         :: a2dVarRouting, a2dVarFlowDeep
        
        integer(kind = 4) :: iTq, iTime, iNTime
        !------------------------------------------------------------------------------------------

        !------------------------------------------------------------------------------------------
        ! Variable(s) initialization
        a2dVarVTot = 0.0; a2dVarVTotPStep = 0.0; a2dVarVSub = 0.0
        a2dVarRain = 0.0; a2dVarIntensity = 0.0;
        a2dVarVErr = 0.0;
        a2dVarRouting = 0.0; a2dVarFlowDeep = 0.0;
        
        !a2dVarG = 0.0; a2dVarCh = 0.0;  dVarVErr = 0.0;
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Integrating step (horton)
        dDtHorton = dDtAct
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------ 
        ! Domain Area
        dDomainArea =  oHMC_Vars(iID)%dDomainArea
        
        ! Variable(s) from global declaration
        a2dVarVTot = oHMC_Vars(iID)%a2dVTot
        a2dVarVSub = oHMC_Vars(iID)%a2dVSub
        a2dVarRouting = oHMC_Vars(iID)%a2dRouting
        a2dVarFlowDeep = oHMC_Vars(iID)%a2dFlowDeep
        
        ! Extracting dynamic forcing variable(s)
        a2dVarRain = oHMC_Vars(iID)%a2dRain
        where (a2dVarRain.lt.0.0.or.a2dVarRain.gt.845.0) a2dVarRain = 0.0
            
        ! Info start
        if (iDEBUG.gt.0) then
            call mprintf(.true., iINFO_Extra, ' Phys :: Convolution :: Horton ... ' )
        endif
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Total volume previous step
        a2dVarVTotPStep = a2dVarVTot
        !------------------------------------------------------------------------------------------
         
        !------------------------------------------------------------------------------------------
        ! Horton filter equation
        where (oHMC_Vars(iID)%a2dS.gt.0.0)
            a2dVarG = oHMC_Vars(iID)%a2dCostF - &
                     (oHMC_Vars(iID)%a2dCostF - oHMC_Vars(iID)%a2dCostF1)/oHMC_Vars(iID)%a2dS*a2dVarVTot
        elsewhere
            a2dVarG = 0.0
        endwhere
        !------------------------------------------------------------------------------------------
                
        !------------------------------------------------------------------------------------------
        ! Debug
        if (iDEBUG.gt.0) then
            call mprintf(.true., iINFO_Extra, ' ========= HORTON START =========== ') 
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarRouting, oHMC_Vars(iID)%a2iMask, 'ROUTING START') )
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarIntensity, oHMC_Vars(iID)%a2iMask, 'INTENSITY START') )
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarVTot, oHMC_Vars(iID)%a2iMask, 'VTOT START') )
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarVSub, oHMC_Vars(iID)%a2iMask, 'VSUB START') )
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarFlowDeep, oHMC_Vars(iID)%a2iMask, 'FLOWDEEP START') )
            call mprintf(.true., iINFO_Extra, checkvar(oHMC_Vars(iID)%a2dHydro, oHMC_Vars(iID)%a2iMask, 'HYDRO UPD START') )
            call mprintf(.true., iINFO_Extra, checkvar(oHMC_Vars(iID)%a2dHydroPrev, oHMC_Vars(iID)%a2iMask, 'HYDRO PREV START') )
            call mprintf(.true., iINFO_Extra, checkvar(oHMC_Vars(iID)%a2dQDisOut, oHMC_Vars(iID)%a2iMask, 'QOUT START (Qtot)') )
            call mprintf(.true., iINFO_Extra, ' ') 
            call mprintf(.true., iINFO_Extra, checkvar(oHMC_Vars(iID)%a2dCostChFix, oHMC_Vars(iID)%a2iMask, 'CHFIX ') )
            call mprintf(.true., iINFO_Extra, checkvar(oHMC_Vars(iID)%a2dCostF, oHMC_Vars(iID)%a2iMask, 'COSTF ') )
            call mprintf(.true., iINFO_Extra, checkvar(oHMC_Vars(iID)%a2dCostF1, oHMC_Vars(iID)%a2iMask, 'COSTF1 ') )
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarG, oHMC_Vars(iID)%a2iMask, 'G ') )
            !call mprintf(.true., iINFO_Extra, checkvar(a2dVarB, oHMC_Vars(iID)%a2iMask, 'B ') )
            !call mprintf(.true., iINFO_Extra, checkvar(a2dVarCh, oHMC_Vars(iID)%a2iMask, 'CH ') )
            call mprintf(.true., iINFO_Extra, checkvar(oHMC_Vars(iID)%a2dS, oHMC_Vars(iID)%a2iMask, 'S ') )
            call mprintf(.true., iINFO_Extra, checkvar(oHMC_Vars(iID)%a2dCoeffResol, oHMC_Vars(iID)%a2iMask, 'COEFF RESOL') )
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarRain, oHMC_Vars(iID)%a2iMask, 'RAIN ') )
            call mprintf(.true., iINFO_Extra, ' ') 
        endif
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Defining subterranean volume 
        where ( (a2dVarVTot.lt.oHMC_Vars(iID)%a2dCt*oHMC_Vars(iID)%a2dS) .and. (oHMC_Vars(iID)%a2dDEM.gt.0.0) )
            a2dVarVSub = 0.0
        elsewhere(oHMC_Vars(iID)%a2dDEM.gt.0.0)
            ! *dDth/3600 perch� a2dCostF1 � in mm/h ma lavoro in mm/dDth
            a2dVarVSub = oHMC_Vars(iID)%a2dF2*(a2dVarVTot - oHMC_Vars(iID)%a2dCt*oHMC_Vars(iID)%a2dS)/ &
                         oHMC_Vars(iID)%a2dS*dDtHorton/3600  
        endwhere   
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Evaluating intensity --> horton initialization = rain + routing drained cells
        a2dVarIntensity = a2dVarRain*3600.0/dDtDataForcing + &
                          oHMC_Vars(iID)%a2dCoeffResol*a2dVarRouting/dDtHorton*3600.0 + &
                          a2dVarFlowDeep*3600.0/dDtDataForcing
        ! Evaluating intensity ( --- development mode --- )                  
        !a2dVarIntensity = a2dVarRain*3600.0/dDtDataForcing + &
        !                  oHMC_Vars(iID)%a2dCoeffResol*(a2dVarRouting/dDtHorton*3600.0 + &
        !                  a2dVarFlowDeep*3600.0/dDtDataForcing)
        !-------------------------------------------------------------------------------
                 
        !------------------------------------------------------------------------------------------
        ! Intensity Evaluation
        ! Condition ----> Intensity == 0
        where ( (a2dVarIntensity.eq.0.0) .and. (oHMC_Vars(iID)%a2dDEM.gt.0.0) .and. &
                (a2dVarVTot.ge.oHMC_Vars(iID)%a2dCt*oHMC_Vars(iID)%a2dS) )     
                
            a2dVarVTot = a2dVarVTot - oHMC_Vars(iID)%a2dF2*(a2dVarVTot - oHMC_Vars(iID)%a2dCt*oHMC_Vars(iID)%a2dS)/ &
                         oHMC_Vars(iID)%a2dS*dDtHorton/3600.0	
            
        endwhere
        	
        ! Condition ----> 0 < Intensity <= G					
        where ( (a2dVarIntensity.gt.0.0) .and. (a2dVarIntensity.le.a2dVarG) .and. (oHMC_Vars(iID)%a2dDEM.gt.0.0) ) 

                where ( (a2dVarVTot.lt.oHMC_Vars(iID)%a2dCt*oHMC_Vars(iID)%a2dS) .and. (oHMC_Vars(iID)%a2dDEM.gt.0.0) )
                        a2dVarVTot = a2dVarVTot + a2dVarRain/dDtDataForcing*dDtHorton + &
                                     oHMC_Vars(iID)%a2dCoeffResol*a2dVarRouting + &
                                     a2dVarFlowDeep/dDtDataForcing*dDtHorton
                        a2dVarIntensity = 0.0
                elsewhere(oHMC_Vars(iID)%a2dDEM.gt.0.0)
                        a2dVarVTot = a2dVarVTot + a2dVarIntensity*dDtHorton/3600.0 - a2dVarVSub
                        a2dVarIntensity = 0.0
                endwhere
                
        endwhere
        
        ! Condition ----> Intensity > G	
        where ( (a2dVarIntensity.gt.a2dVarG) .and. (oHMC_Vars(iID)%a2dDEM.gt.0.0) ) 						

                where ( (a2dVarVTot.lt.oHMC_Vars(iID)%a2dCt*oHMC_Vars(iID)%a2dS) .and. (oHMC_Vars(iID)%a2dDEM.gt.0.0) )
                        a2dVarVTot = a2dVarVTot + a2dVarG*dDtHorton/3600.0              
                elsewhere (oHMC_Vars(iID)%a2dDEM.gt.0.0)
                        a2dVarVTot = a2dVarVTot + a2dVarG*dDtHorton/3600.0 - a2dVarVSub           
                endwhere
                
                a2dVarIntensity = a2dVarIntensity - a2dVarG

        endwhere			
        !------------------------------------------------------------------------------------------

        !------------------------------------------------------------------------------------------
        ! Updating intensity variable
        where ( (a2dVarVTot.gt.oHMC_Vars(iID)%a2dS) .and. (oHMC_Vars(iID)%a2dDEM.gt.0.0) )
                a2dVarIntensity =  a2dVarIntensity + (a2dVarVTot - oHMC_Vars(iID)%a2dS)/dDtHorton*3600.0
                a2dVarVTot = oHMC_Vars(iID)%a2dS
        endwhere
        !------------------------------------------------------------------------------------------

        !------------------------------------------------------------------------------------------
        ! Calculating mass balance errors
        where ( (a2dVarRain.lt.0.0) .and. (oHMC_Vars(iID)%a2dDEM.gt.0.0) ) a2dVarRain = 0.0 ! Checking rain
        
        ! Calculating Volume error
        where ( oHMC_Vars(iID)%a2dDEM.gt.0.0 )
            
                a2dVarVErr = a2dVarFlowDeep/dDtDataForcing*dDtHorton + &
                             a2dVarRain/dDtDataForcing*dDtHorton + oHMC_Vars(iID)%a2dCoeffResol*a2dVarRouting - &
                             a2dVarIntensity*dDtHorton/3600.0 - &
                             (a2dVarVTot - a2dVarVTotPStep + a2dVarVSub)

                where (a2dVarVTot.lt.0.0)
                        a2dVarVTot = 0.0
                endwhere

        endwhere
        
        ! Cumulative mean error
        !dVarVErr = dVarVErr + SUM(SUM(a2dVarVErr, DIM=1, mask=oHMC_Vars(iID)%a2dDEM.gt.0.0))/dDomainArea !DIM=1 columns
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Debug
        if (iDEBUG.gt.0) then
            call mprintf(.true., iINFO_Extra, '')
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarRouting, oHMC_Vars(iID)%a2iMask, 'ROUTING END') )
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarIntensity, oHMC_Vars(iID)%a2iMask, 'INTENSITY END') )
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarVTot, oHMC_Vars(iID)%a2iMask, 'VTOT END') )
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarVSub, oHMC_Vars(iID)%a2iMask, 'VSUB END') )
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarFlowDeep, oHMC_Vars(iID)%a2iMask, 'FLOWDEEP END') )
            call mprintf(.true., iINFO_Extra, checkvar(oHMC_Vars(iID)%a2dHydro, oHMC_Vars(iID)%a2iMask, 'HYDRO UPD END') )
            call mprintf(.true., iINFO_Extra, checkvar(oHMC_Vars(iID)%a2dHydroPrev, oHMC_Vars(iID)%a2iMask, 'HYDRO PREV END') )
            call mprintf(.true., iINFO_Extra, checkvar(oHMC_Vars(iID)%a2dQDisOut, oHMC_Vars(iID)%a2iMask, 'QOUT END (Qtot)') )
            call mprintf(.true., iINFO_Extra, ' ========= HORTON END =========== ') 
        endif
        !------------------------------------------------------------------------------------------

        !------------------------------------------------------------------------------------------
        ! Updating field(s) in global declaration
        oHMC_Vars(iID)%a2dVTot = a2dVarVTot
        oHMC_Vars(iID)%a2dVSub = a2dVarVSub
       
        oHMC_Vars(iID)%a2dIntensity = a2dVarIntensity
        oHMC_Vars(iID)%a2dFlowDeep = a2dVarFlowDeep
        
        oHMC_Vars(iID)%a2dRain = a2dVarRain
        
        oHMC_Vars(iID)%a2dVErr = a2dVarVErr
        !oHMC_Vars(iID)%dVErr = dVarVErr
        
        ! Info end
        if (iDEBUG.gt.0) then
            call mprintf(.true., iINFO_Extra, ' Phys :: Convolution :: Horton ... OK' )
        endif
        !------------------------------------------------------------------------------------------
        
    end subroutine HMC_Phys_Convolution_Apps_Horton
    !------------------------------------------------------------------------------------
    
    !------------------------------------------------------------------------------------------
    ! Subroutine to compute hydraulic structure effect(s)
    subroutine HMC_Phys_Convolution_Apps_HydraulicStructure(iID, iRows, iCols, &
                                                            dDtDataForcing, iNDam, iNLake)
    
        !------------------------------------------------------------------------------------------
        ! Variable(s)
        integer(kind = 4)           :: iID, iRows, iCols
        integer(kind = 4)           :: iNDam, iNLake
        
        real(kind = 4)              :: dDtDataForcing
        
        real(kind = 4), dimension (iRows, iCols)    :: a2dVarFlowDeep, a2dVarHydro
        
        real(kind = 4), dimension (iNLake)          :: a1dVarVLake, a1dVarQoutLake
        
        real(kind = 4), dimension (iNDam)           :: a1dVarVDam, a1dVarHDam, a1dVarLDam
        real(kind = 4), dimension (iNDam)           :: a1dVarCoeffDam, a1dVarQoutDam
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Get variable(s)
        a2dVarFlowDeep = oHMC_Vars(iID)%a2dFlowDeep
        a2dVarHydro = oHMC_Vars(iID)%a2dHydro
        
        a1dVarVLake = oHMC_Vars(iID)%a1dVLake
        a1dVarQoutLake = 0.0
        
        a1dVarVDam = oHMC_Vars(iID)%a1dVDam
        a1dVarHDam = oHMC_Vars(iID)%a1dHDam 
        a1dVarLDam = oHMC_Vars(iID)%a1dLDam
        a1dVarCoeffDam = oHMC_Vars(iID)%a1dCoeffDam 

        ! Info start
        if (iDEBUG.gt.0) then
            call mprintf(.true., iINFO_Extra, ' Phys :: Convolution :: HydraulicStructure ... ' )
        endif
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Method to compute lake tank
        call HMC_Phys_Lake_Tank(iID, iRows, iCols, dDtDataForcing, iNLake, &
                                a1dVarVLake, a1dVarQoutLake, &
                                a2dVarHydro, a2dVarFlowDeep)
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Method to compute dam discharge --> Commentata per cambio calcolo di svaso delle dighe
        !call HMC_Phys_Dam_Discharge(iID, iNDam, dDtDataForcing, &
        !                                a1dVarVDam, a1dVarHDam, a1dVarLDam, a1dVarCoeffDam)
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Updating field(s) in global declaration
        oHMC_Vars(iID)%a2dFlowDeep = a2dVarFlowDeep
        oHMC_Vars(iID)%a2dHydro = a2dVarHydro
        
        oHMC_Vars(iID)%a1dVLake = a1dVarVLake
        oHMC_Vars(iID)%a1dQoutLake = a1dVarQoutLake
        
        oHMC_Vars(iID)%a1dVDam = a1dVarVDam
        oHMC_Vars(iID)%a1dHDam = a1dVarHDam
        oHMC_Vars(iID)%a1dLDam = a1dVarLDam
        oHMC_Vars(iID)%a1dCoeffDam = a1dVarCoeffDam
 
        ! Info end
        if (iDEBUG.gt.0) then
            call mprintf(.true., iINFO_Extra, ' Phys :: Convolution :: HydraulicStructure ... OK' )
        endif
        !------------------------------------------------------------------------------------------
                                                                    
    end subroutine HMC_Phys_Convolution_Apps_HydraulicStructure
    !------------------------------------------------------------------------------------------
        
end module HMC_Module_Phys_Convolution_Apps
!------------------------------------------------------------------------------------------

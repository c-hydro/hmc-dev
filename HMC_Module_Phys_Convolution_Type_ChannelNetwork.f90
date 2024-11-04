 !------------------------------------------------------------------------------------
! File:   HMC_Module_Phys_Type_Convolution_ChannelNetwork.f90
!
! Author:   Fabio Delogu
! Date:     20190410
!
! Physics convolution subroutine(s) for HMC model
!------------------------------------------------------------------------------------

!------------------------------------------------------------------------------------
! Module Header
module HMC_Module_Phys_Convolution_Type_ChannelNetwork

    !------------------------------------------------------------------------------------
    ! External module(s) 
    use HMC_Module_Namelist,                    only:   oHMC_Namelist
    use HMC_Module_Vars_Loader,                 only:   oHMC_Vars
    
    use HMC_Module_Tools_Debug
    
    use HMC_Module_Tools_Generic,               only:   max2Dvar, min2Dvar
    
    use HMC_Module_Phys_Convolution_Apps_SubFlow,               only: HMC_Phys_Convolution_Apps_SubFlow
    use HMC_Module_Phys_Convolution_Apps_HydraulicStructure,    only: HMC_Phys_Convolution_Apps_HydraulicStructure
    
    use HMC_Module_Phys_Convolution_Apps_Discharge,             only: HMC_Phys_Convolution_Apps_Discharge_ChannelNetwork   
    use HMC_Module_Phys_Convolution_Apps_IntegrationStep,       only: HMC_Phys_Convolution_Apps_IntegrationStep_ChannelNetwork 
    use HMC_Module_Phys_Convolution_Apps_Horton,                only: HMC_Phys_Convolution_Apps_Horton_ChannelNetwork  
    use HMC_Module_Phys_Convolution_Apps_SurfaceFlow,           only: HMC_Phys_Convolution_Apps_SurfaceFlow_ChannelNetwork 
    use HMC_Module_Phys_Convolution_Apps_DeepFlow,              only: HMC_Phys_Convolution_Apps_DeepFlow_ChannelNetwork 

    ! Implicit none for all subroutines in this module
    implicit none
    !------------------------------------------------------------------------------------------

contains
    
    !------------------------------------------------------------------------------------------
    ! Subroutine for calculating convolution
    subroutine HMC_Phys_Convolution_Cpl_ChannelNetwork(iID,  &
                                        iRows, iCols, &
                                        iTime, iNTime, iETime, &
                                        iNSection, iNData, &
                                        iNLake, iNDam, & 
                                        iNPlant, iNCatch, iNRelease, iNJoint, sTime, iDaySteps)
        
        !------------------------------------------------------------------------------------------
        ! Variable(s) declaration
        integer(kind = 4)           :: iID
        integer(kind = 4)           :: iRows, iCols
        integer(kind = 4)           :: iTime, iNTime, iETime, iStep, iDaySteps
        integer(kind = 4)           :: iNSection, iNData, iNLake, iNDam
        integer(kind = 4)           :: iNPlant, iNCatch, iNRelease, iNJoint

        integer(kind = 4)           :: iTAct, iTInt, iTq, iDtMax
        integer(kind = 4)           :: iFlagFlowDeep
        real(kind = 4)              :: dDt, dDtAct, dDtDataForcing, dDtMax, dDtIntegrAct 
        
        real(kind = 4), dimension (iRows, iCols)   :: a2dVarTot
        
        character(len = 20)             :: sTInt
        character(len = 19)             :: sTime
        
        character (len=8)               :: sCurrenteDate
        character (len=10)              :: sCurrentTime
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Initialize variable(s)
        iTAct = 0; iTInt = 0; iTq = 0; iDtMax = 0;
        iFlagFlowDeep = 0;
        dDt = 0.0; dDtAct = 0.0; dDtDataForcing = 0.0; dDtMax = 0.0; dDtIntegrAct = 0.0; 
        a2dVarTot = 0.0;
        oHMC_Vars(iID)%a2dQTot = 0.0;
        !------------------------------------------------------------------------------------------

        !------------------------------------------------------------------------------------------
        ! Get model static variable(s)
        ! Temporal step of data forcing
        dDtDataForcing = real(oHMC_Namelist(iID)%iDtData_Forcing)
        ! FlpwDeep flag
        iFlagFlowDeep = oHMC_Namelist(iID)%iFlagFlowDeep
        ! Actual model step 
        iTAct = iTime
        
        ! Info start
        call mprintf(.true., iINFO_Verbose, ' Phys :: Convolution [channel network] ... ' )
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Debug
        if (iDEBUG.gt.0) then
            call mprintf(.true., iINFO_Extra, ' ========= CONVOLUTION START =========== ')  
            call mprintf(.true., iINFO_Extra, checkvar(oHMC_Vars(iID)%a2dVTot, oHMC_Vars(iID)%a2iMask, 'VTOT START ') )
            call mprintf(.true., iINFO_Extra, checkvar(oHMC_Vars(iID)%a2dVSub, oHMC_Vars(iID)%a2iMask, 'VSUB START ') )
            call mprintf(.true., iINFO_Extra, checkvar(oHMC_Vars(iID)%a2dHydro, oHMC_Vars(iID)%a2iMask, 'HYDRO START ') )
            call mprintf(.true., iINFO_Extra, checkvar(oHMC_Vars(iID)%a2dRouting, oHMC_Vars(iID)%a2iMask, 'ROUTING START ') )
            call mprintf(.true., iINFO_Extra, checkvar(oHMC_Vars(iID)%a2dFlowDeep, oHMC_Vars(iID)%a2iMask, 'FLOWDEEP START ') )
            call mprintf(.true., iINFO_Extra, checkvar(oHMC_Vars(iID)%a2dFlowExf, oHMC_Vars(iID)%a2iMask, 'FLOWEXF START ') )
            call mprintf(.true., iINFO_Extra, ' ') 
        endif
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Call subroutine to calcolate temporal integration step
        call HMC_Phys_Convolution_Apps_IntegrationStep_ChannelNetwork(iID, iRows, iCols, dDtDataForcing, dDtIntegrAct)
        !------------------------------------------------------------------------------------------

        !------------------------------------------------------------------------------------------
        ! Estimate integration step
        iDtMax = int(dDtDataForcing/dDtIntegrAct)
        dDtMax = dDtDataForcing/dDtIntegrAct
        
        ! Check if dDtMax is a integer number (in seconds)
        if (dDtMax.eq.iDtMax) then
            dDtMax = 0.0
        else
            dDtMax = (dDtMax - real(iDtMax))*dDtIntegrAct
        endif
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Initialize Vloss = 0.0 because of summing on iTmax steps (if flowdeep activated)
        if  (iFlagFlowDeep.eq.1) then
            oHMC_Vars(iID)%a2dVLoss = 0.0
        endif
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Time integration info
        if (iDEBUG.gt.0) then
            call date_and_time(date=sCurrenteDate, time=sCurrentTime)
            call mprintf(.true., iINFO_Verbose, ' Phys :: Convolution :: TimeStart: ' &
                                //sCurrenteDate(1:4)//'-'//sCurrenteDate(5:6)//'-'//sCurrenteDate(7:8)//' ' &
                                //sCurrentTime(1:2)//':'//sCurrentTime(3:4)//':'//sCurrentTime(5:10) )
        endif
        !------------------------------------------------------------------------------------------

        !------------------------------------------------------------------------------------------
        ! Cycling on integration step
        iTq = 1
        TimeIntLoop : do iTInt = 1, iDtMax
                
                ! Info integration step
                write(sTInt, *) iTInt; call mprintf(.true., iINFO_Extra, ' Phys :: Convolution :: IntStep: '//sTInt//' START' ) 
                
                ! Call infiltration/runoff routine using modified Horton method          
                call HMC_Phys_Convolution_Apps_Horton_ChannelNetwork(iID, iRows, iCols, dDtDataForcing, dDtIntegrAct, &
                                                      iTq, iTime, iNTime)  
                                                                                       
                ! Call sub-flow routine
                call HMC_Phys_Convolution_Apps_SubFlow(iID, iRows, iCols, dDtDataForcing, dDtIntegrAct, iNDam)

                ! Call surface routing routine using a tank model
                oHMC_Vars(iID)%a2dQVolOut = 0.0
                call HMC_Phys_Convolution_Apps_SurfaceFlow_ChannelNetwork(iID, iRows, iCols, &
                                                           dDtDataForcing, dDtIntegrAct, iTAct, iTq, iDtMax, &
                                                           iNData, iNDam, iNLake, & 
                                                           iNPlant, iNCatch, iNRelease, iNJoint, &
                                                           iTime, iNTime, iETime)

                ! Call discharge routine     
                call HMC_Phys_Convolution_Apps_Discharge_ChannelNetwork(iID, iRows, iCols, iNSection, &
                                                         dDtDataForcing, dDtIntegrAct, iNTime, iTq, dDtMax)
                                                         
                ! Discharge temporal counter
                write(sTInt, *) iTInt; call mprintf(.true., iINFO_Extra, ' Phys :: Convolution :: IntStep: '//sTInt//' END' ) 
                iTq = iTq + 1
                
        enddo TimeIntLoop
        !------------------------------------------------------------------------------------------

        !------------------------------------------------------------------------------------------
        ! Call deep flow routine
        call HMC_Phys_Convolution_Apps_DeepFlow_ChannelNetwork(iID, iRows, iCols, dDtDataForcing)
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Call hydraulic structure routine
        call HMC_Phys_Convolution_Apps_HydraulicStructure(iID, iRows, iCols, dDtDataForcing, iNDam, iNLake)
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Time integration info
        if (iDEBUG.gt.0) then
            call date_and_time(date=sCurrenteDate, time=sCurrentTime)
            call mprintf(.true., iINFO_Verbose, ' Phys :: Convolution :: TimeEnd: ' &
                                //sCurrenteDate(1:4)//'-'//sCurrenteDate(5:6)//'-'//sCurrenteDate(7:8)//' ' &
                                //sCurrentTime(1:2)//':'//sCurrentTime(3:4)//':'//sCurrentTime(5:10) )
        endif
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Debug
        if (iDEBUG.gt.0) then
            call mprintf(.true., iINFO_Extra, ' ') 
            call mprintf(.true., iINFO_Extra, checkvar(oHMC_Vars(iID)%a2dVTot, oHMC_Vars(iID)%a2iMask, 'VTOT END ') )
            call mprintf(.true., iINFO_Extra, checkvar(oHMC_Vars(iID)%a2dVSub, oHMC_Vars(iID)%a2iMask, 'VSUB END ') )
            call mprintf(.true., iINFO_Extra, checkvar(oHMC_Vars(iID)%a2dHydro, oHMC_Vars(iID)%a2iMask, 'HYDRO END ') )
            call mprintf(.true., iINFO_Extra, checkvar(oHMC_Vars(iID)%a2dRouting, oHMC_Vars(iID)%a2iMask, 'ROUTING END ') )
            call mprintf(.true., iINFO_Extra, checkvar(oHMC_Vars(iID)%a2dFlowDeep, oHMC_Vars(iID)%a2iMask, 'FLOWDEEP END ') )
            call mprintf(.true., iINFO_Extra, checkvar(oHMC_Vars(iID)%a2dFlowExf, oHMC_Vars(iID)%a2iMask, 'FLOWEXF END ') )
            call mprintf(.true., iINFO_Extra, ' ========= CONVOLUTION END =========== ')  
        endif
        !------------------------------------------------------------------------------------------
                                            
        !------------------------------------------------------------------------------------------
        ! Info end
        call mprintf(.true., iINFO_Verbose, ' Phys :: Convolution [channel network] ... OK' )
        !------------------------------------------------------------------------------------------
        
    end subroutine HMC_Phys_Convolution_Cpl_ChannelNetwork
    !------------------------------------------------------------------------------------------
    
end module HMC_Module_Phys_Convolution_Type_ChannelNetwork
!------------------------------------------------------------------------------------------

 !------------------------------------------------------------------------------------
! File: HMC_Module_Phys_Convolution_Type_ChannelFraction.f90
!
! Author(s):    Fabio Delogu, Francesco Silvestro
! Date:         20190410
!
! Physics convolution subroutine(s) for HMC model
!------------------------------------------------------------------------------------

!------------------------------------------------------------------------------------
! Module Header
module HMC_Module_Phys_Convolution_Type_ChannelFraction

    !------------------------------------------------------------------------------------
    ! External module(s) 
    use HMC_Module_Namelist,                    only:   oHMC_Namelist
    use HMC_Module_Vars_Loader,                 only:   oHMC_Vars
    
    use HMC_Module_Tools_Debug
    
    use HMC_Module_Tools_Generic,               only:   max2Dvar, min2Dvar
    
    use HMC_Module_Phys_Convolution_Apps_SubFlow,               only: HMC_Phys_Convolution_Apps_SubFlow
    use HMC_Module_Phys_Convolution_Apps_HydraulicStructure,    only: HMC_Phys_Convolution_Apps_HydraulicStructure
    
    use HMC_Module_Phys_Convolution_Apps_Discharge,             only: HMC_Phys_Convolution_Apps_Discharge_ChannelFraction  
    use HMC_Module_Phys_Convolution_Apps_IntegrationStep,       only: HMC_Phys_Convolution_Apps_IntegrationStep_ChannelFraction 
    use HMC_Module_Phys_Convolution_Apps_Horton,                only: HMC_Phys_Convolution_Apps_Horton_ChannelFraction
    use HMC_Module_Phys_Convolution_Apps_SurfaceFlow,           only: HMC_Phys_Convolution_Apps_SurfaceFlow_ChannelFraction 
    use HMC_Module_Phys_Convolution_Apps_DeepFlow,              only: HMC_Phys_Convolution_Apps_DeepFlow_ChannelFraction 
    
    ! Implicit none for all subroutines in this module
    implicit none
    !------------------------------------------------------------------------------------------

contains
    
    !------------------------------------------------------------------------------------------
    ! Subroutine for calculating convolution
    subroutine HMC_Phys_Convolution_Cpl_ChannelFraction(iID,  &
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
        integer(kind = 4)           :: iFlagFlowDeep, iFlagFlood
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
        oHMC_Vars(iID)%a2dQfloodCL = 0.0
        oHMC_Vars(iID)%a2dQfloodCR = 0.0
        !------------------------------------------------------------------------------------------

        !------------------------------------------------------------------------------------------
        ! Get model static variable(s)
        ! Temporal step of data forcing
        dDtDataForcing = real(oHMC_Namelist(iID)%iDtData_Forcing)
        ! FlpwDeep flag
        iFlagFlowDeep = oHMC_Namelist(iID)%iFlagFlowDeep
        !Flooding flag
        iFlagFlood = oHMC_Namelist(iID)%iFlagFlood   
        ! Actual model step 
        iTAct = iTime
        
        
        ! Info start
        call mprintf(.true., iINFO_Verbose, ' Phys :: Convolution [channel fraction] ... ' )
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
        call HMC_Phys_Convolution_Apps_IntegrationStep_ChannelFraction(iID, iRows, iCols, dDtDataForcing, dDtIntegrAct)
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
                call HMC_Phys_Convolution_Apps_Horton_ChannelFraction(iID, iRows, iCols, dDtDataForcing, dDtIntegrAct, &
                                                      iTq, iTime, iNTime)  
                                                                                       
                                                      
                ! Call sub-flow routine
                call HMC_Phys_Convolution_Apps_SubFlow(iID, iRows, iCols, dDtDataForcing, dDtIntegrAct, iNDam)


                ! Call surface routing routine using a tank model
                oHMC_Vars(iID)%a2dQVolOut = 0.0
                call HMC_Phys_Convolution_Apps_SurfaceFlow_ChannelFraction(iID, iRows, iCols, &
                                                           dDtDataForcing, dDtIntegrAct, iTAct, iTq, iDtMax, &
                                                           iNData, iNDam, iNLake, & 
                                                           iNPlant, iNCatch, iNRelease, iNJoint, &
                                                           iTime, iNTime, iETime)

                ! Call discharge routine     
                call HMC_Phys_Convolution_Apps_Discharge_ChannelFraction(iID, iRows, iCols, iNSection, &
                                                         dDtDataForcing, dDtIntegrAct, iNTime, iTq, dDtMax)
                                                         
                ! Discharge temporal counter
                write(sTInt, *) iTInt; call mprintf(.true., iINFO_Extra, ' Phys :: Convolution :: IntStep: '//sTInt//' END' ) 
                iTq = iTq + 1
                
        enddo TimeIntLoop
        !------------------------------------------------------------------------------------------

        !------------------------------------------------------------------------------------------
        ! Build 3D variable for distributed discharge
       
        ! Initializing and updating discharge 3d field(s)
        if (all(oHMC_Vars(iID)%a3dQout.lt.0.0))then

            oHMC_Vars(iID)%a3dQout(:,:,int(iDaySteps)) =  oHMC_Vars(iID)%a2dQout
       
        else
            ! Re-initializing 
            do iStep=2, int(iDaySteps)
                oHMC_Vars(iID)%a3dQout(:,:,int(iStep-1)) = oHMC_Vars(iID)%a3dQout(:,:,int(iStep))
            enddo
            ! Updating with new field
            where(oHMC_Vars(iID)%a2dDEM.gt.0.0)
                oHMC_Vars(iID)%a3dQout(:,:,int(iDaySteps)) =  oHMC_Vars(iID)%a2dQout
            elsewhere
                oHMC_Vars(iID)%a3dQout(:,:,int(iDaySteps)) = -9999.0
            endwhere
            
        endif
        !------------------------------------------------------------------------------------------

        !------------------------------------------------------------------------------------------
        ! If Flooding is activated build the Average Qflooding maps
        if (iFlagFlood.eq.1) then
            ! Initializing and updating discharge 3d field(s)
            if (all(oHMC_Vars(iID)%a3dQfloodCR.lt.0.0))then
                oHMC_Vars(iID)%a3dQfloodCR(:,:,int(iDaySteps)) =  oHMC_Vars(iID)%a2dQfloodCR
               
            else
                ! Re-initializing 
                do iStep=2, int(iDaySteps)
                    oHMC_Vars(iID)%a3dQfloodCR(:,:,int(iStep-1)) = oHMC_Vars(iID)%a3dQfloodCR(:,:,int(iStep))
                enddo
                ! Updating with new field
                where(oHMC_Vars(iID)%a2dDEM.gt.0.0)
                    oHMC_Vars(iID)%a3dQfloodCR(:,:,int(iDaySteps)) =  oHMC_Vars(iID)%a2dQfloodCR
                elsewhere
                    oHMC_Vars(iID)%a3dQfloodCR(:,:,int(iDaySteps)) = -9999.0
                endwhere

            endif
            
            if (all(oHMC_Vars(iID)%a3dQfloodCL.lt.0.0))then

                oHMC_Vars(iID)%a3dQfloodCL(:,:,int(iDaySteps)) =  oHMC_Vars(iID)%a2dQfloodCL

            else
                ! Re-initializing 
                do iStep=2, int(iDaySteps)
                    oHMC_Vars(iID)%a3dQfloodCL(:,:,int(iStep-1)) = oHMC_Vars(iID)%a3dQfloodCL(:,:,int(iStep))
                enddo
                ! Updating with new field
                where(oHMC_Vars(iID)%a2dDEM.gt.0.0)
                    oHMC_Vars(iID)%a3dQfloodCL(:,:,int(iDaySteps)) =  oHMC_Vars(iID)%a2dQfloodCL
                elsewhere
                    oHMC_Vars(iID)%a3dQfloodCL(:,:,int(iDaySteps)) = -9999.0
                endwhere

            endif
            
        endif
        !------------------------------------------------------------------------------------------

        !------------------------------------------------------------------------------------------
        ! Call deep flow routine
        call HMC_Phys_Convolution_Apps_DeepFlow_ChannelFraction(iID, iRows, iCols, dDtDataForcing)
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
        call mprintf(.true., iINFO_Verbose, ' Phys :: Convolution [channel fraction] ... OK' )
        !------------------------------------------------------------------------------------------
        
    end subroutine HMC_Phys_Convolution_Cpl_ChannelFraction
    !------------------------------------------------------------------------------------------
    
end module HMC_Module_Phys_Convolution_Type_ChannelFraction
!------------------------------------------------------------------------------------------

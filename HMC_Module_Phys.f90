!------------------------------------------------------------------------------------------    
! File:   HMC_Module_Phys.f90
! Author(s): Fabio Delogu, Francesco Silvestro
!
! Date: 20190410
!
! Coupler for physics subroutine(s)
!------------------------------------------------------------------------------------------

!------------------------------------------------------------------------------------------
! Module Header
module HMC_Module_Phys
    
    !------------------------------------------------------------------------------------------
    ! External module(s) for all subroutine in this module
    use HMC_Module_Namelist,                                only:   oHMC_Namelist
    use HMC_Module_Vars_Loader,                             only:   oHMC_Vars
    
    use HMC_Module_Tools_Debug
    
    use HMC_Module_Phys_StateUpdating,                      only:   HMC_Phys_StateUpdating_Cpl
    use HMC_Module_Phys_Snow,                               only:   HMC_Phys_Snow_Cpl
    use HMC_Module_Phys_LSM,                                only:   HMC_Phys_LSM_Cpl
    use HMC_Module_Phys_ET,                                 only:   HMC_Phys_ET_Cpl
    use HMC_Module_Phys_Retention,                          only:   HMC_Phys_Retention_Cpl

    use HMC_Module_Phys_Convolution_Type_ChannelFraction,   only:   HMC_Phys_Convolution_Cpl_ChannelFraction
    use HMC_Module_Phys_Convolution_Type_ChannelNetwork,    only:   HMC_Phys_Convolution_Cpl_ChannelNetwork
    
    ! Implicit none for all subroutines in this module
    implicit none
    !------------------------------------------------------------------------------------------
    
contains
    
    !------------------------------------------------------------------------------------------
    ! Subroutine to run model physics
    subroutine HMC_Phys_Cpl(iID, &
                            iRowsStart, iRowsEnd, iColsStart, iColsEnd, &
                            iTime, iNTime, iETime, sTime, &
                            iNSection, iNData, &
                            iNLake, iNDam, iNPlant, iNJoint, iNCatch, iNRelease, &
                            iDaySteps, iTMarkedSteps )
        
        !------------------------------------------------------------------------------------------
        ! Variable(s)
        integer(kind = 4)           :: iID
        integer(kind = 4)           :: iFlagCType
        integer(kind = 4)           :: iRows, iRowsStart, iRowsEnd, iCols, iColsStart, iColsEnd
        integer(kind = 4)           :: iNSection, iNData
        integer(kind = 4)           :: iTime, iNTime, iETime
        integer(kind = 4)           :: iNLake, iNDam, iNPlant, iNJoint, iNCatch, iNRelease
        integer(kind = 4)           :: iDaySteps, iTMarkedSteps
        
        character(len = 19)         :: sTime
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Flag for selecting convolution type
        iFlagCType = oHMC_Namelist(iID)%iFlagCType
        
        ! Defining iRows and iCols
        iRows = iRowsEnd - iRowsStart + 1
        iCols = iColsEnd - iColsStart + 1
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Check time step (iT)
        if (iTime .le. iNTime) then
            
            !------------------------------------------------------------------------------------------
            ! Subroutine to update model state(s)
            call HMC_Phys_StateUpdating_Cpl(iID, iRows, iCols)
            !------------------------------------------------------------------------------------------
            
            !------------------------------------------------------------------------------------------
            ! Subroutine to compute SNOW
            call HMC_Phys_Snow_Cpl(iID, iRows, iCols)
            !------------------------------------------------------------------------------------------

            !------------------------------------------------------------------------------------------
            ! Subroutine to compute LSM
            call HMC_Phys_LSM_Cpl(iID, iRows, iCols, sTime)
            !------------------------------------------------------------------------------------------

        else
            
            !------------------------------------------------------------------------------------------
            ! Extra steps condition
            oHMC_Vars(iID)%a2dET = 5.0/24.0*real(oHMC_Namelist(iID)%iDtData_Forcing)/3600
            oHMC_Vars(iID)%a2dETpot = 5.0/24.0*real(oHMC_Namelist(iID)%iDtData_Forcing)/3600
            
            ! Info message for extra time step(s)
            call mprintf(.true., iINFO_Extra, ' Extra time step ---> Updating state(s) routine(s) are skipped!')
            call mprintf(.true., iINFO_Extra, ' Extra time step ---> Energy balance routine(s) are skipped!')
            call mprintf(.true., iINFO_Extra, ' Extra time step ---> Swow routine(s) are skipped!')
            call mprintf(.true., iINFO_Extra, ' Extra time step ---> Convolution routine(s) are activated!')
            !------------------------------------------------------------------------------------------
            
        endif
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Subroutine to compute ET
        call HMC_Phys_ET_Cpl(iID, iRows, iCols, iTime, iDaySteps, sTime, iNLake, iNDam)
        !------------------------------------------------------------------------------------------

        !------------------------------------------------------------------------------------------
        ! Subroutine to compute Retention
        call HMC_Phys_Retention_Cpl(iID, iRows, iCols)
        !------------------------------------------------------------------------------------------

        !------------------------------------------------------------------------------------------
        ! Select convolution type
        if(iFlagCType .eq. 2)then
            
            ! Subroutine to compute convolution for channel fraction (for all steps including extra steps)
            call HMC_Phys_Convolution_Cpl_ChannelFraction(iID, &
                                      iRows, iCols, &
                                      iTime, iNTime, iETime, &
                                      iNSection, iNData, &
                                      iNLake, iNDam, &
                                      iNPlant, iNCatch, iNRelease, iNJoint, sTime, iDaySteps)
        else
            
            ! Subroutine to compute convolution for channel network (for all steps including extra steps)
            call HMC_Phys_Convolution_Cpl_ChannelNetwork(iID, &
                                      iRows, iCols, &
                                      iTime, iNTime, iETime, &
                                      iNSection, iNData, &
                                      iNLake, iNDam, &
                                      iNPlant, iNCatch, iNRelease, iNJoint, sTime, iDaySteps)
        endif
        !------------------------------------------------------------------------------------------

    end subroutine HMC_Phys_Cpl
    !------------------------------------------------------------------------------------------
    
end module HMC_Module_Phys
!------------------------------------------------------------------------------------------ù


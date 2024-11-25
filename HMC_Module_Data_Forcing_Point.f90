!------------------------------------------------------------------------------------------    
! File:   HMC_Module_Data_Forcing_Point.f90
! Author(s): Fabio Delogu, Francesco Silvestro, Simone Gabellani
!
! Created on April 22, 2015, 5:19 PM
!------------------------------------------------------------------------------------------

!------------------------------------------------------------------------------------------
! Module Header
module HMC_Module_Data_Forcing_Point
    
    !------------------------------------------------------------------------------------------
    ! External module(s) for all subroutine in this module
    use HMC_Module_Namelist,                    only:   oHMC_Namelist
    use HMC_Module_Vars_Loader,                 only:   oHMC_Vars
    
    use HMC_Module_Tools_Debug
    use HMC_Module_Tools_Generic,               only:   HMC_Tools_Generic_SmoothTimeSeries, &
                                                        HMC_Tools_Generic_ReplaceText
    
    use HMC_Module_Tools_IO,                    only:   HMC_Tools_IO_GetNd_ASCII
                             
    ! Implicit none for all subroutines in this module
    implicit none
    !------------------------------------------------------------------------------------------
    
contains
    
    !------------------------------------------------------------------------------------------
    ! Subroutine to manage forcing point data
    subroutine HMC_Data_Forcing_Point_Cpl(iID, sTime, &
                                          iNData, iETime, &  
                                          iNLake, iNDam, iNPlant, iNJoint, iNCatch, iNRelease)
        
        !------------------------------------------------------------------------------------------
        ! Variable(s) declaration
        integer(kind = 4)           :: iID
        character(len = 19)         :: sTime
        
        integer(kind = 4)           :: iNData, iETime
        integer(kind = 4)           :: iNLake, iNDam, iNPlant, iNJoint, iNCatch, iNRelease
        
        integer(kind = 4)           :: iFlagTypeData_Forcing
        character(len = 700)        :: sPathData_Forcing
        !------------------------------------------------------------------------------------------

        !------------------------------------------------------------------------------------------
        ! Get global information
        sPathData_Forcing = oHMC_Namelist(iID)%sPathData_Forcing_Point
        iFlagTypeData_Forcing = oHMC_Namelist(iID)%iFlagTypeData_Forcing_Point
        !------------------------------------------------------------------------------------------

        !------------------------------------------------------------------------------------------
        ! Replace general path with specific time feature(s)
        call HMC_Tools_Generic_ReplaceText(sPathData_Forcing, '$yyyy', sTime(1:4))
        call HMC_Tools_Generic_ReplaceText(sPathData_Forcing, '$mm', sTime(6:7))
        call HMC_Tools_Generic_ReplaceText(sPathData_Forcing, '$dd', sTime(9:10))
        call HMC_Tools_Generic_ReplaceText(sPathData_Forcing, '$HH', sTime(12:13))
        call HMC_Tools_Generic_ReplaceText(sPathData_Forcing, '$MM', sTime(15:16))
        !------------------------------------------------------------------------------------------

        !------------------------------------------------------------------------------------------
        ! Info start
        call mprintf(.true., iINFO_Extra, ' Data :: Forcing point ... ' )
        !------------------------------------------------------------------------------------------

        !------------------------------------------------------------------------------------------
        ! Subroutine for reading sequential ASCII forcing data point
        if (iFlagTypeData_Forcing == 1) then

            !------------------------------------------------------------------------------------------
            ! Get point data for dam(s) (1d array)
            call HMC_Data_Forcing_Point_Dam(iID, sPathData_Forcing, iNDam, sTime)
            !------------------------------------------------------------------------------------------

        endif
        !------------------------------------------------------------------------------------------

        !------------------------------------------------------------------------------------------
        ! Subroutine for reading unknown forcing data point
        if (iFlagTypeData_Forcing .ne. 1) then

            !------------------------------------------------------------------------------------------
            ! Choosing data type
            call mprintf(.true., iERROR, ' Using UNKNOWN data point forcing. Check settings file!')
            !------------------------------------------------------------------------------------------

        endif
        !------------------------------------------------------------------------------------------

        !------------------------------------------------------------------------------------------
        ! Info end
        call mprintf(.true., iINFO_Extra, ' Data :: Forcing point ... OK' )
        !------------------------------------------------------------------------------------------
            
    end subroutine HMC_Data_Forcing_Point_Cpl
    !------------------------------------------------------------------------------------------
    
    !------------------------------------------------------------------------------------------
    ! Subroutine to get data of dam(s)
    subroutine HMC_Data_Forcing_Point_Dam(iID, sPathData_Forcing, iNDam, sTime)
        
        !------------------------------------------------------------------------------------------
        ! Variable(s) declaration
        integer(kind = 4)                   :: iID
        integer(kind = 4)                   :: iNDam, iErr
        character(len = 19)                 :: sTime
        character(len = 700)                :: sPathData_Forcing, sFileNameData_Forcing
       
        real(kind = 4), dimension(iNDam)    :: a1dVarData, a1dVarDamV
        real(kind = 4), dimension(iNDam, 3) :: a2dVarData
        
        logical                             :: bFileExist
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Initialize variable(s)
        a1dVarData = -9999.0; a1dVarDamV = -9999.0
        bFileExist = .false.
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Re-initialize global variable(s)
        oHMC_Vars(iID)%a1dVDamObs = -9999.0
        
        ! Info start
        call mprintf(.true., iINFO_Extra, ' Data :: Forcing point :: ASCII ... ' )

        ! Info file(s) time step
        call mprintf(.true., iINFO_Verbose, ' Get (forcing point) at time '//trim(sTime)//' ... ')
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Filename forcing (example: hmc.forcing-point.dam_201404300000.txt)
        sFileNameData_Forcing = trim(sPathData_Forcing)//"damv.db."// &
        sTime(1:4)//sTime(6:7)//sTime(9:10)// & 
        sTime(12:13)//sTime(15:16)// &
        ".txt"
        ! Info file
        call mprintf(.true., iINFO_Extra, ' Get filename (forcing point): '//trim(sFileNameData_Forcing) )

        ! Debug
        call mprintf(.true., iINFO_Extra, checkarray(a1dVarDamV, 'DAM VOLUME START') )
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Check dam(s) availability
        if (iNDam .gt. 0) then
        
            !------------------------------------------------------------------------------------------
            ! Checking file input availability
            inquire (file = sFileNameData_Forcing, exist = bFileExist)
            if ( .not. bFileExist ) then
                ! Warning message
                call mprintf(.true., iWARN, ' No forcing point ASCII data found (dam volume): '//trim(sFileNameData_Forcing) )
                ! Info filename
                call mprintf(.true., iINFO_Verbose, &
                         ' Get filename (forcing point dams): '//trim(sFileNameData_Forcing)//' ... FAILED' )
                a1dVarDamV = -9999.0
                
                ! Info file(s) time step
                call mprintf(.true., iINFO_Verbose, ' Get (forcing point) at time '//trim(sTime)//' ... FAILED')
                ! Info end
                call mprintf(.true., iINFO_Extra, ' Data :: Forcing gridded :: ASCII ... FAILED' )
                
            else
                ! Read ASCII data (dam volume)
                call HMC_Tools_IO_GetNd_ASCII(sFileNameData_Forcing, a2dVarData, iNDam, 3, .false., iErr) 
                a1dVarDamV = a2dVarData(:,2)
                
                ! Info file(s) time step
                call mprintf(.true., iINFO_Verbose, ' Get (forcing gridded) at time '//trim(sTime)//' ... OK')
                ! Info end
                call mprintf(.true., iINFO_Extra, ' Data :: Forcing gridded :: ASCII ... OK' )
            endif
            !------------------------------------------------------------------------------------------
            
        else
            
            !------------------------------------------------------------------------------------------
            ! No dam(s)
            a1dVarDamV = -9999.0
            ! Info file(s) time step
            call mprintf(.true., iINFO_Verbose, ' Get (forcing point) at time '//trim(sTime)//' ... SKIPPED')
            ! Info end
            call mprintf(.true., iINFO_Extra, ' Data :: Forcing gridded :: ASCII ... SKIPPED' )
            !------------------------------------------------------------------------------------------
            
        endif
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Debug
        call mprintf(.true., iINFO_Extra, checkarray(a1dVarDamV, 'DAM VOLUME END') )
        
        ! Pass local variable(s) to global workspace
        oHMC_Vars(iID)%a1dVDamObs = a1dVarDamV
        !------------------------------------------------------------------------------------------
             
    end subroutine HMC_Data_Forcing_Point_Dam
    !------------------------------------------------------------------------------------------
    
end module HMC_Module_Data_Forcing_Point
!------------------------------------------------------------------------------------------
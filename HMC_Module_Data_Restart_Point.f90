!------------------------------------------------------------------------------------------    
! File:   HMC_Module_Data_Restart_Point.f90
! Author(s): Fabio Delogu, Francesco Silvestro, Simone Gabellani
!
! Created on May 20, 2015, 10:15 AM
!------------------------------------------------------------------------------------------

!------------------------------------------------------------------------------------------
! Module Header
module HMC_Module_Data_Restart_Point
    
    !------------------------------------------------------------------------------------------
    ! External module(s) for all subroutine in this module
    use HMC_Module_Namelist,        only:   oHMC_Namelist
    use HMC_Module_Vars_Loader,     only:   oHMC_Vars
                                            
    use HMC_Module_Tools_Debug
    use HMC_Module_Tools_Generic,   only:   HMC_Tools_Generic_ReplaceText, &
                                            HMC_Tools_Generic_CreateFolder
    
    ! Implicit none for all subroutines in this module
    implicit none
    !------------------------------------------------------------------------------------------
    
contains
    
    !------------------------------------------------------------------------------------------
    ! Subroutine to manage restart point data
    subroutine HMC_Data_Restart_Point_Cpl( iID, sTime, &
                                           iNDam, iNLake)
        
        !------------------------------------------------------------------------------------------
        ! Variable(s)
        integer(kind = 4)               :: iID
        integer(kind = 4)               :: iNDam, iNLake
        integer(kind = 4)               :: iFlagRestart, iFlagTypeData_Restart
        
        character(len = 700)            :: sPathData_Restart
        character(len = 700)            :: sFileNameData_Restart
        character(len = 700)            :: sCommandCreateFolder
        
        character(len = 19), intent(in)             :: sTime
        
        integer(kind = 4), dimension(iNDam, 2)      :: a2iVarXYDam
        real(kind = 4), dimension(iNDam)            :: a1dVarVDam, a1dVarVMaxDam, a1dVarCodeDam
        
        integer(kind = 4), dimension(iNLake, 2)     :: a2iVarXYLake
        real(kind = 4), dimension(iNLake)           :: a1dVarVLake, a1dVarVMinLake, a1dVarCodeLake
        
        logical                                     :: bCheckRestart
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Initialize variable(s)
        a2iVarXYDam = -9999; a2iVarXYLake = -9999;
        a1dVarVDam = -9999.0; a1dVarVMaxDam = -9999.0; a1dVarCodeDam = -9999.0;
        a1dVarVLake = -9999.0; a1dVarVMinLake = -9999.0; a1dVarCodeLake = -9999.0; 
        
        sFileNameData_Restart = ""; 
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Get global information
        iFlagRestart = oHMC_Namelist(iID)%iFlagRestart
        sPathData_Restart = oHMC_Namelist(iID)%sPathData_Restart_Point
        iFlagTypeData_Restart = oHMC_Namelist(iID)%iFlagTypeData_Restart_Point
        sCommandCreateFolder = oHMC_Namelist(iID)%sCommandCreateFolder
        ! Info start
        call mprintf(.true., iINFO_Extra, ' Data :: Restart point ... ' )
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Check restart flag value
        if (iFlagRestart == 1) then
        
            !------------------------------------------------------------------------------------------
            ! Check dam or lake availability
            if ( (iNDam.gt.0) .or. (iNLake.gt.0) ) then

                !------------------------------------------------------------------------------------------
                ! Replace general path with specific time feature(s)
                call HMC_Tools_Generic_ReplaceText(sPathData_Restart, '$yyyy', sTime(1:4))
                call HMC_Tools_Generic_ReplaceText(sPathData_Restart, '$mm', sTime(6:7))
                call HMC_Tools_Generic_ReplaceText(sPathData_Restart, '$dd', sTime(9:10))
                call HMC_Tools_Generic_ReplaceText(sPathData_Restart, '$HH', sTime(12:13))
                call HMC_Tools_Generic_ReplaceText(sPathData_Restart, '$MM', sTime(15:16))
                !------------------------------------------------------------------------------------------

                !------------------------------------------------------------------------------------------
                ! Create output folder
                call HMC_Tools_Generic_CreateFolder(sCommandCreateFolder, sPathData_Restart, .true.)
                !------------------------------------------------------------------------------------------

                !------------------------------------------------------------------------------------------
                ! Subroutine for getting ASCII point data restart
                if (iFlagTypeData_Restart == 1) then

                    !------------------------------------------------------------------------------------------
                    ! Choosing data type
                    call mprintf(.true., iINFO_Extra, ' Using ASCII data restart point')
                    !------------------------------------------------------------------------------------------

                    !------------------------------------------------------------------------------------------
                    ! Filename (StateStorage)
                    sFileNameData_Restart = trim(sPathData_Restart)//"hmc.state-point."// &
                    sTime(1:4)//sTime(6:7)//sTime(9:10)// & 
                    sTime(12:13)//sTime(15:16)// &
                    ".txt"

                    ! Info filename
                    call mprintf(.true., iINFO_Verbose, ' Get filename (state point): '//trim(sFileNameData_Restart) )
                    !------------------------------------------------------------------------------------------

                    !------------------------------------------------------------------------------------------
                    ! Call subroutine to get data point in ASCII format
                    call HMC_Data_Restart_Point_ASCII(iID, sTime, &
                                                    sFileNameData_Restart, &
                                                    iNDam, iNLake, &
                                                    a2iVarXYDam, &
                                                    a1dVarVDam, a1dVarVMaxDam, a1dVarCodeDam, &
                                                    a2iVarXYLake, &
                                                    a1dVarVLake, a1dVarVMinLake, a1dVarCodeLake, & 
                                                    .false., bCheckRestart)               
                    !------------------------------------------------------------------------------------------

                endif
                !------------------------------------------------------------------------------------------

                !------------------------------------------------------------------------------------------
                ! Subroutine to write data point in UNKWOWN format
                if (iFlagTypeData_Restart == 2) then

                    !------------------------------------------------------------------------------------------
                    ! Choosing data type
                    call mprintf(.true., iERROR, ' Writing UNKNOWN data point state. Check settings file!')
                    !------------------------------------------------------------------------------------------

                endif
                !------------------------------------------------------------------------------------------
                
                !------------------------------------------------------------------------------------------
                ! Check restart flag on data availability
                if (bCheckRestart .eqv. .true.) then
                
                    !------------------------------------------------------------------------------------------
                    ! Pass variable(s) to global workspace
                    oHMC_Vars(iID)%a1dVDam = a1dVarVDam
                    oHMC_Vars(iID)%a1dVMaxDam = a1dVarVMaxDam
                    oHMC_Vars(iID)%a1dCodeDam = a1dVarCodeDam
                    oHMC_Vars(iID)%a1dVLake = a1dVarVLake
                    oHMC_Vars(iID)%a1dVMinLake = a1dVarVMinLake
                    oHMC_Vars(iID)%a1dCodeLake = a1dVarCodeLake
                    
                    ! Info end
                    call mprintf(.true., iINFO_Extra, ' Data :: Restart point ... OK' )
                    !------------------------------------------------------------------------------------------
                
                else
                    !------------------------------------------------------------------------------------------
                    ! Exit message for not using restart data
                    call mprintf(.true., iINFO_Verbose, ' Restart flag selected but data are N/A (point data)')
                    ! Info end
                    call mprintf(.true., iINFO_Extra, ' Data :: Restart point ... SKIPPED ' )
                    !------------------------------------------------------------------------------------------
                
                endif
                !------------------------------------------------------------------------------------------
                
            else

                !------------------------------------------------------------------------------------------
                ! Choosing data type
                call mprintf(.true., iINFO_Verbose, ' Dam(s) and lake(s) are not available for this run!')
                ! Info end
                call mprintf(.true., iINFO_Extra, ' Data :: Restart point ... SKIPPED' )
                !------------------------------------------------------------------------------------------

            endif
            !------------------------------------------------------------------------------------------
        
        else
            !------------------------------------------------------------------------------------------
            ! Exit message for not using restart data
            call mprintf(.true., iINFO_Verbose, ' No restart run selected (point data)')
            ! Info end
            call mprintf(.true., iINFO_Extra, ' Data :: Restart point ... SKIPPED' )
            !------------------------------------------------------------------------------------------
        endif
        !------------------------------------------------------------------------------------------
        
    end subroutine HMC_Data_Restart_Point_Cpl
    !------------------------------------------------------------------------------------------
    
    !------------------------------------------------------------------------------------------
    ! Subroutine to write state point data in ASCII format
    subroutine HMC_Data_Restart_Point_ASCII(iID, sTime, &
                                          sFileNameData_State, &
                                          iNDam, iNLake, &
                                          a2iVarXYDam, &
                                          a1dVarVDam, a1dVarVMaxDam, a1dVarCodeDam, &
                                          a2iVarXYLake, &
                                          a1dVarVLake, a1dVarVMinLake, a1dVarCodeLake, &
                                          bFatalError, bCheckRestart)
        
        !------------------------------------------------------------------------------------------
        ! Variable(s) 
        integer(kind = 4)                           :: iID, iD, iL
        integer(kind = 4)                           :: iNDam, iNLake     
        
        character(len = 700)                        :: sFileNameData_State
        character(len = 19)                         :: sTime
                                          
        integer(kind = 4), dimension(iNDam, 2)      :: a2iVarXYDam
        real(kind = 4), dimension(iNDam)            :: a1dVarVDam, a1dVarVMaxDam, a1dVarCodeDam
        
        integer(kind = 4), dimension(iNLake, 2)     :: a2iVarXYLake
        real(kind = 4), dimension(iNLake)           :: a1dVarVLake, a1dVarVMinLake, a1dVarCodeLake
        
        logical                                     :: bFatalError, bFileExist, bCheckRestart
        integer(kind = 4)                           :: iRet, iErr
        !------------------------------------------------------------------------------------------                                  
        
        !------------------------------------------------------------------------------------------ 
        ! Initialize variable(s)
        bCheckRestart = .false.
        !------------------------------------------------------------------------------------------ 
        
        !------------------------------------------------------------------------------------------  
        ! Info start
        call mprintf(.true., iINFO_Extra, ' Data :: Restart point :: ASCII ... ' )

        ! Info filename(s) at each time step
        call mprintf(.true., iINFO_Basic, ' Get (restart point) at time '//trim(sTime)//' ... ')
        !------------------------------------------------------------------------------------------

        !------------------------------------------------------------------------------------------
        ! Open file
        open(unit = 20, file = trim(sFileNameData_State), status = 'old', iostat = iRet)
        inquire (file = trim(sFileNameData_State), exist = bFileExist, iostat = iRet)
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Check file availability
        if (.not. bFileExist ) then
            
            !------------------------------------------------------------------------------------------
            ! Check fatal error condition
            if (bFatalError) then
                call mprintf(.true., iERROR, &
                            ' INQUIRE FAILED :: state point ASCII ---> file not readable (filename: ' &
                            //trim(sFileNameData_State)//')')
            else
                call mprintf(.true., iWARN, &
                            ' INQUIRE FAILED :: state point ASCII ---> file not readable (filename: ' &
                            //trim(sFileNameData_State)//')')
                iErr = iRet
         
            endif
            
            bCheckRestart = .false.
            !------------------------------------------------------------------------------------------
            
        elseif ( bFileExist ) then
            
            !------------------------------------------------------------------------------------------
            ! Check file opening
            if (iRet == 0) then 
                
                !------------------------------------------------------------------------------------------
                ! Info
                call mprintf(.true., iINFO_Extra, &
                            ' READ OK :: state point ASCII ---> file readable (filename: '//trim(sFileNameData_State)//')')
                !------------------------------------------------------------------------------------------
                
                !------------------------------------------------------------------------------------------
                ! Write dam(s) data point
                do iD = 1, iNDam
                    ! Dam index coordinates (i,j) 
                    read(20,*) a2iVarXYDam(iD,2), a2iVarXYDam(iD,1)
                    ! Dam code (on choice matrix)
                    read(20,*) a1dVarCodeDam(iD)
                    ! Dam maximum volume
                    read(20,*) a1dVarVMaxDam(iD)
                    ! Dam volume
                    read(20,*) a1dVarVDam(iD)
                    ! Blank space
                    read(20,*)
                enddo
                
                ! Write lake(s) data point
                do iL = 1, iNLake
                    ! Blank space
                    read(20,*)
                    ! Lake index coordinates (i,j) 
                    read(20,*)a2iVarXYLake(iL,2), a2iVarXYLake(iL,1)
                    ! Lake code (on choice matrix)
                    read(20,*)a1dVarCodeLake(iL)
                    ! Lake minimum volume (to guarantee discharge)
                    read(20,*)a1dVarVMinLake(iL)
                    ! Lake volume 
                    read(20,*)a1dVarVLake(iL)
                enddo
                !------------------------------------------------------------------------------------------
                
                !------------------------------------------------------------------------------------------
                ! Close point state file
                close(20)
                
                ! Info end
                bCheckRestart = .true.
                !------------------------------------------------------------------------------------------
                
            elseif (iRet /= 0) then
                
                !------------------------------------------------------------------------------------------
                ! Check fatal error
                if (bFatalError) then
                    call mprintf(.true., iERROR, &
                            ' READ FAILED :: state point ASCII ---> file not readable (filename: '//trim(sFileNameData_State)//')')
                else
                    call mprintf(.true., iWARN, &
                            ' READ FAILED :: state point ASCII ---> file not readable (filename: '//trim(sFileNameData_State)//')')
                    iErr = iRet

                endif
                
                bCheckRestart = .false.
                !------------------------------------------------------------------------------------------
                   
            endif
            !------------------------------------------------------------------------------------------

        endif
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Info filename(s) at each time step
        call mprintf(.true., iINFO_Basic, ' Get (restart point) at time '//trim(sTime)//' ... OK')
        ! Info start
        call mprintf(.true., iINFO_Extra, ' Data :: Restart point :: ASCII ... OK' )
        !------------------------------------------------------------------------------------------
        
  
        !------------------------------------------------------------------------------------------
        ! Check restart
        if (bCheckRestart .eqv. .true.) then
            call mprintf(.true., iINFO_Basic, ' Data :: Restart point :: ASCII :: All variable(s) are loaded! ' )
            bCheckRestart = .true.
        else
            call mprintf(.true., iINFO_Basic, ' Data :: Restart point :: ASCII :: Some/All variable(s) are N/A! ' )
            call mprintf(.true., iWARN, ' Restart flag activated but some data restart are not available! ')
            call mprintf(.true., iWARN, ' Restart point conditions are null! ')
            bCheckRestart = .false.
        endif
        !------------------------------------------------------------------------------------------

    end subroutine HMC_Data_Restart_Point_ASCII
    !------------------------------------------------------------------------------------------
    
end module HMC_Module_Data_Restart_Point
!------------------------------------------------------------------------------------------

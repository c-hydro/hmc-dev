!------------------------------------------------------------------------------------------    
! File:   HMC_Module_Data_State_Point.f90
! Author(s): Fabio Delogu, Francesco Silvestro, Simone Gabellani
!
! Created on May 19, 2015, 5:53 PM
!------------------------------------------------------------------------------------------

!------------------------------------------------------------------------------------------
! Module Header
module HMC_Module_Data_State_Point
    
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
    ! Subroutine to manage state point data
    subroutine HMC_Data_State_Point_Cpl( iID, sTime, &
                                         iNDam, iNLake)
        
        !------------------------------------------------------------------------------------------
        ! Variable(s)
        integer(kind = 4)               :: iID
        integer(kind = 4)               :: iNDam, iNLake
        integer(kind = 4)               :: iFlagTypeData_State
        
        character(len = 700)            :: sPathData_State
        character(len = 700)            :: sFileNameData_State
        character(len = 700)            :: sCommandCreateFolder
        
        character(len = 19), intent(in)         :: sTime
        
        integer(kind = 4), dimension(iNDam, 2)  :: a2iVarXYDam
        real(kind = 4), dimension(iNDam)        :: a1dVarVDam, a1dVarVMaxDam, a1dVarCodeDam
        
        integer(kind = 4), dimension(iNLake, 2)  :: a2iVarXYLake
        real(kind = 4), dimension(iNLake)        :: a1dVarVLake, a1dVarVMinLake, a1dVarCodeLake
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Initialize variable(s)
        a2iVarXYDam = -9999; a2iVarXYLake = -9999;
        a1dVarVDam = -9999.0; a1dVarVMaxDam = -9999.0; a1dVarCodeDam = -9999.0;
        a1dVarVLake = -9999.0; a1dVarVMinLake = -9999.0; a1dVarCodeLake = -9999.0; 
        
        sFileNameData_State = ""; 
        
        ! Info start
        call mprintf(.true., iINFO_Extra, ' Data :: State point ... ' )
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Check dam or lake availability
        if ( (iNDam.gt.0) .or. (iNLake.gt.0) ) then
        
            !------------------------------------------------------------------------------------------
            ! Get global information
            sPathData_State = oHMC_Namelist(iID)%sPathData_State_Point
            iFlagTypeData_State = oHMC_Namelist(iID)%iFlagTypeData_State_Point
            sCommandCreateFolder = oHMC_Namelist(iID)%sCommandCreateFolder
            !------------------------------------------------------------------------------------------

            !------------------------------------------------------------------------------------------
            ! Replace general path with specific time feature(s)
            call HMC_Tools_Generic_ReplaceText(sPathData_State, '$yyyy', sTime(1:4))
            call HMC_Tools_Generic_ReplaceText(sPathData_State, '$mm', sTime(6:7))
            call HMC_Tools_Generic_ReplaceText(sPathData_State, '$dd', sTime(9:10))
            call HMC_Tools_Generic_ReplaceText(sPathData_State, '$HH', sTime(12:13))
            call HMC_Tools_Generic_ReplaceText(sPathData_State, '$MM', sTime(15:16))
            !------------------------------------------------------------------------------------------

            !------------------------------------------------------------------------------------------
            ! Create output folder
            call HMC_Tools_Generic_CreateFolder(sCommandCreateFolder, sPathData_State, .true.)
            !------------------------------------------------------------------------------------------

            !------------------------------------------------------------------------------------------
            ! Get variable(s)
            a2iVarXYDam = oHMC_Vars(iID)%a2iXYDam;
            a1dVarVDam = oHMC_Vars(iID)%a1dVDam
            a1dVarVMaxDam = oHMC_Vars(iID)%a1dVMaxDam
            a1dVarCodeDam = oHMC_Vars(iID)%a1dCodeDam

            a2iVarXYLake = oHMC_Vars(iID)%a2iXYLake
            a1dVarVLake = oHMC_Vars(iID)%a1dVLake
            a1dVarVMinLake = oHMC_Vars(iID)%a1dVMinLake
            a1dVarCodeLake = oHMC_Vars(iID)%a1dCodeLake
            !------------------------------------------------------------------------------------------

            !------------------------------------------------------------------------------------------
            ! Subroutine for writing ascii point state data
            if (iFlagTypeData_State == 1) then

                !------------------------------------------------------------------------------------------
                ! Call subroutine to write data point in ASCII format
                call HMC_Data_State_Point_ASCII(iID, &
                                                sPathData_State, &
                                                iNDam, iNLake, &
                                                sTime, &
                                                a2iVarXYDam, &
                                                a1dVarVDam, a1dVarVMaxDam, a1dVarCodeDam, &
                                                a2iVarXYLake, &
                                                a1dVarVLake, a1dVarVMinLake, a1dVarCodeLake)               
                !------------------------------------------------------------------------------------------

            endif
            !------------------------------------------------------------------------------------------

            !------------------------------------------------------------------------------------------
            ! Subroutine to write data point in UNKWOWN format
            if (iFlagTypeData_State == 2) then

                !------------------------------------------------------------------------------------------
                ! Choosing data type
                call mprintf(.true., iERROR, ' Writing UNKNOWN data point state. Check settings file!')
                !------------------------------------------------------------------------------------------

            endif
            !------------------------------------------------------------------------------------------
            
            !------------------------------------------------------------------------------------------
            ! Info end
            call mprintf(.true., iINFO_Extra, ' Data :: State point ... OK' )
            !------------------------------------------------------------------------------------------
            
        else
            
            !------------------------------------------------------------------------------------------
            ! Exit message
            call mprintf(.true., iINFO_Verbose, ' Dam(s) and lake(s) are not available for this run!')
            ! Info end
            call mprintf(.true., iINFO_Extra, ' Data :: State point ... SKIPPED' )
            !------------------------------------------------------------------------------------------
            
        endif
        !------------------------------------------------------------------------------------------
        
    end subroutine HMC_Data_State_Point_Cpl
    !------------------------------------------------------------------------------------------
    
    !------------------------------------------------------------------------------------------
    ! Subroutine to write state point data in ASCII format
    subroutine HMC_Data_State_Point_ASCII(iID, &
                                          sPathData_State, &
                                          iNDam, iNLake, &
                                          sTime, &
                                          a2iVarXYDam, &
                                          a1dVarVDam, a1dVarVMaxDam, a1dVarCodeDam, &
                                          a2iVarXYLake, &
                                          a1dVarVLake, a1dVarVMinLake, a1dVarCodeLake)
        
        !------------------------------------------------------------------------------------------
        ! Variable(s) 
        integer(kind = 4)                           :: iID, iD, iL
        integer(kind = 4)                           :: iNDam, iNLake     
        
        character(len = 256), intent(in)            :: sPathData_State 
        character(len = 700)                        :: sFileNameData_State
        character(len = 19)                         :: sTime
                                          
        integer(kind = 4), dimension(iNDam, 2)      :: a2iVarXYDam
        real(kind = 4), dimension(iNDam)            :: a1dVarVDam, a1dVarVMaxDam, a1dVarCodeDam
        
        integer(kind = 4), dimension(iNLake, 2)     :: a2iVarXYLake
        real(kind = 4), dimension(iNLake)           :: a1dVarVLake, a1dVarVMinLake, a1dVarCodeLake
        
        logical                                     :: bFatalError, bFileExist
        integer(kind = 4)                           :: iRet, iErr
        !------------------------------------------------------------------------------------------                                  
        
        !------------------------------------------------------------------------------------------
        ! Info end
        call mprintf(.true., iINFO_Extra, ' Data :: State point :: ASCII ... ' )
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Filename (StateStorage)
        sFileNameData_State = trim(sPathData_State)//"hmc.state-point."// &
        sTime(1:4)//sTime(6:7)//sTime(9:10)// & 
        sTime(12:13)//sTime(15:16)// &
        ".txt"

        ! Info
        call mprintf(.true., iINFO_Verbose, ' Save filename (state point): '//trim(sFileNameData_State)//' ... ')
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Open file
        inquire (file = trim(sFileNameData_State), exist = bFileExist)
        
        if (.not. bFileExist ) then
            open(unit = 20, file = trim(sFileNameData_State), status = 'new', iostat = iRet)
        elseif ( bFileExist ) then
            open(unit = 20, file = trim(sFileNameData_State), status = 'replace', iostat = iRet)
        endif
        
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
                return
            endif
            !------------------------------------------------------------------------------------------
            
        elseif ( bFileExist ) then
            
            !------------------------------------------------------------------------------------------
            ! Check file opening
            if (iRet == 0) then 
                
                !------------------------------------------------------------------------------------------
                ! Info
                call mprintf(.true., iINFO_Extra, &
                            ' WRITE OK :: state point ASCII ---> file writable (filename: '//trim(sFileNameData_State)//')')
                !------------------------------------------------------------------------------------------
                
                !------------------------------------------------------------------------------------------
                ! Write dam(s) data point
                do iD = 1, iNDam
                    ! Dam index coordinates (i,j) 
                    write(20,*) a2iVarXYDam(iD,2), a2iVarXYDam(iD,1)
                    ! Dam code (on choice matrix)
                    write(20,*) a1dVarCodeDam(iD)
                    ! Dam maximum volume
                    write(20,*) a1dVarVMaxDam(iD)
                    ! Dam volume
                    write(20,*) a1dVarVDam(iD)
                    ! Blank space
                    write(20,*)
                enddo
                
                ! Write lake(s) data point
                do iL = 1, iNLake
                    ! Blank space
                    write(20,*)
                    ! Lake index coordinates (i,j) 
                    write(20,*)a2iVarXYLake(iL,2), a2iVarXYLake(iL,1)
                    ! Lake code (on choice matrix)
                    write(20,*)a1dVarCodeLake(iL)
                    ! Lake minimum volume (to guarantee discharge)
                    write(20,*)a1dVarVMinLake(iL)
                    ! Lake volume 
                    write(20,*)a1dVarVLake(iL)
                enddo
                !------------------------------------------------------------------------------------------
                
                !------------------------------------------------------------------------------------------
                ! Close point state file
                close(20)
                !------------------------------------------------------------------------------------------
                
                !------------------------------------------------------------------------------------------
                ! Info filename
                call mprintf(.true., iINFO_Verbose, ' Save filename (state point): '//trim(sFileNameData_State)//' ... OK')
                ! Info end
                call mprintf(.true., iINFO_Extra, ' Data :: State point :: ASCII ... OK' )
                !------------------------------------------------------------------------------------------
  
            elseif (iRet /= 0) then
                
                !------------------------------------------------------------------------------------------
                ! Check fatal error
                if (bFatalError) then
                    call mprintf(.true., iERROR, &
                            ' WRITE FAILED :: state point ASCII ---> file not writable (filename: '//trim(sFileNameData_State)//')')
                else
                    call mprintf(.true., iWARN, &
                            ' WRITE FAILED :: state point ASCII ---> file not writable (filename: '//trim(sFileNameData_State)//')')
                    iErr = iRet
                    return
                endif
                !------------------------------------------------------------------------------------------
                   
            endif
            !------------------------------------------------------------------------------------------

        endif
        !------------------------------------------------------------------------------------------

    end subroutine HMC_Data_State_Point_ASCII
    !------------------------------------------------------------------------------------------
    
end module HMC_Module_Data_State_Point
!------------------------------------------------------------------------------------------

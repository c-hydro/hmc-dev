!------------------------------------------------------------------------------------------    
! File:   HMC_Module_Data_Updating_Gridded.f90
! Author(s): Fabio Delogu, Valerio Basso
!
! Created on December 19, 2017, 1:19 PM
!------------------------------------------------------------------------------------------

!------------------------------------------------------------------------------------------
! Module Header
module HMC_Module_Data_Updating_Gridded
    
    !------------------------------------------------------------------------------------------
    ! External module(s) for all subroutine in this module
#ifdef LIB_NC
    use netcdf
#endif

    use HMC_Module_Namelist,        only:   oHMC_Namelist
    use HMC_Module_Vars_Loader,     only:   oHMC_Vars
    
    use HMC_Module_Tools_Debug

#ifdef LIB_NC
    use HMC_Module_Tools_IO,        only:   HMC_Tools_IO_Get2d_Binary_INT, &
                                            HMC_Tools_IO_Get2d_NC, HMC_Tools_IO_CheckVar_NC, &
                                            check
#else
    use HMC_Module_Tools_IO,        only:   HMC_Tools_IO_Get2d_Binary_INT                                      
#endif                                    
                                                                                  
    use HMC_Module_Tools_Generic,   only:   HMC_Tools_Generic_ReplaceText, &
                                            HMC_Tools_Generic_SwitchGrid, &
                                            HMC_Tools_Generic_UnzipFile, &
                                            HMC_Tools_Generic_RemoveFile, &
                                            check2Dvar, getProcessID
                                            
    use HMC_Module_Tools_Time,      only:   HMC_Tools_Time_MonthVal
                             
    ! Implicit none for all subroutines in this module
    implicit none
    !------------------------------------------------------------------------------------------
    
contains
    
    !------------------------------------------------------------------------------------------
    ! Subroutine to manage updating gridded data
    subroutine HMC_Data_Updating_Gridded_Cpl( iID, sTime, &
                                     iRowsStartL, iRowsEndL, iColsStartL, iColsEndL)
        
        !------------------------------------------------------------------------------------------
        ! Variable(s)
        integer(kind = 4)           :: iID
                                    
        integer(kind = 4)           :: iRowsStartL, iRowsEndL, iColsStartL, iColsEndL
        integer(kind = 4)           :: iRowsL, iColsL
        integer(kind = 4)           :: iFlagTypeData_Updating, iFlagSnow
 
        character(len = 19)         :: sTime
        character(len = 12)         :: sTimeMonth
        
        character(len = 256)        :: sPathData_Updating
        
        real(kind = 4)              :: dVarLAI, dVarAlbedo
        
        real(kind = 4), dimension(iRowsEndL - iRowsStartL + 1, &
                                  iColsEndL - iColsStartL + 1) ::   a2dVarSnowCAL, a2dVarSnowQAL, &
                                                                    a2dVarSnowMaskL, &
                                                                    a2dVarSMStarL, a2dVarSMGainL, &
                                                                    a2dVarSnowHeightL, a2dVarSnowKernelL
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Updating data (optional):                                                              
        !   a2dVarSnowCAL       : snow cover area [-2,3] 
        !   a2dVarSnowQAL       : snow cover quality [0,1] 
        !   a2dVarSnowMaskL     : snow mask [0,1] 
        !   
        !   a2dVarSMStarL       : soil moisture value [0, 1]
        !   a2dVarSMGainL       : soil moisture gain [0, 1]
        !   a2dVarSnowHeightF   : snow height [cm]                                                    
        !   a2dVarSnowKernelF   : snow kernel [0,1]  
        !------------------------------------------------------------------------------------------
                                                                                                                        
        !------------------------------------------------------------------------------------------
        ! Initialize variable(s)
        a2dVarSnowCAL = -9999.0; a2dVarSnowQAL = -9999.0;
        a2dVarSnowMaskL = -9999.0;
        a2dVarSMStarL = -9999.0; a2dVarSMGainL = -9999.0;
        a2dVarSnowHeightL = -9999.9; a2dVarSnowKernelL = -9999.0; 
        !------------------------------------------------------------------------------------------
                                                                                                
        !------------------------------------------------------------------------------------------
        ! Defining iRows and iCols (Land data)
        iRowsL = iRowsEndL - iRowsStartL + 1
        iColsL = iColsEndL - iColsStartL + 1
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Get global information
        sPathData_Updating = oHMC_Namelist(iID)%sPathData_Updating_Gridded
        iFlagTypeData_Updating = oHMC_Namelist(iID)%iFlagTypeData_Updating_Gridded
        
        iFlagSnow = oHMC_Namelist(iID)%iFlagSnow
        
        ! Info start
        call mprintf(.true., iINFO_Extra, ' Data :: Updating gridded ... ' )
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Replace general path with specific time feature(s)
        call HMC_Tools_Generic_ReplaceText(sPathData_Updating, '$yyyy', sTime(1:4))
        call HMC_Tools_Generic_ReplaceText(sPathData_Updating, '$mm', sTime(6:7))
        call HMC_Tools_Generic_ReplaceText(sPathData_Updating, '$dd', sTime(9:10))
        call HMC_Tools_Generic_ReplaceText(sPathData_Updating, '$HH', sTime(12:13))
        call HMC_Tools_Generic_ReplaceText(sPathData_Updating, '$MM', sTime(15:16))
        
        ! Checking date
        write(sTimeMonth,'(A,A,A)') sTime(1:4), sTime(6:7), sTime(9:10)
        !------------------------------------------------------------------------------------------

        !------------------------------------------------------------------------------------------
        ! Check time step (iT)
        if (oHMC_Vars(iID)%iTime .lt. oHMC_Namelist(iID)%iNTime) then
 
            !------------------------------------------------------------------------------------------
            ! Subroutine for reading sequential netCDF updating data 
            if (iFlagTypeData_Updating == 2) then

                !------------------------------------------------------------------------------------------
                ! Call subroutine to get updating data in netCDF format
#ifdef LIB_NC
                call HMC_Data_Updating_Gridded_NC(iID, &
                                        sPathData_Updating, &
                                        iRowsL, iColsL, &
                                        sTime, &
                                        a2dVarSnowCAL, a2dVarSnowQAL, &
                                        a2dVarSnowMaskL, &
                                        a2dVarSMStarL, a2dVarSMGainL, &
                                        a2dVarSnowHeightL, a2dVarSnowKernelL)
#else   
                ! Redefinition of updating data flag (if netCDF library is not linked)
                iFlagTypeData_Updating = 1 
                call mprintf(.true., iWARN, ' '// &
                                            'Updating gridded data type selected was netCDF but library is not linked! '// &
                                            'Will be used data in binary format!')
#endif
                !------------------------------------------------------------------------------------------
                           
            endif
            !------------------------------------------------------------------------------------------

            !------------------------------------------------------------------------------------------
            ! Subroutine for reading sequential binary updating data
            if (iFlagTypeData_Updating == 1) then

                !------------------------------------------------------------------------------------------
                ! Calling subroutine to read data in binary format
                call HMC_Data_Updating_Gridded_Binary(iID, &
                                            sPathData_Updating, &
                                            iRowsL, iColsL, sTime, &
                                            a2dVarSnowCAL, a2dVarSnowQAL, &
                                            a2dVarSnowMaskL, &
                                            a2dVarSMStarL, a2dVarSMGainL, &
                                            a2dVarSnowHeightL, a2dVarSnowKernelL)
                !------------------------------------------------------------------------------------------

            endif
            !------------------------------------------------------------------------------------------
            
            !------------------------------------------------------------------------------------------
            ! Debug
            if (iDEBUG.gt.0) then
                call mprintf(.true., iINFO_Extra, ' ========= UPDATING GRIDDED START =========== ')
                call mprintf(.true., iINFO_Extra, checkvar(a2dVarSnowCAL, oHMC_Vars(iID)%a2iMask, 'SNOWCA START') )
                call mprintf(.true., iINFO_Extra, checkvar(a2dVarSnowQAL, oHMC_Vars(iID)%a2iMask, 'SNOWQA START') )
                call mprintf(.true., iINFO_Extra, checkvar(a2dVarSnowMaskL, oHMC_Vars(iID)%a2iMask, 'SNOWMASK START') )
                call mprintf(.true., iINFO_Extra, checkvar(a2dVarSMStarL, oHMC_Vars(iID)%a2iMask, 'SMSTAR START') )
                call mprintf(.true., iINFO_Extra, checkvar(a2dVarSMGainL, oHMC_Vars(iID)%a2iMask, 'SMGAIN START') )
                call mprintf(.true., iINFO_Extra, checkvar(a2dVarSnowHeightL, oHMC_Vars(iID)%a2iMask, 'SNOWHEIGHT START') )
                call mprintf(.true., iINFO_Extra, checkvar(a2dVarSnowKernelL, oHMC_Vars(iID)%a2iMask, 'SNOWKERNEL START') )
                call mprintf(.true., iINFO_Extra, '')                
            endif
            !------------------------------------------------------------------------------------------
                     
            !------------------------------------------------------------------------------------------
            ! Check variable(s) limits and domain
            a2dVarSnowCAL = check2Dvar(a2dVarSnowCAL,           oHMC_Vars(iID)%a2iMask,     -1.0,   3.0,    -9999.0 )  
            a2dVarSnowQAL = check2Dvar(a2dVarSnowQAL,           oHMC_Vars(iID)%a2iMask,     0.0,    1.0 ,   -9999.0 ) 
            a2dVarSnowMaskL = check2Dvar(a2dVarSnowMaskL,       oHMC_Vars(iID)%a2iMask,     0.0,    1.0 ,   -9999.0 ) 
            a2dVarSMStarL = check2Dvar(a2dVarSMStarL,           oHMC_Vars(iID)%a2iMask,     0.0,    1.0 ,   -9999.0 ) 
            a2dVarSMGainL = check2Dvar(a2dVarSMGainL,           oHMC_Vars(iID)%a2iMask,     0.0,    1.0 ,   -9999.0 ) 
            a2dVarSnowHeightL = check2Dvar(a2dVarSnowHeightL,   oHMC_Vars(iID)%a2iMask,     0.0,    1000.0, -9999.0 ) 
            a2dVarSnowKernelL = check2Dvar(a2dVarSnowKernelL,   oHMC_Vars(iID)%a2iMask,     0.0,    1.0,    -9999.0 )
            !------------------------------------------------------------------------------------------
            
        else
            
            !------------------------------------------------------------------------------------------
            ! Extra steps condition
            a2dVarSnowCAL = -9999.0; 
            a2dVarSnowQAL = -9999.0;
            a2dVarSnowMaskL = oHMC_Vars(iID)%a2dMaskS; 
            a2dVarSMStarL = -9999.0;
            a2dVarSMGainL = -9999.0;
            a2dVarSnowHeightL = -9999.0; 
            a2dVarSnowKernelL = -9999.0; 
            
            ! Info message for extra time step(s)
            call mprintf(.true., iINFO_Extra, ' Extra time step ---> Updating data are set null')
            call mprintf(.true., iINFO_Extra, ' Extra time step ---> Snow data are set to -9999.0')
            !------------------------------------------------------------------------------------------
            
        endif
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Check update and save updating data to local variable(s) to global workspace
        ! Snow Cover Area
        if ( .not. all(a2dVarSnowCAL.eq.-9999.0) ) then
            oHMC_Vars(iID)%a2dSCA = a2dVarSnowCAL
        else
            if (iFlagSnow.eq.1) then 
                call mprintf(.true., iWARN, ' All snow CA values are undefined! Check updating data!'// &
                        ' Snow physics is activated! If needed check updating data!' )
                oHMC_Vars(iID)%a2dSCA = -9999.0
            else
                oHMC_Vars(iID)%a2dSCA = -9999.0
            endif
        endif 
        ! Snow Quality Assessment
        if ( .not. all(a2dVarSnowQAL.eq.-9999.0) ) then
            oHMC_Vars(iID)%a2dSQA = a2dVarSnowQAL
        else
            if (iFlagSnow.eq.1) then 
                call mprintf(.true., iWARN, ' All snow QA values are undefined! Check updating data!'// &
                        ' Snow physics is activated! If needed check updating data!' )
                oHMC_Vars(iID)%a2dSQA = -9999.0
            else
                oHMC_Vars(iID)%a2dSQA = -9999.0
            endif
        endif 
        ! Snow Mask
        if ( .not. all(a2dVarSnowMaskL.eq.-9999.0) ) then
            oHMC_Vars(iID)%a2dMaskS = a2dVarSnowMaskL
        else
            if (iFlagSnow.eq.1) then 
                oHMC_Vars(iID)%a2dMaskS = -9999.0
            else
                call mprintf(.true., iWARN, ' All snow mask values are undefined! Check updating data!'// &
                        ' Snow physics is not activated! If Snow mask is provided by external file, check updating data!' )
                oHMC_Vars(iID)%a2dMaskS = -9999.0
            endif
        endif 
        
        ! Snow Height
        if ( .not. all(a2dVarSnowHeightL.eq.-9999.0) ) then
            oHMC_Vars(iID)%a2dSHeight = a2dVarSnowHeightL
        else
            if (iFlagSnow.eq.1) then 
                call mprintf(.true., iWARN, ' All snow height values are undefined!'// &
                    ' Snow physics is activated! If needed check updating data!' )
                oHMC_Vars(iID)%a2dSHeight = -9999.0
            else
                oHMC_Vars(iID)%a2dSHeight = -9999.0
            endif
        endif 
        ! Snow Kernel
        if ( .not. all(a2dVarSnowKernelL.eq.-9999.0) ) then
            oHMC_Vars(iID)%a2dSKernel = a2dVarSnowKernelL
        else
            if (iFlagSnow.eq.1) then 
                call mprintf(.true., iWARN, ' All snow kernel values are undefined!'// &
                    ' Snow physics is activated! If needed check updating data!' )
                oHMC_Vars(iID)%a2dSKernel = -9999.0
            else
                oHMC_Vars(iID)%a2dSKernel = -9999.0
            endif
        endif         
        
        ! Soil moisture star
        if ( .not. all(a2dVarSMStarL.eq.-9999.0) ) then
            oHMC_Vars(iID)%a2dSMStar = a2dVarSMStarL
        else
            oHMC_Vars(iID)%a2dSMStar = -9999.0
        endif 
        ! Soil moisture gain
        if ( .not. all(a2dVarSMGainL.eq.-9999.0) ) then
            oHMC_Vars(iID)%a2dSMGain = a2dVarSMGainL
        else
            oHMC_Vars(iID)%a2dSMGain = -9999.0
        endif 
        !------------------------------------------------------------------------------------------
         
        !------------------------------------------------------------------------------------------
        ! Debug
        if (iDEBUG.gt.0) then
            call mprintf(.true., iINFO_Extra, '')
            call mprintf(.true., iINFO_Extra, checkvar(oHMC_Vars(iID)%a2dSCA, oHMC_Vars(iID)%a2iMask, 'SNOWCA END') )
            call mprintf(.true., iINFO_Extra, checkvar(oHMC_Vars(iID)%a2dSQA, oHMC_Vars(iID)%a2iMask, 'SNOWQA END') )
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarSMStarL, oHMC_Vars(iID)%a2iMask, 'SMSTAR END') )
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarSMGainL, oHMC_Vars(iID)%a2iMask, 'SMGAIN END') )
            call mprintf(.true., iINFO_Extra, checkvar(oHMC_Vars(iID)%a2dSHeight, oHMC_Vars(iID)%a2iMask, 'SNOWHEIGHT END') )
            call mprintf(.true., iINFO_Extra, checkvar(oHMC_Vars(iID)%a2dSKernel, oHMC_Vars(iID)%a2iMask, 'SNOWKERNEL END') )
            call mprintf(.true., iINFO_Extra, ' ========= UPDATING GRIDDED END =========== ')
        endif
        
        ! Info end
        call mprintf(.true., iINFO_Extra, ' Data :: Updating gridded ... OK' )
        !------------------------------------------------------------------------------------------
        
    end subroutine HMC_Data_Updating_Gridded_Cpl
    !------------------------------------------------------------------------------------------
    
    !------------------------------------------------------------------------------------------
    ! Subroutine to read NC data updating
#ifdef LIB_NC
    subroutine HMC_Data_Updating_Gridded_NC(iID,  &
                                  sPathData_Updating, &
                                  iRows, iCols, sTime, &
                                  a2dVarSnowCA, a2dVarSnowQA, &
                                  a2dVarSnowMask, &
                                  a2dVarSMStar, a2dVarSMGain, &
                                  a2dVarSnowHeight, a2dVarSnowKernel)
                                  
        !------------------------------------------------------------------------------------------
        ! Variable(s)
        integer(kind = 4)                       :: iID                  
        
        character(len = 256), intent(in)        :: sPathData_Updating
        character(len = 700)                    :: sFileNameData_Updating, sFileNameData_Updating_Zip, sFileNameData_Temp
        character(len = 700)                    :: sCommandUnzipFile, sCommandRemoveFile
        character(len = 256)                    :: sVarName
        integer(kind = 4), intent(in)           :: iRows, iCols

        character(len = 19), intent(in)         :: sTime
        character(len = 12)                     :: sTimeMonth
        
        integer(kind = 4)                       :: iFlagSnow, iFlagSnowAssim, iFlagSMAssim
        real(kind = 4)                          :: dVarSMGain
        
        real(kind = 4), dimension(iCols, iRows)                 :: a2dVar
        
        real(kind = 4), dimension(iRows, iCols), intent(out)    :: a2dVarSnowCA
        real(kind = 4), dimension(iRows, iCols), intent(out)    :: a2dVarSnowQA
        real(kind = 4), dimension(iRows, iCols), intent(out)    :: a2dVarSnowMask
        real(kind = 4), dimension(iRows, iCols), intent(out)    :: a2dVarSMStar
        real(kind = 4), dimension(iRows, iCols), intent(out)    :: a2dVarSMGain
        
        real(kind = 4), dimension(iRows, iCols), intent(out)    :: a2dVarSnowHeight
        real(kind = 4), dimension(iRows, iCols), intent(out)    :: a2dVarSnowKernel
       
        character(len = 256):: sVarUnits, sPID
        integer(kind = 4)   :: iErr
        integer(kind = 4)   :: iFileID
        
        logical             :: bFileExist
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Initialize variable(s)
        a2dVarSnowCA = -9999.0; a2dVarSnowCA = -9999.0;
        a2dVarSnowMask = -9999.0;
        a2dVarSMStar = -9999.0; a2dVarSMGain = -9999.0;
        a2dVarSnowHeight = -9999.0; a2dVarSnowKernel = -9999.0;
        
        sFileNameData_Updating = ''; sFileNameData_Updating_Zip = ''; sTimeMonth = ''
        
        ! Checking date
        write(sTimeMonth,'(A,A,A)') sTime(1:4), sTime(6:7), sTime(9:10)
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Get global information
        sCommandUnzipFile = oHMC_Namelist(iID)%sCommandUnzipFile
        sCommandRemoveFile = oHMC_Namelist(iID)%sCommandRemoveFile
        iFlagSnow = oHMC_Namelist(iID)%iFlagSnow
        iFlagSnowAssim = oHMC_Namelist(iID)%iFlagSnowAssim
        
        iFlagSMAssim = oHMC_Namelist(iID)%iFlagSMAssim
        dVarSMGain = oHMC_Namelist(iID)%dSMGain
        
        ! Info start
        call mprintf(.true., iINFO_Extra, ' Data :: Updating gridded :: NetCDF ... ' )
        
        ! Get unique process ID
        sPID = adjustl(getProcessID())
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Filename updating (example: hmc.updating-grid.201404300000.nc.gz)
        sFileNameData_Updating = trim(sPathData_Updating)//"hmc.updating-grid."// &
            sTime(1:4)//sTime(6:7)//sTime(9:10)// & 
            sTime(12:13)//sTime(15:16)// &
            ".nc"
        ! Create Filename with unique PID number to avoid simultaneously access to the same Forcing file       
        sFileNameData_Temp = trim(sPathData_Updating)//"hmc.updating-grid."// &
            sTime(1:4)//sTime(6:7)//sTime(9:10)// & 
            sTime(12:13)//sTime(15:16)//'_'//trim(sPID)// &
            ".nc"  

        ! Info netCDF filename
        call mprintf(.true., iINFO_Verbose, ' Get filename (updating gridded): '//trim(sFileNameData_Updating)//' ... ' )
        !------------------------------------------------------------------------------------------

        !------------------------------------------------------------------------------------------
        ! Checking file input availability
        sFileNameData_Updating_Zip = trim(sFileNameData_Updating)//'.gz'
        inquire (file = trim(sFileNameData_Updating)//'.gz', exist = bFileExist)
        if ( .not. bFileExist ) then
            !------------------------------------------------------------------------------------------
            ! Warning message
            call mprintf(.true., iWARN, ' No compressed updating netCDF data found: '//trim(sFileNameData_Updating_Zip) )
            ! Info netCDF filename
            call mprintf(.true., iINFO_Verbose, &
                         ' Get filename (updating gridded): '//trim(sFileNameData_Updating)//' ... FAILED' )
            ! Info end
            call mprintf(.true., iINFO_Extra, ' Data :: Updating gridded :: NetCDF ... SKIPPED!' )
            !------------------------------------------------------------------------------------------
        else
            
            !------------------------------------------------------------------------------------------
            ! Unzip file
            call HMC_Tools_Generic_UnzipFile(oHMC_Namelist(iID)%sCommandUnzipFile, &
                                             sFileNameData_Updating_Zip, &
                                             sFileNameData_Temp, .true.)
            !------------------------------------------------------------------------------------------
        
            !------------------------------------------------------------------------------------------
            ! Open netCDF file
            iErr = nf90_open(trim(sFileNameData_Temp), NF90_NOWRITE, iFileID)
            if (iErr /= 0) then
                call mprintf(.true., iWARN, ' Problem opening uncompressed netCDF file: '// &
                             trim(sFileNameData_Updating)//' --> Undefined updating data values' )
                call mprintf(.true., iINFO_Verbose, &
                             ' Get filename (updating gridded): '//trim(sFileNameData_Updating)//' ... FAILED' )
            else
                
                !------------------------------------------------------------------------------------------
                ! Soil moisture variables
                if (iFlagSMAssim.eq.1) then
                    
                    !------------------------------------------------------------------------------------------
                    ! SWI Star
                    sVarName = 'SWIStar'
                    call HMC_Tools_IO_Get2d_NC((sVarName), iFileID, a2dVar, sVarUnits, iCols, iRows, .false., iErr)
                    if(iErr /= 0) then
                        call mprintf(.true., iWARN, &
                            ' Get updating gridded data FAILED! Check updating data for '//sVarName//'!'// &
                            ' Snow physics is activated! If needed check updating data!')
                        a2dVarSMStar = -9999.0;
                    else
                        a2dVarSMStar = transpose(a2dVar)
                    endif
                    !------------------------------------------------------------------------------------------
                    
                    !------------------------------------------------------------------------------------------
                    ! SWI Gain
                    sVarName = 'SWIGain'
                    call HMC_Tools_IO_Get2d_NC((sVarName), iFileID, a2dVar, sVarUnits, iCols, iRows, .false., iErr)
                    if(iErr /= 0) then
                        call mprintf(.true., iWARN, &
                            ' Get updating gridded data FAILED! Check updating data for '//sVarName//'!'// &
                            ' Snow physics is activated! If needed check updating data!')
                        a2dVarSMGain = dVarSMGain;
                    else
                        a2dVarSMGain = transpose(a2dVar)
                    endif
                    !------------------------------------------------------------------------------------------
                    
                else
                    !------------------------------------------------------------------------------------------
                    ! Exit without soil moisture assimilation
                    a2dVarSMStar = -9999.0          ! assimilation method is switch off
                    a2dVarSMGain = -9999.0          ! assimilation method is switch off
                    !------------------------------------------------------------------------------------------
                endif
                !------------------------------------------------------------------------------------------
                
                !------------------------------------------------------------------------------------------
                ! Snow variable(s)
                if ( (iFlagSnow.eq.1) .and. (iFlagSnowAssim.eq.1) ) then

                    !------------------------------------------------------------------------------------------
                    ! SNOW HEIGHT
                    call HMC_Tools_IO_CheckVar_NC('Snow_Height;SnowHeight', iFileID, sVarName)
                    !sVarName = 'SnowHeight'
                    call HMC_Tools_IO_Get2d_NC((sVarName), iFileID, a2dVar, sVarUnits, iCols, iRows, .false., iErr)
                    if(iErr /= 0) then
                        call mprintf(.true., iWARN, &
                            ' Get updating gridded data FAILED! Check updating data for '//sVarName//'!'// &
                            ' Snow physics is activated! If needed check updating data!')
                        a2dVarSnowHeight = -9999.0;
                    else
                        a2dVarSnowHeight = transpose(a2dVar)
                    endif
                    !------------------------------------------------------------------------------------------
                    
                    !------------------------------------------------------------------------------------------
                    ! SNOW KERNEL
                    call HMC_Tools_IO_CheckVar_NC('Snow_Kernel;Kernel', iFileID, sVarName)
                    !sVarName = 'Kernel'
                    call HMC_Tools_IO_Get2d_NC((sVarName), iFileID, a2dVar, sVarUnits, iCols, iRows, .false., iErr)
                    if(iErr /= 0) then
                        call mprintf(.true., iWARN, &
                            ' Get updating gridded data FAILED! Check updating data for '//sVarName//'!'// &
                            ' Snow physics is activated! If needed check updating data!')
                        a2dVarSnowKernel = -9999.0;
                    else
                        a2dVarSnowKernel = transpose(a2dVar)
                    endif
                    !------------------------------------------------------------------------------------------                    
                    
                    !------------------------------------------------------------------------------------------
                    ! SNOW COVER AREA
                    sVarName = 'SCA'
                    call HMC_Tools_IO_Get2d_NC((sVarName), iFileID, a2dVar, sVarUnits, iCols, iRows, .false., iErr)
                    if(iErr /= 0) then
                        call mprintf(.true., iWARN, &
                            ' Get updating gridded data FAILED! Check updating data for '//sVarName//'!'// &
                            ' Snow physics is activated! If needed check updating data!')
                        a2dVarSnowCA = -9999.0;
                    else
                        a2dVarSnowCA = transpose(a2dVar)
                    endif
                    !------------------------------------------------------------------------------------------

                    !------------------------------------------------------------------------------------------
                    ! SNOW QUALITY ASSESSMENT
                    sVarName = 'SQA'
                    call HMC_Tools_IO_Get2d_NC((sVarName), iFileID, a2dVar, sVarUnits, iCols, iRows, .false., iErr)
                    if(iErr /= 0) then
                        call mprintf(.true., iWARN, &
                            ' Get updating gridded data FAILED! Check updating data for '//sVarName//'!'// &
                            ' Snow physics is activated! If needed check updating data!')
                        a2dVarSnowQA = -9999.0;
                    else
                        a2dVarSnowQA = transpose(a2dVar)
                    endif
                    !------------------------------------------------------------------------------------------
                    
                    !------------------------------------------------------------------------------------------
                    ! SNOW MASK
                    a2dVarSnowMask = -9999.0;       ! compute using snow physics in coupled model
                    !------------------------------------------------------------------------------------------
                    
                elseif ( (iFlagSnow.eq.1) .and. (iFlagSnowAssim.eq.0) ) then
                    
                    !------------------------------------------------------------------------------------------
                    ! Exit without snow assimilation
                    a2dVarSnowCA = -9999.0          ! assimilation method is switch off
                    a2dVarSnowQA = -9999.0          ! assimilation method is switch off
                    a2dVarSnowMask = -9999.0        ! compute using snow physics in coupled model
                    a2dVarSnowKernel = -9999.0      ! assimilation method is switch off
                    a2dVarSnowHeight = -9999.0      ! assimilation method is switch off
                    !------------------------------------------------------------------------------------------
                    !------------------------------------------------------------------------------------------
                    
                else
                    
                    !------------------------------------------------------------------------------------------
                    ! SNOW MASK (using an external file to switch off snow physics)
                    sVarName = 'SnowMask'
                    call HMC_Tools_IO_Get2d_NC((sVarName), iFileID, a2dVar, sVarUnits, iCols, iRows, .false., iErr)
                    if(iErr /= 0) then
                        call mprintf(.true., iWARN, &
                            ' Get updating gridded data FAILED! Check updating data for '//sVarName//'!'// &
                            ' Snow physics is not activated!'// &
                            ' If '//sVarName//' is provided using external file, check updating data!')
                        a2dVarSnowMask = -9999.0;
                    else
                        a2dVarSnowMask = transpose(a2dVar)
                    endif
                    !------------------------------------------------------------------------------------------
                    
                    !------------------------------------------------------------------------------------------
                    ! Exit without snow physics
                    a2dVarSnowCA = -9999.0
                    a2dVarSnowQA = -9999.0
                    a2dVarSnowKernel = -9999.0
                    a2dVarSnowHeight = -9999.0
                    !------------------------------------------------------------------------------------------
                    
                endif
                !------------------------------------------------------------------------------------------
                
                !------------------------------------------------------------------------------------------
                ! Closing netCDF file
                iErr = nf90_close(iFileID)
   
                ! Remove uncompressed file (to save space on disk)
                call HMC_Tools_Generic_RemoveFile(oHMC_Namelist(iID)%sCommandRemoveFile, sFileNameData_Temp, .false.)
                !------------------------------------------------------------------------------------------
                
                !------------------------------------------------------------------------------------------
                ! Info netCDF filename
                call mprintf(.true., iINFO_Verbose, ' Get filename (updating gridded): '//trim(sFileNameData_Updating)//' ... OK' )
                ! Info end
                call mprintf(.true., iINFO_Extra, ' Data :: Updating gridded :: NetCDF ... OK' )
                !------------------------------------------------------------------------------------------
                
            endif
            !------------------------------------------------------------------------------------------
            
        endif
        !------------------------------------------------------------------------------------------
        
    end subroutine HMC_Data_Updating_Gridded_NC
#endif
    !------------------------------------------------------------------------------------------
    
    !------------------------------------------------------------------------------------------
    ! Subroutine to read binary updating data
    subroutine HMC_Data_Updating_Gridded_Binary(iID, &
                                      sPathData_Updating, &
                                      iRows, iCols, sTime, &
                                      a2dVarSnowCA, a2dVarSnowQA, &
                                      a2dVarSnowMask, &
                                      a2dVarSMStar, a2dVarSMGain, &
                                      a2dVarSnowHeight, a2dVarSnowKernel)
    
        !------------------------------------------------------------------------------------------
        ! Variable(s)
        integer(kind = 4)                   :: iID
                                      
        character(len = 256), intent(in)    :: sPathData_Updating
        character(len = 700)                :: sFileNameData_Updating, sFileNameData_Updating_Zip, sFileNameData_Temp
        character(len = 700)                :: sCommandUnzipFile
        character(len = 256)                :: sVarName
        integer(kind = 4), intent(in)       :: iRows, iCols
        real(kind = 4)                      :: dVarSMGain
        
        integer(kind = 4)                   :: iFlagSnow, iFlagSnowAssim, iFlagSMAssim
        
        character(len = 19), intent(in)     :: sTime
        character(len = 12)                 :: sTimeMonth
        
        real(kind = 4), dimension(iRows, iCols)                 :: a2dVar

        real(kind = 4), dimension(iRows, iCols), intent(out)    :: a2dVarSnowCA
        real(kind = 4), dimension(iRows, iCols), intent(out)    :: a2dVarSnowQA
        real(kind = 4), dimension(iRows, iCols), intent(out)    :: a2dVarSnowMask
        real(kind = 4), dimension(iRows, iCols), intent(out)    :: a2dVarSMStar
        real(kind = 4), dimension(iRows, iCols), intent(out)    :: a2dVarSMGain
        
        real(kind = 4), dimension(iRows, iCols), intent(out)    :: a2dVarSnowHeight
        real(kind = 4), dimension(iRows, iCols), intent(out)    :: a2dVarSnowKernel        
       
        character(len = 256):: sVarUnits, sPID
        integer(kind = 4)   :: iErr
        integer(kind = 4)   :: iFileID, iScaleFactor
        
        logical             :: bFileExist
        !------------------------------------------------------------------------------------------
	
        !------------------------------------------------------------------------------------------
        ! Initialize variable(s)
        a2dVarSnowCA = -9999.0; a2dVarSnowCA = -9999.0;
        a2dVarSnowMask = -9999.0;
        a2dVarSMStar = -9999.0; a2dVarSMGain = -9999.0;
        a2dVarSnowKernel = -9999.0; a2dVarSnowHeight = -9999.0;        
        
        iScaleFactor = -9999;
        
        sFileNameData_Updating = ''; sFileNameData_Updating_Zip = ''; sTimeMonth = ''
        
        ! Checking date
        write(sTimeMonth,'(A,A,A)') sTime(1:4), sTime(6:7), sTime(9:10)
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Get global information
        sCommandUnzipFile = oHMC_Namelist(iID)%sCommandUnzipFile
        iFlagSnow = oHMC_Namelist(iID)%iFlagSnow
        iFlagSnowAssim = oHMC_Namelist(iID)%iFlagSnowAssim
        iFlagSMAssim = oHMC_Namelist(iID)%iFlagSMAssim
        
        dVarSMGain = oHMC_Namelist(iID)%dSMGain
        
        ! Info start
        call mprintf(.true., iINFO_Extra, ' Data :: Updating gridded :: Binary ... ' )
        
        ! Get unique process ID
        sPID = adjustl(getProcessID())
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Info binary file(s) time step
        call mprintf(.true., iINFO_Verbose, ' Get (updating gridded) at time '//trim(sTime)//' ... ')
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Soil moisture variables
        if (iFlagSMAssim.eq.1) then
            
            !------------------------------------------------------------------------------------------
            ! SWI (example: SWIStar_$yyyy$mm$dd$HH$MM.bin.gz)
            iScaleFactor = 100
            sFileNameData_Updating = trim(sPathData_Updating)//"SWIStar_"// &
                sTime(1:4)//sTime(6:7)//sTime(9:10)// & 
                sTime(12:13)//sTime(15:16)// &
                ".bin"
            sFileNameData_Temp = trim(sPathData_Updating)//"SWIStar_"// &
                sTime(1:4)//sTime(6:7)//sTime(9:10)// & 
                sTime(12:13)//sTime(15:16)//'_'//trim(sPID)// &
                ".bin"  
            call mprintf(.true., iINFO_Extra, ' Get filename (updating gridded): '//trim(sFileNameData_Updating) )

            ! Checking file input availability
            sFileNameData_Updating_Zip = trim(sFileNameData_Updating)//'.gz'
            inquire (file = sFileNameData_Updating_Zip, exist = bFileExist)
            if ( .not. bFileExist ) then
                call mprintf(.true., iWARN, ' Problem opening uncompressed binary file: '// &
                             trim(sFileNameData_Updating_Zip)//' --> Undefined updating data values!')
                a2dVar = -9999.0;

            else
                ! Unzip file
                call HMC_Tools_Generic_UnzipFile(oHMC_Namelist(iID)%sCommandUnzipFile, &
                                                 sFileNameData_Updating_Zip, &
                                                 sFileNameData_Temp, .true.)
                ! Read binary data
                call HMC_Tools_IO_Get2d_Binary_INT(sFileNameData_Temp, a2dVar, iRows, iCols, iScaleFactor, .true., iErr) 
                ! Remove uncompressed file (to save space on disk)
                call HMC_Tools_Generic_RemoveFile(oHMC_Namelist(iID)%sCommandRemoveFile, &
                                                  sFileNameData_Temp, .false.)
            endif
            a2dVarSMStar = a2dVar
            !------------------------------------------------------------------------------------------
            
            !------------------------------------------------------------------------------------------
            ! Gain (example: SWIGain_$yyyy$mm$dd$HH$MM.bin.gz)   
            iScaleFactor = 10
            sFileNameData_Updating = trim(sPathData_Updating)//"SWIGain_"// &
                sTime(1:4)//sTime(6:7)//sTime(9:10)// & 
                sTime(12:13)//sTime(15:16)// &
                ".bin"
            sFileNameData_Temp = trim(sPathData_Updating)//"SWIGain_"// &
                sTime(1:4)//sTime(6:7)//sTime(9:10)// & 
                sTime(12:13)//sTime(15:16)//'_'//trim(sPID)// &
                ".bin"  
            call mprintf(.true., iINFO_Extra, ' Get filename (updating gridded): '//trim(sFileNameData_Updating) )

            ! Checking file input availability
            sFileNameData_Updating_Zip = trim(sFileNameData_Updating)//'.gz'
            inquire (file = sFileNameData_Updating_Zip, exist = bFileExist)
            if ( .not. bFileExist ) then
                call mprintf(.true., iWARN, ' Problem opening uncompressed binary file: '// &
                             trim(sFileNameData_Updating_Zip)//' --> Undefined updating data values!')
                a2dVar = dVarSMGain;

            else
                ! Unzip file
                call HMC_Tools_Generic_UnzipFile(oHMC_Namelist(iID)%sCommandUnzipFile, &
                                                 sFileNameData_Updating_Zip, &
                                                 sFileNameData_Temp, .true.)
                ! Read binary data
                call HMC_Tools_IO_Get2d_Binary_INT(sFileNameData_Temp, a2dVar, iRows, iCols, iScaleFactor, .true., iErr) 
                ! Remove uncompressed file (to save space on disk)
                call HMC_Tools_Generic_RemoveFile(oHMC_Namelist(iID)%sCommandRemoveFile, &
                                                  sFileNameData_Temp, .false.)
            endif
            a2dVarSMGain = a2dVar
            !------------------------------------------------------------------------------------------
            
        else
            !------------------------------------------------------------------------------------------
            ! Exit without soil moisture assimilation
            a2dVarSMStar = -9999.0
            a2dVarSMGain = -9999.0
            !------------------------------------------------------------------------------------------
            
        endif
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Snow variable(s)
        if ( (iFlagSnow.eq.1) .and. (iFlagSnowAssim.eq.1) ) then 
            
            !------------------------------------------------------------------------------------------
            ! SnowHeight (example: SnowHeight_201405010000.bin.gz)
            iScaleFactor = 10
            sFileNameData_Updating = trim(sPathData_Updating)//"SnowHeight_"// &
                sTime(1:4)//sTime(6:7)//sTime(9:10)// & 
                sTime(12:13)//sTime(15:16)// &
                ".bin"
            sFileNameData_Temp = trim(sPathData_Updating)//"SnowHeight_"// &
                sTime(1:4)//sTime(6:7)//sTime(9:10)// & 
                sTime(12:13)//sTime(15:16)//'_'//trim(sPID)// &
                ".bin"  
            call mprintf(.true., iINFO_Extra, ' Get filename (updating gridded): '//trim(sPathData_Updating) )

            ! Checking file input availability
            sFileNameData_Updating_Zip = trim(sFileNameData_Updating)//'.gz'
            inquire (file = sFileNameData_Updating_Zip, exist = bFileExist)
            if ( .not. bFileExist ) then
                call mprintf(.true., iWARN, ' Problem opening uncompressed binary file: '// &
                             trim(sFileNameData_Updating_Zip)//' --> Undefined updating data values.'// &
                             ' Snow physics is activated! If needed check updating data!')
                a2dVar = -9999.0; 
            else
                ! Unzip file
                call HMC_Tools_Generic_UnzipFile(oHMC_Namelist(iID)%sCommandUnzipFile, &
                                                 sFileNameData_Updating_Zip, &
                                                 sFileNameData_Temp, .true.)
                ! Read binary data
                call HMC_Tools_IO_Get2d_Binary_INT(sFileNameData_Temp, a2dVar, iRows, iCols, iScaleFactor, .true., iErr) 
                ! Remove uncompressed file (to save space on disk)
                call HMC_Tools_Generic_RemoveFile(oHMC_Namelist(iID)%sCommandRemoveFile, &
                                                  sFileNameData_Temp, .false.)
            endif
            a2dVarSnowHeight = a2dVar
            !------------------------------------------------------------------------------------------
            
            !------------------------------------------------------------------------------------------
            ! Snow Kernel (example: Kernel_201405010000.bin.gz) 
            iScaleFactor = 10
            sFileNameData_Updating = trim(sPathData_Updating)//"Kernel_"// &
                sTime(1:4)//sTime(6:7)//sTime(9:10)// & 
                sTime(12:13)//sTime(15:16)// &
                ".bin"
            sFileNameData_Temp = trim(sPathData_Updating)//"Kernel_"// &
                sTime(1:4)//sTime(6:7)//sTime(9:10)// & 
                sTime(12:13)//sTime(15:16)//'_'//trim(sPID)// &
                ".bin"  
            call mprintf(.true., iINFO_Extra, ' Get filename (updating gridded): '//trim(sFileNameData_Updating) )

            ! Checking file input availability
            sFileNameData_Updating_Zip = trim(sFileNameData_Updating)//'.gz'
            inquire (file = sFileNameData_Updating_Zip, exist = bFileExist)
            if ( .not. bFileExist ) then
                call mprintf(.true., iWARN, ' Problem opening uncompressed binary file: '// &
                             trim(sFileNameData_Updating_Zip)//' --> Undefined updating data values.'// &
                             ' Snow physics is activated! If needed check updating data!')
                a2dVar = -9999.0; 
            else
                ! Unzip file
                call HMC_Tools_Generic_UnzipFile(oHMC_Namelist(iID)%sCommandUnzipFile, &
                                                 sFileNameData_Updating_Zip, &
                                                 sFileNameData_Temp, .true.)
                ! Read binary data
                call HMC_Tools_IO_Get2d_Binary_INT(sFileNameData_Temp, a2dVar, iRows, iCols, iScaleFactor, .true., iErr) 
                ! Remove uncompressed file (to save space on disk)
                call HMC_Tools_Generic_RemoveFile(oHMC_Namelist(iID)%sCommandRemoveFile, &
                                                  sFileNameData_Temp, .false.)
            endif
            a2dVarSnowKernel = a2dVar
            !------------------------------------------------------------------------------------------

            !------------------------------------------------------------------------------------------
            ! Snow Cover Area (example: SCA_201405010000.bin.gz)
            iScaleFactor = 10
            sFileNameData_Updating = trim(sPathData_Updating)//"SCA_"// &
                sTime(1:4)//sTime(6:7)//sTime(9:10)// & 
                sTime(12:13)//sTime(15:16)// &
                ".bin"
            sFileNameData_Temp = trim(sPathData_Updating)//"SCA_"// &
                sTime(1:4)//sTime(6:7)//sTime(9:10)// & 
                sTime(12:13)//sTime(15:16)//'_'//trim(sPID)// &
                ".bin"  
            call mprintf(.true., iINFO_Extra, ' Get filename (updating gridded): '//trim(sFileNameData_Updating) )

            ! Checking file input availability
            sFileNameData_Updating_Zip = trim(sFileNameData_Updating)//'.gz'
            inquire (file = sFileNameData_Updating_Zip, exist = bFileExist)
            if ( .not. bFileExist ) then
                call mprintf(.true., iWARN, ' Problem opening uncompressed binary file: '// &
                             trim(sFileNameData_Updating_Zip)//' --> Undefined updating data values.'// &
                             ' Snow physics is activated! If needed check updating data!')
                a2dVar = -9999.0; 
            else
                ! Unzip file
                call HMC_Tools_Generic_UnzipFile(oHMC_Namelist(iID)%sCommandUnzipFile, &
                                                 sFileNameData_Updating_Zip, &
                                                 sFileNameData_Temp, .true.)
                ! Read binary data
                call HMC_Tools_IO_Get2d_Binary_INT(sFileNameData_Temp, a2dVar, iRows, iCols, iScaleFactor, .true., iErr) 
                ! Remove uncompressed file (to save space on disk)
                call HMC_Tools_Generic_RemoveFile(oHMC_Namelist(iID)%sCommandRemoveFile, &
                                                  sFileNameData_Temp, .false.)
            endif
            a2dVarSnowCA = a2dVar
            !------------------------------------------------------------------------------------------
            
            !------------------------------------------------------------------------------------------
            ! Snow Quality Assessment (example: SQA_201405010000.bin.gz)
            iScaleFactor = 10
            sFileNameData_Updating = trim(sPathData_Updating)//"SQA_"// &
                sTime(1:4)//sTime(6:7)//sTime(9:10)// & 
                sTime(12:13)//sTime(15:16)// &
                ".bin"
            sFileNameData_Temp = trim(sPathData_Updating)//"SQA_"// &
                sTime(1:4)//sTime(6:7)//sTime(9:10)// & 
                sTime(12:13)//sTime(15:16)//'_'//trim(sPID)// &
                ".bin" 
            call mprintf(.true., iINFO_Extra, ' Get filename (updating gridded): '//trim(sFileNameData_Updating) )

            ! Checking file input availability
            sFileNameData_Updating_Zip = trim(sFileNameData_Updating)//'.gz'
            inquire (file = sFileNameData_Updating_Zip, exist = bFileExist)
            if ( .not. bFileExist ) then
                call mprintf(.true., iWARN, ' Problem opening uncompressed binary file: '// &
                             trim(sFileNameData_Updating_Zip)//' --> Undefined updating data values.'// &
                             ' Snow physics is activated! If needed check updating data!')
                a2dVar = -9999.0; 
            else
                ! Unzip file
                call HMC_Tools_Generic_UnzipFile(oHMC_Namelist(iID)%sCommandUnzipFile, &
                                                 sFileNameData_Updating_Zip, &
                                                 sFileNameData_Temp, .true.)
                ! Read binary data
                call HMC_Tools_IO_Get2d_Binary_INT(sFileNameData_Temp, a2dVar, iRows, iCols, iScaleFactor, .true., iErr) 
                ! Remove uncompressed file (to save space on disk)
                call HMC_Tools_Generic_RemoveFile(oHMC_Namelist(iID)%sCommandRemoveFile, &
                                                  sFileNameData_Temp, .false.)
            endif
            a2dVarSnowQA = a2dVar
            !------------------------------------------------------------------------------------------
            
        elseif ( (iFlagSnow.eq.1) .and. (iFlagSnowAssim.eq.0) ) then
            
            !------------------------------------------------------------------------------------------
            ! Exit without snow assimilation
            a2dVarSnowCA = -9999.0
            a2dVarSnowQA = -9999.0
            a2dVarSnowMask = -9999.0
            a2dVarSnowKernel = -9999.0
            a2dVarSnowHeight = -9999.0
            !------------------------------------------------------------------------------------------
            
        else
            
            !------------------------------------------------------------------------------------------
            ! SnowMask (example: SnowMask_201405010000.bin.gz)
            iScaleFactor = 10
            sFileNameData_Updating = trim(sPathData_Updating)//"SnowMask_"// &
                sTime(1:4)//sTime(6:7)//sTime(9:10)// & 
                sTime(12:13)//sTime(15:16)// &
                ".bin"
            sFileNameData_Temp = trim(sPathData_Updating)//"SnowMask_"// &
                sTime(1:4)//sTime(6:7)//sTime(9:10)// & 
                sTime(12:13)//sTime(15:16)//'_'//trim(sPID)// &
                ".bin" 
            call mprintf(.true., iINFO_Extra, ' Get filename (updating gridded): '//trim(sFileNameData_Updating) )

            ! Checking file input availability
            sFileNameData_Updating_Zip = trim(sFileNameData_Updating)//'.gz'
            inquire (file = sFileNameData_Updating_Zip, exist = bFileExist)
            if ( .not. bFileExist ) then
                call mprintf(.true., iWARN, ' Problem opening uncompressed binary file: '// &
                             trim(sFileNameData_Updating_Zip)//' --> Undefined updating data values.'// &
                             ' Snow physics is not activated!'// &
                             ' If SnowMask is provided using external file, check updating data!')
                a2dVar = -9999.0; 
            else
                ! Unzip file
                call HMC_Tools_Generic_UnzipFile(oHMC_Namelist(iID)%sCommandUnzipFile, &
                                                 sFileNameData_Updating_Zip, &
                                                 sFileNameData_Temp, .true.)
                ! Read binary data
                call HMC_Tools_IO_Get2d_Binary_INT(sFileNameData_Temp, a2dVar, iRows, iCols, iScaleFactor, .true., iErr) 
                ! Remove uncompressed file (to save space on disk)
                call HMC_Tools_Generic_RemoveFile(oHMC_Namelist(iID)%sCommandRemoveFile, &
                                                  sFileNameData_Temp, .false.)
            endif
            a2dVarSnowMask = a2dVar
            !------------------------------------------------------------------------------------------
            
            !------------------------------------------------------------------------------------------
            ! Exit without snow physics
            a2dVarSnowCA = -9999.0
            a2dVarSnowQA = -9999.0
            a2dVarSnowKernel = -9999.0
            a2dVarSnowHeight = -9999.0
            !------------------------------------------------------------------------------------------
            
        endif
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Info binary file(s) time step
        call mprintf(.true., iINFO_Verbose, ' Get (updating gridded) at time '//trim(sTime)//' ... OK')
        ! Info end
        call mprintf(.true., iINFO_Extra, ' Data :: Updating gridded :: Binary ... OK' )
        !------------------------------------------------------------------------------------------
        
    end subroutine HMC_Data_Updating_Gridded_Binary
    !------------------------------------------------------------------------------------------
    
end module HMC_Module_Data_Updating_Gridded
!-----------------------------------------------------------------------------------------

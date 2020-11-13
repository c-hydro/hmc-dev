!------------------------------------------------------------------------------------
! File:   HMC_Module_Info_Gridded.f90
! Author(s): Fabio Delogu, Francesco Silvestro, Simone Gabellani
! Created on May, 20 2014, 9:57 AM
!
! Module to get info gridded
!------------------------------------------------------------------------------------

!------------------------------------------------------------------------------------
! Module Header
module HMC_Module_Info_Gridded
    
    !------------------------------------------------------------------------------------
    ! External module(s) and implicit none
#ifdef LIB_NC
    use netcdf
#endif
   
    use HMC_Module_Namelist,        only:   oHMC_Namelist
    use HMC_Module_Vars_Loader,     only:   oHMC_Vars
   
    use HMC_Module_Tools_Debug
    use HMC_Module_Tools_Generic,   only:   HMC_Tools_Generic_ReplaceText, &
                                            HMC_Tools_Generic_CreateIndexGrid, &
                                            HMC_Tools_Generic_UnzipFile, &
                                            HMC_Tools_Generic_RemoveFile, &
                                            getProcessID
#ifdef LIB_NC
    use HMC_Module_Tools_IO,        only:   check
#endif
    
    implicit none
    !------------------------------------------------------------------------------------
    
contains 
    
    !------------------------------------------------------------------------------------
    ! Subroutine to get file gridded dimension(s)
    subroutine HMC_Info_Gridded_GetDims_Static(iID, iRows, iCols)
        
        !------------------------------------------------------------------------------------
        ! Variable(s) declaration
        integer(kind = 4)           :: iID
        integer(kind = 4)           :: iRows, iCols
        
        
        integer(kind = 4)           :: iTypeData
        character(len = 256)        :: sDomainName, sPathData
        
        integer(kind = 4)           :: iNCid, iDimId
        
        character(len = 256)        :: sText
        character(len = 700)        :: sFileName
        
        logical                     :: bFileExist
        !------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------
        ! Initialize variable(s)
        iRows = 0; iCols = 0;
        !------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------
        ! Get information
        sDomainName = oHMC_Namelist(iID)%sDomainName
        sPathData = oHMC_Namelist(iID)%sPathData_Static_Gridded
        iTypeData = oHMC_Namelist(iID)%iFlagTypeData_Static

        ! Get variable(s) dimension(s)
        call mprintf(.true., iINFO_Main, ' Define land data dims ... ')
        !------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------
        ! Get info from netCDF file
        if (iTypeData == 2) then
#ifdef LIB_NC
            !------------------------------------------------------------------------------------
            ! Filename 
            sFileName = trim(sPathData)//'hmc.staticdata.nc'
            
            ! Check file availabilty
            inquire (file = trim(sFileName), exist = bFileExist)
            if ( .not. bFileExist ) then
                call mprintf(.true., iERROR, ' No land data netCDF found '//trim(sFileName) )
            else
                ! Info
                call mprintf(.true., iINFO_Extra, ' Land data in netCDF format ')
                ! Open netCDF file
                call check( nf90_open(trim(sFileName), NF90_NOWRITE, iNCid) )
                ! Get dimension(s)
                call check( nf90_inq_dimid(iNCid, "west_east", iDimId) )
                call check( nf90_inquire_dimension(iNCid, iDimId, len = iCols) )
                call check( nf90_inq_dimid(iNCid, "south_north", iDimId) )
                call check( nf90_inquire_dimension(iNCid, iDimId, len = iRows) )
                ! Close netCDF file
                call check( nf90_close(iNCid) )
                ! Info
                call mprintf(.true., iINFO_Main, ' Define land data dims ... OK')

            endif
            !------------------------------------------------------------------------------------ 
#else
            !------------------------------------------------------------------------------------ 
            ! Redefinition of info data flag (if netCDF library is not linked)
            iTypeData = 1 
            call mprintf(.true., iWARN, ' '// &
                                        'info gridded data type selected was netCDF but library is not linked! '// &
                                        'Will be used data in ASCII format!')
            !------------------------------------------------------------------------------------ 
#endif
        endif
        !------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------
        ! Get info from ASCII file
        if (iTypeData == 1) then
            
            !------------------------------------------------------------------------------------ 
            ! Filename dem
            sFileName = trim(sPathData)//trim(sDomainName)//'.dem.txt'
            
            ! Check file availability 
            inquire (file = trim(sFileName), exist = bFileExist)
            if ( .not. bFileExist ) then
                call mprintf(.true., iERROR, ' No land data ASCII found '//trim(sFileName) )
            else
                ! Info
                call mprintf(.true., iINFO_Extra, ' Land data in ASCII format ')
                ! Open, read and close ASCII grid file
                open(unit=1,file=sFileName, status='old')
                read(1,*)   sText, iCols ! 643
                read(1,*)   sText, iRows ! 534
                close(1)
                ! Info
                call mprintf(.true., iINFO_Main, ' Define land data dims ... OK')
            endif
            !------------------------------------------------------------------------------------ 
            
        endif
        !------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------
        ! Dims from local to global workspace
        oHMC_Namelist(iID)%iRowsL = iRows
        oHMC_Namelist(iID)%iColsL = iCols
        !------------------------------------------------------------------------------------
        
    end subroutine HMC_Info_Gridded_GetDims_Static
    !------------------------------------------------------------------------------------
    
    !------------------------------------------------------------------------------------
    ! Subroutine to get file forcing dimension(s)
    subroutine HMC_Info_Gridded_GetDims_Forcing(iID, iRows, iCols)
        
        !------------------------------------------------------------------------------------
        ! Variable(s) declaration
        integer(kind = 4)           :: iID
        integer(kind = 4)           :: iRows, iCols
        
        integer(kind = 4)           :: iTypeData
        character(len = 19)         :: sTime
        character(len = 256)        :: sPathData, sPID
        character(len = 700)        :: sFileName, sFileNameZip, sFileName_Temp
        character(len = 700)        :: sCommandUnzip
        character(len = 256)        :: sDomainName
        
        integer(kind = 4)           :: iFileID, iDimId, ppos

        logical                     :: bFileExist
        !------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------
        ! Initialize variable(s)
        sFileName = ""; sFileNameZip = ""; sFileName_Temp ="";
        !------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------
        ! Get information
        sDomainName = oHMC_Namelist(iID)%sDomainName
        sPathData = oHMC_Namelist(iID)%sPathData_Forcing_Gridded
        iTypeData = oHMC_Namelist(iID)%iFlagTypeData_Forcing_Gridded

        ! Get variable(s) dimension(s)
        call mprintf(.true., iINFO_Main, ' Define forcing data dims ... ')
        !------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------
        ! Get info from netCDF file
        if (iTypeData == 2) then
#ifdef LIB_NC
            !------------------------------------------------------------------------------------
            ! Info
            call mprintf(.true., iINFO_Extra, ' Forcing data in netCDF format ')
            !------------------------------------------------------------------------------------
            
            !------------------------------------------------------------------------------------
            ! Get information
            sTime = oHMC_Namelist(iID)%sTimeStart
            sCommandUnzip = oHMC_Namelist(iID)%sCommandUnzipFile
            
            ! Filename netCDF
            sFileName = trim(sPathData)//"hmc.forcing-grid."// &
                sTime(1:4)//sTime(6:7)//sTime(9:10)// & 
                sTime(12:13)//sTime(15:16)// &
                ".nc"

            ! Replace general path with specific time feature(s)
            call HMC_Tools_Generic_ReplaceText(sFileName, '$yyyy', sTime(1:4))
            call HMC_Tools_Generic_ReplaceText(sFileName, '$mm', sTime(6:7))
            call HMC_Tools_Generic_ReplaceText(sFileName, '$dd', sTime(9:10))

            ! Checking file input availability
            sFileNameZip = sFileName(1:len_trim(sFileName))//'.gz'
            inquire (file = trim(sFileNameZip), exist = bFileExist)
            if ( .not. bFileExist ) then
                call mprintf(.true., iERROR, ' No compressed forcing file netCDF found '//trim(sFileNameZip) )
            endif

            ! Get unique process ID
            sPID = adjustl(getProcessID()) 

            ! Create Filename with unique PID number to avoid simultaneously access to the same Forcing file 
            ! Remove file extension from filename
            ppos = scan(trim(sFileName),".", BACK= .true.)
            sFileName_Temp = trim(sFileName(1:ppos)//'_'//trim(sPID)//".nc") 

            ! Unzip file
            call HMC_Tools_Generic_UnzipFile(sCommandUnzip, sFileNameZip, sFileName_Temp, .true.)

            ! Check file availability
            inquire (file = trim(sFileName_Temp), exist = bFileExist)
            if ( .not. bFileExist ) then
                call mprintf(.true., iERROR, ' No forcing file netCDF found '//trim(sFileName_Temp) )
            else

                ! Open netCDF file
                call check( nf90_open(trim(sFileName_Temp), NF90_NOWRITE, iFileID) )
                ! Get global attribute(s)
                call check( nf90_get_att(iFileID, nf90_global, "ncols", iCols) )
                call check( nf90_get_att(iFileID, nf90_global, "nrows", iRows) )
                ! Close netCDF file
                call check( nf90_close(iFileID) )
                ! Info
                call mprintf(.true., iINFO_Main, ' Define forcing data dims ... OK')

                ! Remove uncompressed file (to save space on disk)
                call HMC_Tools_Generic_RemoveFile(oHMC_Namelist(iID)%sCommandRemoveFile, &
                                                      sFileName_Temp, .false.)
                !--------------------------------------------------------------------------------

            endif           

            !------------------------------------------------------------------------------------
#else
            !------------------------------------------------------------------------------------ 
            ! Redefinition of info data flag (if netCDF library is not linked)
            iTypeData = 1 
            call mprintf(.true., iWARN, ' '// &
                                        'info gridded data type selected was netCDF but library is not linked! '// &
                                        'Will be used data from namelist!')
            !------------------------------------------------------------------------------------ 
#endif
        endif
        !------------------------------------------------------------------------------------   
        
        !------------------------------------------------------------------------------------
        ! Get info from namelist 
        if (iTypeData == 1) then
            
            !------------------------------------------------------------------------------------
            ! Info
            call mprintf(.true., iINFO_Extra, ' Forcing data dims retrieved from namelist ')
            !------------------------------------------------------------------------------------
            
            !------------------------------------------------------------------------------------
            ! Get dimension(s) from namelist
            iRows = oHMC_Namelist(iID)%a1iDimsForcing(1)
            iCols = oHMC_Namelist(iID)%a1iDimsForcing(2)
            ! Info
            call mprintf(.true., iINFO_Main, ' Define forcing data dims ... OK')
            !------------------------------------------------------------------------------------
            
        endif
        !------------------------------------------------------------------------------------
                
        !------------------------------------------------------------------------------------
        ! Dims from local to global workspace
        oHMC_Namelist(iID)%iRowsF = iRows
        oHMC_Namelist(iID)%iColsF = iCols
        !------------------------------------------------------------------------------------
        
    end subroutine HMC_Info_Gridded_GetDims_Forcing
    !------------------------------------------------------------------------------------
    
    !------------------------------------------------------------------------------------
    ! Subroutine to get geographical information
    subroutine HMC_Info_Gridded_GetGeo_Static(iID)
    
        !------------------------------------------------------------------------------------
        ! Variable(s) declaration
        integer(kind = 4)           :: iID
        
        integer(kind = 4)           :: iTypeData
        character(len = 256)        :: sDomainName, sPathData
        
        integer(kind = 4)           :: iNCid, iDimId
        
        character(len = 256)        :: sText
        character(len = 700)        :: sFileName
        
        logical                     :: bFileExist
        
        integer(kind = 4)           :: iVar
        real(kind = 4)              :: dVar
        real(kind = 4)              :: dVarXLLCorner, dVarYLLCorner, dVarCellSize, dVarNoData
        !------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------
        ! Initialize variable(s)
        iVar = 0; dVar = 0;
        dVarXLLCorner = -9999.0; dVarYLLCorner = -9999.0; dVarCellSize = -9999.0; 
        dVarNoData = -9999.0; 
        !------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------
        ! Get information
        sDomainName = oHMC_Namelist(iID)%sDomainName
        sPathData = oHMC_Namelist(iID)%sPathData_Static_Gridded
        iTypeData = oHMC_Namelist(iID)%iFlagTypeData_Static

        ! Get variable(s) dimension(s)
        call mprintf(.true., iINFO_Main, ' Define land geographical data  ... ')
        !------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------
        ! Get info from netCDF data
        if (iTypeData == 2) then
#ifdef LIB_NC
            !------------------------------------------------------------------------------------
            ! Info
            call mprintf(.true., iINFO_Extra, ' Land data in netCDF format ')
            !------------------------------------------------------------------------------------
            
            !------------------------------------------------------------------------------------
            ! Filename
            sFileName = trim(sPathData)//'hmc.staticdata.nc'
            
            ! Check file availability
            inquire (file = trim(sFileName), exist = bFileExist)
            if ( .not. bFileExist ) then
                call mprintf(.true., iERROR, ' No land file netCDF found '//trim(sFileName) )
            else

                ! Open netCDF file
                call check( nf90_open(trim(sFileName), NF90_NOWRITE, iNCid) )
                ! Get global attribute(s)
                call check( nf90_get_att(iNCid, nf90_global, "xllcorner",    dVarXLLCorner) )
                call check( nf90_get_att(iNCid, nf90_global, "yllcorner",    dVarYLLCorner) )
                call check( nf90_get_att(iNCid, nf90_global, "cellsize",     dVarCellSize) )
                call check( nf90_get_att(iNCid, nf90_global, "nodata_value", dVarNoData) )
                ! Close netCDF file
                call check( nf90_close(iNCid) )
                ! Info
                call mprintf(.true., iINFO_Main, ' Define land geographical data  ... OK')
            endif
            !------------------------------------------------------------------------------------
#else
            !------------------------------------------------------------------------------------ 
            ! Redefinition of info data flag (if netCDF library is not linked)
            iTypeData = 1 
            call mprintf(.true., iWARN, ' '// &
                                        'info gridded data type selected was netCDF but library is not linked! '// &
                                        'Will be used data in ASCII format!')
            !------------------------------------------------------------------------------------ 
#endif
        endif
        !------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------
        ! Get info from ASCII data
        if (iTypeData == 1) then
            
            !------------------------------------------------------------------------------------
            ! Info
            call mprintf(.true., iINFO_Extra, ' Land data in ArcGrid format ')
            !------------------------------------------------------------------------------------
                
            !------------------------------------------------------------------------------------
            ! Filename
            sFileName = trim(sPathData)//trim(sDomainName)//'.dem.txt'
            
            ! Check file availability
            inquire (file = trim(sFileName), exist = bFileExist)
            if ( .not. bFileExist ) then
                call mprintf(.true., iERROR, ' No land file ArcGrid found '//trim(sFileName) )
            else
                ! Open, read and close ASCII grid file
                open(unit=1,file=sFileName, status='old')
                read(1,*)   sText, iVar
                read(1,*)   sText, iVar
                read(1,*)   sText, dVarXLLCorner 
                read(1,*)   sText, dVarYLLCorner 
                read(1,*)   sText, dVarCellSize 
                read(1,*)   sText, dVarNoData 
                close(1)
                ! Info
                call mprintf(.true., iINFO_Main, ' Define land geographical data  ... OK')
            endif
            !------------------------------------------------------------------------------------
            
        endif
        !------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------
        ! Pass local variable(s) to global workspace
        oHMC_Namelist(iID)%dXLLCornerL = dVarXLLCorner
        oHMC_Namelist(iID)%dYLLCornerL = dVarYLLCorner
        oHMC_Namelist(iID)%dXCellSizeL = dVarCellSize
        oHMC_Namelist(iID)%dYCellSizeL = dVarCellSize
        oHMC_Namelist(iID)%dNoDataL = dVarNoData
        !------------------------------------------------------------------------------------
        
    end subroutine HMC_Info_Gridded_GetGeo_Static
    !------------------------------------------------------------------------------------
    
    !------------------------------------------------------------------------------------
    ! Subroutine to create meteo grid (if static and forcing data have different grid)
    subroutine HMC_Info_Gridded_GetGeo_Forcing(iID)
        
        !------------------------------------------------------------------------------------
        ! Variable(s) declaration
        integer(kind = 4)           :: iID

        integer(kind = 4)           :: iFlagTypeData
        character(len = 19)         :: sTime
        character(len = 256)        :: sPathData, sPID
        character(len = 700)        :: sFileName, sFileNameZip, sFileName_Temp
        character(len = 700)        :: sCommandUnzip
        
        integer(kind = 4)           :: iVarCols, iVarRows
        real(kind = 4)              :: dVarXLLCorner, dVarYLLCorner
        real(kind = 4)              :: dVarXCellSize, dVarYCellSize
        real(kind = 4)              :: dVarNoData
        
        integer(kind = 4)           :: iErr, iFileID, ppos
        
        logical                     :: bFileExist
        !------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------
        ! Initialize variable(s)
        iVarCols = -9999; iVarRows = -9999; 
        dVarXLLCorner = -9999.0; dVarYLLCorner = -9999.0;
        dVarXCellSize = -9999.0; dVarYCellSize = -9999.0;
        dVarNoData = -9999.0;
        
        sFileName = ""; sFileNameZip = ""; sFileName_Temp = "";
        !------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------
        ! Check if grid control is defined
        if (.not. oHMC_Namelist(iID)%bGridCheck) then
            
            !------------------------------------------------------------------------------------
            ! Get information
            sPathData = oHMC_Namelist(iID)%sPathData_Forcing_Gridded
            iFlagTypeData = oHMC_Namelist(iID)%iFlagTypeData_Forcing_Gridded

            ! Data type
            call mprintf(.true., iINFO_Main, ' Define forcing geographical data  ... ')
            !------------------------------------------------------------------------------------
            
            !------------------------------------------------------------------------------------
            ! Get info from netCDF data
            if (iFlagTypeData == 2) then
#ifdef LIB_NC
                !------------------------------------------------------------------------------------
                ! Info
                call mprintf(.true., iINFO_Extra, ' Forcing data in netCDF format ')
                !------------------------------------------------------------------------------------
                
                !------------------------------------------------------------------------------------ 
                ! Get information
                sTime = oHMC_Namelist(iID)%sTimeStart
                sCommandUnzip = oHMC_Namelist(iID)%sCommandUnzipFile
                !------------------------------------------------------------------------------------ 
                
                !------------------------------------------------------------------------------------   
                ! Filename netCDF
                sFileName = trim(sPathData)//"hmc.forcing-grid."// &
                    sTime(1:4)//sTime(6:7)//sTime(9:10)// & 
                    sTime(12:13)//sTime(15:16)// &
                    ".nc"
                !------------------------------------------------------------------------------------
                
                !------------------------------------------------------------------------------------------
                ! Replace general path with specific time feature(s)
                call HMC_Tools_Generic_ReplaceText(sFileName, '$yyyy', sTime(1:4))
                call HMC_Tools_Generic_ReplaceText(sFileName, '$mm', sTime(6:7))
                call HMC_Tools_Generic_ReplaceText(sFileName, '$dd', sTime(9:10))
                !------------------------------------------------------------------------------------------
                    
                !------------------------------------------------------------------------------------------
                ! Checking file input availability
                sFileNameZip = trim(sFileName)//'.gz'
                inquire (file = trim(sFileNameZip), exist = bFileExist)
                if ( .not. bFileExist ) then
                    call mprintf(.true., iERROR, ' No compressed forcing file netCDF found '//trim(sFileNameZip) )
                endif
                !------------------------------------------------------------------------------------------

                !------------------------------------------------------------------------------------------
                ! Get unique porcess ID
                sPID = adjustl(getProcessID()) 

                ! Create Filename with unique PID number to avoid simultaneously access to the same Forcing file 
                ! Remove file extension from filename
                ppos = scan(trim(sFileName),".", BACK= .true.)
                sFileName_Temp = sFileName(1:ppos-1)//'_'//trim(sPID)//".nc"

                ! Unzip file
                call HMC_Tools_Generic_UnzipFile(sCommandUnzip, sFileNameZip, sFileName_Temp, .true.)
                !------------------------------------------------------------------------------------------
                
                !------------------------------------------------------------------------------------------
                ! Check uncompressed file availability
                inquire (file = trim(sFileName_Temp), exist = bFileExist)
                if ( .not. bFileExist ) then
                    call mprintf(.true., iERROR, ' No forcing file netCDF found '//trim(sFileName_Temp) )
                else
                    
                    ! Open nc file
                    call check( nf90_open(trim(sFileName_Temp), NF90_NOWRITE, iFileID) )
                    
                    ! Get global attribute(s)
                    call check( nf90_get_att(iFileID, nf90_global, "xllcorner",    dVarXLLCorner) )
                    call check( nf90_get_att(iFileID, nf90_global, "yllcorner",    dVarYLLCorner) )
                    call check( nf90_get_att(iFileID, nf90_global, "cellsize",     dVarXCellSize) )
                    call check( nf90_get_att(iFileID, nf90_global, "nodata_value", dVarNoData) )
                    
                    dVarYCellSize = dVarXCellSize
                    
                    ! Close nc file
                    call check( nf90_close(iFileID) )
                    
                    ! Remove uncompressed file (to save space on disk)
                    call HMC_Tools_Generic_RemoveFile(oHMC_Namelist(iID)%sCommandRemoveFile, &
                                                      sFileName_Temp, .false.)

                    ! Info
                    call mprintf(.true., iINFO_Main, ' Define forcing geographical data  ... OK ')

                endif
                !------------------------------------------------------------------------------------------
#else
                !------------------------------------------------------------------------------------ 
                ! Redefinition of info data flag (if netCDF library is not linked)
                iFlagTypeData = 1 
                call mprintf(.true., iWARN, ' '// &
                                            'info gridded data type selected was netCDF but library is not linked! '// &
                                            'Will be used data from namelist!')
                !------------------------------------------------------------------------------------             
#endif
            endif
            !------------------------------------------------------------------------------------------
            
            !------------------------------------------------------------------------------------------
            ! Get data from namelist
            if (iFlagTypeData == 1) then
                
                !------------------------------------------------------------------------------------
                ! Info
                call mprintf(.true., iINFO_Extra, ' Forcing data retrieved from namelist ')
                !------------------------------------------------------------------------------------
                
                !------------------------------------------------------------------------------------
                ! Get information
                dVarXLLCorner = oHMC_Namelist(iID)%a1dGeoForcing(2)
                dVarYLLCorner = oHMC_Namelist(iID)%a1dGeoForcing(1)
                dVarXCellSize = oHMC_Namelist(iID)%a1dResForcing(2)
                dVarYCellSize = oHMC_Namelist(iID)%a1dResForcing(1)
                dVarNoData = -9999.0
                
                ! Info
                call mprintf(.true., iINFO_Main, ' Define forcing geographical data  ... OK ')
                !------------------------------------------------------------------------------------
                
            endif
            !------------------------------------------------------------------------------------
            
            !------------------------------------------------------------------------------------
            ! Pass local variable(s) to global workspace
            oHMC_Namelist(iID)%dXLLCornerF = dVarXLLCorner
            oHMC_Namelist(iID)%dYLLCornerF = dVarYLLCorner
            oHMC_Namelist(iID)%dXCellSizeF = dVarXCellSize
            oHMC_Namelist(iID)%dYCellSizeF = dVarYCellSize
            oHMC_Namelist(iID)%dNoDataF = dVarNoData
            !------------------------------------------------------------------------------------
            
            !------------------------------------------------------------------------------------
            ! Compute indexes grid(s) ---> if grid are different XIndex and YIndex are not equal to -9999
            call HMC_Tools_Generic_CreateIndexGrid(oHMC_Namelist(iID)%iRowsL, oHMC_Namelist(iID)%iColsL, &
                                                   oHMC_Namelist(iID)%iRowsF, oHMC_Namelist(iID)%iColsF, &
                                                   oHMC_Namelist(iID)%dYLLCornerL, oHMC_Namelist(iID)%dXLLCornerL, &
                                                   oHMC_Namelist(iID)%dYLLCornerF, oHMC_Namelist(iID)%dXLLCornerF, &
                                                   oHMC_Namelist(iID)%dYCellSizeL, oHMC_Namelist(iID)%dXCellSizeL, &
                                                   oHMC_Namelist(iID)%dYCellSizeF, oHMC_Namelist(iID)%dXCellSizeF, &
                                                   oHMC_Namelist(iID)%iFlagGrid, &
                                                   oHMC_Vars(iID)%a2iXIndex, oHMC_Vars(iID)%a2iYIndex)
            !------------------------------------------------------------------------------------
                               
            !------------------------------------------------------------------------------------
            ! Flag grid control
            oHMC_Namelist(iID)%bGridCheck = .true.
            !------------------------------------------------------------------------------------
            
        endif
        !------------------------------------------------------------------------------------
        
    end subroutine HMC_Info_Gridded_GetGeo_Forcing
    !--------------------------------------------------------------------------------
    
end module HMC_Module_Info_Gridded
!--------------------------------------------------------------------------------

!------------------------------------------------------------------------------------------    
! File:   HMC_Module_Data_Output_Gridded.f90
! Author(s): Fabio Delogu, Francesco Silvestro, Simone Gabellani
!
! Created on May 6, 2015, 4:36 PM
!------------------------------------------------------------------------------------------

!------------------------------------------------------------------------------------------
! Module Header
module HMC_Module_Data_Output_Gridded
    
    !------------------------------------------------------------------------------------------
    ! External module(s) for all subroutine in this module
#ifdef LIB_NC
    use netcdf
#endif
    
    use HMC_Module_Namelist,        only:   oHMC_Namelist
    use HMC_Module_Vars_Loader,     only:   oHMC_Vars
    
#ifdef LIB_NC
    use HMC_Module_Tools_IO,        only:   HMC_Tools_IO_Put2d_Binary_INT, &
                                            HMC_Tools_IO_Put2d_Binary_DBL, &
                                            HMC_Tools_IO_Put3d_Binary, &
                                            HMC_Tools_IO_Put2d_NC, &
                                            HMC_Tools_IO_Put3d_NC, &
                                            HMC_Tools_IO_PutTime_DBL_NC, &
                                            HMC_Tools_IO_PutTime_STR_NC, &
                                            check
#else
    use HMC_Module_Tools_IO,        only:   HMC_Tools_IO_Put2d_Binary_INT, &
                                            HMC_Tools_IO_Put2d_Binary_DBL, &
                                            HMC_Tools_IO_Put3d_Binary                                         

#endif
                                                                         
    use HMC_Module_Tools_Debug
    use HMC_Module_Tools_Generic,   only:   HMC_Tools_Generic_ReplaceText, & 
                                            HMC_Tools_Generic_CreateFolder, &
                                            HMC_Tools_Generic_ZipFile, &
                                            HMC_Tools_Generic_RemoveFile, &
                                            transpose3Dvar
    
    use HMC_Module_Tools_Time,      only:   HMC_Tools_Time_DateDiff

    ! Implicit none for all subroutines in this module
    implicit none
    !------------------------------------------------------------------------------------------
    
contains
    
    !------------------------------------------------------------------------------------------
    ! Subroutine to manage output gridded data
    subroutine HMC_Data_Output_Gridded_Cpl( iID, sTime, &
                                            iRowsStart, iRowsEnd, iColsStart, iColsEnd, &
                                            iTime, iDaySteps)
        
        !------------------------------------------------------------------------------------------
        ! Variable(s)
        integer(kind = 4)           :: iID
        integer(kind = 4)           :: iRows, iCols
        integer(kind = 4)           :: iRowsStart, iRowsEnd, iColsStart, iColsEnd
        integer(kind = 4)           :: iTime, iDaySteps
        
        integer(kind = 4)           :: iActData_Generic, iActData_Flooding, iActData_Snow
        integer(kind = 4)           :: iAccumData_Hour, iStepData_Hour
        
        integer(kind = 4)           :: iFlagTypeData_Output, iFlagSnow, iFlagFlood
        integer(kind = 4)           :: iScaleFactor
        
        character(len = 19)         :: sTime
        character(len = 700)        :: sPathData_Output
        character(len = 700)        :: sCommandCreateFolder
               
        real(kind = 4), dimension(iRowsEnd - iRowsStart + 1, iColsEnd - iColsStart + 1)     ::  a2dVarDEM, a2dVarS
        
        real(kind = 4), dimension(iRowsEnd - iRowsStart + 1, &
                                  iColsEnd - iColsStart + 1)     ::  a2dVarRain
        real(kind = 4), dimension(iRowsEnd - iRowsStart + 1, &
                                  iColsEnd - iColsStart + 1)     ::  a2dVarLST, &
                                                                     a2dVarH, a2dVarLE, & 
                                                                     a2dVarETCum, a2dVarVTot, &
                                                                     a2dVarETPotCum, a2dVarET, a2dVarETPot, &
                                                                     a2dVarQout, &
                                                                     a2dVarSM, &
                                                                     a2dVarQfloodCR, & 
                                                                     a2dVarQfloodCL, &
                                                                     a2dVarLat, a2dVarLon
                                                                                                
        integer(kind = 4), dimension(iRowsEnd - iRowsStart + 1, &
                                     iColsEnd - iColsStart + 1)  ::  a2iVarAgeS
        real(kind = 4), dimension(iRowsEnd - iRowsStart + 1, &
                                  iColsEnd - iColsStart + 1)     ::  a2dVarSWE, &
                                                                     a2dVarMeltingS, a2dVarAlbedoS, & 
                                                                     a2dVarRhoS, a2dVarMeltingSDayCum, &
                                                                     a2dVarSnowFall                           
        real(kind = 4), dimension(iRowsEnd - iRowsStart + 1, &
                                  iColsEnd - iColsStart + 1, iDaySteps) :: a3dVarET, a3dVarETpot  
                                  
        real(kind = 4), dimension(iRowsEnd - iRowsStart + 1, &
                                  iColsEnd - iColsStart + 1, iDaySteps) :: a3dVarMeltingS
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Initialize variable(s)
        a2dVarDEM = 0.0; a2dVarS = 0.0; a2dVarLat = 0.0; a2dVarLon = 0.0 
        
        a2dVarRain = 0.0
        a2dVarLST = 0.0; a2dVarH = 0.0; a2dVarLE = 0.0; 
        a2dVarET = 0.0; a2dVarETCum = 0.0; a2dVarETPotCum = 0.0;
        a2dVarVTot = 0.0; a2dVarQout = 0.0; a2dVarSM = 0.0
        
        a2dVarSWE = 0.0; a2iVarAgeS = 0; a2dVarMeltingS = 0.0; a2dVarAlbedoS = 0.0; a2dVarRhoS = 0.0;
        
        a2dVarQfloodCR = 0.0; a2dVarQfloodCL = 0.0;
        
        a3dVarET = 0.0; a3dVarETpot = 0.0
        a3dVarMeltingS = 0.0
        
        sCommandCreateFolder = ""
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Defining iRows and iCols (output data)
        iRows = iRowsEnd - iRowsStart + 1
        iCols = iColsEnd - iColsStart + 1
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Get global information
        sPathData_Output = oHMC_Namelist(iID)%sPathData_Output_Gridded
        iFlagTypeData_Output = oHMC_Namelist(iID)%iFlagTypeData_Output_Gridded
        iScaleFactor = oHMC_Namelist(iID)%iScaleFactor
        sCommandCreateFolder = oHMC_Namelist(iID)%sCommandCreateFolder
        iFlagSnow = oHMC_Namelist(iID)%iFlagSnow
        iDaySteps = oHMC_Namelist(iID)%iDaySteps
        iFlagFlood = oHMC_Namelist(iID)%iFlagFlood  
        
        iActData_Generic = oHMC_Namelist(iID)%iActiveData_Output_Generic
        iActData_Flooding = oHMC_Namelist(iID)%iActiveData_Output_Flooding
        iActData_Snow = oHMC_Namelist(iID)%iActiveData_Output_Snow
        
        iAccumData_Hour = oHMC_Namelist(iID)%iAccumData_Output_Hour
        
        ! Info start
        call mprintf(.true., iINFO_Extra, ' Data :: Output gridded ... ' )
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Replace general path with specific time feature(s)
        call HMC_Tools_Generic_ReplaceText(sPathData_Output, '$yyyy', sTime(1:4))
        call HMC_Tools_Generic_ReplaceText(sPathData_Output, '$mm', sTime(6:7))
        call HMC_Tools_Generic_ReplaceText(sPathData_Output, '$dd', sTime(9:10))
        call HMC_Tools_Generic_ReplaceText(sPathData_Output, '$HH', sTime(12:13))
        call HMC_Tools_Generic_ReplaceText(sPathData_Output, '$MM', sTime(15:16))
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Create output folder
        call HMC_Tools_Generic_CreateFolder(sCommandCreateFolder, sPathData_Output, .true.)
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Get variable(s) from global workspace (LSM and convolution physics)
        a2dVarRain = oHMC_Vars(iID)%a2dRain
        a2dVarLST = oHMC_Vars(iID)%a2dLST
        a2dVarH = oHMC_Vars(iID)%a2dH; a2dVarLE = oHMC_Vars(iID)%a2dLE;
        a2dVarVTot = oHMC_Vars(iID)%a2dVTot; 
        
        a2dVarQout = oHMC_Vars(iID)%a2dQout; 
        a2dVarQfloodCR = oHMC_Vars(iID)%a2dQfloodCR
        a2dVarQfloodCL = oHMC_Vars(iID)%a2dQfloodCL
        
        a2dVarET = oHMC_Vars(iID)%a2dAE
        a2dVarETpot = oHMC_Vars(iID)%a2dAEpot
        
        a3dVarET = oHMC_Vars(iID)%a3dAE
        a3dVarETpot = oHMC_Vars(iID)%a3dAEpot

        a2dVarDEM = oHMC_Vars(iID)%a2dDem; a2dVarS = oHMC_Vars(iID)%a2dS; 
        a2dVarLat = oHMC_Vars(iID)%a2dLat; a2dVarLon = oHMC_Vars(iID)%a2dLon; 
        
        read(sTime(12:13) , *) iStepData_Hour
        if( (iStepData_Hour .eq. iAccumData_Hour) .and. (iTime .ge. iDaySteps) ) then
            a2dVarETCum = sum(a3dVarET(:, :, 1:int(iDaySteps)), dim=3)
            a2dVarETPotCum = sum(a3dVarETPot(:, :, 1:int(iDaySteps)), dim=3)
        endif
        
        !set no valid cells as no data
        where (a2dVarDEM.le.0.0)
            a2dVarETCum = -9999.0
            a2dVarETPotCum = -9999.0
        endwhere

        ! Compute derived information
        where (a2dVarDEM.gt.0.0)
            a2dVarSM = a2dVarVTot/a2dVarS
        elsewhere
            a2dVarSM = -9999.0
        endwhere
        
        ! Get variable(s) from global workspace (snow physics)
        if (iFlagSnow.eq.1) then
            a2dVarSWE = oHMC_Vars(iID)%a2dSWE
            a2iVarAgeS = oHMC_Vars(iID)%a2iAge
            a2dVarMeltingS = oHMC_Vars(iID)%a2dMelting
            
            a3dVarMeltingS = oHMC_Vars(iID)%a3dMelting
            !a2dVarMeltingSDayCum = oHMC_Vars(iID)%a2dMeltingDayCum
            a2dVarAlbedoS = oHMC_Vars(iID)%a2dAlbedo_Snow 
            a2dVarRhoS = oHMC_Vars(iID)%a2dRhoS
            a2dVarSnowFall = oHMC_Vars(iID)%a2dSnowFall
            
            if( (iStepData_Hour .eq. iAccumData_Hour) .and. (iTime .ge. iDaySteps) ) then
                a2dVarMeltingSDayCum = sum(a3dVarMeltingS(:, :, 1:int(iDaySteps)), dim=3)
            endif

        else
            a2iVarAgeS = -9999
            a2dVarSWE = -9999.0; a2dVarAlbedoS = -9999.0; a2dVarRhoS = -9999.0
            a2dVarMeltingS = -9999.0; a2dVarMeltingSDayCum = -9999.0; a2dVarSnowFall = -9999.0
        endif
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Subroutine for writing sequential netCDF output data 
        if (iFlagTypeData_Output == 2) then

            !------------------------------------------------------------------------------------------
            ! Call subroutine to write gridded data
#ifdef LIB_NC
            call HMC_Data_Output_Gridded_NC(iID, &
                                    sPathData_Output, &
                                    iRows, iCols, &
                                    iTime, sTime, iDaySteps, &
                                    iFlagSnow, &
                                    iFlagFlood, &
                                    iActData_Generic, iActData_Flooding, iActData_Snow, &
                                    iStepData_Hour, iAccumData_Hour, &
                                    a2dVarRain, &
                                    a2dVarLST, a2dVarH, a2dVarLE, & 
                                    a2dVarETCum, a2dVarETPotCum, a2dVarET, a2dVarETpot, &
                                    a2dVarVTot, &
                                    a2dVarQout, a2dVarQfloodCR, a2dVarQfloodCL, &
                                    a2dVarSWE, a2iVarAgeS, a2dVarMeltingS, a2dVarMeltingSDayCum, &
                                    a2dVarAlbedoS, a2dVarRhoS, a2dVarSnowFall, &
                                    a2dVarSM, &
                                    a2dVarLat, a2dVarLon)
#else
            ! Redefinition of output data flag (if netCDF library is not linked)
            iFlagTypeData_Output = 1  
            call mprintf(.true., iWARN, ' '// &
                                        'Output gridded data type selected was netCDF but library is not linked! '// &
                                        'Will be used data in binary format!')
#endif
            !------------------------------------------------------------------------------------------ 
            
        endif
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Subroutine for writing sequential binary output data 
        if (iFlagTypeData_Output == 1) then
            
            !------------------------------------------------------------------------------------------
            ! Call subroutine to read data
            call HMC_Data_Output_Gridded_Binary(iID, &
                                    sPathData_Output, &
                                    iRows, iCols, &
                                    iTime, sTime, iDaySteps, &
                                    iFlagSnow, iFlagFlood, &
                                    iActData_Generic, iActData_Flooding, iActData_Snow, &
                                    iStepData_Hour, iAccumData_Hour, &
                                    iScaleFactor, &
                                    a2dVarRain, &
                                    a2dVarLST, a2dVarH, a2dVarLE, &
                                    a2dVarETCum, a2dVarETPotCum, a2dVarET, a2dVarETpot, &
                                    a2dVarVTot, a2dVarQout, a2dVarQfloodCR, a2dVarQfloodCL, &
                                    a2dVarSWE, a2iVarAgeS, a2dVarMeltingS, a2dVarMeltingSDayCum, &
                                    a2dVarAlbedoS, a2dVarRhoS, a2dVarSnowFall, &
                                    a2dVarSM, & 
                                    a2dVarLat, a2dVarLon)
                                    
            !------------------------------------------------------------------------------------------
            
        endif
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Info end
        call mprintf(.true., iINFO_Extra, ' Data :: Output gridded ... OK' )
        !------------------------------------------------------------------------------------------
        
    end subroutine HMC_Data_Output_Gridded_Cpl
    !------------------------------------------------------------------------------------------
    
    !------------------------------------------------------------------------------------------
    ! Subroutine to write netCDF gridded data output
#ifdef LIB_NC
    subroutine HMC_Data_Output_Gridded_NC(iID,  &
                                          sPathData_Output, &
                                          iRows, iCols, &
                                          iTime, sTime, iDaySteps, &
                                          iFlagSnow, &
                                          iFlagFlood, &
                                          iActData_Generic, iActData_Flooding, iActData_Snow, &
                                          iStepData_Hour, iAccumData_Hour, &
                                          a2dVarRain, &
                                          a2dVarLST, a2dVarH, a2dVarLE, &
                                          a2dVarETCum, a2dVarETPotCum, a2dVarET, a2dVarETpot, &
                                          a2dVarVTot, &
                                          a2dVarQout, a2dVarQfloodCR, a2dVarQfloodCL, &
                                          a2dVarSWE, a2iVarAgeS, a2dVarMeltingS, a2dVarMeltingSDayCum, &
                                          a2dVarAlbedoS, a2dVarRhoS, a2dVarSnowFall, &
                                          a2dVarSM, &
                                          a2dVarLat, a2dVarLon)
                                      
        !------------------------------------------------------------------------------------------
        ! Variable(s)
        integer(kind = 4)                       :: iID                  
           
        character(len = 700), intent(in)        :: sPathData_Output
        character(len = 700)                    :: sFileNameData_Output
        character(len = 700)                    :: sCommandZipFile
        character(len = 256)                    :: sVarName, sVarNameLong
        character(len = 256)                    :: sVarGridMap, sVarDescription, sVarCoords
        character(len = 256)                    :: sVarUnits
        integer(kind = 4), intent(in)           :: iRows, iCols
        
        integer(kind = 4)                       :: iTime, iDaySteps, iTimeStep
        character(len = 19)                     :: sTime, sTimeSave, sTimeRef
        
        integer(kind = 4)                       :: iFlagSnow, iFlagFlood
        integer(kind = 4)                       :: iActData_Generic, iActData_Flooding, iActData_Snow
        integer(kind = 4)                       :: iStepData_Hour, iAccumData_Hour
        
        real(kind = 4)                          :: dVarMissingValue, dScale_Factor
        real(kind = 4)                          :: dVarXLLCorner, dVarYLLCorner
        real(kind = 4)                          :: dVarCellSizeX, dVarCellSizeY
        
        real(kind = 4), dimension(iRows, iCols), intent(in)         :: a2dVarRain
        real(kind = 4), dimension(iRows, iCols), intent(in)         :: a2dVarLST
        real(kind = 4), dimension(iRows, iCols), intent(in)         :: a2dVarH
        real(kind = 4), dimension(iRows, iCols), intent(in)         :: a2dVarLE
        real(kind = 4), dimension(iRows, iCols), intent(in)         :: a2dVarETCum    
        real(kind = 4), dimension(iRows, iCols), intent(in)         :: a2dVarETPotCum
        real(kind = 4), dimension(iRows, iCols), intent(in)         :: a2dVarET    
        real(kind = 4), dimension(iRows, iCols), intent(in)         :: a2dVarETPot 
        
        real(kind = 4), dimension(iRows, iCols), intent(in)         :: a2dVarVTot       
        real(kind = 4), dimension(iRows, iCols), intent(in)         :: a2dVarQout
        real(kind = 4), dimension(iRows, iCols), intent(in)         :: a2dVarQfloodCR, a2dVarQfloodCL

        real(kind = 4), dimension(iRows, iCols), intent(in)         :: a2dVarLat
        real(kind = 4), dimension(iRows, iCols), intent(in)         :: a2dVarLon
        real(kind = 4), dimension(iRows, iCols), intent(in)         :: a2dVarSM
        
        integer(kind = 4), dimension(iRows, iCols), intent(in)      :: a2iVarAgeS
        real(kind = 4), dimension(iRows, iCols), intent(in)         :: a2dVarSWE
        real(kind = 4), dimension(iRows, iCols), intent(in)         :: a2dVarMeltingS
        real(kind = 4), dimension(iRows, iCols), intent(in)         :: a2dVarMeltingSDayCum
        real(kind = 4), dimension(iRows, iCols), intent(in)         :: a2dVarAlbedoS
        real(kind = 4), dimension(iRows, iCols), intent(in)         :: a2dVarRhoS
        real(kind = 4), dimension(iRows, iCols), intent(in)         :: a2dVarSnowFall

        integer(kind = 4)       :: iErr
        
        integer(kind = 4)       :: iFileID
        integer(kind = 4)       :: iID_Dim_Rows, iID_Dim_Cols, iID_Dim_Time_DBL, iID_Dim_Time_STR
        integer(kind = 4)       :: iID_Dim_Day1Steps

        real(kind = 8)          :: dTimeHours_Diff, dTimeSeconds_Diff
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Initialize variable(s)
        sCommandZipFile = ""
        sTimeRef = '1970-01-01_00:00:00'
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Get information from global workspace
        sCommandZipFile = oHMC_Namelist(iID)%sCommandZipFile
        
        dVarXLLCorner = oHMC_Namelist(iID)%dXLLCornerL
        dVarYLLCorner = oHMC_Namelist(iID)%dYLLCornerL
        dVarCellSizeX = oHMC_Namelist(iID)%dXCellSizeL
        dVarCellSizeY = oHMC_Namelist(iID)%dYCellSizeL
        !dVarMissingValue = oHMC_Namelist(iID)%dNoDataL
        
        ! Info start       
        call mprintf(.true., iINFO_Extra, ' Data :: Output gridded :: NetCDF ... ' )
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Define elapsed hours from reference date
        call HMC_Tools_Time_DateDiff(sTime, sTimeRef, dTimeSeconds_Diff, dTimeHours_Diff)
        !------------------------------------------------------------------------------------------

        !------------------------------------------------------------------------------------------
        ! Filename output (example: hmc.outputdata.201404300000.nc)
        sFileNameData_Output = trim(sPathData_Output)//"hmc.output-grid."// &
        sTime(1:4)//sTime(6:7)//sTime(9:10)// & 
        sTime(12:13)//sTime(15:16)// &
        ".nc"
        
        ! Info netCDF filename
        call mprintf(.true., iINFO_Verbose, ' Save filename (result gridded): '//trim(sFileNameData_Output)//' ... ')
        !------------------------------------------------------------------------------------------

        !------------------------------------------------------------------------------------------
        ! Create netcdf file
        call check( nf90_create(trim(sFileNameData_Output), NF90_NETCDF4, iFileID) )
	
        ! Dimension(s)
        call check( nf90_def_dim(iFileID, "time", 1, iID_Dim_Time_DBL) )
        call check( nf90_def_dim(iFileID, "time_str_length", 19, iID_Dim_Time_STR) )
        call check( nf90_def_dim(iFileID, "south_north", iRows, iID_Dim_Rows) )
        call check( nf90_def_dim(iFileID, "west_east", iCols, iID_Dim_Cols) )
        call check( nf90_def_dim(iFileID, "day1_steps", iDaySteps, iID_Dim_Day1Steps) )
        !------------------------------------------------------------------------------------------

        !------------------------------------------------------------------------------------------
        ! Global attribute(s)
        sTimeSave(1:len_trim(sTime)) = sTime
        call check( nf90_put_att(iFileID, NF90_GLOBAL, "time_coverage_end", sTimeSave) )
        call check( nf90_put_att(iFileID, NF90_GLOBAL, "xllcorner", dVarXLLCorner) )
        call check( nf90_put_att(iFileID, NF90_GLOBAL, "yllcorner", dVarYLLCorner) )
        call check( nf90_put_att(iFileID, NF90_GLOBAL, "xcellsize", dVarCellSizeX) )
        call check( nf90_put_att(iFileID, NF90_GLOBAL, "ycellsize", dVarCellSizeY) )
        call check( nf90_put_att(iFileID, NF90_GLOBAL, "nrows", iRows) )        
        call check( nf90_put_att(iFileID, NF90_GLOBAL, "ncols", iCols) )
        !------------------------------------------------------------------------------------------

        !------------------------------------------------------------------------------------------
        ! Definition mode OFF - Data mode ON
        call check( nf90_enddef(iFileID))
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Writing time variable(s) in netcdf output file
        ! TIMES
        sVarName = 'times'; sVarNameLong = 'times definition of output datasets'; 
        sVarDescription = 'times';
        sVarUnits = 'time units'; 
        sVarCoords = ''; iTimeStep = 1
        call HMC_Tools_IO_PutTime_STR_NC(iFileID, iID_Dim_Time_STR, & 
                             sVarName, sVarNameLong, sVarDescription, &
                             sVarUnits, sVarCoords, &
                             iTimeStep, sTimeSave)
        ! TIME
        sVarName = 'time'; sVarNameLong = 'time definition of output datasets'; 
        sVarDescription = 'synthesized time coordinate from Times(time)'; sVarUnits = 'secs since '//trim(sTimeRef); 
        sVarCoords = 't'; iTimeStep = 1
        call HMC_Tools_IO_PutTime_DBL_NC(iFileID, iID_Dim_Time_DBL, & 
                             sVarName, sVarNameLong, sVarDescription, &
                             sVarUnits, sVarCoords, &
                             iTimeStep, dTimeSeconds_Diff)
        !------------------------------------------------------------------------------------------
                                 
        !------------------------------------------------------------------------------------------
        ! Writing static variable(s) in netcdf output file
        ! LONGITUDE
        sVarName = 'Longitude'; sVarNameLong = 'longitude coordinate'; sVarDescription = 'longitude';
        sVarUnits = 'degree_east'; sVarGridMap = 'epsg:4326'; dVarMissingValue = -9E15;
        sVarCoords = '';
        call HMC_Tools_IO_Put2d_NC(iFileID, iID_Dim_Cols, iID_Dim_Rows, & 
                             sVarName, sVarNameLong, sVarDescription, &
                             sVarUnits, sVarCoords, sVarGridMap, dVarMissingValue, &
                             iCols, iRows, transpose(a2dVarLon))
        ! LATITUDE
        sVarName = 'Latitude'; sVarNameLong = 'latitude coordinate'; sVarDescription = 'latitude';
        sVarUnits = 'degree_north'; sVarGridMap = 'epsg:4326'; dVarMissingValue = -9E15;
        sVarCoords = '';
        call HMC_Tools_IO_Put2d_NC(iFileID, iID_Dim_Cols, iID_Dim_Rows, & 
                             sVarName, sVarNameLong, sVarDescription, &
                             sVarUnits, sVarCoords, sVarGridMap, dVarMissingValue, &
                             iCols, iRows, transpose(a2dVarLat))
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Writing dynamic variable(s) in netCDF output file
        
        ! BASIC DATASETS
        ! Soil Moisture 
        sVarName = 'SM'; sVarNameLong = 'soil_moisture'; sVarDescription = 'soil moisture';
        sVarUnits = '-'; sVarGridMap = 'epsg:4326'; dVarMissingValue = -9E15;
        sVarCoords = 'Longitude Latitude';
        call HMC_Tools_IO_Put2d_NC(iFileID, iID_Dim_Cols, iID_Dim_Rows, & 
                             sVarName, sVarNameLong, sVarDescription, &
                             sVarUnits, sVarCoords, sVarGridMap, dVarMissingValue, &
                             iCols, iRows, transpose(a2dVarSM))  
        
        ! Qout (Distributed discharge values)
        sVarName = 'Discharge'; sVarNameLong = 'discharge'; sVarDescription = 'discharge';
        sVarUnits = 'm^3/s'; sVarGridMap = 'epsg:4326'; dVarMissingValue = -9E15;
        sVarCoords = 'Longitude Latitude';
        call HMC_Tools_IO_Put2d_NC(iFileID, iID_Dim_Cols, iID_Dim_Rows, & 
                             sVarName, sVarNameLong, sVarDescription, &
                             sVarUnits, sVarCoords, sVarGridMap, dVarMissingValue, &
                             iCols, iRows, transpose(a2dVarQout))  
                             
       ! Evapotranspiration
       sVarName = 'ET'; sVarNameLong = 'et'; 
       sVarDescription = 'evapotranspiration';
       sVarUnits = 'mm'; sVarGridMap = 'epsg:4326'; dVarMissingValue = -9E15;
       sVarCoords = 'Longitude Latitude';
       call HMC_Tools_IO_Put2d_NC(iFileID, iID_Dim_Cols, iID_Dim_Rows, & 
                            sVarName, sVarNameLong, sVarDescription, &
                            sVarUnits, sVarCoords, sVarGridMap, dVarMissingValue, &
                            iCols, iRows, transpose(a2dVarET))
                             
        ! EXTENDED DATASETS
        if( iActData_Generic .eq. 2) then
            
            ! Effective Rain
            sVarName = 'REff'; sVarNameLong = 'effective_rain'; sVarDescription = 'effective rain';
            sVarUnits = 'mm'; sVarGridMap = 'epsg:4326'; dVarMissingValue = -9E15;
            sVarCoords = 'Longitude Latitude';
            call HMC_Tools_IO_Put2d_NC(iFileID, iID_Dim_Cols, iID_Dim_Rows, & 
                                 sVarName, sVarNameLong, sVarDescription, &
                                 sVarUnits, sVarCoords, sVarGridMap, dVarMissingValue, &
                                 iCols, iRows, transpose(a2dVarRain))  

            ! H  
            sVarName = 'H'; sVarNameLong = 'sensible_heat'; sVarDescription = 'sensible heat flux';
            sVarUnits = 'W/m^2'; sVarGridMap = 'epsg:4326'; dVarMissingValue = -9E15;
            sVarCoords = 'Longitude Latitude';
            call HMC_Tools_IO_Put2d_NC(iFileID, iID_Dim_Cols, iID_Dim_Rows, & 
                                 sVarName, sVarNameLong, sVarDescription, &
                                 sVarUnits, sVarCoords, sVarGridMap, dVarMissingValue, &
                                 iCols, iRows, transpose(a2dVarH))  
            ! LE
            sVarName = 'LE'; sVarNameLong = 'latent_heat'; sVarDescription = 'latent heat flux';
            sVarUnits = 'W/m^2'; sVarGridMap = 'epsg:4326'; dVarMissingValue = -9E15;
            sVarCoords = 'Longitude Latitude';
            call HMC_Tools_IO_Put2d_NC(iFileID, iID_Dim_Cols, iID_Dim_Rows, & 
                                 sVarName, sVarNameLong, sVarDescription, &
                                 sVarUnits, sVarCoords, sVarGridMap, dVarMissingValue, &
                                 iCols, iRows, transpose(a2dVarLE))      
                              
            ! LST
            sVarName = 'LST'; sVarNameLong = 'land_surface_temperature'; sVarDescription = 'land surface temperature';
            sVarUnits = 'K'; sVarGridMap = 'epsg:4326'; dVarMissingValue = -9E15;
            sVarCoords = 'Longitude Latitude';
            call HMC_Tools_IO_Put2d_NC(iFileID, iID_Dim_Cols, iID_Dim_Rows, & 
                                 sVarName, sVarNameLong, sVarDescription, &
                                 sVarUnits, sVarCoords, sVarGridMap, dVarMissingValue, &
                                 iCols, iRows, transpose(a2dVarLST))             
            ! VTOT
            sVarName = 'VTot'; sVarNameLong = 'volume_tot'; sVarDescription = 'total volume';
            sVarUnits = 'mm'; sVarGridMap = 'epsg:4326'; dVarMissingValue = -9E15;
            sVarCoords = 'Longitude Latitude';
            call HMC_Tools_IO_Put2d_NC(iFileID, iID_Dim_Cols, iID_Dim_Rows, & 
                                 sVarName, sVarNameLong, sVarDescription, &
                                 sVarUnits, sVarCoords, sVarGridMap, dVarMissingValue, &
                                 iCols, iRows, transpose(a2dVarVTot))   
            ! ETCum
            if( (iStepData_Hour .eq. iAccumData_Hour) .and. (iTime .ge. iDaySteps) ) then
                sVarName = 'ETCum'; sVarNameLong = 'et_accumulated'; 
                sVarDescription = 'evapotranspiration accumulated over 1 day';
                sVarUnits = 'mm'; sVarGridMap = 'epsg:4326'; dVarMissingValue = -9E15;
                sVarCoords = 'Longitude Latitude';
                call HMC_Tools_IO_Put2d_NC(iFileID, iID_Dim_Cols, iID_Dim_Rows, & 
                                     sVarName, sVarNameLong, sVarDescription, &
                                     sVarUnits, sVarCoords, sVarGridMap, dVarMissingValue, &
                                     iCols, iRows, transpose(a2dVarETCum))
            endif
            ! ETPotCum
            if( (iStepData_Hour .eq. iAccumData_Hour) .and. (iTime .ge. iDaySteps) ) then
                sVarName = 'ETPotCum'; sVarNameLong = 'potential_et_accumulated'; 
                sVarDescription = 'potential evapotranspiration accumulated over 1 day';
                sVarUnits = 'mm'; sVarGridMap = 'epsg:4326'; dVarMissingValue = -9E15;
                sVarCoords = 'Longitude Latitude';
                call HMC_Tools_IO_Put2d_NC(iFileID, iID_Dim_Cols, iID_Dim_Rows, & 
                                     sVarName, sVarNameLong, sVarDescription, &
                                     sVarUnits, sVarCoords, sVarGridMap, dVarMissingValue, &
                                     iCols, iRows, transpose(a2dVarETPotCum))
            endif
            
        endif
        !------------------------------------------------------------------------------------------ 
                                     
        !------------------------------------------------------------------------------------------
        ! Flooding variable(s)                
        if ( (iFlagFlood .eq. 1) .and. (iActData_Flooding .eq. 1) ) then

            ! Q flooding right banks (Distributed discharge values)
            sVarName = 'QfloodR'; sVarNameLong = 'Right Banks Flood Discharges'; 
            sVarDescription = 'right banks flood discharges';
            sVarUnits = 'm^3/s'; sVarGridMap = 'epsg:4326'; dVarMissingValue = -9E15;
            sVarCoords = 'Longitude Latitude';
            call HMC_Tools_IO_Put2d_NC(iFileID, iID_Dim_Cols, iID_Dim_Rows, & 
                     sVarName, sVarNameLong, sVarDescription, &
                     sVarUnits, sVarCoords, sVarGridMap, dVarMissingValue, &
                     iCols, iRows, transpose(a2dVarQfloodCR))   
            ! Q flooding left banks (Distributed discharge values)
            sVarName = 'QfloodL'; sVarNameLong = 'Left Banks Flood Discharges'; 
            sVarDescription = 'left banks flood discharges';
            sVarUnits = 'm^3/s'; sVarGridMap = 'epsg:4326'; dVarMissingValue = -9E15;
            sVarCoords = 'Longitude Latitude';
            call HMC_Tools_IO_Put2d_NC(iFileID, iID_Dim_Cols, iID_Dim_Rows, & 
                     sVarName, sVarNameLong, sVarDescription, &
                     sVarUnits, sVarCoords, sVarGridMap, dVarMissingValue, &
                     iCols, iRows, transpose(a2dVarQfloodCL))  
        endif
        !------------------------------------------------------------------------------------------           
        
        !------------------------------------------------------------------------------------------                      
        ! Snow variable(s)                
        if ( (iFlagSnow .eq. 1) .and. (iActData_Snow .eq. 1) ) then
            
            ! SWE
            sVarName = 'SWE'; sVarNameLong = 'snow water equivalent'; sVarDescription = 'SWE';
            sVarUnits = 'mm'; sVarGridMap = 'epsg:4326'; dVarMissingValue = -9E15;
            sVarCoords = 'Longitude Latitude';
            call HMC_Tools_IO_Put2d_NC(iFileID, iID_Dim_Cols, iID_Dim_Rows, & 
                                 sVarName, sVarNameLong, sVarDescription, &
                                 sVarUnits, sVarCoords, sVarGridMap, dVarMissingValue, &
                                 iCols, iRows, transpose(a2dVarSWE))   
            ! Snow Melting
            sVarName = 'MeltingS'; sVarNameLong = 'snow melting'; sVarDescription = 'meltings';
            sVarUnits = 'mm'; sVarGridMap = 'epsg:4326'; dVarMissingValue = -9E15;
            sVarCoords = 'Longitude Latitude';
            call HMC_Tools_IO_Put2d_NC(iFileID, iID_Dim_Cols, iID_Dim_Rows, & 
                                 sVarName, sVarNameLong, sVarDescription, &
                                 sVarUnits, sVarCoords, sVarGridMap, dVarMissingValue, &
                                 iCols, iRows, transpose(a2dVarMeltingS))  
            
            ! Snow density
            sVarName = 'RhoS'; sVarNameLong = 'snow density'; sVarDescription = 'rhos';
            sVarUnits = 'kg/m^3'; sVarGridMap = 'epsg:4326'; dVarMissingValue = -9E15;
            sVarCoords = 'Longitude Latitude';
            call HMC_Tools_IO_Put2d_NC(iFileID, iID_Dim_Cols, iID_Dim_Rows, & 
                                 sVarName, sVarNameLong, sVarDescription, &
                                 sVarUnits, sVarCoords, sVarGridMap, dVarMissingValue, &
                                 iCols, iRows, transpose(a2dVarRhoS))
            ! Snowfall
            sVarName = 'SnowFall'; sVarNameLong = 'snowfall'; sVarDescription = 'snowfall';
            sVarUnits = 'mm'; sVarGridMap = 'epsg:4326'; dVarMissingValue = -9E15;
            sVarCoords = 'Longitude Latitude';
            call HMC_Tools_IO_Put2d_NC(iFileID, iID_Dim_Cols, iID_Dim_Rows, & 
                                 sVarName, sVarNameLong, sVarDescription, &
                                 sVarUnits, sVarCoords, sVarGridMap, dVarMissingValue, &
                                 iCols, iRows, transpose(a2dVarSnowFall))
                                 
            ! Daily variable(s)     
            if( iStepData_Hour .eq. iAccumData_Hour ) then
                ! Snow albedo
                sVarName = 'AlbedoS'; sVarNameLong = 'snow albedo'; sVarDescription = 'albedos';
                sVarUnits = '-'; sVarGridMap = 'epsg:4326'; dVarMissingValue = -9E15;
                sVarCoords = 'Longitude Latitude';
                call HMC_Tools_IO_Put2d_NC(iFileID, iID_Dim_Cols, iID_Dim_Rows, & 
                                     sVarName, sVarNameLong, sVarDescription, &
                                     sVarUnits, sVarCoords, sVarGridMap, dVarMissingValue, &
                                     iCols, iRows, transpose(a2dVarAlbedoS))                 
                ! Snow age
                sVarName = 'AgeS'; sVarNameLong = 'snow age'; sVarDescription = 'ages';
                sVarUnits = 'timestep'; sVarGridMap = 'epsg:4326'; dVarMissingValue = -9E15;                
                sVarCoords = 'Longitude Latitude';
                call HMC_Tools_IO_Put2d_NC(iFileID, iID_Dim_Cols, iID_Dim_Rows, & 
                                     sVarName, sVarNameLong, sVarDescription, &
                                     sVarUnits, sVarCoords, sVarGridMap, dVarMissingValue, &
                                     iCols, iRows, transpose(real(a2iVarAgeS)))
                ! Snow melting daily cumulated           
                sVarName = 'MeltingSDayCum'; sVarNameLong = 'daily cumulated snow melting'; sVarDescription = 'meltingsdaycum';
                sVarUnits = 'mm'; sVarGridMap = 'epsg:4326'; dVarMissingValue = -9E15;
                sVarCoords = 'Longitude Latitude';
                call HMC_Tools_IO_Put2d_NC(iFileID, iID_Dim_Cols, iID_Dim_Rows, & 
                                     sVarName, sVarNameLong, sVarDescription, &
                                     sVarUnits, sVarCoords, sVarGridMap, dVarMissingValue, &
                                     iCols, iRows, transpose(a2dVarMeltingSDayCum))                                        
            endif
            
        endif
        !------------------------------------------------------------------------------------------
                
        !------------------------------------------------------------------------------------------
        ! Close
        call check( nf90_close(iFileID) )
        ! Info
        call mprintf(.true., iINFO_Verbose, ' Save filename (result gridded): '//trim(sFileNameData_Output)//' ... OK')
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Zip file
        call HMC_Tools_Generic_ZipFile(sCommandZipFile, &
                                       sFileNameData_Output//'.gz', sFileNameData_Output, .false.)
        ! Remove un-zipped file
        !call HMC_Tools_Generic_RemoveFile(oHMC_Namelist(iID)%sCommandRemoveFile, sFileNameData_Output, .false.)
                                       
        ! Info end      
        call mprintf(.true., iINFO_Extra, ' Data :: Output gridded :: NetCDF ... OK ' )
        !------------------------------------------------------------------------------------------
        
    end subroutine HMC_Data_Output_Gridded_NC
#endif
    !------------------------------------------------------------------------------------------
    
    !------------------------------------------------------------------------------------------
    ! Subroutine to write Binary data output
    subroutine  HMC_Data_Output_Gridded_Binary(iID, &
                                    sPathData_Output, &
                                    iRows, iCols, &
                                    iTime, sTime, iDaySteps, &
                                    iFlagSnow, iFlagFlood, &
                                    iActData_Generic, iActData_Flooding, iActData_Snow, &
                                    iStepData_Hour, iAccumData_Hour, &
                                    iVarScale, &
                                    a2dVarRain, &
                                    a2dVarLST, a2dVarH, a2dVarLE, &
                                    a2dVarETCum, a2dVarETPotCum, a2dVarET, a2dVarETpot, &
                                    a2dVarVTot, a2dVarQout, a2dVarQfloodCR, a2dVarQfloodCL, &
                                    a2dVarSWE, a2iVarAgeS, a2dVarMeltingS, a2dVarMeltingSDayCum, &
                                    a2dVarAlbedoS, a2dVarRhoS, a2dVarSnowFall, &
                                    a2dVarSM, & 
                                    a2dVarLat, a2dVarLon)                                     
                        
        !------------------------------------------------------------------------------------------
        ! Variable(s)
        integer(kind = 4)                       :: iID                  
                                  
        character(len = 700), intent(in)        :: sPathData_Output
        character(len = 700)                    :: sFileNameData_Output
        character(len = 256)                    :: sVarName
        integer(kind = 4), intent(in)           :: iRows, iCols
        integer(kind = 4)                       :: iNSection, iNData
        integer(kind = 4)                       :: iNLake, iNDam, iNPlant, iNJoint, iNCatch, iNRelease
       
        integer(kind = 4)                       :: iFlagSnow, iFlagFlood
        integer(kind = 4)                       :: iActData_Generic, iActData_Flooding, iActData_Snow
        integer(kind = 4)                       :: iStepData_Hour, iAccumData_Hour
        
        integer(kind = 4)                       :: iVarScale
        
        integer(kind = 4)                       :: iTime, iDaySteps
        character(len = 19), intent(in)         :: sTime
        
        real(kind = 4), dimension(iCols, iRows)                 :: a2dVarData
        
        real(kind = 4), dimension(iRows, iCols)                 :: a2dVarRain
        real(kind = 4), dimension(iRows, iCols)                 :: a2dVarLST
        real(kind = 4), dimension(iRows, iCols)                 :: a2dVarH
        real(kind = 4), dimension(iRows, iCols)                 :: a2dVarLE
        real(kind = 4), dimension(iRows, iCols)                 :: a2dVarETCum 
        real(kind = 4), dimension(iRows, iCols)                 :: a2dVarETPotCum 
        real(kind = 4), dimension(iRows, iCols)                 :: a2dVarET
        real(kind = 4), dimension(iRows, iCols)                 :: a2dVarETpot
        
        real(kind = 4), dimension(iRows, iCols)                 :: a2dVarVTot       
        real(kind = 4), dimension(iRows, iCols)                 :: a2dVarQout
        real(kind = 4), dimension(iRows, iCols)                 :: a2dVarQfloodCR, a2dVarQfloodCL

        real(kind = 4), dimension(iRows, iCols)         :: a2dVarLat
        real(kind = 4), dimension(iRows, iCols)         :: a2dVarLon
        real(kind = 4), dimension(iRows, iCols)         :: a2dVarSM

        integer(kind = 4), dimension(iRows, iCols)      :: a2iVarAgeS
        real(kind = 4), dimension(iRows, iCols)         :: a2dVarSWE
        real(kind = 4), dimension(iRows, iCols)         :: a2dVarMeltingS
        real(kind = 4), dimension(iRows, iCols)         :: a2dVarMeltingSDayCum
        real(kind = 4), dimension(iRows, iCols)         :: a2dVarAlbedoS
        real(kind = 4), dimension(iRows, iCols)         :: a2dVarRhoS
        real(kind = 4), dimension(iRows, iCols)         :: a2dVarSnowFall
       
        character(len = 256)    :: sVarUnits
        integer(kind = 4)       :: iErr
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Initialize variable(s)
        a2dVarData = 0.0
        ! Info start       
        call mprintf(.true., iINFO_Extra, ' Data :: Output gridded :: Binary ... ' )
        ! Info filename(s) at each step
        call mprintf(.true., iINFO_Verbose, ' Save (result gridded) at time: '//trim(sTime)//' ... ')
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Writing dynamic variable(s) in binary output file
        
        ! BASIC DATASETS
        ! SM
        iVarScale = 10000
        sFileNameData_Output = trim(sPathData_Output)//"SM_"// &
                           sTime(1:4)//sTime(6:7)//sTime(9:10)//sTime(12:13)//sTime(15:16)// &
                           ".bin"            
        call mprintf(.true., iINFO_Extra, ' Save filename: '//trim(sFileNameData_Output) )
        call HMC_Tools_IO_Put2d_Binary_INT(sFileNameData_Output, a2dVarSM, iRows, iCols, iVarScale, .true., iErr)   
        call HMC_Tools_Generic_ZipFile(oHMC_Namelist(iID)%sCommandZipFile, &
                                       sFileNameData_Output//'.gz', sFileNameData_Output, .false.)
        !call HMC_Tools_Generic_RemoveFile(oHMC_Namelist(iID)%sCommandRemoveFile, sFileNameData_Output, .false. )
        
        ! Evapotranspiration
        iVarScale = 10000
        sFileNameData_Output = trim(sPathData_Output)//"ET_"// &
                           sTime(1:4)//sTime(6:7)//sTime(9:10)//sTime(12:13)//sTime(15:16)// &
                           ".bin"            
        call mprintf(.true., iINFO_Extra, ' Save filename: '//trim(sFileNameData_Output) )
        call HMC_Tools_IO_Put2d_Binary_INT(sFileNameData_Output, a2dVarET, iRows, iCols, iVarScale, .true., iErr)   
        call HMC_Tools_Generic_ZipFile(oHMC_Namelist(iID)%sCommandZipFile, &
                                       sFileNameData_Output//'.gz', sFileNameData_Output, .false.)
        !call HMC_Tools_Generic_RemoveFile(oHMC_Namelist(iID)%sCommandRemoveFile, sFileNameData_Output, .false. )
                                       
        ! Qout (Distributed discharge values)
        iVarScale = 10000
        sFileNameData_Output = trim(sPathData_Output)//"Q_"// &
                           sTime(1:4)//sTime(6:7)//sTime(9:10)//sTime(12:13)//sTime(15:16)// &
                           ".bin"            
        call mprintf(.true., iINFO_Extra, ' Save filename: '//trim(sFileNameData_Output) )
        call HMC_Tools_IO_Put2d_Binary_INT(sFileNameData_Output, a2dVarQout, iRows, iCols, iVarScale, .true., iErr)  
        call HMC_Tools_Generic_ZipFile(oHMC_Namelist(iID)%sCommandZipFile, &
                                       sFileNameData_Output//'.gz', sFileNameData_Output, .false.)
        !call HMC_Tools_Generic_RemoveFile(oHMC_Namelist(iID)%sCommandRemoveFile, sFileNameData_Output, .false. )
        
        ! EXTENDED DATASETS
        if( iActData_Generic .eq. 2) then
            
            ! Effective Rain
            iVarScale = 10
            sFileNameData_Output = trim(sPathData_Output)//"REff_"// &
                               sTime(1:4)//sTime(6:7)//sTime(9:10)//sTime(12:13)//sTime(15:16)// &
                               ".bin"  
            call mprintf(.true., iINFO_Extra, ' Save filename: '//trim(sFileNameData_Output) )
            call HMC_Tools_IO_Put2d_Binary_INT(sFileNameData_Output, a2dVarRain, iRows, iCols, iVarScale, .true., iErr)
            call HMC_Tools_Generic_ZipFile(oHMC_Namelist(iID)%sCommandZipFile, &
                                           sFileNameData_Output//'.gz', sFileNameData_Output, .false.)
            !call HMC_Tools_Generic_RemoveFile(oHMC_Namelist(iID)%sCommandRemoveFile, sFileNameData_Output, .false.)

            ! H  
            iVarScale = 10000
            sFileNameData_Output = trim(sPathData_Output)//"H_"// &
                               sTime(1:4)//sTime(6:7)//sTime(9:10)//sTime(12:13)//sTime(15:16)// &
                               ".bin"  
            call mprintf(.true., iINFO_Extra, ' Save filename: '//trim(sFileNameData_Output) )
            call HMC_Tools_IO_Put2d_Binary_INT(sFileNameData_Output, a2dVarH, iRows, iCols, iVarScale, .true., iErr)
            call HMC_Tools_Generic_ZipFile(oHMC_Namelist(iID)%sCommandZipFile, &
                                           sFileNameData_Output//'.gz', sFileNameData_Output, .false.)
            !call HMC_Tools_Generic_RemoveFile(oHMC_Namelist(iID)%sCommandRemoveFile, sFileNameData_Output, .false.)

            ! LE
            iVarScale = 10000
            sFileNameData_Output = trim(sPathData_Output)//"LE_"// &
                               sTime(1:4)//sTime(6:7)//sTime(9:10)//sTime(12:13)//sTime(15:16)// &
                               ".bin"  
            call mprintf(.true., iINFO_Extra, ' Save filename: '//trim(sFileNameData_Output) )
            call HMC_Tools_IO_Put2d_Binary_INT(sFileNameData_Output, a2dVarLE, iRows, iCols, iVarScale, .true., iErr)
            call HMC_Tools_Generic_ZipFile(oHMC_Namelist(iID)%sCommandZipFile, &
                                           sFileNameData_Output//'.gz', sFileNameData_Output, .false.)
            !call HMC_Tools_Generic_RemoveFile(oHMC_Namelist(iID)%sCommandRemoveFile, sFileNameData_Output, .false. )
                                
            ! LST
            iVarScale = 10000
            sFileNameData_Output = trim(sPathData_Output)//"LST_"// &
                               sTime(1:4)//sTime(6:7)//sTime(9:10)//sTime(12:13)//sTime(15:16)// &
                               ".bin"            
            call mprintf(.true., iINFO_Extra, ' Save filename: '//trim(sFileNameData_Output) )
            call HMC_Tools_IO_Put2d_Binary_INT(sFileNameData_Output, a2dVarLST, iRows, iCols, iVarScale, .true., iErr)  
            call HMC_Tools_Generic_ZipFile(oHMC_Namelist(iID)%sCommandZipFile, &
                                           sFileNameData_Output//'.gz', sFileNameData_Output, .false.)
            !call HMC_Tools_Generic_RemoveFile(oHMC_Namelist(iID)%sCommandRemoveFile, sFileNameData_Output, .false.)

            ! VTOT
            iVarScale = 10000
            sFileNameData_Output = trim(sPathData_Output)//"VTot_"// &
                               sTime(1:4)//sTime(6:7)//sTime(9:10)//sTime(12:13)//sTime(15:16)// &
                               ".bin"            
            call mprintf(.true., iINFO_Extra, ' Save filename: '//trim(sFileNameData_Output) )
            call HMC_Tools_IO_Put2d_Binary_INT(sFileNameData_Output, a2dVarVTot, iRows, iCols, iVarScale, .true., iErr)   
            call HMC_Tools_Generic_ZipFile(oHMC_Namelist(iID)%sCommandZipFile, &
                                           sFileNameData_Output//'.gz', sFileNameData_Output, .false.)
            !call HMC_Tools_Generic_RemoveFile(oHMC_Namelist(iID)%sCommandRemoveFile, sFileNameData_Output, .false. )
            
            ! ETpot
            iVarScale = 10000
            sFileNameData_Output = trim(sPathData_Output)//"ETpot_"// &
                               sTime(1:4)//sTime(6:7)//sTime(9:10)//sTime(12:13)//sTime(15:16)// &
                               ".bin"            
            call mprintf(.true., iINFO_Extra, ' Save filename: '//trim(sFileNameData_Output) )
            call HMC_Tools_IO_Put2d_Binary_INT(sFileNameData_Output, a2dVarETpot, iRows, iCols, iVarScale, .true., iErr)   
            call HMC_Tools_Generic_ZipFile(oHMC_Namelist(iID)%sCommandZipFile, &
                                           sFileNameData_Output//'.gz', sFileNameData_Output, .false.)
            !call HMC_Tools_Generic_RemoveFile(oHMC_Namelist(iID)%sCommandRemoveFile, sFileNameData_Output, .false. )
                                                                 
            ! ETCum
            if( (iStepData_Hour .eq. iAccumData_Hour) .and. (iTime .ge. iDaySteps) ) then
                iVarScale = 10000
                sFileNameData_Output = trim(sPathData_Output)//"ETCum_"// &
                                   sTime(1:4)//sTime(6:7)//sTime(9:10)//sTime(12:13)//sTime(15:16)// &
                                   ".bin"            
                call mprintf(.true., iINFO_Extra, ' Save filename: '//trim(sFileNameData_Output) )
                call HMC_Tools_IO_Put2d_Binary_INT(sFileNameData_Output, a2dVarETCum, iRows, iCols, iVarScale, .true., iErr)
                call HMC_Tools_Generic_ZipFile(oHMC_Namelist(iID)%sCommandZipFile, &
                                               sFileNameData_Output//'.gz', sFileNameData_Output, .false.)

                sFileNameData_Output = trim(sPathData_Output)//"ETPotCum_"// &
                                   sTime(1:4)//sTime(6:7)//sTime(9:10)//sTime(12:13)//sTime(15:16)// &
                                   ".bin"            
                call mprintf(.true., iINFO_Extra, ' Save filename: '//trim(sFileNameData_Output) )
                call HMC_Tools_IO_Put2d_Binary_INT(sFileNameData_Output, a2dVarETPotCum, iRows, iCols, iVarScale, .true., iErr)
                call HMC_Tools_Generic_ZipFile(oHMC_Namelist(iID)%sCommandZipFile, &
                                               sFileNameData_Output//'.gz', sFileNameData_Output, .false.)
                !call HMC_Tools_Generic_RemoveFile(oHMC_Namelist(iID)%sCommandRemoveFile, sFileNameData_Output, .false.)
            endif
     
        endif
        !------------------------------------------------------------------------------------------

        !------------------------------------------------------------------------------------------
        ! Flooding variable(s)                
        if ( (iFlagFlood .eq. 1) .and. (iActData_Flooding .eq. 1) )then
            ! Q flooding right banks (Distributed discharge values)
            iVarScale = 10000
            sFileNameData_Output = trim(sPathData_Output)//"Qfr_"// &
                               sTime(1:4)//sTime(6:7)//sTime(9:10)//sTime(12:13)//sTime(15:16)// &
                               ".bin"            
            call mprintf(.true., iINFO_Extra, ' Save filename: '//trim(sFileNameData_Output) )
            call HMC_Tools_IO_Put2d_Binary_INT(sFileNameData_Output, a2dVarQfloodCR, iRows, iCols, iVarScale, .true., iErr)  
            call HMC_Tools_Generic_ZipFile(oHMC_Namelist(iID)%sCommandZipFile, &
                                            sFileNameData_Output//'.gz', sFileNameData_Output, .false.)

            ! Q flooding left banks (Distributed discharge values)
            iVarScale = 10000
            sFileNameData_Output = trim(sPathData_Output)//"Qfl_"// &
                               sTime(1:4)//sTime(6:7)//sTime(9:10)//sTime(12:13)//sTime(15:16)// &
                               ".bin"            
            call mprintf(.true., iINFO_Extra, ' Save filename: '//trim(sFileNameData_Output) )
            call HMC_Tools_IO_Put2d_Binary_INT(sFileNameData_Output, a2dVarQfloodCL, iRows, iCols, iVarScale, .true., iErr)  
            call HMC_Tools_Generic_ZipFile(oHMC_Namelist(iID)%sCommandZipFile, &
                                            sFileNameData_Output//'.gz', sFileNameData_Output, .false.)
        endif                                       
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Snow variable(s)
        if ( (iFlagSnow .eq. 1) .and. (iActData_Snow .eq. 1) ) then
            
            ! SWE
            iVarScale = 1;
            sFileNameData_Output = trim(sPathData_Output)//"SWE_"// &
                                sTime(1:4)//sTime(6:7)//sTime(9:10)//sTime(12:13)//sTime(15:16)// &
                                ".bin"            
            call mprintf(.true., iINFO_Extra, ' Save filename: '//trim(sFileNameData_Output) )
            call HMC_Tools_IO_Put2d_Binary_DBL(sFileNameData_Output, a2dVarSWE, iRows, iCols, iVarScale, .true., iErr)  
            call HMC_Tools_Generic_ZipFile(oHMC_Namelist(iID)%sCommandZipFile, &
                        sFileNameData_Output//'.gz', sFileNameData_Output, .false.)
            !call HMC_Tools_Generic_RemoveFile(oHMC_Namelist(iID)%sCommandRemoveFile, sFileNameData_Output, .false. )
            
            ! Snow melting
            iVarScale = 1;
            sFileNameData_Output = trim(sPathData_Output)//"Melting_"// &
                                sTime(1:4)//sTime(6:7)//sTime(9:10)//sTime(12:13)//sTime(15:16)// &
                                ".bin"            
            call mprintf(.true., iINFO_Extra, ' Save filename: '//trim(sFileNameData_Output) )
            call HMC_Tools_IO_Put2d_Binary_DBL(sFileNameData_Output, a2dVarMeltingS, iRows, iCols, iVarScale, .true., iErr)  
            call HMC_Tools_Generic_ZipFile(oHMC_Namelist(iID)%sCommandZipFile, &
                        sFileNameData_Output//'.gz', sFileNameData_Output, .false.)
            !call HMC_Tools_Generic_RemoveFile(oHMC_Namelist(iID)%sCommandRemoveFile, sFileNameData_Output, .false. )
            
            ! Snow density
            iVarScale = 1;
            sFileNameData_Output = trim(sPathData_Output)//"Density_"// &
                                sTime(1:4)//sTime(6:7)//sTime(9:10)//sTime(12:13)//sTime(15:16)// &
                                ".bin"            
            call mprintf(.true., iINFO_Extra, ' Save filename: '//trim(sFileNameData_Output) )
            call HMC_Tools_IO_Put2d_Binary_DBL(sFileNameData_Output, a2dVarRhoS, iRows, iCols, iVarScale, .true., iErr)  
            call HMC_Tools_Generic_ZipFile(oHMC_Namelist(iID)%sCommandZipFile, &
                        sFileNameData_Output//'.gz', sFileNameData_Output, .false.)
            !call HMC_Tools_Generic_RemoveFile(oHMC_Namelist(iID)%sCommandRemoveFile, sFileNameData_Output, .false. )
            
            ! Snowfall
            iVarScale = 1;
            sFileNameData_Output = trim(sPathData_Output)//"Snowfall_"// &
                                sTime(1:4)//sTime(6:7)//sTime(9:10)//sTime(12:13)//sTime(15:16)// &
                                ".bin"            
            call mprintf(.true., iINFO_Extra, ' Save filename: '//trim(sFileNameData_Output) )
            call HMC_Tools_IO_Put2d_Binary_DBL(sFileNameData_Output, a2dVarSnowFall, iRows, iCols, iVarScale, .true., iErr)  
            call HMC_Tools_Generic_ZipFile(oHMC_Namelist(iID)%sCommandZipFile, &
                        sFileNameData_Output//'.gz', sFileNameData_Output, .false.)
            !call HMC_Tools_Generic_RemoveFile(oHMC_Namelist(iID)%sCommandRemoveFile, sFileNameData_Output, .false. )
                             
            ! Daily variable(s)     
            if ( iStepData_Hour .eq. iAccumData_Hour ) then
            
                ! Snow albedo
                iVarScale = 1;
                sFileNameData_Output = trim(sPathData_Output)//"AlbedoS_"// &
                                    sTime(1:4)//sTime(6:7)//sTime(9:10)//sTime(12:13)//sTime(15:16)// &
                                    ".bin"            
                call mprintf(.true., iINFO_Extra, ' Save filename: '//trim(sFileNameData_Output) )
                call HMC_Tools_IO_Put2d_Binary_DBL(sFileNameData_Output, a2dVarAlbedoS, iRows, iCols, iVarScale, .true., iErr)  
                call HMC_Tools_Generic_ZipFile(oHMC_Namelist(iID)%sCommandZipFile, &
                            sFileNameData_Output//'.gz', sFileNameData_Output, .false.)
                !call HMC_Tools_Generic_RemoveFile(oHMC_Namelist(iID)%sCommandRemoveFile, sFileNameData_Output, .false. )
                
                ! Snow melting daily cumulated
                iVarScale = 1;
                sFileNameData_Output = trim(sPathData_Output)//"MeltingDay_"// &
                                    sTime(1:4)//sTime(6:7)//sTime(9:10)//sTime(12:13)//sTime(15:16)// &
                                    ".bin"            
                call mprintf(.true., iINFO_Extra, ' Save filename: '//trim(sFileNameData_Output) )
                call HMC_Tools_IO_Put2d_Binary_DBL(sFileNameData_Output, a2dVarMeltingSDayCum, iRows, iCols, & 
                                                   iVarScale, .true., iErr)  
                call HMC_Tools_Generic_ZipFile(oHMC_Namelist(iID)%sCommandZipFile, &
                            sFileNameData_Output//'.gz', sFileNameData_Output, .false.)
                !call HMC_Tools_Generic_RemoveFile(oHMC_Namelist(iID)%sCommandRemoveFile, sFileNameData_Output, .false. )
                
                ! Snow age
                iVarScale = 1;
                sFileNameData_Output = trim(sPathData_Output)//"Age_"// &
                                    sTime(1:4)//sTime(6:7)//sTime(9:10)//sTime(12:13)//sTime(15:16)// &
                                    ".bin"            
                call mprintf(.true., iINFO_Extra, ' Save filename: '//trim(sFileNameData_Output) )
                call HMC_Tools_IO_Put2d_Binary_DBL(sFileNameData_Output, real(a2iVarAgeS), iRows, iCols, iVarScale, .true., iErr)  
                call HMC_Tools_Generic_ZipFile(oHMC_Namelist(iID)%sCommandZipFile, &
                            sFileNameData_Output//'.gz', sFileNameData_Output, .false.)
                !call HMC_Tools_Generic_RemoveFile(oHMC_Namelist(iID)%sCommandRemoveFile, sFileNameData_Output, .false. )
                
            endif
        endif
        !------------------------------------------------------------------------------------------           

        !------------------------------------------------------------------------------------------
        ! Info filename(s) at each step
        call mprintf(.true., iINFO_Verbose, ' Save (result gridded) at time: '//trim(sTime)//' ... OK')
        ! Info end     
        call mprintf(.true., iINFO_Extra, ' Data :: Output gridded :: Binary ... OK' )
        !------------------------------------------------------------------------------------------
                                                                                      
    end subroutine HMC_Data_Output_Gridded_Binary
    !------------------------------------------------------------------------------------------
    
end module HMC_Module_Data_Output_Gridded
!------------------------------------------------------------------------------------------

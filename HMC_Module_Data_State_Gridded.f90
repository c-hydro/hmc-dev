!------------------------------------------------------------------------------------------    
! File:   HMC_Module_Data_State_Gridded.f90
! Author(s): Fabio Delogu, Francesco Silvestro, Simone Gabellani
!
! Created on May 7, 2015, 1:27 PM
!------------------------------------------------------------------------------------------

!------------------------------------------------------------------------------------------
! Module Header
module HMC_Module_Data_State_Gridded
    
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
    ! Subroutine to manage state gridded data
    subroutine HMC_Data_State_Gridded_Cpl( iID, sTime, &
                                           iRowsStart, iRowsEnd, iColsStart, iColsEnd, &
                                           iDaySteps, iTMarkedSteps)
        
        !------------------------------------------------------------------------------------------
        ! Variable(s)
        integer(kind = 4)           :: iID
        integer(kind = 4)           :: iRows, iCols
        integer(kind = 4)           :: iRowsStart, iRowsEnd, iColsStart, iColsEnd
        integer(kind = 4)           :: iDaySteps, iTMarkedSteps

        integer(kind = 4)           :: iFlagTypeData_State, iFlagSnow, iFlagCType
        integer(kind = 4)           :: iScaleFactor
        
        character(len = 19)         :: sTime
        character(len = 700)        :: sPathData_State
        character(len = 700)        :: sCommandCreateFolder
        
        real(kind = 4), dimension(iRowsEnd - iRowsStart + 1, iColsEnd - iColsStart + 1) :: a2dVarDEM
        
        real(kind = 4), dimension(iRowsEnd - iRowsStart + 1, iColsEnd - iColsStart + 1) :: a2dVarVTot, a2dVarVRet, &
                                                                                           a2dVarHydro, a2dVarRouting, &
                                                                                           a2dVarFlowDeep, &
                                                                                           a2dVarWTable, a2dVarLST, &
                                                                                           a2dVarLat, a2dVarLon
                                                                                           
        real(kind = 4), dimension(iRowsEnd - iRowsStart + 1, iColsEnd - iColsStart + 1) :: a2dVarHydroC, a2dVarHydroH, & 
                                                                                           a2dVarQup
                                                                                                                                                                        
        real(kind = 4), dimension(iRowsEnd - iRowsStart + 1, iColsEnd - iColsStart + 1)  :: a2dVarWSRunoff
    
        integer(kind = 4), dimension(iRowsEnd - iRowsStart + 1, iColsEnd - iColsStart + 1)  :: a2iVarAgeS
        real(kind = 4), dimension(iRowsEnd - iRowsStart + 1, iColsEnd - iColsStart + 1)     :: a2dVarSWE, a2dVarAlbedoS, a2dVarRhoS
        real(kind = 4), dimension(iRowsEnd - iRowsStart + 1, iColsEnd - iColsStart + 1, iDaySteps)      :: a3dVarTaC_1Days  
        real(kind = 4), dimension(iRowsEnd - iRowsStart + 1, iColsEnd - iColsStart + 1, iDaySteps*5)    :: a3dVarTaC_5Days
        
        real(kind = 4), dimension(iRowsEnd - iRowsStart + 1, iColsEnd - iColsStart + 1)                 :: a2dVarWTableUpd
        real(kind = 4), dimension(iRowsEnd - iRowsStart + 1, iColsEnd - iColsStart + 1, iTMarkedSteps)  :: a3dVarTaKMarked
        real(kind = 4), dimension(iRowsEnd - iRowsStart + 1, iColsEnd - iColsStart + 1, iDaySteps)      :: a3dVarTaK24    
        !------------------------------------------------------------------------------------------

        !------------------------------------------------------------------------------------------
        ! Initialize variable(s)
        a2dVarVTot = 0.0; a2dVarVRet = 0.0; a2dVarHydro = 0.0; a2dVarRouting = 0.0; a2dVarFlowDeep = 0.0; 
        a2dVarLST = 0.0; a2dVarWTable = 0.0; a3dVarTaKMarked = 0.0; a3dVarTaK24  = 0.0;
        a2dVarLat = 0.0; a2dVarLon = 0.0
        
        a2dVarHydroC = 0.0; a2dVarHydroH = 0.0; a2dVarQup = 0.0;
        
        a2iVarAgeS = 0; a2dVarSWE = 0.0; a2dVarAlbedoS = 0.0; a2dVarRhoS = 0.0;
        
        a2dVarWSRunoff = 0.0
        
        a2dVarDEM = 0.0; a2dVarWTableUpd = 0.0;
        
        iFlagSnow = -9999; iFlagCType = -9999;
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Defining iRows and iCols
        iRows = iRowsEnd - iRowsStart + 1
        iCols = iColsEnd - iColsStart + 1
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Get global information
        sPathData_State = oHMC_Namelist(iID)%sPathData_State_Gridded
        iFlagTypeData_State = oHMC_Namelist(iID)%iFlagTypeData_State_Gridded
        iScaleFactor = oHMC_Namelist(iID)%iScaleFactor
        sCommandCreateFolder = oHMC_Namelist(iID)%sCommandCreateFolder
        iFlagSnow = oHMC_Namelist(iID)%iFlagSnow
        iFlagCType = oHMC_Namelist(iID)%iFlagCType
        
        ! Info start
        call mprintf(.true., iINFO_Extra, ' Data :: State gridded ... ' )
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
        ! Get variable(s) from global workspace
        a2dVarDem = oHMC_Vars(iID)%a2dDem
        
        a2dVarVTot = oHMC_Vars(iID)%a2dVTot; a2dVarVRet = oHMC_Vars(iID)%a2dVRet
        a2dVarHydro = oHMC_Vars(iID)%a2dHydro; a2dVarRouting = oHMC_Vars(iID)%a2dRouting
        a2dVarWTable = oHMC_Vars(iID)%a2dWTable; 
        a2dVarLST = oHMC_Vars(iID)%a2dLST
        a2dVarFlowDeep = oHMC_Vars(iID)%a2dFlowDeep
        a2dVarWSRunoff = oHMC_Vars(iID)%a2dWSRunoff 
        
        a2dVarHydroC = oHMC_Vars(iID)%a2dHydroC;
        a2dVarHydroH = oHMC_Vars(iID)%a2dHydroH;
        a2dVarQup = oHMC_Vars(iID)%a2dQup;
        
        a3dVarTaKMarked = oHMC_Vars(iID)%a3dTaKMarked; a3dVarTaK24 = oHMC_Vars(iID)%a3dTaK24
        a2dVarLat = oHMC_Vars(iID)%a2dLat; a2dVarLon = oHMC_Vars(iID)%a2dLon
        
        ! Get variable(s) from global workspace (snow physics)
        if (iFlagSnow.eq.1) then
            a2dVarSWE = oHMC_Vars(iID)%a2dSWE
            a2iVarAgeS = oHMC_Vars(iID)%a2iAge
            a2dVarAlbedoS = oHMC_Vars(iID)%a2dAlbedo_Snow 
            a2dVarRhoS = oHMC_Vars(iID)%a2dRhoS
            a2dVarRhoS = oHMC_Vars(iID)%a2dRhoS
            a2dVarRhoS = oHMC_Vars(iID)%a2dRhoS
            a3dVarTaC_1Days = oHMC_Vars(iID)%a3dTaC_Days1
            a3dVarTaC_5Days = oHMC_Vars(iID)%a3dTaC_Days5
        else
            a2dVarSWE = -9999.0; a2iVarAgeS = -9999
            a2dVarAlbedoS = -9999.0; a2dVarRhoS = -9999.0 
        endif

        ! Variable(s) conversion
        where(a2dVarDem.gt.0.0)
            a2dVarWTableUpd = (a2dVarDem - a2dVarWTable)*1000
        endwhere
        
        call mprintf(.true., iINFO_Extra, checkvar(a2dVarDem, oHMC_Vars(iID)%a2iMask, 'DEM') )
        call mprintf(.true., iINFO_Extra, checkvar(a2dVarWTable, oHMC_Vars(iID)%a2iMask, 'WTABLE') )
        call mprintf(.true., iINFO_Extra, checkvar(a2dVarWTableUpd, oHMC_Vars(iID)%a2iMask, 'WTABLE UPD') )
        !------------------------------------------------------------------------------------------
        
        
        !------------------------------------------------------------------------------------------
        ! Subroutine to write netCDF state data 
        if (iFlagTypeData_State == 2) then
            
            !------------------------------------------------------------------------------------------
            ! Call subroutine to write data in netCDF format
#ifdef LIB_NC
            call HMC_Data_State_Gridded_NC(iID, &
                                    sPathData_State, &
                                    iRows, iCols, &
                                    iDaySteps, iTMarkedSteps, &
                                    sTime, iFlagSnow, iFlagCType,  &
                                    a2dVarVTot, a2dVarVRet, &
                                    a2dVarHydro, a2dVarRouting, &
                                    a2dVarHydroC, a2dVarHydroH, a2dVarQup, &
                                    a2dVarFlowDeep, &
                                    a2dVarWTableUpd, &
                                    a2dVarLST, a3dVarTaKMarked, a3dVarTaK24, &
                                    a2dVarSWE, a2dVarAlbedoS, a2dVarRhoS, a2iVarAgeS, &
                                    a3dVarTaC_1Days, a3dVarTaC_5Days, &
                                    a2dVarWSRunoff, &
                                    a2dVarLat, a2dVarLon)
#else   
            ! Redefinition of state data flag (if netCDF library is not linked)
            iFlagTypeData_State = 1 
            call mprintf(.true., iWARN, ' '// &
                                        'State gridded data type selected was netCDF but library is not linked! '// &
                                        'Will be used data in binary format!')
#endif
            !------------------------------------------------------------------------------------------

        endif
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Subroutine to write binary state data  
        if (iFlagTypeData_State == 1) then
            
            !------------------------------------------------------------------------------------------
            ! Call subroutine to write data in binary format
            call HMC_Data_State_Gridded_Binary(iID, &
                                    sPathData_State, &
                                    iRows, iCols, &
                                    iDaySteps, iTMarkedSteps, &
                                    sTime, iFlagSnow, iFlagCType, &
                                    iScaleFactor, &
                                    a2dVarVTot, a2dVarVRet, &
                                    a2dVarHydro, a2dVarRouting, &
                                    a2dVarHydroC, a2dVarHydroH, a2dVarQup, &
                                    a2dVarFlowDeep, &
                                    a2dVarWTableUpd, &
                                    a2dVarLST, a3dVarTaKMarked, a3dVarTaK24, &
                                    a2dVarSWE, a2dVarAlbedoS, a2dVarRhoS, a2iVarAgeS, &
                                    a3dVarTaC_1Days, a3dVarTaC_5Days, &
                                    a2dVarWSRunoff, &
                                    a2dVarLat, a2dVarLon)
                                    
            !------------------------------------------------------------------------------------------
            
        endif
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Info end
        call mprintf(.true., iINFO_Extra, ' Data :: State gridded ... OK ' )
        !------------------------------------------------------------------------------------------
        
    end subroutine HMC_Data_State_Gridded_Cpl
    !------------------------------------------------------------------------------------------
    
    !------------------------------------------------------------------------------------------
    ! Subroutine to write netCDF data state
#ifdef LIB_NC
    subroutine HMC_Data_State_Gridded_NC(iID,  &
                                           sPathData_State, &
                                           iRows, iCols, &
                                           iDaySteps, iTMarkedSteps, &
                                           sTime, iFlagSnow, iFlagCType, &
                                           a2dVarVTot, a2dVarVRet, &
                                           a2dVarHydro, a2dVarRouting, &
                                           a2dVarHydroC, a2dVarHydroH, a2dVarQup, &
                                           a2dVarFlowDeep, &
                                           a2dVarWTableUpd, &
                                           a2dVarLST, a3dVarTaKMarked, a3dVarTaK24, &
                                           a2dVarSWE, a2dVarAlbedoS, a2dVarRhoS, a2iVarAgeS, &
                                           a3dVarTaC_1Days, a3dVarTaC_5Days, &
                                           a2dVarWSRunoff, &
                                           a2dVarLat, a2dVarLon)
                                      
        !------------------------------------------------------------------------------------------
        ! Variable(s)
        integer(kind = 4)                       :: iID    
        
        character(len = 256), intent(in)        :: sPathData_State
        character(len = 700)                    :: sFileNameData_State
        character(len = 700)                    :: sCommandZipFile
        character(len = 256)                    :: sVarName, sVarNameLong
        character(len = 256)                    :: sVarGridMap, sVarDescription, sVarCoords
        character(len = 256)                    :: sVarUnits
        integer(kind = 4), intent(in)           :: iRows, iCols
        integer(kind = 4)                       :: iDaySteps, iTMarkedSteps
        integer(kind = 4)                       :: iFlagSnow, iFlagCType
        
        integer(kind = 4)                       :: iTimeStep
        character(len = 19)                     :: sTime, sTimeSave, sTimeRef
        
        real(kind = 4)                          :: dVarMissingValue, dScale_Factor
        real(kind = 4)                          :: dVarXLLCorner, dVarYLLCorner
        real(kind = 4)                          :: dVarCellSizeX, dVarCellSizeY
        
        real(kind = 4), dimension(iRows, iCols), intent(in)                 :: a2dVarVTot
        real(kind = 4), dimension(iRows, iCols), intent(in)                 :: a2dVarVRet
        real(kind = 4), dimension(iRows, iCols), intent(in)                 :: a2dVarHydro
        real(kind = 4), dimension(iRows, iCols), intent(in)                 :: a2dVarRouting      
        real(kind = 4), dimension(iRows, iCols), intent(in)                 :: a2dVarHydroC
        real(kind = 4), dimension(iRows, iCols), intent(in)                 :: a2dVarHydroH
        real(kind = 4), dimension(iRows, iCols), intent(in)                 :: a2dVarQup
        real(kind = 4), dimension(iRows, iCols), intent(in)                 :: a2dVarFlowDeep
        real(kind = 4), dimension(iRows, iCols), intent(in)                 :: a2dVarWTableUpd 
        real(kind = 4), dimension(iRows, iCols), intent(in)                 :: a2dVarLST 
        real(kind = 4), dimension(iRows, iCols), intent(in)                 :: a2dVarLat
        real(kind = 4), dimension(iRows, iCols), intent(in)                 :: a2dVarLon
        real(kind = 4), dimension(iRows, iCols, iTMarkedSteps), intent(in)  :: a3dVarTaKMarked
        real(kind = 4), dimension(iRows, iCols, iDaySteps), intent(in)      :: a3dVarTaK24
        real(kind = 4), dimension(iRows, iCols), intent(in)                 :: a2dVarSWE
        real(kind = 4), dimension(iRows, iCols), intent(in)                 :: a2dVarAlbedoS
        real(kind = 4), dimension(iRows, iCols), intent(in)                 :: a2dVarRhoS
        real(kind = 4), dimension(iRows, iCols, iDaySteps), intent(in)      :: a3dVarTaC_1Days
        real(kind = 4), dimension(iRows, iCols, iDaySteps*5), intent(in)    :: a3dVarTaC_5Days
        
        real(kind = 4), dimension(iRows, iCols), intent(in)                 :: a2dVarWSRunoff
        
        integer(kind = 4), dimension(iRows, iCols), intent(in)              :: a2iVarAgeS
        
        integer(kind = 4)       :: iErr
        
        integer(kind = 4)       :: iFileID
        integer(kind = 4)       :: iID_Dim_Rows, iID_Dim_Cols, iID_Dim_Time_DBL, iID_Dim_Time_STR
        integer(kind = 4)       :: iID_Dim_TMarkedSteps, iID_Dim_Day1Steps, iID_Dim_Day5Steps
        
        real(kind = 8)          :: dTimeHours_Diff, dTimeSeconds_Diff
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Initialize variable(s)
        sCommandZipFile = ""
        sTimeRef = '1970-01-01_00:00:00'
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Get global information
        sCommandZipFile = oHMC_Namelist(iID)%sCommandZipFile
        
        dVarXLLCorner = oHMC_Namelist(iID)%dXLLCornerL
        dVarYLLCorner = oHMC_Namelist(iID)%dYLLCornerL
        dVarCellSizeX = oHMC_Namelist(iID)%dXCellSizeL
        dVarCellSizeY = oHMC_Namelist(iID)%dYCellSizeL
        !dVarMissingValue = oHMC_Namelist(iID)%dNoDataL
        
        ! Info start
        call mprintf(.true., iINFO_Extra, ' Data :: State gridded :: NetCDF... ' )
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Define elapsed hours from reference date
        call HMC_Tools_Time_DateDiff(sTime, sTimeRef, dTimeSeconds_Diff, dTimeHours_Diff)
        !------------------------------------------------------------------------------------------

        !------------------------------------------------------------------------------------------
        ! Filename output
        sFileNameData_State = trim(sPathData_State)//"hmc.state-grid."// &
        sTime(1:4)//sTime(6:7)//sTime(9:10)// & 
        sTime(12:13)//sTime(15:16)// &
        ".nc"

        ! Info netCDF filename
        call mprintf(.true., iINFO_Verbose, ' Save filename (state gridded): '//trim(sFileNameData_State)//' ... ')
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Create netcdf file
        call check( nf90_create(trim(sFileNameData_State), NF90_NETCDF4, iFileID) )
        
        ! Dimension(s)        
        call check( nf90_def_dim(iFileID, "time", 1, iID_Dim_Time_DBL) )
        call check( nf90_def_dim(iFileID, "time_str_length", 19, iID_Dim_Time_STR) )
        call check( nf90_def_dim(iFileID, "west_east", iRows, iID_Dim_Rows) )
        call check( nf90_def_dim(iFileID, "south_north", iCols, iID_Dim_Cols) )
        call check( nf90_def_dim(iFileID, "tmarked_steps", iTMarkedSteps, iID_Dim_TMarkedSteps) )
        call check( nf90_def_dim(iFileID, "day1_steps", iDaySteps, iID_Dim_Day1Steps) )
        call check( nf90_def_dim(iFileID, "day5_steps", iDaySteps*5, iID_Dim_Day5Steps) )
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
                             iCols, iRows, transpose(a2dVarLon) )
        ! LATITUDE
        sVarName = 'Latitude'; sVarNameLong = 'latitude coordinate'; sVarDescription = 'latitude';
        sVarUnits = 'degree_north'; sVarGridMap = 'epsg:4326'; dVarMissingValue = -9E15;
        sVarCoords = '';
        call HMC_Tools_IO_Put2d_NC(iFileID, iID_Dim_Cols, iID_Dim_Rows, & 
                             sVarName, sVarNameLong, sVarDescription, &
                             sVarUnits, sVarCoords, sVarGridMap, dVarMissingValue, &
                             iCols, iRows, transpose(a2dVarLat) )
        !------------------------------------------------------------------------------------------

        !------------------------------------------------------------------------------------------
        ! Writing dynamic variable(s) in netCDF output file
        ! Total volume (V)
        sVarName = 'VTot'; sVarNameLong = 'volume_tot'; sVarDescription = 'total volume';
        sVarUnits = 'mm'; sVarGridMap = 'epsg:4326'; dVarMissingValue = -9E15;
        sVarCoords = 'Longitude Latitude';
        call HMC_Tools_IO_Put2d_NC(iFileID, iID_Dim_Cols, iID_Dim_Rows, & 
                             sVarName, sVarNameLong, sVarDescription, &
                             sVarUnits, sVarCoords, sVarGridMap, dVarMissingValue, &
                             iCols, iRows, transpose(a2dVarVTot) )
        ! Retention volume (RET)
        sVarName = 'VRet'; sVarNameLong = 'volume_retention'; sVarDescription = 'retention volume';
        sVarUnits = 'mm'; sVarGridMap = 'epsg:4326'; dVarMissingValue = -9E15;
        sVarCoords = 'Longitude Latitude';
        call HMC_Tools_IO_Put2d_NC(iFileID, iID_Dim_Cols, iID_Dim_Rows, & 
                             sVarName, sVarNameLong, sVarDescription, &
                             sVarUnits, sVarCoords, sVarGridMap, dVarMissingValue, &
                             iCols, iRows, transpose(a2dVarVRet) )
                             
        ! Channel type variable(s) 
        if(iFlagCType.eq.2) then  
            
            ! Hydro level (Wlc)
            sVarName = 'HydroLevelC'; sVarNameLong = 'hydro_level channel'; sVarDescription = 'hydro level';
            sVarUnits = 'm'; sVarGridMap = 'epsg:4326'; dVarMissingValue = -9E15;
            sVarCoords = 'Longitude Latitude';
            call HMC_Tools_IO_Put2d_NC(iFileID, iID_Dim_Cols, iID_Dim_Rows, & 
                                 sVarName, sVarNameLong, sVarDescription, &
                                 sVarUnits, sVarCoords, sVarGridMap, dVarMissingValue, &
                                 iCols, iRows, transpose(a2dVarHydroC) )
            ! Hydro level (Wlh)
            sVarName = 'HydroLevelH'; sVarNameLong = 'hydro_level hillslope'; sVarDescription = 'hydro level';
            sVarUnits = 'm'; sVarGridMap = 'epsg:4326'; dVarMissingValue = -9E15;
            sVarCoords = 'Longitude Latitude';
            call HMC_Tools_IO_Put2d_NC(iFileID, iID_Dim_Cols, iID_Dim_Rows, & 
                                 sVarName, sVarNameLong, sVarDescription, &
                                 sVarUnits, sVarCoords, sVarGridMap, dVarMissingValue, &
                                 iCols, iRows, transpose(a2dVarHydroH) )
            ! Q channel upstream (Qup)
            sVarName = 'Qup'; sVarNameLong = 'Q channel Upstream'; sVarDescription = 'hydro level';
            sVarUnits = 'm^3/s'; sVarGridMap = 'epsg:4326'; dVarMissingValue = -9E15;
            sVarCoords = 'Longitude Latitude';
            call HMC_Tools_IO_Put2d_NC(iFileID, iID_Dim_Cols, iID_Dim_Rows, & 
                                 sVarName, sVarNameLong, sVarDescription, &
                                 sVarUnits, sVarCoords, sVarGridMap, dVarMissingValue, &
                                 iCols, iRows, transpose(a2dVarQup) )
        else
            
            ! Hydro level (Wl)
            sVarName = 'HydroLevel'; sVarNameLong = 'hydro_level'; sVarDescription = 'hydro level';
            sVarUnits = 'm'; sVarGridMap = 'epsg:4326'; dVarMissingValue = -9E15;
            sVarCoords = 'Longitude Latitude';
            call HMC_Tools_IO_Put2d_NC(iFileID, iID_Dim_Cols, iID_Dim_Rows, & 
                                 sVarName, sVarNameLong, sVarDescription, &
                                 sVarUnits, sVarCoords, sVarGridMap, dVarMissingValue, &
                                 iCols, iRows, transpose(a2dVarHydro) )
        endif

        ! Routing (ROU)
        sVarName = 'Routing'; sVarNameLong = 'routing'; sVarDescription = 'routing';
        sVarUnits = 'mm'; sVarGridMap = 'epsg:4326'; dVarMissingValue = -9E15;
        sVarCoords = 'Longitude Latitude';
        call HMC_Tools_IO_Put2d_NC(iFileID, iID_Dim_Cols, iID_Dim_Rows, & 
                             sVarName, sVarNameLong, sVarDescription, &
                             sVarUnits, sVarCoords, sVarGridMap, dVarMissingValue, &
                             iCols, iRows, transpose(a2dVarRouting) )            
        ! Flow deep - exfiltration 
        sVarName = 'DFE'; sVarNameLong = 'flow_deep-exfiltration'; sVarDescription = 'flow deep - exfiltration';
        sVarUnits = ''; sVarGridMap = 'epsg:4326'; dVarMissingValue = -9E15;
        sVarCoords = 'Longitude Latitude';
        call HMC_Tools_IO_Put2d_NC(iFileID, iID_Dim_Cols, iID_Dim_Rows, & 
                             sVarName, sVarNameLong, sVarDescription, &
                             sVarUnits, sVarCoords, sVarGridMap, dVarMissingValue, &
                             iCols, iRows, transpose(a2dVarFlowDeep) )                  
        ! Water Table Level (Vw)
        sVarName = 'WTLevel'; sVarNameLong = 'watertable_level'; sVarDescription = 'watertable level';
        sVarUnits = ''; sVarGridMap = 'epsg:4326'; dVarMissingValue = -9E15;
        sVarCoords = 'Longitude Latitude';
        call HMC_Tools_IO_Put2d_NC(iFileID, iID_Dim_Cols, iID_Dim_Rows, & 
                             sVarName, sVarNameLong, sVarDescription, &
                             sVarUnits, sVarCoords, sVarGridMap, dVarMissingValue, &
                             iCols, iRows, transpose(a2dVarWTableUpd) ) 
        ! LST 
        sVarName = 'LST'; sVarNameLong = 'land_surface_temperature'; sVarDescription = 'land surface temperature';
        sVarUnits = 'K'; sVarGridMap = 'epsg:4326'; dVarMissingValue = -9E15;
        sVarCoords = 'Longitude Latitude';
        call HMC_Tools_IO_Put2d_NC(iFileID, iID_Dim_Cols, iID_Dim_Rows, & 
                             sVarName, sVarNameLong, sVarDescription, &
                             sVarUnits, sVarCoords, sVarGridMap, dVarMissingValue, &
                             iCols, iRows, transpose(a2dVarLST) )
        ! TMarked
        sVarName = 'Tmk'; sVarNameLong = 'air_temperature_marked'; sVarDescription = 'air temperature marked';
        sVarUnits = 'K'; sVarGridMap = 'epsg:4326'; dVarMissingValue = -9E15;
        dScale_Factor = 10.0;
        sVarCoords = 'Longitude Latitude';
        call HMC_Tools_IO_Put3d_NC(iFileID, iID_Dim_Cols, iID_Dim_Rows, iID_Dim_TMarkedSteps, & 
                             sVarName, sVarNameLong, sVarDescription, &
                             sVarUnits, sVarCoords, sVarGridMap, dVarMissingValue, dScale_Factor, &
                             iCols, iRows, iTMarkedSteps, transpose3Dvar(dScale_Factor * a3dVarTaKMarked)) 
        ! T24
        sVarName = 'T24'; sVarNameLong = 'air_temperature_last24'; sVarDescription = 'air temperature last 24 hours';
        sVarUnits = 'K'; sVarGridMap = 'epsg:4326'; dVarMissingValue = -9E15;
        dScale_Factor = 10.0;
        sVarCoords = 'Longitude Latitude';
        call HMC_Tools_IO_Put3d_NC(iFileID, iID_Dim_Cols, iID_Dim_Rows, iID_Dim_Day1Steps, & 
                             sVarName, sVarNameLong, sVarDescription, &
                             sVarUnits, sVarCoords, sVarGridMap, dVarMissingValue, dScale_Factor, &
                             iCols, iRows, iDaySteps, transpose3Dvar(dScale_Factor * a3dVarTaK24)) 
        ! Water sources
        sVarName = 'WS'; sVarNameLong = 'water_sources'; sVarDescription = 'water sources';
        sVarUnits = 'm^3/s'; sVarGridMap = 'epsg:4326'; dVarMissingValue = -9E15;
        sVarCoords = 'Longitude Latitude';
        call HMC_Tools_IO_Put2d_NC(iFileID, iID_Dim_Cols, iID_Dim_Rows, & 
                             sVarName, sVarNameLong, sVarDescription, &
                             sVarUnits, sVarCoords, sVarGridMap, dVarMissingValue, &
                             iCols, iRows, transpose(a2dVarWSRunoff) )
                                                  
        ! Snow variable(s)                
        if (iFlagSnow.eq.1) then
            ! SWE
            sVarName = 'SWE'; sVarNameLong = 'snow water equivalent'; sVarDescription = 'SWE';
            sVarUnits = 'mm'; sVarGridMap = 'epsg:4326'; dVarMissingValue = -9E15;
            sVarCoords = 'Longitude Latitude';
            call HMC_Tools_IO_Put2d_NC(iFileID, iID_Dim_Cols, iID_Dim_Rows, & 
                                 sVarName, sVarNameLong, sVarDescription, &
                                 sVarUnits, sVarCoords, sVarGridMap, dVarMissingValue, &
                                 iCols, iRows, transpose(a2dVarSWE))   
                                 
            ! Snow density
            sVarName = 'RhoS'; sVarNameLong = 'snow density'; sVarDescription = 'rhos';
            sVarUnits = 'kg/m^3'; sVarGridMap = 'epsg:4326'; dVarMissingValue = -9E15;
            sVarCoords = 'Longitude Latitude';
            call HMC_Tools_IO_Put2d_NC(iFileID, iID_Dim_Cols, iID_Dim_Rows, & 
                                 sVarName, sVarNameLong, sVarDescription, &
                                 sVarUnits, sVarCoords, sVarGridMap, dVarMissingValue, &
                                 iCols, iRows, transpose(a2dVarRhoS))
            
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
                                 
            ! Air Temperature last 1 day(s))
            sVarName = 'T_1Days'; sVarNameLong = 'air_temperature_last1days'; sVarDescription = 'air temperature last 1 day(s)';
            sVarUnits = 'C'; sVarGridMap = 'epsg:4326'; dVarMissingValue = -9E15;
            dScale_Factor = 10.0;
            sVarCoords = 'Longitude Latitude';
            call HMC_Tools_IO_Put3d_NC(iFileID, iID_Dim_Cols, iID_Dim_Rows, iID_Dim_Day1Steps, & 
                                 sVarName, sVarNameLong, sVarDescription, &
                                 sVarUnits, sVarCoords, sVarGridMap, dVarMissingValue, dScale_Factor, &
                                 iCols, iRows, iDaySteps, transpose3Dvar(dScale_Factor * a3dVarTaC_1Days)) 
                                 
            ! Air Temperature last 5 day(s))
            sVarName = 'T_5Days'; sVarNameLong = 'air_temperature_last5days'; sVarDescription = 'air temperature last 5 day(s)';
            sVarUnits = 'C'; sVarGridMap = 'epsg:4326'; dVarMissingValue = -9E15;
            dScale_Factor = 10.0;
            sVarCoords = 'Longitude Latitude';
            call HMC_Tools_IO_Put3d_NC(iFileID, iID_Dim_Cols, iID_Dim_Rows, iID_Dim_Day5Steps, & 
                                 sVarName, sVarNameLong, sVarDescription, &
                                 sVarUnits, sVarCoords, sVarGridMap, dVarMissingValue, dScale_Factor, &
                                 iCols, iRows, iDaySteps*5, transpose3Dvar(dScale_Factor * a3dVarTaC_5Days)) 
        endif
        !------------------------------------------------------------------------------------------
                                 
        !------------------------------------------------------------------------------------------
        ! Close
        call check( nf90_close(iFileID) )
        ! Info
        call mprintf(.true., iINFO_Verbose, ' Save filename (state gridded): '//trim(sFileNameData_State)//' ... OK')
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Zip file
        call HMC_Tools_Generic_ZipFile(sCommandZipFile, &
                                       sFileNameData_State//'.gz', sFileNameData_State, .false.)
                                       
        ! Remove un-zipped file
        !call HMC_Tools_Generic_RemoveFile(oHMC_Namelist(iID)%sCommandRemoveFile, sFileNameData_Restart, .false.)
        
        ! Info end
        call mprintf(.true., iINFO_Extra, ' Data :: State gridded :: NetCDF... OK' )
        !------------------------------------------------------------------------------------------
        
    end subroutine HMC_Data_State_Gridded_NC
#endif
    !------------------------------------------------------------------------------------------
    
    !------------------------------------------------------------------------------------------
    ! Subroutine to write binary data state
    subroutine HMC_Data_State_Gridded_Binary(iID, &
                                               sPathData_State, &
                                               iRows, iCols, &
                                               iDaySteps, iTMarkedSteps, &
                                               sTime, iFlagSnow, iFlagCType, &
                                               iVarScale, &
                                               a2dVarVTot, a2dVarVRet, &
                                               a2dVarHydro, a2dVarRouting, &
                                               a2dVarHydroC, a2dVarHydroH, a2dVarQup, &
                                               a2dVarFlowDeep, &
                                               a2dVarWTableUpd, &
                                               a2dVarLST, a3dVarTaKMarked, a3dVarTaK24, &
                                               a2dVarSWE, a2dVarAlbedoS, a2dVarRhoS, a2iVarAgeS, &
                                               a3dVarTaC_1Days, a3dVarTaC_5Days, &
                                               a2dVarWSRunoff, &
                                               a2dVarLat, a2dVarLon)

        !------------------------------------------------------------------------------------------
        ! Variable(s)
        integer(kind = 4)                       :: iID                  
                                  
        character(len = 700), intent(in)        :: sPathData_State
        character(len = 700)                    :: sFileNameData_State
        character(len = 256)                    :: sVarName
        integer(kind = 4), intent(in)           :: iRows, iCols
        integer(kind = 4)                       :: iDaySteps, iTMarkedSteps
        integer(kind = 4)                       :: iVarScale, iFlagSnow, iFlagCType

        character(len = 19), intent(in)         :: sTime

        real(kind = 4), dimension(iCols, iRows)     :: a2dVarData
        
        real(kind = 4), dimension(iRows, iCols)                  :: a2dVarVTot
        real(kind = 4), dimension(iRows, iCols)                  :: a2dVarVRet
        real(kind = 4), dimension(iRows, iCols)                  :: a2dVarHydro
        real(kind = 4), dimension(iRows, iCols)                  :: a2dVarRouting      
        real(kind = 4), dimension(iRows, iCols)                  :: a2dVarHydroC
        real(kind = 4), dimension(iRows, iCols)                  :: a2dVarHydroH
        real(kind = 4), dimension(iRows, iCols)                  :: a2dVarQup
        real(kind = 4), dimension(iRows, iCols)                  :: a2dVarFlowDeep
        real(kind = 4), dimension(iRows, iCols)                  :: a2dVarWTableUpd 
        real(kind = 4), dimension(iRows, iCols)                  :: a2dVarLST 
        real(kind = 4), dimension(iRows, iCols)                  :: a2dVarLat
        real(kind = 4), dimension(iRows, iCols)                  :: a2dVarLon
        real(kind = 4), dimension(iRows, iCols, iTMarkedSteps)   :: a3dVarTaKMarked
        real(kind = 4), dimension(iRows, iCols, iDaySteps)       :: a3dVarTaK24
        real(kind = 4), dimension(iRows, iCols)                  :: a2dVarSWE
        real(kind = 4), dimension(iRows, iCols)                  :: a2dVarAlbedoS
        real(kind = 4), dimension(iRows, iCols)                  :: a2dVarRhoS
        real(kind = 4), dimension(iRows, iCols)                  :: a2dVarWSRunoff
        integer(kind = 4), dimension(iRows, iCols)               :: a2iVarAgeS
        real(kind = 4), dimension(iRows, iCols, iDaySteps)       :: a3dVarTaC_1Days
        real(kind = 4), dimension(iRows, iCols, iDaySteps*5)     :: a3dVarTaC_5Days
       
        character(len = 256)    :: sVarUnits
        integer(kind = 4)       :: iErr
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Initialize variable(s)
        a2dVarData = 0.0
        ! Info start
        call mprintf(.true., iINFO_Extra, ' Data :: State gridded :: Binary ... ' )
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Info filename(s) at each step
        call mprintf(.true., iINFO_Verbose, ' Save (state gridded) at time '//trim(sTime)//' ... ')
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Writing dynamic variable(s) in binary output file
        ! VTot
        iVarScale = 10000
        sFileNameData_State = trim(sPathData_State)//"V_"// &
                           sTime(1:4)//sTime(6:7)//sTime(9:10)//sTime(12:13)//sTime(15:16)// &
                           ".bin"   
        call mprintf(.true., iINFO_Extra, ' Save filename: '//trim(sFileNameData_State) )
        call HMC_Tools_IO_Put2d_Binary_INT(sFileNameData_State, a2dVarVTot, iRows, iCols, iVarScale, .true., iErr)
        call HMC_Tools_Generic_ZipFile(oHMC_Namelist(iID)%sCommandZipFile, &
                                       sFileNameData_State//'.gz', sFileNameData_State, .false.)
        call HMC_Tools_Generic_RemoveFile(oHMC_Namelist(iID)%sCommandRemoveFile, sFileNameData_State, .false.)
        
        ! VRet
        iVarScale = 10000
        sFileNameData_State = trim(sPathData_State)//"Ret_"// &
                           sTime(1:4)//sTime(6:7)//sTime(9:10)//sTime(12:13)//sTime(15:16)// &
                           ".bin"    
        call mprintf(.true., iINFO_Extra, ' Save filename: '//trim(sFileNameData_State) )
        call HMC_Tools_IO_Put2d_Binary_INT(sFileNameData_State, a2dVarVRet, iRows, iCols, iVarScale, .true., iErr)
        call HMC_Tools_Generic_ZipFile(oHMC_Namelist(iID)%sCommandZipFile, &
                                       sFileNameData_State//'.gz', sFileNameData_State, .false.)
        call HMC_Tools_Generic_RemoveFile(oHMC_Namelist(iID)%sCommandRemoveFile, sFileNameData_State, .false.)
        
        ! Check channel type
        if(iFlagCType.eq.2) then  
            
            ! Hydro C
            iVarScale = 100000
            sFileNameData_State = trim(sPathData_State)//"Wlc_"// &
                               sTime(1:4)//sTime(6:7)//sTime(9:10)//sTime(12:13)//sTime(15:16)// &
                               ".bin"            
            call mprintf(.true., iINFO_Extra, ' Save filename: '//trim(sFileNameData_State) )
            call HMC_Tools_IO_Put2d_Binary_INT(sFileNameData_State, a2dVarHydroC, iRows, iCols, iVarScale, .true., iErr)
            call HMC_Tools_Generic_ZipFile(oHMC_Namelist(iID)%sCommandZipFile, &
                                           sFileNameData_State//'.gz', sFileNameData_State, .false.)
            call HMC_Tools_Generic_RemoveFile(oHMC_Namelist(iID)%sCommandRemoveFile, sFileNameData_State, .false.)

            ! Hydro H
            iVarScale = 100000
            sFileNameData_State = trim(sPathData_State)//"Wlh_"// &
                               sTime(1:4)//sTime(6:7)//sTime(9:10)//sTime(12:13)//sTime(15:16)// &
                               ".bin"            
            call mprintf(.true., iINFO_Extra, ' Save filename: '//trim(sFileNameData_State) )
            call HMC_Tools_IO_Put2d_Binary_INT(sFileNameData_State, a2dVarHydroH, iRows, iCols, iVarScale, .true., iErr)
            call HMC_Tools_Generic_ZipFile(oHMC_Namelist(iID)%sCommandZipFile, &
                                           sFileNameData_State//'.gz', sFileNameData_State, .false.)
            call HMC_Tools_Generic_RemoveFile(oHMC_Namelist(iID)%sCommandRemoveFile, sFileNameData_State, .false.)

            ! Qup
            iVarScale = 10000
            sFileNameData_State = trim(sPathData_State)//"Qup_"// &
                               sTime(1:4)//sTime(6:7)//sTime(9:10)//sTime(12:13)//sTime(15:16)// &
                               ".bin"            
            call mprintf(.true., iINFO_Extra, ' Save filename: '//trim(sFileNameData_State) )
            call HMC_Tools_IO_Put2d_Binary_INT(sFileNameData_State, a2dVarQup, iRows, iCols, iVarScale, .true., iErr)
            call HMC_Tools_Generic_ZipFile(oHMC_Namelist(iID)%sCommandZipFile, &
                                           sFileNameData_State//'.gz', sFileNameData_State, .false.)
            call HMC_Tools_Generic_RemoveFile(oHMC_Namelist(iID)%sCommandRemoveFile, sFileNameData_State, .false.)
        
        else
            ! Hydro level (Wl)
            iVarScale = 100000
            sFileNameData_State = trim(sPathData_State)//"Wl_"// &
                               sTime(1:4)//sTime(6:7)//sTime(9:10)//sTime(12:13)//sTime(15:16)// &
                               ".bin"            
            call mprintf(.true., iINFO_Extra, ' Save filename: '//trim(sFileNameData_State) )
            call HMC_Tools_IO_Put2d_Binary_INT(sFileNameData_State, a2dVarHydro, iRows, iCols, iVarScale, .true., iErr)
            call HMC_Tools_Generic_ZipFile(oHMC_Namelist(iID)%sCommandZipFile, &
                                           sFileNameData_State//'.gz', sFileNameData_State, .false.)
            call HMC_Tools_Generic_RemoveFile(oHMC_Namelist(iID)%sCommandRemoveFile, sFileNameData_State, .false.)

        endif
        
        ! Routing
        iVarScale = 100000
        sFileNameData_State = trim(sPathData_State)//"Rou_"// &
                           sTime(1:4)//sTime(6:7)//sTime(9:10)//sTime(12:13)//sTime(15:16)// &
                           ".bin"            
        call mprintf(.true., iINFO_Extra, ' Save filename: '//trim(sFileNameData_State) )
        call HMC_Tools_IO_Put2d_Binary_INT(sFileNameData_State, a2dVarRouting, iRows, iCols, iVarScale, .true., iErr)
        call HMC_Tools_Generic_ZipFile(oHMC_Namelist(iID)%sCommandZipFile, &
                                                     sFileNameData_State//'.gz', sFileNameData_State, .false.)
        call HMC_Tools_Generic_RemoveFile(oHMC_Namelist(iID)%sCommandRemoveFile, sFileNameData_State, .false.)
        
        ! DFE
        iVarScale = 10000
        sFileNameData_State = trim(sPathData_State)//"DFE_"// &
                           sTime(1:4)//sTime(6:7)//sTime(9:10)//sTime(12:13)//sTime(15:16)// &
                           ".bin"            
        call mprintf(.true., iINFO_Extra, ' Save filename: '//trim(sFileNameData_State) )
        call HMC_Tools_IO_Put2d_Binary_INT(sFileNameData_State, a2dVarFlowDeep, iRows, iCols, iVarScale, .true., iErr)
        call HMC_Tools_Generic_ZipFile(oHMC_Namelist(iID)%sCommandZipFile, &
                                       sFileNameData_State//'.gz', sFileNameData_State, .false.)
        call HMC_Tools_Generic_RemoveFile(oHMC_Namelist(iID)%sCommandRemoveFile, sFileNameData_State, .false.)
        
        ! Water-table level
        iVarScale = 10000
        sFileNameData_State = trim(sPathData_State)//"Vw_"// &
                           sTime(1:4)//sTime(6:7)//sTime(9:10)//sTime(12:13)//sTime(15:16)// &
                           ".bin"            
        call mprintf(.true., iINFO_Extra, ' Save filename: '//trim(sFileNameData_State) )
        call HMC_Tools_IO_Put2d_Binary_INT(sFileNameData_State, a2dVarWTableUpd, iRows, iCols, iVarScale, .true., iErr)
        call HMC_Tools_Generic_ZipFile(oHMC_Namelist(iID)%sCommandZipFile, &
                                       sFileNameData_State//'.gz', sFileNameData_State, .false.)
        call HMC_Tools_Generic_RemoveFile(oHMC_Namelist(iID)%sCommandRemoveFile, sFileNameData_State, .false.)
        
        ! Land surface temperature
        iVarScale = 10000
        sFileNameData_State = trim(sPathData_State)//"Ts_"// &
                           sTime(1:4)//sTime(6:7)//sTime(9:10)//sTime(12:13)//sTime(15:16)// &
                           ".bin"            
        call mprintf(.true., iINFO_Extra, ' Save filename: '//trim(sFileNameData_State) )
        call HMC_Tools_IO_Put2d_Binary_INT(sFileNameData_State, a2dVarLST, iRows, iCols, iVarScale, .true., iErr)
        call HMC_Tools_Generic_ZipFile(oHMC_Namelist(iID)%sCommandZipFile, &
                                       sFileNameData_State//'.gz', sFileNameData_State, .false.)
        call HMC_Tools_Generic_RemoveFile(oHMC_Namelist(iID)%sCommandRemoveFile, sFileNameData_State, .false.)
        
        ! TMarked
        iVarScale = 10000
        sFileNameData_State = trim(sPathData_State)//"Tmk_"// &
                           sTime(1:4)//sTime(6:7)//sTime(9:10)//sTime(12:13)//sTime(15:16)// &
                           ".bin"            
        call mprintf(.true., iINFO_Extra, ' Save filename: '//trim(sFileNameData_State) )
        call HMC_Tools_IO_Put3d_Binary(sFileNameData_State, a3dVarTaKMarked, iRows, iCols, iTMarkedSteps, iVarScale, .true., iErr)
        call HMC_Tools_Generic_ZipFile(oHMC_Namelist(iID)%sCommandZipFile, &
                                       sFileNameData_State//'.gz', sFileNameData_State, .false.)
        call HMC_Tools_Generic_RemoveFile(oHMC_Namelist(iID)%sCommandRemoveFile, sFileNameData_State, .false.)
        
        ! T24
        iVarScale = 10000
        sFileNameData_State = trim(sPathData_State)//"T24_"// &
                           sTime(1:4)//sTime(6:7)//sTime(9:10)//sTime(12:13)//sTime(15:16)// &
                           ".bin"            
        call mprintf(.true., iINFO_Extra, ' Save filename: '//trim(sFileNameData_State) )
        call HMC_Tools_IO_Put3d_Binary(sFileNameData_State, a3dVarTaK24, iRows, iCols, iDaySteps, iVarScale, .true., iErr)
        call HMC_Tools_Generic_ZipFile(oHMC_Namelist(iID)%sCommandZipFile, &
                                       sFileNameData_State//'.gz', sFileNameData_State, .false.)
        call HMC_Tools_Generic_RemoveFile(oHMC_Namelist(iID)%sCommandRemoveFile, sFileNameData_State, .false.)
        
        ! Water sources
        iVarScale = 1000000
        sFileNameData_State = trim(sPathData_State)//"WS_"// &
                           sTime(1:4)//sTime(6:7)//sTime(9:10)//sTime(12:13)//sTime(15:16)// &
                           ".bin"            
        call mprintf(.true., iINFO_Extra, ' Save filename: '//trim(sFileNameData_State) )
        call HMC_Tools_IO_Put2d_Binary_INT(sFileNameData_State, a2dVarWSRunoff, iRows, iCols, iVarScale, .true., iErr)
        call HMC_Tools_Generic_ZipFile(oHMC_Namelist(iID)%sCommandZipFile, &
                                       sFileNameData_State//'.gz', sFileNameData_State, .false.)
        call HMC_Tools_Generic_RemoveFile(oHMC_Namelist(iID)%sCommandRemoveFile, sFileNameData_State, .false.)
        
        ! Snow variable(s)
        if (iFlagSnow.eq.1) then
            
            ! SWE
            iVarScale = 1;
            sFileNameData_State = trim(sPathData_State)//"SWE_"// &
                                sTime(1:4)//sTime(6:7)//sTime(9:10)//sTime(12:13)//sTime(15:16)// &
                                ".bin"            
            call mprintf(.true., iINFO_Extra, ' Save filename: '//trim(sFileNameData_State) )
            call HMC_Tools_IO_Put2d_Binary_DBL(sFileNameData_State, a2dVarSWE, iRows, iCols, iVarScale, .true., iErr)  
            call HMC_Tools_Generic_ZipFile(oHMC_Namelist(iID)%sCommandZipFile, &
                        sFileNameData_State//'.gz', sFileNameData_State, .false.)
            !call HMC_Tools_Generic_RemoveFile(oHMC_Namelist(iID)%sCommandRemoveFile, sFileNameData_State, .false. )
            
            ! Snow density
            iVarScale = 1;
            sFileNameData_State = trim(sPathData_State)//"Density_"// &
                                sTime(1:4)//sTime(6:7)//sTime(9:10)//sTime(12:13)//sTime(15:16)// &
                                ".bin"            
            call mprintf(.true., iINFO_Extra, ' Save filename: '//trim(sFileNameData_State) )
            call HMC_Tools_IO_Put2d_Binary_DBL(sFileNameData_State, a2dVarRhoS, iRows, iCols, iVarScale, .true., iErr)  
            call HMC_Tools_Generic_ZipFile(oHMC_Namelist(iID)%sCommandZipFile, &
                        sFileNameData_State//'.gz', sFileNameData_State, .false.)
            !call HMC_Tools_Generic_RemoveFile(oHMC_Namelist(iID)%sCommandRemoveFile, sFileNameData_State, .false. )
            
            ! Snow albedo
            iVarScale = 1;
            sFileNameData_State = trim(sPathData_State)//"AlbedoS_"// &
                                sTime(1:4)//sTime(6:7)//sTime(9:10)//sTime(12:13)//sTime(15:16)// &
                                ".bin"            
            call mprintf(.true., iINFO_Extra, ' Save filename: '//trim(sFileNameData_State) )
            call HMC_Tools_IO_Put2d_Binary_DBL(sFileNameData_State, a2dVarAlbedoS, iRows, iCols, iVarScale, .true., iErr)  
            call HMC_Tools_Generic_ZipFile(oHMC_Namelist(iID)%sCommandZipFile, &
                        sFileNameData_State//'.gz', sFileNameData_State, .false.)
            !call HMC_Tools_Generic_RemoveFile(oHMC_Namelist(iID)%sCommandRemoveFile, sFileNameData_State, .false. )

            ! Snow age
            iVarScale = 1;
            sFileNameData_State = trim(sPathData_State)//"Age_"// &
                                sTime(1:4)//sTime(6:7)//sTime(9:10)//sTime(12:13)//sTime(15:16)// &
                                ".bin"            
            call mprintf(.true., iINFO_Extra, ' Save filename: '//trim(sFileNameData_State) )
            call HMC_Tools_IO_Put2d_Binary_DBL(sFileNameData_State, real(a2iVarAgeS), iRows, iCols, iVarScale, .true., iErr)  
            call HMC_Tools_Generic_ZipFile(oHMC_Namelist(iID)%sCommandZipFile, &
                        sFileNameData_State//'.gz', sFileNameData_State, .false.)
            !call HMC_Tools_Generic_RemoveFile(oHMC_Namelist(iID)%sCommandRemoveFile, sFileNameData_State, .false. )
            
            ! Air Temperature last 1 Days
            iVarScale = 1;
            sFileNameData_State = trim(sPathData_State)//"Ta_1Days_"// &
                                sTime(1:4)//sTime(6:7)//sTime(9:10)//sTime(12:13)//sTime(15:16)// &
                                ".bin"            
            call mprintf(.true., iINFO_Extra, ' Save filename: '//trim(sFileNameData_State) )
            call HMC_Tools_IO_Put3d_Binary(sFileNameData_State, a3dVarTaC_1Days, iRows, iCols, iDaySteps, iVarScale, .true., iErr)  
            call HMC_Tools_Generic_ZipFile(oHMC_Namelist(iID)%sCommandZipFile, &
                        sFileNameData_State//'.gz', sFileNameData_State, .false.)
            !call HMC_Tools_Generic_RemoveFile(oHMC_Namelist(iID)%sCommandRemoveFile, sFileNameData_State, .false. )
                        
            ! Air Temperature last 5 Days
            iVarScale = 1;
            sFileNameData_State = trim(sPathData_State)//"Ta_5Days_"// &
                                sTime(1:4)//sTime(6:7)//sTime(9:10)//sTime(12:13)//sTime(15:16)// &
                                ".bin"            
            call mprintf(.true., iINFO_Extra, ' Save filename: '//trim(sFileNameData_State) )
            call HMC_Tools_IO_Put3d_Binary(sFileNameData_State, a3dVarTaC_5Days, iRows, iCols, iDaySteps*5, iVarScale, .true., iErr)  
            call HMC_Tools_Generic_ZipFile(oHMC_Namelist(iID)%sCommandZipFile, &
                        sFileNameData_State//'.gz', sFileNameData_State, .false.)
            !call HMC_Tools_Generic_RemoveFile(oHMC_Namelist(iID)%sCommandRemoveFile, sFileNameData_State, .false. )
                         
        endif
        !------------------------------------------------------------------------------------------  

        !------------------------------------------------------------------------------------------
        ! Info filename(s) at each step
        call mprintf(.true., iINFO_Verbose, ' Save (state gridded) at time '//trim(sTime)//' ... OK')
        ! Info end
        call mprintf(.true., iINFO_Extra, ' Data :: State gridded :: Binary ... OK' )
        !------------------------------------------------------------------------------------------
                                                                               
    end subroutine HMC_Data_State_Gridded_Binary
    !------------------------------------------------------------------------------------------
    
end module HMC_Module_Data_State_Gridded
!------------------------------------------------------------------------------------------

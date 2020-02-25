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
        
        integer(kind = 4)           :: iFlagTypeData_Output, iFlagSnow, iFlagFlood
        integer(kind = 4)           :: iScaleFactor
        
        character(len = 19)         :: sTime
        character(len = 700)        :: sPathData_Output
        character(len = 700)        :: sCommandCreateFolder
               
        real(kind = 4), dimension(iRowsEnd - iRowsStart + 1, iColsEnd - iColsStart + 1)     ::  a2dVarDEM, a2dVarS
        
        real(kind = 4), dimension(iRowsEnd - iRowsStart + 1, iColsEnd - iColsStart + 1)     ::  a2dVarRain
        real(kind = 4), dimension(iRowsEnd - iRowsStart + 1, iColsEnd - iColsStart + 1)     ::  a2dVarLST, &
                                                                                                a2dVarH, a2dVarLE, & 
                                                                                                a2dVarETCum, a2dVarVTot, &
                                                                                                a2dVarETCumPot, &
                                                                                                a2dVarQout, &
                                                                                                a2dVarQdailyMax, &
                                                                                                a2dVarSM, &
                                                                                                a2dVarQfloodCRmax, & 
                                                                                                a2dVarQfloodCLmax, &
                                                                                                a2dVarLat, a2dVarLon
                                                                                                
        integer(kind = 4), dimension(iRowsEnd - iRowsStart + 1, iColsEnd - iColsStart + 1)  ::  a2iVarAgeS
        real(kind = 4), dimension(iRowsEnd - iRowsStart + 1, iColsEnd - iColsStart + 1)     ::  a2dVarSWE, &
                                                                                                a2dVarMeltingS, a2dVarAlbedoS, & 
                                                                                                a2dVarRhoS, a2dVarMeltingSDayCum, &
                                                                                                a2dVarSnowFall
                                                                                                
                                                                                                
        real(kind = 4), dimension(iRowsEnd - iRowsStart + 1, iColsEnd - iColsStart + 1, iDaySteps)      :: a3dVarQout, & 
                                                                                                           a3dVarQfloodCR, &
                                                                                                           a3dVarQfloodCL, &
                                                                                                           a3dVarET, &
                                                                                                           a3dVarETpot                                                                            
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Initialize variable(s)
        a2dVarDEM = 0.0; a2dVarS = 0.0; a2dVarLat = 0.0; a2dVarLon = 0.0 
        
        a2dVarRain = 0.0
        a2dVarLST = 0.0; a2dVarH = 0.0; a2dVarLE = 0.0; a2dVarETCum = 0.0; a2dVarETCumPot = 0.0;
        a2dVarVTot = 0.0; a2dVarQout = 0.0; a2dVarSM = 0.0
        
        a2dVarSWE = 0.0; a2iVarAgeS = 0; a2dVarMeltingS = 0.0; a2dVarAlbedoS = 0.0; a2dVarRhoS = 0.0;
        
        a3dVarQout = 0.0; a3dVarQfloodCR = 0.0; a3dVarQfloodCL = 0.0; a2dVarQdailyMax= 0.0;
        a2dVarQfloodCRmax = 0.0; a2dVarQfloodCLmax = 0.0;
        
        a3dVarET = 0.0; a3dVarETpot= 0.0;
        
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
        a2dVarVTot = oHMC_Vars(iID)%a2dVTot; a2dVarQout = oHMC_Vars(iID)%a2dQout; 

        a3dVarQout = oHMC_Vars(iID)%a3dQout     
        a3dVarQfloodCR = oHMC_Vars(iID)%a3dQfloodCR
        a3dVarQfloodCL = oHMC_Vars(iID)%a3dQfloodCL
        
        a3dVarET = oHMC_Vars(iID)%a3dET
        a3dVarETpot = oHMC_Vars(iID)%a3dETpot
        
        a2dVarDEM = oHMC_Vars(iID)%a2dDem; a2dVarS = oHMC_Vars(iID)%a2dS; 
        a2dVarLat = oHMC_Vars(iID)%a2dLat; a2dVarLon = oHMC_Vars(iID)%a2dLon; 

        ! computation 
        ! daily maximum discharge [m3/s]
        a2dVarQdailyMax = maxval(a3dVarQout, dim=3)	

        ! daily maximum right banks flooding discharge [m3/s]
        a2dVarQfloodCRmax = maxval(a3dVarQfloodCR, dim=3)	
        ! daily maximum left banks flooding discharge [m3/s]
        a2dVarQfloodCLmax = maxval(a3dVarQfloodCL, dim=3)	
        
        ! cumulated ET
        a2dVarETCum = sum(oHMC_Vars(iID)%a3dET(:,:,1:int(iDaySteps)),dim=3)
        a2dVarETCumPot = sum(oHMC_Vars(iID)%a3dETpot(:,:,1:int(iDaySteps)),dim=3)
        !set no valid cells as no data
        where (a2dVarDEM.le.0.0)
            a2dVarETCum = -9999.0
            a2dVarETCumPot = -9999.0
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
            a2dVarMeltingSDayCum = oHMC_Vars(iID)%a2dMeltingDayCum
            a2dVarAlbedoS = oHMC_Vars(iID)%a2dAlbedo_Snow 
            a2dVarRhoS = oHMC_Vars(iID)%a2dRhoS
            a2dVarSnowFall = oHMC_Vars(iID)%a2dSnowFall
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
                                    a2dVarRain, &
                                    a2dVarLST, a2dVarH, a2dVarLE, a2dVarETCum, a2dVarETCumPot, &
                                    a3dVarET, a3dVarETpot, &
                                    a2dVarVTot, a2dVarQout, a2dVarQdailyMax, a3dVarQout, &
                                    a2dVarQfloodCRmax, a2dVarQfloodCLmax, &
                                    a3dVarQfloodCR, a3dVarQfloodCL, &
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
                                    iFlagSnow, &
                                    iFlagFlood, &
                                    iScaleFactor, &
                                    a2dVarRain, &
                                    a2dVarLST, a2dVarH, a2dVarLE, a2dVarETCum, a2dVarETCumPot, &
                                    a3dVarET, a3dVarETpot, &
                                    a2dVarVTot, a2dVarQout, a2dVarQdailyMax, a3dVarQout, &
                                    a2dVarQfloodCRmax, a2dVarQfloodCLmax, &
                                    a3dVarQfloodCR, a3dVarQfloodCL, &
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
                                          a2dVarRain, &
                                          a2dVarLST, a2dVarH, a2dVarLE, a2dVarETCum, a2dVarETCumPot, &
                                          a3dVarET, a3dVarETpot, &
                                          a2dVarVTot, a2dVarQout, a2dVarQdailyMax, a3dVarQout,  &
                                          a2dVarQfloodCRmax, a2dVarQfloodCLmax, &
                                          a3dVarQfloodCR, a3dVarQfloodCL, &
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
        
        integer(kind = 4)                       :: iTime, iDaySteps
        character(len = 19)                     :: sTime, sTimeSave
        
        integer(kind = 4)                       :: iFlagSnow, iFlagFlood
        real(kind = 4)                          :: dVarMissingValue, dScale_Factor
        real(kind = 4)                          :: dVarXLLCorner, dVarYLLCorner
        real(kind = 4)                          :: dVarCellSizeX, dVarCellSizeY
        
        real(kind = 4), dimension(iRows, iCols), intent(in)         :: a2dVarRain
        real(kind = 4), dimension(iRows, iCols), intent(in)         :: a2dVarLST
        real(kind = 4), dimension(iRows, iCols), intent(in)         :: a2dVarH
        real(kind = 4), dimension(iRows, iCols), intent(in)         :: a2dVarLE
        real(kind = 4), dimension(iRows, iCols), intent(in)         :: a2dVarETCum    
        real(kind = 4), dimension(iRows, iCols), intent(in)         :: a2dVarETCumPot 
        real(kind = 4), dimension(iRows, iCols, iDaySteps), intent(in)         :: a3dVarET 
        real(kind = 4), dimension(iRows, iCols, iDaySteps), intent(in)         :: a3dVarETpot 

        real(kind = 4), dimension(iRows, iCols), intent(in)         :: a2dVarVTot       
        real(kind = 4), dimension(iRows, iCols), intent(in)         :: a2dVarQout, a2dVarQdailyMax 
        real(kind = 4), dimension(iRows, iCols), intent(in)         :: a2dVarQfloodCRmax, a2dVarQfloodCLmax
        real(kind = 4), dimension(iRows, iCols, iDaySteps), intent(in)         :: a3dVarQout 
        real(kind = 4), dimension(iRows, iCols, iDaySteps), intent(in)         :: a3dVarQfloodCR 
        real(kind = 4), dimension(iRows, iCols, iDaySteps), intent(in)         :: a3dVarQfloodCL 

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
        integer(kind = 4)       :: iID_Dim_Rows, iID_Dim_Cols, iID_Dim_Time
        integer(kind = 4)       :: iID_Dim_Day1Steps
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Initialize variable(s)
        sCommandZipFile = ""
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
        call check( nf90_def_dim(iFileID, "time", NF90_UNLIMITED, iID_Dim_Time) )
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
        ! Soil Moisture 
        sVarName = 'SM'; sVarNameLong = 'soil_moisture'; sVarDescription = 'soil moisture';
        sVarUnits = '-'; sVarGridMap = 'epsg:4326'; dVarMissingValue = -9E15;
        sVarCoords = 'Longitude Latitude';
        call HMC_Tools_IO_Put2d_NC(iFileID, iID_Dim_Cols, iID_Dim_Rows, & 
                             sVarName, sVarNameLong, sVarDescription, &
                             sVarUnits, sVarCoords, sVarGridMap, dVarMissingValue, &
                             iCols, iRows, transpose(a2dVarSM))  
        !------------------------------------------------------------------------------------------      
                             
!        !------------------------------------------------------------------------------------------     
!        ! 3D ET (Distributed evapotranspiration values)
!        sVarName = '3D ET'; sVarNameLong = 'Time Step Evapotranspiration'; 
!        sVarDescription = 'Evapotranspiration at each time step of the previous 24H (saving time included)';
!        sVarUnits = 'mm/h'; sVarGridMap = 'epsg:4326'; dVarMissingValue = -9E15;
!        dScale_Factor = 1000.0;
!        sVarCoords = 'Longitude Latitude';
!        call HMC_Tools_IO_Put3d_NC(iFileID, iID_Dim_Cols, iID_Dim_Rows, iID_Dim_Day1Steps, & 
!                             sVarName, sVarNameLong, sVarDescription, &
!                             sVarUnits, sVarCoords, sVarGridMap, dVarMissingValue, dScale_Factor, &
!                             iCols, iRows, iDaySteps, transpose3Dvar(dScale_Factor * a3dVarET))    
!        
!        ! 3D ET pot (Distributed potential evapotranspiration values)
!        sVarName = '3D ETpot'; sVarNameLong = 'Time Step Potential Evapotranspiration'; 
!        sVarDescription = 'Potential Evapotranspiration at each time step of the previous 24H (saving time included)';
!        sVarUnits = 'mm/h'; sVarGridMap = 'epsg:4326'; dVarMissingValue = -9E15;
!        dScale_Factor = 1000.0;
!        sVarCoords = 'Longitude Latitude';
!        call HMC_Tools_IO_Put3d_NC(iFileID, iID_Dim_Cols, iID_Dim_Rows, iID_Dim_Day1Steps, & 
!                             sVarName, sVarNameLong, sVarDescription, &
!                             sVarUnits, sVarCoords, sVarGridMap, dVarMissingValue, dScale_Factor, &
!                             iCols, iRows, iDaySteps, transpose3Dvar(dScale_Factor * a3dVarETpot))  
!        
!        ! 3D Qout (Distributed discharge values)
!        sVarName = '3D Discharge'; sVarNameLong = 'Time Step discharge'; 
!        sVarDescription = 'discharge at each time step of the previous 24H (saving time included)';
!        sVarUnits = 'm^3/s'; sVarGridMap = 'epsg:4326'; dVarMissingValue = -9E15;
!        dScale_Factor = 1000.0;
!        sVarCoords = 'Longitude Latitude';
!        call HMC_Tools_IO_Put3d_NC(iFileID, iID_Dim_Cols, iID_Dim_Rows, iID_Dim_Day1Steps, & 
!                             sVarName, sVarNameLong, sVarDescription, &
!                             sVarUnits, sVarCoords, sVarGridMap, dVarMissingValue, dScale_Factor, &
!                             iCols, iRows, iDaySteps, transpose3Dvar(dScale_Factor * a3dVarQout)) 
!        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Flooding variable(s)                
        if (iFlagFlood.eq.1) then

            ! 3D Q flooding right banks (Distributed discharge values)
            sVarName = '3D Qrfl'; sVarNameLong = 'Time Step Right Banks Flood Discharges'; 
            sVarDescription = 'discharge at each time step of the previous 24H (saving time included)';
            sVarUnits = 'm^3/s'; sVarGridMap = 'epsg:4326'; dVarMissingValue = -9E15;
            dScale_Factor = 1000.0;
            sVarCoords = 'Longitude Latitude';
            call HMC_Tools_IO_Put3d_NC(iFileID, iID_Dim_Cols, iID_Dim_Rows, iID_Dim_Day1Steps, & 
                                 sVarName, sVarNameLong, sVarDescription, &
                                 sVarUnits, sVarCoords, sVarGridMap, dVarMissingValue, dScale_Factor, &
                                 iCols, iRows, iDaySteps, transpose3Dvar(dScale_Factor * a3dVarQfloodCR))    

            ! 3D Q flooding left banks (Distributed discharge values)
            sVarName = '3D Qlfl'; sVarNameLong = 'Time Step Left Banks Flood Discharges'; &
            sVarDescription = 'discharge at each time step of the previous 24H (saving time included)';
            sVarUnits = 'm^3/s'; sVarGridMap = 'epsg:4326'; dVarMissingValue = -9E15;
            dScale_Factor = 1000.0;
            sVarCoords = 'Longitude Latitude';
            call HMC_Tools_IO_Put3d_NC(iFileID, iID_Dim_Cols, iID_Dim_Rows, iID_Dim_Day1Steps, & 
                                 sVarName, sVarNameLong, sVarDescription, &
                                 sVarUnits, sVarCoords, sVarGridMap, dVarMissingValue, dScale_Factor, &
                                 iCols, iRows, iDaySteps, transpose3Dvar(dScale_Factor * a3dVarQfloodCL))      
        endif
        !------------------------------------------------------------------------------------------           
        
        !------------------------------------------------------------------------------------------                      
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
            
            sVarName = 'SnowFall'; sVarNameLong = 'snowfall'; sVarDescription = 'snowfall';
            sVarUnits = 'mm'; sVarGridMap = 'epsg:4326'; dVarMissingValue = -9E15;
            sVarCoords = 'Longitude Latitude';
            call HMC_Tools_IO_Put2d_NC(iFileID, iID_Dim_Cols, iID_Dim_Rows, & 
                                 sVarName, sVarNameLong, sVarDescription, &
                                 sVarUnits, sVarCoords, sVarGridMap, dVarMissingValue, &
                                 iCols, iRows, transpose(a2dVarSnowFall))
                                 
            ! Daily variable(s)     
            if( sTime(12:13) .eq. '23' ) then
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
                                    iFlagSnow, &
                                    iFlagFlood, &
                                    iVarScale, &
                                    a2dVarRain, &
                                    a2dVarLST, a2dVarH, a2dVarLE, a2dVarETCum, a2dVarETCumPot, &
                                    a3dVarET, a3dVarETpot, &
                                    a2dVarVTot, a2dVarQout, a2dVarQdailyMax, a3dVarQout, &
                                    a2dVarQfloodCRmax, a2dVarQfloodCLmax, &
                                    a3dVarQfloodCR, a3dVarQfloodCL, &
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
        integer(kind = 4)                       :: iVarScale, iFlagSnow, iFlagFlood

        integer(kind = 4)                       :: iTime, iDaySteps
        character(len = 19), intent(in)         :: sTime
        
        real(kind = 4), dimension(iCols, iRows)         :: a2dVarData
        
        real(kind = 4), dimension(iRows, iCols)         :: a2dVarRain
        real(kind = 4), dimension(iRows, iCols)         :: a2dVarLST
        real(kind = 4), dimension(iRows, iCols)         :: a2dVarH
        real(kind = 4), dimension(iRows, iCols)         :: a2dVarLE
        real(kind = 4), dimension(iRows, iCols)         :: a2dVarETCum 
        real(kind = 4), dimension(iRows, iCols)         :: a2dVarETCumPot 
        real(kind = 4), dimension(iRows, iCols, iDaySteps)       :: a3dVarET
        real(kind = 4), dimension(iRows, iCols, iDaySteps)       :: a3dVarETpot
        
        real(kind = 4), dimension(iRows, iCols)         :: a2dVarVTot       
        real(kind = 4), dimension(iRows, iCols)         :: a2dVarQout, a2dVarQdailyMax 
        real(kind = 4), dimension(iRows, iCols)         :: a2dVarQfloodCRmax, a2dVarQfloodCLmax 
        real(kind = 4), dimension(iRows, iCols, iDaySteps)       :: a3dVarQout
        real(kind = 4), dimension(iRows, iCols, iDaySteps)       :: a3dVarQfloodCL
        real(kind = 4), dimension(iRows, iCols, iDaySteps)       :: a3dVarQfloodCR

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
                                          
        ! ETCum
        if( (iTime .ge. iDaySteps) ) then
            iVarScale = 10000
            sFileNameData_Output = trim(sPathData_Output)//"ETCum_"// &
                               sTime(1:4)//sTime(6:7)//sTime(9:10)//sTime(12:13)//sTime(15:16)// &
                               ".bin"            
            call mprintf(.true., iINFO_Extra, ' Save filename: '//trim(sFileNameData_Output) )
            call HMC_Tools_IO_Put2d_Binary_INT(sFileNameData_Output, a2dVarETCum, iRows, iCols, iVarScale, .true., iErr)
            call HMC_Tools_Generic_ZipFile(oHMC_Namelist(iID)%sCommandZipFile, &
                                           sFileNameData_Output//'.gz', sFileNameData_Output, .false.)
                                           
            sFileNameData_Output = trim(sPathData_Output)//"ETCumPot_"// &
                               sTime(1:4)//sTime(6:7)//sTime(9:10)//sTime(12:13)//sTime(15:16)// &
                               ".bin"            
            call mprintf(.true., iINFO_Extra, ' Save filename: '//trim(sFileNameData_Output) )
            call HMC_Tools_IO_Put2d_Binary_INT(sFileNameData_Output, a2dVarETCumPot, iRows, iCols, iVarScale, .true., iErr)
            call HMC_Tools_Generic_ZipFile(oHMC_Namelist(iID)%sCommandZipFile, &
                                           sFileNameData_Output//'.gz', sFileNameData_Output, .false.)
            !call HMC_Tools_Generic_RemoveFile(oHMC_Namelist(iID)%sCommandRemoveFile, sFileNameData_Output, .false.)
        endif
     
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

!        !saving Qmax ober the previous daily interval
!        if( (iTime .ge. iDaySteps) ) then  
!            iVarScale = 10000
!            sFileNameData_Output = trim(sPathData_Output)//"Q_max_daily"// &
!                               sTime(1:4)//sTime(6:7)//sTime(9:10)//sTime(12:13)//sTime(15:16)// &
!                               ".bin"            
!            call mprintf(.true., iINFO_Extra, ' Save filename: '//trim(sFileNameData_Output) )
!            call HMC_Tools_IO_Put2d_Binary_INT(sFileNameData_Output, a2dVarQdailyMax, iRows, iCols, iVarScale, .true., iErr)  
!            call HMC_Tools_Generic_ZipFile(oHMC_Namelist(iID)%sCommandZipFile, &
!                                           sFileNameData_Output//'.gz', sFileNameData_Output, .false.)            
!            
!            ! Flooding variable(s)                
!            if (iFlagFlood.eq.1) then
!                sFileNameData_Output = trim(sPathData_Output)//"Qrfl_max_daily"// &
!                                   sTime(1:4)//sTime(6:7)//sTime(9:10)//sTime(12:13)//sTime(15:16)// &
!                                   ".bin"            
!                call mprintf(.true., iINFO_Extra, ' Save filename: '//trim(sFileNameData_Output) )
!                call HMC_Tools_IO_Put2d_Binary_INT(sFileNameData_Output, a2dVarQfloodCRmax, iRows, iCols, iVarScale, .true., iErr)  
!                call HMC_Tools_Generic_ZipFile(oHMC_Namelist(iID)%sCommandZipFile, &
!                                               sFileNameData_Output//'.gz', sFileNameData_Output, .false.)                    
!                
!                sFileNameData_Output = trim(sPathData_Output)//"Qlfl_max_daily"// &
!                                   sTime(1:4)//sTime(6:7)//sTime(9:10)//sTime(12:13)//sTime(15:16)// &
!                                   ".bin"            
!                call mprintf(.true., iINFO_Extra, ' Save filename: '//trim(sFileNameData_Output) )
!                call HMC_Tools_IO_Put2d_Binary_INT(sFileNameData_Output, a2dVarQfloodCLmax, iRows, iCols, iVarScale, .true., iErr)  
!                call HMC_Tools_Generic_ZipFile(oHMC_Namelist(iID)%sCommandZipFile, &
!                                               sFileNameData_Output//'.gz', sFileNameData_Output, .false.)   
!            endif
!        endif
                
        !------------------------------------------------------------------------------------------

!        !------------------------------------------------------------------------------------------
!        ! 3D ET (Distributed evapotranspiration values)
!        iVarScale = 10000
!        sFileNameData_Output = trim(sPathData_Output)//"3D_ET_"// &
!                           sTime(1:4)//sTime(6:7)//sTime(9:10)//sTime(12:13)//sTime(15:16)// &
!                           ".bin"            
!        call mprintf(.true., iINFO_Extra, ' Save filename: '//trim(sFileNameData_Output) )
!        call HMC_Tools_IO_Put3d_Binary(sFileNameData_Output, a3dVarET, iRows, iCols, iDaySteps, iVarScale, .true., iErr)  
!        call HMC_Tools_Generic_ZipFile(oHMC_Namelist(iID)%sCommandZipFile, &
!                                       sFileNameData_Output//'.gz', sFileNameData_Output, .false.)
!        
!        ! 3D ET POT (Distributed potential evapotranspiration values)
!        iVarScale = 10000
!        sFileNameData_Output = trim(sPathData_Output)//"3D_ETpot_"// &
!                           sTime(1:4)//sTime(6:7)//sTime(9:10)//sTime(12:13)//sTime(15:16)// &
!                           ".bin"            
!        call mprintf(.true., iINFO_Extra, ' Save filename: '//trim(sFileNameData_Output) )
!        call HMC_Tools_IO_Put3d_Binary(sFileNameData_Output, a3dVarETpot, iRows, iCols, iDaySteps, iVarScale, .true., iErr)  
!        call HMC_Tools_Generic_ZipFile(oHMC_Namelist(iID)%sCommandZipFile, &
!                                       sFileNameData_Output//'.gz', sFileNameData_Output, .false.)   
!                                       
!        ! 3D Qout (Distributed discharge values)
!        iVarScale = 10000
!        sFileNameData_Output = trim(sPathData_Output)//"3D_Q_"// &
!                           sTime(1:4)//sTime(6:7)//sTime(9:10)//sTime(12:13)//sTime(15:16)// &
!                           ".bin"            
!        call mprintf(.true., iINFO_Extra, ' Save filename: '//trim(sFileNameData_Output) )
!        call HMC_Tools_IO_Put3d_Binary(sFileNameData_Output, a3dVarQout, iRows, iCols, iDaySteps, iVarScale, .true., iErr)  
!        call HMC_Tools_Generic_ZipFile(oHMC_Namelist(iID)%sCommandZipFile, &
!                                       sFileNameData_Output//'.gz', sFileNameData_Output, .false.)
!        !------------------------------------------------------------------------------------------

        ! Flooding variable(s)                
        if (iFlagFlood.eq.1) then
            ! 3D Q flooding right banks (Distributed discharge values)
            iVarScale = 10000
            sFileNameData_Output = trim(sPathData_Output)//"3D_Qfr_"// &
                               sTime(1:4)//sTime(6:7)//sTime(9:10)//sTime(12:13)//sTime(15:16)// &
                               ".bin"            
            call mprintf(.true., iINFO_Extra, ' Save filename: '//trim(sFileNameData_Output) )
            call HMC_Tools_IO_Put3d_Binary(sFileNameData_Output, a3dVarQfloodCR, iRows, &
                                            iCols, iDaySteps, iVarScale, .true., iErr)  
            call HMC_Tools_Generic_ZipFile(oHMC_Namelist(iID)%sCommandZipFile, &
                                            sFileNameData_Output//'.gz', sFileNameData_Output, .false.)

            ! 3D Q flooding left banks (Distributed discharge values)
            iVarScale = 10000
            sFileNameData_Output = trim(sPathData_Output)//"3D_Qfl_"// &
                               sTime(1:4)//sTime(6:7)//sTime(9:10)//sTime(12:13)//sTime(15:16)// &
                               ".bin"            
            call mprintf(.true., iINFO_Extra, ' Save filename: '//trim(sFileNameData_Output) )
            call HMC_Tools_IO_Put3d_Binary(sFileNameData_Output, a3dVarQfloodCL, iRows, &
                                            iCols, iDaySteps, iVarScale, .true., iErr)  
            call HMC_Tools_Generic_ZipFile(oHMC_Namelist(iID)%sCommandZipFile, &
                                            sFileNameData_Output//'.gz', sFileNameData_Output, .false.)
        endif                                       
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Snow variable(s)
        if (iFlagSnow.eq.1) then
            
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
            if(sTime(12:13) .eq. '23') then
            
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

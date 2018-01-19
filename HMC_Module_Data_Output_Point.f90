!------------------------------------------------------------------------------------------    
! File:   HMC_Module_Data_Output_Point.f90
! Author(s): Fabio Delogu, Francesco Silvestro, Simone Gabellani
!
! Created on May 7, 2015, 4:37 PM
!------------------------------------------------------------------------------------------

!------------------------------------------------------------------------------------------
! Module Header
module HMC_Module_Data_Output_Point
    
    !------------------------------------------------------------------------------------------
    ! External module(s) for all subroutine in this module
#ifdef LIB_NC
    use netcdf
#endif
    
    use HMC_Module_Namelist,        only:   oHMC_Namelist
    use HMC_Module_Vars_Loader,     only:   oHMC_Vars
    
    use HMC_Module_Tools_Debug

#ifdef LIB_NC  
    use HMC_Module_Tools_IO,        only:   HMC_Tools_IO_Put1d_ASCII, &
                                            HMC_Tools_IO_Put2d_ASCII, &
                                            HMC_Tools_IO_Put1d_NC, &
                                            HMC_Tools_IO_Put2d_NC, &
                                            check
#else
    use HMC_Module_Tools_IO,        only:   HMC_Tools_IO_Put1d_ASCII, &
                                            HMC_Tools_IO_Put2d_ASCII                                      
#endif

    use HMC_Module_Tools_Generic,   only:   HMC_Tools_Generic_ReplaceText, & 
                                            HMC_Tools_Generic_SetUnit, &
                                            HMC_Tools_Generic_CreateFolder, &
                                            HMC_Tools_Generic_ZipFile, &
                                            HMC_Tools_Generic_RemoveFile, &
                                            mean2Dvar, max2Dvar, min2Dvar
    
    ! Implicit none for all subroutines in this module
    implicit none
    !------------------------------------------------------------------------------------------
    
contains
    
    !------------------------------------------------------------------------------------------
    ! Subroutine to manage output point data
    subroutine HMC_Data_Output_Point_Cpl( iID, sTime, &
                                          iRowsStart, iRowsEnd, iColsStart, iColsEnd, &
                                          iTime, iETime, &
                                          iNSection, iNData, &
                                          iNLake, iNDam, iNPlant, iNJoint, iNCatch, iNRelease)
        
    !------------------------------------------------------------------------------------------
                                          
        !------------------------------------------------------------------------------------------
        ! Variable(s)
        integer(kind = 4), parameter    :: iNVar = 20, iNAnalysis = 3
                                          
        integer(kind = 4)               :: iID
        integer(kind = 4)               :: iRows, iCols
        integer(kind = 4)               :: iRowsStart, iRowsEnd, iColsStart, iColsEnd
        integer(kind = 4)               :: iTime, iETime
        integer(kind = 4)               :: iNSection, iNData
        integer(kind = 4)               :: iNLake, iNDam, iNPlant, iNJoint, iNCatch, iNRelease
        
        integer(kind = 4)               :: iFlagTypeData_Output
        integer(kind = 4)               :: iScaleFactor
        
        character(len = 19)             :: sTime
        character(len = 700)            :: sPathData_Output
        character(len = 700)            :: sCommandCreateFolder
        
        real(kind = 4), dimension(iNSection)            :: a1dVarQoutSection
        
        real(kind = 4), dimension(iNDam)                :: a1dVarVDam, a1dVarHDam
        
        real(kind = 4), dimension(iNVar, iNAnalysis)    :: a2dVarAnalysisValue
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Initialize variable(s)
        a1dVarVDam = -9999.0; a1dVarHDam= -9999.0; a1dVarQoutSection = -9999.0
        a2dVarAnalysisValue = -9999.0; 
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Defining iRows and iCols (output data)
        iRows = iRowsEnd - iRowsStart + 1
        iCols = iColsEnd - iColsStart + 1
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Get global information
        sPathData_Output = oHMC_Namelist(iID)%sPathData_Output_Point
        iFlagTypeData_Output = oHMC_Namelist(iID)%iFlagTypeData_Output_Point
        sCommandCreateFolder = oHMC_Namelist(iID)%sCommandCreateFolder
        ! Info start
        call mprintf(.true., iINFO_Extra, ' Data :: Output point ... ' )
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
        ! Get variable(s) from global workspace
        
        ! MEAN VARIABLE(S) VALUE(S)
        ! Get Forcing variable(s)
        a2dVarAnalysisValue(1, 1) = mean2Dvar(oHMC_Vars(iID)%a2dRain,          oHMC_Vars(iID)%a2iMask) 
        a2dVarAnalysisValue(2, 1) = mean2Dvar(oHMC_Vars(iID)%a2dTa,            oHMC_Vars(iID)%a2iMask) 
        a2dVarAnalysisValue(3, 1) = mean2Dvar(oHMC_Vars(iID)%a2dK,             oHMC_Vars(iID)%a2iMask) 
        a2dVarAnalysisValue(4, 1) = mean2Dvar(oHMC_Vars(iID)%a2dW,             oHMC_Vars(iID)%a2iMask) 
        a2dVarAnalysisValue(5, 1) = mean2Dvar(oHMC_Vars(iID)%a2dRHum,          oHMC_Vars(iID)%a2iMask) 
        a2dVarAnalysisValue(6, 1) = mean2Dvar(oHMC_Vars(iID)%a2dPres,          oHMC_Vars(iID)%a2iMask) 
        a2dVarAnalysisValue(7, 1) = mean2Dvar(oHMC_Vars(iID)%a2dLAI,           oHMC_Vars(iID)%a2iMask) 
        a2dVarAnalysisValue(8, 1) = mean2Dvar(oHMC_Vars(iID)%a2dAlbedo,        oHMC_Vars(iID)%a2iMask) 
        ! Get Outcome variable(s)
        a2dVarAnalysisValue(9, 1) = mean2Dvar(oHMC_Vars(iID)%a2dLST,           oHMC_Vars(iID)%a2iMask) 
        a2dVarAnalysisValue(10, 1) = mean2Dvar(oHMC_Vars(iID)%a2dH,            oHMC_Vars(iID)%a2iMask) 
        a2dVarAnalysisValue(11, 1) = mean2Dvar(oHMC_Vars(iID)%a2dLE,           oHMC_Vars(iID)%a2iMask) 
        a2dVarAnalysisValue(12, 1) = mean2Dvar(oHMC_Vars(iID)%a2dET,           oHMC_Vars(iID)%a2iMask) 
        a2dVarAnalysisValue(13, 1) = mean2Dvar(oHMC_Vars(iID)%a2dIntensity,    oHMC_Vars(iID)%a2iMask) 
        a2dVarAnalysisValue(14, 1) = mean2Dvar(oHMC_Vars(iID)%a2dVTot,         oHMC_Vars(iID)%a2iMask) 
        a2dVarAnalysisValue(15, 1) = mean2Dvar(oHMC_Vars(iID)%a2dVRet,         oHMC_Vars(iID)%a2iMask) 
        a2dVarAnalysisValue(16, 1) = mean2Dvar(oHMC_Vars(iID)%a2dVSub,         oHMC_Vars(iID)%a2iMask) 
        a2dVarAnalysisValue(17, 1) = mean2Dvar(oHMC_Vars(iID)%a2dVLoss,        oHMC_Vars(iID)%a2iMask) 
        a2dVarAnalysisValue(18, 1) = mean2Dvar(oHMC_Vars(iID)%a2dVExf,         oHMC_Vars(iID)%a2iMask) 
        a2dVarAnalysisValue(19, 1) = mean2Dvar(oHMC_Vars(iID)%a2dFlowDeep,     oHMC_Vars(iID)%a2iMask) 
        a2dVarAnalysisValue(20, 1) = mean2Dvar(oHMC_Vars(iID)%a2dWTable,       oHMC_Vars(iID)%a2iMask)
        
        ! MAXIMUM VARIABLE(S) VALUE(S)
        ! Get Forcing variable(s)
        a2dVarAnalysisValue(1, 2) = max2Dvar(oHMC_Vars(iID)%a2dRain,          oHMC_Vars(iID)%a2iMask) 
        a2dVarAnalysisValue(2, 2) = max2Dvar(oHMC_Vars(iID)%a2dTa,            oHMC_Vars(iID)%a2iMask) 
        a2dVarAnalysisValue(3, 2) = max2Dvar(oHMC_Vars(iID)%a2dK,             oHMC_Vars(iID)%a2iMask) 
        a2dVarAnalysisValue(4, 2) = max2Dvar(oHMC_Vars(iID)%a2dW,             oHMC_Vars(iID)%a2iMask) 
        a2dVarAnalysisValue(5, 2) = max2Dvar(oHMC_Vars(iID)%a2dRHum,          oHMC_Vars(iID)%a2iMask) 
        a2dVarAnalysisValue(6, 2) = max2Dvar(oHMC_Vars(iID)%a2dPres,          oHMC_Vars(iID)%a2iMask) 
        a2dVarAnalysisValue(7, 2) = max2Dvar(oHMC_Vars(iID)%a2dLAI,           oHMC_Vars(iID)%a2iMask) 
        a2dVarAnalysisValue(8, 2) = max2Dvar(oHMC_Vars(iID)%a2dAlbedo,        oHMC_Vars(iID)%a2iMask) 
        ! Get Outcome variable(s)
        a2dVarAnalysisValue(9, 2) = max2Dvar(oHMC_Vars(iID)%a2dLST,           oHMC_Vars(iID)%a2iMask) 
        a2dVarAnalysisValue(10, 2) = max2Dvar(oHMC_Vars(iID)%a2dH,            oHMC_Vars(iID)%a2iMask) 
        a2dVarAnalysisValue(11, 2) = max2Dvar(oHMC_Vars(iID)%a2dLE,           oHMC_Vars(iID)%a2iMask) 
        a2dVarAnalysisValue(12, 2) = max2Dvar(oHMC_Vars(iID)%a2dET,           oHMC_Vars(iID)%a2iMask) 
        a2dVarAnalysisValue(13, 2) = max2Dvar(oHMC_Vars(iID)%a2dIntensity,    oHMC_Vars(iID)%a2iMask) 
        a2dVarAnalysisValue(14, 2) = max2Dvar(oHMC_Vars(iID)%a2dVTot,         oHMC_Vars(iID)%a2iMask) 
        a2dVarAnalysisValue(15, 2) = max2Dvar(oHMC_Vars(iID)%a2dVRet,         oHMC_Vars(iID)%a2iMask) 
        a2dVarAnalysisValue(16, 2) = max2Dvar(oHMC_Vars(iID)%a2dVSub,         oHMC_Vars(iID)%a2iMask) 
        a2dVarAnalysisValue(17, 2) = max2Dvar(oHMC_Vars(iID)%a2dVLoss,        oHMC_Vars(iID)%a2iMask) 
        a2dVarAnalysisValue(18, 2) = max2Dvar(oHMC_Vars(iID)%a2dVExf,         oHMC_Vars(iID)%a2iMask) 
        a2dVarAnalysisValue(19, 2) = max2Dvar(oHMC_Vars(iID)%a2dFlowDeep,     oHMC_Vars(iID)%a2iMask) 
        a2dVarAnalysisValue(20, 2) = max2Dvar(oHMC_Vars(iID)%a2dWTable,       oHMC_Vars(iID)%a2iMask)
       
        ! MINIMUM VARIABLE(S) VALUE(S)
        ! Get Forcing variable(s)
        a2dVarAnalysisValue(1, 3) = min2Dvar(oHMC_Vars(iID)%a2dRain,          oHMC_Vars(iID)%a2iMask) 
        a2dVarAnalysisValue(2, 3) = min2Dvar(oHMC_Vars(iID)%a2dTa,            oHMC_Vars(iID)%a2iMask) 
        a2dVarAnalysisValue(3, 3) = min2Dvar(oHMC_Vars(iID)%a2dK,             oHMC_Vars(iID)%a2iMask) 
        a2dVarAnalysisValue(4, 3) = min2Dvar(oHMC_Vars(iID)%a2dW,             oHMC_Vars(iID)%a2iMask) 
        a2dVarAnalysisValue(5, 3) = min2Dvar(oHMC_Vars(iID)%a2dRHum,          oHMC_Vars(iID)%a2iMask) 
        a2dVarAnalysisValue(6, 3) = min2Dvar(oHMC_Vars(iID)%a2dPres,          oHMC_Vars(iID)%a2iMask) 
        a2dVarAnalysisValue(7, 3) = min2Dvar(oHMC_Vars(iID)%a2dLAI,           oHMC_Vars(iID)%a2iMask) 
        a2dVarAnalysisValue(8, 3) = min2Dvar(oHMC_Vars(iID)%a2dAlbedo,        oHMC_Vars(iID)%a2iMask) 
        ! Get Outcome variable(s)
        a2dVarAnalysisValue(9, 3) = min2Dvar(oHMC_Vars(iID)%a2dLST,           oHMC_Vars(iID)%a2iMask) 
        a2dVarAnalysisValue(10, 3) = min2Dvar(oHMC_Vars(iID)%a2dH,            oHMC_Vars(iID)%a2iMask) 
        a2dVarAnalysisValue(11, 3) = min2Dvar(oHMC_Vars(iID)%a2dLE,           oHMC_Vars(iID)%a2iMask) 
        a2dVarAnalysisValue(12, 3) = min2Dvar(oHMC_Vars(iID)%a2dET,           oHMC_Vars(iID)%a2iMask) 
        a2dVarAnalysisValue(13, 3) = min2Dvar(oHMC_Vars(iID)%a2dIntensity,    oHMC_Vars(iID)%a2iMask) 
        a2dVarAnalysisValue(14, 3) = min2Dvar(oHMC_Vars(iID)%a2dVTot,         oHMC_Vars(iID)%a2iMask) 
        a2dVarAnalysisValue(15, 3) = min2Dvar(oHMC_Vars(iID)%a2dVRet,         oHMC_Vars(iID)%a2iMask) 
        a2dVarAnalysisValue(16, 3) = min2Dvar(oHMC_Vars(iID)%a2dVSub,         oHMC_Vars(iID)%a2iMask) 
        a2dVarAnalysisValue(17, 3) = min2Dvar(oHMC_Vars(iID)%a2dVLoss,        oHMC_Vars(iID)%a2iMask) 
        a2dVarAnalysisValue(18, 3) = min2Dvar(oHMC_Vars(iID)%a2dVExf,         oHMC_Vars(iID)%a2iMask) 
        a2dVarAnalysisValue(19, 3) = min2Dvar(oHMC_Vars(iID)%a2dFlowDeep,     oHMC_Vars(iID)%a2iMask) 
        a2dVarAnalysisValue(20, 3) = min2Dvar(oHMC_Vars(iID)%a2dWTable,       oHMC_Vars(iID)%a2iMask)
        
        ! Get discharge in outlet section(s)
        a1dVarQoutSection = oHMC_Vars(iID)%a1dQoutSection
        ! Get volume, level amd discharge in dam section(s)
        a1dVarVDam = oHMC_Vars(iID)%a1dVDam 
        a1dVarHDam = oHMC_Vars(iID)%a1dHDam 
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Subroutine for writing sequential netCDF output data 
        if (iFlagTypeData_Output == 2) then
                        
            !------------------------------------------------------------------------------------------
            ! Call subroutine to write point data
#ifdef LIB_NC
            call HMC_Data_Output_Point_NC(iID, &
                                          sPathData_Output, &
                                          iNSection, iNData, iNVar, iNAnalysis, &
                                          iNLake, iNDam, iNPlant, iNJoint, iNCatch, iNRelease, &
                                          sTime, &
                                          a1dVarQoutSection, a1dVarVDam, a1dVarHDam, &
                                          a2dVarAnalysisValue)
#else
            ! Redefinition of output data flag (if netCDF library is not linked)
            iFlagTypeData_Output = 1  
            call mprintf(.true., iWARN, ' ATTENTION: '// &
                                            'output point data type selected was netCDF but library is not linked! '// &
                                            'Will be used data in ASCII format!')
#endif
            !------------------------------------------------------------------------------------------
            
        endif
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Subroutine for writing sequential binary output data 
        if (iFlagTypeData_Output == 1) then
                        
            !------------------------------------------------------------------------------------------
            ! Call subroutine to write point data
            call HMC_Data_Output_Point_ASCII(iID, &
                                           sPathData_Output, &
                                           iNSection, iNData, iNVar, iNAnalysis, &
                                           iNLake, iNDam, iNPlant, iNJoint, iNCatch, iNRelease, &
                                           sTime, &
                                           a1dVarQoutSection, a1dVarVDam, a1dVarHDam, &
                                           a2dVarAnalysisValue)
            !------------------------------------------------------------------------------------------
        
        endif
        !------------------------------------------------------------------------------------------
          
        !------------------------------------------------------------------------------------------
        ! Info end
        call mprintf(.true., iINFO_Extra, ' Data :: Output point ... OK ' )
        !------------------------------------------------------------------------------------------
        
    end subroutine HMC_Data_Output_Point_Cpl
    !------------------------------------------------------------------------------------------
     
    !------------------------------------------------------------------------------------------
    ! Subroutine to write netCDF point data output
#ifdef LIB_NC
    subroutine HMC_Data_Output_Point_NC(iID, &
                                          sPathData_Output, &
                                          iNSection, iDataN, iNVar, iNAnalysis, &
                                          iNLake, iNDam, iNPlant, iNJoint, iNCatch, iNRelease, &
                                          sTime, &
                                          a1dVarQoutSection, a1dVarVDam, a1dVarHDam, &
                                          a2dVarAnalysisValue)
        
        !------------------------------------------------------------------------------------------
        ! Variable(s)
        integer(kind = 4)                       :: iID                  
                                  
        character(len = 256), intent(in)        :: sPathData_Output 
        character(len = 700)                    :: sFileNameData_Output
        character(len = 256)                    :: sVarName, sVarNameLong
        character(len = 256)                    :: sVarGridMap, sVarDescription, sVarCoords
        integer(kind = 4), intent(in)           :: iNSection, iDataN, iNVar, iNAnalysis
        integer(kind = 4), intent(in)           :: iNLake, iNDam, iNPlant, iNJoint, iNCatch, iNRelease

        character(len = 19)                     :: sTime, sTimeSave

        real(kind = 4)                          :: dVarMissingValue
        
        character(len = 700)                    :: sCommandZipFile

        real(kind = 4), dimension(iNVar, iNAnalysis), intent(in)        :: a2dVarAnalysisValue
        real(kind = 4), dimension(iNSection), intent(in)                :: a1dVarQoutSection
        real(kind = 4), dimension(iNDam), intent(in)                    :: a1dVarVDam, a1dVarHDam
        
        character(len = 256)    :: sVarUnits
        integer(kind = 4)       :: iErr
        
        integer(kind = 4)       :: iFileID
        integer(kind = 4)       :: iID_Dim_Section, iID_Dim_Var, iID_Dim_Time, iID_Dim_Dam, iID_Dim_Analysis
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Get from global workspace
        sCommandZipFile = oHMC_Namelist(iID)%sCommandZipFile
        ! Info start
        call mprintf(.true., iINFO_Extra, ' Data :: Output point :: NetCDF ... ' )
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Filename output
        sFileNameData_Output = trim(sPathData_Output)//"hmc.output-point."// &
        sTime(1:4)//sTime(6:7)//sTime(9:10)// & 
        sTime(12:13)//sTime(15:16)// &
        ".nc"
            
        ! Info netCDF filename
        call mprintf(.true., iINFO_Verbose, ' Save filename (result point): '//trim(sFileNameData_Output)//' ... ')
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Create netcdf file
        call check( nf90_create(trim(sFileNameData_Output), 0, iFileID) )
	
        ! Dimension(s)
        call check( nf90_def_dim(iFileID, "time", NF90_UNLIMITED, iID_Dim_Time) )
        call check( nf90_def_dim(iFileID, "section", iNSection, iID_Dim_Section) )
        call check( nf90_def_dim(iFileID, "n_variable", iNVar, iID_Dim_Var) )
        call check( nf90_def_dim(iFileID, "n_analysis", iNAnalysis, iID_Dim_Analysis) )
        !------------------------------------------------------------------------------------------

        !------------------------------------------------------------------------------------------
        ! Global attribute(s)
        sTimeSave(1:len_trim(sTime)) = sTime
        call check( nf90_put_att(iFileID, NF90_GLOBAL, "time_coverage_end", sTimeSave) )
        !------------------------------------------------------------------------------------------

        !------------------------------------------------------------------------------------------
        ! Definition mode OFF - Data mode ON
        call check( nf90_enddef(iFileID))
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Writing dynamic point variable(s) in netCDF output file
        ! Section discharge
        sVarName = 'Discharge'; sVarNameLong = 'section_discharge'; sVarDescription = 'outlet section discharge';
        sVarUnits = 'm^3/s'; sVarCoords = 'x y'; sVarGridMap = 'epsg:4326'; dVarMissingValue = -9E15;
        call HMC_Tools_IO_Put1d_NC(iFileID, iID_Dim_Section, & 
                             sVarName, sVarNameLong, sVarDescription, &
                             sVarUnits, sVarCoords, sVarGridMap, dVarMissingValue, &
                             iNSection, a1dVarQoutSection)
        ! Mean point variable(s)
        sVarName = 'AnalysisVar'; sVarNameLong = 'analysis_variable'; sVarDescription = 'analysis variable(s) at each timestep';
        sVarUnits = ''; sVarCoords = 'x y'; sVarGridMap = 'epsg:4326'; dVarMissingValue = -9E15;
        call HMC_Tools_IO_Put2d_NC(iFileID, iID_Dim_Var, iID_Dim_Analysis, & 
                             sVarName, sVarNameLong, sVarDescription, &
                             sVarUnits, sVarCoords, sVarGridMap, dVarMissingValue, &
                             iNVar, iNAnalysis, a2dVarAnalysisValue)
        ! Dam volume
        sVarName = 'VDam'; sVarNameLong = 'dam_volume'; sVarDescription = 'dam volume';
        sVarUnits = 'm^3'; sVarCoords = 'x y'; sVarGridMap = 'epsg:4326'; dVarMissingValue = -9E15;
        call HMC_Tools_IO_Put1d_NC(iFileID, iID_Dim_Dam, & 
                             sVarName, sVarNameLong, sVarDescription, &
                             sVarUnits, sVarCoords, sVarGridMap, dVarMissingValue, &
                             iNDam, a1dVarVDam)
        ! Dam level
        sVarName = 'LDam'; sVarNameLong = 'dam_level'; sVarDescription = 'dam level';
        sVarUnits = 'm'; sVarCoords = 'x y'; sVarGridMap = 'epsg:4326'; dVarMissingValue = -9E15;
        call HMC_Tools_IO_Put1d_NC(iFileID, iID_Dim_Dam, & 
                             sVarName, sVarNameLong, sVarDescription, &
                             sVarUnits, sVarCoords, sVarGridMap, dVarMissingValue, &
                             iNDam, a1dVarHDam)                                   
        !------------------------------------------------------------------------------------------
                             
        !------------------------------------------------------------------------------------------
        ! Close
        call check( nf90_close(iFileID) )
        ! Info netCDF filename
        call mprintf(.true., iINFO_Verbose, ' Save filename (result point): '//trim(sFileNameData_Output)//' ... OK ')
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Zip file
        !call HMC_Tools_Generic_ZipFile(sCommandZipFile, &
        !                               sFileNameData_Output//'.gz', sFileNameData_Output, .false.)
        ! Remove un-zipped file
        !call HMC_Tools_Generic_RemoveFile(oHMC_Namelist(iID)%sCommandRemoveFile, sFileNameData_Output, .false.)
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Info end
        call mprintf(.true., iINFO_Extra, ' Data :: Output point :: NetCDF ... OK ' )
        !------------------------------------------------------------------------------------------
        
    end subroutine HMC_Data_Output_Point_NC
#endif
    !------------------------------------------------------------------------------------------
    
    !------------------------------------------------------------------------------------------
    ! Subroutine to write point ASCII data output
    subroutine HMC_Data_Output_Point_ASCII(iID, &
                                           sPathData_Output, &
                                           iNSection, iNData, iNVar, iNAnalysis,  &
                                           iNLake, iNDam, iNPlant, iNJoint, iNCatch, iNRelease, &
                                           sTime, &
                                           a1dVarQoutSection, a1dVarVDam, a1dVarHDam, &
                                           a2dVarAnalysisValue)
        
        !------------------------------------------------------------------------------------------
        ! Variable(s)
        integer(kind = 4)                       :: iID                  
                                  
        character(len = 700), intent(in)        :: sPathData_Output
        character(len = 700)                    :: sFileNameData_Output
        character(len = 256)                    :: sVarName
        integer(kind = 4)                       :: iNSection, iNData, iNVar, iNAnalysis
        integer(kind = 4)                       :: iNLake, iNDam, iNPlant, iNJoint, iNCatch, iNRelease
        integer(kind = 4)                       :: iRet
        logical                                 :: bFileExist

        character(len = 19), intent(in)         :: sTime

        real(kind = 4), dimension(iNVar, iNAnalysis), intent(in)        :: a2dVarAnalysisValue
        real(kind = 4), dimension(iNSection), intent(in)                :: a1dVarQoutSection
        real(kind = 4), dimension(iNDam), intent(in)                    :: a1dVarVDam, a1dVarHDam
       
        character(len = 256)    :: sVarUnits
        integer(kind = 4)       :: iErr
        
        character(len = 20), parameter :: sFMTDischarge = "(F14.2)"
        character(len = 40), parameter :: sFMTVarAnalysis = "(F20.5, F20.5, F20.5)"
        character(len = 20), parameter :: sFMTVarVDam = "(F20.5)"
        character(len = 20), parameter :: sFMTVarHDam = "(F20.5)"
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Info start
        call mprintf(.true., iINFO_Extra, ' Data :: Output point :: ASCII ... ' )
        ! Info filename(s) at each time step
        call mprintf(.true., iINFO_Verbose, ' Save (result point) at time: '//trim(sTime)//' ... ')
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Writing dynamic point variable(s) in ASCII output file
        
        ! POINT SECTION(S) DISCHARGE
        sFileNameData_Output = trim(sPathData_Output)//"hmc.discharge."// &
                           sTime(1:4)//sTime(6:7)//sTime(9:10)//sTime(12:13)//sTime(15:16)// &
                           ".txt"  
        ! Check and remove old file
        inquire (file = trim(sFileNameData_Output), exist = bFileExist, iostat = iRet)   
        if ( bFileExist ) then              
            call HMC_Tools_Generic_RemoveFile(oHMC_Namelist(iID)%sCommandRemoveFile, sFileNameData_Output, .false.)  
        endif
        ! Save file                        
        call mprintf(.true., iINFO_Extra, ' Save filename: '//trim(sFileNameData_Output) )
        call HMC_Tools_IO_Put1d_ASCII(sFileNameData_Output, a1dVarQoutSection, iNSection, .true., iErr, sFMTDischarge)

        ! POINT MEAN VARIABLE(S)
        sFileNameData_Output = trim(sPathData_Output)//"hmc.var-analysis."// &
                           sTime(1:4)//sTime(6:7)//sTime(9:10)//sTime(12:13)//sTime(15:16)// &
                           ".txt"    
        ! Check and remove old file
        inquire (file = trim(sFileNameData_Output), exist = bFileExist, iostat = iRet)   
        if ( bFileExist ) then              
            call HMC_Tools_Generic_RemoveFile(oHMC_Namelist(iID)%sCommandRemoveFile, sFileNameData_Output, .false.)  
        endif    
        ! Save file
        call mprintf(.true., iINFO_Extra, ' Save filename: '//trim(sFileNameData_Output) )
        call HMC_Tools_IO_Put2d_ASCII(sFileNameData_Output, a2dVarAnalysisValue, &
                                      iNVar, iNAnalysis, .true., iErr, sFMTVarAnalysis)

        ! POINT DAM(S) VOLUME
        sFileNameData_Output = trim(sPathData_Output)//"hmc.vdam."// &
                           sTime(1:4)//sTime(6:7)//sTime(9:10)//sTime(12:13)//sTime(15:16)// &
                           ".txt"  
        ! Check and remove old file
        inquire (file = trim(sFileNameData_Output), exist = bFileExist, iostat = iRet)   
        if ( bFileExist ) then              
            call HMC_Tools_Generic_RemoveFile(oHMC_Namelist(iID)%sCommandRemoveFile, sFileNameData_Output, .false.)  
        endif    
        ! Save file                  
        call mprintf(.true., iINFO_Extra, ' Save filename: '//trim(sFileNameData_Output) )
        call HMC_Tools_IO_Put1d_ASCII(sFileNameData_Output, a1dVarVDam, iNDam, .true., iErr, sFMTVarVDam)
        
        ! POINT DAM(S) LEVEL
        sFileNameData_Output = trim(sPathData_Output)//"hmc.ldam."// &
                           sTime(1:4)//sTime(6:7)//sTime(9:10)//sTime(12:13)//sTime(15:16)// &
                           ".txt"  
        ! Check and remove old file
        inquire (file = trim(sFileNameData_Output), exist = bFileExist, iostat = iRet)   
        if ( bFileExist ) then              
            call HMC_Tools_Generic_RemoveFile(oHMC_Namelist(iID)%sCommandRemoveFile, sFileNameData_Output, .false.)  
        endif    
        ! Save file
        call mprintf(.true., iINFO_Extra, ' Save filename: '//trim(sFileNameData_Output) )
        call HMC_Tools_IO_Put1d_ASCII(sFileNameData_Output, a1dVarHDam, iNDam, .true., iErr, sFMTVarHDam)
        !------------------------------------------------------------------------------------------

        !------------------------------------------------------------------------------------------
        ! Print filename(s) info
        call mprintf(.true., iINFO_Verbose, ' Save (result point) at time: '//trim(sTime)//' ... OK')
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Info end
        call mprintf(.true., iINFO_Extra, ' Data :: Output point :: ASCII ... OK' )
        !------------------------------------------------------------------------------------------
    
    end subroutine HMC_Data_Output_Point_ASCII
    !------------------------------------------------------------------------------------------   
    
end module HMC_Module_Data_Output_Point
!------------------------------------------------------------------------------------------
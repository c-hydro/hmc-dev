!--------------------------------------------------------------------------------  
! File:   HMC_Module_Info_Time.f90
! Author(s): Fabio Delogu, Francesco Silvestro, Simone Gabellani
! Created on November, 06 2015, 8:42 AM
!
! Module to get info time
!--------------------------------------------------------------------------------

!--------------------------------------------------------------------------------
! Module Header
module HMC_Module_Info_Time
    
    !--------------------------------------------------------------------------------
    ! External module(s) for all subroutine in this module
#ifdef LIB_NC
    use netcdf
#endif

    use HMC_Module_Namelist,        only:   oHMC_Namelist
    use HMC_Module_Vars_Loader,     only:   oHMC_Vars
    
    use HMC_Module_Tools_Debug
    
#ifdef LIB_NC
    use HMC_Module_Tools_IO,        only:   HMC_Tools_IO_Get2d_NC, &
                                            HMC_Tools_IO_GetArcGrid_ASCII, &
                                            check
#else
    use HMC_Module_Tools_IO,        only:   HMC_Tools_IO_GetArcGrid_ASCII                                    
#endif
                                            
    ! Implicit none for all subroutines in this module
    implicit none
    !--------------------------------------------------------------------------------

contains 

    !------------------------------------------------------------------------------------------
    ! Subroutine to get time dims
    subroutine HMC_Info_Time_GetDims(iID, iRows, iCols, iETime, iDaySteps, iTMarkedSteps)
        
        !------------------------------------------------------------------------------------
        ! Variable(s) declaration
        integer(kind = 4)               :: iID, iRows, iCols
        integer(kind = 4)               :: iFileID, iErr
        
        integer(kind = 4)               :: iTypeData
        character(len = 256)            :: sDomainName, sPathData, sFileName
        
        character(len = 256)            :: sVarName
        character(len = 256)            :: sVarUnits
        
        real(kind = 4),     dimension (iRows, iCols)    :: a2dVarDEM, a2dVarAreaCell
        real(kind = 4),     dimension (iCols, iRows)    :: a2dVar
        
        integer(kind = 4)               :: iTcMax, iSimLength, iDtDataForcing, iDtModel
        integer(kind = 4)               :: iDomainPixels, iTc, iTcSeconds
        integer(kind = 4)               :: iNData, iNTime, iETime
        integer(kind = 4)               :: iDaySteps, iTMarkedSteps
        real(kind = 4)                  :: dDxM, dDyM
        real(kind = 4)                  :: dDomainArea
        
        logical                         :: bFileExist
        
        character(len = 256)            :: sStrTc, sStrEtime, sStrDaySteps
        !------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------
        ! Variable(s) initialization
        bFileExist = .false.
        sVarName = ''; sVarUnits = ''; iNTime = 0;
        sDomainName = ''; sPathData = ''; sFileName = '';
        iTypeData = 0; iFileID = 0; iErr = 0;
        iTcMax = 0; iSimLength = 0; iDtDataForcing = 0; iDtModel = 0;
        
        a2dVarDEM = 0.0; a2dVarAreaCell = 0.0; a2dVar = 0.0;
        
        iDaySteps = 0; iTMarkedSteps = 0;
        sStrTc = ''
        !------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------
        ! Get information
        sDomainName = oHMC_Namelist(iID)%sDomainName
        sPathData = oHMC_Namelist(iID)%sPathData_Static_Gridded
        iTypeData = oHMC_Namelist(iID)%iFlagTypeData_Static
        
        iTcMax =  oHMC_Namelist(iID)%iTcMax
        iSimLength = oHMC_Namelist(iID)%iSimLength
        iDtDataForcing = oHMC_Namelist(iID)%iDtData_Forcing
        iDtModel = oHMC_Namelist(iID)%iDtModel
        
        iNData = oHMC_Namelist(iID)%iNData
        iNTime = oHMC_Namelist(iID)%iNTime
        
        ! Info
        call mprintf(.true., iINFO_Main, ' Define time dims ... ')
        !------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------
        ! Get info from netCDF data
        if (iTypeData == 2) then
#ifdef LIB_NC
            !------------------------------------------------------------------------------------------
            ! Filename 
            sFileName = trim(sPathData)//'hmc.staticdata.nc'
            
            ! Check file availability
            inquire (file = trim(sFileName), exist = bFileExist)
            if ( .not. bFileExist ) then
                call mprintf(.true., iERROR, ' No land data netCDF found '//trim(sFileName) )
            else
            
                !------------------------------------------------------------------------------------------
                ! Open nc file
                call check( nf90_open(trim(sFileName), NF90_NOWRITE, iFileID) )
                !------------------------------------------------------------------------------------------
                
                !------------------------------------------------------------------------------------------
                ! Get DEM data
                sVarName = 'Terrain'
                call HMC_Tools_IO_Get2d_NC(sVarName, iFileID, a2dVar, sVarUnits, iCols, iRows, .true., iErr)
                a2dVarDEM = transpose(a2dVar)
                
                ! Get areacell data
                sVarName = 'Cell_Area'
                call HMC_Tools_IO_Get2d_NC(sVarName, iFileID, a2dVar, sVarUnits, iCols, iRows, .true., iErr)
                a2dVarAreaCell = transpose(a2dVar)
                !------------------------------------------------------------------------------------------
                
            endif
            !------------------------------------------------------------------------------------------
#else
            !------------------------------------------------------------------------------------ 
            ! Redefinition of info data flag (if netCDF library is not linked)
            iTypeData = 1 
            call mprintf(.true., iWARN, ' '// &
                                            'time info are derived using data in netCDF format but library is not linked! '// &
                                            'Will be used data in ASCII format!')
            !------------------------------------------------------------------------------------    
#endif
        endif
        !------------------------------------------------------------------------------------------ 
        
        !------------------------------------------------------------------------------------------
        ! Get info from ASCII data
        if (iTypeData == 1) then
            
            !------------------------------------------------------------------------------------------
            ! Filename
            sFileName = trim(sPathData)//trim(sDomainName)//'.dem.txt'
            
            ! Check file availability 
            inquire (file = trim(sFileName), exist = bFileExist)
            if ( .not. bFileExist ) then
                call mprintf(.true., iERROR, ' No dem data ASCII found '//trim(sFileName) )
            else
                
                ! Get dem data
                call HMC_Tools_IO_GetArcGrid_ASCII(sFileName, a2dVar, iCols, iRows, .true., iErr)
                a2dVarDEM = reshape(a2dVar, (/iRows, iCols/))
                
            endif
            !------------------------------------------------------------------------------------------
            
            !------------------------------------------------------------------------------------------
            ! Filename
            sFileName = trim(sPathData)//trim(sDomainName)//'.areacell.txt'
            
            ! Check file availability 
            inquire (file = trim(sFileName), exist = bFileExist)
            if ( .not. bFileExist ) then
                call mprintf(.true., iERROR, ' No areacell data ASCII found '//trim(sFileName) )
            else
                
                ! Get areacell data
                call HMC_Tools_IO_GetArcGrid_ASCII(sFileName, a2dVar, iCols, iRows, .true., iErr)
                a2dVarAreaCell = reshape(a2dVar, (/iRows, iCols/))
                
            endif
            !------------------------------------------------------------------------------------------
            
        endif
        !------------------------------------------------------------------------------------------    

        !------------------------------------------------------------------------------------------
        ! Defining cell area mean value (x and y)
        dDxM = nint(sqrt(sum(a2dVarAreaCell, mask=a2dVarAreaCell.gt.0.0) / count(a2dVarAreaCell.gt.0.0)))
        dDyM = nint(sqrt(sum(a2dVarAreaCell, mask=a2dVarAreaCell.gt.0.0) / count(a2dVarAreaCell.gt.0.0)))
       
        ! Computing total catchment pixels and area
        iDomainPixels = count(a2dVarDEM.gt.0.0)
        
        ! Computing total catchment pixels and area           
        dDomainArea = float(iDomainPixels)*dDxM*dDyM/1000000       
        
        ! Computing corrivation time [hour]
        iTc = nint(0.27*sqrt(0.6*dDomainArea) + 0.25)
        if (iTc.gt.840) iTc = 840 !No more than 5 weeks
        
        ! Define iTMaxFor
        if (iTcMax.gt.0) iTc = iTcMax ! defined by user
        iTcSeconds = iTc*3600
        
        ! Compute extra time
        iETime = iNTime + iTc
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------
        ! Day steps
        iDaySteps = 24*3600/oHMC_Namelist(iID)%iDtData_Forcing
        !------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------
        ! Energy-balance extra dimension(s)
        iTMarkedSteps = int(oHMC_Namelist(iID)%iTdeepShift*(3600/oHMC_Namelist(iID)%iDtData_Forcing) + 1)
        !------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Pass local variable(s) to global workspace
        oHMC_Namelist(iID)%iETime = iETime
        oHMC_Namelist(iID)%iTc = iTc
        oHMC_Namelist(iID)%iDaySteps = iDaySteps
        oHMC_Namelist(iID)%iTMarkedSteps = iTMarkedSteps
        
        ! Time info
        write(sStrTc, *) iTc; write(sStrEtime, *) iETime; write(sStrDaySteps, *) iDaySteps;
        call mprintf(.true., iINFO_Main, ' TIME INFO --- '// &
                'CorrivationTime: '//trim(sStrTc)//' [hour] '//'SimLength: '//trim(sStrEtime)//' [m] '// &
                'DaySteps: '//sStrDaySteps)
        
        ! Info
        call mprintf(.true., iINFO_Main, ' Define time dims ... OK')
        !------------------------------------------------------------------------------------------
        
    end subroutine HMC_Info_Time_GetDims
    !------------------------------------------------------------------------------------------
    
end module HMC_Module_Info_Time
!------------------------------------------------------------------------------------------
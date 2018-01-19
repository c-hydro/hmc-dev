!--------------------------------------------------------------------------------  
! File:   HMC_Module_Info_Point.f90
! Author(s): Fabio Delogu, Francesco Silvestro, Simone Gabellani
! Created on May, 20 2014, 9:57 AM
!
! Module to get info point
!--------------------------------------------------------------------------------

!--------------------------------------------------------------------------------
! Module Header
module HMC_Module_Info_Point
    
    !--------------------------------------------------------------------------------
    ! External module(s) and implicit none
    use HMC_Module_Namelist,        only: oHMC_Namelist
    use HMC_Module_Vars_Loader,     only: oHMC_Vars
    
    use HMC_Module_Tools_Debug
    
    use HMC_Module_Tools_Generic,   only: HMC_Tools_Generic_FindFileLength
    
    implicit none
    !--------------------------------------------------------------------------------

contains 
    
    !--------------------------------------------------------------------------------
    ! Subroutine to get section(s) dimension(s)
    subroutine HMC_Info_Point_Section_GetDims(iID, iNSection)
        
        !--------------------------------------------------------------------------------
        ! Variable(s) declaration
        integer(kind = 4)           :: iID
        integer(kind = 4)           :: iNSection

        character(len = 256)        :: sDomainName, sPathData

        character(len = 700)        :: sFileName
        
        logical                     :: bFileExist
        !--------------------------------------------------------------------------------
        
        !--------------------------------------------------------------------------------
        ! Initialize variable(s)
        iNSection = -9999;
        !--------------------------------------------------------------------------------
        
        !--------------------------------------------------------------------------------
        ! Get information
        sDomainName = oHMC_Namelist(iID)%sDomainName
        sPathData = oHMC_Namelist(iID)%sPathData_Static_Point
        ! Info
        call mprintf(.true., iINFO_Main, ' Define section data dims ... ')
        !--------------------------------------------------------------------------------
        
        !--------------------------------------------------------------------------------
        ! Get Lake(s) dims
        sFileName = trim(sPathData)//trim(sDomainName)//'.info_section.txt'
        inquire (file = trim(sFileName), exist = bFileExist)
        if ( .not. bFileExist ) then
            call mprintf(.true., iWARN, 'No section file info found '//trim(sFileName) )
            iNSection = -9999;
        else
            ! Open and read ASCII grid file
            open(unit = 1,file=trim(sFileName), status='old')
            call HMC_Tools_Generic_FindFileLength(1, iNSection, 0)
            close(1)    
        endif
        !--------------------------------------------------------------------------------

        !--------------------------------------------------------------------------------
        ! Dims from local to global workspace
        oHMC_Namelist(iID)%iNSection = iNSection
        ! Info
        call mprintf(.true., iINFO_Main, ' Define section data dims ... OK')
        !--------------------------------------------------------------------------------
        
    end subroutine HMC_Info_Point_Section_GetDims
    !--------------------------------------------------------------------------------
    
    !--------------------------------------------------------------------------------
    ! Subroutine to get water-body dimension(s)
    subroutine HMC_Info_Point_WaterBody_GetDims(iID, iNLake)
        
        !--------------------------------------------------------------------------------
        ! Variable(s) declaration
        integer(kind = 4)           :: iID
        integer(kind = 4)           :: iNLake
        
        character(len = 256)        :: sDomainName, sPathData

        character(len = 700)        :: sFileName
        
        logical                     :: bFileExist
        !--------------------------------------------------------------------------------
        
        !--------------------------------------------------------------------------------
        ! Initialize variable(s)
        iNLake = -9999;
        !--------------------------------------------------------------------------------
        
        !--------------------------------------------------------------------------------
        ! Get information
        sDomainName = oHMC_Namelist(iID)%sDomainName
        sPathData = oHMC_Namelist(iID)%sPathData_Static_Point
        ! Info
        call mprintf(.true., iINFO_Main, ' Define lake(s) data dims ... ')
        !--------------------------------------------------------------------------------
        
        !--------------------------------------------------------------------------------
        ! Get Lake(s) dims
        sFileName = trim(sPathData)//trim(sDomainName)//'.info_lake.txt'
        inquire (file = trim(sFileName), exist = bFileExist)
        if ( .not. bFileExist ) then
            call mprintf(.true., iWARN, 'No lake(s) file info found '//trim(sFileName) )
            iNLake = -9999;
        else
            ! Open and read ASCII grid file
            open(unit = 1,file=trim(sFileName), status='old')
            read(1,*) iNLake
            close(1)    
        endif
        !--------------------------------------------------------------------------------

        !--------------------------------------------------------------------------------
        ! Dims from local to global workspace
        oHMC_Namelist(iID)%iNLake = iNLake
        ! Info
        call mprintf(.true., iINFO_Main, ' Define lake(s) data dims ... OK')
        !--------------------------------------------------------------------------------

    end subroutine HMC_Info_Point_WaterBody_GetDims
    !--------------------------------------------------------------------------------
    
    !------------------------------------------------------------------------------------------
    ! Subroutine to get water-body dimension(s)
    subroutine HMC_Info_Point_HydraulicStructure_GetDims(iID, &
                                                         iNDam, iNPlant, iNJoint, iNCatch, iNRelease)
        
        !------------------------------------------------------------------------------------------
        ! Variable(s) declaration
        integer(kind = 4)      :: iID
        integer(kind = 4)      :: iNDam, iNPlant, iNJoint, iNCatch, iNRelease
        
        character(len = 256)        :: sDomainName, sPathData

        character(len = 700)        :: sFileName
        
        logical                     :: bFileExist
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Initialize variable(s)
        iNDam = -9999; iNPlant = -9999; 
        iNJoint = -9999; 
        iNCatch = -9999; iNRelease = -9999;
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Get information
        sDomainName = oHMC_Namelist(iID)%sDomainName
        sPathData = oHMC_Namelist(iID)%sPathData_Static_Point
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Get dam(s)
        call mprintf(.true., iINFO_Main, ' Define dam(s) data dims ... ')
        ! Get filename
        sFileName = trim(sPathData)//trim(sDomainName)//'.info_dam.txt'
        inquire (file = trim(sFileName), exist = bFileExist)
        if ( .not. bFileExist ) then
            call mprintf(.true., iWARN, 'No dam(s) file info found '//trim(sFileName) )
            iNDam = -9999; iNPlant = -9999
        else
            ! Open and read ascii grid file
            open(unit = 1,file=sFileName, status='old')
            read(1,*) iNDam
            read(1,*) iNPlant
            close(1)
        endif
        ! Info
        call mprintf(.true., iINFO_Main, ' Define dam(s) data dims ... OK')
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Get joint(s)
        call mprintf(.true., iINFO_Main, ' Define joint(s) data dims ... ')
        ! Get filename
        sFileName = trim(sPathData)//trim(sDomainName)//'.info_joint.txt'
        inquire (file = trim(sFileName), exist = bFileExist)
        if ( .not. bFileExist ) then
            call mprintf(.true., iWARN, 'No joint(s) file info found '//trim(sFileName) )
            iNJoint = -9999;
        else
            ! Open and read ascii grid file
            open(unit = 1,file=sFileName, status='old')
            read(1,*) iNJoint
            close(1)
        endif
        ! Info
        call mprintf(.true., iINFO_Main, ' Define joint(s) data dims ... OK')
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Get intake(s)
        call mprintf(.true., iINFO_Main, ' Define intake(s) data dims ... ')
        ! Get filename
        sFileName = trim(sPathData)//trim(sDomainName)//'.info_intake.txt'
        inquire (file = trim(sFileName), exist = bFileExist)
        if ( .not. bFileExist ) then
            call mprintf(.true., iWARN, 'No intake(s) file info found '//trim(sFileName) )
            iNCatch = -9999; iNRelease = -9999;
        else
            ! Open and read ascii grid file
            open(unit = 1,file=sFileName, status='old')
            read(1,*) iNCatch
            read(1,*) iNRelease
            close(1) 
        endif
        ! Info
        call mprintf(.true., iINFO_Main, ' Define intake(s) data dims ... OK')
        !------------------------------------------------------------------------------------------

        !------------------------------------------------------------------------------------------
        ! Dims from local to global workspace
        oHMC_Namelist(iID)%iNDam = iNDam
        oHMC_Namelist(iID)%iNPlant = iNPlant
        oHMC_Namelist(iID)%iNJoint = iNJoint
        oHMC_Namelist(iID)%iNCatch = iNCatch
        oHMC_Namelist(iID)%iNRelease = iNRelease
        !------------------------------------------------------------------------------------------
        
    end subroutine HMC_Info_Point_HydraulicStructure_GetDims
    !------------------------------------------------------------------------------------------
    
end module HMC_Module_Info_Point
!--------------------------------------------------------------------------------        
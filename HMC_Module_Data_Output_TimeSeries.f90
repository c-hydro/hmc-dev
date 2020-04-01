!------------------------------------------------------------------------------------------     
! File:   HMC_Module_Data_Output_TimeSeries.f90
! Author(s): Fabio Delogu, Francesco Silvestro, Simone Gabellani, Alessandro Masoero
! Created on February 6, 2017, 9:25 AM
!
! Module to write time-series output data
!------------------------------------------------------------------------------------------

!------------------------------------------------------------------------------------------
! Module Header
module HMC_Module_Data_Output_TimeSeries
    
    !------------------------------------------------------------------------------------------
    ! External module(s) for all subroutine in this module
    use HMC_Module_Namelist,        only:   oHMC_Namelist
    use HMC_Module_Vars_Loader,     only:   oHMC_Vars
    
    use HMC_Module_Tools_Debug
    
    use HMC_Module_Tools_IO,        only:   HMC_Tools_IO_WriteTS_ASCII
    
    use HMC_Module_Tools_Generic,   only:   HMC_Tools_Generic_ReplaceText, & 
                                            HMC_Tools_Generic_SetUnit, &
                                            HMC_Tools_Generic_CreateFolder, &
                                            HMC_Tools_Generic_RemoveFile
    
    ! Implicit none for all subroutines in this module
    implicit none
    !------------------------------------------------------------------------------------------
    
contains
    
    !------------------------------------------------------------------------------------------
    ! Subroutine to manage output time-series data
    subroutine HMC_Data_Output_TimeSeries_Cpl(iID, sTime, &
                                              iTime, iETime, &
                                              iNSection, iNData, &
                                              iNLake, iNDam, iNPlant, iNJoint, iNCatch, iNRelease)
        
    !------------------------------------------------------------------------------------------
                                          
        !------------------------------------------------------------------------------------------
        ! Variable(s)                                       
        integer(kind = 4)               :: iID
        integer(kind = 4)               :: iTime, iETime
        integer(kind = 4)               :: iNSection, iNData
        integer(kind = 4)               :: iNLake, iNDam, iNPlant, iNJoint, iNCatch, iNRelease
        
        integer(kind = 4)               :: iFlagTypeData_Output
        
        character(len = 19)             :: sTime
        character(len = 700)            :: sPathData_Output
        character(len = 700)            :: sCommandCreateFolder
        
        real(kind = 4), dimension(iNSection)            :: a1dVarQoutSection
        real(kind = 4), dimension(iNDam)                :: a1dVarVDam, a1dVarHDam
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Initialize variable(s)
        a1dVarVDam = -9999.0; a1dVarHDam= -9999.0; a1dVarQoutSection = -9999.0
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Get global information
        sPathData_Output = oHMC_Namelist(iID)%sPathData_Output_TimeSeries
        iFlagTypeData_Output = oHMC_Namelist(iID)%iFlagTypeData_Output_TimeSeries
        sCommandCreateFolder = oHMC_Namelist(iID)%sCommandCreateFolder
        ! Info start
        call mprintf(.true., iINFO_Extra, ' Data :: Output time-series ... ' )
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
        ! Get discharge in outlet section(s)
        a1dVarQoutSection = oHMC_Vars(iID)%a1dQoutSection
        ! Get volume, level amd discharge in dam section(s)
        a1dVarVDam = oHMC_Vars(iID)%a1dVDam 
        a1dVarHDam = oHMC_Vars(iID)%a1dHDam 
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Subroutine for writing time-series in ASCII format
        if (iFlagTypeData_Output == 1) then
        
            !------------------------------------------------------------------------------------------
            ! Call subroutine to write time-series data
            call HMC_Data_Output_TimeSeries_ASCII(iID, sPathData_Output, &
                                                  iNSection, iNDam, sTime, &
                                                  a1dVarQoutSection, a1dVarVDam)
            !------------------------------------------------------------------------------------------
                                                  
        endif
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Subroutine for writing time-series in unknown format
        if (iFlagTypeData_Output == 2) then

            !------------------------------------------------------------------------------------------
            ! Choosing data type
            call mprintf(.true., iERROR, ' Using UNKNOWN data time-series output. Check settings file!')
            !------------------------------------------------------------------------------------------

        endif
        !------------------------------------------------------------------------------------------
        
        
        !------------------------------------------------------------------------------------------
        ! Info end
        call mprintf(.true., iINFO_Extra, ' Data :: Output time-series ... OK ' )
        !------------------------------------------------------------------------------------------
        
    end subroutine HMC_Data_Output_TimeSeries_Cpl
    !------------------------------------------------------------------------------------------
    !------------------------------------------------------------------------------------------
    ! Subroutine to write ASCII time-series output
    subroutine HMC_Data_Output_TimeSeries_ASCII(iID, sPathData_Output, &
                                              iNSection, iNDam, sTime, &
                                              a1dVarQoutSection, a1dVarVDam)
        
        !------------------------------------------------------------------------------------------
        ! Variable(s)
        integer(kind = 4)                       :: iID                  
                                  
        character(len = 700), intent(in)        :: sPathData_Output
        character(len = 700)                    :: sFileNameData_Output_Q, sFileNameData_Output_VDam
        integer(kind = 4)                       :: iI
        integer(kind = 4)                       :: iNSection, iNDam
        integer(kind = 4)                       :: iRet, iFileUnit
        logical                                 :: bFileExist

        character(len = 19), intent(in)         :: sTime
        character(len = 12)                     :: sTimeStep

        real(kind = 4), dimension(iNSection), intent(in)    :: a1dVarQoutSection
        real(kind = 4), dimension(iNSection), intent(in)    :: a1dVarVDam

        character(len = 30) :: sFMTTimeSeriesQ
        character(len = 30) :: sFMTTimeSeriesVDam
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Format definition
        !write(sFMTTimeSeries, '("(f8.1, ",I0,"(f11.2,1x))")') iNSection ! using iTime
        write(sFMTTimeSeriesQ, '("(A12, ",I0,"(f11.2,1x))")') iNSection ! using sTimeStep
        write(sFMTTimeSeriesVDam, '("(A12, ",I0,"(f20.1,1x))")') iNDam ! using sTimeStep
        ! TimeStep definition
        sTimeStep = sTime(1:4)//sTime(6:7)//sTime(9:10)//sTime(12:13)//sTime(15:16)
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Discharge time-series 
        if (iNSection.gt.0) then
            ! Get filename hydrograph
            sFileNameData_Output_Q = trim(sPathData_Output)//"hmc.hydrograph.txt"

            ! Open unit dynamically
            if (oHMC_Vars(iID)%iFileUnitTSQ .lt. 0) then
                ! Check and remove old time-series file
                inquire (file = trim(sFileNameData_Output_Q), exist = bFileExist, iostat = iRet)   
                if ( bFileExist ) then              
                    call HMC_Tools_Generic_RemoveFile(oHMC_Namelist(iID)%sCommandRemoveFile, sFileNameData_Output_Q, .false.)  
                endif
                ! Open unit for new time-series file
                call HMC_Tools_Generic_SetUnit(200, 250, oHMC_Vars(iID)%iFileUnitTSQ)
            endif

            ! Call write time-series function
            call HMC_Tools_IO_WriteTS_ASCII(sFileNameData_Output_Q, sTimeStep, & 
                a1dVarQoutSection, iNSection, oHMC_Vars(iID)%iFileUnitTSQ , oHMC_Vars(iID)%bFileUnitTSQ, .true., sFMTTimeSeriesQ)
        endif
        !------------------------------------------------------------------------------------------

        !------------------------------------------------------------------------------------------
        ! Dam volume time-series
        if (iNDam.gt.0) then
            ! Get filename dam volume
            sFileNameData_Output_VDam = trim(sPathData_Output)//"hmc.vdam.txt"

            ! Open unit dynamically
            if (oHMC_Vars(iID)%iFileUnitTSVDam .lt. 0) then
                ! Check and remove old time-series file
                inquire (file = trim(sFileNameData_Output_VDam), exist = bFileExist, iostat = iRet)   
                if ( bFileExist ) then              
                    call HMC_Tools_Generic_RemoveFile(oHMC_Namelist(iID)%sCommandRemoveFile, sFileNameData_Output_VDam, .false.)  
                endif
                ! Open unit for new time-series file
                call HMC_Tools_Generic_SetUnit(200, 250, oHMC_Vars(iID)%iFileUnitTSVDam)
            endif

            ! Call write time-series function
            call HMC_Tools_IO_WriteTS_ASCII(sFileNameData_Output_VDam, sTimeStep, & 
                a1dVarVDam, iNDam, oHMC_Vars(iID)%iFileUnitTSVDam , oHMC_Vars(iID)%bFileUnitTSVDam, .true., sFMTTimeSeriesVDam)
        endif
        !------------------------------------------------------------------------------------------
            
    end subroutine HMC_Data_Output_TimeSeries_ASCII
    !------------------------------------------------------------------------------------------    
    
end module HMC_Module_Data_Output_TimeSeries
!------------------------------------------------------------------------------------------


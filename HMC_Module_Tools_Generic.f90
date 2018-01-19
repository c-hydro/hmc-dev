!------------------------------------------------------------------------------------------     
! File:   HMC_Module_Tools_Generic.f90
! Author: Fabio Delogu
! Created on March 24, 2014, 1:25 PM
!
! Module to define generic tools
!------------------------------------------------------------------------------------------

!------------------------------------------------------------------------------------------
! Module header
module HMC_Module_Tools_Generic
    
    !------------------------------------------------------------------------------------------
    ! External module(s) and implicit none
    use HMC_Module_Tools_Debug
    
    use HMC_Module_Tools_Interp,            only: nearest_interp_1d, linspace
    
    implicit none
    !------------------------------------------------------------------------------------------
    
contains     
    
    !------------------------------------------------------------------------------------
    ! Subroutine to get integration boundaries
    subroutine getIntRange(a1dDemStep, a1dIntStep, dDEMStepMean, dDemDelta, iIndexStart, iIndexEnd)
        
        !------------------------------------------------------------------------------------
        ! Variable(s) declaration  
        real(kind = 4)                  :: dDEMStepMean

        real(kind = 4)                  :: dDemDelta, dIntDelta
        
        integer(kind = 4)               :: iIndexStart, iIndexEnd

        real(kind = 4), dimension(4)    :: a1dDemStep, a1dIntStep
        
        real(kind = 4), dimension(1)    :: a1dDemStepMean, a1dDemStepMin
        integer(kind = 4), dimension(1) :: a1iIndexStepMin
        !------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------
        ! Compute array indexes to get integration and dem upper and lower boundaries
        a1dDemStepMean = dDEMStepMean
        call nearest_interp_1d( size(a1dIntStep), a1dIntStep, a1dDemStep, 1, a1dDemStepMean, a1dDemStepMin, a1iIndexStepMin)
        
        if (a1dDemStep(a1iIndexStepMin(1)) .lt. dDEMStepMean) then
            iIndexStart = a1iIndexStepMin(1)
            iIndexEnd = a1iIndexStepMin(1) + 1
        else
            iIndexStart = a1iIndexStepMin(1) -  1
            iIndexEnd = a1iIndexStepMin(1)
        endif
        !------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------
        ! Compute dem delta
        dDemDelta = a1dDemStep(iIndexEnd) - a1dDemStep(iIndexStart) + 1
        dIntDelta = a1dIntStep(iIndexEnd) - a1dIntStep(iIndexStart) + 1
        !------------------------------------------------------------------------------------

    end subroutine
    !------------------------------------------------------------------------------------

    !------------------------------------------------------------------------------------
    ! Subroutine to get integration values
    subroutine getIntValue(a1dDemStep, a1dIntStep, a1dDtStep, a1dDtRatioStep, &
        dDemMean, dDemDelta, iIndexStart, iIndexEnd, &
        dIntSel, dDtSel, dDtRatioSel)
        
        !------------------------------------------------------------------------------------
        ! Variable(s) declaration
        real(kind = 4), dimension(4)                    :: a1dDemStep, a1dIntStep, a1dDtStep, a1dDtRatioStep
        real(kind = 4)                                  :: dDEMMean, dDemDelta
       
        integer(kind = 4)                               :: iIndexStart, iIndexEnd

        real(kind = 4), dimension(int(dDemDelta))       :: a1dIntDef, a1dDemDef, a1dDtDef, a1dDtRatioDef
        
        real(kind = 4), dimension(1)                    :: a1dDemMean, a1dIntSel
        integer(kind = 4), dimension(1)                 :: a1iIndexSel
        
        real(kind = 4)                                  :: dIntSel, dDtSel, dDtRatioSel
        !------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------
        ! Compute integration values (using average dem step)
        a1dDemMean = dDEMMean
        
        call linspace(a1dDemStep(iIndexStart),a1dDemStep(iIndexEnd), a1dDemDef)
        call linspace(a1dIntStep(iIndexStart),a1dIntStep(iIndexEnd), a1dIntDef)
        call linspace(a1dDtStep(iIndexStart),a1dDtStep(iIndexEnd), a1dDtDef)
        call linspace(a1dDtRatioStep(iIndexStart),a1dDtRatioStep(iIndexEnd), a1dDtRatioDef)
        
        call nearest_interp_1d( size(a1dDemDef), a1dDemDef, a1dIntDef, 1, a1dDemMean, a1dIntSel, a1iIndexSel)
        
        ! Select integration values
        dIntSel = real(nint(a1dIntDef(a1iIndexSel(1))))
        dDtSel = real(nint(a1dDtDef(a1iIndexSel(1))))
        dDtRatioSel = real(nint(a1dDtRatioDef(a1iIndexSel(1))))
        !------------------------------------------------------------------------------------
       
    end subroutine getIntValue
    !------------------------------------------------------------------------------------
    
    !------------------------------------------------------------------------------------
    ! Subroutine to use nudging assimilation
    subroutine assimNudging(a2iVarMask, a2dVarModel, a2dVarObs, a2dVarKernel, &
        a2dVarModelAssim, a2dVarCorr)
    
        !------------------------------------------------------------------------------------
        ! Variable(s) declaration
        integer(kind = 4)                 :: iCols, iRows
        integer(kind = 4), dimension(:,:) :: a2iVarMask     

        real(kind = 4), dimension(:,:)    :: a2dVarModel, a2dVarObs
        real(kind = 4), dimension(:,:)    :: a2dVarKernel

        real(kind = 4), dimension(lbound(a2dVarModel, dim=1):ubound(a2dVarModel, dim=1), &
                                  lbound(a2dVarModel, dim=2):ubound(a2dVarModel, dim=2)) :: a2dVarModelAssim
        real(kind = 4), dimension(lbound(a2dVarModel, dim=1):ubound(a2dVarModel, dim=1), &
                                  lbound(a2dVarModel, dim=2):ubound(a2dVarModel, dim=2)) :: a2dVarCorr
        !------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------
        ! Get iCols and iRows
        iCols = ubound(a2dVarModel, dim=2) - lbound(a2dVarModel, dim=2) + 1
        iRows = ubound(a2dVarModel, dim=1) - lbound(a2dVarModel, dim=1) + 1
                                  
        ! Initialize variable
        a2dVarModelAssim = 0.0; a2dVarCorr = 0.0
        !------------------------------------------------------------------------------------
          
        !------------------------------------------------------------------------------------
        ! Apply nudging method
        where( (a2iVarMask.gt.0.0) .and. (a2dVarObs.ge.0.0) .and. (a2dVarKernel.ge.0.0) )
            a2dVarCorr = a2dVarKernel*(a2dVarObs - a2dVarModel)
            a2dVarModel = a2dVarModel + a2dVarCorr
        endwhere
        !------------------------------------------------------------------------------------
         
        !------------------------------------------------------------------------------------
        ! Store result(s) in output variable
        a2dVarModelAssim = a2dVarModel
        !------------------------------------------------------------------------------------

    end subroutine assimNudging
    !------------------------------------------------------------------------------------
    
    !------------------------------------------------------------------------------------
    ! Function to transpose 3D variable
    function transpose3Dvar(a3dVar) result(a3dVarT)
        
        !------------------------------------------------------------------------------------
        ! Variable(s) declaration
        integer(kind = 4)                   :: iT
        real(kind = 4), dimension(:,:,:)    :: a3dVar
        
        real(kind = 4), dimension(lbound(a3dVar, dim=2):ubound(a3dVar, dim=2), &
                                  lbound(a3dVar, dim=1):ubound(a3dVar, dim=1), &
                                  lbound(a3dVar, dim=3):ubound(a3dVar, dim=3)) :: a3dVarT
        !------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------
        ! Initialize variable
        a3dVarT = -9999.0
        !------------------------------------------------------------------------------------   
        
        !------------------------------------------------------------------------------------
        ! Transpose array step by step
        do iT = 1, ubound(a3dVar, dim=3)
            a3dVarT(:,:,iT) = transpose(a3dVar(:,:,iT))
        enddo
        !------------------------------------------------------------------------------------
        
    end function transpose3Dvar
    !------------------------------------------------------------------------------------
    
    !------------------------------------------------------------------------------------
    ! Function to nullify 2D variable borders
    function nullborder2DVar(a2dVarValue, dNoData) result(a2dVarValueUpd)
        
        !------------------------------------------------------------------------------------
        ! Variable(s) declaration
        integer(kind = 4)                   :: iRows, iCols
        real(kind = 4)                      :: dNoData
        
        real(kind = 4), dimension(:,:)      :: a2dVarValue
        
        real(kind = 4), dimension(lbound(a2dVarValue, dim=1):ubound(a2dVarValue, dim=1), &
                                  lbound(a2dVarValue, dim=2):ubound(a2dVarValue, dim=2)) :: a2dVarValueUpd
        !------------------------------------------------------------------------------------
                       
        !------------------------------------------------------------------------------------
        ! Init variable(s)
        a2dVarValueUpd = 0.0;
        !------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------
        ! Define dimension(s)
        iCols = ubound(a2dVarValue, dim=2) - lbound(a2dVarValue, dim=2) + 1
        iRows = ubound(a2dVarValue, dim=1) - lbound(a2dVarValue, dim=1) + 1
        !------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------
        ! Pass source map to outcome map             
        a2dVarValueUpd = a2dVarValue
        
        ! Nullify map boundary
        a2dVarValueUpd(1,:) = dNoData
        a2dVarValueUpd(:,1) = dNoData
        a2dVarValueUpd(:,iCols) = dNoData
        a2dVarValueUpd(iRows,:) = dNoData               
        !------------------------------------------------------------------------------------
        
    end function nullborder2DVar
    !------------------------------------------------------------------------------------                            
                            
    !------------------------------------------------------------------------------------
    ! Function to compute minimum 2D variable value
    function min2Dvar(a2dVarValue, a2iVarMask)   result(dVarValue)
        
        !------------------------------------------------------------------------------------
        ! Variable(s) declaration
        integer(kind = 4), dimension(:,:)   :: a2iVarMask
        real(kind = 4), dimension(:,:)      :: a2dVarValue
        integer(kind=4)                     :: iRows, iCols
        
        real(kind = 4)                      :: dVarValue
        
        character(len = 20)                 :: sVarValue
        !------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------
        ! Compute max value
        dVarValue = -9999.0
        dVarValue = minval(a2dVarValue, mask=a2iVarMask.gt.0.0)
        
        ! Info max value
        write(sVarValue, *) dVarValue
        call mprintf(.true., iINFO_Extra, ' Min variable value: '//trim(sVarValue))
        !------------------------------------------------------------------------------------
        
    end function min2Dvar
    !------------------------------------------------------------------------------------
    
    !------------------------------------------------------------------------------------
    ! Function to compute maximum 2D variable value
    function max2Dvar(a2dVarValue, a2iVarMask)   result(dVarValue)
        
        !------------------------------------------------------------------------------------
        ! Variable(s) declaration
        integer(kind = 4), dimension(:,:)   :: a2iVarMask
        real(kind = 4), dimension(:,:)      :: a2dVarValue
        
        real(kind = 4)                      :: dVarValue
        
        character(len = 20)                 :: sVarValue
        !------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------
        ! Compute max value
        dVarValue = -9999.0
        dVarValue = maxval(a2dVarValue, mask=a2iVarMask.gt.0.0)
        
        ! Info max value
        write(sVarValue, *) dVarValue
        call mprintf(.true., iINFO_Extra, ' Max variable value: '//trim(sVarValue))
        !------------------------------------------------------------------------------------
        
    end function max2Dvar
    !------------------------------------------------------------------------------------
        
    !------------------------------------------------------------------------------------
    ! Function to compute average 2D variable value
    function mean2Dvar(a2dVarValue, a2iVarMask)   result(dVarValue)
        
        !------------------------------------------------------------------------------------
        ! Variable(s) declaration
        integer(kind = 4), dimension(:,:)   :: a2iVarMask
        real(kind = 4), dimension(:,:)      :: a2dVarValue
        
        real(kind = 4)                      :: dVarValue
        
        character(len = 20)                 :: sVarValue
        !------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------
        ! Compute average value
        dVarValue = -9999.0
        dVarValue = sum(a2dVarValue, mask=a2iVarMask.gt.0.0)/max(1,count(a2iVarMask.gt.0.0))
        
        ! Info mean value
        write(sVarValue, *) dVarValue
        call mprintf(.true., iINFO_Extra, ' Mean variable value: '//trim(sVarValue))
        !------------------------------------------------------------------------------------
        
    end function mean2Dvar
    !------------------------------------------------------------------------------------
    
    !------------------------------------------------------------------------------------
    ! function to check domain variable
    function checkdomainvar(a2dVarValue, a2iVarMask, dVarNoData) result(a2dVarValueUpd)
    
        !------------------------------------------------------------------------------------
        ! Variable(s) declaration
        integer(kind = 4), dimension(:,:)   :: a2iVarMask
        real(kind = 4), dimension(:,:)      :: a2dVarValue
        real(kind = 4), optional            :: dVarNoData
        
        real(kind = 4), dimension(lbound(a2dVarValue, dim=1):ubound(a2dVarValue, dim=1), &
                                  lbound(a2dVarValue, dim=2):ubound(a2dVarValue, dim=2)) :: a2dVarValueUpd
        !------------------------------------------------------------------------------------
                                
        !------------------------------------------------------------------------------------
        ! Initialize variable
        a2dVarValueUpd = -9999.0
        !------------------------------------------------------------------------------------
                                  
        !------------------------------------------------------------------------------------
        ! Info
        call mprintf(.true., iINFO_Extra, ' Check variable domain ... ')
        !------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------
        ! Check variable on mask domain
        if (present(dVarNoData)) then
            
            !------------------------------------------------------------------------------------
            ! Check value on mask
            where(a2iVarMask.lt.0.0)
                a2dVarValue = dVarNoData
            endwhere
            !------------------------------------------------------------------------------------
            
        else
            
            !------------------------------------------------------------------------------------
            ! Check value on mask
            where(a2iVarMask.lt.0.0)
                a2dVarValue = -9999.0
            endwhere
            !------------------------------------------------------------------------------------
        
        endif
        !------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------
        ! Update variable
        a2dVarValueUpd = a2dVarValue
        !------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------
        ! Info
        call mprintf(.true., iINFO_Extra, ' Check variable domain ... OK')
        !------------------------------------------------------------------------------------
        
    end function checkdomainvar
    !------------------------------------------------------------------------------------
    
    !------------------------------------------------------------------------------------
    ! function to check 2D variable limit(s)
    function check2Dvar(a2dVarValue, a2iVarMask, &
                        dVarValueMin, dVarValueMax, dVarNoData) result(a2dVarValueUpd)
        
        !------------------------------------------------------------------------------------
        ! Variable(s) declaration
        integer(kind = 4), dimension(:,:)   :: a2iVarMask
        real(kind = 4), dimension(:,:)      :: a2dVarValue
        real(kind = 4)                      :: dVarValueMin, dVarValueMax
        real(kind = 4), optional            :: dVarNoData
        
        real(kind = 4), dimension(lbound(a2dVarValue, dim=1):ubound(a2dVarValue, dim=1), &
                                  lbound(a2dVarValue, dim=2):ubound(a2dVarValue, dim=2)) :: a2dVarValueUpd
        !------------------------------------------------------------------------------------
                                
        !------------------------------------------------------------------------------------
        ! Initialize variable
        a2dVarValueUpd = -9999.0
        !------------------------------------------------------------------------------------
                                  
        !------------------------------------------------------------------------------------
        ! Info
        call mprintf(.true., iINFO_Extra, ' Check variable limit (max and min) ... ')
        !------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------
        ! Check variable on mask domain
        if (present(dVarNoData)) then
            
            !------------------------------------------------------------------------------------
            ! Check value on mask
            where(a2iVarMask.ge.0.0)

                !------------------------------------------------------------------------------------
                ! Lower limit
                where(a2dVarValue.lt.dVarValueMin)
                        a2dVarValue = dVarNoData
                endwhere
                !------------------------------------------------------------------------------------

                !------------------------------------------------------------------------------------
                ! Upper limit
                where(a2dVarValue.gt.dVarValueMax)
                    a2dVarValue = dVarNoData
                endwhere
                !------------------------------------------------------------------------------------
            
            elsewhere
                
                !------------------------------------------------------------------------------------
                ! Mask undefined cell(s)
                a2dVarValue = dVarNoData
                !------------------------------------------------------------------------------------
                
            endwhere
            !------------------------------------------------------------------------------------
            
        else
            
            !------------------------------------------------------------------------------------
            ! Check value on mask
            dVarNoData = -9999.0
            where(a2iVarMask.ge.0.0)

                !------------------------------------------------------------------------------------
                ! Lower limit
                where(a2dVarValue.lt.dVarValueMin)
                        a2dVarValue = dVarValueMin
                endwhere
                !------------------------------------------------------------------------------------

                !------------------------------------------------------------------------------------
                ! Upper limit
                where(a2dVarValue.gt.dVarValueMax)
                    a2dVarValue = dVarValueMax
                endwhere
                !------------------------------------------------------------------------------------
            
            elsewhere
                
                !------------------------------------------------------------------------------------
                ! Mask undefined cell(s)
                a2dVarValue = dVarNoData
                !------------------------------------------------------------------------------------
                
            endwhere
            !------------------------------------------------------------------------------------
        
        endif
        !------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------
        ! Update variable
        a2dVarValueUpd = a2dVarValue
        !------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------
        ! Info
        call mprintf(.true., iINFO_Extra, ' Check variable limit (max and min) ... OK')
        !------------------------------------------------------------------------------------
        
    end function check2Dvar
    !------------------------------------------------------------------------------------
    
    !------------------------------------------------------------------------------------------
    ! Subroutine to open file unit
    subroutine HMC_Tools_Generic_SetUnit(iFileUnitMin, iFileUnitMax, iFileUnit)
        
        !------------------------------------------------------------------------------------------
        ! Variable(s) declaration 
        logical                         :: bIsUsed
        integer(kind = 4), intent(in)   :: iFileUnitMin, iFileUnitMax
        integer(kind = 4), intent(out)  :: iFileUnit
        integer(kind = 4)               :: iFileUnitInit
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Search debug unit
        do iFileUnitInit = iFileUnitMin, iFileUnitMax
            inquire(unit = iFileUnitInit, opened = bIsUsed)
            if (.not. bIsUsed) exit
        end do
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Pass to global variables
        iFileUnit = iFileUnitInit
        !------------------------------------------------------------------------------------------
        
    end subroutine HMC_Tools_Generic_SetUnit
    !------------------------------------------------------------------------------------------
    
    !------------------------------------------------------------------------------------------
    ! Subroutine to remove file
    subroutine HMC_Tools_Generic_RemoveFile(sCommandRemoveRaw, sFileName, bFatalError)
        
        !------------------------------------------------------------------------------------------
        ! Variable(s) declaration
        integer(kind = 4)                   :: iErr
        character(len = 700)                :: sFileName
        character(len = 700), intent(in)    :: sCommandRemoveRaw
        character(len = 700)                :: sCommandRemoveDef
        
        logical                             :: bFatalError
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Initialize variable(s)
        iErr = 0
        sCommandRemoveDef = sCommandRemoveRaw
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Define command line
        call HMC_Tools_Generic_ReplaceText(sCommandRemoveDef, 'filename', trim(sFileName))
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Call command liene using system to zip file
        call execute_command_line(sCommandRemoveDef, EXITSTAT = iErr) 
        if(iErr /= 0) then
            if (bFatalError) then
                call mprintf(.true., iERROR, 'Error in removing file: '//trim(sFileName) )
            else
                iErr = 0
                return
            endif
        endif
        !------------------------------------------------------------------------------------------
        
    end subroutine HMC_Tools_Generic_RemoveFile
    !------------------------------------------------------------------------------------------
        
    !------------------------------------------------------------------------------------------
    ! Subroutine to zip file
    subroutine HMC_Tools_Generic_ZipFile(sCommandZipRaw, sFileNameZip, sFileNameUnzip, bFatalError)
        
        !------------------------------------------------------------------------------------------
        ! Variable(s) declaration
        integer(kind = 4)                   :: iErr
        character(len = 700)                :: sFileNameZip, sFileNameUnzip
        character(len = 700), intent(in)    :: sCommandZipRaw
        character(len = 700)                :: sCommandZipDef
        
        logical                             :: bFatalError
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Initialize variable(s)
        iErr = 0
        sCommandZipDef = sCommandZipRaw
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Define command line
        call HMC_Tools_Generic_ReplaceText(sCommandZipDef, 'filenamezip', trim(sFileNameZip))
        call HMC_Tools_Generic_ReplaceText(sCommandZipDef, 'filenameunzip', trim(sFileNameUnzip))
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Call command liene using system to zip file
        call execute_command_line(sCommandZipDef, EXITSTAT = iErr) 
        if(iErr /= 0) then
            if (bFatalError) then
                call mprintf(.true., iERROR, 'Error in zipping file: '//trim(sFileNameUnzip) )
            else
                iErr = 0
                return
            endif
        endif
        !------------------------------------------------------------------------------------------
        
    end subroutine HMC_Tools_Generic_ZipFile
    !------------------------------------------------------------------------------------------
    
    !------------------------------------------------------------------------------------------
    ! Subroutine to unzip file
    subroutine HMC_Tools_Generic_UnzipFile(sCommandUnzipRaw, sFileNameZip, sFileNameUnzip, bFatalError)
        
        !------------------------------------------------------------------------------------------
        ! Variable(s) declaration
        integer(kind = 4)                   :: iErr
        character(len = 700)                :: sFileNameZip, sFileNameUnzip
        character(len = 700), intent(in)    :: sCommandUnzipRaw
        character(len = 700)                :: sCommandUnzipDef
        
        logical                             :: bFatalError
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Initialize variable(s)
        iErr = 0
        sCommandUnzipDef = sCommandUnzipRaw
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Define command line
        call HMC_Tools_Generic_ReplaceText(sCommandUnzipDef, 'filenamezip', trim(sFileNameZip))
        call HMC_Tools_Generic_ReplaceText(sCommandUnzipDef, 'filenameunzip', trim(sFileNameUnzip))
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Call command liene using system to unzip file
        call execute_command_line(sCommandUnzipDef, EXITSTAT = iErr) 
        if(iErr /= 0) then
            if (bFatalError) then
                call mprintf(.true., iERROR, 'Error in unzipping file: '//trim(sFileNameZip) )
            else
                iErr = 0
                return
            endif
        endif
        !------------------------------------------------------------------------------------------

    end subroutine HMC_Tools_Generic_UnzipFile
    !------------------------------------------------------------------------------------------
        
    !------------------------------------------------------------------------------------------
    ! Subroutine to create new folder
    subroutine HMC_Tools_Generic_CreateFolder(sCommandCreateFolder, sPathFolder, bFatalError)
        
        !------------------------------------------------------------------------------------------
        ! Variable(s) declaration
        integer(kind = 4)                       :: iErr
        character(len = 700)                    :: sPathFolder
        character(len = 700), intent(in)        :: sCommandCreateFolder
        
        logical                                 :: bFatalError
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Initialize variable(s)
        iErr = 0
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Define command line
        call HMC_Tools_Generic_ReplaceText(sCommandCreateFolder, 'path', sPathFolder)
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Call command line using system to create the folder
        call execute_command_line(trim(sCommandCreateFolder), EXITSTAT = iErr) 
        if(iErr /= 0) then
            if (bFatalError) then
                call mprintf(.true., iERROR, 'Error in creating folder: '//trim(sPathFolder) )
            else
                iErr = 0
                return
            endif
        endif
        !------------------------------------------------------------------------------------------

    end subroutine HMC_Tools_Generic_CreateFolder
    !------------------------------------------------------------------------------------------

    !------------------------------------------------------------------------------------------
    ! Subroutine to smooth data time series
    subroutine HMC_Tools_Generic_SmoothTimeSeries(a1dDataT, iRank, iNsmooth) 
        
        !------------------------------------------------------------------------------------------
        ! Variable(s) declaration
        integer(kind = 4)           :: it, iit
        integer(kind = 4)           :: iRank, iNsmooth
        real(kind = 4)              :: dNum, dNumT, dVdif, dSumA, dSumB, dMean
        
        real(kind = 4), dimension(iRank)    :: a1dDataT, a1dDataTmp
        !------------------------------------------------------------------------------------------

        !------------------------------------------------------------------------------------------
        ! Initialize variable(s)
        dNumT = 0; dVdif = 0
        dSumB = sum(a1dDataT, dim = 1)
        !iRank = size(a1dDataT,dim = 1)
        
        a1dDataTmp = 0
        a1dDataTmp = a1dDataT
        !------------------------------------------------------------------------------------------

        !------------------------------------------------------------------------------------------
        ! Smooth data 
        do it = 1 + iNsmooth, iRank - iNsmooth - 1
                dNum = 0; dMean = 0;
                if(a1dDataT(it).gt.0.0) dNumT = dNumT + 1

                do iit = it - iNsmooth, it + iNsmooth
                        if(a1dDataT(iit).ge.0.0)then
                                dMean = dMean + a1dDataT(iit)
                                dNum = dNum + 1
                        endif
                enddo
                if(dNum.gt.0)then
                        dMean = dMean/dNum
                        a1dDataTmp(it) = dMean
                endif
        enddo
        a1dDataT = a1dDataTmp
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Control data
        dSumA = sum(a1dDataT, dim = 1)
        if(dNumT.gt.0)then
            dVdif = (dSumB - dSumA)/dNumT
        endif
        ! Control V difference between the raw and generated data series
        if(dVdif.lt.0.0) then 
            where(a1dDataT.gt.abs(dVdif))
                a1dDataT = a1dDataT + dVdif
            endwhere
        else
            where(a1dDataT.gt.0)
                a1dDataT = a1dDataT + dVdif
            endwhere
        endif
        !------------------------------------------------------------------------------------------
   
    end subroutine HMC_Tools_Generic_SmoothTimeSeries
    !------------------------------------------------------------------------------------------

    !------------------------------------------------------------------------------------------
    ! Subroutine to realize grid switcher between land-forcing data
    subroutine HMC_Tools_Generic_SwitchGrid(iFlagGrid, &
                                    iRowsL, iColsL, a2dVarL, &
                                    iRowsF, iColsF, a2dVarF, &
                                    a2dVarMask, a2iVarIndexX, a2iVarIndexY, &
                                    dNoData_Opt)
                                    
        !------------------------------------------------------------------------------------
        ! Variable(s) declaration
        integer(kind = 4)       :: iFlagGrid
        integer(kind = 4)       :: iI, iJ, iRowsL, iColsL, iRowsF, iColsF
        
        real(kind = 4),  intent(in),     dimension(iRowsF, iColsF)   :: a2dVarF
        real(kind = 4),  intent(out),    dimension(iRowsL, iColsL)   :: a2dVarL
        
        integer(kind = 4),  intent(in),     dimension(iRowsL, iColsL)   :: a2iVarIndexX, a2iVarIndexY
        real(kind = 4),     intent(in),     dimension(iRowsL, iColsL)   :: a2dVarMask
        
        real(kind = 4), optional                                        :: dNoData_Opt
        real(kind = 4)                                                  :: dNoData_Set    
        
        real(kind = 4), parameter                                       :: dNoData_Def = -9999.0
        !------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------
        ! Initialize variable(s)
        iI = 0; iJ = 0;
        a2dVarL = -9999.0;
        !------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------
        ! Set no data value
        dNoData_Set = dNoData_Def
        if (present(dNoData_Opt)) dNoData_Set = dNoData_Opt
        !------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------
        ! Check flag condition
        call mprintf(.true., iINFO_Extra, ' Switching grid procedure ...')
        if (iFlagGrid .eq. 1) then
            
            !------------------------------------------------------------------------------------
            ! Info
            call mprintf(.true., iINFO_Extra, ' Grids are different!')
            !------------------------------------------------------------------------------------
            
            !------------------------------------------------------------------------------------
            ! Check values 
            if (any(a2dVarF.ne.dNoData_Set)) then
            
                !------------------------------------------------------------------------------------
                ! Regrid F data on L data using X and Y indexes
                forall(iI = 1:iRowsL, iJ = 1:iColsL, a2dVarMask(iI,iJ) .ge. 0.0)
                        a2dVarL(iI, iJ) = a2dVarF(a2iVarIndexY(iI, iJ), a2iVarIndexX(iI,iJ))
                endforall
                ! Info
                call mprintf(.true., iINFO_Extra, ' Switching grid procedure ... OK')
                !------------------------------------------------------------------------------------
            
            else
                !------------------------------------------------------------------------------------
                ! Data not defined --> array initialized with nodata value
                a2dVarL = dNoData_Set
                !------------------------------------------------------------------------------------
                
            endif
            !------------------------------------------------------------------------------------
            
        elseif (iFlagGrid .eq. 0) then
            
            !------------------------------------------------------------------------------------
            ! Info
            call mprintf(.true., iINFO_Extra, ' Grids are equal!')
            !------------------------------------------------------------------------------------
            
            !------------------------------------------------------------------------------------
            ! Compute output results 
            a2dVarL = a2dVarF
            ! Info
            call mprintf(.true., iINFO_Extra, ' Switching grid procedure ... SKIPPED')
            !------------------------------------------------------------------------------------
            
        else
            
            !------------------------------------------------------------------------------------
            ! Exit
            call mprintf(.true., iERROR, ' Incorrect switch flag grid ' )
            !------------------------------------------------------------------------------------
            
        endif
        !------------------------------------------------------------------------------------

    end subroutine HMC_Tools_Generic_SwitchGrid
    !------------------------------------------------------------------------------------------
    
    !------------------------------------------------------------------------------------------
    ! Subroutine to compute indexes between land-forcing grids
    subroutine HMC_Tools_Generic_CreateIndexGrid(idim_land, jdim_land, idim_forcing, jdim_forcing, &
                                         lat_land, lon_land, lat_forcing, lon_forcing, &
                                         lat_res_land, lon_res_land, lat_res_forcing, lon_res_forcing, &
                                         flag, &
                                         a2iVarXIndex, a2iVarYIndex)
        
        !------------------------------------------------------------------------------------------
        ! Variable(s) declaration
        integer(kind = 4)       :: i, j, flag 
        integer(kind = 4)       :: idim_land, jdim_land, idim_forcing, jdim_forcing
       
        real(kind = 4)          :: lat_land, lon_land, lat_forcing, lon_forcing
        real(kind = 4)          :: lat_res_land, lon_res_land, lat_res_forcing, lon_res_forcing
        
        integer(kind = 4)       :: iny, inx
        real(kind = 4)          :: rlat, rlon, nx, ny
        
        integer(kind = 4), intent(inout), dimension(idim_land, jdim_land)   :: a2iVarXIndex, a2iVarYIndex
        
        logical                 :: bWarnXIdx, bWarnYIdx
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Initialize variable(s)
        rlat = -9999.0; rlon = -9999.0; nx = -9999.0; ny = -9999.0;
        a2iVarXIndex = -9999; a2iVarYIndex = -9999;
        
        bWarnXIdx = .false.; bWarnYIdx = .false.
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Check grids
        if((lat_land.ne.lat_forcing).or.(lon_land.ne.lon_forcing).or.(lon_res_land.ne.lon_res_forcing)) then
            
            !------------------------------------------------------------------------------------------
            ! Info
            call mprintf(.true., iINFO_Main, ' Land and forcing grids are different ---> Create indexes arrays ')
            !------------------------------------------------------------------------------------------
            
            !------------------------------------------------------------------------------------------
            ! Cycling on dim(s)
            do i = 1, idim_land
                do j = 1, jdim_land

                    ! Find land point latitude and longitude
                    rlat = lat_land + (lat_res_land*(i - 1))
                    rlon = lon_land + (lon_res_land*(j - 1))
                    
                    ! Find x,y indexes between forcing and land domains
                    nx = nint((rlon - lon_forcing)/lon_res_forcing) + 1
                    ny = nint((rlat - lat_forcing)/lat_res_forcing) + 1
                    
                    ! Warning message(s)
                    if ( (nx.gt.jdim_forcing) .and. (.not.bWarnXIdx) ) then
                        call mprintf(.true., iWARN, ' Some land x indexes greater then maximum forcing x index ' )
                        bWarnXIdx = .true.
                    endif
                    if ( (ny.gt.idim_forcing) .and. (.not.bWarnYIdx) ) then
                        call mprintf(.true., iWARN, ' Some land y indexes greater then maximum forcing y index ' )
                        bWarnYIdx = .true.
                    endif
                    
                    ! Case 0 --> x,y less then maximum forcing indexes
                    if ( (nx.gt.0.and.nx.le.jdim_forcing) .and. (ny.gt.0.and.ny.le.idim_forcing)) then

                        iny = int(ny)
                        inx = int(nx)

                        a2iVarYIndex(i,j) = iny
                        a2iVarXIndex(i,j) = inx
                    
                    ! Case 1 --> x greater then x forcing index
                    elseif ( (nx.gt.0.and.nx.gt.jdim_forcing) .and. (ny.gt.0.and.ny.le.idim_forcing)) then
                                
                        if (rlon - (lon_forcing+jdim_forcing*lon_res_forcing) .le. lon_res_land) then

                            iny = int(ny)
                            inx = jdim_forcing

                            a2iVarYIndex(i,j) = iny
                            a2iVarXIndex(i,j) = inx  

                        endif
                    
                    ! Case 2 --> y greater then y forcing index
                    elseif ( (nx.gt.0.and.nx.le.jdim_forcing) .and. (ny.gt.0.and.ny.gt.idim_forcing) ) then  

                        if (rlat - (lat_forcing+idim_forcing*lat_res_forcing) .le. lat_res_land) then   

                            iny = idim_forcing
                            inx = int(nx)

                            a2iVarYIndex(i,j) = iny
                            a2iVarXIndex(i,j) = inx  

                        endif  
                        
                    ! Case 3 --> x greater then x forcing index and y greater then y forcing index            
                    elseif ( (nx.gt.0.and.nx.gt.jdim_forcing) .and. (ny.gt.0.and.ny.gt.idim_forcing) ) then  

                        if ( (rlon - (lon_forcing+jdim_forcing*lon_res_forcing) .le. lon_res_land) &
                        .and. (rlat - (lat_forcing+idim_forcing*lat_res_forcing) .le. lat_res_land) ) then   

                            iny = idim_forcing
                            inx = jdim_forcing

                            a2iVarYIndex(i,j) = iny
                            a2iVarXIndex(i,j) = inx  

                        endif  
                                
                    endif
                    
                enddo
            enddo
            !------------------------------------------------------------------------------------------
            
            !------------------------------------------------------------------------------------------
            ! Exit flag code
            flag = 1
            !------------------------------------------------------------------------------------------
        else
            !------------------------------------------------------------------------------------------
            ! Info
            call mprintf(.true., iINFO_Main, ' Land and forcing grids are equals ')
            !------------------------------------------------------------------------------------------
            
            !------------------------------------------------------------------------------------------
            ! Exit flag code
            flag = 0
            ! Grids are the same
            a2iVarXIndex = -9999; 
            a2iVarYIndex = -9999;
            !------------------------------------------------------------------------------------------
        endif
        !------------------------------------------------------------------------------------------

    end subroutine HMC_Tools_Generic_CreateIndexGrid
    !------------------------------------------------------------------------------------------

    !------------------------------------------------------------------------------------------
    ! Subroutine to find length of unknown file
    subroutine HMC_Tools_Generic_FindFileLength(iIDFile, iI, iSkipRows)
        
        integer(kind = 4)           :: iIDFile
        integer(kind = 4)           :: iI, iIO, iSkipRows
        
        iI = 0
        do
            read(iIDFile, *, IOSTAT = iIO)
            IF (iIO > 0) THEN
                call mprintf(.true., iWARN, ' File length failed! Check input. Something was wrong!')
                iI = -9999
                exit
            elseif (iIO < 0) THEN
                exit
            else
               iI = iI + 1
            endif
            
        enddo
        
        iI = iI - iSkipRows
        
        ! Rewind file 
        rewind(iIDFile)
        
    end subroutine HMC_Tools_Generic_FindFileLength
    !------------------------------------------------------------------------------------------

    !--------------------------------------------------------------------------------   
    ! Subroutine to replace string with another string
    subroutine HMC_Tools_Generic_ReplaceText (s,text,rep) 
        
        !--------------------------------------------------------------------------------
        ! Variable(s) declaration
        character(*)            :: s,text,rep
        character(len(s)+700)   :: outs     ! provide outs with extra 700 char len
        integer                 :: i, nt, nr
        !--------------------------------------------------------------------------------
        
        !--------------------------------------------------------------------------------
        ! Replace text (string) with rep (another string)
        outs = s ; nt = LEN_TRIM(text) ; nr = LEN_TRIM(rep)
        do
            i = INDEX(outs,text(:nt)) ; IF (i == 0) exit
            outs = outs(:i-1) // rep(:nr) // outs(i+nt:)
        enddo
        !--------------------------------------------------------------------------------
        
        !--------------------------------------------------------------------------------
        ! Output s (string)
        s = outs
        !--------------------------------------------------------------------------------

    end subroutine HMC_Tools_Generic_ReplaceText
    !--------------------------------------------------------------------------------  
    
end module HMC_Module_Tools_Generic
!--------------------------------------------------------------------------------  
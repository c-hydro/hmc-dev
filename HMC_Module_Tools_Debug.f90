!------------------------------------------------------------------------------------------     
! File:   HMC_Module_Tools_Debug.f90
! Author: Fabio Delogu
! Created on May 12, 2015, 2:54 PM
!
! Module to define debug tools
!------------------------------------------------------------------------------------------

!------------------------------------------------------------------------------------------
! Module header
module HMC_Module_Tools_Debug
    
    !------------------------------------------------------------------------------------------
    ! Declaration of global variable(s)
    implicit none

    integer(kind = 4)   :: iDEBUG = 0
    integer(kind = 4), parameter :: iINFO_Basic = 0, iINFO_Main = 1, iINFO_Verbose = 2, iINFO_Extra = 3
    integer(kind = 4), parameter :: iWARN = 20, iERROR = 30
    
    integer(kind = 4) :: iFlagDebugSet
    integer(kind = 4) :: iDebugLevelSet = iINFO_Basic
    integer(kind = 4) :: iDebugUnit = -1
    
    logical :: bDebugLogUnit = .false.
    logical :: bDebugLogName = .false.

    logical :: bLineLogFile = .false.
    logical :: bLineDebug = .false.
    logical :: bLineInfo = .false.
    logical :: bLineWarn = .false.
    logical :: bLineError = .false.
    !------------------------------------------------------------------------------------------

contains
    
    !------------------------------------------------------------------------------------------
    ! Subroutine to set debug unit
    subroutine HMC_Tools_Debug_SetUnit(iDebugUnitMin, iDebugUnitMax, iDebugUnitInit)
        
        !------------------------------------------------------------------------------------------
        ! Variable(s) declaration 
        logical                         :: bIsUsed
        integer(kind = 4)               :: iDebugUnitMin, iDebugUnitMax
        integer(kind = 4),intent(out)   :: iDebugUnitInit
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Initialization of debug unit
        iDebugUnitInit = -1
        
        ! Search debug unit
        do iDebugUnitInit = iDebugUnitMin, iDebugUnitMax
            inquire(unit = iDebugUnitInit, opened = bIsUsed)
            if (.not. bIsUsed) exit
        end do
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Pass to global variables
        iDebugUnit = iDebugUnitInit
        bDebugLogUnit = .true.
        !------------------------------------------------------------------------------------------
        
    end subroutine HMC_Tools_Debug_SetUnit
    !------------------------------------------------------------------------------------------

    !------------------------------------------------------------------------------------------
    ! Subroutine to set debug level
    subroutine HMC_Tools_Debug_SetLevel(iFlagDebugSetInit, iDebugLevelInit)

        !------------------------------------------------------------------------------------------
        ! Variable(s) declaration 
        integer(kind = 4), intent(in) :: iFlagDebugSetInit, iDebugLevelInit
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Set debug level
        if (iFlagDebugSetInit .gt. 0) then
            
            !------------------------------------------------------------------------------------------
            ! Pass to global variables
            iDEBUG = iFlagDebugSetInit
            iDebugLevelSet = iDebugLevelInit
            !------------------------------------------------------------------------------------------
        else
            !------------------------------------------------------------------------------------------
            ! Debug level == 0; info == iINFO_Basic
            iDEBUG = 0
            iDebugLevelSet = iINFO_Basic
            !------------------------------------------------------------------------------------------
        endif
        !------------------------------------------------------------------------------------------

    end subroutine HMC_Tools_Debug_SetLevel
    !------------------------------------------------------------------------------------------

    !------------------------------------------------------------------------------------------
    ! Subroutine to print debug message
    !    call mprintf(.true., iINFO_Basic, ' Check basic information ')
    !    call mprintf(.true., iINFO_Main, ' Check main information ')
    !    call mprintf(.true., iINFO_Verbose, ' Check verbose information ')
    !    call mprintf(.true., iINFO_Extra, ' Check extra information ')
    !    call mprintf(.true., iWARN, ' Check warning')       
    !    call mprintf(.true., iERROR, ' Check error')
    subroutine mprintf(bAssertion, iDebugLevelCall, sFmtString)
                                       
        !------------------------------------------------------------------------------------------
        ! Arguments
        logical, intent(in)             :: bAssertion
        integer, intent(in)             :: iDebugLevelCall
        character (len=*), intent(in)   :: sFmtString
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Local variables 
        integer(kind = 4)               :: iStrStart, iStrEnd

        character (len=8)               :: sCurrenteDate
        character (len=10)              :: sCurrentTime
        character (len=10)              :: sPrintDate
        character (len=12)              :: sPrintTime

        character (len=1024)            :: sFileName
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Check level debug call, if less then debug level set, then return
        if (( iDebugLevelCall .ne. iERROR ) .and. ( iDebugLevelCall .ne. iWARN) ) then 
            if (iDebugLevelCall .gt. iDebugLevelSet) return
        endif
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Set date and time
        call date_and_time(date=sCurrenteDate, time=sCurrentTime)
        write(sPrintDate,'(a10)') sCurrenteDate(1:4)//'-'//sCurrenteDate(5:6)//'-'//sCurrenteDate(7:8)
        write(sPrintTime,'(a12)') sCurrentTime(1:2)//':'//sCurrentTime(3:4)//':'//sCurrentTime(5:10)
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Open file (hmc.log)
        if (.not. bDebugLogName) then
            sFileName = 'hmc.log'
            
            if (.not. bDebugLogUnit) then
                call HMC_Tools_Debug_SetUnit(80, 100, iDebugUnit)
                bDebugLogUnit = .true.
            endif
                
            open(unit=iDebugUnit, file = trim(sFileName), status = 'replace', form ='formatted')
            
            write(iDebugUnit,'(a)') '----------------------------------------------'
            write(iDebugUnit,'(a)') ' HMC LOGGER '
            write(iDebugUnit,'(a)') ' StartLog: '//sPrintDate//' '//sPrintTime
            write(iDebugUnit,'(a)') '----------------------------------------------'
            bDebugLogName = .true.
        end if
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Initialize variable(s)
        iStrStart = 1; iStrEnd = len_trim(sFmtString)
        !------------------------------------------------------------------------------------------

        !------------------------------------------------------------------------------------------
        ! Check assertion
        if (bAssertion) then
           
            !------------------------------------------------------------------------------------------
            ! Check info level (1=Main, 4=Verbose, 5=Extra; 2=Warning; 3=Error)
            if ( iDebugLevelCall .eq. iINFO_Basic) then      
                write(iDebugUnit,'(a)') ' ['//sPrintDate//' '//sPrintTime//'] INFO:    '//sFmtString(iStrStart:iStrEnd)!//achar(10)
                write(6,*) ' ----> '//sFmtString(iStrStart:iStrEnd)
            
            elseif ( iDebugLevelCall .eq. iINFO_Main) then      
                write(iDebugUnit,'(a)') ' ['//sPrintDate//' '//sPrintTime//'] INFO:    '//sFmtString(iStrStart:iStrEnd)!//achar(10)
                write(6,*) ' ----> '//sFmtString(iStrStart:iStrEnd)
                
            elseif ( iDebugLevelCall .eq. iINFO_Verbose) then      
                write(iDebugUnit,'(a)') ' ['//sPrintDate//' '//sPrintTime//'] INFO:    '//sFmtString(iStrStart:iStrEnd)!//achar(10)
                write(6,*) ' ----> '//sFmtString(iStrStart:iStrEnd)
                
            elseif ( iDebugLevelCall .eq. iINFO_Extra) then      
                write(iDebugUnit,'(a)') ' ['//sPrintDate//' '//sPrintTime//'] INFO:    '//sFmtString(iStrStart:iStrEnd)!//achar(10)
                write(6,*) ' ----> '//sFmtString(iStrStart:iStrEnd)
                
            elseif ( iDebugLevelCall .eq. iWARN) then      
                write(iDebugUnit,'(a)') ' ['//sPrintDate//' '//sPrintTime//'] WARNING: '//sFmtString(iStrStart:iStrEnd) !//achar(10)
                write(6,*) ' ------> WARNING: '//sFmtString(iStrStart:iStrEnd)
                
            elseif ( iDebugLevelCall .eq. iERROR) then      
                write(iDebugUnit,'(a)') ' ['//sPrintDate//' '//sPrintTime//'] ERROR:   '//sFmtString(iStrStart:iStrEnd)!//achar(10)
                write(6,*) ' --------> ERROR: '//sFmtString(iStrStart:iStrEnd)
                
            endif
            !------------------------------------------------------------------------------------------
            
            !------------------------------------------------------------------------------------------
            ! Flush on file (to update each step or comment to update at the end of program)
            call flush(iDebugUnit)      
            !------------------------------------------------------------------------------------------
                   
            !------------------------------------------------------------------------------------------
            ! Level Debug == Error
            if (iDebugLevelCall == iERROR) then
                close(iDebugUnit)
                stop "Stopped"
            endif
            !------------------------------------------------------------------------------------------
            
      end if
      !------------------------------------------------------------------------------------------
      
    end subroutine mprintf
    !------------------------------------------------------------------------------------------
    
    !------------------------------------------------------------------------------------------
    ! Function to check variable 2d (max, min and mean values) 
    function checkvar(a2dVarValue, a2iVarMask, sVarName)   result(sVarCheck)
        
        !------------------------------------------------------------------------------------
        ! Variable(s) declaration
        integer(kind = 4), dimension(:,:)   :: a2iVarMask
        real(kind = 4), dimension(:,:)      :: a2dVarValue
        
        character(len = *), optional       :: sVarName
        
        real(kind = 4)                      :: dVarMaxValue, dVarMinValue, dVarMeanValue
        
        character(len = 20)                 :: sVarMaxValue, sVarMinValue, sVarMeanValue
        character(len = 200)                :: sVarCheck
        !------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------
        ! Initialize variable(s)
        dVarMaxValue = -9999.0; dVarMinValue = -9999.0; dVarMeanValue = -9999.0
        sVarCheck = ""
        !------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------
        ! Define name if not defined
        if (.not. present(sVarName)) then
            sVarName = 'VAR'
        endif
        !------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------
        ! Compute maximum value
        dVarMaxValue = maxval(a2dVarValue, mask=a2iVarMask.gt.0.0)
        write(sVarMaxValue, *) dVarMaxValue
        ! Compute minimum value
        dVarMinValue = minval(a2dVarValue, mask=a2iVarMask.gt.0.0)
        write(sVarMinValue, *) dVarMinValue
        ! Compute average value
        dVarMeanValue = sum(a2dVarValue, mask=a2iVarMask.gt.0.0)/max(1,count(a2iVarMask.gt.0.0))
        write(sVarMeanValue, *) dVarMeanValue
        !------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------
        ! Print check message
        sVarCheck = trim(sVarName)//" :: Max: "//trim(sVarMaxValue)//" - Min: "//trim(sVarMinValue)// &
                    " - Mean: "//trim(sVarMeanValue)
        !------------------------------------------------------------------------------------
        
    end function checkvar
    !------------------------------------------------------------------------------------
    
    !------------------------------------------------------------------------------------------
    ! Function to check variable 1d (max, min and mean values) 
    function checkarray(a1dVarValue, sVarName)   result(sVarCheck)
        
        !------------------------------------------------------------------------------------
        ! Variable(s) declaration
        real(kind = 4), dimension(:)      :: a1dVarValue
        
        character(len = *), optional       :: sVarName
        
        real(kind = 4)                      :: dVarMaxValue, dVarMinValue, dVarMeanValue
        
        character(len = 20)                 :: sVarMaxValue, sVarMinValue, sVarMeanValue
        character(len = 200)                :: sVarCheck
        !------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------
        ! Initialize variable(s)
        dVarMaxValue = -9999.0; dVarMinValue = -9999.0; dVarMeanValue = -9999.0
        sVarCheck = ""
        !------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------
        ! Define name if not defined
        if (.not. present(sVarName)) then
            sVarName = 'VAR'
        endif
        !------------------------------------------------------------------------------------

        !------------------------------------------------------------------------------------
        ! Compute maximum value
        dVarMaxValue = maxval(a1dVarValue, mask=a1dVarValue.ge.0.0)
        write(sVarMaxValue, *) dVarMaxValue
        ! Compute minimum value
        dVarMinValue = minval(a1dVarValue, mask=a1dVarValue.ge.0.0)
        write(sVarMinValue, *) dVarMinValue
        ! Compute average value
        dVarMeanValue = sum(a1dVarValue, mask=a1dVarValue.ge.0.0)/max(1,count(a1dVarValue.gt.0.0))
        write(sVarMeanValue, *) dVarMeanValue
        !------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------
        ! Print check message
        sVarCheck = trim(sVarName)//" :: Max: "//trim(sVarMaxValue)//" - Min: "//trim(sVarMinValue)// &
                    " - Mean: "//trim(sVarMeanValue)
        !------------------------------------------------------------------------------------
        
    end function checkarray
    !------------------------------------------------------------------------------------
    
end module HMC_Module_Tools_Debug
!------------------------------------------------------------------------------------------
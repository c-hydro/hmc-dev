!------------------------------------------------------------------------------------------     
! File:   HMC_Module_Tools_Time.f90
! Author: Fabio Delogu
! Created on March 24, 2014, 1:25 PM
!
! Module to define time information
!------------------------------------------------------------------------------------------

!------------------------------------------------------------------------------------------
! Module header
module HMC_Module_Tools_Time
    
    !------------------------------------------------------------------------------------------
    ! Implicit none for all subroutines in this module
    implicit none
    !------------------------------------------------------------------------------------------
    
contains   

    !------------------------------------------------------------------------------------------
    ! Subroutine to get time now
    subroutine HMC_Tools_Time_Printer(sPrintDate, sPrintTime)
        
        !------------------------------------------------------------------------------------------
        ! Variable(s) declaration
        character (len=8)               :: sCurrenteDate
        character (len=10)              :: sCurrentTime
        character (len=10), intent(out) :: sPrintDate
        character (len=12), intent(out) :: sPrintTime
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Initialize variable(s)
        sCurrenteDate = ""; sCurrentTime = "";
        sPrintDate = ""; sPrintTime = ""
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Get date and time
        call date_and_time(date=sCurrenteDate, time=sCurrentTime)
        write(sPrintDate,'(a10)') sCurrenteDate(1:4)//'-'//sCurrenteDate(5:6)//'-'//sCurrenteDate(7:8)
        write(sPrintTime,'(a12)') sCurrentTime(1:2)//':'//sCurrentTime(3:4)//':'//sCurrentTime(5:10)
        !------------------------------------------------------------------------------------------
    
    end subroutine HMC_Tools_Time_Printer
    !------------------------------------------------------------------------------------------

    !------------------------------------------------------------------------------------------
    ! Subroutine to define monthly value
    subroutine HMC_Tools_Time_MonthVal(a12, nowdate, nowval) 
      !
      ! Given a set of 12 values, taken to be valid on the fifteenth of each month (Jan through Dec)
      ! and a date in the form <YYYYMMDD[HHmmss]> ....
      ! 
      ! Return a value valid for the day given in <nowdate>, as an interpolation from the 12
      ! monthly values.
      !

      implicit none
      real(kind=4), dimension(12), intent(in) :: a12 ! 12 monthly values, taken to be valid on the 15th of
      !                                      ! the month
      character(len=12), intent(in) :: nowdate ! Date, in the form <YYYYMMDD[HHmmss]>
      integer(kind = 4) :: nowy, nowm, nowd
      integer(kind = 4) :: prevm, postm
      real(kind=4)    :: factor
      integer(kind = 4), dimension(12) :: ndays = (/ 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 /)

      real(kind = 4) :: nowval
      
      integer, external     :: nfeb

      !
      ! Handle leap year by setting the number of days in February for the year in question.
      !
      read(nowdate(1:8),'(I4,I2,I2)') nowy, nowm, nowd
      ndays(2) = nfeb_yw(nowy)

      !
      ! Do interpolation between the fifteenth of two successive months.
      !
      if (nowd == 15) then
         nowval = a12(nowm)
         return
      else if (nowd < 15) then
         postm = nowm
         prevm = nowm - 1
         if (prevm == 0) prevm = 12
         factor = real(ndays(prevm)-15+nowd)/real(ndays(prevm))
      else if (nowd > 15) then
         prevm = nowm
         postm = nowm + 1
         if (postm == 13) postm = 1
         factor = real(nowd-15)/real(ndays(prevm))
      endif

      nowval = a12(prevm)*(1.0-factor) + a12(postm)*factor

    end subroutine HMC_Tools_Time_MonthVal
    !------------------------------------------------------------------------------------------

    !------------------------------------------------------------------------------------------
    ! Function for sweeping blanks
    character(30) function sweep_blanks(in_str)
    character(*), intent(in) :: in_str
    character(30) :: out_str
    character :: ch
    integer :: j

    out_str = " "
    do j=1, len_trim(in_str)
        ! get j-th char
        ch = in_str(j:j)
        if (ch .ne. " ") then
            out_str = trim(out_str) // ch
        endif
        sweep_blanks = trim((out_str)) 
    end do
    
    end function sweep_blanks
    !------------------------------------------------------------------------------------------
    
    !------------------------------------------------------------------------------------------
    ! Function for computing the number of days in February (given year)
    integer function nfeb_yw(year)
        
        !------------------------------------------------------------------------------------------
        implicit none
        integer, intent(in) :: year ! Four-digit year
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Testing given year and select number of days
        
        nfeb_yw = 28 ! By default, February has 28 days ...
        if (mod(year,4).eq.0) then
           nfeb_yw = 29  ! But every four years, it has 29 days ...
           if (mod(year,100).eq.0) then
              nfeb_yw = 28  ! Except every 100 years, when it has 28 days ...
              if (mod(year,400).eq.0) then
                 nfeb_yw = 29  ! Except every 400 years, when it has 29 days ...
                 if (mod(year,3600).eq.0) then
                    nfeb_yw = 28  ! Except every 3600 years, when it has 28 days.
                 endif
              endif
           endif
        endif
        !------------------------------------------------------------------------------------------

    end function nfeb_yw
    !------------------------------------------------------------------------------------------
    
    !------------------------------------------------------------------------------------------
    ! Subroutine geth_newdate
    subroutine HMC_Tools_Time_GetNewDate (ndate, odate, idt)
        
        !------------------------------------------------------------------------------------------
        ! Implicit none and external libraries
        implicit none
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        !  From old date ("YYYY-MM-DD HH:MM:SS.ffff" or "YYYYMMDDHHMMSSffff") and 
        !  delta-time, compute the new date.

        !  on entry     -  odate  -  the old hdate.
        !                  idt    -  the change in time

        !  on exit      -  ndate  -  the new hdate.

        integer, intent(in)           :: idt
        
        
        character (len= 19),  intent(inout)  :: odate
        character (len= 19),  intent(out) :: ndate
        
        !  Local Variables

        !  yrold    -  indicates the year associated with "odate"
        !  moold    -  indicates the month associated with "odate"
        !  dyold    -  indicates the day associated with "odate"
        !  hrold    -  indicates the hour associated with "odate"
        !  miold    -  indicates the minute associated with "odate"
        !  scold    -  indicates the second associated with "odate"

        !  yrnew    -  indicates the year associated with "ndate"
        !  monew    -  indicates the month associated with "ndate"
        !  dynew    -  indicates the day associated with "ndate"
        !  hrnew    -  indicates the hour associated with "ndate"
        !  minew    -  indicates the minute associated with "ndate"
        !  scnew    -  indicates the second associated with "ndate"

        !  mday     -  a list assigning the number of days in each month

        !  i        -  loop counter
        !  nday     -  the integer number of days represented by "idt"
        !  nhour    -  the integer number of hours in "idt" after taking out
        !              all the whole days
        !  nmin     -  the integer number of minutes in "idt" after taking out
        !              all the whole days and whole hours.
        !  nsec     -  the integer number of minutes in "idt" after taking out
        !              all the whole days, whole hours, and whole minutes.

        integer :: newlen, oldlen
        integer :: yrnew, monew, dynew, hrnew, minew, scnew, frnew
        integer :: yrold, moold, dyold, hrold, miold, scold, frold
        integer :: nday, nhour, nmin, nsec, nfrac, i, ifrc
        logical :: opass
        character (len=10) :: hfrc
        character (len=1) :: sp
        logical :: punct
        integer :: yrstart, yrend, mostart, moend, dystart, dyend
        integer :: hrstart, hrend, mistart, miend, scstart, scend, frstart
        integer :: units
        integer, dimension(12) :: mday = (/31,28,31,30,31,30,31,31,30,31,30,31/)
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Determine if odate is "YYYY-MM-DD_HH ... " or "YYYYMMDDHH...."
        if (odate(5:5) == "-") then
           punct = .TRUE.
        else
           punct = .FALSE.
        endif
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Break down old hdate into parts
        hrold = 0
        miold = 0
        scold = 0
        frold = 0
        oldlen = LEN((odate))
        if (punct) then
            yrstart = 1
            yrend = 4
            mostart = 6
            moend = 7
            dystart = 9
            dyend = 10
            hrstart = 12
            hrend = 13
            mistart = 15
            miend = 16
            scstart = 18
            scend = 19
            frstart = 21
           
            select case (oldlen)
            case (10)
               ! Days
               units = 1
            case (13)
               ! Hours
               units = 2
            case (16)
               ! Minutes
               units = 3
            case (19)
               ! Seconds
               units = 4
            case (21)
               ! Tenths
               units = 5
            case (22)
               ! Hundredths
               units = 6
            case (23)
               ! Thousandths
               units = 7
            case (24)
               ! Ten thousandths
               units = 8
            case default
            write(*,*) 'ERROR: geth_newdate:  odd length: #'//trim(odate)//'#'
            stop
            end select

            if (oldlen.ge.11) then
               sp = odate(11:11)
            else
               sp = ' '
            end if

        else

            yrstart = 1
            yrend = 4
            mostart = 5
            moend = 6
            dystart = 7
            dyend = 8
            hrstart = 9
            hrend = 10
            mistart = 11
            miend = 12
            scstart = 13
            scend = 14
            frstart = 15

            select case (oldlen)
            case (8)
               ! Days
               units = 1
            case (10)
               ! Hours
               units = 2
            case (12)
               ! Minutes
               units = 3
            case (14)
               ! Seconds
               units = 4
            case (15)
               ! Tenths
               units = 5
            case (16)
               ! Hundredths
               units = 6
            case (17)
               ! Thousandths
               units = 7
            case (18)
               ! Ten thousandths
               units = 8
            case default
            write(*,*) 'ERROR: geth_newdate:  odd length: #'//trim(odate)//'#'
            
            stop
            end select
            
        endif
        !------------------------------------------------------------------------------------------

        !------------------------------------------------------------------------------------------
        !  Use internal READ statements to convert the CHARACTER string
        !  date into INTEGER components.
        read(odate(yrstart:yrend),  '(i4)') yrold
        read(odate(mostart:moend),  '(i2)') moold
        read(odate(dystart:dyend), '(i2)') dyold
        if (units.ge.2) then
           read(odate(hrstart:hrend),'(i2)') hrold
           if (units.ge.3) then
              read(odate(mistart:miend),'(i2)') miold
              if (units.ge.4) then
                 read(odate(scstart:scend),'(i2)') scold
                 if (units.ge.5) then
                    read(odate(frstart:oldlen),*) frold
                 end if
              end if
           end if
        end if
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        !  Set the number of days in February for that year.
        mday(2) = nfeb_yw(yrold)
        !------------------------------------------------------------------------------------------

        !------------------------------------------------------------------------------------------
        !  Check that ODATE makes sense.
        opass = .TRUE.

        !  Check that the month of ODATE makes sense.
        if ((moold.gt.12).or.(moold.lt.1)) then
            write(*,*) 'GETH_NEWDATE:  Month of ODATE = ', moold
            opass = .FALSE.
        end if

        !  Check that the day of ODATE makes sense.
        if ((dyold.gt.mday(moold)).or.(dyold.lt.1)) then
            write(*,*) 'GETH_NEWDATE:  Day of ODATE = ', dyold
            opass = .FALSE.
        end if

        !  Check that the hour of ODATE makes sense.
        if ((hrold.gt.23).or.(hrold.lt.0)) then
            write(*,*) 'GETH_NEWDATE:  Hour of ODATE = ', hrold
            opass = .FALSE.
        end if

        !  Check that the minute of ODATE makes sense.
        if ((miold.gt.59).or.(miold.lt.0)) then
            write(*,*) 'GETH_NEWDATE:  Minute of ODATE = ', miold
            opass = .FALSE.
        end if

        !  Check that the second of ODATE makes sense.
        if ((scold.gt.59).or.(scold.lt.0)) then
            write(*,*) 'GETH_NEWDATE:  Second of ODATE = ', scold
            opass = .FALSE.
        end if

        !  Check that the fractional part  of ODATE makes sense.
        if (.not.opass) then
            write(*,*) 'Crazy ODATE: ', odate(1:oldlen), oldlen
            stop
        end if
        
        ! --> Date Checks are completed.  Continue.
        !------------------------------------------------------------------------------------------

        !------------------------------------------------------------------------------------------
        !  Compute the number of days, hours, minutes, and seconds in idt
        if (units.ge.5) then !idt should be in fractions of seconds
            ifrc = oldlen-(frstart)+1
            ifrc = 10**ifrc
            nday   = abs(idt)/(86400*ifrc)
            nhour  = mod(abs(idt),86400*ifrc)/(3600*ifrc)
            nmin   = mod(abs(idt),3600*ifrc)/(60*ifrc)
            nsec   = mod(abs(idt),60*ifrc)/(ifrc)
            nfrac = mod(abs(idt), ifrc)
        else if (units.eq.4) then  !idt should be in seconds
            ifrc = 1
            nday   = abs(idt)/86400 ! integer number of days in delta-time
            nhour  = mod(abs(idt),86400)/3600
            nmin   = mod(abs(idt),3600)/60
            nsec   = mod(abs(idt),60)
            nfrac  = 0
        else if (units.eq.3) then !idt should be in minutes
            ifrc = 1
            nday   = abs(idt)/1440 ! integer number of days in delta-time
            nhour  = mod(abs(idt),1440)/60
            nmin   = mod(abs(idt),60)
            nsec   = 0
            nfrac  = 0
        else if (units.eq.2) then !idt should be in hours
            ifrc = 1
            nday   = abs(idt)/24 ! integer number of days in delta-time
            nhour  = mod(abs(idt),24)
            nmin   = 0
            nsec   = 0
            nfrac  = 0
        else if (units.eq.1) then !idt should be in days
            ifrc = 1
            nday   = abs(idt)    ! integer number of days in delta-time
            nhour  = 0
            nmin   = 0
            nsec   = 0
            nfrac  = 0
        else

            write(*,'(''GETH_NEWDATE: Strange length for ODATE: '', i3)') &
                 oldlen
            write(*,*) '#'//odate(1:oldlen)//'#'
            stop

        end if
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        if (idt.ge.0) then

            frnew = frold + nfrac
            if (frnew.ge.ifrc) then
                frnew = frnew - ifrc
                nsec = nsec + 1
            end if

            scnew = scold + nsec
            if (scnew .ge. 60) then
                scnew = scnew - 60
                nmin  = nmin + 1
            end if

            minew = miold + nmin
            if (minew .ge. 60) then
                minew = minew - 60
                nhour  = nhour + 1
            end if

            hrnew = hrold + nhour
            if (hrnew .ge. 24) then
                hrnew = hrnew - 24
                nday  = nday + 1
            end if

            dynew = dyold
            monew = moold
            yrnew = yrold
            do i = 1, nday
                dynew = dynew + 1
                if (dynew.gt.mday(monew)) then
                    dynew = dynew - mday(monew)
                    monew = monew + 1
                    if (monew .gt. 12) then
                        monew = 1
                        yrnew = yrnew + 1
                        ! If the year changes, recompute the number of days in February
                        mday(2) = nfeb_yw(yrnew)
                    end if
                end if
            end do

        else if (idt.lt.0) then

            frnew = frold - nfrac
            if (frnew .lt. 0) then
                frnew = frnew + ifrc
                nsec = nsec + 1
            end if

            scnew = scold - nsec
            if (scnew .lt. 00) then
                scnew = scnew + 60
                nmin  = nmin + 1
            end if

            minew = miold - nmin
            if (minew .lt. 00) then
                minew = minew + 60
                nhour  = nhour + 1
            end if

            hrnew = hrold - nhour
            if (hrnew .lt. 00) then
                hrnew = hrnew + 24
                nday  = nday + 1
            end if

            dynew = dyold
            monew = moold
            yrnew = yrold
            do i = 1, nday
                dynew = dynew - 1
                if (dynew.eq.0) then
                    monew = monew - 1
                    if (monew.eq.0) then
                        monew = 12
                        yrnew = yrnew - 1
                        ! If the year changes, recompute the number of days in February
                        mday(2) = nfeb_yw(yrnew)
                    end if
                   dynew = mday(monew)
                end if
            end do
        end if
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        !  Now construct the new mdate
        newlen = LEN((ndate))

        if (punct) then

            if (newlen.gt.frstart) then
                write(ndate(1:scend),19) yrnew, monew, dynew, hrnew, minew, scnew
                write(hfrc,'(i10)') frnew+1000000000
                ndate = ndate(1:scend)//'.'//hfrc(31-newlen:10)

            else if (newlen.eq.scend) then
                write(ndate(1:scend),19) yrnew, monew, dynew, hrnew, minew, scnew
      19        format(i4,'-',i2.2,'-',i2.2,'_',i2.2,':',i2.2,':',i2.2)

            else if (newlen.eq.miend) then
                write(ndate,16) yrnew, monew, dynew, hrnew, minew
      16        format(i4,'-',i2.2,'-',i2.2,'_',i2.2,':',i2.2)

            else if (newlen.eq.hrend) then
                write(ndate,13) yrnew, monew, dynew, hrnew
      13        format(i4,'-',i2.2,'-',i2.2,'_',i2.2)

            else if (newlen.eq.dyend) then
                write(ndate,10) yrnew, monew, dynew
      10        format(i4,'-',i2.2,'-',i2.2)

            end if

        else

            if (newlen.gt.frstart) then
                write(ndate(1:scend),119) yrnew, monew, dynew, hrnew, minew, scnew
                write(hfrc,'(i10)') frnew+1000000000
                ndate = ndate(1:scend)//'.'//hfrc(31-newlen:10)

            else if (newlen.eq.scend) then
                write(ndate(1:scend),119) yrnew, monew, dynew, hrnew, minew, scnew
      119       format(i4,i2.2,i2.2,i2.2,i2.2,i2.2)

            else if (newlen.eq.miend) then
                write(ndate,116) yrnew, monew, dynew, hrnew, minew
      116       format(i4,i2.2,i2.2,i2.2,i2.2)

            else if (newlen.eq.hrend) then
                write(ndate,113) yrnew, monew, dynew, hrnew
      113       format(i4,i2.2,i2.2,i2.2)

            else if (newlen.eq.dyend) then
                write(ndate,110) yrnew, monew, dynew
      110       format(i4,i2.2,i2.2)

            end if

        endif
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        if (punct .and. (oldlen.ge.11) .and. (newlen.ge.11)) ndate(11:11) = sp
        !------------------------------------------------------------------------------------------

    end subroutine HMC_Tools_Time_GetNewDate
    !------------------------------------------------------------------------------------------

   
end module HMC_Module_Tools_Time
!------------------------------------------------------------------------------------------
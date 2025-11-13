!------------------------------------------------------------------------------------------     
! File:   HMC_Module_Tools_Interp.f90
! Author: Fabio Delogu
! Created on April 28, 2017, 4:12 PM
!
! Module to define interpolation tools
!------------------------------------------------------------------------------------------

!------------------------------------------------------------------------------------------
! Module header
module HMC_Module_Tools_Interp
    
    !------------------------------------------------------------------------------------------
    ! External module(s) and implicit none
    use HMC_Module_Tools_Debug
    
    implicit none
    !------------------------------------------------------------------------------------------
    
contains     
    
    !------------------------------------------------------------------------------------------
    ! Subroutine to space linearly an array to two numbers 
    subroutine linspace(xmin,xmax,x)

        real(kind = 4), intent(in)      :: xmin,xmax
        real(kind = 4), intent(out)     :: x(:)
        integer(kind = 4)               :: i,n
        
        n = size(x)
        if (n == 1) then
            if(xmin /= xmax) then
                call mprintf(.true., iERROR, 'Cannot call linspace with n=1 and xmin /= xmax')
                stop
            else
                x = xmin
            end if
        else
            do i=1,n
                x(i) = (xmax-xmin) * real(i-1,kind = 4) / real(n-1,kind = 4) + xmin
            end do
        end if
        
    end subroutine linspace
    !------------------------------------------------------------------------------------------
    
    !------------------------------------------------------------------------------------------
    ! Subroutine to evaluate the nearest neighbor interpolant
    subroutine nearest_interp_1d ( nd, xd, yd, ni, xi, yi, ki)

        ! The nearest neighbor interpolant L(ND,XD,YD)(X) is the piecewise
        ! constant function which interpolates the data (XD(I),YD(I)) for I = 1
        ! to ND.

        ! Input, integer ND, the number of data points.
        ! ND must be at least 1.

        ! Input, double precision XD(ND), the data points.
        ! Input, double precision YD(ND), the data values.
        ! Input, integer NI, the number of interpolation points.
        ! Input, double precision XI(NI), the interpolation points.
        ! Output, double precision YI(NI), the interpolated values.

        integer(kind = 4)   ::  nd
        integer(kind = 4)   ::  ni

        real(kind = 4)      ::  d
        real(kind = 4)      ::  d2
        integer(kind = 4)   ::  i
        integer(kind = 4)   ::  j
        integer(kind = 4)   ::  k
        real(kind = 4)      ::  xd(nd)
        real(kind = 4)      ::  xi(ni)
        real(kind = 4)      ::  yd(nd)
        real(kind = 4)      ::  yi(ni)
        integer(kind = 4)   ::  ki(ni)

        do i = 1, ni

            k = 1
            d = abs ( xi(i) - xd(k) )

            do j = 2, nd

                d2 = abs ( xi(i) - xd(j) )

                if ( d2 .lt. d ) then
                    k = j
                    d = d2
                end if

            end do

            yi(i) = yd(k)
            ki(i) = k

        end do

    end subroutine nearest_interp_1d
    !------------------------------------------------------------------------------------

    !------------------------------------------------------------------------------------------
    ! Subroutine to linear interp data
    subroutine linear_interp_1d(ndata, xdata, ydata, ninterp, xinterp, yinterp)
        
        ! Interpolate input data (xdata, ydata) to determine the value 
        ! of yinterp at the given points xinterp.
        !
        ! INPUTs:
        ! ndata = number of input data (i.e. size of xdata, ydata arrays)
        ! xdata, ydata = arrays of input data
        ! ninterp = number of points to interp
        ! xinterp = points to interp
        !
        ! OUTPUT:
        ! yinterp = interpolated values corresponding to points "xinterp"
        !
        ! WARING: ndata must be >= 2
        
        integer(kind = 4)                           :: ndata, ninterp
        real(kind = 4),     dimension(ndata)        :: xdata, ydata
        real(kind = 4),     dimension(ninterp)      :: xinterp, yinterp
   
        integer(kind = 4)                           :: iI, kneg, kpos, kexact
        real(kind = 4),     dimension(ndata)        :: xdiff
        
        ! check if data are sufficient for linear interpolation
        if (ndata.lt.2) then
            write(6, *) "ERROR: Linear interpolation requires at least 2 known data"
            stop "Program stopped"
        endif
        
        ! linear interpolation and extrapolation
        do iI = 1, ninterp
            
            ! giulia: commentato perche' funziona solo da gfortran >= 9.0 in avanti
            ! https://gcc.gnu.org/wiki/Fortran2008Status
            ! Find location in an array	Yes (since 9.0, 2018-10-28)
            ! kexact = findloc(xdata, xinterp(iI), dim=1)
            
            ! giulia: kexact=0 da togliere se si riattiva il findloc 
            kexact = 0;

            if (kexact.gt.0) then
                yinterp(iI) = ydata(kexact)
            else
                xdiff = xdata - xinterp(iI)
                kneg = maxloc(xdiff, dim=1, mask=(xdiff<0))
                kpos = minloc(xdiff, dim=1, mask=(xdiff>=0))
            
                if ( (kneg.gt.0) .and. (kpos.gt.0) ) then
                    yinterp(iI) = ( ydata(kpos) - ydata(kneg) ) / ( xdata(kpos) - xdata(kneg) ) * &
                                  ( xinterp(iI) - xdata(kneg) ) + ydata(kneg)
                elseif ( (kneg.gt.0) ) then
                    yinterp(iI) = ( ydata(kneg) - ydata(kneg-1) ) / ( xdata(kneg) - xdata(kneg-1) ) * &
                                  ( xinterp(iI) - xdata(kneg-1) ) + ydata(kneg-1)
                elseif ( (kpos.gt.0) ) then
                    yinterp(iI) = ( ydata(kpos+1) - ydata(kpos) ) / ( xdata(kpos+1) - xdata(kpos) ) * &
                                  ( xinterp(iI) - xdata(kpos) ) + ydata(kpos)
                endif
                                  
            endif

        enddo
        
    end subroutine linear_interp_1d
    !------------------------------------------------------------------------------------------
    
end module HMC_Module_Tools_Interp
!------------------------------------------------------------------------------------------

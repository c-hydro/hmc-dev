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

end module HMC_Module_Tools_Interp
!------------------------------------------------------------------------------------------
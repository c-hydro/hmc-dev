!-----------------------------------------------------------------------------------------------------------------------------------
! Subroutine to analyze and plot 2d variable(s) used in Hydrological Model Continuum
!
! MATLAB SIDE:
! COMMAND LINE (SAME FOLDER):   matlab -nosplash -nodesktop -r "debug_2dVar()"
! COMMAND LINE (OTHER FOLDER):  matlab -nosplash -nodesktop -r "debug_2dVar('/home/fabio/NetBeansProjects/Fortran_Continuum_RegMarche/')"
!    
! FORTRAN SIDE:
! COMMAND LINE (CODE SIDE): call debug_2dVar(dble(a2dVarData), iRows, iCols, 1)
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine debug_2dVar(z, irows, icols, input)

    !implicit none
    real(kind=8), intent(in)            :: z(irows, icols)
    real(kind=8)			:: xyz(3,size(z(:,1)),size(z(1,:)))
    !character(len=*),optional           :: input
    integer				:: nx, ny, nrow
    integer                             :: i, j, file_unit, icols, irows
    integer                             :: ios, ierror
    integer ,optional :: input
    
    character(len=100)                  :: data_file_name, string_input
    
    nx=size(z(:,1))
    ny=size(z(1,:))
    nrow=nx*ny
    
    do i=1,nx
            do j=1,ny
                    xyz(1,i,j)=dble(i)
                    xyz(2,i,j)=dble(j)
                    xyz(3,i,j)=z(i,j)
            end do
    end do
    
    write(string_input,'(I2.2)') input
    
    if (present(input)) then
            data_file_name='data_file_'//trim(string_input)//'.txt'	
    else
            data_file_name='data_file.txt'
    end if

    ierror=0	
    call get_unit(file_unit)	
    if (file_unit==0) then
            ierror=1
            print *,'write_vector_date - fatal error! Could not get a free FORTRAN unit.'
            stop
    end if
    open (unit=file_unit, file=data_file_name, status='replace', iostat=ios)	
    if (ios/=0) then
            ierror=2
            print *,'write_vector_data - fatal error! Could not open the terminal data file.'
            stop
    end if

    do j=1,ny
            do i=1,nx
                    write (file_unit,'(3E15.7)') xyz(1:3,i,j)
            end do
            !write (file_unit,'(a)')
    end do
    close (unit=file_unit)

end subroutine debug_2dVar

subroutine get_unit(iunit)

    !implicit none
    integer i
    integer ios
    integer iunit
    logical lopen

    iunit=0
    do i=1,99
            if (i/= 5 .and. i/=6) then	
                    inquire (unit=i, opened=lopen, iostat=ios)
                    if (ios==0) then
                            if (.not.lopen) then
                                    iunit=i
                                    return
                            end if
                    end if

            end if
    end do	
    return
    
end subroutine get_unit



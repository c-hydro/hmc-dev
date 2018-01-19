!--------------------------------------------------------------------------------  
! File:   HMC_Module_Args.f90
! Author(s): Fabio Delogu, Francesco Silvestro, Simone Gabellani
! Created on February 15, 2017, 4:40 PM
!
! Module to read argument(s) defined by command-line
!--------------------------------------------------------------------------------

!--------------------------------------------------------------------------------
! Module Args
module HMC_Module_Args
    
    !--------------------------------------------------------------------------------
    ! External module(s) and implicit none
    use HMC_Module_Tools_Debug          ! to import global variable(s) declaration

    implicit none
    !--------------------------------------------------------------------------------
    
contains 
    
    !--------------------------------------------------------------------------------
    ! Subroutine to read argument(s)
    subroutine HMC_Args_Read(dUc, dUh, dCt, dCf, dCPI, dWTableHbr, dKSatRatio, dSlopeMax, &
                             sDomainName, &
                             sFileInfo, iArgsType) 
        
        !------------------------------------------------------------------------------------------
        ! Variable(s)
        integer(kind = 4)               :: iArgsN, iArgsType
        real(kind = 4)                  :: dUc, dUh, dCt, dCf, dCPI, dWTableHbr, dKSatRatio, dSlopeMax
        
        character(len = 700)            :: sLineBuffer
        character(len = 256)            :: sDomainName   
        character(len = 700)            :: sFileInfo
        !------------------------------------------------------------------------------------------                    
        
        !------------------------------------------------------------------------------------------
        ! Variable(s) initialization
        iArgsN = -9999; iArgsType = -9999
        dUc = 0.0; dUh = 0.0; dCt = 0.0; dCf = 0.0; dCPI = 0.0; 
        dWTableHbr = 0.0; dKSatRatio = 0.0; dSlopeMax = 0.0; sDomainName = "";
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Get number of argument(s)
        iArgsN = iargc()   
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Get argument(s) from command line
        if (iArgsN == 9) then
            
            !------------------------------------------------------------------------------------------
            ! Get friction coefficient in channels [m^0.5 s^-1] (Uc)
            call getarg(1, sLineBuffer); read(sLineBuffer,*) dUc
            ! Get flow motion coefficient in hillslopes [s^-1] (Uh)
            call getarg(2, sLineBuffer); read(sLineBuffer,*) dUh
            ! Get mean field capacity [-] (Ct)
            call getarg(3, sLineBuffer); read(sLineBuffer,*) dCt
            ! Get infiltration capacity at saturation [-] (Cf)
            call getarg(4, sLineBuffer); read(sLineBuffer,*) dCf
            ! Get domain name
            call getarg(5, sLineBuffer); sDomainName = sLineBuffer
            ! Get soil humidity initial condition (CPI)
            call getarg(6, sLineBuffer); read(sLineBuffer,*) dCPI
            ! Get maximum water capacity of the aquifer [mm] (WTableHbr)
            call getarg(7, sLineBuffer); read(sLineBuffer,*) dWTableHbr
            ! Get anisotropy between the vertical and horizontal saturated conductivity, and to soil porosity [-]
            call getarg(8, sLineBuffer); read(sLineBuffer,*) dKSatRatio
            ! Get subsoil maximum slope [%]
            call getarg(9, sLineBuffer); read(sLineBuffer,*) dSlopeMax
            
            ! Define information file name
            sFileInfo = trim(sDomainName)//'.info.txt' 

            ! Define argument(s) type
            iArgsType = 1
            !------------------------------------------------------------------------------------------
            
        elseif (iArgsN == 1) then
            
            !------------------------------------------------------------------------------------------
            ! Get information file name 
            call getarg(1, sLineBuffer); sFileInfo = sLineBuffer

            ! Define argument(s) type
            iArgsType = 2
            !------------------------------------------------------------------------------------------
            
        endif
        !------------------------------------------------------------------------------------------
        
    end subroutine HMC_Args_Read
    !--------------------------------------------------------------------------------
    
end module HMC_Module_Args
!--------------------------------------------------------------------------------
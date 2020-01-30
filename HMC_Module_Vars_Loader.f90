!------------------------------------------------------------------------------------
! File:   HMC_Module_Vars_Loader.f90
! Author(s): Fabio Delogu, Francesco Silvestro, Simone Gabellani
! Created on March, 4 2015, 9:57 AM
!
! Module to import global variable(s)
!------------------------------------------------------------------------------------

!------------------------------------------------------------------------------------
! Module Header
module HMC_Module_Vars_Loader
    
    !------------------------------------------------------------------------------------
    ! External module(s) and implicit none
    implicit none
    integer, parameter :: iMaxDomain = 1
    !------------------------------------------------------------------------------------
    
    !------------------------------------------------------------------------------------
    ! Defining global variables type
    include "HMC_Type_Vars.inc"
    type (HMC_Type_Vars), dimension (iMaxDomain) :: oHMC_Vars
    save oHMC_Vars
    !------------------------------------------------------------------------------------
    
end module HMC_Module_Vars_Loader
!------------------------------------------------------------------------------------
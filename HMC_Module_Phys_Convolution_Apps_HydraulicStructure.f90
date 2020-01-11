!------------------------------------------------------------------------------------
! File:   HMC_Module_Phys_Convolution_Apps_HydraulicStructure.f90
!
! Author(s):    Fabio Delogu, Francesco Silvestro, Simone Gabellani
! Date:         20190410
!
! Convolution Apps HydraulicStructure subroutine(s) for HMC model
!------------------------------------------------------------------------------------

!------------------------------------------------------------------------------------
! Module Header
module HMC_Module_Phys_Convolution_Apps_HydraulicStructure

    !------------------------------------------------------------------------------------
    ! External module(s) 
    use HMC_Module_Namelist,                only: oHMC_Namelist
    use HMC_Module_Vars_Loader,             only: oHMC_Vars
    
    use HMC_Module_Tools_Debug
    
    use HMC_Module_Phys_HydraulicStructure, only: HMC_Phys_Lake_Tank, HMC_Phys_Dam_Discharge
    
    ! Implicit none for all subroutines in this module
    implicit none
    !------------------------------------------------------------------------------------------

contains 

    !------------------------------------------------------------------------------------------
    ! Subroutine to compute hydraulic structure effect(s)
    subroutine HMC_Phys_Convolution_Apps_HydraulicStructure(iID, iRows, iCols, &
                                                            dDtDataForcing, iNDam, iNLake)
    
        !------------------------------------------------------------------------------------------
        ! Variable(s)
        integer(kind = 4)           :: iID, iRows, iCols
        integer(kind = 4)           :: iNDam, iNLake
        
        real(kind = 4)              :: dDtDataForcing
        
        real(kind = 4), dimension (iRows, iCols)    :: a2dVarFlowDeep, a2dVarHydro
        
        real(kind = 4), dimension (iNLake)          :: a1dVarVLake, a1dVarQoutLake
        
        real(kind = 4), dimension (iNDam)           :: a1dVarVDam, a1dVarHDam, a1dVarLDam
        real(kind = 4), dimension (iNDam)           :: a1dVarCoeffDam, a1dVarQoutDam
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Get variable(s)
        a2dVarFlowDeep = oHMC_Vars(iID)%a2dFlowDeep
        a2dVarHydro = oHMC_Vars(iID)%a2dHydro
        
        a1dVarVLake = oHMC_Vars(iID)%a1dVLake
        a1dVarQoutLake = oHMC_Vars(iID)%a1dQoutLake
        
        a1dVarVDam = oHMC_Vars(iID)%a1dVDam
        a1dVarHDam = oHMC_Vars(iID)%a1dHDam 
        a1dVarLDam = oHMC_Vars(iID)%a1dLDam
        a1dVarCoeffDam = oHMC_Vars(iID)%a1dCoeffDam 

        ! Info start
        if (iDEBUG.gt.0) then
            call mprintf(.true., iINFO_Extra, ' Phys :: Convolution :: HydraulicStructure ... ' )
        endif
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Method to compute lake tank
        call HMC_Phys_Lake_Tank(iID, iRows, iCols, dDtDataForcing, iNLake, &
                                a1dVarVLake, a1dVarQoutLake, &
                                a2dVarHydro, a2dVarFlowDeep)
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Method to compute dam discharge --> Commentata per cambio calcolo di svaso delle dighe
        !call HMC_Phys_Dam_Discharge(iID, iNDam, dDtDataForcing, &
        !                                a1dVarVDam, a1dVarHDam, a1dVarLDam, a1dVarCoeffDam)
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Updating field(s) in global declaration
        oHMC_Vars(iID)%a2dFlowDeep = a2dVarFlowDeep
        oHMC_Vars(iID)%a2dHydro = a2dVarHydro
        
        oHMC_Vars(iID)%a1dVLake = a1dVarVLake
        oHMC_Vars(iID)%a1dQoutLake = a1dVarQoutLake
        
        oHMC_Vars(iID)%a1dVDam = a1dVarVDam
        oHMC_Vars(iID)%a1dHDam = a1dVarHDam
        oHMC_Vars(iID)%a1dLDam = a1dVarLDam
        oHMC_Vars(iID)%a1dCoeffDam = a1dVarCoeffDam
 
        ! Info end
        if (iDEBUG.gt.0) then
            call mprintf(.true., iINFO_Extra, ' Phys :: Convolution :: HydraulicStructure ... OK' )
        endif
        !------------------------------------------------------------------------------------------
                                                                    
    end subroutine HMC_Phys_Convolution_Apps_HydraulicStructure
    !------------------------------------------------------------------------------------------

end module HMC_Module_Phys_Convolution_Apps_HydraulicStructure 
!------------------------------------------------------------------------------------------
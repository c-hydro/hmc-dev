!------------------------------------------------------------------------------------------    
! File:   HMC_Module_Phys_StateUpdating.f90
! Author: Fabio Delogu, Valerio Basso
!
! Created on December 19, 2017, 4:02 PM
!------------------------------------------------------------------------------------------

!------------------------------------------------------------------------------------------
! Module Header
module HMC_Module_Phys_StateUpdating
    
    !------------------------------------------------------------------------------------------
    ! External module(s) for all subroutine in this module
    use HMC_Module_Namelist,            only:   oHMC_Namelist
    use HMC_Module_Vars_Loader,         only:   oHMC_Vars
    
    use HMC_Module_Tools_Generic,       only:   assimNudging
    
    use HMC_Module_Tools_Debug
    
    ! Implicit none for all subroutines in this module
    implicit none
    !------------------------------------------------------------------------------------------
    
contains

    !------------------------------------------------------------------------------------------
    ! Subroutine to update model state(s)
    subroutine HMC_Phys_StateUpdating_Cpl(iID, iRows, iCols)
    
	!------------------------------------------------------------------------------------------
        ! Variable(s) declaration
        integer(kind = 4)           :: iID
        integer(kind = 4)           :: iRows, iCols
        
        integer(kind = 4)           :: iFlagSMAssim

        integer(kind = 4), dimension (iRows, iCols)         :: a2iVarMask
        real(kind = 4), dimension (iRows, iCols)            :: a2dVarDEM, a2dVarS
        
        real(kind = 4), dimension (iRows, iCols)            :: a2dVarVTot
        real(kind = 4), dimension (iRows, iCols)            :: a2dVarSMStar, a2dVarSMGain
        
        real(kind = 4), dimension (iRows, iCols)            :: a2dVarVTot_Obs, a2dVarVTot_Assim, a2dVarVTot_Corr
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Initialization variable(s)
        iFlagSMAssim = 0;
        a2iVarMask = 0; a2dVarDEM = 0.0; a2dVarS = 0.0;
        a2dVarVTot = -9999.0; a2dVarSMStar = -9999.0; a2dVarSMGain = -9999.0; 
        a2dVarVTot_Obs = -9999.0; a2dVarVTot_Assim = -9999.0; a2dVarVTot_Corr = -9999.0
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Soil Moisture Assimilation flag
        iFlagSMAssim = oHMC_Namelist(iID)%iFlagSMAssim 
        ! Data static definition
        a2dVarDEM = oHMC_Vars(iID)%a2dDem
        a2iVarMask = oHMC_Vars(iID)%a2iMask
        a2dVarS = oHMC_Vars(iID)%a2dS
        ! Data dynamic definition
        a2dVarVTot = oHMC_Vars(iID)%a2dVTot
        a2dVarSMStar = oHMC_Vars(iID)%a2dSMStar
        a2dVarSMGain = oHMC_Vars(iID)%a2dSMGain
        
        ! Info start
        call mprintf(.true., iINFO_Verbose, ' Phys :: StateUpdating ... ' )
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Debug
        if (iDEBUG.gt.0) then
            call mprintf(.true., iINFO_Extra, ' ========= STATEUPDATING START =========== ')  
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarVTot, a2iVarMask, 'VTOT START ') )
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarSMStar, a2iVarMask, 'SMSTAR START ') )
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarSMGain, a2iVarMask, 'SMGAIN START ') )
            call mprintf(.true., iINFO_Extra, '') 
        endif
        !------------------------------------------------------------------------------------------
        
        !-------------------------------------------------------------------------------------
        ! Call subroutine to compute SM assimilation
        call mprintf(.true., iINFO_Verbose, ' Phys :: StateUpdating :: Soil Moisture ... ' )
        if (iFlagSMAssim.eq.1) then 

            !-------------------------------------------------------------------------------------
            ! Check forcing(s) to use assimilation method
            if ( any(a2dVarSMStar.ne.-9999.0) .and. any(a2dVarSMGain.ne.-9999.0) ) then

                !------------------------------------------------------------------------------------------
                ! Convert SM star from HSAF product(s) into volumetric soil water content [mm]
                where( (a2dVarSMStar.gt.0.0) .and. (a2dVarSMGain.ge.0.0) )
                    a2dVarVTot_Obs = a2dVarSMStar * a2dVarS
                elsewhere
                    a2dVarVTot_Obs = a2dVarVTot
                endwhere
                !------------------------------------------------------------------------------------------

                !------------------------------------------------------------------------------------------
                ! Nudging assimilation method
                call assimNudging(a2iVarMask, a2dVarVTot, a2dVarVTot_Obs, a2dVarSMGain, &
                                  a2dVarVTot_Assim, a2dVarVTot_Corr) 
                !------------------------------------------------------------------------------------------
                                  
                !------------------------------------------------------------------------------------------
                ! Update volumetric soil water content
                a2dVarVTot = -9999.0
                a2dVarVTot = a2dVarVTot_Assim
                !------------------------------------------------------------------------------------------                                  

                ! Info start assimilation
                call mprintf(.true., iINFO_Verbose, ' Phys :: StateUpdating :: Soil Moisture ... OK' )
                !------------------------------------------------------------------------------------------

            else

                !-------------------------------------------------------------------------------------
                ! Info assimilation no data available
                call mprintf(.true., iINFO_Verbose, ' Phys :: StateUpdating :: Soil Moisture ... DATA NO AVAILABLE ' )
                !-------------------------------------------------------------------------------------

            endif
            !-------------------------------------------------------------------------------------

        else

            !-------------------------------------------------------------------------------------
            ! Info assimilation no data available
            call mprintf(.true., iINFO_Verbose, ' Phys :: StateUpdating :: Soil Moisture ... NOT ACTIVATED ' )
            !-------------------------------------------------------------------------------------

        endif
        !------------------------------------------------------------------------------------------
                            
        !------------------------------------------------------------------------------------------
        ! Debug
        if (iDEBUG.gt.0) then
            call mprintf(.true., iINFO_Extra, '') 
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarVTot, a2iVarMask, 'VTOT END ') )
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarSMStar, a2iVarMask, 'SMSTAR END ') )
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarSMGain, a2iVarMask, 'SMGAIN END ') )
            call mprintf(.true., iINFO_Extra, ' ========= STATEUPDATING END =========== ')  
        endif
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Passing variable(s) to global declaration
        oHMC_Vars(iID)%a2dVTot = a2dVarVTot
        
        ! Info end
        call mprintf(.true., iINFO_Verbose, ' Phys :: StateUpdating ... OK ' )
        !------------------------------------------------------------------------------------------
    
    end subroutine HMC_Phys_StateUpdating_Cpl
    !------------------------------------------------------------------------------------------

end module HMC_Module_Phys_StateUpdating
!------------------------------------------------------------------------------------------

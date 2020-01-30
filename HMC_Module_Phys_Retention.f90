!------------------------------------------------------------------------------------------    
! File:   HMC_Module_Phys_Retention.f90
! Author: fabio
!
! Created on April 2, 2014, 5:19 PM
!------------------------------------------------------------------------------------------

!------------------------------------------------------------------------------------------
! Module Header
module HMC_Module_Phys_Retention
    
    !------------------------------------------------------------------------------------------
    ! External module(s) for all subroutine in this module
    use HMC_Module_Namelist, only: oHMC_Namelist
    use HMC_Module_Vars_Loader, only: oHMC_Vars
    
    use HMC_Module_Tools_Debug
    
    ! Implicit none for all subroutines in this module
    implicit none
    !------------------------------------------------------------------------------------------
    
contains
    
    !------------------------------------------------------------------------------------------
    ! Subroutine for calculating volume retention
    subroutine HMC_Phys_Retention_Cpl(iID, iRows, iCols)
        
        !------------------------------------------------------------------------------------------
        ! Variable(s) declaration
        integer(kind = 4)           :: iID
        integer(kind = 4)           :: iRows, iCols
        
        integer(kind = 4)           :: iFlagLAI
        
        real(kind = 4), dimension (iRows, iCols)         :: a2dVarRain
        real(kind = 4), dimension (iRows, iCols)         :: a2dVarLAI
        
        integer(kind = 4),  dimension (iRows, iCols)        :: a2iVarMask
        real(kind = 4),     dimension (iRows, iCols)        :: a2dVarDEM, a2dVarS
        real(kind = 4),     dimension (iRows, iCols)        :: a2dVarVRet, a2dVarVRetMax, a2dVarVRetStep
        real(kind = 4),     dimension (iRows, iCols)        :: a2dVarRainStep 
        real(kind = 4),     dimension (iRows, iCols)        :: a2dVarRetErr
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Initialize variable(s)
        iFlagLAI = 0
        
        a2dVarDEM = 0.0; a2dVarS = 0.0;
        a2dVarRain = 0.0; a2dVarRainStep = 0.0; a2dVarLAI = 0.0;
        a2dVarVRet = 0.0; a2dVarVRetStep = 0.0; a2dVarRetErr = 0.0; a2dVarVRetMax = 0.0
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Loading flag(s)
        iFlagLAI = oHMC_Namelist(iID)%iFlagLAI
        ! Loading static data
        a2iVarMask = oHMC_Vars(iID)%a2iMask
        a2dVarDEM = oHMC_Vars(iID)%a2dDem
        a2dVarS = oHMC_Vars(iID)%a2dS
        ! Loading dynamic data
        a2dVarVRet = oHMC_Vars(iID)%a2dVRet 
        a2dVarRain = oHMC_Vars(iID)%a2dRain
        a2dVarLAI = oHMC_Vars(iID)%a2dLAI
        
        ! Temporary variable(s) for retention subroutine
        a2dVarRainStep = a2dVarRain
        a2dVarVRetStep = a2dVarVRet

        ! Info start
        call mprintf(.true., iINFO_Verbose, ' Phys :: Retention ... ' )
        !------------------------------------------------------------------------------------------

        !------------------------------------------------------------------------------------------
        ! Debug
        if (iDEBUG.gt.0) then
            call mprintf(.true., iINFO_Extra, ' ========= RETENTION START =========== ') 
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarRainStep, a2iVarMask, 'RAIN START ') )
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarVRetStep, a2iVarMask, 'VRET START ') )
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarVRetMax, a2iVarMask, 'VRETMAX START ') )
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarS, a2iVarMask, 'S ') )
            call mprintf(.true., iINFO_Extra, '') 
        endif
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Calculating volume retention max and retrieving Vret and Rain
        if (iFlagLAI .eq. 0) then
            
            ! Without LAI data (all LAI values are undefined)
            where (a2dVarDEM.gt.0.0)
                a2dVarVRetMax = (0.038*a2dVarS + 0.4909)*1 ! magic numbers :)
            elsewhere
                a2dVarVRetMax = 0.0
            endwhere
 
        elseif (iFlagLAI .eq. 1) then
            
            ! With LAI data
            where (a2dVarDEM.gt.0.0.and.a2dVarLAI.gt.0.0)
                a2dVarVRetMax = 0.95 + 0.5*a2dVarLAI-0.006*a2dVarLAI**2
            elsewhere
                a2dVarVRetMax = 0.0
            endwhere
            
            where (a2dVarVRetMax.gt.50.0) a2dVarVRetMax = 0.0
                
        else
            ! Exit error
            call mprintf(.true., iERROR, ' Phys :: Retention :: Incorrect LAI type definition!' ) 
        endif
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Calculating rain and retention volume 
        where( (a2dVarVRet + a2dVarRain) .le. a2dVarVRetMax .and. a2dVarDEM.gt.0.0 )
            a2dVarVRet = a2dVarVRet + a2dVarRain
            a2dVarRain = 0.0
        endwhere

        where( (a2dVarVRet + a2dVarRain) .gt. a2dVarVRetMax .and. a2dVarDEM.gt.0.0 )
            a2dVarRain = (a2dVarVRet + a2dVarRain) - a2dVarVRetMax
            a2dVarVRet = a2dVarVRetMax
        endwhere
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Calculating retention subroutine error
        a2dVarRetErr = a2dVarRainStep - (a2dVarVRet - a2dVarVRetStep) - a2dVarRain
        !------------------------------------------------------------------------------------------

        !------------------------------------------------------------------------------------------
        ! Checking for volume retention less then 0.0
        where (a2dVarVRet.lt.0.0) a2dVarVRet = 0.0
        !------------------------------------------------------------------------------------------
  
        !------------------------------------------------------------------------------------------
        ! Debug
        if (iDEBUG.gt.0) then
            call mprintf(.true., iINFO_Extra, '') 
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarRain, a2iVarMask, 'RAIN END ') )
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarVRet, a2iVarMask, 'VRET END ') )
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarVRetMax, a2iVarMask, 'VRETMAX END ') )
            call mprintf(.true., iINFO_Extra, ' ========= RETENTION END =========== ') 
        endif
        !------------------------------------------------------------------------------------------

        !------------------------------------------------------------------------------------------
        ! Passing variable(s) to global declaration
        oHMC_Vars(iID)%a2dVRet = a2dVarVRet
        oHMC_Vars(iID)%a2dRain = a2dVarRain
        
        ! Info end
        call mprintf(.true., iINFO_Verbose, ' Phys :: Retention ... OK' )
        !------------------------------------------------------------------------------------------
      
    end subroutine HMC_Phys_Retention_Cpl
    !------------------------------------------------------------------------------------------

end module HMC_Module_Phys_Retention
!------------------------------------------------------------------------------------------
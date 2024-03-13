 !------------------------------------------------------------------------------------
! File:   HMC_Module_Phys_LSM_Apps.f90
!
! Author:   Fabio Delogu
! Date:     20150210
!
! Force-Restore applications module
!------------------------------------------------------------------------------------

!------------------------------------------------------------------------------------
! Module Header
module HMC_Module_Phys_LSM_Apps

    !------------------------------------------------------------------------------------
    ! External module(s) 
    use HMC_Module_Namelist,           only:    oHMC_Namelist
    use HMC_Module_Vars_Loader,        only:    oHMC_Vars
   
    use HMC_Module_Tools_Debug
    
    use HMC_Module_Tools_Time,          only:   HMC_Tools_Time_MonthVal
    
    ! Implicit none for all subroutines in this module
    implicit none
    !------------------------------------------------------------------------------------------

contains

    !------------------------------------------------------------------------------------------
    ! Subroutine for calculating beta function
    subroutine HMC_Phys_LSM_Apps_BetaFunction(iID, iRows, iCols, &
                                a2dVarSM, a2dVarDEM, &
                                a2dVarBF, a2dVarBF_BareSoil)
                                                   
        !------------------------------------------------------------------------------------------
        ! Variable(s)
        integer(kind = 4)       :: iID, iRows, iCols
        real(kind = 4)          :: dBFMin, dBFMax
        real(kind = 4), dimension(iRows, iCols) :: a2dVarSM, a2dVarDEM
        real(kind = 4), dimension(iRows, iCols) :: a2dVarBF, a2dVarBF_BareSoil 
        real(kind = 4), dimension(iRows, iCols) :: a2dVarCt
        real(kind = 4), dimension(iRows, iCols) :: a2dVarCtWP, a2dVarKb1, a2dVarKc1, a2dVarKb2, a2dVarKc2
        integer(kind = 4), dimension(iRows, iCols) :: a2iVarMask
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Constant(s)
        dBFMin = oHMC_Namelist(iID)%dBFMin
        dBFMax = oHMC_Namelist(iID)%dBFMax 
        ! Static variable(s)
        a2dVarCt = oHMC_Vars(iID)%a2dCt
        a2dVarCtWP = oHMC_Vars(iID)%a2dCtWP
        a2dVarKb1 = oHMC_Vars(iID)%a2dKb1
        a2dVarKc1 = oHMC_Vars(iID)%a2dKc1
        a2dVarKb2 = oHMC_Vars(iID)%a2dKb2 
        a2dVarKc2 = oHMC_Vars(iID)%a2dKc2 
        a2iVarMask = oHMC_Vars(iID)%a2iMask
        
        ! Info start
        call mprintf(.true., iINFO_Extra, ' Phys :: Land surface model :: BetaFuntion ... ' )
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Calculating Beta function values
        where (a2dVarSM.lt.a2dVarCtWP.and.a2iVarMask.gt.0.0)
                a2dVarBF = dBFMin
        elsewhere ((a2dVarSM.ge.a2dVarCtWP).and.(a2dVarSM.le. (a2dVarCt)).and.(a2iVarMask.gt.0.))
                a2dVarBF = a2dVarKb1*a2dVarSM + a2dVarKc1
        elsewhere (a2iVarMask.gt.0.0)
                a2dVarBF = dBFMax       
        endwhere
        
        ! Calculating Beta function values for bare soil
        where (a2iVarMask.gt.0.0)
                a2dVarBF_BareSoil = 1 + 1/(exp(50.0*(a2dVarSM-a2dVarCtWP))/(1000.0*(a2dVarCt-a2dVarCtWP)+1.0))
        endwhere
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Check beta limit(s) --> just in case
        where (a2dVarBF.gt.1.0)
            a2dVarBF = 1.0
            a2dVarBF_BareSoil = 1.0
        elsewhere (a2dVarBF.le.0.001)
            a2dVarBF = 0.001
            a2dVarBF_BareSoil = 0.001
        endwhere
        !------------------------------------------------------------------------------------------

        !------------------------------------------------------------------------------------------
        ! Debug
        !call surf(a2dVarBF,pm3d='pm3d implicit map', palette='rgbformulae 31, -11, 32')
        !write(*,*) 'VAR BF' , sum(a2dVarBF)/max(1,count(a2dVarBF.gt.0.0))
        
        ! Info end
        call mprintf(.true., iINFO_Extra, ' Phys :: Land surface model :: BetaFuntion ... OK' )
        !------------------------------------------------------------------------------------------

    end subroutine HMC_Phys_LSM_Apps_BetaFunction
    !------------------------------------------------------------------------------------------
    
    !------------------------------------------------------------------------------------------
    ! Calculating CH 
    subroutine HMC_Phys_LSM_Apps_CH(iID, iRows, iCols, sTime, &
                              a2dVarDEM, &
                              a2dVarRb, &
                              a2dVarBF, a2dVarWind, &
                              a2dVarRatm, a2dVarRsurf, a2dVarRsurf_pot)
        
        !------------------------------------------------------------------------------------------
        ! Variable(s)
        integer(kind = 4)       :: iID, iRows, iCols, iT, iStep    
        
        real(kind = 4)          :: dVarCH, dVarCHn, dVarPSI
        
        real(kind = 4), dimension(iRows, iCols) :: a2dVarDEM, a2dVarRb, a2dVarPSIstable, a2dVarBF, a2dVarWind
        real(kind = 4), dimension(iRows, iCols) :: a2dVarCH, a2dVarRatm, a2dVarRsurf, a2dVarRsurf_pot  
        integer(kind = 4), dimension(iRows, iCols) :: a2iVarMask
        
        character(len = 19)       :: sTime
        character(len = 12)       :: sTimeMonth
        !------------------------------------------------------------------------------------------                
        
        !------------------------------------------------------------------------------------------
        ! Initialize variable(s)
        a2dVarPSIstable = 0.0; a2dVarCH = -9999.0;
        
        a2dVarRsurf_pot = -9999.0;          ! Surface resistance without limitation caused by shortage of water in the soils [s/m]
        a2dVarRatm = -9999.0;               ! Bulk Atmospheric resistance [s/m]
        a2dVarRsurf = -9999.0;              ! Surface resistance [s/m]
        a2iVarMask = oHMC_Vars(iID)%a2iMask
        
        ! Checking date
        write(sTimeMonth,'(A,A,A)') sTime(1:4), sTime(6:7), sTime(9:10)
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Constant(s)
        call HMC_Tools_Time_MonthVal(oHMC_Namelist(iID)%a1dCHMonthly, sTimeMonth, dVarCH)
        
        ! Static variable(s)
        dVarPSI = log(2.0)
        dVarCHn = exp(dVarCH)
        
        ! Info start
        call mprintf(.true., iINFO_Extra, ' Phys :: Land surface model :: CH ... ' )
        
        ! Debug
        !call mprintf(.true., iINFO_Extra, checkvar(a2dVarCH, int(a2dVarDEM), 'CH START') )
        !call mprintf(.true., iINFO_Extra, checkvar(a2dVarRb, int(a2dVarDEM), 'RB START') )
        !call mprintf(.true., iINFO_Extra, checkvar(a2dVarPSIstable, int(a2dVarDEM), 'PSISTABLE START') )
        !------------------------------------------------------------------------------------------

        !------------------------------------------------------------------------------------------
        ! Calculating PSI stable values (from 1 to 3)
        where(a2dVarRb.le.0.0.and.a2iVarMask.gt.0.0)
            a2dVarPSIstable = 1 + exp(dVarPSI)*(1 - exp(10*a2dVarRb))
        elsewhere(a2iVarMask.gt.0.0)
            a2dVarPSIstable = 1.0
        elsewhere
            a2dVarPSIstable = 0.0
        endwhere
        
        ! Calculating CH values
        where(a2iVarMask.gt.0.0.and.a2dVarWind.gt.0.0)
            a2dVarCH = dVarCHn*a2dVarPSIstable
            a2dVarRatm = 1.0/(a2dVarCH*a2dVarWind)
            a2dVarRsurf = a2dVarRatm/a2dVarBF
            a2dVarRsurf_pot = a2dVarRatm
        elsewhere(a2iVarMask.gt.0.0)
            a2dVarRatm = 10000.0 ! set resistances to extremely high values to interrupt ET when wind speed is zero
            a2dVarRsurf = 10000.0      
            a2dVarRsurf_pot = 10000.0
        endwhere
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! not considering water stress as limitation for ET
        if (oHMC_Namelist(iID)%iFlagBetaET .eq. 0) then
            a2dVarRsurf = a2dVarRsurf_pot
        endif 
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Debug
        !call mprintf(.true., iINFO_Extra, checkvar(a2dVarCH, int(a2dVarDEM), 'CH END') )
        !call mprintf(.true., iINFO_Extra, checkvar(a2dVarRb, int(a2dVarDEM), 'RB END') )
        !call mprintf(.true., iINFO_Extra, checkvar(a2dVarPSIstable, int(a2dVarDEM), 'PSISTABLE END') )
        
        ! Info end
        call mprintf(.true., iINFO_Extra, ' Phys :: Land surface model :: CH ... OK' )
        !------------------------------------------------------------------------------------------
        
    end subroutine HMC_Phys_LSM_Apps_CH
    !------------------------------------------------------------------------------------------
    
    !------------------------------------------------------------------------------------------
    ! Subroutine for solving force restore equation using runge-kutta 4
    subroutine HMC_Phys_LSM_Apps_RK4(iID, iRows, iCols, & 
                            dTRef, &
                            dIntStep, iIntDelta, &
                            a2dVarTDeep, a2dVarPit, a2dVarRatm, a2dVarRsurf, &
                            a2dVarRn, &
                            a2dVarRelHum, a2dVarWind, a2dVarTaK, a2dVarPa, & 
                            a2dVarLambda, a2dVarEA, a2dVarRhoA, &
                            a2dVarLSTPStep, a2dVarLSTUpd)
        
        !------------------------------------------------------------------------------------------
        ! Variable(s)
        integer(kind = 4)       :: iID, iRows, iCols
        integer(kind = 4)       :: iIntDelta
        
        real(kind = 4)          :: dTRef, dIntStep
        real(kind = 4)          :: dCp, dPiGreco, dOmega
        real(kind = 4)          :: dKDeltaMax               ! Delta max for applying Runge-Kutta
        
        real(kind = 4), dimension(iRows, iCols) :: a2dVarDEM
        real(kind = 4), dimension(iRows, iCols) :: a2dVarTDeep, a2dVarPit, a2dVarRatm, a2dVarRsurf
        real(kind = 4), dimension(iRows, iCols) :: a2dVarRn
        real(kind = 4), dimension(iRows, iCols) :: a2dVarRelHum, a2dVarWind, a2dVarTaK, a2dVarPa
        real(kind = 4), dimension(iRows, iCols) :: a2dVarLambda, a2dVarEA, a2dVarRhoA
        real(kind = 4), dimension(iRows, iCols) :: a2dVarLSTPStep, a2dVarLSTUpd
        real(kind = 4), dimension(iRows, iCols) :: a2dVarK1, a2dVarK2, a2dVarK3, a2dVarK4
        
        integer(kind = 4), dimension(iRows, iCols) :: a2iVarMask
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Constant(s)
        dCp = oHMC_Namelist(iID)%dCp        ! Specific heat (cost pressure)
        ! Static variable(s)
        dPiGreco = 3.14                     ! Pi greco
        dOmega = 1.0/(60.0*60.0*24.0)       ! Day length
        
        ! LST maximum delta value to limit runge-kutta integration method
        dKDeltaMax = oHMC_Namelist(iID)%dLSTDeltaMax 

        ! Extracting static variable(s)
        a2dVarDEM = oHMC_Vars(iID)%a2dDem
        a2iVarMask = oHMC_Vars(iID)%a2iMask
        
        ! Info start
        !call mprintf(.true., iINFO_Extra, ' Phys :: Land surface model :: RK4 ... ' )
        
        ! Debug
        !call mprintf(.true., iINFO_Extra, checkvar(a2dVarLSTPStep, int(a2dVarDEM), 'LST INT START') )
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Updating land surface temperature using Runge-Kutta (fourth order)
        a2dVarLSTUpd = 0.0
        a2dVarK1 = 0.0; a2dVarK2 = 0.0; a2dVarK3 = 0.0; a2dVarK4 = 0.0
        where (a2iVarMask.gt.0.0)
            
            !------------------------------------------------------------------------------------------
            ! Compute K1 RK argument
            a2dVarLSTUpd = a2dVarLSTPStep
            
            a2dVarK1 = iIntDelta *(2*sqrt(dPiGreco*dOmega)/a2dVarPit*(a2dVarRn - &
            a2dVarRhoA*dCp*(a2dVarLSTUpd - a2dVarTaK)/a2dVarRatm - &
            a2dVarRhoA*a2dVarLambda*(0.611*exp(17.3*(a2dVarLSTUpd - dTRef)/ &
            (237.3 + a2dVarLSTUpd - dTRef)) - a2dVarEA)/(a2dVarPa*a2dVarRsurf)*0.622) - &
            2*dPiGreco*dOmega*(a2dVarLSTUpd - a2dVarTDeep))

            ! Check K1 RK argument with K maximum delta 
            where (a2dVarK1.gt.dKDeltaMax)
                a2dVarK1 = dKDeltaMax
            endwhere
            ! Update LST using K1 argument
            a2dVarLSTUpd = a2dVarLSTPStep + a2dVarK1/2.0
            !------------------------------------------------------------------------------------------
            
            !------------------------------------------------------------------------------------------
            ! Compute K2 argument
            a2dVarK2 = iIntDelta *( 2*sqrt(dPiGreco*dOmega)/a2dVarPit*(a2dVarRn - &
            a2dVarRhoA*dCp*(a2dVarLSTUpd - a2dVarTaK)/a2dVarRatm - &
            a2dVarRhoA*a2dVarLambda*(0.611*exp(17.3*(a2dVarLSTUpd - dTRef)/ &
            (237.3 + a2dVarLSTUpd - dTRef)) - a2dVarEA)/(a2dVarPa*a2dVarRsurf)*0.622) - &
            2*dPiGreco*dOmega*(a2dVarLSTUpd - a2dVarTDeep))
            
            ! Check K2 RK argument with K maximum delta 
            where (a2dVarK2.gt.dKDeltaMax)
                a2dVarK2 = dKDeltaMax
            endwhere
            ! Update LST using K2 argument
            a2dVarLSTUpd = a2dVarLSTPStep + a2dVarK2/2.0
            !------------------------------------------------------------------------------------------
            
            !------------------------------------------------------------------------------------------
            ! Compute K3 RK argument 
            a2dVarK3 = iIntDelta *( 2*sqrt(dPiGreco*dOmega)/a2dVarPit*(a2dVarRn - &
            a2dVarRhoA*dCp*(a2dVarLSTUpd - a2dVarTaK)/a2dVarRatm - &
            a2dVarRhoA*a2dVarLambda*(0.611*exp(17.3*(a2dVarLSTUpd - dTRef)/ &
            (237.3 + a2dVarLSTUpd - dTRef)) - a2dVarEA)/(a2dVarPa*a2dVarRsurf)*0.622) - &
            2*dPiGreco*dOmega*(a2dVarLSTUpd - a2dVarTDeep))
            
            ! Check K3 RK argument with K maximum delta
            where (a2dVarK3.gt.dKDeltaMax/2)
                a2dVarK3 = dKDeltaMax/2
            endwhere
            ! Update LST using K3 argument
            a2dVarLSTUpd = a2dVarLSTPStep + a2dVarK3
            !------------------------------------------------------------------------------------------
            
            !------------------------------------------------------------------------------------------
            ! Compute K4 RK argument 
            a2dVarK4 = iIntDelta *( 2*sqrt(dPiGreco*dOmega)/a2dVarPit*(a2dVarRn - &
            a2dVarRhoA*dCp*(a2dVarLSTUpd - a2dVarTaK)/a2dVarRatm - &
            a2dVarRhoA*a2dVarLambda*(0.611*exp(17.3*(a2dVarLSTUpd - dTRef)/ &
            (237.3 + a2dVarLSTUpd - dTRef)) - a2dVarEA)/(a2dVarPa*a2dVarRsurf)*0.622) - &
            2*dPiGreco*dOmega*(a2dVarLSTUpd - a2dVarTDeep))

            ! Check K4 RK argument with K maximum delta
            where (a2dVarK4.gt.dKDeltaMax)
                a2dVarK4 = dKDeltaMax
            endwhere
            ! Update LST using K$ argument
            a2dVarLSTUpd = a2dVarLSTPStep + (a2dVarK1 + (2.*(a2dVarK2 + a2dVarK3)) + a2dVarK4)/6.0
            !------------------------------------------------------------------------------------------
            
        elsewhere
            
            !------------------------------------------------------------------------------------------
            ! Otherwise
            a2dVarLSTUpd = 0.0
            !------------------------------------------------------------------------------------------
            
        endwhere
        
        ! Debug
        !call mprintf(.true., iINFO_Extra, checkvar(a2dVarLSTUpd, int(a2dVarDEM), 'LST INT END') )
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Info end
        !call mprintf(.true., iINFO_Extra, ' Phys :: Land surface model :: RK4 ... OK' )
        !------------------------------------------------------------------------------------------
        
    end subroutine HMC_Phys_LSM_Apps_RK4
    !------------------------------------------------------------------------------------------
    
    !------------------------------------------------------------------------------------------
    ! Subroutine for calculating deep soil temperature
    subroutine HMC_Phys_LSM_Apps_TDeep(iID, iRows, iCols, iT, &
                                   a2dVarDEM, &
                                   a2dVarTaK, &
                                   a2dVarTDeep)
                        
        !------------------------------------------------------------------------------------------
        ! Variable(s)
        integer(kind = 4)       :: iID, iRows, iCols, iT, iStep
        integer(kind = 4)       :: iDayThr, iTMarkedSteps, iDaySteps, iTdeepShift
        real(kind = 4)          :: dTRef
        
        real(kind = 4), dimension(iRows, iCols) :: a2dVarTaK, a2dVarDEM
        real(kind = 4), dimension(iRows, iCols) :: a2dVarTaK24, a2dVarTaK12
        
        real(kind = 4), dimension(iRows, iCols) :: a2dVarTDeep
        
        integer(kind = 4), dimension(iRows, iCols) :: a2iVarMask
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Constant(s)
        dTRef = oHMC_Namelist(iID)%dTRef
        iTdeepShift = oHMC_Namelist(iID)%iTdeepShift
        ! Static variable(s)
        iDaySteps = oHMC_Namelist(iID)%iDaySteps
        iTMarkedSteps = oHMC_Namelist(iID)%iTMarkedSteps
        a2iVarMask = oHMC_Vars(iID)%a2iMask
        
        
        ! Info start
        call mprintf(.true., iINFO_Extra, ' Phys :: Land surface model :: TDeep ... ' )
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Calculating deep soil temperature
        
        ! Initializing and updating temperature 3d mean field(s)
        if (all(oHMC_Vars(iID)%a3dTaK24.le.0.0))then
            
            call mprintf(.true., iINFO_Extra, ' Phys :: Land surface model :: TDeep :: '// &
                                              ' First mean temperature 3d field storing step ... ')

            do iStep=1, int(iDaySteps)
                where(a2iVarMask.gt.0.0)
                    oHMC_Vars(iID)%a3dTaK24(:,:,int(iStep)) =  a2dVarTaK
                elsewhere
                    oHMC_Vars(iID)%a3dTaK24(:,:,int(iStep)) =  0.0
                endwhere
            enddo
            
            ! Updating with new field
            where(a2iVarMask.gt.0.0)
                oHMC_Vars(iID)%a3dTaK24(:,:,int(iDaySteps)) =  a2dVarTaK + 10
            elsewhere
                oHMC_Vars(iID)%a3dTaK24(:,:,int(iDaySteps)) = 0.0
            endwhere
            
            call mprintf(.true., iINFO_Extra, ' Phys :: Land surface model :: TDeep :: '// &
                                              ' First mean temperature 3d field storing step ... OK')
            
        else
            ! Re-initializing 
            do iStep=2, int(iDaySteps)
                oHMC_Vars(iID)%a3dTaK24(:,:,int(iStep-1)) = oHMC_Vars(iID)%a3dTaK24(:,:,int(iStep))
            enddo
            ! Updating with new field
            where(a2iVarMask.gt.0.0)
                oHMC_Vars(iID)%a3dTaK24(:,:,int(iDaySteps)) =  a2dVarTaK
            elsewhere
                oHMC_Vars(iID)%a3dTaK24(:,:,int(iDaySteps)) = 0.0
            endwhere
            
        endif
        
        ! Calculating mean temperature last 12 and 24 hours
        where(a2iVarMask.gt.0.0)
            a2dVarTaK24 = sum(oHMC_Vars(iID)%a3dTaK24(:,:,1:int(iDaySteps)),3)/(iDaySteps)
            a2dVarTaK12 = sum(oHMC_Vars(iID)%a3dTaK24(:,:,int(iDaySteps/2+1):int(iDaySteps)),3)/int(iDaySteps/2.) 
        elsewhere
            a2dVarTaK24 = 0.0
            a2dVarTaK12 = 0.0
        endwhere
        
        ! Initializing and updating temperature 3d mean field(s)
        if (all(oHMC_Vars(iID)%a3dTaKMarked.le.0.0))then
            
            call mprintf(.true., iINFO_Extra, ' Phys :: Land surface model :: TDeep :: '// &
                                              ' First marked temperature 3d field storing step ... ')
            do iStep=1, int(iTMarkedSteps)
                where(a2iVarMask.gt.0.0)
                    oHMC_Vars(iID)%a3dTaKMarked(:,:,iStep) = 17.3 + dTRef
                elsewhere
                    oHMC_Vars(iID)%a3dTaKMarked(:,:,iStep) =  0.0
                endwhere
            enddo
            
            call mprintf(.true., iINFO_Extra, ' Phys :: Land surface model :: TDeep :: '// &
                                              ' First marked temperature 3d field storing step ... OK ')
        else
            ! Re-initializing 
            do iStep = 2, int(iTMarkedSteps)
                oHMC_Vars(iID)%a3dTaKMarked(:,:,int(iStep-1)) = oHMC_Vars(iID)%a3dTaKMarked(:,:,int(iStep))
            enddo
            
            ! Updating with new field
            where(a2iVarMask.gt.0.0)
                oHMC_Vars(iID)%a3dTaKMarked(:,:,iTMarkedSteps) = a2dVarTaK24 + (a2dVarTaK12 - a2dVarTaK24)/exp(1.0) 
            elsewhere
                oHMC_Vars(iID)%a3dTaKMarked(:,:,iTMarkedSteps) = 0.0
            endwhere
        
        endif
        !------------------------------------------------------------------------------------------

        !------------------------------------------------------------------------------------------
        ! Calculating deep soil temperature [k] 
        a2dVarTDeep = oHMC_Vars(iID)%a3dTaKMarked(:,:,iTMarkedSteps - iTdeepShift*int(iDaySteps/24) )        
        ! Info end
        call mprintf(.true., iINFO_Extra, ' Phys :: Land surface model :: TDeep ... OK' )
        !------------------------------------------------------------------------------------------
        
    end subroutine HMC_Phys_LSM_Apps_TDeep
    !------------------------------------------------------------------------------------------

    !------------------------------------------------------------------------------------------
    ! Subroutine for calculating thermal inertia
    subroutine HMC_Phys_LSM_Apps_ThermalInertia(iID, iRows, iCols, &
                                            a2dVarSM, a2dVarDEM, a2dVarPit)
        
        !------------------------------------------------------------------------------------------
        ! Variable(s)
        integer(kind = 4)       :: iID, iRows, iCols
        real(kind = 4)          :: dRhoS, dRhoW, dCpS, dCpW, dKq, dKw, dKo, dFqS, dPorS
        real(kind = 4), dimension(iRows, iCols) :: a2dVarSM, a2dVarDEM
        
        real(kind = 4) :: dRhoDa, dKDa
        real(kind = 4), dimension(iRows, iCols) :: a2dVarSMTemp, a2dVarC, a2dVarFqS, a2dVarKe
        real(kind = 4), dimension(iRows, iCols) :: a2dKsol, a2dKsolsat, a2dVarKs, a2dVarPit
        
        integer(kind = 4), dimension(iRows, iCols) :: a2iVarMask
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Constant(s)
        dRhoS = oHMC_Namelist(iID)%dRhoS
        dRhoW = oHMC_Namelist(iID)%dRhoW
        dCpS = oHMC_Namelist(iID)%dCpS
        dCpW = oHMC_Namelist(iID)%dCpW
        dKq = oHMC_Namelist(iID)%dKq
        dKw = oHMC_Namelist(iID)%dKw
        dKo = oHMC_Namelist(iID)%dKo
        dFqS = oHMC_Namelist(iID)%dFqS
        dPorS = oHMC_Namelist(iID)%dPorS
        ! Static data
        a2iVarMask = oHMC_Vars(iID)%a2iMask
        ! Info start
        call mprintf(.true., iINFO_Extra, ' Phys :: Land surface model :: ThermalInertia ... ' )
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Initialization variable(s)
        a2dVarSMTemp = 0.0;
        a2dVarC = 0.0;          ! Heat Capacity
        a2dVarFqS = 0.0;
        a2dVarKe = 0.0;
        a2dKsol = 0.0;
        a2dVarKs = 0.0;
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Quartz soil fraction [⅜]
        a2dVarFqS = dFqS
        ! Soil Moisture Temp
        a2dVarSMTemp = a2dVarSM
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Variable(s) conditions
        where( (a2iVarMask.gt.0.0) .and. (a2dVarSM.gt.10.0) ) a2dVarSMTemp = 1.0
        where( (a2iVarMask.gt.0.0) .and. (a2dVarSM.lt.0.0) ) a2dVarSMTemp = 0.0
        !------------------------------------------------------------------------------------------

        !------------------------------------------------------------------------------------------
        ! Heat Capacity [J K^-1 m^-3]
        where( a2iVarMask.gt.0.0)
            a2dVarC = (1 - dPorS)*dRhoS*dCpS + dPorS*a2dVarSMTemp*dRhoW*dCpW
        endwhere
        
        ! Air dry density [kg m^-3]
        dRhoDa = (1 - dPorS)*dRhoS
        ! Air dry conductivity [W m^-1 K^-1]
        dKDa = (0.135*dRhoDa+64.7)/(dRhoS - 0.947*dRhoDa)
        ! Solids conductivity [W m^-1 K^-1]	
        a2dKsol = dKq**(a2dVarFqS)*dKo**(1-a2dVarFqS)
        ! Saturated soil conductivity [W m^-1 K^-1]
        a2dKsolsat = a2dKsol**(1 - dPorS)*dKw**(dPorS)
        
        ! Kersten number (funzione solo del grado di saturazione VV per terreni sottili)
        where (a2iVarMask.gt.0.0)
            
            where(a2dVarSMTemp.ge.0.1) 
                    a2dVarKe = log10(a2dVarSMTemp) + 1
            elsewhere
                    a2dVarKe = log10(0.1) + 1
            endwhere

            ! Soil thermal conductivity [W m^-1 K^-1]
            a2dVarKs = a2dVarKe*(a2dKsolsat - dKDa) + dKDa
            ! Thermal Inertia[J m^-2 K S^-(1/2)]
            a2dVarPit = sqrt(a2dVarC*a2dVarKs)
            
            ! Thermal Inertia scaling (to reduce max values from 2500 to 600 --> temporary fixing)
            a2dVarPit = a2dVarPit/5 + 1500

        elsewhere
            a2dVarPit = 0.0
        endwhere
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Info end
        call mprintf(.true., iINFO_Extra, ' Phys :: Land surface model :: ThermalInertia ... OK' )
        !------------------------------------------------------------------------------------------
        
    end subroutine HMC_Phys_LSM_Apps_ThermalInertia
    !------------------------------------------------------------------------------------------
 
    !------------------------------------------------------------------------------------------
    ! Subroutine for calculating Richardson number
    subroutine HMC_Phys_LSM_Apps_Richardson(iID, iRows, iCols, & 
                                        a2dVarDEM, &
                                        a2dVarWind, a2dVarTaK, a2dVarPa, &
                                        a2dVarLST, & 
                                        a2dVarRb)
        
        !------------------------------------------------------------------------------------------
        ! Variable(s)
        integer(kind = 4) :: iID, iRows, iCols
        
        real(kind = 4) :: dZRef, dCp, dG, dRd
        
        real(kind = 4), dimension(iRows, iCols) :: a2dVarDEM
        real(kind = 4), dimension(iRows, iCols) :: a2dVarWind, a2dVarTaK, a2dVarPa, a2dVarLST
        real(kind = 4), dimension(iRows, iCols) :: a2dVarTp, a2dVarTp0
        real(kind = 4), dimension(iRows, iCols) :: a2dVarRb
        
        integer(kind = 4), dimension(iRows, iCols) :: a2iVarMask
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Constant(s)
        dZRef = oHMC_Namelist(iID)%dZRef
        dG = oHMC_Namelist(iID)%dG
        dCp = oHMC_Namelist(iID)%dCp
        dRd = oHMC_Namelist(iID)%dRd
        ! Static data
        a2iVarMask = oHMC_Vars(iID)%a2iMask
        ! Info start
        call mprintf(.true., iINFO_Extra, ' Phys :: Land surface model :: Richardson ... ' )
        !------------------------------------------------------------------------------------------

        !------------------------------------------------------------------------------------------
        ! Variable(s) initialization
        a2dVarTp = 0.0; a2dVarTp0 = 0.0; a2dVarRb = -0.9
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Calculating distributed Richardson number (Richardson from -1 to 0 values)
        where((a2iVarMask.gt.0.0) .and. (a2dVarWind.gt.0.0))
            
            a2dVarTp = a2dVarTaK*(1000.0/a2dVarPa)**(dRd/dCp)
            a2dVarTp0 = a2dVarLST*(1000.0/a2dVarPa)**(dRd/dCp)
            a2dVarRb = (dG/a2dVarTp)*(a2dVarTp - a2dVarTp0)*dZRef/(a2dVarWind**2) 
            
        elsewhere(a2iVarMask.gt.0.0)
            
            a2dVarTp = a2dVarTaK*(1000.0/a2dVarPa)**(dRd/dCp)
            a2dVarTp0 = a2dVarLST*(1000.0/a2dVarPa)**(dRd/dCp)
            a2dVarRb = (dG/a2dVarTp)*(a2dVarTp - a2dVarTp0)*dZRef/(0.1**2)
            
        elsewhere(a2iVarMask.le.0.0)
            
            a2dVarRb = 0.0
            a2dVarTp0 = 0.0
            a2dVarTp = 0.0
            
        endwhere
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Debug
        !call surf(a2dVarRb,pm3d='pm3d implicit map', palette='rgbformulae 31, -11, 32')
        !write(*,*) 'VAR Rb' , sum(a2dVarRb)/max(1,count(a2dVarRb.gt.-1.0))
        !write(*,*) 'VAR TaK' , sum(a2dVarTaK)/max(1,count(a2dVarTaK.gt.0.0))
        !write(*,*) 'VAR LST' , sum(a2dVarLST)/max(1,count(a2dVarLST.gt.0.0))
        !write(*,*) 'VAR Tp' , sum(a2dVarTp)/max(1,count(a2dVarTp.gt.0.0))
        !write(*,*) 'VAR Tp0' , sum(a2dVarTp0)/max(1,count(a2dVarTp0.gt.0.0))
        !write(*,*) 'VAR Pa' , sum(a2dVarPa)/max(1,count(a2dVarPa.gt.0.0))
        
        ! Info end
        call mprintf(.true., iINFO_Extra, ' Phys :: Land surface model :: Richardson ... OK' )
        !------------------------------------------------------------------------------------------
        
    end subroutine HMC_Phys_LSM_Apps_Richardson
    !------------------------------------------------------------------------------------------
    
    !------------------------------------------------------------------------------------------
    ! Subroutine for calculating canopy resistance by Jarvis-type formualtion 
    ! (source: LSA-SAF DMET Algorithm Theoretical Basis Document)
    subroutine HMC_Phys_LSM_Apps_Rsurf_Jarvis(iID, iRows, iCols, sTime, & 
                                        a2dVarDEM, a2dVarBareSoil, a2dVarRb, a2dVarBF, a2dVarBF_BareSoil, &
                                        a2dVarWind, a2dVarEA, &
                                        a2dVarEAsat, a2dVarSM, & 
                                        a2dVarIncRad, a2dVarPa, dTRef, &
                                        a2dVarRatm, a2dVarRsurf, a2dVarRsurf_pot)
                                        
        !------------------------------------------------------------------------------------------
        ! Variable(s)
        integer(kind = 4) :: iID, iRows, iCols

        real(kind = 4) :: dTRef, dCO2atm, dNPP_An_ratio, dD0, dG0, dPatm0, dPVKconst, dMc
        real(kind = 4) :: dVarCH, dVarCHn, dTfreeze, dMolConv
        
        real(kind = 4) :: dA, dB, dC, dVarPSI, dRSmin_BareSoil, dSMsat
        
        real(kind = 4), dimension(iRows, iCols) :: a2dVarDEM, a2dVarIncRad, a2dVarLAI, a2dVarPa
        real(kind = 4), dimension(iRows, iCols) :: a2dVarWind, a2dVarBF, a2dVarEA, a2dVarEAsat, a2dVarSM
        real(kind = 4), dimension(iRows, iCols) :: a2dVarPSIstableLE, a2dVarPSIstableM, a2dVarPSIstableF, a2dVarRb
        real(kind = 4), dimension(iRows, iCols) :: a2dVarRSmin, a2dVarGd, a2dVarHveg, a2dVarBareSoil, a2dVarBF_BareSoil
        real(kind = 4), dimension(iRows, iCols) :: a2dVarZmeas, a2dVarZdisp, a2dVarZroughM, a2dVarZroughLE
        real(kind = 4), dimension(iRows, iCols) :: a2dVarfS, a2dVarfDa, a2dVarCtWP
        real(kind = 4), dimension(iRows, iCols) :: a2dVarCatm, a2dVarRatm, a2dVarRsurf, a2dVarRsurf_pot, a2dVarRcan, a2dVarRcan_pot       
        real(kind = 4), dimension(iRows, iCols) :: a2dVarFC
        
        integer(kind = 4), dimension(iRows, iCols) :: a2iVarMask
        
        character(len = 19)       :: sTime
        character(len = 12)       :: sTimeMonth      
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------    
        ! Static parameters(s)
        dVarPSI = log(2.0)
        dPVKconst = 0.4;                     ! Prandtl-von Karman constant for turbulent flows velocity distribution [-]
        dTfreeze = 273.2;                    ! Freezing temperature [°K]
        dA = 0.85;                           ! empirical parameter for solar radiation factor (Jarvis-type parameters)
        dB = 0.004;                          ! empirical parameter for solar radiation factor (Jarvis-type parameters)
        dC = 0.05;                           ! empirical parameter for solar radiation factor (Jarvis-type parameters)
        dRSmin_BareSoil = 250.0;             ! [s/m] minimum 'stomatal' resistance for bare soil as in DMET - LSASAF product manual 
        dSMsat = 1.0;                        ! soil moisture content at saturation [-]
        !------------------------------------------------------------------------------------------  
        
        !------------------------------------------------------------------------------------------
        ! Initialization variable(s)
        a2dVarZmeas = 0.0;                  ! height of the measurement of wind speed [m]
        a2dVarZdisp = 0.0;                  ! zero-plane displacement height [m]
        a2dVarZroughM = 0.0;                ! roughness height for Momentum transfer[m]
        a2dVarZroughLE = 0.0;               ! roughness height for Latent Heat transfer[m]
        a2dVarCatm = -9999.0;               ! Bulk Canopy conductance [m/s]
        a2dVarRcan = -9999.0;               ! Bulk Canopy resistance [s/m]
        a2dVarRcan_pot = -9999.0;           ! Bulk Canopy resistance without limitation caused by shortage of water in the soils [s/m]
        a2dVarRatm = -9999.0;               ! Bulk Atmospheric resistance [s/m]
        a2dVarRsurf = -9999.0;              ! Surface resistance [s/m]
        a2dVarRsurf_pot = -9999.0;          ! Surface resistance without limitation caused by shortage of water in the soils [s/m]
        a2dVarPSIstableLE = -9999.0;        ! stability factor for Water vapor transfer[-] computed by Richardson number
        a2dVarPSIstableM = -9999.0;         ! stability factor for Momentum transfer[-] computed by Richardson number
        a2dVarPSIstableF = 1.0;             ! final stability factor [-]
        a2dVarfS = 1.0;                     ! limitation factor due to not optimal solar radiation (Jarvis type parameter)
        a2dVarfDa = 1.0;                    ! limitation factor due to water vapour deficit in the atmosphere (for tree only - Jarvis type parameter)
        a2dVarFC = 1.0;                     ! Fractional Vegetation Cover
               
        ! Load Static variable(s) --> mandatory
        a2iVarMask = oHMC_Vars(iID)%a2iMask
        a2dVarRSmin = oHMC_Vars(iID)%a2dRSmin   ! Minimum stomatal resistance [s/m]
        a2dVarGd = oHMC_Vars(iID)%a2dGd         ! Vegetation-dependant parameters for computing limitation due to water vapor deficit [-]
        a2dVarHveg = oHMC_Vars(iID)%a2dHveg     ! Vegetation height [m]
        ! Load Static variable(s) --> not mandatory (if gridded data does not exist --> 0.4*Ct)
        a2dVarCtWP = oHMC_Vars(iID)%a2dCtWP     ! Soil water permanent wilting point [-]

        ! Load dynamic variable(s) --> mandatory [LSA-SAF --> LAI --> MDLAI (1 day) ]
        a2dVarLAI = oHMC_Vars(iID)%a2dLAI
        ! Load Static variable(s) --> not mandatory (if gridded data does not exist FC = f(LAI) )
        a2dVarFC = oHMC_Vars(iID)%a2dFC
        
        ! Info start
        call mprintf(.true., iINFO_Extra, ' Phys :: Land surface model :: Jarvis Canopy Resistance ... ' )
        !------------------------------------------------------------------------------------------

        !------------------------------------------------------------------------------------------
        ! Calculating PSI stable values for LE and Momentum tranfert (source Dingman book pag. 599)
        where (a2dVarRb.lt.-0.03 .and. a2iVarMask.gt.0.0)
            a2dVarPSIstableLE = 1.3 * (1-18*a2dVarRb)**(-1/4.0)
            a2dVarPSIstableM = (1 - 18*a2dVarRb)**(-1/4.0)
        elsewhere (a2dVarRb.ge.-0.03 .and. a2dVarRb.le.0.0 .and. a2iVarMask.gt.0.0)
            a2dVarPSIstableLE = (1 - 18*a2dVarRb)**(-1/4.0)
            a2dVarPSIstableM = (1 - 18*a2dVarRb)**(-1/4.0)
        elsewhere (a2dVarRb.gt.0.0 .and. a2dVarRb.lt.0.19 .and. a2iVarMask.gt.0.0)
            a2dVarPSIstableLE = (1 - 5.2*a2dVarRb)**(-1)
            a2dVarPSIstableM = (1 - 5.2*a2dVarRb)**(-1)
        endwhere
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Calculating final PSI stable values (source Dingman book pag. 599)
        a2dVarPSIstableF = 1/(a2dVarPSIstableM*a2dVarPSIstableLE)

        where (a2dVarPSIstableF.gt.3.0 .and. a2iVarMask.gt.0.0)
            a2dVarPSIstableF = 3.0
        elsewhere (a2dVarPSIstableF.lt.0.0 .and. a2iVarMask.gt.0.0)
            a2dVarPSIstableF = 0.0
        endwhere    

        ! Vegetation derived heights [m] (source Dingman book pag. 296 and pag. 595)
        where ((a2iVarMask.gt.0.0) .and. (a2dVarHveg.gt.0.0)) 
            a2dVarZdisp = 0.67*a2dVarHveg       ! zero-plane displacement [2/3 of Hveg]
            a2dVarZroughM = 0.1*a2dVarHveg      ! roughness height for momentum
            a2dVarZroughLE = 0.1*a2dVarZroughM  ! roughness height for LE 
            a2dVarZmeas = 2.0 + a2dVarHveg      ! measurement(s) height
        elsewhere (a2iVarMask.gt.0.0) ! open water, snow
            a2dVarZdisp = 0.0
            a2dVarZroughM = 0.00023
            a2dVarZroughLE = 0.1*a2dVarZroughM
            a2dVarZmeas = 2.0 + a2dVarHveg                
        endwhere
        
        ! Atmospheric Conductance [m/s] (source Dingman book pag. 296)
        where (a2iVarMask.gt.0.0)
            a2dVarCatm = a2dVarPSIstableF * a2dVarWind*(dPVKconst**2)/(log((a2dVarZmeas-a2dVarZdisp)/a2dVarZroughM)* &
                        log((a2dVarZmeas-a2dVarZdisp)/a2dVarZroughLE))   
        endwhere
        
        ! Atmospheric Resistance [s/m] (source Dingman book pag. 296)
        where (a2iVarMask.gt.0.0 .and. a2dVarCatm.le.0.0001)
            a2dVarCatm = 0.0001
        endwhere
        
        where (a2iVarMask.gt.0.0)
            a2dVarRatm = 1.0 / a2dVarCatm
        endwhere
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Canopy Resistance [s/m] - Jarvis-type 
        ! Limitation due to not optimal solar radiation
        where (a2iVarMask.gt.0.0 .and. a2dVarIncRad.ge.0.0)
            a2dVarfS = ((dB * a2dVarIncRad) + dC)/(dA * ((dB * a2dVarIncRad) + 1))
        endwhere       

        !check upper limit to be maximum 1
        where (a2iVarMask.gt.0.0 .and. a2dVarfS.gt.1.0)
            a2dVarfS = 1.0
        endwhere

        ! Limitation for trees due to vapour deficit in the atmosphere 
        where (a2iVarMask.gt.0.0 .and. a2dVarGd.ge.0.0)
            a2dVarfDa = exp(-a2dVarGd * 10 * (a2dVarEAsat - a2dVarEA))
        endwhere

        ! Computation of canopy resistance
        where ((a2iVarMask.gt.0.0) .and. (a2dVarBareSoil.gt.0.0)) ! bare soil
            ! Approach proposed by Sellers et al. 1992 for soil surface resistance 
            ! (Sellers, P. J., A.Berry, J., Collatz, G. J., Field, C. B., Hall, F. G. (1992) Canopy reflectance,....)
            ! a2dVarRcan = exp(8.206-4.255*(a2dVarSM - a2dVarCtWP) / (dSMsat - a2dVarCtWP))
            ! GIULIA: prova ritorno alla vecchia beta anche per suolo
            a2dVarRcan =  exp(8.206-4.255) / (a2dVarBF)
            a2dVarRcan_pot = exp(8.206-4.255)
        elsewhere ((a2iVarMask.gt.0.0) .and. (a2dVarGd.lt.0.0)) ! rocks, urban, open water
            a2dVarRcan = a2dVarRSmin 
            a2dVarRcan_pot = a2dVarRSmin
        elsewhere ((a2iVarMask.gt.0.0) .and. (a2dVarLAI.lt.0.0)) ! LAI not valid - assumed urban
            a2dVarRcan = a2dVarRSmin 
            a2dVarRcan_pot = a2dVarRSmin      
        elsewhere (a2iVarMask.gt.0.0) ! all vegetated pixels according to land cover map
            ! a2dVarRcan = a2dVarFC * a2dVarRSmin / (a2dVarfS * a2dVarfDa * a2dVarBF * a2dVarLAI) + &
            !             (1-a2dVarFC)*exp(8.206-4.255*(a2dVarSM - a2dVarCtWP) / (dSMsat - a2dVarCtWP))
            ! GIULIA: prova ritorno alla vecchia beta anche per suolo
            a2dVarRcan = a2dVarFC * a2dVarRSmin / (a2dVarfS * a2dVarfDa * a2dVarBF * a2dVarLAI) + &
                        (1-a2dVarFC)*exp(8.206-4.255) / (a2dVarBF)
            a2dVarRcan_pot = a2dVarFC * a2dVarRSmin / (a2dVarfS * a2dVarfDa * a2dVarLAI) + &
                        (1-a2dVarFC)*exp(8.206-4.255)
        endwhere            
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! not considering water stress as limitation for ET
        if (oHMC_Namelist(iID)%iFlagBetaET .eq. 0) then
            a2dVarRcan = a2dVarRcan_pot
        endif 
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Surface Resistance [s/m]
        where (a2iVarMask.gt.0.0)
            a2dVarRsurf = a2dVarRcan + a2dVarRatm
            a2dVarRsurf_pot = a2dVarRcan_pot + a2dVarRatm 
        endwhere
        
        ! Info end
        call mprintf(.true., iINFO_Extra, ' Phys :: Land surface model :: Jarvis Canopy Resistance ... OK  ' )
        !------------------------------------------------------------------------------------------
       
    end subroutine HMC_Phys_LSM_Apps_Rsurf_Jarvis
   !------------------------------------------------------------------------------------------

end module HMC_Module_Phys_LSM_Apps
!------------------------------------------------------------------------------------
 

!------------------------------------------------------------------------------------
! File:   HMC_Module_ForceRestore.f90
!
! Author:   Fabio Delogu
! Date:     20150210
!
! Physics subroutine(s) for LSM model
!------------------------------------------------------------------------------------

!------------------------------------------------------------------------------------
! Module Header
module HMC_Module_Phys_LSM

    !------------------------------------------------------------------------------------
    ! External module(s) 
    use HMC_Module_Namelist,            only:   oHMC_Namelist
    use HMC_Module_Vars_Loader,         only:   oHMC_Vars
    
    use HMC_Module_Tools_Debug
    
    use HMC_Module_Phys_LSM_Apps,       only:  HMC_Phys_LSM_Apps_BetaFunction, &
                                                HMC_Phys_LSM_Apps_ThermalInertia, & 
                                                HMC_Phys_LSM_Apps_Richardson, &
                                                HMC_Phys_LSM_Apps_CH, &
                                                HMC_Phys_LSM_Apps_TDeep, &
                                                HMC_Phys_LSM_Apps_RK4, &
                                                HMC_Phys_LSM_Apps_Rsurf_Jarvis
                                                                        
    ! Implicit none for all subroutines in this module
    implicit none
    !------------------------------------------------------------------------------------------

contains

    !------------------------------------------------------------------------------------------
    ! Subroutine for calculating force-restore equation
    subroutine HMC_Phys_LSM_Cpl(iID, iRows, iCols, sTime)
                                                   
        !-------------------------------------------------------------------------------------
        ! Variable(s) declaration
        integer(kind = 4)           :: iID, iRows, iCols, iT, iFlagDynVeg

        integer(kind = 4)           :: iDtDataForcing, iDtZero, iDtUpd, iDtDelta
        
        integer(kind = 4)           :: iNSoil, iDaySteps
        integer(kind = 4)           :: iIntDelta, iIntStep
        real(kind = 4)              :: dTRef, dSigma, dEpsS, dCp
        real(kind = 4)              :: dVarLST, dVarRn, dVarH, dVarLE
        
        integer(kind = 4), dimension(iRows, iCols)  :: a2iVarMask
        real(kind = 4), dimension(iRows, iCols)     :: a2dVarDEM, a2dVarS
        
        character(len=19)           :: sTime

        ! State variable(s) units
        !a2dVarPa:          atmospheric pressure [Kpa]
        !a2dVarTa:          air temperature [C]
        !a2dVarIncRad:      incoming radiation [W/m^2]
        !a2dVarWind:        wind speed [m/s]
        !a2dVarRelHum:      relative humidity [%]
        
        real(kind = 4),  dimension(iRows, iCols) :: a2dVarET, a2dVarETpot
        real(kind = 4),  dimension(iRows, iCols) :: a2dVarVTot
        real(kind = 4),  dimension(iRows, iCols) :: a2dVarLST
        
        real(kind = 4), dimension(iRows, iCols) :: a2dVarTa, &
                                             a2dVarIncRad, a2dVarWind, & 
                                             a2dVarRelHum, a2dVarPa, &
                                             a2dVarAlbedo
        
        real(kind = 4), dimension(iRows, iCols) :: a2dVarTaK, a2dVarSM
        real(kind = 4), dimension(iRows, iCols) :: a2dVarPit, a2dVarRb
        
        real(kind = 4), dimension(iRows, iCols) :: a2dVarLambda, a2dVarRhoW, &
                                             a2dVarEA, a2dVarEAsat, a2dVarEpsA, a2dVarRhoA

        real(kind = 4), dimension(iRows, iCols) :: a2dVarLSTPStep, a2dVarLSTUpd, a2dVarETUpd
        
        real(kind = 4), dimension(iRows, iCols) :: a2dVarTDeep, a2dVarBF, a2dVarCH, a2dVarEpsS, a2dVarBF_BareSoil, a2dVarBareSoil 
        
        real(kind = 4), dimension(iRows, iCols) :: a2dVarRatm, a2dVarRsurf, a2dVarRsurf_pot
        
        real(kind = 4), dimension(iRows, iCols) :: a2dVarRn, a2dVarH, a2dVarLE, a2dVarG
        real(kind = 4), dimension(iRows, iCols) :: a2dVarEF
        real(kind = 4), dimension(iRows, iCols) :: a2dVarSnowMask
        real(kind = 4), dimension(iRows, iCols) :: a2dVarLST_Diff
        
        real(kind = 4)              :: dVarMeanLST, dVarMeanH, dVarMeanLE
        real(kind = 4)              :: dVarMeanEF, dVarMeanET, dVarMeanRn   
        real(kind = 4)              :: dVarLST_DiffMax
        
        character(len = 6)               :: sDtDelta
        character(len = 10)              :: sVarLST, sVarRn, sVarH, sVarLE
        character(len = 10), parameter  :: sFMTVarLST = "(F5.1)"
        character(len = 10), parameter  :: sFMTVarRn = "(F6.1)"
        character(len = 10), parameter  :: sFMTVarH = "(F6.1)"
        character(len = 10), parameter  :: sFMTVarLE = "(F6.1)"
        character(len = 10), parameter  :: sFMTVarDtDelta = "(I4)"
        !-------------------------------------------------------------------------------------
          
        !-------------------------------------------------------------------------------------
        ! Local variable(s) initialization
        a2iVarMask = 0; a2dVarSM = 0.0; a2dVarTaK = 0.0;
        
        a2dVarBF = 0.0;         ! Beta function
        a2dVarBF_BareSoil = 0.0; ! Beta function for bare soil 
        a2dVarPit = 0.0;        ! Thermal Inertia
        a2dVarRb = -0.9;        ! Richardson number
        a2dVarCH = 0.0;         ! CH
        a2dVarTDeep = 0.0;      ! Deep soil temperature
       
        a2dVarLambda = 0.0; a2dVarRhoW = 0.0; a2dVarEA = 0.0; a2dVarEAsat = 0.0;
        a2dVarEpsA = 0.0; a2dVarRhoA = 0.0; 
        
        a2dVarRatm = 0.0; a2dVarRsurf = 0.0; a2dVarRsurf_pot = 0.0;
        
        a2dVarRn = 0.0; a2dVarH = 0.0; a2dVarLE = 0.0; a2dVarG = 0.0; 

        a2dVarLSTPStep = 0.0; a2dVarLSTUpd = 0.0;
        a2dVarET = 0.0; a2dVarETpot = 0.0; a2dVarETUpd = 0.0; a2dVarEF = 0.0;
        a2dVarSnowMask = 0.0
        
        dVarMeanLST = 0.0; dVarMeanH = 0.0; dVarMeanLE = 0.0; 
        dVarMeanEF = 0.0; dVarMeanET = 0.0; dVarMeanRn = 0.0;
        
        iDtZero = 0; iDtUpd = 0;
        
        a2dVarLST_Diff = 0.0; dVarLST_DiffMax = 12.0;
        !-------------------------------------------------------------------------------------
        
        !-------------------------------------------------------------------------------------
        ! Extracting constant(s)
        dCp = oHMC_Namelist(iID)%dCp
        dTRef = oHMC_Namelist(iID)%dTRef
        dSigma = oHMC_Namelist(iID)%dSigma
        dEpsS = oHMC_Namelist(iID)%dEpsS
       
        ! Extracting flag(s)
        iFlagDynVeg = oHMC_Namelist(iID)%iFlagDynVeg
        
        ! Extracting data information
        iDtDataForcing = oHMC_Namelist(iID)%iDtData_Forcing
        
        ! Extracting static variable(s)
        a2iVarMask = oHMC_Vars(iID)%a2iMask
        a2dVarDEM = oHMC_Vars(iID)%a2dDem
        a2dVarS = oHMC_Vars(iID)%a2dS
        a2dVarBareSoil = oHMC_Vars(iID)%a2dBareSoil

        ! Extracting dynamic forcing variable(s)
        a2dVarTa = oHMC_Vars(iID)%a2dTa
        a2dVarWind = oHMC_Vars(iID)%a2dW
        a2dVarIncRad = oHMC_Vars(iID)%a2dK
        a2dVarRelHum = oHMC_Vars(iID)%a2dRHum
        a2dVarPa = oHMC_Vars(iID)%a2dPres

        ! Extracting extra-dynamic forcing variable(s)
        a2dVarAlbedo = oHMC_Vars(iID)%a2dAlbedo

        ! Extracting dynamic state variable(s)
        a2dVarLSTPStep = oHMC_Vars(iID)%a2dLST          ! LST previous step
        a2dVarVTot = oHMC_Vars(iID)%a2dVTot		! Total soil volume
        a2dVarET = oHMC_Vars(iID)%a2dET                 ! Evapotranspiration
        a2dVarETpot = oHMC_Vars(iID)%a2dETpot           ! Potential Evapotranspiration
        a2dVarSnowMask = oHMC_Vars(iID)%a2dMaskS        ! Snow mask (to nullify evapotranspiration under snow)

        ! Info start
        call mprintf(.true., iINFO_Verbose, ' Phys :: Land surface model ... ' )
        !-------------------------------------------------------------------------------------
        
        !-------------------------------------------------------------------------------------
        ! Checking variable(s)
        where( a2dVarDEM.gt.0.0 )
            
            ! Soil Moisture [%]
            a2dVarSM = a2dVarVTot/a2dVarS
            ! Air temperature [K]
            a2dVarTaK = a2dVarTa + dTRef
            ! Relative Humidity [-]
            a2dVarRelHum = a2dVarRelHum/100.0;

        elsewhere
            
            a2dVarSM = 0.0
            a2dVarTaK = 0.0
            a2dVarLST = 0.0
            a2dVarRelHum = 0.0
            a2dVarWind = 0.0
            a2dVarPa = 0.0
           
        endwhere
        
        ! Initializing LST (initial step)
        if( ALL(a2dVarLSTPStep.le.0.0) ) then
            where(a2dVarTa.ge.-40.0.and.a2dVarDEM.gt.0.0) a2dVarLSTPStep = a2dVarTaK + 1.0
        endif
        
        !a2dVarLSTPStep = check2Dvar(a2dVarLSTPStep, oHMC_Vars(iID)%a2iMask, -70.0,  60.0,   273.15 )
        !-------------------------------------------------------------------------------------
        
        !-------------------------------------------------------------------------------------
        ! Nullify evapotranspiration under snow mask
        where( a2dVarSnowMask.eq.1.0 )
            a2dVarET = -1   ! to avoid updating evapotranspiration at the end of subroutine
        endwhere
        !-------------------------------------------------------------------------------------
        
        !-------------------------------------------------------------------------------------
        ! Debug
        if (iDEBUG.gt.0) then
            call mprintf(.true., iINFO_Extra, ' ========= LSM START =========== ')  
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarTa, a2iVarMask, 'TA ') )
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarWind, a2iVarMask, 'WIND ') )
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarIncRad, a2iVarMask, 'INCRAD ') )
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarRelHum, a2iVarMask, 'RELHUM ') )
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarPa, a2iVarMask, 'PA ') )
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarAlbedo, a2iVarMask, 'ALBEDO ') )
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarET, a2iVarMask, 'ET START ') )
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarVTot, a2iVarMask, 'VTOT START ') )
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarLSTPStep, a2iVarMask, 'LST START ') )
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarSM, a2iVarMask, 'SM START ') )
            call mprintf(.true., iINFO_Extra, '') 
        endif
        !-------------------------------------------------------------------------------------

        !-------------------------------------------------------------------------------------
        ! Subroutine for calculating beta function
        call HMC_Phys_LSM_Apps_BetaFunction( iID, iRows, iCols, &
                                              a2dVarSM, a2dVarDEM, &
                                              a2dVarBF, a2dVarBF_BareSoil)   
        !-------------------------------------------------------------------------------------
  
        !-------------------------------------------------------------------------------------
        ! Subroutine for calculating thermal inertia
        call HMC_Phys_LSM_Apps_ThermalInertia( iID, iRows, iCols, &
                                                  a2dVarSM, a2dVarDEM, &
                                                  a2dVarPit )
        !-------------------------------------------------------------------------------------
	
	!-------------------------------------------------------------------------------------
        ! Subroutine for calculating Richardson number
        call HMC_Phys_LSM_Apps_Richardson( iID, iRows, iCols, &
                                              a2dVarDEM, &
                                              a2dVarWind, a2dVarTaK, a2dVarPa, &
                                              a2dVarLSTPStep, &
                                              a2dVarRb )
        !-------------------------------------------------------------------------------------
                           
        !-----------------------------------------------------------------------------------------
        ! Subroutine for calculating desp soil temperature (TDeep)
        call HMC_Phys_LSM_Apps_TDeep( iID, iRows, iCols, iT, &
                                         a2dVarDEM, &
                                         a2dVarTaK, &
                                         a2dVarTDeep )
        !-----------------------------------------------------------------------------------------
                                    
        !-----------------------------------------------------------------------------------------
        ! Calculating variables for energy fluxes
        where( a2dVarDEM.gt.0.0 )
            
            ! Latent heat of vaporization [J/kg]
            a2dVarLambda = (2.5 - 2.36*0.001*(a2dVarTa))*1000000  
            ! Water density [kg/m^3]
            a2dVarRhoW = 1000.0 - 0.019549*abs(a2dVarTa - 3.98)**1.68
            ! Vapor pressure [kPa] --> RelHum [%], Ta [C]
            a2dVarEA = (a2dVarRelHum)*0.611*exp(17.3*a2dVarTa/(237.3 + a2dVarTa))
            ! Vapor pressure at saturation [kPa] --> RelHum [%], Ta [C]
            a2dVarEAsat = (a2dVarRelHum)*0.611*exp(17.3*a2dVarTa/(237.3 + a2dVarTa))
            ! Atmospheric actual emissivity [%] --> ea[kPa]*10 = [millibars]
            a2dVarEpsA = 0.740 + 0.0049*a2dVarEA*10.0
            ! Air density [kg/m^3] --> Gas air constant R=0.288, Pa [kPa], Ta [K]
            a2dVarRhoA = a2dVarPa/(a2dVarTaK*0.288)
            
        elsewhere
            
            a2dVarLambda = 0.0
            a2dVarRhoW = 0.0
            a2dVarEA = 0.0
            a2dVarEAsat = 0.0
            a2dVarEpsA = 0.0
            a2dVarRhoA = 0.0
            
        endwhere
        !-----------------------------------------------------------------------------------------
  
        !-------------------------------------------------------------------------------------
        ! Subroutine for calculating Surface conductance/resistance for LE and H computation                                                                                      
        if ( iFlagDynVeg.eq.0 ) then
            ! Use constant CH
            call HMC_Phys_LSM_Apps_CH(iID, iRows, iCols, sTime, &
                              a2dVarDEM, &
                              a2dVarRb, &
                              a2dVarBF, a2dVarWind, &
                              a2dVarRatm, a2dVarRsurf, a2dVarRsurf_pot)
                
        else
            ! Use vegetation dynamic approach
            call HMC_Phys_LSM_Apps_Rsurf_Jarvis(iID, iRows, iCols, sTime, & 
                                        a2dVarDEM, a2dVarBareSoil, a2dVarRb, a2dVarBF, a2dVarBF_BareSoil, &
                                        a2dVarWind, a2dVarEA, &
                                        a2dVarEAsat, a2dVarSM, & 
                                        a2dVarIncRad, a2dVarPa, dTRef, &
                                        a2dVarRatm, a2dVarRsurf, a2dVarRsurf_pot)
        endif
        !-----------------------------------------------------------------------------------------
        
        !-----------------------------------------------------------------------------------------
	! Net Radiation [W/m^2] --> sigma [W/m^2 K^4], EpsA [%], EpsS [%], albedo [-], Ta [K], LST [K], K [W/m^2]
        where( a2dVarDEM.gt.0.0 )
            a2dVarRn = a2dVarIncRad*(1.0 - a2dVarAlbedo) + dSigma*a2dVarEpsA*a2dVarTaK**4 - &
                                              dSigma*dEpsS*a2dVarLSTPStep**4
        elsewhere
            a2dVarRn = 0.0
        endwhere

        !-----------------------------------------------------------------------------------------

        !-----------------------------------------------------------------------------------------
        ! Calculating land surface temperature using runge-kutta (LST updating)
        iDtDelta = MIN(int(int(iDtDataForcing)),900)
        !iDtDelta = int(int(iDtDataForcing)/4) 
        !do iIntStep = 1,  int(iDtDataForcing), iDtDelta
        do iIntStep = 1,  int(iDtDataForcing)/int(iDtDelta)

            call HMC_Phys_LSM_Apps_RK4( iID, iRows, iCols, & 
                                           dTRef, &
                                           float(iIntStep), iDtDelta, &
                                           a2dVarTDeep, a2dVarPit, a2dVarRatm, a2dVarRsurf, &
                                           a2dVarRn, &
                                           a2dVarRelHum, a2dVarWind, a2dVarTaK, a2dVarPa, & 
                                           a2dVarLambda, a2dVarEA, a2dVarRhoA, &
                                           a2dVarLSTPStep, a2dVarLSTUpd )

        enddo
        
        ! Check LST update delta
        where(a2dVarDem.gt.0.0)
            a2dVarLST_Diff = abs(a2dVarLSTUpd - a2dVarLSTPStep)
        elsewhere
            a2dVarLST_Diff = 0.0
        endwhere
        
        ! LST checking
        where(a2dVarDem.gt.0.0)
                where(a2dVarLST_Diff.gt.dVarLST_DiffMax*iDtDataForcing/3600.0) ! Where dLST is too large put old LST
                        a2dVarLSTUpd = a2dVarLSTPStep	
                endwhere

                where(a2dVarLSTUpd.lt.273.15 - 70.0) ! Where LST is too small put old LST
                        a2dVarLSTUpd = a2dVarLSTPStep	
                endwhere

                where(a2dVarLSTUpd.gt.273.15 + 70.0) ! 70 degree as maximum of soil temperature
                        a2dVarLSTUpd = a2dVarLSTPStep	
                endwhere
                
                ! Last check
                where((a2dVarLSTUpd.lt.273.15 - 70.0).or.(a2dVarLSTUpd.gt.273.15 + 70.0) ) ! Where LST is too large or too small put standard LST
                        a2dVarLSTUpd = 273.15 + 20	
                endwhere
                
        elsewhere
                a2dVarLSTUpd = 0.0
        endwhere
        !-----------------------------------------------------------------------------------------

        !-----------------------------------------------------------------------------------------
        ! Calculating heat fluxes and evapotraspiration
        ! Calculating EPS_S, H, LE and G
        where( a2dVarDEM.gt.0.0 )
            a2dVarEpsS = 0.611*exp(17.3*(a2dVarLSTUpd - dTRef)/(237.3 + a2dVarLSTUpd - dTRef))  
            a2dVarH = a2dVarRhoA*dCp*(a2dVarLSTUpd - a2dVarTaK)/a2dVarRatm
            a2dVarLE = a2dVarRhoA*a2dVarLambda*(a2dVarEpsS - a2dVarEA)/(a2dVarPa*a2dVarRsurf)*0.622
            a2dVarG = a2dVarH + a2dVarLE - a2dVarRn
            
        elsewhere
            a2dVarEpsS = 0.0
            a2dVarH = 0.0
            a2dVarLE = 0.0
            a2dVarG = 0.0
        endwhere

        ! Calculating EF and ET and ET potential
        where( (a2dVarLE.gt.0.0) .and. (a2dVarDEM.gt.0.0) .and. (a2dVarET.ge.0.0) )
            
            ! Evaporative Fraction [-]
            a2dVarEF = a2dVarLE/(a2dVarLE + a2dVarH)
            ! Evapotranspiration [mm]
            a2dVarETUpd = a2dVarLE/(a2dVarRhoW*a2dVarLambda)*1000*iDtDataForcing 
            ! Potential Evapotranspiration [mm]
            a2dVarETpot = a2dVarETUpd*a2dVarRsurf/a2dVarRsurf_pot 
            
        elsewhere( a2dVarDEM.gt.0.0 )
            
            a2dVarEF = 0.0
            a2dVarETUpd = 0.0
            a2dVarETpot = 0.0
            
        endwhere
        !-----------------------------------------------------------------------------------------

        !------------------------------------------------------------------------------------------
        ! LSM information time step
        dVarLST = sum(a2dVarLSTUpd, mask=a2dVarDem.gt.0.0)/max(1,count(a2dVarDem.gt.0.0))
        dVarH = sum(a2dVarH, mask=a2dVarDem.gt.0.0)/max(1,count(a2dVarDem.gt.0.0))
        dVarLE = sum(a2dVarLE, mask=a2dVarDem.gt.0.0)/max(1,count(a2dVarDem.gt.0.0))
        dVarRn = sum(a2dVarRn, mask=a2dVarDem.gt.0.0)/max(1,count(a2dVarDem.gt.0.0))
        
        write(sDtDelta, sFMTVarDtDelta) iDtDelta
        write(sVarLST, sFMTVarLST) dVarLST
        write(sVarRn, sFMTVarRn) dVarRn
        write(sVarH, sFMTVarH) dVarH
        write(sVarLE, sFMTVarLE) dVarLE
        call mprintf(.true., iINFO_Basic, ' Phys :: LSM :: IntegrationStep :: '//sDtDelta// ' [s]')
        call mprintf(.true., iINFO_Basic, ' Phys :: LSM :: AvgValue :: '// &
                                          ' LST: '//sVarLST//' [K] '// &
                                          ' Rn: '//sVarRn//' [W/m^2] '// &
                                          ' H: '//sVarH//' [W/m^2] '// &
                                          ' LE: '//sVarLE//' [W/m^2]')
        !------------------------------------------------------------------------------------------
        
        !-----------------------------------------------------------------------------------------
        ! Debug
        if (iDEBUG.gt.0) then
            call mprintf(.true., iINFO_Extra, '') 
            call mprintf(.true., iINFO_Verbose, checkvar(a2dVarETUpd, a2iVarMask, 'ET END ') )
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarLSTUpd, a2iVarMask, 'VTOT END ') )
            call mprintf(.true., iINFO_Verbose, checkvar(a2dVarLSTUpd, a2iVarMask, 'LST END ') )
            call mprintf(.true., iINFO_Verbose, checkvar(a2dVarSM, a2iVarMask, 'SM ') )
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarPit, a2iVarMask, 'PIT ') )
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarRb, a2iVarMask, 'RB ') )
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarCH, a2iVarMask, 'CH ') )
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarTDeep, a2iVarMask, 'TDEEP ') )
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarLambda, a2iVarMask, 'LAMBDA ') )
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarRhoW, a2iVarMask, 'RHOW ') )
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarEA, a2iVarMask, 'EA ') )
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarEAsat, a2iVarMask, 'EA ') )
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarEpsA, a2iVarMask, 'EPSA ') )
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarRhoA, a2iVarMask, 'RHOA ') )
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarEpsS, a2iVarMask, 'EPSS ') )
            call mprintf(.true., iINFO_Verbose, checkvar(a2dVarRn, a2iVarMask, 'RN ') )
            call mprintf(.true., iINFO_Verbose, checkvar(a2dVarH, a2iVarMask, 'H ') )
            call mprintf(.true., iINFO_Verbose, checkvar(a2dVarLE, a2iVarMask, 'LE ') )
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarG, a2iVarMask, 'G ') )
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarEF, a2iVarMask, 'EF ') )
            call mprintf(.true., iINFO_Extra, ' ========= LSM END =========== ') 
        endif
        !-----------------------------------------------------------------------------------------
        
        !-----------------------------------------------------------------------------------------
        ! Update energy fluxes and evaporative fraction
        oHMC_Vars(iID)%a2dRn = a2dVarRn
        oHMC_Vars(iID)%a2dH = a2dVarH
        oHMC_Vars(iID)%a2dLE = a2dVarLE
        oHMC_Vars(iID)%a2dG = a2dVarG
        oHMC_Vars(iID)%a2dEF = a2dVarEF

        ! Update LST
        oHMC_Vars(iID)%a2dLST = a2dVarLSTUpd

	! Update ET, ETpot and VTot
	oHMC_Vars(iID)%a2dET = a2dVarETUpd
        oHMC_Vars(iID)%a2dETpot = a2dVarETpot

	oHMC_Vars(iID)%a2dVTot = a2dVarVTot
        
        ! Info end
        call mprintf(.true., iINFO_Verbose, ' Phys :: Land surface model ... OK' )
	!-----------------------------------------------------------------------------------------
	
    end subroutine HMC_Phys_LSM_Cpl
    !------------------------------------------------------------------------------------------

end module HMC_Module_Phys_LSM
!------------------------------------------------------------------------------------
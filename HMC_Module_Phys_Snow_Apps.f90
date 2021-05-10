 !------------------------------------------------------------------------------------
! File:   HMC_Module_Phys_Snow_Apps.f90
!
! Author:   Simone Gabellani, Fabio Delogu
! Date:     201500715
!
! Snow applications module
!------------------------------------------------------------------------------------

!------------------------------------------------------------------------------------
! Module Header
module HMC_Module_Phys_Snow_Apps

    !------------------------------------------------------------------------------------
    ! External module(s) 
    use HMC_Module_Namelist,            only:   oHMC_Namelist
    use HMC_Module_Vars_Loader,         only:   oHMC_Vars
   
    use HMC_Module_Tools_Debug
    
    use HMC_Module_Tools_Generic,       only:   assimNudging
    
    ! Implicit none for all subroutines in this module
    implicit none
    !------------------------------------------------------------------------------------------

contains
    
    !------------------------------------------------------------------------------------------
    ! Subroutine to compute snow age
    subroutine HMC_Phys_Snow_Apps_Albedo(iID, iRows, iCols, &
                                         sTime, iTime, iGlacierValue, &
                                         a2dVarDem, a2iVarNature, &
                                         a2dVarTaC_MeanDays1, a2iVarAgeS, &
                                         a2dVarAlbedoS)

        !------------------------------------------------------------------------------------------
        ! Variable(s) declaration 
        integer(kind = 4)   :: iID, iRows, iCols

        integer(kind = 4)   :: iTime, iGlacierValue
        character(len = 19) :: sTime
        
        real(kind = 4), dimension(iRows, iCols)         :: a2dVarDem
        
        integer(kind = 4), dimension(iRows, iCols)      :: a2iVarAgeS, a2iVarNature
        real(kind = 4), dimension(iRows, iCols)         :: a2dVarTaC_MeanDays1, a2dVarAlbedoS
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Info start
        call mprintf(.true., iINFO_Extra, ' Phys :: Snow :: Albedo ... ' )
        !------------------------------------------------------------------------------------------

        !------------------------------------------------------------------------------------------
        ! Update snow albedo once each day
        if ( (sTime(12:13).eq.'00') .and. (iTime.gt.1) ) then
            
            ! Compute snow albedo
            where( (a2dVarDem.ge.0.0) .and. (a2dVarTaC_MeanDays1.gt.0.0) )
                a2dVarAlbedoS = 0.4 + 0.44*exp(dble(-a2iVarAgeS)*0.12)
            endwhere
            
            where ( (a2dVarDem.ge.0.0) .and. (a2dVarTaC_MeanDays1.le.0.0) )
                a2dVarAlbedoS = 0.4 + 0.44*exp(dble(-a2iVarAgeS)*0.05)
            endwhere
            
            ! Check snow albedo boundaries conditions
            where( (a2dVarDem.ge.0.0) .and. (a2dVarAlbedoS.le.0.5) )
                a2dVarAlbedoS = 0.5
            endwhere
            where( (a2dVarDem.ge.0.0) .and. (a2dVarAlbedoS.gt.0.95) )
                a2dVarAlbedoS = 0.95
            endwhere
            
            ! Update albedo where Age is 0!
            where( (a2dVarDem.ge.0.0) .and. (a2iVarAgeS.eq.0) )
                a2dVarAlbedoS = 0.95 
            endwhere  
            
            ! Set snow albedo glaciers condition
            where( (a2dVarDem.ge.0.0) .and. (a2iVarNature.eq.iGlacierValue) )
                a2dVarAlbedoS = 0.9
            endwhere
            
        endif
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Info end
        call mprintf(.true., iINFO_Extra, ' Phys :: Snow :: Albedo ... OK' )
        !------------------------------------------------------------------------------------------

    end subroutine HMC_Phys_Snow_Apps_Albedo
    !------------------------------------------------------------------------------------------
        
    !------------------------------------------------------------------------------------------
    ! Subroutine to compute snow age
    subroutine HMC_Phys_Snow_Apps_Age(iID, iRows, iCols, &
                                       sTime, iTime, &
                                       a2dVarDem, &
                                       a2dVarSnowFallDayCum, a2dVarSWE, &
                                       a2iVarAgeS)

        !------------------------------------------------------------------------------------------
        ! Variable(s) declaration 
        integer(kind = 4)   :: iID, iRows, iCols

        integer(kind = 4)   :: iTime
        character(len = 19) :: sTime
        
        real(kind = 4), dimension(iRows, iCols)         :: a2dVarDem
        
        integer(kind = 4), dimension(iRows, iCols)      :: a2iVarAgeS
        real(kind = 4), dimension(iRows, iCols)         :: a2dVarSnowFallDayCum, a2dVarSWE
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Info start
        call mprintf(.true., iINFO_Extra, ' Phys :: Snow :: Age ... ' )
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Initialize variable(s) every day at 00.00
        if ( (sTime(12:13).eq.'00') .and. (iTime.gt.1) ) then
            ! Snow age updating
            where( (a2dVarDem.ge.0.0) .and. (a2dVarSnowFallDayCum.le.3.0) )
                a2iVarAgeS = a2iVarAgeS + 1
            endwhere
            
            where( (a2dVarDem.ge.0.0) .and. (a2dVarSnowFallDayCum.gt.3.0) )
                a2iVarAgeS = 0
            endwhere
            
            ! Snow age re-initialization with SWE equal to 0.0 (without snow --> no data value)
            where( (a2dVarDem.ge.0.0) .and. (a2dVarSWE.eq.0.0) )
                a2iVarAgeS = 0
            endwhere

        endif
        !-----------------------------------------------------------------------------------------

        !------------------------------------------------------------------------------------------
        ! Info end
        call mprintf(.true., iINFO_Extra, ' Phys :: Snow :: Age ... OK' )
        !------------------------------------------------------------------------------------------
        
    end subroutine HMC_Phys_Snow_Apps_Age
    !------------------------------------------------------------------------------------------
    
    !------------------------------------------------------------------------------------------
    ! Subroutine to compute snow density
    subroutine HMC_Phys_Snow_Apps_Rho(iID, iRows, iCols, &
                                       sTime, iTime, &
                                       iDt, &
                                       a2dVarDem, &
                                       a2dVarTa, a2dVarSnowFall, a2dVarSWE, a2dVarMeltingSDayCum, &
                                       a2dVarRhoS, a2dVarRhoS0)

        !------------------------------------------------------------------------------------------
        ! Variable(s) declaration 
        integer(kind = 4)   :: iID, iRows, iCols
        
        integer(kind = 4)   :: iTime
        integer(kind = 4)   :: iDt
        
        real(kind = 4)      :: dVarRhoSMax, dVarRhoS0
        character(len = 19) :: sTime
        
        real(kind = 4), dimension(iRows, iCols)         :: a2dVarDem
        real(kind = 4), dimension(iRows, iCols)         :: a2dVarTa, a2dVarSnowFall, a2dVarSWE, a2dVarMeltingSDayCum
        
        real(kind = 4), dimension(iRows, iCols)         :: a2dVarExpRhoSLow, a2dVarExpRhoSHigh
        
        real(kind = 4), dimension(iRows, iCols)         :: a2dVarRhoS0, a2dVarExpRhoS
        
        real(kind = 4), dimension(iRows, iCols)         :: a2dVarRhoS
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Initialize variable(s)
        a2dVarRhoS0 = -9999.0; a2dVarExpRhoS = -9999.0
        a2dVarExpRhoSHigh = -9999.0; a2dVarExpRhoSLow = -9999.0
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Get information
        dVarRhoSMax = oHMC_Namelist(iID)%dRhoSnowMax
        dVarRhoS0 = oHMC_Namelist(iID)%dRhoSnowFresh
        
        a2dVarExpRhoSLow = oHMC_Vars(iID)%a2dExpRhoLow
        a2dVarExpRhoSHigh = oHMC_Vars(iID)%a2dExpRhoHigh
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Info start
        call mprintf(.true., iINFO_Extra, ' Phys :: Snow :: Rho ... ' )
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Compute ExpRhoSnow
        where( (a2dVarDem.ge.0.0) .AND. (a2dVarMeltingSDayCum.lt.3.0) )
            a2dVarExpRhoS = a2dVarExpRhoSLow        ! No Melting 0.033=1/30 -> dRoMax in 30 days
        elsewhere
            a2dVarExpRhoS = a2dVarExpRhoSHigh       ! Melting   0.2=1/5 -> dRoMax in 5 days
        endwhere
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Compute snow density
        where( (a2dVarDem.ge.0.0) .and. (a2dVarSnowFall.gt.0.0) )
            a2dVarRhoS0 = 67.9 + 51.3*exp(a2dVarTa/2.6)
        elsewhere
            a2dVarRhoS0 = 0.0
        endwhere
        
        ! Check rhos limits
        where( (a2dVarDem.ge.0.0) .and. (a2dVarRhoS0.gt.200.0) )
            a2dVarRhoS0 = 200.0
        endwhere
        where( (a2dVarDem.ge.0.0) .and. (a2dVarRhoS0.gt.0.0) .and. (a2dVarRhoS0.lt.67.9) )
            a2dVarRhoS0 = 67.9
        endwhere
        where( (a2dVarDem.ge.0.0) .and. (a2dVarRhoS0.lt.0.0) )
            a2dVarRhoS0 = 0.0
        endwhere
                  
        where( (a2dVarDem.ge.0.0) .and. (a2dVarSWE.gt.1.0) .and. (a2dVarSnowFall.gt.1.0) .and. &
               (a2dVarRhoS0.gt.67.9) .and. (a2dVarRhoS.gt.67.9) )

               a2dVarRhoS = (a2dVarSWE + a2dVarSnowFall)/(a2dVarSnowFall/a2dVarRhoS0 + a2dVarSWE/a2dVarRhoS)

        endwhere
          
        where( (a2dVarDem.ge.0.0) .and. (a2dVarSWE.le.1.0) .and. (a2dVarSnowFall.gt.0.0) )
            a2dVarRhoS = a2dVarRhoS0
        endwhere
        
        ! se lo strato di SWE presente � basso, ha densit� bassa e ci nevica uno SF>SWE con Densit� + alta della 
        ! neve a terra l'aggiornamento da problemi (densit� negativa) allora assumo tutto lo strato di neve con 
        !Densit� pari all'ulitma Rho_zero
        where( (a2dVarDem.ge.0.0) .and. (a2dVarRhoS.lt.67.9) )
            a2dVarRhoS = 67.9
        endwhere
        
        ! A new snow parameterization for the Meteo-France climate model
        ! Part I: validation in stand-alone experiments
        ! H. Douville, J.-F. Royer, J.-F. Mahfouf Climate Dynamics (1995) 12:21-35
        ! a1dRhoS(t)=(a1dRhoS(t-1)-dRhomax)*exp(-.033*Dt)+dRhomax
        if ( (sTime(12:13).eq.'00') .and. (iTime.gt.1) ) then
            where( (a2dVarDem.ge.0.0) .and. (a2dVarSWE.gt.1.0) )
                a2dVarRhoS = (a2dVarRhoS - dVarRhoSMax)*exp(-a2dVarExpRhoS*real(iDt)) + dVarRhoSMax
            endwhere
        endif
        
        ! Check rhos limit(s)
        where( (a2dVarDem.ge.0.0) .and. (a2dVarRhoS.gt.dVarRhoSMax) )
            a2dVarRhoS = dVarRhoSMax
        endwhere
        where( (a2dVarDem.ge.0.0) .and. (a2dVarRhoS.lt.0.0) )
            a2dVarRhoS = 0.0
        endwhere
        !------------------------------------------------------------------------------------------
              
        !------------------------------------------------------------------------------------------
        ! Info end
        call mprintf(.true., iINFO_Extra, ' Phys :: Snow :: Rho ... OK' )
        !------------------------------------------------------------------------------------------
        
    end subroutine HMC_Phys_Snow_Apps_Rho
    !------------------------------------------------------------------------------------------

    !------------------------------------------------------------------------------------------
    ! Subroutine to compute average temperature over n days
    subroutine HMC_Phys_Snow_Apps_TMean(iID, iRows, iCols, &
                                        iDaySteps, &    
                                        a2iVarMask, &
                                        a3dVarT, &
                                        a2dVarT, a2dVarTMean)
                                        
        !------------------------------------------------------------------------------------------
        ! Variable(s) declaration 
        integer(kind = 4)   :: iID, iRows, iCols
        
        integer(kind = 4)   :: iStep, iDaySteps
        
        integer(kind = 4), dimension(iRows, iCols)          :: a2iVarMask
        real(kind = 4), dimension(iRows, iCols)             :: a2dVarT, a2dVarTMean
        real(kind = 4), dimension(iRows, iCols, iDaySteps)  :: a3dVarT
        !------------------------------------------------------------------------------------------           
        
        !------------------------------------------------------------------------------------------
        ! Info start
        call mprintf(.true., iINFO_Extra, ' Phys :: Snow :: TMean ... ' )
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Compute and update temperature 3d mean field(s)
        if (all(a3dVarT.eq.0.0))then

            ! Debug
            call mprintf(.true., iINFO_Extra, ' Phys :: Snow :: TMean over days :: '// &
                                              ' First mean temperature 3d field storing step ... ')

            ! Update with a starting field all temporal steps
            do iStep = 1, int(iDaySteps)
                where(a2iVarMask.gt.0.0)
                    a3dVarT(:,:,int(iStep)) = a2dVarT
                elsewhere
                    a3dVarT(:,:,int(iStep)) = 0.0
                endwhere
            enddo

            call mprintf(.true., iINFO_Extra, ' Phys :: Snow :: TMean over days :: '// &
                                              ' First mean temperature 3d field storing step ... OK')
        else
            ! Re-initialize temperature 3d array
            do iStep=2, int(iDaySteps)
                a3dVarT(:,:,int(iStep-1)) = a3dVarT(:,:,int(iStep))
            enddo

            ! Update with new field
            where(a2iVarMask.gt.0.0)
                a3dVarT(:,:,int(iDaySteps)) =  a2dVarT
            elsewhere
                a3dVarT(:,:,int(iDaySteps)) = 0.0
            endwhere

        endif

        ! Calculate mean temperature over n days
        where(a2iVarMask.gt.0.0)
            a2dVarTMean = sum(a3dVarT(:,:,1:int(iDaySteps)),3)/(iDaySteps)
        elsewhere
            a2dVarTMean = 0.0
        endwhere
        !------------------------------------------------------------------------------------------ 
        
        !------------------------------------------------------------------------------------------
        ! Info end
        call mprintf(.true., iINFO_Extra, ' Phys :: Snow :: TMean ... OK' )
        !------------------------------------------------------------------------------------------
                                        
    end subroutine HMC_Phys_Snow_Apps_TMean
    !------------------------------------------------------------------------------------------
        
    !------------------------------------------------------------------------------------------
    ! Subroutine to compute open-loop melting
    subroutine HMC_Phys_Snow_Apps_MeltingOL(iID, iRows, iCols, iDtForcing, iDaySteps1Days, &
                                                sTime, iTime, &
                                                iGlacierValue, dVarRhoW, dVarMeltingTRef, &
                                                a2dVarDem, a2iVarNature, a2dVarArctUp, &
                                                a2dVarTa, a2dVarIncRad, &
                                                a2dVarTaCDays5, a2dVarCloudFactor, &
                                                a2dVarAlbedoS, a2dVarSWE, a2dVarMeltingS, a2dVarMeltingSc)
        
        !------------------------------------------------------------------------------------------
        ! Variable(s) declaration 
        integer(kind = 4)   :: iID, iRows, iCols
        
        integer(kind = 4)   :: iTime, iDaySteps1Days, iDtForcing
        integer(kind = 4)   :: iGlacierValue
        real(kind = 4)      :: dVarSigma, dVarRhoW, dVarMeltingTRef, dVarLamba
        character(len = 19) :: sTime
        
        integer(kind = 4), dimension(iRows, iCols)      :: a2iVarNature
        real(kind = 4), dimension(iRows, iCols)         :: a2dVarDem
        
        real(kind = 4), dimension(iRows, iCols)         :: a2dVarTa, a2dVarIncRad
        real(kind = 4), dimensioN(iRows, iCols)         :: a2dVarTaCDays5, a2dVarCloudFactor
        real(kind = 4), dimension(iRows, iCols)         :: a2dVarMeltingSc, a2dVarMeltingScG
        real(kind = 4), dimension(iRows, iCols)         :: a2dVarAlbedoS, a2dVarSWE, a2dVarMeltingS
        
        real(kind = 4), dimension(iRows, iCols)         :: a2dVarArctUp
        real(kind = 4), dimension(iRows, iCols)         :: a2dVarLW
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Initialize variable(s)
        a2dVarMeltingSc = 0.0; a2dVarMeltingScG = 0.0; a2dVarLW = 0.0
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Stefan-Boltzmann Constant [MJ/(m^2 day-1 K^(-4))]
        dVarSigma = 0.0000000049
        ! Fusion latent heat [MJ/kg] 
        dVarLamba = 0.334	
        
        ! Stefan-Boltzmann Constant [MJ/(m^2 h-1 K^(-4))]
        dVarSigma = dVarSigma/iDaySteps1Days
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Info start
        call mprintf(.true., iINFO_Extra, ' Phys :: Snow :: MeltingOL ... ' )
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Compute snow melting first coefficient
        a2dVarMeltingSc = 1.7367*atan(0.27439*a2dVarTaCDays5 - 0.5988)-1.7367*3.14/2 + a2dVarArctUp 
        
        ! Melting coefficient lower limit
        where(a2dVarMeltingSc.lt.0.3) a2dVarMeltingSc = 0.3 
        
        ! Compute snow melting second coefficient (for glacier(s))
        where( (a2dVarDem.gt.0.0) .and. (a2dVarSWE.gt.0.0) .and. (a2iVarNature.eq.iGlacierValue) ) 
            a2dVarMeltingScG = a2dVarMeltingSc
        elsewhere ( (a2dVarDem.gt.0.0) .and. (a2dVarSWE.eq.0.0) .and. (a2iVarNature.eq.iGlacierValue) )
            a2dVarMeltingScG = 3.0*a2dVarMeltingSc
        endwhere

        ! Check melting limit(s)
        where(a2dVarMeltingScG.lt.0.0) a2dVarMeltingScG = 0
        !------------------------------------------------------------------------------------------
         
        !------------------------------------------------------------------------------------------
        ! Compute snow melting for each day step
        where ( (a2dVarDem.ge.0.0) .and. (a2dVarTa.ge.dVarMeltingTRef) &
                .and. (a2dVarSWE.gt.0.0) )
            a2dVarIncRad = (a2dVarIncRad*(1.0 - a2dVarAlbedoS))*(iDtForcing)/1000000
            a2dVarLW = -(dVarSigma*a2dVarCloudFactor)*(-0.02+0.261*(exp(-7.77*0.0001*a2dVarTa**2)))*(a2dVarTa + 273.2)**4
            a2dVarMeltingS = ((1000.0/(dVarRhoW*dVarLamba))*(a2dVarIncRad + a2dVarLW)) + a2dVarMeltingSc*a2dVarTa
            a2dVarMeltingS = a2dVarMeltingS/iDaySteps1Days
            
        elsewhere
            a2dVarMeltingS = 0.0
        endwhere
        
        ! Glacier (Albedo == 0.9)
        where ( (a2dVarDem.ge.0.0) .and. (a2dVarTa.ge.dVarMeltingTRef) & 
                .and. (a2dVarSWE.eq.0.0) .and. (a2iVarNature.eq.iGlacierValue) )
            a2dVarIncRad = (a2dVarIncRad*(1.0 - 0.9))*(iDtForcing)/1000000
            a2dVarLW = -(dVarSigma*a2dVarCloudFactor)*(-0.02+0.261*(exp(-7.77*0.0001*a2dVarTa**2)))*(a2dVarTa + 273.2)**4
            a2dVarMeltingS = ((1000.0/(dVarRhoW*dVarLamba))*(a2dVarIncRad + a2dVarLW)) + a2dVarMeltingScG*a2dVarTa
            a2dVarMeltingS = a2dVarMeltingS/iDaySteps1Days
        elsewhere
            a2dVarMeltingS = 0.0
        endwhere
                
        ! Check snow melting
        where (a2dVarMeltingS.lt.0.0) a2dVarMeltingS = 0.0
  
        ! Debug
        !call mprintf(.true., iINFO_Extra, checkvar(a2dVarIncRad, oHMC_Vars(iID)%a2iMask, 'INCRAD') ) 
        !call mprintf(.true., iINFO_Extra, checkvar(a2dVarLW, oHMC_Vars(iID)%a2iMask, 'LW') ) 
        !call mprintf(.true., iINFO_Extra, checkvar(a2dVarMeltingS, oHMC_Vars(iID)%a2iMask, 'MELTINGS') ) 
        !------------------------------------------------------------------------------------------
          
        !------------------------------------------------------------------------------------------
        ! Info end
        call mprintf(.true., iINFO_Extra, ' Phys :: Snow :: MeltingOL ... OK' )
        !------------------------------------------------------------------------------------------
              
    end subroutine HMC_Phys_Snow_Apps_MeltingOL
    !------------------------------------------------------------------------------------------

    !------------------------------------------------------------------------------------------
    ! Subroutine to compute SWE assimilation
    subroutine HMC_Phys_Snow_Apps_SWEAssim(iID, iRows, iCols, &
                                            dVarRhoW, &
                                            a2iVarMask, &
                                            a2dVarSnowHeight, a2dVarSnowKernel, &
                                            a2dVarSnowCA, a2dVarSnowQA, &
                                            a2dVarSWE, a2iVarAge, a2dVarAlbedoS, a2dVarRhoS)
        
        !------------------------------------------------------------------------------------------
        ! Variable(s) declaration 
        integer(kind = 4)   :: iID, iRows, iCols
        
        real(kind = 4)      :: dVarRhoW, dVarSnowQThr
        
        integer(kind = 4), dimension(iRows, iCols)      :: a2iVarMask 
        
        real(kind = 4), dimension(iRows, iCols)         :: a2dVarSnowHeight, a2dVarSnowKernel
        real(kind = 4), dimension(iRows, iCols)         :: a2dVarSnowCA, a2dVarSnowQA
        
        integer(kind = 4), dimension(iRows, iCols)      :: a2iVarAge 
        real(kind = 4), dimension(iRows, iCols)         :: a2dVarSWE, a2dVarSWE_Assim
        real(kind = 4), dimension(iRows, iCols)         :: a2dVarAlbedoS, a2dVarRhoS
        real(kind = 4), dimension(iRows, iCols)         :: a2dVarCorr
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Snow Cover Area (SCA) value(s)
        ! NoSnow = 0; Snow = 1; Cloud = 2; NoData = -1, NoDecision = 3
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Get information
        dVarSnowQThr = oHMC_Namelist(iID)%dSnowQualityThr 
        
        ! Info start
        call mprintf(.true., iINFO_Extra, ' Phys :: Snow :: SWEAssim ... ' )
        
        ! Debug start
        !call mprintf(.true., iINFO_Extra, checkvar(a2dVarSWE, oHMC_Vars(iID)%a2iMask, 'SWE ASSIM START') ) 
        !call mprintf(.true., iINFO_Extra, checkvar(a2dVarSnowHeight, oHMC_Vars(iID)%a2iMask, 'SHEIGHT ASSIM START') ) 
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Snow Height condition(s)
        
        ! Check snow quality value(s) using a threshold
        where( (a2dVarSnowQA.le.dVarSnowQThr ) ) a2dVarSnowHeight = -9999.0
        ! Terrain condition --> use SCA value instead of snow height 
        where( (a2dVarSnowCA.eq.0.0) .and. (a2dVarSnowQA.gt.dVarSnowQThr) .and. (a2dVarSnowQA.lt.0.99) )  a2dVarSnowHeight = 0.0
            
        ! Cloud condition
        where( (a2dVarSWE.LT.20.0) .and. (a2dVarSnowCA.eq.2.0) .and. (a2dVarSnowQA.gt.dVarSnowQThr) ) a2dVarSnowHeight = -9999.0
        ! NoDecision condition
        where( (a2dVarSWE.LT.20.0) .and. (a2dVarSnowCA.eq.3.0) .and. (a2dVarSnowQA.gt.dVarSnowQThr) ) a2dVarSnowHeight = -9999.0 
            
        ! NoData condition
        where( (a2dVarSWE.LT.20.0) .and. (a2dVarSnowCA.eq.-1.0) .and. (a2dVarSnowQA.gt.dVarSnowQThr) ) a2dVarSnowHeight = -9999.0  
        !-----------------------------------------------------------------------------------------
            
        !------------------------------------------------------------------------------------------
        ! Snow Kernel condition(s)
        
        ! Terrain condition
        where( (a2dVarSnowCA.eq.0.0) .and. (a2dVarSnowQA.gt.dVarSnowQThr*2.5) .and. (a2dVarSnowQA.lt.0.99) ) 
            a2dVarSnowKernel = a2dVarSnowKernel*2.0
        endwhere
        ! Check upper boundary
        where( a2dVarSnowKernel.gt.1) a2dVarSnowKernel = 0.9;
        !------------------------------------------------------------------------------------------   

        !------------------------------------------------------------------------------------------
        ! Compute observed SWE starting from snow height interpolated observations
        where(a2dVarSnowHeight.gt.0.0) a2dVarSnowHeight = a2dVarSnowHeight*a2dVarRhoS/dVarRhoW
        !------------------------------------------------------------------------------------------
                     
        !------------------------------------------------------------------------------------------
        ! Nudging assimilation method
        call assimNudging(a2iVarMask, a2dVarSWE, a2dVarSnowHeight, a2dVarSnowKernel, &
                          a2dVarSWE_Assim, a2dVarCorr) 
        !------------------------------------------------------------------------------------------
                            
        !------------------------------------------------------------------------------------------
        ! Debug end
        !call mprintf(.true., iINFO_Extra, checkvar(a2dVarSWE, oHMC_Vars(iID)%a2iMask, 'SWE ASSIM END') ) 
        !call mprintf(.true., iINFO_Extra, checkvar(a2dVarSnowHeight, oHMC_Vars(iID)%a2iMask, 'SHEIGHT ASSIM END') ) 
        !------------------------------------------------------------------------------------------         
        
        !------------------------------------------------------------------------------------------  
        ! Update SWE with SWE with nudging assimilation
        a2dVarSWE = a2dVarSWE_Assim              
        !------------------------------------------------------------------------------------------  
                          
        !------------------------------------------------------------------------------------------
        ! Info end
        call mprintf(.true., iINFO_Extra, ' Phys :: Snow :: SWEAssim ... OK' )
        !------------------------------------------------------------------------------------------
        
    end subroutine HMC_Phys_Snow_Apps_SWEAssim
    !------------------------------------------------------------------------------------------
    
end module HMC_Module_Phys_Snow_Apps
!------------------------------------------------------------------------------------


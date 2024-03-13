!------------------------------------------------------------------------------------
! File:   HMC_Module_Phys_Convolution_Apps_SurfaceFlow.f90
!
! Author(s):    Fabio Delogu, Francesco Silvestro, Simone Gabellani
! Date:         20190410
!
! Convolution Apps Surface subroutine(s) for HMC model
!------------------------------------------------------------------------------------

!------------------------------------------------------------------------------------
! Module Header
module HMC_Module_Phys_Convolution_Apps_SurfaceFlow

    !------------------------------------------------------------------------------------
    ! External module(s) 
    use HMC_Module_Namelist,                                only: oHMC_Namelist
    use HMC_Module_Vars_Loader,                             only: oHMC_Vars
    
    use HMC_Module_Tools_Debug
    
    use HMC_Module_Phys_HydraulicStructure,                 only: HMC_Phys_Dam_Spilling, &
                                                                    HMC_Phys_Dam_Discharge, &
                                                                    HMC_Phys_Lake_Tank
                                                  
    use HMC_Module_Tools_Generic,                           only: getIntValue, getIntRange
    
    use HMC_Module_Phys_Convolution_Apps_Flooding,          only: HMC_Phys_Convolution_Apps_Flooding


    ! Implicit none for all subroutines in this module
    implicit none
    !------------------------------------------------------------------------------------------

contains 

    !------------------------------------------------------------------------------------------
    ! Subroutine for calculating surface flow channel network
    subroutine HMC_Phys_Convolution_Apps_SurfaceFlow_ChannelNetwork(iID, iRows, iCols, &
                                                     dDtDataForcing, dDtAct, iTAct, iTq, iDtMax, &
                                                     iNData, iNDam, iNLake, & 
                                                     iNPlant, iNCatch, iNRelease, iNJoint, &
                                                     iTime, iNTime, iETime)
        
        !------------------------------------------------------------------------------------------
        ! Variable(s) declaration
        integer(kind = 4)   :: iID, iRows, iCols, iTAct, iTq, iDtMax
        integer(kind = 4)   :: iNData, iNDam, iNLake, iNPlant, iNCatch, iNRelease, iNJoint
        integer(kind = 4)   :: iShift, iRank
        real(kind = 4)      :: dDtDataForcing, dDtAct
        
        integer(kind = 4)   :: iTTemp
        integer(kind = 4)   :: iI, iII, iIII, iJ, iJJ, iJJJ, iP, iR, iD, iL, iC
        integer(kind = 4)   :: iIm, iJm, iIin, iJin, iIout, iJout
        real(kind = 4)      :: dHm, dHin
        real(kind = 4)      :: dDtSurfaceFlow
        
        integer(kind = 4)   :: iFlagReleaseMass
        integer(kind = 4)   :: iVarPNT
        real(kind = 4)      :: dUMax
        real(kind = 4)      :: dRm, dBc
        real(kind = 4)      :: dVLake, dDh, dDhPrev, dRoutPrev
        real(kind = 4)      :: dQt, dHinFD
        real(kind = 4)      :: dVarAreaCell, dVarTcCatch, dVarQMinCatch
        
        real(kind = 4), dimension (iRows, iCols)            :: a2dVarIntensityPrev, a2dVarIntensityUpd
        real(kind = 4), dimension (iRows, iCols)            :: a2dVarHydroPrev, a2dVarHydroUpd
        
        real(kind = 4), dimension (iRows, iCols)            :: a2dVarFlowExf
        real(kind = 4), dimension (iRows, iCols)            :: a2dVarUcAct, a2dVarUhAct
        real(kind = 4), dimension (iRows, iCols)            :: a2dVarQDisOut, a2dVarQVolOut
        real(kind = 4), dimension (iRows, iCols)            :: a2dVarRouting
        
        real(kind = 4), dimension (iNDam)                   :: a1dVarVDam, a1dVarHDam, a1dVarLDam, & 
                                                               a1dVarCoeffDam, a1dVarQoutDam
      
        real(kind = 4), dimension (iNLake)                  :: a1dVarVLake

        real(kind = 4), dimension (iNPlant)                 :: a1dVarQPlant
           
        real(kind = 4), dimension (iNPlant, iETime + 1)     :: a2dVarHydroPlant
        real(kind = 4), dimension (iNCatch, iETime + 1)     :: a2dVarHydroCatch
        real(kind = 4), dimension (iNRelease, iETime + 1)   :: a2dVarHydroRelease
        
        integer(kind=4) :: iTime, iNTime, iETime
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Variable(s) initialization 
        a2dVarIntensityPrev = 0.0; a2dVarIntensityUpd = 0.0;
        
        a2dVarHydroPrev = 0.0; a2dVarHydroUpd = 0.0;

        a2dVarUcAct = 0.0; a2dVarUhAct = 0.0; 
        a2dVarFlowExf = 0.0; a2dVarRouting = 0.0;
        
        a2dVarQDisOut = 0.0 ! Outgoing discharge in m^3/s from each cell
        a2dVarQVolOut = 0.0 ! Outgoing discharge in volume from each cell
        
        a1dVarVDam = 0.0; a1dVarHDam = 0.0; a1dVarLDam = 0.0; a1dVarCoeffDam = 0.0; a1dVarQoutDam = 0.0
        
        a1dVarVLake = 0.0;
        
        a1dVarQPlant = 0.0; 

        a2dVarHydroPlant = 0.0;
        a2dVarHydroCatch = 0.0;
        a2dVarHydroRelease = 0.0;

        ! Null global variable(s)
        oHMC_Vars(iID)%a2dQVolOut = 0.0 ! Initialize each step (Portata in volume in uscita da una cella == Qtmp)
        oHMC_Vars(iID)%a2dQDisOut = 0.0 ! Initialize each step (Portata in uscita da una cella == Qout)
        
        oHMC_Vars(iID)%a1dQoutDam = 0.0
        
        !oHMC_Vars(iID)%a2dHydroPrev = 0.0 ! Initialize each step
        dDh = 0.0
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Temporal step 
        iTTemp = int(iTAct)
        
        ! Integrating step (SurfaceFlow)
        dDtSurfaceflow = dDtAct
        
        ! Variable(s) time dependent from global declaration
        a2dVarFlowExf = oHMC_Vars(iID)%a2dFlowExf        
        a2dVarHydroPrev = oHMC_Vars(iID)%a2dHydro
        a2dVarIntensityPrev = oHMC_Vars(iID)%a2dIntensity
        a2dVarRouting = oHMC_Vars(iID)%a2dRouting

        ! Dam variable(s)
        a1dVarVDam = oHMC_Vars(iID)%a1dVDam
        a1dVarHDam = oHMC_Vars(iID)%a1dHDam
        a1dVarLDam = oHMC_Vars(iID)%a1dLDam
        a1dVarCoeffDam = oHMC_Vars(iID)%a1dCoeffDam

        ! Lake variable(s)
        a1dVarVLake = oHMC_Vars(iID)%a1dVLake
        
        ! Plant data variable(s)
        a2dVarHydroPlant = oHMC_Vars(iID)%a2dHydroPlant
        ! Catch data variable(s)
        a2dVarHydroCatch = oHMC_Vars(iID)%a2dHydroCatch
        ! Release data variable(s)
        a2dVarHydroRelease = oHMC_Vars(iID)%a2dHydroRelease
        
        ! Exponent of dUcAct formula
        dBc = oHMC_Namelist(iID)%dBc
        
        ! Activate/Deactivate release mass update
        iFlagReleaseMass = oHMC_Namelist(iID)%iFlagReleaseMass     
        
        ! Info start
        if (iDEBUG.gt.0) then
            call mprintf(.true., iINFO_Extra, ' Phys :: Convolution :: SurfaceFlow ... ' )
        endif
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Debug
        if (iDEBUG.gt.0) then
            call mprintf(.true., iINFO_Extra, ' ========= SURFACE FLOW START ========= ') 
            call mprintf(.true., iINFO_Extra, checkvar(oHMC_Vars(iID)%a2dAreaCell, oHMC_Vars(iID)%a2iMask, 'AREACELL') )
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarHydroUpd, oHMC_Vars(iID)%a2iMask, 'HYDRO UPD START') )
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarHydroPrev, oHMC_Vars(iID)%a2iMask, 'HYDRO PREV START') )
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarQDisOut, oHMC_Vars(iID)%a2iMask, 'QDIS OUT START (Qout)') )
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarQVolOut, oHMC_Vars(iID)%a2iMask, 'QVOL OUT START (Qtmp)') )
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarRouting, oHMC_Vars(iID)%a2iMask, 'ROUTING START') )
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarIntensityPrev, oHMC_Vars(iID)%a2iMask, 'INTENSITY START') )
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarFlowExf, oHMC_Vars(iID)%a2iMask, 'EXFILTRATION START') )
            call mprintf(.true., iINFO_Extra, checkarray(a2dVarHydroPlant(:,2), 'HYDRO PLANT START') )
            call mprintf(.true., iINFO_Extra, checkarray(a2dVarHydroCatch(:,2), 'HYDRO CATCH START') )
            call mprintf(.true., iINFO_Extra, checkarray(a2dVarHydroRelease(:,2), 'HYDRO RELEASE START') )
            call mprintf(.true., iINFO_Extra, ' ') 
        endif
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Channel max surface velocity (UcMax)
        dUMax = 3600.0/dDtSurfaceflow*0.5
        
        ! Hill overland equation 
        a2dVarUhAct = oHMC_Vars(iID)%a2dUh
        where ( (oHMC_Vars(iID)%a2iChoice.eq.0) .and. (a2dVarUhact.gt.dUMax))  ! numerical check
            a2dVarUhAct = dUMax
        endwhere
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Hydro variable (previous and update array)
        where (a2dVarHydroPrev .lt. 0.0)
            a2dVarHydroPrev = 0.0000001
        endwhere        
        where (a2dVarHydroPrev .gt. 100000.0)
            a2dVarHydroPrev = 0.0000001
        endwhere
        
        ! Updating hydro variable (using previous step and checking values under zero) --> WaterLevel (tirante)
        a2dVarHydroUpd = a2dVarHydroPrev
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Routing variable
        where( a2dVarRouting .lt. 0.0)
            a2dVarRouting = 0.0
        endwhere
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Intensity variable (previous and update) 
        where(a2dVarIntensityPrev.lt.0.0)
            a2dVarIntensityPrev = 0.0
        endwhere
        ! Updating variables surface cell input (exfiltration + runoff [mm/h]) --> CHECKING CONVERSION
        where(oHMC_Vars(iID)%a2iMask.gt.0.0)
            a2dVarIntensityUpd = a2dVarIntensityPrev + a2dVarFlowExf*1000.0*3600.0 + &
                                 (1 - oHMC_Vars(iID)%a2dCoeffResol)*a2dVarRouting/dDtSurfaceflow*3600.0 + &
                                 !(1 - oHMC_Vars(iID)%a2dCoeffResol)*oHMC_Vars(iID)%a2dFlowDeep + &      ! Tevere settings ( --- development mode ---)
                                 (oHMC_Vars(iID)%a2dWSRunoff*1000.0*3600.0)/oHMC_Vars(iID)%a2dAreaCell   ! WSRunoff from m^3/s to mm/h            
        endwhere
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Check plant(s) availability
        if (iNPlant .gt. 0) then
            
            !------------------------------------------------------------------------------------------
            ! Cycle on plant(s)
            iP = 0;
            do iP = 1, iNPlant
                
                !------------------------------------------------------------------------------------------
                ! Get plant information
                iI = 0; iJ = 0;
                iI = oHMC_Vars(iID)%a2iXYPlant(iP, 2); 
                iJ = oHMC_Vars(iID)%a2iXYPlant(iP, 1);

                dVarAreaCell = oHMC_Vars(iID)%a2dAreaCell(iI, iJ)
               
                ! Check area cell
                if (dVarAreaCell.lt.0) then
                    dVarAreaCell = oHMC_Vars(iID)%dDxM*oHMC_Vars(iID)%dDyM
                endif
                !------------------------------------------------------------------------------------------
                
                !------------------------------------------------------------------------------------------
                ! Add plant data to intensity and updata dam volume
                if (a2dVarHydroPlant(iP, iTTemp + 1) .ge. 0) then !Se ho le turbinate (si presume che i tempi siano relativi all'immissione)
                    
                    !------------------------------------------------------------------------------------------
                    ! Update intensity
                    a2dVarIntensityUpd(iI, iJ) = a2dVarIntensityUpd(iI, iJ) + a2dVarHydroPlant(iP, iTTemp + 1)

                    ! Update dam volume
                    a1dVarVDam(oHMC_Vars(iID)%a1iFlagDamPlant(iP)) = a1dVarVDam(oHMC_Vars(iID)%a1iFlagDamPlant(iP)) - &
                        a2dVarHydroPlant(iP, iTTemp + 1)*(dVarAreaCell)/(1000*3600)*dDtSurfaceflow !m^3
                    !------------------------------------------------------------------------------------------
                        
                else
                    
                    !------------------------------------------------------------------------------------------
                    ! Compute plant max discharge (transform to mm/h)
                    if ( a1dVarVDam(oHMC_Vars(iID)%a1iFlagDamPlant(iP)) .lt. &
                         oHMC_Vars(iID)%a1dVMaxDam(oHMC_Vars(iID)%a1iFlagDamPlant(iP)) )then
                        
                        ! VDam < VDamMax --> Q**6;
                        a1dVarQPlant(iP) = oHMC_Vars(iID)%a1dQMaxPlant(iP)* &
                                           (a1dVarVDam(oHMC_Vars(iID)%a1iFlagDamPlant(iP))/ &
                                            oHMC_Vars(iID)%a1dVMaxDam(oHMC_Vars(iID)%a1iFlagDamPlant(iP)))**6
                                            
                    else
                        ! VDam => VDamMax --> QMax
                        a1dVarQPlant(iP) = oHMC_Vars(iID)%a1dQMaxPlant(iP)
                    endif

                    ! Check plant discharge
                    if (a1dVarQPlant(iP) .lt. 0.0) a1dVarQPlant(iP) = 0.0
                    !------------------------------------------------------------------------------------------
                    
                    !------------------------------------------------------------------------------------------
                    ! Update intensity
                    a2dVarIntensityUpd(iI, iJ) = a2dVarIntensityUpd(iI, iJ) + &
                                                 a1dVarQPlant(iP)*1000*3600/(dVarAreaCell)

                    ! Update dam volume
                    a1dVarVDam(oHMC_Vars(iID)%a1iFlagDamPlant(iP)) = a1dVarVDam(oHMC_Vars(iID)%a1iFlagDamPlant(iP)) - &
                                                                     a1dVarQPlant(iP)*dDtSurfaceflow
                    
                    !write(*,*) a1dVarVDam(oHMC_Vars(iID)%a1iFlagDamPlant(iP)), a1dVarQPlant(iP)
                    !------------------------------------------------------------------------------------------
                                                                     
                endif
                !------------------------------------------------------------------------------------------
                
                !------------------------------------------------------------------------------------------
                ! Check dam volume
                if(a1dVarVDam(oHMC_Vars(iID)%a1iFlagDamPlant(iP)) .lt. 0.0) a1dVarVDam(oHMC_Vars(iID)%a1iFlagDamPlant(iP)) = 0.0
                !------------------------------------------------------------------------------------------
                
            enddo 
            !------------------------------------------------------------------------------------------
        
        endif
        !------------------------------------------------------------------------------------------ 
        
        !------------------------------------------------------------------------------------------ 
        ! Check release(s) availability
        if (iNRelease .gt. 0) then
            
            !------------------------------------------------------------------------------------------
            ! Cycle on release(s)
            iR = 0;
            do iR = 1, iNRelease
                
                !------------------------------------------------------------------------------------------
                ! Get plant information
                iI = 0; iJ = 0;
                iI = oHMC_Vars(iID)%a2iXYRelease(iR, 2); iJ = oHMC_Vars(iID)%a2iXYRelease(iR, 1);
                !------------------------------------------------------------------------------------------
                
                !------------------------------------------------------------------------------------------
                ! Check hydro release(s) data 
                if (a2dVarHydroRelease(iR,iTTemp + 1) .lt. 0.0) a2dVarHydroRelease(iR,iTTemp + 1) = 0.0
                ! Update intensity using hydro release
                a2dVarIntensityUpd(iI, iJ) = a2dVarIntensityUpd(iI, iJ) + a2dVarHydroRelease(iR,iTTemp + 1)
                !------------------------------------------------------------------------------------------
                
            enddo
            !------------------------------------------------------------------------------------------
            
        endif
        !------------------------------------------------------------------------------------------ 
        
        !------------------------------------------------------------------------------------------
        ! Surface Routing
        
        ! HILLS
        ! Surface equation for hills (direct euler's method)
        where ( (oHMC_Vars(iID)%a2iChoice.eq.0.0) .and. (oHMC_Vars(iID)%a2iMask.gt.0.0) ) 
            a2dVarHydroUpd = a2dVarHydroUpd + a2dVarIntensityUpd*dDtSurfaceflow/3600 - &
                             a2dVarHydroUpd*a2dVarUhAct*dDtSurfaceflow/3600.0
            a2dVarQDisOut = a2dVarHydroPrev*a2dVarUhAct*dDtSurfaceflow/3600.0
        endwhere
        
        ! CHANNELS
        ! Surface equation for channels (direct euler's method)
        where ( (oHMC_Vars(iID)%a2iChoice.eq.1.0) .and. (oHMC_Vars(iID)%a2iMask.gt.0.0) ) 

            a2dVarUcAct = oHMC_Vars(iID)%a2dUc*(tan(oHMC_Vars(iID)%a2dBeta)**0.5)*a2dVarHydroUpd**dBc ! FPI settings
            
            where (a2dVarUcAct.gt.dUMax)
                a2dVarUcAct = dUMax
            endwhere

            a2dVarQDisOut = a2dVarHydroUpd*a2dVarUcAct*dDtSurfaceflow/3600.0

            ! Surface tank equation (runoff with routing + exfiltration) 
            a2dVarHydroUpd = a2dVarHydroUpd + a2dVarIntensityUpd*dDtSurfaceflow/3600 - &
                          a2dVarHydroUpd*a2dVarUcAct*dDtSurfaceflow/3600.0

        endwhere
        
        where ( (oHMC_Vars(iID)%a2iChoice.eq.1.0) .and. (oHMC_Vars(iID)%a2iMask.gt.0.0) ) 

            a2dVarQDisOut = oHMC_Vars(iID)%a2dUc*(tan(oHMC_Vars(iID)%a2dBeta)**0.5)*(0.5*a2dVarHydroPrev**(1 + dBc) + &
                            0.5*a2dVarHydroUpd**(1 + dBc))*dDtSurfaceflow/3600
                          
            where (a2dVarQDisOut .gt. (a2dVarHydroPrev + a2dVarIntensityUpd*dDtSurfaceflow/3600)*0.7)
                a2dVarQDisOut = (a2dVarHydroPrev + a2dVarIntensityUpd*dDtSurfaceflow/3600)*0.7
            endwhere

            a2dVarHydroUpd = a2dVarHydroPrev + a2dVarIntensityUpd*dDtSurfaceflow/3600 - a2dVarQDisOut
           
        endwhere
        !------------------------------------------------------------------------------------------

        !------------------------------------------------------------------------------------------
        ! Check dam availability
        if (iNDam .gt. 0) then
            ! Cycle on dam(s)
            iD = 0;
            do iD = 1, iNDam
                
                iI = 0; iJ = 0;
                iI = oHMC_Vars(iID)%a2iXYDam(iD, 2); iJ = oHMC_Vars(iID)%a2iXYDam(iD, 1);
                
                ! Lake condition
                if (oHMC_Vars(iID)%a1dCodeDam(iD) .gt. 0.0) then
                    ! Distributed lake
                    dVLake = 0.0;
                    dVLake = sum(sum(a2dVarIntensityUpd, dim=1, mask=oHMC_Vars(iID)%a2iChoice.eq.oHMC_Vars(iID)%a1dCodeDam(iD)))
                    a1dVarVDam(iD) = a1dVarVDam(iD) + dVLake*dDtSurfaceflow/(3600*1000)*(oHMC_Vars(iID)%a2dAreaCell(iI, iJ)) ! in m^3
                else
                    ! Punctual lake
                    a1dVarVDam(iD) = a1dVarVDam(iD) + a2dVarQDisOut(iI, iJ)/1000*(oHMC_Vars(iID)%a2dAreaCell(iI, iJ))
                    !write(*,*) 'id, dam v, q ',iD, a1dVarVDam(iD), a2dVarQDisOut(iI, iJ)/1000*(oHMC_Vars(iID)%a2dAreaCell(iI, iJ))
                endif
                
                a2dVarQDisOut(iI, iJ) = 0.0
                
            enddo
        endif
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Check lake availability
        if (iNLake .gt. 0) then
            ! Cycle on lake
            iL = 0;
            do iL = 1, iNLake
                iI = 0; iJ = 0;
                iI = oHMC_Vars(iID)%a2iXYLake(iL, 2); iJ = oHMC_Vars(iID)%a2iXYLake(iL, 1);
                
                if (oHMC_Vars(iID)%a1dCodeLake(iL) .gt. 0) then
                    ! Distributed lake
                    dVLake = 0.0;
                    dVLake = sum(sum(a2dVarIntensityUpd, dim=1, mask=oHMC_Vars(iID)%a2iChoice.eq.oHMC_Vars(iID)%a1dCodeLake(iL)))
                    a1dVarVLake(iL) = a1dVarVLake(iL) + dVLake*dDtSurfaceflow/(3600*1000)*(oHMC_Vars(iID)%a2dAreaCell(iI, iJ)) ! in m^3
                else
                    ! Punctual lake
                    a1dVarVLake(iL) = a1dVarVLake(iL) + a2dVarQDisOut(iI, iJ)/1000*(oHMC_Vars(iID)%a2dAreaCell(iI, iJ))
                endif
                
                a2dVarQDisOut(iI, iJ) = 0.0
                
            enddo
        endif
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Flow part for following cell 
        ! In Horton this flow will add at rain rate in the following step

        ! Checking hydro variable
        where(a2dVarHydroUpd.lt.0.0)
            a2dVarHydroUpd = 0.0000001
        endwhere

        ! Calculating flow 
        ! Calcolo la porzione di acqua che va nella cella successiva
        ! Essa verr� sommata alla pioggia nella subroutine di Horton
        ! l'istante successivo
        a2dVarRouting = 0.0
        do iI = 1, iRows
            do iJ = 1, iCols 
                
                ! DEM condition
                if (oHMC_Vars(iID)%a2iMask(iI,iJ).gt.0.0) then
                    
                    ! Rate and pointers definition
                    !iVarPNT = 0
                    !iVarPNT = int(oHMC_Vars(iID)%a2iPNT(iI,iJ))
                    
                    ! Defining flow directions
                    iII = int((int(oHMC_Vars(iID)%a2iPNT(iI,iJ))  - 1)/3) - 1
                    iJJ = int(oHMC_Vars(iID)%a2iPNT(iI,iJ)) - 5 - 3*iII
                    iIII = iI + iII
                    iJJJ = iJ + iJJ
                    
                    if ( (iIII.ge.1) .and. (iJJJ.ge.1) ) then
                        
                        ! Integrazione del routing in mm/passo_integrazione_del_routing
                        ! L'acqua viene mandata nella cella successiva e utilizzata nella Subrotine
                        ! Horton
                        dRm = 0.0;
                        dRm = a2dVarQDisOut(iI, iJ) ! Trapezi
                        

                        a2dVarRouting(iIII, iJJJ) = a2dVarRouting(iIII, iJJJ) + dRm  ![mm]
                        a2dVarQVolOut(iI, iJ) = dRm/dDtSurfaceflow	
                        
                    endif 
                endif   
            enddo
        enddo				
        !------------------------------------------------------------------------------------------

        !------------------------------------------------------------------------------------------
        ! Check catch availability
        if (iNCatch .gt. 0) then
            ! Cycle on catch(es) ---> subtract turbinate(s) from routing
            iC = 0;
            do iC = 1, iNCatch
                
                iI = 0; iJ = 0; dDh = 0.0
                iI = oHMC_Vars(iID)%a2iXYCatch(iC, 2); iJ = oHMC_Vars(iID)%a2iXYCatch(iC, 1);
                dVarTcCatch = oHMC_Vars(iID)%a1dTCorrCatch(iC)
                dVarQminCatch = oHMC_Vars(iID)%a1dQMinCatch(iC)
                
                dVarAreaCell = oHMC_Vars(iID)%a2dAreaCell(iI, iJ)
                
                ! Check area cell
                if (dVarAreaCell.lt.0) then
                    dVarAreaCell = oHMC_Vars(iID)%dDxM*oHMC_Vars(iID)%dDyM
                endif
                
                ! Compute minimum discharge
                dVarQminCatch = oHMC_Vars(iID)%a1dQMinCatch(iC)/dVarAreaCell*dDtSurfaceflow*1000 ! [mm]

                ! Compute h
                dDh = a2dVarHydroCatch(iC, iTTemp + 1)/dVarAreaCell*dDtSurfaceflow*1000 ! [mm]
                
                ! Avoid < 0 values in turbinate for mismatch in length of simulation
                if (dDh.lt.0.0) dDh = 0.0

                ! Defining flow directions
                iII = int((int(oHMC_Vars(iID)%a2iPNT(iI,iJ))  - 1)/3) - 1
                iJJ = int(oHMC_Vars(iID)%a2iPNT(iI,iJ)) - 5 - 3*iII
                iIII = iI + iII
                iJJJ = iJ + iJJ
                
                ! Index(es) not allowed
                dRoutPrev = 0.0
                if( (iIII.ge.1) .and. (iJJJ.ge.1) ) then
                    
                    dRoutPrev = a2dVarRouting(iIII, iJJJ) 
                    
                    ! Q less than QMV
                    if(a2dVarRouting(iIII, iJJJ).le.dVarQminCatch) then
                        dDh = 0.0 
                    ! Q larger than QMV
                    else
                        ! Q-QMV < Qplant
                        if((a2dVarRouting(iIII, iJJJ) - dDh).lt.dVarQminCatch) then
                                dDh = a2dVarRouting(iIII, iJJJ)-dVarQminCatch				
                        endif
                    endif

                    !Q - QplantCorrected
                    a2dVarRouting(iIII, iJJJ)=a2dVarRouting(iIII, iJJJ)-dDh
                    
                    ! Update data release(s)
                    if (iFlagReleaseMass .eq. 1) then   ! activate/deactivate release mass update
                        if (iNCatch .eq. iNRelease) then
                            iShift = nint(dVarTcCatch*60/dDtDataForcing)
                            iRank = size(a2dVarHydroRelease, dim = 2)

                            if (iTTemp + iShift .lt. iRank ) then
                                a2dVarHydroRelease(iC, iTTemp + 1) = dDh/dDtSurfaceflow*3600
                            endif

                        endif
                    endif

                    if(a2dVarRouting(iIII, iJJJ).lt.0.0)then
                        a2dVarRouting(iIII, iJJJ) = 0.0
                    endif

                endif

            enddo
        endif
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Check joint availability
        if (iNJoint .gt. 0) then
            ! Cycle on joint(s) 
            iJ = 0;
            do iJ = 1, iNJoint
                iIm = 0; iJm = 0; iIin = 0; iJin = 0;
                iIm = oHMC_Vars(iID)%a2iXYJoint(iJ,2); iJm = oHMC_Vars(iID)%a2iXYJoint(iJ,1);
                iIin = oHMC_Vars(iID)%a2iXYInJoint(iJ,2); iJin = oHMC_Vars(iID)%a2iXYInJoint(iJ,1);
                
                dHm = 0.0; dHin = 0.0;
                dHm = a2dVarHydroUpd(iIm, iJm) !dH2
                dHin = a2dVarHydroUpd(iIin,iJin) !dH1
                
                if ( (dHin.lt.dHm) .and. (dHm.gt.0.0) ) then
                    dQt = 0.0; dHinFD = 0.0;
                    dQt = a2dVarQVolOut(iIin, iJin)/1000 + oHMC_Vars(iID)%a2dAreaCell(iIm, iJm) ! in [m^3/s]
                    dHinFD = sqrt(dHin**2 + 1000*1000*2*(dQt**2)/(oHMC_Vars(iID)%a2dAreaCell(iIm, iJm)*9.8*dHin/1000)) ! derivata eq delle spinte
                    
                    if (dHinFD/dHm .lt. oHMC_Vars(iID)%a1dThrLevelJoint(iJ)) then
                        
                        iIout = 0; iJout = 0;
                        iIout = oHMC_Vars(iID)%a2iXYOutJoint(iJ,2); iJout = oHMC_Vars(iID)%a2iXYOutJoint(iJ,1);
                        
                        ! Main channel
                        a2dVarRouting(iIout,iJout) = a2dVarRouting(iIout,iJout) + a2dVarRouting(iIm,iJm)*(1 - dHinFD/dHm) ! Routing dove immetto la derivazione del Master 
                        a2dVarRouting(iIm,iJm) = a2dVarRouting(iIm,iJm)*dHinFD/dHm ! Routing in main channel
                        
                        ! Tributary channel
			a2dVarRouting(iIout,iJout) = a2dVarRouting(iIout,iJout) + a2dVarRouting(iIin,iJin)*(1 - dHinFD/dHm) ! Routing dove immetto la derivazione dell'immissario
			a2dVarRouting(iIin,iJin) = a2dVarRouting(iIin,iJin)*dHinFD/dHm ! Routing in main channel
                        
                    endif
                endif
            enddo    
        endif
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Compute dam spilling
        call HMC_Phys_Dam_Spilling(iID, iNDam, dDtSurfaceflow, &
                                                     a1dVarVDam, a1dVarQoutDam, a1dVarCoeffDam, a1dVarLDam)
        !------------------------------------------------------------------------------------------
                                     
        !------------------------------------------------------------------------------------------
        ! Check dam availability
        if (iNDam .gt. 0) then
            ! Cycle on dam
            iD = 0
            do iD = 1, iNDam
            
                iI = 0; iJ = 0;
                iI = oHMC_Vars(iID)%a2iXYDam(iD, 2); iJ = oHMC_Vars(iID)%a2iXYDam(iD, 1);
                
                ! Pointers definition
                !iVarPNT = 0
                !iVarPNT = int(oHMC_Vars(iID)%a2iPNT(iI,iJ))
                
                ! Defining flow directions
                iII = int((int(oHMC_Vars(iID)%a2iPNT(iI,iJ))  - 1)/3) - 1
                iJJ = int(oHMC_Vars(iID)%a2iPNT(iI,iJ)) - 5 - 3*iII
                iIII = iI + iII
                iJJJ = iJ + iJJ
                
                ! Update routing 
                a2dVarRouting(iIII, iJJJ) = a2dVarRouting(iIII, iJJJ) + a1dVarQoutDam(iD)
                
                ! Volume to mean dam level [mm]
                where( oHMC_Vars(iID)%a2iChoice.eq.oHMC_Vars(iID)%a1dCodeDam(iD) .and. (oHMC_Vars(iID)%a2iMask.gt.0.0) ) 
                    a2dVarHydroUpd = a1dVarVDam(iD)/(oHMC_Vars(iID)%a1iNCellDam(iD)*oHMC_Vars(iID)%a2dAreaCell(iI,iJ))*1000 ! [mm]
                endwhere
                    
            enddo
            
        endif
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Debug
        if (iDEBUG.gt.0) then
            call mprintf(.true., iINFO_Extra, ' ') 
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarHydroUpd, oHMC_Vars(iID)%a2iMask, 'HYDRO UPD END') )
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarHydroPrev, oHMC_Vars(iID)%a2iMask, 'HYDRO PREV END') )
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarQDisOut, oHMC_Vars(iID)%a2iMask, 'QDIS OUT END (Qout)') )
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarQVolOut, oHMC_Vars(iID)%a2iMask, 'QVOL OUT END (Qtmp)') )
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarRouting, oHMC_Vars(iID)%a2iMask, 'ROUTING END') )
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarIntensityUpd, oHMC_Vars(iID)%a2iMask, 'INTENSITY END') )
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarFlowExf, oHMC_Vars(iID)%a2iMask, 'EXFILTRATION END') )     
            call mprintf(.true., iINFO_Extra, checkarray(a2dVarHydroPlant(:,2), 'HYDRO PLANT END') )
            call mprintf(.true., iINFO_Extra, checkarray(a2dVarHydroCatch(:,2), 'HYDRO CATCH END') )
            call mprintf(.true., iINFO_Extra, checkarray(a2dVarHydroRelease(:,2), 'HYDRO RELEASE END') )
            call mprintf(.true., iINFO_Extra, ' ========= SURFACE FLOW END =========== ') 
        endif
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Updating model global variable(s)
	oHMC_Vars(iID)%a2dQVolOut = a2dVarQVolOut
        oHMC_Vars(iID)%a2dQDisOut = a2dVarQDisOut
        
        oHMC_Vars(iID)%a2dHydro = a2dVarHydroUpd
        oHMC_Vars(iID)%a2dHydroPrev = a2dVarHydroPrev

        oHMC_Vars(iID)%a2dRouting = a2dVarRouting   ! Compute to use in horton and after set to zero
        
        oHMC_Vars(iID)%a1dVDam = a1dVarVDam
        oHMC_Vars(iID)%a1dHDam = a1dVarHDam 
        oHMC_Vars(iID)%a1dLDam = a1dVarLDam
        oHMC_Vars(iID)%a1dCoeffDam = a1dVarCoeffDam 
        oHMC_Vars(iID)%a1dQoutDam = a1dVarQoutDam
        
        oHMC_Vars(iID)%a1dVLake = a1dVarVLake
        
        oHMC_Vars(iID)%a2dHydroPlant = a2dVarHydroPlant
        oHMC_Vars(iID)%a2dHydroCatch = a2dVarHydroCatch
        oHMC_Vars(iID)%a2dHydroRelease = a2dVarHydroRelease
        
        ! Info end
        if (iDEBUG.gt.0) then
            call mprintf(.true., iINFO_Extra, ' Phys :: Convolution :: SurfaceFlow ... OK' )
        endif
        !------------------------------------------------------------------------------------------
           
    end subroutine HMC_Phys_Convolution_Apps_SurfaceFlow_ChannelNetwork
    !------------------------------------------------------------------------------------------
      
    !------------------------------------------------------------------------------------------
    ! Subroutine for calculating surface flow channel fraction
    subroutine HMC_Phys_Convolution_Apps_SurfaceFlow_ChannelFraction(iID, iRows, iCols, &
                                                     dDtDataForcing, dDtAct, iTAct, iTq, iDtMax, &
                                                     iNData, iNDam, iNLake, & 
                                                     iNPlant, iNCatch, iNRelease, iNJoint, &
                                                     iTime, iNTime, iETime)
        
        !------------------------------------------------------------------------------------------
        ! Variable(s) declaration
        integer(kind = 4)   :: iID, iRows, iCols, iTAct, iTq, iDtMax
        integer(kind = 4)   :: iNData, iNDam, iNLake, iNPlant, iNCatch, iNRelease, iNJoint
        integer(kind = 4)   :: iShift, iRank
        real(kind = 4)      :: dDtDataForcing, dDtAct
        
        integer(kind = 4)   :: iTTemp
        integer(kind = 4)   :: iI, iII, iIII, iJ, iJJ, iJJJ, iP, iR, iD, iL, iC
        integer(kind = 4)   :: iIm, iJm, iIin, iJin, iIout, iJout
        real(kind = 4)      :: dHm, dHin,dKint
        real(kind = 4)      :: dDtSurfaceFlow
        
        integer(kind = 4)   :: iFlagReleaseMass, iFlagFlood
        integer(kind = 4)   :: iVarPNT
        real(kind = 4)      :: dUMax
        real(kind = 4)      :: dRm, dBc
        real(kind = 4)      :: dVLake, dDh, dDhPrev, dRoutPrev
        real(kind = 4)      :: dQt, dHinFD
        real(kind = 4)      :: dVarAreaCell, dVarTcCatch, dVarQMinCatch
        real(kind = 4)      :: dPa, dPb, dTmmm !parameters for length of inundation
        
        real(kind = 4), dimension (iRows, iCols)            :: a2dVarIntensityPrev, a2dVarIntensityUpd
        real(kind = 4), dimension (iRows, iCols)            :: a2dVarHydroPrevC, a2dVarHydroUpdC
        real(kind = 4), dimension (iRows, iCols)            :: a2dVarHydroPrevH, a2dVarHydroUpdH
        
        real(kind = 4), dimension (iRows, iCols)            :: a2dVarFlowExf
        real(kind = 4), dimension (iRows, iCols)            :: a2dVarUcAct, a2dVarUhAct
        real(kind = 4), dimension (iRows, iCols)            :: a2dVarQDisOut, a2dVarQVolOut
        real(kind = 4), dimension (iRows, iCols)            :: a2dVarRouting
        real(kind = 4), dimension (iRows, iCols)            :: a2dPartition, a2dVarBF
        real(kind = 4), dimension (iRows, iCols)            :: a2dVarWidthC, a2dVarAreaCell, a2dVarWidthH
        
        real(kind = 4), dimension (iNDam)                   :: a1dVarVDam, a1dVarHDam, a1dVarLDam, &
                                                               a1dVarCoeffDam, a1dVarQoutDam
      
        real(kind = 4), dimension (iNLake)                  :: a1dVarVLake

        real(kind = 4), dimension (iNPlant)                 :: a1dVarQPlant
           
        real(kind = 4), dimension (iNPlant, iETime + 1)     :: a2dVarHydroPlant
        real(kind = 4), dimension (iNCatch, iETime + 1)     :: a2dVarHydroCatch
        real(kind = 4), dimension (iNRelease, iETime + 1)   :: a2dVarHydroRelease
        
        integer(kind=4) :: iTime, iNTime, iETime
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Variable(s) initialization 
        a2dVarIntensityPrev = 0.0; a2dVarIntensityUpd = 0.0;
        
        a2dVarUcAct = 0.0; a2dVarUhAct = 0.0; 
        a2dVarFlowExf = 0.0; a2dVarRouting = 0.0;
               
        a2dVarQDisOut = 0.0; ! Outgoing discharge in m^3/s from each cell
        a2dVarQVolOut = 0.0; ! Outgoing discharge in volume from each cell
        
        a1dVarVDam = 0.0; a1dVarHDam = 0.0; a1dVarLDam = 0.0; a1dVarCoeffDam = 0.0; a1dVarQoutDam = 0.0
        a1dVarVLake = 0.0; a1dVarQPlant = 0.0; 

        a2dVarHydroPlant = 0.0;
        a2dVarHydroCatch = 0.0;
        a2dVarHydroRelease = 0.0;
        
        a2dVarWidthC = 0.0; a2dVarAreaCell = 0.0; a2dVarWidthH = 0.0
        a2dVarHydroPrevH = 0.0;  a2dVarHydroUpdH = 0.0;
        a2dVarHydroPrevC = 0.0; a2dVarHydroUpdC = 0.0;
        a2dVarBF = 0.0;
        
        ! Null global variable(s)
        oHMC_Vars(iID)%a2dQVolOut = 0.0 ! Initialize each step (Portata in volume in uscita da una cella == Qtmp)
        oHMC_Vars(iID)%a2dQDisOut = 0.0 ! Initialize each step (Portata in uscita da una cella == Qout)
        
        oHMC_Vars(iID)%a1dQoutDam = 0.0
        
        !oHMC_Vars(iID)%a2dHydroPrev = 0.0 ! Initialize each step
        dDh = 0.0
        !------------------------------------------------------------------------------------------
       
        !------------------------------------------------------------------------------------------
        ! Temporal step 
        iTTemp = int(iTAct)
        
        ! Integrating step (SurfaceFlow)
        dDtSurfaceflow = dDtAct
        
        ! Limit for numerical integration
        dKint = 0.75
        
        ! Flooding parameters
        dPa = 2.5
        dPb = 0.5
        
        ! Variable(s) time dependent from global declaration
        a2dVarFlowExf = oHMC_Vars(iID)%a2dFlowExf         
        a2dVarIntensityPrev = oHMC_Vars(iID)%a2dIntensity
        a2dVarRouting = oHMC_Vars(iID)%a2dRouting
        
        a2dVarWidthC = oHMC_Vars(iID)%a2dWidthC
        a2dVarAreaCell = oHMC_Vars(iID)%a2dAreaCell
        a2dVarWidthH = oHMC_Vars(iID)%a2dWidthH

        a2dVarHydroPrevH = oHMC_Vars(iID)%a2dHydroH
        a2dVarHydroPrevC = oHMC_Vars(iID)%a2dHydroC

        ! Dam variable(s)
        a1dVarVDam = oHMC_Vars(iID)%a1dVDam
        a1dVarHDam = oHMC_Vars(iID)%a1dHDam
        a1dVarLDam = oHMC_Vars(iID)%a1dLDam
        a1dVarCoeffDam = oHMC_Vars(iID)%a1dCoeffDam

        ! Lake variable(s)
        a1dVarVLake = oHMC_Vars(iID)%a1dVLake
        
        ! Plant data variable(s)
        a2dVarHydroPlant = oHMC_Vars(iID)%a2dHydroPlant
        ! Catch data variable(s)
        a2dVarHydroCatch = oHMC_Vars(iID)%a2dHydroCatch
        ! Release data variable(s)
        a2dVarHydroRelease = oHMC_Vars(iID)%a2dHydroRelease
        
        ! Exponent of dUcAct formula
        dBc = oHMC_Namelist(iID)%dBc
        
        ! Activate/Deactivate release mass update
        iFlagReleaseMass = oHMC_Namelist(iID)%iFlagReleaseMass
        
        ! Flooding flag
        iFlagFlood = oHMC_Namelist(iID)%iFlagFlood 
        
        ! Info start
        if (iDEBUG.gt.0) then
            call mprintf(.true., iINFO_Extra, ' Phys :: Convolution :: SurfaceFlow ... ' )
        endif
        !------------------------------------------------------------------------------------------
             
        !------------------------------------------------------------------------------------------
        ! Debug
        if (iDEBUG.gt.0) then
            call mprintf(.true., iINFO_Extra, ' ========= SURFACE FLOW START ========= ') 
            call mprintf(.true., iINFO_Extra, checkvar(oHMC_Vars(iID)%a2dAreaCell, oHMC_Vars(iID)%a2iMask, 'AREACELL') )
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarHydroUpdC, oHMC_Vars(iID)%a2iMask, 'HYDRO UPD START') )
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarHydroPrevC, oHMC_Vars(iID)%a2iMask, 'HYDRO PREV START') )
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarQDisOut, oHMC_Vars(iID)%a2iMask, 'QDIS OUT START (Qout)') )
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarQVolOut, oHMC_Vars(iID)%a2iMask, 'QVOL OUT START (Qtmp)') )
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarRouting, oHMC_Vars(iID)%a2iMask, 'ROUTING START') )
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarIntensityPrev, oHMC_Vars(iID)%a2iMask, 'INTENSITY START') )
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarFlowExf, oHMC_Vars(iID)%a2iMask, 'EXFILTRATION START') )
            call mprintf(.true., iINFO_Extra, checkarray(a2dVarHydroPlant(:,2), 'HYDRO PLANT START') )
            call mprintf(.true., iINFO_Extra, checkarray(a2dVarHydroCatch(:,2), 'HYDRO CATCH START') )
            call mprintf(.true., iINFO_Extra, checkarray(a2dVarHydroRelease(:,2), 'HYDRO RELEASE START') )
            call mprintf(.true., iINFO_Extra, ' ') 
        endif
        !------------------------------------------------------------------------------------------

        !------------------------------------------------------------------------------------------
        ! Partition coefficient and length of flooding
        a2dPartition = 0.0;
        where (oHMC_Vars(iID)%a2iMask.gt.0.0)
            a2dPartition = (a2dVarWidthC/sqrt(a2dVarAreaCell))**((100.0/sqrt(a2dVarAreaCell))**0.35)
            a2dVarBF = dPa*a2dVarWidthC**dPb
        endwhere
        where ( (oHMC_Vars(iID)%a2iMask.gt.0.0) .and. (a2dPartition.le.0) )
            a2dPartition = 0.01
        endwhere
        where ( (oHMC_Vars(iID)%a2iMask.gt.0.0) .and. (a2dPartition.ge.1) )
            a2dPartition = 0.99
        endwhere
        !------------------------------------------------------------------------------------------

        !------------------------------------------------------------------------------------------
        ! Channel max surface velocity (UcMax)
        dUMax = 3600.0/dDtSurfaceflow*0.5
        dTmmm = MAXVAL(MAXVAL(oHMC_Vars(iID)%a2dUh,dim=1,mask=oHMC_Vars(iID)%a2iChoice.le.1.and. &
                    oHMC_Vars(iID)%a2iMask.gt.0))
        dTmmm = MAXVAL(MAXVAL(oHMC_Vars(iID)%a2dUc,dim=1,mask=oHMC_Vars(iID)%a2iChoice.le.1.and. &
                    oHMC_Vars(iID)%a2iMask.gt.0))  
                    
        ! Hill overland equation 
        a2dVarUhAct = oHMC_Vars(iID)%a2dUh
        where ( (oHMC_Vars(iID)%a2iChoice.eq.0) .and. (a2dVarUhact.gt.dUMax) )  ! numerical check
            a2dVarUhAct = dUMax
        endwhere
        
        a2dVarUcAct = oHMC_Vars(iID)%a2dUc
        where ( (oHMC_Vars(iID)%a2iChoice.eq.0) .and. (a2dVarUcAct.gt.dUMax) )  ! numerical check
            a2dVarUcAct = dUMax
        endwhere
        
        dTmmm = MAXVAL(MAXVAL(oHMC_Vars(iID)%a2dUh,dim=1,mask=oHMC_Vars(iID)%a2iChoice.le.1.and. &
                    oHMC_Vars(iID)%a2iMask.gt.0))
        dTmmm = MAXVAL(MAXVAL(oHMC_Vars(iID)%a2dUc,dim=1,mask=oHMC_Vars(iID)%a2iChoice.le.1.and. &
                    oHMC_Vars(iID)%a2iMask.gt.0))  
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Hydro variable (previous and update array)
        where (a2dVarHydroPrevC .lt. 0.0) ! in meters
            a2dVarHydroPrevC = 0.0000001
            a2dVarHydroPrevH = 0.0000001
        endwhere        
        where (a2dVarHydroPrevC .gt. 200.0) ! in meters
            a2dVarHydroPrevC = 0.0000001
            a2dVarHydroPrevH = 0.0000001
        endwhere
        
        ! Updating hydro variable (using previous step and checking values under zero) --> WaterLevel (tirante)
        a2dVarHydroUpdH = a2dVarHydroPrevH
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Routing sui versanti variable
        where( a2dVarRouting .lt. 0.0)
            a2dVarRouting = 0.0
        endwhere

        where ((oHMC_Vars(iID)%a2iChoice.le.1) .and. (oHMC_Vars(iID)%a2iMask.gt.0.0))
            a2dVarAreaCell = a2dVarAreaCell
        elsewhere
            a2dVarAreaCell = 0.0
        endwhere        
        !------------------------------------------------------------------------------------------
       
        !------------------------------------------------------------------------------------------
        ! Equations for hillslope
        where(a2dVarIntensityPrev.lt.0.0)
            a2dVarIntensityPrev = 0.0
        endwhere
        ! Updating variables surface hillsolpe cell input (exfiltration + runoff m^3/s]) --> CHECKING CONVERSION
        where(oHMC_Vars(iID)%a2iMask.gt.0.0.and.(oHMC_Vars(iID)%a2iChoice.le.1))
            a2dVarIntensityUpd = oHMC_Vars(iID)%a2dRunoffH*a2dVarAreaCell   + a2dVarFlowExf*a2dVarAreaCell 
        endwhere
                   
        where ((oHMC_Vars(iID)%a2iChoice.le.1) .and. (oHMC_Vars(iID)%a2iMask.gt.0.0))
            
            a2dVarQDisOut = a2dVarWidthH*a2dVarUhact*(tan(oHMC_Vars(iID)%a2dBeta)**0.5)*a2dVarHydroPrevH**(5.0/3.0)
            
            where (a2dVarQDisOut*dDtSurfaceflow.gt.dKint*(a2dVarHydroPrevH*a2dVarWidthH*sqrt(a2dVarAreaCell)+ &
                        a2dVarIntensityUpd*dDtSurfaceflow))
                a2dVarQDisOut = dKint*(a2dVarHydroPrevH*a2dVarWidthH*sqrt(a2dVarAreaCell)+a2dVarIntensityUpd*dDtSurfaceflow)/ &
                                dDtSurfaceflow
            endwhere
             
            !Upgrade Level on hillslopes
            a2dVarHydroUpdH = a2dVarHydroPrevH +(-a2dVarQDisOut*dDtSurfaceflow+a2dVarIntensityUpd*dDtSurfaceflow)/ &
                        (a2dVarWidthH*sqrt(a2dVarAreaCell))
            !Trapeeze method 1 iteration
            a2dVarQDisOut = a2dVarWidthH*a2dVarUhact*(tan(oHMC_Vars(iID)%a2dBeta)**0.5)* &
                            (0.5*a2dVarHydroPrevH**(5.0/3.0)+0.5*a2dVarHydroUpdH**(5.0/3.0))
            where (a2dVarQDisOut*dDtSurfaceflow.gt.dKint*(a2dVarHydroPrevH*a2dVarWidthH*sqrt(a2dVarAreaCell)+ &
                a2dVarIntensityUpd*dDtSurfaceflow))
                a2dVarQDisOut = dKint*(a2dVarHydroPrevH*a2dVarWidthH*sqrt(a2dVarAreaCell)+a2dVarIntensityUpd*dDtSurfaceflow)/ &
                                dDtSurfaceflow
            endwhere
            oHMC_Vars(iID)%a2dQH = a2dVarQDisOut !Streamflow in hillslopes
            a2dVarHydroUpdH = a2dVarHydroPrevH+(-a2dVarQDisOut*dDtSurfaceflow+a2dVarIntensityUpd*dDtSurfaceflow)/ &
                        (a2dVarWidthH*sqrt(a2dVarAreaCell))
                        
        endwhere       
        !-------------------------------------------------------------------------------------------

        !------------------------------------------------------------------------------------------ 
        ! Put null on input matrix because now used for Channels
        where(oHMC_Vars(iID)%a2iMask.gt.0.0)
            a2dVarIntensityUpd = 0.0
        endwhere
        !------------------------------------------------------------------------------------------ 

        !------------------------------------------------------------------------------------------
        ! Check plant(s) availability
        if (iNPlant .gt. 0) then
            
            !------------------------------------------------------------------------------------------
            ! Cycle on plant(s)
            iP = 0;
            do iP = 1, iNPlant
                
                !------------------------------------------------------------------------------------------
                ! Get plant information
                iI = 0; iJ = 0;
                iI = oHMC_Vars(iID)%a2iXYPlant(iP, 2); 
                iJ = oHMC_Vars(iID)%a2iXYPlant(iP, 1);

                dVarAreaCell = oHMC_Vars(iID)%a2dAreaCell(iI, iJ)
               
                ! Check area cell
                if (dVarAreaCell.lt.0) then
                    dVarAreaCell = oHMC_Vars(iID)%dDxM*oHMC_Vars(iID)%dDyM
                endif
                !------------------------------------------------------------------------------------------
                
                !------------------------------------------------------------------------------------------
                ! Add plant data to intensity and updata dam volume
                if (a2dVarHydroPlant(iP, iTTemp + 1) .ge. 0) then !Se ho le turbinate (si presume che i tempi siano relativi all'immissione)
                    
                    !------------------------------------------------------------------------------------------
                    ! Update intensity
                    a2dVarIntensityUpd(iI, iJ) = a2dVarIntensityUpd(iI, iJ) + a2dVarHydroPlant(iP, iTTemp + 1)*(dVarAreaCell)/ &
                                                (1000*3600) !m^3 (giulia: eventualmente m3/s, no?)

                    ! Update dam volume
                    a1dVarVDam(oHMC_Vars(iID)%a1iFlagDamPlant(iP)) = a1dVarVDam(oHMC_Vars(iID)%a1iFlagDamPlant(iP)) - &
                        a2dVarHydroPlant(iP, iTTemp + 1)*(dVarAreaCell)/(1000*3600)*dDtSurfaceflow !m^3
                    !------------------------------------------------------------------------------------------
                        
                else
                    
                    !------------------------------------------------------------------------------------------
                    ! Compute plant max discharge (m^3/s)
                    if ( a1dVarVDam(oHMC_Vars(iID)%a1iFlagDamPlant(iP)) .lt. &
                         oHMC_Vars(iID)%a1dVMaxDam(oHMC_Vars(iID)%a1iFlagDamPlant(iP)) )then
                        
                        ! VDam < VDamMax --> Q**6;
                        a1dVarQPlant(iP) = oHMC_Vars(iID)%a1dQMaxPlant(iP)* &
                                           (a1dVarVDam(oHMC_Vars(iID)%a1iFlagDamPlant(iP))/ &
                                            oHMC_Vars(iID)%a1dVMaxDam(oHMC_Vars(iID)%a1iFlagDamPlant(iP)))**6
                                            
                    else
                        ! VDam => VDamMax --> QMax
                        a1dVarQPlant(iP) = oHMC_Vars(iID)%a1dQMaxPlant(iP)
                    endif

                    ! Check plant discharge
                    if (a1dVarQPlant(iP) .lt. 0.0) a1dVarQPlant(iP) = 0.0
                    !------------------------------------------------------------------------------------------
                    
                    !------------------------------------------------------------------------------------------
                    ! Update intensity
                    a2dVarIntensityUpd(iI, iJ) = a2dVarIntensityUpd(iI, iJ) + &
                                                 a1dVarQPlant(iP) !m^3 (giulia: eventualmente m3/s, no?)

                    ! Update dam volume
                    a1dVarVDam(oHMC_Vars(iID)%a1iFlagDamPlant(iP)) = a1dVarVDam(oHMC_Vars(iID)%a1iFlagDamPlant(iP)) - &
                                                                     a1dVarQPlant(iP)*dDtSurfaceflow
                    
                    !write(*,*) a1dVarVDam(oHMC_Vars(iID)%a1iFlagDamPlant(iP)), a1dVarQPlant(iP)
                    !------------------------------------------------------------------------------------------
                                                                     
                endif
                !------------------------------------------------------------------------------------------
                
                !------------------------------------------------------------------------------------------
                ! Check dam volume
                if(a1dVarVDam(oHMC_Vars(iID)%a1iFlagDamPlant(iP)) .lt. 0.0) a1dVarVDam(oHMC_Vars(iID)%a1iFlagDamPlant(iP)) = 0.0
                !------------------------------------------------------------------------------------------
                
            enddo 
            !------------------------------------------------------------------------------------------
        
        endif
        !------------------------------------------------------------------------------------------ 
        
        !------------------------------------------------------------------------------------------ 
        ! Check release(s) availability
        if (iNRelease .gt. 0) then
            
            !------------------------------------------------------------------------------------------
            ! Cycle on release(s)
            iR = 0;
            do iR = 1, iNRelease
                
                !------------------------------------------------------------------------------------------
                ! Get plant information
                iI = 0; iJ = 0;
                iI = oHMC_Vars(iID)%a2iXYRelease(iR, 2); iJ = oHMC_Vars(iID)%a2iXYRelease(iR, 1);
                
                dVarAreaCell = oHMC_Vars(iID)%a2dAreaCell(iI, iJ)
               
                ! Check area cell
                if (dVarAreaCell.lt.0) then
                    dVarAreaCell = oHMC_Vars(iID)%dDxM*oHMC_Vars(iID)%dDyM
                endif
                !------------------------------------------------------------------------------------------
                
                !------------------------------------------------------------------------------------------
                ! Check hydro release(s) data 
                if (a2dVarHydroRelease(iR,iTTemp + 1) .lt. 0.0) a2dVarHydroRelease(iR,iTTemp + 1) = 0.0
                
                
                ! Update intensity using hydro release (m^3/s)
                a2dVarIntensityUpd(iI, iJ) = a2dVarIntensityUpd(iI, iJ) + a2dVarHydroRelease(iR,iTTemp + 1)/(1000.0*3600) &
                                             *dVarAreaCell
                !------------------------------------------------------------------------------------------
                
            enddo
            !------------------------------------------------------------------------------------------
            
        endif
        !------------------------------------------------------------------------------------------ 
        
        !------------------------------------------------------------------------------------------
        ! Check catch and Modification of Channel level because of volume abstraction
        if (iNCatch .gt. 0) then
            ! Cycle on catch(es) ---> subtract turbinate(s) from routing
            iC = 0;
            do iC = 1, iNCatch
                
                iI = 0; iJ = 0; dDh = 0.0
                iI = oHMC_Vars(iID)%a2iXYCatch(iC, 2); iJ = oHMC_Vars(iID)%a2iXYCatch(iC, 1);
                dVarTcCatch = oHMC_Vars(iID)%a1dTCorrCatch(iC)
                dVarQminCatch = oHMC_Vars(iID)%a1dQMinCatch(iC)
                
                dVarAreaCell = oHMC_Vars(iID)%a2dAreaCell(iI, iJ)
                
                ! Check area cell
                if (dVarAreaCell.lt.0) then
                    dVarAreaCell = oHMC_Vars(iID)%dDxM*oHMC_Vars(iID)%dDyM
                endif
                
                ! Compute minimum discharge
                dVarQminCatch = oHMC_Vars(iID)%a1dQMinCatch(iC) ! [m^3/s]

                ! Compute Q catch
                dDh = a2dVarHydroCatch(iC, iTTemp + 1) ! [m^3/s]
                
                ! Avoid < 0 values in turbinate for mismatch in length of simulation
                if (dDh.lt.0.0) dDh = 0.0

                ! Defining flow directions
                iII = int((int(oHMC_Vars(iID)%a2iPNT(iI,iJ))  - 1)/3) - 1
                iJJ = int(oHMC_Vars(iID)%a2iPNT(iI,iJ)) - 5 - 3*iII
                iIII = iI + iII
                iJJJ = iJ + iJJ
                
                ! Index(es) not allowed
                dRoutPrev = 0.0
                if( (iIII.ge.1) .and. (iJJJ.ge.1) ) then
                    
                    ! --- start correction 2020/07/18 ---
                    dRoutPrev = oHMC_Vars(iID)%a2dQup(iIII, iJJJ) 
                    
                    ! Q less than QMV
                    if(oHMC_Vars(iID)%a2dQup(iIII, iJJJ).le.dVarQminCatch)then
                        dDh=0.0
                    ! Q larger than QMV
                    else
                        !Q-QMV < Qplant
                        if((oHMC_Vars(iID)%a2dQup(iIII, iJJJ)-dDh).lt.dVarQminCatch) then
                                dDh = oHMC_Vars(iID)%a2dQup(iIII, iJJJ)-dVarQminCatch				
                        endif
                    endif
                    
                    oHMC_Vars(iID)%a2dQup(iIII, iJJJ) = oHMC_Vars(iID)%a2dQup(iIII, iJJJ) - dDh
                    
                    !Hc- Hc due to QplantCorrected
                    !a2dVarHydroPrevC(iIII, iJJJ) = a2dVarHydroPrevC(iIII, iJJJ)-dDh*dDtSurfaceflow/ &
                    !                            (a2dVarWidthC(iIII, iJJJ)*sqrt(dVarAreaCell))
                    
                    ! --- end correction 2020/07/18 ---
                    
                    ! Update data release(s)
                    if (iFlagReleaseMass .eq. 1) then   ! activate/deactivate release mass update
                        if (iNCatch .eq. iNRelease) then
                            iShift = nint(dVarTcCatch*60/dDtDataForcing)
                            iRank = size(a2dVarHydroRelease, dim = 2)

                            if (iTTemp + iShift .lt. iRank ) then
                                a2dVarHydroRelease(iC, iTTemp + 1) = dDh*(1000.0*3600) &
                                             /dVarAreaCell ! in m^3/s to maintain the mass balance
                            endif

                        endif
                    endif
                                   
                    if (oHMC_Vars(iID)%a2dQC(iIII, iJJJ) .lt. 0.0) then
                        oHMC_Vars(iID)%a2dQC(iIII, iJJJ) = 0.0
                    endif
                    
                    if (oHMC_Vars(iID)%a2dQup(iIII, iJJJ) .lt. 0.0) then
                        oHMC_Vars(iID)%a2dQup(iIII, iJJJ) = 0.0
                    endif
                    
                    if(a2dVarHydroPrevC(iIII, iJJJ).lt.0.0) then
                        a2dVarHydroPrevC(iIII, iJJJ) = 0.0
                    endif

                endif

            enddo
            
        endif
        !------------------------------------------------------------------------------------------       

        !------------------------------------------------------------------------------------------
        ! Compute flooding
        if(iFlagFlood.eq.1) then
            call HMC_Phys_Convolution_Apps_Flooding(iID, iRows, iCols, &
                                                    dDtSurfaceflow, a2dVarBF, a2dVarHydroPrevC)
        endif
        !------------------------------------------------------------------------------------------

        !------------------------------------------------------------------------------------------
        ! CHANNELS 
        ! a2dVarHydroUpdC = a2dVarHydroPrevC
        where(oHMC_Vars(iID)%a2iMask.gt.0.0.and.(oHMC_Vars(iID)%a2iChoice.le.1.0))
            a2dVarIntensityUpd = a2dVarIntensityUpd +  & !Plant and realese data
                                 oHMC_Vars(iID)%a2dQH*a2dPartition + & !From hillslopes to channels
                                 oHMC_Vars(iID)%a2dQup + & !From upstream cells
                                 oHMC_Vars(iID)%a2dFlowDeep*a2dVarWidthC*sqrt(a2dVarAreaCell)/(1000.0*dDtDataForcing) + &      ! Deep flow
                                 oHMC_Vars(iID)%a2dWSRunoff   ! WSRunoff m^3/s            
        endwhere
        
        a2dVarQDisOut = 0.0
        where ((oHMC_Vars(iID)%a2iChoice.le.1) .and. (oHMC_Vars(iID)%a2iMask.gt.0.0))
            a2dVarQDisOut = a2dVarWidthC*a2dVarUcact*(tan(oHMC_Vars(iID)%a2dBeta)**0.5)*a2dVarHydroPrevC**(1+dBc)
            where (a2dVarQDisOut*dDtSurfaceflow.gt.dKint*(a2dVarHydroPrevC*a2dVarWidthC*sqrt(a2dVarAreaCell)+ &
                        a2dVarIntensityUpd*dDtSurfaceflow))
                a2dVarQDisOut = dKint*(a2dVarHydroPrevC*a2dVarWidthC*sqrt(a2dVarAreaCell)+a2dVarIntensityUpd*dDtSurfaceflow)/ &
                                dDtSurfaceflow
            ENDwhere
            ! Upgrade Level on CHannels
            a2dVarHydroUpdC = a2dVarHydroPrevC +(-a2dVarQDisOut*dDtSurfaceflow+a2dVarIntensityUpd*dDtSurfaceflow)/ &
                        (a2dVarWidthC*sqrt(a2dVarAreaCell))
            ! Trapeeze method 1 iteration
            a2dVarQDisOut = a2dVarWidthC*a2dVarUcact*(tan(oHMC_Vars(iID)%a2dBeta)**0.5)* &
                            (0.5*a2dVarHydroPrevC**(1+dBc)+0.5*a2dVarHydroUpdC**(1+dBc))
            where (a2dVarQDisOut*dDtSurfaceflow.gt.dKint*(a2dVarHydroPrevC*a2dVarWidthC*sqrt(a2dVarAreaCell)+ &
                    a2dVarIntensityUpd*dDtSurfaceflow))
                a2dVarQDisOut = dKint*(a2dVarHydroPrevC*a2dVarWidthC*sqrt(a2dVarAreaCell)+a2dVarIntensityUpd*dDtSurfaceflow)/ &
                                dDtSurfaceflow
            ENDwhere
            
            a2dVarHydroUpdC = a2dVarHydroPrevC+(-a2dVarQDisOut*dDtSurfaceflow+a2dVarIntensityUpd*dDtSurfaceflow)/ &
                        (a2dVarWidthC*sqrt(a2dVarAreaCell))
        ENDwhere    
        dTmmm = MAXVAL(MAXVAL(a2dVarHydroUpdC,dim=1,mask=oHMC_Vars(iID)%a2iChoice.le.1.and. &
                    oHMC_Vars(iID)%a2iMask.gt.0))   
                           
        ! Check for zero values
        where(a2dVarHydroUpdH.lt.0.0)
                a2dVarHydroUpdH = 0.0
        endwhere
        where(a2dVarHydroUpdC.lt.0.0)
                a2dVarHydroUpdC = 0.0
        endwhere                    
                            
        ! Input in Linear Lakes
        a2dVarIntensityUpd = 0.0
        where(oHMC_Vars(iID)%a2iMask.gt.0.0.and.(oHMC_Vars(iID)%a2iChoice.gt.1))
            a2dVarIntensityUpd = oHMC_Vars(iID)%a2dRunoffH   + a2dVarFlowExf*a2dVarAreaCell 
        endwhere
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Check dam availability
        if (iNDam .gt. 0) then
            ! Cycle on dam(s)
            iD = 0;
            do iD = 1, iNDam
                
                iI = 0; iJ = 0;
                iI = oHMC_Vars(iID)%a2iXYDam(iD, 2); iJ = oHMC_Vars(iID)%a2iXYDam(iD, 1);
                
                ! Lake condition
                if (oHMC_Vars(iID)%a1dCodeDam(iD) .gt. 0.0) then
                    ! Distributed lake
                    dVLake = 0.0;
                    dVLake = sum(sum(a2dVarIntensityUpd, dim=1, mask=oHMC_Vars(iID)%a2iChoice.eq.oHMC_Vars(iID)%a1dCodeDam(iD)))
                    a1dVarVDam(iD) = a1dVarVDam(iD) + dVLake*dDtSurfaceflow ! in m^3
                else
                    ! Punctual lake
                    a1dVarVDam(iD) = a1dVarVDam(iD) + a2dVarQDisOut(iI, iJ)*dDtSurfaceflow + & 
                                        oHMC_Vars(iID)%a2dQH(iI, iJ)*(1-a2dPartition(iI, iJ))*dDtSurfaceflow 
                    a2dVarQDisOut(iI, iJ) = 0.0
                    oHMC_Vars(iID)%a2dQH(iI, iJ) = 0.0
                endif
                
            enddo
        endif
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Check lake availability
        if (iNLake .gt. 0) then
            ! Cycle on lake
            iL = 0;
            do iL = 1, iNLake
                iI = 0; iJ = 0;
                iI = oHMC_Vars(iID)%a2iXYLake(iL, 2); iJ = oHMC_Vars(iID)%a2iXYLake(iL, 1);
                
                if (oHMC_Vars(iID)%a1dCodeLake(iL) .gt. 0) then
                    ! Distributed lake
                    dVLake = 0.0;
                    dVLake = sum(sum(a2dVarIntensityUpd, dim=1, mask=oHMC_Vars(iID)%a2iChoice.eq.oHMC_Vars(iID)%a1dCodeLake(iL)))
                    a1dVarVLake(iL) = a1dVarVLake(iL) + dVLake*dDtSurfaceflow ! in m^3
                    !Aggiungere annullamento in tutti i pixel del lago
                else
                    ! Punctual lake
                    a1dVarVLake(iL) = a1dVarVLake(iL) + a2dVarQDisOut(iI, iJ)*dDtSurfaceflow + & 
                                        oHMC_Vars(iID)%a2dQH(iI, iJ)*(1-a2dPartition(iI, iJ))*dDtSurfaceflow 
                                        
                    a2dVarQDisOut(iI, iJ) = 0.0
                    oHMC_Vars(iID)%a2dQH(iI, iJ) = 0.0
                endif
                
            enddo
        endif
        !------------------------------------------------------------------------------------------

        !------------------------------------------------------------------------------------------  
        ! Calculating flow 
        ! Calcolo la porzione di acqua che va nella cella successiva
        ! Essa verr� sommata alla pioggia nella subroutine di Horton
        ! l'istante successivo  
        a2dVarRouting = 0.0
        oHMC_Vars(iID)%a2dQup = 0.0 !Initialize and put in channels the following t
        oHMC_Vars(iID)%a2dQC = a2dVarQDisOut !Streamflow in Channels Updated
        do iI = 1, iRows
            do iJ = 1, iCols 
                
                ! DEM condition
                if (oHMC_Vars(iID)%a2iMask(iI,iJ).gt.0.0) then
                    
                    ! Rate and pointers definition
                    !iVarPNT = 0
                    !iVarPNT = int(oHMC_Vars(iID)%a2iPNT(iI,iJ))
                    
                    ! Defining flow directions
                    iII = int((int(oHMC_Vars(iID)%a2iPNT(iI,iJ))  - 1)/3) - 1
                    iJJ = int(oHMC_Vars(iID)%a2iPNT(iI,iJ)) - 5 - 3*iII
                    iIII = iI + iII
                    iJJJ = iJ + iJJ
                    
                    if ( (iIII.ge.1) .and. (iJJJ.ge.1) ) then
                        
                        ! Integrazione del routing in mm/passo_integrazione_del_routing
                        ! L'acqua viene mandata nella cella successiva e utilizzata nella Subrotine
                        ! Horton
                        dRm = oHMC_Vars(iID)%a2dQC(iI, iJ);                        
                        oHMC_Vars(iID)%a2dQup(iIII, iJJJ) = oHMC_Vars(iID)%a2dQup(iIII, iJJJ) + dRm ![m3/s] Channels
                        
                        dRm = (1-a2dPartition(iI, iJ))* oHMC_Vars(iID)%a2dQH(iI, iJ)*dDtSurfaceflow*1000/a2dVarAreaCell(iI, iJ) !Routing hillslope in mm
                        a2dVarRouting(iIII, iJJJ) = a2dVarRouting(iIII, iJJJ) + dRm  ![mm]
                        
                        a2dVarQVolOut(iI, iJ) = dRm/dDtSurfaceflow !Controllare se serve
                        
                    endif 
                endif   
            enddo
        enddo				
        !------------------------------------------------------------------------------------------

        !------------------------------------------------------------------------------------------
        ! Check joint availability
        if (iNJoint .gt. 0) then
            ! Cycle on joint(s) 
            iJ = 0;
            do iJ = 1, iNJoint
                iIm = 0; iJm = 0; iIin = 0; iJin = 0;
                iIm = oHMC_Vars(iID)%a2iXYJoint(iJ,2); iJm = oHMC_Vars(iID)%a2iXYJoint(iJ,1);
                iIin = oHMC_Vars(iID)%a2iXYInJoint(iJ,2); iJin = oHMC_Vars(iID)%a2iXYInJoint(iJ,1);
                
                dHm = 0.0; dHin = 0.0;
                dHm = a2dVarHydroUpdC(iIm, iJm) !dH2
                dHin = a2dVarHydroUpdC(iIin,iJin) !dH1
                
                if ( (dHin.lt.dHm) .and. (dHm.gt.0.0) ) then
                    dQt = 0.0; dHinFD = 0.0;
                    dQt = a2dVarQDisOut(iIin, iJin) ! in [m^3/s]
                    dHinFD = sqrt(dHin**2 + 1000*1000*2*(dQt**2)/(oHMC_Vars(iID)%a2dAreaCell(iIm, iJm)*9.8*dHin/1000)) ! derivata eq delle spinte
                    
                    if (dHinFD/dHm .lt. oHMC_Vars(iID)%a1dThrLevelJoint(iJ)) then
                        
                        iIout = 0; iJout = 0;
                        iIout = oHMC_Vars(iID)%a2iXYOutJoint(iJ,2); iJout = oHMC_Vars(iID)%a2iXYOutJoint(iJ,1);
                        
                        ! Main channel
                        oHMC_Vars(iID)%a2dQC(iIout,iJout) = oHMC_Vars(iID)%a2dQC(iIout,iJout) +  &
                                        oHMC_Vars(iID)%a2dQC(iIm,iJm)*(1 - dHinFD/dHm) ! Routing dove immetto la derivazione del Master 
                        oHMC_Vars(iID)%a2dQC(iIm,iJm) = oHMC_Vars(iID)%a2dQC(iIm,iJm)*dHinFD/dHm ! Routing in main channel
                        
                        ! Tributary channel
			oHMC_Vars(iID)%a2dQC(iIout,iJout) = oHMC_Vars(iID)%a2dQC(iIout,iJout) +  &
                                         oHMC_Vars(iID)%a2dQC(iIin,iJin)*(1 - dHinFD/dHm) ! Routing dove immetto la derivazione dell'immissario
			oHMC_Vars(iID)%a2dQC(iIin,iJin) = oHMC_Vars(iID)%a2dQC(iIin,iJin)*dHinFD/dHm ! Routing in main channel
                        
                    endif
                endif
            enddo    
        endif
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Compute dam spilling
        call HMC_Phys_Dam_Spilling(iID, iNDam, dDtSurfaceflow, &
                                                     a1dVarVDam, a1dVarQoutDam, a1dVarCoeffDam, a1dVarLDam)
        !------------------------------------------------------------------------------------------
                                     
        !------------------------------------------------------------------------------------------
        ! Check dam availability
        if (iNDam .gt. 0) then
            ! Cycle on dam
            iD = 0
            do iD = 1, iNDam
            
                iI = 0; iJ = 0;
                iI = oHMC_Vars(iID)%a2iXYDam(iD, 2); iJ = oHMC_Vars(iID)%a2iXYDam(iD, 1);
                
                ! Pointers definition
                !iVarPNT = 0
                !iVarPNT = int(oHMC_Vars(iID)%a2iPNT(iI,iJ))
                
                ! Defining flow directions
                iII = int((int(oHMC_Vars(iID)%a2iPNT(iI,iJ))  - 1)/3) - 1
                iJJ = int(oHMC_Vars(iID)%a2iPNT(iI,iJ)) - 5 - 3*iII
                iIII = iI + iII
                iJJJ = iJ + iJJ
                
                ! Update Qup in Channels in m^3/s                
                oHMC_Vars(iID)%a2dQup(iIII, iJJJ) = oHMC_Vars(iID)%a2dQup(iIII, iJJJ) + &
                                a1dVarQoutDam(iD)/(dDtSurfaceflow*1000)*oHMC_Vars(iID)%a2dAreaCell(iIII,iJJJ)                
                ! Volume to mean dam level [m]
                where( oHMC_Vars(iID)%a2iChoice.eq.oHMC_Vars(iID)%a1dCodeDam(iD) .and. (oHMC_Vars(iID)%a2iMask.gt.0.0) ) 
                    a2dVarHydroUpdC = a1dVarVDam(iD)/(oHMC_Vars(iID)%a1iNCellDam(iD)*oHMC_Vars(iID)%a2dAreaCell(iI,iJ)) ! [m]
                endwhere
                    
            enddo
            
        endif
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Debug
        if (iDEBUG.gt.0) then
            call mprintf(.true., iINFO_Extra, ' ') 
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarHydroUpdC, oHMC_Vars(iID)%a2iMask, 'HYDRO UPD END') )
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarHydroPrevC, oHMC_Vars(iID)%a2iMask, 'HYDRO PREV END') )
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarQDisOut, oHMC_Vars(iID)%a2iMask, 'QDIS OUT END (Qout)') )
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarQVolOut, oHMC_Vars(iID)%a2iMask, 'QVOL OUT END (Qtmp)') )
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarRouting, oHMC_Vars(iID)%a2iMask, 'ROUTING END') )
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarIntensityUpd, oHMC_Vars(iID)%a2iMask, 'INTENSITY END') )
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarFlowExf, oHMC_Vars(iID)%a2iMask, 'EXFILTRATION END') )     
            call mprintf(.true., iINFO_Extra, checkarray(a2dVarHydroPlant(:,2), 'HYDRO PLANT END') )
            call mprintf(.true., iINFO_Extra, checkarray(a2dVarHydroCatch(:,2), 'HYDRO CATCH END') )
            call mprintf(.true., iINFO_Extra, checkarray(a2dVarHydroRelease(:,2), 'HYDRO RELEASE END') )
            call mprintf(.true., iINFO_Extra, ' ========= SURFACE FLOW END =========== ') 
        endif
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Updating model global variable(s)
	oHMC_Vars(iID)%a2dQVolOut = a2dVarQVolOut
        oHMC_Vars(iID)%a2dQDisOut = a2dVarQDisOut ! Forse non serve
        
        oHMC_Vars(iID)%a2dHydroC = a2dVarHydroUpdC
        oHMC_Vars(iID)%a2dHydroH = a2dVarHydroUpdH
        
        ! Routing to Hillslope cell portion from upstream
        oHMC_Vars(iID)%a2dRouting = a2dVarRouting   ! Compute to use in horton and after set to zero
        
        oHMC_Vars(iID)%a1dVDam = a1dVarVDam
        oHMC_Vars(iID)%a1dHDam = a1dVarHDam 
        oHMC_Vars(iID)%a1dLDam = a1dVarLDam
        oHMC_Vars(iID)%a1dCoeffDam = a1dVarCoeffDam 
        oHMC_Vars(iID)%a1dQoutDam = a1dVarQoutDam
        
        oHMC_Vars(iID)%a1dVLake = a1dVarVLake
        
        oHMC_Vars(iID)%a2dHydroPlant = a2dVarHydroPlant
        oHMC_Vars(iID)%a2dHydroCatch = a2dVarHydroCatch
        oHMC_Vars(iID)%a2dHydroRelease = a2dVarHydroRelease
        
        end subroutine HMC_Phys_Convolution_Apps_SurfaceFlow_ChannelFraction
        !------------------------------------------------------------------------------------------
        
end module HMC_Module_Phys_Convolution_Apps_SurfaceFlow
!------------------------------------------------------------------------------------------

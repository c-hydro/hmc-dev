!------------------------------------------------------------------------------------------    
! File:   HMC_Module_Phys_ET.f90
! Author: Fabio Delogu
!
! Created on April 2, 2014, 5:19 PM
!------------------------------------------------------------------------------------------

!------------------------------------------------------------------------------------------
! Module Header
module HMC_Module_Phys_ET
    
    !------------------------------------------------------------------------------------------
    ! External module(s) for all subroutine in this module
    use HMC_Module_Vars_Loader, only: oHMC_Vars
    
    use HMC_Module_Tools_Debug
    
    ! Implicit none for all subroutines in this module
    implicit none
    !------------------------------------------------------------------------------------------
    
contains

    !------------------------------------------------------------------------------------------
    ! Subroutine to calculate evapotranspiration
    subroutine HMC_Phys_ET_Cpl(iID, iRows, iCols, iTime, iDaySteps, sTime, iNLake, iNDam)
    
	!------------------------------------------------------------------------------------------
        ! Variable(s) declaration
        integer(kind = 4)           :: iID
        integer(kind = 4)           :: iRows, iCols
        integer(kind = 4)           :: iNLake, iNDam
        integer(kind = 4)           :: iTime, iDaySteps, iStep
        integer(kind = 4)           :: iI, iJ, iL, iD
        
        real(kind = 4), dimension (iRows, iCols)         :: a2dVarET, a2dVarVTot, a2dVarETPot
        real(kind = 4), dimension (iRows, iCols)         :: a2dVarVRet, a2dVarVTotWP
        
        real(kind = 4)              :: dVarET, dVarAE, dVarETLake, dVarETTot, dVarETPot
        
        integer(kind = 4), dimension (iRows, iCols)         :: a2iVarMask, a2iVarChoice
        real(kind = 4), dimension (iRows, iCols)            :: a2dVarDEM, a2dVarAreaCell, a2dVarCtWP
        
        real(kind = 4), dimension (iRows, iCols)            :: a2dVarAE, a2dVarAEres
        
        integer(kind = 4), dimension (iNDam, 2)             :: a2iVarXYDam   
        real(kind = 4), dimension (iNDam)                   :: a1dVarCodeDam 
        real(kind = 4), dimension (iNDam)                   :: a1dVarVDam 
        integer(kind = 4), dimension (iNLake, 2)            :: a2iVarXYLake  
        real(kind = 4), dimension (iNLake)                  :: a1dVarCodeLake 
        real(kind = 4), dimension (iNLake)                  :: a1dVarVLake
        
        
        character(len = 19)                                 :: sTime
        
        character(len = 10)                                 :: sVarAE, sVarET, sVarETTot, sVarETPot
        character(len = 10), parameter                      :: sFMTVarET = "(F7.4)"
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Initialization variable(s)
        a2iVarMask = 0; a2iVarChoice = 0; a2dVarDEM = 0.0; a2dVarAreaCell = 0.0; a2dVarCtWP = 0.0;
        a2dVarAE = 0.0; a2dVarET = 0.0; a2dVarETPot = 0.0; 
        a2dVarVRet = 0.0; a2dVarVTot = 0.0; a2dVarVTotWP = 0.0;
        dVarAE = 0.0; dVarET = 0.0; dVarETLake = 0.0; a2dVarAEres = 0.0;
        dVarETTot = 0.0; dVarETPot = 0.0;
        
        a2iVarXYDam = 0; a1dVarCodeDam = 0.0; a1dVarVDam = 0.0; 
        a2iVarXYLake = 0; a1dVarCodeLake = 0.0; a1dVarVLake = 0.0;
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Data static definition
        a2dVarDEM = oHMC_Vars(iID)%a2dDem
        a2iVarMask = oHMC_Vars(iID)%a2iMask
        a2iVarChoice = oHMC_Vars(iID)%a2iChoice
        a2dVarAreaCell = oHMC_Vars(iID)%a2dAreaCell
        a2dVarCtWP = oHMC_Vars(iID)%a2dCtWP
        
        ! Lake(s) and dam(s) variable(s)
        a2iVarXYDam = oHMC_Vars(iID)%a2iXYDam
        a1dVarCodeDam = oHMC_Vars(iID)%a1dCodeDam 
        a1dVarVDam = oHMC_Vars(iID)%a1dVDam
        a2iVarXYLake = oHMC_Vars(iID)%a2iXYLake
        a1dVarCodeLake = oHMC_Vars(iID)%a1dCodeLake 
        a1dVarVLake = oHMC_Vars(iID)%a1dVLake
        
        ! Extracting dynamic variable(s)
        a2dVarVRet = oHMC_Vars(iID)%a2dVRet

        ! Extracting dynamic state variable(s)
        a2dVarVTot = oHMC_Vars(iID)%a2dVTot         ! Total soil volume
        a2dVarET = oHMC_Vars(iID)%a2dET             ! Evapotranspiration ==> from LST phys or forcing dataset
        a2dVarETPot = oHMC_Vars(iID)%a2dETPot       ! Potential Evapotranspiration == from LST phys or forcing dataset

        ! Compute soil water content at wilting point - minimum threshold for evapotranspiration
        a2dVarVTotWP = oHMC_Vars(iID)%a2dVTotWP
        
        ! Extracting checking variable(s)
        dVarETTot = oHMC_Vars(iID)%dVarETTot
        
        ! Info start
        call mprintf(.true., iINFO_Verbose, ' Phys :: Evapotranspiration ... ' )
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Debug
        if (iDEBUG.gt.0) then
            call mprintf(.true., iINFO_Extra, ' ========= EVAPOTRANSPIRATION START =========== ')  
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarET, a2iVarMask, 'ET START ') )   
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarAE, a2iVarMask, 'AE START ') )
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarETPot, a2iVarMask, 'ET.POT START ') )
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarVTot, a2iVarMask, 'VTOT START ') )
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarVRet, a2iVarMask, 'VRET START ') )
            call mprintf(.true., iINFO_Extra, '') 
        endif
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Calculating control variable(s) 
        dVarAE = sum(a2dVarET, mask=a2dVarDem.gt.0.0)/max(1,count(a2dVarDem.gt.0.0))
        dVarET = sum(a2dVarET, mask=a2dVarDem.gt.0.0)/max(1,count(a2dVarDem.gt.0.0))
        dVarETPot = sum(a2dVarETPot, mask=a2dVarDem.gt.0.0)/max(1,count(a2dVarDem.gt.0.0))
        dVarETTot = dVarETTot + dVarAE

        ! ET information time step
        write(sVarAE, sFMTVarET) dVarAE
        write(sVarETPot, sFMTVarET) dVarETPot
        write(sVarET, sFMTVarET) dVarET
        write(sVarETTot, sFMTVarET) dVarETTot
        call mprintf(.true., iINFO_Basic, ' Phys :: EVT START :: AvgValue :: '// &
                                          ' AEvt: '//sVarAE//' [mm] '// &
                                          ' PEvt: '//sVarETPot//' [mm] '// &
                                          ' Evt: '//sVarET//' [mm] '// &
                                          ' Evt Tot: '//sVarETTot//' [mm]')
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Lake and dam ET updating
        
        ! Dam updating
        if (iNDam .gt. 0) then
            
            ! Cycle on dam(s)
            do iD = 1, iNDam
                iI = 0; iJ = 0;
                iI = a2iVarXYDam(iD, 2)
		iJ = a2iVarXYDam(iD, 1)
                
                ! Compute dam volume
                if (a1dVarCodeDam(iD) .gt. 0) then
                    ! Distributed dam-lake
                    dVarETLake = 0.0
                    dVarETLake = sum(sum( a2dVarETPot, DIM = 1, mask=a2iVarChoice .eq. a1dVarCodeDam(iD)))
                    ! Update dam volume
                    a1dVarVDam(iD) = a1dVarVDam(iD) - dVarETLake/1000*a2dVarAreaCell(iI, iJ) !in m^3
                endif
                
                if (a1dVarVDam(iD) .lt. 0.0) a1dVarVDam(iD) = 0.0
                dVarETLake = 0.0 
                
            enddo
        endif
           
        ! Lake updating
        if (iNLake .gt. 0 ) then
            
            ! Cycle on lake(s)
            do iL = 1, iNLake
            
                iI = 0; iJ = 0;
                iI = a2iVarXYLake(iL,2)
                iJ = a2iVarXYLake(iL,1)
                
                ! Compute lake volume
                if (a1dVarCodeLake(iL) .gt. 0) then
                    ! Distributed lake
                    dVarETLake = 0.0
                    dVarETLake = sum(sum( a2dVarETPot, DIM = 1, mask=a2iVarChoice .eq. a1dVarCodeLake(iL)))
                    ! Update dam volume
                    a1dVarVLake(iL) = a1dVarVLake(iL) - dVarETLake/1000*a2dVarAreaCell(iI, iJ) !in m^3
                endif
                
                if (a1dVarVLake(iL) .lt. 0.0) a1dVarVLake(iL) = 0.0
                dVarETLake = 0.0 
                
            enddo
        endif
        !------------------------------------------------------------------------------------------
       
        !------------------------------------------------------------------------------------------
        ! Passing evapotranspiration (ET) to Actual evapotranspiration (AE)
        where (a2dVarDEM.gt.0.0)
            a2dVarAE = a2dVarET
        endwhere
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Calculating retention volume 
        where( (a2dVarVRet.gt.0.0) .and. (a2dVarVRet.ge.a2dVarETPot) .and.  &
               (a2dVarDEM.gt.0.0) .and. (a2iVarChoice.le.1) )
               
            a2dVarVRet = a2dVarVRet - a2dVarETPot
            a2dVarAE = a2dVarETPot
            
        elsewhere( (a2dVarVRet.gt.0.0) .and. (a2dVarVRet.lt.a2dVarETPot) .and. &
                (a2dVarDEM.gt.0.0) .and. (a2iVarChoice.le.1) )
            
            ! Compute residual evapotranspiration demand
            a2dVarAEres = a2dVarETPot - a2dVarVRet
            
            where( a2dVarAEres .lt. a2dVarAE)
                a2dVarAE = a2dVarAEres
            endwhere
            
            where( a2dVarAE .lt. 0.0)
                a2dVarAE = 0.0
            endwhere
            
            where( (a2dVarVTot - a2dVarVTotWP) .gt. a2dVarAE )
                a2dVarVTot = a2dVarVTot - a2dVarAE
                a2dVarAE = a2dVarVRet + a2dVarAE
            elsewhere ( (a2dVarVTot - a2dVarVTotWP) .le. a2dVarAE .and. (a2dVarVTot - a2dVarVTotWP) .gt. 0.0 ) 
                a2dVarAE = a2dVarVRet + (a2dVarVTot - a2dVarVTotWP)
                a2dVarVTot = a2dVarVTotWP
                
            elsewhere ( (a2dVarVTot-a2dVarVTotWP) .le. 0.0 ) ! to account also for VTot<Vwp situations
                a2dVarAE = 0.0
            endwhere
            a2dVarVRet = 0.0       
            
        elsewhere (a2dVarDEM.gt.0.0 .and. a2iVarChoice.le.1) ! Retention == 0.0 not on lakes
            
            where( a2dVarAE .lt. 0.0)
                a2dVarAE = 0.0
            endwhere
            
            where ((a2dVarVTot - a2dVarVTotWP).gt.a2dVarAE)
                ! tolgo evt da a2dV solo quando "non piove" cio� a2dRetention=0 
                ! quando piove l'evaporazione dal suolo � trascurabile
                a2dVarVTot = a2dVarVTot - a2dVarAE
            elsewhere ((a2dVarVTot-a2dVarVTotWP).le.a2dVarAE .and. (a2dVarVTot-a2dVarVTotWP) .gt. 0.0 )
                a2dVarAE = a2dVarVTot - a2dVarVTotWP
                a2dVarVTot = a2dVarVTotWP
            elsewhere ( (a2dVarVTot-a2dVarVTotWP) .le. 0.0 ) ! to account also for VTot<Vwp situations
                a2dVarAE = 0.0
            endwhere
            
        endwhere
        !------------------------------------------------------------------------------------------

        !------------------------------------------------------------------------------------------
        ! Calculating control variable(s) 
        dVarAE = sum(a2dVarAE, mask=a2dVarDem.gt.0.0)/max(1,count(a2dVarDem.gt.0.0))
        dVarET = sum(a2dVarET, mask=a2dVarDem.gt.0.0)/max(1,count(a2dVarDem.gt.0.0))
        dVarETPot = sum(a2dVarETPot, mask=a2dVarDem.gt.0.0)/max(1,count(a2dVarDem.gt.0.0))
        dVarETTot = dVarETTot + dVarAE

        ! ET information time step
        write(sVarAE, sFMTVarET) dVarAE
        write(sVarETPot, sFMTVarET) dVarETPot
        write(sVarET, sFMTVarET) dVarET
        write(sVarETTot, sFMTVarET) dVarETTot
        call mprintf(.true., iINFO_Basic, ' Phys :: EVT END :: AvgValue :: '// &
                                          ' AEvt: '//sVarAE//' [mm] '// &
                                          ' PEvt: '//sVarETPot//' [mm] '// &
                                          ' Evt: '//sVarET//' [mm] '// &
                                          ' Evt Tot: '//sVarETTot//' [mm]')
        !------------------------------------------------------------------------------------------
                                          
        !------------------------------------------------------------------------------------------
        ! Debug
        if (iDEBUG.gt.0) then
            call mprintf(.true., iINFO_Extra, '') 
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarET, a2iVarMask, 'ET END ') )
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarAE, a2iVarMask, 'AE END ') )
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarETPot, a2iVarMask, 'ET.POT END ') )
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarVTot, a2iVarMask, 'VTOT END ') )
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarVRet, a2iVarMask, 'VRET END ') )
            call mprintf(.true., iINFO_Extra, ' ========= EVAPOTRANSPIRATION END =========== ')  
        endif
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Copy instantaneous AE & AEpot to 3D var
        ! AE - 3D
        if (all(oHMC_Vars(iID)%a3dAE.lt.0.0))then
            oHMC_Vars(iID)%a3dAE(:,:,int(iDaySteps)) =  a2dVarAE
        else
            ! Re-initializing 
            do iStep=2, int(iDaySteps)
                oHMC_Vars(iID)%a3dAE(:,:,int(iStep-1)) = oHMC_Vars(iID)%a3dAE(:,:,int(iStep))
            enddo
            ! Updating with new field
            where(oHMC_Vars(iID)%a2dDEM.gt.0.0)
                oHMC_Vars(iID)%a3dAE(:,:,int(iDaySteps)) =  a2dVarAE
            elsewhere
                oHMC_Vars(iID)%a3dAE(:,:,int(iDaySteps)) = -9999.0
            endwhere
        endif
        
        ! AEpot - 3D
        if (all(oHMC_Vars(iID)%a3dAEpot.lt.0.0))then
            oHMC_Vars(iID)%a3dAEpot(:,:,int(iDaySteps)) =  a2dVarETPot
        else
            ! Re-initializing 
            do iStep=2, int(iDaySteps)
                oHMC_Vars(iID)%a3dAEpot(:,:,int(iStep-1)) = oHMC_Vars(iID)%a3dAEpot(:,:,int(iStep))
            enddo
            ! Updating with new field
            where(oHMC_Vars(iID)%a2dDEM.gt.0.0)
                oHMC_Vars(iID)%a3dAEpot(:,:,int(iDaySteps)) =  a2dVarETPot
            elsewhere
                oHMC_Vars(iID)%a3dAEpot(:,:,int(iDaySteps)) = -9999.0
            endwhere
        endif

        ! Passing variable(s) to global declaration
        oHMC_Vars(iID)%a2dAE = a2dVarAE
        oHMC_Vars(iID)%a2dAEPot = a2dVarETPot
        oHMC_Vars(iID)%a2dVRet = a2dVarVRet
        oHMC_Vars(iID)%a1dVLake = a1dVarVLake
        oHMC_Vars(iID)%a1dVDam = a1dVarVDam
        
        ! Updating state variable(s): ET and Total Volume
        oHMC_Vars(iID)%a2dET = 0.0              ! Re-initializing ET
        oHMC_Vars(iID)%a2dETPot = 0.0           ! Re-initializing ET pot
        oHMC_Vars(iID)%a2dVTot = a2dVarVTot     ! Updating total volume  

        oHMC_Vars(iID)%dVarETTot = dVarETTot    ! Check mean cumulated ET 
        
        ! Info end
        call mprintf(.true., iINFO_Verbose, ' Phys :: Evapotranspiration ... OK ' )
        !------------------------------------------------------------------------------------------
    
    end subroutine HMC_Phys_ET_Cpl
    !------------------------------------------------------------------------------------------

end module HMC_Module_Phys_ET
!------------------------------------------------------------------------------------------

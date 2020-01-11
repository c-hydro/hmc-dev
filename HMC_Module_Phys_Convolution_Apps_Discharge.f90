!------------------------------------------------------------------------------------
! File:   HMC_Module_Phys_Convolution_Apps_Discharge.f90
!
! Author(s):    Fabio Delogu, Francesco Silvestro, Simone Gabellani
! Date:         20190410
!
! Convolution Apps Discharge subroutine(s) for HMC model
!------------------------------------------------------------------------------------

!------------------------------------------------------------------------------------
! Module Header
module HMC_Module_Phys_Convolution_Apps_Discharge

    !------------------------------------------------------------------------------------
    ! External module(s) 
    use HMC_Module_Namelist,                only: oHMC_Namelist
    use HMC_Module_Vars_Loader,             only: oHMC_Vars
    
    use HMC_Module_Tools_Debug
    
    use HMC_Module_Phys_HydraulicStructure, only: HMC_Phys_Dam_Spilling, &
                                                  HMC_Phys_Dam_Discharge, &
                                                  HMC_Phys_Lake_Tank
                                                  
    use HMC_Module_Tools_Generic,           only: getIntValue, getIntRange
    
    ! Implicit none for all subroutines in this module
    implicit none
    !------------------------------------------------------------------------------------------

contains 

    !------------------------------------------------------------------------------------
    ! Subroutine for calculating discharge channel network type
    subroutine HMC_Phys_Convolution_Apps_Discharge_ChannelNetwork(iID, iRows, iCols, iNSection, &
                                                   dDtDataForcing, dDtAct, iNTime, iTq, dDtMax)
        
        !------------------------------------------------------------------------------------------
        ! Variable(s) declaration
        integer(kind = 4)   :: iID, iRows, iCols, iTAct
        real(kind = 4)      :: dDtDataForcing, dDtMax, dDtAct, dDtDischarge
        
        integer(kind = 4), intent(inout)    :: iNTime, iTq  
        integer(kind = 4)                   :: iI, iJ, iT, iS, iD
        integer(kind = 4)                   :: iNSection
        
        !integer(kind = 4)   :: iFlagVarUc
        real(kind = 4)      :: dRate
        
        !real(kind = 4), dimension (iRows, iCols)          :: a2dVarHydroPrev, a2dVarHydro, a2dVarIntensity
        real(kind = 4), dimension (iRows, iCols)          :: a2dVarQDisOut, a2dVarQVolOut, a2dVarQTot, a2dVarQout

        real(kind = 4), dimension (iNSection)             :: a1dVarQoutSection
        
        character(len = 10)                               :: sVarQoutSectionFirst, sVarQoutSectionMax
        character(len = 10), parameter                    :: sFMTVarQoutSection = "(F10.2)"
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Variable(s) initialization 
        a2dVarQDisOut = 0.0; a2dVarQVolOut = 0.0; a2dVarQTot = 0.0; a2dVarQout = 0.0; 

        a1dVarQoutSection = 0.0; 
        
        ! Re-initializing Q distributed discharge matrix (in global memory)
        oHMC_Vars(iID)%a2dQout = 0.0;           ! Distributed discharge
        oHMC_Vars(iID)%a1dQoutSection = 0.0;    ! Section discharge OUT
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Flags
        !iFlagVarUc = oHMC_Namelist(iID)%iFlagVarUc

        ! Temporal model step
        iT = int(oHMC_Vars(iID)%iTime)
        
        ! Variable(s) time dependent from global declaration
        a2dVarQDisOut = oHMC_Vars(iID)%a2dQDisOut ! Qout
        a2dVarQVolOut = oHMC_Vars(iID)%a2dQVolOut ! Qtmp
        a2dVarQTot = oHMC_Vars(iID)%a2dQTot
        a2dVarQout = oHMC_Vars(iID)%a2dQout

        a1dVarQoutSection = oHMC_Vars(iID)%a1dQoutSection
        
        ! Info start
        if (iDEBUG.gt.0) then
            call mprintf(.true., iINFO_Extra, ' Phys :: Convolution :: Discharge ... ' )
        endif
        !------------------------------------------------------------------------------------------
       
        !------------------------------------------------------------------------------------------
        ! Debug
        if (iDEBUG.gt.0) then
            call mprintf(.true., iINFO_Extra, ' ========= DISCHARGE START =========== ') 
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarQDisOut, oHMC_Vars(iID)%a2iMask, 'QDIS OUT START (Qout)') )
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarQVolOut, oHMC_Vars(iID)%a2iMask, 'QVOL OUT START (Qtmp)') )
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarQTot, oHMC_Vars(iID)%a2iMask, 'QTOT START (Q)') ) 
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarQout, oHMC_Vars(iID)%a2iMask, 'QOUT START (Qmap)') )  
            call mprintf(.true., iINFO_Extra, checkvar(oHMC_Vars(iID)%a2dUc, oHMC_Vars(iID)%a2iMask, 'UC ') )
            call mprintf(.true., iINFO_Extra, ' ') 
        endif
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Integrating step (SurfaceFlow)
        dDtDischarge = dDtAct
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Discharge in channel cells [mm/s]
        where( (oHMC_Vars(iID)%a2iChoice.eq.1) .and. (oHMC_Vars(iID)%a2dDEM.gt.0.0) )
            a2dVarQTot = a2dVarQTot + a2dVarQVolOut
        endwhere

        ! Discharge in hills cells [mm/s]
        where( (oHMC_Vars(iID)%a2iChoice.eq.0) .and. (oHMC_Vars(iID)%a2dDEM.gt.0.0) )
            a2dVarQTot = a2dVarQTot + a2dVarQVolOut
        endwhere
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Compute discharge from mm/s to m^3/s (constant is in 1/h)
        if( (real(iTq)*dDtDischarge) .ge. (dDtDataForcing - dDtMax*1.001) ) then ! 1.001 for numerical approx
            
            !------------------------------------------------------------------------------------------
            ! Compute distributed discharge (total and for each step)
            where( oHMC_Vars(iID)%a2dDEM.gt.0.0 )
                a2dVarQTot = a2dVarQTot/(real(iTq)*1000)*oHMC_Vars(iID)%a2dAreaCell
                a2dVarQout = a2dVarQTot                                
            endwhere
            !------------------------------------------------------------------------------------------
            
            !------------------------------------------------------------------------------------------
            ! Calculating discharge in selected outlet(s)
            a1dVarQoutSection = 0.0
            do iS = 1, iNSection
                
                ! Extracting outlet(s) indexes
                iI = 0; iJ = 0;
                iI = oHMC_Vars(iID)%a2iXYSection(iS, 2); iJ = oHMC_Vars(iID)%a2iXYSection(iS, 1)
                
                ! Get section discharge
                a1dVarQoutSection(iS) =  a2dVarQTot(iI, iJ)
               
                ! Subsurface flux
                dRate = sin(oHMC_Vars(iID)%a2dBeta(iI, iJ))
                
                if (dRate.gt.1) dRate = 0.99
                if (dRate.lt.0) dRate = 0.1
                
                if (oHMC_Vars(iID)%a2dWTableMax(iI,iJ).eq.0.0) dRate = 1.0 !Celle dove non ho falda
        
            enddo
                        
            ! Re-initialize qtot array and q counter
            !write(*,*) a1dVarQoutSection; write(*,*) a1dVarQinDam
            a2dVarQTot = 0.0 
            iTq = 0
            !------------------------------------------------------------------------------------------
            
            !------------------------------------------------------------------------------------------
            ! Discharge information time step
            write(sVarQoutSectionFirst, sFMTVarQoutSection) a1dVarQoutSection(1)
            write(sVarQoutSectionMax, sFMTVarQoutSection) maxval(a1dVarQoutSection)
            call mprintf(.true., iINFO_Basic, ' Phys :: Convolution :: Discharge ::'// &
                                  ' Q SecFirst: '//sVarQoutSectionFirst//' [m^3/s]'// &
                                  ' Q SecMax: '//sVarQoutSectionMax//' [m^3/s]')
            !------------------------------------------------------------------------------------------
                    
        endif
        !------------------------------------------------------------------------------------------
               
        !------------------------------------------------------------------------------------------
        ! Debug
        if (iDEBUG.gt.0) then
            call mprintf(.true., iINFO_Extra, ' ') 
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarQDisOut, oHMC_Vars(iID)%a2iMask, 'QDIS OUT END (Qout)') )
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarQVolOut, oHMC_Vars(iID)%a2iMask, 'QVOL OUT END (Qtmp)') )
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarQTot, oHMC_Vars(iID)%a2iMask, 'QTOT END (Q)') )  
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarQout, oHMC_Vars(iID)%a2iMask, 'QOUT END (Qmap)') )  
            call mprintf(.true., iINFO_Extra, ' ========= DISCHARGE END =========== ') 
        endif
        !------------------------------------------------------------------------------------------
     
        !------------------------------------------------------------------------------------------
        ! Updating model global variable(s)
        oHMC_Vars(iID)%a2dQTot = a2dVarQTot    
        oHMC_Vars(iID)%a2dQout = a2dVarQout   ! Distributed discharge
        oHMC_Vars(iID)%a1dQoutSection = a1dVarQoutSection   ! Section discharge OUT
        
        ! Info end
        if (iDEBUG.gt.0) then
            call mprintf(.true., iINFO_Extra, ' Phys :: Convolution :: Discharge ... OK' )
        endif
        !------------------------------------------------------------------------------------------
        
    end subroutine HMC_Phys_Convolution_Apps_Discharge_ChannelNetwork
    !------------------------------------------------------------------------------------------
    
     !------------------------------------------------------------------------------------
    ! Subroutine for calculating discharge channel fraction type
    subroutine HMC_Phys_Convolution_Apps_Discharge_ChannelFraction(iID, iRows, iCols, iNSection, &
                                                   dDtDataForcing, dDtAct, iNTime, iTq, dDtMax)
        
        !------------------------------------------------------------------------------------------
        ! Variable(s) declaration
        integer(kind = 4)   :: iID, iRows, iCols, iTAct
        real(kind = 4)      :: dDtDataForcing, dDtMax, dDtAct, dDtDischarge
        
        integer(kind = 4), intent(inout)    :: iNTime, iTq  
        integer(kind = 4)                   :: iI, iJ, iT, iS, iD
        integer(kind = 4)                   :: iNSection
        
        !integer(kind = 4)   :: iFlagVarUc
        real(kind = 4)      :: dRate
        
        !real(kind = 4), dimension (iRows, iCols)          :: a2dVarHydroPrev, a2dVarHydro, a2dVarIntensity
        real(kind = 4), dimension (iRows, iCols)          :: a2dVarQDisOut, a2dVarQVolOut, a2dVarQTot, a2dVarQout

        real(kind = 4), dimension (iNSection)             :: a1dVarQoutSection
        
        character(len = 10)                               :: sVarQoutSectionFirst, sVarQoutSectionMax
        character(len = 10), parameter                    :: sFMTVarQoutSection = "(F10.2)"
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Variable(s) initialization 
        a2dVarQDisOut = 0.0; a2dVarQVolOut = 0.0; a2dVarQTot = 0.0; a2dVarQout = 0.0; 

        a1dVarQoutSection = 0.0; 
        
        ! Re-initializing Q distributed discharge matrix (in global memory)
        oHMC_Vars(iID)%a2dQout = 0.0;           ! Distributed discharge
        oHMC_Vars(iID)%a1dQoutSection = 0.0;    ! Section discharge OUT
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Flags
        !iFlagVarUc = oHMC_Namelist(iID)%iFlagVarUc

        ! Temporal model step
        iT = int(oHMC_Vars(iID)%iTime)
        
        ! Variable(s) time dependent from global declaration
        a2dVarQDisOut = oHMC_Vars(iID)%a2dQC ! Q channel 2G (Forse non serve)
        a2dVarQVolOut = oHMC_Vars(iID)%a2dQC ! Q channel 2G
        a2dVarQTot = oHMC_Vars(iID)%a2dQTot
        a2dVarQout = oHMC_Vars(iID)%a2dQout

        a1dVarQoutSection = oHMC_Vars(iID)%a1dQoutSection
        
        ! Info start
        if (iDEBUG.gt.0) then
            call mprintf(.true., iINFO_Extra, ' Phys :: Convolution :: Discharge ... ' )
        endif
        !------------------------------------------------------------------------------------------
       
        !------------------------------------------------------------------------------------------
        ! Debug
        if (iDEBUG.gt.0) then
            call mprintf(.true., iINFO_Extra, ' ========= DISCHARGE START =========== ') 
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarQDisOut, oHMC_Vars(iID)%a2iMask, 'QDIS OUT START (Qout)') )
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarQVolOut, oHMC_Vars(iID)%a2iMask, 'QVOL OUT START (Qtmp)') )
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarQTot, oHMC_Vars(iID)%a2iMask, 'QTOT START (Q)') ) 
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarQout, oHMC_Vars(iID)%a2iMask, 'QOUT START (Qmap)') )  
            call mprintf(.true., iINFO_Extra, checkvar(oHMC_Vars(iID)%a2dUc, oHMC_Vars(iID)%a2iMask, 'UC ') )
            call mprintf(.true., iINFO_Extra, ' ') 
        endif
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Integrating step (SurfaceFlow)
        dDtDischarge = dDtAct
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Discharge in channel part of cells [m^3/s]
        where( (oHMC_Vars(iID)%a2iChoice.le.1) .and. (oHMC_Vars(iID)%a2dDEM.gt.0.0) )
            a2dVarQTot = a2dVarQTot + a2dVarQVolOut
        endwhere

        
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Compute discharge from mm/s to m^3/s (constant is in 1/h)
        if( (real(iTq)*dDtDischarge) .ge. (dDtDataForcing - dDtMax*1.001) ) then ! 1.001 for numerical approx
            
            !------------------------------------------------------------------------------------------
            ! Compute distributed discharge (total and for each step)
            where( oHMC_Vars(iID)%a2dDEM.gt.0.0 )
                a2dVarQTot = a2dVarQTot/(real(iTq))
                a2dVarQout = a2dVarQTot                                
            endwhere
            !------------------------------------------------------------------------------------------
            
            !------------------------------------------------------------------------------------------
            ! Calculating discharge in selected outlet(s)
            a1dVarQoutSection = 0.0
            do iS = 1, iNSection
                
                ! Extracting outlet(s) indexes
                iI = 0; iJ = 0;
                iI = oHMC_Vars(iID)%a2iXYSection(iS, 2); iJ = oHMC_Vars(iID)%a2iXYSection(iS, 1)
                
                ! Get section discharge
                a1dVarQoutSection(iS) =  a2dVarQTot(iI, iJ)
                !a1dVarQoutSection(iS) =  oHMC_Vars(iID)%a2dQC(iI, iJ)
                ! Subsurface flux
                dRate = sin(oHMC_Vars(iID)%a2dBeta(iI, iJ))
                
                if (dRate.gt.1) dRate = 0.99
                if (dRate.lt.0) dRate = 0.1
                
                if (oHMC_Vars(iID)%a2dWTableMax(iI,iJ).eq.0.0) dRate = 1.0 !Celle dove non ho falda
        
            enddo
                        
            ! Re-initialize qtot array and q counter
            !write(*,*) a1dVarQoutSection; write(*,*) a1dVarQinDam
            a2dVarQTot = 0.0 
            iTq = 0
            !------------------------------------------------------------------------------------------
            
            !------------------------------------------------------------------------------------------
            ! Discharge information time step
            write(sVarQoutSectionFirst, sFMTVarQoutSection) a1dVarQoutSection(1)
            write(sVarQoutSectionMax, sFMTVarQoutSection) maxval(a1dVarQoutSection)
            call mprintf(.true., iINFO_Basic, ' Phys :: Convolution :: Discharge ::'// &
                                  ' Q SecFirst: '//sVarQoutSectionFirst//' [m^3/s]'// &
                                  ' Q SecMax: '//sVarQoutSectionMax//' [m^3/s]')
            !------------------------------------------------------------------------------------------
                    
        endif
        !------------------------------------------------------------------------------------------
               
        !------------------------------------------------------------------------------------------
        ! Debug
        if (iDEBUG.gt.0) then
            call mprintf(.true., iINFO_Extra, ' ') 
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarQDisOut, oHMC_Vars(iID)%a2iMask, 'QDIS OUT END (Qout)') )
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarQVolOut, oHMC_Vars(iID)%a2iMask, 'QVOL OUT END (Qtmp)') )
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarQTot, oHMC_Vars(iID)%a2iMask, 'QTOT END (Q)') )  
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarQout, oHMC_Vars(iID)%a2iMask, 'QOUT END (Qmap)') )  
            call mprintf(.true., iINFO_Extra, ' ========= DISCHARGE END =========== ') 
        endif
        !------------------------------------------------------------------------------------------
     
        !------------------------------------------------------------------------------------------
        ! Updating model global variable(s)
        oHMC_Vars(iID)%a2dQTot = a2dVarQTot    
        oHMC_Vars(iID)%a2dQout = a2dVarQout   ! Distributed discharge
        oHMC_Vars(iID)%a1dQoutSection = a1dVarQoutSection   ! Section discharge OUT
        
        ! Info end
        if (iDEBUG.gt.0) then
            call mprintf(.true., iINFO_Extra, ' Phys :: Convolution :: Discharge ... OK' )
        endif
        !------------------------------------------------------------------------------------------
        
    end subroutine HMC_Phys_Convolution_Apps_Discharge_ChannelFraction
    !------------------------------------------------------------------------------------------
        
        
end module HMC_Module_Phys_Convolution_Apps_Discharge
!------------------------------------------------------------------------------------------
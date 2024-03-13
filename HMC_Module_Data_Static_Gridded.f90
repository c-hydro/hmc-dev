!------------------------------------------------------------------------------------------    
! File:   HMC_Module_Data_Static_Gridded.f90
! Author(s): Fabio Delogu, Francesco Silvestro, Simone Gabellani
!
! Created on April 30, 2015, 9:45 AM
!------------------------------------------------------------------------------------------

!------------------------------------------------------------------------------------
! Module Header
module HMC_Module_Data_Static_Gridded
    
    !------------------------------------------------------------------------------------
    ! External module(s) and implicit none
#ifdef LIB_NC
    use netcdf
#endif
    
    use HMC_Module_Namelist,        only:   oHMC_Namelist
    use HMC_Module_Vars_Loader,     only:   oHMC_Vars
    
    use HMC_Module_Tools_Debug
    
#ifdef LIB_NC
    use HMC_Module_Tools_IO,        only:   HMC_Tools_IO_Get1d_ASCII, &  
                                            HMC_Tools_IO_GetArcGrid_ASCII, &
                                            HMC_Tools_IO_Get1D_NC, &
                                            HMC_Tools_IO_Get2d_NC, &
                                            check
#else 
    use HMC_Module_Tools_IO,        only:   HMC_Tools_IO_Get1d_ASCII, &  
                                            HMC_Tools_IO_GetArcGrid_ASCII
#endif
                                                                             
    use HMC_Module_Tools_Generic,   only:   nullborder2DVar, check2Dvar

    implicit none
    !------------------------------------------------------------------------------------
    
contains

    !------------------------------------------------------------------------------------
    ! Subroutine to compute static data gridded
    subroutine HMC_Data_Static_Gridded_Cpl(iID, iRows, iCols)

        !------------------------------------------------------------------------------------
        ! Variable(s) declaration
        integer(kind = 4)       :: iID, iRows, iCols
        !------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------
        ! Info start
        call mprintf(.true., iINFO_Verbose, ' Data :: Static gridded ... ' )
        !------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------
        ! Calling subroutine to compute static data land
        call HMC_Data_Static_Gridded_Land(iID, iRows, iCols)
        !------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------
        ! Calling subroutine to compute extra derived model parameter(s)
        call HMC_Data_Static_Gridded_Params(iID, iRows, iCols)
        !------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------
        ! Calling subroutine to compute water-table
        call HMC_Data_Static_Gridded_WTable(iID, iRows, iCols)
        !------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------
        ! Calling subroutine to compute channel fraction 
        call HMC_Data_Static_Gridded_ChannelFraction(iID, iRows, iCols)
        !------------------------------------------------------------------------------------

        
        !------------------------------------------------------------------------------------
        ! Info end
        call mprintf(.true., iINFO_Verbose, ' Data :: Static gridded ... OK' )
        !------------------------------------------------------------------------------------
        
    end subroutine
    !------------------------------------------------------------------------------------
    
    !------------------------------------------------------------------------------------ 
    ! Subroutine to define model derived parameter(s)
    subroutine HMC_Data_Static_Gridded_Params(iID, iRows, iCols)
        
        !------------------------------------------------------------------------------------
        ! Variable(s)
        integer(kind = 4)       :: iID, iRows, iCols
        
        integer(kind = 4)       :: iFlagSnow
        real(kind = 4)          :: dBFMax, dBFMin, dCPI
        real(kind = 4)          :: dBc
        real(kind = 4)          :: dKb1, dKc1, dKb2, dKc2
        real(kind = 4)          :: dDxM, dDyM, dVarDEMMax
        
        integer(kind = 4), dimension (iRows, iCols)      :: a2iVarMask
        real(kind = 4), dimension (iRows, iCols)         :: a2dVarDEM, a2dVarS
        real(kind = 4), dimension (iRows, iCols)         :: a2dVarCt, a2dVarCf, a2dVarUc, a2dVarUh
        real(kind = 4), dimension (iRows, iCols)         :: a2dVarVTot
        
        real(kind = 4), dimension (iRows, iCols)         :: a2dVarExpRhoLow, a2dVarExpRhoHigh, a2dVarArctUp
        
        real(kind = 4), dimension (iRows, iCols)         :: a2dVarCtWP, a2dVarKb1, a2dVarKc1
        real(kind = 4), dimension (iRows, iCols)         :: a2dVarKb2, a2dVarKc2 
        !------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------
        ! Variable(s) initialization
        dCPI = 0.0; dBc = 0.0; dBFMax = 0.0; dBFMin = 0.0; dDxM = 0.0; dDyM = 0.0; dVarDEMMax = 0.0
        a2dVarDEM = 0.0; a2dVarS = 0.0; a2dVarVTot = 0.0; 
        a2dVarCt = 0.0; a2dVarCf = 0.0; a2dVarUh = 0.0; a2dVarUc = 0.0
        a2dVarExpRhoLow = 0.0; a2dVarExpRhoHigh = 0.0; a2dVarArctUp = 0.0; 
        !------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------
        ! Extracting information
        iFlagSnow = oHMC_Namelist(iID)%iFlagSnow
        !------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------ 
        ! Extracting parameters
        dCPI = oHMC_Namelist(iID)%dCPI

        dBc = oHMC_Namelist(iID)%dBc
        dBFMax = oHMC_Namelist(iID)%dBFMax
        dBFMin = oHMC_Namelist(iID)%dBFMin
        
        dDxM = oHMC_Vars(iID)%dDxM
        dDyM = oHMC_Vars(iID)%dDyM
        dVarDEMMax = oHMC_Vars(iID)%dDEMMax
        
        a2dVarDEM = oHMC_Vars(iID)%a2dDem
        a2dVarS = oHMC_Vars(iID)%a2dS
        a2dVarVTot = oHMC_Vars(iID)%a2dVTot
        a2iVarMask = oHMC_Vars(iID)%a2iMask
     
        a2dVarCt = oHMC_Vars(iID)%a2dCt
        a2dVarCtWP = oHMC_Vars(iID)%a2dCtWP

        a2dVarCf = oHMC_Vars(iID)%a2dCf
        a2dVarUh = oHMC_Vars(iID)%a2dUh
        a2dVarUc = oHMC_Vars(iID)%a2dUc
        
        ! Info start
        call mprintf(.true., iINFO_Verbose, ' Data :: Static gridded :: Get parameter(s) information ... ' )
        !------------------------------------------------------------------------------------ 
        
        !------------------------------------------------------------------------------------ 
        ! Debug
        if (iDEBUG.gt.0) then
            call mprintf(.true., iINFO_Extra, ' ========= STATIC GRIDDED PARAMS BEGIN =========== ')   
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarVTot, a2iVarMask, 'VTOT ') )
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarS, a2iVarMask, 'S ') )
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarDEM, a2iVarMask, 'DEM ') )
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarUc, a2iVarMask, 'UC ') )
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarUh, a2iVarMask, 'UH ') )
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarCtWP, a2iVarMask, 'CTWP ') )
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarKb1, a2iVarMask, 'KB1 ') )
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarKc1, a2iVarMask, 'KC1 ') )
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarKb2, a2iVarMask, 'KB2 ') )
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarKc2, a2iVarMask, 'KC2 ') )
            call mprintf(.true., iINFO_Extra, ' ')
        endif
        !------------------------------------------------------------------------------------ 
        
        !------------------------------------------------------------------------------------ 
        ! Change units the surface flow parameters in the one used in the code
        where (a2iVarMask.gt.0.0 .and. a2dVarUh.lt.0.05)
            a2dVarUh = a2dVarUh*3600 !from 1/s to 1/h
            a2dVarUc = a2dVarUc*(3600*1000)/(sqrt(dDxM*dDyM)*1000**(dBc + 1)) !from m^0.5/s to 1/(h*mm^0.5)
        endwhere
        !------------------------------------------------------------------------------------ 
      
        !------------------------------------------------------------------------------------
        ! Total Volume initial conditions
        where( (a2iVarMask.gt.0.0) .and. (a2dVarS.gt.0.0) ) 
            a2dVarVTot = dCPI/2*a2dVarS + dCPI/2*a2dVarS*(dVarDEMMax - a2dVarDEM)/dVarDEMMax
        elsewhere
            a2dVarVTot = 0.0
        endwhere
        !------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------
        ! Define snow static variable(s)
        if (iFlagSnow.eq.1) then
            where( a2dVarDEM .le. oHMC_Namelist(iID)%a1dAltRange(1) )
                a2dVarExpRhoLow = oHMC_Namelist(iID)%a1dExpRhoLow(1)
                a2dVarExpRhoHigh = oHMC_Namelist(iID)%a1dExpRhoHigh(1)
                a2dVarArctUp  = oHMC_Namelist(iID)%a1dArctUp(1)
            endwhere 
            where( (a2dVarDEM .gt. oHMC_Namelist(iID)%a1dAltRange(1)) .and. &
                   (a2dVarDEM .le. oHMC_Namelist(iID)%a1dAltRange(2)) )
                a2dVarExpRhoLow = oHMC_Namelist(iID)%a1dExpRhoLow(2)
                a2dVarExpRhoHigh = oHMC_Namelist(iID)%a1dExpRhoHigh(2)
                a2dVarArctUp  = oHMC_Namelist(iID)%a1dArctUp(2)
            endwhere
            where( (a2dVarDEM .gt. oHMC_Namelist(iID)%a1dAltRange(2)) .and. &
                   (a2dVarDEM .le. oHMC_Namelist(iID)%a1dAltRange(3)) )
                a2dVarExpRhoLow = oHMC_Namelist(iID)%a1dExpRhoLow(3)
                a2dVarExpRhoHigh = oHMC_Namelist(iID)%a1dExpRhoHigh(3)
                a2dVarArctUp  = oHMC_Namelist(iID)%a1dArctUp(3)
            endwhere
            where( a2dVarDem .gt. oHMC_Namelist(iID)%a1dAltRange(4))
                a2dVarExpRhoLow = oHMC_Namelist(iID)%a1dExpRhoLow(4)
                a2dVarExpRhoHigh = oHMC_Namelist(iID)%a1dExpRhoHigh(4)
                a2dVarArctUp  = oHMC_Namelist(iID)%a1dArctUp(4)
            endwhere
        else
            a2dVarExpRhoLow = -9999.0
            a2dVarExpRhoHigh = -9999.0
            a2dVarArctUp = -9999.0
        endif
        !------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------ 
        ! Define Beta Function parameters
        where(a2iVarMask.gt.0.0)
            
            ! Calculate parameters (straight line slopes)
            a2dVarKb1 = (dBFMax - dBFMin)/(a2dVarCt - a2dVarCtWP)    
            a2dVarKc1 = dBFMin - (dBFMax - dBFMin)/(a2dVarCt - a2dVarCtWP)*a2dVarCtWP

            a2dVarKb2 = (1 - dBFMax)/(1 - a2dVarCt)    
            a2dVarKc2 = 1 - a2dVarKb2
            
        elsewhere
            
            a2dVarCtWP = 0.0
            a2dVarKb1 = 0.0
            a2dVarKc1 = 0.0
            a2dVarKb2 = 0.0
            a2dVarKc2 = 0.0
            
        endwhere
        !------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------
        ! Debug
        if (iDEBUG.gt.0) then
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarVTot, a2iVarMask, 'VTOT ') )
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarUc, a2iVarMask, 'UC ') )
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarUh, a2iVarMask, 'UH ') )
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarCtWP, a2iVarMask, 'CTWP ') )
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarKb1, a2iVarMask, 'KB1 ') )
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarKc1, a2iVarMask, 'KC1 ') )
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarKb2, a2iVarMask, 'KB2 ') )
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarKc2, a2iVarMask, 'KC2 ') )
            call mprintf(.true., iINFO_Extra, ' ========= STATIC GRIDDED PARAMS END =========== ') 
        endif
        !------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------
        ! Passing parameter(s) to global declaration
        oHMC_Vars(iID)%a2dUc = a2dVarUc
        oHMC_Vars(iID)%a2dUh = a2dVarUh

        ! Energy balance parameter(s)
        oHMC_Vars(iID)%a2dCtWP = a2dVarCtWP
        oHMC_Vars(iID)%a2dKb1 = a2dVarKb1
        oHMC_Vars(iID)%a2dKc1 = a2dVarKc1
        oHMC_Vars(iID)%a2dKb2 = a2dVarKb2 
        oHMC_Vars(iID)%a2dKc2 = a2dVarKc2 
        
        ! Total volume (initialization)
        oHMC_Vars(iID)%a2dVTot = a2dVarVTot
        
        ! Snow static variable(s)
        oHMC_Vars(iID)%a2dExpRhoLow = a2dVarExpRhoLow; 
        oHMC_Vars(iID)%a2dExpRhoHigh = a2dVarExpRhoHigh; 
        oHMC_Vars(iID)%a2dArctUp = a2dVarArctUp; 

        ! Info end
        call mprintf(.true., iINFO_Verbose, ' Data :: Static gridded :: Get parameter(s) information ... OK' )
        !------------------------------------------------------------------------------------ 
  
    end subroutine HMC_Data_Static_Gridded_Params
    !------------------------------------------------------------------------------------ 
    
    !------------------------------------------------------------------------------------
    ! Subroutine for loading and initializing channel fraction parameter(s)
    subroutine HMC_Data_Static_Gridded_ChannelFraction(iID, iRows, iCols)
        
        !------------------------------------------------------------------------------------
        ! Variable(s)
        integer(kind = 4)       :: iID, iRows, iCols

        real(kind = 4)          :: dMaxW, dMinW
        real(kind = 4)          :: dMinW_Thr    ! Min fraction of cellsize devoted to Channel width
        real(kind = 4)          :: dMaxW_Thr    ! Max fraction of cellsize devoted to Channel width
        
        integer(kind = 4),  dimension (iRows, iCols)    :: a2iVarMask
        real(kind = 4),     dimension (iRows, iCols)    :: a2dVarSizeCell, a2dVarArea
        real(kind = 4),     dimension (iRows, iCols)    :: a2dVarWidthC, a2dVarWidthH
        !------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------
        ! Variable(s) initialization
        dMaxW = 0.0; dMinW = 0.0; ! [m]
        dMinW_Thr = 0.005  ! [-]
        dMaxW_Thr = 0.8   ! [-]
        a2iVarMask = 0;
        a2dVarWidthC = -9999.0; a2dVarWidthH = -9999.0
        a2dVarSizeCell = 0.0
        !------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------
        ! Info start
        call mprintf(.true., iINFO_Verbose, ' Data :: Static gridded :: Get channel fraction information ... ' )
        
        ! Check channel type
        if (oHMC_Namelist(iID)%iFlagCType .eq. 2) then
        
            !------------------------------------------------------------------------------------ 
            ! Extracting parameters
            a2dVarArea = float(oHMC_Vars(iID)%a2iArea)
!            a2dVarArea = oHMC_Vars(iID)%a2dArea

            a2dVarSizeCell = (oHMC_Vars(iID)%a2dAreaCell)**0.5

            a2dVarWidthC = oHMC_Vars(iID)%a2dWidthC
            a2dVarWidthH = oHMC_Vars(iID)%a2dWidthH
            
            a2iVarMask = oHMC_Vars(iID)%a2iMask 
            !------------------------------------------------------------------------------------

            !------------------------------------------------------------------------------------ 
            ! Debug
            if (iDEBUG.gt.0) then
                call mprintf(.true., iINFO_Extra, ' ========= STATIC GRIDDED CHANNEL FRACTION BEGIN =========== ')   
                call mprintf(.true., iINFO_Extra, checkvar(a2dVarWidthC, a2iVarMask, 'WIDTHC ') )
                call mprintf(.true., iINFO_Extra, checkvar(a2dVarWidthH, a2iVarMask, 'WIDTHH ') )
                call mprintf(.true., iINFO_Extra, ' ')
            endif
            !------------------------------------------------------------------------------------ 

            !------------------------------------------------------------------------------------
            ! Channel width
            dMaxW = MAXVAL(MAXVAL(a2dVarWidthC, dim=1, & 
                                  mask=oHMC_Vars(iID)%a2iChoice.ge.0 .and. a2iVarMask.gt.0))
            dMinW = MINVAL(MINVAL(a2dVarWidthC, dim=1, &
                                  mask=oHMC_Vars(iID)%a2iChoice.ge.0 .and. a2iVarMask.gt.0))
                                  
            ! Build channel width with area function
            where( (oHMC_Vars(iID)%a2iChoice.ge.0.0) .and. (a2iVarMask.gt.0.0) .and. (a2dVarWidthC.lt.0.0) )
                a2dVarWidthC = 0.005*(a2dVarArea*oHMC_Vars(iID)%a2dAreaCell/1000000)**0.4*1000 ! width in m                    
            endwhere
            
            ! Check max and min of witdhc
            dMaxW = MAXVAL(MAXVAL(a2dVarWidthC, dim=1, &
                                  mask=oHMC_Vars(iID)%a2iChoice.ge.0 .and. a2iVarMask.gt.0))
            dMinW = MINVAL(MINVAL(a2dVarWidthC, dim=1, &
                                  mask=oHMC_Vars(iID)%a2iChoice.ge.0 .and. a2iVarMask.gt.0))
            !------------------------------------------------------------------------------------
                                  
            !------------------------------------------------------------------------------------
            ! Verify maximum values for numerical stability
            where(a2dVarWidthC .ge. dMaxW_Thr*a2dVarSizeCell)
                a2dVarWidthC = dMaxW_Thr*a2dVarSizeCell
            endwhere
            where(a2dVarWidthC .lt. dMinW_Thr*a2dVarSizeCell)
                a2dVarWidthC = dMinW_Thr*a2dVarSizeCell
            endwhere
            ! Check max and min of witdhc
            dMaxW = MAXVAL(MAXVAL(a2dVarWidthC, dim=1, &
                                  mask=oHMC_Vars(iID)%a2iChoice.ge.0 .and. a2iVarMask.gt.0))
            dMinW = MINVAL(MINVAL(a2dVarWidthC, dim=1, &
                                  mask=oHMC_Vars(iID)%a2iChoice.ge.0 .and. a2iVarMask.gt.0))
            !------------------------------------------------------------------------------------
                                  
            !------------------------------------------------------------------------------------
            ! Hillslope width
            where(oHMC_Vars(iID)%a2iChoice.ge.0 .and. a2iVarMask.gt.0)
                a2dVarWidthH = a2dVarSizeCell - a2dVarWidthC
            endwhere
            !------------------------------------------------------------------------------------ 
            
            !------------------------------------------------------------------------------------ 
            ! Debug
            if (iDEBUG.gt.0) then
                call mprintf(.true., iINFO_Extra, ' ')
                call mprintf(.true., iINFO_Extra, checkvar(a2dVarWidthC, a2iVarMask, 'WIDTHC ') )
                call mprintf(.true., iINFO_Extra, checkvar(a2dVarWidthH, a2iVarMask, 'WIDTHH ') )
                call mprintf(.true., iINFO_Extra, ' ========= STATIC GRIDDED CHANNEL FRACTION END =========== ') 
            endif
            !------------------------------------------------------------------------------------ 

            !------------------------------------------------------------------------------------
            ! Passing parameter(s) to global declaration
            oHMC_Vars(iID)%a2dWidthC = a2dVarWidthC
            oHMC_Vars(iID)%a2dWidthH = a2dVarWidthH
                 
            ! Info end
            call mprintf(.true., iINFO_Verbose, ' Data :: Static gridded :: Get channel fraction information ... OK' )
            !------------------------------------------------------------------------------------
        
        else
            
            !------------------------------------------------------------------------------------
            ! Initialize with undefined values
            oHMC_Vars(iID)%a2dWidthC = -9999.0
            oHMC_Vars(iID)%a2dWidthH = -9999.0
            
            ! Info end
            call mprintf(.true., iINFO_Verbose, ' Data :: Static gridded :: Get channel fraction information ... SKIPPED' )
            !------------------------------------------------------------------------------------
            
        endif
        !------------------------------------------------------------------------------------
        
    end subroutine HMC_Data_Static_Gridded_ChannelFraction
    !------------------------------------------------------------------------------------
    
    !------------------------------------------------------------------------------------
    ! Subroutine for loading and initializing land variable(s)
    subroutine HMC_Data_Static_Gridded_Land(iID, iRows, iCols)
        
        !------------------------------------------------------------------------------------------
        ! Variable(s)
        integer(kind = 4)       :: iTypeData
        character(len = 256)    :: sDomainName, sPathData, sFileName
        
        integer(kind = 4)       :: iID, iI, iJ, iRows, iCols
        integer(kind = 4)       :: iPixCount
        
        integer(kind = 4)       :: iFileID, iErr
        
        integer(kind = 4)       :: iDomainPixels, iTc, iTcSeconds, iTcMax, iETime, iNTime
        integer(kind = 4)       :: iFlagCoeffRes
        integer(kind = 4)       :: iFlagWS, iFlagWDL, iFlagFrac
        integer(kind = 4)       :: iFlagCType, iFlagDynVeg, iFlagFlood
        real(kind = 4)          :: dDxM, dDyM, dDEMMax, dDEMMin, dDEMStepMean
        real(kind = 4)          :: dDomainArea
        real(kind = 4)          :: dDtDataForcing
        
        real(kind = 4)          :: dUc, dUh, dCt, dCf
        real(kind = 4)          :: dCN, dWS, dWDL, dFrac, dKSatRatio !giulia
        real(kind = 4)          :: dWTLossMax
        
        character(len = 256)    :: sVarName
        character(len = 256)    :: sVarUnits
        
        real(kind = 4),     dimension (100)             :: a1dVar 
        real(kind = 4),     dimension (iCols, iRows)    :: a2dVar 
        
        real(kind = 4), dimension (iRows, iCols)        :: a2dVarCon
        
        integer(kind = 4),  dimension (iRows, iCols)    :: a2iVarPNT, a2iVarMask, a2iVarChoice, a2iVarArea
        integer(kind = 4),  dimension (iRows, iCols)    :: a2iVarNature
         
        real(kind = 4),     dimension (iRows, iCols)    :: a2dVarLon, a2dVarLat 
        real(kind = 4),     dimension (iRows, iCols)    :: a2dVarDEM, a2dVarCN, a2dVarS, a2dVarVTotWP
        real(kind = 4),     dimension (iRows, iCols)    :: a2dVarAreaCell
        real(kind = 4),     dimension (iRows, iCols)    :: a2dVarAlpha, a2dVarBeta, a2dVarWTMax, a2dVarKSatRatio !giulia
        real(kind = 4),     dimension (iRows, iCols)    :: a2dVarWTksatH
        real(kind = 4),     dimension (iRows, iCols)    :: a2dVarC1, a2dVarF2 
        real(kind = 4),     dimension (iRows, iCols)    :: a2dVarCostF, a2dVarCostF1, a2dVarCostChFix
        real(kind = 4),     dimension (iRows, iCols)    :: a2dVarCt, a2dVarCtWP, a2dVarCf, a2dVarUc, a2dVarUh 
        real(kind = 4),     dimension (iRows, iCols)    :: a2dVarCoeffResol, a2dVarCoeffWS, a2dVarCoeffWDL
        real(kind = 4),     dimension (100)             :: a1dVarFCN
        
        !vegetation static parameters
        real(kind = 4),     dimension (iRows, iCols)    :: a2dVarGd, a2dVarRSmin, a2dVarHveg, a2dVarBareSoil

        !flooding static parameters
        real(kind = 4),     dimension (iRows, iCols)    :: a2dVarLevBankR, a2dVarLevBankL, a2dVarFirst
        
        real(kind = 4),     dimension (iRows, iCols)    :: a2dVarWidthC, a2dVarFrac
               
        logical                                         :: bFileExist
        
        character(len = 256)                            :: sStrDomPix, sStrDomArea, sStrTc, sStrDemMax
        character(len = 256)                            :: sPixCount, sParDefault
        !------------------------------------------------------------------------------------------

        !------------------------------------------------------------------------------------------
        ! Variable(s) initialization
        bFileExist = .false.
        sVarName = ''; sVarUnits = ''; iNTime = 0;
        sDomainName = ''; sPathData = ''; sFileName = '';
        iTypeData = 0; iI = 0; iJ = 0; iFileID = 0; iErr = 0; iDomainPixels = 0; iTc = 0; iTcMax = 0;
        dDxM = 0.0; dDyM = 0.0; dDEMMax = 0.0; dDEMMin = 0.0; dDEMStepMean = 0.0; dDomainArea = 0.0;
       
        a2dVarCon = 0.0; a1dVar = 0.0; a2dVar = 0.0
        
        a2iVarPNT = 0; a2iVarMask = 0; a2iVarChoice = 0; a2iVarArea = 0;
        a2dVarLon = 0.0; a2dVarLat = 0.0; 
        a2dVarDEM = 0.0; a2dVarCN = 0.0; a2dVarS = 0.0; a2dVarVTotWP = 0.0;
        a2dVarAreaCell = 0.0; 
        a2dVarAlpha = 0.0; a2dVarBeta = 0.0; a2dVarWTMax = -9999.0; a2dVarKSatRatio = 0.0; !giulia
        a2dVarWTksatH = 0.0
        a2dVarC1 = 0.0; a2dVarF2 = 0.0; 
        a2dVarCostF = 0.0; a2dVarCostF1 = 0.0; a2dVarCostChFix = 0.0;
        a2dVarCt = 0.0; a2dVarCf = 0.0; a2dVarUc = 0.0; a2dVarUh = 0.0;  
        a2dVarCoeffResol = 0.0; a2dVarCoeffWS = 0.0; a2dVarCoeffWDL = 0.0;
        a1dVarFCN = 0.0
        a2dVarFrac = 0.0; a2dVarWidthC = -9999.0;
        a2dVarCtWP = 0.0; a2dVarGd = 0.0; a2dVarRSmin = 0.0; a2dVarHveg = 0.0; a2dVarBareSoil = 0.0;
        a2dVarLevBankR = -9999.0; a2dVarLevBankL = -9999.0; a2dVarFirst = -9999.0;
        
        dUc = -9999.0; dUh = -9999.0; dCt = -9999.0; dCf = -9999.0; dCN = -9999.0; 
        dWS = -9999.0; dWDL = -9999.0; dFrac = -9999.0;
        dKSatRatio = -9999.0; !giulia
        
        iFlagCoeffRes = -9999; 
        iFlagWS = -9999; iFlagWDL = -9999; iFlagFrac = -9999; 
        iFlagCType = -9999; iFlagDynVeg = -9999; iFlagFlood = -9999;
        
        iPixCount = 0
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Get global info
        iNTime = oHMC_Namelist(iID)%iNTime
        sDomainName = oHMC_Namelist(iID)%sDomainName
        sPathData = oHMC_Namelist(iID)%sPathData_Static_Gridded
        iTypeData = oHMC_Namelist(iID)%iFlagTypeData_Static
        ! Get the corrivation time and the forcing dt
        iTcMax = oHMC_Namelist(iID)%iTcMax
        dDtDataForcing = dble(oHMC_Namelist(iID)%iDtData_Forcing)
        
        ! Get mean parameter(s)
        dCt = oHMC_Namelist(iID)%dCt
        dCf = oHMC_Namelist(iID)%dCf
        dUh = oHMC_Namelist(iID)%dUh
        dUc = oHMC_Namelist(iID)%dUc 
        dCN = oHMC_Namelist(iID)%dCN
        dWS = oHMC_Namelist(iID)%dWS
        dWDL = oHMC_Namelist(iID)%dWDL
        dFrac = oHMC_Namelist(iID)%dFrac
        dKSatRatio = oHMC_Namelist(iID)%dKSatRatio !giulia
        dWTLossMax = oHMC_Namelist(iID)%dWTLossMax
        
        ! Flag to set coeff resolution map default mode
        iFlagCoeffRes = oHMC_Namelist(iID)%iFlagCoeffRes
        ! Flag to activate/deactivate water sources map
        iFlagWS = oHMC_Namelist(iID)%iFlagWS
        ! Flag to activate/deactivate groundwater bedrock fracturation map
        iFlagFrac = oHMC_Namelist(iID)%iFlagFrac
        ! Flag to activate/deactivate channel fraction map
        iFlagCType = oHMC_Namelist(iID)%iFlagCType
        ! Flag to activate/deactivate dynamic vegetation for ET computation
        iFlagDynVeg = oHMC_Namelist(iID)%iFlagDynVeg
        !Flooding flag
        iFlagFlood = oHMC_Namelist(iID)%iFlagFlood  
        
        ! Get Initialized variable(s)
        a2dVarS = oHMC_Vars(iID)%a2dS
        
        ! Info start
        call mprintf(.true., iINFO_Verbose, ' Data :: Static gridded :: Get land information ... ' )
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Land data type in netCDF format
        if (iTypeData == 2) then
#ifdef LIB_NC
            !------------------------------------------------------------------------------------------
            ! Info
            call mprintf(.true., iINFO_Extra, ' Data static gridded in netCDF format.' )
            !------------------------------------------------------------------------------------------
            
            !------------------------------------------------------------------------------------------
            ! Filename nc
            sFileName = trim(sPathData)//'hmc.staticdata.nc'
            
            ! Check file availability
            inquire (file = trim(sFileName), exist = bFileExist)
            if ( .not. bFileExist ) then
                !------------------------------------------------------------------------------------------
                ! Exit code file not found
                call mprintf(.true., iERROR, ' No data static gridded file found: '//trim(sFileName) )
                !------------------------------------------------------------------------------------------
            else
                
                !------------------------------------------------------------------------------------------
                ! Open nc file
                call check( nf90_open(trim(sFileName), NF90_NOWRITE, iFileID) )
                !------------------------------------------------------------------------------------------
                
                !------------------------------------------------------------------------------------------
                ! Longitude
                sVarName = 'Longitude'
                call HMC_Tools_IO_Get2d_NC(sVarName, iFileID, a2dVar, sVarUnits, iCols, iRows, .true., iErr)
                a2dVarLon = transpose(a2dVar)
                !------------------------------------------------------------------------------------------
                
                !------------------------------------------------------------------------------------------
                ! Latitude
                sVarName = 'Latitude'
                call HMC_Tools_IO_Get2d_NC(sVarName, iFileID, a2dVar, sVarUnits, iCols, iRows, .true., iErr)
                a2dVarLat = transpose(a2dVar)
                !------------------------------------------------------------------------------------------
                
                !------------------------------------------------------------------------------------------
                ! DEM
                sVarName = 'Terrain'
                call HMC_Tools_IO_Get2d_NC(sVarName, iFileID, a2dVar, sVarUnits, iCols, iRows, .true., iErr)
                a2dVarDEM = transpose(a2dVar)
                !------------------------------------------------------------------------------------------
                
                !------------------------------------------------------------------------------------------
                ! PNT
                sVarName = 'Flow_Directions'
                call HMC_Tools_IO_Get2d_NC(sVarName, iFileID, a2dVar, sVarUnits, iCols, iRows, .true., iErr)
                a2iVarPNT = transpose(int(a2dVar))
                !------------------------------------------------------------------------------------------
                
                !------------------------------------------------------------------------------------------
                ! CHOICE
                sVarName = 'Channels_Distinction'
                call HMC_Tools_IO_Get2d_NC(sVarName, iFileID, a2dVar, sVarUnits, iCols, iRows, .true., iErr)
                a2iVarChoice = transpose(int(a2dVar))
                !------------------------------------------------------------------------------------------
                
                !------------------------------------------------------------------------------------------
                ! CN
                call HMC_Tools_IO_Get2d_NC(sVarName, iFileID, a2dVar, sVarUnits, iCols, iRows, .false., iErr)
                if (iErr /= 0) then
                    call mprintf(.true., iWARN, ' CN data not found. Initializing CN with default values.')
                    a2dVarCN = dCN
                else
                    a2dVarCN = transpose(a2dVar)
                endif
                !------------------------------------------------------------------------------------------
                
                !------------------------------------------------------------------------------------------
                ! Mask - GIULIA - qui ancora da aggiornare per seguire i nodata
                sVarName = 'Mask'
                call HMC_Tools_IO_Get2d_NC(sVarName, iFileID, a2dVar, sVarUnits, iCols, iRows, .false., iErr)
                
                if (iErr /= 0) then 
                    call mprintf(.true., iWARN, ' Mask data not found. Initializing Mask with default values.')
                    where(a2dVarDEM.gt.0.0)
                        a2iVarMask = 1 
                    elsewhere
                        a2iVarMask = 0
                    endwhere
                else
                    a2iVarMask = int(transpose(a2dVar))
                endif
                !------------------------------------------------------------------------------------------
                
                !------------------------------------------------------------------------------------------
                ! AREA
                sVarName = 'Drainage_Area'
                call HMC_Tools_IO_Get2d_NC(sVarName, iFileID, a2dVar, sVarUnits, iCols, iRows, .true., iErr)
                a2iVarArea = transpose(int(a2dVar))
!                a2dVarArea = transpose(a2dVar)

                !------------------------------------------------------------------------------------------
                
                !------------------------------------------------------------------------------------------
                ! CELLAREA
                sVarName = 'Cell_Area'
                call HMC_Tools_IO_Get2d_NC(sVarName, iFileID, a2dVar, sVarUnits, iCols, iRows, .true., iErr)
                a2dVarAreaCell = transpose(a2dVar)
                !------------------------------------------------------------------------------------------
                
                !------------------------------------------------------------------------------------------
                ! WATERTABLE ALPHA
                sVarName = 'Wt_Alpha'
                call HMC_Tools_IO_Get2d_NC(sVarName, iFileID, a2dVar, sVarUnits, iCols, iRows, .true., iErr)
                a2dVarAlpha = transpose(a2dVar) 

                ! WATERTABLE BETA
                sVarName = 'Wt_Beta'
                call HMC_Tools_IO_Get2d_NC(sVarName, iFileID, a2dVar, sVarUnits, iCols, iRows, .true., iErr)
                a2dVarBeta = transpose(a2dVar)

                ! WATERTABLE MAX
                sVarName = 'Wt_Max'
                call HMC_Tools_IO_Get2d_NC(sVarName, iFileID, a2dVar, sVarUnits, iCols, iRows, .false., iErr)
                if (iErr /= 0) then
                    call mprintf(.true., iWARN, 'WT maximum data not found. Initializing with default values.')   
                    a2dVarWTMax = -9999.0
                else
                    a2dVarWTMax = transpose(a2dVar)
                endif
                
                ! WATERTABLE KSATRATIO !giulia
                sVarName = 'KSatRatio'
                call HMC_Tools_IO_Get2d_NC(sVarName, iFileID, a2dVar, sVarUnits, iCols, iRows, .false., iErr)
                if (iErr /= 0) then
                    call mprintf(.true., iWARN, 'KSatRatio data not found. Initializing KSatRatio with scalar value from namelist.')   
                    a2dVarKSatRatio = dKSatRatio
                else
                    a2dVarKSatRatio = transpose(a2dVar)
                endif
                !------------------------------------------------------------------------------------------
                
                !------------------------------------------------------------------------------------------
                ! CT
                sVarName = 'Ct'
                call HMC_Tools_IO_Get2d_NC(sVarName, iFileID, a2dVar, sVarUnits, iCols, iRows, .false., iErr)
                a2dVarCt = transpose(a2dVar)
                !------------------------------------------------------------------------------------------
                
                !------------------------------------------------------------------------------------------
                ! CT_WP - soil moisture wilting point
                sVarName = 'Ct_wp'
                call HMC_Tools_IO_Get2d_NC(sVarName, iFileID, a2dVar, sVarUnits, iCols, iRows, .false., iErr)
                if (iErr /= 0) then
                    call mprintf(.true., iWARN, 'Soil Moisture Wilting Point data not found. Initializing with default values.')   
                    a2dVarCtWP = 0.4*a2dVarCt
                else
                    a2dVarCtWP = transpose(a2dVar)
                endif
                !------------------------------------------------------------------------------------------
                
                !------------------------------------------------------------------------------------------
                ! CF
                sVarName = 'Cf'
                call HMC_Tools_IO_Get2d_NC(sVarName, iFileID, a2dVar, sVarUnits, iCols, iRows, .false., iErr)
                a2dVarCf = transpose(a2dVar)
                !------------------------------------------------------------------------------------------
                
                !------------------------------------------------------------------------------------------
                ! UC
                sVarName = 'Uc'
                call HMC_Tools_IO_Get2d_NC(sVarName, iFileID, a2dVar, sVarUnits, iCols, iRows, .false., iErr)
                a2dVarUc = transpose(a2dVar)
                !------------------------------------------------------------------------------------------
                
                !------------------------------------------------------------------------------------------
                ! UH
                sVarName = 'Uh'
                call HMC_Tools_IO_Get2d_NC(sVarName, iFileID, a2dVar, sVarUnits, iCols, iRows, .false., iErr)
                a2dVarUh = transpose(a2dVar)
                !------------------------------------------------------------------------------------------
                                
                !------------------------------------------------------------------------------------------
                ! FCN
                sVarName = 'VegetationIA'
                call HMC_Tools_IO_Get1D_NC(sVarName, iFileID, a1dVar, sVarUnits, 100, .false., iErr)
                
                if (iErr /= 0) then
                    call mprintf(.true., iWARN, ' VegIA data not found. Initializing VegIA with default values.')
                    a1dVarFCN = (/603.3,572.9,543.8,515.9,489.2,463.6,439.2,415.9,393.6,372.3, &
                                352.0,332.7,314.3,296.8,280.2,264.4,249.4,235.2,221.8,209.1,197.1, &
                                185.8,175.1,165.1,155.6,146.8,138.4,130.7,123.4,116.6,110.3,104.4, &
                                98.9,93.8,89.1,84.7,80.7,77.0,73.7,70.5,67.7,65.1,62.8,60.6,58.7,56.9, &
                                55.3,53.9,52.6,51.4,50.4,49.4,48.6,47.8,47.1,46.4,45.9,45.3,44.8,44.3, &
                                43.8,43.3,42.8,42.3,41.8,41.2,40.7,40.1,39.5,38.8,38.1,37.3,36.5,35.7, &
                                34.7,33.8,32.8,31.7,30.5,29.4,28.1,26.8,25.5,24.1,22.6,21.2,19.7,18.1, &
                                16.5,14.9,13.3,11.7,10.0,8.4,6.8,5.2,3.6,2.1,0.6,0.1/)
                else
                    a1dVarFCN = a1dVar  
                endif
                !------------------------------------------------------------------------------------------
                
                !------------------------------------------------------------------------------------------
                ! Coefficient Resolution
                sVarName = 'Coeff_Resol_Map'
                call HMC_Tools_IO_Get2d_NC(sVarName, iFileID, a2dVar, sVarUnits, iCols, iRows, .false., iErr)
                if (iErr /= 0) then
                    call mprintf(.true., iWARN, ' CoeffRes data not found. Initializing CoeffRes with namelist flag condition.')
                    where(a2dVarDEM.gt.0.0)
                        a2dVarCoeffResol = iFlagCoeffRes
                    elsewhere
                        a2dVarCoeffResol = 0.0
                    endwhere
                else
                    a2dVarCoeffResol = transpose(a2dVar)
                endif
                !------------------------------------------------------------------------------------------
                
                !------------------------------------------------------------------------------------------
                ! Nature map
                sVarName = 'Nature'
                call HMC_Tools_IO_Get2d_NC(sVarName, iFileID, a2dVar, sVarUnits, iCols, iRows, .false., iErr)
                if (iErr /= 0) then
                    call mprintf(.true., iWARN, ' Nature data not found. Initializing Nature with default values.')
                    a2iVarNature = -9999
                else
                    a2iVarNature = int(transpose(a2dVar))
                endif
                !------------------------------------------------------------------------------------------
            
                !------------------------------------------------------------------------------------------
                ! Watertable sources map
                sVarName = 'WS'
                if (iFlagWS .eq. 1) then
                    call HMC_Tools_IO_Get2d_NC(sVarName, iFileID, a2dVar, sVarUnits, iCols, iRows, .false., iErr)
                    if (iErr /= 0) then
                        call mprintf(.true., iWARN, ' CoeffWS data not found. Initializing CoeffWS with default values.')
                        a2dVarCoeffWS = dWS
                    else
                        a2dVarCoeffWS = transpose(a2dVar)
                    endif
                else
                    call mprintf(.true., iWARN, ' Watertable sources not activated. CoeffWS is null.')
                    a2dVarCoeffWS = 0.0
                endif   
                !------------------------------------------------------------------------------------------
                
                !------------------------------------------------------------------------------------------
                ! Watertable deep losses map
                sVarName = 'WDL'
                if (iFlagWDL .eq. 1) then
                    call HMC_Tools_IO_Get2d_NC(sVarName, iFileID, a2dVar, sVarUnits, iCols, iRows, .false., iErr)
                    if (iErr /= 0) then
                        call mprintf(.true., iWARN, ' CoeffWDL data not found. Initializing CoeffDL with default values.')
                        a2dVarCoeffWDL = dWDL
                    else
                        a2dVarCoeffWDL = transpose(a2dVar)
                    endif
                else
                    call mprintf(.true., iWARN, ' Watertable deep losses not activated. CoeffWDL is null.')
                    a2dVarCoeffWDL = 0.0
                endif   
                !------------------------------------------------------------------------------------------
                
                !------------------------------------------------------------------------------------------
                ! Section width map
                sVarName = 'SectionWidth'
                if (iFlagCType .eq. 2) then
                    call HMC_Tools_IO_Get2d_NC(sVarName, iFileID, a2dVar, sVarUnits, iCols, iRows, .false., iErr)
                    if (iErr /= 0) then
                         call mprintf(.true., iWARN, ' SectionWidth data not found. Use morphologic function.') 
                        a2dVarWidthC = -9999.0
                    else
                        a2dVarWidthC = transpose(a2dVar)
                    endif
                else
                    call mprintf(.true., iWARN, ' Channel Network activated. Initializing SectionWidth with undefined values.')
                    a2dVarWidthC = -9999.0
                endif   
                !------------------------------------------------------------------------------------------
                
                !------------------------------------------------------------------------------------------
                ! Fracturing map
                sVarName = 'Fracturation'
                if (iFlagFrac .eq. 1) then
                    call HMC_Tools_IO_Get2d_NC(sVarName, iFileID, a2dVar, sVarUnits, iCols, iRows, .false., iErr)
                    if (iErr /= 0) then
                        call mprintf(.true., iWARN, ' Fracturing data not found. Initializing Fracturing with default values.')   
                        a2dVarFrac = dFrac
                    else
                        a2dVarFrac = transpose(a2dVar)
                    endif
                else
                    call mprintf(.true., iWARN, ' Fracturing not activated. Initializing Fracturing with zero values.')
                    a2dVarFrac = 0.0
                endif  
                !------------------------------------------------------------------------------------------

                !------------------------------------------------------------------------------------------
                ! Left and Right bank depths needed to compute flooding discharges
                if (iFlagFlood.eq.1) then
                
                    sVarName = 'LeftBankDepth'
                    call HMC_Tools_IO_Get2d_NC(sVarName, iFileID, a2dVar, sVarUnits, iCols, iRows, .false., iErr)
                    if (iErr /= 0) then
                        call mprintf(.true., iWARN, ' Left Bank depths data not found. Flooding not activated.')   
                        a2dVarLevBankL = -9999.0
                        a2dVarLevBankR = -9999.0
                        iFlagFlood = 0
                    else
                        a2dVarLevBankL = transpose(a2dVar)
                        sVarName = 'RightBankDepth'
                        call HMC_Tools_IO_Get2d_NC(sVarName, iFileID, a2dVar, sVarUnits, iCols, iRows, .false., iErr)
                        if (iErr /= 0) then
                            call mprintf(.true., iWARN, ' Right Bank depths data not found. Flooding not activated.')   
                            a2dVarLevBankR = -9999.0
                            a2dVarLevBankL = -9999.0
                            iFlagFlood = 0
                        else
                            a2dVarLevBankR = transpose(a2dVar)
                        endif
                        
                    endif
                else
                    call mprintf(.true., iWARN, ' Flooding not activated.')
                    a2dVarLevBankR = -9999.0
                    a2dVarLevBankL = -9999.0
                    iFlagFlood = 0
                endif  
                !------------------------------------------------------------------------------------------

                
                !------------------------------------------------------------------------------------------  
                ! Define dynamic vegetation static variable(s)
                if (iFlagDynVeg.eq.1) then

                    !------------------------------------------------------------------------------------------
                    ! Minimum Stomata resistance [s/m]
                    sVarName = 'RSmin'
                    call HMC_Tools_IO_Get2d_NC(sVarName, iFileID, a2dVar, sVarUnits, iCols, iRows, .false., iErr)
                    if (iErr /= 0) then
                        call mprintf(.true., iERROR,'Minimum stomatal resistance not found.' // &
                        'Dynamic vegetation module not applicable. Program stopped!')
                    else
                        a2dVarRSmin = transpose(a2dVar)
                    endif
                    !------------------------------------------------------------------------------------------
                    
                    !------------------------------------------------------------------------------------------
                    ! Vegetation height [m]
                    sVarName = 'Hveg'
                    call HMC_Tools_IO_Get2d_NC(sVarName, iFileID, a2dVar, sVarUnits, iCols, iRows, .false., iErr)
                    if (iErr /= 0) then
                        call mprintf(.true., iERROR, 'Vegetation Height not found. Dynamic vegetation module not applicable.' // &
                                            'Program stopped!')
                    else
                        a2dVarHveg = transpose(a2dVar)
                    endif
                    !------------------------------------------------------------------------------------------
                    
                    !------------------------------------------------------------------------------------------
                    ! coefficient for the dependency of canopy resistance on water vapor deficit [hPa-1]
                    sVarName = 'Gd'
                    call HMC_Tools_IO_Get2d_NC(sVarName, iFileID, a2dVar, sVarUnits, iCols, iRows, .false., iErr)
                    if (iErr /= 0) then
                        call mprintf(.true., iERROR,'Vapor deficit coefficient not found.'  // &
                        'Dynamic vegetation module not applicable. Program stopped!')
                    else
                        a2dVarGd = transpose(a2dVar)
                    endif        
                    !------------------------------------------------------------------------------------------
                    
                    !------------------------------------------------------------------------------------------
                    ! Bare soil mask layer 
                    sVarName = 'BareSoil'
                    call HMC_Tools_IO_Get2d_NC(sVarName, iFileID, a2dVar, sVarUnits, iCols, iRows, .false., iErr)
                    if (iErr /= 0) then
                        call mprintf(.true., iERROR, 'Bare Soil mask not found. Dynamic vegetation module not applicable.' // &
                                            'Program stopped!')
                    else
                        a2dVarBareSoil = transpose(a2dVar)
                    endif    
                    !------------------------------------------------------------------------------------------
                    
                endif  
                
                !------------------------------------------------------------------------------------------
                ! Closing netcdf file
                iErr = nf90_close(iFileID)
                !------------------------------------------------------------------------------------------
                
            endif
            !------------------------------------------------------------------------------------------
#else
            !------------------------------------------------------------------------------------------
            ! Redefinition of forcing data flag (if netCDF library is not linked)
            iTypeData = 1 
            call mprintf(.true., iWARN, ' '// &
                                        'static gridded data type selected was netCDF but library is not linked! '// &
                                        'Will be used data in ASCII format!')
            !------------------------------------------------------------------------------------------
#endif
        endif
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Land data type in ascii-grid format
        if (iTypeData == 1) then
            
            !------------------------------------------------------------------------------------------
            ! Info data type
            call mprintf(.true., iINFO_Extra, ' Data static gridded in ASCII format')
            !------------------------------------------------------------------------------------------
            
            !------------------------------------------------------------------------------------------
            ! DEM
            sFileName = trim(sPathData)//trim(sDomainName)//'.dem.txt'
            call HMC_Tools_IO_GetArcGrid_ASCII(sFileName, a2dVar, iCols, iRows, .true., iErr)
            a2dVarDEM = reshape(a2dVar, (/iRows, iCols/))
            !------------------------------------------------------------------------------------------
            
            !------------------------------------------------------------------------------------------
            ! LON
            sFileName = trim(sPathData)//trim(sDomainName)//'.lon.txt'
            call HMC_Tools_IO_GetArcGrid_ASCII(sFileName, a2dVar, iCols, iRows, .false., iErr)
            a2dVarLon = reshape(a2dVar, (/iRows, iCols/))
            !------------------------------------------------------------------------------------------
            
            !------------------------------------------------------------------------------------------
            ! LAT
            sFileName = trim(sPathData)//trim(sDomainName)//'.lat.txt'
            call HMC_Tools_IO_GetArcGrid_ASCII(sFileName, a2dVar, iCols, iRows, .false., iErr)
            a2dVarLat = reshape(a2dVar, (/iRows, iCols/))
            !------------------------------------------------------------------------------------------
            
            
            !------------------------------------------------------------------------------------------
            ! MASK
            sFileName = trim(sPathData)//trim(sDomainName)//'.mask.txt'
            call HMC_Tools_IO_GetArcGrid_ASCII(sFileName, a2dVar, iCols, iRows, .false., iErr)
            if (iErr /= 0) then 
                ! call mprintf(.true., iWARN, ' Mask data not found. Initializing Mask with default values.')
                ! where(a2dVarDEM .gt. 0.0)
                !     a2iVarMask = 1 
                ! endwhere
                ! GIULIA
                call mprintf(.true., iWARN, ' Mask data not found. Defining Mask based on DEM (negative elevations allowed).')
                where(a2dVarDEM .ne. oHMC_Namelist(iID)%dNoDataL)
                    a2iVarMask = 1 
                endwhere
            else
                a2iVarMask = int(reshape(a2dVar, (/iRows, iCols/)))
            endif
            ! ! giulia
            ! giulia = 1.2
            ! where( (a2iVarMask.gt.0.0) )
            !     giulia = 7.3
            ! endwhere
            !write(*,*) oHMC_Namelist(iID)%dNoDataL
            !call debug_2dVar(dble(a2iVarMask), iRows, iCols, 10)
            ! call debug_2dVar(dble(giulia), iRows, iCols, 11)
            !------------------------------------------------------------------------------------------
            
            !------------------------------------------------------------------------------------------
            ! CN
            if (oHMC_Namelist(iID)%iFlagSoilParamsType .eq. 1) then
                sFileName = trim(sPathData)//trim(sDomainName)//'.cn.txt'
                call HMC_Tools_IO_GetArcGrid_ASCII(sFileName, a2dVar, iCols, iRows, .false., iErr)
                if (iErr /= 0) then 
                    call mprintf(.true., iWARN, ' CN data not found. Initializing CN with average value.')
                    where(a2iVarMask.gt.0.0)  !giulia
                        a2dVarCN = dCN
                    endwhere
                else
                    a2dVarCN = reshape(a2dVar, (/iRows, iCols/))
                endif
            elseif (oHMC_Namelist(iID)%iFlagSoilParamsType .eq. 2) then
                call mprintf(.true., iWARN, ' CN parameter is not used as iFlagSoilParamsType=2.')        
            endif
            !------------------------------------------------------------------------------------------
            
            !------------------------------------------------------------------------------------------
            ! PNT
            sFileName = trim(sPathData)//trim(sDomainName)//'.pnt.txt'
            call HMC_Tools_IO_GetArcGrid_ASCII(sFileName, a2dVar, iCols, iRows, .true., iErr)
            a2iVarPNT = int(reshape(a2dVar, (/iRows, iCols/)))
            !------------------------------------------------------------------------------------------
            
            !------------------------------------------------------------------------------------------
            ! CHOICE
            sFileName = trim(sPathData)//trim(sDomainName)//'.choice.txt'
            call HMC_Tools_IO_GetArcGrid_ASCII(sFileName, a2dVar, iCols, iRows, .true., iErr)
            a2iVarChoice = int(reshape(a2dVar, (/iRows, iCols/)))
            !------------------------------------------------------------------------------------------
            
            !------------------------------------------------------------------------------------------
            ! Area
            sFileName = trim(sPathData)//trim(sDomainName)//'.area.txt'
            call HMC_Tools_IO_GetArcGrid_ASCII(sFileName, a2dVar, iCols, iRows, .true., iErr)
            a2iVarArea = int(reshape(a2dVar, (/iRows, iCols/)))
!            a2dVarArea = reshape(a2dVar, (/iRows, iCols/))
            !------------------------------------------------------------------------------------------
            
            !------------------------------------------------------------------------------------------
            ! CELLAREA
            sFileName = trim(sPathData)//trim(sDomainName)//'.areacell.txt'
            call HMC_Tools_IO_GetArcGrid_ASCII(sFileName, a2dVar, iCols, iRows, .true., iErr)
            a2dVarAreaCell = reshape(a2dVar, (/iRows, iCols/))
            !------------------------------------------------------------------------------------------

            !------------------------------------------------------------------------------------------
            ! WATERTABLE ALPHA
            sFileName = trim(sPathData)//trim(sDomainName)//'.alpha.txt'
            call HMC_Tools_IO_GetArcGrid_ASCII(sFileName, a2dVar, iCols, iRows, .true., iErr)
            a2dVarAlpha = reshape(a2dVar, (/iRows, iCols/))

            ! WATERTABLE BETA
            sFileName = trim(sPathData)//trim(sDomainName)//'.beta.txt'
            call HMC_Tools_IO_GetArcGrid_ASCII(sFileName, a2dVar, iCols, iRows, .true., iErr)
            a2dVarBeta = reshape(a2dVar, (/iRows, iCols/))

            ! WATERTABLE MAX [mm]
            sFileName = trim(sPathData)//trim(sDomainName)//'.wt_max.txt'
            call HMC_Tools_IO_GetArcGrid_ASCII(sFileName, a2dVar, iCols, iRows, .false., iErr)
            if (iErr /= 0) then 
                call mprintf(.true., iWARN, ' WT maximum data not found. Initializing WT maximum data with default values.')
                a2dVarWTMax = -9999.0
            else
                a2dVarWTMax = reshape(a2dVar, (/iRows, iCols/))
            endif
            
            ! WATERTABLE KSATRATIO !giulia
            sFileName = trim(sPathData)//trim(sDomainName)//'.ksatratio.txt'
            call HMC_Tools_IO_GetArcGrid_ASCII(sFileName, a2dVar, iCols, iRows, .false., iErr)
            if (iErr /= 0) then 
                call mprintf(.true., iWARN, ' KSatRatio data not found. Initializing KSatRatio with scalar value from namelist.')
                where(a2dVarDEM.gt.0.0)
                    a2dVarKSatRatio = dKSatRatio
                endwhere
            else
                a2dVarKSatRatio = reshape(a2dVar, (/iRows, iCols/))
            endif
            !------------------------------------------------------------------------------------------

            !------------------------------------------------------------------------------------------
            ! CT
            sFileName = trim(sPathData)//trim(sDomainName)//'.ct.txt'
            call HMC_Tools_IO_GetArcGrid_ASCII(sFileName, a2dVar, iCols, iRows, .false., iErr)
            if (iErr /= 0) then 
                call mprintf(.true., iWARN, ' Ct data not found. Initializing Ct with average value.')
                where(a2iVarMask.gt.0.0)  !giulia
                    a2dVarCt = dCt
                endwhere
            else
                a2dVarCt = reshape(a2dVar, (/iRows, iCols/))
            endif
            !------------------------------------------------------------------------------------------

            !------------------------------------------------------------------------------------------
            ! ALTERNATIVE SOIL PARAMETRIZATION: 1)saturated vertical hydraulic conductivity for infiltration capacity (mm/h)
            if (oHMC_Namelist(iID)%iFlagSoilParamsType .eq. 2) then
                sFileName = trim(sPathData)//trim(sDomainName)//'.soil_ksat_infilt.txt'
                call HMC_Tools_IO_GetArcGrid_ASCII(sFileName, a2dVar, iCols, iRows, .false., iErr)
                if (iErr /= 0) then 
                    call mprintf(.true., iWARN, &
                    ' Using dSoil_ksat_infilt (mm/h) from info file as spatially constant value.')
                    where(a2iVarMask.gt.0.0)  
                        a2dVarCostF1 = oHMC_Namelist(iID)%dSoil_ksat_infilt 
                    endwhere
                else
                    call mprintf(.true., iINFO_Basic, ' soil_ksat_infilt (mm/h) raster found and read ' // &
                                 '(filename: '//trim(sFileName)//').')
                    a2dVarCostF1 = reshape(a2dVar, (/iRows, iCols/))
                endif
                if (any( ((a2iVarMask .gt. 0.0) .and. (a2dVarCostF1 .le. 0.0)) )) then
                    call mprintf(.true., iERROR,'soil_ksat_infilt raster must have only positive values inside ' // &
                                                'the computational domain! Program stopped!') 
                endif
            endif
            !------------------------------------------------------------------------------------------
            
            !------------------------------------------------------------------------------------------
            ! ALTERNATIVE SOIL PARAMETRIZATION: 2) soil maximum storage volume (mm)
            if (oHMC_Namelist(iID)%iFlagSoilParamsType .eq. 2) then
                sFileName = trim(sPathData)//trim(sDomainName)//'.soil_vmax.txt'
                call HMC_Tools_IO_GetArcGrid_ASCII(sFileName, a2dVar, iCols, iRows, .false., iErr)
                if (iErr /= 0) then 
                    call mprintf(.true., iWARN, &
                    ' Using dSoil_vmax (mm) from info file as spatially constant value.')
                    where(a2iVarMask.gt.0.0)  
                        a2dVarS = oHMC_Namelist(iID)%dSoil_vmax 
                    endwhere
                else
                    call mprintf(.true., iINFO_Basic, ' soil_vmax raster (mm) found and read ' // &
                                 '(filename: '//trim(sFileName)//').')
                    a2dVarS = reshape(a2dVar, (/iRows, iCols/))
                endif
                if (any( ((a2iVarMask .gt. 0.0) .and. (a2dVarS .le. 0.0)) )) then
                    call mprintf(.true., iERROR,'soil_vmax raster must have only positive values inside ' // &
                                                'the computational domain! Program stopped!') 
                endif
            endif
            !------------------------------------------------------------------------------------------
            
            !------------------------------------------------------------------------------------------
            ! ALTERNATIVE SOIL PARAMETRIZATION: 3) saturated vertical hydraulic conductivity for drainage capacity (mm/h)
            if (oHMC_Namelist(iID)%iFlagSoilParamsType .eq. 2) then
                sFileName = trim(sPathData)//trim(sDomainName)//'.soil_ksat_drain.txt'
                call HMC_Tools_IO_GetArcGrid_ASCII(sFileName, a2dVar, iCols, iRows, .false., iErr)
                if (iErr /= 0) then 
                    call mprintf(.true., iWARN, &
                    ' Using dSoil_ksat_drain (mm/h) from info file as spatially constant value.')
                    where(a2iVarMask.gt.0.0)  
                        a2dVarF2 = oHMC_Namelist(iID)%dSoil_ksat_drain / (1 - a2dVarCt)
                    endwhere
                else
                    call mprintf(.true., iINFO_Basic, ' soil_ksat_drain raster (mm/h) found and read ' // &
                                 '(filename: '//trim(sFileName)//').')
                    a2dVarF2 = reshape(a2dVar, (/iRows, iCols/)) / (1 - a2dVarCt)
                endif
                if (any( ((a2iVarMask .gt. 0.0) .and. (a2dVarF2 .le. 0.0)) )) then
                    call mprintf(.true., iERROR,'soil_ksat_drain raster must have only positive values inside ' // &
                                                'the computational domain! Program stopped!') 
                endif
            endif
            !------------------------------------------------------------------------------------------

            !------------------------------------------------------------------------------------------
            ! ALTERNATIVE SOIL PARAMETRIZATION: 4) aquifer horizontal saturated hydraulic conductivity (mm/h)
            if (oHMC_Namelist(iID)%iFlagSoilParamsType .eq. 2) then
                sFileName = trim(sPathData)//trim(sDomainName)//'.wtable_ksath.txt'
                call HMC_Tools_IO_GetArcGrid_ASCII(sFileName, a2dVar, iCols, iRows, .false., iErr)
                if (iErr /= 0) then 
                    call mprintf(.true., iWARN, &
                    ' Using dWTable_ksath (mm/h) from info file as spatially constant value.')
                    where(a2iVarMask.gt.0.0)  
                        a2dVarWTksatH = oHMC_Namelist(iID)%dWTable_ksath
                    endwhere
                else
                    call mprintf(.true., iINFO_Basic, ' wtable_ksath raster (mm/h) found and read ' // &
                                 '(filename: '//trim(sFileName)//').')
                    a2dVarWTksatH = reshape(a2dVar, (/iRows, iCols/))
                endif
                if (any( ((a2iVarMask .gt. 0.0) .and. (a2dVarWTksatH .le. 0.0)) )) then
                    call mprintf(.true., iERROR,'wtable_ksath raster must have only positive values inside ' // &
                                                'the computational domain! Program stopped!') 
                endif
            endif
            !------------------------------------------------------------------------------------------
            
            !------------------------------------------------------------------------------------------
            ! CT_WP - soil moisture wilting point
            sFileName = trim(sPathData)//trim(sDomainName)//'.ct_wp.txt'
            call HMC_Tools_IO_GetArcGrid_ASCII(sFileName, a2dVar, iCols, iRows, .false., iErr)
            if (iErr /= 0) then 
                !call mprintf(.true., iWARN, ' Permanent wilting point data not found. Initializing with default value 0.4*Ct.')
            else
                call mprintf(.true., iWARN, ' Permanent wilting point data NO MORE USED - raster is ineffective.')
                a2dVarCtWP = reshape(a2dVar, (/iRows, iCols/))
            endif
            !------------------------------------------------------------------------------------------

            !------------------------------------------------------------------------------------------
            ! CF
            if ((oHMC_Namelist(iID)%iFlagSoilParamsType .eq. 1) .or. &
               ( (oHMC_Namelist(iID)%iFlagSoilParamsType .eq. 2) .and. (oHMC_Namelist(iID)%iFlagInfiltRateVariable .eq. 1) ) .or. &
               ( (oHMC_Namelist(iID)%iFlagSoilParamsType .eq. 2) .and. (oHMC_Namelist(iID)%iFlagInfiltRateVariable .eq. 2) )) then
                sFileName = trim(sPathData)//trim(sDomainName)//'.cf.txt'
                call HMC_Tools_IO_GetArcGrid_ASCII(sFileName, a2dVar, iCols, iRows, .false., iErr)
                if (iErr /= 0) then
                    call mprintf(.true., iWARN, ' Cf data not found. Initializing Cf with namelist value.')
                    where(a2iVarMask.gt.0.0)
                        a2dVarCf = dCf
                    endwhere
                else
                    a2dVarCf = reshape(a2dVar, (/iRows, iCols/))
                endif
            elseif ((oHMC_Namelist(iID)%iFlagSoilParamsType .eq. 2) .and. (oHMC_Namelist(iID)%iFlagInfiltRateVariable .eq. 0)) then
                call mprintf(.true., iWARN, ' Cf parameter is not used as iFlagSoilParamsType=2 and iFlagInfiltRateVariable=0.') 
            endif
            !------------------------------------------------------------------------------------------

            !------------------------------------------------------------------------------------------
            ! UC
            sFileName = trim(sPathData)//trim(sDomainName)//'.uc.txt'
            call HMC_Tools_IO_GetArcGrid_ASCII(sFileName, a2dVar, iCols, iRows, .false., iErr)
            if (iErr /= 0) then 
                call mprintf(.true., iWARN, ' Uc data not found. Initializing Uc with average values.')
                where(a2iVarMask.gt.0.0)  !giulia
                    a2dVarUc = dUc
                endwhere
            else
                a2dVarUc = reshape(a2dVar, (/iRows, iCols/))
            endif
            !------------------------------------------------------------------------------------------

            !------------------------------------------------------------------------------------------
            ! UH
            sFileName = trim(sPathData)//trim(sDomainName)//'.uh.txt'
            call HMC_Tools_IO_GetArcGrid_ASCII(sFileName, a2dVar, iCols, iRows, .false., iErr)
            if (iErr /= 0) then 
                call mprintf(.true., iWARN, ' Uh data not found. Initializing Uh with average values.')
                where(a2iVarMask.gt.0.0)  !giulia
                    a2dVarUh = dUh
                endwhere
            else
                a2dVarUh = reshape(a2dVar, (/iRows, iCols/))
            endif
            !------------------------------------------------------------------------------------------
            
        !    !------------------------------------------------------------------------------------------
        !    ! MASK
        !    sFileName = trim(sPathData)//trim(sDomainName)//'.mask.txt'
        !    call HMC_Tools_IO_GetArcGrid_ASCII(sFileName, a2dVar, iCols, iRows, .false., iErr)
        !    if (iErr /= 0) then 
        !        ! call mprintf(.true., iWARN, ' Mask data not found. Initializing Mask with default values.')
        !        ! where(a2dVarDEM .gt. 0.0)
        !        !     a2iVarMask = 1 
        !        ! endwhere
        !        ! GIULIA
        !        call mprintf(.true., iWARN, ' Mask data not found. Defining Mask based on DEM (negative elevations allowed).')
        !        where(a2dVarDEM .ne. oHMC_Namelist(iID)%dNoDataL)
        !            a2iVarMask = 1 
        !        endwhere
        !    else
        !        a2iVarMask = int(reshape(a2dVar, (/iRows, iCols/)))
        !    endif
        !    ! Giulia
        !    call debug_2dVar(dble(a2iVarMask), iRows, iCols, 11)
        !    !------------------------------------------------------------------------------------------
            
            !------------------------------------------------------------------------------------------
            ! VegetationIA
            sFileName = trim(sPathData)//trim(sDomainName)//'.ia.txt'
            call HMC_Tools_IO_Get1d_ASCII(sFileName, a1dVar, 100, .false., iErr)
            if (iErr /= 0) then
                call mprintf(.true., iWARN, ' VegIA data not found. Initializing VegIA with default values.')
                a1dVarFCN = (/603.3,572.9,543.8,515.9,489.2,463.6,439.2,415.9,393.6,372.3, &
                            352.0,332.7,314.3,296.8,280.2,264.4,249.4,235.2,221.8,209.1,197.1, &
                            185.8,175.1,165.1,155.6,146.8,138.4,130.7,123.4,116.6,110.3,104.4, &
                            98.9,93.8,89.1,84.7,80.7,77.0,73.7,70.5,67.7,65.1,62.8,60.6,58.7,56.9, &
                            55.3,53.9,52.6,51.4,50.4,49.4,48.6,47.8,47.1,46.4,45.9,45.3,44.8,44.3, &
                            43.8,43.3,42.8,42.3,41.8,41.2,40.7,40.1,39.5,38.8,38.1,37.3,36.5,35.7, &
                            34.7,33.8,32.8,31.7,30.5,29.4,28.1,26.8,25.5,24.1,22.6,21.2,19.7,18.1, &
                            16.5,14.9,13.3,11.7,10.0,8.4,6.8,5.2,3.6,2.1,0.6,0.1/)
            else
                a1dVarFCN = a1dVar
                
                ! Check for avoiding use of old version of IA file (2 columns). New file has only 1 column with values.
                if (all(a1dVar == 1)) then
                    call mprintf(.true., iERROR, ' all VegIA data equal to 1.'// &
                    ' Check if you are using an old version of IA file (2 columns). New file has only 1 column with values!')
                endif
                
            endif
            !------------------------------------------------------------------------------------------
            
            !------------------------------------------------------------------------------------------
            ! Coefficient Resolution
            sFileName = trim(sPathData)//trim(sDomainName)//'.coeffres.txt'
            call HMC_Tools_IO_GetArcGrid_ASCII(sFileName, a2dVar, iCols, iRows, .false., iErr)
            if (iErr /= 0) then 
                call mprintf(.true., iWARN, ' CoeffRes data not found. Initializing CoeffRes with namelist flag condition.')
                a2dVarCoeffResol = iFlagCoeffRes
            else
                a2dVarCoeffResol = reshape(a2dVar, (/iRows, iCols/))
            endif
            !------------------------------------------------------------------------------------------
            
            !------------------------------------------------------------------------------------------
            ! Nature map
            sFileName = trim(sPathData)//trim(sDomainName)//'.nature.txt'
            call HMC_Tools_IO_GetArcGrid_ASCII(sFileName, a2dVar, iCols, iRows, .false., iErr)
            if (iErr /= 0) then 
                call mprintf(.true., iWARN, ' Nature data not found. Initializing Nature with default values.')
                a2iVarNature = -9999
            else
                a2iVarNature = int(reshape(a2dVar, (/iRows, iCols/)))
            endif
            !------------------------------------------------------------------------------------------
            
            !------------------------------------------------------------------------------------------
            ! Watertable sources map
            if (iFlagWS .eq. 1) then
                sFileName = trim(sPathData)//trim(sDomainName)//'.ws.txt'
                call HMC_Tools_IO_GetArcGrid_ASCII(sFileName, a2dVar, iCols, iRows, .false., iErr)
                if (iErr /= 0) then 
                    call mprintf(.true., iWARN, ' CoeffWS data not found. Initializing CoeffWS with average values.')
                    where(a2iVarMask.gt.0.0)  !giulia
                        a2dVarCoeffWS = dWS
                    endwhere
                else
                    a2dVarCoeffWS = reshape(a2dVar, (/iRows, iCols/))
                endif
            else
                call mprintf(.true., iWARN, ' Watertable sources not activated. CoeffWS is null.')
                a2dVarCoeffWS = 0.0
            endif
            !------------------------------------------------------------------------------------------
            
            !------------------------------------------------------------------------------------------
            ! Watertable deep losses map
            if (iFlagWDL .eq. 1) then
                sFileName = trim(sPathData)//trim(sDomainName)//'.wdl.txt'
                call HMC_Tools_IO_GetArcGrid_ASCII(sFileName, a2dVar, iCols, iRows, .false., iErr)
                if (iErr /= 0) then 
                    call mprintf(.true., iWARN, ' CoeffWDL data not found. Initializing CoeffWDL with average values.')
                    where(a2iVarMask.gt.0.0)  !giulia
                        a2dVarCoeffWDL = dWDL
                    endwhere
                else
                    a2dVarCoeffWDL = reshape(a2dVar, (/iRows, iCols/))
                endif
            else
                call mprintf(.true., iWARN, ' Watertable deep losses not activated. CoeffWDL is null.')
                a2dVarCoeffWDL = 0.0
            endif
            !------------------------------------------------------------------------------------------
            
            !------------------------------------------------------------------------------------------
            ! Channel Section Width
            if (iFlagCType .eq. 2) then
                sFileName = trim(sPathData)//trim(sDomainName)//'.width.txt'
                call HMC_Tools_IO_GetArcGrid_ASCII(sFileName, a2dVar, iCols, iRows, .false., iErr)
                if (iErr /= 0) then 
                    call mprintf(.true., iWARN, ' SectionWidth data not found. Use morphologic function.')           
                else
                    a2dVarWidthC = reshape(a2dVar, (/iRows, iCols/))
                endif
            else
                call mprintf(.true., iWARN, ' Channel Network activated. Initializing SectionWidth with undefined values.')
            endif
            !------------------------------------------------------------------------------------------

            !------------------------------------------------------------------------------------------
            ! Fracturation percentage
            if (iFlagFrac .eq. 1) then
                sFileName = trim(sPathData)//trim(sDomainName)//'.fr.txt'
                call HMC_Tools_IO_GetArcGrid_ASCII(sFileName, a2dVar, iCols, iRows, .false., iErr)
                if (iErr /= 0) then 
                    call mprintf(.true., iWARN, ' Fracturing data not found. Initializing CoeffWS with average values.')    
                    where(a2iVarMask.gt.0.0)  !giulia
                        a2dVarFrac = dFrac
                    endwhere
                else
                    a2dVarFrac = reshape(a2dVar, (/iRows, iCols/)) 
                endif
            else
                call mprintf(.true., iWARN, ' Fracturing not activated. Initializing Fracturing with zero values.')
                a2dVarFrac = 0.0
            endif
            !------------------------------------------------------------------------------------------  

            !------------------------------------------------------------------------------------------
            ! Left and Right bank depths
            if (iFlagFlood.eq.1) then
                sFileName = trim(sPathData)//trim(sDomainName)//'.lfl.txt'
                call HMC_Tools_IO_GetArcGrid_ASCII(sFileName, a2dVar, iCols, iRows, .false., iErr)
                if (iErr /= 0) then
                    call mprintf(.true., iWARN, ' Left Bank depths data not found. Flooding not activated.')   
                    a2dVarLevBankL = -9999.0
                    a2dVarLevBankR = -9999.0
                    iFlagFlood = 0
                else
                    a2dVarLevBankL = reshape(a2dVar, (/iRows, iCols/)) 
                    sFileName = trim(sPathData)//trim(sDomainName)//'.rfl.txt'
                    call HMC_Tools_IO_GetArcGrid_ASCII(sFileName, a2dVar, iCols, iRows, .false., iErr)
                    if (iErr /= 0) then
                        call mprintf(.true., iWARN, ' Right Bank depths data not found. Flooding not activated.')   
                        a2dVarLevBankR = -9999.0
                        a2dVarLevBankL = -9999.0
                        iFlagFlood = 0
                    else
                        a2dVarLevBankR = reshape(a2dVar, (/iRows, iCols/)) 
                    endif

                endif
            else
                call mprintf(.true., iWARN, ' Flooding not activated.')
                a2dVarLevBankR = -9999.0
                a2dVarLevBankL = -9999.0
                iFlagFlood = 0
            endif  
            !------------------------------------------------------------------------------------------

            !------------------------------------------------------------------------------------------  
            ! Define dynamic vegetation static variable(s)
            if (iFlagDynVeg.eq.1) then
               
                !------------------------------------------------------------------------------------------
                ! Minimum Stomata resistance [s/m]
                sFileName = trim(sPathData)//trim(sDomainName)//'.RSmin.txt'
                call HMC_Tools_IO_GetArcGrid_ASCII(sFileName, a2dVar, iCols, iRows, .true., iErr)
                if (iErr /= 0) then 
                    call mprintf(.true., iERROR, 'Minimum stomatal resistance not found.'// & 
                    'Dynamic vegetation module not applicable. Program stopped!')
                else
                    a2dVarRSmin = reshape(a2dVar, (/iRows, iCols/))
                endif           
                !------------------------------------------------------------------------------------------

                !------------------------------------------------------------------------------------------
                ! Vegetation height [m]
                sFileName = trim(sPathData)//trim(sDomainName)//'.Hveg.txt'
                call HMC_Tools_IO_GetArcGrid_ASCII(sFileName, a2dVar, iCols, iRows, .true., iErr)
                if (iErr /= 0) then 
                    call mprintf(.true., iERROR, 'Vegetation Height not found. Dynamic vegetation module not applicable.' // &
                                                  'Program stopped!')
                else
                    a2dVarHveg = reshape(a2dVar, (/iRows, iCols/))
                endif
                !------------------------------------------------------------------------------------------

                !------------------------------------------------------------------------------------------
                ! Coefficient for the dependancy of canopy resistance on water vapor deficit [hPa-1]
                sFileName = trim(sPathData)//trim(sDomainName)//'.Gd.txt'
                call HMC_Tools_IO_GetArcGrid_ASCII(sFileName, a2dVar, iCols, iRows, .true., iErr)
                if (iErr /= 0) then 
                    call mprintf(.true., iERROR, 'Vapor deficit coefficient not found.'// & 
                    'Dynamic vegetation module not applicable. Program stopped!')
                else
                    a2dVarGd = reshape(a2dVar, (/iRows, iCols/))
                endif
                !------------------------------------------------------------------------------------------

                !------------------------------------------------------------------------------------------
                ! Bare soil mask layer 
                sFileName = trim(sPathData)//trim(sDomainName)//'.BareSoil.txt'
                call HMC_Tools_IO_GetArcGrid_ASCII(sFileName, a2dVar, iCols, iRows, .true., iErr)
                if (iErr /= 0) then 
                    call mprintf(.true., iERROR, 'Vapor deficit coefficient not found.'// & 
                    'Dynamic vegetation module not applicable. Program stopped!')
                else
                    a2dVarBareSoil = reshape(a2dVar, (/iRows, iCols/))
                endif
                !------------------------------------------------------------------------------------------                     
                
            endif
            !------------------------------------------------------------------------------------------
            
        endif
        !------------------------------------------------------------------------------------------

        !------------------------------------------------------------------------------------------
        ! Nullify map boundary
        a2dVarDEM = nullborder2DVar(a2dVarDEM, -9999.0)
        a2dVarAreaCell = nullborder2DVar(a2dVarAreaCell, -9999.0)
        a2dVarCN = nullborder2DVar(a2dVarCN, -9999.0)
        a2dVarCt = nullborder2DVar(a2dVarCt, -9999.0)
        a2dVarCtWP = nullborder2DVar(a2dVarCtWP, -9999.0)
        a2dVarCf = nullborder2DVar(a2dVarCf, -9999.0)
        a2dVarUh = nullborder2DVar(a2dVarUh, -9999.0)
        a2dVarUc = nullborder2DVar(a2dVarUc, -9999.0)
        a2dVarAlpha = nullborder2DVar(a2dVarAlpha, -9999.0)
        a2dVarBeta = nullborder2DVar(a2dVarBeta, -9999.0)
        a2dVarWTMax = nullborder2DVar(a2dVarWTMax, -9999.0)
        a2dVarKSatRatio = nullborder2DVar(a2dVarKSatRatio, -9999.0) !giulia
        a2iVarMask = int(nullborder2DVar(float(a2iVarMask), -9999.0))
        a2iVarChoice = int(nullborder2DVar(float(a2iVarChoice), -9999.0))
        a2iVarPNT = int(nullborder2DVar(float(a2iVarPNT), -9999.0))
        a2dVarCoeffWS = nullborder2DVar(a2dVarCoeffWS, -9999.0)
        a2dVarCoeffWDL = nullborder2DVar(a2dVarCoeffWDL, -9999.0)
        a2dVarFrac = nullborder2DVar(a2dVarFrac, -9999.0)
        a2dVarWidthC = nullborder2DVar(a2dVarWidthC, -9999.0)
        a2dVarLevBankR = nullborder2DVar(a2dVarLevBankR, -9999.0)
        a2dVarLevBankL = nullborder2DVar(a2dVarLevBankL, -9999.0)
        a2dVarRSmin = nullborder2DVar(a2dVarRSmin, -9999.0)
        a2dVarGd = nullborder2DVar(a2dVarGd, -9999.0)
        a2dVarHveg = nullborder2DVar(a2dVarHveg, -9999.0)
        a2dVarBareSoil = nullborder2DVar(a2dVarBareSoil, -9999.0)   
        !------------------------------------------------------------------------------------------

        !------------------------------------------------------------------------------------------
        ! Check Ct values over domain
        iPixCount = count((a2dVarCt.le.0.0 .and. a2iVarMask.gt.0.0)) !giulia
        if (iPixCount.gt.0) then
            write(sPixCount, *) iPixCount;
            write(sParDefault, *) dCt;
            call mprintf(.true., iWARN, ' Ct values are lower than or equal to 0.0 in '//trim(sPixCount)// & 
            ' pixels over domain. '// 'Initialize with default value: '//trim(sParDefault)//'.')
        endif
        where (a2dVarCt.le.0.0 .and. a2iVarMask.gt.0.0) !giulia
            a2dVarCt = dCt
        endwhere

        ! Check CtWP values over domain
        iPixCount = count((a2dVarCtWP.le.0.0 .and. a2iVarMask.gt.0.0)) !giulia
        !if (iPixCount.gt.0) then
        !    write(sPixCount, *) iPixCount;
        !    call mprintf(.true., iWARN, ' CtWP values are lower than or equal to 0.0 in '//trim(sPixCount)//& 
        !    ' pixels over domain. '// 'Initialize with default value: 0.4*Ct.')
        !endif
        where (a2dVarCtWP.le.0.0 .and. a2iVarMask.gt.0.0) !giulia
            a2dVarCtWP = 0.4*a2dVarCt
        endwhere 
        
        ! Check Cf values over domain
        iPixCount = count((a2dVarCf.eq.0.0 .and. a2iVarMask.gt.0.0)) !giulia
        if (iPixCount.gt.0) then
            write(sPixCount, *) iPixCount;
            write(sParDefault, *) dCf;
            call mprintf(.true., iWARN, ' Cf values are equal to 0.0 in '//trim(sPixCount)//' pixels over domain. '// &
            'Initialize with default value: '//trim(sParDefault)//'.')
        endif
        where (a2dVarCf.le.0.0 .and. a2iVarMask.gt.0.0) !giulia
            a2dVarCf = dCf
        endwhere
        
        ! Check Uh values over domain
        iPixCount = count((a2dVarUh.eq.0.0 .and. a2iVarMask.gt.0.0)) !giulia
        if (iPixCount.gt.0) then
            write(sPixCount, *) iPixCount;
            write(sParDefault, *) dUh;
            call mprintf(.true., iWARN, ' Uh values are equal to 0.0 in '//trim(sPixCount)//' pixels over domain. '// &
            'Initialize with default value: '//trim(sParDefault)//'.')
        endif
        where (a2dVarUh.le.0.0 .and. a2iVarMask.gt.0.0) !giulia
            a2dVarUh = dUh
        endwhere
        
        ! Check Uc values over domain
        iPixCount = count((a2dVarUc.eq.0.0 .and. a2iVarMask.gt.0.0)) !giulia
        if (iPixCount.gt.0) then
            write(sPixCount, *) iPixCount;
            write(sParDefault, *) dUc;
            call mprintf(.true., iWARN, ' Uc values are equal to 0.0 in '//trim(sPixCount)//' pixels over domain. '// &
            'Initialize with default value: '//trim(sParDefault)//'.')
        endif
        where (a2dVarUc.le.0.0 .and. a2iVarMask.gt.0.0) !giulia
            a2dVarUc = dUc
        endwhere
        
        ! Check water-table angle(s)
        where(a2iVarMask.gt.0.0 .and. a2dVarAlpha.le.0.0) !giulia
            a2dVarAlpha = 0.00001
        endwhere
        where(a2iVarMask.gt.0.0 .and. a2dVarBeta.le.0.0) !giulia    
            a2dVarBeta = 0.00001
        endwhere
        
        ! Check multiplier for vertical saturated hydraulic conductivity - to get horizontal conductivity !giulia
	! Zero values should be allowed - to be confirmed
	iPixCount = count((a2dVarKSatRatio.lt.0.0 .and. a2iVarMask.gt.0))
        if (iPixCount.gt.0) then
            write(sPixCount, *) iPixCount;
            write(sParDefault, *) dKSatRatio;
            call mprintf(.true., iWARN, ' KSatRatio values are lower than 0 in '//trim(sPixCount)//' pixels over domain. '// &
            'Initialize with default value: '//trim(sParDefault)//'.')
        endif 
        
        ! Info end
        call mprintf(.true., iINFO_Verbose, ' Data :: Static gridded :: Get land information ... OK' )
        !------------------------------------------------------------------------------------ 
        
        !------------------------------------------------------------------------------------ 
        ! Land data derived fields
        call mprintf(.true., iINFO_Verbose, ' Data :: Static gridded :: Compute derived land information ... ' )
      
        ! Defining cell area mean value (x and y)
        dDxM = nint(sqrt(sum(a2dVarAreaCell, mask=a2iVarMask.gt.0.0) / count(a2iVarMask.gt.0.0))) !giulia
        dDyM = nint(sqrt(sum(a2dVarAreaCell, mask=a2iVarMask.gt.0.0) / count(a2iVarMask.gt.0.0))) !giulia

        ! DEM max and min values and step mean
        dDEMMax = maxval(maxval(a2dVarDEM,DIM = 1, MASK=a2iVarMask.gt.0),DIM = 1) !giulia
        dDEMMin = minval(minval(a2dVarDEM,DIM = 1, MASK=a2iVarMask.gt.0),DIM = 1) !giulia
        dDEMStepMean = sqrt(dDxM*dDyM)
       
        ! Computing total catchment pixels and area
        iDomainPixels = sum(sum(a2iVarMask,dim=1, mask=a2iVarMask.gt.0.0)) !giulia          
        dDomainArea = float(iDomainPixels)*dDxM*dDyM/1000000       
        
        ! Domain information
        write(sStrDomPix, *) iDomainPixels; write(sStrDomArea, *) dDomainArea; 
        write(sStrTc, *) iTc; write(sStrDemMax, *) dDEMMax;
        call mprintf(.true., iINFO_Main, ' DOMAIN INFO --- '// &
                    'NPixels : '//trim(sStrDomPix)//' [-] '//'Area: '//trim(sStrDomArea)//' [Km^2] '// &
                    'TerrainHeigthMax: '//trim(sStrDemMax)//' [m] ')
        !------------------------------------------------------------------------------------ 
        
        !------------------------------------------------------------------------------------
        ! Defining resolution coefficient map in default mode if external file is not available
        if ( all(a2dVarCoeffResol.eq.1.0 )) then
            
            ! Compute default value(s) based on cell area 
            where ( a2iVarMask.gt.0.0 .and. a2iVarChoice.ge.0 ) !giulia
                a2dVarCoeffResol = exp(-sqrt(a2dVarAreaCell)*0.0009)
                ! a2dVarCoeffResol = exp(-sqrt(a2dVarAreaCell)*0.0009) ! Tevere settings
                ! a2dVarCoeffResol = exp(-sqrt(a2dVarAreaCell)*0.0007) ! Other basins settings
            endwhere
            
            ! Channels condition
            where ( a2iVarMask.gt.0.0 .and. a2iVarChoice.eq.1 .and. a2dVarCoeffResol.gt.0.05) !giulia
        	a2dVarCoeffResol = 0.05
            endwhere
            
            ! Check variable lower limit 
            where ( a2dVarCoeffResol.lt.0.0 )
                a2dVarCoeffResol = 0.0
            endwhere
        
        elseif ( all(a2dVarCoeffResol.eq.0.0 )) then
            a2dVarCoeffResol = 1.0
            call mprintf(.true., iWARN, ' CoeffRes data is set to null in according with namelist flag condition')
        endif   
        
        ! Nullify coefficient resolution out of the domain
        where (a2iVarMask .le. 0.0) !giulia
            a2dVarCoeffResol = 0.0
        endwhere
        !------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------
        ! Check watertable sources coefficient map lower limit
        where (a2dVarCoeffWS.lt.0.0 .and. a2dVarCoeffWS.ne.-9999.0)
            a2dVarCoeffWS = 0.0
        endwhere
        
        ! Check watertable deep losses sources coefficient map lower limit
        where (a2dVarCoeffWDL.lt.0.0 .and. a2dVarCoeffWDL.ne.-9999.0)
            a2dVarCoeffWDL = 0.0
        endwhere
        
        if (dWTLossMax .lt. 0) then
            call mprintf(.true., iWARN, ' Watertable maximum losses < 0. Set to 0.0')
            dWTLossMax = 0.0
        endif
        if (dWTLossMax .gt. 1) then
            call mprintf(.true., iWARN, ' Watertable maximum losses > 1. Set to 1.0')
            dWTLossMax = 1.0
        endif
        
        if ( .not. all(a2dVarCoeffWS*dDtDataForcing .le. dWTLossMax)) then
            call mprintf(.true., iError, ' The water loss due to the watertable sources '// & 
            'is higher then the maximum allowed losses.')
        endif   
        
        if ( .not. all(a2dVarCoeffWDL*dDtDataForcing .le. dWTLossMax)) then
            call mprintf(.true., iError, ' The water loss due to the watertable deep losses '// & 
            'is higher then the maximum allowed losses.')
        endif   
        
        if ( .not. all(a2dVarCoeffWS*dDtDataForcing + a2dVarCoeffWDL*dDtDataForcing .le. dWTLossMax)) then
            call mprintf(.true., iError, ' The water loss due to the watertable sources and the '// &
            'watertable deep losses is higher then the maximum allowed losses.')
        endif   
        
        !where (a2dVarCoeffWS.gt.0.99) ! 
        !    a2dVarCoeffWS = 0.0
        !endwhere
        !------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------
        ! Check fracturation  coefficient map limit
        where (a2dVarFrac.gt.1)
            a2dVarFrac = 1.0
        endwhere
        where (a2iVarMask.gt.0.0.and.a2dVarFrac.lt.0.0) !giulia
            a2dVarFrac = 0.0
        endwhere
        !------------------------------------------------------------------------------------

        !------------------------------------------------------------------------------------
        ! Check minimum stomatal resistance when dynamic vegetation is active
        if (iFlagDynVeg.eq.1) then
            where (a2iVarMask.gt.0.0 .and. a2dVarRSmin.lt.0.0) !giulia
                a2dVarRSmin = 0.0
            endwhere     
        endif
        !------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------ 
        ! Defining S and CON
        if (oHMC_Namelist(iID)%iFlagSoilParamsType .eq. 1) then
            where ( (a2dVarCN.gt.0 .and. a2dVarCN.le.100) .and. a2iVarMask.gt.0.0)
                a2dVarS = (1000.0/a2dVarCN - 10)*25.4
                a2dVarCon = int(a2dVarCN)
            endwhere
        endif
        
        ! Checking lower limit of a2dVarS inside the computational domain
        where (a2iVarMask.gt.0.0.and.a2dVarS.lt.1.0)
            a2dVarS = 1.0
        endwhere

        ! Checking lower limit of a2dVarS outside the computational domain
        where (a2dVarS.lt.0.0)
            a2dVarS = 0.0
        endwhere
        
        where (a2iVarMask.le.0.0)
            a2dVarS = 0.0
        endwhere
        !------------------------------------------------------------------------------------ 
        
        !------------------------------------------------------------------------------------ 
        ! Defining Soil Water Content at Wilting Point
        where ( a2iVarMask.gt.0.0 ) 
            a2dVarVTotWP = 0.0 ! condizione che tagliava un 20% del vtot a2dVarS * a2dVarCtWP ! ct_wp = 0.4*ct
        endwhere
        !------------------------------------------------------------------------------------ 
        
        !------------------------------------------------------------------------------------ 
        ! Defining Horton constants
        if (oHMC_Namelist(iID)%iFlagSoilParamsType .eq. 1) then
            where (a2dVarCon.lt.1 .or. a2dVarCon.gt.99)
                a2dVarCon = 1
            endwhere

            ! Calculating CostF
            forall(iI = 1:iRows, iJ = 1:iCols)
                a2dVarCostF(iI, iJ) = a1dVarFCN( int( a2dVarCon(iI, iJ) ) )
            end forall
        
            where (a2iVarMask.gt.0.0)
                ! Compute F1 Horton term
                a2dVarCostF1 = a2dVarCf*a2dVarCostF
                ! Horton exponent case I>g ( a2dCostChFix=((1-dCt)*a2dCostF+dCt*a2dCostF1)/((1-dCt)*a2dS) )
                a2dVarCostChFix = ( (1 - a2dVarCt) * a2dVarCostF + a2dVarCt*a2dVarCostF1) / ( (1 - a2dVarCt)*a2dVarS ) 
                ! Horton Ct correction term 1 ( a2dC1=a2dCostF1*dCt/(1-dCt) )
                a2dVarC1 = a2dVarCostF1*a2dVarCt/(1 - a2dVarCt)
                ! Horton Ct correction term 2 ( a2dF2=a2dCostF1/(1-dCt) )
                a2dVarF2 = a2dVarCostF1/(1 - a2dVarCt)
                ! Compute explicitly aquifer saturated horizontal conductivity (mm/h)
                a2dVarWTksatH =  a2dVarCostF1*oHMC_Namelist(iID)%dKSatRatio
            endwhere
        elseif (oHMC_Namelist(iID)%iFlagSoilParamsType .eq. 2) then
            if (oHMC_Namelist(iID)%iFlagInfiltRateVariable .eq. 0) then
                a2dVarCostF = a2dVarCostF1 !infiltration capacity does not change with saturation
            elseif ((oHMC_Namelist(iID)%iFlagInfiltRateVariable .eq. 1) .or. &
                    (oHMC_Namelist(iID)%iFlagInfiltRateVariable .eq. 2)) then
                a2dVarCostF = a2dVarCostF1 / a2dVarCf !infiltration capacity changes with saturation
            endif
        endif
        !------------------------------------------------------------------------------------ 
        
        !------------------------------------------------------------------------------------ 
        ! Define priority side of flooding
        ! Initialize the matrix with 0 Same Overbanking level, 1 Right first, 2 Left first
        if(iFlagFlood.eq.1)then
            where (a2iVarMask.GT.0.0.and.a2dVarLevBankR.gt.a2dVarLevBankL)
                a2dVarFirst = 2
            endwhere
            where (a2iVarMask.GT.0.0.and.a2dVarLevBankL.gt.a2dVarLevBankR)
                a2dVarFirst = 1
            endwhere
            where (a2iVarMask.GT.0.0.and.a2dVarLevBankR.eq.a2dVarLevBankL)
                a2dVarFirst = 0
            endwhere
            where (a2iVarMask.GT.0.0.and.a2dVarLevBankR.le.0.0)
                a2dVarFirst = -9999.0
            endwhere
        endif        
        
        ! Info end
        call mprintf(.true., iINFO_Verbose, ' Data :: Static gridded :: Compute derived land information ... OK' )
        !------------------------------------------------------------------------------------ 
        
        !------------------------------------------------------------------------------------
        ! Debug
        if (iDEBUG.gt.0) then
            call mprintf(.true., iINFO_Extra, ' ========= STATIC GRIDDED START =========== ')   
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarDEM, a2iVarMask, 'DEM ') )
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarLon, a2iVarMask, 'LON ') )
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarLat, a2iVarMask, 'LAT ') )
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarCN, a2iVarMask, 'CN ') )
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarS, a2iVarMask, 'S ') )
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarVTotWP, a2iVarMask, 'Vtot_WP ') )
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarCt, a2iVarMask, 'CT ') )
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarCtWP, a2iVarMask, 'CTWP ') )
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarCf, a2iVarMask, 'CF ') )
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarUc, a2iVarMask, 'UC ') )
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarUh, a2iVarMask, 'UH ') )
            call mprintf(.true., iINFO_Extra, checkvar(float(a2iVarNature), a2iVarMask, 'NATURE ') )
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarAlpha, a2iVarMask, 'Watertable ALPHA ') )
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarBeta, a2iVarMask, 'Watertable BETA ') )
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarWTMax, a2iVarMask, 'Watertable MAX ') )
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarKSatRatio, a2iVarMask, 'KSAT RATIO ') ) !giulia
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarCon, a2iVarMask, 'CON ') )
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarCostF, a2iVarMask, 'COSTF ') )
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarCostF1, a2iVarMask, 'COSTF1 ') )
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarCostChFix, a2iVarMask, 'CHFIX ') )
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarC1, a2iVarMask, 'C1 ') )
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarF2, a2iVarMask, 'F2 ') )
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarCoeffResol, a2iVarMask, 'CRES ') )   
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarCoeffWS, a2iVarMask, 'WS ') )
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarCoeffWDL, a2iVarMask, 'WDL ') )
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarWidthC, a2iVarMask, 'WIDTHC ') )
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarFrac, a2iVarMask, 'FRAC ') )
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarLevBankL, a2iVarMask, 'LEFT BANKS ') )
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarLevBankR, a2iVarMask, 'RIGHT BANKS ') )
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarFirst, a2iVarMask, 'FLOOODING SIDE PRIORITY ') )
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarRSmin, a2iVarMask, 'RSmin ') )
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarGd, a2iVarMask, 'Gd ') )
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarHveg, a2iVarMask, 'Hveg ') )
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarBareSoil, a2iVarMask, 'BareSoil ') )
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarWTksatH, a2iVarMask, 'Watertable KSAT horizontal ') )
            call mprintf(.true., iINFO_Extra, ' ========= STATIC GRIDDED END =========== ') 
        endif
        !------------------------------------------------------------------------------------
       
        !------------------------------------------------------------------------------------
        ! Pass local variable(s) to global workspace
        oHMC_Vars(iID)%iDomainPixels = iDomainPixels
        oHMC_Vars(iID)%dDxM = dDxM
        oHMC_Vars(iID)%dDyM = dDyM
        oHMC_Vars(iID)%dDEMMax = dDEMMax
        oHMC_Vars(iID)%dDEMMin = dDEMMin
        oHMC_Vars(iID)%dDEMStepMean = dDEMStepMean
        oHMC_Vars(iID)%dDomainArea = dDomainArea
        
        oHMC_Vars(iID)%a2dCt = a2dVarCt
        oHMC_Vars(iID)%a2dCtWP = a2dVarCtWP
        oHMC_Vars(iID)%a2dCf = a2dVarCf
        oHMC_Vars(iID)%a2dUc = a2dVarUc
        oHMC_Vars(iID)%a2dUh = a2dVarUh
        
        oHMC_Vars(iID)%a2iMask = a2iVarMask
        oHMC_Vars(iID)%a2iPNT = a2iVarPNT
        oHMC_Vars(iID)%a2iChoice = a2iVarChoice
        oHMC_Vars(iID)%a2iArea = a2iVarArea
        oHMC_Vars(iID)%a2dAreaCell = a2dVarAreaCell
        oHMC_Vars(iID)%a2iNature = a2iVarNature
        
        oHMC_Vars(iID)%a2dLon = a2dVarLon
        oHMC_Vars(iID)%a2dLat = a2dVarLat
        
        oHMC_Vars(iID)%a2dDem = a2dVarDEM
        oHMC_Vars(iID)%a2dAlpha = a2dVarAlpha
        oHMC_Vars(iID)%a2dBeta = a2dVarBeta
        oHMC_Vars(iID)%a2dWTableMax = a2dVarWTMax
        oHMC_Vars(iID)%a2dKSatRatio = a2dVarKSatRatio !giulia
        
        oHMC_Vars(iID)%a2dS = a2dVarS
        oHMC_Vars(iID)%a2dVTotWP = a2dVarVTotWP
        
        oHMC_Vars(iID)%a2dC1 = a2dVarC1
        oHMC_Vars(iID)%a2dF2 = a2dVarF2
        oHMC_Vars(iID)%a2dCostF = a2dVarCostF
        oHMC_Vars(iID)%a2dCostF1 = a2dVarCostF1
        oHMC_Vars(iID)%a2dCostChFix = a2dVarCostChFix
        
        oHMC_Vars(iID)%a2dWTksatH = a2dVarWTksatH
        
        oHMC_Vars(iID)%a2dCoeffResol = a2dVarCoeffResol
        oHMC_Vars(iID)%a2dCoeffWS = a2dVarCoeffWS
        oHMC_Vars(iID)%a2dCoeffWDL = a2dVarCoeffWDL
        
        oHMC_Vars(iID)%a2dWidthC = a2dVarWidthC
        oHMC_Vars(iID)%a2dFrac = a2dVarFrac
        
        oHMC_Vars(iID)%a2dLevBankL = a2dVarLevBankL
        oHMC_Vars(iID)%a2dLevBankR = a2dVarLevBankR
        oHMC_Vars(iID)%a2dFirst = a2dVarFirst

        oHMC_Vars(iID)%a2dRSmin = a2dVarRSmin
        oHMC_Vars(iID)%a2dHveg = a2dVarHveg
        oHMC_Vars(iID)%a2dGd = a2dVarGd
        oHMC_Vars(iID)%a2dBareSoil = a2dVarBareSoil
        !------------------------------------------------------------------------------------ 
        
    end subroutine HMC_Data_Static_Gridded_Land
    !------------------------------------------------------------------------------------ 
    
    !------------------------------------------------------------------------------------ 
    ! Subroutine for loading and initializing watertable variable(s)
    subroutine HMC_Data_Static_Gridded_WTable(iID, iRows, iCols)
        
        !------------------------------------------------------------------------------------------
        ! Variable(s)
        integer(kind = 4) :: iID, iRows, iCols
        
        real(kind = 4) :: dVarDEMMin, dVarDEMMax,  dVarSlopeMax
        real(kind = 4) :: dVarWTableHMin, dVarWTableHMax
        real(kind = 4) :: dVarAlphaMin, dVarAlphaMax, dVarAlphaExtreme
        real(kind = 4) :: dVarWTableHUSoil, dVarWTableHUChannel, dVarWTableSlopeBM, dVarWTableHOBedRock
        real(kind = 4) :: dVarWTableAlphaMin, dVarWTableAlphaMax
        
        integer(kind = 4), dimension (iRows, iCols)      :: a2iVarMask
        real(kind = 4), dimension (iRows, iCols)         :: a2iVarChoice
        real(kind = 4), dimension (iRows, iCols)         :: a2dVarDEM, a2dVarAlpha
        
        real(kind = 4), dimension (iRows, iCols)         :: a2dVarWTable, a2dVarWTableMax, a2dVarWTableMax_Tmp
        !------------------------------------------------------------------------------------------

        !------------------------------------------------------------------------------------------
        ! Parameter(s)
        dVarAlphaMin = 10.0; dVarAlphaMax = 0.0; 
        dVarWTableHMin = 10.0 ! Min value in [mm]
        
        dVarWTableHUSoil = 0.0      ! fmin
        dVarWTableHUChannel = 0.0   ! fcan
        dVarWTableSlopeBM = 0.0     ! fpen
        dVarWTableHOBedRock = 0.0   ! f0v
                
        a2dVarWTable = 0.0; a2dVarWTableMax = 0.0; a2dVarWTableMax_Tmp = 0.0;
        a2dVarAlpha = 0.0; 
        !------------------------------------------------------------------------------------------

        !------------------------------------------------------------------------------------------
        ! Variable(s) from global declaration (namelist and variable(s)
        a2dVarDEM = oHMC_Vars(iID)%a2dDEM
        a2iVarMask = oHMC_Vars(iID)%a2iMask
        
        a2iVarChoice = oHMC_Vars(iID)%a2iChoice
        a2dVarAlpha = oHMC_Vars(iID)%a2dAlpha
        
        a2dVarWTableMax = oHMC_Vars(iID)%a2dWTableMax
        
        dVarDEMMin = oHMC_Vars(iID)%dDEMMin
        dVarDEMMax = oHMC_Vars(iID)%dDEMMax
        
        dVarSlopeMax = oHMC_Namelist(iID)%dSlopeMax
        
        dVarWTableHMax = oHMC_Namelist(iID)%dWTableHBr
        dVarWTableHMin = oHMC_Namelist(iID)%dWTableHMin 
        
        dVarWTableHUSoil = oHMC_Namelist(iID)%dWTableHUSoil
        dVarWTableHUChannel = oHMC_Namelist(iID)%dWTableHUChannel
        dVarWTableSlopeBM = oHMC_Namelist(iID)%dWTableSlopeBM
        dVarWTableHOBedRock = oHMC_Namelist(iID)%dWTableHOBedRock

        ! Info start
        call mprintf(.true., iINFO_Verbose, ' Data :: Static gridded :: Get watertable information ... ' )
        
        ! Debug
        if (iDEBUG.gt.0) then
            call mprintf(.true., iINFO_Extra, ' ======== WATERTABLE START ========== ') 
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarWTable, a2iVarMask, 'Watertable VALUES') )
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarWTableMax, a2iVarMask, 'Watertable MAX ') )
            call mprintf(.true., iINFO_Extra, '') 
        endif   
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Calculate alpha min and max
        dVarAlphaMin = minval( minval( a2dVarAlpha, DIM = 1, MASK=a2dVarAlpha.gt.0.001 ),DIM = 1 )
        dVarAlphaMax = maxval( maxval( a2dVarAlpha, DIM = 1, MASK=a2dVarAlpha.gt.0.001 ), DIM = 1 )
        
        ! Calculate max angle for having a watertable definition                        
        dVarAlphaExtreme = 3.14/180*dVarSlopeMax   
        
        ! Update alpha max and watertable h min (using angles)
        if(dVarAlphaMax.gt.dVarAlphaExtreme)then
            dVarAlphaMax = dVarAlphaExtreme
            dVarWTableHMin = 0
        endif
        !------------------------------------------------------------------------------------------

        !------------------------------------------------------------------------------------------
        ! Define default watertable max values
        where(a2iVarMask .gt. 0)
            a2dVarWTableMax_Tmp = dVarWTableHMax*(1 - (tan(a2dVarAlpha) - &
                                  tan(dVarAlphaMin))/(tan(dVarAlphaMax) - & 
                                  tan(dVarAlphaMin))*(1 - dVarWTableHMin/dVarWTableHMax))
        endwhere
        ! Check default watertable values limits
        where( (a2iVarMask.gt.0.0) .and. (a2dVarWTableMax_Tmp .gt. dVarWTableHMax) )
            a2dVarWTableMax_Tmp = dVarWTableHMax
        endwhere
        
        ! Update watertable maximum values using default relationship where dataset is not defined
        where( (a2iVarMask .gt. 0) .and. (a2dVarWTableMax .lt. 0.0) )
            a2dVarWTableMax = a2dVarWTableMax_Tmp    
        endwhere
        
        ! Check watertable maximum limits 
        where( (a2iVarMask.gt.0.0).and.(a2dVarWTableMax.lt.0.0) )
            a2dVarWTableMax = 0.0 
        endwhere
        where(a2iVarMask.gt.0.0)
            a2dVarWTableMax = a2dVarDEM - a2dVarWTableMax/1000.0
        endwhere
        where( (a2iVarMask.gt.0.0).and.(a2dVarWTableMax.lt.0.0) )
            a2dVarWTableMax = 0.0 
        endwhere
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Define watertable values
	where( a2iVarMask.gt.0.0 )
		a2dVarWTable = (dVarWTableHMax - dVarWTableHUSoil)*((tan(a2dVarAlpha) - &
                                   tan(dVarAlphaMin))/(tan(dVarAlphaMax) - tan(dVarAlphaMin))) & 
                                   + dVarWTableHUSoil 
                
		a2dVarWTable = a2dVarWTable/1000.0
        endwhere
        
	where( a2iVarMask.gt.0.0 )
		a2dVarWTable = a2dVarDEM - a2dVarWTable
        endwhere
        
	! Riempimento WT in mm per pendenze elevate
	where( a2dVarAlpha.gt.dVarWTableSlopeBM )  a2dVarWTable = a2dVarWTableMax + dVarWTableHOBedRock/1000.0 
            
        ! Riempimento WT in mm sotto i canali
	where( a2iVarChoice.eq.1 )  a2dVarWTable = a2dVarDEM - dVarWTableHUChannel/1000.0

        ! Check watertable limits
        ! Upper limit
        where(a2dVarWTable.gt.a2dVarDEM)
                a2dVarWTable = a2dVarDEM
        endwhere
        ! Lower limit
        where(a2dVarWTable.lt.a2dVarWTableMax)
                a2dVarWTable = a2dVarWTableMax
        endwhere
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------
        ! Debug
        if (iDEBUG.gt.0) then
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarWTable, a2iVarMask, 'Watertable VALUES') )
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarWTableMax, a2iVarMask, 'Watertable MAX ') )
            call mprintf(.true., iINFO_Extra, ' ========= WATERTABLE END =========== ') 
        endif
        
        ! Updating variable(s) to global declaration (WTable, WTableMax)
        oHMC_Vars(iID)%a2dWTable = a2dVarWTable
        oHMC_Vars(iID)%a2dWTableMax = a2dVarWTableMax
        
        ! Info end
        call mprintf(.true., iINFO_Verbose, ' Data :: Static gridded :: Get watertable information ... OK' )
        !------------------------------------------------------------------------------------------
        
    end subroutine HMC_Data_Static_Gridded_WTable
    !------------------------------------------------------------------------------------ 
      
end module HMC_Module_Data_Static_Gridded
!------------------------------------------------------------------------------------

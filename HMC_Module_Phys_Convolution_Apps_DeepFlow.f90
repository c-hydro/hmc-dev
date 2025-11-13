!------------------------------------------------------------------------------------
! File:   HMC_Module_Phys_Convolution_Apps_DeepFlow.f90
!
! Author(s):    Fabio Delogu, Francesco Silvestro, Simone Gabellani
! Date:         20190410
!
! Convolution Apps DeepFlow subroutine(s) for HMC model
!------------------------------------------------------------------------------------

!------------------------------------------------------------------------------------
! Module Header
module HMC_Module_Phys_Convolution_Apps_DeepFlow

    !------------------------------------------------------------------------------------
    ! External module(s) 
    use HMC_Module_Namelist,                only: oHMC_Namelist
    use HMC_Module_Vars_Loader,             only: oHMC_Vars
    
    use HMC_Module_Tools_Debug
    
                                                      
    use HMC_Module_Tools_Generic,           only: getIntValue, getIntRange
    
    ! Implicit none for all subroutines in this module
    implicit none
    !------------------------------------------------------------------------------------------

contains 

    !------------------------------------------------------------------------------------------
    ! Subroutine for calculating deepflow channel network
    subroutine HMC_Phys_Convolution_Apps_DeepFlow_ChannelNetwork(iID, iRows, iCols, dDtDataForcing)
        
        !------------------------------------------------------------------------------------------
        ! Variable(s) declaration
        integer(kind = 4)           :: iID
        integer(kind = 4)           :: iRows, iCols
        
        integer(kind = 4)           :: iI, iII, iIII, iJ, iJJ, iJJJ
        integer(kind = 4)           :: iFlagFlowDeep, iNgr
        real(kind = 4)              :: dHt, dHm
        real(kind = 4)              :: dDtDataForcing
        
        real(kind = 4), dimension (iRows, iCols)    :: a2dVarVTot, a2dVarVLoss, a2dVarFlowDeep
        real(kind = 4), dimension (iRows, iCols)    :: a2dVarDarcy, a2dVarHydro
        real(kind = 4), dimension (iRows, iCols)    :: a2dVarWSRunoff, a2dVarWDL
        
        real(kind = 4), dimension (iRows, iCols)    :: a2dVarWTable, a2dVarWTableStep
        real(kind = 4), dimension (iRows, iCols)    :: a2dVarWTable_Source_Losses, a2dVarWTable_Deep_Losses
        
        real(kind = 4), dimension (iRows, iCols)    :: a2dVarFrac
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Initialization variable(s)
        a2dVarVTot = 0.0; a2dVarVLoss = 0.0; a2dVarFlowDeep = 0.0;
        a2dVarDarcy = 0.0; a2dVarHydro = 0.0;
        a2dVarWSRunoff = 0.0; a2dVarWDL = 0.0; a2dVarFrac = 0.0
        
        a2dVarWTable = 0.0; a2dVarWTableStep = 0.0; 
        a2dVarWTable_Source_Losses = 0.0; a2dVarWTable_Deep_Losses = 0.0;
        
        iFlagFlowDeep = 0; 
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Flow deep flag
        iFlagFlowDeep = oHMC_Namelist(iID)%iFlagFlowDeep 
                    
        ! Info start
        if (iDEBUG.gt.0) then
            call mprintf(.true., iINFO_Extra, ' Phys :: Convolution :: DeepFlow ... ' )
        endif
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Condition for activating flow deep process
        if (iFlagFlowDeep.eq.1) then
            
            !------------------------------------------------------------------------------------------
            ! Re-initializing flow deep 
            oHMC_Vars(iID)%a2dFlowDeep = 0.0
            oHMC_Vars(iID)%a2dDarcy = 0.0
            !------------------------------------------------------------------------------------------
            
            !------------------------------------------------------------------------------------------
            ! Temporal step of model and forcing data
            !iT = oHMC_Vars(iID)%iTime
            !dDt = real(oHMC_Namelist(iID)%iDtModel)
            !------------------------------------------------------------------------------------------

            !------------------------------------------------------------------------------------------
            ! Land data from global declaration
            a2dVarWTable = oHMC_Vars(iID)%a2dWTable
            !a2dVarHydro = oHMC_Vars(iID)%a2dHydro
            a2dVarVTot = oHMC_Vars(iID)%a2dVTot
            a2dVarVLoss = oHMC_Vars(iID)%a2dVLoss
            !a2dVarWSRunoff = oHMC_Vars(iID)%a2dWSRunoff
            a2dVarFrac = oHMC_Vars(iID)%a2dFrac
            !------------------------------------------------------------------------------------------
            
            !-----------------------------------------------------------------------------------------
            ! Debug
            if (iDEBUG.gt.0) then
                call mprintf(.true., iINFO_Extra, ' ========= DEEPFLOW START =========== ')  
                call mprintf(.true., iINFO_Extra, checkvar(a2dVarFlowDeep, oHMC_Vars(iID)%a2iMask, 'FLOWDEEP START ') )
                call mprintf(.true., iINFO_Extra, checkvar(a2dVarVTot, oHMC_Vars(iID)%a2iMask, 'VTOT START ') )
                call mprintf(.true., iINFO_Extra, checkvar(a2dVarWTable, oHMC_Vars(iID)%a2iMask, 'WTABLE START ') )
                !call mprintf(.true., iINFO_Extra, checkvar(a2dVarHydro, oHMC_Vars(iID)%a2iMask, 'HYDRO START ') )
                call mprintf(.true., iINFO_Extra, ' ') 
            endif
            !-----------------------------------------------------------------------------------------
            
            !------------------------------------------------------------------------------------------
            ! Update WTable
            where( oHMC_Vars(iID)%a2iMask.gt.0.0 )
                a2dVarWTableStep = a2dVarWTable + a2dVarVLoss/1000
            endwhere
            !------------------------------------------------------------------------------------------

            !------------------------------------------------------------------------------------------
            ! Compute deep flow
            do iJ = 3, iCols - 1
                do iI = 3, iRows - 1
                    
                    ! Initialization cycle variable(s)
                    iNgr = 0
                    dHt = 0.0
                    dHm = 0.0
                    
                    ! Defining flow directions
                    iII = int((oHMC_Vars(iID)%a2iPNT(iI,iJ)  - 1)/3) - 1
                    iJJ = oHMC_Vars(iID)%a2iPNT(iI,iJ) - 5 - 3*iII
                    iIII = iI + iII !riga cella di valle secondo PNT
                    iJJJ = iJ + iJJ !colonna cella di valle secondo PNT
                    
                    !write(*,*) 'PNT: ',iVarPNT   
                    !write(*,*) 'iJ: ',iJ, ' iI: ',iI, ' iJJ: ',iJJ, ' iII: ',iII , ' iJJJ: ',iJJJ, ' iIII: ',iIII
                    !------------------------------------------------------------------------------------------

                    !------------------------------------------------------------------------------------------
                    ! Terrain condition for i,j and iii,jjj 
                    ! (~controlla che sia la cella di partenza che di arrivo -valle secondo PNT- siano nel dominio)
                    if ( (oHMC_Vars(iID)%a2iMask(iI,iJ).gt.0.0 ) .and. (oHMC_Vars(iID)%a2iMask(iIII,iJJJ).gt.0.0) ) then

                        !------------------------------------------------------------------------------------------
                        ! Cycle(s) on buffer area
                        do iII = iI - 1, iI + 1
                            do iJJ = iJ - 1, iJ + 1

                                ! if ( (oHMC_Vars(iID)%a2iMask(iII,iJJ).gt.0.0) .and. ((iII.ne.iI).and.(iJJ.ne.iJ)) ) then
                                ! GIULIA - baco - spostamenti erano possibili solo verso celle diagonali
                                if ( (oHMC_Vars(iID)%a2iMask(iII,iJJ).gt.0.0) .and. ((iII.ne.iI).or.(iJJ.ne.iJ)) ) then    

                                    if ( (a2dVarWTable(iI, iJ) - a2dVarWTable(iII,iJJ)).gt.0.0 ) then

                                        dHt = dHt + (a2dVarWTable(iI, iJ) - a2dVarWTable(iII, iJJ))
                                        iNgr = iNgr + 1

                                    endif

                                endif

                            enddo
                        enddo
                        !------------------------------------------------------------------------------------------
                        
                        !------------------------------------------------------------------------------------------
                        if (iNgr.gt.0) then

                            !------------------------------------------------------------------------------------------
                            dHm = dHt/iNgr
                            
                            !a2dVarDarcy(iI,iJ) = dHm/sqrt(oHMC_Vars(iID)%a2dAreaCell(iI,iJ)) * &
                            !                     oHMC_Vars(iID)%a2dCostF1(iI,iJ) * &
                            !                     dDtDataForcing/3600*oHMC_Namelist(iID)%dKSatRatio
                            
                            a2dVarDarcy(iI,iJ) = dHm/sqrt(oHMC_Vars(iID)%a2dAreaCell(iI,iJ)) * &
                                                 oHMC_Vars(iID)%a2dWTksatH(iI,iJ) * dDtDataForcing/3600
                                               
                            if ( a2dVarDarcy(iI,iJ) .gt. ( a2dVarWTable(iI,iJ) - oHMC_Vars(iID)%a2dWTableMax(iI,iJ))*1000 ) then
                                a2dVarDarcy(iI,iJ) = (a2dVarWTable(iI,iJ) - oHMC_Vars(iID)%a2dWTableMax(iI,iJ))*1000
                            endif
                            !------------------------------------------------------------------------------------------
                            
                            !------------------------------------------------------------------------------------------
                            do iII = iI - 1, iI + 1
                                do iJJ = iJ - 1, iJ + 1

                                    ! if ( (oHMC_Vars(iID)%a2iMask(iII,iJJ).gt.0.0) .and. ((iII.ne.iI).and.(iJJ.ne.iJ)) ) then
                                    ! GIULIA - baco - spostamenti erano possibili solo verso celle diagonali
                                    if ( (oHMC_Vars(iID)%a2iMask(iII,iJJ).gt.0.0) .and. ((iII.ne.iI).or.(iJJ.ne.iJ)) ) then
                            
                                        if ( (a2dVarWTable(iI,iJ) - a2dVarWTable(iII,iJJ)).gt.0.0 ) then

                                            a2dVarWTableStep(iII, iJJ) = a2dVarWTableStep(iII, iJJ) + & 
                                                                         a2dVarDarcy(iI,iJ)*(a2dVarWTable(iI, iJ) - &
                                                                         a2dVarWTable(iII,iJJ))/(dHt*1000)
                                            dHm = dHm

                                        endif
                                    endif
                                    
                                enddo
                            enddo
                            !------------------------------------------------------------------------------------------
                            
                        endif
                        !------------------------------------------------------------------------------------------

                    endif
                    !------------------------------------------------------------------------------------------
                    
                    !------------------------------------------------------------------------------------------
                    ! Outlet cell
                    if ( (oHMC_Vars(iID)%a2iMask(iI,iJ).gt.0.0) .and. (oHMC_Vars(iID)%a2iMask(iIII, iJJJ).lt.0.0) ) then
                        
                        !a2dVarDarcy(iI, iJ) = oHMC_Vars(iID)%a2dAlpha(iI,iJ)*oHMC_Vars(iID)%a2dCostF1(iI,iJ)* &
                        !                      dDtDataForcing/(3600*1000)*oHMC_Namelist(iID)%dKSatRatio
                                              
                        a2dVarDarcy(iI, iJ) = oHMC_Vars(iID)%a2dAlpha(iI,iJ)* &
                                              oHMC_Vars(iID)%a2dWTksatH(iI,iJ) * dDtDataForcing/3600 !giulia - credo /1000=baco

                        ! giulia - credo baco (mancava il *1000)                      
                        !if ( a2dVarDarcy(iI,iJ) .gt. (a2dVarWTable(iI,iJ) - oHMC_Vars(iID)%a2dWTableMax(iI,iJ)) ) then
                        !    a2dVarDarcy(iI,iJ) = (a2dVarWTable(iI,iJ) - oHMC_Vars(iID)%a2dWTableMax(iI,iJ))
                        !endif
                                                                   
                        ! giulia - credo baco (mancava il *1000)                      
                        if ( a2dVarDarcy(iI,iJ) .gt. (a2dVarWTable(iI,iJ) - oHMC_Vars(iID)%a2dWTableMax(iI,iJ))*1000 ) then
                            a2dVarDarcy(iI,iJ) = (a2dVarWTable(iI,iJ) - oHMC_Vars(iID)%a2dWTableMax(iI,iJ))*1000 
                        endif

                    endif
                    !------------------------------------------------------------------------------------------

                enddo
            enddo
            !------------------------------------------------------------------------------------------

            !------------------------------------------------------------------------------------------
            where( oHMC_Vars(iID)%a2iMask.gt.0.0 )
                a2dVarWTableStep = a2dVarWTableStep - a2dVarDarcy/1000
            endwhere
            !------------------------------------------------------------------------------------------
            

            !------------------------------------------------------------------------------------------
            ! Flow deep - Interaction between watertable and surface
            where( (oHMC_Vars(iID)%a2iMask.gt.0.0) .and. (a2dVarWTableStep.gt.oHMC_Vars(iID)%a2dDem) )
                a2dVarFlowDeep = (1 - a2dVarFrac)*(a2dVarWTableStep - oHMC_Vars(iID)%a2dDem)*dDtDataForcing/3600*1000
                a2dVarWTableStep = oHMC_Vars(iID)%a2dDem
            endwhere

            ! Updating watertable
            where( oHMC_Vars(iID)%a2iMask.gt.0.0 )
                a2dVarWTable = a2dVarWTableStep
            endwhere
            
            a2dVarWSRunoff = 0.0
            where( oHMC_Vars(iID)%a2iMask.gt.0.0 )

                a2dVarWDL = (a2dVarWTable - oHMC_Vars(iID)%a2dWTableMax)*oHMC_Vars(iID)%a2dCoeffWDL*oHMC_Vars(iID)%a2dAreaCell ! m^3/s
                a2dVarWSRunoff = (a2dVarWTable - oHMC_Vars(iID)%a2dWTableMax)*oHMC_Vars(iID)%a2dCoeffWS*oHMC_Vars(iID)%a2dAreaCell ! m^3/s
                
                a2dVarWTable_Source_Losses = (a2dVarWTable - oHMC_Vars(iID)%a2dWTableMax)*oHMC_Vars(iID)%a2dCoeffWS*dDtDataForcing 
                a2dVarWTable_Deep_Losses = (a2dVarWTable - oHMC_Vars(iID)%a2dWTableMax)*oHMC_Vars(iID)%a2dCoeffWDL*dDtDataForcing
                
                a2dVarWTable = a2dVarWTable - a2dVarWTable_Source_Losses - a2dVarWTable_Deep_Losses ! m
                
                !a2dVarWTable = a2dVarWTable - (a2dVarWTable - oHMC_Vars(iID)%a2dWTableMax)*oHMC_Vars(iID)%a2dCoeffWS*dDtDataForcing ! m
            endwhere
            
            ! Updating VTot
            where( oHMC_Vars(iID)%a2iMask.gt.0.0 )
                a2dVarVTot = a2dVarVTot + a2dVarFlowDeep
            endwhere
            
            ! Updating flow deep and vtot where vtot >= vmax
            a2dVarFlowDeep = 0.0 
            where( (oHMC_Vars(iID)%a2iMask.gt.0.0) .and. (a2dVarVTot.gt.oHMC_Vars(iID)%a2dS) )
                a2dVarFlowDeep = a2dVarVTot - oHMC_Vars(iID)%a2dS
                a2dVarVTot = oHMC_Vars(iID)%a2dS
            endwhere
            !------------------------------------------------------------------------------------------
            
            !-----------------------------------------------------------------------------------------
            ! Debug
            if (iDEBUG.gt.0) then
                call mprintf(.true., iINFO_Extra, ' ') 
                call mprintf(.true., iINFO_Extra, checkvar(a2dVarFlowDeep, oHMC_Vars(iID)%a2iMask, 'FLOWDEEP END ') )
                call mprintf(.true., iINFO_Extra, checkvar(a2dVarVTot, oHMC_Vars(iID)%a2iMask, 'VTOT END ') )
                call mprintf(.true., iINFO_Extra, checkvar(a2dVarWTable, oHMC_Vars(iID)%a2iMask, 'WTABLE END ') )
                !call mprintf(.true., iINFO_Extra, checkvar(a2dVarHydro, oHMC_Vars(iID)%a2iMask, 'HYDRO END ') )
                call mprintf(.true., iINFO_Extra, ' ========= DEEPFLOW END =========== ') 
            endif
            !-----------------------------------------------------------------------------------------
            
        else
            
            !-----------------------------------------------------------------------------------------
            ! Deep flow process not activated
            if (iDEBUG.gt.0) then
                call mprintf(.true., iINFO_Extra, ' Phys :: Convolution :: DeepFlow ... Skipped!' )
            endif
            a2dVarVTot = oHMC_Vars(iID)%a2dVTot
            a2dVarFlowDeep = 0.0
            a2dVarWSRunoff = 0.0
            !-----------------------------------------------------------------------------------------
            
        endif
        !-----------------------------------------------------------------------------------------

        !-----------------------------------------------------------------------------------------
        ! Updating model global variable(s)
        oHMC_Vars(iID)%a2dFlowDeep = a2dVarFlowDeep
        oHMC_Vars(iID)%a2dVTot = a2dVarVTot
        oHMC_Vars(iID)%a2dWTable = a2dVarWTable
        oHMC_Vars(iID)%a2dWSRunoff = a2dVarWSRunoff
        oHMC_Vars(iID)%a2dWDL = a2dVarWDL
        
        !oHMC_Vars(iID)%a2dHydro = a2dVarHydro

        ! Info end
        if (iDEBUG.gt.0) then
            call mprintf(.true., iINFO_Extra, ' Phys :: Convolution :: DeepFlow ... OK' )
        endif
        !-----------------------------------------------------------------------------------------

    end subroutine HMC_Phys_Convolution_Apps_DeepFlow_ChannelNetwork
    !------------------------------------------------------------------------------------------

    !------------------------------------------------------------------------------------------
    ! Subroutine for calculating deepflow channel fraction
    subroutine HMC_Phys_Convolution_Apps_DeepFlow_ChannelFraction(iID, iRows, iCols, dDtDataForcing)
        
        !------------------------------------------------------------------------------------------
        ! Variable(s) declaration
        integer(kind = 4)           :: iID
        integer(kind = 4)           :: iRows, iCols
        
        integer(kind = 4)           :: iI, iII, iIII, iJ, iJJ, iJJJ
        integer(kind = 4)           :: iFlagFlowDeep, iNgr
        real(kind = 4)              :: dHt, dHm
        real(kind = 4)              :: dDtDataForcing
        
        real(kind = 4), dimension (iRows, iCols)    :: a2dVarVTot, a2dVarVLoss, a2dVarFlowDeep
        real(kind = 4), dimension (iRows, iCols)    :: a2dVarDarcy, a2dVarHydro
        real(kind = 4), dimension (iRows, iCols)    :: a2dVarWSRunoff, a2dVarWDL
        
        real(kind = 4), dimension (iRows, iCols)    :: a2dVarWTable, a2dVarWTableStep
        real(kind = 4), dimension (iRows, iCols)    :: a2dVarWTable_Source_Losses, a2dVarWTable_Deep_Losses

        real(kind = 4), dimension (iRows, iCols)    :: a2dVarFrac
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Initialization variable(s)
        a2dVarVTot = 0.0; a2dVarVLoss = 0.0; a2dVarFlowDeep = 0.0;
        a2dVarDarcy = 0.0; a2dVarHydro = 0.0;
        a2dVarWSRunoff = 0.0; a2dVarWDL = 0.0; a2dVarFrac = 0.0;
        
        a2dVarWTable = 0.0; a2dVarWTableStep = 0.0; 
        a2dVarWTable_Source_Losses = 0.0; a2dVarWTable_Deep_Losses = 0.0;
        
        iFlagFlowDeep = 0;
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Flow deep flag
        iFlagFlowDeep = oHMC_Namelist(iID)%iFlagFlowDeep 
                    
        ! Info start
        if (iDEBUG.gt.0) then
            call mprintf(.true., iINFO_Extra, ' Phys :: Convolution :: DeepFlow ... ' )
        endif
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Condition for activating flow deep process
        if (iFlagFlowDeep.eq.1) then
            
            !------------------------------------------------------------------------------------------
            ! Re-initializing flow deep 
            oHMC_Vars(iID)%a2dFlowDeep = 0.0
            oHMC_Vars(iID)%a2dDarcy = 0.0
            !------------------------------------------------------------------------------------------
            
            !------------------------------------------------------------------------------------------
            ! Land data from global declaration
            a2dVarWTable = oHMC_Vars(iID)%a2dWTable
            a2dVarVTot = oHMC_Vars(iID)%a2dVTot
            a2dVarVLoss = oHMC_Vars(iID)%a2dVLoss
            ! a2dVarWSRunoff = oHMC_Vars(iID)%a2dWSRunoff
            a2dVarFrac = oHMC_Vars(iID)%a2dFrac
            !------------------------------------------------------------------------------------------
            
            !-----------------------------------------------------------------------------------------
            ! Debug
            if (iDEBUG.gt.0) then
                call mprintf(.true., iINFO_Extra, ' ========= DEEPFLOW START =========== ')  
                call mprintf(.true., iINFO_Extra, checkvar(a2dVarFlowDeep, oHMC_Vars(iID)%a2iMask, 'FLOWDEEP START ') )
                call mprintf(.true., iINFO_Extra, checkvar(a2dVarVTot, oHMC_Vars(iID)%a2iMask, 'VTOT START ') )
                call mprintf(.true., iINFO_Extra, checkvar(a2dVarWTable, oHMC_Vars(iID)%a2iMask, 'WTABLE START ') )
                call mprintf(.true., iINFO_Extra, ' ') 
            endif
            !-----------------------------------------------------------------------------------------
            
            !------------------------------------------------------------------------------------------
            ! Update WTable
            where( oHMC_Vars(iID)%a2iMask.gt.0.0 )
                a2dVarWTableStep = a2dVarWTable + a2dVarVLoss/1000
            endwhere
            !------------------------------------------------------------------------------------------

            !------------------------------------------------------------------------------------------
            ! Compute deep flow
            do iJ = 3, iCols - 1
                do iI = 3, iRows - 1
                    
                    ! Initialization cycle variable(s)
                    iNgr = 0
                    dHt = 0.0
                    dHm = 0.0
                    
                    ! Defining flow directions
                    iII = int((oHMC_Vars(iID)%a2iPNT(iI,iJ)  - 1)/3) - 1
                    iJJ = oHMC_Vars(iID)%a2iPNT(iI,iJ) - 5 - 3*iII
                    iIII = iI + iII
                    iJJJ = iJ + iJJ
                    
                    !write(*,*) 'PNT: ',iVarPNT   
                    !write(*,*) 'iJ: ',iJ, ' iI: ',iI, ' iJJ: ',iJJ, ' iII: ',iII , ' iJJJ: ',iJJJ, ' iIII: ',iIII
                    !------------------------------------------------------------------------------------------

                    !------------------------------------------------------------------------------------------
                    ! Terrain condition for i,j and iii,jjj 
                    if ( (oHMC_Vars(iID)%a2iMask(iI,iJ).gt.0.0 ) .and. (oHMC_Vars(iID)%a2iMask(iIII,iJJJ).gt.0.0) ) then

                        !------------------------------------------------------------------------------------------
                        ! Cycle(s) on buffer area
                        do iII = iI - 1, iI + 1
                            do iJJ = iJ - 1, iJ + 1

                                ! if ( (oHMC_Vars(iID)%a2iMask(iII,iJJ).gt.0.0 ) .and. ((iII.ne.iI).and.(iJJ.ne.iJ)) ) then
                                ! GIULIA - baco - spostamenti erano possibili solo verso celle diagonali
                                if ( (oHMC_Vars(iID)%a2iMask(iII,iJJ).gt.0.0 ) .and. ((iII.ne.iI).or.(iJJ.ne.iJ)) ) then
                                    
                                    if ( (a2dVarWTable(iI, iJ) - a2dVarWTable(iII,iJJ)).gt.0.0 ) then

                                        dHt = dHt + (a2dVarWTable(iI, iJ) - a2dVarWTable(iII, iJJ))
                                        iNgr = iNgr + 1

                                    endif

                                endif

                            enddo
                        enddo
                        !------------------------------------------------------------------------------------------
                        
                        !------------------------------------------------------------------------------------------
                        if (iNgr.gt.0) then

                            !------------------------------------------------------------------------------------------
                            dHm = dHt/iNgr
                            
                            !a2dVarDarcy(iI,iJ) = dHm/sqrt(oHMC_Vars(iID)%a2dAreaCell(iI,iJ)) * &
                            !                     oHMC_Vars(iID)%a2dCostF1(iI,iJ) * &
                            !                     dDtDataForcing/3600*oHMC_Namelist(iID)%dKSatRatio
                            
                            a2dVarDarcy(iI,iJ) = dHm/sqrt(oHMC_Vars(iID)%a2dAreaCell(iI,iJ)) * &
                                                 oHMC_Vars(iID)%a2dWTksatH(iI,iJ) * dDtDataForcing/3600
                                               
                            if ( a2dVarDarcy(iI,iJ) .gt. ( a2dVarWTable(iI,iJ) - oHMC_Vars(iID)%a2dWTableMax(iI,iJ))*1000 ) then
                                a2dVarDarcy(iI,iJ) = (a2dVarWTable(iI,iJ) - oHMC_Vars(iID)%a2dWTableMax(iI,iJ))*1000
                            endif
                            !------------------------------------------------------------------------------------------
                            
                            !------------------------------------------------------------------------------------------
                            do iII = iI - 1, iI + 1
                                do iJJ = iJ - 1, iJ + 1

                                    ! if ( (oHMC_Vars(iID)%a2iMask(iII, iJJ).gt.0.0) .and. ((iII.ne.iI).and.(iJJ.ne.iJ)) ) then
                                    ! GIULIA - baco - spostamenti erano possibili solo verso celle diagonali
                                    if ( (oHMC_Vars(iID)%a2iMask(iII, iJJ).gt.0.0) .and. ((iII.ne.iI).or.(iJJ.ne.iJ)) ) then  
                                        if ( (a2dVarWTable(iI,iJ) - a2dVarWTable(iII,iJJ)).gt.0.0 ) then

                                            a2dVarWTableStep(iII, iJJ) = a2dVarWTableStep(iII, iJJ) + & 
                                                                         a2dVarDarcy(iI,iJ)*(a2dVarWTable(iI, iJ) - &
                                                                         a2dVarWTable(iII,iJJ))/(dHt*1000)
                                            dHm = dHm

                                        endif
                                    endif
                                    
                                enddo
                            enddo
                            !------------------------------------------------------------------------------------------
                            
                        endif
                        !------------------------------------------------------------------------------------------

                    endif
                    !------------------------------------------------------------------------------------------
                    
                    !------------------------------------------------------------------------------------------
                    ! Outlet cell
                    if ( (oHMC_Vars(iID)%a2iMask(iI,iJ).gt.0.0) .and. (oHMC_Vars(iID)%a2iMask(iIII, iJJJ).lt.0.0) ) then
                        
                        !a2dVarDarcy(iI, iJ) = oHMC_Vars(iID)%a2dAlpha(iI,iJ)*oHMC_Vars(iID)%a2dCostF1(iI,iJ)* &
                        !                      dDtDataForcing/(3600*1000)*oHMC_Namelist(iID)%dKSatRatio
                                              
                        a2dVarDarcy(iI, iJ) = oHMC_Vars(iID)%a2dAlpha(iI,iJ)* &
                                              oHMC_Vars(iID)%a2dWTksatH(iI,iJ) * dDtDataForcing/3600 !giulia - credo /1000=baco

                        ! giulia - credo baco (mancava il *1000)                      
                        !if ( a2dVarDarcy(iI,iJ) .gt. (a2dVarWTable(iI,iJ) - oHMC_Vars(iID)%a2dWTableMax(iI,iJ)) ) then
                        !    a2dVarDarcy(iI,iJ) = (a2dVarWTable(iI,iJ) - oHMC_Vars(iID)%a2dWTableMax(iI,iJ)) 
                        !endif
                        
                        ! giulia - credo baco (mancava il *1000)                      
                        if ( a2dVarDarcy(iI,iJ) .gt. (a2dVarWTable(iI,iJ) - oHMC_Vars(iID)%a2dWTableMax(iI,iJ))*1000 ) then
                            a2dVarDarcy(iI,iJ) = (a2dVarWTable(iI,iJ) - oHMC_Vars(iID)%a2dWTableMax(iI,iJ))*1000 
                        endif

                    endif
                    !------------------------------------------------------------------------------------------

                enddo
            enddo
            !------------------------------------------------------------------------------------------

            !------------------------------------------------------------------------------------------
            where( oHMC_Vars(iID)%a2iMask.gt.0.0 )
                a2dVarWTableStep = a2dVarWTableStep - a2dVarDarcy/1000
            endwhere
            !------------------------------------------------------------------------------------------
            
            !------------------------------------------------------------------------------------------
            ! Flow deep - Interaction between watertable and surface
            where( (oHMC_Vars(iID)%a2iMask.gt.0.0) .and. (a2dVarWTableStep.gt.oHMC_Vars(iID)%a2dDem) )
                a2dVarFlowDeep = (1 - a2dVarFrac)*(a2dVarWTableStep - oHMC_Vars(iID)%a2dDem)*dDtDataForcing/3600*1000
                a2dVarWTableStep = oHMC_Vars(iID)%a2dDem
            endwhere

            ! Updating watertable
            where( oHMC_Vars(iID)%a2iMask.gt.0.0 )
                a2dVarWTable = a2dVarWTableStep
            endwhere
            
            where( oHMC_Vars(iID)%a2iMask.gt.0.0 )

                a2dVarWDL = (a2dVarWTable - oHMC_Vars(iID)%a2dWTableMax)*oHMC_Vars(iID)%a2dCoeffWDL*oHMC_Vars(iID)%a2dAreaCell ! m^3/s
                a2dVarWSRunoff = (a2dVarWTable - oHMC_Vars(iID)%a2dWTableMax)*oHMC_Vars(iID)%a2dCoeffWS*oHMC_Vars(iID)%a2dAreaCell ! m^3/s
                
                a2dVarWTable_Source_Losses = (a2dVarWTable - oHMC_Vars(iID)%a2dWTableMax)*oHMC_Vars(iID)%a2dCoeffWS*dDtDataForcing 
                a2dVarWTable_Deep_Losses = (a2dVarWTable - oHMC_Vars(iID)%a2dWTableMax)*oHMC_Vars(iID)%a2dCoeffWDL*dDtDataForcing
                
                a2dVarWTable = a2dVarWTable - a2dVarWTable_Source_Losses - a2dVarWTable_Deep_Losses ! m
                
                !a2dVarWTable = a2dVarWTable - (a2dVarWTable - oHMC_Vars(iID)%a2dWTableMax)*oHMC_Vars(iID)%a2dCoeffWS*dDtDataForcing ! m
                 
            endwhere
            
            ! Updating VTot
            where( oHMC_Vars(iID)%a2iMask.gt.0.0 )
                a2dVarVTot = a2dVarVTot + a2dVarFlowDeep
            endwhere
            
            ! Updating flow deep and vtot where vtot >= vmax
            a2dVarFlowDeep = 0.0 
            where( (oHMC_Vars(iID)%a2iMask.gt.0.0) .and. (a2dVarVTot.gt.oHMC_Vars(iID)%a2dS) )
                a2dVarFlowDeep = a2dVarVTot - oHMC_Vars(iID)%a2dS
                a2dVarVTot = oHMC_Vars(iID)%a2dS
            endwhere
            !------------------------------------------------------------------------------------------
            
            !-----------------------------------------------------------------------------------------
            ! Debug
            if (iDEBUG.gt.0) then
                call mprintf(.true., iINFO_Extra, ' ') 
                call mprintf(.true., iINFO_Extra, checkvar(a2dVarFlowDeep, oHMC_Vars(iID)%a2iMask, 'FLOWDEEP END ') )
                call mprintf(.true., iINFO_Extra, checkvar(a2dVarVTot, oHMC_Vars(iID)%a2iMask, 'VTOT END ') )
                call mprintf(.true., iINFO_Extra, checkvar(a2dVarWTable, oHMC_Vars(iID)%a2iMask, 'WTABLE END ') )
                call mprintf(.true., iINFO_Extra, ' ========= DEEPFLOW END =========== ') 
            endif
            !-----------------------------------------------------------------------------------------
            
        else
            
            !-----------------------------------------------------------------------------------------
            ! Deep flow process not activated
            if (iDEBUG.gt.0) then
                call mprintf(.true., iINFO_Extra, ' Phys :: Convolution :: DeepFlow ... Skipped!' )
            endif
            a2dVarVTot = oHMC_Vars(iID)%a2dVTot
            a2dVarFlowDeep = 0.0
            a2dVarWSRunoff = 0.0
            a2dVarWDL = 0.0
            !-----------------------------------------------------------------------------------------
            
        endif
        !-----------------------------------------------------------------------------------------

        !-----------------------------------------------------------------------------------------
        ! Updating model global variable(s)
        oHMC_Vars(iID)%a2dFlowDeep = a2dVarFlowDeep
        oHMC_Vars(iID)%a2dVTot = a2dVarVTot
        oHMC_Vars(iID)%a2dWTable = a2dVarWTable
        oHMC_Vars(iID)%a2dWSRunoff = a2dVarWSRunoff
        oHMC_Vars(iID)%a2dWDL = a2dVarWDL
        
        ! Info end
        if (iDEBUG.gt.0) then
            call mprintf(.true., iINFO_Extra, ' Phys :: Convolution :: DeepFlow ... OK' )
        endif
        !-----------------------------------------------------------------------------------------

    end subroutine HMC_Phys_Convolution_Apps_DeepFlow_ChannelFraction
    !------------------------------------------------------------------------------------------

end module HMC_Module_Phys_Convolution_Apps_DeepFlow
!------------------------------------------------------------------------------------------
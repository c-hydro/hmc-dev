!------------------------------------------------------------------------------------
! File:   HMC_Module_Phys_Convolution_Apps_SubFlow.f90
!
! Author(s):    Fabio Delogu, Francesco Silvestro, Simone Gabellani
! Date:         20190410
!
! Convolution Apps SubFlow subroutine(s) for HMC model
!------------------------------------------------------------------------------------

!------------------------------------------------------------------------------------
! Module Header
module HMC_Module_Phys_Convolution_Apps_SubFlow

    !------------------------------------------------------------------------------------
    ! External module(s) 
    use HMC_Module_Namelist,                only: oHMC_Namelist
    use HMC_Module_Vars_Loader,             only: oHMC_Vars
    
    use HMC_Module_Tools_Debug
    
    ! Implicit none for all subroutines in this module
    implicit none
    !------------------------------------------------------------------------------------------

contains 

    !------------------------------------------------------------------------------------------
    ! Subroutine for calculating hypodermic flow
    subroutine HMC_Phys_Convolution_Apps_SubFlow(iID, iRows, iCols, dDtDataForcing, dDtAct, iNDam)
        
        !------------------------------------------------------------------------------------------
        ! Variable(s) declaration
        integer(kind = 4)   :: iID, iRows, iCols
        integer(kind = 4)   :: iNDam
        real(kind = 4)      :: dDtDataForcing, dDtAct 
        
        integer(kind = 4)   :: iI, iII, iIII, iJ, iJJ, iJJJ, iD
        !integer(kind = 4)   :: iVarPNT
        
        integer(kind = 4)   :: iFlagFlowDeep
        real(kind = 4)      :: dDtSubflow
        real(kind = 4)      :: dRate, dRateMin
        
        real(kind = 4), dimension (iRows, iCols)            :: a2dVarVTot, a2dVarVTotStep, a2dVarVLoss

        real(kind = 4), dimension (iRows, iCols)            :: a2dVarFlowExf
        !real(kind = 4), dimension (iRows, iCols)            :: a2dVarVSub

        real(kind = 4), dimension (iNDam)                   :: a1dVarVDam
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Initialize variable(s)
        dDtSubflow = 0.0; dRate = 0.0;  dRateMin = 0.0; iFlagFlowDeep = 0;
        
        a2dVarVTot = 0.0; a2dVarVTotStep = 0.0;  a2dVarVLoss = 0.0; 
        a2dVarFlowExf = 0.0
        
        a1dVarVDam = 0.0
        
        oHMC_Vars(iID)%a2dFlowExf = 0.0
        
        !iVarPNT = 0; a2dVarVSub = 0.0;
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Integrating step (Subflow)
        dDtSubflow = dDtAct
        
        ! Hypodermic flow minimum rate
        dRateMin = oHMC_Namelist(iID)%dRateMin
        iFlagFlowDeep = oHMC_Namelist(iID)%iFlagFlowDeep

        ! Dam(s) data from global declaration
        a1dVarVDam = oHMC_Vars(iID)%a1dVDam
 
        ! Variable(s) from global declaration
        a2dVarVTot = oHMC_Vars(iID)%a2dVTot
        !a2dVarVSub = oHMC_Vars(iID)%a2dVSub
        a2dVarVLoss = oHMC_Vars(iID)%a2dVLoss
        
        a2dVarFlowExf = oHMC_Vars(iID)%a2dFlowExf
        
        ! Info start
        if (iDEBUG.gt.0) then
            call mprintf(.true., iINFO_Extra, ' Phys :: Convolution :: SubFlow ... ' )
        endif
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Debug
        if (iDEBUG.gt.0) then
            call mprintf(.true., iINFO_Extra, ' ========= SUBFLOW START ========= ') 
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarVTot, oHMC_Vars(iID)%a2iMask, 'VTOT START ') )
            !call mprintf(.true., iINFO_Extra, checkvar(a2dVarVSub, oHMC_Vars(iID)%a2iMask, 'VSUB START ') )
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarVLoss, oHMC_Vars(iID)%a2iMask, 'VLOSS START ') )
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarFlowExf, oHMC_Vars(iID)%a2iMask, 'FLOWEXF START ') )        
            call mprintf(.true., iINFO_Extra, '')
        endif
        !------------------------------------------------------------------------------------------
         
        !------------------------------------------------------------------------------------------
        ! Conditions on total and loss volume
        where( (oHMC_Vars(iID)%a2iMask.gt.0.0).and.(a2dVarVTot.lt.0.0) ) a2dVarVTot = 0.0
        where( (oHMC_Vars(iID)%a2iMask.gt.0.0).and.(a2dVarVLoss.lt.0.0) ) a2dVarVLoss = 0.0
        !------------------------------------------------------------------------------------------
         
        !------------------------------------------------------------------------------------------
        ! Check dam availability
        if (iNDam .gt. 0) then
            ! Set VTot equal zero into Dam(s) cell(s)
            do iD = 1,iNDam
                iI = 0; iJ = 0;
                iI = oHMC_Vars(iID)%a2iXYDam(iD, 2); iJ = oHMC_Vars(iID)%a2iXYDam(iD, 1)
                !a1dVarVDam(iD) = a1dVarVDam(iD) + a2dVarVTot(iI,iJ)*(oHMC_Vars(iID)%a2dAreaCell(iI,iJ))/1000
                a2dVarVTot(iI,iJ) = 0.0
            enddo
        endif
        !------------------------------------------------------------------------------------------
            
        !------------------------------------------------------------------------------------------
        ! Cycling on each pixels
        ! Calcola il volume di uscita dalla cella nei due casi: a2dV > o < di a2dS;
        ! Qsup � la portata che esce dalla parte superiore della cella e si aggiunge al deflusso superficiale               
        ! il contatore punta alla cella successiva(controllare se vale per l'ultima cella)
        iI = 0; iJ = 0;
        do iJ = 1, iCols
            do iI = 1, iRows
                
                ! DEM condition
                if (oHMC_Vars(iID)%a2iMask(iI,iJ).gt.0.0) then
                    
                    ! Pointers definition
  
                    !iVarPNT = 0
                    !iVarPNT = int(oHMC_Vars(iID)%a2iPNT(iI,iJ))
                    
                    ! Defining flow directions
                    iII = int((int(oHMC_Vars(iID)%a2iPNT(iI,iJ))  - 1)/3) - 1
                    iJJ = int(oHMC_Vars(iID)%a2iPNT(iI,iJ)) - 5 - 3*iII
                    iIII = iI + iII
                    iJJJ = iJ + iJJ
                    
                    ! Debugging ndexes
                    !write(*,*) 'Rate: ',dRate,' iPNT: ',iVarPNT
                    !write(*,*) 'iJ: ',iJ, ' iI: ',iI, ' iJJ: ',iJJ, ' iII: ',iII , ' iJJJ: ',iJJJ, ' iIII: ',iIII
                    
                    ! Calculating VTot and VLoss using flowdeep condition
                    if (iFlagFlowDeep.eq.0) then
                        
                        ! VTot (Vloss == 0)
                        if(iIII.ge.1.and.iJJJ.ge.1) then
                            a2dVarVTotStep(iIII,iJJJ) = a2dVarVTotStep(iIII,iJJJ) + oHMC_Vars(iID)%a2dVSub(iI, iJ)
                        endif
                        
                    else
                        
                        ! Rate definition
                        dRate = 0.0; 
                        dRate = sin(oHMC_Vars(iID)%a2dBeta(iI,iJ))
                        
                        ! Checking rate value
                        if(dRate.gt.0.99)        dRate = 0.99
                        if(dRate.lt.dRateMin)   dRate = dRateMin

                        ! Rescaling dRate according to dRateRescaling set in the namelist
                        !(dRate=fraction to hypodermic flow; (1-dRate)=fraction to percolation)
                        ! No rescaling if dRateRescaling>=0.99; otherwise, dRateRescaling will be the new maximum
                        if(oHMC_Namelist(iID)%dRateRescaling.ge.0.99)  oHMC_Namelist(iID)%dRateRescaling = 0.99
                        dRate = dRateMin + (dRate-dRateMin)/(0.99-dRateMin) *(oHMC_Namelist(iID)%dRateRescaling-dRateMin)
                        
                        ! VTot
                        if(iIII.ge.1.and.iJJJ.ge.1) then
                            a2dVarVTotStep(iIII,iJJJ) = a2dVarVTotStep(iIII,iJJJ) + oHMC_Vars(iID)%a2dVSub(iI, iJ)*dRate
                        endif
                        ! Vloss
                        if(iIII.ge.1.and.iJJJ.ge.1) then 
                            a2dVarVLoss(iIII,iJJJ) = a2dVarVLoss(iIII,iJJJ) + oHMC_Vars(iID)%a2dVSub(iI, iJ)*(1 - dRate)
                        endif
                    
                    endif
                    
                endif
              
            enddo
        enddo
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Updating total volume 
        a2dVarVTot = a2dVarVTot + a2dVarVTotStep
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Checking total volume and calculating exfiltration volume
	where ( (oHMC_Vars(iID)%a2iMask.gt.0.0) .and. (a2dVarVTot.gt.oHMC_Vars(iID)%a2dS) )
            ! Calculating esfiltration flow [m/seconds]
            a2dVarFlowExf = (a2dVarVTot - oHMC_Vars(iID)%a2dS)/(1000.0*dDtSubflow) !in m/sec
            ! Updating total volume information
            a2dVarVTot = 1.0*oHMC_Vars(iID)%a2dS 
        endwhere
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Hypodermic flow to fill cell(s)-lake of the dam
        if (iNDam .gt. 0) then
            do iD = 1,iNdam
                iI = 0; iJ = 0;
                iI = oHMC_Vars(iID)%a2iXYDam(iD,2); iJ = oHMC_Vars(iID)%a2iXYDam(iD,1)
                ! Amount of upstream volume at dam section. V set zero at the subroutine begin
                a1dVarVDam(iD) = a1dVarVDam(iD) + a2dVarVTot(iI,iJ)*(oHMC_Vars(iID)%a2dAreaCell(iI,iJ))/1000
            enddo
        endif
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Debug
        if (iDEBUG.gt.0) then
            call mprintf(.true., iINFO_Extra, '')
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarVTot, oHMC_Vars(iID)%a2iMask, 'VTOT END ') )
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarVTotStep, oHMC_Vars(iID)%a2iMask, 'VTOTSTEP START ') )
            !call mprintf(.true., iINFO_Extra, checkvar(a2dVarVSub, oHMC_Vars(iID)%a2iMask, 'VSUB END ') )
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarVLoss, oHMC_Vars(iID)%a2iMask, 'VLOSS END ') )
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarFlowExf, oHMC_Vars(iID)%a2iMask, 'FLOWEXF END ') ) 
            call mprintf(.true., iINFO_Extra, ' ========= SUBFLOW END ========= ') 
        endif
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Updating variable(s)
        oHMC_Vars(iID)%a2dVTot = a2dVarVTot
        oHMC_Vars(iID)%a2dVLoss = a2dVarVLoss
        
        oHMC_Vars(iID)%a2dFlowExf = a2dVarFlowExf
        
        oHMC_Vars(iID)%a1dVDam = a1dVarVDam
        
        ! Info end
        if (iDEBUG.gt.0) then
            call mprintf(.true., iINFO_Extra, ' Phys :: Convolution :: SubFlow ... OK' )
        endif
        !------------------------------------------------------------------------------------------

    end subroutine HMC_Phys_Convolution_Apps_SubFlow
    !------------------------------------------------------------------------------------------
    
end module HMC_Module_Phys_Convolution_Apps_SubFlow
!------------------------------------------------------------------------------------------

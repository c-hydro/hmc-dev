!------------------------------------------------------------------------------------
! File:   HMC_Module_Phys_Convolution_Apps_Horton.f90
!
! Author(s):    Fabio Delogu, Francesco Silvestro, Simone Gabellani
! Date:         20190410
!
! Convolution Apps Horton subroutine(s) for HMC model
!------------------------------------------------------------------------------------

!------------------------------------------------------------------------------------
! Module Header
module HMC_Module_Phys_Convolution_Apps_Horton

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
    ! Subroutine for calculating infiltration/runoff (using horton modified method) channel network
    subroutine HMC_Phys_Convolution_Apps_Horton_ChannelNetwork(iID, iRows, iCols, dDtDataForcing, dDtAct, iTq, iTime, iNTime)
        
        !------------------------------------------------------------------------------------------
        ! Variable(s) declaration
        integer(kind = 4) :: iID, iRows, iCols
        real(kind = 4) :: dDtDataForcing, dDtAct
        
        real(kind = 4) :: dVarVErr
        real(kind = 4) :: dDtHorton
        real(kind = 4) :: dDomainArea

        real(kind = 4), dimension (iRows, iCols)         :: a2dVarVTot, a2dVarVTotPStep
        real(kind = 4), dimension (iRows, iCols)         :: a2dVarVSub
        real(kind = 4), dimension (iRows, iCols)         :: a2dVarRain, a2dVarIntensity
        
        !real(kind = 4), dimension (iRows, iCols)         :: a2dVarB, a2dVarCh
        real(kind = 4), dimension (iRows, iCols)         :: a2dVarG, a2dVarVErr
        
        real(kind = 4), dimension (iRows, iCols)         :: a2dVarRouting, a2dVarFlowDeep
        
        integer(kind = 4) :: iTq, iTime, iNTime
        !------------------------------------------------------------------------------------------

        !------------------------------------------------------------------------------------------
        ! Variable(s) initialization
        a2dVarVTot = 0.0; a2dVarVTotPStep = 0.0; a2dVarVSub = 0.0
        a2dVarRain = 0.0; a2dVarIntensity = 0.0;
        a2dVarVErr = 0.0;
        a2dVarRouting = 0.0; a2dVarFlowDeep = 0.0;
        
        !a2dVarG = 0.0; a2dVarCh = 0.0;  dVarVErr = 0.0;
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Integrating step (horton)
        dDtHorton = dDtAct
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------ 
        ! Domain Area
        dDomainArea =  oHMC_Vars(iID)%dDomainArea
        
        ! Variable(s) from global declaration
        a2dVarVTot = oHMC_Vars(iID)%a2dVTot
        a2dVarVSub = oHMC_Vars(iID)%a2dVSub
        a2dVarRouting = oHMC_Vars(iID)%a2dRouting
        a2dVarFlowDeep = oHMC_Vars(iID)%a2dFlowDeep
        
        ! Extracting dynamic forcing variable(s)
        a2dVarRain = oHMC_Vars(iID)%a2dRain
        where (a2dVarRain.lt.0.0.or.a2dVarRain.gt.845.0) a2dVarRain = 0.0
            
        ! Info start
        if (iDEBUG.gt.0) then
            call mprintf(.true., iINFO_Extra, ' Phys :: Convolution :: Horton ... ' )
        endif
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Total volume previous step
        a2dVarVTotPStep = a2dVarVTot
        !------------------------------------------------------------------------------------------
         
        !------------------------------------------------------------------------------------------
        ! Horton filter equation
        if (oHMC_Namelist(iID)%iFlagInfiltRateVariable .eq. 2) then
            where (oHMC_Vars(iID)%a2dS.gt.0.0)          
                    a2dVarG = oHMC_Vars(iID)%a2dCostF1 + (oHMC_Vars(iID)%a2dCostF - oHMC_Vars(iID)%a2dCostF1) * &
                              exp( -oHMC_Namelist(iID)%dPowVarInfiltRate * & 
                                   ( a2dVarVTot / (oHMC_Vars(iID)%a2dS-a2dVarVTot) ) )    
            elsewhere
                a2dVarG = 0.0
            endwhere
        else
            where (oHMC_Vars(iID)%a2dS.gt.0.0)
                a2dVarG = oHMC_Vars(iID)%a2dCostF - &
                          (oHMC_Vars(iID)%a2dCostF - oHMC_Vars(iID)%a2dCostF1)/oHMC_Vars(iID)%a2dS*a2dVarVTot
            elsewhere
                a2dVarG = 0.0
            endwhere
        endif
        !------------------------------------------------------------------------------------------
                
        !------------------------------------------------------------------------------------------
        ! Debug
        if (iDEBUG.gt.0) then
            call mprintf(.true., iINFO_Extra, ' ========= HORTON START =========== ') 
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarRouting, oHMC_Vars(iID)%a2iMask, 'ROUTING START') )
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarIntensity, oHMC_Vars(iID)%a2iMask, 'INTENSITY START') )
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarVTot, oHMC_Vars(iID)%a2iMask, 'VTOT START') )
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarVSub, oHMC_Vars(iID)%a2iMask, 'VSUB START') )
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarFlowDeep, oHMC_Vars(iID)%a2iMask, 'FLOWDEEP START') )
            call mprintf(.true., iINFO_Extra, checkvar(oHMC_Vars(iID)%a2dHydro, oHMC_Vars(iID)%a2iMask, 'HYDRO UPD START') )
            call mprintf(.true., iINFO_Extra, checkvar(oHMC_Vars(iID)%a2dHydroPrev, oHMC_Vars(iID)%a2iMask, 'HYDRO PREV START') )
            call mprintf(.true., iINFO_Extra, checkvar(oHMC_Vars(iID)%a2dQDisOut, oHMC_Vars(iID)%a2iMask, 'QOUT START (Qtot)') )
            call mprintf(.true., iINFO_Extra, ' ') 
            call mprintf(.true., iINFO_Extra, checkvar(oHMC_Vars(iID)%a2dCostChFix, oHMC_Vars(iID)%a2iMask, 'CHFIX ') )
            call mprintf(.true., iINFO_Extra, checkvar(oHMC_Vars(iID)%a2dCostF, oHMC_Vars(iID)%a2iMask, 'COSTF ') )
            call mprintf(.true., iINFO_Extra, checkvar(oHMC_Vars(iID)%a2dCostF1, oHMC_Vars(iID)%a2iMask, 'COSTF1 ') )
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarG, oHMC_Vars(iID)%a2iMask, 'G ') )
            !call mprintf(.true., iINFO_Extra, checkvar(a2dVarB, oHMC_Vars(iID)%a2iMask, 'B ') )
            !call mprintf(.true., iINFO_Extra, checkvar(a2dVarCh, oHMC_Vars(iID)%a2iMask, 'CH ') )
            call mprintf(.true., iINFO_Extra, checkvar(oHMC_Vars(iID)%a2dS, oHMC_Vars(iID)%a2iMask, 'S ') )
            call mprintf(.true., iINFO_Extra, checkvar(oHMC_Vars(iID)%a2dCoeffResol, oHMC_Vars(iID)%a2iMask, 'COEFF RESOL') )
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarRain, oHMC_Vars(iID)%a2iMask, 'RAIN ') )
            call mprintf(.true., iINFO_Extra, ' ') 
        endif
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Defining subterranean volume 
        where ( (a2dVarVTot.lt.oHMC_Vars(iID)%a2dCt*oHMC_Vars(iID)%a2dS) .and. (oHMC_Vars(iID)%a2iMask.gt.0.0) )
            a2dVarVSub = 0.0
        elsewhere(oHMC_Vars(iID)%a2iMask.gt.0.0)
            ! *dDth/3600 perch� a2dCostF1 � in mm/h ma lavoro in mm/dDth
            a2dVarVSub = oHMC_Vars(iID)%a2dF2*(a2dVarVTot - oHMC_Vars(iID)%a2dCt*oHMC_Vars(iID)%a2dS)/ &
                         oHMC_Vars(iID)%a2dS*dDtHorton/3600   
            ! giulia prova
            !a2dVarVSub = 100.0/(1 - oHMC_Vars(iID)%a2dCt)*(a2dVarVTot - oHMC_Vars(iID)%a2dCt*oHMC_Vars(iID)%a2dS)/ &
            !             oHMC_Vars(iID)%a2dS*dDtHorton/3600   
        endwhere   
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Evaluating intensity --> horton initialization = rain + routing drained cells
        a2dVarIntensity = a2dVarRain*3600.0/dDtDataForcing + &
                          oHMC_Vars(iID)%a2dCoeffResol*a2dVarRouting/dDtHorton*3600.0 + &
                          a2dVarFlowDeep*3600.0/dDtDataForcing
        ! Evaluating intensity ( --- development mode --- )                  
        !a2dVarIntensity = a2dVarRain*3600.0/dDtDataForcing + &
        !                  oHMC_Vars(iID)%a2dCoeffResol*(a2dVarRouting/dDtHorton*3600.0 + &
        !                  a2dVarFlowDeep*3600.0/dDtDataForcing)
        !-------------------------------------------------------------------------------
        
        ! DA QUI GIULIA - ATTENZIONE costruito senza WiltingPoint (anche il vecchio)!!!
        !------------------------------------------------------------------------------------------
        ! Intensity Evaluation
        ! Condition ----> Intensity == 0        
        where ( (a2dVarIntensity.eq.0.0) .and. (oHMC_Vars(iID)%a2iMask.gt.0.0) .and. &
                (a2dVarVTot.ge.oHMC_Vars(iID)%a2dCt*oHMC_Vars(iID)%a2dS) )     
        
                where (a2dVarVtot.ge.(oHMC_Vars(iID)%a2dCt*oHMC_Vars(iID)%a2dS + a2dVarVSub))
                    a2dVarVTot = a2dVarVTot - a2dVarVSub	
                elsewhere
                    a2dVarVSub = a2dVarVTot - oHMC_Vars(iID)%a2dCt*oHMC_Vars(iID)%a2dS
                    a2dVarVTot = oHMC_Vars(iID)%a2dCt*oHMC_Vars(iID)%a2dS
                endwhere
                         
        endwhere                  
           
        ! Condition ----> 0 < Intensity <= G
        where ( (a2dVarIntensity.gt.0.0) .and. (a2dVarIntensity.le.a2dVarG) .and. (oHMC_Vars(iID)%a2iMask.gt.0.0) ) 
        
                where ( (a2dVarVTot.lt.oHMC_Vars(iID)%a2dCt*oHMC_Vars(iID)%a2dS) .and. (oHMC_Vars(iID)%a2iMask.gt.0.0) )
                        a2dVarVTot = a2dVarVTot + a2dVarRain/dDtDataForcing*dDtHorton + &
                                     oHMC_Vars(iID)%a2dCoeffResol*a2dVarRouting + &
                                     a2dVarFlowDeep/dDtDataForcing*dDtHorton
                        a2dVarIntensity = 0.0
                elsewhere(oHMC_Vars(iID)%a2iMask.gt.0.0)
                        a2dVarVTot = a2dVarVTot + a2dVarIntensity*dDtHorton/3600.0
                        a2dVarIntensity = 0.0
                        where (a2dVarVtot.ge.(oHMC_Vars(iID)%a2dCt*oHMC_Vars(iID)%a2dS + a2dVarVSub))
                            a2dVarVTot = a2dVarVTot - a2dVarVSub	
                        elsewhere
                            a2dVarVSub = a2dVarVTot - oHMC_Vars(iID)%a2dCt*oHMC_Vars(iID)%a2dS
                            a2dVarVTot = oHMC_Vars(iID)%a2dCt*oHMC_Vars(iID)%a2dS
                        endwhere
                endwhere
                
        endwhere
        
        ! Condition ----> Intensity > G	
        where ( (a2dVarIntensity.gt.a2dVarG) .and. (oHMC_Vars(iID)%a2iMask.gt.0.0) ) 						
        
                where ( (a2dVarVTot.lt.oHMC_Vars(iID)%a2dCt*oHMC_Vars(iID)%a2dS) .and. (oHMC_Vars(iID)%a2iMask.gt.0.0) )
                        a2dVarVTot = a2dVarVTot + a2dVarG*dDtHorton/3600.0              
                elsewhere (oHMC_Vars(iID)%a2iMask.gt.0.0)
                        a2dVarVTot = a2dVarVTot + a2dVarG*dDtHorton/3600.0                        
                endwhere
                where (a2dVarVtot.ge.(oHMC_Vars(iID)%a2dCt*oHMC_Vars(iID)%a2dS + a2dVarVSub))
                        a2dVarVTot = a2dVarVTot - a2dVarVSub	
                elsewhere
                        a2dVarVSub = a2dVarVTot - oHMC_Vars(iID)%a2dCt*oHMC_Vars(iID)%a2dS
                        a2dVarVTot = oHMC_Vars(iID)%a2dCt*oHMC_Vars(iID)%a2dS
                endwhere
                a2dVarIntensity = a2dVarIntensity - a2dVarG
        
        endwhere						
        !------------------------------------------------------------------------------------------
        ! FINE VERSIONE GIULIA
        
                          
        !!------------------------------------------------------------------------------------------
        !! Intensity Evaluation
        !! Condition ----> Intensity == 0
        !where ( (a2dVarIntensity.eq.0.0) .and. (oHMC_Vars(iID)%a2iMask.gt.0.0) .and. &
        !        (a2dVarVTot.ge.oHMC_Vars(iID)%a2dCt*oHMC_Vars(iID)%a2dS) )     
        !        
        !    a2dVarVTot = a2dVarVTot - oHMC_Vars(iID)%a2dF2*(a2dVarVTot - oHMC_Vars(iID)%a2dCt*oHMC_Vars(iID)%a2dS)/ &
        !                 oHMC_Vars(iID)%a2dS*dDtHorton/3600.0	
        !    
        !endwhere
        !	
        !! Condition ----> 0 < Intensity <= G					
        !where ( (a2dVarIntensity.gt.0.0) .and. (a2dVarIntensity.le.a2dVarG) .and. (oHMC_Vars(iID)%a2iMask.gt.0.0) ) 
        !
        !        where ( (a2dVarVTot.lt.oHMC_Vars(iID)%a2dCt*oHMC_Vars(iID)%a2dS) .and. (oHMC_Vars(iID)%a2iMask.gt.0.0) )
        !                a2dVarVTot = a2dVarVTot + a2dVarRain/dDtDataForcing*dDtHorton + &
        !                             oHMC_Vars(iID)%a2dCoeffResol*a2dVarRouting + &
        !                             a2dVarFlowDeep/dDtDataForcing*dDtHorton
        !                a2dVarIntensity = 0.0
        !        elsewhere(oHMC_Vars(iID)%a2iMask.gt.0.0)
        !                a2dVarVTot = a2dVarVTot + a2dVarIntensity*dDtHorton/3600.0 - a2dVarVSub
        !                a2dVarIntensity = 0.0
        !        endwhere
        !        
        !endwhere
        !
        !! Condition ----> Intensity > G	
        !where ( (a2dVarIntensity.gt.a2dVarG) .and. (oHMC_Vars(iID)%a2iMask.gt.0.0) ) 						
        !
        !        where ( (a2dVarVTot.lt.oHMC_Vars(iID)%a2dCt*oHMC_Vars(iID)%a2dS) .and. (oHMC_Vars(iID)%a2iMask.gt.0.0) )
        !                a2dVarVTot = a2dVarVTot + a2dVarG*dDtHorton/3600.0              
        !        elsewhere (oHMC_Vars(iID)%a2iMask.gt.0.0)
        !                a2dVarVTot = a2dVarVTot + a2dVarG*dDtHorton/3600.0 - a2dVarVSub           
        !        endwhere
        !        
        !        a2dVarIntensity = a2dVarIntensity - a2dVarG
        !
        !endwhere			
        !!------------------------------------------------------------------------------------------

        !------------------------------------------------------------------------------------------
        ! Updating intensity variable
        where ( (a2dVarVTot.gt.oHMC_Vars(iID)%a2dS) .and. (oHMC_Vars(iID)%a2iMask.gt.0.0) )
                a2dVarIntensity =  a2dVarIntensity + (a2dVarVTot - oHMC_Vars(iID)%a2dS)/dDtHorton*3600.0
                a2dVarVTot = oHMC_Vars(iID)%a2dS
        endwhere
        !------------------------------------------------------------------------------------------

        !------------------------------------------------------------------------------------------
        ! Calculating mass balance errors
        where ( (a2dVarRain.lt.0.0) .and. (oHMC_Vars(iID)%a2iMask.gt.0.0) ) a2dVarRain = 0.0 ! Checking rain
        
        ! Calculating Volume error
        where ( oHMC_Vars(iID)%a2iMask.gt.0.0 )
            
                a2dVarVErr = a2dVarFlowDeep/dDtDataForcing*dDtHorton + &
                             a2dVarRain/dDtDataForcing*dDtHorton + oHMC_Vars(iID)%a2dCoeffResol*a2dVarRouting - &
                             a2dVarIntensity*dDtHorton/3600.0 - &
                             (a2dVarVTot - a2dVarVTotPStep + a2dVarVSub)

                where (a2dVarVTot.lt.0.0)
                        a2dVarVTot = 0.0
                endwhere

        endwhere
        
        ! Cumulative mean error
        !dVarVErr = dVarVErr + SUM(SUM(a2dVarVErr, DIM=1, mask=oHMC_Vars(iID)%a2dDEM.gt.0.0))/dDomainArea !DIM=1 columns
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Debug
        if (iDEBUG.gt.0) then
            call mprintf(.true., iINFO_Extra, '')
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarRouting, oHMC_Vars(iID)%a2iMask, 'ROUTING END') )
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarIntensity, oHMC_Vars(iID)%a2iMask, 'INTENSITY END') )
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarVTot, oHMC_Vars(iID)%a2iMask, 'VTOT END') )
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarVSub, oHMC_Vars(iID)%a2iMask, 'VSUB END') )
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarFlowDeep, oHMC_Vars(iID)%a2iMask, 'FLOWDEEP END') )
            call mprintf(.true., iINFO_Extra, checkvar(oHMC_Vars(iID)%a2dHydro, oHMC_Vars(iID)%a2iMask, 'HYDRO UPD END') )
            call mprintf(.true., iINFO_Extra, checkvar(oHMC_Vars(iID)%a2dHydroPrev, oHMC_Vars(iID)%a2iMask, 'HYDRO PREV END') )
            call mprintf(.true., iINFO_Extra, checkvar(oHMC_Vars(iID)%a2dQDisOut, oHMC_Vars(iID)%a2iMask, 'QOUT END (Qtot)') )
            call mprintf(.true., iINFO_Extra, ' ========= HORTON END =========== ') 
        endif
        !------------------------------------------------------------------------------------------

        !------------------------------------------------------------------------------------------
        ! Updating field(s) in global declaration
        oHMC_Vars(iID)%a2dVTot = a2dVarVTot
        oHMC_Vars(iID)%a2dVSub = a2dVarVSub
       
        oHMC_Vars(iID)%a2dIntensity = a2dVarIntensity
        oHMC_Vars(iID)%a2dFlowDeep = a2dVarFlowDeep
        
        oHMC_Vars(iID)%a2dRain = a2dVarRain
        
        oHMC_Vars(iID)%a2dVErr = a2dVarVErr
        !oHMC_Vars(iID)%dVErr = dVarVErr
        
        ! Info end
        if (iDEBUG.gt.0) then
            call mprintf(.true., iINFO_Extra, ' Phys :: Convolution :: Horton ... OK' )
        endif
        !------------------------------------------------------------------------------------------
        
    end subroutine HMC_Phys_Convolution_Apps_Horton_ChannelNetwork
    !------------------------------------------------------------------------------------
    
    !------------------------------------------------------------------------------------------
    ! Subroutine for calculating infiltration/runoff (using horton modified method) channel fraction
    subroutine HMC_Phys_Convolution_Apps_Horton_ChannelFraction(iID, iRows, iCols, dDtDataForcing, dDtAct, iTq, iTime, iNTime)
        
        !------------------------------------------------------------------------------------------
        ! Variable(s) declaration
        integer(kind = 4) :: iID, iRows, iCols
        real(kind = 4) :: dDtDataForcing, dDtAct
        
        real(kind = 4) :: dVarVErr,dTmp
        real(kind = 4) :: dDtHorton
        real(kind = 4) :: dDomainArea

        real(kind = 4), dimension (iRows, iCols)         :: a2dVarVTot, a2dVarVTotPStep
        real(kind = 4), dimension (iRows, iCols)         :: a2dVarVSub
        real(kind = 4), dimension (iRows, iCols)         :: a2dVarRain, a2dVarIntensity
        
        !real(kind = 4), dimension (iRows, iCols)         :: a2dVarB, a2dVarCh
        real(kind = 4), dimension (iRows, iCols)         :: a2dVarG, a2dVarVErr
        
        real(kind = 4), dimension (iRows, iCols)         :: a2dVarRouting, a2dVarFlowDeep
        
        integer(kind = 4) :: iTq, iTime, iNTime
        !------------------------------------------------------------------------------------------

        !------------------------------------------------------------------------------------------
        ! Variable(s) initialization
        a2dVarVTot = 0.0; a2dVarVTotPStep = 0.0; a2dVarVSub = 0.0
        a2dVarRain = 0.0; a2dVarIntensity = 0.0;
        a2dVarVErr = 0.0;
        a2dVarRouting = 0.0; a2dVarFlowDeep = 0.0;
        
        !a2dVarG = 0.0; a2dVarCh = 0.0;  dVarVErr = 0.0;
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Integrating step (horton)
        dDtHorton = dDtAct
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------ 
        ! Domain Area
        dDomainArea =  oHMC_Vars(iID)%dDomainArea
        
        ! Variable(s) from global declaration
        a2dVarVTot = oHMC_Vars(iID)%a2dVTot
        a2dVarVSub = oHMC_Vars(iID)%a2dVSub
        a2dVarRouting = oHMC_Vars(iID)%a2dRouting
        a2dVarFlowDeep = oHMC_Vars(iID)%a2dFlowDeep
        
        ! Extracting dynamic forcing variable(s)
        a2dVarRain = oHMC_Vars(iID)%a2dRain
        where (a2dVarRain.lt.0.0.or.a2dVarRain.gt.845.0) a2dVarRain = 0.0
        
            
        !dTmp=MAXVAL(MAXVAL(a2dVarRain,dim=1,mask=oHMC_Vars(iID)%a2iChoice.ge.0.and. &
         !   oHMC_Vars(iID)%a2dDem.gt.0))  
               
        ! Info start
        if (iDEBUG.gt.0) then
            call mprintf(.true., iINFO_Extra, ' Phys :: Convolution :: Horton ... ' )
        endif
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Total volume previous step
        a2dVarVTotPStep = a2dVarVTot
        !------------------------------------------------------------------------------------------
         
        !------------------------------------------------------------------------------------------
        ! Horton filter equation
        if (oHMC_Namelist(iID)%iFlagInfiltRateVariable .eq. 2) then
            where (oHMC_Vars(iID)%a2dS.gt.0.0)          
                    a2dVarG = oHMC_Vars(iID)%a2dCostF1 + (oHMC_Vars(iID)%a2dCostF - oHMC_Vars(iID)%a2dCostF1) * &
                              exp( -oHMC_Namelist(iID)%dPowVarInfiltRate * & 
                                   ( a2dVarVTot / (oHMC_Vars(iID)%a2dS-a2dVarVTot) ) )    
            elsewhere
                a2dVarG = 0.0
            endwhere
        else
            where (oHMC_Vars(iID)%a2dS.gt.0.0)
                a2dVarG = oHMC_Vars(iID)%a2dCostF - &
                         (oHMC_Vars(iID)%a2dCostF - oHMC_Vars(iID)%a2dCostF1)/oHMC_Vars(iID)%a2dS*a2dVarVTot
            elsewhere
                a2dVarG = 0.0
            endwhere
        endif
        !------------------------------------------------------------------------------------------
                
        !------------------------------------------------------------------------------------------
        ! Debug
        if (iDEBUG.gt.0) then
            call mprintf(.true., iINFO_Extra, ' ========= HORTON START =========== ') 
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarRouting, oHMC_Vars(iID)%a2iMask, 'ROUTING START') )
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarIntensity, oHMC_Vars(iID)%a2iMask, 'INTENSITY START') )
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarVTot, oHMC_Vars(iID)%a2iMask, 'VTOT START') )
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarVSub, oHMC_Vars(iID)%a2iMask, 'VSUB START') )
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarFlowDeep, oHMC_Vars(iID)%a2iMask, 'FLOWDEEP START') )
            call mprintf(.true., iINFO_Extra, checkvar(oHMC_Vars(iID)%a2dHydro, oHMC_Vars(iID)%a2iMask, 'HYDRO UPD START') )
            call mprintf(.true., iINFO_Extra, checkvar(oHMC_Vars(iID)%a2dHydroPrev, oHMC_Vars(iID)%a2iMask, 'HYDRO PREV START') )
            call mprintf(.true., iINFO_Extra, checkvar(oHMC_Vars(iID)%a2dQDisOut, oHMC_Vars(iID)%a2iMask, 'QOUT START (Qtot)') )
            call mprintf(.true., iINFO_Extra, ' ') 
            call mprintf(.true., iINFO_Extra, checkvar(oHMC_Vars(iID)%a2dCostChFix, oHMC_Vars(iID)%a2iMask, 'CHFIX ') )
            call mprintf(.true., iINFO_Extra, checkvar(oHMC_Vars(iID)%a2dCostF, oHMC_Vars(iID)%a2iMask, 'COSTF ') )
            call mprintf(.true., iINFO_Extra, checkvar(oHMC_Vars(iID)%a2dCostF1, oHMC_Vars(iID)%a2iMask, 'COSTF1 ') )
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarG, oHMC_Vars(iID)%a2iMask, 'G ') )
            !call mprintf(.true., iINFO_Extra, checkvar(a2dVarB, oHMC_Vars(iID)%a2iMask, 'B ') )
            !call mprintf(.true., iINFO_Extra, checkvar(a2dVarCh, oHMC_Vars(iID)%a2iMask, 'CH ') )
            call mprintf(.true., iINFO_Extra, checkvar(oHMC_Vars(iID)%a2dS, oHMC_Vars(iID)%a2iMask, 'S ') )
            call mprintf(.true., iINFO_Extra, checkvar(oHMC_Vars(iID)%a2dCoeffResol, oHMC_Vars(iID)%a2iMask, 'COEFF RESOL') )
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarRain, oHMC_Vars(iID)%a2iMask, 'RAIN ') )
            call mprintf(.true., iINFO_Extra, ' ') 
        endif
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Defining subterranean volume 
        where ( (a2dVarVTot.lt.oHMC_Vars(iID)%a2dCt*oHMC_Vars(iID)%a2dS) .and. (oHMC_Vars(iID)%a2iMask.gt.0.0) )
            a2dVarVSub = 0.0
        elsewhere(oHMC_Vars(iID)%a2iMask.gt.0.0)
            ! *dDth/3600 perch� a2dCostF1 � in mm/h ma lavoro in mm/dDth
            a2dVarVSub = oHMC_Vars(iID)%a2dF2*(a2dVarVTot - oHMC_Vars(iID)%a2dCt*oHMC_Vars(iID)%a2dS)/ &
                         oHMC_Vars(iID)%a2dS*dDtHorton/3600  
        endwhere   
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Evaluating intensity --> horton initialization = rain + routing from hillslope + portion of Deep flow
        a2dVarIntensity = a2dVarRain*3600.0/dDtDataForcing + &
                          a2dVarRouting/dDtHorton*3600.0 + &
                          a2dVarFlowDeep*3600.0/dDtDataForcing*oHMC_Vars(iID)%a2dWidthH/sqrt(oHMC_Vars(iID)%a2dAreaCell)
        !-------------------------------------------------------------------------------
         
        ! DA QUI GIULIA - ATTENZIONE costruito senza WiltingPoint (anche il vecchio)!!!
        !------------------------------------------------------------------------------------------
        ! Intensity Evaluation
        ! Condition ----> Intensity == 0        
        where ( (a2dVarIntensity.eq.0.0) .and. (oHMC_Vars(iID)%a2iMask.gt.0.0) .and. &
                (a2dVarVTot.ge.oHMC_Vars(iID)%a2dCt*oHMC_Vars(iID)%a2dS) )     
        
                where (a2dVarVtot.ge.(oHMC_Vars(iID)%a2dCt*oHMC_Vars(iID)%a2dS + a2dVarVSub))
                    a2dVarVTot = a2dVarVTot - a2dVarVSub	
                elsewhere
                    a2dVarVSub = a2dVarVTot - oHMC_Vars(iID)%a2dCt*oHMC_Vars(iID)%a2dS
                    a2dVarVTot = oHMC_Vars(iID)%a2dCt*oHMC_Vars(iID)%a2dS
                endwhere
                         
        endwhere    
          
        ! Condition ----> 0 < Intensity <= G
        where ( (a2dVarIntensity.gt.0.0) .and. (a2dVarIntensity.le.a2dVarG) .and. (oHMC_Vars(iID)%a2iMask.gt.0.0) ) 
        
                where ( (a2dVarVTot.lt.oHMC_Vars(iID)%a2dCt*oHMC_Vars(iID)%a2dS) .and. (oHMC_Vars(iID)%a2iMask.gt.0.0) )
                        a2dVarVTot = a2dVarVTot + a2dVarRain/dDtDataForcing*dDtHorton + &
                                     a2dVarRouting + &
                                     a2dVarFlowDeep/dDtDataForcing*dDtHorton* &
                                     oHMC_Vars(iID)%a2dWidthH/sqrt(oHMC_Vars(iID)%a2dAreaCell)
                        a2dVarIntensity = 0.0
                elsewhere(oHMC_Vars(iID)%a2iMask.gt.0.0)
                        a2dVarVTot = a2dVarVTot + a2dVarIntensity*dDtHorton/3600.0
                        a2dVarIntensity = 0.0
                        where (a2dVarVtot.ge.(oHMC_Vars(iID)%a2dCt*oHMC_Vars(iID)%a2dS + a2dVarVSub))
                            a2dVarVTot = a2dVarVTot - a2dVarVSub	
                        elsewhere
                            a2dVarVSub = a2dVarVTot - oHMC_Vars(iID)%a2dCt*oHMC_Vars(iID)%a2dS
                            a2dVarVTot = oHMC_Vars(iID)%a2dCt*oHMC_Vars(iID)%a2dS
                        endwhere
                endwhere
                
        endwhere
                
        ! Condition ----> Intensity > G	
        where ( (a2dVarIntensity.gt.a2dVarG) .and. (oHMC_Vars(iID)%a2iMask.gt.0.0) ) 						
        
                where ( (a2dVarVTot.lt.oHMC_Vars(iID)%a2dCt*oHMC_Vars(iID)%a2dS) .and. (oHMC_Vars(iID)%a2iMask.gt.0.0) )
                        a2dVarVTot = a2dVarVTot + a2dVarG*dDtHorton/3600.0              
                elsewhere (oHMC_Vars(iID)%a2iMask.gt.0.0)
                        a2dVarVTot = a2dVarVTot + a2dVarG*dDtHorton/3600.0
                        where (a2dVarVtot.ge.(oHMC_Vars(iID)%a2dCt*oHMC_Vars(iID)%a2dS + a2dVarVSub))
                            a2dVarVTot = a2dVarVTot - a2dVarVSub	
                        elsewhere
                            a2dVarVSub = a2dVarVTot - oHMC_Vars(iID)%a2dCt*oHMC_Vars(iID)%a2dS
                            a2dVarVTot = oHMC_Vars(iID)%a2dCt*oHMC_Vars(iID)%a2dS
                        endwhere
                endwhere
                a2dVarIntensity = a2dVarIntensity - a2dVarG
        
        endwhere						
        !------------------------------------------------------------------------------------------
        ! FINE VERSIONE GIULIA                  
                              
        !!------------------------------------------------------------------------------------------
        !! Intensity Evaluation
        !! Condition ----> Intensity == 0
        !where ( (a2dVarIntensity.eq.0.0) .and. (oHMC_Vars(iID)%a2iMask.gt.0.0) .and. &
        !        (a2dVarVTot.ge.oHMC_Vars(iID)%a2dCt*oHMC_Vars(iID)%a2dS) )     
        !        
        !    a2dVarVTot = a2dVarVTot - oHMC_Vars(iID)%a2dF2*(a2dVarVTot - oHMC_Vars(iID)%a2dCt*oHMC_Vars(iID)%a2dS)/ &
        !                 oHMC_Vars(iID)%a2dS*dDtHorton/3600.0	
        !    
        !endwhere
        !	
        !! Condition ----> 0 < Intensity <= G					
        !where ( (a2dVarIntensity.gt.0.0) .and. (a2dVarIntensity.le.a2dVarG) .and. (oHMC_Vars(iID)%a2iMask.gt.0.0) ) 
        !
        !        where ( (a2dVarVTot.lt.oHMC_Vars(iID)%a2dCt*oHMC_Vars(iID)%a2dS) .and. (oHMC_Vars(iID)%a2iMask.gt.0.0) )
        !                a2dVarVTot = a2dVarVTot + a2dVarRain/dDtDataForcing*dDtHorton + &
        !                             a2dVarRouting + &
        !                       a2dVarFlowDeep/dDtDataForcing*dDtHorton*oHMC_Vars(iID)%a2dWidthH/sqrt(oHMC_Vars(iID)%a2dAreaCell)
        !                a2dVarIntensity = 0.0
        !        elsewhere(oHMC_Vars(iID)%a2iMask.gt.0.0)
        !                a2dVarVTot = a2dVarVTot + a2dVarIntensity*dDtHorton/3600.0 - a2dVarVSub
        !                a2dVarIntensity = 0.0
        !        endwhere
        !        
        !endwhere
        !
        !! Condition ----> Intensity > G	
        !where ( (a2dVarIntensity.gt.a2dVarG) .and. (oHMC_Vars(iID)%a2iMask.gt.0.0) ) 						
        !
        !        where ( (a2dVarVTot.lt.oHMC_Vars(iID)%a2dCt*oHMC_Vars(iID)%a2dS) .and. (oHMC_Vars(iID)%a2iMask.gt.0.0) )
        !                a2dVarVTot = a2dVarVTot + a2dVarG*dDtHorton/3600.0              
        !        elsewhere (oHMC_Vars(iID)%a2iMask.gt.0.0)
        !                a2dVarVTot = a2dVarVTot + a2dVarG*dDtHorton/3600.0 - a2dVarVSub           
        !        endwhere
        !        
        !        a2dVarIntensity = a2dVarIntensity - a2dVarG
        !
        !endwhere			
        !!------------------------------------------------------------------------------------------

        !------------------------------------------------------------------------------------------
        ! Updating intensity variable
        where ( (a2dVarVTot.gt.oHMC_Vars(iID)%a2dS) .and. (oHMC_Vars(iID)%a2iMask.gt.0.0) )
                a2dVarIntensity =  a2dVarIntensity + (a2dVarVTot - oHMC_Vars(iID)%a2dS)/dDtHorton*3600.0
                a2dVarVTot = oHMC_Vars(iID)%a2dS
                
        endwhere
        
        ! Runoff in m/s
        where ( oHMC_Vars(iID)%a2iMask.gt.0.0 )
                oHMC_Vars(iID)%a2dRunoffH=a2dVarIntensity/(1000.0*3600.0)
        endwhere
        !------------------------------------------------------------------------------------------
        
        
        !------------------------------------------------------------------------------------------
        ! Calculating mass balance errors
        where ( (a2dVarRain.lt.0.0) .and. (oHMC_Vars(iID)%a2iMask.gt.0.0) ) a2dVarRain = 0.0 ! Checking rain
        
        ! Calculating Volume error
        where ( oHMC_Vars(iID)%a2iMask.gt.0.0 )
            
                a2dVarVErr = a2dVarFlowDeep/dDtDataForcing*dDtHorton + &
                             a2dVarRain/dDtDataForcing*dDtHorton + oHMC_Vars(iID)%a2dCoeffResol*a2dVarRouting - &
                             a2dVarIntensity*dDtHorton/3600.0 - &
                             (a2dVarVTot - a2dVarVTotPStep + a2dVarVSub)

                where (a2dVarVTot.lt.0.0)
                        a2dVarVTot = 0.0
                endwhere

        endwhere
        
        ! Cumulative mean error
        !dVarVErr = dVarVErr + SUM(SUM(a2dVarVErr, DIM=1, mask=oHMC_Vars(iID)%a2dDEM.gt.0.0))/dDomainArea !DIM=1 columns
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Debug
        if (iDEBUG.gt.0) then
            call mprintf(.true., iINFO_Extra, '')
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarRouting, oHMC_Vars(iID)%a2iMask, 'ROUTING END') )
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarIntensity, oHMC_Vars(iID)%a2iMask, 'INTENSITY END') )
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarVTot, oHMC_Vars(iID)%a2iMask, 'VTOT END') )
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarVSub, oHMC_Vars(iID)%a2iMask, 'VSUB END') )
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarFlowDeep, oHMC_Vars(iID)%a2iMask, 'FLOWDEEP END') )
            call mprintf(.true., iINFO_Extra, checkvar(oHMC_Vars(iID)%a2dHydro, oHMC_Vars(iID)%a2iMask, 'HYDRO UPD END') )
            call mprintf(.true., iINFO_Extra, checkvar(oHMC_Vars(iID)%a2dHydroPrev, oHMC_Vars(iID)%a2iMask, 'HYDRO PREV END') )
            call mprintf(.true., iINFO_Extra, checkvar(oHMC_Vars(iID)%a2dQDisOut, oHMC_Vars(iID)%a2iMask, 'QOUT END (Qtot)') )
            call mprintf(.true., iINFO_Extra, ' ========= HORTON END =========== ') 
        endif
        !------------------------------------------------------------------------------------------

        !------------------------------------------------------------------------------------------
        ! Updating field(s) in global declaration
        oHMC_Vars(iID)%a2dVTot = a2dVarVTot
        oHMC_Vars(iID)%a2dVSub = a2dVarVSub
       
        oHMC_Vars(iID)%a2dIntensity = a2dVarIntensity
        oHMC_Vars(iID)%a2dFlowDeep = a2dVarFlowDeep
        
        oHMC_Vars(iID)%a2dRain = a2dVarRain
        
        oHMC_Vars(iID)%a2dVErr = a2dVarVErr
        !oHMC_Vars(iID)%dVErr = dVarVErr
        
        ! Info end
        if (iDEBUG.gt.0) then
            call mprintf(.true., iINFO_Extra, ' Phys :: Convolution :: Horton ... OK' )
        endif
        !------------------------------------------------------------------------------------------
        
    end subroutine HMC_Phys_Convolution_Apps_Horton_ChannelFraction
    !------------------------------------------------------------------------------------
        
end module HMC_Module_Phys_Convolution_Apps_Horton
!------------------------------------------------------------------------------------------
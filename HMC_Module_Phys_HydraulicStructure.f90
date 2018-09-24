!------------------------------------------------------------------------------------------    
! File:   HMC_Module_Phys_HydraulicStructure.f90
! Author: fabio
!
! Created on February 24, 2016, 2:11 PM
!------------------------------------------------------------------------------------------

!------------------------------------------------------------------------------------------
! Module Header
module HMC_Module_Phys_HydraulicStructure
    
    !------------------------------------------------------------------------------------------
    ! External module(s) for all subroutine in this module
    use HMC_Module_Namelist, only: oHMC_Namelist
    use HMC_Module_Vars_Loader, only: oHMC_Vars
    
    use HMC_Module_Tools_Debug
    
    ! Implicit none for all subroutines in this module
    implicit none
    !------------------------------------------------------------------------------------------
    
contains
    
    !------------------------------------------------------------------------------------------
    ! Subroutine for calculating dam volume spilling
    subroutine HMC_Phys_Dam_Spilling(iID, iNDam, dDt, &
                                        a1dVarVDam, a1dVarQoutDam, a1dVarCoeffDam)
                                                    
        !------------------------------------------------------------------------------------------
        ! Variable(s)
        integer(kind = 4)           :: iID
        integer(kind = 4)           :: iI, iJ, iD, iK, iRank  !aggiunte variabili
        integer(kind = 4)           :: iNDam
        integer(kind = 4)           :: iFlagD, iMaxC !aggiunte variabili
        real(kind = 4)              :: dDt, dQtmp, dQsprec  !aggiunte variabili
        
        real(kind = 4), dimension (iNDam)           :: a1dVarVDam 
        real(kind = 4), dimension (iNDam)           :: a1dVarQoutDam, a1dVarCoeffDam
        real(kind = 4), dimension (iNDam)           :: a1dVarHDam, a1dVarLDam !aggiunte variabili
        
        character(len = 256)                        :: sQtmp, sD, sHDam, sHMaxDam  !aggiunte variabili
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Initialize variable(s)
        a1dVarQoutDam = 0.0; 
        
        dQtmp = 0.0;
        iFlagD = 0; 
        sQtmp = ''; sHDam = ''; sHMaxDam = '';        
        !------------------------------------------------------------------------------------------
        !------------------------------------------------------------------------------------------
        ! Re-initialize dam height (part added to compute Q of spilling WITHIN the convolution in [m^3/s])
        a1dVarHDam = 0.0

!        ! Update dam volume using observation(s) 
!        where (oHMC_Vars(iID)%a1dVDamObs .gt. -9999.0)
!            a1dVarVDam = oHMC_Vars(iID)%a1dVDamObs
!        endwhere
        !------------------------------------------------------------------------------------------
                      
        !------------------------------------------------------------------------------------------
        ! Check dam availability
        if (iNDam .gt. 0) then

            !------------------------------------------------------------------------------------------
            ! Cycle on dam
            do iD = 1, iNDam
                
                !------------------------------------------------------------------------------------------
                ! Check limit(s)
                if (a1dVarVDam(iD) .gt. oHMC_Namelist(iID)%dTV*oHMC_Vars(iID)%a1dVMaxDam(iD)) then   
                    
                    !------------------------------------------------------------------------------------------
                    ! Get dam indexes
                    iI = 0; iJ = 0;
                    iI = oHMC_Vars(iID)%a2iXYDam(iD,2); iJ = oHMC_Vars(iID)%a2iXYDam(iD,1)
		    !------------------------------------------------------------------------------------------

                    !------------------------------------------------------------------------------------------
                    ! Check dam length
                    if (a1dVarLDam(iD) .le. 0) then
                        a1dVarLDam(iD) = 40; ! [m]
                    endif

                    !------------------------------------------------------------------------------------------

                    !------------------------------------------------------------------------------------------
                    ! Compute H dam
                    iFlagD = 0

                    ! Compute TV array length
                    !iRank = count(mask=oHMC_Vars(iID)%a2dVDam(iD,:).ge.0.0, dim=1)
                    iRank = SIZE (oHMC_Vars(iID)%a2dVDam,dim=2)

                    ! Cycle on array dimension
                    do iK = iRank, 2,-1

                        ! Check TV curve availability
                        if ( (oHMC_Vars(iID)%a2dVDam(iD, iK) .gt. a1dVarVDam(iD)) .and. &
                             (oHMC_Vars(iID)%a2dVDam(iD, iK-1) .lt. a1dVarVDam(iD))  ) then 

                            ! TV curve defined 
                            iFlagD = 1

                            ! Dam H (absolute) --> Linear interpolation
                            a1dVarHDam(iD) = oHMC_Vars(iID)%a2dLDam(iD,iK-1) + &
                                             ( oHMC_Vars(iID)%a2dLDam(iD,iK) - oHMC_Vars(iID)%a2dLDam(iD,iK-1) )* &
                                             ( a1dVarVDam(iD) - oHMC_Vars(iID)%a2dVDam(iD,iK-1) )/ &
                                             ( oHMC_Vars(iID)%a2dVDam(iD,iK) - oHMC_Vars(iID)%a2dVDam(iD,iK-1) )
                            ! Dam h max (absolute)            
                            !dHMaxDam = oHMC_Vars(iID)%a1dHMaxDam(iD) + oHMC_Vars(iID)%a2dLDam(iD,1)

                        elseif (oHMC_Vars(iID)%a2dVDam(iD, iK) .lt. 0.0) then
                            ! TV curve undefined
                            iFlagD = 2  
                        endif

                    enddo

                    ! TV curve defined but V > VMax defined
                    if (iFlagD .eq. 0) then !Sono fuori dalla curva invaso volume

                        iMaxC=MAXLOC(oHMC_Vars(iID)%a2dLDam(iD,:),1)

                        ! Dam H (absolute)
                        a1dVarHDam(iD) = oHMC_Vars(iID)%a2dLDam(iD,iMaxC) + &       
                                         ( oHMC_Vars(iID)%a2dLDam(iD,iMaxC) - oHMC_Vars(iID)%a2dLDam(iD,iMaxC-1) )* &                                        
                                         ( a1dVarVDam(iD) - oHMC_Vars(iID)%a2dVDam(iD,iMaxC) )/ &                                       
                                         ( oHMC_Vars(iID)%a2dVDam(iD,iMaxC) - oHMC_Vars(iID)%a2dVDam(iD,iMaxC-1) )
                        ! Dam h max (absolute)              
                        !dHMaxDam = oHMC_Vars(iID)%a1dHMaxDam(iD) + oHMC_Vars(iID)%a2dLDam(iD,1)

                    endif

                    ! TV curve undefined
                    if (iFlagD .eq. 2) then !Non ho la curva invaso volume uso relazione lineare
                        ! Dam h (relative)
                        a1dVarHDam(iD) = oHMC_Vars(iID)%a1dHMaxDam(iD)*a1dVarVDam(iD)/oHMC_Vars(iID)%a1dVMaxDam(iD)
                        ! Dam h max (relative)
                        !dHMaxDam = oHMC_Vars(iID)%a1dHMaxDam(iD)
                    endif    
                    !------------------------------------------------------------------------------------------

                    !------------------------------------------------------------------------------------------
                    ! Check Hdam > Hdammax-DiffDamSpill (if true starting with dam outgoing flow)   !
                    if ( a1dVarHDam(iD) .gt. (oHMC_Vars(iID)%a1dHMaxDam(iD) - oHMC_Namelist(iID)%dDamSpillH) ) then

                        ! Outgoing dam discharge [m^3/s]
                        a1dVarQoutDam(iD) = 0.0;
                        a1dVarQoutDam(iD) = 0.385*a1dVarLDam(iD)*( (2*9.81)**0.5)*(a1dVarHDam(iD) - &
                                ( oHMC_Vars(iID)%a1dHMaxDam(iD) - oHMC_Namelist(iID)%dDamSpillH) )**1.5
                        
                        ! Check Qdam <= Qdamoutmax 
                        if (a1dVarQoutDam(iD) .gt. oHMC_Vars(iID)%a1dQcSLDam(iD)) then
                            a1dVarQoutDam(iD) = oHMC_Vars(iID)%a1dQcSLDam(iD)

                        endif

                        ! Check Hdam > Hdammax
                        if ( a1dVarHDam(iD) .gt. oHMC_Vars(iID)%a1dHMaxDam(iD) ) then

                            ! Exceeded volume used for outgoing dam discharge [m^3/s]
                            a1dVarQoutDam(iD) = (a1dVarVDam(iD) - oHMC_Vars(iID)%a1dVMaxDam(iD))/3600

                            ! Check max between a1dVarQoutDam(iD) and max outgoing dam flow
                            a1dVarQoutDam(iD) = max(a1dVarQoutDam(iD), oHMC_Vars(iID)%a1dQcSLDam(iD));

                        endif

                    else
                        a1dVarQoutDam(iD) = 0.0;
                    endif
                    !------------------------------------------------------------------------------------------

                    !------------------------------------------------------------------------------------------
                    ! Impose Vdam not less than dTV * Vdam_max --> static spillage
                    if ((a1dVarQoutDam(iD)*dDt).gt.(a1dVarVDam(iD) - (oHMC_Namelist(iID)%dTV*oHMC_Vars(iID)%a1dVMaxDam(iD)))) then
                        ! Discharge in [m^3/s] and update dam volume
                        a1dVarQoutDam(iD) = (a1dVarVDam(iD) - oHMC_Namelist(iID)%dTV*oHMC_Vars(iID)%a1dVMaxDam(iD))/dDt
                        a1dVarVDam(iD) = oHMC_Namelist(iID)%dTV*oHMC_Vars(iID)%a1dVMaxDam(iD)
                    else
                        a1dVarVDam(iD) = a1dVarVDam(iD) - a1dVarQoutDam(iD)*dDt                        
                    endif
                        
                    ! Discharge in [mm]
                    a1dVarQoutDam(iD) = a1dVarQoutDam(iD)*1000*dDt/(oHMC_Vars(iID)%a2dAreaCell(iI,iJ))

                    if (a1dVarVDam(iD) .lt. 0.0) then
                        a1dVarVDam(iD) = 0.0
                        a1dVarQoutDam(iD) = 0.0
                    endif
                    !------------------------------------------------------------------------------------------
                    
                endif
                !------------------------------------------------------------------------------------------
        
            enddo
            !------------------------------------------------------------------------------------------            

        endif
        !------------------------------------------------------------------------------------------
            
    end subroutine HMC_Phys_Dam_Spilling
    !------------------------------------------------------------------------------------------
    
    !------------------------------------------------------------------------------------------
    ! Subroutine to compute dam discharge
    subroutine HMC_Phys_Dam_Discharge(iID, iNDam, dDt, &
                                        a1dVarVDam, a1dVarHDam, a1dVarLDam, a1dVarCoeffDam)
                                                    
        !------------------------------------------------------------------------------------------
        ! Variable(s)
        integer(kind = 4)           :: iID
        integer(kind = 4)           :: iD, iK, iRank
        integer(kind = 4)           :: iNDam
        real(kind = 4)              :: dDt, dQtmp
        real(kind = 4)              :: dHMaxDam
        
        character(len = 256)                        :: sQtmp, sD, sCoeffDam, sHDam, sHMaxDam
        
        integer(kind = 4)                           :: iFlagD
        
        real(kind = 4), dimension (iNDam)           :: a1dVarVDam, a1dVarHDam, a1dVarLDam, a1dVarCoeffDam
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Initialize variable(s)
        iFlagD = 0; dQtmp = 0.0;
        
        sQtmp = ''; sCoeffDam = ''; sHDam = ''; sHMaxDam = '';
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Check dam availability
        if (iNDam .gt. 0) then

            !------------------------------------------------------------------------------------------
            ! Cycle on dam
            do iD = 1, iNDam
                
                !------------------------------------------------------------------------------------------
                ! Re-initialize dam height
                a1dVarHDam(iD) = 0.0
                
                ! Update dam volume using observation(s) 
                where (oHMC_Vars(iID)%a1dVDamObs .gt. -9999.0)
                    a1dVarVDam = oHMC_Vars(iID)%a1dVDamObs
                endwhere
                !------------------------------------------------------------------------------------------

                !------------------------------------------------------------------------------------------
                ! Check considering 0.95 total volume
                if ( a1dVarVDam(iD) .gt. oHMC_Namelist(iID)%dTV*oHMC_Vars(iID)%a1dVMaxDam(iD) ) then

                    !------------------------------------------------------------------------------------------
                    ! Check dam length
                    if (a1dVarLDam(iD) .le. 0) then
                        a1dVarLDam(iD) = 40; ! [m]
                    endif
                    !------------------------------------------------------------------------------------------

                    !------------------------------------------------------------------------------------------
                    ! Compute H dam
                    iFlagD = 0
                    
                    ! Compute TV array length
                    iRank = count(mask=oHMC_Vars(iID)%a2dVDam(iD,:).ge.0.0, dim=1)
                    
                    ! Cycle on array dimension
                    do iK = iRank, 1, -1

                        ! Check TV curve availability
                        if ( (oHMC_Vars(iID)%a2dVDam(iD, iK) .gt. a1dVarVDam(iD)) .and. &
                             (oHMC_Vars(iID)%a2dVDam(iD, iK-1) .lt. a1dVarVDam(iD))  ) then 
                            
                            ! TV curve defined 
                            iFlagD = 1
                            
                            ! Dam H (absolute) --> Linear interpolation
                            a1dVarHDam(iD) = oHMC_Vars(iID)%a2dLDam(iD,iK-1) + &
                                             ( oHMC_Vars(iID)%a2dLDam(iD,iK) - oHMC_Vars(iID)%a2dLDam(iD,iK-1) )* &
                                             ( a1dVarVDam(iD) - oHMC_Vars(iID)%a2dVDam(iD,iK-1) )/ &
                                             ( oHMC_Vars(iID)%a2dVDam(iD,iK) - oHMC_Vars(iID)%a2dVDam(iD,iK-1) )
                            ! Dam h max (absolute)            
                            dHMaxDam = oHMC_Vars(iID)%a1dHMaxDam(iD) + oHMC_Vars(iID)%a2dLDam(iD,1)

                        elseif (oHMC_Vars(iID)%a2dVDam(iD, iK) .lt. 0.0) then
                            ! TV curve undefined
                            iFlagD = 2  
                        endif

                    enddo
                    
                    ! TV curve defined but V > VMax defined
                    if (iFlagD .eq. 0) then !Sono fuori dalla curva invaso volume
                        
                        ! Dam H (absolute)
                        a1dVarHDam(iD) = oHMC_Vars(iID)%a2dLDam(iD,iRank) + &       
                                         ( oHMC_Vars(iID)%a2dLDam(iD,iRank) - oHMC_Vars(iID)%a2dLDam(iD,iRank-1) )* &                                        
                                         ( a1dVarVDam(iD) - oHMC_Vars(iID)%a2dVDam(iD,iRank) )/ &                                       
                                         ( oHMC_Vars(iID)%a2dVDam(iD,iRank) - oHMC_Vars(iID)%a2dVDam(iD,iRank-1) )
                        ! Dam h max (absolute)              
                        dHMaxDam = oHMC_Vars(iID)%a1dHMaxDam(iD) + oHMC_Vars(iID)%a2dLDam(iD,1)
                                         
                    endif
                    
                    ! TV curve undefined
                    if (iFlagD .eq. 2) then !Non ho la curva invaso volume uso relazione lineare
                        ! Dam h (relative)
                        a1dVarHDam(iD) = oHMC_Vars(iID)%a1dHMaxDam(iD)*a1dVarVDam(iD)/oHMC_Vars(iID)%a1dVMaxDam(iD)
                        ! Dam h max (relative)
                        dHMaxDam = oHMC_Vars(iID)%a1dHMaxDam(iD)
                    endif    
                    !------------------------------------------------------------------------------------------
                    
                    !------------------------------------------------------------------------------------------
                    ! Check Hdam > Hdammax-DiffDamSpill (if true starting with dam outgoing flow)
                    if ( a1dVarHDam(iD) .gt. (dHMaxDam - oHMC_Namelist(iID)%dDamSpillH) ) then
                        
                        ! Outgoing dam discharge [m^3/s]
                        dQtmp = 0.0;
                        dQtmp = 0.385*a1dVarLDam(iD)*( (2*9.81)**0.5)*(a1dVarHDam(iD) - &
                                ( dHMaxDam - oHMC_Namelist(iID)%dDamSpillH) )**1.5
                        
     
                        ! Check Qdam <= Qdamoutmax 
                        if (dQtmp .gt. oHMC_Vars(iID)%a1dQcSLDam(iD)) then
                            dQtmp = oHMC_Vars(iID)%a1dQcSLDam(iD)
                        endif
                                
                        ! Check Hdam > Hdammax
                        if ( a1dVarHDam(iD) .gt. dHMaxDam ) then

                            ! Exceeded volume used for outgoing dam discharge [m^3/s]
                            dQtmp = (a1dVarVDam(iD) - oHMC_Vars(iID)%a1dVMaxDam(iD))/3600
                            
                            ! Check max between dQtmp and max outgoing dam flow
                            dQtmp = max(dQtmp, oHMC_Vars(iID)%a1dQcSLDam(iD));

                        endif

                    else
                        dQtmp = 0.0;
                    endif
                    !------------------------------------------------------------------------------------------

                    !------------------------------------------------------------------------------------------
                    ! Compute dam outgoing flow coefficient [1/s]
                    if (dQtmp .gt. 0.0) then
                        a1dVarCoeffDam(iD) = dQtmp/(a1dVarVDam(iD) - &
                                             oHMC_Namelist(iID)%dTV*oHMC_Vars(iID)%a1dVMaxDam(iD))
                    else
                        a1dVarCoeffDam(iD) = 0.000
                    endif
                    !------------------------------------------------------------------------------------------

                    !------------------------------------------------------------------------------------------
                    ! Debug
                    !write(*,*) '==============================='
                    !write(*,*) iRank, iD
                    !write(*,*) oHMC_Vars(iID)%a2dLDam(iD,iRank)
                    !write(*,*) oHMC_Vars(iID)%a2dVDam(iD,iRank)
                    !write(*,*) oHMC_Vars(iID)%a2dLDam(iD,iRank-1)
                    !write(*,*) oHMC_Vars(iID)%a2dVDam(iD,iRank-1)
                    !write(*,*) a1dVarLDam(iD)
                    !write(*,*) a1dVarHDam(iD)
                    !write(*,*) oHMC_Vars(iID)%a1dHMaxDam(iD)
                    !write(*,*) a1dVarVDam(iD)
                    !write(*,*) oHMC_Vars(iID)%a1dVMaxDam(iD)
                    !write(*,*) dQtmp
                    !write(*,*) a1dVarCoeffDam(iD)
                    !write(*,*) '==============================='
                    !------------------------------------------------------------------------------------------
                    
                endif
                !------------------------------------------------------------------------------------------

                !------------------------------------------------------------------------------------------
                ! Info dam coefficient updating
                write(sQtmp, *) dQtmp; write(sD, *) iD; write(sCoeffDam, *) a1dVarCoeffDam(iD); 
                write(sHDam, *) a1dVarHDam(iD); write(sHMaxDam, *) oHMC_Vars(iID)%a1dHMaxDam(iD)
                call mprintf(.true., iINFO_Verbose, &
                            ' Phys :: Convolution :: Discharge Dam :: NDam: '//trim(sD)//' [-]'// &
                            ' QDam: '//trim(sQtmp)//' [m^3/s]'// &
                            ' CoeffDam: '//trim(sCoeffDam)//' [-]'// &
                            ' HDam: '//trim(sHDam)//' [m]'// &
                            ' HMaxDam: '//trim(sHMaxDam)//' [m]')
                !------------------------------------------------------------------------------------------

            enddo
            !------------------------------------------------------------------------------------------
            
        endif
        !------------------------------------------------------------------------------------------
    
    end subroutine HMC_Phys_Dam_Discharge
    !------------------------------------------------------------------------------------------
    
    !------------------------------------------------------------------------------------------
    ! Subroutine to compute lake tank
    subroutine HMC_Phys_Lake_Tank(iID, iRows, iCols, dDtDataForcing, iNLake, &
                                a1dVarVLake, a1dVarQoutLake, &
                                a2dVarHydro, a2dVarFlowDeep)
        
        !------------------------------------------------------------------------------------------
        ! Variable(s)
        integer(kind = 4)           :: iID
        integer(kind = 4)           :: iL, iRows, iCols, iNLake
        integer(kind = 4)           :: iI, iII, iIII, iJ, iJJ, iJJJ
        
        real(kind = 4)              :: dDtDataForcing
        
        real(kind = 4), dimension (iRows, iCols)    :: a2dVarFlowDeep, a2dVarHydro
        
        real(kind = 4), dimension (iNLake)          :: a1dVarVLake, a1dVarQoutLake
        !------------------------------------------------------------------------------------------

        !------------------------------------------------------------------------------------------
        ! Check lake availability
        if (iNLake .gt. 0) then
            
            ! Cycle on lake
            iL = 0
            do iL = 1, iNLake
                
                ! Check lake volume 
                if ( a1dVarVLake(iL) .gt. oHMC_Vars(iID)%a1dVMinLake(iL) ) then
                    
                    iI = 0; iJ = 0;
                    iI = oHMC_Vars(iID)%a2iXYLake(iL,2); iJ = oHMC_Vars(iID)%a2iXYLake(iL,1);

                    ! Output lake discharge [mm/h] 
                    a1dVarQoutLake(iL) = (a1dVarVLake(iL) - oHMC_Vars(iID)%a1dVMinLake(iL))* &
                                         (oHMC_Vars(iID)%a1dCostLake(iL)/3600)/(oHMC_Vars(iID)%a2dAreaCell(iI,iJ))*3600*1000
                    ! Lake outgoing flow
                    a1dVarVLake(iL) = a1dVarVLake(iL) - &
                                      (a1dVarVLake(iL) - oHMC_Vars(iID)%a1dVMinLake(iL))* &
                                      (oHMC_Vars(iID)%a1dCostLake(iL)/3600)*dDtDataForcing

                    ! Defining flow directions
                    iII = int((int(oHMC_Vars(iID)%a2iPNT(iI,iJ))  - 1)/3) - 1; 
                    iJJ = oHMC_Vars(iID)%a2iPNT(iI,iJ) - 5 - 3*iII
                    iIII = iI + iII; iJJJ = iJ + iJJ

                    ! Lake equation to fill lake [mm]
                    if (oHMC_Vars(iID)%a1dCodeLake(iL).gt.0) then
                        where (oHMC_Vars(iID)%a2iChoice.eq.oHMC_Vars(iID)%a1dCodeLake(iL) .and. oHMC_Vars(iID)%a2dDem.gt.0.0)
                            ! Lake volume to lake mean level
                            a2dVarHydro = a1dVarVLake(iL)/(oHMC_Vars(iID)%a1dCodeLake(iL)*oHMC_Vars(iID)%a2dAreaCell(iI,iJ))*1000
                        endwhere
                    endif

                    ! Add lake flow to deep flow (summed to intensity in Horton subroutine) [mm/dt]
                    a2dVarFlowDeep(iIII, iJJJ) = a2dVarFlowDeep(iIII, iJJJ) + a1dVarQoutLake(iL)*dDtDataForcing/3600

                endif        
            enddo
        endif
        !------------------------------------------------------------------------------------------
    
    end subroutine HMC_Phys_Lake_Tank
    !------------------------------------------------------------------------------------------
        
end module HMC_Module_Phys_HydraulicStructure
!------------------------------------------------------------------------------------------

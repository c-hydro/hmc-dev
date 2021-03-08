!------------------------------------------------------------------------------------------    
! File:   HMC_Module_Data_Static_Point.f90
! Author(s): Fabio Delogu, Francesco Silvestro, Simone Gabellani
!
! Created on April 30, 2015, 9:45 AM
!------------------------------------------------------------------------------------------

!------------------------------------------------------------------------------------
! Module Header
module HMC_Module_Data_Static_Point
    
    !------------------------------------------------------------------------------------
    ! External module(s) and implicit none
    use HMC_Module_Namelist,                    only:   oHMC_Namelist
    use HMC_Module_Vars_Loader,                 only:   oHMC_Vars
    
    use HMC_Module_Tools_Debug
    
    use HMC_Module_Tools_Generic,   only: HMC_Tools_Generic_FindFileLength
    
    implicit none
    !------------------------------------------------------------------------------------
    
contains
    
    !------------------------------------------------------------------------------------
    ! Subroutine to compute static data point
    subroutine HMC_Data_Static_Point_Cpl(iID, iRows, iCols, &
                                         iNSection, &
                                         iNLake, &
                                         iNDam, iNPlant, &
                                         iNJoint, &                                   
                                         iNCatch, iNRelease)
        
        !------------------------------------------------------------------------------------
        ! Variable(s) declaration
        integer(kind = 4)       :: iID
        integer(kind = 4)       :: iRows, iCols
        integer(kind = 4)       :: iNSection
        integer(kind = 4)       :: iNLake
        integer(kind = 4)       :: iNDam, iNPlant
        integer(kind = 4)       :: iNJoint
        integer(kind = 4)       :: iNCatch, iNRelease
        !------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------
        ! Info start
        call mprintf(.true., iINFO_Verbose, ' Data :: Static point ... ' )
        !------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------
        ! Initialize section(s) variable(s)
        call HMC_Data_Static_Point_Section(iID, iRows, iCols, iNSection)
        !------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------
        ! Initialize lake(s) variable(s)
        call HMC_Data_Static_Point_Lake(iID, iRows, iCols, iNLake)
        !------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------
        ! Initialize dam(s) variable(s)
        call HMC_Data_Static_Point_HydraulicStructure_Dam(iID, iRows, iCols, iNDam, iNPlant)
        !------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------
        ! Initialize joint(s) variable(s)
        call HMC_Data_Static_Point_HydraulicStructure_Joint(iID, iRows, iCols, iNJoint)
        !------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------
        ! Initialize intake(s) variable(s)
        call HMC_Data_Static_Point_HydraulicStructure_Intake(iID, iRows, iCols, iNCatch, iNRelease)
        !------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------
        ! Info end
        call mprintf(.true., iINFO_Verbose, ' Data :: Static point ... OK' )
        !------------------------------------------------------------------------------------
        
    end subroutine HMC_Data_Static_Point_Cpl
    !------------------------------------------------------------------------------------
    
    !--------------------------------------------------------------------------------
    ! Subroutine to get section(s) info
    subroutine HMC_Data_Static_Point_Section(iID, iRows, iCols, iNSection)
        
        !------------------------------------------------------------------------------------------
        ! Variable(s)
        integer(kind = 4)               :: iID
        integer(kind = 4)               :: iRows, iCols, iNSection
        integer(kind = 4)               :: iI
        
        character(len = 256)            :: sFileName, sDomainName, sPathData_Land
        
        integer(kind = 4), dimension (iNSection, 2)     :: a2iVarXYSection
        character(len = 500), dimension(iNSection)      :: a1sVarNameSection
        
        character(len = 10)             :: sI
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Initialize variable(s)
        a2iVarXYSection = 0; a1sVarNameSection = "";
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Get global settings(s)
        sDomainName = oHMC_Namelist(iID)%sDomainName
        sPathData_Land = oHMC_Namelist(iID)%sPathData_Static_Point
        ! Info
        call mprintf(.true., iINFO_Main, ' Define section info ... ')
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Define filename
        sFileName = trim(sPathData_Land)//trim(sDomainName)//'.info_section.txt'
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Check lake(s) availability
        if (iNSection .gt. 0) then
            
            !------------------------------------------------------------------------------------------
            ! Open and read ASCII file
            open(unit=1,file = sFileName, status='old')

            do iI = 1,iNSection
                ! Lake outlet section index coordinates (i,j)
                read(1,*) a2iVarXYSection(iI,2), a2iVarXYSection(iI,1), a1sVarNameSection(iI)
                a2iVarXYSection(iI,2) = iRows - a2iVarXYSection(iI,2) + 1
                if (a2iVarXYSection(iI,2).le.0) then
                    write(sI,*) iI; call mprintf(.true., iERROR, ' Section '//trim(sI)//' number of rows negative ')
                endif
                
            enddo
            
            ! Close ASCII file
            close(1)
            !------------------------------------------------------------------------------------------
            
            !------------------------------------------------------------------------------------------
            ! Pass local variable(s) to global workspace
            oHMC_Vars(iID)%a2iXYSection = a2iVarXYSection
            oHMC_Vars(iID)%a1sNameSection = a1sVarNameSection
            ! Info
            call mprintf(.true., iINFO_Main, ' Define section info ... OK')
            !------------------------------------------------------------------------------------------
            
        else
            !------------------------------------------------------------------------------------------
            ! Info
            call mprintf(.true., iINFO_Main, ' No section(s) available for this run!' )
            !------------------------------------------------------------------------------------------
        endif
        !------------------------------------------------------------------------------------------
    
    end subroutine HMC_Data_Static_Point_Section
    !--------------------------------------------------------------------------------
    
    !--------------------------------------------------------------------------------
    ! Subroutine to get lake(s) info
    subroutine HMC_Data_Static_Point_Lake(iID, iRows, iCols, iNLake)
        
        !------------------------------------------------------------------------------------------
        ! Variable(s)
        integer(kind = 4)               :: iID
        integer(kind = 4)               :: iRows, iCols, iNLake
        integer(kind = 4)               :: iI
        
        character(len = 256)            :: sFileName, sDomainName, sPathData
        
        integer(kind = 4), dimension (iRows, iCols) :: a2iVarCounter, a2iVarChoice
        real(kind = 4), dimension (iRows, iCols)    :: a2dVarDem
        
        integer(kind = 4), dimension (iNLake, 2)    :: a2iVarXYLake
        real(kind = 4), dimension (iNLake)          :: a1dVarCodeLake, a1dVarVLakeMin
        real(kind = 4), dimension (iNLake)          :: a1dVarVLake, a1dVarCostLake
        integer(kind = 4), dimension (iNLake)       :: a1dVarNCellLake
        
        character(len = 10)                         :: sI
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Initialize variable(s)
        a2iVarCounter = 0; a2iVarChoice = 0; a2dVarDem = 0.0
        a2iVarXYLake = 0; a1dVarNCellLake = 0;
        a1dVarCodeLake = 0.0; a1dVarVLakeMin = 0.0; a1dVarVLake = 0.0; a1dVarCostLake = 0.0;
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Get global settings(s)
        sDomainName = oHMC_Namelist(iID)%sDomainName
        sPathData = oHMC_Namelist(iID)%sPathData_Static_Point
        ! Get global variable(s)
        a2iVarChoice = oHMC_Vars(iID)%a2iChoice
        a2dVarDem = oHMC_Vars(iID)%a2dDem
        ! Info
        call mprintf(.true., iINFO_Main, ' Define lake(s) info ... ')
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Define filename
        sFileName = trim(sPathData)//trim(sDomainName)//'.info_lake.txt'
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Check lake(s) availability
        if (iNLake .gt. 0) then
            
            !------------------------------------------------------------------------------------------
            ! Open and read ASCII grid file
            open(unit=1,file = sFileName, status='old')
            read(1,*)
            
            do iI = 1,iNlake
		read(1,*)
		! Lake outlet section name
                read(1,*)
                ! Lake outlet section index coordinates (i,j)
                read(1,*) a2iVarXYLake(iI,2), a2iVarXYLake(iI,1)
                a2iVarXYLake(iI,2) = iRows - a2iVarXYLake(iI,2) + 1
                if (a2iVarXYLake(iI,2).le.0) then
                    write(sI,*) iI; call mprintf(.true., iERROR, ' Lake '//trim(sI)//' number of rows negative ')
                endif
                
                ! Lake code on choice matrix
                read(1,*) a1dVarCodeLake(iI)
                ! Lake minimum volume value to have discharge
                read(1,*) a1dVarVLakeMin(iI)
                ! Lake initial volume
                read(1,*) a1dVarVLake(iI)
                ! Lake outgoing constant  [1/h]
                read(1,*) a1dVarCostLake(iI)

                ! Cell counter to estimate lake extension (before dam)
		a2iVarCounter = 0
                where ( (a2iVarChoice .eq. int(a1dVarCodeLake(iI))) .and. (a2dVarDem .gt. 0.0) )
                    a2iVarCounter = 1
                endwhere
                ! Lake cell domain before dam outlet section
		a1dVarNCellLake(iI) = SUM(SUM(a2iVarCounter, DIM=1, MASK = a2iVarCounter.EQ.1)) 

            enddo
            
            ! Close ASCII file
            close(1)
            !------------------------------------------------------------------------------------------
            
            !------------------------------------------------------------------------------------------
            ! Pass local variable(s) to global workspace
            oHMC_Vars(iID)%a2iXYLake = a2iVarXYLake
            oHMC_Vars(iID)%a1dCodeLake = a1dVarCodeLake
            oHMC_Vars(iID)%a1iNCellLake = a1dVarNCellLake
            oHMC_Vars(iID)%a1dVMinLake = a1dVarVLakeMin
            oHMC_Vars(iID)%a1dVLake = a1dVarVLake
            oHMC_Vars(iID)%a1dCostLake = a1dVarCostLake
            ! Info
            call mprintf(.true., iINFO_Main, ' Define lake(s) info ... OK')
            !------------------------------------------------------------------------------------------
            
        else
            !------------------------------------------------------------------------------------------
            ! Info
            call mprintf(.true., iINFO_Main, ' No lake(s) available for this run!' )
            !------------------------------------------------------------------------------------------
        endif
        !------------------------------------------------------------------------------------------
        
    end subroutine HMC_Data_Static_Point_Lake
    !--------------------------------------------------------------------------------
    
    !------------------------------------------------------------------------------------------
    ! Subroutine to load intake information
    subroutine HMC_Data_Static_Point_HydraulicStructure_Intake(iID, iRows, iCols, iNCatch, iNRelease)
    
        !------------------------------------------------------------------------------------------
        ! Variable(s)
        integer(kind = 4)               :: iID
        integer(kind = 4)               :: iRows, iCols, iNCatch, iNRelease
        integer(kind = 4)               :: iI, iJ
        integer(kind = 4)               :: iCatch, iCatchTot
        real(kind = 4)                  :: dQMaxCatch, dQMinCatch
        
        character(len = 256)            :: sText
        character(len = 256)            :: sFileName, sDomainName, sPathData
        
        integer(kind = 4), dimension (iNCatch, 2)     :: a2iVarXYCatch
        real(kind = 4), dimension (iNCatch)           :: a1dVarWeightCatch, a1dVarTCorrCatch
        real(kind = 4), dimension (iNCatch)           :: a1dVarQMinCatch
        character(len = 500), dimension(iNCatch)      :: a1sVarNameCatch
        
        integer(kind = 4), dimension (iNRelease, 2)   :: a2iVarXYRelease
        character(len = 500), dimension(iNRelease)    :: a1sVarNameRelease
        real(kind = 4), dimension (iNRelease)         :: a1dVarQMaxRelease
        
        character(len = 10)                           :: sJ
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Initialize variable(s)
        a2iVarXYCatch = 0; a2iVarXYRelease = 0;
        a1dVarWeightCatch = 0.0; a1dVarTCorrCatch = 0.0;
        a1sVarNameCatch = ""; a1sVarNameRelease = ""
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Get global variable(s)
        sDomainName = oHMC_Namelist(iID)%sDomainName
        sPathData = oHMC_Namelist(iID)%sPathData_Static_Point
        ! Info
        call mprintf(.true., iINFO_Main, ' Define catch(s) and release(s) info ... ')
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Define filename
        sFileName = trim(sPathData)//trim(sDomainName)//'.info_intake.txt'
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Check joint availability
        if (iNRelease .gt. 0) then
            
            !------------------------------------------------------------------------------------------
            ! Open and read ASCII file
            open(unit=1,file=sFileName, status='old')
            read(1,*) sText
            read(1,*) sText
            
            ! Cycle(s) on release(s)
            iCatchTot = 1
            do iI = 1, iNRelease
                
                ! Comment line 
                read(1,*) sText
                
                ! Release section name
                read(1,*) a1sVarNameRelease(iI)
  
                ! Release section index coordinates (i,j)
                read(1,*) a2iVarXYRelease(iI,2), a2iVarXYRelease(iI,1)
                a2iVarXYRelease(iI,2) = iRows - a2iVarXYRelease(iI,2) + 1
                
                ! Catch section number
                read(1,*) iCatch ! inC

                ! Cycle(s) on catch(es)
                a1dVarQMaxRelease(iI) = 0.0
                do iJ = iCatchTot, iCatch + iCatchTot - 1
                    
                    ! Catch section name
                    read(1,*) 
                    a1sVarNameCatch(iJ) = a1sVarNameRelease(iI)
                    
                    ! Corrivation time catch-release
                    read(1,*) a1dVarTCorrCatch(iJ)
                    
                    ! Catch section index coordinates (i,j)
                    read(1,*) a2iVarXYCatch(iJ,2), a2iVarXYCatch(iJ,1)
                    a2iVarXYCatch(iJ,2) = iRows - a2iVarXYCatch(iJ,2) + 1
                    if (a2iVarXYCatch(iI,2).le.0) then
                        write(sJ,*) iJ; call mprintf(.true., iERROR, ' Catch '//trim(sJ)//' number of rows negative ')
                    endif
                    
                    ! Max discharge catch (and max release discharge value)
                    read(1,*) dQMaxCatch
                    if (dQMaxCatch .gt. 0.0) then
                        a1dVarQMaxRelease(iI) = a1dVarQMaxRelease(iI) + dQMaxCatch
                    endif
                    
                    ! Min discharge catch
                    read(1,*) dQMinCatch
                    if (dQMinCatch .gt. 0.0) then
                        a1dVarQMinCatch(iJ) = dQMinCatch
                    endif
                    
                    ! Catch - Release weight
                    read(1,*) a1dVarWeightCatch(iJ)
                    
                enddo
                iCatchTot = iCatchTot + iCatch
                
            enddo
                   
            ! Close ASCII file
            close(1)
            !------------------------------------------------------------------------------------------

            !------------------------------------------------------------------------------------------
            ! Pass local variable(s) to global workspace
            ! Catch
            oHMC_Vars(iID)%a2iXYCatch = a2iVarXYCatch
            oHMC_Vars(iID)%a1sNameCatch = a1sVarNameCatch
            oHMC_Vars(iID)%a1dWeigthCatch = a1dVarWeightCatch
            oHMC_Vars(iID)%a1dTCorrCatch = a1dVarTCorrCatch
            oHMC_Vars(iID)%a1dQMinCatch = a1dVarQMinCatch
            
            ! Release
            oHMC_Vars(iID)%a2iXYRelease = a2iVarXYRelease
            oHMC_Vars(iID)%a1sNameRelease = a1sVarNameRelease
            oHMC_Vars(iID)%a1dQMaxRelease = a1dVarQMaxRelease
            
            ! Info
            call mprintf(.true., iINFO_Main, ' Define catch(s) and release(s) info ... OK')
            !------------------------------------------------------------------------------------------
        
        else
            !------------------------------------------------------------------------------------------
            ! Info
            call mprintf(.true., iINFO_Main, ' No catch(es) and release(s) available for this run!' )
            !------------------------------------------------------------------------------------------
        endif 
        !------------------------------------------------------------------------------------------
        
    end subroutine HMC_Data_Static_Point_HydraulicStructure_Intake
    !------------------------------------------------------------------------------------------
        
    !------------------------------------------------------------------------------------------
    ! Subroutine to load joint information
    subroutine HMC_Data_Static_Point_HydraulicStructure_Joint(iID, iRows, iCols, iNJoint)
    
        !------------------------------------------------------------------------------------------
        ! Variable(s)
        integer(kind = 4)               :: iID
        integer(kind = 4)               :: iRows, iCols, iNJoint
        integer(kind = 4)               :: iI
        
        character(len = 256)            :: sFileName, sDomainName, sPathData
        
        real(kind = 4), dimension(iNJoint)              :: a1dVarThrLevel
        integer(kind = 4), dimension (iNJoint, 2)       :: a2iVarXY, a2iVarXYIn, a2iVarXYOut
        
        character(len = 10)                             :: sI
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Initialize variable(s)
        a2iVarXY = 0; a2iVarXYIn = 0; a2iVarXYOut = 0;
        a1dVarThrLevel = 0.0;
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Get global variable(s)
        sDomainName = oHMC_Namelist(iID)%sDomainName
        sPathData = oHMC_Namelist(iID)%sPathData_Static_Point
        ! Info
        call mprintf(.true., iINFO_Main, ' Define joint(s) info ... ')
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Define filename
        sFileName = trim(sPathData)//trim(sDomainName)//'.info_joint.txt'
        !------------------------------------------------------------------------------------------

        !------------------------------------------------------------------------------------------
        ! Check joint availability
        if (iNJoint .gt. 0) then
            
            !------------------------------------------------------------------------------------------
            ! Open and read ASCII file
            open(unit=1,file = sFileName, status='old')
            read(1,*)

            do iI = 1, iNJoint
                
                ! Nothing to read here
                read(1, *)
                
                ! Reference name to identify rivers linking
                read(1, *)
                
                ! Joint section index coordinates (i,j) of main river 
                read(1, *) a2iVarXY(iI,2), a2iVarXY(iI,1)
                a2iVarXY(iI,2) = iRows - a2iVarXY(iI,2) + 1
                if (a2iVarXY(iI,2).le.0) then
                    write(sI,*) iI; call mprintf(.true., iERROR, ' Joint '//trim(sI)//' number of rows negative (main) ')
                endif
                
                ! Joint section index coordinates (i,j) of river that receives water from main river
                read(1, *) a2iVarXYIn(iI,2), a2iVarXYIn(iI,1)
                a2iVarXYIn(iI,2) = iRows - a2iVarXYIn(iI,2) + 1
                if (a2iVarXYIn(iI,2).le.0) then
                    write(sI,*) iI; call mprintf(.true., iERROR, ' Joint '//trim(sI)//' number of rows negative (in) ')
                endif
                
                ! Joint section index coordinates (i,j) of river where finishes water from main river
                read(1, *) a2iVarXYOut(iI,2), a2iVarXYOut(iI,1)
                a2iVarXYOut(iI,2) = iRows - a2iVarXYOut(iI,2) + 1
                if (a2iVarXYOut(iI,2).le.0) then
                    write(sI,*) iI; call mprintf(.true., iERROR, ' Joint '//trim(sI)//' number of rows negative (out) ')
                endif
                
                ! Threshold of outflow starting
                read(1, *) a1dVarThrLevel(iI)
                
            enddo
            
            ! Close ASCII file
            close(1)
            !------------------------------------------------------------------------------------------
            
            !------------------------------------------------------------------------------------------
            ! Pass local variable(s) to global workspace
            ! Joint
            oHMC_Vars(iID)%a1dThrLevelJoint = a1dVarThrLevel
            oHMC_Vars(iID)%a2iXYJoint = a2iVarXY
            oHMC_Vars(iID)%a2iXYInJoint = a2iVarXYIn
            oHMC_Vars(iID)%a2iXYOutJoint = a2iVarXYOut
            ! Info
            call mprintf(.true., iINFO_Main, ' Define joint(s) info ... OK')
            !------------------------------------------------------------------------------------------
  
        else
            !------------------------------------------------------------------------------------------
            ! Info
            call mprintf(.true., iINFO_Main, ' No joint(s) available for this run!' )
            !------------------------------------------------------------------------------------------
        endif 
        
        !------------------------------------------------------------------------------------------
        
    end subroutine HMC_Data_Static_Point_HydraulicStructure_Joint
    !------------------------------------------------------------------------------------------
    
    !------------------------------------------------------------------------------------------
    ! Subroutine to load dam information
    subroutine HMC_Data_Static_Point_HydraulicStructure_Dam(iID, iRows, iCols, iNDam, iNPlant)
        
        !------------------------------------------------------------------------------------------
        ! Variable(s)
        integer(kind = 4)               :: iID
        integer(kind = 4)               :: iRows, iCols, iNDam, iNPlant
        integer(kind = 4)               :: iI, iJ
        
        integer(kind = 4)               :: iDamPlant, iDamPlantTot
        character(len = 256)            :: sFileName, sDomainName, sPathData
        character(len = 256)            :: sFileNameCurveTV, sFileNameCurveTVComplete
        character(len = 256)            :: sVarNameDam
        
        integer(kind = 4)               :: iVarQFlagPlant
        
        integer(kind = 4), dimension (iRows, iCols) :: a2iVarChoice, a2iVarCounter
        real(kind = 4), dimension (iRows, iCols) :: a2dVarDem
        
        integer(kind = 4), dimension (iNDam, 2)     :: a2iVarXYDam
        real(kind = 4), dimension (iNDam)           :: a1dVarCodeDam, a1dVarVMaxDam, a1dVarVDam
        real(kind = 4), dimension (iNDam)           :: a1dVarQcSLDam, a1dVarLDam, a1dVarHMaxDam, a1dVarCoeffDam
        integer(kind = 4), dimension (iNDam)        :: a1iVarNCellDam
        
        integer(kind = 4), dimension (iNPlant, 2)   :: a2iVarXYPlant
        integer(kind = 4), dimension (iNPlant)      :: a1iVarFlagDamPlant
        
        real(kind = 4), dimension (iNPlant)         :: a1dVarQMaxPlant, a1dVarTcPlant
        character(len = 500), dimension(iNPlant)    :: a1sVarNamePlant
        
        character(len = 10)                         :: sI, sJ
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Initialize variable(s)
        a2iVarChoice = 0; a2iVarCounter = 0; a2dVarDem = 0.0
        a2iVarXYDam = -9999
        a1dVarCodeDam = -9999.0; a1dVarVMaxDam  = -9999.0; a1dVarVDam = -9999.0; 
        a1dVarQcSLDam = -9999.0; a1dVarLDam = -9999.0; a1dVarHMaxDam = -9999.0; a1dVarCoeffDam = -9999.0; 
        a2iVarXYPlant = -9999; a1iVarFlagDamPlant = -9999
        a1dVarQMaxPlant = -9999.0; a1dVarTcPlant = -9999.0; 
        a1sVarNamePlant = ''
        
        a2iVarCounter = 0
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Get global setting(s)
        sDomainName = oHMC_Namelist(iID)%sDomainName
        sPathData = oHMC_Namelist(iID)%sPathData_Static_Point
        ! Get global variable(s)
        a2iVarChoice = oHMC_Vars(iID)%a2iChoice
        a2dVarDem = oHMC_Vars(iID)%a2dDem
        ! Info
        call mprintf(.true., iINFO_Main, ' Define dam(s) and plant(s) info ... ')
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Define filename
        sFileName = trim(sPathData)//trim(sDomainName)//'.info_dam.txt'
        !------------------------------------------------------------------------------------------

        !------------------------------------------------------------------------------------------
        ! Check dam number
        if (iNDam .gt. 0) then
            
            !------------------------------------------------------------------------------------------
            ! Open and read ascii grid file
            open(unit=1,file=sFileName, status='old')
            read(1,*)
            read(1,*)
            read(1,*)
            
            iDamPlantTot = 1
            do iI = 1, iNDam
                
                ! Dam outlet section name
                read(1,*) sVarNameDam
                call mprintf(.true., iINFO_Verbose, ' Dam Name:  '//trim(sVarNameDam) )
                
                ! Dam outlet section index coordinates (i,j)
                read(1,*) a2iVarXYDam(iI,2), a2iVarXYDam(iI,1)
                a2iVarXYDam(iI,2) = iRows - a2iVarXYDam(iI,2) + 1
                if (a2iVarXYDam(iI,2).le.0) then
                    write(sI,*) iI; call mprintf(.true., iERROR, ' Dam '//trim(sI)//' number of rows negative')
                endif
                
                ! Numero di centrali a valle della diga (inC)
                read(1,*) iDamPlant
                
                ! Dam code on the choice array
                read(1,*) a1dVarCodeDam(iI)
                
                ! Dam maximum volume value to have discharge
                read(1,*) a1dVarVMaxDam(iI)
                
                ! Dam initial volume
                read(1,*) a1dVarVDam(iI)
                
                ! Dam lateral critical discharge (a1dQ_sLC Portata critica dallo scarico laterale o altro tipo)
                read(1,*) a1dVarQcSLDam(iI)
                
                ! Dam equivalent length of surface spilling (adL Lunghezza equivalente scarico superficiale della diga)
                read(1,*) a1dVarLDam(iI)
                
                ! Dam water maximum height
                read(1,*) a1dVarHMaxDam(iI)
                
                ! Dam tank coefficient (Coefficiente serbatoio lineare equivalente allo scarico superficiale della diga)
                read(1,*) a1dVarCoeffDam(iI)
                
                ! Dam filename (curva invaso-volume)
                read(1,*) sFileNameCurveTV
                sFileNameCurveTVComplete = trim(sPathData)//sFileNameCurveTV
                ! Call curve tank-volume function
                call HMC_Data_Static_Point_HydraulicStructure_Dam_StorageCurve(iID, sVarNameDam, sFileNameCurveTVComplete, iI) 
                !------------------------------------------------------------------------------------------
                
                !------------------------------------------------------------------------------------------
                ! Cycle(s) on plant(s)
                do iJ = iDamPlantTot, iDamPlant + iDamPlantTot - 1
                    
                    ! Plant name
                    read(1,*) a1sVarNamePlant(iJ)
                    
                    ! Plant outlet section index coordinates (i,j)
                    read(1,*) a2iVarXYPlant(iJ,2), a2iVarXYPlant(iJ,1)
                    a2iVarXYPlant(iJ,2) = iRows - a2iVarXYPlant(iJ,2) + 1
                    if (a2iVarXYPlant(iI,2).le.0) then
                        write(sJ,*) iJ; call mprintf(.true., iERROR, ' Plant '//trim(sJ)//' number of rows negative')
                    endif
                    
                    ! Dam-Plant time corrivation (dismiss)
                    read(1, *) a1dVarTcPlant(iJ)
                    ! Plant max discharge
                    read(1, *) a1dVarQMaxPlant(iJ)
                    ! Plant-Dam flag
                    read(1, *) iVarQFlagPlant
                    
                    ! Refer dam index to plant 
                    a1iVarFlagDamPlant(iJ) = int(iI)
                    
                enddo
                !------------------------------------------------------------------------------------------
                
                !------------------------------------------------------------------------------------------
                iDamPlantTot = iDamPlantTot + iDamPlant
                
                read(1,*)
                !------------------------------------------------------------------------------------------
                
                !------------------------------------------------------------------------------------------
                ! Cell counter to estimate lake extension (before dam)
		a2iVarCounter = 0
                where ( (a2iVarChoice .eq. int(a1dVarCodeDam(iI))) .and. (a2dVarDem .gt. 0.0) )
                    a2iVarCounter = 1
                endwhere
                ! Lake cell domain before dam outlet section
		a1iVarNCellDam(iI) = SUM(SUM(a2iVarCounter, DIM=1, MASK = a2iVarCounter.EQ.1)) 
                !------------------------------------------------------------------------------------------
  
            enddo
            !------------------------------------------------------------------------------------------
            
            !------------------------------------------------------------------------------------------
            ! Close file
            close(1)
            !------------------------------------------------------------------------------------------
            
            !------------------------------------------------------------------------------------------
            ! Pass local variable(s) to global workspace
            ! Dam(s)
            oHMC_Vars(iID)%a2iXYDam = a2iVarXYDam
            oHMC_Vars(iID)%a1dVDam = a1dVarVDam
            oHMC_Vars(iID)%a1dLDam = a1dVarLDam
            oHMC_Vars(iID)%a1dCodeDam = a1dVarCodeDam
            oHMC_Vars(iID)%a1dVMaxDam = a1dVarVMaxDam
            oHMC_Vars(iID)%a1dQcSLDam = a1dVarQcSLDam
            oHMC_Vars(iID)%a1dHMaxDam = a1dVarHMaxDam
            oHMC_Vars(iID)%a1dCoeffDam = a1dVarCoeffDam
            oHMC_Vars(iID)%a1iNCellDam = a1iVarNCellDam
            ! Plant(s)
            oHMC_Vars(iID)%a2iXYPlant = a2iVarXYPlant
            oHMC_Vars(iID)%a1iFlagDamPlant = a1iVarFlagDamPlant
            oHMC_Vars(iID)%a1dQMaxPlant = a1dVarQMaxPlant
            oHMC_Vars(iID)%a1dTcPlant = a1dVarTcPlant
            oHMC_Vars(iID)%a1sNamePlant = a1sVarNamePlant
            ! Info
            call mprintf(.true., iINFO_Main, ' Define dam(s) and plant(s) info ... OK')
            !------------------------------------------------------------------------------------------
            
        else
            !------------------------------------------------------------------------------------------
            ! Info
            call mprintf(.true., iINFO_Main, ' No dam(s) and plant(s) available for this run!' )
            !------------------------------------------------------------------------------------------
        endif
        !------------------------------------------------------------------------------------------
            
    end subroutine HMC_Data_Static_Point_HydraulicStructure_Dam
    !------------------------------------------------------------------------------------------
    
    !------------------------------------------------------------------------------------------
    ! Subroutine to read Tank-Volume Curve
    subroutine HMC_Data_Static_Point_HydraulicStructure_Dam_StorageCurve(iID, sDamName, sFileName, iI)
        
        !------------------------------------------------------------------------------------------
        ! Inserisco i dati in modo che i valori pi� alti di livello e volume risultino alla 
        ! coordinata massima della matrice. Sotto al valore inferiore di validit� inserisco
        ! H e V minimi
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Variable(s)
        integer(kind = 4)           :: iID
        integer(kind = 4)           :: iI, iJ, iLength, iStep, iSkipRows, iMod
        character(len = 256)        :: sFileName, sDamName
        
        real(kind = 4)              :: dVarL, dVarV
        integer(kind = 4)           :: iErr

        logical                     :: bFileExist
        
        character(len = 100)         :: sLength, sStep
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Initialize variable(s)
        iLength = 0; iSkipRows = 3;
        ! Info
        call mprintf(.true., iINFO_Verbose, ' Define dam storage curve ... ')
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Check ASCII file availability
        inquire (file = trim(sFileName), exist = bFileExist, iostat = iErr)
        if ( .not. bFileExist ) then
            
            !------------------------------------------------------------------------------------------
            ! Info
            call mprintf(.true., iWARN, &
                        ' No dam storage curve file found: '//trim(sFileName)//' (Dam: '//trim(sDamName)//')')
            oHMC_Vars(iID)%a2dVDam(iI,:) = -9999.0; oHMC_Vars(iID)%a2dLDam(iI,:) = -9999.0
            !------------------------------------------------------------------------------------------
        else
            
            !------------------------------------------------------------------------------------------
            ! Info
            call mprintf(.true., iINFO_Verbose, &
                        ' Dam storage curve file found: '//trim(sFileName)//' (Dam: '//trim(sDamName)//')')
            !------------------------------------------------------------------------------------------
            
            !------------------------------------------------------------------------------------------
            ! Open ASCII grid file
            open(unit=2,file=sFileName, status='old')
            
            ! Compute file length
            call HMC_Tools_Generic_FindFileLength(2, iLength, iSkipRows)
            
            ! Compute selected step of curve TV
            iStep = (iLength - 1)/size(oHMC_Vars(iID)%a2dVDam, dim = 2)
            iStep = int(real(iStep)) + 1
            if(iStep.lt.1) iStep = 1
            
            ! Info
            write(sLength,*) iLength; write(sStep,*) iStep;
            call mprintf(.true., iINFO_Verbose, ' Dam storage curve length and step:  '//trim(sLength)//' '//trim(sStep) )
 
            ! Read ASCII file
            ! Skip data
            do iJ = 1, iSkipRows
                read(2,*) 
            enddo         
            ! Read and save data
            do iJ = 1, iLength
                read(2,*) dVarL, dVarV
                if( (iJ .eq. 1) .or. (iJ/iStep.gt.0) .and. (iJ/iStep.le.iLength)) then
                        
                        ! Compute mod between iJ position and step data resolution 
                        iMod = mod(iJ, iStep)
                        
                        ! Condition on iJ position
                        if (iJ.eq.1) then
                            oHMC_Vars(iID)%a2dVDam(iI, iJ) = dVarV
                            oHMC_Vars(iID)%a2dLDam(iI, iJ) = dVarL
                        elseif (iMod .eq. 0 ) then 
                            oHMC_Vars(iID)%a2dVDam(iI, iJ/iStep) = dVarV
                            oHMC_Vars(iID)%a2dLDam(iI, iJ/iStep) = dVarL
                        endif
                endif
            enddo 
            
            ! Close ASCII file
            close(2)
            !------------------------------------------------------------------------------------------
            
        endif
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Info end
        call mprintf(.true., iINFO_Verbose, ' Define dam storage curve ... OK')
        !------------------------------------------------------------------------------------------
        
    end subroutine HMC_Data_Static_Point_HydraulicStructure_Dam_StorageCurve
    !------------------------------------------------------------------------------------------
    
    
    
end module HMC_Module_Data_Static_Point
!------------------------------------------------------------------------------------
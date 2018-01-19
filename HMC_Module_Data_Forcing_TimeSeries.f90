!------------------------------------------------------------------------------------------    
! File:   HMC_Module_Data_Forcing_TimeSeries.f90
! Author(s): Fabio Delogu, Francesco Silvestro, Simone Gabellani
!
! Created on April 6, 2017, 4:33 PM
!------------------------------------------------------------------------------------------

!------------------------------------------------------------------------------------------
! Module Header
module HMC_Module_Data_Forcing_TimeSeries
    
    !------------------------------------------------------------------------------------------
    ! External module(s) for all subroutine in this module
    use HMC_Module_Namelist,                    only:   oHMC_Namelist
    use HMC_Module_Vars_Loader,                 only:   oHMC_Vars
    
    use HMC_Module_Tools_Debug
    use HMC_Module_Tools_Generic,               only:   HMC_Tools_Generic_SmoothTimeSeries, &
                                                        HMC_Tools_Generic_ReplaceText
    
    use HMC_Module_Tools_IO,                    only:   HMC_Tools_IO_Get1d_ASCII
                             
    ! Implicit none for all subroutines in this module
    implicit none
    !------------------------------------------------------------------------------------------
   
contains
    
    !------------------------------------------------------------------------------------------
    ! Subroutine to manage forcing time-series data
    subroutine HMC_Data_Forcing_TimeSeries_Cpl(iID, sTime, &
                                          iNData, iETime, &  
                                          iNLake, iNDam, iNPlant, iNJoint, iNCatch, iNRelease)
        
        !------------------------------------------------------------------------------------------
        ! Variable(s) declaration
        integer(kind = 4)           :: iID
        character(len = 19)         :: sTime
        
        integer(kind = 4)           :: iNData, iETime
        integer(kind = 4)           :: iNLake, iNDam, iNPlant, iNJoint, iNCatch, iNRelease
        
        integer(kind = 4)           :: iFlagTypeData_Forcing
        character(len = 700)        :: sPathData_Forcing
        !------------------------------------------------------------------------------------------

        !------------------------------------------------------------------------------------------
        ! Get global information
        sPathData_Forcing = oHMC_Namelist(iID)%sPathData_Forcing_TimeSeries
        iFlagTypeData_Forcing = oHMC_Namelist(iID)%iFlagTypeData_Forcing_TimeSeries
        !------------------------------------------------------------------------------------------

        !------------------------------------------------------------------------------------------
        ! Replace general path with specific time feature(s)
        call HMC_Tools_Generic_ReplaceText(sPathData_Forcing, '$yyyy', sTime(1:4))
        call HMC_Tools_Generic_ReplaceText(sPathData_Forcing, '$mm', sTime(6:7))
        call HMC_Tools_Generic_ReplaceText(sPathData_Forcing, '$dd', sTime(9:10))
        call HMC_Tools_Generic_ReplaceText(sPathData_Forcing, '$HH', sTime(12:13))
        call HMC_Tools_Generic_ReplaceText(sPathData_Forcing, '$MM', sTime(15:16))
        !------------------------------------------------------------------------------------------

        !------------------------------------------------------------------------------------------
        ! Info start
        call mprintf(.true., iINFO_Extra, ' Data :: Forcing time-series ... ' )
        !------------------------------------------------------------------------------------------

        !------------------------------------------------------------------------------------------
        ! Subroutine for reading sequential ASCII forcing data point
        if (iFlagTypeData_Forcing == 1) then

            !------------------------------------------------------------------------------------------
            ! Check logical variable to control forcing point section (2d array)
            if (.not. oHMC_Vars(iID)%bFileForcingTimeSeries) then

                !------------------------------------------------------------------------------------------
                ! Get point time-series for plant(s)
                call HMC_Data_Forcing_TimeSeries_Plant(iID, iNPlant, iNData, iETime)
                !------------------------------------------------------------------------------------------
                
                !------------------------------------------------------------------------------------------
                ! Get point time-series for intake(s)
                call HMC_Data_Forcing_TimeSeries_Intake(iID, iNCatch, iNRelease, iNData, iETime)
                !------------------------------------------------------------------------------------------
                
                !------------------------------------------------------------------------------------------
                ! Update logical variable to control forcing point section
                oHMC_Vars(iID)%bFileForcingTimeSeries = .true.
                !------------------------------------------------------------------------------------------

            else
                !------------------------------------------------------------------------------------------
                ! Info forcing point data
                call mprintf(.true., iINFO_Extra, ' Forcing point data (plant, intake) loaded previously! Skipping this step!')
                ! Info end
                call mprintf(.true., iINFO_Extra, ' Data :: Forcing time-series ... SKIPPED!' )
                !------------------------------------------------------------------------------------------
            endif

        endif
        !------------------------------------------------------------------------------------------

        !------------------------------------------------------------------------------------------
        ! Subroutine for reading unknown forcing data time-series
        if (iFlagTypeData_Forcing .ne. 1) then

            !------------------------------------------------------------------------------------------
            ! Choosing data type
            call mprintf(.true., iERROR, ' Using UNKNOWN data time-series forcing. Check settings file!')
            !------------------------------------------------------------------------------------------

        endif
        !------------------------------------------------------------------------------------------

        !------------------------------------------------------------------------------------------
        ! Info end
        call mprintf(.true., iINFO_Extra, ' Data :: Forcing time-series ... OK' )
        !------------------------------------------------------------------------------------------
            
    end subroutine HMC_Data_Forcing_TimeSeries_Cpl
    !------------------------------------------------------------------------------------------
    
    !------------------------------------------------------------------------------------------
    ! Subroutine to get time series of plant(s)
    subroutine HMC_Data_Forcing_TimeSeries_Plant(iID, iNPlant, iNData, iETime)

        !------------------------------------------------------------------------------------------
        ! Variable(s) declaration
        integer(kind = 4)           :: iID
        integer(kind = 4)           :: iP, iI, iJ, iErr
        integer(kind = 4)           :: iNPlant, iNData, iDtDataForcing, iETime
        real(kind = 4)              :: dVarAreaCell
        
        character(len = 256)        :: sPathData
        character(len = 256)        :: sNamePlant
        character(len = 256)        :: sFilePlant
        
        real(kind = 4), dimension(iNData)              :: a1dVar, a1dVarHydro
        real(kind = 4), dimension(iNPlant, iETime)     :: a2dVarHydro

        logical                     :: bFileExist
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Initialize variable(s)
        a1dVar = 0.0; a1dVarHydro = 0.0
        a2dVarHydro = 0.0; 
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Info start
        call mprintf(.true., iINFO_Extra, ' Data :: Forcing time-series :: ASCII :: Plant ... ')

        ! Debug
        call mprintf(.true., iINFO_Extra, checkarray(a2dVarHydro(:,2), 'HYDRO PLANT START') )
        !------------------------------------------------------------------------------------------

        !------------------------------------------------------------------------------------------
        ! Check plant(s) availability
        if (iNPlant .gt. 0) then
        
            !------------------------------------------------------------------------------------------
            ! Get global information
            sPathData = oHMC_Namelist(iID)%sPathData_Forcing_TimeSeries
            iDtDataForcing = oHMC_Namelist(iID)%iDtData_Forcing
            !------------------------------------------------------------------------------------------

            !------------------------------------------------------------------------------------------
            ! Cycle on plant(s)
            do iP = 1, iNPlant

                !------------------------------------------------------------------------------------------
                ! Initialize step variable(s)
                a1dVar = 0.0; iI = 0; iJ = 0; dVarAreaCell = 0.0
                !------------------------------------------------------------------------------------------

                !------------------------------------------------------------------------------------------
                ! Get global information
                iI = oHMC_Vars(iID)%a2iXYPlant(iP,2); iJ = oHMC_Vars(iID)%a2iXYPlant(iP,1);
                dVarAreaCell = oHMC_Vars(iID)%a2dAreaCell(iI, iJ)
                
                ! Plant name and filename data
                sNamePlant = oHMC_Vars(iID)%a1sNamePlant(iP)
                sFilePlant = trim(sPathData)//'hmc.forcing-ts.plant_'//trim(sNamePlant)//'.txt'
                !------------------------------------------------------------------------------------------
                
                !------------------------------------------------------------------------------------------
                ! Check area cell
                if (dVarAreaCell.lt.0) then
                    call mprintf(.true., iWARN, &
                        ' Area cell for '//trim(sNamePlant)//' plant is undefined! Model will use a default value!')
                    dVarAreaCell = oHMC_Vars(iID)%dDxM*oHMC_Vars(iID)%dDyM
                endif
                !------------------------------------------------------------------------------------------

                !------------------------------------------------------------------------------------------
                ! Info file
                call mprintf(.true., iINFO_Extra, ' Get filename (plant forcing time-series): '//trim(sFilePlant) )

                ! Check file availability
                inquire (file = sFilePlant, exist = bFileExist)
                if ( .not. bFileExist ) then
                    
                    !------------------------------------------------------------------------------------------
                    ! Warning message
                    call mprintf(.true., iWARN, ' No forcing time-series ASCII data found (plant): '//trim(sFilePlant) )
                    ! Info filename
                    call mprintf(.true., iINFO_Verbose, &
                                 ' Get filename (plant forcing time-series): '//trim(sFilePlant)//' ... FAILED')
                    ! No data values
                    a2dVarHydro(iP, :) = -9999.0    ! NoData dam value == -9999.0 NOT CHANGE
                    !------------------------------------------------------------------------------------------
                    
                else
                    
                    !------------------------------------------------------------------------------------------
                    ! Read ASCII file
                    call HMC_Tools_IO_Get1d_ASCII(sFilePlant, a1dVar, iNData, .false. , iErr)
                    !------------------------------------------------------------------------------------------

                    !------------------------------------------------------------------------------------------
                    ! Check data
                    if (.not. all(a1dVar.eq.-9999.0) ) then

                        !------------------------------------------------------------------------------------------
                        ! Control data 
                        where (a1dVar .gt. 0.0)
                            ! from m^3/s to mm/h
                            a1dVarHydro = a1dVar*1000*dble(iDtDataForcing)/dVarAreaCell
                        elsewhere
                            a1dVarHydro = 0.0
                        endwhere
                        
                        ! Smooth data
                        !call HMC_Tools_Generic_SmoothTimeSeries(a1dVarHydro, iNData, 2)
                        !------------------------------------------------------------------------------------------
                        
                        !------------------------------------------------------------------------------------------
                        ! Save data for each dam in one workspace (from start to simulation length)
                        a2dVarHydro(iP, 1 : iNData) = a1dVarHydro       
                        
                        ! No data condition for extra time steps
                        if (iNData .lt. iETime) then
                            a2dVarHydro(iP, iNData + 1 : iETime) = 0.0 ! Set to zero for value in extra time step(s)
                        endif
                        !------------------------------------------------------------------------------------------

                        !------------------------------------------------------------------------------------------
                    else
                        !------------------------------------------------------------------------------------------
                        ! No data available
                        call mprintf(.true., iWARN, ' All plant values are undefined!')
                        a2dVarHydro(iP, :) = -9999.0 ! NoData dam value == -9999.0 NOT CHANGE
                        !------------------------------------------------------------------------------------------
                    endif
                    !------------------------------------------------------------------------------------------
                    
                endif
                !------------------------------------------------------------------------------------------
 
            enddo 
            !------------------------------------------------------------------------------------------
            
            !------------------------------------------------------------------------------------------
            ! Info end
            call mprintf(.true., iINFO_Extra, ' Data :: Forcing time-series :: ASCII :: Plant ... OK')
            !------------------------------------------------------------------------------------------
            
        else
            !------------------------------------------------------------------------------------------
            ! No plant(s)
            a2dVarHydro = -9999.0 ! NoData dam value == -9999.0 NOT CHANGE
            ! Info end
            call mprintf(.true., iINFO_Extra, ' Data :: Forcing time-series :: ASCII :: Plant ... SKIPPED')
            !------------------------------------------------------------------------------------------
        endif
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Debug
        call mprintf(.true., iINFO_Extra, checkarray(a2dVarHydro(:,2), 'HYDRO PLANT END') )
        
        ! Pass local variable(s) to global workspace
        oHMC_Vars(iID)%a2dHydroPlant = a2dVarHydro
        !------------------------------------------------------------------------------------------
        
    end subroutine HMC_Data_Forcing_TimeSeries_Plant
    !------------------------------------------------------------------------------------------
    
    !------------------------------------------------------------------------------------------
    ! Subroutine to get time series of intake(s)
    subroutine HMC_Data_Forcing_TimeSeries_Intake(iID, iNCatch, iNRelease, iNData, iETime)
        
        !------------------------------------------------------------------------------------------
        ! Variable(s) declaration
        integer(kind = 4)           :: iID
        integer(kind = 4)           :: iR, iP, iStep, iI, iJ, iErr, iTimeShift, iETime
        integer(kind = 4)           :: iNCatch, iNRelease, iNData, iDtDataForcing
        integer(kind = 4)           :: iLenStr
        real(kind = 4)              :: dAreaCell, dTCorrCatch, dWeigthCatch
        real(kind = 4)              :: dQMaxRelease
        
        character(len = 256)        :: sPathData
        character(len = 256)        :: sNameRelease, sNameCatch
        character(len = 256)        :: sFileRelease
        
        character(len = 256), dimension(iNData)                     :: a1sVar 
        real(kind = 4), dimension(iNData)                           :: a1dVar
        real(kind = 4), dimension(iNData)                           :: a1dVarHydroO
        real(kind = 4), dimension(iNData)                           :: a1dVarHydroR
        real(kind = 4), dimension(iNData)                           :: a1dVarHydroC
       
        real(kind = 4), dimension(iNRelease, iETime)                :: a2dVarHydroR
        real(kind = 4), dimension(iNCatch, iETime)                  :: a2dVarHydroC
        
        logical, dimension(iNCatch)                                 :: a1bVarCheck
   
        logical                     :: bFileExist
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Initialize variable(s)
        a1sVar = ''; a1dVar = 0.0; 
        a1dVarHydroO = 0.0; a1dVarHydroR = 0.0; a1dVarHydroC = 0.0;
        a2dVarHydroR = 0.0; a2dVarHydroC = 0.0; 
        
        a1bVarCheck = .false.
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Info start
        call mprintf(.true., iINFO_Extra, ' Data :: Forcing time-series :: ASCII :: Intake (Releases and Catches) ... ')

        ! Debug
        call mprintf(.true., iINFO_Extra, checkarray(a2dVarHydroC(:,2), 'HYDRO CATCH START') )
        call mprintf(.true., iINFO_Extra, checkarray(a2dVarHydroR(:,2), 'HYDRO RELEASE START') )
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Check release(s) availability
        if (iNRelease .gt. 0) then
        
            !------------------------------------------------------------------------------------------
            ! Get global information
            sPathData = oHMC_Namelist(iID)%sPathData_Forcing_TimeSeries
            iDtDataForcing = oHMC_Namelist(iID)%iDtData_Forcing
            !------------------------------------------------------------------------------------------

            !------------------------------------------------------------------------------------------
            ! Cycle on release(s)
            do iR = 1, iNRelease

                !------------------------------------------------------------------------------------------
                ! Initialize step variable(s)
                a1dVar = -9999.0; iI = 0; iJ = 0; dAreaCell = 0.0
                !------------------------------------------------------------------------------------------

                !------------------------------------------------------------------------------------------
                ! Get global information
                iI = oHMC_Vars(iID)%a2iXYRelease(iR,2); iJ = oHMC_Vars(iID)%a2iXYRelease(iR,1);
                dAreaCell = oHMC_Vars(iID)%a2dAreaCell(iI, iJ)
                dQMaxRelease = oHMC_Vars(iID)%a1dQMaxRelease(iR)

                ! Release name and filename data
                sNameRelease = oHMC_Vars(iID)%a1sNameRelease(iR)
                sFileRelease = trim(sPathData)//'hmc.forcing-ts.plant_'//trim(sNameRelease)//'.txt'
                !------------------------------------------------------------------------------------------
                
                !------------------------------------------------------------------------------------------
                ! Check area cell
                if (dAreaCell.lt.0) then
                    call mprintf(.true., iWARN, &
                        ' Area cell for '//trim(sNameRelease)//' release is undefined! Model will use a default value!')
                    dAreaCell = oHMC_Vars(iID)%dDxM*oHMC_Vars(iID)%dDyM
                endif
                !------------------------------------------------------------------------------------------
                
                !------------------------------------------------------------------------------------------
                ! Info file
                call mprintf(.true., iINFO_Extra, ' Get filename (intake forcing time-series): '//trim(sFileRelease) )
                
                ! Check file availability
                inquire (file = sFileRelease, exist = bFileExist)
                if ( .not. bFileExist ) then
                    
                    !------------------------------------------------------------------------------------------
                    ! Warning message
                    call mprintf(.true., iWARN, &
                        ' No forcing time-series ASCII data found (release): '//trim(sFileRelease)//';'// &
                        ' model will use 80% of max release discharge!')
                    ! Info filename
                    call mprintf(.true., iINFO_Verbose, &
                                 ' Get filename (intake forcing time-series): '//trim(sFileRelease)//' ... FAILED')

                    ! Default value if plant data file is not available
                    a1dVarHydroO = dQMaxRelease*0.8  ! m^3/s
                    a1dVarHydroR = dQMaxRelease*1000*real(iDtDataForcing)/dAreaCell*0.8 ! from m^3/s to mm/h
                    !------------------------------------------------------------------------------------------
                    
                else
                
                    !------------------------------------------------------------------------------------------
                    ! Read ASCII file
                    call HMC_Tools_IO_Get1d_ASCII(sFileRelease, a1dVar, iNData, .false. , iErr)
                    !------------------------------------------------------------------------------------------
                    
                    !------------------------------------------------------------------------------------------
                    ! Check data
                    if (.not. all(a1dVar.eq.-9999.)) then

                        !------------------------------------------------------------------------------------------
                        ! Control data 
                        where (a1dVar .gt. 0.0)
                            a1dVarHydroO = a1dVar*1.0   ! m^3/s
                            a1dVarHydroR = a1dVar*1000*real(iDtDataForcing)/dAreaCell   ! from m^3/s to mm/h
                        elsewhere
                            a1dVarHydroO = 0.0
                            a1dVarHydroR = 0.0
                        endwhere

                        ! Smooth timeseries data
                        !call HMC_Tools_Generic_SmoothTimeSeries(a1dVarHydroO, iNData, 3)
                        !call HMC_Tools_Generic_SmoothTimeSeries(a1dVarHydroR, iNData, 3)
                        !------------------------------------------------------------------------------------------
 
                    else
                        
                        !------------------------------------------------------------------------------------------
                        ! No data available
                        call mprintf(.true., iWARN, ' All catch(es) values are undefined! Check forcing data!')
                        a1dVarHydroO = 0.0
                        a1dVarHydroR = 0.0
                        !------------------------------------------------------------------------------------------
                        
                    endif
                    !------------------------------------------------------------------------------------------
                    
                endif
                !------------------------------------------------------------------------------------------
                
                !------------------------------------------------------------------------------------------
                ! Cycle on catch(es)
                do iP = 1, iNCatch

                    !------------------------------------------------------------------------------------------
                    ! Get release and catch name
                    sNameRelease = oHMC_Vars(iID)%a1sNameRelease(iR)
                    sNameCatch = oHMC_Vars(iID)%a1sNameCatch(iP)

                    iLenStr = len(trim(sNameRelease))
                    !------------------------------------------------------------------------------------------

                    !------------------------------------------------------------------------------------------
                    ! Check release and catch name(s9
                    if ( sNameRelease(1:iLenStr) .eq. sNameCatch(1:iLenStr) ) then

                        !------------------------------------------------------------------------------------------
                        ! Get info catch
                        dTCorrCatch = oHMC_Vars(iID)%a1dTCorrCatch(iP)
                        dWeigthCatch = oHMC_Vars(iID)%a1dWeigthCatch(iP)

                        iTimeShift = nint(dTCorrCatch*60/iDtDataForcing)
                        !------------------------------------------------------------------------------------------

                        !------------------------------------------------------------------------------------------
                        ! Compute hydro arrays for catch(es)
                        do iStep = 1, iNData - 1
                            if ( (iStep - iTimeShift).ge.1 ) then
                                a1dVarHydroC(iStep - iTimeShift) = a1dVarHydroO(iStep)*dWeigthCatch ! [m^3/s]
                            endif
                        enddo
                        do iStep = iNData - 1 - iTimeShift, iNData - 1
                            a1dVarHydroC(iStep) = a1dVarHydroO(iNData - 1)*dWeigthCatch
                        enddo
                        !------------------------------------------------------------------------------------------
                        
                        !------------------------------------------------------------------------------------------
                        ! Save data for each catch in one workspace (from start to simulation length)
                        a2dVarHydroC(iP, 1 : iNData) = a1dVarHydroC   
                        
                        ! No data condition for extra time steps
                        if (iNData .lt. iETime) then
                            a2dVarHydroC(iP, iNData + 1 : iETime) = 0.0   
                        endif

                        a1bVarCheck(iP) = .true.
                        !------------------------------------------------------------------------------------------
                    else
                        !------------------------------------------------------------------------------------------
                        ! Link between release and catch does not exist
                        a1bVarCheck(iP) = .false.
                        !------------------------------------------------------------------------------------------
                    endif
                    !------------------------------------------------------------------------------------------

                enddo
                !------------------------------------------------------------------------------------------

                !------------------------------------------------------------------------------------------
                ! Check link(s)
                if (all(a1bVarCheck .eqv. .false.)) then
                    call mprintf(.true., iWARN, &
                    ' Release '//trim(sNameRelease)//' does not have a catch! '// &
                    ' Check time-series forcing data (catch)!')
                endif
                !------------------------------------------------------------------------------------------

                !------------------------------------------------------------------------------------------
                ! Save data for each release in one workspace (from start to simulation length)
                a2dVarHydroR(iR, 1 : iNData) = a1dVarHydroR       
                
                ! No data condition for extra time steps
                if (iNData .lt. iETime) then
                    a2dVarHydroR(iR, iNData + 1 : iETime) = 0.0  
                endif
                !------------------------------------------------------------------------------------------
                 
            enddo 
            !------------------------------------------------------------------------------------------
            
            !------------------------------------------------------------------------------------------
            ! Info end
            call mprintf(.true., iINFO_Extra, ' Data :: Forcing time-series :: ASCII :: Intake (Releases and Catches) ... OK')
            !------------------------------------------------------------------------------------------
            
        else
            !------------------------------------------------------------------------------------------
            ! No release(s) and no catch(es)
            a2dVarHydroR = 0.0
            a2dVarHydroC = 0.0
            ! Info end
            call mprintf(.true., iINFO_Extra, ' Data :: Forcing time-series :: ASCII :: Intake (Releases and Catches) ... SKIPPED')
            !------------------------------------------------------------------------------------------
        endif
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Debug
        call mprintf(.true., iINFO_Extra, checkarray(a2dVarHydroC(:,2), 'HYDRO CATCH END') )
        call mprintf(.true., iINFO_Extra, checkarray(a2dVarHydroR(:,2), 'HYDRO RELEASE END') )
        
        ! Pass local variable(s) to global workspace
        oHMC_Vars(iID)%a2dHydroRelease = a2dVarHydroR
        oHMC_Vars(iID)%a2dHydroCatch = a2dVarHydroC
        !------------------------------------------------------------------------------------------
        
    end subroutine HMC_Data_Forcing_TimeSeries_Intake
    !------------------------------------------------------------------------------------------
    
end module HMC_Module_Data_Forcing_TimeSeries
!------------------------------------------------------------------------------------------
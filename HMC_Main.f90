!******************************************************************************************
! EUPL
! Code Version 1.0
! This code is distributed following the European Union Public Licence:
! https://joinup.ec.europa.eu/software/page/eupl/licence-eupl
!******************************************************************************************

!******************************************************************************************
! HYDROLOGICAL MODEL CONTINUUM
! Version 3.1.1
! Date: 2020/03/30
!
! 
! 0) COMMAND LINE:
!   TYPE 1:
!       ./Continuum.x --> parameter(s): uc=20 uh= 1.5 ct=0.5 cf=0.02 domain=marche cpi=0.3 Rf=1 Vmax=500 slopemax=70
!       ./Continuum.x 20 1.5 0.5 0.02 marche 0.3 500 1 70
!   TYPE 2:
!       ./Continuum.x --> parameter(s): file.info.txt
!       ./Continuum.x marche.info.txt
!
! 1) SET NETCDF LIBRARY CONFIGURATION:
! Add in: Project --> Properties --> Linker --> Libraries
!   netcdff.a and netcdff.so (note double f for fortran libraries)
! Add in: Project --> Properties --> Linker --> Additional Options:
!   -I/home/fabio/Documents/Work_Area/Code_Development/Library/nc4_fortran-4.4.2/include/ 
!   -L/home/fabio/Documents/Work_Area/Code_Development/Library/nc4_fortran-4.4.2/lib/ 
!   -lnetcdff -lnetcdff   
! Add in: Project --> Properties --> Fortran Compiler --> Additional Options:
!   -I//home/fabio/Documents/Work_Area/Code_Development/Library/nc4_fortran-4.4.2/include/ 
!   -L/home/fabio/Documents/Work_Area/Code_Development/Library/nc4_fortran-4.4.2/lib/ 
!   -lnetcdff -lnetcdff
! Add in: Project --> Properties --> Fortran Compiler --> Additional Options:
!   gfortran: -cpp -DLIB_NC
!   ifort: -fpp -DLIB_NC            
! Add in: Project --> Properties --> Run --> Environment --> NewValue
!   LD_LIBRARY_PATH $LD_LIBRARY_PATH:/home/fabio/Documents/Work_Area/Code_Development/Library/nc4_fortran-4.4.2/lib/
!
! 2) SET COMMAND LINE OPTION:
! Add in: Project --> Properties --> Debug --> Debug Command
!   TYPE 1:
!       ${OUTPUT_PATH} 20 1.5 0.5 0.02 marche 0.3 500 1 70  # marche
!       ${OUTPUT_PATH} 30 1.7 0.4 0.1 vda 0.15 1000.0 5 30  # vda
!   TYPE 2:
!       ${OUTPUT_PATH} marche.info.txt
!       ${OUTPUT_PATH} vda.info.txt
!
! 3) DEBUG USING GNUPLOT (gnufor2):
! call surf(dble(VAR),pm3d='pm3d implicit map', palette='rgbformulae 31, -11, 32')
! 4) DEBUG USING FORTRAN+MATLAB: [ var*8 = dble(var*4) ]
! call debug_2dVar(dble(a2dVarTa), iRows, iCols, 1) + debug_2dVar.m
!
! NOTE ABOUT FP_MARCHE:
!   1) iCols 643 iY jdim iRows 534 iX idim
!
! NOTE ABOUT FP_TEVERE:
!   1) in HMC_Module_Data_Static_Gridded:867: coefficient equal to 0.0009 (previously 0.0007) 
!   2) in HMC_Phys_Convolution_Apps_Horton:1701: change intensity equation
!   3) in HMC_Phys_Convolution_Apps_SurfaceFlow:924: add DeepFlow terms to intensity update
!   4) in HMC_Phys_Convolution_Apps_SurfaceFlow:1031: delete 0.1 in UC equation
!
! NOTE ABOUT FP_ITALY VERSION:
!   1) in HMC_Phys_Convolution_Apps_SurfaceFlow:1031: delete 0.1 in UC equation
!******************************************************************************************

!------------------------------------------------------------------------------------------
! Hydrological Model Continuum main 
program HMC_Main
    
    !------------------------------------------------------------------------------------------
    ! Use and implicit none
    use HMC_Module_Args,                    only:   HMC_Args_Read
    
    use HMC_Module_Namelist,                only:   oHMC_Namelist, HMC_Namelist_Read
    
    use HMC_Module_Info_Gridded,            only:   HMC_Info_Gridded_GetDims_Static, &
                                                     HMC_Info_Gridded_GetDims_Forcing, &
                                                     HMC_Info_Gridded_GetGeo_Static, &
                                                     HMC_Info_Gridded_GetGeo_Forcing
                                                    
    use HMC_Module_Info_Point,              only:   HMC_Info_Point_Section_GetDims, &
                                                     HMC_Info_Point_WaterBody_GetDims, &  
                                                     HMC_Info_Point_HydraulicStructure_GetDims
    
    use HMC_Module_Info_Time

    use HMC_Module_Vars_Loader,             only:   HMC_Type_Vars, oHMC_Vars
    use HMC_Module_Vars_Manager,            only:   HMC_Vars_InitDefault, HMC_Vars_Allocate
  
    use HMC_Module_Tools_Time,              only:   HMC_Tools_Time_GetNewDate, &
                                                     HMC_Tools_Time_Printer
    use HMC_Module_Tools_Debug
    
    use HMC_Module_Data_Static_Gridded,     only:   HMC_Data_Static_Gridded_Cpl
    use HMC_Module_Data_Static_Point,       only:   HMC_Data_Static_Point_Cpl
    use HMC_Module_Data_Forcing_Gridded,    only:   HMC_Data_Forcing_Gridded_Cpl 
    use HMC_Module_Data_Forcing_Point,      only:   HMC_Data_Forcing_Point_Cpl
    use HMC_Module_Data_Forcing_TimeSeries, only:   HMC_Data_Forcing_TimeSeries_Cpl
    use HMC_Module_Data_Output_Gridded,     only:   HMC_Data_Output_Gridded_Cpl
    use HMC_Module_Data_Output_Point,       only:   HMC_Data_Output_Point_Cpl
    use HMC_Module_Data_Output_TimeSeries,  only:   HMC_Data_Output_TimeSeries_Cpl
    use HMC_Module_Data_State_Gridded,      only:   HMC_Data_State_Gridded_Cpl
    use HMC_Module_Data_State_Point,        only:   HMC_Data_State_Point_Cpl
    use HMC_Module_Data_Restart_Gridded,    only:   HMC_Data_Restart_Gridded_Cpl
    use HMC_Module_Data_Restart_Point,      only:   HMC_Data_Restart_Point_Cpl
    use HMC_Module_Data_Updating_Gridded,   only:   HMC_Data_Updating_Gridded_Cpl
    
    use HMC_Module_Phys,                    only:   HMC_Phys_Cpl
    use HMC_Module_Tools_Generic,           only:   getProcessID

    implicit none
    !------------------------------------------------------------------------------------------
    
    !------------------------------------------------------------------------------------------
    ! Variable(s) declaration
    integer(kind = 4)               :: iArgsType
    character(len = 700)            :: sFileInfo
    
    integer(kind = 4)               :: iID
    integer(kind = 4)               :: iColsL, iRowsL, iColsF, iRowsF, iTime, iT
    integer(kind = 4)               :: iNSection
    integer(kind = 4)               :: iNLake, iNDam, iNPlant, iNJoint, iNCatch, iNRelease
    integer(kind = 4)               :: iDaySteps, iTMarkedSteps
    real(kind = 4)                  :: dUc, dUh, dCt, dCf, dCPI, dWTableHbr, dKSatRatio, dSlopeMax
    character(len = 256)            :: sDomainName

    integer(kind = 4)               :: iDtModel, iDtPhysConv
    integer(kind = 4)               :: iDtData_Forcing
    integer(kind = 4)               :: iDtData_Updating
    integer(kind = 4)               :: iDtData_Output_Gridded, iDtData_Output_Point
    integer(kind = 4)               :: iDtData_State_Gridded, iDtData_State_Point
    integer(kind = 4)               :: iSimLength, iNTime, iETime, iNData
    
    character(len = 19)             :: sTimeOld, sTimeForcing, sTimeUpdating, sTimeRestart, sTimeNew
    character(len = 19)             :: sTimeOutput_Gridded, sTimeOutput_Point
    character(len = 19)             :: sTimeState_Gridded, sTimeState_Point 
    character(len = 19)             :: sTimeEndLAI, sTimeEndFC
    character(len = 256)            :: sTime, sNTime, sDtModel
    character(len = 256)            :: sPID

    
    character(len=10)               :: sDateRunStart, sDateRunStep, sDateRunEnd
    character(len=12)               :: sTimeRunStart, sTimeRunStep, sTimeRunEnd
    !------------------------------------------------------------------------------------------
    
    !------------------------------------------------------------------------------------------
    ! Initialize variable(s)
    sFileInfo = ""; iArgsType = -9999;
    iID = 1
    iColsL = -9999; iRowsL = -9999; iColsF = -9999; iRowsF = -9999; iTime = -9999;
    iDaySteps = 0; iTMarkedSteps = -9999; iSimLength = 0;
    iNTime = -9999; iT = -9999;
    iNSection = -9999; iNLake = -9999; iNDam = -9999; 
    iNPlant = -9999; iNJoint = -9999; iNCatch = -9999; iNRelease = -9999;
    dUc = 0.0; dUh = 0.0; dCt = 0.0; dCf = 0.0; dCPI = 0.0; 
    dWTableHbr = 0.0; dKSatRatio = 0.0; dSlopeMax = 0.0; sDomainName = ""; 
    sTimeOld = ""; sTimeForcing = ""; sTimeUpdating = ""; sTimeRestart = ""; sTimeNew = ""; 
    sTimeOutput_Gridded = ""; sTimeOutput_Point = ""; 
    sTimeState_Gridded = "";  sTimeState_Point = ""; 
    sTimeEndLAI = ""; sTimeEndFC = "";
    !------------------------------------------------------------------------------------------
    
    !------------------------------------------------------------------------------------------
    ! Get unique process ID
    sPID = adjustl(getProcessID())
    !------------------------------------------------------------------------------------------
    
    !------------------------------------------------------------------------------------------
    ! Call time start
    call HMC_Tools_Time_Printer(sDateRunStart, sTimeRunStart)
    !------------------------------------------------------------------------------------------
        
    !------------------------------------------------------------------------------------------
    ! Read command line argument(s)
    call HMC_Args_Read(dUc, dUh, dCt, dCf, dCPI, dWTableHbr, dKSatRatio, dSlopeMax, &
                       sDomainName, &
                       sFileInfo, iArgsType)
    !------------------------------------------------------------------------------------------
    
    !------------------------------------------------------------------------------------------
    ! Read namelist file
    call HMC_Namelist_Read(dUc, dUh, dCt, dCf, dCPI, dWTableHbr, dKSatRatio, dSlopeMax, &
                          sDomainName, & 
                          sFileInfo, iArgsType, &
                          oHMC_Namelist(iID))
                          
    ! Start simulation message
    call mprintf(.true., iINFO_Basic, ' SIMULATION START :: Time: ' //trim(sDateRunStart)//' '//trim(sTimeRunStart) )
    
    ! Domain name
    sDomainName = oHMC_Namelist(iID)%sDomainName
                          
    ! Time definition(s)                
    sTimeOld = oHMC_Namelist(iID)%sTimeStart
    sTimeForcing = oHMC_Namelist(iID)%sTimeStart
    sTimeUpdating = oHMC_Namelist(iID)%sTimeStart
    sTimeOutput_Gridded = oHMC_Namelist(iID)%sTimeStart
    sTimeOutput_Point = oHMC_Namelist(iID)%sTimeStart
    sTimeRestart = oHMC_Namelist(iID)%sTimeRestart
    
    call HMC_Tools_Time_GetNewDate(sTimeState_Gridded, oHMC_Namelist(iID)%sTimeStart, &
        nint(real(oHMC_Namelist(iID)%iDtData_State_Gridded - real(oHMC_Namelist(iID)%iDtData_Forcing))))

    call HMC_Tools_Time_GetNewDate(sTimeState_Point, oHMC_Namelist(iID)%sTimeStart, &
        nint(real(oHMC_Namelist(iID)%iDtData_State_Gridded - real(oHMC_Namelist(iID)%iDtData_Forcing))))

    ! Model dt(s)
    iDtModel = oHMC_Namelist(iID)%iDtModel
    iDtData_Forcing = oHMC_Namelist(iID)%iDtData_Forcing
    iDtData_Updating = oHMC_Namelist(iID)%iDtData_Updating
    iDtData_Output_Gridded = oHMC_Namelist(iID)%iDtData_Output_Gridded
    iDtData_Output_Point = oHMC_Namelist(iID)%iDtData_Output_Point
    iDtData_State_Gridded = oHMC_Namelist(iID)%iDtData_State_Gridded
    iDtData_State_Point = oHMC_Namelist(iID)%iDtData_State_Point
    iDtPhysConv = oHMC_Namelist(iID)%iDtPhysConv
    iSimLength = oHMC_Namelist(iID)%iSimLength

    ! Simulation step(s)
    iNTime = oHMC_Namelist(iID)%iNTime
    ! Data step(s)
    iNData = oHMC_Namelist(iID)%iNData
    !------------------------------------------------------------------------------------------
    
    !------------------------------------------------------------------------------------------
    ! Get static data dimension(s)
    call HMC_Info_Gridded_GetDims_Static(iID, iRowsL, iColsL)
    ! Get forcing data dimension(s)
    call HMC_Info_Gridded_GetDims_Forcing(iID, iRowsF, iColsF)
    
    ! Get section data dimension(s)
    call HMC_Info_Point_Section_GetDims(iID, iNSection)
    ! Get water-body data dimension(s)                  
    call HMC_Info_Point_WaterBody_GetDims(iID, iNLake)
    ! Get hydraulic structure data dimension(s)                  
    call HMC_Info_Point_HydraulicStructure_GetDims(iID, iNDam, iNPlant, iNJoint, iNCatch, iNRelease)
    
    ! Get time dimension(s)
    call HMC_Info_Time_GetDims(iID, iRowsL, iColsL, iETime, iDaySteps, iTMarkedSteps)
    !------------------------------------------------------------------------------------------
    
    !------------------------------------------------------------------------------------------
    ! Allocate static and dynamic global variable(s)
    call HMC_Vars_Allocate( iID, & 
                            iRowsL, iColsL, & 
                            iNSection, &
                            iNLake, iNDam, iNPlant, iNJoint, iNCatch, iNRelease, &
                            iDaySteps, iTMarkedSteps, &
                            iNData, iETime )
                            
    ! Initialize static and dynamic global variable(s) using default value(s)
    call HMC_Vars_InitDefault(iID)
    !------------------------------------------------------------------------------------------
    
    !------------------------------------------------------------------------------------------
    ! Get land data static geographical information
    call HMC_Info_Gridded_GetGeo_Static(iID)
    ! Get land data forcing geographical information
    call HMC_Info_Gridded_GetGeo_Forcing(iID)
    !------------------------------------------------------------------------------------------
    
    !------------------------------------------------------------------------------------------
    ! Initialize static gridded variable(s)
    call HMC_Data_Static_Gridded_Cpl(iID, iRowsL, iColsL)
    
    ! Initialize static point variable(s)
    call HMC_Data_Static_Point_Cpl(iID, iRowsL, iColsL, &
                                   iNSection, &
                                   iNLake, &
                                   iNDam, iNPlant, iNJoint, iNCatch, iNRelease)
    !------------------------------------------------------------------------------------------
                      
    !------------------------------------------------------------------------------------------
    ! Initialize restart data
    call HMC_Data_Restart_Gridded_Cpl(iID, sTimeRestart, &
                                      1, iRowsL, 1, iColsL, &
                                      iDaySteps, iTMarkedSteps)                          
    call HMC_Data_Restart_Point_Cpl(iID, sTimeRestart, &
                                    iNDam, iNLake)
    !------------------------------------------------------------------------------------------
                            
    !------------------------------------------------------------------------------------------
    ! Cycling on time period (and extra time steps)
    TimeLoop : do iTime = 1, iETime
        
        !------------------------------------------------------------------------------------------
        ! Time step start
        write(sTime, *) iTime; write(sNTime, *) iNTime;  write(sDtModel, *) iDtModel
        call mprintf(.true., iINFO_Basic, ' ===== TIME STEP START :: Date, iT, iNTime, iDt, Clock :: ' &
                                          //sTimeOld//' '//trim(sTime)//' '// &
                                          trim(sNTime)//' '//trim(sDtModel)// ' ===== ')
        ! Clock step start                         
        call HMC_Tools_Time_Printer(sDateRunStep, sTimeRunStep)
        call mprintf(.true., iINFO_Basic, ' STEP CLOCK START :: Time: ' //trim(sDateRunStep)//' '//trim(sTimeRunStep) )
        
        ! Update time step
        oHMC_Vars(iID)%iTime = iTime; oHMC_Vars(iID)%sTimeStep = sTimeOld
        !------------------------------------------------------------------------------------------
       
        !------------------------------------------------------------------------------------------
        ! Forcing data step
        if(sTimeOld == sTimeForcing) then

            ! Get forcing data point
            call HMC_Data_Forcing_Point_Cpl(iID, sTimeOld, &
                                        iNData, iETime,  &
                                        iNLake, iNDam, iNPlant, iNJoint, iNCatch, iNRelease)
                                        
            ! Get forcing data time-series
            call HMC_Data_Forcing_TimeSeries_Cpl(iID, sTimeOld, &
                                        iNData, iETime,  &
                                        iNLake, iNDam, iNPlant, iNJoint, iNCatch, iNRelease)

            ! Get forcing gridded data
            call HMC_Data_Forcing_Gridded_Cpl( iID, sTimeForcing, &
                                       1, iRowsL, 1, iColsL, &
                                       1, iRowsF, 1, iColsF)
 
            ! Update data forcing time
            call HMC_Tools_Time_GetNewDate(sTimeNew, sTimeForcing, nint(real(iDtData_Forcing)))
            sTimeForcing = sTimeNew
        endif
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Updating data step
        if(sTimeOld == sTimeUpdating) then

            ! Get updating gridded data
            call HMC_Data_Updating_Gridded_Cpl( iID, sTimeUpdating, &
                                       1, iRowsL, 1, iColsL)
 
            ! Update data forcing time
            call HMC_Tools_Time_GetNewDate(sTimeNew, sTimeUpdating, nint(real(iDtData_Updating)))
            sTimeUpdating = sTimeNew
        endif
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Physics step
        call HMC_Phys_Cpl(iID, &
                          1, iRowsL, 1, iColsL, &
                          iTime, iNTime, iETime, sTimeOld, &
                          iNSection, iNData, &
                          iNLake, iNDam, iNPlant, iNJoint, iNCatch, iNRelease, &
                          iDaySteps, iTMarkedSteps )
        !------------------------------------------------------------------------------------------
                    
        !------------------------------------------------------------------------------------------
        ! Output data step
        ! Save data output gridded
        if( (sTimeOld .eq. sTimeOutput_Gridded) .and. (iDtData_Output_Gridded .gt. 0) ) then
          
            ! Save output gridded data
            call HMC_Data_Output_Gridded_Cpl( iID, sTimeOutput_Gridded, &
                                              1, iRowsL, 1, iColsL, &
                                              iTime, iDaySteps)
                                                          
            ! Update data output time
            call HMC_Tools_Time_GetNewDate(sTimeNew, sTimeOutput_Gridded, nint(real(iDtData_Output_Gridded)))
            sTimeOutput_Gridded = sTimeNew
        endif
        
        ! Save data output point
        if( (sTimeOld .eq. sTimeOutput_Point) .and. (iDtData_Output_Point .gt. 0)) then
          
            ! Save output point data
            call HMC_Data_Output_Point_Cpl( iID, sTimeOutput_Point, &
                                            1, iRowsL, 1, iColsL, &
                                            iTime, iETime, &
                                            iNSection, iNData, &
                                            iNLake, iNDam, iNPlant, iNJoint, iNCatch, iNRelease)
                                              
            ! Update data output time
            call HMC_Tools_Time_GetNewDate(sTimeNew, sTimeOutput_Point, nint(real(iDtData_Output_Point)))
            sTimeOutput_Point = sTimeNew
        endif
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! State data step
        ! Save data state gridded
        if( (sTimeOld .eq. sTimeState_Gridded) .and. (iDtData_State_Gridded .gt. 0) ) then
          
            ! Save state gridded data
            call HMC_Data_State_Gridded_Cpl( iID, sTimeState_Gridded, &
                                             1, iRowsL, 1, iColsL, &
                                             iDaySteps, iTMarkedSteps)
                                       
            ! Update data state time
            call HMC_Tools_Time_GetNewDate(sTimeNew, sTimeState_Gridded, nint(real(iDtData_State_Gridded)))
            sTimeState_Gridded = sTimeNew
            
        endif
        
        ! Save data state point
        if( (sTimeOld .eq. sTimeState_Point) .and. (iDtData_State_Gridded .gt. 0) ) then
          
            ! Save state point data
            call HMC_Data_State_Point_Cpl( iID, sTimeState_Point, &
                                           iNDam, iNLake)
                                       
            ! Update data state time
            call HMC_Tools_Time_GetNewDate(sTimeNew, sTimeState_Point, nint(real(iDtData_State_Point)))
            sTimeState_Point = sTimeNew
            
        endif
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Save data time-series
        call HMC_Data_Output_TimeSeries_Cpl(iID, sTimeOld, &
                                            iTime, iETime, &
                                            iNSection, iNData, &
                                            iNLake, iNDam, iNPlant, iNJoint, iNCatch, iNRelease)
        !------------------------------------------------------------------------------------------
    
        !------------------------------------------------------------------------------------------
        ! Clock step end                          
        call HMC_Tools_Time_Printer(sDateRunStep, sTimeRunStep)
        call mprintf(.true., iINFO_Basic, ' STEP CLOCK END :: Time: ' //trim(sDateRunStep)//' '//trim(sTimeRunStep) )
        
        ! Global time updating
        call mprintf(.true., iINFO_Basic, ' ===== TIME STEP END :: Date, iT, iNTime, iDt :: ' &
                                          //sTimeOld//' '//trim(sTime)//' '// &
                                          trim(sNTime)//' '//trim(sDtModel)// ' ===== ')
                  
        call HMC_Tools_Time_GetNewDate(sTimeNew, sTimeOld, nint(real(iDtModel)))
        sTimeOld = sTimeNew
        !------------------------------------------------------------------------------------------

    enddo TimeLoop
    !------------------------------------------------------------------------------------------
    
    !------------------------------------------------------------------------------------------
    ! Call time end
    call HMC_Tools_Time_Printer(sDateRunEnd, sTimeRunEnd)
    ! Ending simulation message
    call mprintf(.true., iINFO_Basic, ' SIMULATION END :: Time: ' //trim(sDateRunEnd)//' '//trim(sTimeRunEnd) )
    call mprintf(.true., iINFO_Basic, '************************************************************************')
    !------------------------------------------------------------------------------------------

end program HMC_Main
!-----------------------------------------------------------------------------------

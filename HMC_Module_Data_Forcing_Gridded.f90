!------------------------------------------------------------------------------------------    
! File:   HMC_Module_Data_Forcing_Gridded.f90
! Author(s): Fabio Delogu, Francesco Silvestro, Simone Gabellani
!
! Created on April 22, 2015, 5:19 PM
!------------------------------------------------------------------------------------------

!------------------------------------------------------------------------------------------
! Module Header
module HMC_Module_Data_Forcing_Gridded
    
    !------------------------------------------------------------------------------------------
    ! External module(s) for all subroutine in this module
#ifdef LIB_NC
    use netcdf
#endif

    use HMC_Module_Namelist,        only:   oHMC_Namelist
    use HMC_Module_Vars_Loader,     only:   oHMC_Vars
    
    use HMC_Module_Tools_Debug

#ifdef LIB_NC
    use HMC_Module_Tools_IO,        only:   HMC_Tools_IO_Get2d_Binary_INT, &
                                            HMC_Tools_IO_Get2d_NC, &
#ifdef LIB_DYNARRAY
                                            HMC_Tools_IO_CheckVar_NC, &
#endif 
                                            check
#else
    use HMC_Module_Tools_IO,        only:   HMC_Tools_IO_Get2d_Binary_INT                                      
#endif                                    
                                                                                  
    use HMC_Module_Tools_Generic,   only:   HMC_Tools_Generic_ReplaceText, &
                                            HMC_Tools_Generic_SwitchGrid, &
                                            HMC_Tools_Generic_UnzipFile, &
                                            HMC_Tools_Generic_RemoveFile, &
                                            check2Dvar, getProcessID
                                            
    use HMC_Module_Tools_Time,      only:   HMC_Tools_Time_MonthVal, &
                                            HMC_Tools_Time_GetNewDate

    
    
    ! Implicit none for all subroutines in this module
    implicit none
    !------------------------------------------------------------------------------------------
    
contains
    
    !------------------------------------------------------------------------------------------
    ! Subroutine to manage forcing gridded data
    subroutine HMC_Data_Forcing_Gridded_Cpl( iID, sTime, &
                                     iRowsStartL, iRowsEndL, iColsStartL, iColsEndL, &
                                     iRowsStartF, iRowsEndF, iColsStartF, iColsEndF)
        
        !------------------------------------------------------------------------------------------
        ! Variable(s)
        integer(kind = 4)           :: iID
                                    
        integer(kind = 4)           :: iRowsStartL, iRowsEndL, iColsStartL, iColsEndL
        integer(kind = 4)           :: iRowsStartF, iRowsEndF, iColsStartF, iColsEndF
        integer(kind = 4)           :: iRowsL, iColsL, iRowsF, iColsF
        integer(kind = 4)           :: iFlagTypeData_Forcing
        integer(kind = 4)           :: iScaleFactor, iFlagDynVeg, iFlagEnergyBalance
        
        character(len = 19)         :: sTime

        character(len = 12)         :: sTimeMonth
        
        character(len = 256)        :: sPathData_Forcing
        
        real(kind = 4)              :: dVarLAI, dVarAlbedo
        
        character(len = 19)         :: sTimeEndLAI, sTimeStartLAI
        
        real(kind = 4), dimension(iRowsEndL - iRowsStartL + 1, &
                                  iColsEndL - iColsStartL + 1) ::   a2dVarRainL, a2dVarTaL, &
                                                                    a2dVarIncRadL, a2dVarWindL, & 
                                                                    a2dVarRelHumL, a2dVarPaL, &
                                                                    a2dVarAlbedoL, a2dVarLAIL, a2dVarFCL, &
                                                                    a2dVarAEvtL, a2dVarPEvtL
                                                                    
        real(kind = 4), dimension(iRowsEndF - iRowsStartF + 1, &
                                  iColsEndF - iColsStartF + 1) ::   a2dVarRainF, a2dVarTaF, &
                                                                    a2dVarIncRadF, a2dVarWindF, & 
                                                                    a2dVarRelHumF, a2dVarPaF, &
                                                                    a2dVarAlbedoF, a2dVarLAIF, a2dVarFCF, &
                                                                    a2dVarAEvtF, a2dVarPEvtF
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Forcing data (mandatory):
        !   a2dVarRainF         : rain [mm]
        !   a2dVarPaF           : atmospheric pressure [kPa]
        !   a2dVarTaF           : air temperature [C]
        !   a2dVarIncRadF       : incoming radiation [W/m^2]
        !   a2dVarWindF         : wind speed [m/s]
        !   a2dVarRelHumF       : relative humidity [%]
        
        ! Forcing data (optional):
        !   a2dVarAlbedoF       : albedo [0,1]
        !   a2dVarLAIF          : leaf area index [0,8] 
        !   a2dVarFCF           : fraction cover [-] 
        !   a2dVarAEvtF         : actual evapotranspiration [mm] 
        !   a2dVarPEvtF         : potential evapotranspiration [mm] 
        !------------------------------------------------------------------------------------------
                                                                                                                        
        !------------------------------------------------------------------------------------------
        ! Initialize variable(s)
        a2dVarRainF = -9999.0; a2dVarTaF = -9999.0; a2dVarIncRadF = -9999.0;  
        a2dVarWindF = -9999.0; a2dVarRelHumF = -9999.0;  a2dVarPaF = -9999.0; 
        a2dVarAlbedoF = -9999.0; a2dVarLAIF = -9999.0; a2dVarFCF = -9999.0;
        a2dVarAEvtF = -9999.0; a2dVarPEvtF = -9999.0;
        
        a2dVarRainL = -9999.0; a2dVarTaL = -9999.0; a2dVarIncRadL = -9999.0;
        a2dVarWindL = -9999.0; a2dVarRelHumL = -9999.0; a2dVarPaL = -9999.0;
        a2dVarAlbedoL = -9999.0; a2dVarLAIL = -9999.0; a2dVarFCL = -9999.0;
        a2dVarAEvtL = -9999.0; a2dVarPEvtL = -9999.0;
        
        sTimeEndLAI = ''; sTimeStartLAI = '';
        !------------------------------------------------------------------------------------------
                                                                                                
        !------------------------------------------------------------------------------------------
        ! Defining iRows and iCols (Land data)
        iRowsL = iRowsEndL - iRowsStartL + 1
        iColsL = iColsEndL - iColsStartL + 1
        ! Defining iRows and iCols (forcing data)
        iRowsF = iRowsEndF - iRowsStartF + 1
        iColsF = iColsEndF - iColsStartF + 1
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Get global information
        sPathData_Forcing = oHMC_Namelist(iID)%sPathData_Forcing_Gridded
        iFlagTypeData_Forcing = oHMC_Namelist(iID)%iFlagTypeData_Forcing_Gridded
        iScaleFactor = oHMC_Namelist(iID)%iScaleFactor
        iFlagDynVeg = oHMC_Namelist(iID)%iFlagDynVeg
        iFlagEnergyBalance = oHMC_Namelist(iID)%iFlagEnergyBalance
        
        sTimeEndLAI = oHMC_Vars(iID)%sTimeMaxLAI
        write(sTimeStartLAI,'(A,A,A)') sTime

        ! Info start
        call mprintf(.true., iINFO_Extra, ' Data :: Forcing gridded ... ' )
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Replace general path with specific time feature(s)
        call HMC_Tools_Generic_ReplaceText(sPathData_Forcing, '$yyyy', sTime(1:4))
        call HMC_Tools_Generic_ReplaceText(sPathData_Forcing, '$mm', sTime(6:7))
        call HMC_Tools_Generic_ReplaceText(sPathData_Forcing, '$dd', sTime(9:10))
        call HMC_Tools_Generic_ReplaceText(sPathData_Forcing, '$HH', sTime(12:13))
        call HMC_Tools_Generic_ReplaceText(sPathData_Forcing, '$MM', sTime(15:16))
        
        ! Checking date
        write(sTimeMonth,'(A,A,A)') sTime(1:4), sTime(6:7), sTime(9:10)
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Check time step (iT)
        if (oHMC_Vars(iID)%iTime .lt. oHMC_Namelist(iID)%iNTime) then
 
            !------------------------------------------------------------------------------------------
            ! Subroutine for reading sequential netCDF forcing data 
            if (iFlagTypeData_Forcing == 2) then

                !------------------------------------------------------------------------------------------
                ! Call subroutine to get forcing data in netCDF format
#ifdef LIB_NC
                call HMC_Data_Forcing_Gridded_NC(iID, &
                                        sPathData_Forcing, &
                                        iRowsF, iColsF, &
                                        sTime, &
                                        a2dVarRainF, a2dVarTaF, a2dVarIncRadF, &
                                        a2dVarWindF, a2dVarRelHumF, a2dVarPaF, &
                                        a2dVarAlbedoF, a2dVarLAIF, a2dVarFCF, &
                                        a2dVarAEvtF, a2dVarPEvtF)
#else   
                ! Redefinition of forcing data flag (if netCDF library is not linked)
                iFlagTypeData_Forcing = 1 
                call mprintf(.true., iWARN, ' '// &
                                            'Forcing gridded data type selected was netCDF but library is not linked! '// &
                                            'Will be used data in binary format!')
#endif
                !------------------------------------------------------------------------------------------
                           
            endif
            !------------------------------------------------------------------------------------------

            !------------------------------------------------------------------------------------------
            ! Subroutine for reading sequential binary forcing data
            if (iFlagTypeData_Forcing == 1) then

                !------------------------------------------------------------------------------------------
                ! Calling subroutine to read data in binary format
                call HMC_Data_Forcing_Gridded_Binary(iID, &
                                            sPathData_Forcing, &
                                            iRowsF, iColsF, &
                                            sTime, &
                                            a2dVarRainF, a2dVarTaF, a2dVarIncRadF, &
                                            a2dVarWindF, a2dVarRelHumF, a2dVarPaF, &
                                            a2dVarAlbedoF, a2dVarLAIF, a2dVarFCF, &
                                            a2dVarAEvtF, a2dVarPEvtF, &
                                            iScaleFactor)
                !------------------------------------------------------------------------------------------

            endif
            !------------------------------------------------------------------------------------------
                 
            !------------------------------------------------------------------------------------------
            ! Debug
            if (iDEBUG.gt.0) then
                call mprintf(.true., iINFO_Extra, ' ========= FORCING GRIDDED START =========== ')
                call mprintf(.true., iINFO_Extra, checkvar(a2dVarRainF, oHMC_Vars(iID)%a2iMask, 'RAIN START') )
                call mprintf(.true., iINFO_Extra, checkvar(a2dVarTaF, oHMC_Vars(iID)%a2iMask, 'TA START') )
                call mprintf(.true., iINFO_Extra, checkvar(a2dVarIncRadF, oHMC_Vars(iID)%a2iMask, 'INCRAD START') )
                call mprintf(.true., iINFO_Extra, checkvar(a2dVarWindF, oHMC_Vars(iID)%a2iMask, 'WIND START') )
                call mprintf(.true., iINFO_Extra, checkvar(a2dVarRelHumF, oHMC_Vars(iID)%a2iMask, 'RELHUM START') )
                call mprintf(.true., iINFO_Extra, checkvar(a2dVarPaF, oHMC_Vars(iID)%a2iMask, 'PA START') )
                call mprintf(.true., iINFO_Extra, checkvar(a2dVarAlbedoF, oHMC_Vars(iID)%a2iMask, 'ALBEDO START') )
                call mprintf(.true., iINFO_Extra, checkvar(a2dVarLAIF, oHMC_Vars(iID)%a2iMask, 'LAI START') )
                call mprintf(.true., iINFO_Extra, checkvar(a2dVarFCF, oHMC_Vars(iID)%a2iMask, 'FRACT.VEG.COVER START') )
                call mprintf(.true., iINFO_Extra, checkvar(a2dVarAEvtF, oHMC_Vars(iID)%a2iMask, 'EVT.ACT START') )
                call mprintf(.true., iINFO_Extra, checkvar(a2dVarPEvtF, oHMC_Vars(iID)%a2iMask, 'EVT.POT. START') )
                call mprintf(.true., iINFO_Extra, '')
            endif
            !------------------------------------------------------------------------------------------
            
            !------------------------------------------------------------------------------------------
            ! Grid switcher land-forcing
            call HMC_Tools_Generic_SwitchGrid(oHMC_Namelist(iID)%iFlagGrid, &
                                              iRowsL, iColsL, a2dVarRainL, &
                                              iRowsF, iColsF, a2dVarRainF, &
                                              oHMC_Vars(iID)%a2dDem, &
                                              oHMC_Vars(iID)%a2iXIndex, oHMC_Vars(iID)%a2iYIndex)
                                              
            call HMC_Tools_Generic_SwitchGrid(oHMC_Namelist(iID)%iFlagGrid, &
                                              iRowsL, iColsL, a2dVarTaL, &
                                              iRowsF, iColsF, a2dVarTaF, &
                                              oHMC_Vars(iID)%a2dDem, &
                                              oHMC_Vars(iID)%a2iXIndex, oHMC_Vars(iID)%a2iYIndex)        
                                              
            call HMC_Tools_Generic_SwitchGrid(oHMC_Namelist(iID)%iFlagGrid, &
                                              iRowsL, iColsL, a2dVarIncRadL, &
                                              iRowsF, iColsF, a2dVarIncRadF, &
                                              oHMC_Vars(iID)%a2dDem, &
                                              oHMC_Vars(iID)%a2iXIndex, oHMC_Vars(iID)%a2iYIndex)
                                                                            
            call HMC_Tools_Generic_SwitchGrid(oHMC_Namelist(iID)%iFlagGrid, &
                                              iRowsL, iColsL, a2dVarWindL, &
                                              iRowsF, iColsF, a2dVarWindF, &
                                              oHMC_Vars(iID)%a2dDem, &
                                              oHMC_Vars(iID)%a2iXIndex, oHMC_Vars(iID)%a2iYIndex)
                                              
            call HMC_Tools_Generic_SwitchGrid(oHMC_Namelist(iID)%iFlagGrid, &
                                              iRowsL, iColsL, a2dVarRelHumL, &
                                              iRowsF, iColsF, a2dVarRelHumF, &
                                              oHMC_Vars(iID)%a2dDem, &
                                              oHMC_Vars(iID)%a2iXIndex, oHMC_Vars(iID)%a2iYIndex)    
                                              
            call HMC_Tools_Generic_SwitchGrid(oHMC_Namelist(iID)%iFlagGrid, &
                                              iRowsL, iColsL, a2dVarPaL, &
                                              iRowsF, iColsF, a2dVarPaF, &
                                              oHMC_Vars(iID)%a2dDem, &
                                              oHMC_Vars(iID)%a2iXIndex, oHMC_Vars(iID)%a2iYIndex)   

            call HMC_Tools_Generic_SwitchGrid(oHMC_Namelist(iID)%iFlagGrid, &
                                              iRowsL, iColsL, a2dVarAlbedoL, &
                                              iRowsF, iColsF, a2dVarAlbedoF, &
                                              oHMC_Vars(iID)%a2dDem, &
                                              oHMC_Vars(iID)%a2iXIndex, oHMC_Vars(iID)%a2iYIndex) 
                                              
            call HMC_Tools_Generic_SwitchGrid(oHMC_Namelist(iID)%iFlagGrid, &
                                              iRowsL, iColsL, a2dVarLAIL, &
                                              iRowsF, iColsF, a2dVarLAIF, &
                                              oHMC_Vars(iID)%a2dDem, &
                                              oHMC_Vars(iID)%a2iXIndex, oHMC_Vars(iID)%a2iYIndex)  
                                              
            call HMC_Tools_Generic_SwitchGrid(oHMC_Namelist(iID)%iFlagGrid, &
                                              iRowsL, iColsL, a2dVarFCL, &
                                              iRowsF, iColsF, a2dVarFCF, &
                                              oHMC_Vars(iID)%a2dDem, &
                                              oHMC_Vars(iID)%a2iXIndex, oHMC_Vars(iID)%a2iYIndex)  
            
            call HMC_Tools_Generic_SwitchGrid(oHMC_Namelist(iID)%iFlagGrid, &
                                              iRowsL, iColsL, a2dVarAEvtL, &
                                              iRowsF, iColsF, a2dVarAEvtF, &
                                              oHMC_Vars(iID)%a2dDem, &
                                              oHMC_Vars(iID)%a2iXIndex, oHMC_Vars(iID)%a2iYIndex)
            
            call HMC_Tools_Generic_SwitchGrid(oHMC_Namelist(iID)%iFlagGrid, &
                                              iRowsL, iColsL, a2dVarPEvtL, &
                                              iRowsF, iColsF, a2dVarPEvtF, &
                                              oHMC_Vars(iID)%a2dDem, &
                                              oHMC_Vars(iID)%a2iXIndex, oHMC_Vars(iID)%a2iYIndex)  
            !------------------------------------------------------------------------------------------
                                       
            !------------------------------------------------------------------------------------------
            ! Check variable(s) limits and domain
            a2dVarRainL = check2Dvar(a2dVarRainL,               oHMC_Vars(iID)%a2iMask,     0.0,    850.0,  0.0)
            a2dVarTaL = check2Dvar(a2dVarTaL,                   oHMC_Vars(iID)%a2iMask,     -70.0,  60.0,   0.0 )    
            a2dVarIncRadL = check2Dvar(a2dVarIncRadL,           oHMC_Vars(iID)%a2iMask,     0.0,    1412.0, 0.0 ) 
            a2dVarWindL = check2Dvar(a2dVarWindL,               oHMC_Vars(iID)%a2iMask,     0.0,    80.0,   0.0 ) 
            a2dVarRelHumL = check2Dvar(a2dVarRelHumL,           oHMC_Vars(iID)%a2iMask,     0.0,    100.0,  0.0 )
            a2dVarPaL = check2Dvar(a2dVarPaL,                   oHMC_Vars(iID)%a2iMask,     50.0,   101.3,  101.3 )
            a2dVarAEvtL = check2Dvar(a2dVarAEvtL,               oHMC_Vars(iID)%a2iMask,     0.0,    50.0,   0.0 )
            a2dVarPEvtL = check2Dvar(a2dVarPEvtL,               oHMC_Vars(iID)%a2iMask,     0.0,    50.0,   0.0 )
            !------------------------------------------------------------------------------------------
  
        else
            
            !------------------------------------------------------------------------------------------
            ! Extra steps condition
            a2dVarRainL = 0.0;
            a2dVarTaL = oHMC_Vars(iID)%a2dTa; 
            a2dVarIncRadL = oHMC_Vars(iID)%a2dK;  
            a2dVarWindL = oHMC_Vars(iID)%a2dW; 
            a2dVarRelHumL = oHMC_Vars(iID)%a2dRHum
            a2dVarPaL = oHMC_Vars(iID)%a2dPres
            a2dVarAlbedoL = oHMC_Vars(iID)%a2dAlbedo; 
            a2dVarLAIL = oHMC_Vars(iID)%a2dLAI; 
            a2dVarFCL = oHMC_Vars(iID)%a2dFC; 
            
            ! Info message for extra time step(s)
            call mprintf(.true., iINFO_Extra, ' Extra time step ---> Forcing data are set constant to last real value')
            call mprintf(.true., iINFO_Extra, ' Extra time step ---> Rain data are set to 0.0')
            !------------------------------------------------------------------------------------------
        endif
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Check update and save forcing data to local variable(s) to global workspace
        ! Rain
        if ( .not. all(a2dVarRainL.eq.-9999.0) ) then
            oHMC_Vars(iID)%a2dRain = a2dVarRainL
        else
            call mprintf(.true., iWARN, ' All rain values are undefined! Check forcing data!' )
        endif

        ! Air temperature
        if ( .not. all(a2dVarTaL.eq.-9999.0) ) then
            oHMC_Vars(iID)%a2dTa = a2dVarTaL
        else
            call mprintf(.true., iWARN, ' All air temperature values are undefined! Check forcing data!' )
        endif

        ! Incoming radiation
        if ( .not. all(a2dVarIncRadL.eq.-9999.0) ) then
            oHMC_Vars(iID)%a2dK = a2dVarIncRadL
        else
            call mprintf(.true., iWARN, ' All incoming radiation values are undefined! Check forcing data!' )
        endif
        
        ! Wind
        if ( .not. all(a2dVarWindL.eq.-9999.0) ) then
            oHMC_Vars(iID)%a2dW = a2dVarWindL
        else
            call mprintf(.true., iWARN, ' All wind values are undefined! Check forcing data!' )
        endif
        
        ! Relative humidity
        if ( .not. all(a2dVarRelHumL.eq.-9999.0) ) then
            oHMC_Vars(iID)%a2dRHum = a2dVarRelHumL
        else
            call mprintf(.true., iWARN, ' All relative humidity values are undefined! Check forcing data!' )
        endif
        
        ! Actual evapotranspiration (passed in the code from ET and AET)
        if (iFlagEnergyBalance.eq.0) then
            if ( .not. all(a2dVarAEvtL.eq.-9999.0) ) then
                oHMC_Vars(iID)%a2dET = a2dVarAEvtL
            else
                call mprintf(.true., iWARN, ' All actual evapotranspiration values are undefined! Check forcing data!' )
            endif
        else
            oHMC_Vars(iID)%a2dET = 0.0
        endif
        
        ! Potential evapotranspiration
        if (iFlagEnergyBalance.eq.0) then
            if ( .not. all(a2dVarPEvtL.eq.-9999.0) ) then
                oHMC_Vars(iID)%a2dETpot = a2dVarPEvtL
            else
                call mprintf(.true., iWARN, ' All potential evapotranspiration values are undefined! Check forcing data!' )
            endif
        else
            oHMC_Vars(iID)%a2dETPot = 0.0 
        endif
        
        ! Air pressure
        if ( .not. all(a2dVarPaL.eq.101.3) ) then
            oHMC_Vars(iID)%a2dPres = a2dVarPaL
        else
            if ( all(a2dVarPaL.eq.101.3) ) then
                call mprintf(.true., iWARN, ' All air pressure values are equal to 101.3 [kPa]!'// &
                                            ' AirPressure will be initialized using altitude (DEM) information!')
                where (oHMC_Vars(iID)%a2dDem.gt.0.0)
                    a2dVarPaL = 101.3*((293-0.0065*oHMC_Vars(iID)%a2dDem)/293)**5.26 ![kPa]
                elsewhere
                    a2dVarPaL = 0.0
                endwhere
                oHMC_Vars(iID)%a2dPres = a2dVarPaL
            else
                call mprintf(.true., iWARN, ' All air pressure values are undefined! Check forcing data!' )
            endif
        endif 
        
        ! LAI          
        if ( all(a2dVarLAIL.eq.-9999.0) ) then
            if (iFlagDynVeg.eq.1) then
                if (sTimeStartLAI .eq. sTimeEndLAI) then
                    call mprintf(.true., iERROR, ' Dynamic Vegetation Phys activated -- LAI valid values not found.'// &
                        ' LAI was valid until '//sTimeEndLAI//' -- Program STOPPED!')
                else
                    if ( (all(oHMC_Vars(iID)%a2dLAI .eq. -9999.0)) .and. ( .not. oHMC_Vars(iID)%bInitLAI) ) then 
                        call HMC_Tools_Time_MonthVal(oHMC_Namelist(iID)%a1dLAIMonthly, sTimeMonth, dVarLAI)
                        a2dVarLAIL = dVarLAI 
                        
                        oHMC_Vars(iID)%bInitLAI = .true.
                        call mprintf(.true., iWarn, ' Dynamic Vegetation Phys activated -- LAI valid values not found.'// &
                        ' LAI initialized using a default monthly value. This condition will be allowed until: '//sTimeEndLAI) 
                    else
                        if ( oHMC_Vars(iID)%bInitLAI ) then
                            call mprintf(.true., iWarn, ' Dynamic Vegetation Phys activated -- LAI valid values not found.'// &
                            ' LAI was defined by the default monthly value.'// &
                            ' LAI valid until: '//sTimeEndLAI//' -- Actual TimeStep: '//sTime)
                        else
                            call mprintf(.true., iWarn, ' Dynamic Vegetation Phys activated -- LAI valid values not found.'// &
                            ' LAI was defined by the forcing datasets.'// &
                            ' LAI valid until: '//sTimeEndLAI//' -- Actual TimeStep: '//sTime)
                        endif
                    endif 
                endif
            else
                call mprintf(.true., iWARN, ' All LAI values are undefined!'// &
                                            ' LAI will be initialized using a default monthly value!')
                call HMC_Tools_Time_MonthVal(oHMC_Namelist(iID)%a1dLAIMonthly, sTimeMonth, dVarLAI)
                a2dVarLAIL = dVarLAI  
            endif
        endif 
       
        !check limits
        where (oHMC_Vars(iID)%a2dDem.gt.0.0.and.a2dVarLAIL.lt.0.0)
            a2dVarLAIL = -9999.0
        elsewhere (oHMC_Vars(iID)%a2dDem.gt.0.0.and.a2dVarLAIL.ge.0.0.and.a2dVarLAIL.le.0.01) !to avoid very small LAI values
            a2dVarLAIL = 0.01            
        elsewhere (oHMC_Vars(iID)%a2dDem.gt.0.0.and.a2dVarLAIL.gt.8.0)
            a2dVarLAIL = 8.0
        endwhere
        oHMC_Vars(iID)%a2dLAI = a2dVarLAIL

        ! Fractional Vegetation Cover
        if ( all(a2dVarFCL.eq.-9999.0) ) then
            if (iFlagDynVeg.eq.1) then
                call mprintf(.true., iWARN, ' Fractional vegetation cover valid values not found. ' // &
                                            'Use empirical formulation based on LAI!')
                !check limits
                where (oHMC_Vars(iID)%a2dDem.gt.0.0.and.a2dVarLAIL.ge.0.0)
                    !compute Fractional vegetation cover by empirical function (source Model NOAH)
                    a2dVarFCL = 1 - exp(-0.52 * a2dVarLAIL)
                elsewhere (oHMC_Vars(iID)%a2dDem.gt.0.0)
                    a2dVarFCL = 0.0 !where LAI is not valid assumed as not vegetated pixel                   
                endwhere                    

            else
                call mprintf(.true., iWARN, ' Fractional vegetation Cover set equal to one!')
                !Set FC=1 to have single source Evapotranspiration
                a2dVarFCL = 1.0 
            endif

        endif   

        ! check limits
        where (oHMC_Vars(iID)%a2dDem.gt.0.0.and.a2dVarFCL.lt.0.0)
            a2dVarFCL = 0.0
        elsewhere (oHMC_Vars(iID)%a2dDem.gt.0.0.and.a2dVarFCL.gt.1.0)
            a2dVarFCL = 1.0
        endwhere
        oHMC_Vars(iID)%a2dFC = a2dVarFCL

        ! Albedo
        if ( .not. all(a2dVarAlbedoL.eq.-9999.0) ) then
            oHMC_Vars(iID)%a2dAlbedo = a2dVarAlbedoL
        else
            call mprintf(.true., iWARN, ' All albedo values are undefined!'// &
                                        ' Albedo will be initialized using monthly mean information!')
            call HMC_Tools_Time_MonthVal(oHMC_Namelist(iID)%a1dAlbedoMonthly, sTimeMonth, dVarAlbedo)
            oHMC_Vars(iID)%a2dAlbedo = dVarAlbedo;
        endif 
        !------------------------------------------------------------------------------------------

        !------------------------------------------------------------------------------------------
        ! Debug
        if (iDEBUG.gt.0) then
            call mprintf(.true., iINFO_Extra, '')
            call mprintf(.true., iINFO_Extra, checkvar(oHMC_Vars(iID)%a2dRain, oHMC_Vars(iID)%a2iMask, 'RAIN END') )
            call mprintf(.true., iINFO_Extra, checkvar(oHMC_Vars(iID)%a2dTa, oHMC_Vars(iID)%a2iMask, 'TA END') )
            call mprintf(.true., iINFO_Extra, checkvar(oHMC_Vars(iID)%a2dK, oHMC_Vars(iID)%a2iMask, 'INCRAD END') )
            call mprintf(.true., iINFO_Extra, checkvar(oHMC_Vars(iID)%a2dW, oHMC_Vars(iID)%a2iMask, 'WIND END') )
            call mprintf(.true., iINFO_Extra, checkvar(oHMC_Vars(iID)%a2dRHum, oHMC_Vars(iID)%a2iMask, 'RELHUM END') )
            call mprintf(.true., iINFO_Extra, checkvar(oHMC_Vars(iID)%a2dPres, oHMC_Vars(iID)%a2iMask, 'PA END') )
            call mprintf(.true., iINFO_Extra, checkvar(oHMC_Vars(iID)%a2dAlbedo, oHMC_Vars(iID)%a2iMask, 'ALBEDO END') )
            call mprintf(.true., iINFO_Extra, checkvar(oHMC_Vars(iID)%a2dLAI, oHMC_Vars(iID)%a2iMask, 'LAI END') )
            call mprintf(.true., iINFO_Extra, checkvar(oHMC_Vars(iID)%a2dFC, oHMC_Vars(iID)%a2iMask, 'FRACT.VEG.COVER END') )
            call mprintf(.true., iINFO_Extra, checkvar(oHMC_Vars(iID)%a2dETpot, oHMC_Vars(iID)%a2iMask, 'EVT.POT END') )
            call mprintf(.true., iINFO_Extra, checkvar(oHMC_Vars(iID)%a2dET, oHMC_Vars(iID)%a2iMask, 'EVT.ACT END') )
            call mprintf(.true., iINFO_Extra, ' ========= FORCING GRIDDED END =========== ')
        endif
        
        ! Info end
        call mprintf(.true., iINFO_Extra, ' Data :: Forcing gridded ... OK' )
        !------------------------------------------------------------------------------------------
        
    end subroutine HMC_Data_Forcing_Gridded_Cpl
    !------------------------------------------------------------------------------------------
    
    !------------------------------------------------------------------------------------------
    ! Subroutine to read NC data forcing
#ifdef LIB_NC
    subroutine HMC_Data_Forcing_Gridded_NC(iID,  &
                                  sPathData_Forcing, &
                                  iRows, iCols, sTime, &
                                  a2dVarRain, a2dVarTa, a2dVarIncRad, &
                                  a2dVarWind, a2dVarRelHum, a2dVarPa, &
                                  a2dVarAlbedo, a2dVarLAI, a2dVarFC, &
                                  a2dVarAEvt, a2dVarPEvt)
                                  
        !------------------------------------------------------------------------------------------
        ! Variable(s)
        integer(kind = 4)                       :: iID, iDtModel, iTVeg                  
        
        character(len = 256), intent(in)        :: sPathData_Forcing
        character(len = 700)                    :: sFileNameData_Forcing, sFileNameData_Temp, sFileNameData_Forcing_Zip
        character(len = 700)                    :: sCommandUnzipFile, sCommandRemoveFile
        character(len = 256)                    :: sVarName
        integer(kind = 4), intent(in)           :: iRows, iCols

        character(len = 19), intent(in)         :: sTime
        character(len = 19)                     :: sTimeEndLAI, sTimeEndFC, sTimeStartLAI, sTimeStartFC
        character(len = 12)                     :: sTimeMonth 
        
        real(kind = 4)                          :: dVarAlbedo
        
        real(kind = 4), dimension(iCols, iRows)                 :: a2dVar
        
        real(kind = 4), dimension(iRows, iCols), intent(out)    :: a2dVarRain
        real(kind = 4), dimension(iRows, iCols), intent(out)    :: a2dVarTa
        real(kind = 4), dimension(iRows, iCols), intent(out)    :: a2dVarIncRad
        real(kind = 4), dimension(iRows, iCols), intent(out)    :: a2dVarWind      
        real(kind = 4), dimension(iRows, iCols), intent(out)    :: a2dVarRelHum       
        real(kind = 4), dimension(iRows, iCols), intent(out)    :: a2dVarPa 
        real(kind = 4), dimension(iRows, iCols), intent(out)    :: a2dVarAlbedo
        real(kind = 4), dimension(iRows, iCols), intent(out)    :: a2dVarLAI
        real(kind = 4), dimension(iRows, iCols), intent(out)    :: a2dVarFC
        real(kind = 4), dimension(iRows, iCols), intent(out)    :: a2dVarAEvt
        real(kind = 4), dimension(iRows, iCols), intent(out)    :: a2dVarPEvt

        character(len = 256):: sVarUnits, sPID
        integer(kind = 4)   :: iErr, iFlagDynVeg, iFlagEnergyBalance
        integer(kind = 4)   :: iFileID

        logical             :: bFileExist
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Initialize variable(s)
        a2dVarRain = -9999.0; a2dVarTa = -9999.0; a2dVarIncRad = -9999.0; a2dVarWind = -9999.0;
        a2dVarRelHum = -9999.0; a2dVarPa = -9999.0; a2dVarAlbedo = -9999.0; a2dVarLAI = -9999.0;
        a2dVarFC = -9999.0; a2dVarAEvt = -9999.0; a2dVarPEvt = -9999.0

        sFileNameData_Forcing = ''; sFileNameData_Temp = ''; sFileNameData_Forcing_Zip = ''; sTimeMonth = ''; 
        sTimeEndLAI = ''; sTimeEndFC = ''; sTimeStartLAI = ''; sTimeStartFC = '';
        
        ! Checking date
        write(sTimeMonth,'(A,A,A)') sTime(1:4), sTime(6:7), sTime(9:10)
        
        !write current date to new variable for checking LAI validity
        write(sTimeStartLAI,'(A,A,A)') sTime
        write(sTimeStartFC,'(A,A,A)') sTime
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Get global information
        sCommandUnzipFile = oHMC_Namelist(iID)%sCommandUnzipFile
        sCommandRemoveFile = oHMC_Namelist(iID)%sCommandRemoveFile
        
        ! Get global parameters
        sTimeEndLAI = oHMC_Vars(iID)%sTimeMaxLAI
        sTimeEndFC = oHMC_Vars(iID)%sTimeMaxFC
        iFlagDynVeg = oHMC_Namelist(iID)%iFlagDynVeg
        iFlagEnergyBalance = oHMC_Namelist(iID)%iFlagEnergyBalance
        iDtModel = oHMC_Namelist(iID)%iDtModel
        iTVeg = oHMC_Namelist(iID)%iTVeg
        
        ! Info start
        call mprintf(.true., iINFO_Extra, ' Data :: Forcing gridded :: NetCDF ... ' )
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Get unique process ID
        sPID = adjustl(getProcessID())
              
        ! Filename forcing (example: hmc.dynamicdata.201404300000.nc.gz)
        sFileNameData_Forcing = trim(sPathData_Forcing)//"hmc.forcing-grid."// &
            sTime(1:4)//sTime(6:7)//sTime(9:10)// & 
            sTime(12:13)//sTime(15:16)// &
            ".nc"       
        
        ! Create Filename with unique PID number to avoid simultaneously access to the same Forcing file       
        sFileNameData_Temp = trim(sPathData_Forcing)//"hmc.forcing-grid."// &
            sTime(1:4)//sTime(6:7)//sTime(9:10)// & 
            sTime(12:13)//sTime(15:16)//'_'//trim(sPID)// &
            ".nc"  
        
        ! Info netCDF filename
        call mprintf(.true., iINFO_Verbose, ' Get filename (forcing gridded): '//trim(sFileNameData_Forcing)//' ... ' )
        !------------------------------------------------------------------------------------------

        !------------------------------------------------------------------------------------------
        ! Checking file input availability
        sFileNameData_Forcing_Zip = trim(sFileNameData_Forcing)//'.gz'
        inquire (file = trim(sFileNameData_Forcing)//'.gz', exist = bFileExist)
        if ( .not. bFileExist ) then
            !------------------------------------------------------------------------------------------
            ! Warning message
            call mprintf(.true., iWARN, ' No compressed forcing netCDF data found: '//trim(sFileNameData_Forcing_Zip) )
            ! Info netCDF filename
            call mprintf(.true., iINFO_Verbose, &
                         ' Get filename (forcing gridded): '//trim(sFileNameData_Forcing)//' ... FAILED' )
            ! Info end
            call mprintf(.true., iINFO_Extra, ' Data :: Forcing gridded :: NetCDF ... SKIPPED!' )
            !------------------------------------------------------------------------------------------
        else
            
            !------------------------------------------------------------------------------------------
            ! Unzip file
            call HMC_Tools_Generic_UnzipFile(oHMC_Namelist(iID)%sCommandUnzipFile, &
                                             sFileNameData_Forcing_Zip, &
                                             sFileNameData_Temp, .true.)
            !-----------------------------------------------------------------------------------------
        
            !------------------------------------------------------------------------------------------
            ! Open netCDF file
            iErr = nf90_open(trim(sFileNameData_Temp), NF90_NOWRITE, iFileID)
            if (iErr /= 0) then
                call mprintf(.true., iWARN, ' Problem opening uncompressed netCDF file: '// &
                             trim(sFileNameData_Forcing)//' --> Undefined forcing data values' )
                call mprintf(.true., iINFO_Verbose, &
                             ' Get filename (forcing gridded): '//trim(sFileNameData_Forcing)//' ... FAILED' )
            else
                
                !------------------------------------------------------------------------------------------
                ! RAIN
#ifdef LIB_DYNARRAY
                call HMC_Tools_IO_CheckVar_NC('Precipitation;Rain', iFileID, sVarName)
#else 
                sVarName = 'Rain'
#endif
                call HMC_Tools_IO_Get2d_NC((sVarName), iFileID, a2dVar, sVarUnits, iCols, iRows, .false., iErr)
                if(iErr /= 0) then
                    call mprintf(.true., iWARN, ' Get forcing gridded data FAILED! Check forcing data for '//trim(sVarName)//'!')
                    a2dVarRain = -9999.0;
                else
                    a2dVarRain = transpose(a2dVar)
                endif
                !------------------------------------------------------------------------------------------
                
                !------------------------------------------------------------------------------------------
                ! AIR TEMPERATURE
#ifdef LIB_DYNARRAY
                call HMC_Tools_IO_CheckVar_NC('AirTemperature;Air_Temperature', iFileID, sVarName)
#else
                sVarName = 'AirTemperature'
#endif
                call HMC_Tools_IO_Get2d_NC((sVarName), iFileID, a2dVar, sVarUnits, iCols, iRows, .false., iErr)
                if(iErr /= 0) then
                    call mprintf(.true., iWARN, ' Get forcing gridded data FAILED! Check forcing data for '//trim(sVarName)//'!')
                    a2dVarTa = -9999.0;
                else
                    a2dVarTa = transpose(a2dVar)
                endif
                !------------------------------------------------------------------------------------------
                
                
                
                !------------------------------------------------------------------------------------------
                ! INCOMING RADIATION
#ifdef LIB_DYNARRAY
                call HMC_Tools_IO_CheckVar_NC('Incoming_Radiation;IncomingRadiation;IncRadiation', iFileID, sVarName)
#else
                sVarName = 'IncRadiation'
#endif
                call HMC_Tools_IO_Get2d_NC((sVarName), iFileID, a2dVar, sVarUnits, iCols, iRows, .false., iErr)
                if(iErr /= 0) then
                    call mprintf(.true., iWARN, ' Get forcing gridded data FAILED! Check forcing data for '//trim(sVarName)//'!')
                    a2dVarIncRad = -9999.0;
                else
                    a2dVarIncRad = transpose(a2dVar)
                endif
                !------------------------------------------------------------------------------------------

                !------------------------------------------------------------------------------------------
                ! RELATIVE HUMIDITY
#ifdef LIB_DYNARRAY
                call HMC_Tools_IO_CheckVar_NC('Relative_Humidity;RelativeHumidity;RelHumidity', iFileID, sVarName)
#else
                sVarName = 'RelHumidity'
#endif
                call HMC_Tools_IO_Get2d_NC((sVarName), iFileID, a2dVar, sVarUnits, iCols, iRows, .false., iErr)
                if(iErr /= 0) then
                    call mprintf(.true., iWARN, ' Get forcing gridded data FAILED! Check forcing data for '//trim(sVarName)//'!')
                    a2dVarRelHum = -9999.0;
                else
                    a2dVarRelHum = transpose(a2dVar)
                endif
                !------------------------------------------------------------------------------------------
                
                !------------------------------------------------------------------------------------------
                ! WIND 
#ifdef LIB_DYNARRAY
                call HMC_Tools_IO_CheckVar_NC('WindSpeed;Wind', iFileID, sVarName)
#else
                sVarName = 'Wind'
#endif
                call HMC_Tools_IO_Get2d_NC((sVarName), iFileID, a2dVar, sVarUnits, iCols, iRows, .false., iErr)
                if(iErr /= 0) then
                    call mprintf(.true., iWARN, ' Get forcing gridded data FAILED! Check forcing data for '//trim(sVarName)//'!')
                    a2dVarWind = -9999.0;
                else
                    a2dVarWind = transpose(a2dVar)
                endif
                !------------------------------------------------------------------------------------------
                
                !------------------------------------------------------------------------------------------
                ! AIR PRESSURE
                sVarName = 'AirPressure'
                call HMC_Tools_IO_Get2d_NC((sVarName), iFileID, a2dVar, sVarUnits, iCols, iRows, .false., iErr)
                if(iErr /= 0) then
                    call mprintf(.true., iWARN, ' Get forcing gridded data FAILED! Check forcing data for '// &
                                                trim(sVarName)//'! Variable initializes in default mode using Altitude values!')
                    a2dVarPa = -9999.0;
                    !a2dVarPa = 101.3*((293-0.0065*oHMC_Vars(iID)%a2dDem)/293)**5.26	![kPa]
                else
                    a2dVarPa = transpose(a2dVar)
                endif
                !------------------------------------------------------------------------------------------
                
                !------------------------------------------------------------------------------------------
                ! ALBEDO
                sVarName = 'Albedo'
                call HMC_Tools_IO_Get2d_NC((sVarName), iFileID, a2dVar, sVarUnits, iCols, iRows, .false., iErr)
                if(iErr /= 0) then
                    call mprintf(.true., iWARN, ' Get forcing gridded data FAILED! Check forcing data for '//trim(sVarName)//'!')
                    a2dVarAlbedo = -9999.0; 
                    !dVarAlbedo = -9999.0
                    !call HMC_Tools_Time_MonthVal(oHMC_Namelist(iID)%a1dAlbedoMonthly, sTimeMonth, dVarAlbedo)
                    !a2dVarAlbedo = dVarAlbedo;
                else
                    a2dVarAlbedo = transpose(a2dVar)
                endif
                !------------------------------------------------------------------------------------------
                
                !------------------------------------------------------------------------------------------
                ! ACTUAL EVAPOTRANSPIRATION 
                if (iFlagEnergyBalance.eq.0) then
                    sVarName = 'AEvt'
                    call HMC_Tools_IO_Get2d_NC((sVarName), iFileID, a2dVar, sVarUnits, iCols, iRows, .false., iErr)
                    if(iErr /= 0) then
                        call mprintf(.true., iWARN, ' Get forcing gridded data FAILED! Check forcing data for ' &
                                                    //trim(sVarName)//'!')
                        a2dVarAEvt = -9999.0; 
                    else
                        a2dVarAEvt = transpose(a2dVar)
                    endif
                endif
                !------------------------------------------------------------------------------------------
                
                !------------------------------------------------------------------------------------------
                ! POTENTIAL EVAPOTRANSPIRATION 
                if (iFlagEnergyBalance.eq.0) then
                    sVarName = 'PEvt'
                    call HMC_Tools_IO_Get2d_NC((sVarName), iFileID, a2dVar, sVarUnits, iCols, iRows, .false., iErr)
                    if(iErr /= 0) then
                        call mprintf(.true., iWARN, ' Get forcing gridded data FAILED! Check forcing data for ' &
                                                    //trim(sVarName)//'!')
                        a2dVarPEvt = -9999.0; 
                    else
                        a2dVarPEvt = transpose(a2dVar)
                    endif
                endif
                !------------------------------------------------------------------------------------------

                
                !------------------------------------------------------------------------------------------
                ! LAI
                sVarName = 'LAI'
                call HMC_Tools_IO_Get2d_NC((sVarName), iFileID, a2dVar, sVarUnits, iCols, iRows, .false., iErr)
                
                if(iErr /= 0) then
                    call mprintf(.true., iWARN, ' Get forcing gridded data FAILED! Check forcing data for '//trim(sVarName)//'!')
                    
                    if (iFlagDynVeg .eq. 1) then
                        if (sTimeEndLAI .eq. sTimeStartLAI) then

                            a2dVarLAI = -9999.0;
                            call mprintf(.true., iWARN , ' Dynamic Vegetation Phys activated --'// &
                                ' LAI was valid until: '//sTimeEndLAI//'. LAI is mandatory.')

                            ! Update time period within which LAI is still valid  - 1 model step because it is
                            ! obtained from climatology
                            ! call HMC_Tools_Time_GetNewDate(sTimeEndLAI, sTimeStartLAI, nint(real(iDtModel)))
                            ! oHMC_Vars(iID)%sTimeMaxLAI = sTimeEndLAI
                        else
                            call mprintf(.true., iWARN , ' Dynamic Vegetation Phys activated --'// &
                                ' Using previous valid LAI! LAI valid until: '//sTimeEndLAI)
                            a2dVarLAI = oHMC_Vars(iID)%a2dLAI
                        endif
                    else
                        call mprintf(.true., iWARN , ' Dynamic Vegetation Phys not activated --'// &
                            ' Using previous valid LAI!')
                            a2dVarLAI = oHMC_Vars(iID)%a2dLAI
                    endif

                else
                    a2dVarLAI = transpose(a2dVar)
                    
                    ! update time period within which LAI is still valid 
                    call HMC_Tools_Time_GetNewDate(sTimeEndLAI, sTimeStartLAI, nint(real(iTVeg * iDtModel)))
                    oHMC_Vars(iID)%sTimeMaxLAI = sTimeEndLAI
                
                endif               
                !------------------------------------------------------------------------------------------
                
                !------------------------------------------------------------------------------------------
                ! Fractional Vegetation Cover
                sVarName = 'FC'
                call HMC_Tools_IO_Get2d_NC((sVarName), iFileID, a2dVar, sVarUnits, iCols, iRows, .false., iErr)
                
                if(iErr /= 0) then
                    call mprintf(.true., iWARN, ' Get forcing gridded data FAILED! Check forcing data for '//trim(sVarName)//'!')
                    
                    if (iFlagDynVeg .eq. 1) then
                        if (sTimeEndFC .eq. sTimeStartFC) then
                            a2dVarFC = -9999.0;
                            call mprintf(.true., iWARN , ' Dynamic Vegetation Phys activated --'// &
                                ' FC was valid until: '//sTimeEndFC//'. FC is not mandatory.')
                            
                            ! update time period within which LAI is still valid - 1 model step because it is obtained from climatology
                            ! call HMC_Tools_Time_GetNewDate(sTimeEndFC, sTimeStartFC, nint(real(iDtModel)))
                            ! oHMC_Vars(iID)%sTimeMaxFC = sTimeEndFC
                        else
                            call mprintf(.true., iWARN , ' Dynamic Vegetation Phys activated -- Using previous valid FC ')
                            a2dVarFC = oHMC_Vars(iID)%a2dFC
                        endif
                    else
                        a2dVarFC = -9999.0
                    endif

                else
                    a2dVarFC = transpose(a2dVar)
                    
                    !update time period within which FC is still valid 
                    call HMC_Tools_Time_GetNewDate(sTimeEndFC, sTimeStartFC, nint(real(iTVeg * iDtModel)))
                    oHMC_Vars(iID)%sTimeMaxFC = sTimeEndFC
                
                endif               
                !------------------------------------------------------------------------------------------

                !------------------------------------------------------------------------------------------
                ! Closing netCDF file
                iErr = nf90_close(iFileID)
                ! Remove uncompressed file (to save space on disk)
                call HMC_Tools_Generic_RemoveFile(oHMC_Namelist(iID)%sCommandRemoveFile, &
                                                  sFileNameData_Temp, .false.)
                !------------------------------------------------------------------------------------------
                
                !------------------------------------------------------------------------------------------
                ! Info netCDF filename
                call mprintf(.true., iINFO_Verbose, ' Get filename (forcing gridded): '//trim(sFileNameData_Forcing)//' ... OK' )
                ! Info end
                call mprintf(.true., iINFO_Extra, ' Data :: Forcing gridded :: NetCDF ... OK' )
                !------------------------------------------------------------------------------------------
                
            endif
            !------------------------------------------------------------------------------------------
            
        endif
        !------------------------------------------------------------------------------------------
        
    end subroutine HMC_Data_Forcing_Gridded_NC
#endif
    !------------------------------------------------------------------------------------------
    
    !------------------------------------------------------------------------------------------
    ! Subroutine to read binary forcing data
    subroutine HMC_Data_Forcing_Gridded_Binary(iID, &
                                      sPathData_Forcing, &
                                      iRows, iCols, sTime, &
                                      a2dVarRain, a2dVarTa, a2dVarIncRad, &
                                      a2dVarWind, a2dVarRelHum, a2dVarPa, &
                                      a2dVarAlbedo, a2dVarLAI, a2dVarFC, &
                                      a2dVarAEvt, a2dVarPEvt, &
                                      iScaleFactor)
    
        !------------------------------------------------------------------------------------------
        ! Variable(s)
        integer(kind = 4)                   :: iID, iTVeg, iDtModel, iFlagDynVeg, iFlagEnergyBalance
                                      
        character(len = 256), intent(in)    :: sPathData_Forcing
        character(len = 700)                :: sFileNameData_Forcing, sFileNameData_Forcing_Zip, sFileNameData_Temp
        character(len = 700)                :: sCommandUnzipFile
        character(len = 256)                :: sVarName
        integer(kind = 4), intent(in)       :: iRows, iCols
        real(kind = 4)                      :: dVar
               
        character(len = 19), intent(in)     :: sTime
        character(len = 19)                 :: sTimeEndLAI, sTimeEndFC, sTimeStartLAI, sTimeStartFC
        character(len = 12)                 :: sTimeMonth
        
        real(kind = 4), dimension(iRows, iCols)                 :: a2dVar

        real(kind = 4), dimension(iRows, iCols), intent(out)    :: a2dVarRain
        real(kind = 4), dimension(iRows, iCols), intent(out)    :: a2dVarTa
        real(kind = 4), dimension(iRows, iCols), intent(out)    :: a2dVarIncRad
        real(kind = 4), dimension(iRows, iCols), intent(out)    :: a2dVarWind      
        real(kind = 4), dimension(iRows, iCols), intent(out)    :: a2dVarRelHum       
        real(kind = 4), dimension(iRows, iCols), intent(out)    :: a2dVarPa 
        real(kind = 4), dimension(iRows, iCols), intent(out)    :: a2dVarAlbedo
        real(kind = 4), dimension(iRows, iCols), intent(out)    :: a2dVarLAI
        real(kind = 4), dimension(iRows, iCols), intent(out)    :: a2dVarFC
        real(kind = 4), dimension(iRows, iCols), intent(out)    :: a2dVarAEvt
        real(kind = 4), dimension(iRows, iCols), intent(out)    :: a2dVarPEvt
       
        character(len = 256):: sVarUnits, sPID
        integer(kind = 4)   :: iErr
        integer(kind = 4)   :: iFileID, iScaleFactor
        
        logical             :: bFileExist
        !------------------------------------------------------------------------------------------
	
        !------------------------------------------------------------------------------------------
        ! Initialize variable(s)
        a2dVarRain = -9999.0; a2dVarTa = -9999.0; a2dVarIncRad = -9999.0; a2dVarWind = -9999.0
        a2dVarRelHum = -9999.0; a2dVarPa = -9999.0; a2dVarAlbedo = -9999.0; 
        a2dVarLAI = -9999.0; a2dVarFC = -9999.0; a2dVarAEvt = -9999.0; a2dVarPEvt = -9999.0

        sFileNameData_Forcing = ''; sFileNameData_Forcing_Zip = ''; sFileNameData_Temp = ''; sTimeMonth = ''
        sTimeEndLAI = ''; sTimeEndFC = ''; sTimeStartLAI = ''; sTimeStartFC = ''; sPid = ''

        ! Checking date
        write(sTimeMonth,'(A,A,A)') sTime(1:4), sTime(6:7), sTime(9:10)
        
        !write current date to new variable for checking LAI validity
        write(sTimeStartLAI,'(A,A,A)') sTime
        write(sTimeStartFC,'(A,A,A)') sTime
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Get global information
        sCommandUnzipFile = oHMC_Namelist(iID)%sCommandUnzipFile
        
        !Get global parameters
        sTimeEndLAI = oHMC_Vars(iID)%sTimeMaxLAI
        sTimeEndFC = oHMC_Vars(iID)%sTimeMaxFC
        iFlagDynVeg = oHMC_Namelist(iID)%iFlagDynVeg
        iFlagEnergyBalance = oHMC_Namelist(iID)%iFlagEnergyBalance
        iDtModel = oHMC_Namelist(iID)%iDtModel
        iTVeg = oHMC_Namelist(iID)%iTVeg
        
        ! Info start
        call mprintf(.true., iINFO_Extra, ' Data :: Forcing gridded :: Binary ... ' )
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Info binary file(s) time step
        call mprintf(.true., iINFO_Verbose, ' Get (forcing gridded) at time '//trim(sTime)//' ... ')
        
        ! Get unique process ID
        sPID = adjustl(getProcessID())
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Rain  (example: Rain_201405010000.bin.gz)
        sFileNameData_Forcing = trim(sPathData_Forcing)//"Rain_"// &
            sTime(1:4)//sTime(6:7)//sTime(9:10)// & 
            sTime(12:13)//sTime(15:16)// &
            ".bin"  
        sFileNameData_Temp = trim(sPathData_Forcing)//"Rain_"// &
            sTime(1:4)//sTime(6:7)//sTime(9:10)// & 
            sTime(12:13)//sTime(15:16)//'_'//trim(sPID)// &
            ".bin"  
            
        call mprintf(.true., iINFO_Extra, ' Get filename (forcing gridded): '//trim(sFileNameData_Forcing) )

        ! Checking file input availability
        sFileNameData_Forcing_Zip = trim(sFileNameData_Forcing)//'.gz'
        inquire (file = sFileNameData_Forcing_Zip, exist = bFileExist)
        if ( .not. bFileExist ) then
            call mprintf(.true., iWARN, ' Problem opening uncompressed binary file: '// &
                         trim(sFileNameData_Forcing_Zip)//' --> Undefined forcing data values!' )
            a2dVar = -9999.0
        else
            ! Unzip file
            call HMC_Tools_Generic_UnzipFile(oHMC_Namelist(iID)%sCommandUnzipFile, &
                                             sFileNameData_Forcing_Zip, &
                                             sFileNameData_Temp, .true.)
            ! Read binary data
            call HMC_Tools_IO_Get2d_Binary_INT(sFileNameData_Temp, a2dVar, iRows, iCols, iScaleFactor, .true., iErr) 
            ! Remove uncompressed file (to save space on disk)
            call HMC_Tools_Generic_RemoveFile(oHMC_Namelist(iID)%sCommandRemoveFile, &
                                              sFileNameData_Temp, .false.)
            
        endif
        a2dVarRain = a2dVar
        !------------------------------------------------------------------------------------------

        !------------------------------------------------------------------------------------------
        ! Temperature  (example: Temperature_201405010000.bin.gz)
        sFileNameData_Forcing = trim(sPathData_Forcing)//"Temperature_"// &
            sTime(1:4)//sTime(6:7)//sTime(9:10)// & 
            sTime(12:13)//sTime(15:16)// &
            ".bin"
        sFileNameData_Temp = trim(sPathData_Forcing)//"Temperature_"// &
            sTime(1:4)//sTime(6:7)//sTime(9:10)// & 
            sTime(12:13)//sTime(15:16)//'_'//trim(sPID)// &
            ".bin"
        call mprintf(.true., iINFO_Extra, ' Get filename (forcing gridded): '//trim(sFileNameData_Forcing) )

        ! Checking file input availability
        sFileNameData_Forcing_Zip = trim(sFileNameData_Forcing)//'.gz'
        inquire (file = sFileNameData_Forcing_Zip, exist = bFileExist)
        if ( .not. bFileExist ) then
            call mprintf(.true., iWARN, ' Problem opening uncompressed binary file: '// &
                         trim(sFileNameData_Forcing_Zip)//' --> Undefined forcing data values!' )
            a2dVar = -9999.0
        else
            ! Unzip file
            call HMC_Tools_Generic_UnzipFile(oHMC_Namelist(iID)%sCommandUnzipFile, &
                                             sFileNameData_Forcing_Zip, &
                                             sFileNameData_Temp, .true.)
            ! Read binary data
            call HMC_Tools_IO_Get2d_Binary_INT(sFileNameData_Temp, a2dVar, iRows, iCols, iScaleFactor, .true., iErr) 
            ! Remove uncompressed file (to save space on disk)
            call HMC_Tools_Generic_RemoveFile(oHMC_Namelist(iID)%sCommandRemoveFile, &
                                              sFileNameData_Temp, .false.)
        endif
        a2dVarTa = a2dVar
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Radiation  (example: Radiation_201405010000.bin.gz)
        sFileNameData_Forcing = trim(sPathData_Forcing)//"Radiation_"// &
            sTime(1:4)//sTime(6:7)//sTime(9:10)// & 
            sTime(12:13)//sTime(15:16)// &
            ".bin"
        sFileNameData_Temp = trim(sPathData_Forcing)//"Radiation_"// &
            sTime(1:4)//sTime(6:7)//sTime(9:10)// & 
            sTime(12:13)//sTime(15:16)//'_'//trim(sPID)// &
            ".bin"  
        call mprintf(.true., iINFO_Extra, ' Get filename (forcing gridded): '//trim(sFileNameData_Forcing) )

        ! Checking file input availability
        sFileNameData_Forcing_Zip = trim(sFileNameData_Forcing)//'.gz'
        inquire (file = sFileNameData_Forcing_Zip, exist = bFileExist)
        if ( .not. bFileExist ) then
            call mprintf(.true., iWARN, ' Problem opening uncompressed binary file: '// &
                         trim(sFileNameData_Forcing_Zip)//' --> Undefined forcing data values!' )
            a2dVar = -9999.0
        else
            ! Unzip file
            call HMC_Tools_Generic_UnzipFile(oHMC_Namelist(iID)%sCommandUnzipFile, &
                                             sFileNameData_Forcing_Zip, &
                                             sFileNameData_Temp, .true.)
            ! Read binary data
            call HMC_Tools_IO_Get2d_Binary_INT(sFileNameData_Temp, a2dVar, iRows, iCols, iScaleFactor, .true., iErr) 
            ! Remove uncompressed file (to save space on disk)
            call HMC_Tools_Generic_RemoveFile(oHMC_Namelist(iID)%sCommandRemoveFile, &
                                              sFileNameData_Temp, .false.)
        endif
        a2dVarIncRad = a2dVar
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Wind  (example: Wind_201405010000.bin.gz)
        sFileNameData_Forcing = trim(sPathData_Forcing)//"Wind_"// &
            sTime(1:4)//sTime(6:7)//sTime(9:10)// & 
            sTime(12:13)//sTime(15:16)// &
            ".bin"
        sFileNameData_Temp = trim(sPathData_Forcing)//"Wind_"// &
            sTime(1:4)//sTime(6:7)//sTime(9:10)// & 
            sTime(12:13)//sTime(15:16)//'_'//trim(sPID)// &
            ".bin" 
        call mprintf(.true., iINFO_Extra, ' Get filename (forcing gridded): '//trim(sFileNameData_Forcing) )

        ! Checking file input availability
        sFileNameData_Forcing_Zip = trim(sFileNameData_Forcing)//'.gz'
        inquire (file = sFileNameData_Forcing_Zip, exist = bFileExist)
        if ( .not. bFileExist ) then
            call mprintf(.true., iWARN, ' Problem opening uncompressed binary file: '// &
                         trim(sFileNameData_Forcing_Zip)//' --> Undefined forcing data values!' )
            a2dVar = -9999.0
        else
            ! Unzip file
            call HMC_Tools_Generic_UnzipFile(oHMC_Namelist(iID)%sCommandUnzipFile, & 
                                             sFileNameData_Forcing_Zip, &
                                             sFileNameData_Temp, .true.)
            ! Read binary data
            call HMC_Tools_IO_Get2d_Binary_INT(sFileNameData_Temp, a2dVar, iRows, iCols, iScaleFactor, .true., iErr) 
            ! Remove uncompressed file (to save space on disk)
            call HMC_Tools_Generic_RemoveFile(oHMC_Namelist(iID)%sCommandRemoveFile, &
                                              sFileNameData_Temp, .false.)
        endif
        a2dVarWind = a2dVar
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! RelHum  (example: RelUmid_201405010000.bin.gz)
        sFileNameData_Forcing = trim(sPathData_Forcing)//"RelUmid_"// &
            sTime(1:4)//sTime(6:7)//sTime(9:10)// & 
            sTime(12:13)//sTime(15:16)// &
            ".bin"
        sFileNameData_Temp = trim(sPathData_Forcing)//"RelUmid_"// &
            sTime(1:4)//sTime(6:7)//sTime(9:10)// & 
            sTime(12:13)//sTime(15:16)//'_'//trim(sPID)// &
            ".bin" 
        call mprintf(.true., iINFO_Extra, ' Get filename (forcing gridded): '//trim(sFileNameData_Forcing) )

        ! Checking file input availability
        sFileNameData_Forcing_Zip = trim(sFileNameData_Forcing)//'.gz'
        inquire (file = sFileNameData_Forcing_Zip, exist = bFileExist)
        if ( .not. bFileExist ) then
            call mprintf(.true., iWARN, ' Problem opening uncompressed binary file: '// &
                         trim(sFileNameData_Forcing_Zip)//' --> Undefined forcing data values!' )
            a2dVar = -9999.0
        else
            ! Unzip file
            call HMC_Tools_Generic_UnzipFile(oHMC_Namelist(iID)%sCommandUnzipFile, & 
                                             sFileNameData_Forcing_Zip, & 
                                             sFileNameData_Temp, .true.)
            ! Read binary data
            call HMC_Tools_IO_Get2d_Binary_INT(sFileNameData_Temp, a2dVar, iRows, iCols, iScaleFactor, .true., iErr) 
            ! Remove uncompressed file (to save space on disk)
            call HMC_Tools_Generic_RemoveFile(oHMC_Namelist(iID)%sCommandRemoveFile, &
                                              sFileNameData_Temp, .false.)
        endif
        a2dVarRelHum = a2dVar
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! AirPressure (example: Pressure_201405010000.bin.gz)
        sFileNameData_Forcing = trim(sPathData_Forcing)//"Pressure_"// &
            sTime(1:4)//sTime(6:7)//sTime(9:10)// & 
            sTime(12:13)//sTime(15:16)// &
            ".bin"
        sFileNameData_Temp = trim(sPathData_Forcing)//"Pressure_"// &
            sTime(1:4)//sTime(6:7)//sTime(9:10)// & 
            sTime(12:13)//sTime(15:16)//'_'//trim(sPID)// &
            ".bin" 
        call mprintf(.true., iINFO_Extra, ' Get filename (forcing gridded): '//trim(sFileNameData_Forcing) )

        ! Checking file input availability
        sFileNameData_Forcing_Zip = trim(sFileNameData_Forcing)//'.gz'
        inquire (file = sFileNameData_Forcing_Zip, exist = bFileExist)
        if ( .not. bFileExist ) then
            call mprintf(.true., iWARN, ' Problem opening uncompressed binary file: '// &
                         trim(sFileNameData_Forcing_Zip)//' --> Undefined forcing data values!')
            a2dVar = -9999.0
            !a2dVar = 101.3*((293-0.0065*oHMC_Vars(iID)%a2dDem)/293)**5.26	![kPa]
        else
            ! Unzip file
            call HMC_Tools_Generic_UnzipFile(oHMC_Namelist(iID)%sCommandUnzipFile, & 
                                             sFileNameData_Forcing_Zip, &
                                             sFileNameData_Temp, .true.)
            ! Read binary data
            call HMC_Tools_IO_Get2d_Binary_INT(sFileNameData_Temp, a2dVar, iRows, iCols, iScaleFactor, .true., iErr) 
            ! Remove uncompressed file (to save space on disk)
            call HMC_Tools_Generic_RemoveFile(oHMC_Namelist(iID)%sCommandRemoveFile, &
                                              sFileNameData_Temp, .false.)
        endif
        a2dVarPa = a2dVar
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Actual Evapotranspiration (example: AEvt_201405010000.bin.gz)
        sFileNameData_Forcing = trim(sPathData_Forcing)//"AEvt_"// &
            sTime(1:4)//sTime(6:7)//sTime(9:10)// & 
            sTime(12:13)//sTime(15:16)// &
            ".bin"
        sFileNameData_Temp = trim(sPathData_Forcing)//"AEvt_"// &
            sTime(1:4)//sTime(6:7)//sTime(9:10)// & 
            sTime(12:13)//sTime(15:16)//'_'//trim(sPID)// &
            ".bin" 
        call mprintf(.true., iINFO_Extra, ' Get filename (forcing gridded): '//trim(sFileNameData_Forcing) )

        ! Checking file input availability
        sFileNameData_Forcing_Zip = trim(sFileNameData_Forcing)//'.gz'
        inquire (file = sFileNameData_Forcing_Zip, exist = bFileExist)
        if ( .not. bFileExist ) then
            call mprintf(.true., iWARN, ' Problem opening uncompressed binary file: '// &
                         trim(sFileNameData_Forcing_Zip)//' --> Undefined forcing data values!' )
            a2dVar = -9999.0
        else
            ! Unzip file
            call HMC_Tools_Generic_UnzipFile(oHMC_Namelist(iID)%sCommandUnzipFile, & 
                                             sFileNameData_Forcing_Zip, & 
                                             sFileNameData_Temp, .true.)
            ! Read binary data
            call HMC_Tools_IO_Get2d_Binary_INT(sFileNameData_Temp, a2dVar, iRows, iCols, iScaleFactor, .true., iErr) 
            ! Remove uncompressed file (to save space on disk)
            call HMC_Tools_Generic_RemoveFile(oHMC_Namelist(iID)%sCommandRemoveFile, &
                                              sFileNameData_Temp, .false.)
        endif
        a2dVarAEvt = a2dVar
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Potential Evapotranspiration (example: PEvt_201405010000.bin.gz)
        sFileNameData_Forcing = trim(sPathData_Forcing)//"PEvt_"// &
            sTime(1:4)//sTime(6:7)//sTime(9:10)// & 
            sTime(12:13)//sTime(15:16)// &
            ".bin"
        sFileNameData_Temp = trim(sPathData_Forcing)//"PEvt_"// &
            sTime(1:4)//sTime(6:7)//sTime(9:10)// & 
            sTime(12:13)//sTime(15:16)//'_'//trim(sPID)// &
            ".bin" 
        call mprintf(.true., iINFO_Extra, ' Get filename (forcing gridded): '//trim(sFileNameData_Forcing) )

        ! Checking file input availability
        sFileNameData_Forcing_Zip = trim(sFileNameData_Forcing)//'.gz'
        inquire (file = sFileNameData_Forcing_Zip, exist = bFileExist)
        if ( .not. bFileExist ) then
            call mprintf(.true., iWARN, ' Problem opening uncompressed binary file: '// &
                         trim(sFileNameData_Forcing_Zip)//' --> Undefined forcing data values!' )
            a2dVar = -9999.0
        else
            ! Unzip file
            call HMC_Tools_Generic_UnzipFile(oHMC_Namelist(iID)%sCommandUnzipFile, & 
                                             sFileNameData_Forcing_Zip, & 
                                             sFileNameData_Temp, .true.)
            ! Read binary data
            call HMC_Tools_IO_Get2d_Binary_INT(sFileNameData_Temp, a2dVar, iRows, iCols, iScaleFactor, .true., iErr) 
            ! Remove uncompressed file (to save space on disk)
            call HMC_Tools_Generic_RemoveFile(oHMC_Namelist(iID)%sCommandRemoveFile, &
                                              sFileNameData_Temp, .false.)
        endif
        a2dVarPEvt = a2dVar
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Albedo (example: albedo_201405010000.bin.gz)
        sFileNameData_Forcing = trim(sPathData_Forcing)//"Albedo_"// &
            sTime(1:4)//sTime(6:7)//sTime(9:10)// & 
            sTime(12:13)//sTime(15:16)// &
            ".bin"
        sFileNameData_Temp = trim(sPathData_Forcing)//"Albedo_"// &
            sTime(1:4)//sTime(6:7)//sTime(9:10)// & 
            sTime(12:13)//sTime(15:16)//'_'//trim(sPID)// &
            ".bin" 
        call mprintf(.true., iINFO_Extra, ' Get filename (forcing gridded): '//trim(sFileNameData_Forcing) )

        ! Checking file input availability
        sFileNameData_Forcing_Zip = trim(sFileNameData_Forcing)//'.gz'
        inquire (file = trim(sFileNameData_Forcing)//'.gz', exist = bFileExist)
        if ( .not. bFileExist ) then
            call mprintf(.true., iWARN, ' Problem opening uncompressed binary file: '// &
                         trim(sFileNameData_Forcing_Zip)//' --> Undefined forcing data values!')
            a2dVar = -9999.0; 
            !dVar = -9999.0;
            !call HMC_Tools_Time_MonthVal(oHMC_Namelist(iID)%a1dAlbedoMonthly, sTimeMonth, dVar)
            !a2dVar = dVar;
        else
            ! Unzip file
            call HMC_Tools_Generic_UnzipFile(oHMC_Namelist(iID)%sCommandUnzipFile, &
                                             sFileNameData_Forcing_Zip, &
                                             sFileNameData_Temp, .true.)
            ! Read binary data
            call HMC_Tools_IO_Get2d_Binary_INT(sFileNameData_Temp, a2dVar, iRows, iCols, iScaleFactor, .true., iErr) 
            ! Remove uncompressed file (to save space on disk)
            call HMC_Tools_Generic_RemoveFile(oHMC_Namelist(iID)%sCommandRemoveFile, &
                                              sFileNameData_Temp, .false.)
        endif
        a2dVarAlbedo = a2dVar
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! LAI (example: LAI_201405010000.bin.gz)
        sFileNameData_Forcing = trim(sPathData_Forcing)//"LAI_"// &
            sTime(1:4)//sTime(6:7)//sTime(9:10)// & 
            sTime(12:13)//sTime(15:16)// &
            ".bin"
        sFileNameData_Temp = trim(sPathData_Forcing)//"LAI_"// &
            sTime(1:4)//sTime(6:7)//sTime(9:10)// & 
            sTime(12:13)//sTime(15:16)//'_'//trim(sPID)// &
            ".bin" 
        call mprintf(.true., iINFO_Extra, ' Get filename (forcing gridded): '//trim(sFileNameData_Forcing) )
  
        ! Checking file input availability
        sFileNameData_Forcing_Zip = trim(sFileNameData_Forcing)//'.gz'
        inquire (file = sFileNameData_Forcing_Zip, exist = bFileExist)
        if ( .not. bFileExist ) then

            call mprintf(.true., iWARN, ' Problem opening uncompressed binary file: '// &
                         trim(sFileNameData_Forcing_Zip))
            
            if (iFlagDynVeg .eq. 1) then             
                if (sTimeEndLAI.eq.sTimeStartLAI) then
  
                    a2dVar = -9999.0;
                    call mprintf(.true., iWARN , ' Dynamic Vegetation Phys activated --'// &
                                ' LAI was valid until: '//sTimeEndLAI)

                    ! update time period within which LAI is still valid  - 1 model step becasue it is obtained from climatology
                    ! call HMC_Tools_Time_GetNewDate(sTimeEndLAI, sTimeStartLAI, nint(real(iDtModel)))
                    ! oHMC_Vars(iID)%sTimeMaxLAI = sTimeEndLAI       
                else
                    call mprintf(.true., iWARN , ' Dynamic Vegetation Phys activated --'// &
                        ' Using previous valid LAI! LAI valid until: '//sTimeEndLAI)
                    a2dVar = oHMC_Vars(iID)%a2dLAI
                endif
            else
                call mprintf(.true., iWARN , ' Dynamic Vegetation Phys not activated --'// &
                        ' Using previous valid LAI!')
                a2dVar = oHMC_Vars(iID)%a2dLAI
            endif

        else
            ! Unzip file
            call HMC_Tools_Generic_UnzipFile(oHMC_Namelist(iID)%sCommandUnzipFile, &
                                             sFileNameData_Forcing_Zip, &
                                             sFileNameData_Temp, .true.)
            ! Read binary data
            call HMC_Tools_IO_Get2d_Binary_INT(sFileNameData_Temp, a2dVar, iRows, iCols, iScaleFactor, .true., iErr) 
            ! Remove uncompressed file (to save space on disk)
            call HMC_Tools_Generic_RemoveFile(oHMC_Namelist(iID)%sCommandRemoveFile, &
                                              sFileNameData_Temp, .false.)
                                              
            !update time period in which LAI is still valid 
            call HMC_Tools_Time_GetNewDate(sTimeEndLAI, sTimeStartLAI, nint(real(iTVeg * iDtModel)))
            oHMC_Vars(iID)%sTimeMaxLAI = sTimeEndLAI
        endif
        a2dVarLAI = a2dVar
        !------------------------------------------------------------------------------------------

        !------------------------------------------------------------------------------------------
        ! Fractional Vegetation Cover (example: FC_201405010000.bin.gz)
        sFileNameData_Forcing = trim(sPathData_Forcing)//"FC_"// &
            sTime(1:4)//sTime(6:7)//sTime(9:10)// & 
            sTime(12:13)//sTime(15:16)// &
            ".bin"
        sFileNameData_Temp = trim(sPathData_Forcing)//"FC_"// &
            sTime(1:4)//sTime(6:7)//sTime(9:10)// & 
            sTime(12:13)//sTime(15:16)//'_'//trim(sPID)// &
            ".bin" 
        call mprintf(.true., iINFO_Extra, ' Get filename (forcing gridded): '//trim(sFileNameData_Forcing) )
  
        ! Checking file input availability
        sFileNameData_Forcing_Zip = trim(sFileNameData_Forcing)//'.gz'
        inquire (file = sFileNameData_Forcing_Zip, exist = bFileExist)
        if ( .not. bFileExist ) then

            call mprintf(.true., iWARN, ' Problem opening uncompressed binary file: '// &
                         trim(sFileNameData_Forcing_Zip))

            if (sTimeEndFC.eq.sTimeStartLAI) then
                call mprintf(.true., iWARN, ' --> Undefined data forcing values')
                a2dVar = -9999.0;
                !update time period within which LAI is still valid  - 1 model step becasue it is obtained from climatology
                call HMC_Tools_Time_GetNewDate(sTimeEndFC, sTimeStartFC, nint(real(iDtModel)))
                oHMC_Vars(iID)%sTimeMaxFC = sTimeEndFC
            else
                call mprintf(.true., iWARN, ' --> Using previous valid FC')
                a2dVar = oHMC_Vars(iID)%a2dFC
            endif
                      
        else
            ! Unzip file
            call HMC_Tools_Generic_UnzipFile(oHMC_Namelist(iID)%sCommandUnzipFile, &
                                             sFileNameData_Forcing_Zip, &
                                             sFileNameData_Temp, .true.)
            ! Read binary data
            call HMC_Tools_IO_Get2d_Binary_INT(sFileNameData_Temp, a2dVar, iRows, iCols, iScaleFactor, .true., iErr) 
            ! Remove uncompressed file (to save space on disk)
            call HMC_Tools_Generic_RemoveFile(oHMC_Namelist(iID)%sCommandRemoveFile, &
                                              sFileNameData_Temp, .false.)
                                              
            !update time period in which FC is still valid 
            call HMC_Tools_Time_GetNewDate(sTimeEndFC, sTimeStartFC, nint(real(iTVeg * iDtModel)))
            oHMC_Vars(iID)%sTimeMaxFC = sTimeEndFC
        endif     
        a2dVarFC = a2dVar
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Info binary file(s) time step
        call mprintf(.true., iINFO_Verbose, ' Get (forcing gridded) at time '//trim(sTime)//' ... OK')
        ! Info end
        call mprintf(.true., iINFO_Extra, ' Data :: Forcing gridded :: Binary ... OK' )
        !------------------------------------------------------------------------------------------
        
        
    end subroutine HMC_Data_Forcing_Gridded_Binary
    !------------------------------------------------------------------------------------------
    
end module HMC_Module_Data_Forcing_Gridded
!-----------------------------------------------------------------------------------------

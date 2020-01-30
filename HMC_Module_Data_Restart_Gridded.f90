!------------------------------------------------------------------------------------------    
! File:   HMC_Module_Data_Restart_Gridded.f90
! Author(s): Fabio Delogu, Francesco Silvestro, Simone Gabellani
!
! Created on May 7, 2015, 1:27 PM
!------------------------------------------------------------------------------------------

!------------------------------------------------------------------------------------------
! Module Header
module HMC_Module_Data_Restart_Gridded
    
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
                                            HMC_Tools_IO_Get2d_Binary_DBL, &
                                            HMC_Tools_IO_Get3d_Binary, &
                                            HMC_Tools_IO_Get2d_NC, &
                                            HMC_Tools_IO_Get3d_NC, &
                                            check
#else
    use HMC_Module_Tools_IO,        only:   HMC_Tools_IO_Get2d_Binary_INT, &
                                            HMC_Tools_IO_Get2d_Binary_DBL, &
                                            HMC_Tools_IO_Get3d_Binary                                        
#endif                                   
                                            
    
    use HMC_Module_Tools_Generic,   only:   HMC_Tools_Generic_ReplaceText, & 
                                            HMC_Tools_Generic_CreateFolder, &
                                            HMC_Tools_Generic_ZipFile, &
                                            HMC_Tools_Generic_UnzipFile, &
                                            HMC_Tools_Generic_RemoveFile, &
                                            transpose3Dvar, &
                                            checkdomainvar
    
    ! Implicit none for all subroutines in this module
    implicit none
    !------------------------------------------------------------------------------------------
    
contains
    
    !------------------------------------------------------------------------------------------
    ! Subroutine to manage restart gridded data
    subroutine HMC_Data_Restart_Gridded_Cpl( iID, sTime, &
                                             iRowsStart, iRowsEnd, &
                                             iColsStart, iColsEnd, &
                                             iDaySteps, iTMarkedSteps)

        !------------------------------------------------------------------------------------------
        ! Variable(s)                                    
        integer(kind = 4)           :: iID
        integer(kind = 4)           :: iFlagRestart, iFlagSnow, iFlagCType
        integer(kind = 4)           :: iRows, iCols
        integer(kind = 4)           :: iRowsStart, iColsStart, iRowsEnd, iColsEnd
        integer(kind = 4)           :: iDaySteps, iTMarkedSteps

        integer(kind = 4)           :: iFlagTypeData_Restart 
        integer(kind = 4)           :: iScaleFactor
        
        character(len = 19)         :: sTime
        character(len = 700)        :: sPathData_Restart
        character(len = 700)        :: sFileNameData_Restart, sFileNameData_Restart_Zip
        character(len = 700)        :: sCommandUnzipFile
        
        real(kind = 4), dimension(iRowsEnd - iRowsStart + 1, iColsEnd - iColsStart + 1)     :: a2dVarDEM
        
        real(kind = 4), dimension(iRowsEnd - iRowsStart + 1, iColsEnd - iColsStart + 1)     :: a2dVarVTot, a2dVarVRet, &
                                                                                               a2dVarHydro, a2dVarRouting, &
                                                                                               a2dVarFlowDeep, &
                                                                                               a2dVarWTable, a2dVarLST, &
                                                                                               a2dVarLat, a2dVarLon
                                                                                              
        real(kind = 4), dimension(iRowsEnd - iRowsStart + 1, iColsEnd - iColsStart + 1)     :: a2dVarHydroC, a2dVarHydroH, &
                                                                                               a2dVarQup
                                                                                           
        integer(kind = 4), dimension(iRowsEnd - iRowsStart + 1, iColsEnd - iColsStart + 1)              :: a2iVarAgeS
        real(kind = 4), dimension(iRowsEnd - iRowsStart + 1, iColsEnd - iColsStart + 1)                 :: a2dVarSWE, a2dVarAlbedoS
        real(kind = 4), dimension(iRowsEnd - iRowsStart + 1, iColsEnd - iColsStart + 1)                 :: a2dVarRhoS, a2dVarRhoS0
        real(kind = 4), dimension(iRowsEnd - iRowsStart + 1, iColsEnd - iColsStart + 1, iDaySteps)      :: a3dVarTaC_1Days 
        real(kind = 4), dimension(iRowsEnd - iRowsStart + 1, iColsEnd - iColsStart + 1, iDaySteps*5)    :: a3dVarTaC_5Days 
        
        real(kind = 4), dimension(iRowsEnd - iRowsStart + 1, iColsEnd - iColsStart + 1)                 :: a2dVarWSRunoff
                                                                                           
        real(kind = 4), dimension(iRowsEnd - iRowsStart + 1, iColsEnd - iColsStart + 1)                 :: a2dVarWTableUpd
        real(kind = 4), dimension(iRowsEnd - iRowsStart + 1, iColsEnd - iColsStart + 1, iTMarkedSteps)  :: a3dVarTaKMarked
        real(kind = 4), dimension(iRowsEnd - iRowsStart + 1, iColsEnd - iColsStart + 1, iDaySteps)      :: a3dVarTaK24   
        
        logical                     :: bFileExist, bCheckRestart, bCheckRestartS
        !------------------------------------------------------------------------------------------

        !------------------------------------------------------------------------------------------
        ! Initialize variable(s)
        a2dVarVTot = 0.0; a2dVarVRet = 0.0; a2dVarHydro = 0.0; a2dVarRouting = 0.0; a2dVarFlowDeep = 0.0; 
        a2dVarLST = 0.0; a2dVarWTable = 0.0; a3dVarTaKMarked = 0.0; a3dVarTaK24  = 0.0;
        a2dVarLat = 0.0; a2dVarLon = 0.0
        
        a2dVarHydroC = 0.0; a2dVarHydroH = 0.0; a2dVarQup = 0.0;
        
        a2iVarAgeS = 0; a2dVarSWE = 0.0; a2dVarAlbedoS = 0.0; a2dVarRhoS = 0.0; a2dVarRhoS0 = 0.0
        a3dVarTaC_1Days = 0.0; a3dVarTaC_5Days = 0.0;
        
        a2dVarWTableUpd = 0.0;
        a2dVarWSRunoff = 0.0;
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Defining iRows and iCols (output data)
        iRows = iRowsEnd - iRowsStart + 1
        iCols = iColsEnd - iColsStart + 1
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Get global information
        iFlagRestart = oHMC_Namelist(iID)%iFlagRestart
        iFlagSnow = oHMC_Namelist(iID)%iFlagSnow
        iFlagCType = oHMC_Namelist(iID)%iFlagCType
        sPathData_Restart = oHMC_Namelist(iID)%sPathData_Restart_Gridded
        iFlagTypeData_Restart = oHMC_Namelist(iID)%iFlagTypeData_Restart_Gridded
        iScaleFactor = oHMC_Namelist(iID)%iScaleFactor
        sCommandUnzipFile = oHMC_Namelist(iID)%sCommandUnzipFile
        
        ! Get glabal variable(s)
        a2dVarDem = oHMC_Vars(iID)%a2dDem

        ! Info start
        call mprintf(.true., iINFO_Extra, ' Data :: Restart gridded ... ' )
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Debug
        if (iDEBUG.gt.0) then
            call mprintf(.true., iINFO_Extra, ' ========= RESTART GRIDDED START =========== ')
            call mprintf(.true., iINFO_Extra, checkvar(oHMC_Vars(iID)%a2dVTot, oHMC_Vars(iID)%a2iMask, 'VTOT START') )
            call mprintf(.true., iINFO_Extra, checkvar(oHMC_Vars(iID)%a2dVRet, oHMC_Vars(iID)%a2iMask, 'VRET START') )
            call mprintf(.true., iINFO_Extra, checkvar(oHMC_Vars(iID)%a2dHydro, oHMC_Vars(iID)%a2iMask, 'HYDRO START') )
            call mprintf(.true., iINFO_Extra, checkvar(oHMC_Vars(iID)%a2dHydroC, oHMC_Vars(iID)%a2iMask, 'HYDROC START') )
            call mprintf(.true., iINFO_Extra, checkvar(oHMC_Vars(iID)%a2dHydroH, oHMC_Vars(iID)%a2iMask, 'HYDROH START') )
            call mprintf(.true., iINFO_Extra, checkvar(oHMC_Vars(iID)%a2dQup, oHMC_Vars(iID)%a2iMask, 'QUP START') )
            call mprintf(.true., iINFO_Extra, checkvar(oHMC_Vars(iID)%a2dRouting, oHMC_Vars(iID)%a2iMask, 'ROUTING START') )
            call mprintf(.true., iINFO_Extra, checkvar(oHMC_Vars(iID)%a2dWTable, oHMC_Vars(iID)%a2iMask, 'WTABLE START') )
            call mprintf(.true., iINFO_Extra, checkvar(oHMC_Vars(iID)%a2dLST, oHMC_Vars(iID)%a2iMask, 'LST START') )
            call mprintf(.true., iINFO_Extra, checkvar(oHMC_Vars(iID)%a3dTaKMarked(:,:,1), oHMC_Vars(iID)%a2iMask, 'TAMk START') )
            call mprintf(.true., iINFO_Extra, checkvar(oHMC_Vars(iID)%a3dTaK24(:,:,1), oHMC_Vars(iID)%a2iMask, 'TA24 END') )
            call mprintf(.true., iINFO_Extra, checkvar(oHMC_Vars(iID)%a2dFlowDeep, oHMC_Vars(iID)%a2iMask, 'FLOWDEEP START') )
            call mprintf(.true., iINFO_Extra, checkvar(real(oHMC_Vars(iID)%a2iAge), oHMC_Vars(iID)%a2iMask, 'AGES START') )
            call mprintf(.true., iINFO_Extra, checkvar(oHMC_Vars(iID)%a2dSWE, oHMC_Vars(iID)%a2iMask, 'SWE START') )
            call mprintf(.true., iINFO_Extra, checkvar(oHMC_Vars(iID)%a2dRhoS, oHMC_Vars(iID)%a2iMask, 'RHOS START') )
            call mprintf(.true., iINFO_Extra, checkvar(oHMC_Vars(iID)%a2dAlbedo_Snow, oHMC_Vars(iID)%a2iMask, 'ALBEDOS START') )
            call mprintf(.true., iINFO_Extra, &
                checkvar(oHMC_Vars(iID)%a3dTaC_Days1(:,:,1), oHMC_Vars(iID)%a2iMask, 'TA 1DAYS START') )
            call mprintf(.true., iINFO_Extra, &
                checkvar(oHMC_Vars(iID)%a3dTaC_Days5(:,:,1), oHMC_Vars(iID)%a2iMask, 'TA 5DAYS START') )
            call mprintf(.true., iINFO_Extra, '')
        endif
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Check restart flag value
        if (iFlagRestart == 1) then
        
            !------------------------------------------------------------------------------------------
            ! Replace general path with specific time feature(s)
            call HMC_Tools_Generic_ReplaceText(sPathData_Restart, '$yyyy', sTime(1:4))
            call HMC_Tools_Generic_ReplaceText(sPathData_Restart, '$mm', sTime(6:7))
            call HMC_Tools_Generic_ReplaceText(sPathData_Restart, '$dd', sTime(9:10))
            call HMC_Tools_Generic_ReplaceText(sPathData_Restart, '$HH', sTime(12:13))
            call HMC_Tools_Generic_ReplaceText(sPathData_Restart, '$MM', sTime(15:16))
            !------------------------------------------------------------------------------------------

            !------------------------------------------------------------------------------------------
            ! Subroutine for reading netCDF restart data 
            if (iFlagTypeData_Restart == 2) then

                !------------------------------------------------------------------------------------------
                ! Call subroutine to read data in netCDF format
#ifdef LIB_NC
                call HMC_Data_Restart_Gridded_NC(iID, &
                                        sPathData_Restart, &
                                        iRows, iCols, &
                                        iDaySteps, iTMarkedSteps, &
                                        sTime, iFlagSnow, iFlagCType, &
                                        a2dVarVTot, a2dVarVRet, &
                                        a2dVarHydro, a2dVarRouting, &
                                        a2dVarHydroC, a2dVarHydroH, a2dVarQup, &
                                        a2dVarFlowDeep, &
                                        a2dVarWTable, &
                                        a2dVarLST, a3dVarTaKMarked, a3dVarTaK24, &
                                        a2iVarAgeS, a2dVarSWE, a2dVarAlbedoS, a2dVarRhoS, &
                                        a3dVarTaC_1Days, a3dVarTaC_5Days, &
                                        a2dVarWSRunoff, &
                                        a2dVarLat, a2dVarLon, &
                                        bCheckRestart, bCheckRestartS)
#else
                ! Redefinition of forcing data flag (if netCDF library is not linked)
                iFlagTypeData_Restart = 1   
                call mprintf(.true., iWARN, ' '// &
                                            'restart gridded data type selected was netCDF but library is not linked! '// &
                                            'Will be used data in binary format!')
#endif
                !------------------------------------------------------------------------------------------
                                            
            endif
            !------------------------------------------------------------------------------------------                                    

            !------------------------------------------------------------------------------------------
            ! Subroutine for reading restart data in binary format
            if (iFlagTypeData_Restart == 1) then

                !------------------------------------------------------------------------------------------
                ! Call subroutine to read data in binary format
                call HMC_Data_Restart_Gridded_Binary(iID, &
                                        sPathData_Restart, &
                                        iRows, iCols, &
                                        iDaySteps, iTMarkedSteps, &
                                        sTime, iFlagSnow, iFlagCType, &
                                        a2dVarVTot, a2dVarVRet, &
                                        a2dVarHydro, a2dVarRouting, &
                                        a2dVarHydroC, a2dVarHydroH, a2dVarQup, &
                                        a2dVarFlowDeep, &
                                        a2dVarWTable, &
                                        a2dVarLST, a3dVarTaKMarked, a3dVarTaK24, &
                                        a2iVarAgeS, a2dVarSWE, a2dVarAlbedoS, a2dVarRhoS, &
                                        a3dVarTaC_1Days, a3dVarTaC_5Days, &
                                        a2dVarWSRunoff, &
                                        bCheckRestart, bCheckRestartS)
                !------------------------------------------------------------------------------------------

            endif
            !------------------------------------------------------------------------------------------
            
            !------------------------------------------------------------------------------------------
            ! Check restart flag on data availability
            if (bCheckRestart .eqv. .true.) then

                !------------------------------------------------------------------------------------------
                ! Variable(s) conversion (Watertable)
                where(a2dVarDem.gt.0.0)
                    a2dVarWTableUpd = a2dVarDem - a2dVarWTable/1000
                endwhere
                
                ! Check limit(s)
                where(a2dVarWTableUpd.gt.a2dVarDem)
                    a2dVarWTableUpd = a2dVarDem
                endwhere
                where(a2dVarWTableUpd.lt.oHMC_Vars(iID)%a2dWTableMax)
                    a2dVarWTableUpd = oHMC_Vars(iID)%a2dWTableMax
                endwhere
                
                ! Initialize hydro variable(s) according with channel type
                if (iFlagCType.eq.2) then
                
                    ! Check hydro c initialization
                    where (a2dVarHydroC .lt. 0.0000001)
                        a2dVarHydroC = 0.0000001
                    endwhere
                    where (a2dVarHydroC .gt. 100000.0)
                        a2dVarHydroC = 0.0000001
                    endwhere

                    ! Check hydro h initialization
                    where (a2dVarHydroH .lt. 0.0000001)
                        a2dVarHydroH = 0.0000001
                    endwhere
                    where (a2dVarHydroH .gt. 100000.0)
                        a2dVarHydroH = 0.0000001
                    endwhere
                
                else
                
                    ! Check hydro initialization
                    where (a2dVarHydro .lt. 0.0000001)
                        a2dVarHydro = 0.0000001
                    endwhere
                    where (a2dVarHydro .gt. 100000.0)
                        a2dVarHydro = 0.0000001
                    endwhere
                
                endif
                !------------------------------------------------------------------------------------------
                
                !------------------------------------------------------------------------------------------
                ! Check variable(s) domain
                a2dVarVTot = checkdomainvar(a2dVarVTot, oHMC_Vars(iID)%a2iMask,             -9999.0 )
                a2dVarVRet = checkdomainvar(a2dVarVRet, oHMC_Vars(iID)%a2iMask,             0.001 )
                
                if (iFlagCType.eq.2) then
                    a2dVarHydroC = checkdomainvar(a2dVarHydroC, oHMC_Vars(iID)%a2iMask,     0.0 )
                    a2dVarHydroH = checkdomainvar(a2dVarHydroH, oHMC_Vars(iID)%a2iMask,     0.0 )
                    a2dVarQup = checkdomainvar(a2dVarQup, oHMC_Vars(iID)%a2iMask,           0.0 ) 
                else
                    a2dVarHydro = checkdomainvar(a2dVarHydro, oHMC_Vars(iID)%a2iMask,       0.0 )
                endif
                
                a2dVarRouting = checkdomainvar(a2dVarRouting, oHMC_Vars(iID)%a2iMask,       0.0 )
                a2dVarWTableUpd = checkdomainvar(a2dVarWTableUpd, oHMC_Vars(iID)%a2iMask,   -9999.0 )
                a2dVarLST = checkdomainvar(a2dVarLST, oHMC_Vars(iID)%a2iMask,               -9999.0 )
                a2dVarFlowDeep = checkdomainvar(a2dVarFlowDeep, oHMC_Vars(iID)%a2iMask,     0.0 )
                a2dVarWSRunoff = checkdomainvar(a2dVarWSRunoff, oHMC_Vars(iID)%a2iMask,     0.0 )
                !------------------------------------------------------------------------------------------
                
                !------------------------------------------------------------------------------------------
                ! Pass variable(s) to global workspace
                oHMC_Vars(iID)%a2dVTot = a2dVarVTot;
                oHMC_Vars(iID)%a2dVRet = a2dVarVRet;
                oHMC_Vars(iID)%a2dHydro = a2dVarHydro;
                oHMC_Vars(iID)%a2dHydroC = a2dVarHydroC;
                oHMC_Vars(iID)%a2dHydroH = a2dVarHydroH;
                oHMC_Vars(iID)%a2dQup = a2dVarQup;
                oHMC_Vars(iID)%a2dRouting = a2dVarRouting;
                oHMC_Vars(iID)%a2dWTable = a2dVarWTableUpd;
                oHMC_Vars(iID)%a2dLST = a2dVarLST;
                oHMC_Vars(iID)%a2dFlowDeep = a2dVarFlowDeep;
                oHMC_Vars(iID)%a3dTaKMarked = a3dVarTaKMarked;
                oHMC_Vars(iID)%a3dTaK24 = a3dVarTaK24
                oHMC_Vars(iID)%a2dWSRunoff = a2dVarWSRunoff
  
                ! Info end
                call mprintf(.true., iINFO_Extra, ' Data :: Restart gridded ... OK' )
                !------------------------------------------------------------------------------------------
                
            else
                !------------------------------------------------------------------------------------------
                ! Exit message for not using restart data
                call mprintf(.true., iINFO_Verbose, ' Restart flag selected but data are N/A (gridded data)')
                ! Info end
                call mprintf(.true., iINFO_Extra, ' Data :: Restart gridded ... SKIPPED ' )
                !------------------------------------------------------------------------------------------
                
            endif
            !------------------------------------------------------------------------------------------  
            
            !------------------------------------------------------------------------------------------
            ! Check restart flag on snow data availability
            if (bCheckRestartS .eqv. .true.) then
                
                !------------------------------------------------------------------------------------------
                ! Definition of snow density where SWE equal to 0.0 in restart condition
                where(a2dVarSWE.eq.0.0) a2dVarRhoS = oHMC_Namelist(iID)%dRhoSnowFresh
                where(a2dVarSWE.eq.0.0) a2dVarRhoS0 = oHMC_Namelist(iID)%dRhoSnowFresh
                !------------------------------------------------------------------------------------------
                      
                !------------------------------------------------------------------------------------------
                ! Pass variable(s) to global workspace
                oHMC_Vars(iID)%a2iAge = a2iVarAgeS
                oHMC_Vars(iID)%a2dSWE = a2dVarSWE
                oHMC_Vars(iID)%a2dRhoS = a2dVarRhoS
                oHMC_Vars(iID)%a2dRhoS0 = a2dVarRhoS0
                oHMC_Vars(iID)%a2dAlbedo_Snow = a2dVarAlbedoS
                oHMC_Vars(iID)%a3dTaC_Days1 = a3dVarTaC_1Days
                oHMC_Vars(iID)%a3dTaC_Days5 = a3dVarTaC_5Days
                
                ! Info end
                call mprintf(.true., iINFO_Extra, ' Data :: Restart gridded ... OK' )
                !------------------------------------------------------------------------------------------
                
            else
                !------------------------------------------------------------------------------------------
                ! Exit message for not using restart data
                call mprintf(.true., iINFO_Verbose, ' Restart flag selected but snow data are N/A (gridded data)')
                ! Info end
                call mprintf(.true., iINFO_Extra, ' Data :: Restart gridded for snow data ... SKIPPED ' )
                !------------------------------------------------------------------------------------------
                
            endif
            !------------------------------------------------------------------------------------------  
            
        else
            !------------------------------------------------------------------------------------------
            ! Exit message for not using restart data
            call mprintf(.true., iINFO_Verbose, ' No restart run selected (gridded data)')
            ! Info end
            call mprintf(.true., iINFO_Extra, ' Data :: Restart gridded ... SKIPPED ' )
            !------------------------------------------------------------------------------------------
        endif
        !------------------------------------------------------------------------------------------
       
        !------------------------------------------------------------------------------------------
        ! Debug
        if (iDEBUG.gt.0) then
            call mprintf(.true., iINFO_Extra, '')
            call mprintf(.true., iINFO_Extra, checkvar(oHMC_Vars(iID)%a2dVTot, oHMC_Vars(iID)%a2iMask, 'VTOT END') )
            call mprintf(.true., iINFO_Extra, checkvar(oHMC_Vars(iID)%a2dVRet, oHMC_Vars(iID)%a2iMask, 'VRET END') )
            call mprintf(.true., iINFO_Extra, checkvar(oHMC_Vars(iID)%a2dHydro, oHMC_Vars(iID)%a2iMask, 'HYDRO END') )
            call mprintf(.true., iINFO_Extra, checkvar(oHMC_Vars(iID)%a2dHydroC, oHMC_Vars(iID)%a2iMask, 'HYDROC END') )
            call mprintf(.true., iINFO_Extra, checkvar(oHMC_Vars(iID)%a2dHydroH, oHMC_Vars(iID)%a2iMask, 'HYDROH END') )
            call mprintf(.true., iINFO_Extra, checkvar(oHMC_Vars(iID)%a2dQup, oHMC_Vars(iID)%a2iMask, 'QUP END') )
            call mprintf(.true., iINFO_Extra, checkvar(oHMC_Vars(iID)%a2dRouting, oHMC_Vars(iID)%a2iMask, 'ROUTING END') )
            call mprintf(.true., iINFO_Extra, checkvar(oHMC_Vars(iID)%a2dWTable, oHMC_Vars(iID)%a2iMask, 'WTABLE END') )
            call mprintf(.true., iINFO_Extra, checkvar(oHMC_Vars(iID)%a2dLST, oHMC_Vars(iID)%a2iMask, 'LST END') )
            call mprintf(.true., iINFO_Extra, checkvar(oHMC_Vars(iID)%a3dTaKMarked(:,:,1), oHMC_Vars(iID)%a2iMask, 'TAMk END') )
            call mprintf(.true., iINFO_Extra, checkvar(oHMC_Vars(iID)%a3dTaK24(:,:,1), oHMC_Vars(iID)%a2iMask, 'TA24 END') )
            call mprintf(.true., iINFO_Extra, checkvar(oHMC_Vars(iID)%a2dFlowDeep, oHMC_Vars(iID)%a2iMask, 'FLOWDEEP END') )
            call mprintf(.true., iINFO_Extra, checkvar(real(oHMC_Vars(iID)%a2iAge), oHMC_Vars(iID)%a2iMask, 'AGES END') )
            call mprintf(.true., iINFO_Extra, checkvar(oHMC_Vars(iID)%a2dSWE, oHMC_Vars(iID)%a2iMask, 'SWE END') )
            call mprintf(.true., iINFO_Extra, checkvar(oHMC_Vars(iID)%a2dRhoS, oHMC_Vars(iID)%a2iMask, 'RHOS END') )
            call mprintf(.true., iINFO_Extra, checkvar(oHMC_Vars(iID)%a2dAlbedo_Snow, oHMC_Vars(iID)%a2iMask, 'ALBEDOS END') )
            call mprintf(.true., iINFO_Extra, &
                checkvar(oHMC_Vars(iID)%a3dTaC_Days1(:,:,1), oHMC_Vars(iID)%a2iMask, 'TA 1DAYS END') )
            call mprintf(.true., iINFO_Extra, &
                checkvar(oHMC_Vars(iID)%a3dTaC_Days5(:,:,1), oHMC_Vars(iID)%a2iMask, 'TA 5DAYS END') )
            call mprintf(.true., iINFO_Extra, '')
            call mprintf(.true., iINFO_Extra, ' ========= RESTART GRIDDED END =========== ')
        endif
        !------------------------------------------------------------------------------------------
        
    end subroutine HMC_Data_Restart_Gridded_Cpl
    !------------------------------------------------------------------------------------------
    
    !------------------------------------------------------------------------------------------
    ! Subroutine to read netCDF data restart
#ifdef LIB_NC
    subroutine HMC_Data_Restart_Gridded_NC(iID, &
                                           sPathData_Restart, &
                                           iRows, iCols, &
                                           iDaySteps, iTMarkedSteps, &
                                           sTime, iFlagSnow, iFlagCType, &
                                           a2dVarVTot, a2dVarVRet, &
                                           a2dVarHydro, a2dVarRouting, &
                                           a2dVarHydroC, a2dVarHydroH, a2dVarQup, &
                                           a2dVarFlowDeep, &
                                           a2dVarWTable, &
                                           a2dVarLST, a3dVarTaKMarked, a3dVarTaK24, &
                                           a2iVarAgeS, a2dVarSWE, a2dVarAlbedoS, a2dVarRhoS, &
                                           a3dVarTaC_1Days, a3dVarTaC_5Days, &
                                           a2dVarWSRunoff, &
                                           a2dVarLat, a2dVarLon, &
                                           bCheckRestart, bCheckRestartS)
                                      
        !------------------------------------------------------------------------------------------
        ! Variable(s)
        integer(kind = 4)                        :: iID                  
                                  
        character(len = 256), intent(in)        :: sPathData_Restart
        character(len = 700)                     :: sFileNameData_Restart, sFileNameData_Restart_Zip
        character(len = 700)                     :: sCommandUnzipFile
        character(len = 256)                     :: sVarName
        integer(kind = 4), intent(in)           :: iRows, iCols
        integer(kind = 4), intent(in)           :: iDaySteps, iTMarkedSteps
        integer(kind = 4), intent(in)           :: iFlagSnow, iFlagCType

        character(len = 19), intent(in)         :: sTime

        real(kind = 4), dimension(iCols, iRows)                                :: a2dVar
        real(kind = 4), dimension(iCols, iRows, iTMarkedSteps)                 :: a3dVar1
        real(kind = 4), dimension(iCols, iRows, iDaySteps)                     :: a3dVar2
        real(kind = 4), dimension(iCols, iRows, iDaySteps)                     :: a3dVar3
        real(kind = 4), dimension(iCols, iRows, iDaySteps*5)                   :: a3dVar4
        
        real(kind = 4), dimension(iRows, iCols),                intent(out)    :: a2dVarVTot
        real(kind = 4), dimension(iRows, iCols),                intent(out)    :: a2dVarVRet
        real(kind = 4), dimension(iRows, iCols),                intent(out)    :: a2dVarHydro
        real(kind = 4), dimension(iRows, iCols),                intent(out)    :: a2dVarRouting    
        real(kind = 4), dimension(iRows, iCols),                intent(out)    :: a2dVarHydroC
        real(kind = 4), dimension(iRows, iCols),                intent(out)    :: a2dVarHydroH   
        real(kind = 4), dimension(iRows, iCols),                intent(out)    :: a2dVarQup
        real(kind = 4), dimension(iRows, iCols),                intent(out)    :: a2dVarFlowDeep       
        real(kind = 4), dimension(iRows, iCols),                intent(out)    :: a2dVarWTable
        real(kind = 4), dimension(iRows, iCols),                intent(out)    :: a2dVarLST
        real(kind = 4), dimension(iRows, iCols, iTMarkedSteps), intent(out)    :: a3dVarTaKMarked
        real(kind = 4), dimension(iRows, iCols, iDaySteps),     intent(out)    :: a3dVarTaK24
        integer(kind = 4), dimension(iRows, iCols),             intent(out)    :: a2iVarAgeS
        real(kind = 4), dimension(iRows, iCols),                intent(out)    :: a2dVarSWE
        real(kind = 4), dimension(iRows, iCols),                intent(out)    :: a2dVarAlbedoS
        real(kind = 4), dimension(iRows, iCols),                intent(out)    :: a2dVarRhoS
        real(kind = 4), dimension(iRows, iCols, iDaySteps),     intent(out)    :: a3dVarTaC_1Days
        real(kind = 4), dimension(iRows, iCols, iDaySteps*5),   intent(out)    :: a3dVarTaC_5Days
        real(kind = 4), dimension(iRows, iCols),                intent(out)    :: a2dVarWSRunoff
       
        real(kind = 4), dimension(iRows, iCols),                intent(out)    :: a2dVarLat
        real(kind = 4), dimension(iRows, iCols),                intent(out)    :: a2dVarLon
        
        character(len = 256)    :: sVarUnits
        integer(kind = 4)       :: iErr
        integer(kind = 4)       :: iFileID
        real(kind = 4)          :: dScaleFactor
        
        logical                 :: bFileExist
        
        logical                 :: bCheckRestart, bCheckRestartS
        logical                 :: bCheckVar, bCheckVarS
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Initialize variable(s)
        a2dVarVTot = -9999.0; a2dVarVRet = -9999.0; 
        a2dVarHydro = -9999.0; a2dVarRouting = -9999.0; 
        a2dVarHydroC = -9999.0; a2dVarHydroH = -9999.0; a2dVarQup = -9999.0;
        a2dVarFlowDeep = -9999.0; a2dVarWTable = -9999.0; 
        a2dVarLST = -9999.0; a3dVarTaKMarked = -9999.0; a3dVarTaK24 = -9999.0; 
        a2iVarAgeS = -9999; a2dVarSWE = -9999.0; a2dVarAlbedoS = -9999.0; a2dVarRhoS = -9999.0;
        a3dVarTaC_1Days = -9999.0; a3dVarTaC_5Days = -9999.0;
        a2dVarWSRunoff = -9999.0
        a2dVarLat = -9999.0; a2dVarLon = -9999.0;
        
        dScaleFactor = 0.0
        
        bCheckRestart = .false.; 
        
        bCheckVar = .true.; bCheckVarS = .true.
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Get global information
        sCommandUnzipFile = oHMC_Namelist(iID)%sCommandUnzipFile
        ! Info start
        call mprintf(.true., iINFO_Extra, ' Data :: Restart gridded :: NetCDF ... ' )
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Filename restart (example: hmc.state.201404300000.nc)
        sFileNameData_Restart = trim(sPathData_Restart)//"hmc.state-grid."// &
        sTime(1:4)//sTime(6:7)//sTime(9:10)// & 
        sTime(12:13)//sTime(15:16)// &
        ".nc"

        ! Info netCDF filename
        call mprintf(.true., iINFO_Basic, ' Get filename (restart gridded): '//trim(sFileNameData_Restart)//' ... ' )
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Checking file input availability
        sFileNameData_Restart_Zip = trim(sFileNameData_Restart)//'.gz'
        inquire (file = trim(sFileNameData_Restart)//'.gz', exist = bFileExist)
        if ( .not. bFileExist ) then
            !------------------------------------------------------------------------------------------
            ! Warning message
            call mprintf(.true., iWARN, ' No compressed restart netCDF data found: '//trim(sFileNameData_Restart_Zip) )
            call mprintf(.true., iINFO_Verbose, &
                         ' Get filename (restart gridded): '//trim(sFileNameData_Restart)//' ... FAILED' )
            bCheckVar = .false.
            !------------------------------------------------------------------------------------------
        else
            !------------------------------------------------------------------------------------------
            ! Unzip file
            call HMC_Tools_Generic_UnzipFile(sCommandUnzipFile, &
                                             sFileNameData_Restart_Zip, &
                                             sFileNameData_Restart, .true.)
            !------------------------------------------------------------------------------------------
        
            !------------------------------------------------------------------------------------------
            ! Opening netCDF file
            iErr = nf90_open(trim(sFileNameData_Restart), NF90_NOWRITE, iFileID)
            if (iErr /= 0) then
                
                !------------------------------------------------------------------------------------------
                ! Condition for no file restart found
                call mprintf(.true., iWARN, ' Problem opening uncompressed netCDF file: '// &
                             trim(sFileNameData_Restart)//' --> Undefined restart data values' ) 
                call mprintf(.true., iINFO_Verbose, &
                            ' Get filename (restart gridded): '//trim(sFileNameData_Restart)//' ... FAILED' )
                
                ! Flag check restart 
                bCheckVar = .false.
                !------------------------------------------------------------------------------------------
                            
            else
                
                !------------------------------------------------------------------------------------------
                ! Condition for file restart found
                ! VTot
                sVarName = 'VTot';
                call HMC_Tools_IO_Get2d_NC((sVarName), iFileID, a2dVar, sVarUnits, iCols, iRows, .true., iErr)
                if(iErr /= 0) then
                    call mprintf(.true., iWARN, ' Get restart gridded data FAILED! Check restart data for '//sVarName//'!')
                    a2dVarVTot = -9999.0;
                    bCheckVar = bCheckVar .and. .false. 
                else
                    a2dVarVTot = transpose(a2dVar)
                    bCheckVar = bCheckVar .and. .true. 
                    
                endif

                ! VRet
                sVarName = 'VRet';
                call HMC_Tools_IO_Get2d_NC((sVarName), iFileID, a2dVar, sVarUnits, iCols, iRows, .true., iErr)
                if(iErr /= 0) then
                    call mprintf(.true., iWARN, ' Get restart gridded data FAILED! Check restart data for '//sVarName//'!')
                    a2dVarVRet = -9999.0;
                    bCheckVar = bCheckVar .and. .false. 
                else
                    a2dVarVRet = transpose(a2dVar)
                    bCheckVar = bCheckVar .and. .true. 
                endif
                
                ! Check channel type
                if (iFlagCType.eq.2) then
                    
                    ! HydroLevel C
                    sVarName = 'HydroLevelC';
                    call HMC_Tools_IO_Get2d_NC((sVarName), iFileID, a2dVar, sVarUnits, iCols, iRows, .true., iErr)
                    if(iErr /= 0) then
                        call mprintf(.true., iWARN, ' Get restart gridded data FAILED! Check restart data for '//sVarName//'!')
                        a2dVarHydroC = -9999.0;
                        bCheckVar = bCheckVar .and. .false. 
                    else
                        a2dVarHydroC = transpose(a2dVar)
                        bCheckVar = bCheckVar .and. .true. 
                    endif
                    
                    ! HydroLevel H
                    sVarName = 'HydroLevelH';
                    call HMC_Tools_IO_Get2d_NC((sVarName), iFileID, a2dVar, sVarUnits, iCols, iRows, .true., iErr)
                    if(iErr /= 0) then
                        call mprintf(.true., iWARN, ' Get restart gridded data FAILED! Check restart data for '//sVarName//'!')
                        a2dVarHydroH = -9999.0;
                        bCheckVar = bCheckVar .and. .false. 
                    else
                        a2dVarHydroH = transpose(a2dVar)
                        bCheckVar = bCheckVar .and. .true. 
                    endif

                    ! Q upstream
                    sVarName = 'Qup';
                    call HMC_Tools_IO_Get2d_NC((sVarName), iFileID, a2dVar, sVarUnits, iCols, iRows, .true., iErr)
                    if(iErr /= 0) then
                        call mprintf(.true., iWARN, ' Get restart gridded data FAILED! Check restart data for '//sVarName//'!')
                        a2dVarQup = -9999.0;
                        bCheckVar = bCheckVar .and. .false. 
                    else
                        a2dVarQup = transpose(a2dVar)
                        bCheckVar = bCheckVar .and. .true. 
                    endif
                    
                else
                    
                    ! HydroLevel
                    sVarName = 'HydroLevel';
                    call HMC_Tools_IO_Get2d_NC((sVarName), iFileID, a2dVar, sVarUnits, iCols, iRows, .true., iErr)
                    if(iErr /= 0) then
                        call mprintf(.true., iWARN, ' Get restart gridded data FAILED! Check restart data for '//sVarName//'!')
                        a2dVarHydro = -9999.0;
                        bCheckVar = bCheckVar .and. .false. 
                    else
                        a2dVarHydro = transpose(a2dVar)
                        bCheckVar = bCheckVar .and. .true. 
                    endif
                    
                endif

                ! Routing
                sVarName = 'Routing';
                call HMC_Tools_IO_Get2d_NC((sVarName), iFileID, a2dVar, sVarUnits, iCols, iRows, .true., iErr)
                if(iErr /= 0) then
                    call mprintf(.true., iWARN, ' Get restart gridded data FAILED! Check restart data for '//sVarName//'!')
                    a2dVarRouting = -9999.0;
                    bCheckVar = bCheckVar .and. .false. 
                else
                    a2dVarRouting = transpose(a2dVar)
                    bCheckVar = bCheckVar .and. .true. 
                endif

                ! DFE
                sVarName = 'DFE';
                call HMC_Tools_IO_Get2d_NC((sVarName), iFileID, a2dVar, sVarUnits, iCols, iRows, .true., iErr)
                if(iErr /= 0) then
                    call mprintf(.true., iWARN, ' Get restart gridded data FAILED! Check restart data for '//sVarName//'!')
                    a2dVarFlowDeep = -9999.0;
                    bCheckVar = bCheckVar .and. .false. 
                else
                    a2dVarFlowDeep = transpose(a2dVar)
                    bCheckVar = bCheckVar .and. .true. 
                endif
                    
                ! WTLevel
                sVarName = 'WTLevel';
                call HMC_Tools_IO_Get2d_NC((sVarName), iFileID, a2dVar, sVarUnits, iCols, iRows, .true., iErr)
                if(iErr /= 0) then
                    call mprintf(.true., iWARN, ' Get restart gridded data FAILED! Check restart data for '//sVarName//'!')
                    a2dVarWTable = -9999.0;
                    bCheckVar = bCheckVar .and. .false. 
                else
                    a2dVarWTable = transpose(a2dVar)
                    bCheckVar = bCheckVar .and. .true. 
                endif

                ! LST
                sVarName = 'LST';
                call HMC_Tools_IO_Get2d_NC((sVarName), iFileID, a2dVar, sVarUnits, iCols, iRows, .true., iErr)
                if(iErr /= 0) then
                    call mprintf(.true., iWARN, ' Get restart gridded data FAILED! Check restart data for '//sVarName//'!')
                    a2dVarLST = -9999.0;
                    bCheckVar = bCheckVar .and. .false. 
                else
                    a2dVarLST = transpose(a2dVar)
                    bCheckVar = bCheckVar .and. .true. 
                endif

                ! Tmk
                sVarName = 'Tmk';
                call HMC_Tools_IO_Get3d_NC((sVarName), iFileID, a3dVar1, sVarUnits, dScaleFactor, &
                                            iTMarkedSteps, iCols, iRows, .true., iErr)
!                call HMC_Tools_IO_Get3d_NC((sVarName), iFileID, a3dVar1, sVarUnits, iTMarkedSteps, iCols, iRows, .true., iErr)

                
                if(iErr /= 0) then
                    call mprintf(.true., iWARN, ' Get restart gridded data FAILED! Check restart data for '//sVarName//'!')
                    a3dVarTaKMarked = -9999.0;
                    bCheckVar = bCheckVar .and. .false. 
                else
                    a3dVarTaKMarked = transpose3Dvar(a3dVar1 * dScaleFactor)
                    bCheckVar = bCheckVar .and. .true. 
                endif

                ! T24
                sVarName = 'T24';
                call HMC_Tools_IO_Get3d_NC((sVarName), iFileID, a3dVar2, sVarUnits, dScaleFactor, &
                                            iDaySteps, iCols, iRows, .true., iErr)
!                call HMC_Tools_IO_Get3d_NC((sVarName), iFileID, a3dVar2, sVarUnits, iDaySteps, iCols, iRows, .true., iErr)

                if(iErr /= 0) then
                    call mprintf(.true., iWARN, ' Get restart gridded data FAILED! Check restart data for '//sVarName//'!')
                    a3dVarTaK24 = -9999.0;
                    bCheckVar = bCheckVar .and. .false. 
                else
                    a3dVarTaK24 = transpose3Dvar(a3dVar2 * dScaleFactor)
                    bCheckVar = bCheckVar .and. .true. 
                endif

                ! WS
                sVarName = 'WS';
                call HMC_Tools_IO_Get2d_NC((sVarName), iFileID, a2dVar, sVarUnits, iCols, iRows, .false., iErr)
                if(iErr /= 0) then
                    call mprintf(.true., iWARN, ' Get restart gridded data FAILED! '// &
                        'Not in mandatory restart variables! If needed check restart data for '//sVarName//'!')
                    a2dVarWSRunoff = 0.0
                    bCheckVar = bCheckVar .and. .true. 
                else
                    a2dVarWSRunoff = transpose(a2dVar)
                    bCheckVar = bCheckVar .and. .true. 
                endif

                ! Snow variable(s)                
                if (iFlagSnow.eq.1) then
                    ! SWE
                    sVarName = 'SWE'
                    call HMC_Tools_IO_Get2d_NC((sVarName), iFileID, a2dVar, sVarUnits, iCols, iRows, .false., iErr)
                    if(iErr /= 0) then
                        call mprintf(.true., iWARN, ' Get restart gridded data FAILED! '// &
                            'Snow physics is activated! If needed check restart data for '//sVarName//'!')
                        a2dVarSWE = -9999.0;       
                        bCheckVarS = bCheckVarS .and. .false. 
                    else
                        a2dVarSWE = transpose(a2dVar)
                        bCheckVarS = bCheckVarS .and. .true. 
                    endif
                   
                    ! Snow density
                    sVarName = 'RhoS';
                    call HMC_Tools_IO_Get2d_NC((sVarName), iFileID, a2dVar, sVarUnits, iCols, iRows, .false., iErr)
                    if(iErr /= 0) then
                        call mprintf(.true., iWARN, ' Get restart gridded data FAILED! '// &
                            'Snow physics is activated! If needed check restart data for '//sVarName//'!')
                        a2dVarRhoS = -9999.0;
                        bCheckVarS = bCheckVarS .and. .false.
                    else
                        a2dVarRhoS = transpose(a2dVar)
                        bCheckVarS = bCheckVarS .and. .true. 
                    endif
                    
                    ! Snow albedo
                    sVarName = 'AlbedoS';
                    call HMC_Tools_IO_Get2d_NC((sVarName), iFileID, a2dVar, sVarUnits, iCols, iRows, .false., iErr)
                    if(iErr /= 0) then
                        call mprintf(.true., iWARN, ' Get restart gridded data FAILED! '// &
                            'Snow physics is activated! If needed check restart data for '//sVarName//'!')
                        a2dVarAlbedoS = -9999.0;
                        bCheckVarS = bCheckVarS .and. .false.
                    else
                        a2dVarAlbedoS = transpose(a2dVar)
                        bCheckVarS = bCheckVarS .and. .true. 
                    endif
                    
                    ! Snow age
                    sVarName = 'AgeS';
                    call HMC_Tools_IO_Get2d_NC((sVarName), iFileID, a2dVar, sVarUnits, iCols, iRows, .false., iErr)
                    if(iErr /= 0) then
                        call mprintf(.true., iWARN, ' Get restart gridded data FAILED! '// &
                            'Snow physics is activated! If needed check restart data for '//sVarName//'!')
                        a2iVarAgeS = -9999;
                        bCheckVarS = bCheckVarS .and. .false.
                    else
                        a2iVarAgeS = int(transpose(a2dVar))
                        bCheckVarS = bCheckVarS .and. .true. 
                    endif
                    
                    ! Air temperature last 1 day(s)
                    sVarName = 'T_1Days';
                    call HMC_Tools_IO_Get3d_NC((sVarName), iFileID, a3dVar3, sVarUnits, dScaleFactor, &
                                                iDaySteps, iCols, iRows, .true., iErr)
!                    call HMC_Tools_IO_Get3d_NC((sVarName), iFileID, a3dVar3, sVarUnits, iDaySteps, iCols, iRows, .true., iErr)

                    if(iErr /= 0) then
                        call mprintf(.true., iWARN, ' Get restart gridded data FAILED! '// &
                            'Snow physics is activated! If needed check restart data for '//sVarName//'!')
                        a3dVarTaC_1Days = -9999.0;
                        bCheckVarS = bCheckVarS .and. .false.
                    else
                        a3dVarTaC_1Days = transpose3Dvar(a3dVar3 * dScaleFactor)
                        bCheckVarS = bCheckVarS .and. .true. 
                    endif
                    
                    ! Air temperature last 5 day(s)
                    sVarName = 'T_5Days';
                    call HMC_Tools_IO_Get3d_NC((sVarName), iFileID, a3dVar4, sVarUnits, dScaleFactor, &
                                                iDaySteps*5, iCols, iRows, .true., iErr)
!                    call HMC_Tools_IO_Get3d_NC((sVarName), iFileID, a3dVar4, sVarUnits, iDaySteps*5, iCols, iRows, .true., iErr)

                    if(iErr /= 0) then
                        call mprintf(.true., iWARN, ' Get restart gridded data FAILED! '// &
                            'Snow physics is activated! If needed check restart data for '//sVarName//'!')
                        a3dVarTaC_5Days = -9999.0;
                        bCheckVarS = bCheckVarS .and. .false.
                    else
                        a3dVarTaC_5Days = transpose3Dvar(a3dVar4 * dScaleFactor)
                        bCheckVarS = bCheckVarS .and. .true. 
                    endif
                
                else
                    ! Condition snow not activated
                    a2dVarSWE = -9999.0; 
                    a2dVarRhoS = -9999.0; 
                    a2dVarAlbedoS = -9999.0;
                    a2iVarAgeS = -9999; 
                    a3dVarTaC_1Days = -9999; 
                    a3dVarTaC_5Days = -9999; 
                    
                    bCheckVarS = .true. 
                endif

                ! Closing netcdf file (drops db)
                iErr = nf90_close(iFileID)
                !------------------------------------------------------------------------------------------
                    
                !------------------------------------------------------------------------------------------
                ! Info filename
                call mprintf(.true., iINFO_Basic, ' Get filename (restart gridded): '//trim(sFileNameData_Restart)//' ... OK' )
                ! Info end
                call mprintf(.true., iINFO_Extra, ' Data :: Restart gridded :: NetCDF ... OK' )
                !------------------------------------------------------------------------------------------

            endif
            !------------------------------------------------------------------------------------------
            
        endif
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Check restart
        if (bCheckVar .eqv. .true.) then
            call mprintf(.true., iINFO_Basic, ' Data :: Restart gridded :: NetCDF :: All variable(s) are loaded! ' )
            bCheckRestart = .true.
        else
            call mprintf(.true., iINFO_Basic, ' Data :: Restart gridded :: NetCDF :: Some/All variable(s) are N/A! ' )
            call mprintf(.true., iWARN, ' Restart flag activated but some data restart are not available! ')
            call mprintf(.true., iWARN, ' Restart gridded conditions are null! ')
            bCheckRestart = .false.
        endif
        
        ! Check restart snow
        if (bCheckVarS .eqv. .true.) then
            call mprintf(.true., iINFO_Verbose, ' Data :: Restart gridded :: NetCDF :: All snow variable(s) are loaded! ' )
            bCheckRestartS = .true.
        else
            call mprintf(.true., iINFO_Verbose, ' Data :: Restart gridded :: NetCDF :: Some/All snow variable(s) are N/A! ' )
            call mprintf(.true., iWARN, ' Restart flag activated but some data snow restart are not available! ')
            call mprintf(.true., iWARN, ' Restart snow conditions are null! ')
            bCheckRestartS = .false.
        endif
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Debug
        if (iDEBUG.gt.0) then
            call mprintf(.true., iINFO_Extra, ' ========= CHECK FORCING GRIDDED NC =========== ')
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarVTot, int(oHMC_Vars(iID)%a2dDEM), 'VTOT NC') )
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarVRet, oHMC_Vars(iID)%a2iMask, 'VRET NC') )
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarHydro, oHMC_Vars(iID)%a2iMask, 'HYDRO NC') )
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarRouting, oHMC_Vars(iID)%a2iMask, 'ROUTING NC') )
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarWTable, oHMC_Vars(iID)%a2iMask, 'WTABLE NC') )
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarLST, oHMC_Vars(iID)%a2iMask, 'LST NC') )
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarFlowDeep, oHMC_Vars(iID)%a2iMask, 'FLOWDEEP NC') )
            call mprintf(.true., iINFO_Extra, checkvar(real(a2iVarAgeS), oHMC_Vars(iID)%a2iMask, 'AGES NC') )
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarSWE, oHMC_Vars(iID)%a2iMask, 'SWE NC') )
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarRhoS, oHMC_Vars(iID)%a2iMask, 'RHOS NC') )
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarAlbedoS, oHMC_Vars(iID)%a2iMask, 'ALBEDOS NC') )
            call mprintf(.true., iINFO_Extra, ' ========= CHECK FORCING GRIDDED NC =========== ')
        endif
        !------------------------------------------------------------------------------------------
        
    end subroutine HMC_Data_Restart_Gridded_NC
#endif
    !------------------------------------------------------------------------------------------
    
    !------------------------------------------------------------------------------------------
    ! Subroutine to read binary data restart
    subroutine HMC_Data_Restart_Gridded_Binary(iID, &
                                               sPathData_Restart, &
                                               iRows, iCols, &
                                               iDaySteps, iTMarkedSteps, &
                                               sTime, iFlagSnow, iFlagCType, &
                                               a2dVarVTot, a2dVarVRet, &
                                               a2dVarHydro, a2dVarRouting, &
                                               a2dVarHydroC, a2dVarHydroH, a2dVarQup, &
                                               a2dVarFlowDeep, &
                                               a2dVarWTable, &
                                               a2dVarLST, a3dVarTaKMarked, a3dVarTaK24, &
                                               a2iVarAgeS, a2dVarSWE, a2dVarAlbedoS, a2dVarRhoS, &
                                               a3dVarTaC_1Days, a3dVarTaC_5Days, &
                                               a2dVarWSRunoff, &
                                               bCheckRestart, bCheckRestartS)
    
        !------------------------------------------------------------------------------------------
        ! Variable(s)
        integer(kind = 4)                       :: iID                  
                                  
        character(len = 700), intent(in)        :: sPathData_Restart
        character(len = 700)                    :: sFileNameData_Restart, sFileNameData_Restart_Zip
        character(len = 700)                    :: sCommandUnzipFile
        character(len = 256)                    :: sVarName
        integer(kind = 4), intent(in)           :: iRows, iCols
        integer(kind = 4), intent(in)           :: iDaySteps, iTMarkedSteps
        integer(kind = 4), intent(in)           :: iFlagSnow, iFlagCType
        
        integer(kind = 4)                       :: iScaleFactor
        character(len = 19), intent(in)         :: sTime

        real(kind = 4), dimension(iRows, iCols)                                :: a2dVar
        real(kind = 4), dimension(iRows, iCols, iTMarkedSteps)                 :: a3dVar1
        real(kind = 4), dimension(iRows, iCols, iDaySteps)                     :: a3dVar2
        real(kind = 4), dimension(iRows, iCols, iDaySteps)                     :: a3dVar3
        real(kind = 4), dimension(iRows, iCols, iDaySteps*5)                   :: a3dVar4
        
        real(kind = 4), dimension(iRows, iCols),                intent(out)    :: a2dVarVTot
        real(kind = 4), dimension(iRows, iCols),                intent(out)    :: a2dVarVRet
        real(kind = 4), dimension(iRows, iCols),                intent(out)    :: a2dVarHydro
        real(kind = 4), dimension(iRows, iCols),                intent(out)    :: a2dVarRouting   
        real(kind = 4), dimension(iRows, iCols),                intent(out)    :: a2dVarHydroC
        real(kind = 4), dimension(iRows, iCols),                intent(out)    :: a2dVarHydroH
        real(kind = 4), dimension(iRows, iCols),                intent(out)    :: a2dVarQup
        real(kind = 4), dimension(iRows, iCols),                intent(out)    :: a2dVarFlowDeep       
        real(kind = 4), dimension(iRows, iCols),                intent(out)    :: a2dVarWTable
        real(kind = 4), dimension(iRows, iCols),                intent(out)    :: a2dVarLST
        real(kind = 4), dimension(iRows, iCols, iTMarkedSteps), intent(out)    :: a3dVarTaKMarked
        real(kind = 4), dimension(iRows, iCols, iDaySteps),     intent(out)    :: a3dVarTaK24
        integer(kind = 4), dimension(iRows, iCols),             intent(out)    :: a2iVarAgeS
        real(kind = 4), dimension(iRows, iCols),                intent(out)    :: a2dVarSWE
        real(kind = 4), dimension(iRows, iCols),                intent(out)    :: a2dVarAlbedoS
        real(kind = 4), dimension(iRows, iCols),                intent(out)    :: a2dVarRhoS
        real(kind = 4), dimension(iRows, iCols, iDaySteps),     intent(out)    :: a3dVarTaC_1Days
        real(kind = 4), dimension(iRows, iCols, iDaySteps*5),   intent(out)    :: a3dVarTaC_5Days
        real(kind = 4), dimension(iRows, iCols),                intent(out)    :: a2dVarWSRunoff
        
        character(len = 256)    :: sVarUnits
        integer(kind = 4)       :: iErr
        integer(kind = 4)       :: iFileID
        
        logical                 :: bFileExist, bCheckRestart, bCheckRestartS
        logical                 :: bCheckVar, bCheckVarS
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Initialize variable(s)
        a2dVarVTot = -9999.0; a2dVarVRet = -9999.0; a2dVarHydro = -9999.0; a2dVarRouting = -9999.0; 
        a2dVarHydroC = -9999.0; a2dVarHydroH = -9999.0; a2dVarQup = -9999.0; 
        a2dVarFlowDeep = -9999.0; a2dVarWTable = -9999.0; 
        a2dVarLST = -9999.0; a3dVarTaKMarked = -9999.0; a3dVarTaK24 = -9999.0; 
        a2iVarAgeS = -9999; a2dVarSWE = -9999.0; a2dVarAlbedoS = -9999.0; a2dVarRhoS = -9999.0;
        a3dVarTaC_1Days = -9999.0; a3dVarTaC_5Days = -9999.0;
        a2dVarWSRunoff = -9999.0
        
        sFileNameData_Restart = ""; sCommandUnzipFile = "";
        
        bCheckVar = .true.; bCheckVarS = .true.
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Get global information
        sCommandUnzipFile = oHMC_Namelist(iID)%sCommandUnZipFile
        ! Info start
        call mprintf(.true., iINFO_Extra, ' Data :: Restart gridded :: Binary ... ' )
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Info filename(s) at each time step
        call mprintf(.true., iINFO_Basic, ' Get (restart gridded) at time '//trim(sTime)//' ... ')
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! VTot  (example: V_201405010000.bin.gz)
        iScaleFactor = 10000
        sFileNameData_Restart = trim(sPathData_Restart)//"V_"// &
            sTime(1:4)//sTime(6:7)//sTime(9:10)// & 
            sTime(12:13)//sTime(15:16)// &
            ".bin"  
        call mprintf(.true., iINFO_Extra, ' Get filename: '//trim(sFileNameData_Restart) )

        ! Checking file input availability
        sFileNameData_Restart_Zip = trim(sFileNameData_Restart)//'.gz'
        inquire (file = sFileNameData_Restart_Zip, exist = bFileExist)
        if ( .not. bFileExist ) then
            call mprintf(.true., iWARN, ' Problem opening uncompressed binary file: '// &
                         trim(sFileNameData_Restart_Zip)//' --> Undefined restart data values' )
            a2dVar = -9999.0
            bCheckVar = bCheckVar .and. .false.
        else
            ! Unzip file
            call HMC_Tools_Generic_UnzipFile(sCommandUnzipFile, &
                                             sFileNameData_Restart_Zip, &
                                             sFileNameData_Restart, .true.)
            ! Read binary data
            a2dVar = -9999.0
            call HMC_Tools_IO_Get2d_Binary_INT(sFileNameData_Restart, a2dVar, iRows, iCols, iScaleFactor, .true., iErr) 
            bCheckVar = bCheckVar .and. .true.
        endif
        a2dVarVTot = a2dVar
        !------------------------------------------------------------------------------------------

        !------------------------------------------------------------------------------------------
        ! VRet  (example: Ret_201405010000.bin.gz)
        iScaleFactor = 10000
        sFileNameData_Restart = trim(sPathData_Restart)//"Ret_"// &
            sTime(1:4)//sTime(6:7)//sTime(9:10)// & 
            sTime(12:13)//sTime(15:16)// &
            ".bin"  
        call mprintf(.true., iINFO_Extra, ' Get filename: '//trim(sFileNameData_Restart) )

        ! Checking file input availability
        sFileNameData_Restart_Zip = trim(sFileNameData_Restart)//'.gz'
        inquire (file = sFileNameData_Restart_Zip, exist = bFileExist)
        if ( .not. bFileExist ) then
            call mprintf(.true., iWARN, ' Problem opening uncompressed binary file: '// &
                         trim(sFileNameData_Restart_Zip)//' --> Undefined restart data values' )
            a2dVar = -9999.0
            bCheckVar = bCheckVar .and. .false.
        else
            ! Unzip file
            call HMC_Tools_Generic_UnzipFile(sCommandUnzipFile, &
                                             sFileNameData_Restart_Zip, &
                                             sFileNameData_Restart, .true.)
            ! Read binary data
            a2dVar = -9999.0
            call HMC_Tools_IO_Get2d_Binary_INT(sFileNameData_Restart, a2dVar, iRows, iCols, iScaleFactor, .true., iErr) 
            bCheckVar = bCheckVar .and. .true.
        endif
        a2dVarVRet = a2dVar
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Check channel type
        if (iFlagCType.eq.2) then
        
            !------------------------------------------------------------------------------------------
            ! HydroLevel Channel (example: Wlc_201405010000.bin.gz) 
            iScaleFactor = 100000
            sFileNameData_Restart = trim(sPathData_Restart)//"Wlc_"// &
                sTime(1:4)//sTime(6:7)//sTime(9:10)// & 
                sTime(12:13)//sTime(15:16)// &
                ".bin"  
            call mprintf(.true., iINFO_Extra, ' Get filename: '//trim(sFileNameData_Restart) )

            ! Checking file input availability
            sFileNameData_Restart_Zip = trim(sFileNameData_Restart)//'.gz'
            inquire (file = sFileNameData_Restart_Zip, exist = bFileExist)
            if ( .not. bFileExist ) then
                call mprintf(.true., iWARN, ' Problem opening uncompressed binary file: '// &
                             trim(sFileNameData_Restart_Zip)//' --> Undefined restart data values' )
                a2dVar = -9999.0
                bCheckVar = bCheckVar .and. .false.
            else
                ! Unzip file
                call HMC_Tools_Generic_UnzipFile(sCommandUnzipFile, &
                                                 sFileNameData_Restart_Zip, &
                                                 sFileNameData_Restart, .true.)
                ! Read binary data
                a2dVar = -9999.0
                call HMC_Tools_IO_Get2d_Binary_INT(sFileNameData_Restart, a2dVar, iRows, iCols, iScaleFactor, .true., iErr) 
                bCheckVar = bCheckVar .and. .true.
            endif
            a2dVarHydroC = a2dVar
            !------------------------------------------------------------------------------------------

            !------------------------------------------------------------------------------------------
            ! HydroLevel Hillslopes (example: Wlh_201405010000.bin.gz) 
            iScaleFactor = 100000
            sFileNameData_Restart = trim(sPathData_Restart)//"Wlh_"// &
                sTime(1:4)//sTime(6:7)//sTime(9:10)// & 
                sTime(12:13)//sTime(15:16)// &
                ".bin"  
            call mprintf(.true., iINFO_Extra, ' Get filename: '//trim(sFileNameData_Restart) )

            ! Checking file input availability
            sFileNameData_Restart_Zip = trim(sFileNameData_Restart)//'.gz'
            inquire (file = sFileNameData_Restart_Zip, exist = bFileExist)
            if ( .not. bFileExist ) then
                call mprintf(.true., iWARN, ' Problem opening uncompressed binary file: '// &
                             trim(sFileNameData_Restart_Zip)//' --> Undefined restart data values' )
                a2dVar = -9999.0
                bCheckVar = bCheckVar .and. .false.
            else
                ! Unzip file
                call HMC_Tools_Generic_UnzipFile(sCommandUnzipFile, &
                                                 sFileNameData_Restart_Zip, &
                                                 sFileNameData_Restart, .true.)
                ! Read binary data
                a2dVar = -9999.0
                call HMC_Tools_IO_Get2d_Binary_INT(sFileNameData_Restart, a2dVar, iRows, iCols, iScaleFactor, .true., iErr) 
                bCheckVar = bCheckVar .and. .true.
            endif
            a2dVarHydroH = a2dVar
            !------------------------------------------------------------------------------------------

            !------------------------------------------------------------------------------------------
            ! Qupstream in channels (example: Qup_201405010000.bin.gz) 
            iScaleFactor = 10000
            sFileNameData_Restart = trim(sPathData_Restart)//"Qup_"// &
                sTime(1:4)//sTime(6:7)//sTime(9:10)// & 
                sTime(12:13)//sTime(15:16)// &
                ".bin"  
            call mprintf(.true., iINFO_Extra, ' Get filename: '//trim(sFileNameData_Restart) )

            ! Checking file input availability
            sFileNameData_Restart_Zip = trim(sFileNameData_Restart)//'.gz'
            inquire (file = sFileNameData_Restart_Zip, exist = bFileExist)
            if ( .not. bFileExist ) then
                call mprintf(.true., iWARN, ' Problem opening uncompressed binary file: '// &
                             trim(sFileNameData_Restart_Zip)//' --> Undefined restart data values' )
                a2dVar = -9999.0
                bCheckVar = bCheckVar .and. .false.
            else
                ! Unzip file
                call HMC_Tools_Generic_UnzipFile(sCommandUnzipFile, &
                                                 sFileNameData_Restart_Zip, &
                                                 sFileNameData_Restart, .true.)
                ! Read binary data
                a2dVar = -9999.0
                call HMC_Tools_IO_Get2d_Binary_INT(sFileNameData_Restart, a2dVar, iRows, iCols, iScaleFactor, .true., iErr) 
                bCheckVar = bCheckVar .and. .true.
            endif
            a2dVarQup = a2dVar
            !------------------------------------------------------------------------------------------
        
        
        else
        
            !------------------------------------------------------------------------------------------
            ! HydroLevel  (example: Wl_201405010000.bin.gz) 
            iScaleFactor = 100000
            sFileNameData_Restart = trim(sPathData_Restart)//"Wl_"// &
                sTime(1:4)//sTime(6:7)//sTime(9:10)// & 
                sTime(12:13)//sTime(15:16)// &
                ".bin"  
            call mprintf(.true., iINFO_Extra, ' Get filename: '//trim(sFileNameData_Restart) )

            ! Checking file input availability
            sFileNameData_Restart_Zip = trim(sFileNameData_Restart)//'.gz'
            inquire (file = sFileNameData_Restart_Zip, exist = bFileExist)
            if ( .not. bFileExist ) then
                call mprintf(.true., iWARN, ' Problem opening uncompressed binary file: '// &
                             trim(sFileNameData_Restart_Zip)//' --> Undefined restart data values' )
                a2dVar = -9999.0
                bCheckVar = bCheckVar .and. .false.
            else
                ! Unzip file
                call HMC_Tools_Generic_UnzipFile(sCommandUnzipFile, &
                                                 sFileNameData_Restart_Zip, &
                                                 sFileNameData_Restart, .true.)
                ! Read binary data
                a2dVar = -9999.0
                call HMC_Tools_IO_Get2d_Binary_INT(sFileNameData_Restart, a2dVar, iRows, iCols, iScaleFactor, .true., iErr) 
                bCheckVar = bCheckVar .and. .true.
            endif
            a2dVarHydro = a2dVar
            !------------------------------------------------------------------------------------------
        
        endif
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Routing  (example: Rou_201405010000.bin.gz)
        iScaleFactor = 100000
        sFileNameData_Restart = trim(sPathData_Restart)//"Rou_"// &
            sTime(1:4)//sTime(6:7)//sTime(9:10)// & 
            sTime(12:13)//sTime(15:16)// &
            ".bin"  
        call mprintf(.true., iINFO_Extra, ' Get filename: '//trim(sFileNameData_Restart) )

        ! Checking file input availability
        sFileNameData_Restart_Zip = trim(sFileNameData_Restart)//'.gz'
        inquire (file = sFileNameData_Restart_Zip, exist = bFileExist)
        if ( .not. bFileExist ) then
            call mprintf(.true., iWARN, ' Problem opening uncompressed binary file: '// &
                         trim(sFileNameData_Restart_Zip)//' --> Undefined restart data values' )
            a2dVar = -9999.0
            bCheckVar = bCheckVar .and. .false.
        else
            ! Unzip file
            call HMC_Tools_Generic_UnzipFile(sCommandUnzipFile, &
                                             sFileNameData_Restart_Zip, &
                                             sFileNameData_Restart, .true.)
            ! Read binary data
            a2dVar = -9999.0
            call HMC_Tools_IO_Get2d_Binary_INT(sFileNameData_Restart, a2dVar, iRows, iCols, iScaleFactor, .true., iErr) 
            bCheckVar = bCheckVar .and. .true.
        endif
        a2dVarRouting = a2dVar
        !------------------------------------------------------------------------------------------

        !------------------------------------------------------------------------------------------
        ! DFE  (example: DFE_201405010000.bin.gz)
        iScaleFactor = 10000
        sFileNameData_Restart = trim(sPathData_Restart)//"DFE_"// &
            sTime(1:4)//sTime(6:7)//sTime(9:10)// & 
            sTime(12:13)//sTime(15:16)// &
            ".bin"  
        call mprintf(.true., iINFO_Extra, ' Get filename: '//trim(sFileNameData_Restart) )

        ! Checking file input availability
        sFileNameData_Restart_Zip = trim(sFileNameData_Restart)//'.gz'
        inquire (file = sFileNameData_Restart_Zip, exist = bFileExist)
        if ( .not. bFileExist ) then
            call mprintf(.true., iWARN, ' Problem opening uncompressed binary file: '// &
                         trim(sFileNameData_Restart_Zip)//' --> Undefined restart data values' )
            a2dVar = -9999.0
            bCheckVar = bCheckVar .and. .false.
        else
            ! Unzip file
            call HMC_Tools_Generic_UnzipFile(sCommandUnzipFile, &
                                             sFileNameData_Restart_Zip, &
                                             sFileNameData_Restart, .true.)
            ! Read binary data
            a2dVar = -9999.0
            call HMC_Tools_IO_Get2d_Binary_INT(sFileNameData_Restart, a2dVar, iRows, iCols, iScaleFactor, .true., iErr) 
            bCheckVar = bCheckVar .and. .true.
        endif
        a2dVarFlowDeep = a2dVar
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! WTLevel  (example: Vw_201405010000.bin.gz)
        iScaleFactor = 10000
        sFileNameData_Restart = trim(sPathData_Restart)//"Vw_"// &
            sTime(1:4)//sTime(6:7)//sTime(9:10)// & 
            sTime(12:13)//sTime(15:16)// &
            ".bin"  
        call mprintf(.true., iINFO_Extra, ' Get filename: '//trim(sFileNameData_Restart) )

        ! Checking file input availability
        sFileNameData_Restart_Zip = trim(sFileNameData_Restart)//'.gz'
        inquire (file = sFileNameData_Restart_Zip, exist = bFileExist)
        if ( .not. bFileExist ) then
            call mprintf(.true., iWARN, ' Problem opening uncompressed binary file: '// &
                         trim(sFileNameData_Restart_Zip)//' --> Undefined restart data values' )
            a2dVar = -9999.0
            bCheckVar = bCheckVar .and. .false.
        else
            ! Unzip file
            call HMC_Tools_Generic_UnzipFile(sCommandUnzipFile, &
                                             sFileNameData_Restart_Zip, &
                                             sFileNameData_Restart, .true.)
            ! Read binary data
            a2dVar = -9999.0
            call HMC_Tools_IO_Get2d_Binary_INT(sFileNameData_Restart, a2dVar, iRows, iCols, iScaleFactor, .true., iErr) 
            bCheckVar = bCheckVar .and. .true.
        endif
        a2dVarWTable = a2dVar
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! LST  (example: Ts_201405010000.bin.gz)
        iScaleFactor = 10000
        sFileNameData_Restart = trim(sPathData_Restart)//"Ts_"// &
            sTime(1:4)//sTime(6:7)//sTime(9:10)// & 
            sTime(12:13)//sTime(15:16)// &
            ".bin"  
        call mprintf(.true., iINFO_Extra, ' Get filename: '//trim(sFileNameData_Restart) )

        ! Checking file input availability
        sFileNameData_Restart_Zip = trim(sFileNameData_Restart)//'.gz'
        inquire (file = sFileNameData_Restart_Zip, exist = bFileExist)
        if ( .not. bFileExist ) then
            call mprintf(.true., iWARN, ' Problem opening uncompressed binary file: '// &
                         trim(sFileNameData_Restart_Zip)//' --> Undefined restart data values' )
            a2dVar = -9999.0
            bCheckVar = bCheckVar .and. .false.
        else
            ! Unzip file
            call HMC_Tools_Generic_UnzipFile(sCommandUnzipFile, &
                                             sFileNameData_Restart_Zip, &
                                             sFileNameData_Restart, .true.)
            ! Read binary data
            a2dVar = -9999.0
            call HMC_Tools_IO_Get2d_Binary_INT(sFileNameData_Restart, a2dVar, iRows, iCols, iScaleFactor, .true., iErr) 
            bCheckVar = bCheckVar .and. .true.
        endif
        a2dVarLST = a2dVar
        !------------------------------------------------------------------------------------------

        !------------------------------------------------------------------------------------------
        ! TMarked  (example: Tmk_201405010000.bin.gz)
        iScaleFactor = 10000
        sFileNameData_Restart = trim(sPathData_Restart)//"Tmk_"// &
            sTime(1:4)//sTime(6:7)//sTime(9:10)// & 
            sTime(12:13)//sTime(15:16)// &
            ".bin"  
        call mprintf(.true., iINFO_Extra, ' Get filename: '//trim(sFileNameData_Restart) )

        ! Checking file input availability
        sFileNameData_Restart_Zip = trim(sFileNameData_Restart)//'.gz'
        inquire (file = sFileNameData_Restart_Zip, exist = bFileExist)
        if ( .not. bFileExist ) then
            call mprintf(.true., iWARN, ' Problem opening uncompressed binary file: '// &
                         trim(sFileNameData_Restart_Zip)//' --> Undefined restart data values' )
            a3dVar1 = -9999.0
            bCheckVar = bCheckVar .and. .false.
        else
            ! Unzip file
            call HMC_Tools_Generic_UnzipFile(sCommandUnzipFile, &
                                             sFileNameData_Restart_Zip, &
                                             sFileNameData_Restart, .true.)
            ! Read binary data
            a3dVar1 = -9999.0
            call HMC_Tools_IO_Get3d_Binary(sFileNameData_Restart, a3dVar1, &
                                           iRows, iCols, iTMarkedSteps, iScaleFactor, .true., iErr) 
            bCheckVar = bCheckVar .and. .true.
        endif
        a3dVarTaKMarked = a3dVar1
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! T24  (example: T24_201405010000.bin.gz)
        iScaleFactor = 10000
        sFileNameData_Restart = trim(sPathData_Restart)//"T24_"// &
            sTime(1:4)//sTime(6:7)//sTime(9:10)// & 
            sTime(12:13)//sTime(15:16)// &
            ".bin"  
        call mprintf(.true., iINFO_Extra, ' Get filename: '//trim(sFileNameData_Restart) )

        ! Checking file input availability
        sFileNameData_Restart_Zip = trim(sFileNameData_Restart)//'.gz'
        inquire (file = sFileNameData_Restart_Zip, exist = bFileExist)
        if ( .not. bFileExist ) then
            call mprintf(.true., iWARN, ' Problem opening uncompressed binary file: '// &
                         trim(sFileNameData_Restart_Zip)//' --> Undefined restart data values' )
            a3dVar2 = -9999.0
            bCheckVar = bCheckVar .and. .false.
        else
            ! Unzip file
            call HMC_Tools_Generic_UnzipFile(sCommandUnzipFile, &
                                             sFileNameData_Restart_Zip, &
                                             sFileNameData_Restart, .true.)
            ! Read binary data
            a3dVar2 = -9999.0
            call HMC_Tools_IO_Get3d_Binary(sFileNameData_Restart, a3dVar2, &
                                           iRows, iCols, iDaySteps, iScaleFactor, .true., iErr) 
            bCheckVar = bCheckVar .and. .true.
        endif
        a3dVarTaK24 = a3dVar2
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! WS  (example: WS_201405010000.bin.gz)
        iScaleFactor = 1000000
        sFileNameData_Restart = trim(sPathData_Restart)//"WS_"// &
            sTime(1:4)//sTime(6:7)//sTime(9:10)// & 
            sTime(12:13)//sTime(15:16)// &
            ".bin"  
        call mprintf(.true., iINFO_Extra, ' Get filename: '//trim(sFileNameData_Restart) )

        ! Checking file input availability
        sFileNameData_Restart_Zip = trim(sFileNameData_Restart)//'.gz'
        inquire (file = sFileNameData_Restart_Zip, exist = bFileExist)
        if ( .not. bFileExist ) then
            call mprintf(.true., iWARN, ' Problem opening uncompressed binary file: '// &
                         trim(sFileNameData_Restart_Zip)//' --> Undefined restart data values. Not in mandatory restart variables' )
            a2dVar = 0.0
            bCheckVar = bCheckVar .and. .true.
        else
            ! Unzip file
            call HMC_Tools_Generic_UnzipFile(sCommandUnzipFile, &
                                             sFileNameData_Restart_Zip, &
                                             sFileNameData_Restart, .true.)
            ! Read binary data
            a2dVar = -9999.0
            call HMC_Tools_IO_Get2d_Binary_INT(sFileNameData_Restart, a2dVar, iRows, iCols, iScaleFactor, .false., iErr) 
            bCheckVar = bCheckVar .and. .true.
        endif
        a2dVarWSRunoff = a2dVar
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Snow variable(s)                
        if (iFlagSnow.eq.1) then
            
            !------------------------------------------------------------------------------------------
            ! SWE  (example: SWE_201405010000.bin.gz)
            iScaleFactor = 1
            sFileNameData_Restart = trim(sPathData_Restart)//"SWE_"// &
                sTime(1:4)//sTime(6:7)//sTime(9:10)// & 
                sTime(12:13)//sTime(15:16)// &
                ".bin"  
            call mprintf(.true., iINFO_Extra, ' Get filename: '//trim(sFileNameData_Restart) )

            ! Checking file input availability
            sFileNameData_Restart_Zip = trim(sFileNameData_Restart)//'.gz'
            inquire (file = sFileNameData_Restart_Zip, exist = bFileExist)
            if ( .not. bFileExist ) then
                call mprintf(.true., iWARN, ' Problem opening uncompressed binary file: '// &
                             trim(sFileNameData_Restart_Zip)//' --> Undefined restart data values. '// &
                             'Snow physics is activated! If needed check restart data!' )
                a2dVar = -9999.0
                bCheckVarS = bCheckVarS .and. .false.
            else
                ! Unzip file
                call HMC_Tools_Generic_UnzipFile(sCommandUnzipFile, &
                                             sFileNameData_Restart_Zip, &
                                             sFileNameData_Restart, .true.)
                ! Read binary data
                a2dVar = -9999.0
                call HMC_Tools_IO_Get2d_Binary_DBL(sFileNameData_Restart, a2dVar, iRows, iCols, iScaleFactor, .true., iErr) 
                bCheckVarS = bCheckVarS .and. .true.
            endif
            a2dVarSWE = a2dVar
            !------------------------------------------------------------------------------------------
            
            !------------------------------------------------------------------------------------------
            ! Snow density  (example: Density_201405010000.bin.gz)
            iScaleFactor = 1
            sFileNameData_Restart = trim(sPathData_Restart)//"Density_"// &
                sTime(1:4)//sTime(6:7)//sTime(9:10)// & 
                sTime(12:13)//sTime(15:16)// &
                ".bin"  
            call mprintf(.true., iINFO_Extra, ' Get filename: '//trim(sFileNameData_Restart) )

            ! Checking file input availability
            sFileNameData_Restart_Zip = trim(sFileNameData_Restart)//'.gz'
            inquire (file = sFileNameData_Restart_Zip, exist = bFileExist)
            if ( .not. bFileExist ) then
                call mprintf(.true., iWARN, ' Problem opening uncompressed binary file: '// &
                             trim(sFileNameData_Restart_Zip)//' --> Undefined restart data values. '// &
                             'Snow physics is activated! If needed check restart data!' )
                a2dVar = -9999.0
                bCheckVarS = bCheckVarS .and. .false.
            else
                ! Unzip file
                call HMC_Tools_Generic_UnzipFile(sCommandUnzipFile, &
                                             sFileNameData_Restart_Zip, &
                                             sFileNameData_Restart, .true.)
                ! Read binary data
                a2dVar = -9999.0
                call HMC_Tools_IO_Get2d_Binary_DBL(sFileNameData_Restart, a2dVar, iRows, iCols, iScaleFactor, .true., iErr) 
                bCheckVarS = bCheckVarS .and. .true.
            endif
            a2dVarRhoS = a2dVar
            !------------------------------------------------------------------------------------------
            
            !------------------------------------------------------------------------------------------
            ! Snow albedo  (example: Density_201405010000.bin.gz)
            iScaleFactor = 1
            sFileNameData_Restart = trim(sPathData_Restart)//"AlbedoS_"// &
                sTime(1:4)//sTime(6:7)//sTime(9:10)// & 
                sTime(12:13)//sTime(15:16)// &
                ".bin"  
            call mprintf(.true., iINFO_Extra, ' Get filename: '//trim(sFileNameData_Restart) )

            ! Checking file input availability
            sFileNameData_Restart_Zip = trim(sFileNameData_Restart)//'.gz'
            inquire (file = sFileNameData_Restart_Zip, exist = bFileExist)
            if ( .not. bFileExist ) then
                call mprintf(.true., iWARN, ' Problem opening uncompressed binary file: '// &
                             trim(sFileNameData_Restart_Zip)//' --> Undefined restart data values. '// &
                             'Snow physics is activated! If needed check restart data!' )
                a2dVar = -9999.0
                bCheckVarS = bCheckVarS .and. .false.
            else
                ! Unzip file
                call HMC_Tools_Generic_UnzipFile(sCommandUnzipFile, &
                                             sFileNameData_Restart_Zip, &
                                             sFileNameData_Restart, .true.)
                ! Read binary data
                a2dVar = -9999.0
                call HMC_Tools_IO_Get2d_Binary_DBL(sFileNameData_Restart, a2dVar, iRows, iCols, iScaleFactor, .true., iErr) 
                bCheckVarS = bCheckVarS .and. .true.
            endif
            a2dVarAlbedoS = a2dVar
            !------------------------------------------------------------------------------------------
            
            !------------------------------------------------------------------------------------------
            ! Snow age  (example: Age_201405010000.bin.gz)
            iScaleFactor = 1
            sFileNameData_Restart = trim(sPathData_Restart)//"Age_"// &
                sTime(1:4)//sTime(6:7)//sTime(9:10)// & 
                sTime(12:13)//sTime(15:16)// &
                ".bin"  
            call mprintf(.true., iINFO_Extra, ' Get filename: '//trim(sFileNameData_Restart) )

            ! Checking file input availability
            sFileNameData_Restart_Zip = trim(sFileNameData_Restart)//'.gz'
            inquire (file = sFileNameData_Restart_Zip, exist = bFileExist)
            if ( .not. bFileExist ) then
                call mprintf(.true., iWARN, ' Problem opening uncompressed binary file: '// &
                             trim(sFileNameData_Restart_Zip)//' --> Undefined restart data values. '// &
                             'Snow physics is activated! If needed check restart data!' )
                a2dVar = -9999.0
                bCheckVarS = bCheckVarS .and. .false.
            else
                ! Unzip file
                call HMC_Tools_Generic_UnzipFile(sCommandUnzipFile, &
                                             sFileNameData_Restart_Zip, &
                                             sFileNameData_Restart, .true.)
                ! Read binary data
                a2dVar = -9999.0
                call HMC_Tools_IO_Get2d_Binary_DBL(sFileNameData_Restart, a2dVar, iRows, iCols, iScaleFactor, .true., iErr) 
                bCheckVarS = bCheckVarS .and. .true.
            endif
            a2iVarAgeS = int(a2dVar)
            !------------------------------------------------------------------------------------------
            
            !------------------------------------------------------------------------------------------
            ! Air Temperature last 1 day(s)  (example: Ta_1Days_201405010000.bin.gz)
            iScaleFactor = 1
            sFileNameData_Restart = trim(sPathData_Restart)//"Ta_1Days_"// &
                sTime(1:4)//sTime(6:7)//sTime(9:10)// & 
                sTime(12:13)//sTime(15:16)// &
                ".bin"  
            call mprintf(.true., iINFO_Extra, ' Get filename: '//trim(sFileNameData_Restart) )

            ! Checking file input availability
            sFileNameData_Restart_Zip = trim(sFileNameData_Restart)//'.gz'
            inquire (file = sFileNameData_Restart_Zip, exist = bFileExist)
            if ( .not. bFileExist ) then
                call mprintf(.true., iWARN, ' Problem opening uncompressed binary file: '// &
                             trim(sFileNameData_Restart_Zip)//' --> Undefined restart data values. '// &
                             'Snow physics is activated! If needed check restart data!' )
                a3dVar3 = -9999.0
                bCheckVarS = bCheckVarS .and. .false.
            else
                ! Unzip file
                call HMC_Tools_Generic_UnzipFile(sCommandUnzipFile, &
                                                 sFileNameData_Restart_Zip, &
                                                 sFileNameData_Restart, .true.)
                ! Read binary data
                a3dVar3 = -9999.0
                call HMC_Tools_IO_Get3d_Binary(sFileNameData_Restart, a3dVar3, &
                                               iRows, iCols, iDaySteps, iScaleFactor, .true., iErr) 
                bCheckVarS = bCheckVarS .and. .true.
            endif
            a3dVarTaC_1Days = a3dVar3
            !------------------------------------------------------------------------------------------
            
            !------------------------------------------------------------------------------------------
            ! Air Temperature last 5 day(s)  (example: Ta_5Days_201405010000.bin.gz)
            iScaleFactor = 1
            sFileNameData_Restart = trim(sPathData_Restart)//"Ta_5Days_"// &
                sTime(1:4)//sTime(6:7)//sTime(9:10)// & 
                sTime(12:13)//sTime(15:16)// &
                ".bin"  
            call mprintf(.true., iINFO_Extra, ' Get filename: '//trim(sFileNameData_Restart) )

            ! Checking file input availability
            sFileNameData_Restart_Zip = trim(sFileNameData_Restart)//'.gz'
            inquire (file = sFileNameData_Restart_Zip, exist = bFileExist)
            if ( .not. bFileExist ) then
                call mprintf(.true., iWARN, ' Problem opening uncompressed binary file: '// &
                             trim(sFileNameData_Restart_Zip)//' --> Undefined restart data values. '// &
                             'Snow physics is activated! If needed check restart data!' )
                a3dVar4 = -9999.0
                bCheckVarS = bCheckVarS .and. .false.
            else
                ! Unzip file
                call HMC_Tools_Generic_UnzipFile(sCommandUnzipFile, &
                                                 sFileNameData_Restart_Zip, &
                                                 sFileNameData_Restart, .true.)
                ! Read binary data
                a3dVar4 = -9999.0
                call HMC_Tools_IO_Get3d_Binary(sFileNameData_Restart, a3dVar4, &
                                               iRows, iCols, iDaySteps*5, iScaleFactor, .true., iErr) 
                bCheckVarS = bCheckVarS .and. .true.
            endif
            a3dVarTaC_5Days = a3dVar4
            !------------------------------------------------------------------------------------------
            
        else
            
            !------------------------------------------------------------------------------------------
            ! Condition(s) if snow not activated
            a2dVarSWE = -9999.0; 
            a2dVarRhoS = -9999.0; 
            a2dVarAlbedoS = -9999.0; 
            a2iVarAgeS = -9999; 
            a3dVarTaC_1Days = -9999.0; 
            a3dVarTaC_5Days = -9999.0; 
            
            bCheckVarS = .true.
            !------------------------------------------------------------------------------------------
            
        endif
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Info filename(s) at each time step
        call mprintf(.true., iINFO_Basic, ' Get (restart gridded) at time '//trim(sTime)//' ... OK')
        ! Info start
        call mprintf(.true., iINFO_Extra, ' Data :: Restart gridded :: Binary ... OK' )
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Check restart
        if (bCheckVar .eqv. .true.) then
            call mprintf(.true., iINFO_Basic, ' Data :: Restart gridded :: Binary :: All variable(s) are loaded! ' )
            bCheckRestart = .true.
        else
            call mprintf(.true., iINFO_Basic, ' Data :: Restart gridded :: Binary :: Some/All variable(s) are N/A! ' )
            call mprintf(.true., iWARN, ' Restart flag activated but data restart are not available! ')
            call mprintf(.true., iWARN, ' Restart gridded conditions are null! ')
            bCheckRestart = .false.
        endif
        ! Check restart snow
        if (bCheckVarS .eqv. .true.) then
            call mprintf(.true., iINFO_Verbose, ' Data :: Restart gridded :: Binary :: All snow variable(s) are loaded! ' )
            bCheckRestartS = .true.
        else
            call mprintf(.true., iINFO_Verbose, ' Data :: Restart gridded :: Binary :: Some/All snow variable(s) are N/A! ' )
            call mprintf(.true., iWARN, ' Restart flag activated but snow data restart are not available! ')
            call mprintf(.true., iWARN, ' Restart snow conditions are null! ')
            bCheckRestartS = .false.
        endif
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Debug
        if (iDEBUG.gt.0) then
            call mprintf(.true., iINFO_Extra, ' ========= CHECK FORCING GRIDDED BINARY =========== ')
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarVTot, int(oHMC_Vars(iID)%a2dDEM), 'VTOT BIN') )
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarVRet, oHMC_Vars(iID)%a2iMask, 'VRET BIN') )
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarHydro, oHMC_Vars(iID)%a2iMask, 'HYDRO BIN') )
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarRouting, oHMC_Vars(iID)%a2iMask, 'ROUTING BIN') )
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarWTable, oHMC_Vars(iID)%a2iMask, 'WTABLE BIN') )
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarLST, oHMC_Vars(iID)%a2iMask, 'LST BIN') )
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarFlowDeep, oHMC_Vars(iID)%a2iMask, 'FLOWDEEP BIN') )
            call mprintf(.true., iINFO_Extra, checkvar(real(a2iVarAgeS), oHMC_Vars(iID)%a2iMask, 'AGES BIN') )
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarSWE, oHMC_Vars(iID)%a2iMask, 'SWE BIN') )
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarRhoS, oHMC_Vars(iID)%a2iMask, 'RHOS BIN') )
            call mprintf(.true., iINFO_Extra, checkvar(a2dVarAlbedoS, oHMC_Vars(iID)%a2iMask, 'ALBEDOS BIN') )
            call mprintf(.true., iINFO_Extra, ' ========= CHECK FORCING GRIDDED BINARY =========== ')
        endif
        !------------------------------------------------------------------------------------------
        
    end subroutine HMC_Data_Restart_Gridded_Binary
    !------------------------------------------------------------------------------------------
    
end module HMC_Module_Data_Restart_Gridded
!------------------------------------------------------------------------------------------

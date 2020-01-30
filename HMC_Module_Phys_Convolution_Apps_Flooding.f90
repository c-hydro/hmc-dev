!------------------------------------------------------------------------------------
! File:   HMC_Module_Phys_Convolution_Apps_Flooding.f90
!
! Author(s):    Fabio Delogu, Francesco Silvestro, Simone Gabellani, Valerio Basso
! Date:         20190805
!
! Convolution Apps Flooding subroutine(s) for HMC model
!------------------------------------------------------------------------------------

!------------------------------------------------------------------------------------
! Module Header
module HMC_Module_Phys_Convolution_Apps_Flooding

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
    ! Subroutine to compute flooding distributed discharge
    subroutine HMC_Phys_Convolution_Apps_Flooding(iID, iRows, iCols, &
                                                   dDtSurfaceflow, a2dVarBF, a2dVarHydroC)
    
        !------------------------------------------------------------------------------------------
        ! Variable(s)
        integer(kind = 4)           :: iID, iRows, iCols
        
        real(kind = 4)              :: dDtSurfaceflow
        
        real(kind = 4), dimension (iRows, iCols)                    :: a2dVarQfloodIL, a2dVarQfloodIR
        real(kind = 4), dimension (iRows, iCols)                    :: a2dVarLevBankR, a2dVarLevBankL
        real(kind = 4), dimension (iRows, iCols)                    :: a2dVarFirst, a2dVarAreaCell
        real(kind = 4), dimension (iRows, iCols), intent(in)       :: a2dVarBF
        real(kind = 4), dimension (iRows, iCols), intent(inout)    :: a2dVarHydroC

        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Variable(s) initialization 
        a2dVarQfloodIL = 0.0; a2dVarQfloodIR = 0.0; ! left and right flooding discharge
        !------------------------------------------------------------------------------------------
        
        !------------------------------------------------------------------------------------------
        ! Get variable(s)
        a2dVarLevBankL = oHMC_Vars(iID)%a2dLevBankL                           
        a2dVarLevBankR = oHMC_Vars(iID)%a2dLevBankR
        a2dVarFirst = oHMC_Vars(iID)%a2dFirst
        a2dVarAreaCell = oHMC_Vars(iID)%a2dAreaCell

        ! Info start
        if (iDEBUG.gt.0) then
            call mprintf(.true., iINFO_Extra, ' Phys :: Convolution :: Flooding ... ' )
        endif

        a2dVarQfloodIL = 0.0
        a2dVarQfloodIR = 0.0

        ! Flooding on right bank first
        where (oHMC_Vars(iID)%a2dDEM.gt.0.0.and.a2dVarHydroC.gt.a2dVarLevBankR.and.a2dVarFirst.eq.1.0)     

            ! Q for thick wall weir
            a2dVarQfloodIR = 1.705*a2dVarBF*(a2dVarHydroC-a2dVarLevBankR)**(3/2)
            !a2dVarHydroC=a2dVarHydroC-a2dVarQfloodIR*dDtSurfaceflow/((a2dVarHydroC-a2dVarLevBankR)*dsqrt(a2dVarAreaCell))
            a2dVarHydroC = a2dVarHydroC-a2dVarQfloodIR*dDtSurfaceflow/(a2dVarAreaCell)

            !Check Level
            where (a2dVarHydroC .lt. a2dVarLevBankR)
                a2dVarHydroC = a2dVarLevBankR            
            endwhere

        endwhere

        where(oHMC_Vars(iID)%a2dDEM.gt.0.0.and.a2dVarHydroC.gt.a2dVarLevBankL.and.a2dVarFirst.eq.1.0.and.a2dVarLevBankL.gt.0.0)   

            a2dVarQfloodIL = 1.705*a2dVarBF*(a2dVarHydroC-a2dVarLevBankL)**(3/2)
            !a2dVarHydroC=a2dVarHydroC-a2dVarQfloodIL*dDtSurfaceflow/((a2dVarHydroC-a2dLevBankL)*dsqrt(a2dVarAreaCell))                       
            a2dVarHydroC = a2dVarHydroC-a2dVarQfloodIL*dDtSurfaceflow/(a2dVarAreaCell)

            where(a2dVarHydroC .lt. a2dVarLevBankL)
                a2dVarHydroC = a2dVarLevBankL            
            endwhere

        endwhere

        ! Flooding on Left bank first
        where(oHMC_Vars(iID)%a2dDEM.gt.0.0.and.a2dVarHydroC.gt.a2dVarLevBankL.and.a2dVarFirst.eq.2.0)
            a2dVarQfloodIL = 1.705*a2dVarBF*(a2dVarHydroC-a2dVarLevBankL)**(3/2)
            a2dVarHydroC = a2dVarHydroC-a2dVarQfloodIL*dDtSurfaceflow/(a2dVarAreaCell)   

            where (a2dVarHydroC.lt.a2dVarLevBankL)
                a2dVarHydroC = a2dVarLevBankL          
            endwhere

        endwhere

        where(oHMC_Vars(iID)%a2dDEM.gt.0.0.and.a2dVarHydroC.gt.a2dVarLevBankR.and.a2dVarFirst.eq.2.0.and.a2dVarLevBankR.gt.0.0)
            a2dVarQfloodIR = 1.705*a2dVarBF*(a2dVarHydroC-a2dVarLevBankR)**(3/2)
            a2dVarHydroC = a2dVarHydroC-a2dVarQfloodIR*dDtSurfaceflow/(a2dVarAreaCell)  

            where (a2dVarHydroC.lt.a2dVarLevBankR)
                a2dVarHydroC = a2dVarLevBankR            
            endwhere        

        endwhere

        ! Flooding with same rate on left and right banks  
        where(oHMC_Vars(iID)%a2dDEM.gt.0.0.and.a2dVarFirst.eq.0.0.and.a2dVarHydroC.gt.a2dVarLevBankR)              
            ! Q for thick wall weir
            a2dVarQfloodIR = 1.705*a2dVarBF*(a2dVarHydroC-a2dVarLevBankR)**(3/2)
            ! Multiply X2 because flooding on both right and left banks
            a2dVarHydroC = a2dVarHydroC-a2dVarQfloodIR*dDtSurfaceflow/(a2dVarAreaCell)*2.0
            ! Split between right and left
            a2dVarQfloodIL = a2dVarQfloodIR   

            where(a2dVarHydroC .lt. a2dVarLevBankR)
                a2dVarHydroC = a2dVarLevBankR            
            endwhere

        endwhere  
        !------------------------------------------------------------------------------------------       

        !------------------------------------------------------------------------------------------
        ! Updating field(s) in global declaration
        oHMC_Vars(iID)%a2dQfloodIL = a2dVarQfloodIL
        oHMC_Vars(iID)%a2dQfloodIR = a2dVarQfloodIR

        ! Info end
        if (iDEBUG.gt.0) then
            call mprintf(.true., iINFO_Extra, ' Phys :: Convolution :: Flooding ... OK' )
        endif
        !------------------------------------------------------------------------------------------
                                                                    
    end subroutine HMC_Phys_Convolution_Apps_Flooding
    !------------------------------------------------------------------------------------------

end module HMC_Module_Phys_Convolution_Apps_Flooding
!------------------------------------------------------------------------------------------
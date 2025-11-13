#
# Generated Makefile - do not edit!
#
# Edit the Makefile in the project folder instead (../Makefile). Each target
# has a -pre and a -post target defined where you can add customized code.
#
# This makefile implements configuration specific macros and targets.


# Environment
MKDIR=mkdir
CP=cp
GREP=grep
NM=nm
CCADMIN=CCadmin
RANLIB=ranlib
CC=gcc
CCC=g++
CXX=g++
FC=gfortran
AS=as

# Macros
CND_PLATFORM=GNU-Linux
CND_DLIB_EXT=so
CND_CONF=Release
CND_DISTDIR=dist
CND_BUILDDIR=build

# Include project Makefile
include Makefile

# Object Directory
OBJECTDIR=${CND_BUILDDIR}/${CND_CONF}/${CND_PLATFORM}

# Object Files
OBJECTFILES= \
	${OBJECTDIR}/HMC_Main.o \
	${OBJECTDIR}/HMC_Module_Args.o \
	${OBJECTDIR}/HMC_Module_Data_Forcing_Gridded.o \
	${OBJECTDIR}/HMC_Module_Data_Forcing_Point.o \
	${OBJECTDIR}/HMC_Module_Data_Forcing_TimeSeries.o \
	${OBJECTDIR}/HMC_Module_Data_Output_Gridded.o \
	${OBJECTDIR}/HMC_Module_Data_Output_Point.o \
	${OBJECTDIR}/HMC_Module_Data_Output_TimeSeries.o \
	${OBJECTDIR}/HMC_Module_Data_Restart_Gridded.o \
	${OBJECTDIR}/HMC_Module_Data_Restart_Point.o \
	${OBJECTDIR}/HMC_Module_Data_State_Gridded.o \
	${OBJECTDIR}/HMC_Module_Data_State_Point.o \
	${OBJECTDIR}/HMC_Module_Data_Static_Gridded.o \
	${OBJECTDIR}/HMC_Module_Data_Static_Point.o \
	${OBJECTDIR}/HMC_Module_Data_Updating_Gridded.o \
	${OBJECTDIR}/HMC_Module_Info_Gridded.o \
	${OBJECTDIR}/HMC_Module_Info_Point.o \
	${OBJECTDIR}/HMC_Module_Info_Time.o \
	${OBJECTDIR}/HMC_Module_Namelist.o \
	${OBJECTDIR}/HMC_Module_Phys.o \
	${OBJECTDIR}/HMC_Module_Phys_Convolution_Apps_DeepFlow.o \
	${OBJECTDIR}/HMC_Module_Phys_Convolution_Apps_Discharge.o \
	${OBJECTDIR}/HMC_Module_Phys_Convolution_Apps_Flooding.o \
	${OBJECTDIR}/HMC_Module_Phys_Convolution_Apps_Horton.o \
	${OBJECTDIR}/HMC_Module_Phys_Convolution_Apps_HydraulicStructure.o \
	${OBJECTDIR}/HMC_Module_Phys_Convolution_Apps_IntegrationStep.o \
	${OBJECTDIR}/HMC_Module_Phys_Convolution_Apps_SubFlow.o \
	${OBJECTDIR}/HMC_Module_Phys_Convolution_Apps_SurfaceFlow.o \
	${OBJECTDIR}/HMC_Module_Phys_Convolution_Type_ChannelFraction.o \
	${OBJECTDIR}/HMC_Module_Phys_Convolution_Type_ChannelNetwork.o \
	${OBJECTDIR}/HMC_Module_Phys_ET.o \
	${OBJECTDIR}/HMC_Module_Phys_HydraulicStructure.o \
	${OBJECTDIR}/HMC_Module_Phys_LSM.o \
	${OBJECTDIR}/HMC_Module_Phys_LSM_Apps.o \
	${OBJECTDIR}/HMC_Module_Phys_Retention.o \
	${OBJECTDIR}/HMC_Module_Phys_Snow.o \
	${OBJECTDIR}/HMC_Module_Phys_Snow_Apps.o \
	${OBJECTDIR}/HMC_Module_Phys_StateUpdating.o \
	${OBJECTDIR}/HMC_Module_Tools_Debug.o \
	${OBJECTDIR}/HMC_Module_Tools_Generic.o \
	${OBJECTDIR}/HMC_Module_Tools_IO.o \
	${OBJECTDIR}/HMC_Module_Tools_Interp.o \
	${OBJECTDIR}/HMC_Module_Tools_Time.o \
	${OBJECTDIR}/HMC_Module_Vars_Loader.o \
	${OBJECTDIR}/HMC_Module_Vars_Manager.o \
	${OBJECTDIR}/debug_2dVar.o \
	${OBJECTDIR}/debug_2dVar.o \
	${OBJECTDIR}/gnufor2.o


# C Compiler Flags
CFLAGS=

# CC Compiler Flags
CCFLAGS=
CXXFLAGS=

# Fortran Compiler Flags
FFLAGS=

# Assembler Flags
ASFLAGS=

# Link Libraries and Options
LDLIBSOPTIONS=

# Build Targets
.build-conf: ${BUILD_SUBPROJECTS}
	"${MAKE}"  -f nbproject/Makefile-${CND_CONF}.mk ${CND_DISTDIR}/${CND_CONF}/${CND_PLATFORM}/fortran_continuum_codes

${CND_DISTDIR}/${CND_CONF}/${CND_PLATFORM}/fortran_continuum_codes: ${OBJECTFILES}
	${MKDIR} -p ${CND_DISTDIR}/${CND_CONF}/${CND_PLATFORM}
	${LINK.f} -o ${CND_DISTDIR}/${CND_CONF}/${CND_PLATFORM}/fortran_continuum_codes ${OBJECTFILES} ${LDLIBSOPTIONS}

${OBJECTDIR}/HMC_Main.o: HMC_Main.f90
	${MKDIR} -p ${OBJECTDIR}
	$(COMPILE.f) -O2 -o ${OBJECTDIR}/HMC_Main.o HMC_Main.f90

${OBJECTDIR}/HMC_Module_Args.o: HMC_Module_Args.f90
	${MKDIR} -p ${OBJECTDIR}
	$(COMPILE.f) -O2 -o ${OBJECTDIR}/HMC_Module_Args.o HMC_Module_Args.f90

${OBJECTDIR}/HMC_Module_Data_Forcing_Gridded.o: HMC_Module_Data_Forcing_Gridded.f90
	${MKDIR} -p ${OBJECTDIR}
	$(COMPILE.f) -O2 -o ${OBJECTDIR}/HMC_Module_Data_Forcing_Gridded.o HMC_Module_Data_Forcing_Gridded.f90

${OBJECTDIR}/HMC_Module_Data_Forcing_Point.o: HMC_Module_Data_Forcing_Point.f90
	${MKDIR} -p ${OBJECTDIR}
	$(COMPILE.f) -O2 -o ${OBJECTDIR}/HMC_Module_Data_Forcing_Point.o HMC_Module_Data_Forcing_Point.f90

${OBJECTDIR}/HMC_Module_Data_Forcing_TimeSeries.o: HMC_Module_Data_Forcing_TimeSeries.f90
	${MKDIR} -p ${OBJECTDIR}
	$(COMPILE.f) -O2 -o ${OBJECTDIR}/HMC_Module_Data_Forcing_TimeSeries.o HMC_Module_Data_Forcing_TimeSeries.f90

${OBJECTDIR}/HMC_Module_Data_Output_Gridded.o: HMC_Module_Data_Output_Gridded.f90
	${MKDIR} -p ${OBJECTDIR}
	$(COMPILE.f) -O2 -o ${OBJECTDIR}/HMC_Module_Data_Output_Gridded.o HMC_Module_Data_Output_Gridded.f90

${OBJECTDIR}/HMC_Module_Data_Output_Point.o: HMC_Module_Data_Output_Point.f90
	${MKDIR} -p ${OBJECTDIR}
	$(COMPILE.f) -O2 -o ${OBJECTDIR}/HMC_Module_Data_Output_Point.o HMC_Module_Data_Output_Point.f90

${OBJECTDIR}/HMC_Module_Data_Output_TimeSeries.o: HMC_Module_Data_Output_TimeSeries.f90
	${MKDIR} -p ${OBJECTDIR}
	$(COMPILE.f) -O2 -o ${OBJECTDIR}/HMC_Module_Data_Output_TimeSeries.o HMC_Module_Data_Output_TimeSeries.f90

${OBJECTDIR}/HMC_Module_Data_Restart_Gridded.o: HMC_Module_Data_Restart_Gridded.f90
	${MKDIR} -p ${OBJECTDIR}
	$(COMPILE.f) -O2 -o ${OBJECTDIR}/HMC_Module_Data_Restart_Gridded.o HMC_Module_Data_Restart_Gridded.f90

${OBJECTDIR}/HMC_Module_Data_Restart_Point.o: HMC_Module_Data_Restart_Point.f90
	${MKDIR} -p ${OBJECTDIR}
	$(COMPILE.f) -O2 -o ${OBJECTDIR}/HMC_Module_Data_Restart_Point.o HMC_Module_Data_Restart_Point.f90

${OBJECTDIR}/HMC_Module_Data_State_Gridded.o: HMC_Module_Data_State_Gridded.f90
	${MKDIR} -p ${OBJECTDIR}
	$(COMPILE.f) -O2 -o ${OBJECTDIR}/HMC_Module_Data_State_Gridded.o HMC_Module_Data_State_Gridded.f90

${OBJECTDIR}/HMC_Module_Data_State_Point.o: HMC_Module_Data_State_Point.f90
	${MKDIR} -p ${OBJECTDIR}
	$(COMPILE.f) -O2 -o ${OBJECTDIR}/HMC_Module_Data_State_Point.o HMC_Module_Data_State_Point.f90

${OBJECTDIR}/HMC_Module_Data_Static_Gridded.o: HMC_Module_Data_Static_Gridded.f90
	${MKDIR} -p ${OBJECTDIR}
	$(COMPILE.f) -O2 -o ${OBJECTDIR}/HMC_Module_Data_Static_Gridded.o HMC_Module_Data_Static_Gridded.f90

${OBJECTDIR}/HMC_Module_Data_Static_Point.o: HMC_Module_Data_Static_Point.f90
	${MKDIR} -p ${OBJECTDIR}
	$(COMPILE.f) -O2 -o ${OBJECTDIR}/HMC_Module_Data_Static_Point.o HMC_Module_Data_Static_Point.f90

${OBJECTDIR}/HMC_Module_Data_Updating_Gridded.o: HMC_Module_Data_Updating_Gridded.f90
	${MKDIR} -p ${OBJECTDIR}
	$(COMPILE.f) -O2 -o ${OBJECTDIR}/HMC_Module_Data_Updating_Gridded.o HMC_Module_Data_Updating_Gridded.f90

${OBJECTDIR}/HMC_Module_Info_Gridded.o: HMC_Module_Info_Gridded.f90
	${MKDIR} -p ${OBJECTDIR}
	$(COMPILE.f) -O2 -o ${OBJECTDIR}/HMC_Module_Info_Gridded.o HMC_Module_Info_Gridded.f90

${OBJECTDIR}/HMC_Module_Info_Point.o: HMC_Module_Info_Point.f90
	${MKDIR} -p ${OBJECTDIR}
	$(COMPILE.f) -O2 -o ${OBJECTDIR}/HMC_Module_Info_Point.o HMC_Module_Info_Point.f90

${OBJECTDIR}/HMC_Module_Info_Time.o: HMC_Module_Info_Time.f90
	${MKDIR} -p ${OBJECTDIR}
	$(COMPILE.f) -O2 -o ${OBJECTDIR}/HMC_Module_Info_Time.o HMC_Module_Info_Time.f90

${OBJECTDIR}/HMC_Module_Namelist.o: HMC_Module_Namelist.f90
	${MKDIR} -p ${OBJECTDIR}
	$(COMPILE.f) -O2 -o ${OBJECTDIR}/HMC_Module_Namelist.o HMC_Module_Namelist.f90

${OBJECTDIR}/HMC_Module_Phys.o: HMC_Module_Phys.f90
	${MKDIR} -p ${OBJECTDIR}
	$(COMPILE.f) -O2 -o ${OBJECTDIR}/HMC_Module_Phys.o HMC_Module_Phys.f90

${OBJECTDIR}/HMC_Module_Phys_Convolution_Apps_DeepFlow.o: HMC_Module_Phys_Convolution_Apps_DeepFlow.f90
	${MKDIR} -p ${OBJECTDIR}
	$(COMPILE.f) -O2 -o ${OBJECTDIR}/HMC_Module_Phys_Convolution_Apps_DeepFlow.o HMC_Module_Phys_Convolution_Apps_DeepFlow.f90

${OBJECTDIR}/HMC_Module_Phys_Convolution_Apps_Discharge.o: HMC_Module_Phys_Convolution_Apps_Discharge.f90
	${MKDIR} -p ${OBJECTDIR}
	$(COMPILE.f) -O2 -o ${OBJECTDIR}/HMC_Module_Phys_Convolution_Apps_Discharge.o HMC_Module_Phys_Convolution_Apps_Discharge.f90

${OBJECTDIR}/HMC_Module_Phys_Convolution_Apps_Flooding.o: HMC_Module_Phys_Convolution_Apps_Flooding.f90
	${MKDIR} -p ${OBJECTDIR}
	$(COMPILE.f) -O2 -o ${OBJECTDIR}/HMC_Module_Phys_Convolution_Apps_Flooding.o HMC_Module_Phys_Convolution_Apps_Flooding.f90

${OBJECTDIR}/HMC_Module_Phys_Convolution_Apps_Horton.o: HMC_Module_Phys_Convolution_Apps_Horton.f90
	${MKDIR} -p ${OBJECTDIR}
	$(COMPILE.f) -O2 -o ${OBJECTDIR}/HMC_Module_Phys_Convolution_Apps_Horton.o HMC_Module_Phys_Convolution_Apps_Horton.f90

${OBJECTDIR}/HMC_Module_Phys_Convolution_Apps_HydraulicStructure.o: HMC_Module_Phys_Convolution_Apps_HydraulicStructure.f90
	${MKDIR} -p ${OBJECTDIR}
	$(COMPILE.f) -O2 -o ${OBJECTDIR}/HMC_Module_Phys_Convolution_Apps_HydraulicStructure.o HMC_Module_Phys_Convolution_Apps_HydraulicStructure.f90

${OBJECTDIR}/HMC_Module_Phys_Convolution_Apps_IntegrationStep.o: HMC_Module_Phys_Convolution_Apps_IntegrationStep.f90
	${MKDIR} -p ${OBJECTDIR}
	$(COMPILE.f) -O2 -o ${OBJECTDIR}/HMC_Module_Phys_Convolution_Apps_IntegrationStep.o HMC_Module_Phys_Convolution_Apps_IntegrationStep.f90

${OBJECTDIR}/HMC_Module_Phys_Convolution_Apps_SubFlow.o: HMC_Module_Phys_Convolution_Apps_SubFlow.f90
	${MKDIR} -p ${OBJECTDIR}
	$(COMPILE.f) -O2 -o ${OBJECTDIR}/HMC_Module_Phys_Convolution_Apps_SubFlow.o HMC_Module_Phys_Convolution_Apps_SubFlow.f90

${OBJECTDIR}/HMC_Module_Phys_Convolution_Apps_SurfaceFlow.o: HMC_Module_Phys_Convolution_Apps_SurfaceFlow.f90
	${MKDIR} -p ${OBJECTDIR}
	$(COMPILE.f) -O2 -o ${OBJECTDIR}/HMC_Module_Phys_Convolution_Apps_SurfaceFlow.o HMC_Module_Phys_Convolution_Apps_SurfaceFlow.f90

${OBJECTDIR}/HMC_Module_Phys_Convolution_Type_ChannelFraction.o: HMC_Module_Phys_Convolution_Type_ChannelFraction.f90
	${MKDIR} -p ${OBJECTDIR}
	$(COMPILE.f) -O2 -o ${OBJECTDIR}/HMC_Module_Phys_Convolution_Type_ChannelFraction.o HMC_Module_Phys_Convolution_Type_ChannelFraction.f90

${OBJECTDIR}/HMC_Module_Phys_Convolution_Type_ChannelNetwork.o: HMC_Module_Phys_Convolution_Type_ChannelNetwork.f90
	${MKDIR} -p ${OBJECTDIR}
	$(COMPILE.f) -O2 -o ${OBJECTDIR}/HMC_Module_Phys_Convolution_Type_ChannelNetwork.o HMC_Module_Phys_Convolution_Type_ChannelNetwork.f90

${OBJECTDIR}/HMC_Module_Phys_ET.o: HMC_Module_Phys_ET.f90
	${MKDIR} -p ${OBJECTDIR}
	$(COMPILE.f) -O2 -o ${OBJECTDIR}/HMC_Module_Phys_ET.o HMC_Module_Phys_ET.f90

${OBJECTDIR}/HMC_Module_Phys_HydraulicStructure.o: HMC_Module_Phys_HydraulicStructure.f90
	${MKDIR} -p ${OBJECTDIR}
	$(COMPILE.f) -O2 -o ${OBJECTDIR}/HMC_Module_Phys_HydraulicStructure.o HMC_Module_Phys_HydraulicStructure.f90

${OBJECTDIR}/HMC_Module_Phys_LSM.o: HMC_Module_Phys_LSM.f90
	${MKDIR} -p ${OBJECTDIR}
	$(COMPILE.f) -O2 -o ${OBJECTDIR}/HMC_Module_Phys_LSM.o HMC_Module_Phys_LSM.f90

${OBJECTDIR}/HMC_Module_Phys_LSM_Apps.o: HMC_Module_Phys_LSM_Apps.f90
	${MKDIR} -p ${OBJECTDIR}
	$(COMPILE.f) -O2 -o ${OBJECTDIR}/HMC_Module_Phys_LSM_Apps.o HMC_Module_Phys_LSM_Apps.f90

${OBJECTDIR}/HMC_Module_Phys_Retention.o: HMC_Module_Phys_Retention.f90
	${MKDIR} -p ${OBJECTDIR}
	$(COMPILE.f) -O2 -o ${OBJECTDIR}/HMC_Module_Phys_Retention.o HMC_Module_Phys_Retention.f90

${OBJECTDIR}/HMC_Module_Phys_Snow.o: HMC_Module_Phys_Snow.f90
	${MKDIR} -p ${OBJECTDIR}
	$(COMPILE.f) -O2 -o ${OBJECTDIR}/HMC_Module_Phys_Snow.o HMC_Module_Phys_Snow.f90

${OBJECTDIR}/HMC_Module_Phys_Snow_Apps.o: HMC_Module_Phys_Snow_Apps.f90
	${MKDIR} -p ${OBJECTDIR}
	$(COMPILE.f) -O2 -o ${OBJECTDIR}/HMC_Module_Phys_Snow_Apps.o HMC_Module_Phys_Snow_Apps.f90

${OBJECTDIR}/HMC_Module_Phys_StateUpdating.o: HMC_Module_Phys_StateUpdating.f90
	${MKDIR} -p ${OBJECTDIR}
	$(COMPILE.f) -O2 -o ${OBJECTDIR}/HMC_Module_Phys_StateUpdating.o HMC_Module_Phys_StateUpdating.f90

${OBJECTDIR}/HMC_Module_Tools_Debug.o: HMC_Module_Tools_Debug.f90
	${MKDIR} -p ${OBJECTDIR}
	$(COMPILE.f) -O2 -o ${OBJECTDIR}/HMC_Module_Tools_Debug.o HMC_Module_Tools_Debug.f90

${OBJECTDIR}/HMC_Module_Tools_Generic.o: HMC_Module_Tools_Generic.f90
	${MKDIR} -p ${OBJECTDIR}
	$(COMPILE.f) -O2 -o ${OBJECTDIR}/HMC_Module_Tools_Generic.o HMC_Module_Tools_Generic.f90

${OBJECTDIR}/HMC_Module_Tools_IO.o: HMC_Module_Tools_IO.f90
	${MKDIR} -p ${OBJECTDIR}
	$(COMPILE.f) -O2 -o ${OBJECTDIR}/HMC_Module_Tools_IO.o HMC_Module_Tools_IO.f90

${OBJECTDIR}/HMC_Module_Tools_Interp.o: HMC_Module_Tools_Interp.f90
	${MKDIR} -p ${OBJECTDIR}
	$(COMPILE.f) -O2 -o ${OBJECTDIR}/HMC_Module_Tools_Interp.o HMC_Module_Tools_Interp.f90

${OBJECTDIR}/HMC_Module_Tools_Time.o: HMC_Module_Tools_Time.f90
	${MKDIR} -p ${OBJECTDIR}
	$(COMPILE.f) -O2 -o ${OBJECTDIR}/HMC_Module_Tools_Time.o HMC_Module_Tools_Time.f90

${OBJECTDIR}/HMC_Module_Vars_Loader.o: HMC_Module_Vars_Loader.f90
	${MKDIR} -p ${OBJECTDIR}
	$(COMPILE.f) -O2 -o ${OBJECTDIR}/HMC_Module_Vars_Loader.o HMC_Module_Vars_Loader.f90

${OBJECTDIR}/HMC_Module_Vars_Manager.o: HMC_Module_Vars_Manager.f90
	${MKDIR} -p ${OBJECTDIR}
	$(COMPILE.f) -O2 -o ${OBJECTDIR}/HMC_Module_Vars_Manager.o HMC_Module_Vars_Manager.f90

${OBJECTDIR}/debug_2dVar.o: debug_2dVar.f90
	${MKDIR} -p ${OBJECTDIR}
	$(COMPILE.f) -O2 -o ${OBJECTDIR}/debug_2dVar.o debug_2dVar.f90

${OBJECTDIR}/debug_2dVar.o: debug_2dVar.m
	${MKDIR} -p ${OBJECTDIR}
	${RM} "$@.d"
	$(COMPILE.c) -O2 -MMD -MP -MF "$@.d" -o ${OBJECTDIR}/debug_2dVar.o debug_2dVar.m

${OBJECTDIR}/gnufor2.o: gnufor2.f90
	${MKDIR} -p ${OBJECTDIR}
	$(COMPILE.f) -O2 -o ${OBJECTDIR}/gnufor2.o gnufor2.f90

# Subprojects
.build-subprojects:

# Clean Targets
.clean-conf: ${CLEAN_SUBPROJECTS}
	${RM} -r ${CND_BUILDDIR}/${CND_CONF}
	${RM} *.mod

# Subprojects
.clean-subprojects:

# Enable dependency checking
.dep.inc: .depcheck-impl

include .dep.inc

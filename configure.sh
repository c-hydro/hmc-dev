#!/bin/bash -e

#-----------------------------------------------------------------------------------------
# Script option(s)
Script="HMC Library Builder"
Version="1.6.0"
Date='2022/03/28'

# Other option(s)
Archive_Default="hmc_v316.tar.gz"
# Default folder
Lib_Dir_Deps_Default="$HOME/fp_libs_system"
# Executables folder
Lib_Dir_Exec_Default="/share/coordinator/fp-libs/libs_hmc"
# Compilation Mode
Lib_Building_Default=false
# Executable name
Exec_Default='HMC_Model_V3_$RUN.x'
#-----------------------------------------------------------------------------------------

#-----------------------------------------------------------------------------------------
# Start - Script
echo "----------------------------------------------------------------"
echo "$Script - Version $Version "
echo "Script to set, compile and build HMC model"
echo "----------------------------------------------------------------"
echo ""
#-----------------------------------------------------------------------------------------

#-----------------------------------------------------------------------------------------
# Parse script argument(s)
echo "----------------------------------------------------------------"
echo "Parse argument(s) ... "

# Get arguments number and values
Args_N=$#
Args_Values=$@

echo ""
echo " => Script arguments number: $Args_N"
echo " => Script arguments values: $Args_Values"
echo ""
echo " => Script arguments 1 - Archive Name [string: filename]-> $1"
echo " => Script arguments 2 - Directory of dependencies [string: path] -> $2"
echo " => Script arguments 3 - Directory of HMC executable [string: path] -> $3"
echo " => Script arguments 4 - Compilation Mode [boolean: {true, false}] -> $4"
echo ""

# Set argument(s)
if [ $# -eq 0 ]; then
	Archive=$Archive_Default
	Lib_Dir_Deps=$Lib_Dir_Deps_Default
	Lib_Dir_Exec=$Lib_Dir_Exec_Default
	Lib_Building_Automatic=$Lib_Building_Default
	echo " => Script arguments - SET [None] DEFAULT [1,2,3,4]"
elif [ $# -eq 1 ]; then
	Archive=$1
	Lib_Dir_Deps=$Lib_Dir_Deps_Default
	Lib_Dir_Exec=$Lib_Dir_Exec_Default
	Lib_Building_Automatic=$Lib_Building_Default
	echo " => Script arguments - SET [1] DEFAULT [2,3,4]"
elif [ $# -eq 2 ]; then
	Archive=$1
	Lib_Dir_Deps=$2
	Lib_Dir_Exec="$Lib_Dir_Deps/hmc"
	Lib_Building_Automatic=$Lib_Building_Default
	echo " => Script arguments - SET [1,2] DEFAULT [3,4]"
elif [ $# -eq 3 ]; then
	Archive=$1
	Lib_Dir_Deps=$2
	Lib_Dir_Exec=$3
	Lib_Building_Automatic=$Lib_Building_Default
	echo " => Script arguments - SET [1,2,3] DEFAULT [4]"
elif [ $# -eq 4 ]; then
	Archive=$1
	Lib_Dir_Deps=$2
	Lib_Dir_Exec=$3
	Lib_Building_Automatic=$4
	echo " => Script arguments - SET [1,2,3,4] DEFAULT [None]"
else
	echo " => Script arguments - FATAL ERROR"
	exit 1
fi

# Message about compilation mode
if $Lib_Building_Automatic ; then 
	echo " => Script compilation mode: AUTOMATIC"
else
	echo " => Script compilation mode: MANUAL"
fi

echo ""
echo "Parse Argument(s)  ... OK!"
echo "----------------------------------------------------------------"
echo ""
#-----------------------------------------------------------------------------------------

#-----------------------------------------------------------------------------------------
# Automatically checking for netcdf library
echo "----------------------------------------------------------------"
echo "Step 0 - Configure ==> Detection of NetCDF4 path ... "
echo ""

if (find $Lib_Dir_Deps -type d  -path '*nc4'); then
    NC_Dir_Default=$(find $Lib_Dir_Deps -type d  -path '*nc4')
    echo "NetCDF4 complete library path set using automatic detection [$NC_Dir_Default]"
else
    NC_Dir_Default="/$HOME/fp_libs_system/nc4/"
    echo "NetCDF4 complete library path set using a DEFAULT path [$NC_Dir_Default]"
fi

echo ""
echo "Step 0 - Configure ==> Detection of NetCDF4 ... OK!"
echo "----------------------------------------------------------------"
echo ""
#-----------------------------------------------------------------------------------------

#-----------------------------------------------------------------------------------------
# Menu to set compiler type
echo "----------------------------------------------------------------"
echo "Step 1 - Configure ==> Set compiler type ... "
echo ""
if $Lib_Building_Automatic ; then  
	echo " ==> GNU/GFortran Compiler selected in automatic mode"
	Comp_Name="GNU/GFortran"
	Comp_Exec="gfortran"
	Comp_Version=$(gfortran -dumpversion)
	
	if (( $Comp_Version > 7 )); then
		Comp_Obj="-c -g -O2 -cpp -DLIB_DYNARRAY "
		echo " ===> Compiler GFortran Version: " $Comp_Version " greather then version 7 "
		echo " ===> Building with string dynamic allocatable arrays "
	else
		Comp_Obj="-c -g -O2 -cpp "
		echo " ===> Compiler GFortran Version: " $Comp_Version " lower then version 7 "
		echo " ===> Building without string dynamic allocatable arrays "
	fi
else
	PS3=' ==> Please enter your choice: '
	Comp_Opts=( " GNU/GFortran  INTEL/Fortran  Quit" )
	select Opt in $Comp_Opts; do
		if [ "$Opt" = "Quit" ]; then
			echo Quit
			exit
		elif [ "$Opt" = "GNU/GFortran" ]; then
			Comp_Name="GNU/GFortran"
			Comp_Exec="gfortran"
			Comp_Version=$(gfortran -dumpversion)
			
			if (( $Comp_Version > 7 )); then
				Comp_Obj="-c -g -O2 -cpp -DLIB_DYNARRAY "
				echo " ===> Compiler GFortran Version: " $Comp_Version " greather then version 7 "
				echo " ===> Building with string dynamic allocatable arrays "
			else
				Comp_Obj="-c -g -O2 -cpp "
				echo " ===> Compiler GFortran Version: " $Comp_Version " lower then version 7 "
				echo " ===> Building without string dynamic allocatable arrays "
			fi
			
			break
		elif [ "$Opt" = "INTEL/Fortran" ]; then
			Comp_Name="INTEL/Fortran"
			Comp_Exec="ifort"
			Comp_Obj="-c -g -O2 -fpp "
			break
		else
			echo 'Bad Option!'
		fi
	done
fi

echo " ==> Compiler Name: " $Comp_Name "; Compiler Version: " $Comp_Version "; Compiler Exec: " $Comp_Exec "; Comp Obj: " $Comp_Obj
echo ""
echo "Step 1 - Configure ==> Set compiler type ... OK!"
echo "----------------------------------------------------------------"
echo ""
#-----------------------------------------------------------------------------------------

#-----------------------------------------------------------------------------------------
# Menu to set optimization option
echo "----------------------------------------------------------------"
echo "Step 2 - Configure ==> Set optimization option ... "
echo ""

if $Lib_Building_Automatic ; then  
	echo " ==> Production optimization for GNU/GFortran compiler selected in automatic mode"
	Optim_Opt="Production"
	Optim_Exec='-O3 -march=native -Ofast -funroll-loops -fimplicit-none  -Wall  -Wline-truncation  -fwhole-file  -std=f2008 -fall-intrinsics '
else
	PS3=' ==> Please enter your choice: '
	Optim_Opts=( " Debug  Production  Quit" )
	select Opt in $Optim_Opts; do
		if [ "$Opt" = "Quit" ]; then

			echo Quit
			exit

		elif [ "$Opt" = "Debug" ]; then
			Optim_Opt="Debug"
			
			if [ "$Comp_Name" = "GNU/GFortran" ]; then
				Optim_Exec='-O2 -g3 -ggdb -fimplicit-none  -Wall  -Wline-truncation  -Wcharacter-truncation  -Wsurprising  -Waliasing  -Wimplicit-interface  -Wunused-parameter  -fwhole-file  -fcheck=all  -std=f2008  -pedantic  -fbacktrace  -fall-intrinsics '
			elif [ "$Comp_Name" = "INTEL/Fortran" ]; then
				Optim_Exec='-O2 -static -static-intel -assume byterecl -align dcommons -fast '
			fi
			break

		elif [ "$Opt" = "Production" ]; then
			Optim_Opt="Production"

			if [ "$Comp_Name" = "GNU/GFortran" ]; then
				Optim_Exec='-O3 -march=native -Ofast -funroll-loops -fimplicit-none  -Wall  -Wline-truncation  -fwhole-file  -std=f2008 -fall-intrinsics '
			elif [ "$Comp_Name" = "INTEL/Fortran" ]; then
				Optim_Exec='-O2 -static -static-intel -assume byterecl -align dcommons -fast '
			fi
			break

		else
			echo 'Bad Option!'
		fi
	done
fi

echo " ==> Optimization Option: " $Optim_Opt "; Optimization Exec: " $Optim_Exec
echo ""
echo "Step 2 - Configure ==> Set optimization option ... OK!"
echo "----------------------------------------------------------------"
echo ""
#-----------------------------------------------------------------------------------------

#-----------------------------------------------------------------------------------------
# Menu to set profiler option
echo "----------------------------------------------------------------"
echo "Step 3 - Configure ==> Set profiler option ... "
echo ""

if $Lib_Building_Automatic ; then 
	echo " ==> Profiler option selected in automatic mode"
	Prof_Opt=""
else
	PS3=' ==> Please enter your choice: '
	Prof_Opts=( " Yes  No  Quit" )
	select Opt in $Prof_Opts; do
		if [ "$Opt" = "Quit" ]; then
			echo Quit
			exit
		elif [ "$Opt" = "Yes" ]; then
			Prof_Opt="-pg"
			break
		elif [ "$Opt" = "No" ]; then
			Prof_Opt=""
			break
		else
			echo 'Bad Option!'
		fi
	done
fi

echo " ==> Profiler Option: " $Prof_Opt
echo "" 
echo "Step 3 - Configure ==> Set profiler option ... OK!"
echo "----------------------------------------------------------------"
echo ""
#-----------------------------------------------------------------------------------------

#-----------------------------------------------------------------------------------------
# Menu to set netCDF4 library
echo "----------------------------------------------------------------"
echo "Step 4 - Configure ==> Set NetCDF4 library ... "
echo ""

if $Lib_Building_Automatic ; then 
	echo " ==> NetCDF4 library for GNU/GFortran compiler selected in automatic mode"

	NC_Dir=$NC_Dir_Default

	NC_Inc=$NC_Dir/include \
	NC_Lib=$NC_Dir/lib \
	NC_Libs="-lnetcdff -lnetcdf"
	NC_Opt="-DLIB_NC"

	Comp_Obj=$Comp_Obj$NC_Opt 
else
	PS3=' ==> Please enter your choice: '
	NC_Opts=( " Yes  No  Quit" )
	select Opt in $NC_Opts; do
		if [ "$Opt" = "Quit" ]; then
			echo Quit
			exit
		elif [ "$Opt" = "Yes" ]; then
			read -r -p ' ==> Please enter NetCDF4 complete library path: ' NC_Dir
			
			if [ -z "$NC_Dir" ]; then
				NC_Dir=$NC_Dir_Default
	  			echo " ==> NetCDF4 complete library path set using DEFAULT path!"
			else
				echo " ==> NetCDF4 complete library path set by USER"
			fi

			NC_Inc=$NC_Dir/include \
			NC_Lib=$NC_Dir/lib \
			NC_Libs="-lnetcdff -lnetcdf"
			NC_Opt="-DLIB_NC"

			Comp_Obj=$Comp_Obj$NC_Opt 	

			break
		elif [ "$Opt" = "No" ]; then
			NC_Dir=''
			NC_Inc=""
			NC_Lib=""
			NC_Libs=""
			NC_Opt=""

			Comp_Obj=$Comp_Obj$NC_Opt 

			break
		else
			echo 'Bad Option!'
		fi
	done
fi

echo " ==> NetCDF4 path: " $NC_Dir "; NetCDF4 Option: " $NC_Opt "; NetCDF4 Comp Obj: " $Comp_Obj
echo ""
echo "Step 4 - Configure ==> Set NetCDF4 library ... OK!"
echo "----------------------------------------------------------------"
echo ""
#-----------------------------------------------------------------------------------------

#-----------------------------------------------------------------------------------------
# Set executable HMC name
echo "----------------------------------------------------------------"
echo "Step 5 - Configure ==> Set HMC executable name ... "
echo ""

if $Lib_Building_Automatic ; then 
	echo " ==> HMC executable name selected in automatic mode"
	Exec=$Exec_Default
else
	read -r -p ' ==> Please enter HMC executable name: ' Exec
	if [ -z "$Exec" ]; then
		Exec=$Exec_Default
	  	echo " ==> HMC executable name [Default]: " $Exec 
	else
		echo " ==> HMC executable name [User]: " $Exec 
	fi
fi

echo ""
echo "Step 5 - Configure ==> Set HMC executable name ... OK"
echo "----------------------------------------------------------------"
echo ""
#-----------------------------------------------------------------------------------------

#-----------------------------------------------------------------------------------------
# Set stack size (kbytes, -s) unlimited
echo "----------------------------------------------------------------"
echo "Step 1 - Compile ==> Set stack size to unlimited ... "
echo ""

ulimit -s unlimited
ulimit -a

echo ""
echo "Step 1 - Compile ==> Set stack size to unlimited ... OK!"
echo "----------------------------------------------------------------"
echo ""
#-----------------------------------------------------------------------------------------

#-----------------------------------------------------------------------------------------
# Expand archive file
echo "----------------------------------------------------------------"
echo "Step 2 - Compile ==> Expand archive file ... "
echo ""

# Get current folder
Current_Dir=${PWD}

if [ -e $Archive ]; then

    echo " ==> tar xvfz $Archive -C $Archive_Dir"
    tar xvfz $Archive -C $Archive_Dir --strip-components=1
    
    echo " ==> Files in compressed format -- Unzipped!"
    Archive_Dir=$Current_Dir/temp
    
    if [ -d "$Archive_Dir" ]; then
        rm -R $Archive_Dir
    fi
    
    mkdir $Archive_Dir
    
else
    echo " ==> Files in uncompressed format -- Skipped!"
    Archive_Dir=$Current_Dir
fi

echo "" 
echo "Step 2 - Compile ==> Expand archive file ... DONE!"
echo "----------------------------------------------------------------"
echo ""
#-----------------------------------------------------------------------------------------

#-----------------------------------------------------------------------------------------
# Build module(s) and subroutine(s)
echo "----------------------------------------------------------------"
echo "Step 3 - Compile ==> Build module(s) and subroutine(s) ... "
echo ""

cd $Archive_Dir

$Comp_Exec $Comp_Obj gnufor2.f90 $Prof_Opt
$Comp_Exec $Comp_Obj HMC_Module_Tools_Debug.f90 $Prof_Opt
$Comp_Exec $Comp_Obj HMC_Module_Args.f90 $Prof_Opt
$Comp_Exec $Comp_Obj HMC_Module_Namelist.f90 $Prof_Opt
$Comp_Exec $Comp_Obj HMC_Module_Tools_Interp.f90 $Prof_Opt
$Comp_Exec $Comp_Obj HMC_Module_Tools_Generic.f90 $Prof_Opt
$Comp_Exec $Comp_Obj HMC_Module_Tools_IO.f90 $Prof_Opt -I${NC_Inc} -L${NC_Lib} ${NC_Libs} 
$Comp_Exec $Comp_Obj HMC_Module_Tools_Time.f90 $Prof_Opt
$Comp_Exec $Comp_Obj HMC_Module_Vars_Loader.f90 $Prof_Opt
$Comp_Exec $Comp_Obj HMC_Module_Vars_Manager.f90 $Prof_Opt
$Comp_Exec $Comp_Obj HMC_Module_Phys_HydraulicStructure.f90 $Prof_Opt
$Comp_Exec $Comp_Obj HMC_Module_Phys_Convolution_Apps_Flooding.f90 $Prof_Opt
$Comp_Exec $Comp_Obj HMC_Module_Phys_Convolution_Apps_DeepFlow.f90 $Prof_Opt
$Comp_Exec $Comp_Obj HMC_Module_Phys_Convolution_Apps_Discharge.f90 $Prof_Opt
$Comp_Exec $Comp_Obj HMC_Module_Phys_Convolution_Apps_Horton.f90 $Prof_Opt
$Comp_Exec $Comp_Obj HMC_Module_Phys_Convolution_Apps_HydraulicStructure.f90 $Prof_Opt
$Comp_Exec $Comp_Obj HMC_Module_Phys_Convolution_Apps_IntegrationStep.f90 $Prof_Opt
$Comp_Exec $Comp_Obj HMC_Module_Phys_Convolution_Apps_SubFlow.f90 $Prof_Opt
$Comp_Exec $Comp_Obj HMC_Module_Phys_Convolution_Apps_SurfaceFlow.f90 $Prof_Opt
$Comp_Exec $Comp_Obj HMC_Module_Phys_Convolution_Type_ChannelFraction.f90 $Prof_Opt
$Comp_Exec $Comp_Obj HMC_Module_Phys_Convolution_Type_ChannelNetwork.f90 $Prof_Opt
$Comp_Exec $Comp_Obj HMC_Module_Phys_LSM_Apps.f90 $Prof_Opt
$Comp_Exec $Comp_Obj HMC_Module_Phys_LSM.f90 $Prof_Opt
$Comp_Exec $Comp_Obj HMC_Module_Phys_Snow_Apps.f90 $Prof_Opt
$Comp_Exec $Comp_Obj HMC_Module_Phys_Snow.f90 $Prof_Opt
$Comp_Exec $Comp_Obj HMC_Module_Phys_ET.f90 $Prof_Opt
$Comp_Exec $Comp_Obj HMC_Module_Phys_Retention.f90 $Prof_Opt
$Comp_Exec $Comp_Obj HMC_Module_Phys_StateUpdating.f90 $Prof_Opt
$Comp_Exec $Comp_Obj HMC_Module_Phys.f90 $Prof_Opt
$Comp_Exec $Comp_Obj HMC_Module_Data_Forcing_Gridded.f90 $Prof_Opt -I${NC_Inc} -L${NC_Lib} ${NC_Libs}
$Comp_Exec $Comp_Obj HMC_Module_Data_Forcing_Point.f90 $Prof_Opt -I${NC_Inc} -L${NC_Lib} ${NC_Libs}
$Comp_Exec $Comp_Obj HMC_Module_Data_Forcing_TimeSeries.f90 $Prof_Opt -I${NC_Inc} -L${NC_Lib} ${NC_Libs}
$Comp_Exec $Comp_Obj HMC_Module_Data_Updating_Gridded.f90 $Prof_Opt -I${NC_Inc} -L${NC_Lib} ${NC_Libs}
$Comp_Exec $Comp_Obj HMC_Module_Data_Output_Gridded.f90 $Prof_Opt -I${NC_Inc} -L${NC_Lib} ${NC_Libs}
$Comp_Exec $Comp_Obj HMC_Module_Data_Output_Point.f90 $Prof_Opt -I${NC_Inc} -L${NC_Lib} ${NC_Libs}
$Comp_Exec $Comp_Obj HMC_Module_Data_Output_TimeSeries.f90 $Prof_Opt -I${NC_Inc} -L${NC_Lib} ${NC_Libs}
$Comp_Exec $Comp_Obj HMC_Module_Data_Restart_Gridded.f90 $Prof_Opt -I${NC_Inc} -L${NC_Lib} ${NC_Libs}
$Comp_Exec $Comp_Obj HMC_Module_Data_Restart_Point.f90 $Prof_Opt -I${NC_Inc} -L${NC_Lib} ${NC_Libs}
$Comp_Exec $Comp_Obj HMC_Module_Data_State_Gridded.f90 $Prof_Opt -I${NC_Inc} -L${NC_Lib} ${NC_Libs}
$Comp_Exec $Comp_Obj HMC_Module_Data_State_Point.f90 $Prof_Opt -I${NC_Inc} -L${NC_Lib} ${NC_Libs}
$Comp_Exec $Comp_Obj HMC_Module_Data_Static_Gridded.f90 $Prof_Opt -I${NC_Inc} -L${NC_Lib} ${NC_Libs}
$Comp_Exec $Comp_Obj HMC_Module_Data_Static_Point.f90 $Prof_Opt -I${NC_Inc} -L${NC_Lib} ${NC_Libs}
$Comp_Exec $Comp_Obj HMC_Module_Info_Gridded.f90 $Prof_Opt -I${NC_Inc} -L${NC_Lib} ${NC_Libs}
$Comp_Exec $Comp_Obj HMC_Module_Info_Point.f90 $Prof_Opt -I${NC_Inc} -L${NC_Lib} ${NC_Libs}
$Comp_Exec $Comp_Obj HMC_Module_Info_Time.f90 $Prof_Opt -I${NC_Inc} -L${NC_Lib} ${NC_Libs}

echo ""
echo "Step 3 - Compile ==> Build module(s) and subroutine(s) ... DONE!"
echo "----------------------------------------------------------------"
echo ""
#-----------------------------------------------------------------------------------------

#-----------------------------------------------------------------------------------------
# Link object(s) and create executable(s)
echo "----------------------------------------------------------------"
echo "Step 4 - Compile ==> Link object(s) and create HMC model executable ... "
echo ""

$Comp_Exec $Optim_Exec gnufor2.o HMC_Module_Data_Forcing_Gridded.o HMC_Module_Data_Forcing_Point.o HMC_Module_Data_Forcing_TimeSeries.o HMC_Module_Data_Updating_Gridded.o HMC_Module_Data_Output_Gridded.o HMC_Module_Data_Output_Point.o HMC_Module_Data_Output_TimeSeries.o HMC_Module_Data_Restart_Gridded.o HMC_Module_Data_Restart_Point.o HMC_Module_Data_State_Gridded.o HMC_Module_Data_State_Point.o HMC_Module_Data_Static_Gridded.o HMC_Module_Data_Static_Point.o HMC_Module_Info_Gridded.o HMC_Module_Info_Point.o HMC_Module_Info_Time.o HMC_Module_Args.o HMC_Module_Namelist.o HMC_Module_Phys_Convolution_Type_ChannelNetwork.o HMC_Module_Phys_Convolution_Type_ChannelFraction.o HMC_Module_Phys_Convolution_Apps_SurfaceFlow.o HMC_Module_Phys_Convolution_Apps_SubFlow.o HMC_Module_Phys_Convolution_Apps_IntegrationStep.o HMC_Module_Phys_Convolution_Apps_HydraulicStructure.o HMC_Module_Phys_Convolution_Apps_Horton.o HMC_Module_Phys_Convolution_Apps_Discharge.o HMC_Module_Phys_Convolution_Apps_DeepFlow.o HMC_Module_Phys_Convolution_Apps_Flooding.o HMC_Module_Phys_HydraulicStructure.o HMC_Module_Phys_ET.o HMC_Module_Phys.o HMC_Module_Phys_LSM_Apps.o HMC_Module_Phys_LSM.o HMC_Module_Phys_Snow_Apps.o HMC_Module_Phys_Snow.o HMC_Module_Phys_Retention.o HMC_Module_Phys_StateUpdating.o HMC_Module_Tools_Debug.o HMC_Module_Tools_Generic.o HMC_Module_Tools_Interp.o HMC_Module_Tools_IO.o HMC_Module_Tools_Time.o HMC_Module_Vars_Loader.o HMC_Module_Vars_Manager.o HMC_Main.f90 -o $Exec $Prof_Opt -I${NC_Inc} -L${NC_Lib} ${NC_Libs}

echo ""
echo "Step 4 - Compile ==> Link object(s) and create HMC model executable ... DONE!"
echo "----------------------------------------------------------------"
echo ""
#-----------------------------------------------------------------------------------------

#-----------------------------------------------------------------------------------------
# Change option(s) if HMC executable
echo "----------------------------------------------------------------"
echo "Step 5 - Compile ==> Change option(s) of HMC model executable ... "
echo ""

if [ -d "$Lib_Dir_Exec" ]; then
    rm -R $Lib_Dir_Exec
fi
mkdir -p $Lib_Dir_Exec

if [ -e $Exec ]; then
    chmod +x $Exec
    cp -r $Archive_Dir/$Exec $Lib_Dir_Exec/$Exec
    echo " ==> $Exec copied in library folder ... DONE!"
else
    echo " ==> $Exec copied in library folder ... FAILED!"
fi

echo ""
echo "Step 5 - Compile ==> Change option(s) of HMC model executable ... DONE!"
echo "----------------------------------------------------------------"
echo ""
#-----------------------------------------------------------------------------------------

#-----------------------------------------------------------------------------------------
# Get extra tool(s) for HMC model testing
echo "----------------------------------------------------------------"
echo "Step 6 - Compile ==> Get extra tool(s) for HMC model testing ... "
echo ""

file_tool="gprof2dot.py"
if [ -e $file_tool ]; then
    chmod +x $file_tool
    cp -r $Archive_Dir/$file_tool $Lib_Dir_Exec/$file_tool
    echo " ==> $file_tool copied in library folder ... DONE!"
else
    echo " ==> $file_tool copied in library folder ... FILE NOT FOUND!"
fi

file_tool="HMC_Launcher_Debug_Test.sh"
if [ -e $file_tool ]; then
    chmod +x $file_tool
    cp -r $Archive_Dir/$file_tool $Lib_Dir_Exec/$file_tool
    echo " ==> $file_tool copied in library folder ... DONE!"
else
    echo " ==> $file_tool copied in library folder ... FILE NOT FOUND!"
fi

file_tool="HMC_Launcher_Test_Profiling.sh"
if [ -e $file_tool ]; then
    chmod +x $file_tool
    cp -r $Archive_Dir/$file_tool $Lib_Dir_Exec/$file_tool
    echo " ==> $file_tool copied in library folder ... DONE!"
else
    echo " ==> $file_tool copied in library folder ... FILE NOT FOUND!"
fi

file_tool="HMC_Launcher_Test_Memory.sh"
if [ -e $file_tool ]; then
    chmod +x $file_tool
    cp -r $Archive_Dir/$file_tool $Lib_Dir_Exec/$file_tool
    echo " ==> $file_tool copied in library folder ... DONE!"
else
    echo " ==> $file_tool copied in library folder ... FILE NOT FOUND!"
fi

file_tool="HMC_Launcher_Debug_Live.sh"
if [ -e $file_tool ]; then
    chmod +x $file_tool
    cp $Archive_Dir/$file_tool $Lib_Dir_Exec/$file_tool
    echo " ==> $file_tool copied in library folder ... DONE!"
else
    echo " ==> $file_tool copied in library folder ... FILE NOT FOUND!"
fi

file_tool="HMC_Launcher_Test_Massif.sh"
if [ -e $file_tool ]; then
    chmod +x $file_tool
    cp -r $Archive_Dir/$file_tool $Lib_Dir_Exec/$file_tool
    echo " ==> $file_tool copied in library folder ... DONE!"
else
    echo " ==> $file_tool copied in library folder ... FILE NOT FOUND!"
fi

file_tool="HMC_Launcher_Test_Execution.sh"
if [ -e $file_tool ]; then
    chmod +x $file_tool
    cp -r $Archive_Dir/$file_tool $Lib_Dir_Exec/$file_tool
    echo " ==> $file_tool copied in library folder ... DONE!"
else
    echo " ==> $file_tool copied in library folder ... FILE NOT FOUND!"
fi

echo ""
echo "Step 6 - Compile ==> Get extra tool(s) for HMC model testing ... DONE!"
echo "----------------------------------------------------------------"
echo ""
#-----------------------------------------------------------------------------------------

#-----------------------------------------------------------------------------------------
# End - Script
echo "----------------------------------------------------------------"
echo "$Script - Version $Version "
echo "Script to set, compile and build HMC model"
echo "COMPLETED - Bye, Bye"
echo "----------------------------------------------------------------"
echo ""
#-----------------------------------------------------------------------------------------








#!/bin/bash -e

#-----------------------------------------------------------------------------------------
# Script option(s)
Script="HMC Model Builder"
Version="1.0.5"
# Other option(s)
Archive="hmc.tar.gz"
Exec_Default='HMC_Model_V2_$RUN.x'
# Default folder
Lib_Dir_Default="$HOME/fp_libs_system/"
# Executables folder
Lib_Dir_Exec="$HOME/fp_libs_system/hmc"
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
# Autotically checking for netcdf library
echo "----------------------------------------------------------------"
echo "Step 0 - Configure ==> Detection of NetCDF4 path ... "
echo ""

if (find $Lib_Dir_Default -type d  -path '*nc4'); then
    NC_Dir_Default=$(find $Lib_Dir_Default -type d  -path '*nc4')
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

PS3=' ==> Please enter your choice: '
Comp_Opts=( " GNU/GFortran  INTEL/Fortran  Quit" )
select Opt in $Comp_Opts; do
	if [ "$Opt" = "Quit" ]; then
		echo Quit
		exit
	elif [ "$Opt" = "GNU/GFortran" ]; then
		Comp_Name="GNU/GFortran"
		Comp_Exec="gfortran"
		Comp_Obj="-c -g -O2 -cpp "
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

echo " ==> Compiler Name: " $Comp_Name "; Compiler Exec: " $Comp_Exec "; Comp Obj: " $Comp_Obj
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

read -r -p ' ==> Please enter HMC executable name: ' Exec
if [ -z "$Exec" ]; then
	Exec=$Exec_Default
  	echo " ==> HMC executable name [Default]: " $Exec 
else
	echo " ==> HMC executable name [User]: " $Exec 
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
$Comp_Exec $Comp_Obj HMC_Module_Phys_Convolution_Apps.f90 $Prof_Opt
$Comp_Exec $Comp_Obj HMC_Module_Phys_Convolution.f90 $Prof_Opt
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

$Comp_Exec $Optim_Exec gnufor2.o HMC_Module_Data_Forcing_Gridded.o HMC_Module_Data_Forcing_Point.o HMC_Module_Data_Forcing_TimeSeries.o HMC_Module_Data_Updating_Gridded.o HMC_Module_Data_Output_Gridded.o HMC_Module_Data_Output_Point.o HMC_Module_Data_Output_TimeSeries.o HMC_Module_Data_Restart_Gridded.o HMC_Module_Data_Restart_Point.o HMC_Module_Data_State_Gridded.o HMC_Module_Data_State_Point.o HMC_Module_Data_Static_Gridded.o HMC_Module_Data_Static_Point.o HMC_Module_Info_Gridded.o HMC_Module_Info_Point.o HMC_Module_Info_Time.o HMC_Module_Args.o HMC_Module_Namelist.o HMC_Module_Phys_Convolution_Apps.o HMC_Module_Phys_Convolution.o HMC_Module_Phys_HydraulicStructure.o HMC_Module_Phys_ET.o HMC_Module_Phys.o HMC_Module_Phys_LSM_Apps.o HMC_Module_Phys_LSM.o HMC_Module_Phys_Snow_Apps.o HMC_Module_Phys_Snow.o HMC_Module_Phys_Retention.o HMC_Module_Phys_StateUpdating.o HMC_Module_Tools_Debug.o HMC_Module_Tools_Generic.o HMC_Module_Tools_Interp.o HMC_Module_Tools_IO.o HMC_Module_Tools_Time.o HMC_Module_Vars_Loader.o HMC_Module_Vars_Manager.o HMC_Main.f90 -o $Exec $Prof_Opt -I${NC_Inc} -L${NC_Lib} ${NC_Libs}

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

file_tool="HMC_Main_ExecTest.sh"
if [ -e $file_tool ]; then
    chmod +x $file_tool
    cp -r $Archive_Dir/$file_tool $Lib_Dir_Exec/$file_tool
    echo " ==> $file_tool copied in library folder ... DONE!"
else
    echo " ==> $file_tool copied in library folder ... FILE NOT FOUND!"
fi

file_tool="HMC_Main_ProfTest.sh"
if [ -e $file_tool ]; then
    chmod +x $file_tool
    cp -r $Archive_Dir/$file_tool $Lib_Dir_Exec/$file_tool
    echo " ==> $file_tool copied in library folder ... DONE!"
else
    echo " ==> $file_tool copied in library folder ... FILE NOT FOUND!"
fi

file_tool="HMC_Main_MemoryTest.sh"
if [ -e $file_tool ]; then
    chmod +x $file_tool
    cp -r $Archive_Dir/$file_tool $Lib_Dir_Exec/$file_tool
    echo " ==> $file_tool copied in library folder ... DONE!"
else
    echo " ==> $file_tool copied in library folder ... FILE NOT FOUND!"
fi

file_tool="HMC_Main_DebugTest.sh"
if [ -e $file_tool ]; then
    chmod +x $file_tool
    cp $Archive_Dir/$file_tool $Lib_Dir_Exec/$file_tool
    echo " ==> $file_tool copied in library folder ... DONE!"
else
    echo " ==> $file_tool copied in library folder ... FILE NOT FOUND!"
fi

file_tool="HMC_Args_DebugTest.txt"
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








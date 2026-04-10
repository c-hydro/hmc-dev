#!/usr/bin/env bash
# -----------------------------------------------------------------------------
# HMC Library Builder
# Version: 3.4.2 (robust shell rewrite)
# Date: 2026/04/09
#
# PURPOSE
#   Configure, compile, and install the HMC model executable, with optional
#   profiling support through gprof (-pg).
#
# EXAMPLES
#   1) Automatic build with defaults
#      ./configure.sh
#
#   2) Automatic build from archive using explicit paths
#      ./configure.sh hmc_v342.tar.gz "$HOME/fp_libs_system" "$HOME/fp_libs_system/hmc" true
#
#   3) Automatic profiling build using environment variable
#      RUN=Profile ./configure.sh hmc_v342.tar.gz "$HOME/fp_libs_system" "$HOME/fp_libs_system/hmc" true
#
#   4) Automatic profiling build using command-line option
#      ./configure.sh --run Profile hmc_v342.tar.gz "$HOME/fp_libs_system" "$HOME/fp_libs_system/hmc" true
#
#   5) Automatic profiling build with explicit PROFILE flag
#      RUN=Profile PROFILE=true ./configure.sh
#
#   6) Custom executable name override
#      RUN=Profile EXEC_NAME="HMC_Model_V3_MyProfile.x" ./configure.sh
#
#   7) Manual mode
#      ./configure.sh hmc_v340.tar.gz "$HOME/fp_libs_system" "$HOME/fp_libs_system/hmc" false
#
# OPTIONAL ENVIRONMENT VARIABLES
#   RUN=tag                     Build tag used in executable naming
#                               Default: Exec
#                               Special value: Profile
#   PROFILE=true|false          Enable/disable gprof flags (-pg)
#   EXEC_NAME=filename          Override executable name completely
#   NETCDF_DIR=/path/to/nc4     Force NetCDF root directory
#   KEEP_BUILD_DIR=true|false   Keep temporary extracted build dir
#
# RUN TAG LOGIC
#   If RUN is not provided, RUN=Exec is used.
#   If RUN=Profile, the script applies profiling-oriented defaults:
#     - executable name becomes HMC_Model_V3_Profile.x
#     - PROFILE defaults to true unless explicitly set
#
# ARGUMENTS
#   Positional:
#     $1 Archive name or source directory
#     $2 Dependencies directory
#     $3 Executable install directory
#     $4 Compilation mode: true=automatic, false=manual
#
#   Optional flags:
#     --run TAG
# -----------------------------------------------------------------------------

set -Eeuo pipefail

# -----------------------------------------------------------------------------
# Script option(s)
Script="HMC Library Builder"
Version="3.4.2"
Date='2026/04/09'

# Defaults
Archive_Default="hmc_v342.tar.gz"
Lib_Dir_Deps_Default="$HOME/fp_libs_system"
Lib_Dir_Exec_Default="$Lib_Dir_Deps_Default/hmc"
Lib_Building_Default=false
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# Logging and helpers
log()  { printf '[INFO] %s\n' "$*"; }
warn() { printf '[WARN] %s\n' "$*" >&2; }
err()  { printf '[ERROR] %s\n' "$*" >&2; }
die()  { err "$*"; exit 1; }

on_error() {
    local exit_code=$?
    err "Script failed at line ${BASH_LINENO[0]} with exit code ${exit_code}"
    exit "${exit_code}"
}
trap on_error ERR

require_cmd() {
    command -v "$1" >/dev/null 2>&1 || die "Required command not found: $1"
}

bool_is_true() {
    [[ "${1:-false}" == "true" ]]
}

to_lower() {
    printf '%s' "$1" | tr '[:upper:]' '[:lower:]'
}

cleanup() {
    if [[ -n "${Build_Dir:-}" && -d "${Build_Dir:-}" ]]; then
        if ! bool_is_true "$KEEP_BUILD_DIR"; then
            rm -rf "$Build_Dir"
        else
            log "Keeping build directory: $Build_Dir"
        fi
    fi
}
trap cleanup EXIT
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# Parse optional flags first
RUN_FROM_FLAG=""

POSITIONAL_ARGS=()
while [[ $# -gt 0 ]]; do
    case "$1" in
        --run)
            [[ $# -ge 2 ]] || die "Missing value for --run"
            RUN_FROM_FLAG="$2"
            shift 2
            ;;
        --run=*)
            RUN_FROM_FLAG="${1#*=}"
            shift
            ;;
        --help|-h)
            sed -n '1,70p' "$0"
            exit 0
            ;;
        *)
            POSITIONAL_ARGS+=("$1")
            shift
            ;;
    esac
done

set -- "${POSITIONAL_ARGS[@]}"
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# RUN / PROFILE defaults and special handling
RUN="${RUN:-}"
if [[ -n "$RUN_FROM_FLAG" ]]; then
    RUN="$RUN_FROM_FLAG"
fi
RUN="${RUN:-Exec}"

RUN_LOWER="$(to_lower "$RUN")"

PROFILE="${PROFILE:-}"
if [[ -z "$PROFILE" && "$RUN_LOWER" == "profile" ]]; then
    PROFILE="true"
fi
PROFILE="${PROFILE:-false}"

KEEP_BUILD_DIR="${KEEP_BUILD_DIR:-false}"

Exec_Default="HMC_Model_V3_${RUN}.x"
EXEC_NAME="${EXEC_NAME:-$Exec_Default}"
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# Start - Script
echo "----------------------------------------------------------------"
echo "$Script - Version $Version"
echo "Script to set, compile and build HMC model"
echo "----------------------------------------------------------------"
echo ""

# Parse script argument(s)
echo "----------------------------------------------------------------"
echo "Parse argument(s) ... "

Args_N=$#
Args_Values="$*"

echo ""
echo " => Script arguments number: $Args_N"
echo " => Script arguments values: $Args_Values"
echo " => RUN tag (resolved): $RUN"
echo " => PROFILE flag: $PROFILE"
echo ""
echo " => Script arguments 1 - Archive/Source [string: filename|path] -> ${1:-}"
echo " => Script arguments 2 - Directory of dependencies [string: path] -> ${2:-}"
echo " => Script arguments 3 - Directory of HMC executable [string: path] -> ${3:-}"
echo " => Script arguments 4 - Compilation Mode [boolean: {true, false}] -> ${4:-}"
echo ""

case "$#" in
    0)
        Archive="$Archive_Default"
        Lib_Dir_Deps="$Lib_Dir_Deps_Default"
        Lib_Dir_Exec="$Lib_Dir_Exec_Default"
        Lib_Building_Automatic="$Lib_Building_Default"
        echo " => Script arguments - SET [None] DEFAULT [1,2,3,4]"
        ;;
    1)
        Archive="$1"
        Lib_Dir_Deps="$Lib_Dir_Deps_Default"
        Lib_Dir_Exec="$Lib_Dir_Exec_Default"
        Lib_Building_Automatic="$Lib_Building_Default"
        echo " => Script arguments - SET [1] DEFAULT [2,3,4]"
        ;;
    2)
        Archive="$1"
        Lib_Dir_Deps="$2"
        Lib_Dir_Exec="$Lib_Dir_Deps/hmc"
        Lib_Building_Automatic="$Lib_Building_Default"
        echo " => Script arguments - SET [1,2] DEFAULT [3,4]"
        ;;
    3)
        Archive="$1"
        Lib_Dir_Deps="$2"
        Lib_Dir_Exec="$3"
        Lib_Building_Automatic="$Lib_Building_Default"
        echo " => Script arguments - SET [1,2,3] DEFAULT [4]"
        ;;
    4)
        Archive="$1"
        Lib_Dir_Deps="$2"
        Lib_Dir_Exec="$3"
        Lib_Building_Automatic="$4"
        echo " => Script arguments - SET [1,2,3,4] DEFAULT [None]"
        ;;
    *)
        die "Too many positional arguments"
        ;;
esac

[[ "$Lib_Building_Automatic" == "true" || "$Lib_Building_Automatic" == "false" ]] || \
    die "Compilation mode must be 'true' or 'false'"

if bool_is_true "$Lib_Building_Automatic"; then
    echo " => Script compilation mode: AUTOMATIC"
else
    echo " => Script compilation mode: MANUAL"
fi

if [[ "$RUN_LOWER" == "profile" ]]; then
    echo " => RUN special mode detected: PROFILE"
fi

echo ""
echo "Parse Argument(s) ... OK!"
echo "----------------------------------------------------------------"
echo ""
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# Requirements
require_cmd mkdir
require_cmd rm
require_cmd cp
require_cmd chmod
require_cmd find
require_cmd tar
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# Automatically checking for netcdf library
echo "----------------------------------------------------------------"
echo "Step 0 - Configure ==> Detection of NetCDF4 path ... "
echo ""

if [[ -n "${NETCDF_DIR:-}" ]]; then
    NC_Dir_Default="$NETCDF_DIR"
    echo "NetCDF4 complete library path forced by environment [$NC_Dir_Default]"
else
    NC_Dir_Default="$(find "$Lib_Dir_Deps" -type d -path '*nc4*' 2>/dev/null | head -n 1 || true)"
    if [[ -n "$NC_Dir_Default" ]]; then
        echo "NetCDF4 complete library path set using automatic detection [$NC_Dir_Default]"
    else
        NC_Dir_Default="$HOME/fp_libs_system/nc4"
        echo "NetCDF4 complete library path set using a DEFAULT path [$NC_Dir_Default]"
    fi
fi

echo ""
echo "Step 0 - Configure ==> Detection of NetCDF4 ... OK!"
echo "----------------------------------------------------------------"
echo ""
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# Menu to set compiler type
echo "----------------------------------------------------------------"
echo "Step 1 - Configure ==> Set compiler type ... "
echo ""

Comp_Name=""
Comp_Exec=""
Comp_Version=""
Comp_Version_Major=""
Comp_Obj=""

if bool_is_true "$Lib_Building_Automatic"; then
    echo " ==> GNU/GFortran Compiler selected in automatic mode"
    require_cmd gfortran
    Comp_Name="GNU/GFortran"
    Comp_Exec="gfortran"
    Comp_Version="$(gfortran -dumpfullversion 2>/dev/null || gfortran -dumpversion)"
    Comp_Version_Major="${Comp_Version%%.*}"

    if [[ "$Comp_Version_Major" =~ ^[0-9]+$ ]] && (( Comp_Version_Major > 7 )); then
        Comp_Obj="-c -g -O2 -cpp -DLIB_DYNARRAY"
        echo " ===> Compiler GFortran Version: $Comp_Version greater than version 7"
        echo " ===> Building with string dynamic allocatable arrays"
    else
        Comp_Obj="-c -g -O2 -cpp"
        echo " ===> Compiler GFortran Version: $Comp_Version lower/equal than version 7"
        echo " ===> Building without string dynamic allocatable arrays"
    fi
else
    PS3=' ==> Please enter your choice: '
    select Opt in "GNU/GFortran" "INTEL/Fortran" "Quit"; do
        case "$Opt" in
            "GNU/GFortran")
                require_cmd gfortran
                Comp_Name="GNU/GFortran"
                Comp_Exec="gfortran"
                Comp_Version="$(gfortran -dumpfullversion 2>/dev/null || gfortran -dumpversion)"
                Comp_Version_Major="${Comp_Version%%.*}"

                if [[ "$Comp_Version_Major" =~ ^[0-9]+$ ]] && (( Comp_Version_Major > 7 )); then
                    Comp_Obj="-c -g -O2 -cpp -DLIB_DYNARRAY"
                    echo " ===> Compiler GFortran Version: $Comp_Version greater than version 7"
                    echo " ===> Building with string dynamic allocatable arrays"
                else
                    Comp_Obj="-c -g -O2 -cpp"
                    echo " ===> Compiler GFortran Version: $Comp_Version lower/equal than version 7"
                    echo " ===> Building without string dynamic allocatable arrays"
                fi
                break
                ;;
            "INTEL/Fortran")
                require_cmd ifort
                Comp_Name="INTEL/Fortran"
                Comp_Exec="ifort"
                Comp_Version="$(ifort --version 2>/dev/null | head -n 1 || true)"
                Comp_Obj="-c -g -O2 -fpp"
                break
                ;;
            "Quit")
                exit 0
                ;;
            *)
                echo "Bad Option!"
                ;;
        esac
    done
fi

echo " ==> Compiler Name: $Comp_Name; Compiler Version: $Comp_Version; Compiler Exec: $Comp_Exec; Comp Obj: $Comp_Obj"
echo ""
echo "Step 1 - Configure ==> Set compiler type ... OK!"
echo "----------------------------------------------------------------"
echo ""
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# Menu to set optimization option
echo "----------------------------------------------------------------"
echo "Step 2 - Configure ==> Set optimization option ... "
echo ""

Optim_Opt=""
Optim_Exec=""

if bool_is_true "$Lib_Building_Automatic"; then
    if [[ "$RUN_LOWER" == "profile" ]]; then
        echo " ==> Profiling-oriented automatic optimization selected"
        Optim_Opt="Profile"
        if [[ "$Comp_Name" == "GNU/GFortran" ]]; then
            Optim_Exec="-O2 -g -fimplicit-none -Wall -Wline-truncation -fbacktrace -std=f2008 -fall-intrinsics"
        else
            Optim_Exec="-O2 -g"
        fi
    else
        echo " ==> Production optimization selected in automatic mode"
        Optim_Opt="Production"
        if [[ "$Comp_Name" == "GNU/GFortran" ]]; then
            Optim_Exec="-O3 -march=native -Ofast -funroll-loops -fimplicit-none -Wall -Wline-truncation -fwhole-file -std=f2008 -fall-intrinsics"
        else
            Optim_Exec="-O2 -static -static-intel -assume byterecl -align dcommons -fast"
        fi
    fi
else
    PS3=' ==> Please enter your choice: '
    select Opt in "Debug" "Production" "Quit"; do
        case "$Opt" in
            "Debug")
                Optim_Opt="Debug"
                if [[ "$Comp_Name" == "GNU/GFortran" ]]; then
                    Optim_Exec="-O2 -g3 -ggdb -fimplicit-none -Wall -Wline-truncation -Wcharacter-truncation -Wsurprising -Waliasing -Wimplicit-interface -Wunused-parameter -fwhole-file -fcheck=all -std=f2008 -pedantic -fbacktrace -fall-intrinsics"
                else
                    Optim_Exec="-O2 -static -static-intel -assume byterecl -align dcommons -fast"
                fi
                break
                ;;
            "Production")
                Optim_Opt="Production"
                if [[ "$Comp_Name" == "GNU/GFortran" ]]; then
                    Optim_Exec="-O3 -march=native -Ofast -funroll-loops -fimplicit-none -Wall -Wline-truncation -fwhole-file -std=f2008 -fall-intrinsics"
                else
                    Optim_Exec="-O2 -static -static-intel -assume byterecl -align dcommons -fast"
                fi
                break
                ;;
            "Quit")
                exit 0
                ;;
            *)
                echo "Bad Option!"
                ;;
        esac
    done
fi

echo " ==> Optimization Option: $Optim_Opt; Optimization Exec: $Optim_Exec"
echo ""
echo "Step 2 - Configure ==> Set optimization option ... OK!"
echo "----------------------------------------------------------------"
echo ""
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# Menu to set profiler option
echo "----------------------------------------------------------------"
echo "Step 3 - Configure ==> Set profiler option ... "
echo ""

Prof_Opt=""

if bool_is_true "$PROFILE"; then
    echo " ==> Profiling enabled"
    Prof_Opt="-pg"
elif bool_is_true "$Lib_Building_Automatic"; then
    echo " ==> Profiler option selected in automatic mode"
    Prof_Opt=""
else
    PS3=' ==> Please enter your choice: '
    select Opt in "Yes" "No" "Quit"; do
        case "$Opt" in
            "Yes")
                Prof_Opt="-pg"
                break
                ;;
            "No")
                Prof_Opt=""
                break
                ;;
            "Quit")
                exit 0
                ;;
            *)
                echo "Bad Option!"
                ;;
        esac
    done
fi

echo " ==> Profiler Option: $Prof_Opt"
echo ""
echo "Step 3 - Configure ==> Set profiler option ... OK!"
echo "----------------------------------------------------------------"
echo ""
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# Menu to set netCDF4 library
echo "----------------------------------------------------------------"
echo "Step 4 - Configure ==> Set NetCDF4 library ... "
echo ""

NC_Dir=""
NC_Inc=""
NC_Lib=""
NC_Libs=""
NC_Opt=""

if bool_is_true "$Lib_Building_Automatic"; then
    echo " ==> NetCDF4 library selected in automatic mode"
    NC_Dir="$NC_Dir_Default"
    NC_Inc="${NC_Dir}/include"
    NC_Lib="${NC_Dir}/lib"
    NC_Libs="-lnetcdff -lnetcdf"
    NC_Opt="-DLIB_NC"
else
    PS3=' ==> Please enter your choice: '
    select Opt in "Yes" "No" "Quit"; do
        case "$Opt" in
            "Yes")
                read -r -p ' ==> Please enter NetCDF4 complete library path: ' NC_Dir
                if [[ -z "$NC_Dir" ]]; then
                    NC_Dir="$NC_Dir_Default"
                    echo " ==> NetCDF4 complete library path set using DEFAULT path!"
                else
                    echo " ==> NetCDF4 complete library path set by USER"
                fi
                NC_Inc="${NC_Dir}/include"
                NC_Lib="${NC_Dir}/lib"
                NC_Libs="-lnetcdff -lnetcdf"
                NC_Opt="-DLIB_NC"
                break
                ;;
            "No")
                NC_Dir=""
                NC_Inc=""
                NC_Lib=""
                NC_Libs=""
                NC_Opt=""
                break
                ;;
            "Quit")
                exit 0
                ;;
            *)
                echo "Bad Option!"
                ;;
        esac
    done
fi

if [[ -n "$NC_Opt" ]]; then
    Comp_Obj="${Comp_Obj} ${NC_Opt}"
fi

echo " ==> NetCDF4 path: $NC_Dir; NetCDF4 Option: $NC_Opt; NetCDF4 Comp Obj: $Comp_Obj"
echo ""
echo "Step 4 - Configure ==> Set NetCDF4 library ... OK!"
echo "----------------------------------------------------------------"
echo ""
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# Set executable HMC name
echo "----------------------------------------------------------------"
echo "Step 5 - Configure ==> Set HMC executable name ... "
echo ""

Exec="$EXEC_NAME"

if bool_is_true "$Lib_Building_Automatic"; then
    echo " ==> HMC executable name selected in automatic mode"
    echo " ==> HMC executable name [Auto]: $Exec"
else
    read -r -p ' ==> Please enter HMC executable name: ' Exec_Input
    if [[ -n "$Exec_Input" ]]; then
        Exec="$Exec_Input"
        echo " ==> HMC executable name [User]: $Exec"
    else
        echo " ==> HMC executable name [Default]: $Exec"
    fi
fi

echo ""
echo "Step 5 - Configure ==> Set HMC executable name ... OK"
echo "----------------------------------------------------------------"
echo ""
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# Set stack size
echo "----------------------------------------------------------------"
echo "Step 1 - Compile ==> Set stack size to unlimited ... "
echo ""

ulimit -s unlimited || warn "Could not set stack size to unlimited"
ulimit -a || true

echo ""
echo "Step 1 - Compile ==> Set stack size to unlimited ... OK!"
echo "----------------------------------------------------------------"
echo ""
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# Expand archive file or use source directory
echo "----------------------------------------------------------------"
echo "Step 2 - Compile ==> Prepare source directory ... "
echo ""

Current_Dir="${PWD}"
Build_Dir=""

if [[ -d "$Archive" ]]; then
    Archive_Dir="$(cd "$Archive" && pwd)"
    echo " ==> Source directory detected: $Archive_Dir"
elif [[ -f "$Archive" ]]; then
    Build_Dir="${Current_Dir}/temp_build_hmc_$$"
    Archive_Dir="$Build_Dir"
    mkdir -p "$Archive_Dir"

    echo " ==> Extracting archive: $Archive"
    tar xvfz "$Archive" -C "$Archive_Dir" --strip-components=1
    echo " ==> Files in compressed format -- Unzipped!"
else
    die "Archive/source not found: $Archive"
fi

echo ""
echo "Step 2 - Compile ==> Prepare source directory ... DONE!"
echo "----------------------------------------------------------------"
echo ""
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# Build module(s) and subroutine(s)
echo "----------------------------------------------------------------"
echo "Step 3 - Compile ==> Build module(s) and subroutine(s) ... "
echo ""

cd "$Archive_Dir"

sources=(
    gnufor2.f90
    HMC_Module_Tools_Debug.f90
    HMC_Module_Args.f90
    HMC_Module_Namelist.f90
    HMC_Module_Tools_Interp.f90
    HMC_Module_Tools_Generic.f90
    HMC_Module_Tools_IO.f90
    HMC_Module_Tools_Time.f90
    HMC_Module_Vars_Loader.f90
    HMC_Module_Vars_Manager.f90
    HMC_Module_Phys_HydraulicStructure.f90
    HMC_Module_Phys_Convolution_Apps_Flooding.f90
    HMC_Module_Phys_Convolution_Apps_DeepFlow.f90
    HMC_Module_Phys_Convolution_Apps_Discharge.f90
    HMC_Module_Phys_Convolution_Apps_Horton.f90
    HMC_Module_Phys_Convolution_Apps_HydraulicStructure.f90
    HMC_Module_Phys_Convolution_Apps_IntegrationStep.f90
    HMC_Module_Phys_Convolution_Apps_SubFlow.f90
    HMC_Module_Phys_Convolution_Apps_SurfaceFlow.f90
    HMC_Module_Phys_Convolution_Type_ChannelFraction.f90
    HMC_Module_Phys_Convolution_Type_ChannelNetwork.f90
    HMC_Module_Phys_LSM_Apps.f90
    HMC_Module_Phys_LSM.f90
    HMC_Module_Phys_Snow_Apps.f90
    HMC_Module_Phys_Snow.f90
    HMC_Module_Phys_ET.f90
    HMC_Module_Phys_Retention.f90
    HMC_Module_Phys_StateUpdating.f90
    HMC_Module_Phys.f90
    HMC_Module_Data_Forcing_Gridded.f90
    HMC_Module_Data_Forcing_Point.f90
    HMC_Module_Data_Forcing_TimeSeries.f90
    HMC_Module_Data_Updating_Gridded.f90
    HMC_Module_Data_Output_Gridded.f90
    HMC_Module_Data_Output_Point.f90
    HMC_Module_Data_Output_TimeSeries.f90
    HMC_Module_Data_Restart_Gridded.f90
    HMC_Module_Data_Restart_Point.f90
    HMC_Module_Data_State_Gridded.f90
    HMC_Module_Data_State_Point.f90
    HMC_Module_Data_Static_Gridded.f90
    HMC_Module_Data_Static_Point.f90
    HMC_Module_Info_Gridded.f90
    HMC_Module_Info_Point.f90
    HMC_Module_Info_Time.f90
)

netcdf_sources=(
    HMC_Module_Tools_IO.f90
    HMC_Module_Data_Forcing_Gridded.f90
    HMC_Module_Data_Forcing_Point.f90
    HMC_Module_Data_Forcing_TimeSeries.f90
    HMC_Module_Data_Updating_Gridded.f90
    HMC_Module_Data_Output_Gridded.f90
    HMC_Module_Data_Output_Point.f90
    HMC_Module_Data_Output_TimeSeries.f90
    HMC_Module_Data_Restart_Gridded.f90
    HMC_Module_Data_Restart_Point.f90
    HMC_Module_Data_State_Gridded.f90
    HMC_Module_Data_State_Point.f90
    HMC_Module_Data_Static_Gridded.f90
    HMC_Module_Data_Static_Point.f90
    HMC_Module_Info_Gridded.f90
    HMC_Module_Info_Point.f90
    HMC_Module_Info_Time.f90
)

for src in "${sources[@]}"; do
    [[ -f "$src" ]] || die "Missing source file: $src"

    needs_netcdf=false
    for nc_src in "${netcdf_sources[@]}"; do
        if [[ "$src" == "$nc_src" ]]; then
            needs_netcdf=true
            break
        fi
    done

    log "Compiling $src"
    if $needs_netcdf && [[ -n "$NC_Inc" && -n "$NC_Lib" ]]; then
        "$Comp_Exec" $Comp_Obj "$src" $Prof_Opt -I"$NC_Inc" -L"$NC_Lib" $NC_Libs
    else
        "$Comp_Exec" $Comp_Obj "$src" $Prof_Opt
    fi
done

echo ""
echo "Step 3 - Compile ==> Build module(s) and subroutine(s) ... DONE!"
echo "----------------------------------------------------------------"
echo ""
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# Link object(s) and create executable(s)
echo "----------------------------------------------------------------"
echo "Step 4 - Compile ==> Link object(s) and create HMC model executable ... "
echo ""

objects=(
    gnufor2.o
    HMC_Module_Data_Forcing_Gridded.o
    HMC_Module_Data_Forcing_Point.o
    HMC_Module_Data_Forcing_TimeSeries.o
    HMC_Module_Data_Updating_Gridded.o
    HMC_Module_Data_Output_Gridded.o
    HMC_Module_Data_Output_Point.o
    HMC_Module_Data_Output_TimeSeries.o
    HMC_Module_Data_Restart_Gridded.o
    HMC_Module_Data_Restart_Point.o
    HMC_Module_Data_State_Gridded.o
    HMC_Module_Data_State_Point.o
    HMC_Module_Data_Static_Gridded.o
    HMC_Module_Data_Static_Point.o
    HMC_Module_Info_Gridded.o
    HMC_Module_Info_Point.o
    HMC_Module_Info_Time.o
    HMC_Module_Args.o
    HMC_Module_Namelist.o
    HMC_Module_Phys_Convolution_Type_ChannelNetwork.o
    HMC_Module_Phys_Convolution_Type_ChannelFraction.o
    HMC_Module_Phys_Convolution_Apps_SurfaceFlow.o
    HMC_Module_Phys_Convolution_Apps_SubFlow.o
    HMC_Module_Phys_Convolution_Apps_IntegrationStep.o
    HMC_Module_Phys_Convolution_Apps_HydraulicStructure.o
    HMC_Module_Phys_Convolution_Apps_Horton.o
    HMC_Module_Phys_Convolution_Apps_Discharge.o
    HMC_Module_Phys_Convolution_Apps_DeepFlow.o
    HMC_Module_Phys_Convolution_Apps_Flooding.o
    HMC_Module_Phys_HydraulicStructure.o
    HMC_Module_Phys_ET.o
    HMC_Module_Phys.o
    HMC_Module_Phys_LSM_Apps.o
    HMC_Module_Phys_LSM.o
    HMC_Module_Phys_Snow_Apps.o
    HMC_Module_Phys_Snow.o
    HMC_Module_Phys_Retention.o
    HMC_Module_Phys_StateUpdating.o
    HMC_Module_Tools_Debug.o
    HMC_Module_Tools_Generic.o
    HMC_Module_Tools_Interp.o
    HMC_Module_Tools_IO.o
    HMC_Module_Tools_Time.o
    HMC_Module_Vars_Loader.o
    HMC_Module_Vars_Manager.o
)

for obj in "${objects[@]}"; do
    [[ -f "$obj" ]] || die "Missing object file: $obj"
done

[[ -f "HMC_Main.f90" ]] || die "Missing main source file: HMC_Main.f90"

"$Comp_Exec" $Optim_Exec \
    "${objects[@]}" \
    HMC_Main.f90 \
    -o "$Exec" \
    $Prof_Opt \
    ${NC_Inc:+-I"$NC_Inc"} \
    ${NC_Lib:+-L"$NC_Lib"} \
    $NC_Libs

echo ""
echo "Step 4 - Compile ==> Link object(s) and create HMC model executable ... DONE!"
echo "----------------------------------------------------------------"
echo ""
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# Change option(s) if HMC executable
echo "----------------------------------------------------------------"
echo "Step 5 - Compile ==> Change option(s) of HMC model executable ... "
echo ""

if [[ -d "$Lib_Dir_Exec" ]]; then
    rm -rf "$Lib_Dir_Exec"
fi
mkdir -p "$Lib_Dir_Exec"

if [[ -e "$Exec" ]]; then
    chmod +x "$Exec"
    cp "$Archive_Dir/$Exec" "$Lib_Dir_Exec/$Exec"
    echo " ==> $Exec copied in library folder ... DONE!"
else
    echo " ==> $Exec copied in library folder ... FAILED!"
    die "Executable not found after linking: $Exec"
fi

echo ""
echo "Step 5 - Compile ==> Change option(s) of HMC model executable ... DONE!"
echo "----------------------------------------------------------------"
echo ""
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# Get extra tool(s) for HMC model testing
echo "----------------------------------------------------------------"
echo "Step 6 - Compile ==> Get extra tool(s) for HMC model testing ... "
echo ""

tools=(
    gprof2dot.py
    hmc_debug_execution.sh
    hmc_debug_launcher.sh
    hmc_debug_profiler.sh
    hmc_debug_memory.sh
)

for file_tool in "${tools[@]}"; do
    if [[ -e "$file_tool" ]]; then
        chmod +x "$file_tool" || true
        cp "$Archive_Dir/$file_tool" "$Lib_Dir_Exec/$file_tool"
        echo " ==> $file_tool copied in library folder ... DONE!"
    else
        echo " ==> $file_tool copied in library folder ... FILE NOT FOUND!"
    fi
done

echo ""
echo "Step 6 - Compile ==> Get extra tool(s) for HMC model testing ... DONE!"
echo "----------------------------------------------------------------"
echo ""
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# Final summary
echo "----------------------------------------------------------------"
echo "Build summary"
echo "----------------------------------------------------------------"
echo "RUN tag              : $RUN"
echo "Executable name      : $Exec"
echo "Install directory    : $Lib_Dir_Exec"
echo "Compiler             : $Comp_Exec"
echo "Compiler version     : $Comp_Version"
echo "Optimization         : $Optim_Opt"
echo "Profiling enabled    : $PROFILE"
echo "Profiler flags       : ${Prof_Opt:-<none>}"
echo "NetCDF directory     : ${NC_Dir:-<disabled>}"
echo "Source directory     : $Archive_Dir"
echo "----------------------------------------------------------------"
echo ""
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# End - Script
echo "----------------------------------------------------------------"
echo "$Script - Version $Version"
echo "Script to set, compile and build HMC model"
echo "COMPLETED - Bye, Bye"
echo "----------------------------------------------------------------"
echo ""
# -----------------------------------------------------------------------------

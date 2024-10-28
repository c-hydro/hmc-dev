#!/bin/bash -e

#-----------------------------------------------------------------------------------------
# Script information
script_name='HMC DEBUGGER - LAUCHER'
script_version="1.5.0"
script_date='2022/05/31'

# Deps:
# apt install python3.8-venv

# Argument(s) default definition(s)
script_folder="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd)"

virtual_env_folder="${script_folder}/hmc_debug_venv"
virtual_env_name="hmc_debug_settings"
virtual_env_requirements='hmc_debug_requirements.txt'
# ----------------------------------------------------------------------------------------

# ----------------------------------------------------------------------------------------
# Info script start
echo " ==================================================================================="
echo " ==> "$script_name" (Version: "$script_version" Release_Date: "$script_date")"
echo " ==> START ..."

# Define virtual_env path and binaries
virtual_env_path=${virtual_env_folder}/${virtual_env_name}
virtual_env_bin=$virtual_env_path/bin/activate

# Install (if needed) the hmc debug environment
echo " ===> INSTALL HMC DEBUG ENVIRONMENT ... "

if [ ! -f ${virtual_env_bin} ]; then

	# Create virtual_env folder
	if [ ! -d "$virtual_env_folder" ]; then
		mkdir -p $virtual_env_folder
	fi

	# Create virtual_env system
	python3 -m venv $virtual_env_path

	# Use pip inside venv 
	pip_env="$virtual_env_path/bin/pip"
	# Upgrade pip
	$pip_env install --upgrade pip

	# Add libraries to virtual_env
	$pip_env install -r ${virtual_env_requirements}

	echo " ===> INSTALL HMC DEBUG ENVIRONMENT ... DONE"
	
else
	echo " ===> INSTALL HMC DEBUG ENVIRONMENT ... PREVIOUSLY INSTALLED"
fi

# Run the hmc debug environment
echo " ===> RUN HMC DEBUG ENVIRONMENT ... "
source $virtual_env_path/bin/activate
python3 debug_2dVar.py -path_data ${script_folder}
echo " ===> RUN HMC DEBUG ENVIRONMENT ... DONE"
# ----------------------------------------------------------------------------------------

# ----------------------------------------------------------------------------------------
# Info script end
echo " ==> "$script_name" (Version: "$script_version" Release_Date: "$script_date")"
echo " ==> ... END"
echo " ==> Bye, Bye"
echo " ==================================================================================="
# ----------------------------------------------------------------------------------------

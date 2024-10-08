#!/bin/bash -e

#-----------------------------------------------------------------------------------------
# Script information
script_name='HMC - LAUNCHER EXECUTION'
script_version="1.0.0"
script_date='2021/03/23'

# Python virtual environment information
hmc_file_library="/share/idrologia/DTE/workspace_run_et/library/fp_env_system"

hmc_file_executable="/share/idrologia/DTE/workspace_run_et/exec/HMC_Model_V314.x"
hmc_file_settings="/share/idrologia/DTE/workspace_run_et/exec/po.info_run_et.txt"
#-----------------------------------------------------------------------------------------

#-----------------------------------------------------------------------------------------
# Set the libraries and the system environment
source ${hmc_file_library}
# Set the unlimited memory usage
ulimit -s unlimited 
# Set executable flag
chmod +x ${hmc_file_executable}
# ----------------------------------------------------------------------------------------

# ----------------------------------------------------------------------------------------
# Info script start
echo " ==================================================================================="
echo " ==> "$script_name" (Version: "$script_version" Release_Date: "$script_date")"
echo " ==> START ..."

# Run hmc model with a selected setting file
${hmc_file_executable} ${hmc_file_settings}

# Info script end
echo " ==> "$script_name" (Version: "$script_version" Release_Date: "$script_date")"
echo " ==> ... END"
echo " ==> Bye, Bye"
echo " ==================================================================================="
# ----------------------------------------------------------------------------------------


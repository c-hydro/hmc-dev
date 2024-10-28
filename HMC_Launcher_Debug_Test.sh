#!/bin/bash

# NOTE 1:
# In 'HMC_Args_DebugTest.txt' we have:
# run 30 3 0.6 0.015 marche 0.3 500 1 70 

# NOTE 2:
# ISSUE with PTRACE (UBUNTU system)
# sudo -i
# sudo echo 0 > /proc/sys/kernel/yama/ptrace_scope

# Test to debug hmc model
name_comp='HMC_Model_V2_$RUN.x'
name_exec='HMC_Model_V2_DebugTest.x'
name_args='HMC_Args_DebugTest.txt'
name_results='HMC_Results_DebugTest.txt'

# Settings
export LD_LIBRARY_PATH=${LD_LIBRARY_PATH}:/usr/local/netcdf-4.1.2/lib/
ulimit -s unlimited

# Control executable file existence
if [ -f $name_exec ];
then
   echo "File $name_exec exists. Removing it ... "
   rm $name_exec
   echo "File $name_exec exists. Removing it ... OK "
fi

# Debug Test
echo " DebugTest ... "

cp $name_comp $name_exec
gdb ./$name_exec < $name_args > $name_results

echo " DebugTest ... OK"



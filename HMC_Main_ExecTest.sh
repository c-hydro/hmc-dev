#!/bin/bash

# Test to execute hmc model
name_comp='HMC_Model_V2_$RUN.x'
name_exec='HMC_Model_V2_ExecTest.x'

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

# Execution test
echo " ExecTest ... "

cp $name_comp $name_exec
./$name_exec 30 3 0.6 0.015 marche 0.3 500 1 70

echo " ExecTest ... OK"



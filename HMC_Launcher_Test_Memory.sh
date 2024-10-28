#!/bin/bash

# tool: sudo apt-get install valgrind; sudo apt-get install tee

name_comp='HMC_Model_V2_$RUN.x'
name_exec='HMC_Model_V2_MemoryTest.x'

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

# Memory test (for output : 2>&1 | tee memory_check.txt
echo " MemoryTest ... "

cp $name_comp $name_exec
valgrind -v --track-origins=yes --tool=memcheck --leak-check=full ./$name_exec 30 3 0.6 0.015 marche 0.3 500 1 70 

echo " MemoryTest ... OK"


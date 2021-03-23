#!/bin/bash

echo " --> NetBeans 8.1 Loading ... "
/bin/sh "/home/fabio/Documents/Working_Area/Code_Development/Apps/netbeans-8.1/bin/netbeans" &
echo " --> NetBeans 8.1 Loading ... OK"

echo " --> MATLAB Script for debugging Loading ... "
matlab -nosplash -nodesktop -r "debug_2dVar()"
echo " --> MATLAB Script for debugging Loading ... OK"

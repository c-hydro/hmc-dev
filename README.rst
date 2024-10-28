Hydrological Model Continuum
============================

Welcome to the **Hydrological Model Continuum** GitHub repository. This is an hydrological model supported by the Italian Civil Department (DPC) and is used for preventing and reducing hydrogeological risk.

Background
**********

The **Hydrological Model Continuum**, commonly named **HMC**, is the hydrological model used by **Flood-PROOFS Modelling System**. The Flood-PROODS framework is a modelling system designed to assist decision makers during the operational phases of flood forecasting, nowcasting, mitigation and monitoring in various-sized catchments. 

The system is operational in real time in different basins on the whole Italian territory for the National Civil Protection Department and in some basins of Bolivia, Albania and Lebanon. It is also used for water management purposed by Italian hydro-power companies (Compagnia Valdostana delle Acque, CVA, ERG Power Generation). 

The main components of the HMC are reported below:
- Modified Horton Method for Infiltration
- Runoff Routing;
- Subsurface Flow Routing;
- Complete Energy Balance using Force Restore Equation to compute Soil Temperature;
- Tdeep Filter;
- WaterTable and Deep Flow Routing;
- Snow model and correction;
- SoilMoisture correction;
- Groundwater fracturation.

Components
**********

All codes and datasets are freely available and users can be get them from our github repository [1_].

Prerequisites
*************

In order to use the HMC, users are strongly raccomanted to control if the following characteristics, libraries and packages are available and correctly installed on their machine.

Usually, HMC libraries are installed on **Linux Debian/Ubuntu 64bit** environment and all libraries, packages and applications must be compilled and/or installed in according with this operative system.

All codes, subroutines and scripts are developed using **Fortran** (version 2003 and greater) programming language [2_].

The section for installing all needed libraries and environments is usually named **fp-envs** [3_]and the users can find it our codes repository hosted by GitHub [1_].

Fortran libraries
-----------------

HMC needs netcdf4 library to read input provided by other preprocessing tools and to write output for external applications (such as Panoply, cdo, ncview ...).
To set and compile netcdf4 library a bash script named **setup_fp_env_system.sh** is provided. 
Script downloads **zlib** [4_], **hdf5** [5_] and **netcdf4** [6_] libraries from their repositories; after downloading source compressed archives, script creates a folder in “$HOME/user/fp_libs_system/” where all libraries will be compilled and installed. During the installation, a environment file named “fp_env_system” is created for saving LD_LIBRARY_PATH (for native code libraries) and PATH (for executables) references of installed libraries.

HMC libraries
-------------
After preparing all necessary libraries and environmental settings, source files of HMC must be compiled to run properly [7_]; usually, sources are compiled using **GNU compilers** (such as gcc and gfortran) that have to be installed on user’s machine. To compile and optimize HMC codes a bash file named **setup_fp_env_hmc.sh** is provided. Using this setup file, user will have to answer some questions about how to compile HMC codes.
Usually, to build Continuum for production use, options have to set as follows:

    • set compiler type [1] for using GNU/GFortran compiler;
    • set optimization option [2] for using production mode; 
    • set profiler option [2] for skipping profiling used to control model performances;
    • set NetCDF4 library [1] for using NetCDF4 input and output files format.

Netbeans configuration
----------------------
In order to use and develop the HMC, users are strongly raccomanded to set their devoloping environment
using a IDE such as NetBeans Apache [8_].

The users could be interested in debugging codes to have a deeper knowledge of the HMC; usually, for doing this task, the common advice is used a IDE (e.g, CodeBlock, VS Code, Apache Netbeans) in order to easily analyze codes and memory usage. In this part, the configuration of a debug workspace in Apache NetBeans IDE will be presented.

First of all, the package for C, C++ and Fortran programming languages have to be installed in Apache NetBeans; to complete this step, the users have to install the package related with C/C++ language. Particuarly, following these instructions:

  1) Tools --> Plugins --> Settings --> Tick "NetBeans 8.2 Plugin Portal" --> Close 

and Reboot the Apache NetBeans IDE.
Next step, users should create a New Project following these instructions: 

  2) File --> New Project --> Category :: Sample :: C/C++ --> Project :: Hello World Fortran Application --> Next --> Choose Name --> Close

After creating a folder project, users have to import all source code in the project folder; Using the left menu where the name of projects
are visible, right click on selected project:

  3) Source Files --> Add existing items ...

performing this action, a form to select all file will be opened. Finally, all source files will be available into source file folder
of the selected project. 
Next steps cover the configuration of the dependencies in the project. Particularly, the main task is linking the NetCDF library against
the project.
For configuring the NetCDF4 in Apache NetBeans IDE, the users have to add in:

  4) Project --> Properties --> Linker --> Libraries
     
     in /path_to_netcdf/ find the following files
     netcdff.a and netcdff.so 
     and note "double f" for fortran libraries

  5) Project --> Properties --> Linker --> Additional Options
     
      -I/path_to_netcdf/include/ 
      -L/path_to_netcdf/lib/ 
      -lnetcdff -lnetcdf   

  6) Project --> Properties --> Fortran Compiler --> Additional Options

      -I/path_to_netcdf/include/ 
      -L/path_to_netcdf/lib/ 
      -lnetcdff -lnetcdf  

  7) Project --> Properties --> Fortran Compiler --> Additional Options
  
      gfortran: -cpp -DLIB_NC
      ifort: -fpp -DLIB_NC  

  8) Project --> Properties --> Run --> Environment --> NewValue
  
      Name: LD_LIBRARY_PATH 
      Value: $LD_LIBRARY_PATH:/path_to_necdf/lib/

Once the NetCDF4 are linked, it will be possible to compile each source file using the F9 key.
After doing all these steps, the users have to set the debug command to run Hydrological Model Continuum 
using, for instance, a namelist file of a study case:  
  
  9) Debug --> Debug Command 
  	
  "${OUTPUT_PATH}" domainname.info.txt

After setting the environment and all needed options for running the model, the users will are able to get a deeper information using the options to execute code in a debugging mode using breakpoints and all the features available in **gdb** debugging library [9_].

Potential Users
***************
The HMC has been released to enable different applications (for example local/regional scenario assessment) and further development by external users.

Potential users are anticipated to predominately be interested in the ability to run the model with local data (including scenario modelling) and to modify the model with new capabilities. The potential collaborators have expressed a range of potential goals for their use of the HMC, including performing comparisons with existing models, tailoring the hydrological performance to specific land uses and cropping types.

Contribute and Guidelines
*************************

We are happy if you want to contribute. Please raise an issue explaining what is missing or if you find a bug. We will also gladly accept pull requests against our master branch for new features or bug fixes.

If you want to contribute please follow these steps:

    • fork the hmc-dev repository to your account;
    • clone the repository, make sure you use "git clone --recursive" to also get the test data repository;
    • make a new feature branch from the repository master branch;
    • add your feature;
    • please include tests for your contributions in one of the test directories;
    • submit a pull request to our master branch.

Authors
*******

All authors involved in the library development for the HMC are reported in this authors_ file.

License
*******

By accessing or using the HMC, code, data or documentation, you agree to be bound by the HMC license available. See the license_ for details. 

Changelog
*********

All notable changes and bugs fixing to this project will be documented in this changelog_ file.

References
**********
| [1_] CIMA Hydrology and Hydraulics GitHub Repository
| [2_] Fortran programming language
| [3_] FloodPROOFS virtual environment tools
| [4_] ZLIB compression library
| [5_] HDF5 data software library 
| [6_] NetCDF4 data software library 
| [7_] Hydrological Model Continuum codes
| [8_] NetBeans Apache IDE 
| [9_] GDB 

.. _1: https://github.com/c-hydro
.. _2: https://en.wikipedia.org/wiki/Fortran
.. _3: https://github.com/c-hydro/fp-env
.. _4: https://zlib.net/
.. _5: https://www.hdfgroup.org/solutions/hdf5/
.. _6: https://www.unidata.ucar.edu/
.. _7: https://github.com/c-hydro/hmc-dev
.. _8: https://netbeans.apache.org/
.. _9: https://www.gnu.org/software/gdb/
.. _license: LICENSE.rst
.. _changelog: CHANGELOG.rst
.. _authors: AUTHORS.rst

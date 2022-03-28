=========
Changelog
=========

Version 3.1.6 [2022-03-28]
**************************
- FIX: for dam hydraulic structure remove statement to select the max discharge between the dam discharge and dam discharge set by namelist file.

Version 3.1.5 [2021-06-25]
**************************
- ADD: the watertable deep losses parameter and flag;
- ADD: the watertable allowed maximum water loss;
- ADD: the group of the debug variable(s) in writing output;
- ADD: the checking of Fortran version to build the routines using string allocatable arrays (GFortran > 7);
- ADD: the SnowMask variable in the output file;
- FIX: bugs in writing actual accumulated et with basic variable group;
- FIX: bug in the initial condition of water content of wilting point; the variable is set to zero to avoid unexpected behaviour of cells reservoir;
- FIX: bug in restart conditions related to the integration step parameters to avoid errors in the routing rescaling;
- FIX: bug in the snow module related to the SWE, RhoS and SnowMask variables;
- FIX: bug in the channel network physics adding the fracturation variable.

Version 3.1.4 [2021-03-08]
**************************
- ADD: the flag settings for switching on/off the energy-balance physics;
- ADD: the forcing datasets of actual and potential evapotranspiration according with the flag settings of energy balance physics;
- FIX: bugs in reading intake info file;
- FIX: bugs in managing LAI datasets in different vegetation physics modules;
- FIX: bugs in creating empty files of dams and analysis.

Version 3.1.3 [2020-10-26]
**************************
- ADD: the distributed static layer of the watertable maximum in the list of optional static information( {domain}.wt_max.txt or variable Wt_Max in the netcdf file);
- ADD: the variable of namelist to set the maximum delta of LST in the runge-kutta integration method (dLSTDeltaMax);
- ADD: add pid process to binary files for multiprocess model execution;
- FIX: in the namelist, the flag of snow physics to correctly set the model execution; 
- FIX: some bugs in the Jarvis method to compute canopy resistence (units of datasets, operative conditions ... );
- FIX: the maximum delta of the soil temperature in the integration of LST by Runge-Kutta method in order to avoid some physics incongruences;
- FIX: the scaling of thermal inertia to obtain a reduction of the expected maximum value;
- FIX: the dimensions name in state output filename (south_north, east_west);
- FIX: bug in iterations of the LST Runge-Kutta method.

Version 3.1.2 [2020-07-23]
**************************
- ADD: variable names checking in reading netcdf datasets;
- ADD: parameters configuration for CN, WS and Fracturation;
- ADD: variable time in state and outcome datasets;
- FIX: restart mandatory flags for snow variables;
- FIX: undefined time step variable in snow physics for unavailable datasets;
- FIX: hydraulic structures bug in surface flow for channel fraction.

Version 3.1.1 [2020-03-30]
**************************
- ADD: flags for selecting the outcome datasets;
- FIX: bugs of data dumping.

Version 3.1.0 [2020-01-30]
**************************
- ADD: dynamic vegetation module (Jarvis);
- ADD: flooding module;
- FIX: bugs in the dams output;
- FIX: bugs in dams observed time-series.

Version 3.0.0 [2019-04-10]
**************************
- RELEASE: beta release based on previous model generation.
- ADD: convolution type (channel network and channel fraction modules);
- ADD: groundwater bedrock fracturation module.

Version 2.0.7 [2018-01-19]
**************************
- ADD: release mass balance control module
- ADD: water sources module
- ADD: soil moisture data assimilation
- ADD: arguments module for using a namelist file or a parameters list;
- FIX: generic bugs and bugs in controlling dams volume and level

Version 2.0.6 [2016-04-20]
**************************
- RELEASE: final release based on previous model generation.

Version 2.0.5 [2015-11-09]
**************************
- FIX: generic bugs 
- FIX: bugs in the discharge module

Version 2.0.4 [2015-10-30]
**************************
- FIX: bugs in the managing of dams and hydraulics structure;
- FIX: bugs in the managing of convolution integration step;
- FIX: bugs in the deep-flow module.

Version 2.0.3 [2015-10-20]
**************************
- FIX: bugs in the retention module;
- FIX: bugs in the evapotranspiration module;
- FIX: bugs in the convolution-surfaceflow module.

Version 2.0.2 [2015-10-10]
**************************
- FIX: bugs in the convolution-horton module; 
- FIX: bugs in the convolution-subflow module;
- FIX: bugs in the convolution-surfaceflow module.

Version 2.0.1 [2015-10-06]
**************************
- FIX: bugs in the energy balance module;
- FIX: bugs in the convolution-horton module; 
- FIX: bugs in the convolution-subflow module;
- FIX: bugs in the convolution-surfaceflow module.

Version 2.0.0 [2015-05-15]
**************************
- RELEASE: beta release based on previous model generation.

Version 1.0.0 [2015-05-01]
**************************
- RELEASE: final release based on DRiFt model.




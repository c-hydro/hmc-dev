# Changelog

All notable changes to the Hydrological Model Continuum (HMC) are documented in this file.

---

## [3.4.1] - 2026-04-09

### Updated
- Fortran debugger for checking and viewing 2d arrays (v.1.6.0)

### Fixed
- Fix and update suborutines to manage the not equal grids (forcing/updating vs land)
- Removed testing/debug messages

---

## [3.4.0] - 2026-03-20

### Added
- Routing methods based on:
  - grid approach (`iFlagRoutingType = 1`, default)
  - indexed approach (`iFlagRoutingType = 2`)
- Subroutines and methods to activate routing using grid or indexed approach

### Fixed
- Removed testing/debug messages

---

## [3.3.1] - 2026-03-18

### Added
- Plant release curve exponent from file (default compatibility: `exp = 6`)
- Minimum plant discharge from file (default compatibility: `min = 0`)
- Support for compressed and uncompressed NetCDF forcing/updating files
- Multiple methods for UDt evaluation (`iUDtMethod`):
  - `1` :: maximum
  - `2` :: percentage (`dUDtParam ∈ [0,1]`)
  - `3` :: percentile (`dUDtParam ∈ [25,100]`, default = 1)

---

## [3.3.0] - 2025-11-13

### Added
- Groundwater initialization using fill level percentage (requires thickness raster)

### Fixed
- Channel network configuration
- Deep-losses activation flag
- Groundwater subflow at outlet cell (units)
- Numeric limits (`uc`, `uh`) in channel fraction

---

## [3.2.0] - 2024-11-04

### Added
- New optional soil and groundwater parametrization
- Soil infiltration dependency on saturation degree
- Constraint of actual evapotranspiration by soil water availability
- Rescaling factor for hypodermic flow ratio
- Spatial map support for `KSatRatio` (`domain.ksatratio.txt`)

### Fixed
- Domain mask and DEM values < 0
- Soil mass balance bug
- Removal of Ct-WP

### Updated
- License updated to **EUPL 1.2**

---

## [3.1.6] - 2022-06-23

### Added
- Optional irrigation water requirement module

### Fixed
- Dam discharge selection logic
- Dam spilling routine at maximum volume

---

## [3.1.5] - 2021-06-25

### Added
- Water table deep losses parameter and flag
- Maximum allowed water loss
- Grouping of debug variables in output
- Fortran version check for allocatable strings (GFortran > 7)
- SnowMask variable in output

### Fixed
- Actual ET output writing
- Wilting point initialization
- Restart condition issues
- Snow module variables (SWE, RhoS, SnowMask)
- Channel network fracturation handling

---

## [3.1.4] - 2021-03-08

### Added
- Energy balance activation flag
- Forcing datasets for ET (actual and potential)

### Fixed
- Intake info reading
- LAI handling in vegetation modules
- Empty file generation (dams, analysis)

---

## [3.1.3] - 2020-10-26

### Added
- Water table max layer (`{domain}.wt_max.txt`)
- Runge-Kutta LST delta limit (`dLSTDeltaMax`)
- PID in binary files for multiprocess execution

### Fixed
- Snow physics activation flag
- Jarvis canopy resistance method
- Soil temperature integration stability
- Thermal inertia scaling
- Output dimension naming
- Runge-Kutta iteration bug

---

## [3.1.2] - 2020-07-23

### Added
- NetCDF variable name checking
- CN, WS, Fracturation configuration
- Time variable in outputs

### Fixed
- Restart flags for snow variables
- Missing timestep in snow physics
- Hydraulic structures in surface flow

---

## [3.1.1] - 2020-03-30

### Added
- Output dataset selection flags

### Fixed
- Data dumping bugs

---

## [3.1.0] - 2020-01-30

### Added
- Dynamic vegetation module (Jarvis)
- Flooding module

### Fixed
- Dam output issues
- Dam observed time-series

---

## [3.0.0] - 2019-04-10

### Released
- Beta release based on previous model generation

### Added
- Convolution types (channel network, channel fraction)
- Groundwater bedrock fracturation module

---

## [2.0.7] - 2018-01-19

### Added
- Mass balance control module
- Water sources module
- Soil moisture data assimilation
- Arguments module (namelist or parameter list)

### Fixed
- Dam volume and level control bugs

---

## [2.0.6] - 2016-04-20

### Released
- Final release (previous model generation)

---

## [2.0.5] - 2015-11-09

### Fixed
- Generic bugs
- Discharge module bugs

---

## [2.0.4] - 2015-10-30

### Fixed
- Hydraulic structures and dams
- Convolution integration step
- Deep-flow module

---

## [2.0.3] - 2015-10-20

### Fixed
- Retention module
- Evapotranspiration module
- Surface flow convolution

---

## [2.0.2] - 2015-10-10

### Fixed
- Horton convolution
- Subflow convolution
- Surface flow convolution

---

## [2.0.1] - 2015-10-06

### Fixed
- Energy balance module
- Horton, subflow, and surface flow convolution modules

---

## [2.0.0] - 2015-05-15

### Released
- Beta release (previous generation)

---

## [1.0.0] - 2015-05-01

### Released
- Initial release based on DRiFt model

<p align="center">
  <img src="docs/img/app_logo_continuum.jpg" alt="HMC Logo" width="260"/>
</p>

# Hydrological Model Continuum (HMC)

[![Build HMC](https://github.com/c-hydro/hmc-dev/actions/workflows/build.yml/badge.svg)](https://github.com/c-hydro/hmc-dev/actions/workflows/build.yml)
[![Release](https://img.shields.io/github/v/release/c-hydro/hmc-dev)](https://github.com/c-hydro/hmc-dev/releases)
[![License: EUPL-1.2](https://img.shields.io/badge/License-EUPL%201.2-blue.svg)](LICENSE.rst)
[![Fortran](https://img.shields.io/badge/Fortran-2008%2B-purple.svg)](https://fortran-lang.org/)
[![Platform](https://img.shields.io/badge/platform-linux--64bit-lightgrey)]()

---

## 🌍 Overview

The **Hydrological Model Continuum (HMC)** is a distributed hydrological model developed by the **CIMA Research Foundation**.

It is operationally used by the **Italian Civil Protection Department (DPC)** and international partners.

HMC is the computational core of the **Flood-PROOFS modelling system**, designed to support:

- flood forecasting and nowcasting  
- hydrogeological risk prevention  
- water resource management  
- environmental and climate simulations  

The system is operational in:

- Italy  
- Bolivia  
- Albania  
- Lebanon  

---

## 🚀 Version

- **Current version:** `v3.4.2`  
- **Release date:** `2026-04-09`  

👉 See [CHANGELOG.md](CHANGELOG.md)

---

## ⚙️ Main Features

- Distributed hydrological simulation  
- **Routing modes (v3.4.0):**
  - Grid (`iFlagRoutingType = 1`)
  - Indexed (`iFlagRoutingType = 2`)
- Surface and subsurface flow  
- Snow model and energy balance  
- Soil moisture and groundwater dynamics  
- Channel network and convolution schemes  
- NetCDF input/output support  
- High-performance Fortran implementation  

---

## 🧩 Model Components

- Modified Horton infiltration method  
- Runoff routing  
- Subsurface flow routing  
- Energy balance (Force-Restore equation)  
- Deep soil temperature filter (Tdeep)  
- Water table and deep flow routing  
- Snow model and corrections  
- Soil moisture correction  
- Groundwater fracturation  

---

## 🖥️ Requirements

### System
- Linux (Debian/Ubuntu recommended)  
- 64-bit architecture  

### Compiler
- **Fortran 2008+** (GFortran ≥ 8 recommended)

### Libraries
- NetCDF4  
- HDF5  
- Zlib  

---

## 📦 Installation

### 1. Install dependencies

```bash
git clone https://github.com/c-hydro/fp-env
cd fp-env
bash setup_fp_env_system.sh
source $HOME/fp_libs_system/fp_env_system
```

---

### 2. Compile HMC

#### Automatic mode (recommended)

```bash
./configure.sh hmc-dev $HOME/fp_libs_system $HOME/fp_libs_system/hmc true
```

#### Default mode

```bash
./configure.sh
```

---

### ⚙️ Compilation configuration

Recommended settings:

- Compiler: GNU/GFortran  
- Optimization: Production  
- NetCDF: enabled  
- Profiling: disabled  

---

### 3. Build output

Executable will be created in:

```bash
$HOME/fp_libs_system/hmc/
```

Example:

```bash
HMC_Model_V3_Exec.x
```

---

## ▶️ Run Model

HMC uses a simplified interface:

```bash
HMC.x {domain}.info.txt
```

### Example

```bash
./HMC_Model_V3_Exec.x po.info_run_et.txt
```

---

## 📊 Profiling

### Compile profiling version

```bash
RUN=Profile ./configure.sh
```

### Run profiler

```bash
./HMC_Tools_Profiler.sh HMC_Model_V3_Profile.x domain.info.txt
```

### Outputs

- `hmc_model_analysis.txt`  
- `hmc_model_analysis.png`  

---

## 🧠 Memory Analysis

### Valgrind (memcheck)

```bash
./HMC_Tools_Memory.sh HMC_Model_V3_Exec.x domain.info.txt
```

### Callgrind

```bash
valgrind --tool=callgrind ./HMC_Model_V3_Exec.x domain.info.txt
```

---

## 📁 Input / Output

### Input
- `{domain}.info.txt` (namelist configuration)  
- NetCDF forcing datasets  
- Static maps (DEM, soil, vegetation, etc.)  

### Output
- NetCDF datasets  
- Time series  
- Diagnostic variables  

---

## 🧪 Development

### Recommended IDEs
- VS Code  
- NetBeans  
- Code::Blocks  

### Debugging tools
- gdb  
- valgrind  
- gprof  

---

## ⚙️ Continuous Integration

GitHub Actions automatically:

- builds HMC  
- checks compilation  
- produces executable artifacts  

Workflow:

```
.github/workflows/build.yml
```

---

## 🤝 Contributing

1. Fork repository  
2. Clone with submodules  
   ```bash
   git clone --recursive
   ```
3. Create a feature branch  
4. Add code and tests  
5. Submit a Pull Request  

---

## 👨‍💻 Authors

See 👉 [AUTHORS.md](AUTHORS.md)

---

## 📜 License

Licensed under the **European Union Public Licence (EUPL v1.2)**  

👉 See [LICENSE.rst](LICENSE.rst)

---

## 📖 Citation

If you use HMC:

```
Delogu, F., Gabellani, S., Silvestro, F., Libertino, A., Ercolani, G. (2026)
Hydrological Model Continuum (HMC) v3.4.0
CIMA Research Foundation
```

### BibTeX

```bibtex
@software{hmc_v340,
  title = {Hydrological Model Continuum (HMC)},
  version = {3.4.0},
  author = {
    Delogu, Fabio and
    Gabellani, Simone and
    Silvestro, Francesco and
    Libertino, Andrea and
    Ercolani, Giulia
  },
  year = {2026},
  publisher = {CIMA Research Foundation},
  url = {https://github.com/c-hydro/hmc-dev}
}
```

👉 Machine-readable citation: [CITATION.cff](CITATION.cff)

---

## 🔗 References

- https://www.cimafoundation.org/  
- https://github.com/c-hydro/hmc-dev  
- https://github.com/c-hydro/fp-env  
- https://fortran-lang.org/  

---

## 🧠 Topics

Hydrology · Hydrological Model · Flood Forecasting · Fortran · NetCDF · HPC · Environmental Modeling  

---

<p align="center">
Hydrological Model Continuum — Scientific Modeling for Water Systems
</p>

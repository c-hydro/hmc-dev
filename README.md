<p align="center">
  <img src="docs/img/app_logo_continuum.jpg" width="250">
</p>

# Hydrological Model Continuum (HMC)

**Hydrological Model Continuum (HMC)** is a distributed hydrological model developed by the **CIMA Research Foundation** and supported by the **Italian Civil Protection Department (DPC)**.

HMC is designed for:

- 🌊 flood forecasting and nowcasting  
- ⚠️ hydrogeological risk prevention  
- 💧 water resources management  
- 🛰️ environmental and operational simulations  

---

## 🌍 Background

HMC is the core hydrological engine of the **Flood-PROOFS modelling system**, supporting decision-making in:

- real-time forecasting chains  
- civil protection operations  
- environmental monitoring  

The system is operational in:

- 🇮🇹 Italy (national-scale real-time chain)  
- 🇧🇴 Bolivia  
- 🇦🇱 Albania  
- 🇱🇧 Lebanon  

It is also used for **hydropower management** (CVA, ERG).

---

## ⚙️ Model Overview

HMC is a **grid-based distributed hydrological model** based on terrain representation and river network extraction.

### Main Processes

- Modified Horton infiltration  
- Surface runoff routing  
- Subsurface flow routing  
- Water table and deep flow  
- Energy balance (Force-Restore method)  
- Snow model  
- Soil moisture correction  
- Groundwater processes  

---

## 🧱 Code Structure (v3.4.0)

The model is organized into modular Fortran components:

- `HMC_Main.f90` → model entry point  
- `HMC_Module_Phys_*` → physical processes  
- `HMC_Module_Data_*` → forcing, output, restart  
- `HMC_Module_Tools_*` → utilities (IO, time, interpolation)  
- `HMC_Module_Vars_*` → variables management  
- `configure.sh` → build system (v3.4.0)  

---

## ⚙️ Requirements

### System
- Linux (Debian/Ubuntu recommended)
- 64-bit architecture

### Language
- **Fortran 2008 or newer**

### Dependencies
- `zlib`
- `hdf5`
- `netcdf4` (with Fortran interface)

👉 Environment setup tools:  
https://github.com/c-hydro/fp-env

---

## 🔧 Installation

### 1. Setup environment

```bash
git clone https://github.com/c-hydro/fp-env
cd fp-env
bash setup_fp_env_system.sh

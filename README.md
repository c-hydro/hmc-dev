<p align="center">
  <img src="app_logo_continuum.jpg" alt="HMC Logo" width="250"/>
</p>

# Hydrological Model Continuum (HMC)

**Hydrological Model Continuum (HMC)** is a distributed hydrological model designed for simulation of the water cycle, flood forecasting, and environmental analysis.

It is developed and maintained by **CIMA Research Foundation** and operationally used by the **Italian Civil Protection Department (DPC)** and international partners.

---

## 🚀 Version

Current release: **v3.4.0**  
Release date: **2026-03-20**

See 👉 [CHANGELOG.md](CHANGELOG.md) for full details.

---

## 🌍 Overview

HMC is the core engine of the **Flood-PROOFS modelling system**, supporting:

- flood forecasting and nowcasting  
- hydrogeological risk prevention  
- water resource management  
- environmental and climate simulations  

---

## ⚙️ Main Features

- Distributed hydrological modeling  
- Grid-based and indexed routing (v3.4.0)  
- Surface and subsurface flow routing  
- Snow model and energy balance  
- Soil moisture and groundwater dynamics  
- NetCDF-based I/O  

---

## 🖥️ Requirements

- Linux (Debian/Ubuntu recommended)
- Fortran 2008+ (GFortran recommended)
- NetCDF4, HDF5, Zlib

---

## 📦 Installation

```bash
./configure.sh
```

---

## ▶️ Run Model

```bash
HMC.x domain.info.txt
```

---

## 👨‍💻 Authors

See 👉 AUTHORS.md

---

## 📜 License

EUPL v1.2 — see LICENSE.rst

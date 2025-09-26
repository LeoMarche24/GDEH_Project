# Geospatial Data Science for Environmental Health

## Project Overview
This project analyzes **population presence data** (residents and tourists) in Lombardy (Italy) combined with an interpolated **pollution field** (smoothed concentration).  
The workflow includes:
1. Preprocessing tourism presence data (geocoding, clipping, aggregation).
2. Computing exposure at the municipality and province levels.
3. Visualizing exposure on selected pilot days.
4. Analyzing full time series with daily aggregation.
5. Functional PCA on provincial exposure curves to identify common temporal patterns.

The smoothing is perfomed in the **C++** library **fdaPDE**; the analyses are conducted in **R**; spatial clipping, spatial joins, and georeferencing checks were additionally performed in **QGIS**, too.

---

## General Folder Structure
The folder is divided in two. First, this ReadMe describes the actual structure of the analysis of exposure. Secondly, the codes for the smoothing procedure are presented.

---

- `aree_con_comuni.csv` – Mapping of areas to municipalities (from QGIS spatial join)
- `Data_Presenze.csv` – Preprocessed presence data used in the analyses
  
- **Geo Data/**
  - `Compuni_Lombardia.shp` – Shapefile of Lombardy municipalities
  - `Provincie.shp` – Shapefile of Lombardy provinces
  - `Data_First_Day_*.csv`, `Data_First_Weekend_*.csv`, `Data_Christmas_*.csv`, `Data_Jan06_*.csv` – CSV layers exported for QGIS plotting
  - Other CSV exports used in QGIS for visualization and checks

- **scripts/**
  - `PreProcessingDataTurismo.R` – Script 1: preprocessing of presence data
  - `Exposure_1.R` – Script 2: pilot exposure visualization (municipal and provincial)
  - `Exposure_Full.R` – Script 3+4: full time-series analysis and Functional PCA

### Smoothing folder structure
i.e. **smoothing_analysis/**

- **dati_ARPA/**  
  Raw data from ARPA Lombardia including monitoring stations and pollutant concentration values.  

- **mesh_geospatial/**  
  Mesh used for model fitting. It represents a reinforced convex hull of the Lombardy region.  

- **mesh_lombardy/**  
  Mesh of the Lombardy region used for plotting purposes.  

- **smoothing/**  
  Contains data and results of the smoothing model.

- **results_smoothing/**    (outside of the general smoothing folder for simplicity of execution)
  - `f_eval_fine2.csv` – Smoothed field (pollution values per node and day)
  - `nodes_eval_fine2.csv` – Coordinates of interpolation nodes

### Files

- **boundary_nodes_lombardy.txt**  
  Boundary nodes of Lombardy, used for plotting.  

- **geospat_strpde.cpp**  
  C++ source file implementing the smoothing model, based on the *fdapde* library.  

- **Plot_Data.R**  
  R script for plotting input data.  

- **Plot_Smoothed_Field.R**  
  R script for visualising the smoothed pollution field.  

- **PreProcess_ARPA.R**  
  R script to read raw ARPA data and preprocess it into a format suitable for the smoothing model.  

- **Smoothing_analysis.R**  
  R script performing the analysis of smoothing results.  
---

## Requirements

- **R (≥ 4.2)**
- Required R packages:
  - sf
  - dplyr
  - ggplot2
  - tidyr
  - fda
  - viridis
  - gridExtra

To install all packages in one go:

    install.packages(c("sf", "dplyr", "ggplot2", "tidyr", "fda", "viridis", "gridExtra"))

- **fdaPDE library**
  installation from GitHub https://github.com/fdaPDE/fdaPDE-cpp

---

## Notes

- The raw file `Turismo_Presenza.csv` is **not** included due to size and privacy constraints. The analyses rely on the preprocessed file `Data_Presenze.csv`.
- QGIS was used to perform **spatial clipping**, **spatial joins**, and **georeferencing checks** for area layers.
- Functional PCA is applied at the **provincial level** to summarize temporal patterns of exposure.
- Precise liks for downloading raw data from ARPA Lombardia are below:
  
      1) Download pollution data: https://www.dati.lombardia.it/Ambiente/Dati-sensori-aria-dal-2018/g2hp-ar79/about_data
  
      2) Download sensors information: https://www.dati.lombardia.it/Ambiente/Stazioni-qualit-dell-aria/ib47-atvt/about_data

---

## Suggested Workflow for Reviewers

0. Run the procedure in **smoothing_analysis/**. Not reccomanded if not used to the fdapde library.
1. Run **PreProcessingDataTurismo.R** to build the cleaned dataset (`Data_Presenze.csv`) and pilot CSVs for QGIS.
2. Run **Exposure_1.R** to verify the pilot exposure maps and aggregation logic.
3. Run **Exposure_Full.R** to generate the full daily series, perform Functional PCA, and produce all figures used in the report.

## Disclosure on AI Assistance

This project was developed with the support of Artificial Intelligence (AI) tools.  
AI was used for:
- generating and refactoring R code,
- improving code readability and documentation,
- drafting and revising text (including this README).  

All results, analyses, and interpretations were reviewed and validated by the author.


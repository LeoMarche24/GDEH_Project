# Geospatial Data Science for Environmental Health

## Project Overview
This project analyzes **population presence data** (residents and tourists) in Lombardy (Italy) combined with an interpolated **pollution field** (smoothed concentration).  
The workflow includes:
1. Preprocessing tourism presence data (geocoding, clipping, aggregation).
2. Computing exposure at the municipality and province levels.
3. Visualizing exposure on selected pilot days.
4. Analyzing full time series with daily aggregation.
5. Functional PCA on provincial exposure curves to identify common temporal patterns.

All analyses are conducted in **R**; spatial clipping, spatial joins, and georeferencing checks were additionally performed in **QGIS**.

---

## Folder Structure

- **Geo Data/**
  - `Compuni_Lombardia.shp` – Shapefile of Lombardy municipalities
  - `Provincie.shp` – Shapefile of Lombardy provinces
  - `Data_First_Day_*.csv`, `Data_First_Weekend_*.csv`, `Data_Christmas_*.csv`, `Data_Befana_*.csv` – CSV layers exported for QGIS plotting
  - Other CSV exports used in QGIS for visualization and checks

- **results_smoothing/**
  - `f_eval_fine2.csv` – Smoothed field (pollution values per node and day)
  - `nodes_eval_fine2.csv` – Coordinates of interpolation nodes

- `aree_con_comuni.csv` – Mapping of areas to municipalities (from QGIS spatial join)
- `Data_Presenze.csv` – Preprocessed presence data used in the analyses

- **scripts/**
  - `PreProcessingDataTurismo.R` – Script 1: preprocessing of presence data
  - `Exposure_1.R` – Script 2: pilot exposure visualization (municipal and provincial)
  - `Exposure_Full.R` – Script 3+4: full time-series analysis and Functional PCA

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

---

## Notes

- The raw file `Turismo_Presenza.csv` is **not** included due to size constraints. The analyses rely on the preprocessed file `Data_Presenze.csv`.
- QGIS was used to perform **spatial clipping**, **spatial joins**, and **georeferencing checks** for area layers.
- Functional PCA is applied at the **provincial level** to summarize temporal patterns of exposure.

---

## Suggested Workflow for Reviewers

1. Run **PreProcessingDataTurismo.R** to build the cleaned dataset (`Data_Presenze.csv`) and pilot CSVs for QGIS.
2. Run **Exposure_1.R** to verify the pilot exposure maps and aggregation logic.
3. Run **Exposure_Full.R** to generate the full daily series, perform Functional PCA, and produce all figures used in the report.

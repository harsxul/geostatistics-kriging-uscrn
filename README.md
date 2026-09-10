# Geospatial interpolation of near-surface air temperature (USCRN 2015)

Mini-project for GNR640 Geospatial Statistics, IIT Bombay, Spring 2025 (Prof. Karthikeyan Lanka).
Group project (team of four); the R scripts and notebook here are my own contribution.

Daily air-temperature data from the US Climate Reference Network (2015) is used to fit experimental variograms
(omnidirectional, directional, detrended), compare variogram models, and interpolate a temperature surface by
ordinary and universal kriging, with cross-validation error analysis.

## Contents

- `variogram semiver.R` – variogram estimation and model comparison (gstat)
- `OK_error.R` – ordinary kriging and error evaluation
- `decorative.R` – publication plots (sf, ggplot2)
- `GNR_640_Mini_Project.ipynb` – Python version (pykrige, scikit-gstat)
- `USCRN_2015/` – data download scripts and the processed CSVs (raw station files removed for size)

Paths inside the R scripts point to the original author's machine; change `setwd()` before running.

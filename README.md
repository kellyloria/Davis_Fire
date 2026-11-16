# MS: Hydrologic Variability and Water Quality Responses Following Wildfire in Semi-Arid Mountain Catchments 

## Data cleaning
- [Data cleaning scripts](https://github.com/kellyloria/Davis_Fire/tree/main/Data_cleaning_scripts) for water chemistry, algal biomass, canopy cover, and YSI observations.

## Manuscript analyses

### 01. Hydroclimate and water quality
[Hydro_clim_context_01.md](Hydro_clim_context_01.md)

- Characterizes seasonal flow regimes, precipitation events, and water quality sampling (Figures 2–4).
- Evaluates differences in solute concentrations among streams and sampling locations using ANOVA.
- Generates summary statistics and supporting water chemistry tables.

### 02. Double mass curve analysis
[DMC_residual_panels_02.md](DMC_residual_panels_02.md)

- Estimates cumulative water volumes and solute loads for Winters USGS and Ophir Creek.
- Uses double mass curves (DMCs) to characterize post-fire solute transport dynamics (Figures 5–6).
- Evaluates temporal departures from proportional solute transport using DMC residuals.

### 03. Bayesian regression analysis
[Bayesian_regression_03.Rmd](Bayesian_regression_03.Rmd)

- Fits left-censored lognormal Bayesian regression models of post-fire solute concentrations.
- Evaluates the relative importance of time since fire, observed discharge, and antecedent precipitation.
- Summarizes posterior estimates, credible intervals, and model diagnostics.

## Input datasets

All three manuscript analyses use the same datasets located in `Dryad_data/`:

- `DFMS_water_quality_data_2026.csv` — Water chemistry observations and laboratory detection-limit metadata.
- `DFMS_hydro_clim_WY2024_2026.csv` — Daily precipitation, observed discharge, and functional flow classifications for water years 2024–2026.

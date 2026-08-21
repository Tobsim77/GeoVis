# GeoVis Optimized

Improved copy of the original GeoVis Shiny app.

## Focus of this version

1. API efficiency
2. Clearer structure
3. Better perceived speed
4. More coherent design

## What changed

- API metadata and CSV responses are cached in `GeoVis_Optimized/cache`.
- Data fetching, transformation, and plotting logic are separated into `R/helpers.R` and `R/analysis.R`.
- The overview tab reuses one hourly dataset for both recent and history views.
- The expensive monthly elevation analysis is loaded lazily and cached for one week.
- The UI is reorganized into three clearer sections: overview, trends, and altitude/terrain.

## Run

Open the `GeoVis_Optimized` folder as the app directory and start `app.R`.

The app expects the existing GeoVis `.rds` terrain files to stay either:

- in the same folder as this optimized copy, or
- one folder above it, which matches the current repository layout

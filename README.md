# California Ocean & Coastal Monitoring Inventory

An interactive Leaflet map of California ocean and coastal monitoring programs within 12 nautical miles of the coast. Displays monitoring coverage as hex grid cells at 1/3/5 km resolutions, survey transects, discharger/WWTP stations, monitoring gaps, and wind energy area overlays.

---

## Repository Structure

```
cal-ocean-coastal-monitoring-map/
├── .github/workflows/
│   └── pages.yml                  # Auto-deploy web/ to GitHub Pages on push to main
├── R/                             # build pipeline (inputs, not served)
│   ├── build_program_code.R       # Process one monitoring program → hex GeoJSON
│   ├── build_discharger_code.R    # Process discharger CSVs → point GeoJSON
│   ├── build_combine_code.R       # Combine all layers → Master_Inventory GeoJSONs
│   ├── remove_inland_hexes.R      # Drop hexes far inland (river beaches, bad coordinates)
│   ├── build_asbs_layer.R         # Clip Master_Inventory hexes to ASBS areas
│   ├── build_mpa_nms_layer.R      # Clip Master_Inventory hexes to MPAs and sanctuaries
│   └── build_gaps_code.R          # Generate monitoring gap hex cells
├── WEA/
│   └── CA_Wind.shp                # BOEM wind energy area shapefile (+ sidecar files)
├── ca_state/
│   └── CA_State.shp               # CA boundary shapefile (+ sidecar files)
├── Attribute_Table.csv
├── web/                           # ← published static site (served root)
│   ├── index.html                 # Interactive map
│   ├── Dischargers/
│   │   └── Dischargers.geojson
│   ├── CHIS/
│   │   └── CHIS_polygons.geojson
│   ├── CA_Wind_WEA.geojson
│   ├── California_MPA_polygons.geojson
│   ├── Master_Inventory_1km.geojson.gz
│   ├── Master_Inventory_3km.geojson.gz
│   ├── Master_Inventory_5km.geojson.gz
│   ├── Master_WEA_1km.geojson
│   ├── Master_WEA_3km.geojson
│   ├── Master_WEA_5km.geojson
│   ├── monitoring_gaps.geojson.gz
│   ├── gap_stats.json
│   ├── transects.csv
│   └── gebco_compressed.tif       ← Download separately (see Prerequisites)
└── README.md
```

All runtime data the map fetches lives in `web/` alongside `index.html`, and every fetch
in `index.html` is **relative** (no leading `/`), so the site is portable to any URL
subpath. Build scripts (`R/`) and source shapefiles (`WEA/`, `ca_state/`) stay at the
repo root as build inputs and are not deployed.

---

## Prerequisites

**R packages:**
```r
install.packages(c("readr", "dplyr", "tidyr", "stringr", "purrr",
                   "sf", "terra", "janitor", "tidyverse"))
```

**Input data not included in this repo:**
- Monitoring program CSVs in per-program folders
- Discharger monitoring CSVs
- GEBCO 2025 bathymetry GeoTIFF — download from [GEBCO](https://www.gebco.net/data_and_products/gridded_bathymetry_data/)

---

## Local setup

The build scripts don't hard-code any folder paths. They read three folder
locations from your personal `.Renviron` file, which lives on your computer and
is never committed. Set it up once:

```r
usethis::edit_r_environ()   # or: file.edit("~/.Renviron")
```

Add these lines, using your own folder locations, then save and restart R:

```
MONITORING_DATA_DIR="C:/path/to/Monitoring Data"
MONITORING_OUTPUTS_DIR="C:/path/to/Monitoring_Outputs"
MAP_REPO_DIR="C:/path/to/2026-ucla-cal-ocean-coastal-monitoring-map"
```

- `MONITORING_DATA_DIR`: per-program input folders, `ca_state/`, `Dischargers/` and `Attribute_Table.csv`
- `MONITORING_OUTPUTS_DIR`: where the scripts write outputs (also holds the GEBCO raster and `WEA/`)
- `MAP_REPO_DIR`: this repo; only `build_asbs_layer.R` uses it, to read `web/ASBS.geojson`
- Optional: `R_TEMP_DIR` sets the temp folder `build_program_code.R` uses for large CSV reads (default `~/R_temp`)

Check the values with `Sys.getenv("MONITORING_DATA_DIR")`.

Just trying the scripts once? Skip `.Renviron` and set the folders for the current
R session only (they reset when R restarts):

```r
Sys.setenv(MONITORING_DATA_DIR    = "C:/path/to/Monitoring Data",
           MONITORING_OUTPUTS_DIR = "C:/path/to/Monitoring_Outputs",
           MAP_REPO_DIR           = "C:/path/to/2026-ucla-cal-ocean-coastal-monitoring-map")
```

The same steps are repeated in the FOLDER LOCATIONS comment at the top of each
script, and a script stops with a message naming any folder setting that is missing.

---

## How to Run

### Step 1 — Build each monitoring program layer
Set `program_folder` in USER SETTINGS at the top of `build_program_code.R` and run once per program folder. Outputs per-resolution GeoJSONs and contributes to `transects.csv`. WEA hex layers are generated automatically for programs with offshore wind energy area coverage.

Large programs can optionally be split into numbered "chunk" folders (e.g. `CalCOFI1`, `CalCOFI2`). Chunks only make each run smaller, so it's faster and less likely to run out of memory and crash; the results are the same as running the whole program folder. Build each chunk, and `build_combine_code.R` merges them back into one program.

### Step 2 — Build discharger layer
Edit USER SETTINGS in `build_discharger_code.R` and run. Outputs `Dischargers/Dischargers.geojson`.

### Step 3 — Combine everything
Run `build_combine_code.R`. Outputs `Master_Inventory_Xkm.geojson.gz` (one per resolution), `Master_WEA_Xkm.geojson`, and the combined `transects.csv`. Hexes and transects more than 13.4 miles from the shoreline are dropped here (`ZONE_MILES_FROM_SHORE`). Note that `CA_State.shp` already extends 3 nautical miles offshore (state waters), so the script measures from the shoreline, not from that boundary.

### Step 4 — Remove inland hexes
Run `remove_inland_hexes.R`. It removes hexes more than `INLAND_MAX_KM` (10 km) plus half a cell from the ocean, such as river beaches far upstream and points with bad coordinates, and keeps lagoons, bays and harbors. The removed hexes are listed in `inland_hexes_removed.csv`. The untouched combine output is saved as `Master_Inventory_Xkm_before_inland.geojson`, so you can change `INLAND_MAX_KM` or `KEEP_PROGRAMS` and rerun this step without rerunning Step 3.

### Step 5 — ASBS, MPA and sanctuary layers
Run `build_asbs_layer.R` and `build_mpa_nms_layer.R`. They clip the Master Inventory hexes to those boundaries, so rerun them whenever Steps 3 or 4 change the Master Inventory.

### Step 6 — Build gap layer (optional)
Run `build_gaps_code.R` (after Steps 3–5) to generate `monitoring_gaps.geojson.gz`, `monitoring_gap_zone.geojson.gz` (lets the map recount gaps for the programs checked) and `gap_stats.json`. Gaps use 1 km cells on every map view (1 km allows for boat drift around a sampling location). The total area is the ocean (GEBCO elevation below 0 m) within 13.4 miles of shore, in US waters, excluding San Francisco Bay and the Delta. A cell counts as monitored only if a 1 km program hex sits on it (same grid as the programs).

The build scripts write their outputs into `web/` (the published folder). When adding a
new program/discharger/gap layer, regenerate the affected files into `web/`.

### Step 7 — Serve the map locally
Serve the `web/` folder with any static server:

```bash
cd path/to/cal-ocean-coastal-monitoring-map/web
python -m http.server 8000
# Open http://localhost:8000
```

---

## Hosting

This repo is published to GitHub Pages and served under the CalCOFI org domain at:

**https://calcofi.io/2026-ucla-cal-ocean-coastal-monitoring-map/**

Deployment is automatic via `.github/workflows/pages.yml`, which uploads the `web/`
folder as the Pages artifact on every push to `main` (Pages source = "GitHub Actions").
Because all data fetches are relative, the same `web/` folder also works unchanged at
`http://localhost:8000` or under any other subpath.

---

## Map Features

- **Hex grid** — 1/3/5 km resolution, colored and patterned per program; overlapping programs shown with fill patterns
- **Filters** — filter by program, parameter, or GOOS EOV group
- **Transects** — survey cruise track lines colored per program
- **Dischargers** — WWTP/ocean discharger stations with filter panel, colored per facility
- **Bathymetry** — optional GEBCO 2025 seafloor depth layer with depth zone scale bar
- **Monitoring Gaps** — unmonitored hex cells within the 12 nmi coastal buffer with regional coverage statistics
- **Wind Energy Areas** — BOEM-designated offshore wind areas (Humboldt, Morro Bay) with per-program hex overlays
- **Polygon overlays** — program zone boundaries, Channel Islands polygons, CA MPAs
- **Popups** — parameters, frequency, platform, depth range, GEBCO seafloor depth, coordinates, program overlap
- **Legend tabs** — auto-switches to the active layer (Programs / Transects / Dischargers / Gaps / Wind Energy)
- **Resolution switching** — 1/3/5 km hex grid toggle with cached layers for instant switching

---

## Adding a New Monitoring Program

1. Place CSVs in a subfolder of the repo
2. Run `build_program_code.R` pointing to that folder
3. Run `build_combine_code.R` to regenerate `Master_Inventory_Xkm.geojson.gz`

---

## Adding a New Discharger Layer

1. Run `build_discharger_code.R` with updated folder/name settings
2. Add folder name to `discharger_folder_names` in `build_combine_code.R`
3. Add an entry to `DISCHARGER_SOURCES` in `index.html`:

```javascript
const DISCHARGER_SOURCES = [
  { path: 'Dischargers/Dischargers.geojson', label: 'Dischargers' },
  { path: 'NewLayer/NewLayer.geojson',        label: 'New Layer'  }
];
```

---

## Notes

- Hex inventory files are gzip-compressed (`.geojson.gz`); the map decompresses them in-browser via `pako`
- WEA hex files are uncompressed `.geojson` (smaller size)
- `#legend` must remain a sibling of `#welcome-modal` inside `#map` in `index.html` — nesting it inside the modal will hide the legend when the modal closes
- Program colors are pre-assigned alphabetically on load to ensure consistent coloring across resolution switches

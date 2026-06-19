# California Ocean & Coastal Monitoring Inventory

An interactive Leaflet map of California ocean and coastal monitoring programs within 12 nautical miles of the coast. Displays monitoring coverage as hex grid cells at 1/3/5 km resolutions, survey transects, discharger/WWTP stations, monitoring gaps, and wind energy area overlays.

---

## Repository Structure

```
ca-ocean-monitoring-map/
├── R/
│   ├── build_program_layer.R      # Process one monitoring program → hex GeoJSON
│   ├── build_wea_hexes_only.R     # Build WEA-specific hex GeoJSON
│   ├── build_transects_only.R     # Build transects.csv from program folders
│   ├── build_discharger_layer.R   # Process discharger CSVs → point GeoJSON
│   ├── build_combine_map.R        # Combine all layers → Master_Inventory GeoJSONs
│   └── build_gap_layer.R          # Generate monitoring gap hex cells
├── Monitoring_Outputs/
│   └── index.html                 # Interactive map 
├── README.md
└── .gitignore
```

---

## Output Folder Structure

Running the R scripts populates `Monitoring_Outputs/` alongside `index.html`:

```
Monitoring_Outputs/
├── index.html                          ← Interactive map 
├── Master_Inventory_1km.geojson.gz
├── Master_Inventory_3km.geojson.gz
├── Master_Inventory_5km.geojson.gz
├── Master_WEA_1km.geojson
├── Master_WEA_3km.geojson
├── Master_WEA_5km.geojson
├── Master_Polygons.geojson
├── transects.csv
├── monitoring_gaps.geojson
├── gap_stats.json
├── CA_Wind_WEA.geojson
├── California_MPA_polygons.geojson
├── gebco_compressed2.tif               ← Download separately (see Prerequisites)
├── Dischargers/
│   └── Dischargers.geojson
├── CHIS/
│   └── CHIS_polygons.geojson
└── [Program folders]/
    └── [Program].geojson
```

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
- CA boundary shapefile (`CA_State.shp`)
- Attribute lookup table (`Attribute_Table.csv`)
- GEBCO 2025 bathymetry GeoTIFF — download from [GEBCO](https://www.gebco.net/data_and_products/gridded_bathymetry_data/)
- BOEM wind energy area boundaries (`CA_Wind_WEA.geojson`)
- CA MPA polygons (`California_MPA_polygons.geojson`)

---

## How to Run

### Step 1 — Build each monitoring program layer
Edit USER SETTINGS at the top of `build_program_layer.R` and run once per program folder. Outputs per-resolution GeoJSONs and contributes to `transects.csv`.

### Step 2 — Build WEA hex layers (if needed)
Run `build_wea_hexes_only.R` to generate WEA-specific hex outputs (`Master_WEA_Xkm.geojson`) for programs with offshore wind energy area coverage.

### Step 3 — Build discharger layer
Edit USER SETTINGS in `build_discharger_layer.R` and run. Outputs `Dischargers/Dischargers.geojson`.

### Step 4 — Combine everything
Run `build_combine_map.R`. Outputs `Master_Inventory_Xkm.geojson.gz` (one per resolution), `Master_Polygons.geojson`, and the combined `transects.csv`.

### Step 5 — Build gap layer (optional)
Run `build_gap_layer.R` to generate `monitoring_gaps.geojson` and `gap_stats.json`.

### Step 6 — Serve the map
Open `Monitoring_Outputs/` in VS Code and launch with Live Server, or use Python:

```bash
cd path/to/Monitoring_Outputs
python -m http.server 8000
# Open http://localhost:8000
```

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

1. Place CSVs in a subfolder of `Monitoring_Outputs/`
2. Run `build_program_layer.R` pointing to that folder
3. Run `build_combine_map.R` to regenerate `Master_Inventory_Xkm.geojson.gz`

---

## Adding a New Discharger Layer

1. Run `build_discharger_layer.R` with updated folder/name settings
2. Add folder name to `discharger_folder_names` in `build_combine_map.R`
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

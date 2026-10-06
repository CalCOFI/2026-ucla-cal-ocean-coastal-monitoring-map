###############################################################################
# build_gaps_code.R
#
# Finds unmonitored coastal ocean: hex cells within the coastal zone where no
# program in the Master Inventory has a hex.
#
# Gaps use 1 km cells on every map view: a sample covers about the 1 km around
# it (allows for boat drift), so a 1 km cell with no program hex is a gap.
#
# How it works:
#   1. Build the SAME 1 km hex grid the programs use, so every gap cell sits exactly
#      where a program hex would be (no partial overlaps, no neighbour cells
#      counted as monitored).
#   2. Keep cells whose centre is
#        - within ZONE_MILES_FROM_SHORE of the shoreline (same zone the combine
#          script trims the map to),
#        - ocean: GEBCO elevation below 0 m (not inland below-sea-level land
#          like the Salton Sea or Death Valley),
#        - in US waters (between the Mexico and Oregon borders),
#        - not in San Francisco Bay / the Delta (inland estuary; left out).
#      These cells are the total area.
#   3. A cell is monitored if a 1 km program hex is on it; the rest are gaps.
#   4. For each monitored cell, keep which parameters each program measured
#      there, so the map can show gaps for one parameter or a group of them.
#   5. Flag cells in state waters (s = 1: centre inside CA_State.shp, which
#      reaches 3 nm offshore), so the map can show gaps for 0-3 nm vs beyond.
#   6. Add 1 km cells for the Wind Energy Areas (they sit outside the coastal
#      zone): z = 0, w = area name. The map counts them only when "Wind Energy
#      Areas" is picked, so the coastal zone totals don't change.
#
# INPUTS:
#   - Master_Inventory_1km.geojson (from build_combine_code.R)
#   - CA_State.shp (Census state boundary; includes the 0-3 nm state waters)
#   - GEBCO raster in MONITORING_OUTPUTS_DIR (to tell ocean from land)
#
# OUTPUTS (in MONITORING_OUTPUTS_DIR, and copied into the repo's web/ folder
# automatically; see WEB FOLDER below):
#   - monitoring_gaps.geojson.gz      gap cells (compressed; the map's fallback)
#   - monitoring_gap_zone.geojson.gz  every zone cell with the programs on it and
#                                     the parameters each one measured there
#                                     (compressed; the map recounts gaps from this
#                                     for the programs and parameters switched on)
#   - gap_stats.json                  stats shown on the map
#   - monitoring_gaps_statistics.csv  same stats as a table
#
# Run AFTER build_combine_code.R.
###############################################################################


library(tidyverse)
library(sf)
library(terra)

# =============================================================================
# USER SETTINGS 
# =============================================================================

# -----------------------------------------------------------------------------
# FOLDER LOCATIONS  (set once per computer, not in this file)
# -----------------------------------------------------------------------------
# Folder paths are not written in this script, so the public repo never has
# anyone's personal paths. They're read from your .Renviron file instead: a small
# settings file R loads every time it starts. It lives in your home folder,
# outside this repo, so it is never committed.
#
# One-time setup:
#   1. In the R console, run:  usethis::edit_r_environ()
#      (no usethis? run  file.edit("~/.Renviron")  instead)
#   2. Add these lines with YOUR folders (use forward slashes, keep the quotes):
#        MONITORING_DATA_DIR="C:/path/to/Monitoring Data"
#        MONITORING_OUTPUTS_DIR="C:/path/to/Monitoring_Outputs"
#   3. Save the file, then restart R (RStudio: Session > Restart R).
#   Check it worked:  Sys.getenv("MONITORING_DATA_DIR")
#
#   MONITORING_DATA_DIR      input data: one subfolder per program, plus
#                            ca_state/, Dischargers/ and Attribute_Table.csv
#   MONITORING_OUTPUTS_DIR   where results are written (also holds the
#                            GEBCO raster and WEA/ shapefile)
#   MONITORING_WEB_DIR       optional: the repo's web/ folder. Only needed if
#                            the script can't find it on its own (see below).
#
# Just trying it once? Skip .Renviron and run this in the console before
# sourcing the script (it lasts until R restarts):
#   Sys.setenv(MONITORING_DATA_DIR = "...", MONITORING_OUTPUTS_DIR = "...")

data_dir     <- Sys.getenv("MONITORING_DATA_DIR")
output_root  <- Sys.getenv("MONITORING_OUTPUTS_DIR")
missing <- c(MONITORING_DATA_DIR = data_dir == "", MONITORING_OUTPUTS_DIR = output_root == "")
if (any(missing))
  stop("Not set: ", paste(names(missing)[missing], collapse = ", "),
       ". See FOLDER LOCATIONS at the top of this script, then restart R.",
       call. = FALSE)
if (!dir.exists(data_dir))
  stop("MONITORING_DATA_DIR folder not found: ", data_dir, call. = FALSE)

# -----------------------------------------------------------------------------
# WEB FOLDER  (where the map reads its files; outputs are copied here)
# -----------------------------------------------------------------------------
# Looked for in this order:
#   1. MONITORING_WEB_DIR in .Renviron, if set
#   2. web/ next to this script's R/ folder (when run with source())
#   3. web/ in the working directory, or one level up (e.g. the RStudio project)
# If none is found, the script lists the files to copy by hand instead.
script_dir <- tryCatch(dirname(sys.frame(1)$ofile), error = function(e) NA_character_)
find_web_dir <- function() {
  env <- Sys.getenv("MONITORING_WEB_DIR")
  if (env != "") {
    if (dir.exists(env)) return(normalizePath(env))
    warning("MONITORING_WEB_DIR folder not found: ", env, call. = FALSE)
    return(NA_character_)
  }
  candidates <- c(if (!is.na(script_dir)) file.path(script_dir, "..", "web"), "web", file.path("..", "web"))
  for (d in candidates) if (file.exists(file.path(d, "index.html"))) return(normalizePath(d))
  NA_character_
}
web_dir <- find_web_dir()

# -----------------------------------------------------------------------------
# SETTINGS
# -----------------------------------------------------------------------------
ca_boundary_path <- file.path(data_dir, "ca_state", "CA_State.shp")
gebco_path <- file.path(output_root, "gebco_2025_n48.0_s30.0_w-130.0_e-110.0_geotiff.tif")
if (!file.exists(gebco_path)) gebco_path <- file.path(output_root, "gebco_compressed.tif")
if (!file.exists(gebco_path))
  stop("No GEBCO raster in MONITORING_OUTPUTS_DIR (needed to tell ocean from land).",
       call. = FALSE)

# How far offshore counts, measured from the shoreline.
# Must match ZONE_MILES_FROM_SHORE in build_combine_code.R.
ZONE_MILES_FROM_SHORE <- 13.4

# Must match buffer_miles in build_program_code.R. That buffer sets the corner
# of the program hex grid; using it here lines gap cells up with program hexes.
PROGRAM_BUFFER_MILES <- 13.4

# Gap cell size. 1 km allows for boat drift around a sampling location.
# (Uses the matching Master_Inventory_<size>.geojson.)
HEX_SIZES_M <- c("1km" = 1000)

if (!requireNamespace("R.utils", quietly = TRUE)) install.packages("R.utils")

# =============================================================================
# COASTAL ZONE
# =============================================================================

ca_boundary <- st_read(ca_boundary_path, quiet = TRUE) %>%
  st_transform(3310) %>%
  st_union()

# CA_State.shp already reaches 3 nautical miles offshore (state waters), so
# the zone edge is the boundary plus (zone distance - 3 nm).
STATE_WATERS_M <- 3 * 1852
coastal_zone   <- st_buffer(ca_boundary, ZONE_MILES_FROM_SHORE * 1609.34 - STATE_WATERS_M)
grid_corner    <- st_bbox(st_buffer(ca_boundary, PROGRAM_BUFFER_MILES * 1609.34))[c("xmin", "ymin")]

# San Francisco Bay + Delta, east of the Golden Gate (left out of the total).
# Drawn so Ocean Beach, Pacifica, Half Moon Bay and Tomales Bay stay in.
sf_bay_delta <- st_polygon(list(rbind(
  c(-122.477, 37.812), c(-122.425, 37.750), c(-122.420, 37.580), c(-122.340, 37.500),
  c(-122.200, 37.300),
  c(-121.000, 37.300), c(-121.000, 38.750), c(-122.560, 38.750), c(-122.560, 38.000),
  c(-122.478, 37.826), c(-122.477, 37.812)
))) %>% st_sfc(crs = 4326) %>% st_transform(3310)

gebco <- terra::rast(gebco_path)

# Region edges are the landmarks named on the map's coverage card
CAPE_MENDOCINO_LAT  <- 40.44
SANTA_CRUZ_LAT      <- 36.96
PT_CONCEPTION_LAT   <- 34.45
region_of <- function(lat) case_when(
  lat >= CAPE_MENDOCINO_LAT ~ "North Coast",          # Oregon border to Cape Mendocino
  lat >= SANTA_CRUZ_LAT     ~ "Bay Area / Central",   # Cape Mendocino to Santa Cruz
  lat >= PT_CONCEPTION_LAT  ~ "Central Coast",        # Santa Cruz to Pt. Conception
  TRUE                      ~ "Southern CA"           # Pt. Conception to Mexico
)
pct_txt <- function(x) paste0(format(round(x, 1), nsmall = 1), "%")
km2_txt <- function(x) paste0(format(round(x), big.mark = ","), " km²")

stats_table <- list()

# =============================================================================
# GAPS
# =============================================================================

for (res in names(HEX_SIZES_M)) {

  cat("\n=== Gaps at", res, "===\n")
  master_path <- file.path(output_root, paste0("Master_Inventory_", res, ".geojson"))
  if (!file.exists(master_path)) {
    cat("  ", basename(master_path), " not found; skipping.\n", sep = "")
    next
  }

  master   <- st_read(master_path, quiet = TRUE)
  prog_col <- grep("^Program[ ._]?Name$", names(master), value = TRUE)[1]
  if (is.na(prog_col)) stop("No Program Name column in ", basename(master_path),
                            "; columns are: ", paste(names(master), collapse = ", "), call. = FALSE)
  # Only program hexes count as monitored: skip any feature with no program name
  no_name <- is.na(master[[prog_col]]) | trimws(as.character(master[[prog_col]])) == ""
  cat("Program hexes read: ", sum(!no_name), " (", basename(master_path), ")\n", sep = "")
  if (any(no_name)) {
    cat("Ignored ", sum(no_name), " features with no program name. Geometry types: ",
        paste(unique(as.character(st_geometry_type(master[no_name, ]))), collapse = ", "),
        ". Rerun build_combine_code.R if this is unexpected.\n", sep = "")
  }
  param_col <- grep("^Parameters$", names(master), value = TRUE)[1]
  if (is.na(param_col)) stop("No Parameters column in ", basename(master_path), call. = FALSE)
  master <- master[!no_name, c(prog_col, param_col)] %>% st_transform(3310)

  # 1. Program hex grid, limited to cells centred in the coastal zone
  grid    <- st_make_grid(coastal_zone, cellsize = HEX_SIZES_M[[res]],
                          square = FALSE, offset = grid_corner)
  # Line the grid up exactly with the program hexes: move it by the distance
  # from each program hex centre to the nearest grid cell centre. (0 when the
  # grid corners already match; otherwise gap cells would half-overlap hexes.)
  prog_ctr <- st_centroid(st_geometry(master)[seq_len(min(500, nrow(master)))])
  grid_ctr <- st_centroid(grid)
  nearest  <- st_nearest_feature(prog_ctr, grid_ctr)
  offsets  <- st_coordinates(prog_ctr) - st_coordinates(grid_ctr[nearest])
  # Use the single most common offset. (Taking the median of x and y separately
  # can mix two equally near cells, +500 m and -500 m, and leave the gap grid
  # half a cell off the program hexes.)
  key      <- paste(round(offsets[, 1]), round(offsets[, 2]))
  shift    <- offsets[match(names(which.max(table(key))), key), ]
  if (any(abs(shift) > 0.5)) {
    grid     <- st_sfc(st_geometry(grid) + shift, crs = 3310)
    grid_ctr <- st_sfc(st_geometry(grid_ctr) + shift, crs = 3310)
    cat(sprintf("Moved gap grid by (%.0f m, %.0f m) to line up with the program hexes\n", shift[1], shift[2]))
  }
  # Check: program hex centres should now sit exactly on gap cell centres
  off_by <- as.numeric(st_distance(prog_ctr, grid_ctr[st_nearest_feature(prog_ctr, grid_ctr)], by_element = TRUE))
  if (median(off_by) > 1)
    stop(sprintf("Gap grid is %.0f m off the program hexes; check PROGRAM_BUFFER_MILES.", median(off_by)), call. = FALSE)
  centres <- st_centroid(grid)
  in_zone <- lengths(st_intersects(centres, coastal_zone)) > 0
  grid    <- grid[in_zone]
  centres <- centres[in_zone]

  # 2. Ocean cells in US waters, outside SF Bay / Delta
  ll        <- st_coordinates(st_transform(centres, 4326))
  elevation <- terra::extract(gebco, ll)[, 1]
  keep <- !is.na(elevation) & elevation < 0 &
    ll[, 2] >= 32.534 & ll[, 2] <= 42.0 &   # Mexico border to Oregon border
    # below-sea-level land inland (Salton Sea, Imperial Valley, Death Valley):
    !(ll[, 1] > -117.1 | (ll[, 2] > 35.5 & ll[, 1] > -120.5)) &
    lengths(st_intersects(centres, sf_bay_delta)) == 0
  grid    <- grid[keep]
  centres <- centres[keep]
  lat     <- ll[keep, 2]
  # state waters = centre inside CA_State.shp (it already reaches 3 nm offshore)
  in_state <- lengths(st_intersects(centres, ca_boundary)) > 0

  # 3. Monitored = a program hex of the same size is on the cell
  hits      <- st_intersects(centres, master)
  monitored <- lengths(hits) > 0

  # ---- statistics
  cell_km2  <- as.numeric(st_area(grid[1])) / 1e6
  total_km2 <- length(grid) * cell_km2
  gap_km2   <- sum(!monitored) * cell_km2
  mon_km2   <- total_km2 - gap_km2

  regional <- tibble(region = region_of(lat), gap = !monitored) %>%
    group_by(region) %>%
    summarise(total_cells = n(), gap_cells = sum(gap),
              pct_unmonitored = 100 * gap_cells / total_cells, .groups = "drop")

  cat("Total zone area:  ", km2_txt(total_km2), " (", length(grid), " cells)\n", sep = "")
  cat("Monitored:        ", km2_txt(mon_km2), " (", pct_txt(100 * mon_km2 / total_km2), ")\n", sep = "")
  cat("Unmonitored (gap):", km2_txt(gap_km2), " (", pct_txt(100 * gap_km2 / total_km2), ")\n", sep = "")
  print(regional)

  # ---- gap cells for the map
  gaps_sf <- st_sf(region = region_of(lat[!monitored]), geometry = grid[!monitored]) %>%
    st_transform(4326)
  gaps_path <- file.path(output_root, "monitoring_gaps.geojson")
  st_write(gaps_sf, gaps_path, delete_dsn = TRUE, quiet = TRUE,
           layer_options = "COORDINATE_PRECISION=5")
  R.utils::gzip(gaps_path, destname = paste0(gaps_path, ".gz"),
                overwrite = TRUE, remove = FALSE)

  # ---- every zone cell, with the programs on it, so the map can recount gaps
  #      for just the programs and parameters switched on
  #      r = region 1-4 (north to south), a = cell area km², p = programs ("" = gap)
  #      q = parameters each program measured on the cell, in the same order as p:
  #          programs split by "|", parameters by ";"
  #          e.g. p = "CalCOFI;SCCOOS", q = "Salinity;Temperature|Chlorophyll-a;Temperature"
  #      s = 1 in state waters (0-3 nm), 0 beyond
  #      z = 1 coastal zone cell; 0 = Wind Energy Area cell outside the zone (w = its name)
  region_num  <- c("North Coast" = 1, "Bay Area / Central" = 2, "Central Coast" = 3, "Southern CA" = 4)
  # p and q for each cell from the program hexes on it
  cell_pq <- function(hits, prog_names, prog_params) {
    cp <- character(length(hits)); cq <- character(length(hits))
    for (k in which(lengths(hits) > 0)) {
      i     <- hits[[k]]
      progs <- sort(unique(prog_names[i]))
      cp[k] <- paste(progs, collapse = ";")
      cq[k] <- paste(vapply(progs, function(pr) {
        x <- trimws(unlist(prog_params[i[prog_names[i] == pr]]))
        paste(sort(unique(x[!is.na(x) & x != ""])), collapse = ";")
      }, character(1)), collapse = "|")
    }
    list(p = cp, q = cq)
  }
  prog_names  <- as.character(master[[prog_col]])
  prog_params <- strsplit(as.character(master[[param_col]]), ";")   # one list of names per program hex
  if (any(grepl("|", unlist(prog_params), fixed = TRUE)))
    stop("A parameter name contains '|', which the zone file uses as a separator.", call. = FALSE)
  pq <- cell_pq(hits, prog_names, prog_params)
  cell_progs  <- pq$p
  cell_params <- pq$q
  # every monitored cell must name at least one program, or the map would show it as a gap
  if (sum(cell_progs != "") != sum(monitored))
    stop("Zone export: ", sum(monitored), " monitored cells but only ", sum(cell_progs != ""),
         " have program names. Check the '", prog_col, "' column.", call. = FALSE)
  no_params <- sum(cell_progs != "" & gsub("|", "", cell_params, fixed = TRUE) == "")
  if (no_params > 0)
    cat("Note:", no_params, "monitored cells have no parameters listed; they count as monitored",
        "only when no parameter is checked on the map.\n")
  zone_sf <- st_sf(r = unname(region_num[region_of(lat)]), a = round(cell_km2, 4),
                   p = cell_progs, q = cell_params, s = as.integer(in_state), z = 1L, w = "",
                   geometry = grid) %>%
    st_transform(4326)

  # ---- Wind Energy Area cells (outside the coastal zone, so added separately)
  wea_shp    <- file.path(output_root, "WEA", "CA_Wind.shp")
  wea_master <- file.path(output_root, paste0("Master_WEA_", res, ".geojson"))
  if (file.exists(wea_shp)) {
    wea_area <- st_read(wea_shp, quiet = TRUE) %>% st_transform(3310) %>% st_make_valid() %>% st_union()
    # Start the grid just south-west of the WEAs but on the same lattice as the
    # program hexes (a pointy-top hex grid repeats every cellsize across and
    # every sqrt(3) * cellsize up), so WEA cells sit exactly on program hexes.
    size   <- HEX_SIZES_M[[res]]
    wbb    <- st_bbox(wea_area)
    period <- c(size, sqrt(3) * size)
    w_corner <- grid_corner + floor((c(wbb[["xmin"]], wbb[["ymin"]]) - grid_corner) / period) * period - period
    wgrid  <- st_make_grid(wea_area, cellsize = size, square = FALSE, offset = w_corner)
    if (any(abs(shift) > 0.5)) wgrid <- st_sfc(st_geometry(wgrid) + shift, crs = 3310)
    wctr   <- st_centroid(wgrid)
    in_wea <- lengths(st_intersects(wctr, wea_area)) > 0
    wgrid  <- wgrid[in_wea]
    wctr   <- wctr[in_wea]
    wlat   <- st_coordinates(st_transform(wctr, 4326))[, 2]
    wp <- character(length(wgrid)); wq <- character(length(wgrid))
    if (file.exists(wea_master)) {
      wm   <- st_read(wea_master, quiet = TRUE)
      wpc  <- grep("^Program[ ._]?Name$", names(wm), value = TRUE)[1]
      wqc  <- grep("^Parameters$", names(wm), value = TRUE)[1]
      wm   <- wm[!is.na(wm[[wpc]]) & trimws(as.character(wm[[wpc]])) != "", c(wpc, wqc)] %>% st_transform(3310)
      wpq  <- cell_pq(st_intersects(wctr, wm), as.character(wm[[wpc]]), strsplit(as.character(wm[[wqc]]), ";"))
      wp <- wpq$p; wq <- wpq$q
    } else {
      cat("Note: ", basename(wea_master), " not found, so every Wind Energy Area cell counts as a gap.\n", sep = "")
    }
    wea_sf <- st_sf(r = unname(region_num[region_of(wlat)]), a = round(cell_km2, 4), p = wp, q = wq,
                    s = 0L, z = 0L,
                    w = ifelse(wlat > 38, "Humboldt Wind Energy Area", "Morro Bay Wind Energy Area"),
                    geometry = wgrid) %>%
      st_transform(4326)
    cat("Wind Energy Area cells: ", length(wgrid), " (", sum(wp != ""), " monitored)\n", sep = "")
    zone_sf <- rbind(zone_sf, wea_sf)
  } else {
    cat("Note: WEA/CA_Wind.shp not found in MONITORING_OUTPUTS_DIR; no Wind Energy Area cells added.\n")
  }
  # Named without "_1km" and kept only as .gz, so build_combine_code.R never
  # mistakes it for a program output and merges it into the Master Inventory
  zone_path <- file.path(output_root, "monitoring_gap_zone.geojson")
  st_write(zone_sf, zone_path, delete_dsn = TRUE, quiet = TRUE,
           layer_options = "COORDINATE_PRECISION=5")
  R.utils::gzip(zone_path, destname = paste0(zone_path, ".gz"),
                overwrite = TRUE, remove = TRUE)
  # clean up the old name from earlier runs
  old_zone <- file.path(output_root, c("monitoring_zone_1km.geojson", "monitoring_zone_1km.geojson.gz"))
  invisible(file.remove(old_zone[file.exists(old_zone)]))
  cat("Zone cells for the live map: ", length(grid), " (", sum(in_state), " in state waters), plus ",
      nrow(zone_sf) - length(grid), " Wind Energy Area cells (", basename(zone_path), ".gz)\n", sep = "")

  # ---- stats shown on the map
  region_pct <- function(r) {
    v <- regional$pct_unmonitored[regional$region == r]
    if (length(v) == 0) "0.0%" else pct_txt(v)
  }
  jsonlite::write_json(list(
    res       = res,
    total     = km2_txt(total_km2),
    monitored = km2_txt(mon_km2),
    gap       = km2_txt(gap_km2),
    pct_mon   = pct_txt(100 * mon_km2 / total_km2),
    pct_gap   = pct_txt(100 * gap_km2 / total_km2),
    r1 = region_pct("North Coast"),
    r2 = region_pct("Bay Area / Central"),
    r3 = region_pct("Central Coast"),
    r4 = region_pct("Southern CA")
  ), file.path(output_root, "gap_stats.json"), auto_unbox = TRUE)

  stats_table[[res]] <- tibble(
    resolution = res,
    total_km2 = round(total_km2), monitored_km2 = round(mon_km2), gap_km2 = round(gap_km2),
    pct_monitored = round(100 * mon_km2 / total_km2, 1),
    pct_unmonitored = round(100 * gap_km2 / total_km2, 1),
    gap_cells = sum(!monitored)
  )
}

bind_rows(stats_table) %>%
  write_csv(file.path(output_root, "monitoring_gaps_statistics.csv"))
cat("\nWritten: monitoring_gaps.geojson(.gz), monitoring_gap_zone.geojson.gz,",
    "gap_stats.json, monitoring_gaps_statistics.csv\n")
print(bind_rows(stats_table))

# =============================================================================
# COPY TO THE MAP (web/)
# =============================================================================
web_files <- file.path(output_root, c("monitoring_gap_zone.geojson.gz",
                                      "monitoring_gaps.geojson.gz",
                                      "gap_stats.json"))
web_files <- web_files[file.exists(web_files)]
if (!is.na(web_dir)) {
  copied <- file.copy(web_files, web_dir, overwrite = TRUE)
  old_web <- file.path(web_dir, "monitoring_zone_1km.geojson.gz")
  if (file.exists(old_web)) file.remove(old_web)
  cat("\nCopied to ", web_dir, ":\n", paste0("  ", basename(web_files[copied]), collapse = "\n"), "\n", sep = "")
  if (any(!copied)) cat("Could not copy:", basename(web_files[!copied]), "\n")
} else {
  cat("\nweb/ folder not found, so nothing was copied. Copy these into the repo's web/ folder:\n",
      paste0("  ", web_files, collapse = "\n"),
      "\n(Or add MONITORING_WEB_DIR=\"C:/path/to/repo/web\" to .Renviron so this happens automatically.)\n",
      sep = "")
}

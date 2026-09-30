###############################################################################
# remove_inland_hexes.R
#
# Removes program hexes that sit on land well away from the ocean (river
# beaches far upstream, bad coordinates) from the combined Master Inventory
# files, without rerunning any program folders.
#
# A hex is removed when its centre is on land and more than
#   INLAND_MAX_KM + half a cell
# from the ocean (so a 3 km or 5 km hex that still reaches the coast is kept).
# Ocean = GEBCO below 0 m, leaving out inland below-sea-level areas (Salton Sea,
# Imperial Valley, Death Valley), same as build_gaps_code.R.
#
# INPUTS  (MONITORING_OUTPUTS_DIR): Master_Inventory_<res>.geojson, GEBCO raster
# OUTPUTS (MONITORING_OUTPUTS_DIR): the same files with inland hexes removed,
#          plus their .geojson.gz copies, and inland_hexes_removed.csv
#          The .gz files are also copied into the repo's web/ folder.
#          Master_Inventory_<res>_before_inland.geojson keeps the untouched
#          combine output, so you can change the settings and rerun this
#          script without rerunning build_combine_code.R.
#
# Run AFTER build_combine_code.R and BEFORE build_asbs_layer.R,
# build_mpa_nms_layer.R and build_gaps_code.R.
# Safe to run more than once (each run starts from the untouched copy).
###############################################################################

library(tidyverse)
library(sf)
library(terra)

output_root <- Sys.getenv("MONITORING_OUTPUTS_DIR")
if (output_root == "")
  stop("MONITORING_OUTPUTS_DIR not set. See FOLDER LOCATIONS in build_gaps_code.R.", call. = FALSE)

# How far inland (km) a hex may reach before it is removed. GEBCO cells are
# ~450 m, so it misses lagoons, small bays and harbour channels (Mission Bay,
# Upper Newport Bay, Batiquitos Lagoon read as land up to ~8 km from "ocean").
# 10 km keeps those and tidal channels, and still removes upstream river beaches
# (Russian River at Guerneville and above) and points with bad coordinates.
INLAND_MAX_KM <- 10

# Programs never removed here, e.g. c("SWRCB") to keep every BeachWatch site,
# including freshwater river beaches.
KEEP_PROGRAMS <- character(0)

RESOLUTIONS <- c("1km" = 1, "3km" = 3, "5km" = 5)

gebco_path <- file.path(output_root, "gebco_2025_n48.0_s30.0_w-130.0_e-110.0_geotiff.tif")
if (!file.exists(gebco_path)) gebco_path <- file.path(output_root, "gebco_compressed.tif")
if (!file.exists(gebco_path)) stop("No GEBCO raster in MONITORING_OUTPUTS_DIR.", call. = FALSE)
gebco <- terra::rast(gebco_path)

if (!requireNamespace("R.utils", quietly = TRUE)) install.packages("R.utils")

# web/ folder (same search as build_gaps_code.R)
script_dir <- tryCatch(dirname(sys.frame(1)$ofile), error = function(e) NA_character_)
find_web_dir <- function() {
  env <- Sys.getenv("MONITORING_WEB_DIR")
  if (env != "") return(if (dir.exists(env)) normalizePath(env) else NA_character_)
  candidates <- c(if (!is.na(script_dir)) file.path(script_dir, "..", "web"), "web", file.path("..", "web"))
  for (d in candidates) if (file.exists(file.path(d, "index.html"))) return(normalizePath(d))
  NA_character_
}
web_dir <- find_web_dir()

# Distance (km) from each point to the nearest ocean cell, checking rings every
# 0.5 km in 24 directions. Returns Inf when no ocean is found within max_km.
km_to_ocean <- function(lon, lat, max_km) {
  is_ocean <- function(x, y) {
    e <- terra::extract(gebco, cbind(x, y))[, 1]
    !is.na(e) & e < 0 & !(x > -117.1 | (y > 35.5 & x > -120.5))
  }
  d    <- ifelse(is_ocean(lon, lat), 0, Inf)
  todo <- which(is.infinite(d))
  ang  <- seq(0, 345, by = 15) * pi / 180
  for (r in seq(0.5, max_km, by = 0.5)) {
    if (length(todo) == 0) break
    lon_t <- rep(lon[todo], each = length(ang))
    lat_t <- rep(lat[todo], each = length(ang))
    xs    <- lon_t + r / (111 * cos(lat_t * pi / 180)) * sin(ang)
    ys    <- lat_t + r / 111 * cos(ang)
    found <- colSums(matrix(is_ocean(xs, ys), nrow = length(ang))) > 0
    d[todo[found]] <- r
    todo <- todo[!found]
  }
  d
}

# Remove features from a GeoJSON by editing its text, so everything else in the
# file (column names, number formats, feature order) stays exactly as
# build_combine_code.R wrote it. GDAL writes one feature per line.
write_without <- function(path, drop) {
  # Read and write raw bytes so characters like ° and µ are never re-encoded.
  lines <- readLines(path, warn = FALSE)
  is_feat <- grepl('^\\s*\\{ *"type": *"Feature"', lines, useBytes = TRUE)
  if (sum(is_feat) != length(drop))
    stop(basename(path), ": expected one feature per line (", length(drop), " features, ",
         sum(is_feat), " feature lines). File left unchanged.", call. = FALSE)
  feats <- sub(",\\s*$", "", lines[is_feat], useBytes = TRUE)[!drop]
  first <- which(is_feat)[1]; last <- tail(which(is_feat), 1)
  out <- c(lines[seq_len(first - 1)],
           if (length(feats)) paste0(feats, c(rep(",", length(feats) - 1), "")),
           lines[seq(last + 1, length(lines))])
  con <- file(path, "wb"); writeLines(out, con, sep = "\n", useBytes = TRUE); close(con)
}

removed_all <- list()

for (res in names(RESOLUTIONS)) {
  path <- file.path(output_root, paste0("Master_Inventory_", res, ".geojson"))
  if (!file.exists(path)) { cat(basename(path), "not found; skipping.\n"); next }

  # Start from the untouched combine output. If the file still matches what this
  # script wrote last time, restore the saved copy; otherwise it is a fresh
  # build_combine_code.R output, so save it as the new untouched copy.
  backup <- file.path(output_root, paste0("Master_Inventory_", res, "_before_inland.geojson"))
  stamp  <- paste0(backup, ".md5")
  ours   <- file.exists(backup) && file.exists(stamp) &&
            identical(unname(tools::md5sum(path)), readLines(stamp, warn = FALSE)[1])
  if (ours) {
    file.copy(backup, path, overwrite = TRUE)
    cat("\n", res, ": starting from the saved combine output\n", sep = "")
  } else {
    file.copy(path, backup, overwrite = TRUE)
    if (file.exists(stamp)) file.remove(stamp)
  }

  hexes <- st_read(path, quiet = TRUE)
  lat   <- as.numeric(hexes[["Centroid Latitude"]])
  lon   <- as.numeric(hexes[["Centroid Longitude"]])
  if (all(is.na(lat))) {  # fall back to the geometry if the columns are missing
    xy  <- st_coordinates(st_centroid(st_geometry(hexes)))
    lon <- xy[, 1]; lat <- xy[, 2]
  }

  prog_col <- grep("^Program[ ._]?Name$", names(hexes), value = TRUE)[1]
  keep     <- if (is.na(prog_col) || !length(KEEP_PROGRAMS)) rep(FALSE, nrow(hexes)) else
              sapply(strsplit(as.character(hexes[[prog_col]]), ";\\s*"),
                     function(p) any(trimws(p) %in% KEEP_PROGRAMS))

  limit <- INLAND_MAX_KM + RESOLUTIONS[[res]] / 2
  dist  <- km_to_ocean(lon, lat, limit + 0.5)
  drop  <- !is.na(dist) & dist > limit & !keep

  cat("\n", res, ": ", sum(drop), " of ", nrow(hexes), " hexes more than ", limit,
      " km from the ocean\n", sep = "")
  if (sum(drop) > 0) {
    removed <- tibble(resolution = res,
                      program    = if (is.na(prog_col)) NA_character_ else hexes[[prog_col]][drop],
                      centroid_lat = round(lat[drop], 4), centroid_lon = round(lon[drop], 4),
                      km_from_ocean = ifelse(is.infinite(dist[drop]), NA, dist[drop]))
    print(count(removed, program, sort = TRUE), n = Inf)
    removed_all[[res]] <- removed
    write_without(path, drop)
  }

  # Always refresh the stamp and .gz, in case the file was restored above.
  writeLines(unname(tools::md5sum(path)), stamp)
  R.utils::gzip(path, destname = paste0(path, ".gz"), overwrite = TRUE, remove = FALSE)
  if (!is.na(web_dir)) file.copy(paste0(path, ".gz"), web_dir, overwrite = TRUE)
}

removed_tbl <- if (length(removed_all)) bind_rows(removed_all) else
  tibble(resolution = character(), program = character(), centroid_lat = numeric(),
         centroid_lon = numeric(), km_from_ocean = numeric())
write_csv(removed_tbl, file.path(output_root, "inland_hexes_removed.csv"))
cat("\nList of removed hexes: inland_hexes_removed.csv\n")
if (is.na(web_dir)) {
  cat("web/ folder not found: copy the Master_Inventory_*.geojson.gz files into web/ by hand.\n")
} else {
  cat("Updated Master_Inventory_*.geojson.gz copied to", web_dir, "\n")
}
cat("Now run build_asbs_layer.R, build_mpa_nms_layer.R, then build_gaps_code.R.\n")

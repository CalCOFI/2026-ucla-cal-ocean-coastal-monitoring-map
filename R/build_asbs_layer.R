###################################################################################
# build_asbs_layer.R
#
# Produces the ASBS hex layer (Master_ASBS_1km/3km/5km.geojson.gz) by clipping
# the already-built Master_Inventory hex files to the ASBS boundary polygons —
# a centroid-in-polygon filter, no per-program reprocessing needed (unlike WEA,
# ASBS sits inside the coastal buffer so every hex that could match is already
# in Master_Inventory).
#
# Run AFTER build_combine_code.R (needs Master_Inventory_<res>.geojson present
# in output_root).
###################################################################################

library(tidyverse)
library(sf)

# =============================================================================
# USER SETTINGS — adjust paths if yours differ
# =============================================================================

output_root      <- "C:/Users/bhuan/Downloads/Monitoring_Outputs"
asbs_boundary_path <- "C:/Users/bhuan/Documents/calcofi-work/ASBS.geojson"

HEX_RESOLUTIONS <- c("1km", "3km", "5km")

# =============================================================================
# LOAD ASBS BOUNDARY
# =============================================================================

cat("Loading ASBS boundary polygons...\n")
asbs_boundary <- st_read(asbs_boundary_path, quiet = TRUE) %>%
  st_make_valid() %>%
  st_union()
cat("ASBS boundary ready.\n")

# =============================================================================
# CLIP EACH RESOLUTION'S MASTER INVENTORY TO ASBS
# =============================================================================

for (res in HEX_RESOLUTIONS) {

  cat("\n--- Resolution:", res, "---\n")

  master_path <- file.path(output_root, paste0("Master_Inventory_", res, ".geojson"))
  if (!file.exists(master_path)) {
    cat("  Master_Inventory_", res, ".geojson not found — skipping.\n", sep = "")
    next
  }

  master_sf <- st_read(master_path, quiet = TRUE) %>% st_make_valid()

  # Use hex centroid (from geometry, not a stored column) for the point-in-
  # polygon test, so this works regardless of which centroid columns a given
  # rebuild happens to carry
  centroids <- st_centroid(master_sf)

  inside <- st_intersects(centroids, asbs_boundary, sparse = FALSE)[, 1]
  asbs_sf <- master_sf[inside, ]

  cat("  ", sum(inside), "of", nrow(master_sf), "hexes fall inside ASBS boundaries.\n", sep = "")

  if (nrow(asbs_sf) == 0) {
    cat("  No hexes inside ASBS at this resolution — skipping write.\n")
    next
  }

  out_geojson <- file.path(output_root, paste0("Master_ASBS_", res, ".geojson"))
  out_gz      <- paste0(out_geojson, ".gz")

  suppressWarnings(st_write(asbs_sf, out_geojson, delete_dsn = TRUE, quiet = TRUE))

  if (!requireNamespace("R.utils", quietly = TRUE)) install.packages("R.utils")
  library(R.utils)
  if (file.exists(out_gz)) file.remove(out_gz)
  gzip(out_geojson, destname = out_gz, remove = TRUE)  # remove=TRUE deletes the plain .geojson after gzipping

  cat("  Written:", basename(out_gz), "(", nrow(asbs_sf), "features)\n", sep = "")
}

cat("\n=== ASBS LAYER BUILD COMPLETE ===\n")

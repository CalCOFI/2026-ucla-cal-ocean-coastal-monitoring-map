###################################################################################
# build_mpa_nms_layer.R
#
# Produces two new hex-clipped layers, following the same pattern as
# build_asbs_layer.R: a centroid-in-polygon filter on the already-built
# Master_Inventory hex files — no per-program reprocessing needed.
#
#   1) MPAs — California Marine Protected Areas, OPEN COAST ONLY.
#      Excludes the San Francisco Bay Study Region (CDFW's own "SFBSR" field)
#      plus named estuary/lagoon/slough MPAs elsewhere in the state (that
#      field doesn't exist, so these are excluded by name — see
#      MPA_ESTUARY_EXCLUDE below; review/edit that list before running).
#      Source: CDFW California Marine Protected Areas [ds582]
#      https://data-cdfw.opendata.arcgis.com/datasets/117a99c8745a48c6a48bac70005b1b11_0
#
#   2) NMS — the 4 California National Marine Sanctuaries (Channel Islands,
#      Chumash Heritage, Greater Farallones, Monterey Bay).
#      Source: NOAA Office of National Marine Sanctuaries boundary service
#      https://sanctuaries.noaa.gov/library/imast_gis.html
#
# Outputs (in output_root):
#   MPA.geojson, NMS.geojson                          — boundary polygons
#   Master_MPA_1km/3km/5km.geojson.gz                  — hex-clipped overlap
#   Master_NMS_1km/3km/5km.geojson.gz                  — hex-clipped overlap
#
# Run AFTER build_combine_code.R (needs Master_Inventory_<res>.geojson present
# in output_root). Requires internet access to query the two live REST
# services below; if that fails, download the GeoJSON manually from the
# source pages above and point MPA_SOURCE / NMS_SOURCE at the local file.
###################################################################################

library(tidyverse)
library(sf)

# =============================================================================
# USER SETTINGS — adjust paths if yours differ
# =============================================================================

output_root <- "C:/Users/bhuan/Downloads/Monitoring_Outputs"

HEX_RESOLUTIONS <- c("1km", "3km", "5km")

# Live ArcGIS REST query URLs — sf::st_read can read these directly as GeoJSON.
# If your network blocks direct REST access, download the GeoJSON by hand from
# the source pages in the header comment and set these to local file paths
# instead.
MPA_SOURCE <- "https://services2.arcgis.com/Uq9r85Potqm3MfRV/arcgis/rest/services/biosds582_fpu/FeatureServer/0/query?where=1%3D1&outFields=*&f=geojson"
NMS_SOURCE <- "https://services2.arcgis.com/C8EMgrsFcRFL6LrL/arcgis/rest/services/NMS_Boundaries_02032022/FeatureServer/0/query?where=1%3D1&outFields=*&f=geojson"

# MPAs to exclude beyond the SF Bay Study Region (SFBSR) — enclosed
# bays/lagoons/sloughs/estuaries elsewhere in the state. CDFW's dataset has
# no bay/estuary flag field, so this is a name-based list; review it against
# your report's Criterion 3 before running, and edit as needed.
MPA_ESTUARY_EXCLUDE <- c(
  "Ten Mile Estuary SMCA",
  "South Humboldt Bay SMRMA",
  "Big River Estuary SMCA",
  "Navarro River Estuary SMCA",
  "Russian River SMRMA",
  "Russian River SMCA",
  "Estero Americano SMRMA",
  "Estero de San Antonio SMRMA",
  "Estero de Limantour SMR",
  "Drakes Estero SMCA",
  "Elkhorn Slough SMR",
  "Elkhorn Slough SMCA",
  "Moro Cojo Slough SMR",
  "Morro Bay SMRMA",
  "Morro Bay SMR",
  "Goleta Slough SMCA (No-Take)",
  "Batiquitos Lagoon SMCA (No-Take)",
  "San Elijo Lagoon SMCA (No-Take)",
  "San Dieguito Lagoon SMCA",
  "Famosa Slough SMCA (No-Take)",
  "Tijuana River Mouth SMCA",
  "Bolsa Bay SMCA",
  "Bolsa Chica Basin SMCA (No-Take)",
  "Cat Harbor SMCA",
  "Upper Newport Bay SMCA"
)

# The 4 California sanctuaries — matched by substring so exact NMS_Name
# punctuation/wording doesn't have to be guessed.
NMS_INCLUDE_PATTERNS <- c("Channel Islands", "Chumash", "Greater Farallones", "Monterey Bay")

# =============================================================================
# STEP 1 — FETCH & FILTER BOUNDARIES
# =============================================================================

cat("Fetching MPA boundaries...\n")
mpa_all <- st_read(MPA_SOURCE, quiet = TRUE) %>% st_make_valid()
cat("  Total CA MPAs:", nrow(mpa_all), "\n")

mpa_open_coast <- mpa_all %>%
  filter(Study_Regi != "SFBSR") %>%
  filter(!NAME %in% MPA_ESTUARY_EXCLUDE)

cat("  Excluded (SF Bay):", sum(mpa_all$Study_Regi == "SFBSR"), "\n")
cat("  Excluded (named estuary/lagoon/slough):", sum(mpa_all$NAME %in% MPA_ESTUARY_EXCLUDE), "\n")
cat("  Open-coast MPAs kept:", nrow(mpa_open_coast), "\n")

mpa_path <- file.path(output_root, "MPA.geojson")
suppressWarnings(st_write(mpa_open_coast, mpa_path, delete_dsn = TRUE, quiet = TRUE))
cat("  Written:", mpa_path, "\n")

cat("\nFetching National Marine Sanctuary boundaries...\n")
nms_all <- st_read(NMS_SOURCE, quiet = TRUE) %>% st_make_valid()
cat("  Total sanctuaries (nationwide):", nrow(nms_all), "\n")

nms_ca <- nms_all %>%
  filter(str_detect(NMS_Name, paste(NMS_INCLUDE_PATTERNS, collapse = "|")))

cat("  California sanctuaries kept:", nrow(nms_ca), "—", paste(nms_ca$NMS_Name, collapse = "; "), "\n")

nms_path <- file.path(output_root, "NMS.geojson")
suppressWarnings(st_write(nms_ca, nms_path, delete_dsn = TRUE, quiet = TRUE))
cat("  Written:", nms_path, "\n")

# =============================================================================
# STEP 2 — HEX-CLIP MASTER_INVENTORY AGAINST EACH BOUNDARY SET
# (same centroid-in-polygon approach as build_asbs_layer.R)
# =============================================================================

layers <- list(
  list(label = "MPA", boundary = mpa_open_coast, prefix = "Master_MPA"),
  list(label = "NMS", boundary = nms_ca,         prefix = "Master_NMS")
)

for (layer in layers) {

  cat("\n=== Clipping Master_Inventory to", layer$label, "boundaries ===\n")
  boundary_union <- st_union(layer$boundary)

  for (res in HEX_RESOLUTIONS) {

    cat("\n--- Resolution:", res, "---\n")

    master_path <- file.path(output_root, paste0("Master_Inventory_", res, ".geojson"))
    if (!file.exists(master_path)) {
      cat("  Master_Inventory_", res, ".geojson not found — skipping.\n", sep = "")
      next
    }

    master_sf <- st_read(master_path, quiet = TRUE) %>% st_make_valid()
    centroids <- st_centroid(master_sf)

    inside <- st_intersects(centroids, boundary_union, sparse = FALSE)[, 1]
    out_sf <- master_sf[inside, ]

    cat("  ", sum(inside), "of", nrow(master_sf), "hexes fall inside", layer$label, "boundaries.\n", sep = "")

    if (nrow(out_sf) == 0) {
      cat("  No hexes inside", layer$label, "at this resolution — skipping write.\n")
      next
    }

    out_geojson <- file.path(output_root, paste0(layer$prefix, "_", res, ".geojson"))
    out_gz      <- paste0(out_geojson, ".gz")

    suppressWarnings(st_write(out_sf, out_geojson, delete_dsn = TRUE, quiet = TRUE))

    if (!requireNamespace("R.utils", quietly = TRUE)) install.packages("R.utils")
    library(R.utils)
    if (file.exists(out_gz)) file.remove(out_gz)
    gzip(out_geojson, destname = out_gz, remove = TRUE)

    cat("  Written:", basename(out_gz), "(", nrow(out_sf), "features)\n", sep = "")
  }
}

cat("\n=== MPA / NMS LAYER BUILD COMPLETE ===\n")
cat("Outputs:\n")
cat("  MPA.geojson, NMS.geojson (boundaries)\n")
cat("  Master_MPA_1km/3km/5km.geojson.gz\n")
cat("  Master_NMS_1km/3km/5km.geojson.gz\n")

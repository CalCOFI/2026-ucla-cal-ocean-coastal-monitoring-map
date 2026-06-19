###############################################################################
# build_gaps_code.R
#
# Identifies unmonitored coastal ocean areas by comparing the Master Inventory
# against a fine-resolution hex grid over the 13.4-mile coastal buffer.
#
# INPUTS:
#   - Master_Inventory_1km.geojson (from build_combine_code.R)
#   - CA state boundary shapefile (for coastal buffer and land mask)
#
# OUTPUT:
#   - monitoring_gaps.geojson (hex grid of unmonitored ocean areas)
#   - monitoring_gaps_statistics.csv
#   - gap_stats.json (sidecar for HTML auto-population)
#
# Also prints regional % unmonitored breakdown to console.
#
# Run AFTER build_combine_map.R.
###############################################################################

library(tidyverse)
library(sf)

# =============================================================================
# USER SETTINGS 
# =============================================================================

master_geojson_path <- "C:/Users/bhuan/Downloads/Monitoring_Outputs/Master_Inventory_1km.geojson"
ca_boundary_path    <- "C:/Users/bhuan/Downloads/Monitoring Data/ca_state/CA_State.shp"
output_path         <- "C:/Users/bhuan/Downloads/Monitoring_Outputs/monitoring_gaps.geojson"

buffer_miles   <- 13.4
buffer_meters  <- buffer_miles * 1609.34
gap_hex_size_m <- 1000   # smaller than program hexes (3500) for finer gap resolution

# =============================================================================
# BUILD COASTAL BUFFER
# =============================================================================

ca_boundary    <- st_read(ca_boundary_path, quiet = TRUE) %>%
  st_transform(3310) %>%
  st_union()
coastal_buffer <- st_buffer(ca_boundary, dist = buffer_meters)

# Subtract land so the buffer covers ocean only — prevents inland hexes
# from being counted as monitored or unmonitored coastal area
ocean_buffer <- st_difference(coastal_buffer, ca_boundary)

# Clip to a bounding box that excludes offshore islands and Gulf of CA artifacts
ocean_bbox <- st_bbox(c(xmin=-126, ymin=29, xmax=-117.2, ymax=41.8), crs=st_crs(4326)) %>%
  st_as_sfc() %>%
  st_transform(3310)
ocean_buffer <- st_intersection(ocean_buffer, ocean_bbox)

# Second land subtraction with a 500m inset to remove slivers along the coastline
# that survive the first st_difference due to geometry precision
ocean_buffer <- st_difference(ocean_buffer, st_buffer(ca_boundary, 500))

# =============================================================================
# BUILD GAP HEX GRID
# =============================================================================

master <- st_read(master_geojson_path, quiet = TRUE) %>%
  st_transform(3310)

# Build a fine hex grid over the ocean buffer, then keep only cells that
# fall inside it — this is the universe of possible monitored/gap hexes
gap_grid <- st_make_grid(ocean_buffer, cellsize = gap_hex_size_m, square = FALSE) %>%
  st_sf() %>%
  st_set_crs(3310) %>%
  mutate(gap_hex_id = paste0("GAP_", row_number())) %>%
  st_filter(ocean_buffer)

cat("Total buffer hexes:", nrow(gap_grid), "\n")

# =============================================================================
# IDENTIFY GAP HEXES
# =============================================================================

# A gap hex is any buffer hex that does not intersect a monitored program hex
monitored_idx <- st_intersects(gap_grid, master, sparse = TRUE)
is_monitored  <- lengths(monitored_idx) > 0

gap_hexes <- gap_grid %>%
  filter(!is_monitored) %>%
  st_transform(3310)

# Drop hexes whose centroids land on land — these are coastal fringe cells
# that survived the ocean clip but don't represent actual ocean coverage
gap_centroids <- st_centroid(gap_hexes)
on_land       <- st_intersects(gap_centroids, ca_boundary, sparse = FALSE)[,1]

# Drop hexes east of -120.5° — these are inland bays and estuaries,
# not open coast, and skew the gap statistics
centroid_coords <- st_centroid(gap_hexes %>% st_transform(4326)) %>% st_coordinates()
west_enough     <- centroid_coords[,1] < -120.5

# Apply both filters on the same row indices so they don't get out of sync
gap_hexes <- gap_hexes %>%
  filter(!on_land & west_enough) %>%
  st_transform(4326)

cat("Monitored hexes:", sum(is_monitored), "\n")
cat("Gap hexes:", nrow(gap_hexes), "\n")

# =============================================================================
# COVERAGE STATISTICS
# =============================================================================

total_area_km2     <- as.numeric(st_area(st_transform(gap_grid,    3310))) %>% sum() / 1e6
gap_area_km2       <- as.numeric(st_area(st_transform(gap_hexes,   3310))) %>% sum() / 1e6
monitored_area_km2 <- total_area_km2 - gap_area_km2

cat("\n=== COVERAGE STATISTICS ===\n")
cat("Total coastal buffer area:  ", round(total_area_km2), "km²\n")
cat("Monitored area:             ", round(monitored_area_km2), "km²\n")
cat("Unmonitored (gap) area:     ", round(gap_area_km2), "km²\n")
cat("Percent monitored:          ", round(monitored_area_km2/total_area_km2*100, 1), "%\n")
cat("Percent unmonitored:        ", round(gap_area_km2/total_area_km2*100, 1), "%\n")

# =============================================================================
# REGIONAL BREAKDOWN
# =============================================================================

# Latitude thresholds follow rough CA regional conventions
gap_hexes_proj <- gap_hexes %>% st_transform(4326)
gap_hexes_proj$centroid_lat <- st_centroid(gap_hexes_proj) %>% st_coordinates() %>% .[,2]

regional_stats <- gap_hexes_proj %>%
  st_drop_geometry() %>%
  mutate(region = case_when(
    centroid_lat >= 40   ~ "North Coast",
    centroid_lat >= 37   ~ "Bay Area / Central",
    centroid_lat >= 34.5 ~ "Central Coast",
    TRUE                 ~ "Southern CA"
  )) %>%
  count(region, name = "gap_hex_count") %>%
  arrange(desc(gap_hex_count))

gap_grid_proj <- gap_grid %>% st_transform(4326)
gap_grid_proj$centroid_lat <- st_centroid(gap_grid_proj) %>% st_coordinates() %>% .[,2]

total_by_region <- gap_grid_proj %>%
  st_drop_geometry() %>%
  mutate(region = case_when(
    centroid_lat >= 40   ~ "North Coast",
    centroid_lat >= 37   ~ "Bay Area / Central",
    centroid_lat >= 34.5 ~ "Central Coast",
    TRUE                 ~ "Southern CA"
  )) %>%
  count(region, name = "total_hex_count")

regional_pct <- regional_stats %>%
  left_join(total_by_region, by = "region") %>%
  mutate(pct_unmonitored = round(gap_hex_count / total_hex_count * 100, 1))

cat("\n=== REGIONAL % UNMONITORED ===\n")
print(regional_pct)

cat("\n=== GAPS BY REGION ===\n")
print(regional_stats)

# =============================================================================
# WRITE GAP GEOJSON
# =============================================================================

st_write(gap_hexes, output_path, delete_dsn = TRUE, quiet = TRUE)
cat("\nGap GeoJSON written:", output_path, "\n")

# =============================================================================
# WRITE STATISTICS CSV
# =============================================================================

bind_rows(
  tibble(metric = "Total buffer area (km²)",    value = round(total_area_km2)),
  tibble(metric = "Monitored area (km²)",        value = round(monitored_area_km2)),
  tibble(metric = "Unmonitored area (km²)",      value = round(gap_area_km2)),
  tibble(metric = "Percent monitored (%)",       value = round(monitored_area_km2/total_area_km2*100, 1)),
  tibble(metric = "Percent unmonitored (%)",     value = round(gap_area_km2/total_area_km2*100, 1))
) %>%
  write_csv(str_replace(output_path, "\\.geojson$", "_statistics.csv"))

cat("Statistics CSV written.\n")

# =============================================================================
# WRITE SIDECAR JSON FOR HTML AUTO-POPULATION
# =============================================================================

# gap_stats is printed to console so values can be spot-checked before
# the JSON is written — useful when re-running with different buffer settings
gap_stats <- list(
  total_area_km2     = round(total_area_km2),
  monitored_area_km2 = round(monitored_area_km2),
  gap_area_km2       = round(gap_area_km2),
  pct_monitored      = round(monitored_area_km2/total_area_km2*100, 1),
  pct_unmonitored    = round(gap_area_km2/total_area_km2*100, 1),
  gap_hex_count      = nrow(gap_hexes)
)
cat("\nStats for HTML display:\n")
cat(jsonlite::toJSON(gap_stats, auto_unbox=TRUE), "\n")

jsonlite::write_json(list(
  total     = paste0(format(round(total_area_km2),    big.mark=","), " km²"),
  monitored = paste0(format(round(monitored_area_km2),big.mark=","), " km²"),
  gap       = paste0(format(round(gap_area_km2),      big.mark=","), " km²"),
  pct_mon   = paste0(round(monitored_area_km2/total_area_km2*100, 1), "%"),
  pct_gap   = paste0(round(gap_area_km2/total_area_km2*100,       1), "%"),
  r1 = paste0(regional_pct %>% filter(str_detect(region,"North"))        %>% pull(pct_unmonitored), "%"),
  r2 = paste0(regional_pct %>% filter(str_detect(region,"Bay"))          %>% pull(pct_unmonitored), "%"),
  r3 = paste0(regional_pct %>% filter(str_detect(region,"Central Coast"))%>% pull(pct_unmonitored), "%"),
  r4 = paste0(regional_pct %>% filter(str_detect(region,"Southern"))     %>% pull(pct_unmonitored), "%")
), file.path(dirname(output_path), "gap_stats.json"), auto_unbox=TRUE)
cat("gap_stats.json written.\n")
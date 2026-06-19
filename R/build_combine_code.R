###################################################################################
# build_combine_code.R
#
# Merges all program outputs into resolution-specific master GeoJSONs.
#
# STEP 1 — Chunk combine:
#   Merges split program folders (e.g. CalCOFI_1, CalCOFI_2) into one
#   GeoJSON per program before they enter the master.
#
# STEP 2 — Master combine:
#   Merges all program GeoJSONs into three master files:
#     Master_Inventory_1km.geojson
#     Master_Inventory_3km.geojson
#     Master_Inventory_5km.geojson
#   Also produces Master_Polygons.geojson, Master_WEA_Xkm.geojson,
#   CA_Wind_WEA.geojson, and transects.csv.
#
# STEP 3 — Compress:
#   Gzips each master GeoJSON for web delivery.
#
# Run AFTER all programs processed with build_program_layer.R.
# Run build_discharger_layer.R first if discharger layer is needed.
###################################################################################

library(tidyverse)
library(sf)

# =============================================================================
# USER SETTINGS
# =============================================================================

output_root                <- "C:/Users/bhuan/Downloads/Monitoring_Outputs"
combined_name              <- "Master_Inventory"
wea_shapefile_path_combine <- "C:/Users/bhuan/Downloads/Monitoring_Outputs/WEA/CA_Wind.shp"

# Names of discharger output folders — excluded from hex combine
discharger_folder_names <- c("Dischargers")

# =============================================================================
# CONSTANTS
# =============================================================================

HEX_RESOLUTIONS <- 
  c("1km",
    "3km", 
    "5km")

# =============================================================================
# HELPER FUNCTIONS
# =============================================================================

collapse_unique <- function(x, sep = "; ") {
  x <- as.character(x)
  x <- x[!is.na(x) & str_trim(x) != "" & x != "NA"]
  x <- unique(x)
  if (length(x) == 0) return(NA_character_)
  paste(sort(x), collapse = sep)
}

# =============================================================================
# COASTAL BUFFER
# =============================================================================

ca_boundary_path_combine <- "C:/Users/bhuan/Downloads/Monitoring Data/ca_state/CA_State.shp"
buffer_meters_combine    <- 13.4 * 1609.34

ca_buffer_combine <- st_read(ca_boundary_path_combine, quiet = TRUE) %>%
  st_transform(3310) %>%
  st_union() %>%
  st_buffer(dist = buffer_meters_combine) %>%
  st_transform(4326)
cat("Coastal buffer ready.\n")

# =============================================================================
# STEP 1 — CHUNK COMBINE
# Merges split program folders (e.g. CalCOFI_1, CalCOFI_2) into one GeoJSON per program.
# =============================================================================

cat("=== STEP 1: Chunk combine ===\n")

chunk_folders <- list.dirs(output_root, full.names = TRUE, recursive = TRUE) %>%
  { gsub("\\\\", "/", .) } %>%
  .[str_detect(basename(.), "[_ ]?\\d+$")] %>%
  .[!str_detect(., regex("Master_", ignore_case = TRUE))]

if (length(chunk_folders) == 0) {
  cat("No chunk folders found — skipping.\n")
} else {
  
  folder_map <- tibble(
    full_path  = chunk_folders,
    chunk_name = basename(chunk_folders),
    # Strip the trailing number to get the base program name used for grouping
    # e.g. CalCOFI_1 → CalCOFI
    base_name  = str_remove(chunk_name, "[_ ]?\\d+$")
  )
  
  for (prog in unique(folder_map$base_name)) {
    prog_chunks <- folder_map %>% filter(base_name == prog) %>% arrange(chunk_name)
    chunk_names <- prog_chunks$chunk_name
    cat("\nCombining", prog, ":", paste(chunk_names, collapse = ", "), "\n")
    
    for (res in HEX_RESOLUTIONS) {
      chunk_files <- file.path(prog_chunks$full_path,
                               paste0(chunk_names, "_", res, ".geojson"))
      missing <- chunk_files[!file.exists(chunk_files)]
      if (length(missing) == length(chunk_files)) next
      if (length(missing) > 0) {
        warning("Missing: ", paste(missing, collapse = ", "))
        next
      }
      
      # If there's only one chunk folder for this program, there's nothing to merge
      # skip the combine logic and copy the file straight to the output folder
      if (length(chunk_names) == 1) {
        out_folder <- file.path(output_root, prog)
        dir.create(out_folder, showWarnings = FALSE, recursive = TRUE)
        file.copy(chunk_files,
                  file.path(out_folder, paste0(prog, "_", res, ".geojson")),
                  overwrite = TRUE)
        cat("  [", res, "] Copied directly.\n", sep = "")
        next
      }
      
      chunk_sf_list <- map(chunk_files, st_read, quiet = TRUE)
      
      empty <- map_lgl(chunk_sf_list, ~ nrow(.x) == 0)
      if (any(empty)) {
        cat("  [", res, "] Skipping empty chunks: ",
            paste(chunk_names[empty], collapse = ", "), "\n", sep = "")
        chunk_sf_list   <- chunk_sf_list[!empty]
        prog_chunks_res <- prog_chunks[!empty, ]
        chunk_names_res <- chunk_names[!empty]
      } else {
        prog_chunks_res <- prog_chunks
        chunk_names_res <- chunk_names
      }
      if (length(chunk_sf_list) == 0) {
        warning("All chunks empty for ", prog, " [", res, "]")
        next
      }
      
      # Get the full list of param_ columns across all chunks before merging
      all_param_cols <- chunk_sf_list %>%
        map(~ names(.x)[str_detect(names(.x), "^param_")]) %>%
        reduce(union)
      
      chunk_sf_list <- map(chunk_sf_list, function(sf_obj) {
        gebco_vals <- sf_obj[["Gebco.Mean.Depth"]]
        for (col in setdiff(all_param_cols, names(sf_obj))) sf_obj[[col]] <- 0L
        # Cast all non-geometry columns to character first to prevent
        # type conflicts (e.g. integer vs character) in bind_rows
        out <- sf_obj %>% mutate(across(-geometry, ~ as.character(.x)))
        out[["Gebco.Mean.Depth"]] <- gebco_vals
        out
      }) %>%
        # Second pass after list assembly to catch any remaining type conflicts
        map(~ .x %>% mutate(across(-geometry, ~ as.character(.x))))
      
      combined_sf <- bind_rows(chunk_sf_list) %>%
        rename_with(~ str_replace_all(.x, "\\.", " "), everything())
      
      combined_sf <- combined_sf %>%
        mutate(across(where(is.character), ~ na_if(str_trim(.x), "NA"))) %>%
        mutate(`Gebco Mean Depth` = suppressWarnings(as.numeric(`Gebco Mean Depth`)))
      
      names(combined_sf) <- names(combined_sf) %>%
        str_replace("Depth Range  m ", "Depth Range (m)")
      
      lat_col <- names(combined_sf)[str_detect(names(combined_sf), regex("centroid.*lat", ignore_case = TRUE))][1]
      lon_col <- names(combined_sf)[str_detect(names(combined_sf), regex("centroid.*lon", ignore_case = TRUE))][1]
      
      if (is.na(lat_col) | is.na(lon_col)) {
        warning("No centroid columns for ", prog, " [", res, "]")
        next
      }
      
      merged_df <- combined_sf %>%
        st_drop_geometry() %>%
        group_by(.data[[lat_col]], .data[[lon_col]]) %>%
        summarise(
          `Program Name`       = first(na.omit(`Program Name`)),
          `Full Program Name`  = first(na.omit(`Full Program Name`)),
          `Monitoring Program` = first(na.omit(`Monitoring Program`)),
          `Frequency`          = collapse_unique(Frequency),
          `Platform`           = collapse_unique(Platform),
          `First Year`         = suppressWarnings(min(as.numeric(`First Year`),    na.rm = TRUE)),
          `Last Year`          = suppressWarnings(max(as.numeric(`Last Year`),     na.rm = TRUE)),
          `Gebco Mean Depth`   = suppressWarnings(mean(as.numeric(`Gebco Mean Depth`), na.rm = TRUE)),
          `Years Sampled`      = suppressWarnings(max(as.numeric(`Years Sampled`), na.rm = TRUE)),
          `Depth Range (m)`    = collapse_unique(`Depth Range (m)`),
          `Sample Locations`   = suppressWarnings(sum(as.numeric(`Sample Locations`), na.rm = TRUE)),
          `Parameters`         = collapse_unique(Parameters),
          `EOV Groups`         = collapse_unique(`EOV Groups`),
          `Parameter Count`    = suppressWarnings(max(as.numeric(`Parameter Count`), na.rm = TRUE)),
          `Source Files`       = if ("Source Files" %in% names(pick(everything()))) collapse_unique(`Source Files`) else NA_character_,
          across(all_of(all_param_cols), ~ max(.x, na.rm = TRUE)),
          .groups = "drop"
        ) %>%
        mutate(
          `First Year`       = na_if(as.numeric(`First Year`),        Inf),
          `Gebco Mean Depth` = na_if(`Gebco Mean Depth`,              NaN),
          `Last Year`        = na_if(as.numeric(`Last Year`),        -Inf),
          `Years Sampled`    = na_if(as.numeric(`Years Sampled`),    -Inf)
        )
      
      # merged_df lost its geometry when we ran st_drop_geometry(), so we pull
      # the geometry back in by joining on the centroid coordinates
      merged_sf <- combined_sf %>%
        select(all_of(c(lat_col, lon_col)), geometry) %>%
        distinct(.data[[lat_col]], .data[[lon_col]], .keep_all = TRUE) %>%
        left_join(merged_df, by = c(lat_col, lon_col)) %>%
        st_as_sf()
      
      cat("Checking geometries for", prog, "[", res, "]...\n")
      bad <- which(!st_is_valid(merged_sf) | st_is_empty(merged_sf))
      if (length(bad) > 0) cat("  Problem rows:", paste(bad, collapse = ", "), "\n")
      
      out_folder <- file.path(output_root, prog)
      dir.create(out_folder, showWarnings = FALSE, recursive = TRUE)
      out_path <- file.path(out_folder, paste0(prog, "_", res, ".geojson"))
      
      merged_sf <- merged_sf %>%
        filter(!st_is_empty(geometry)) %>%
        filter(!is.na(st_dimension(geometry))) %>%
        st_make_valid()
      
      suppressWarnings(st_write(merged_sf, out_path, delete_dsn = TRUE, quiet = TRUE))
      cat("  [", res, "] Written:", out_path, "(", nrow(merged_sf), "rows)\n", sep = "")
    }
    
    # Promote WEA-only GeoJSONs from chunk folders
    for (res in HEX_RESOLUTIONS) {
      wea_chunk_files <- file.path(prog_chunks$full_path,
                                   paste0(chunk_names, "_wea_", res, ".geojson"))
      wea_exist <- wea_chunk_files[file.exists(wea_chunk_files)]
      if (length(wea_exist) == 0) next
      
      out_wea <- file.path(output_root, prog, paste0(prog, "_wea_", res, ".geojson"))
      dir.create(file.path(output_root, prog), showWarnings = FALSE, recursive = TRUE)
      
      if (length(wea_exist) == 1) {
        file.copy(wea_exist, out_wea, overwrite = TRUE)
      } else {
        map(wea_exist, st_read, quiet = TRUE) %>%
          map(~ .x %>% mutate(across(-geometry, ~ as.character(.x)))) %>%
          bind_rows() %>%
          suppressWarnings(st_write(out_wea, delete_dsn = TRUE, quiet = TRUE))
      }
      cat("  [", res, "] WEA hexes promoted →", basename(out_wea), "\n", sep = "")
    }
    
    # Combine transects (once per program, not per resolution)
    transect_files <- file.path(prog_chunks$full_path, "transects.csv") %>%
      .[file.exists(.)]
    if (length(transect_files) > 0) {
      out_folder <- file.path(output_root, prog)
      dir.create(out_folder, showWarnings = FALSE, recursive = TRUE)
      raw_t <- map_dfr(transect_files, read_csv, show_col_types = FALSE,
                       col_types = cols(.default = col_character())) %>%
        distinct()
      t_sf <- raw_t %>%
        mutate(
          lat_check = suppressWarnings(as.numeric(coalesce(`Latitude Mid`, `Latitude Start`))),
          lon_check = suppressWarnings(as.numeric(coalesce(`Longitude Mid`, `Longitude Start`)))
        ) %>%
        filter(!is.na(lat_check), !is.na(lon_check)) %>%
        st_as_sf(coords = c("lon_check", "lat_check"), crs = 4326, remove = FALSE)
      keep <- st_intersects(t_sf, ca_buffer_combine, sparse = FALSE)[, 1]
      t_sf[keep, ] %>%
        st_drop_geometry() %>%
        select(-lat_check, -lon_check) %>%
        write_csv(file.path(out_folder, "transects.csv"))
      cat("  Transects written:", sum(keep), "of", nrow(t_sf), "rows after coastal clip.\n")
    }
  }
}

# =============================================================================
# STEP 2 — MASTER COMBINE (per resolution)
# Produces three resolution-specific masters:
#   Master_Inventory_1km.geojson
#   Master_Inventory_3km.geojson
#   Master_Inventory_5km.geojson
# =============================================================================

cat("\n=== STEP 2: Master combine (per resolution) ===\n")

for (res in HEX_RESOLUTIONS) {
  
  cat("\n--- Resolution:", res, "---\n")
  
  res_pattern <- paste0("_", res, "\\.geojson$")
  
  geojson_files <- list.files(output_root, pattern = res_pattern,
                              full.names = TRUE, recursive = TRUE) %>%
    { gsub("\\\\", "/", .) } %>%
    .[!str_detect(., regex("Master_", ignore_case = TRUE))] %>%
    .[!str_detect(., regex("/chunks/",         ignore_case = TRUE))] %>%
    # Skip chunk subfolders like CalCOFI_1, CalCOFI_2 — those were already
    # merged into one file per program in Step 1
    .[!str_detect(basename(dirname(.)), "^.*[_ ]?\\d+$")] %>%
    .[!basename(dirname(.)) %in% discharger_folder_names] %>%
    .[!tools::file_path_sans_ext(basename(.)) %in% discharger_folder_names] %>%
    .[!str_detect(basename(.), "_polygons\\.geojson$")] %>%
    .[!str_detect(basename(.), regex("_wea_", ignore_case = TRUE))]
  
  cat("GeoJSON files found for", res, ":", length(geojson_files), "\n")
  print(basename(geojson_files))
  
  if (length(geojson_files) == 0) {
    cat("No files for resolution", res, "— skipping.\n")
    next
  }
  
  combined_sf <- map(geojson_files, st_read, quiet = TRUE) %>%
    # Cast ALL non-geometry columns to character uniformly before bind_rows.
    # Do NOT try to preserve gebco as numeric here — convert it after binding.
    map(~ .x %>% mutate(across(-geometry, ~ as.character(.x)))) %>%
    bind_rows() %>%
    st_make_valid()
  
  # Restore readable column names
  names(combined_sf) <- names(combined_sf) %>%
    str_replace_all("\\.", " ") %>%
    str_replace("Depth Range  m ", "Depth Range (m)") %>%
    str_squish()
  
  combined_sf <- combined_sf %>%
    select(-any_of(c("Geometry Types", "Geometry.Types")))
  
  combined_sf <- combined_sf %>%
    mutate(across(where(is.numeric), ~ ifelse(is.infinite(.x) | is.nan(.x), NA, .x)))
  
  cat("Total rows:", nrow(combined_sf), "\n")
  cat("Programs:", paste(unique(combined_sf$`Program Name`), collapse = ", "), "\n")
  
  # Short pause to make sure the previous file is fully closed before deleting it
  final_path <- file.path(output_root, paste0(combined_name, "_", res, ".geojson"))
  if (file.exists(final_path)) { Sys.sleep(0.5); file.remove(final_path) }
  
  suppressWarnings(
    st_write(combined_sf, final_path, delete_dsn = TRUE, quiet = TRUE,
             layer_options = "RFC7946=NO")
  )
  cat("Written:", final_path, "\n")
}

# =============================================================================
# STEP 2a — MASTER POLYGON OVERLAY COMBINE
# =============================================================================

cat("\nCombining polygon overlays...\n")

polygon_exclude <- c("CCLEAN", "NOAA IWCPS", "CHIS", "MPAs")
polygon_files <- list.files(output_root,
                            pattern = "_polygons\\.geojson$",
                            full.names = TRUE, recursive = TRUE) %>%
  { gsub("\\\\", "/", .) } %>%
  .[!str_detect(., regex("Master_", ignore_case = TRUE))] %>%
  .[!map_lgl(., ~ any(str_detect(str_split(.x, "/")[[1]],
                                 paste(polygon_exclude, collapse = "|"))))]

if (length(polygon_files) > 0) {
  cat("Polygon files found:", length(polygon_files), "\n")
  walk(polygon_files, ~ cat("  •", basename(.x), "\n"))
  
  # Limit to a fixed set of columns so GDAL never hits the field-count ceiling
  keep_cols <- c("Program Name", "Full Program Name", "Tooltip Label",
                 "zone_name", "Parameters", "Frequency", "Platform",
                 "First Year", "Last Year", "Notes")
  
  poly_sf <- map(polygon_files, function(f) {
    tryCatch({
      layer <- st_read(f, quiet = TRUE) %>% st_make_valid()
      names(layer) <- names(layer) %>%
        str_replace_all("\\.", " ") %>% str_squish()
      layer <- layer %>% mutate(across(-geometry, ~ as.character(.x)))
      # Add any missing keep_cols as NA so bind_rows is clean
      for (col in keep_cols) {
        if (!col %in% names(layer)) layer[[col]] <- NA_character_
      }
      layer %>% select(all_of(keep_cols), geometry)
    }, error = function(e) {
      warning("Could not read: ", basename(f))
      NULL
    })
  }) %>%
    compact() %>%
    bind_rows() %>%
    st_make_valid()
  
  poly_out <- file.path(output_root, "Master_Polygons.geojson")
  poly_tmp <- file.path(tempdir(), "Master_Polygons_tmp.geojson")
  
  # Write to a temp file first, then copy to final destination — avoids
  # leaving a half-written file if something goes wrong mid-write
  suppressWarnings(
    st_write(poly_sf, poly_tmp, driver = "GeoJSON", delete_dsn = TRUE, quiet = TRUE)
  )
  
  if (file.exists(poly_tmp) && file.size(poly_tmp) > 500) {
    file.copy(poly_tmp, poly_out, overwrite = TRUE)
    try(file.remove(poly_tmp), silent = TRUE)
    cat("Master polygon overlays written:", poly_out, "\n")
    cat("Features:", nrow(poly_sf), "\n")
  } else {
    cat("!! Polygon write to temp failed\n")
  }
}

# =============================================================================
# STEP 2b — MASTER TRANSECTS (with coastal clip)
# =============================================================================

cat("\nCombining transects...\n")

ca_boundary_path <- "C:/Users/bhuan/Downloads/Monitoring Data/ca_state/CA_State.shp"
buffer_meters    <- 13.4 * 1609.34

transect_files <- list.files(output_root, pattern = "^transects\\.csv$",
                             full.names = TRUE, recursive = TRUE) %>%
  { gsub("\\\\", "/", .) } %>%
  .[!str_detect(., regex("/chunks/",        ignore_case = TRUE))] %>%
  .[!str_detect(dirname(.), "[_ ]?\\d+$")] %>%
  .[!str_detect(., regex("Master_Inventory", ignore_case = TRUE))]

if (length(transect_files) > 0) {
  
  ca_buffer <- st_read(ca_boundary_path, quiet = TRUE) %>%
    st_transform(3310) %>%
    st_union() %>%
    st_buffer(dist = buffer_meters) %>%
    st_transform(4326)
  
  all_transects <- map_dfr(transect_files, read_csv, show_col_types = FALSE,
                           col_types = cols(.default = col_character())) %>%
    distinct()
  
  # Use midpoint coordinate if available, otherwise fall back to start —
  # midpoint is more representative for long transects
  all_transects_sf_start <- all_transects %>%
    mutate(
      lat_check = suppressWarnings(as.numeric(coalesce(`Latitude Mid`, `Latitude Start`))),
      lon_check = suppressWarnings(as.numeric(coalesce(`Longitude Mid`, `Longitude Start`)))
    ) %>%
    filter(!is.na(lat_check), !is.na(lon_check)) %>%
    st_as_sf(coords = c("lon_check", "lat_check"), crs = 4326, remove = FALSE)
  
  keep_start <- st_intersects(all_transects_sf_start, ca_buffer, sparse = FALSE)[, 1]
  
  has_stop <- !is.na(suppressWarnings(as.numeric(all_transects_sf_start$`Latitude Stop`))) &
    !is.na(suppressWarnings(as.numeric(all_transects_sf_start$`Longitude Stop`)))
  
  stop_sf <- all_transects_sf_start[has_stop, ] %>%
    st_drop_geometry() %>%
    mutate(
      lat_stop_num = suppressWarnings(as.numeric(`Latitude Stop`)),
      lon_stop_num = suppressWarnings(as.numeric(`Longitude Stop`))
    ) %>%
    st_as_sf(coords = c("lon_stop_num", "lat_stop_num"), crs = 4326, remove = TRUE)
  
  keep_stop_vals <- st_intersects(stop_sf, ca_buffer, sparse = FALSE)[, 1]
  
  # Both the start and end of a transect must fall inside the coastal buffer —
  # if either end is outside, the whole transect is dropped
  keep_final <- keep_start
  keep_final[has_stop] <- keep_start[has_stop] & keep_stop_vals
  
  # Drop zero-length transects (Latitude Start == Latitude Stop)
  has_real_extent <- with(st_drop_geometry(all_transects_sf_start), {
    lat_start <- suppressWarnings(as.numeric(`Latitude Start`))
    lat_stop  <- suppressWarnings(as.numeric(`Latitude Stop`))
    lon_start <- suppressWarnings(as.numeric(`Longitude Start`))
    lon_stop  <- suppressWarnings(as.numeric(`Longitude Stop`))
    !(round(lat_start, 5) == round(lat_stop, 5) &
        round(lon_start, 5) == round(lon_stop, 5))
  })
  keep_final <- keep_final & has_real_extent
  
  all_transects_sf_start[keep_final, ] %>%
    st_drop_geometry() %>%
    select(-lat_check, -lon_check) %>%
    write_csv(file.path(output_root, "transects.csv"))
  
  cat("Master transects written:", sum(keep_final), "of", nrow(all_transects_sf_start),
      "rows kept after coastal clip.\n")
  
} else {
  cat("No transect files found.\n")
}

# =============================================================================
# STEP 2c — MASTER WEA HEX COMBINE
# =============================================================================

cat("\nCombining WEA hex files...\n")

for (res in HEX_RESOLUTIONS) {
  wea_files <- list.files(output_root,
                          pattern = paste0("_wea_", res, "\\.geojson$"),
                          full.names = TRUE, recursive = TRUE) %>%
    { gsub("\\\\", "/", .) } %>%
    .[!str_detect(., regex("Master_", ignore_case = TRUE))]
  
  if (length(wea_files) == 0) {
    cat("No WEA files for", res, "\n")
    next
  }
  
  cat(res, "— WEA files found:", length(wea_files), "\n")
  walk(wea_files, ~ cat("  •", basename(.x), "\n"))
  
  all_cols <- wea_files %>%
    map(~ tryCatch(names(st_read(.x, quiet = TRUE)), error = function(e) character(0))) %>%
    reduce(union)
  
  sf_list <- map(wea_files, function(f) {
    tryCatch({
      obj <- st_read(f, quiet = TRUE)
      missing <- setdiff(setdiff(all_cols, "geometry"), names(obj))
      for (col in missing) obj[[col]] <- NA_character_
      # Force all non-geometry columns to character across every object
      # before bind_rows to prevent type conflicts (e.g. integer vs character)
      obj %>% mutate(across(-geometry, ~ as.character(.x)))
    }, error = function(e) {
      warning("Could not read: ", basename(f))
      NULL
    })
  }) %>%
    compact() %>%
    map(~ .x %>% mutate(across(-geometry, as.character)))
  
  if (length(sf_list) == 0) {
    cat("All WEA files failed for", res, "\n")
    next
  }
  
  combined <- bind_rows(sf_list) %>%
    st_make_valid() %>%
    mutate(across(where(is.character),
                  ~ if_else(str_trim(.x) %in% c("","NA","NaN","NULL","null"), NA_character_, str_trim(.x))))
  
  for (fld in c("Parameter Count","First Year","Last Year",
                "Years Sampled","Sample Locations","Gebco Mean Depth")) {
    if (fld %in% names(combined)) {
      combined[[fld]] <- suppressWarnings(as.numeric(combined[[fld]]))
    }
  }
  
  out_path <- file.path(output_root, paste0("Master_WEA_", res, ".geojson"))
  suppressWarnings(st_write(combined, out_path, delete_dsn = TRUE, quiet = TRUE))
  cat("Written:", basename(out_path), "—", nrow(combined), "features\n")
}

# =============================================================================
# STEP 2d — EXPORT WEA BOUNDARY POLYGON
# =============================================================================

cat("\nExporting WEA boundary polygon...\n")

if (file.exists(wea_shapefile_path_combine)) {
  wea_web <- st_read(wea_shapefile_path_combine, quiet = TRUE) %>%
    st_transform(4326) %>%
    st_make_valid() %>%
    select(any_of(c("LEASE_NUMB","COMPANY","STATE","LEASE_TYPE","geometry"))) %>%
    mutate(label = paste0("CA Wind Energy Area",
                          if ("COMPANY" %in% names(.)) paste0("\n", COMPANY) else ""))
  
  wea_boundary_path <- file.path(output_root, "WEA", "CA_Wind_WEA.geojson")
  suppressWarnings(st_write(wea_web, wea_boundary_path, delete_dsn = TRUE, quiet = TRUE))
  cat("WEA boundary written:", wea_boundary_path, "\n")
} else {
  cat("WEA shapefile not found — skipping boundary export.\n")
}

# =============================================================================
# STEP 3 — COMPRESS MASTER GEOJSONS
# =============================================================================

cat("\nCompressing master GeoJSONs...\n")

if (!requireNamespace("R.utils", quietly = TRUE)) install.packages("R.utils")
library(R.utils)

for (res in HEX_RESOLUTIONS) {
  f_in  <- file.path(output_root, paste0("Master_Inventory_", res, ".geojson"))
  f_out <- paste0(f_in, ".gz")
  if (file.exists(f_out)) file.remove(f_out)
  gzip(f_in, destname = f_out, remove = FALSE)
  cat("  Compressed:", basename(f_out), "\n")
}

cat("\n=== PIPELINE COMPLETE ===\n")
cat("Outputs:\n")
cat("  Master_Inventory_1km.geojson\n")
cat("  Master_Inventory_3km.geojson\n")
cat("  Master_Inventory_5km.geojson\n")
cat("  Master_Polygons.geojson\n")
cat("  Master_WEA_1km.geojson\n")
cat("  Master_WEA_3km.geojson\n")
cat("  Master_WEA_5km.geojson\n")
cat("  CA_Wind_WEA.geojson\n")
cat("  transects.csv\n")
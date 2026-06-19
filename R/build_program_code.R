# =============================================================================
# build_program_code.R
#
# Processes one monitoring program's files into outputs:
#   - CSV files         → hex grid GeoJSON (3 resolutions: 1km, 3km, 5km)
#   - SHP/GeoJSON files → polygon overlay GeoJSON
#
# STRUCTURE:
#   Steps 1–4 + GEBCO run ONCE (the slow work: reading, clipping, bathymetry)
#   Steps 5–export loop 3× (hex assignment, parameter detection, outputs)
#
# WEA SEPARATION:
#   Step 4 clips to COASTAL BUFFER ONLY for the main pipeline.
#   WEA-only points (outside coastal buffer) go to _wea_Xkm.geojson separately.
#   This ensures WEA hexes never appear in the master map until the toggle is clicked.
#
# Run once per program.
# After all programs processed, run build_combine_map.R
# =============================================================================

# =============================================================================
# LIBRARIES
# =============================================================================

library(tidyverse)
library(janitor)
library(sf)
library(terra)

# =============================================================================
# USER SETTINGS 
# =============================================================================

# Clear any leftover temp files from a previous run before starting
file.remove(list.files("C:/Users/bhuan/Documents/R_temp", full.names = TRUE))

# Redirect all temp file writes to a controlled folder — prevents R and 
# vroom from filling up the system temp drive on large CSV reads
Sys.setenv(VROOM_TEMP_PATH = "C:/Users/bhuan/Documents/R_temp")
Sys.setenv(TMPDIR          = "C:/Users/bhuan/Documents/R_temp")
Sys.setenv(TMP             = "C:/Users/bhuan/Documents/R_temp")
Sys.setenv(TEMP            = "C:/Users/bhuan/Documents/R_temp")
dir.create("C:/Users/bhuan/Documents/R_temp", showWarnings = FALSE, recursive = TRUE)

program_folder <- "C:/Users/bhuan/Downloads/Monitoring Data/CalCOFI"
output_root <- "C:/Users/bhuan/Downloads/Monitoring_Outputs"
ca_boundary_path <- "C:/Users/bhuan/Downloads/Monitoring Data/ca_state/CA_State.shp"
attribute_table_path <- "C:/Users/bhuan/Downloads/Monitoring Data/Attribute_Table.csv"
gebco_raster_path <- "C:/Users/bhuan/Downloads/Monitoring_Outputs/gebco_2025_n48.0_s30.0_w-130.0_e-110.0_geotiff.tif"
# alternate: compressed GEBCO from GitHub (smaller file)
# gebco_raster_path <- "C:/Users/bhuan/Downloads/Monitoring_Outputs/gebco_compressed.tif"
wea_shapefile_path <- "C:/Users/bhuan/Downloads/Monitoring_Outputs/WEA/CA_Wind.shp"
apply_wea_clip <- TRUE

start_year <- 2000
active_cutoff_year <- start_year

buffer_miles <- 13.4
buffer_meters <- buffer_miles * 1609.34

apply_coastal_clip <- TRUE

# =============================================================================
# DERIVED PATHS
# =============================================================================

program_name  <- basename(program_folder) %>% str_remove("[_ ]?\\d+$")
chunk_name    <- basename(program_folder)

# If the folder name matches the program name exactly, output goes directly
# into the program folder. If it has a chunk suffix (e.g. CalCOFI_1), output
# goes into a subfolder so chunks stay separate until build_combine_map.R merges them.
output_folder <- if (chunk_name == program_name) {
  file.path(output_root, program_name)
} else {
  file.path(output_root, program_name, chunk_name)
}

dir.create(output_folder, showWarnings = FALSE, recursive = TRUE)

# =============================================================================
# BLOCK 1: AUTO-UNZIP
# =============================================================================

zip_files <- list.files(program_folder, pattern = "\\.zip$",
                        full.names = TRUE, recursive = TRUE)
if (length(zip_files) > 0) {
  cat("Found", length(zip_files), "ZIP file(s) — extracting...\n")
  walk(zip_files, function(z) {
    dest <- file.path(dirname(z), tools::file_path_sans_ext(basename(z)))
    dir.create(dest, showWarnings = FALSE, recursive = TRUE)
    unzip(z, exdir = dest, overwrite = FALSE)
    cat("  Extracted:", basename(z), "→", basename(dest), "\n")
  })
}

# =============================================================================
# BLOCK 2: DETECT + ROUTE SPATIAL FILES
# =============================================================================

all_spatial_files <- list.files(program_folder,
                                pattern = "\\.(shp|geojson|gpkg)$",
                                full.names = TRUE, recursive = TRUE) %>%
  .[!str_detect(., regex("ca_state|CA_State|ca_boundary", ignore_case = TRUE))] %>%
  .[!str_detect(., regex("(/|\\\\)(output[^/\\\\]*)(/|\\\\)", ignore_case = TRUE))]

if (length(all_spatial_files) > 0) {
  all_spatial_files <- tibble(
    path  = all_spatial_files,
    fname = basename(all_spatial_files),
    depth = str_count(all_spatial_files, "/|\\\\")
  ) %>%
    group_by(fname) %>%
    slice_min(depth, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    pull(path)
}

classify_spatial_file <- function(f) {
  tryCatch({
    sf_obj   <- st_read(f, quiet = TRUE) %>% st_make_valid()
    gt       <- unique(as.character(st_geometry_type(sf_obj)))
    gt       <- gt[!is.na(gt)]
    is_point <- length(gt) > 0 && all(gt %in% c("POINT", "MULTIPOINT"))
    list(path = f, is_point = is_point, ok = TRUE)
  }, error = function(e) list(path = f, is_point = FALSE, ok = FALSE))
}

spatial_classified    <- map(all_spatial_files, classify_spatial_file)
spatial_files_skip    <- keep(spatial_classified, ~ .x$is_point)          %>% map_chr("path")
spatial_files_overlay <- keep(spatial_classified, ~ !.x$is_point & .x$ok) %>% map_chr("path")
has_spatial           <- length(spatial_files_overlay) > 0

if (length(spatial_files_skip) > 0) {
  cat("\nPoint shapefiles found (skipped — use CSV or discharger workflow):\n")
  walk(spatial_files_skip, ~ cat("  •", basename(.x), "\n"))
}
if (has_spatial) {
  cat("\nPolygon/line shapefiles found (→ overlay layer):\n")
  walk(spatial_files_overlay, ~ cat("  •", basename(.x), "\n"))
} else {
  cat("\nNo polygon/line spatial files found — polygon layer will be skipped.\n")
}

# =============================================================================
# LOAD ATTRIBUTE TABLE
# =============================================================================

attr_table_raw <- read_csv(attribute_table_path, show_col_types = FALSE) %>%
  clean_names() %>%
  filter(!is.na(acronym), str_trim(acronym) != "",
         !is.na(standard_parameter), str_trim(standard_parameter) != "") %>%
  mutate(across(everything(), str_trim))

attr_param_lookup <- attr_table_raw %>%
  select(acronym, standard_parameter,
         attr_frequency = frequency,
         attr_platform  = sampling_platform_vessel_buoy_etc) %>%
  filter(!is.na(attr_frequency) | !is.na(attr_platform)) %>%
  distinct()

attr_table_programs <- read_csv(attribute_table_path, show_col_types = FALSE) %>%
  clean_names() %>%
  filter(!is.na(acronym), str_trim(acronym) != "") %>%
  mutate(across(everything(), str_trim))

program_metadata <- attr_table_programs %>%
  group_by(acronym) %>%
  summarise(
    full_name = first(na.omit(program)),
    frequency = {f <- na.omit(frequency); if(length(f) == 0) NA_character_ else first(f)},
    platform  = {p <- na.omit(sampling_platform_vessel_buoy_etc); if(length(p) == 0) NA_character_ else first(p)},
    .groups = "drop"
  ) %>%
  rename(program_name = acronym) %>%
  mutate(across(everything(), ~ if_else(is.na(.x) | str_trim(.x) == "", "Unknown", str_trim(.x))))

program_meta <- program_metadata %>% filter(program_name == !!program_name)

# If the program isn't in the Attribute Table, default everything to Unknown
# and warn - the map will still build but metadata will be incomplete
if (nrow(program_meta) == 0) {
  warning("'", program_name, "' not found in Attribute_Table.csv — ",
          "add a row to set full_name, frequency, and platform.")
  display_name       <- chunk_name
  program_full_name  <- "Unknown"
  sampling_frequency <- "Unknown"
  program_platform   <- "Unknown"
} else {
  display_name       <- chunk_name
  program_full_name  <- program_meta$full_name
  sampling_frequency <- program_meta$frequency
  program_platform   <- program_meta$platform
}

cat("Program:", program_name, "| Frequency:", sampling_frequency, "| Platform:", program_platform, "\n")

platform_lookup <- tribble(
  ~pattern,                          ~platform,
  "acoustic",                        "Vessel - Towed Acoustic Array",
  "continuous.*fish.*egg|underway",  "Vessel - Continuous Underway Sampler",
  "glider",                          "Glider",
  "buoy|mooring",                    "Mooring / Buoy",
  "aerial",                          "Aerial",
  "shore|land|beach",                "Shore-based",
  "satellite|model",                 "Satellite / Model",
  ".*",                              program_platform
)

# =============================================================================
# WORKFLOW-INJECTED COLUMN NAMES
# =============================================================================

# Columns added by this workflow (coords, keys, flags) — excluded from
# parameter detection so they don't get mistaken for data columns
WORKFLOW_INJECTED_COLS <- c(
  "source_row_id", "source_file", "source_path", "file_stub", "program",
  "detected_lat_col", "detected_lon_col", "detected_year_col",
  "detected_date_col", "detected_program_col", "detected_depth_col",
  "detected_coord_role",
  "year_detected", "latitude_std", "longitude_std", "depth_std",
  "sample_point_key", "hex_id", "station_key",
  "geometry_type", "classification_reason",
  ".row_id_temp", ".join_key", "last_year", "activity_status"
)

# =============================================================================
# HELPER FUNCTIONS
# =============================================================================

get_file_stub <- function(path) {
  tools::file_path_sans_ext(basename(path)) %>%
    str_replace_all("[^A-Za-z0-9]+", "_")
}

clean_character_cols <- function(df) {
  df %>% mutate(across(where(is.character), str_squish))
}

collapse_unique_text <- function(x, sep = "; ") {
  x <- as.character(x)
  x <- x[!is.na(x) & str_trim(x) != ""]
  x <- unique(x)
  if (length(x) == 0) {
    return(NA_character_)
  }
  paste(sort(x), collapse = sep)
}

detect_col <- function(df, patterns) {
  nm   <- names(df)
  hits <- nm[map_lgl(nm, ~ any(str_detect(.x, regex(patterns, ignore_case = TRUE))))]
  if (length(hits) == 0) {
    return(NA_character_)
  }
  hits[1]
}

detect_col_priority <- function(df, pattern_vector, fallback_type = NULL) {
  nm <- names(df)
  for (pat in pattern_vector) {
    hits <- nm[str_detect(nm, regex(pat, ignore_case = TRUE))]
    if (length(hits) > 0) {
      return(hits[1])
    }
  }
  if (!is.null(fallback_type) && fallback_type == "lat") {
    hits <- nm[str_detect(nm, regex("latitude|lat|y", ignore_case = TRUE))]
    if (length(hits) > 0) {
      return(hits[1])
    }
  }
  if (!is.null(fallback_type) && fallback_type == "lon") {
    hits <- nm[str_detect(nm, regex("longitude|lon|long|x", ignore_case = TRUE))]
    if (length(hits) > 0) {
      return(hits[1])
    }
  }
  NA_character_
}

parse_season_to_approx_date <- function(x) {
  x_clean    <- str_squish(str_to_lower(as.character(x)))
  year_match <- str_extract(x_clean, "\\d{4}")
  year_val   <- suppressWarnings(as.integer(year_match))
  season_md  <- case_when(
    str_detect(x_clean, "spring")        ~ "04-15",
    str_detect(x_clean, "summer")        ~ "07-15",
    str_detect(x_clean, "fall|autumn")   ~ "10-15",
    str_detect(x_clean, "winter")        ~ "01-15",
    str_detect(x_clean, "q1|quarter.?1") ~ "02-15",
    str_detect(x_clean, "q2|quarter.?2") ~ "05-15",
    str_detect(x_clean, "q3|quarter.?3") ~ "08-15",
    str_detect(x_clean, "q4|quarter.?4") ~ "11-15",
    TRUE                                  ~ NA_character_
  )
  suppressWarnings(as.Date(
    if_else(!is.na(season_md) & !is.na(year_val),
            paste(year_val, season_md, sep = "-"),
            NA_character_)
  ))
}

contains_seasonal_language <- function(x) {
  x_clean <- str_squish(str_to_lower(as.character(x)))
  any(str_detect(x_clean, "spring|summer|fall|autumn|winter|\\bq[1-4]\\b|quarter"), na.rm = TRUE)
}

parse_date_time_safe <- function(x) {
  x <- as.character(x)
  formats <- c(
    "%Y-%m-%d", "%m/%d/%Y", "%d/%m/%Y", "%Y/%m/%d",
    "%Y-%m-%d %H:%M:%S", "%m/%d/%Y %H:%M:%S",
    "%m/%d/%Y %I:%M:%S %p",
    "%Y%m%d", "%d%b%Y", "%d%b%Y:%H:%M:%S", "%m/%d/%y"
  )
  result <- rep(as.Date(NA), length(x))
  for (fmt in formats) {
    idx <- which(is.na(result))
    if (length(idx) == 0) break
    result[idx] <- suppressWarnings(as.Date(x[idx], format = fmt))
  }
  still_na <- which(is.na(result))
  if (length(still_na) > 0)
    result[still_na] <- parse_season_to_approx_date(x[still_na])
  result
}

extract_year <- function(df, year_col = NA, date_col = NA) {
  out <- rep(NA_integer_, nrow(df))
  if (!is.na(year_col) && year_col %in% names(df))
    out <- suppressWarnings(as.integer(df[[year_col]]))
  if (!is.na(date_col) && date_col %in% names(df)) {
    yr <- suppressWarnings(as.integer(format(parse_date_time_safe(df[[date_col]]), "%Y")))
    out[is.na(out)] <- yr[is.na(out)]
  }
  out
}

PLACEHOLDER_VALUES <- c(
  "", "na", "n/a", "nan", "null", "none", "nd", "n.d.",
  "-999", "-9999", "-99999", "999", "9999", "99999"
)

is_placeholder_value <- function(x) {
  str_trim(str_to_lower(as.character(x))) %in% PLACEHOLDER_VALUES
}

count_real_values <- function(x) {
  sum(!is.na(as.character(x)) & !is_placeholder_value(x))
}

normalize_col_name <- function(x) {
  x %>% str_to_lower() %>% str_replace_all("[^a-z0-9]+", "")
}

clean_text_value <- function(x) {
  x %>% tolower() %>% str_replace_all("[^a-z0-9]+", " ") %>% str_squish()
}

parse_coord_vector <- function(vals, is_lon = FALSE, file_label = "", col_label = "") {
  x_chr   <- str_trim(as.character(vals))
  is_ph   <- is_placeholder_value(x_chr) | x_chr == ""
  hemi    <- str_extract(x_chr, regex("[NSEWnsew]", ignore_case = TRUE))
  x_clean <- str_trim(str_remove_all(
    x_chr,
    paste0("[NSEWnsew\u00b0\u00ba\u2032\u2033\u2019\u201d'\"", "]")
  ))
  result  <- suppressWarnings(as.numeric(x_clean))
  sw_mask <- !is.na(hemi) & str_detect(hemi, regex("[SWsw]"))
  result  <- if_else(!is.na(result) & sw_mask, -abs(result), result)
  
  need_dms <- which(is.na(result) & !is_ph)
  if (length(need_dms) > 0) {
    dms <- vapply(need_dms, function(i) {
      parts <- suppressWarnings(
        as.numeric(str_split(x_clean[i], "[:\\s]+|(?<=[0-9])-(?=[0-9])")[[1]])
      )
      parts <- parts[!is.na(parts)]
      if      (length(parts) == 2) parts[1] + parts[2] / 60
      else if (length(parts) == 3) parts[1] + parts[2] / 60 + parts[3] / 3600
      else                         NA_real_
    }, numeric(1))
    neg  <- is.na(hemi[need_dms]) & str_starts(x_chr[need_dms], "-")
    sw2  <- !is.na(hemi[need_dms]) & str_detect(hemi[need_dms], regex("[SWsw]"))
    dms  <- if_else(neg | sw2, -abs(dms), dms)
    result[need_dms] <- dms
  }
  
  if (is_lon) result <- if_else(!is.na(result) & (result < -180 | result > 180), NA_real_, result)
  else        result <- if_else(!is.na(result) & (result < -90  | result > 90),  NA_real_, result)
  
  n_failed <- sum(is.na(result) & !is_ph & !is.na(x_chr))
  if (n_failed > 0)
    warning(n_failed, " coordinate(s) could not be parsed in ", file_label,
            " [", col_label, "] — those rows will be dropped.")
  result
}

validate_coord_col <- function(df, detected_col, all_patterns) {
  if (!is.na(detected_col) && detected_col %in% names(df) &&
      count_real_values(suppressWarnings(as.numeric(df[[detected_col]]))) > 0)
    return(detected_col)
  for (pat in all_patterns) {
    for (candidate in names(df)[str_detect(names(df), regex(pat, ignore_case = TRUE))]) {
      if (!identical(candidate, detected_col) &&
          count_real_values(suppressWarnings(as.numeric(df[[candidate]]))) > 0)
        return(candidate)
    }
  }
  detected_col
}

interval_to_frequency_label <- function(median_days) {
  case_when(
    is.na(median_days)    ~ "Unknown",
    median_days <= 1      ~ "Daily or sub-daily",
    median_days <= 10     ~ "Weekly",
    median_days <= 45     ~ "Monthly",
    median_days <= 100    ~ "Quarterly",
    median_days <= 200    ~ "Semi-annual",
    median_days <= 400    ~ "Annual",
    TRUE                  ~ "Multi-year / Episodic"
  )
}

compute_parameter_frequency <- function(param_hits_df, raw_df, fallback_frequency) {
  
  file_date_map <- raw_df %>%
    distinct(source_file, detected_date_col) %>%
    filter(!is.na(detected_date_col))
  
  parsed_date_lookup <- pmap_dfr(file_date_map, function(source_file, detected_date_col) {
    raw_slice <- raw_df %>%
      filter(source_file == !!source_file) %>%
      select(source_file, source_row_id, any_of(detected_date_col))
    if (!detected_date_col %in% names(raw_slice)) return(tibble())
    raw_vals <- raw_slice[[detected_date_col]]
    raw_slice %>%
      mutate(
        parsed_date           = parse_date_time_safe(.data[[detected_date_col]]),
        frequency_source_hint = if_else(
          contains_seasonal_language(raw_vals),
          "estimated_from_seasonal_labels", "computed_from_dates"
        )
      ) %>%
      select(source_file, source_row_id, parsed_date, frequency_source_hint) %>%
      distinct(source_file, source_row_id, .keep_all = TRUE)
  })
  if (!all(c("source_file","source_row_id","parsed_date","frequency_source_hint") %in% names(parsed_date_lookup)))
    parsed_date_lookup <- tibble(source_file = character(), source_row_id = integer(),
                                 parsed_date = as.Date(character()), frequency_source_hint = character())
  
  hits_with_dates <- param_hits_df %>%
    left_join(parsed_date_lookup, by = c("source_file", "source_row_id"),
              relationship = "many-to-many")
  
  freq_from_dates <- hits_with_dates %>%
    filter(!is.na(parsed_date)) %>%
    group_by(source_file, standard_parameter) %>%
    summarise(
      n_obs                = n_distinct(parsed_date),
      frequency_source     = first(frequency_source_hint),
      median_interval_days = {
        d <- sort(unique(parsed_date))
        if (length(d) < 2) NA_real_ else {
          ivs <- as.numeric(diff(d))
          raw_med <- median(ivs)
          # Burst filter: if raw median is very small but long return gaps exist,
          # use the median of only the long gaps (>= 200 days) to capture
          # the true revisit cadence (e.g. annual surveys sampled over several
          # consecutive days produce median ≈ 1 day but max ≈ 350 days).
          lg <- ivs[ivs >= 90]
          if (length(lg) >= 2 && raw_med <= 30 && max(ivs) >= 90) {
            median(lg)
          } else {
            raw_med
          }
        }
      },
      .groups = "drop"
    ) %>%
    filter(!is.na(median_interval_days)) %>%
    mutate(median_interval_days = if_else(n_obs < 5, NA_real_, median_interval_days)) %>%
    filter(!is.na(median_interval_days)) %>%
    mutate(median_interval_days = if_else(
      frequency_source == "estimated_from_seasonal_labels" & between(median_interval_days, 75, 110),
      91.25, median_interval_days
    )) %>%
    group_by(standard_parameter) %>%
    slice_min(order_by = median_interval_days, n = 1, with_ties = FALSE) %>%
    ungroup()
  
  needs_fallback <- param_hits_df %>% distinct(standard_parameter) %>%
    anti_join(freq_from_dates, by = "standard_parameter")
  
  freq_from_years <- hits_with_dates %>%
    filter(standard_parameter %in% needs_fallback$standard_parameter,
           is.na(parsed_date), !is.na(year_detected)) %>%
    group_by(standard_parameter) %>%
    summarise(
      n_obs        = n(),
      n_years      = n_distinct(year_detected, na.rm = TRUE),
      obs_per_year = n() / max(n_distinct(year_detected, na.rm = TRUE), 1),
      .groups = "drop"
    ) %>%
    mutate(
      median_interval_days = case_when(
        n_years < 2         ~ NA_real_,
        obs_per_year >= 3   ~ 91.25,
        obs_per_year >= 1.5 ~ 182,
        TRUE                ~ 365
      ),
      frequency_source = "estimated_from_year_counts"
    ) %>%
    filter(!is.na(median_interval_days))
  
  bind_rows(
    freq_from_dates %>% select(standard_parameter, n_obs, median_interval_days, frequency_source),
    freq_from_years %>% select(standard_parameter, n_obs, median_interval_days, frequency_source)
  ) %>%
    mutate(frequency_label = interval_to_frequency_label(median_interval_days)) %>%
    right_join(param_hits_df %>% distinct(standard_parameter), by = "standard_parameter") %>%
    mutate(
      frequency_label  = coalesce(frequency_label, fallback_frequency, "Unknown"),
      frequency_source = coalesce(frequency_source, "program_metadata_fallback")
    ) %>%
    select(standard_parameter, frequency_label, frequency_source, median_interval_days, n_obs) %>%
    arrange(standard_parameter)
}

# =============================================================================
# COLUMN DETECTION PATTERNS
# =============================================================================

lat_priority_patterns  <- c("lat.*mid|mid.*lat", "^latitude$|^lat$|^lat_|_lat$|_lat_|latitude",
                            "^cc_lat", "lat.*start|start.*lat", "lat.*stop|stop.*lat")
lon_priority_patterns  <- c("lon.*mid|long.*mid|mid.*lon|mid.*long",
                            "^longitude$|^lon$|^long$|^lon_|_lon$|_lon_|^long_|_long$|longitude",
                            "^cc_lon", "lon.*start|long.*start|start.*lon|start.*long",
                            "lon.*stop|long.*stop|stop.*lon|stop.*long")
year_patterns    <- "^year$|^year_|_year$|sample_year|sampling_year|obs_year|collection_year|survey_year"
date_patterns    <- "^date$|^date_|_date$|sample_date|cast_date|collection_date|date_time|datetime|julian|^time$|timestamp"
depth_patterns   <- "^depth_m$|^depth$|^depth_|_depth$|depthm|bottom_depth|sample_depth|station_depth|mean_depth|pressure|^z$"
program_patterns <- "^program$|^program_|_program$|monitoring_program|program_name|project|agency|dataset"

# =============================================================================
# METADATA COLUMN EXCLUSION PATTERNS
# =============================================================================

metadata_patterns <- c(
  "^source_file$","^source_path$","^file_stub$","^program$","^source_row_id$","^detected_",
  "^year_detected$","^latitude_std$","^longitude_std$","^depth_std$",
  "^station_key$","^sample_point_key$","^detected_coord_role$",
  "^year$","^month$","^day$","^date$","^time$","^datetime$",
  "^lat$","^latitude$","^lon$","^long$","^longitude$",
  "^depth$","^depth_m$","^depthm$","^pressure$",
  "^sample_id$","^cast_id$","^event_id$","^record_id$","^id$","^index$",
  "^flag","^qc","^quality","^comment$","^comments$","^notes$","^note$",
  "^cruise","^agency$","^platform$","^vessel$","^ship$",
  "^hex_id$","^geometry_type$","^classification_reason$"
)

exclude_patterns_broad <- c(
  "_qual$","_qc$","_flag$","_prec$","_cnt$",
  "^btl_cnt$","^btl_num$","^btlnum$",
  "^station_lat$","^station_lon$","^lat$","^lon$","^long$","^latitude$","^longitude$",
  "^depth_id$","^station_id$","^sample_id$","^cast_id$","^event_id$","^record_id$","^taxon_id$",
  "^collection$","^female$","^male$","^sex$","^mesh_size$","^mesh$",
  "^scale_factor$","^conversion_factor$","^proportion$","^percent_sampled$",
  "^area$","^area_m2$","^bin_number$","^julian_date$","^julian_day$",
  "^latitude_start$","^latitude_stop$","^latitude_mid$",
  "^longitude_start$","^longitude_stop$","^longitude_mid$",
  "^time_sec$","^transect_number$","^length_m$","^width_m$",
  "^effort$","^observer$","^on_effort$","^off_effort$",
  "^segment$","^course$","^speed$","^bearing$","^distance$","^strip_width$",
  "^start_time$","^stop_time$","^end_time$",
  "^line$","^line_number$","^station$","^station_number$",
  "^cst_cnt$","^sta_id$","^depthm$","^recind$",
  "^t_prec$","^t_qual$","^s_prec$","^s_qual$","^p_qual$","^o_qual$",
  "^sthtaq$","^o2satq$","^chlqua$","^phaqua$","^invertebrate_weight","^po4q$","^sio3qu$",
  "^no2q$","^no3q$","^nh3q$","^dic_quality_comment$",
  "^lat_deg$","^lon_deg$","^lon_min$","^bottom_d$","^secchi$",
  "^ship_code$","^data_type$","^order_occ$","^cruz_leg$","^inc_str$","^inc_end$",
  "^pst_lan$","^civil_t$","^timezone$","^time_zone$",
  "^wave_dir$","^wave_ht$","^wave_prd$","^wind_dir$","^wind_spd$","^barometer$",
  "^dry_t$","^wet_t$","^wea$","^cloud_typ$","^cloud_amt$","^visibility$",
  "^int_chl$","^depth_strata$","^depth_stratum$","^strata$","^stratum$",
  "^c14as","^c14a","^darkas$","^darkap$","^darkaq$",
  "^meanas$","^meanap$","^meanaq$","^inctim$","^r_depth$","^r_temp$",
  "^r_sal$","^r_dynht$","^r_nuts$","^r_oxy","^wind_from_direction",
  "^eastward_sea_water_velocity","^relative_humidity",
  "^air_temperature","^air_pressure","^wind_speed","^lightp$",
  "^northward_sea_water_velocity","^z$", "^lat_uv$","^lon_uv$",
  "^seabird_size$",
  "^seabird_abundance$"
)

is_probably_metadata_column <- function(col_name) {
  str_detect(col_name, regex(paste(c(
    "^x$","^y$","^latitude$","^longitude$","^lat$","^lon$","^long$",
    "^date$","^time$","^datetime$","^year$","^month$","^day$","^timestamp$",
    "^station$","^station id$","^station number$","^cast$","^cast id$",
    "^bottle$","^btl$","^line$","^cruise$","^survey id$",
    "^sample id$","^record id$","^event id$","^id$","^index$","^depth id$",
    "^source file$","^source path$","^source row$","^file stub$","^program$",
    "^latitude std$","^longitude std$","^year detected$","^hex id$",
    "^detected lat$","^detected lon$","^sample point key$","^station key$",
    "^geometry type$","^classification reason$","^activity status$","^last year$",
    "^qual$","qual$","^prec$","^flag$"," flag$","^qc$"," qc$",
    "^r depth$","^r dynht$","^r nuts$","^r temp$","^r sal$","^r oxy$",
    "^darkas$","^darkap$","^darkaq$","^meanas$","^meanap$","^meanaq$",
    "^inctim$","^lightp$"
  ), collapse = "|"), ignore_case = TRUE))
}

# =============================================================================
# CONTEXT + PARAMETER DICTIONARIES
# =============================================================================

file_context_dictionary <- tribble(
  ~pattern, ~context,
  "\\btemp\\b|temperature",                                   "temperature",
  "\\bsalin|salinity",                                        "salinity",
  "\\boxygen\\b|\\bo2\\b|dissolved.*oxygen",                 "oxygen",
  "\\bdensity\\b|\\bsigma\\b|\\btheta\\b",                  "density",
  "\\bnitrate\\b|\\bno3\\b",                                 "nitrate",
  "\\bnitrite\\b|\\bno2\\b",                                 "nitrite",
  "\\bphosphate\\b|\\bpo4\\b",                               "phosphate",
  "\\bsilicate\\b|\\bsio3\\b",                               "silicate",
  "\\bammonium\\b|\\bnh3\\b|\\bnh4\\b",                     "ammonium",
  "\\bchlorophyll\\b|\\bchl\\b|chlora",                     "chlorophyll_a",
  "\\bphaeo\\b|phaeop",                                      "phaeopigments",
  "\\bproductivity\\b|\\b14c\\b|primary.*production.*14c|int_c14|c14.*uptake",             "primary_production",
  "\\bdic\\b|dissolved.*inorganic.*carbon|\\btco2\\b",      "dic",
  "\\balkalinity\\b|\\btalk\\b|total.*alk",                 "alkalinity",
  "\\bpco2\\b|p_co2|p co2",                                 "pco2",
  "\\bfco2\\b|f_co2|f co2",                                 "fco2",
  "(^|[_\\-\\s])ph([_\\-\\s]|$)|\\bseawater_ph\\b",        "ph",
  "transmissiv",                                             "transmissivity",
  "radiative.*flux|\\birradiance\\b|\\bpar\\b",             "radiative_flux",
  "marine.*mammal|\\bcetacean\\b|\\bwhale\\b|\\bdolphin\\b","marine_mammal_visual",
  "acoustic|bioacoustic",                                    "whale_acoustic",
  "\\bseabird\\b|sea.*bird",                                 "seabird",
  "fish.*egg|egg.*count|egg.*stage|continuous.*fish.*egg",  "fish_egg",
  "fish.*larvae|fish.*larval|\\blarval\\b|\\bichthyoplankton\\b|rockfish.*recruit", "fish_larvae",
  "\\binvertebrate\\b|\\binvertabrate\\b",                  "invertebrate",
  "\\bzooplankton\\b|\\bholoplankton\\b",                   "zooplankton",
  "\\bkrill\\b|\\beuphausii",                               "krill",
  "\\btrawl\\b|\\bhaul\\b|length.*frequency|\\bspecimen\\b","trawl_biota",
  "\\bpigment\\b|\\bhplc\\b|\\bfucoxanthin\\b",            "phytoplankton_pigments",
  "\\bphytoplankton\\b",                                    "phytoplankton_abundance",
  "\\bcdom\\b",                                             "cdom",
  "\\bphycoerythrin\\b|(^|[_\\-])pe([_\\-]|$)",            "pe_fluorescence",
  "\\bfluorescence\\b|\\bfluorometer\\b",                   "chlorophyll_fluorescence",
  "\\bmicrob|\\bbacteria\\b|\\b16s\\b|\\b18s\\b|\\botu\\b|\\basv\\b|\\bmrna\\b", "microbial",
  "picoplankton.*abundance|\\bprochlorococcus\\b|\\bsynechococcus\\b", "picoplankton_abundance",
  "bacterial.*abundance|bacteria.*abundance",               "bacterial_abundance",
  "\\bkelp\\b|macrocystis|\\bfrond\\b",                           "kelp",
  "\\bwrack\\b|wrack.*cover|wrack.*biomass|wrack.*volume",        "wrack"
)

column_pattern_dictionary <- tribble(
  ~pattern, ~measurement_type,
  "\\begg_count\\b|\\beggs_10m2\\b|\\beggs_100m3\\b",               "egg_count",
  "\\begg_stage\\b|\\begg_stages\\b",                                "egg_stage",
  "\\blarval_count\\b|\\blarval_abundance\\b",                       "larvae_count",
  "\\bdisplacement_volume\\b|\\bzooplankton_volume\\b",              "volume",
  "\\bstandard_length\\b|\\bfork_length\\b|\\btotal_length\\b",     "size",
  "\\bcount\\b|\\bnumber\\b",                                        "abundance",
  "\\bscientific_name\\b|\\btaxon\\b|\\bspecies\\b",                "observation_presence",
  "^sex$|^gender$",                                                  "sex_structure",
  "^weight$|^wet_weight$|^dry_weight$",                              "carbon_biomass",
  "\\bpigment\\b|\\bfucoxanthin\\b|\\bperidinin\\b",                "pigment",
  "\\bcdom\\b",                                                      "cdom",
  "\\bphycoerythrin\\b|pe_fluorescence",                            "pe_fluorescence",
  "\\bfluorescence\\b|\\bchl_fluorescence\\b",                      "chlorophyll_fluorescence",
  "\\bmrna\\b|\\bgene_expression\\b|\\btranscript\\b",              "mrna",
  "\\b16s\\b|\\b18s\\b|\\botu\\b|\\basv\\b|\\btaxonomy\\b",        "community_composition",
  "\\bcarbon\\b|\\bbiomass\\b",                                     "carbon_biomass",
  "\\btemperature\\b|\\bt_deg_c\\b|\\bctd_temp\\b",                "direct_value",
  "\\bsalinity\\b|\\bsalnty\\b|\\bctd_sal\\b",                     "direct_value",
  "\\boxygen\\b|\\bo2ml_l\\b|\\bdo_mgl\\b|oxy_mol_kg|dissolved_oxygen", "direct_value",
  "\\bsigma_theta\\b|\\bstheta\\b|\\bdensity\\b",                  "direct_value",
  "\\bnitrate\\b|\\bno3\\b|\\bno3um\\b",                           "direct_value",
  "\\bnitrite\\b|\\bno2\\b|\\bno2um\\b",                           "direct_value",
  "\\bphosphate\\b|\\bpo4\\b|\\bpo4um\\b",                         "direct_value",
  "\\bsilicate\\b|\\bsio3\\b|\\bsio3um\\b",                        "direct_value",
  "\\bammonium\\b|\\bnh3\\b|\\bnh4\\b|\\bnh3um\\b",               "direct_value",
  "\\bchlorophyll\\b|\\bchlora\\b|\\bchl_a\\b|\\bchlorophylla\\b", "direct_value",
  "\\bphaeo\\b|\\bphaeop\\b",                                       "direct_value",
  "\\bproductivity\\b|\\b14c\\b|primary.*prod|\\bintc14\\b",       "direct_value",
  "\\bdic\\b|dissolved.*inorganic.*carbon|\\bdic1\\b|\\bdic2\\b",  "direct_value",
  "\\btalk\\b|\\balkalinity\\b|\\bta1\\b|\\bta2\\b",              "direct_value",
  "^ph$|^p_h$|^ph_|\\bph1\\b|\\bph2\\b|sea.*ph|ph.*total",        "direct_value",
  "\\bpco2\\b|\\bfco2\\b",                                         "direct_value",
  "\\btransmissiv\\b|beam.*attenuation",                           "direct_value",
  "radiative.*flux|\\birradiance\\b|\\bpar\\b",                    "direct_value",
  "\\bo2sat\\b|oxygen.*sat|\\bdo_sat\\b|do.*percent",             "direct_value",
  "\\be\\.?\\s?coli\\b|escherichia.*coli",                         "direct_value",
  "\\benterococcus\\b|\\benterococci\\b",                          "direct_value",
  "total.*coliform|\\btotal_coliform\\b",                          "direct_value",
  "fecal.*coliform|faecal.*coliform|\\bfecal_coliform\\b",        "direct_value"
)

context_parameter_map <- tribble(
  ~context,               ~measurement_type,       ~standard_parameter,                                ~eov_group,
  "temperature",          "direct_value",          "Temperature",                                      "Physical",
  "salinity",             "direct_value",          "Salinity",                                         "Physical",
  "oxygen",               "direct_value",          "Dissolved Oxygen",                                 "Biogeochemical",
  "density",              "direct_value",          "Density (Sigma Theta)",                            "Physical",
  "nitrate",              "direct_value",          "Nitrate",                                          "Biogeochemical",
  "nitrite",              "direct_value",          "Nitrite",                                          "Biogeochemical",
  "phosphate",            "direct_value",          "Phosphate",                                        "Biogeochemical",
  "silicate",             "direct_value",          "Silicate",                                         "Biogeochemical",
  "ammonium",             "direct_value",          "Ammonium",                                         "Biogeochemical",
  "chlorophyll_a",        "direct_value",          "Chlorophyll-a",                                    "Biogeochemical",
  "phaeopigments",        "direct_value",          "Phaeopigments",                                    "Biogeochemical",
  "primary_production",   "direct_value",          "Primary Production (14C uptake)",                  "Biogeochemical",
  "dic",                  "direct_value",          "Dissolved Inorganic Carbon (DIC)",                 "Biogeochemical",
  "alkalinity",           "direct_value",          "Total Alkalinity",                                 "Biogeochemical",
  "ph",                   "direct_value",          "pH",                                               "Biogeochemical",
  "pco2",                 "direct_value",          "pCO2",                                             "Biogeochemical",
  "fco2",                 "direct_value",          "fCO2",                                             "Biogeochemical",
  "transmissivity",       "direct_value",          "Transmissivity",                                   "Physical",
  "radiative_flux",       "direct_value",          "Radiative Flux",                                   "Physical",
  "marine_mammal_visual", "observation_presence",  "Marine Mammal Abundance",                          "Biological",
  "whale_acoustic",       "observation_presence",  "Whale Acoustic",                                   "Biological",
  "fish_egg",             "egg_count",             "Fish Egg Counts",                                  "Biological",
  "fish_egg",             "egg_stage",             "Fish Egg Stages",                                  "Biological",
  "fish_egg",             "observation_presence",  "Fish Egg Presence",                                "Biological",
  "fish_larvae",          "larvae_count",          "Fish Larvae Counts",                               "Biological",
  "fish_larvae",          "observation_presence",  "Fish Larvae Presence",                             "Biological",
  "trawl_biota",          "abundance",             "Fish Abundance and Distribution",                  "Biological",
  "trawl_biota",          "size",                  "Fish Size",                                        "Biological",
  "trawl_biota",          "observation_presence",  "Fish Abundance and Distribution",                  "Biological",
  "invertebrate",         "size",                  "Invertebrate Size",                                "Biological",
  "invertebrate",         "abundance",             "Invertebrate Abundance",                           "Biological",
  "invertebrate",         "observation_presence",  "Invertebrate Abundance",                           "Biological",
  "zooplankton",          "abundance",             "Zooplankton Abundance",                            "Biological",
  "zooplankton",          "observation_presence",  "Zooplankton Abundance",                            "Biological",
  "krill",                "abundance",             "Krill (Euphausiid) Abundance",                     "Biological",
  "krill",                "observation_presence",  "Krill (Euphausiid) Abundance",                     "Biological",
  "phytoplankton_pigments","pigment",              "Phytoplankton Taxon-Specific Pigments",            "Biological",
  "phytoplankton_abundance","abundance",           "Phytoplankton Abundance",                          "Biological",
  "cdom",                 "cdom",                  "CDOM Fluorescence",                                "Biogeochemical",
  "pe_fluorescence",      "pe_fluorescence",       "Phycoerythrin (PE) Fluorescence",                  "Biogeochemical",
  "chlorophyll_fluorescence","chlorophyll_fluorescence","Chlorophyll fluorescence",                    "Biogeochemical",
  "microbial",            "community_composition", "Microbial community composition",                  "Biological",
  "microbial",            "mrna",                  "Microbial Genomics (mRNA)",                        "Biological",
  "picoplankton_abundance","abundance",            "Picoplankton Abundance",                           "Biological",
  "bacterial_abundance",  "abundance",             "Bacterial Abundance",                              "Biological",
  "kelp",                    "abundance",          "Kelp Abundance",                                   "Biological",
  "kelp",                  "direct_value",         "Algal Primary Production",                         "Biological",
  "wrack",                 "direct_value",         "Kelp Wrack Percent Cover",                         "Biological",
  "wrack",                 "direct_value",         "Kelp Wrack Volume",                                "Biological",
  "wrack",                 "direct_value",         "Kelp Wrack Biomass",                               "Biological",
)

parameter_dictionary <- tribble(
  ~standard_parameter,                     ~eov_group,       ~raw_pattern,                                                                                                                                                      ~detection_type, ~assignment_scope,
  "Temperature",                           "Physical",       "\\btemp\\b|\\btemperature\\b|t degc|t deg c|t_deg_c|ctd temp|ctdtemp|cc temp|water temp|\\bsst\\b",                                                              "measurement",   "column",
  "Salinity",                              "Physical",       "\\bsalinity\\b|\\bsalnty\\b|cc sal|ctd sal|ctdsal|practical salinity",                                                                                            "measurement",   "column",
  "Density (Sigma Theta)",                 "Physical",       "sigma theta|\\bsigma\\b|\\bstheta\\b|s theta|\\bdynht\\b|\\bdensity\\b",                                                                                         "measurement",   "column",
  "Transmissivity",                        "Physical",       "transmissiv|beam.*attenuation|optical.*attenuation",                                                                                                               "measurement",   "column",
  "Radiative Flux",                        "Physical",       "radiative.*flux|solar.*flux|\\birradiance\\b|\\bpar\\b",                                                                                                          "measurement",   "column",
  "Dissolved Oxygen",                      "Biogeochemical", "\\boxygen\\b|\\bo2ml\\b|o2ml l|oxy umol|dissolved.*oxygen|do_mg|do_mgl",                                                                                         "measurement",   "column",
  "Oxygen Saturation",                     "Biogeochemical", "o2sat|o2_sat|o2 sat|oxygen.*sat|sat.*oxygen|\\bdo_sat\\b|do.*percent",                                                                                           "measurement",   "column",
  "Nitrate",                               "Biogeochemical", "\\bno3\\b|\\bnitrate\\b|no3_um|no3um|no3u_m|no3u m|no3u",                                                                                                        "measurement",   "column",
  "Nitrite",                               "Biogeochemical", "\\bno2\\b|\\bnitrite\\b|no2_um|no2um|no2u_m|no2u m|no2u",                                                                                                        "measurement",   "column",
  "Phosphate",                             "Biogeochemical", "\\bpo4\\b|\\bphosphate\\b|po4_um|po4um|po4u_m|po4u m|po4u",                                                                                                      "measurement",   "column",
  "Silicate",                              "Biogeochemical", "\\bsio3\\b|\\bsilicate\\b|sio3_um|sio3um|sio3u_m|sio3u m|si_o3u_m|si o3u m|sio3u|si o3",                                                                       "measurement",   "column",
  "Turbidity",                             "Physical",       "\\bturbidity\\b|\\bntu\\b|light.*attenuation|beam.*attenuation|water.*clarity",                                                                                   "measurement",   "column",
  "Ammonium",                              "Biogeochemical", "\\bnh3\\b|\\bnh4\\b|\\bammonium\\b|\\bammonia\\b|nh3_um|nh4_um|nh3um|nh4um|nh3u_m|nh3u m|nh3u|n_h3u_m|n h3u m",                                               "measurement",   "column",
  "Chlorophyll-a",                         "Biogeochemical", "\\bchlorophyll\\b|cc chl|\\bchlora\\b|\\bchl_a\\b|\\bt_chla\\b|\\bchla\\b|\\blogchl\\b|chlor_a|chlor a|\\bchl[12]\\b|avg_chloro|chloro_mg",                    "measurement",   "column",
  "Sea Level",                             "Physical",       "sea.?surface.?height|\\bssh\\b|\\bwater_level\\b|\\bwater.level\\b|above_mllw|\\bmllw\\b|\\bsea_level\\b",                                                      "measurement",   "column",
  "Phaeopigments",                         "Biogeochemical", "\\bphaeop\\b|\\bphaeo\\b|\\bpheopigment\\b|phaeo[12]|avg_phaeo",                                                                                                "measurement",   "column",
  "Primary Production (14C uptake)",       "Biogeochemical", "\\b14c\\b|\\bc14\\b|14c uptake|primary.*prod|\\bproductivity\\b|lightp|light_p|light p|meanap|mean_as|mean as|int_c14|int c14",                                 "measurement",   "column",
  "Dissolved Inorganic Carbon (DIC)",      "Biogeochemical", "\\bdic\\b|dissolved inorganic carbon|\\bdic1\\b|\\bdic2\\b",                                                                                                     "measurement",   "column",
  "Total Alkalinity",                      "Biogeochemical", "\\btalk\\b|\\balkalinity\\b|total alkalinity|\\bta1\\b|\\bta2\\b",                                                                                               "measurement",   "column",
  "pH",                                    "Biogeochemical", "^ph$|^p_h$|^ph_|\\bph1\\b|\\bph2\\b|p_h1|p_h2|p h1|p h2|sea.*ph|ph.*total|seawater.*ph",                                                                      "measurement",   "column",
  "pCO2",                                  "Biogeochemical", "\\bpco2\\b|p co2|p_co2|co2 partial pressure",                                                                                                                    "measurement",   "column",
  "Aragonite Saturation State",            "Biogeochemical", "\\baragonite\\b|omega_aragonite|omega.*arag|arag.*sat",                                                                                                           "measurement",   "column",
  "CDOM Fluorescence",                     "Biogeochemical", "\\bcdom\\b|cdom[_ ]|chromophoric dissolved organic matter",                                                                                                       "measurement",   "column",
  "Chlorophyll fluorescence",              "Biogeochemical", "^cf_rb|^cf_rg|^fv_fm|\\bfluorescence\\b|\\bfluorometer\\b",                                                                                                     "measurement",   "column",
  "Phycoerythrin (PE) Fluorescence",       "Biogeochemical", "\\bphycoerythrin\\b|pe fluorescence|^pe1_|^pe2_|^pe3_",                                                                                                         "measurement",   "column",
  "Phytoplankton Taxon-Specific Pigments", "Biological",     "\\bpigment\\b|\\bfucoxanthin\\b|\\bperidinin\\b|\\bprasinoxanthin\\b|\\bviolaxanthin\\b",                                                                       "measurement",   "column",
  "Picoplankton Abundance",                "Biological",     "\\bprochlorococcus\\b|\\bsynechococcus\\b|\\bpicoeukaryote\\b|\\bpicoplankton_abundance\\b", "measurement", "column",
  "Bacterial Abundance",                   "Biological",     "\\bbacterial_abundance\\b|\\bheterotrophic_bacteria_abundance\\b|bacteria.*ug",                                                                                                       "measurement",   "column",
  "Phytoplankton Abundance",               "Biological",     "diatom|dinoflagellat|\\bdinoflag\\b|auto.*euk|total.*phyto|phytoplankton.*abundance",                                                                            "measurement",   "column",
  "Seabird Species",                       "Biological",     "seabird.*species|sea.*bird.*species",                                                                                                                             "observation",   "file_or_column",
  "Seabird Behavior",                      "Biological",     "seabird.*behavior",                                                                                                                                               "observation",   "file_or_column",
  "Marine Mammal Species",                 "Biological",     "marine mammal.*species|mammal.*species",                                                                                                                          "observation",   "file_or_column",
  "Marine Mammal Behavior",                "Biological",     "marine mammal.*behavior|mammal.*behavior",                                                                                                                        "observation",   "file_or_column",
  "Marine Mammal Abundance",               "Biological",     "marine mammal.*count|marine mammal.*abundance|mammal.*count",                                                                                                    "observation",   "file_or_column",
  "Fish Larvae Counts",                    "Biological",     "fish larvae.*count|larvae.*count|\\bichthyoplankton\\b|rockfish.*recruit",                                                                                       "observation",   "file_or_column",
  "Fish Egg Counts",                       "Biological",     "fish egg.*count|\\begg.*count\\b|continuous.*fish.*egg|sardine_eggs|anchovy_eggs|jack_mackerel_eggs|hake_eggs|squid_eggs|other_fish_eggs|\\beggs?$|_eggs$",                                                                                                         "observation",   "file_or_column",
  "Fish Egg Stages",                       "Biological",     "fish egg.*stage|\\begg.*stage\\b",                                                                                                                               "observation",   "file_or_column",
  "Fish Abundance and Distribution",       "Biological",     "scientific name|\\btaxon\\b|\\bspecies\\b|\\bspecimen\\b",                                                                                                       "observation",   "file_or_column",
  "Fish Size",                             "Biological",     "standard length|total length|fork length",                                                                                                                        "observation",   "file_or_column",
  "Fish Biomass",                          "Biological",     "^weight$|^wet_weight$|^dry_weight$|\\bbiomass\\b",                                                                                                               "observation",   "file_or_column",
  "Fish Population Structure",             "Biological",     "^sex$|^gender$|^male$|^female$",                                                                                                                                 "observation",   "file_or_column",
  "Zooplankton Volume",                    "Biological",     "zooplankton.*volume|\\bdisplacement.*volume\\b",                                                                                                                 "observation",   "file_or_column",
  "Zooplankton Abundance",                 "Biological",     "zooplankton.*abundance|zooplankton.*count",                                                                                                                       "observation",   "file_or_column",
  "Krill (Euphausiid) Abundance",          "Biological",     "\\bkrill\\b|\\beuphausii",                                                                                                                                      "observation",   "file_or_column",
  "Invertebrate Abundance",                "Biological",     "\\bsettlement\\b|\\bbrush.*density\\b|\\barthropoda\\b|\\bbivalvia\\b|\\bgastropoda\\b",                                                                                                      "observation",   "file_or_column",
  "Harmful Algal Blooms",                  "Biological",     "\\bpda\\b|\\btda\\b|\\bdda\\b|domoic|pseudo_nitzschia|alexandrium|dinophysis|lingulodinium|akashiwo|prorocentrum|cochlodinium|gymnodinium|ceratium|\\bcells_l\\b","measurement",  "column",
  "Plankton Carbon Biomass",               "Biological",     "(?<!phyto)(?<!pico)plankton.*carbon|carbon.*biomass.*(?<!phyto)(?<!pico)plankton", "measurement", "column",
  "Picoplankton Carbon Biomass",           "Biological",     "picoplankton.*carbon|picoplankton_carbon_biomass",                                  "measurement", "column",
  "Carbonate Ion Concentration",           "Biogeochemical", "^carbonate$|\\bcarbonate_ion\\b|\\bco3\\b",                                                                                                                      "measurement",   "column",
  "Bacterial Carbon Biomass",              "Biological",     "bacterial.*carbon|bacteria.*carbon|carbon.*biomass.*bact",                                                                                                       "measurement",   "column",
  "Bacterioplankton Abundance",            "Biological",     "\\bbacterioplankton\\b", "measurement", "column",  
  "Current Velocity",                      "Physical",       "\\bu_current\\b|\\bv_current\\b|current.*velocity|\\bADCP\\b",                                                                                                   "measurement",   "column",
  "Acoustic Backscatter",                  "Physical",       "\\bacoustic.*backscatter\\b|\\bbackscatter\\b|\\bvbs\\b|\\bsv\\b",                                                                                               "measurement",   "column",
  "Seabird Size",                          "Biological",     "\\bseabird_size\\b|seabird size",                                                                                                                                 "measurement",   "column",
  "Seabird Abundance",                     "Biological",     "\\bseabird_abundance\\b|seabird abundance",                                                                                                                       "measurement",   "column",
  "Escherichia coli",                      "Biological",     "\\be\\.?\\s?coli\\b|escherichia.*coli",                                                                                                                          "measurement",   "column",
  "Enterococcus",                          "Biological",     "\\benterococcus\\b|\\benterococci\\b",                                                                                                                            "measurement",   "column",
  "Total coliforms",                       "Biological",     "total.*coliform|\\btotal_coliform\\b",                                                                                                                            "measurement",   "column",
  "Fecal coliforms",                       "Biological",     "fecal.*coliform|faecal.*coliform|\\bfecal_coliform\\b",                                                                                                          "measurement",   "column",
  "Krill (Euphausiid) Biomass",            "Biological",     "krill.?biomass|euphausiid.?biomass|euphausia.?biomass|\\bepac\\b|epac.*biomass",                                                                                 "measurement",   "column",
  "Krill (Euphausiid) Size",               "Biological",     "adult.*length|epac.*length|krill.*length|krill.*size",                                                                                                            "measurement",   "column",
  "Optical Backscatter",                   "Physical",       "optical.*backscatter",                                                                                                                          "measurement",   "column",
  "Mussel Biomass",                        "Biological",     "mussel.*biomass|\\bmussel_biomass\\b",                                                                                                                            "measurement",   "column",
  "Invertebrate Biomass",                  "Biological",     "\\binvertebrate_biomass\\b|invertebrate.*biomass",                                                                                                                "measurement",   "column",
  "Algal Abundance",                       "Biological",     "\\balgal_abundance\\b|algal.*abundance",                                                                                                                         "measurement",   "column",
  "Kelp Abundance",                        "Biological",     "\\bkelp_abundance\\b|kelp.*abundance",                                                                                                                           "measurement",   "column",
  "Benthic Abundance",                     "Biological",     "\\bbenthic_abundance\\b",                                                                                                                                        "measurement",   "column",
  "Benthic Percent Cover",                 "Biological",     "benthic.*percent.*cover|\\bbenthic_percent_cover\\b",                                                                                                            "measurement",   "column",
  "fCO2",                                  "Biogeochemical", "\\bfco2\\b|f co2|f_co2|fco2 sw|f co2 sw|fco2.*sst|f co2.*sst",                                                                                                  "measurement",   "column",
  "Algal Percent Cover",                   "Biological",     "\\bpercent_cover\\b|\\balgal.*cover\\b",                                       "measurement", "column",
  "Invertebrate Percent Cover",            "Biological",     "\\binvertebrate.*cover\\b","measurement", "column",
  "Algal Primary Production",              "Biological",     "\\bnpp\\b|net.*primary.*prod|algal.*prod|npp_season|npp_gc",                   "measurement", "column",
  "Algal Biomass",                         "Biological",     "\\balgal.*biomass\\b|\\bwet_wt\\b|\\bdry.*mass\\b|\\bafdm\\b|\\bsfdm\\b",     "measurement", "column",
  "Kelp Abundance",                        "Biological",     "\\bfronds\\b|\\bkelp.*count\\b|\\bmacrocystis\\b",                             "measurement", "column",
  "Kelp Wrack Percent Cover",              "Biological",     "\\bwrack_cover\\b",                                                            "measurement", "column",
  "Kelp Wrack Volume",                     "Biological",     "\\bwrack_volume\\b",                                                           "measurement", "column",
  "Kelp Wrack Biomass",                    "Biological",     "\\bwrack_biomass\\b",                                                          "measurement", "column",
  "Invertebrate Abundance",                "Biological",     "\\bsettlement\\b|\\bbrush.*density\\b|\\barthropoda\\b|\\bbivalvia\\b|\\bgastropoda\\b", "measurement", "column",
  "Invertebrate Biomass",                  "Biological",     "\\bwet_weight\\b|\\bwet.*mass\\b",                                             "measurement", "column",
  "Lobster Abundance",                     "Biological",     "\\blobster.*count\\b|\\bpanulirus\\b|\\bspiny.*lobster\\b",          "measurement", "column",
  "Lobster Size",                          "Biological",     "\\blobster.*size\\b|\\bcarapace.*length\\b|\\bsize_mm\\b",            "measurement", "column",
  "Carbon (Kelp Tissue)",                  "Biological",     "\\bpercent_carbon\\b|\\bnpp_carbon\\b|\\bfsc_carbon\\b|\\bc_mass\\b", "measurement", "column",
  "Nitrogen (Kelp Tissue)",                "Biological",     "\\bpercent_nitrogen\\b|\\bnpp_nitrogen\\b|\\bfsc_nitrogen\\b",        "measurement", "column",
  "Total Dissolved Nitrogen",              "Biogeochemical", "total_dissolved_nitrogen|\\btdn\\b",                                          "measurement", "column",
  "Total Dissolved Phosphorus",            "Biogeochemical", "total_dissolved_phosphorus|\\btdp\\b",                                        "measurement", "column",
  "Particulate Inorganic Carbon",          "Biogeochemical", "particulate_inorganic_carbon|\\bpic\\b",                                      "measurement", "column",
  "Particulate Inorganic Nitrogen",        "Biogeochemical", "particulate_inorganic_nitrogen|\\bpin\\b",                                    "measurement", "column",
  "Particulate Biogenic Silica",           "Biogeochemical", "particulate_biogenic_silica|\\bpbsi\\b|\\bbsi\\b",                            "measurement", "column",
  "Lithogenic Silica",                     "Biogeochemical", "\\blithogenic_silica\\b|\\blsi\\b",                                           "measurement", "column",
  "Particulate Organic Carbon",            "Biogeochemical", "\\bpoc\\b|particulate_organic_carbon|particulate_carbon(?!.*flux)", "measurement", "column",
  "Particulate Organic Nitrogen",          "Biogeochemical", "\\bpon\\b|particulate_organic_nitrogen",                                      "measurement", "column",
  "pCO2",                                  "Biogeochemical", "\\bcarbon_dioxide\\b|carbon_dioxide_partial_pressure|\\bxco2\\b",  "measurement", "column",
  "Bicarbonate Ion Concentration",         "Biogeochemical", "\\bbicarbonate_ion\\b|\\bhco3\\b|\\bbicarbonate\\b",              "measurement", "column",
  "Calcite Saturation State",              "Biogeochemical", "\\bcalcite\\b|omega_calcite|omega.*calc",                          "measurement", "column",
  "Kelp Size",                             "Biological",     "\\bholdfast\\b|\\bstipe.*length\\b|\\bfrond.*length\\b|\\bkelp.*size\\b|\\bkelp.*diameter\\b", "measurement", "column",
  "Fish Abundance and Distribution",       "Biological",     "\\bfish_abundace\\b|\\bfish_abundance\\b",                          "measurement", "column",
  "Fish Size",                             "Biological",     "\\bfish_size\\b",                                                    "measurement", "column",
  "Fish Biomass",                          "Biological",     "\\bfish_biomass\\b|\\bbiomass_density\\b|\\bfish_standing_stock\\b", "measurement", "column",
  "Fish Spawning Activity",                "Biological",     "\\bfish_spawning_activity\\b|\\bfish_spawning\\b|\\bspawn_start_date\\b|\\bspawn_end_date\\b", "measurement", "column",
  "Fish Species Richness",                 "Biological",     "\\bfish_richness\\b",                                               "measurement", "column",
  "Kelp Abundance",                        "Biological",     "\\bkelp_abundace\\b|\\bkelp_abundance\\b",                          "measurement", "column",
  "Kelp Size",                             "Biological",     "\\bkelp_size\\b",                                                   "measurement", "column",
  "Benthic Substrate Cover",               "Biological",     "\\bbenthic_substrate_cover\\b|\\bsubstrate_cover\\b|\\bsubstrate_code\\b", "measurement", "column",
  "Algal Abundance",                       "Biological",     "\\balgal_abundance\\b|\\balgal_abundace\\b",                        "measurement", "column",
  "Invertebrate Abundance",                "Biological",     "\\binvertebrate_abundance\\b|\\binvertebrate_abundace\\b",          "measurement", "column",
  "Algal Percent Cover",                   "Biological",     "\\balgal_percent_cover\\b",                                         "measurement", "column",
  "Invertebrate Percent Cover",            "Biological",     "\\binvertebrate_percent_cover\\b",                                  "measurement", "column",
  "Invertebrate Species Richness",         "Biological",     "\\binvertebrate_richness\\b",                                       "measurement", "column",
  "Algal Species Richness",                "Biological",     "\\balgal_richness\\b",                                              "measurement", "column",
  "Phytoplankton Carbon Biomass",          "Biological",     "phytoplankton.*carbon|phytoplankton.*biomass|phytoplankton_carbon_biomass", "measurement", "column",
  "Dissolved Organic Carbon",              "Biogeochemical", "\\bdoc\\b|dissolved_organic_carbon|dissolved.*organic.*carbon", "measurement", "column",
  "Trace Metals",                          "Biogeochemical", "\\biron\\b|\\blead\\b|\\bnickel\\b|\\bcopper\\b|\\bzinc\\b|\\bmanganese\\b|\\bcadmium\\b|\\bcobalt\\b|total_dissolvable_iron|\\btdi\\b", "measurement", "column",
  "Particulate Carbon Flux",               "Biogeochemical", "\\bcarbon_flux\\b|carbon.*flux", "measurement", "column",
  "Particulate Nitrogen Flux",             "Biogeochemical", "\\bnitrogen_flux\\b|nitrogen.*flux", "measurement", "column",
  "Picoplankton Carbon Biomass",           "Biological",     "picoplankton.*carbon|picoplankton_carbon_biomass", "measurement", "column",  # already exists
  "Phytoplankton Carbon Biomass",          "Biological",     "phytoplankton.*carbon|phytoplankton.*biomass|phytoplankton_carbon_biomass", "measurement", "column",
  "Size-Fractionated Chlorophyll",         "Biogeochemical", "chlorophyll_a_lt|chl_a_lt|chl.*[<>]|fractionated.*chl|chl.*fraction", "measurement", "column",
  "Phytoplankton Carbon Biomass",          "Biological",     "phytoplankton.*carbon|phytoplankton.*biomass|phytoplankton_carbon_biomass", "measurement", "column",
  "Picoplankton Carbon Biomass",           "Biological",     "picoplankton_biomass(?!.*carbon)",                       "measurement", "column",
  "Phytoplankton Carbon Biomass",          "Biological",     "phytoplankton_biomass(?!.*carbon)",                      "measurement", "column",
  "Particulate Organic Nitrogen",          "Biogeochemical", "\\bpon\\b|particulate_organic_nitrogen|particulate_nitrogen", "measurement", "column",
  "Bacterial Carbon Biomass",              "Biological",     "bacterial.*carbon|bacteria.*carbon|carbon.*biomass.*bact|heterotrophic_bacteria_biomass|heterotrophic.*biomass", "measurement", "column",
  "Phaeopigments",                         "Biogeochemical", "\\bphaeop\\b|\\bphaeo\\b|\\bpheopigment\\b|phaeo[12]|avg_phaeo|phaeopigments|phaeopigment", "measurement", "column",
  "Mussel Condition (Histopathology)",     "Biological",     "\\bhistopath\\b|\\bhistopathology\\b|\\babnormality\\b|gonad.*condition|mussel.*condition", "measurement", "column",
  "Mussel Size",                           "Biological",     "\\bmussel_length\\b|\\bmussel_size\\b",                    "measurement", "column",
  "Mussel Biomass",                        "Biological",     "\\bmussel_wet_weight\\b|\\bmussel_biomass\\b|\\bmussel_dry_weight\\b", "measurement", "column",
  "Mussel Population Structure (Sex)",     "Biological",     "\\bmussel_sex\\b",                                      "measurement", "column",
  "Bioaccumulative Contaminants (Mussel Tissue)", "Biogeochemical", "bioaccumulative_contaminants_mussel|mussel.*tissue.*contaminant", "measurement", "column",
  "Bioaccumulative Contaminants (Sediment)",      "Biogeochemical", "bioaccumulative_contaminants_sediment|sediment.*contaminant",    "measurement", "column",
  "Trace Metals (Mussel Tissue)",          "Biogeochemical", "trace_elements_mussel_tissue|trace.*metal.*mussel|mussel.*trace.*metal", "measurement", "column",
  "Trace Metals (Sediment)",               "Biogeochemical", "trace_elements_sediment|trace.*metal.*sediment|sediment.*trace.*metal",  "measurement", "column",
  "Sewage Indicator",                      "Biological",     "sewage_indicator|clostridium|clostridium.*perfringens",                  "measurement", "column",
  "Benthic Percent Cover",                 "Biological",     "benthic.*percent.*cover|\\bbenthic_percent_cover\\b", "measurement", "column",
  "Benthic Cover",                         "Biological",     "\\bbenthic_cover\\b", "measurement", "column",
  "Invertebrate Percent Cover",            "Biological",     "\\binvertebrate.*cover\\b", "measurement", "column",
  "Beach Morphology",                      "Physical",       "\\bbeach_morphology\\b|foredune.*slope|beach.*slope",     "measurement", "column",
  "Swash Characteristics",                 "Physical",       "\\bswash_characteristics\\b|swash.*limit|swash.*abundance","measurement", "column",
  "Seagrass Abundance",                    "Biological",     "\\bseagrass_abundance\\b|seagrass.*abundance",             "measurement", "column",
  "Beach Wrack",                           "Biological",     "\\bwrack\\b|beach.*wrack",                                 "measurement", "column",
  "Kelp Canopy Cover",                     "Biological",     "\\bkelp_canopy_cover\\b|kelp.*canopy|canopy.*cover|kelp canopy cover", "measurement", "column",
  "Algal Size",                            "Biological",     "\\balgal_size\\b|algal.*size", "measurement", "column",
  "Kelp Disease/Condition",                "Biological",     "\\bkelp_disease\\b|\\bdisease\\b|kelp.*condition|kelp.*health", "measurement", "column",
  "Kelp Population Structure",             "Biological",     "\\bkelp_population_structure\\b|\\bkelp.*sex\\b|\\bkelp.*age\\b|\\bkelp.*stage\\b|\\bkelp.*recruit\\b", "measurement", "column",
  "Benthic Infauna Abundance",             "Biological",     "\\bbenthic_infauna_abundance\\b|benthic.*infauna",          "measurement", "column",
  "Sediment Cover",                        "Biological",     "\\bsediment_cover\\b|\\bsediment\\b",                      "measurement", "column",
  "Species Richness",                      "Biological",     "\\bspecies_richness\\b|biodiversity.*index",           "measurement", "column",
  "Benthic Infauna Abundance",             "Biological",     "\\bbenthic_infauna_abundance\\b|benthic.*infauna",      "measurement", "column",
  "Sediment Cover",                        "Biological",     "\\bsediment_cover\\b|\\bsediment\\b",                  "measurement", "column",
  "Invertebrate Species Richness",         "Biological",     "\\binvertebrate_richness\\b|invertebrate.*richness",    "measurement", "column",
  "Kelp Canopy Cover",                     "Biological",     "\\bkelp_canopy_cover\\b|kelp.*canopy|canopy.*cover",   "measurement", "column",
  "Environmental DNA (eDNA)",              "Biological",     "\\bedna\\b|environmental_dna", "measurement", "column"
  
)

observation_context_rules <- tribble(
  ~context_name,               ~file_pattern,                                                                     ~column_pattern,                                                            ~standard_parameter,                 ~eov_group,
  "seabird_species",           "\\bseabird\\b|sea.*bird",                                                         "species|common_name|scientific_name|taxon",                                "Seabird Species",                   "Biological",
  "seabird_behavior",          "\\bseabird\\b|sea.*bird",                                                         "behavior|behaviour",                                                       "Seabird Behavior",                  "Biological",
  "seabird_species_generic",   "\\bseabird\\b|sea.*bird",                                                         "\\btransect\\b|\\bcruise\\b|\\bsurvey\\b",                                "Seabird Species",                   "Biological",
  "seabird_behavior_generic",  "\\bseabird\\b|sea.*bird",                                                         "\\btransect\\b|\\bcruise\\b|\\bsurvey\\b",                                "Seabird Behavior",                  "Biological",
  "mm_species",                "marine.?mammal|\\bcetacean\\b|\\bwhale\\b|\\bdolphin\\b",                         "species|common_name|scientific_name|\\btaxon\\b",                         "Marine Mammal Species",             "Biological",
  "mm_behavior",               "marine.?mammal|\\bcetacean\\b|\\bwhale\\b|\\bdolphin\\b",                         "behavior|behaviour",                                                       "Marine Mammal Behavior",            "Biological",
  "mm_abundance",              "marine.?mammal|\\bcetacean\\b|\\bwhale\\b|\\bdolphin\\b",                         "\\bcount\\b|\\babundance\\b|\\bnumber\\b|\\bpod\\b",                      "Marine Mammal Abundance",           "Biological",
  "mm_presence_generic",       "marine.?mammal|\\bcetacean\\b|\\bwhale\\b|\\bdolphin\\b",                         "\\btransect\\b|\\bcruise\\b|\\bsurvey\\b",                                "Marine Mammal Abundance",           "Biological",
  "mm_species_generic",        "marine.?mammal|\\bcetacean\\b|\\bwhale\\b|\\bdolphin\\b",                         "\\btransect\\b|\\bcruise\\b|\\bsurvey\\b",                                "Marine Mammal Species",             "Biological",
  "larvae_count",              "fish.*larvae|\\blarval\\b|\\bichthyoplankton\\b",                                  "\\bcount\\b|\\babundance\\b|\\bnumber\\b",                                "Fish Larvae Counts",                "Biological",
  "egg_count",                 "fish.*egg|continuous.*fish.*egg",                                                  "\\bcount\\b|\\babundance\\b|\\bnumber\\b|_eggs$|eggs_",                   "Fish Egg Counts",                   "Biological",
  "egg_stage",                 "fish.*egg|continuous.*fish.*egg",                                                  "\\bstage\\b",                                                              "Fish Egg Stages",                   "Biological",
  "zoo_volume",                "\\bzooplankton\\b",                                                                "\\bvolume\\b|displacement",                                               "Zooplankton Volume",                "Biological",
  "rreas_catch_larvae",        "\\bRREAS\\b",                                                                      "\\bcatch\\b|\\babundance\\b",                                             "Fish Larvae Counts",                "Biological",
  "rreas_catch_fish",          "\\bRREAS\\b|catch.*data|fish.*abundance",                                         "\\bcatch\\b|\\babundance\\b",                                             "Fish Abundance and Distribution",   "Biological",
  "rreas_catch_krill",         "\\bRREAS\\b",                                                                      "\\bcatch\\b",                                                              "Krill (Euphausiid) Abundance",      "Biological",
  "rreas_catch_invert",        "\\bRREAS\\b",                                                                      "\\bcatch\\b",                                                              "Invertebrate Abundance",            "Biological",
  "rreas_catch_zoo",           "\\bRREAS\\b",                                                                      "\\bcatch\\b",                                                              "Zooplankton Abundance",             "Biological",
  "zoo_abundance",             "\\bzooplankton\\b",                                                                "\\bcount\\b|\\babundance\\b|\\bnumber\\b",                                "Zooplankton Abundance",             "Biological",
  "invert_size",               "\\binvertebrate\\b|\\binvertabrate\\b",                                            "\\bsize\\b|\\blength\\b",                                                 "Invertebrate Size",                 "Biological",
  "invert_abund",              "\\binvertebrate\\b|\\binvertabrate\\b|\\bbycatch\\b|\\btrawl\\b",                  "\\bcount\\b|\\babundance\\b|\\bnumber\\b",                                "Invertebrate Abundance",            "Biological",
  "krill_abund",               "\\bkrill\\b|\\beuphausii",                                                        "\\bcount\\b|\\babundance\\b|\\bnumber\\b",                                "Krill (Euphausiid) Abundance",      "Biological",
  "fish_specimen_presence",    "\\bcps\\b|\\bfish\\b|\\btrawl\\b|\\bhaul\\b|\\bspecimen\\b",                      "scientific_name|\\btaxon\\b|\\bspecies\\b",                               "Fish Abundance and Distribution",   "Biological",
  "fish_specimen_size",        "\\bcps\\b|\\bfish\\b|\\btrawl\\b|\\bhaul\\b|\\bspecimen\\b",                      "standard_length|fork_length|total_length",                                 "Fish Population Structure",         "Biological",
  "fish_specimen_biomass",     "\\bcps\\b|\\bfish\\b|\\btrawl\\b|\\bhaul\\b|\\bspecimen\\b",                      "^weight$|^wet_weight$|^dry_weight$",                                       "Fish Biomass",                      "Biological",
  "fish_specimen_sex",         "\\bcps\\b|\\bfish\\b|\\btrawl\\b|\\bhaul\\b|\\bspecimen\\b",                      "^sex$|^gender$",                                                           "Fish Population Structure",         "Biological",
  "cps_catch_abundance",       "\\bCPS\\b|[Cc]atch[Aa]bundance|[Cc]atch.*[Aa]bundance|[Cc]atch.*[Bb]iomass",     "\\babundance\\b|\\bbiomass\\b|\\bcount\\b|scientific_name|\\bspecies\\b",  "Catch Abundance",                   "Biological",
  "cps_catch_biomass",         "\\bCPS\\b|[Cc]atch[Bb]iomass|[Cc]atch.*[Bb]iomass",                              "\\bbiomass\\b|\\bweight\\b",                                               "Catch Biomass",                     "Biological",
  "cps_catch_size",            "\\bCPS\\b|[Cc]atch[Ss]ize|[Cc]atch.*[Ss]ize|[Cc]atch.*[Ss]tructure|[Cc]atch.*[Pp]opulation", "\\blength\\b|\\bsize\\b|\\bfork\\b|\\bstandard\\b|\\btotal.*length\\b", "Catch Size",               "Biological",
  "cps_catch_sex",             "\\bCPS\\b|[Ll]ife.*[Hh]istory|[Ss]pecimen|[Cc]atch.*[Ss]tructure|[Cc]atch.*[Pp]opulation",   "^sex$|^gender$|\\bsex\\b",                                   "Catch Population Structure (Sex)",  "Biological",
  "cps_specimen_abund",        "\\bCPS\\b|[Ll]ife.*[Hh]istory|[Ss]pecimen|[Cc]atch.*[Dd]ata",                    "scientific_name|\\btaxon\\b|\\bspecies\\b",                               "Catch Abundance",                   "Biological",
  "seabird_abundance_obs",     "\\bseabird\\b|sea.*bird",                                                         "\\bcount\\b|\\babundance\\b|\\bnumber\\b",                                "Seabird Abundance",                 "Biological",
  "songs_fish_transect",       "UCSB_SONGS.*Transect.*Fish|Transect.*Fish",                                       "fish_abundace|fish_abundance|fish_size",                                   "Fish Abundance and Distribution",   "Biological",
  "songs_fish_transect2",      "UCSB_SONGS.*Transect.*Fish|Transect.*Fish",                                       "fish_size",                                                                "Fish Size",                         "Biological",
  "songs_kelp_transect",       "UCSB_SONGS.*Transect.*Kelp|Transect.*Kelp",                                       "kelp_abundance|kelp_abundace|kelp_size",                                   "Kelp Abundance",                    "Biological",
  "songs_kelp_size",           "UCSB_SONGS.*Transect.*Kelp|Transect.*Kelp",                                       "kelp_size",                                                                "Kelp Size",                         "Biological",
  "songs_fish_spawn",          "FishSpawning|fish.*spawn",                                                        "fish_spawning_activity|spawn_start|spawn_end",                            "Fish Spawning Activity",            "Biological",
  "songs_fish_richness",       "FishAbundance.*FishRichness|FishRichness",                                        "fish_richness",                                                            "Fish Species Richness",             "Biological",
  "cciea_hci",                 "CCIEA|cciea|habitat.*compression",                                                "zone_name|lat_range|parameters",                                           "Habitat Compression Index",         "Physical",
  "cciea_upwelling",           "CCIEA|cciea|upwelling",                                                           "zone_name|lat_range|parameters",                                           "Upwelling Index",                   "Physical",
  "cciea_nitrate",             "CCIEA|cciea|nitrate|beuti",                                                       "zone_name|lat_range|parameters",                                           "Nitrate",                           "Biogeochemical",
  "cciea_current",             "CCIEA|cciea|current.*velocity|cuti",                                              "zone_name|lat_range|parameters",                                           "Current Velocity",                  "Physical",
  "cciea_temp",                "CCIEA|cciea|temperature",                                                         "zone_name|lat_range|parameters",                                           "Temperature",                       "Physical",
  "edna_genomics",             "\\bedna\\b|\\bncog\\b|\\b16s\\b|\\b18s\\b|\\bamplicon\\b",                        "\\bdate\\b|\\blat\\b|\\blon\\b|\\bsample_id\\b",                          "Environmental DNA (eDNA)",          "Biological"
)

filename_parameter_dictionary <- tibble(
  pattern = c(
    "\\btemp\\b|\\btemperature\\b",
    "\\bsalinity\\b|\\bsalin",
    "saturated[_ ]oxygen|oxygen[_ ]saturation|oxygen.*sat|o2.*sat|oxygen_sat",
    "\\bchl\\b|\\bchlorophyll\\b",
    "\\bnitrate\\b|\\bnitrite\\b|\\bammonium\\b|\\bphosphate\\b|\\bsilicate\\b|\\bnutrients\\b",
    "\\bdic\\b|dissolved_inorganic_carbon",
    "\\baragonite\\b|omega.*aragonite",
    "\\balkalinity\\b|\\btalk\\b",
    "(^|[_\\-])ph([_\\-]|$)|seawater_ph",
    "\\bfco2\\b|fco2.*underway|underway.*fco2|f_co2",
    "transmissiv",
    "radiative.*flux|\\birradiance\\b",
    "\\bproductivity\\b|\\b14c\\b|primary.*prod",
    "fish.*egg|egg.*count|FishEgg|EggCount",
    "\\blarva|\\blarval\\b|\\bichthyoplankton\\b",
    "BenthicAbundance|Benthic_Abundance|benthic.*abundance|benthic.*infauna",
    "SedimentChem|Sediment_Chem|sediment.*chem",
    "SurveyToxic|SedimentToxic|sediment.*toxic|survey.*toxic|\\btoxicit",
    "Toxicity[Rr]esult|ToxicityWQ|toxicity.*result|toxicity.*wq",
    "[Tt]rash|[Dd]ebris|[Pp]lastic.*[Bb]enthic|[Ee]pibenthic",
    "total.*dissolvable.*iron",
    "[Hh]armful.*[Aa]lgal|\\bHAB\\b|algal.*bloom",
    "\\bmammal\\b|\\bcetacean\\b|\\bwhale\\b",
    "\\bseabird\\b|sea.*bird",
    "\\bacoustic\\b|bioacoustic",
    "\\bcdom\\b|cdom[_ ]|chromophoric dissolved organic matter",
    "\\bphycoerythrin\\b|(^|[_\\-])pe([_\\-]|$)",
    "microbial.*assemblag|community.*assemblag|\\b16s\\b|\\b18s\\b|\\bmrna\\b|\\botu\\b|\\basv\\b",
    "\\b16s\\b|\\b18s\\b|\\bmrna\\b|\\botu\\b|\\basv\\b",   
    "\\binvertebrate\\b|\\bspecimen\\b|\\bbycatch\\b|\\btrawl\\b",
    "InvertebrateBiomass|Invertebrate_Biomass|invertebrate.*biomass",
    "FishBiomass|Fish_Biomass|fish.*biomass",
    "FishAbundance|Fish_Abundance|fish.*abundance",
    "BiologicalTrait|Biological_Trait|BiologicalTraits|bio.*trait",
    "WaterQuality|Water_Quality|water.*qual",
    "EcosystemProd|Ecosystem_Prod|EcosystemProductivity|ecosystem.*prod",
    "TrophicStructure|Trophic_Structure|trophic.*struct",
    "\\bRREAS\\b.*[Cc]atch|rockfish.*recruit|[Rr]ockfish.*[Rr]ecruitment",
    "\\bturbidity\\b|\\bntu\\b",
    "water[_ ]surface[_ ]height|WaterSurfaceHeight|sea.?surface.?height|sea.?level|water.?level|\\bmllw\\b",
    "(?i)(^|[_])Depth([_]|$)",
    "\\bbacterioplankton\\b",
    "\\bmethane\\b|\\bch4\\b",
    "\\bpoc\\b|particulate.*organic.*carbon",
    "\\bpon\\b|particulate.*organic.*nitrogen",
    "FishPopulationStructure|Fish_Population_Structure|fish.*population.*struct",
    "u_current|v_current|current.?velocity|CurrentVelocity|currentvelocity|\\bADCP\\b|eastward.*current|northward.*current|surface.*current",
    "optical[_ ]backscatter|OpticalBackscatter|optical.*backscatter|\\bopbs\\b|AcousticBackscatter|acoustic.*backscatter",
    "CPS.*[Cc]atch[Aa]bundance|CPS.*[Cc]atch.*[Aa]bundance|CPS.*[Nn]earshore.*[Ss]et.*[Cc]atch|CPS.*[Ll]ife.*[Hh]istory",
    "CPS.*[Cc]atch[Bb]iomass|CPS.*[Cc]atch.*[Bb]iomass",
    "CPS.*[Cc]atch[Ss]ize|CPS.*[Cc]atch.*[Ss]ize|[Cc]atch[Pp]opulation[Ss]tructure",
    "CPS.*[Ll]ife.*[Hh]istory.*[Ss]pecimen|CPS.*[Ss]pecimen",
    "[Ii]nvertebrate.*[Ss]ize",
    "[Ii]nvertebrate.*[Aa]bundance",
    "[Ff]ish[Ll]arvae[Cc]ounts|[Ff]ish.*[Ll]arvae.*[Cc]ount|CALCOFI.*[Ff]ish.*[Ll]arvae",
    "[Ss]eabird.*[Ss]ize|[Ss]eabirds.*[Ss]ize",
    "[Ss]eabird.*[Aa]bundance|[Ss]eabirds.*[Aa]bundance",
    "marine.*mammal.*species|mammal.*species",
    "marine.*mammal.*behavior|mammal.*behavior",
    "[Ss]eabird.*[Ss]pecies|[Ss]eabirds.*[Ss]pecies",
    "[Ss]eabird.*[Bb]ehavior|[Ss]eabirds.*[Bb]ehavior",
    "total.*coliform|\\btotal_coliform\\b",
    "\\benterococcus\\b|\\benterococci\\b",
    "fecal.*coliform|faecal.*coliform|\\bfecal_coliform\\b",
    "\\be\\.?\\s?coli\\b|escherichia.*coli",
    "KrillBiomass|Krill.*Biomass|epacBiomass",
    "KrillSize|Krill.*Size|epacLength",
    "water[_ ]surface[_ ]height|WaterSurfaceHeight|sea.?surface.?height|sea.?level|water.?level|\\bmllw\\b",
    "optical.?backscatter|OpticalBackscatter|optical.*backscatter|\\bbackscatter\\b|\\bopbs\\b",
    "krill.?biomass|euphausiid.?biomass|euphausia.?biomass|\\bepac\\b|epac.*biomass",
    "optical.?backscatter|OpticalBackscatter|optical.*backscatter|\\bbackscatter\\b|\\bopbs\\b",
    "\\bcdom\\b|cdom.?fluorescence|cdom[_ ]|chromophoric dissolved organic matter",
    "u_current|v_current|current.?velocity|CurrentVelocity|currentvelocity|\\bADCP\\b|eastward.*current|northward.*current|surface.*current",
    "Cleaned.*Current.*Use.*Pesticides|Current.*Use.*Pesticides",
    "Cleaned.*Heavy.*Metals|Heavy.*Metals.*Fish|heavy.*metals",
    "Cleaned.*Organochlorine|Organochlorine.*Pesticides",
    "Cleaned.*POPs|\\bPOPs\\b|persistent.*organic.*pollutant",
    "AlgalPercentCover|Algal_Percent_Cover|algal.*percent.*cover",
    "InvertebratePercentCover|Invertebrate_Percent_Cover|invertebrate.*percent.*cover",
    "AlgalPrimaryProduction|Algal_Primary_Production|algal.*primary.*prod|AlgalNPP|algal.*npp|understory.*npp|npp.*macroalgae",
    "(?i)AlgalBiomass|Algal_Biomass",
    "KelpAbundance|Kelp_Abundance|kelp.*abundance",
    "(?i)FishSize|Fish_Size",
    "wrack.*consumer|beach.*wrack.*consumer",
    "wrack.*consumer|beach.*wrack.*consumer",
    "wrack.*cover|wrack.*volume|wrack.*biomass|beach.*wrack|SBC_LTER_wrack",
    "wrack.*cover|wrack.*volume|wrack.*biomass|beach.*wrack|SBC_LTER_wrack",
    "wrack.*cover|wrack.*volume|wrack.*biomass|beach.*wrack|SBC_LTER_wrack",
    "InvertebrateLarvalSettlement|invertebrate.*settlement|larval.*settlement|urchin.*settlement",
    "[Ll]obster.*[Aa]bundance|[Ll]obster.*[Ss]ize.*[Aa]bundance|[Pp]anulirus",
    "[Ll]obster.*[Ss]ize|[Ll]obster.*[Ss]ize.*[Aa]bundance|[Pp]anulirus",
    "[Ll]obster.*[Tt]rap|[Tt]rap.*[Cc]ount|fishing.*pressure|trap_count",
    "\\bCHN\\b|kelp.*chn|macrocystis.*chn|blade.*carbon|blade.*nitrogen",
    "\\bCHN\\b|kelp.*chn|macrocystis.*chn|blade.*carbon|blade.*nitrogen",
    "monthly.*bottle|bottle.*data|SBC.*LTER.*bottle",
    "monthly.*bottle|bottle.*data|SBC.*LTER.*bottle",
    "monthly.*bottle|bottle.*data|SBC.*LTER.*bottle",
    "monthly.*bottle|bottle.*data|SBC.*LTER.*bottle",
    "monthly.*bottle|bottle.*data|SBC.*LTER.*bottle",
    "monthly.*bottle|bottle.*data|SBC.*LTER.*bottle",
    "salinity.*ph|ph.*salinity|SBC.*LTER.*salinity.*ph",
    "salinity.*ph|ph.*salinity|SBC.*LTER.*salinity.*ph",
    "salinity.*ph|ph.*salinity|SBC.*LTER.*salinity.*ph",
    "salinity.*ph|ph.*salinity|SBC.*LTER.*salinity.*ph",
    "salinity.*ph|ph.*salinity|SBC.*LTER.*salinity.*ph",
    "UCSB_SONGS.*Transect.*Fish|Transect.*FishSize|Transect.*FishAbundance",
    "UCSB_SONGS.*Transect.*Kelp|Transect.*KelpSize|Transect.*KelpAbundance",
    "UCSB_SONGS.*BenthicSubstrateCover|BenthicSubstrate.*Cover",
    "UCSB_SONGS.*FishBiomass|Fish_Biomass|FishBiomass",
    "UCSB_SONGS.*FishAbundace.*FishSize|FishAbundace.*FishBiomass",
    "UCSB_SONGS.*KelpAbundance|Kelp_Abundance|KelpAbundance",
    "UCSB_SONGS.*FishSpawning|FishSpawning.*Activity|fish.*spawn",
    "UCSB_SONGS.*FishAbundance.*FishRichness|FishRichness",
    "UCSB_SONGS.*AlgalPercent.*Invertebrate|AlgalPercentCover.*InvertebratePercentCover",
    "UCSB_SONGS.*BenthicSubstrate.*Algal.*Invertebrate|BenthicSubstrateCover.*AlgalAbundance",
    "UCSB_SONGS.*Transect.*AlgalAbundance.*InvertebrateAbundance",
    "CCIEA_representative_points",
    "[Zz]ooplankton.*[Bb]iomass|Zooplankton_Biomass",
    "dissolved.*organic.*carbon|\\bdoc\\b",
    "carbon.*nitrogen.*flux|carbon.*flux|nitrogen.*flux|sediment.*trap",
    "\\bphaeopigment\\b|\\bphaeo\\b",
    "[Mm]ussel.*[Hh]istopath|[Hh]istopath.*[Mm]ussel|NMW.*[Mm]ussel",
    "trace_elements_mussel_tissue|trace.*metal.*mussel|mussel.*trace.*metal",
    "trace_elements_sediment|trace.*metal.*sediment|sediment.*trace.*metal",
    "sewage_indicator|clostridium|clostridium.*perfringens",
    "SBC_MBON.*NWFSC.*FishSpeciesRichness|FishSpeciesRichness",
    "MLPA.*AlgalAbundance.*SeagrassAbundance.*BeachWrack.*SubstrateCover|AlgalAbundance.*SeagrassAbundance.*BeachWrack",
    "MLPA.*BeachMorphology.*SwashCharacteristics|BeachMorphology.*Swash",
    "MLPA.*SB.*KelpAbundance.*MarineMammal.*Seagrass.*Substrate.*BeachWrack",
    "MLPA.*SB.*KelpAbundance.*MarineMammal.*Seagrass.*Substrate.*BeachWrack",
    "MLPA.*SB.*KelpAbundance.*MarineMammal.*Seagrass.*Substrate.*BeachWrack",
    "MLPA.*SB.*KelpAbundance.*MarineMammal.*Seagrass.*Substrate.*BeachWrack",
    "MLPA.*SB.*SeabirdAbundance|MLPA.*SeabirdAbundance",
    "MLPA.*SB.*SeabirdAbundance|MLPA.*SeabirdAbundance",
    "KelpCanopyCover|kelp.*canopy.*cover|canopy.*cover.*kelp",
    "AlgalSize|algal.*size",
    "KelpDisease|kelp.*disease|kelp.*condition|kelp.*health",
    "KelpPopulationStructure|kelp.*population.*struct|kelp.*recruit",
    "CHIS.*biodiversity|CHIS.*invertebrate.*benthic.*fish|CHIS.*kelp",
    "CHIS.*biodiversity|CHIS.*invertebrate.*benthic.*fish|CHIS.*kelp",
    "CHIS.*biodiversity|CHIS.*invertebrate.*benthic.*fish|CHIS.*kelp",
    "CHIS.*biodiversity|CHIS.*invertebrate.*benthic.*fish|CHIS.*kelp",
    "CHIS.*biodiversity|CHIS.*invertebrate.*benthic.*fish|CHIS.*kelp",
    "MBARI.*nitrate|mbari.*nitrate|[Nn]itrate.*MBARI",
    "MBARI.*[Mm]odes.*[Ss]ea.*[Ss]urface|mbari.*modes.*sea.*surface|temp.*modes|modes.*sst",
    "MBARI.*[Ee][Dd][Nn][Aa]|mbari.*edna|\\bedna\\.csv",
    "[Zz]ooplankton[Bb]iomass|[Zz]ooplankton.*[Bb]iomass|mbari.*zooplankton",
    "NOAA.*eDNA|noaa.*edna|fish.*eDNA|eDNA.*chlorophyll"
    
    
    
    
  ),
  filename_parameter = c(
    "Temperature", "Salinity", "Dissolved Oxygen", "Chlorophyll-a",
    "Nutrients", "Dissolved Inorganic Carbon (DIC)", "Aragonite Saturation State",
    "Total Alkalinity", "pH", "fCO2", "Transmissivity", "Radiative Flux",
    "Primary Production (14C uptake)", "Fish Egg Counts", "Fish Larvae Counts",
    "Benthic Infauna Abundance", "Sediment Chemistry", "Sediment Toxicity",
    "Sediment Toxicity", "Trash and Debris", "Contaminant Bioaccumulation",
    "Harmful Algal Blooms", "Marine Mammal Abundance", "Seabird Abundance",
    "Whale Acoustic", "CDOM Fluorescence", "Phycoerythrin (PE) Fluorescence",
    "Microbial community composition", "Microbial Genomics (mRNA)",
    "Invertebrate Abundance", "Invertebrate Biomass", "Fish Biomass",
    "Fish Abundance and Distribution", "Biological Traits", "Water Quality",
    "Ecosystem Productivity", "Trophic Structure", 
    "Fish Abundance and Distribution", "Turbidity", "Sea Level", "Depth",
    "Bacterioplankton Abundance", "Methane",
    "Particulate Organic Carbon", "Particulate Organic Nitrogen",
    "Fish Population Structure", "Current Velocity", "Acoustic Backscatter",
    "Catch Abundance", "Catch Biomass", "Catch Size",
    "Catch Population Structure (Sex)", "Invertebrate Size", "Invertebrate Abundance",
    "Fish Larvae Counts", "Seabird Size", "Seabird Abundance",
    "Marine Mammal Species", "Marine Mammal Behavior",
    "Seabird Species", "Seabird Behavior",
    "Total coliforms", "Enterococcus", "Fecal coliforms", "Escherichia coli",
    "Krill (Euphausiid) Biomass", "Krill (Euphausiid) Size",
    "Sea Level", "Optical Backscatter", "Krill (Euphausiid) Biomass",
    "Optical Backscatter", "CDOM Fluorescence", "Current Velocity",
    "Current-Use Pesticides (Fish Tissue)", "Heavy Metals (Fish Tissue)",
    "Organochlorine Pesticides (Fish Tissue)", "POPs (Fish Tissue)",
    "Algal Percent Cover",
    "Invertebrate Percent Cover",
    "Algal Primary Production",
    "Algal Biomass",
    "Kelp Abundance",
    "Fish Size",
    "Invertebrate Abundance",
    "Invertebrate Biomass",
    "Kelp Wrack Percent Cover",
    "Kelp Wrack Volume",
    "Kelp Wrack Biomass",
    "Invertebrate Abundance",
    "Lobster Abundance",
    "Lobster Size",
    "Fishing Pressure",
    "Carbon (Kelp Tissue)",
    "Nitrogen (Kelp Tissue)",
    "Total Dissolved Nitrogen",
    "Total Dissolved Phosphorus",
    "Particulate Inorganic Carbon",
    "Particulate Inorganic Nitrogen",
    "Particulate Biogenic Silica",
    "Lithogenic Silica",
    "pH",
    "Salinity",
    "Total Alkalinity",
    "pCO2",
    "Aragonite Saturation State",
    "Fish Abundance and Distribution",
    "Kelp Abundance",
    "Benthic Substrate Cover",
    "Fish Biomass",
    "Fish Abundance and Distribution",
    "Kelp Abundance",
    "Fish Spawning Activity",
    "Fish Abundance and Distribution",
    "Algal Percent Cover",
    "Benthic Substrate Cover",
    "Algal Abundance",
    "Habitat Compression Index",
    "Zooplankton Biomass",
    "Dissolved Organic Carbon",
    "Particulate Carbon Flux",
    "Phaeopigments",
    "Mussel Condition (Histopathology)",
    "Trace Metals (Mussel Tissue)",
    "Trace Metals (Sediment)",
    "Sewage Indicator",
    "Fish Species Richness",
    "Algal Abundance",
    "Beach Morphology",
    "Kelp Abundance",
    "Seagrass Abundance",
    "Benthic Substrate Cover",
    "Beach Wrack",
    "Seabird Abundance",
    "Seabird Species",
    "Kelp Canopy Cover",
    "Algal Size",
    "Kelp Disease/Condition",
    "Kelp Population Structure", "Species Richness",
    "Benthic Infauna Abundance",
    "Sediment Cover",
    "Invertebrate Species Richness",
    "Kelp Canopy Cover", 
    "Nitrate",
    "Temperature",
    "Environmental DNA (eDNA)",
    "Zooplankton Biomass",
    "Environmental DNA (eDNA)"
    
    
  ),
  eov_group = c(
    "Physical",        "Physical",        "Biogeochemical",  "Biogeochemical",
    "Biogeochemical",  "Biogeochemical",  "Biogeochemical",  "Biogeochemical",
    "Biogeochemical",  "Biogeochemical",  "Physical",        "Physical",
    "Biogeochemical",  "Biological",      "Biological",      "Biological",
    "Biogeochemical",  "Biogeochemical",  "Biogeochemical",  "Biogeochemical",
    "Biogeochemical",  "Biological",      "Biological",      "Biological",
    "Biological",      "Biogeochemical",  "Biogeochemical",  "Biological",
    "Biological",      "Biological",      "Biological",      "Biological",
    "Biological",      "Biological",      "Physical",        "Biological",
    "Biological",      "Biological",      "Physical",        "Physical",
    "Physical",        "Biological",      "Biogeochemical",  "Biogeochemical",
    "Biogeochemical",  "Biological",      "Physical",        "Physical",
    "Biological",      "Biological",      "Biological",      "Biological",
    "Biological",      "Biological",      "Biological",      "Biological",
    "Biological",      "Biological",      "Biological",      "Biological",
    "Biological",      "Biological",      "Biological",      "Biological",
    "Biological",      "Biological",      "Physical",        "Physical",
    "Biological",      "Physical",        "Biogeochemical",  "Physical",
    "Biogeochemical",  "Biogeochemical",  "Biogeochemical",  "Biogeochemical",
    "Biological",      "Biological",      "Biological",      "Biological",
    "Biological",      "Biological",      "Biological",      "Biological",
    "Biological",      "Biological",      "Biological",      "Biological",
    "Biological",      "Biological",      "Biological",      "Biological",
    "Biogeochemical",  "Biogeochemical",  "Biogeochemical",  "Biogeochemical",
    "Biogeochemical",  "Biogeochemical",  "Biogeochemical",  "Physical",
    "Biogeochemical",  "Biogeochemical",  "Biogeochemical",  "Biological",
    "Biological",      "Biological",      "Biological",      "Biological",
    "Biological",      "Biological",      "Biological",      "Biological",
    "Biological",      "Biological",      "Physical",        "Biological",
    "Biogeochemical",  "Biogeochemical",  "Biogeochemical",  "Biological",
    "Biogeochemical",  "Biogeochemical",  "Biological",      "Biological",
    "Biological",      "Physical",        "Biological",      "Biological",
    "Biological",      "Biological",      "Biological",      "Biological",
    "Biological",      "Biological",      "Biological",      "Biological",
    "Biological",      "Biological",      "Biological",      "Biogeochemical",
    "Physical",        "Biological",      "Biological",      "Biological"
  )
)

detect_parameter_from_filename <- function(file_name, dictionary_tbl) {
  matches <- dictionary_tbl %>%
    filter(str_detect(file_name, regex(pattern, ignore_case = TRUE)))
  if (nrow(matches) == 0) return(tibble(filename_parameter = NA_character_))
  matches %>% distinct(filename_parameter, .keep_all = TRUE)
}

# =============================================================================
# FIND CSV FILES
# =============================================================================

csv_files <- list.files(program_folder, pattern = "\\.csv$",
                        full.names = TRUE, recursive = TRUE) %>%
  .[!str_detect(., regex("(/|\\\\)(output[^/\\\\]*)(/|\\\\)", ignore_case = TRUE))]

has_csv <- length(csv_files) > 0

if (!has_csv && !has_spatial) stop("No CSV or spatial files found in program_folder.")
if (!file.exists(ca_boundary_path)) stop("Boundary shapefile not found. Check ca_boundary_path.")

cat("CSV files found:", length(csv_files), "\n")
if (has_csv) print(basename(csv_files))

# =============================================================================
# BLOCK 3: PROCESS SPATIAL FILES → POLYGON OVERLAY
# =============================================================================

polygon_output_path <- file.path(output_folder, paste0(display_name, "_polygons.geojson"))

if (has_spatial) {
  cat("\n=== Processing spatial files → polygon layer ===\n")
  
  polygon_sf_list <- map(spatial_files_overlay, function(f) {
    cat("  Reading:", basename(f), "\n")
    tryCatch({
      sf_obj <- st_read(f, quiet = TRUE)
      sf_obj <- st_make_valid(sf_obj)
      epsg <- tryCatch(st_crs(sf_obj)$epsg, error = function(e) NA)
      if (!is.na(st_crs(sf_obj)) && (is.na(epsg) || epsg != 4326))
        sf_obj <- st_transform(sf_obj, 4326)
      sf_obj <- st_make_valid(sf_obj)
      
      geom_types <- unique(as.character(st_geometry_type(sf_obj)))
      geom_types <- geom_types[!is.na(geom_types)]
      spatial_type <- case_when(
        length(geom_types) == 0                                                ~ "polygon",
        all(geom_types %in% c("POINT","MULTIPOINT"))                           ~ "point",
        all(geom_types %in% c("LINESTRING","MULTILINESTRING"))                 ~ "line",
        all(geom_types %in% c("POLYGON","MULTIPOLYGON"))                       ~ "polygon",
        TRUE                                                                    ~ "mixed"
      )
      
      label_candidates <- c("name","NAME","Name","label","LABEL","Label",
                            "site","SITE","area","AREA","zone","ZONE",
                            "region","REGION","title","TITLE","id","ID")
      label_col <- intersect(label_candidates, names(sf_obj))[1]
      if (is.na(label_col)) label_col <- names(sf_obj)[1]
      
      sf_obj %>%
        mutate(
          `Program Name`      = program_name,
          `Full Program Name` = program_full_name,
          `Source File`       = basename(f),
          `Spatial Type`      = spatial_type,
          `Label Field`       = label_col,
          `Tooltip Label`     = if (label_col %in% names(sf_obj))
            as.character(.data[[label_col]])
          else NA_character_
        )
    }, error = function(e) {
      warning("Could not read: ", basename(f), " — ", conditionMessage(e))
      NULL
    })
  }) %>% compact()
  
  if (length(polygon_sf_list) > 0) {
    polygon_combined_sf <- bind_rows(polygon_sf_list) %>% st_make_valid()
    
    # Clip polygons to coastal buffer before writing
    ca_buffer_for_poly <- st_read(ca_boundary_path, quiet = TRUE) %>%
      st_transform(3310) %>% st_union() %>%
      st_buffer(dist = buffer_meters) %>%
      st_transform(4326) %>% st_make_valid()
    
    ca_land <- st_read(ca_boundary_path, quiet = TRUE) %>%
      st_transform(4326) %>% st_union() %>% st_make_valid()
    
    ca_ocean_strip <- tryCatch(
      st_difference(ca_buffer_for_poly, ca_land) %>% st_make_valid(),
      error = function(e) ca_buffer_for_poly
    )
    
    polygon_combined_sf <- tryCatch({
      polygon_combined_sf %>%
        st_transform(4326) %>% st_make_valid() %>%
        st_intersection(ca_ocean_strip) %>%
        st_make_valid() %>%
        st_simplify(preserveTopology = TRUE, dTolerance = 0.001)
    }, error = function(e) {
      warning("Ocean coastal clip failed — writing unclipped: ", e$message)
      polygon_combined_sf
    })
    
    suppressWarnings(st_write(polygon_combined_sf, polygon_output_path,
                              delete_dsn = TRUE, quiet = TRUE))
    cat("Polygon GeoJSON written:", polygon_output_path, "\n")
    cat("Features:", nrow(polygon_combined_sf),
        "| Types:", paste(unique(polygon_combined_sf$`Spatial Type`), collapse = ", "), "\n")
    
    polygon_combined_sf %>%
      st_drop_geometry() %>%
      group_by(`Source File`, `Spatial Type`) %>%
      summarise(feature_count = n(), .groups = "drop") %>%
      mutate(`Program Name` = program_name) %>%
      write_csv(file.path(output_folder, paste0(program_name, "_polygon_summary.csv")))
    cat("Polygon summary CSV written.\n")
  } else {
    cat("WARNING: All spatial files failed to read — polygon layer not written.\n")
    has_spatial <- FALSE
  }
}

if (!has_csv) {
  cat("\nNo CSV files — hex workflow skipped.\n")
  cat("\n===== WORKFLOW COMPLETE (spatial only) =====\n")
  cat("Program:        ", program_name, "\n")
  cat("Polygon GeoJSON:", polygon_output_path, "\n")
  cat("============================================\n")
  stop("Stopping after spatial-only processing.", call. = FALSE)
}


# If polygon/line shapefiles were processed, build a JS snippet that embeds
# them into the standalone HTML map as a styled Leaflet overlay layer.
# If no spatial files exist, this stays empty and nothing is added to the map.
polygon_js_standalone <- ""
if (has_spatial && file.exists(polygon_output_path)) {
  polygon_geojson_content <- readr::read_file(polygon_output_path)
  polygon_js_standalone <- paste0('
// Polygon overlay layer
var polygonData = ', polygon_geojson_content, ';
L.geoJSON(polygonData, {
  style: function(f) {
    var src = (f.properties["Source File"] || "").toLowerCase();
    var t = f.properties["Spatial Type"] || "polygon";
    if (src.indexOf("abalone") > -1)
      return { color: "#8B4513", weight: 1, fillColor: "#D2691E", fillOpacity: 0.3 };
    if (t === "line") return { color: "#8b0000", weight: 2, dashArray: "4 4" };
    return { color: "#8b0000", weight: 1.5, fillColor: "#c0392b", fillOpacity: 0.15 };
  },
  onEachFeature: function(feature, layer) {
    var lbl = feature.properties["Tooltip Label"] || feature.properties["Source File"] || "";
    if (lbl) layer.bindTooltip(lbl, { className: "polygon-label" });
  }
}).addTo(map);
')
}

# =============================================================================
# STEP 1-2: READ + CLEAN FILES
# =============================================================================

cat("\n=== STEP 1-2: Read and clean ===\n")

read_and_clean_file <- function(file, default_program) {
  message("Reading: ", basename(file))
  
  df_raw <- read_csv(file, show_col_types = FALSE,
                     col_types = cols(.default = col_character()))
  
  raw_names <- names(df_raw) %>%
    str_replace_all("\u00b5", "u") %>%
    str_replace_all("[^[:ascii:]]", "u") %>%
    str_replace_all("/", "_per_")
  names(df_raw) <- raw_names
  
  df <- df_raw %>%
    clean_names() %>% clean_character_cols()
  
  # Check if the first data row looks like an ERDDAP units row
  # (e.g. "degrees_north", "UTC") and strip it if so
  if (nrow(df) >= 1) {
    first_row <- as.character(unlist(df[1, ]))
    n_unit <- sum(str_detect(
      first_row[!is.na(first_row) & first_row != ""],
      regex("degrees_north|degrees_east|^UTC$|^m$|millibar|mS\\.cm|1e-3",
            ignore_case = TRUE)
    ))
    if (n_unit >= 2) {
      df <- df[-1, ]
      message("  Stripped ERDDAP units row from: ", basename(file))
    }
  }
  
  df <- df %>% mutate(source_row_id = row_number())
  
  lat_col <- validate_coord_col(df, detect_col_priority(df, lat_priority_patterns), lat_priority_patterns)
  lon_col <- validate_coord_col(df, detect_col_priority(df, lon_priority_patterns), lon_priority_patterns)
  
  coord_exclusions <- c(
    "relative_humidity","air_temperature","air_pressure",
    "wind_speed","wind_from_direction",
    "eastward_sea_water_velocity","northward_sea_water_velocity","z"
  )
  if (!is.na(lat_col) && lat_col %in% coord_exclusions) lat_col <- NA_character_
  if (!is.na(lon_col) && lon_col %in% coord_exclusions) lon_col <- NA_character_
  
  year_col    <- detect_col(df, year_patterns)
  date_col    <- detect_col(df, date_patterns)
  program_col <- detect_col(df, program_patterns)
  depth_col   <- detect_col(df, depth_patterns)
  
  if (!is.na(depth_col) && depth_col %in% names(df)) {
    # Parse ranges to midpoint before validation
    parsed_check <- suppressWarnings(sapply(as.character(df[[depth_col]]), function(v) {
      m <- regmatches(v, regexpr("-?[\\d.]+\\s*(?:to|-)\\s*-?[\\d.]+", v, perl = TRUE))
      if (length(m) > 0) {
        nums <- as.numeric(regmatches(m, gregexpr("-?[\\d.]+", m))[[1]])
        if (length(nums) == 2) return(abs(mean(nums)))
      }
      suppressWarnings(abs(as.numeric(v)))
    }, USE.NAMES = FALSE))
    valid_vals <- parsed_check[!is.na(parsed_check)]
    if (length(valid_vals) == 0 || median(valid_vals) > 12000)
      depth_col <- NA_character_
  }
  
  coord_role <- case_when(
    !is.na(lat_col) & str_detect(lat_col, regex("mid",   ignore_case = TRUE)) ~ "mid",
    !is.na(lat_col) & str_detect(lat_col, regex("start", ignore_case = TRUE)) ~ "start",
    !is.na(lat_col) & str_detect(lat_col, regex("stop",  ignore_case = TRUE)) ~ "stop",
    TRUE ~ "standard"
  )
  
  df %>%
    mutate(
      source_file          = basename(file),
      source_path          = file,
      file_stub            = get_file_stub(file),
      program              = if (!is.na(program_col) && program_col %in% names(df))
        as.character(.data[[program_col]])
      else rep(as.character(default_program), nrow(df)),
      program              = if_else(is.na(program) | str_trim(program) == "",
                                     as.character(default_program), as.character(program)),
      detected_lat_col     = lat_col,
      detected_lon_col     = lon_col,
      detected_year_col    = year_col,
      detected_date_col    = date_col,
      detected_program_col = program_col,
      detected_depth_col   = depth_col,
      detected_coord_role  = coord_role,
      year_detected        = extract_year(df, year_col, date_col),
      latitude_std         = if (!is.na(lat_col) && lat_col %in% names(df))
        parse_coord_vector(df[[lat_col]], FALSE, basename(file), lat_col)
      else rep(NA_real_, nrow(df)),
      longitude_std        = if (!is.na(lon_col) && lon_col %in% names(df))
        parse_coord_vector(df[[lon_col]], TRUE, basename(file), lon_col)
      else rep(NA_real_, nrow(df)),
      depth_std            = if (!is.na(depth_col) && depth_col %in% names(df)) {
        raw_vals <- df[[depth_col]]
        parsed <- suppressWarnings(sapply(as.character(raw_vals), function(v) {
          v <- str_trim(v)
          m <- regmatches(v, regexpr("-?[\\d.]+\\s*(?:to|-)\\s*-?[\\d.]+", v, perl = TRUE))
          if (length(m) > 0) {
            nums <- as.numeric(regmatches(m, gregexpr("-?[\\d.]+", m))[[1]])
            
            if (length(nums) == 2) return(abs(mean(nums)))
          }
          suppressWarnings(abs(as.numeric(v)))
        }, USE.NAMES = FALSE))
        if_else(parsed >= 0 & parsed <= 12000, parsed, NA_real_)
      } else rep(NA_real_, nrow(df)),
      sample_point_key     = paste0(get_file_stub(file), "_ROW_", source_row_id)
    )
  
}

all_raw_df <- map_dfr(csv_files, read_and_clean_file, default_program = program_name)
cat("Total rows read:", nrow(all_raw_df), "\n")

detection_summary <- all_raw_df %>%
  distinct(source_file, source_path, detected_lat_col, detected_lon_col,
           detected_year_col, detected_date_col, detected_program_col,
           detected_depth_col, detected_coord_role)

# =============================================================================
# STEP 3: YEAR FILTER
# =============================================================================

cat("\n=== STEP 3: Year filter (>= ", start_year, ") ===\n", sep = "")

all_filtered_df <- all_raw_df %>%
  filter(is.na(year_detected) | year_detected >= start_year)

all_filtered_coords_df_raw <- all_filtered_df %>%
  filter(!is.na(latitude_std), !is.na(longitude_std))

cat("Rows after year filter:", nrow(all_filtered_df),
    "| rows with coords:", nrow(all_filtered_coords_df_raw), "\n")

# =============================================================================
# STEP 4: COASTAL CLIP + WEA SEPARATION
#
# Main pipeline clips to COASTAL BUFFER ONLY.
# WEA-only points (outside coastal buffer) are tracked separately
# and written to _wea_Xkm.geojson at the end of each resolution
# iteration — they never go into the master GeoJSON.
# =============================================================================

cat("\n=== STEP 4: Coastal clip + WEA separation ===\n")

ca_boundary_proj  <- st_read(ca_boundary_path, quiet = TRUE) %>%
  st_transform(3310) %>% st_union()
ca_coastal_buffer <- st_buffer(ca_boundary_proj, dist = buffer_meters)

# Load WEA for separation only — NOT merged into the coastal clip
wea_sf <- NULL
if (apply_wea_clip && file.exists(wea_shapefile_path)) {
  wea_sf <- st_read(wea_shapefile_path, quiet = TRUE) %>%
    st_transform(3310) %>%
    st_union()
  cat("WEA shapefile loaded.\n")
}

if (nrow(all_filtered_coords_df_raw) == 0) {
  all_filtered_coords_df <- all_filtered_coords_df_raw
  wea_only_coords_df_raw <- all_filtered_coords_df_raw[0, ]
  clip_summary <- tibble(clip_applied = apply_coastal_clip,
                         rows_before_clip = 0L, rows_after_clip = 0L,
                         rows_removed_by_clip = 0L, rows_wea_only = 0L)
} else {
  coords_sf <- all_filtered_coords_df_raw %>%
    st_as_sf(coords = c("longitude_std","latitude_std"), crs = 4326, remove = FALSE) %>%
    st_transform(3310)
  
  if (apply_coastal_clip) {
    keep_coastal <- st_intersects(coords_sf, ca_coastal_buffer, sparse = FALSE)[, 1]
    all_filtered_coords_df <- st_drop_geometry(coords_sf[keep_coastal, ])
    
    if (!is.null(wea_sf)) {
      keep_wea      <- st_intersects(coords_sf, wea_sf, sparse = FALSE)[, 1]
      wea_only_mask <- keep_wea & !keep_coastal
      wea_only_coords_df_raw <- st_drop_geometry(coords_sf[wea_only_mask, ])
      cat("WEA-only points (outside coastal buffer):", nrow(wea_only_coords_df_raw), "\n")
    } else {
      wea_only_coords_df_raw <- all_filtered_coords_df_raw[0, ]
    }
  } else {
    all_filtered_coords_df <- st_drop_geometry(coords_sf)
    wea_only_coords_df_raw <- all_filtered_coords_df_raw[0, ]
  }
  
  clip_summary <- tibble(
    clip_applied         = apply_coastal_clip,
    rows_before_clip     = nrow(all_filtered_coords_df_raw),
    rows_after_clip      = nrow(all_filtered_coords_df),
    rows_removed_by_clip = nrow(all_filtered_coords_df_raw) - nrow(all_filtered_coords_df),
    rows_wea_only        = nrow(wea_only_coords_df_raw)
  )
  cat("Rows after coastal clip (main):", nrow(all_filtered_coords_df), "\n")
}

if (apply_coastal_clip && nrow(all_filtered_coords_df_raw) > 0) {
  files_before <- all_filtered_coords_df_raw %>% distinct(source_file) %>% pull()
  files_after  <- all_filtered_coords_df     %>% distinct(source_file) %>% pull()
  files_lost   <- setdiff(files_before, files_after)
  if (length(files_lost) > 0) {
    cat("\n⚠ FILES FULLY CLIPPED (all rows outside", buffer_miles, "mi coastal buffer):\n")
    walk(files_lost, ~ cat("  •", .x, "\n"))
    cat("\n  Parameters that would have been detected from clipped files:\n")
    walk(files_lost, function(f) {
      fn_hits <- detect_parameter_from_filename(f, filename_parameter_dictionary)
      if (nrow(fn_hits) > 0 && !all(is.na(fn_hits$filename_parameter))) {
        params <- fn_hits %>% filter(!is.na(filename_parameter)) %>% pull(filename_parameter)
        cat("  ", f, "→", paste(params, collapse = ", "), "\n")
      } else {
        cat("  ", f, "→ (no filename match)\n")
      }
    })
  } else {
    cat("All files retained after coastal clip.\n")
  }
}

# =============================================================================
# GEBCO BATHYMETRY — COASTAL DEPTH POINTS
# =============================================================================

cat("\n=== Extracting GEBCO bathymetry (coastal points) ===\n")

if (file.exists(gebco_raster_path) && nrow(all_filtered_coords_df) > 0) {
  gebco_raster <- terra::rast(gebco_raster_path)
  bathy_pts    <- all_filtered_coords_df %>%
    filter(!is.na(latitude_std), !is.na(longitude_std)) %>%
    select(sample_point_key, longitude_std, latitude_std) %>% distinct()
  pts_vect   <- terra::vect(bathy_pts, geom = c("longitude_std","latitude_std"), crs = "EPSG:4326")
  bathy_vals <- terra::extract(gebco_raster, pts_vect)
  bathy_pts$gebco_depth_m <- bathy_vals[, 2]
  all_filtered_coords_df  <- all_filtered_coords_df %>%
    left_join(bathy_pts %>% select(sample_point_key, gebco_depth_m), by = "sample_point_key")
  cat("Bathymetry extracted for", sum(!is.na(all_filtered_coords_df$gebco_depth_m)), "coastal points\n")
} else {
  if (!file.exists(gebco_raster_path)) cat("WARNING: GEBCO raster not found — depth will be NA\n")
  all_filtered_coords_df$gebco_depth_m <- NA_real_
}

# =============================================================================
# GEBCO BATHYMETRY — WEA-ONLY POINTS
# =============================================================================

if (nrow(wea_only_coords_df_raw) > 0 && file.exists(gebco_raster_path)) {
  cat("\n=== Extracting GEBCO bathymetry (WEA-only points) ===\n")
  if (!exists("gebco_raster")) gebco_raster <- terra::rast(gebco_raster_path)
  wea_bathy_pts  <- wea_only_coords_df_raw %>%
    select(sample_point_key, longitude_std, latitude_std) %>% distinct()
  wea_pts_vect   <- terra::vect(wea_bathy_pts, geom = c("longitude_std","latitude_std"), crs = "EPSG:4326")
  wea_bathy_vals <- terra::extract(gebco_raster, wea_pts_vect)
  wea_bathy_pts$gebco_depth_m <- wea_bathy_vals[, 2]
  wea_only_coords_df <- wea_only_coords_df_raw %>%
    left_join(wea_bathy_pts %>% select(sample_point_key, gebco_depth_m), by = "sample_point_key")
  cat("Bathymetry extracted for", sum(!is.na(wea_only_coords_df$gebco_depth_m)), "WEA-only points\n")
} else {
  wea_only_coords_df <- wea_only_coords_df_raw %>% mutate(gebco_depth_m = NA_real_)
}

write_csv(detection_summary, file.path(output_folder, paste0(program_name, "_detection_summary.csv")))
write_csv(clip_summary,      file.path(output_folder, paste0(program_name, "_clip_summary.csv")))
cat("Non-resolution diagnostics written.\n")

# =============================================================================
# PARAMETER DETECTION HELPERS
# =============================================================================

empty_param_schema <- tibble(
  source_file = character(), raw_parameter_name = character(),
  detected_context = character(), detected_measurement_type = character(),
  standard_parameter = character(), eov_group = character(), mapping_status = character()
)

WORKFLOW_COL_REGEX <- regex(
  paste(c("^detected_","^source_","^file_stub$","^sample_point_key$","^station_key$",
          "^hex_id$","^\\.row_id_temp$","^\\.join_key$","^geometry_type$",
          "^classification_reason$","^program$","^year_detected$",
          "^latitude_std$","^longitude_std$","^depth_std$","^activity_status$","^last_year$"),
        collapse = "|"), ignore_case = TRUE)

detect_measurement_parameter_columns <- function(df, source_file, dictionary, raw_df = NULL) {
  if (nrow(df) == 0) {
    return(empty_param_schema)
  }
  df_names <- names(df)[!str_detect(names(df), WORKFLOW_COL_REGEX)]
  cleaned  <- clean_text_value(df_names)
  map2_dfr(df_names, cleaned, function(orig, clean) {
    if (is_probably_metadata_column(clean)) return(tibble())
    has_vals <- count_real_values(df[[orig]]) > 0
    if (!has_vals && !is.null(raw_df)) {
      sf <- unique(df$source_file)[1]
      raw_slice <- raw_df %>% filter(source_file == sf)
      has_vals  <- orig %in% names(raw_slice) && count_real_values(raw_slice[[orig]]) > 0
    }
    if (!has_vals) {
      return(tibble())
    }
    hits <- dictionary %>%
      filter(detection_type == "measurement",
             str_detect(clean, regex(raw_pattern, ignore_case = TRUE)) |
               str_detect(orig,  regex(raw_pattern, ignore_case = TRUE)))
    if (nrow(hits) == 0) return(tibble())
    hits %>% transmute(source_file = source_file, raw_parameter_name = orig,
                       detected_context = "column_name", detected_measurement_type = "measurement_column",
                       standard_parameter, eov_group, mapping_status = "matched_from_column")
  }) %>% distinct()
}

detect_observation_parameters_from_file_and_columns <- function(df, source_file, rules_table) {
  if (nrow(df) == 0) {
    return(empty_param_schema)
  }
  file_clean <- clean_text_value(source_file)
  candidate_cols <- names(df)[!str_detect(names(df), WORKFLOW_COL_REGEX)]
  candidate_cols <- candidate_cols[map_lgl(candidate_cols, ~ count_real_values(df[[.x]]) > 0)]
  col_clean <- clean_text_value(candidate_cols)
  pmap_dfr(rules_table, function(context_name, file_pattern, column_pattern, standard_parameter, eov_group) {
    if (str_detect(file_clean, regex(file_pattern, ignore_case = TRUE)) &&
        any(str_detect(col_clean, regex(column_pattern, ignore_case = TRUE)))) {
      tibble(source_file = source_file, raw_parameter_name = paste0("[context] ", context_name),
             detected_context = "file_and_column_context", detected_measurement_type = "observation_context",
             standard_parameter = standard_parameter, eov_group = eov_group,
             mapping_status = "matched_from_file_and_columns")
    } else tibble()
  }) %>% distinct()
}

detect_fish_specimen_observation_parameters <- function(df, source_file) {
  if (nrow(df) == 0) {
    return(empty_param_schema)
  }
  non_wf <- names(df)[!str_detect(names(df), WORKFLOW_COL_REGEX)]
  col_clean <- clean_text_value(non_wf[map_lgl(non_wf, ~ count_real_values(df[[.x]]) > 0)])
  has_taxon <- any(str_detect(col_clean, regex("scientific name|taxon|species", ignore_case = TRUE)))
  has_size  <- any(str_detect(col_clean, regex("length|size", ignore_case = TRUE)))
  has_mass  <- any(str_detect(col_clean, regex("weight|biomass", ignore_case = TRUE)))
  has_sex   <- any(str_detect(col_clean, regex("^sex$|gender", ignore_case = TRUE)))
  if (!has_taxon || !any(c(has_size, has_mass, has_sex))) return(empty_param_schema)
  file_clean <- clean_text_value(source_file)
  is_invert  <- str_detect(file_clean, "invertebrate|invert")
  is_fish    <- str_detect(file_clean, "fish|trawl|gbts|haul")
  is_benthic <- str_detect(file_clean, "benthic|infauna")
  if (!is_invert && !is_fish && !is_benthic) {
    return(empty_param_schema)
  }
  abund_label   <- if (is_invert) "Invertebrate Abundance" else if (is_benthic) "Benthic Infauna Abundance" else "Fish Abundance and Distribution"
  biomass_label <- if (is_invert) "Invertebrate Biomass"   else if (is_benthic) "Benthic Infauna Abundance" else "Fish Biomass"
  size_label    <- if (is_invert) "Invertebrate Size"      else if (is_benthic) "Benthic Infauna Abundance" else "Fish Size"
  out <- tibble(
    source_file = source_file,
    raw_parameter_name = c("[specimen] taxon","[specimen] size","[specimen] biomass","[specimen] sex"),
    detected_context = "specimen_context",
    detected_measurement_type = "observation_context",
    standard_parameter = c(abund_label, size_label, biomass_label, "Fish Population Structure"),
    eov_group = "Biological",
    mapping_status = "matched_from_specimen_context"
  )
  out[c(TRUE, has_size, has_mass, has_sex), , drop = FALSE] %>% distinct()
}

param_value_col_patterns <- regex(
  "parameter[_\\s]?group|parameter[_\\s]?name|parameter[_\\s]?type|\\banalyte\\b|\\bmeasurand\\b|\\bparameter$|\\bspecies_group\\b|\\bspecies\\b|\\btaxon\\b",
  ignore_case = TRUE
)

param_value_lookup <- tribble(
  ~value_pattern,                                                                                   ~standard_parameter,                              ~eov_group,
  "bioaccumulative contaminant.*sediment|sediment.*bioaccumulative",                               "Bioaccumulative Contaminants (Sediment)",         "Biogeochemical",
  "bioaccumulative contaminant.*mussel|mussel.*bioaccumulative|bioaccumulative contaminant.*tissue","Bioaccumulative Contaminants (Mussel Tissue)",   "Biogeochemical",
  "bioaccumulative contaminant|bioaccumulate",                                                      "Bioaccumulative Contaminants",                   "Biogeochemical",
  "current.?use pesticide.*sediment|sediment.*current.?use",                                        "Current-Use Pesticides (Sediment)",              "Biogeochemical",
  "current.?use pesticide.*mussel|mussel.*current.?use|current.?use pesticide.*tissue",            "Current-Use Pesticides (Mussel Tissue)",          "Biogeochemical",
  "current.?use pesticide|pesticide",                                                               "Current-Use Pesticides",                         "Biogeochemical",
  "sediment chemistry|sediment contaminant",                                                        "Sediment Chemistry",                             "Biogeochemical",
  "water quality",                                                                                  "Water Quality",                                  "Biogeochemical",
  "nutrient",                                                                                       "Nutrients",                                      "Biogeochemical",
  "\\btemperature\\b",                                                                              "Temperature",                                    "Physical",
  "\\bsalinity\\b",                                                                                 "Salinity",                                       "Physical",
  "dissolved oxygen|\\boxygen\\b",                                                                  "Dissolved Oxygen",                               "Biogeochemical",
  "chlorophyll|\\bchl\\b",                                                                          "Chlorophyll-a",                                  "Biogeochemical",
  "\\bph\\b",                                                                                       "pH",                                             "Biogeochemical",
  "nitrate|nitrite|ammonium|phosphate|silicate",                                                    "Nutrients",                                      "Biogeochemical",
  "\\bdic\\b|dissolved inorganic carbon",                                                           "Dissolved Inorganic Carbon (DIC)",               "Biogeochemical",
  "alkalinity",                                                                                     "Total Alkalinity",                               "Biogeochemical",
  "\\bpco2\\b",                                                                                     "pCO2",                                           "Biogeochemical",
  "zooplankton",                                                                                    "Zooplankton Abundance",                          "Biological",
  "invertebrate",                                                                                   "Invertebrate Abundance",                         "Biological",
  "fish|trawl",                                                                                     "Fish Abundance and Distribution",                "Biological",
  "marine mammal|cetacean|whale|dolphin",                                                           "Marine Mammal Abundance",                        "Biological",
  "seabird|sea bird",                                                                               "Seabird Count",                                  "Biological",
  "\\bkrill\\b|euphausii",                                                                          "Krill (Euphausiid) Abundance",                   "Biological",
  "\\brockfish\\b|\\bcottid\\b",                                                                    "Fish Abundance and Distribution",                "Biological",
  "\\bclupeoid\\b|\\bflatfish\\b|\\bsalmonid\\b|\\bmyctophid\\b|deep.sea smelt|\\bsmelt\\b|other groundfish|\\belasmobranch\\b", "Fish Abundance and Distribution", "Biological",
  "\\beuphausii\\b",                                                                                "Krill (Euphausiid) Abundance",                   "Biological",
  "\\bcephalopod\\b|\\bcrustacean\\b",                                                              "Invertebrate Abundance",                         "Biological",
  "gelatinous|hyperiid|jellyfish",                                                                  "Zooplankton Abundance",                          "Biological",
  "phytoplankton",                                                                                  "Phytoplankton Abundance",                        "Biological",
  "catch abundance|catch_abundance",                                                                "Catch Abundance",                                "Biological",
  "catch biomass|catch_biomass",                                                                    "Catch Biomass",                                  "Biological",
  "catch population structure|catch.*sex|catch size population",                                    "Catch Population Structure (Sex)",               "Biological",
  "catch size|catch_size",                                                                          "Catch Size",                                     "Biological",
  "e\\.?\\s?coli|escherichia.*coli",                                                                "Escherichia coli",                               "Biological",
  "\\benterococcus\\b|\\benterococci\\b",                                                           "Enterococcus",                                   "Biological",
  "total.*coliform|\\btotal_coliform\\b",                                                           "Total coliforms",                                "Biological",
  "fecal.*coliform|faecal.*coliform",                                                               "Fecal coliforms",                                "Biological",
  "\\bpah\\b|polycyclic.*aromatic|\\bpyrene\\b|\\bfluoranthene\\b|\\banthracene\\b|\\bbenzo.*pyrene\\b|\\bacenaphth", "PAHs (Fish Tissue)",           "Biogeochemical",
  "\\bpbde\\b|polybrominated|brominated.*diphenyl|flame.*retardant",                                "PBDEs (Fish Tissue)",                            "Biogeochemical",
  "\\bpop\\b|persistent.*organic|\\bdioxin\\b|\\bfuran\\b|\\bpcdd\\b|\\bpcdf\\b",                   "POPs (Fish Tissue)",                             "Biogeochemical",
  "\\bcsci\\b|california.*stream.*condition",                                                       "Benthic Condition Index (CSCI)",                 "Biological",
  "\\basci\\b|algal.*stream.*condition",                                                            "Algae Condition Index (ASCI)",                   "Biological",
  "\\bipi\\b|index.*physical.*habitat|physical.*habitat.*integrity",                                "Physical Habitat Integrity (IPI)",               "Physical",
  "heavy.*metal|\\blead\\b|\\bcadmium\\b|\\bmercury\\b|\\barsenic\\b|\\bcopper\\b|\\bzinc\\b|\\bselenium\\b", "Heavy Metals (Fish Tissue)",           "Biogeochemical",
  "organochlorine|\\bDDT\\b|\\bDDE\\b|\\bDDD\\b|\\bchlordan|\\baldrin\\b|\\bdieldrin\\b|\\bendrin\\b|\\bheptachlor\\b|\\blindane\\b", "Organochlorine Pesticides (Fish Tissue)", "Biogeochemical",
  "\\bpop\\b|persistent.*organic|\\bdioxin\\b|\\bfuran\\b|\\bpcdd\\b|\\bpcdf\\b|\\bpcb\\b|polychlorinated.*biphenyl", "POPs (Fish Tissue)",           "Biogeochemical",
  "current.?use pesticide|\\bbifenthrin\\b|\\bchlorpyrifos\\b|\\bcypermethrin\\b|\\batrazine\\b",   "Current-Use Pesticides (Fish Tissue)",           "Biogeochemical",
  "algal percent cover",                                                                            "Algal Percent Cover",                            "Biological",
  "invertebrate percent cover",                                                                     "Invertebrate Percent Cover",                     "Biological",
  "algal.*primary production|\\bnpp\\b",                                                            "Algal Primary Production",                       "Biological",
  "algal biomass|detritus biomass",                                                                 "Algal Biomass",                                  "Biological",
  "kelp abundance|macrocystis|\\bfrond\\b",                                                         "Kelp Abundance",                                 "Biological",
  "wrack.*cover",                                                                                   "Kelp Wrack Percent Cover",                       "Biological",
  "wrack.*volume",                                                                                  "Kelp Wrack Volume",                              "Biological",
  "wrack.*biomass",                                                                                 "Kelp Wrack Biomass",                             "Biological",
  "lobster.*abundance|panulirus.*count|spiny.*lobster",                                             "Lobster Abundance",                              "Biological",
  "lobster.*size|carapace.*length",                                                                 "Lobster Size",                                   "Biological",
  "fishing.*pressure|lobster.*trap|trap.*count",                                                    "Fishing Pressure",                               "Biological",
  "percent.*carbon|carbon.*kelp|npp.*carbon|fsc.*carbon",                                           "Carbon (Kelp Tissue)",                           "Biological",
  "percent.*nitrogen|nitrogen.*kelp|npp.*nitrogen",                                                 "Nitrogen (Kelp Tissue)",                         "Biological",
  "particulate inorganic carbon|\\bpic\\b",                                                         "Particulate Inorganic Carbon",                   "Biogeochemical",
  "particulate inorganic nitrogen|\\bpin\\b",                                                       "Particulate Inorganic Nitrogen",                 "Biogeochemical",
  "particulate biogenic silica|\\bpbsi\\b|\\bbsi\\b",                                               "Particulate Biogenic Silica",                    "Biogeochemical",
  "lithogenic silica|\\blsi\\b",                                                                    "Lithogenic Silica",                              "Biogeochemical",
  "total dissolved nitrogen|\\btdn\\b",                                                             "Total Dissolved Nitrogen",                       "Biogeochemical",
  "total dissolved phosphorus|\\btdp\\b",                                                           "Total Dissolved Phosphorus",                     "Biogeochemical",
  "bicarbonate|\\bhco3\\b",                                                                         "Bicarbonate Ion Concentration",                  "Biogeochemical",
  "calcite.*sat|omega.*calcite|\\bcalcite\\b",                                                      "Calcite Saturation State",                       "Biogeochemical",
  "carbon.?dioxide|carbon dioxide partial pressure",                                                "pCO2",                                           "Biogeochemical"
)

detect_parameters_from_value_column <- function(df, source_file) {
  if (nrow(df) == 0) {
    return(empty_param_schema)
  }
  param_val_cols <- names(df)[str_detect(names(df), param_value_col_patterns)]
  if (length(param_val_cols) == 0) {
    return(empty_param_schema)
  }
  map_dfr(param_val_cols, function(col) {
    unique_vals <- unique(na.omit(as.character(df[[col]])))
    unique_vals <- unique_vals[str_trim(unique_vals) != ""]
    if (length(unique_vals) == 0) return(tibble())
    map_dfr(unique_vals, function(val) {
      hit <- param_value_lookup %>%
        filter(str_detect(str_to_lower(val), regex(value_pattern, ignore_case = TRUE)))
      if (nrow(hit) == 0) return(tibble())
      hit %>% transmute(
        source_file = source_file,
        raw_parameter_name = paste0("[value_col:", col, "] ", val),
        detected_context = "value_column",
        detected_measurement_type = "value_column_inference",
        standard_parameter, eov_group,
        mapping_status = "matched_from_value_column"
      )
    })
  }) %>% distinct()
}

expand_file_level_parameters_to_rows <- function(df, file_level_hits) {
  if (nrow(df) == 0 || nrow(file_level_hits) == 0) return(tibble())
  src <- unique(df$source_file)[1]
  df %>% mutate(.row_id_temp = row_number(), .join_key = 1L) %>%
    left_join(file_level_hits %>%
                select(raw_parameter_name, detected_context, detected_measurement_type,
                       standard_parameter, eov_group, mapping_status) %>%
                distinct() %>% mutate(.join_key = 1L),
              by = ".join_key", relationship = "many-to-many") %>%
    select(-.join_key) %>%
    mutate(source_file = src)
}

enforce_param_schema <- function(df) {
  for (col in c("source_file","raw_parameter_name","detected_context",
                "detected_measurement_type","standard_parameter","eov_group","mapping_status"))
    if (!col %in% names(df)) df[[col]] <- NA_character_
  df
}

eov_fill <- filename_parameter_dictionary %>%
  select(standard_parameter = filename_parameter, eov_group_fill = eov_group) %>%
  distinct()

# =============================================================================
# CONSTANTS
# =============================================================================

HEX_RESOLUTIONS <- c("1km", "3km", "5km")

for (hex_resolution in HEX_RESOLUTIONS) {
  
  hex_cellsize_m <- switch(hex_resolution,
                           "1km" = 1000,
                           "3km" = 3000,
                           "5km" = 5000)
  
  cat("\n\n========================================\n")
  cat("RESOLUTION:", hex_resolution, "| cellsize:", hex_cellsize_m, "m\n")
  cat("========================================\n")
  
  #####################################
  # STEP 5: HEX GRID + ASSIGN HEX CELLS
  #####################################
  
  cat("\n=== STEP 5: Hex grid ===\n")
  
  if (nrow(all_filtered_coords_df) == 0) {
    hex_grid_sf        <- st_sf(hex_id = character(), geometry = st_sfc(crs = 3310))
    points_with_hex_df <- all_filtered_coords_df %>%
      mutate(hex_id = NA_character_, station_key = NA_character_)
  } else {
    points_sf <- all_filtered_coords_df %>%
      st_as_sf(coords = c("longitude_std","latitude_std"), crs = 4326, remove = FALSE) %>%
      st_transform(3310)
    
    grid_extent <- if (apply_coastal_clip) ca_coastal_buffer else
      st_buffer(st_as_sfc(st_bbox(points_sf)), dist = hex_cellsize_m * 2)
    
    hex_grid_sf <- st_make_grid(grid_extent, cellsize = hex_cellsize_m, square = FALSE) %>%
      st_sf() %>% mutate(hex_id = paste0("HEX_", row_number())) %>%
      st_set_crs(3310)
    
    idx     <- st_intersects(points_sf, hex_grid_sf, sparse = TRUE)
    hex_ids <- vapply(idx, function(i) if (length(i) > 0) hex_grid_sf$hex_id[i[1]] else NA_character_, character(1))
    # Any point that didn't land inside a hex gets assigned to the nearest one —
    # this catches edge points that fall just outside the grid boundary
    na_mask <- is.na(hex_ids)
    if (any(na_mask))
      hex_ids[na_mask] <- hex_grid_sf$hex_id[st_nearest_feature(points_sf[na_mask,], hex_grid_sf)]
    
    points_with_hex_df <- st_drop_geometry(points_sf) %>%
      mutate(hex_id      = hex_ids,
             station_key = if_else(!is.na(hex_ids), paste(program, hex_ids, sep=" | "), NA_character_))
    
    hex_grid_sf <- hex_grid_sf %>% filter(hex_id %in% unique(na.omit(hex_ids)))
    cat("Hex cells used:", nrow(hex_grid_sf), "\n")
  }
  
  if (nrow(points_with_hex_df) == 0) {
    warning("No rows remain after Step 5 for resolution ", hex_resolution, " — skipping.")
    next
  }
  
  #################################################################
  # STEP 6: CLASSIFY GEOMETRY TYPE
  #################################################################
  
  cat("\n=== STEP 6: Classify geometry type ===\n")
  
  # Classify each file as point, transect, or underway based on filename keywords —
  # this determines how the data gets displayed on the map
  dataset_classification <- points_with_hex_df %>%
    distinct(source_file, program) %>%
    mutate(
      src_lower     = str_to_lower(source_file),
      geometry_type = case_when(
        str_detect(src_lower, "underway|continuous|flow.?through|flowthrough") ~ "underway",
        str_detect(src_lower, "transect|line|cugn|glider|underway")            ~ "line",
        TRUE                                                                    ~ "point"
      ),
      classification_reason = case_when(
        geometry_type == "underway" ~ "Continuous underway sampling",
        geometry_type == "line"     ~ "Mobile transect/tow sampling",
        TRUE                        ~ "Fixed station"
      )
    ) %>%
    select(source_file, program, geometry_type, classification_reason)
  
  points_with_hex_df <- points_with_hex_df %>%
    left_join(dataset_classification, by = c("source_file","program"))
  
  ############################################
  # STEP 7: PARAMETER DETECTION
  ############################################
  
  cat("\n=== STEP 7: Parameter detection ===\n")
  
  split_file_list               <- split(points_with_hex_df, points_with_hex_df$source_file)
  row_level_parameter_hits_list <- list()
  filename_parameter_hits_list  <- list()
  ambiguous_filename_cases_list <- list()
  likely_parameter_columns      <- list()
  
  for (this_file in names(split_file_list)) {
    df_file <- split_file_list[[this_file]]
    cat("  Processing:", basename(this_file), "- rows:", nrow(df_file), "\n")
    
    # Four parameter detection passes per file, each using a different strategy:
    # column names, file+column context rules, specimen tables, and value columns
    meas_hits     <- detect_measurement_parameter_columns(df_file, this_file, parameter_dictionary, all_raw_df)
    obs_hits      <- detect_observation_parameters_from_file_and_columns(df_file, this_file, observation_context_rules)
    specimen_hits <- detect_fish_specimen_observation_parameters(df_file, this_file)
    val_col_hits  <- detect_parameters_from_value_column(df_file, this_file)
    
    fn_raw  <- detect_parameter_from_filename(basename(this_file), filename_parameter_dictionary)
    fn_hits <- if (nrow(fn_raw) > 0 && !all(is.na(fn_raw$filename_parameter))) {
      fn_raw %>%
        filter(!is.na(filename_parameter)) %>%
        transmute(
          source_file               = this_file,
          raw_parameter_name        = paste0("[filename] ", filename_parameter),
          detected_context          = "filename_only",
          detected_measurement_type = "filename_inference",
          standard_parameter        = filename_parameter,
          eov_group                 = if ("eov_group" %in% names(fn_raw)) eov_group else NA_character_,
          mapping_status            = "filename_only_candidate"
        )
    } else empty_param_schema
    
    if (nrow(meas_hits) > 0) likely_parameter_columns[[length(likely_parameter_columns)+1]] <- meas_hits
    if (nrow(fn_hits) > 0)   filename_parameter_hits_list[[length(filename_parameter_hits_list)+1]] <- fn_hits
    
    TRUSTED_FILENAME_PROMOTE <- c(
      "Sediment Chemistry",                        "Sediment Toxicity",                         "Survey Toxicity",
      "Trash and Debris",                          "Harmful Algal Blooms",                      "Biological Traits",
      "Ecosystem Productivity",                    "Trophic Structure",                         "Water Quality",
      "Microbial community composition",           "Bacterioplankton Abundance",                "Environmental DNA (eDNA)",
      "Methane",                                   "Nitrate",                                   "Oxygen Saturation",
      "CDOM Fluorescence",                         "Sea Level",                                 "Size-Fractionated Chlorophyll",
      "Phaeopigments",                             "Dissolved Organic Carbon",                  "Particulate Organic Carbon",
      "Particulate Organic Nitrogen",              "Particulate Carbon Flux",                   "Particulate Nitrogen Flux",
      "Particulate Inorganic Carbon",              "Particulate Inorganic Nitrogen",            "Particulate Biogenic Silica",
      "Lithogenic Silica",                         "Total Dissolved Nitrogen",                  "Total Dissolved Phosphorus",
      "Bicarbonate Ion Concentration",             "Calcite Saturation State",                  "Aragonite Saturation State",
      "Current Velocity",                          "Acoustic Backscatter",                      "Optical Backscatter",
      "Current-Use Pesticides (Fish Tissue)",      "Heavy Metals (Fish Tissue)",                "Organochlorine Pesticides (Fish Tissue)",
      "POPs (Fish Tissue)",                        "Bioaccumulative Contaminants (Mussel Tissue)", "Bioaccumulative Contaminants (Sediment)",
      "Sewage Indicator",                          "Mussel Condition (Histopathology)",         "Mussel Size",
      "Mussel Biomass",                            "Mussel Population Structure (Sex)",         "Fish Population Structure",
      "Fish Biomass",                              "Fish Abundance and Distribution",           "Fish Size",
      "Fish Larvae Counts",                        "Fish Egg Counts",                           "Fish Spawning Activity",
      "Fish Species Richness",                     "Phytoplankton Carbon Biomass",              "Picoplankton Carbon Biomass",
      "Bacterial Carbon Biomass",                  "Zooplankton Abundance",                     "Zooplankton Biomass",
      "Krill Biomass",                             "Invertebrate Abundance",                    "Invertebrate Biomass",
      "Invertebrate Size",                         "Invertebrate Species Richness",             "Invertebrate Percent Cover",
      "Algal Abundance",                           "Algal Biomass",                             "Algal Percent Cover",
      "Algal Primary Production",                  "Algal Species Richness",                    "Algal Size",
      "Kelp Abundance",                            "Kelp Size",                                 "Kelp Canopy Cover",
      "Kelp Wrack Percent Cover",                  "Kelp Wrack Volume",                         "Kelp Wrack Biomass",
      "Kelp Disease/Condition",                    "Kelp Population Structure",                 "Seagrass Abundance",
      "Lobster Abundance",                         "Lobster Size",                              "Fishing Pressure",
      "Carbon (Kelp Tissue)",                      "Nitrogen (Kelp Tissue)",                    "Benthic Substrate Cover",
      "Benthic Infauna Abundance",                 "Benthic Cover",                             "Sediment Cover",
      "Species Richness",                          "Beach Morphology",                          "Swash Characteristics",
      "Beach Wrack",                               "Seabird Size",                              "Seabird Abundance",
      "Seabird Species",                           "Marine Mammal Species",                     "Marine Mammal Behavior",
      "Catch Abundance",                           "Catch Biomass",                             "Catch Size",
      "Catch Population Structure (Sex)"
    )
    
    trusted <- bind_rows(meas_hits, obs_hits, specimen_hits, val_col_hits) %>% distinct()
    
    # Filename-promoted parameters are ones where the filename alone is strong
    # enough evidence to assign the parameter without needing a column match
    fn_promotable <- fn_hits %>% filter(standard_parameter %in% TRUSTED_FILENAME_PROMOTE)
    if (nrow(fn_promotable) > 0) {
      trusted <- bind_rows(trusted, fn_promotable %>%
                             mutate(mapping_status = "filename_promoted_to_trusted")) %>% distinct()
      cat("    → Filename promoted:", paste(unique(fn_promotable$standard_parameter), collapse=", "), "\n")
    }
    
    if (nrow(trusted) == 0 && nrow(fn_hits) > 1) {
      ambiguous_filename_cases_list[[length(ambiguous_filename_cases_list)+1]] <-
        fn_hits %>% mutate(mapping_status = "ambiguous_filename_only_review")
    }
    
    if (nrow(trusted) > 0) {
      expanded <- expand_file_level_parameters_to_rows(df_file, trusted)
      if (nrow(expanded) > 0)
        row_level_parameter_hits_list[[length(row_level_parameter_hits_list)+1]] <- expanded
    }
  }
  
  likely_parameter_columns <- bind_rows(c(list(empty_param_schema), likely_parameter_columns)) %>% distinct()
  row_level_parameter_hits <- bind_rows(c(list(empty_param_schema), row_level_parameter_hits_list)) %>% distinct()
  filename_parameter_hits  <- bind_rows(c(list(empty_param_schema), filename_parameter_hits_list)) %>% distinct()
  ambiguous_filename_cases <- bind_rows(c(list(empty_param_schema), ambiguous_filename_cases_list)) %>% distinct()
  
  cat("Parameter rows detected:", nrow(row_level_parameter_hits), "\n")
  
  ###################################
  # STEP 8: PARAMETER STANDARDIZATION
  ###################################
  
  cat("\n=== STEP 8: Standardize parameters ===\n")
  
  standardized_hits <- row_level_parameter_hits %>%
    mutate(across(c(standard_parameter, eov_group, mapping_status), as.character))
  
  standardized_hits_mapped <- standardized_hits %>%
    left_join(eov_fill, by = "standard_parameter") %>%
    mutate(eov_group = coalesce(eov_group, eov_group_fill)) %>%
    select(-eov_group_fill) %>%
    filter(!is.na(standard_parameter), !is.na(eov_group))
  
  parameter_review_table <- bind_rows(
    enforce_param_schema(standardized_hits) %>%
      select(source_file, raw_parameter_name, detected_context,
             detected_measurement_type, standard_parameter, eov_group, mapping_status) %>%
      distinct() %>% mutate(review_type = "assigned"),
    enforce_param_schema(ambiguous_filename_cases) %>%
      select(source_file, raw_parameter_name, detected_context,
             detected_measurement_type, standard_parameter, eov_group, mapping_status) %>%
      distinct() %>% mutate(review_type = "needs_review"),
    enforce_param_schema(filename_parameter_hits) %>%
      filter(!source_file %in% standardized_hits$source_file) %>%
      select(source_file, raw_parameter_name, detected_context,
             detected_measurement_type, standard_parameter, eov_group, mapping_status) %>%
      distinct() %>% mutate(review_type = "filename_only_candidate")
  ) %>% distinct()
  
  if (nrow(standardized_hits_mapped) == 0) {
    warning("No parameters detected for resolution ", hex_resolution, " — skipping this resolution.")
    next
  }
  
  cat("Distinct parameters:\n")
  print(standardized_hits_mapped %>% distinct(standard_parameter, eov_group) %>% arrange(standard_parameter))
  
  ######################################
  # STEP 8B: PRELIMINARY ACTIVITY STATUS
  ######################################
  
  latest_year_by_file <- points_with_hex_df %>%
    filter(!is.na(year_detected)) %>%
    group_by(source_file) %>%
    summarise(last_year = max(year_detected, na.rm = TRUE), .groups = "drop")
  
  standardized_hits_prelim_active <- standardized_hits_mapped %>%
    left_join(latest_year_by_file, by = "source_file") %>%
    mutate(activity_status = case_when(
      !is.na(last_year) & last_year >= active_cutoff_year ~ "Active",
      !is.na(last_year) & last_year < active_cutoff_year  ~ "Inactive",
      TRUE ~ "Unknown"
    ))
  
  ##################################
  # STEP 8C: PER-PARAMETER FREQUENCY
  ##################################
  
  cat("\n=== STEP 8C: Per-parameter frequency ===\n")
  
  parameter_frequency_table <- compute_parameter_frequency(
    param_hits_df      = standardized_hits_prelim_active,
    raw_df             = all_raw_df,
    fallback_frequency = sampling_frequency
  ) %>%
    left_join(
      attr_param_lookup %>% filter(acronym == program_name) %>%
        select(standard_parameter, attr_frequency, attr_platform),
      by = "standard_parameter"
    ) %>%
    mutate(
      frequency_label = case_when(
        !is.na(attr_frequency) & attr_frequency != ""       ~ attr_frequency,
        !is.na(sampling_frequency) & sampling_frequency != "Unknown" ~ sampling_frequency,
        frequency_source %in% c("computed_from_dates","estimated_from_seasonal_labels","estimated_from_year_counts") ~ frequency_label,
        TRUE ~ frequency_label
      ),
      frequency_source = case_when(
        !is.na(attr_frequency) & attr_frequency != ""       ~ "attribute_table",
        !is.na(sampling_frequency) & sampling_frequency != "Unknown" ~ "program_level_attribute_table",
        TRUE ~ frequency_source
      )
    )
  
  ################################
  # STEP 9: HEX PARAMETER PRESENCE
  ################################
  
  cat("\n=== STEP 9: Hex parameter presence ===\n")
  
  program_hex_parameter_table <- standardized_hits_mapped %>%
    group_by(program, hex_id, station_key, standard_parameter, eov_group) %>%
    summarise(
      parameter_present    = 1L,
      first_year           = if (all(is.na(year_detected))) NA_integer_ else min(year_detected, na.rm = TRUE),
      last_year            = if (all(is.na(year_detected))) NA_integer_ else max(year_detected, na.rm = TRUE),
      min_depth            = if (all(is.na(depth_std))) NA_real_ else min(depth_std, na.rm = TRUE),
      max_depth            = if (all(is.na(depth_std))) NA_real_ else max(depth_std, na.rm = TRUE),
      geometry_types_found = collapse_unique_text(geometry_type),
      source_files         = collapse_unique_text(source_file),
      .groups = "drop"
    ) %>%
    mutate(program_hex_key = paste(program, hex_id, sep = " | "))
  
  ############################################
  # STEP 9B: ACTIVITY STATUS FILTER
  ############################################
  
  # Hex cells are marked inactive if the most recent data year is before
  # active_cutoff_year — those hexes are filtered out before export
  active_program_hex <- standardized_hits_mapped %>%
    group_by(program, hex_id) %>%
    summarise(
      last_year = if (all(is.na(year_detected))) NA_integer_ else max(year_detected, na.rm = TRUE),
      activity_status = case_when(
        !is.na(last_year) & last_year >= active_cutoff_year ~ "active_since_2015",
        !is.na(last_year) & last_year < active_cutoff_year  ~ "inactive_pre_2015",
        TRUE ~ "unknown_year"
      ),
      .groups = "drop"
    )
  
  program_hex_parameter_table_active <- program_hex_parameter_table %>%
    left_join(active_program_hex, by = c("program","hex_id")) %>%
    filter(activity_status != "inactive_pre_2015") %>%
    left_join(
      parameter_frequency_table %>%
        select(standard_parameter, frequency_label, frequency_source) %>%
        distinct(standard_parameter, .keep_all = TRUE),
      by = "standard_parameter"
    )
  
  standardized_hits_active <- standardized_hits_mapped %>%
    left_join(active_program_hex, by = c("program","hex_id")) %>%
    filter(activity_status != "inactive_pre_2015")
  
  cat("Active hex rows:", nrow(program_hex_parameter_table_active), "\n")
  
  ############################################
  # STEP 9C: PER-HEX DEPTH RANGES BY PARAMETER
  ############################################
  
  hex_parameter_depths <- program_hex_parameter_table_active %>%
    group_by(hex_id, standard_parameter) %>%
    summarise(
      param_min = if (all(is.na(min_depth))) NA_real_ else min(min_depth, na.rm = TRUE),
      param_max = if (all(is.na(max_depth))) NA_real_ else max(max_depth, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    filter(!is.na(param_min) | !is.na(param_max)) %>%
    mutate(depth_label = case_when(
      !is.na(param_min) & !is.na(param_max) & round(param_min) != round(param_max) ~
        paste0(standard_parameter, ": ", round(param_min), "\u2013", round(param_max), " m"),
      !is.na(param_min) ~ paste0(standard_parameter, ": ", round(param_min), " m"),
      TRUE ~ NA_character_
    )) %>%
    filter(!is.na(depth_label)) %>%
    group_by(hex_id) %>%
    summarise(parameter_depths = collapse_unique_text(depth_label), .groups = "drop")
  
  ############################################
  # STEP 10: SUMMARIES + WIDE PARAMETER FLAGS
  ############################################
  
  cat("\n=== STEP 10: Summaries ===\n")
  
  # Creates one yes/no column per parameter so the map can show or hide hexes based
  # on which parameter toggle the user clicks.
  program_hex_parameter_wide <- program_hex_parameter_table_active %>%
    select(program, hex_id, standard_parameter, parameter_present) %>%
    distinct() %>%
    mutate(yes_no_field = paste0("param_", str_replace_all(str_to_lower(standard_parameter), "[^a-z0-9]+", "_"))) %>%
    select(-standard_parameter) %>%
    pivot_wider(names_from = yes_no_field, values_from = parameter_present, values_fill = 0)
  
  program_hex_summary <- standardized_hits_active %>%
    group_by(program, hex_id, activity_status) %>%
    summarise(
      first_year           = if (all(is.na(year_detected))) NA_integer_ else min(year_detected, na.rm = TRUE),
      last_year            = if (all(is.na(year_detected))) NA_integer_ else max(year_detected, na.rm = TRUE),
      year_count           = if (all(is.na(year_detected))) NA_integer_ else
        as.integer(max(year_detected, na.rm = TRUE) - min(year_detected, na.rm = TRUE) + 1L),
      years_with_data      = n_distinct(year_detected, na.rm = TRUE),
      min_depth            = if (all(is.na(depth_std))) NA_real_ else min(depth_std, na.rm = TRUE),
      max_depth            = if (all(is.na(depth_std))) NA_real_ else max(depth_std, na.rm = TRUE),
      parameter_count      = n_distinct(standard_parameter),
      eov_group_count      = n_distinct(eov_group),
      parameters           = collapse_unique_text(standard_parameter),
      eov_groups           = collapse_unique_text(eov_group),
      geometry_types_found = collapse_unique_text(geometry_type),
      source_files         = collapse_unique_text(source_file),
      gebco_mean_depth     = if (all(is.na(gebco_depth_m))) NA_real_ else mean(gebco_depth_m, na.rm = TRUE),
      gebco_min_depth      = if (all(is.na(gebco_depth_m))) NA_real_ else min(gebco_depth_m,  na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(station_key     = paste(program, hex_id, sep = " | "),
           program_hex_key = paste(program, hex_id, sep = " | "))
  
  hex_centroids_ll <- st_centroid(st_geometry(hex_grid_sf)) %>%
    st_as_sf() %>% st_set_crs(st_crs(hex_grid_sf)) %>%
    mutate(hex_id = hex_grid_sf$hex_id) %>%
    st_transform(4326) %>%
    mutate(centroid_longitude = st_coordinates(.)[,1],
           centroid_latitude  = st_coordinates(.)[,2]) %>%
    st_drop_geometry() %>%
    select(hex_id, centroid_longitude, centroid_latitude)
  
  program_hex_inventory <- program_hex_summary %>%
    left_join(program_hex_parameter_wide, by = c("program","hex_id")) %>%
    left_join(hex_centroids_ll, by = "hex_id") %>%
    arrange(program, hex_id)
  
  hex_point_count <- standardized_hits_active %>%
    filter(!is.na(latitude_std), !is.na(longitude_std)) %>%
    group_by(hex_id) %>%
    summarise(sample_location_count = n_distinct(paste(round(latitude_std,5), round(longitude_std,5))),
              .groups = "drop")
  
  hex_inventory_for_map <- program_hex_inventory %>%
    group_by(hex_id) %>%
    summarise(
      programs             = collapse_unique_text(program),
      activity_status      = collapse_unique_text(activity_status),
      geometry_types_found = collapse_unique_text(geometry_types_found),
      first_year           = suppressWarnings(ifelse(all(is.na(first_year)), NA_integer_, min(first_year, na.rm = TRUE))),
      last_year            = suppressWarnings(ifelse(all(is.na(last_year)),  NA_integer_, max(last_year,  na.rm = TRUE))),
      year_count           = suppressWarnings(ifelse(all(is.na(year_count)), NA_integer_, max(year_count, na.rm = TRUE))),
      parameter_count      = max(parameter_count, na.rm = TRUE),
      eov_group_count      = max(eov_group_count, na.rm = TRUE),
      gebco_mean_depth     = suppressWarnings(ifelse(all(is.na(gebco_mean_depth)), NA_real_, mean(gebco_mean_depth, na.rm = TRUE))),
      parameters           = collapse_unique_text(parameters),
      eov_groups           = collapse_unique_text(eov_groups),
      source_files         = collapse_unique_text(source_files),
      centroid_longitude   = first(na.omit(centroid_longitude)),
      centroid_latitude    = first(na.omit(centroid_latitude)),
      min_depth            = suppressWarnings(ifelse(all(is.na(min_depth)), NA_real_, min(min_depth, na.rm = TRUE))),
      max_depth            = suppressWarnings(ifelse(all(is.na(max_depth)), NA_real_, max(max_depth, na.rm = TRUE))),
      across(starts_with("param_"), ~ {
        v <- max(.x, na.rm = TRUE)
        if (is.infinite(v)) 0L else v
      }),
      .groups = "drop"
    ) %>%
    left_join(hex_point_count,      by = "hex_id") %>%
    left_join(hex_parameter_depths, by = "hex_id")
  
  ############################################
  # STEP 11: OVERLAP TABLE + SAMPLED POINTS
  ############################################
  
  hex_overlap_table <- standardized_hits_active %>%
    group_by(hex_id) %>%
    summarise(
      program_count   = n_distinct(program),
      programs        = collapse_unique_text(program),
      parameter_count = n_distinct(standard_parameter),
      parameters      = collapse_unique_text(standard_parameter),
      eov_groups      = collapse_unique_text(eov_group),
      first_year      = if (all(is.na(year_detected))) NA_integer_ else min(year_detected, na.rm = TRUE),
      last_year       = if (all(is.na(year_detected))) NA_integer_ else max(year_detected, na.rm = TRUE),
      overlap_flag    = ifelse(n_distinct(program) > 1, 1L, 0L),
      .groups = "drop"
    ) %>%
    left_join(hex_centroids_ll, by = "hex_id")
  
  all_active_sampled_points <- standardized_hits_active %>%
    transmute(source_file, source_row_id, sample_point_key, station_key, program,
              year = year_detected, latitude = latitude_std, longitude = longitude_std,
              depth = depth_std, hex_id, geometry_type,
              raw_parameter_name, standard_parameter, eov_group,
              detected_context, detected_measurement_type) %>%
    arrange(program, hex_id, year, source_file)
  
  ############################################
  # TRANSECT EXPORT (first resolution only)
  ############################################
  
  if (hex_resolution == "1km") {
    cat("\n=== Transect export ===\n")
    
    mobile_sampling_points <- all_active_sampled_points %>% filter(geometry_type == "line")
    
    files_with_start_stop <- all_raw_df %>%
      distinct(source_file) %>%
      filter(purrr::map_lgl(source_file, function(sf) {
        nm <- names(all_raw_df %>% filter(source_file == sf) %>%
                      select(where(~ any(!is.na(.) & . != ""))))
        any(str_detect(nm, regex("lat.*(start|begin)|start.*lat", ignore_case = TRUE))) &&
          any(str_detect(nm, regex("lat.*(stop|end)|stop.*lat",   ignore_case = TRUE)))
      })) %>%
      pull(source_file)
    
    # Files with explicit start/stop columns get full transect geometry;
    # files without them get reconstructed transects grouped by cruise or year
    transect_source_rows <- bind_rows(
      mobile_sampling_points,
      standardized_hits_active %>%
        filter(source_file %in% files_with_start_stop,
               !source_file %in% mobile_sampling_points$source_file) %>%
        transmute(source_file, source_row_id, sample_point_key, station_key, program,
                  year = year_detected, latitude = latitude_std, longitude = longitude_std,
                  depth = depth_std, hex_id, geometry_type = "line",
                  raw_parameter_name, standard_parameter, eov_group,
                  detected_context, detected_measurement_type)
    ) %>% distinct(source_file, source_row_id, .keep_all = TRUE)
    
    if (nrow(transect_source_rows) > 0) {
      
      row_param_lookup <- standardized_hits_active %>%
        group_by(source_file, source_row_id) %>%
        summarise(Parameters  = collapse_unique_text(standard_parameter),
                  `EOV Groups`= collapse_unique_text(eov_group), .groups = "drop")
      
      expanded <- purrr::map_dfr(files_with_start_stop, function(sf) {
        raw_slice <- all_raw_df %>% filter(source_file == sf)
        nm        <- names(raw_slice)
        lat_s <- nm[str_detect(nm, regex("lat.*(start|begin)|start.*lat", ignore_case = TRUE))][1]
        lon_s <- nm[str_detect(nm, regex("lon.*(start|begin)|start.*lon", ignore_case = TRUE))][1]
        lat_m <- nm[str_detect(nm, regex("lat.*mid|mid.*lat",              ignore_case = TRUE))][1]
        lon_m <- nm[str_detect(nm, regex("lon.*mid|mid.*lon",              ignore_case = TRUE))][1]
        lat_e <- nm[str_detect(nm, regex("lat.*(stop|end)|stop.*lat",      ignore_case = TRUE))][1]
        lon_e <- nm[str_detect(nm, regex("lon.*(stop|end)|stop.*lon",      ignore_case = TRUE))][1]
        if (is.na(lat_s) || is.na(lat_e)) return(tibble())
        d_col   <- unique(na.omit(raw_slice$detected_date_col))[1]
        dep_col <- unique(na.omit(raw_slice$detected_depth_col))[1]
        svy_col <- nm[str_detect(nm, regex("^svy$|^survey$|^cruise$", ignore_case = TRUE))][1]
        raw_slice %>%
          left_join(row_param_lookup, by = c("source_file","source_row_id")) %>%
          transmute(
            Date              = if (!is.na(d_col)   && d_col   %in% nm) .data[[d_col]]   else NA_character_,
            `Latitude Start`  = suppressWarnings(as.numeric(.data[[lat_s]])),
            `Longitude Start` = suppressWarnings(as.numeric(.data[[lon_s]])),
            `Latitude Mid`    = if (!is.na(lat_m) && lat_m %in% nm) suppressWarnings(as.numeric(.data[[lat_m]])) else NA_real_,
            `Longitude Mid`   = if (!is.na(lon_m) && lon_m %in% nm) suppressWarnings(as.numeric(.data[[lon_m]])) else NA_real_,
            `Latitude Stop`   = suppressWarnings(as.numeric(.data[[lat_e]])),
            `Longitude Stop`  = suppressWarnings(as.numeric(.data[[lon_e]])),
            `Depth (m)`       = if (!is.na(dep_col) && dep_col %in% nm) suppressWarnings(as.numeric(.data[[dep_col]])) else NA_real_,
            SVY               = if (!is.na(svy_col) && svy_col %in% nm) .data[[svy_col]] else program_name,
            Parameters        = coalesce(Parameters, NA_character_),
            `EOV Groups`      = coalesce(`EOV Groups`, NA_character_),
            source_file, program = program_name
          ) %>%
          filter(!is.na(`Latitude Start`), !is.na(`Latitude Stop`)) %>%
          distinct(`Latitude Start`, `Longitude Start`, `Latitude Stop`, `Longitude Stop`, Date, .keep_all = TRUE)
      })
      
      if (nrow(expanded) > 0) {
        start_sf <- expanded %>%
          filter(!is.na(`Latitude Start`), !is.na(`Longitude Start`)) %>%
          st_as_sf(coords = c("Longitude Start", "Latitude Start"), crs = 4326, remove = FALSE) %>%
          st_transform(3310)
        keep_start <- st_intersects(start_sf, ca_coastal_buffer, sparse = FALSE)[, 1]
        has_stop_exp <- !is.na(expanded$`Latitude Stop`) & !is.na(expanded$`Longitude Stop`)
        stop_sf <- expanded[has_stop_exp, ] %>%
          st_as_sf(coords = c("Longitude Stop", "Latitude Stop"), crs = 4326, remove = FALSE) %>%
          st_transform(3310)
        keep_stop_exp  <- st_intersects(stop_sf, ca_coastal_buffer, sparse = FALSE)[, 1]
        keep_exp_final <- keep_start
        keep_exp_final[has_stop_exp] <- keep_start[has_stop_exp] & keep_stop_exp
        has_extent_exp <- !(round(expanded$`Latitude Start`, 5) == round(expanded$`Latitude Stop`, 5) &
                              round(expanded$`Longitude Start`, 5) == round(expanded$`Longitude Stop`, 5))
        has_extent_exp[is.na(has_extent_exp)] <- TRUE
        expanded <- expanded[keep_exp_final & has_extent_exp, ]
        cat("  Expanded transects after clip:", nrow(expanded), "\n")
      }
      
      single_pt_transects <- transect_source_rows %>%
        filter(!source_file %in% files_with_start_stop, !is.na(latitude), !is.na(longitude)) %>%
        left_join(row_param_lookup, by = c("source_file","source_row_id")) %>%
        transmute(
          Date = as.character(year), `Latitude Start` = latitude, `Longitude Start` = longitude,
          `Latitude Mid` = NA_real_, `Longitude Mid` = NA_real_,
          `Latitude Stop` = latitude, `Longitude Stop` = longitude,
          `Depth (m)` = depth, SVY = program_name,
          Parameters   = coalesce(Parameters, NA_character_),
          `EOV Groups` = coalesce(`EOV Groups`, NA_character_),
          source_file, program = program_name
        )
      
      mission_transect_files <- transect_source_rows %>%
        filter(!source_file %in% files_with_start_stop) %>%
        distinct(source_file) %>% pull(source_file)
      
      mission_transects <- purrr::map_dfr(mission_transect_files, function(sf) {
        raw_slice <- all_raw_df %>% filter(source_file == sf)
        nm        <- names(raw_slice)
        group_col <- nm[str_detect(nm, regex(
          "^id$|^buoy_id$|^drifter_id$|^mission$|^cruise$|^survey$|^deployment$|^mission_id$|^cruise_id$",
          ignore_case = TRUE))][1]
        use_year    <- is.na(group_col)
        d_col_inner <- unique(na.omit(raw_slice$detected_date_col))[1]
        params_for_file <- standardized_hits_active %>%
          filter(source_file == sf) %>%
          summarise(Parameters   = collapse_unique_text(standard_parameter),
                    `EOV Groups` = collapse_unique_text(eov_group))
        tryCatch({
          df_work <- raw_slice %>%
            filter(!is.na(latitude_std), !is.na(longitude_std),
                   !is.na(year_detected), year_detected >= 2000)
          if (nrow(df_work) == 0) return(tibble())
          df_work <- df_work %>%
            mutate(
              .parsed_date = parse_date_time_safe(as.character(
                if (!is.na(d_col_inner) && d_col_inner %in% names(df_work))
                  .data[[d_col_inner]]
                else as.character(year_detected)
              )),
              .group_key = if (!use_year) {
                as.character(.data[[group_col]])
              } else {
                # Split on gaps > 1 day to separate individual cruises
                gap_days <- c(0, as.numeric(diff(.parsed_date)))
                gap_days[is.na(gap_days)] <- 999
                cumsum(gap_days > 7)
              }
            )
          result <- df_work %>%
            group_by(.group_key) %>%
            summarise(
              `SamplingDate`      = as.character(first(year_detected)),
              `Latitude Start`  = first(latitude_std),
              `Longitude Start` = first(longitude_std),
              `Latitude Mid`    = NA_real_,
              `Longitude Mid`   = NA_real_,
              `Latitude Stop`   = last(latitude_std),
              `Longitude Stop`  = last(longitude_std),
              `Depth (m)`       = if (all(is.na(depth_std))) NA_real_ else mean(depth_std, na.rm = TRUE),
              .groups = "drop"
            ) %>%
            filter(`Latitude Start` != `Latitude Stop` | `Longitude Start` != `Longitude Stop`) %>%
            mutate(SVY = program_name, Parameters = params_for_file$Parameters,
                   `EOV Groups` = params_for_file$`EOV Groups`,
                   source_file = sf, program = program_name) %>%
            select(-.group_key)
          cat("  Mission transects from", basename(sf),
              "(grouped by", if (use_year) "year" else group_col, "):", nrow(result), "rows\n")
          result
        }, error = function(e) {
          cat("  ⚠ Mission transect failed for", basename(sf), "—", conditionMessage(e), "\n")
          tibble()
        })
      })
      
      if (nrow(mission_transects) > 0) {
        mt_sf <- mission_transects %>%
          filter(!is.na(`Latitude Start`), !is.na(`Longitude Start`)) %>%
          st_as_sf(coords = c("Longitude Start", "Latitude Start"), crs = 4326, remove = FALSE) %>%
          st_transform(3310)
        keep_mt <- st_intersects(mt_sf, ca_coastal_buffer, sparse = FALSE)[, 1]
        has_extent_mt <- !(round(mission_transects$`Latitude Start`, 5) == round(mission_transects$`Latitude Stop`, 5) &
                             round(mission_transects$`Longitude Start`, 5) == round(mission_transects$`Longitude Stop`, 5))
        has_extent_mt[is.na(has_extent_mt)] <- TRUE
        mission_transects <- mission_transects[keep_mt & has_extent_mt, ]
        cat("  Mission transects after coastal clip:", nrow(mission_transects), "\n")
      } else {
        cat("  No mission transects generated.\n")
      }
      
      all_transects <- bind_rows(
        if (nrow(expanded) > 0) {
          if ("Date" %in% names(expanded)) expanded %>% rename(SamplingDate = Date) else expanded
        } else tibble(),
        if (nrow(mission_transects) > 0) mission_transects else tibble()
      )
      
      if (nrow(all_transects) > 0) {
        all_transects <- all_transects %>%
          mutate(SamplingDate = as.character(SamplingDate)) %>%
          filter(is.na(SamplingDate) | suppressWarnings(as.integer(str_extract(SamplingDate, "\\d{4}"))) >= 2000) %>%
          distinct()
        write_csv(all_transects, file.path(output_folder, "transects.csv"))
        cat("transects.csv written:", nrow(all_transects), "rows\n")
      } else {
        cat("No transect rows passed filters — transects.csv not written.\n")
        
      }
    }
  }  
  
  ############################################
  # WRITE DIAGNOSTIC CSV OUTPUTS
  ############################################
  
  cat("\n=== Writing diagnostic CSVs ===\n")
  
  if (hex_resolution == "1km") {
    write_csv(dataset_classification,   file.path(output_folder, paste0(program_name, "_geometry_classification.csv")))
    write_csv(likely_parameter_columns, file.path(output_folder, paste0(program_name, "_parameter_columns.csv")))
    write_csv(filename_parameter_hits,  file.path(output_folder, paste0(program_name, "_filename_parameter_hits.csv")))
    write_csv(ambiguous_filename_cases, file.path(output_folder, paste0(program_name, "_ambiguous_filename_cases.csv")))
    write_csv(parameter_review_table,   file.path(output_folder, paste0(program_name, "_parameter_review.csv")))
    cat("Non-resolution CSVs written (1km pass).\n")
  }
  
  write_csv(parameter_frequency_table,
            file.path(output_folder, paste0(program_name, "_", hex_resolution, "_parameter_frequency.csv")))
  write_csv(program_hex_parameter_table_active,
            file.path(output_folder, paste0(program_name, "_", hex_resolution, "_hex_parameter_table.csv")))
  write_csv(program_hex_inventory,
            file.path(output_folder, paste0(program_name, "_", hex_resolution, "_hex_inventory.csv")))
  write_csv(hex_overlap_table,
            file.path(output_folder, paste0(program_name, "_", hex_resolution, "_hex_overlap.csv")))
  write_csv(all_active_sampled_points,
            file.path(output_folder, paste0(program_name, "_", hex_resolution, "_active_sampled_points.csv")))
  
  ############################################
  # GEOJSON MAP EXPORT
  ############################################
  
  cat("\n=== Writing GeoJSON ===\n")
  
  hex_map_sf <- hex_grid_sf %>%
    left_join(hex_inventory_for_map, by = "hex_id") %>%
    filter(!is.na(programs)) %>%
    st_transform(4326) %>% st_make_valid()
  
  hex_map_sf_export <- hex_map_sf %>%
    mutate(
      `Station Key`       = paste(programs, hex_id, sep = " | "),
      `Program Name`      = program_name,
      `Full Program Name` = program_full_name,
      `Geometry Types`    = case_when(
        str_detect(geometry_types_found, "underway") & str_detect(geometry_types_found, "point") ~ "Underway + Station",
        str_detect(geometry_types_found, "underway") & str_detect(geometry_types_found, "line")  ~ "Underway + Transect",
        str_detect(geometry_types_found, "underway") ~ "Continuous Underway",
        str_detect(geometry_types_found, "line")     ~ "Transect / Tow",
        TRUE                                          ~ "Fixed Station"
      ),
      `Frequency` = map_chr(parameters, function(param_str) {
        hex_params  <- str_split(param_str, "; ")[[1]]
        freq_labels <- parameter_frequency_table %>%
          filter(standard_parameter %in% hex_params) %>%
          pull(frequency_label) %>% unique() %>% sort()
        if (length(freq_labels) == 0) sampling_frequency else paste(freq_labels, collapse = "; ")
      }),
      `Platform` = map2_chr(parameters, source_files, function(param_str, src_str) {
        hex_params <- str_split(param_str, "; ")[[1]]
        attr_p     <- parameter_frequency_table %>%
          filter(standard_parameter %in% hex_params, !is.na(attr_platform), attr_platform != "") %>%
          pull(attr_platform) %>% unique() %>% sort()
        if (length(attr_p) > 0) paste(attr_p, collapse = "; ") else {
          hit <- platform_lookup %>%
            filter(str_detect(str_to_lower(src_str), regex(pattern, ignore_case = TRUE))) %>% slice(1)
          if (nrow(hit) == 0) program_platform else hit$platform
        }
      }),
      `Depth Range (m)` = case_when(
        !is.na(min_depth) & !is.na(max_depth) & min_depth != max_depth ~
          paste0(round(min_depth), "\u2013", round(max_depth), " m"),
        !is.na(min_depth) ~ paste0(round(min_depth), " m"),
        TRUE ~ NA_character_
      )
    ) %>%
    rename(
      `Monitoring Program` = programs,
      `Parameters`         = parameters,
      `EOV Groups`         = eov_groups,
      `Parameter Count`    = parameter_count,
      `First Year`         = first_year,
      `Last Year`          = last_year,
      `Years Sampled`      = year_count,
      `Source Files`       = source_files,
      `Centroid Latitude`  = centroid_latitude,
      `Centroid Longitude` = centroid_longitude,
      `Sample Locations`   = sample_location_count,
      `Gebco Mean Depth`   = gebco_mean_depth
    ) %>%
    select(
      `Program Name`, `Full Program Name`, `Monitoring Program`,
      `Parameters`, `EOV Groups`, `Parameter Count`,
      `First Year`, `Last Year`, `Years Sampled`,
      `Frequency`, `Platform`, `Geometry Types`,
      `Depth Range (m)`, `Sample Locations`,
      `Centroid Latitude`, `Centroid Longitude`,
      `Gebco Mean Depth`, `Source Files`,
      starts_with("param_"),
      geometry
    )
  
  geojson_out <- file.path(output_folder, paste0(display_name, "_", hex_resolution, ".geojson"))
  suppressWarnings(st_write(hex_map_sf_export, geojson_out, delete_dsn = TRUE, quiet = TRUE))
  cat("GeoJSON written:", geojson_out, "\n")
  
  ############################################
  # WEA-ONLY HEX EXPORT
  # Separate pipeline — outputs _wea_Xkm.geojson
  ############################################
  
  # Points that land outside the coastal buffer but inside the WEA footprint
  # get their own separate hex grid, outside the main master GeoJSON
  if (!is.null(wea_sf) && nrow(wea_only_coords_df) > 0) {
    cat("\n=== WEA-only hex export (", hex_resolution, ") ===\n", sep = "")
    
    wea_pts_sf_res <- wea_only_coords_df %>%
      st_as_sf(coords = c("longitude_std","latitude_std"), crs = 4326, remove = FALSE) %>%
      st_transform(3310)
    
    wea_extent_res <- st_buffer(wea_sf, dist = hex_cellsize_m)
    
    wea_hex_grid_res <- st_make_grid(wea_extent_res, cellsize = hex_cellsize_m, square = FALSE) %>%
      st_sf() %>% mutate(hex_id = paste0("WEA_HEX_", row_number())) %>%
      st_set_crs(3310)
    
    wea_idx     <- st_intersects(wea_pts_sf_res, wea_hex_grid_res, sparse = TRUE)
    wea_hex_ids <- vapply(wea_idx,
                          function(i) if (length(i) > 0) wea_hex_grid_res$hex_id[i[1]] else NA_character_,
                          character(1))
    wea_na_mask <- is.na(wea_hex_ids)
    if (any(wea_na_mask))
      wea_hex_ids[wea_na_mask] <- wea_hex_grid_res$hex_id[
        st_nearest_feature(wea_pts_sf_res[wea_na_mask, ], wea_hex_grid_res)]
    
    wea_pts_with_hex <- st_drop_geometry(wea_pts_sf_res) %>%
      mutate(hex_id        = wea_hex_ids,
             station_key   = paste(program_name, wea_hex_ids, sep = " | "),
             program       = program_name,
             geometry_type = "point")
    
    wea_hex_grid_res <- wea_hex_grid_res %>%
      filter(hex_id %in% unique(na.omit(wea_hex_ids)))
    cat("WEA hex cells:", nrow(wea_hex_grid_res), "\n")
    
    wea_param_hits_res <- if (exists("standardized_hits_mapped") && nrow(standardized_hits_mapped) > 0) {
      standardized_hits_mapped %>%
        filter(sample_point_key %in% wea_only_coords_df$sample_point_key) %>%
        left_join(wea_pts_with_hex %>% select(sample_point_key, hex_id, station_key),
                  by = "sample_point_key")
    } else tibble()
    
    if (nrow(wea_param_hits_res) == 0) {
      wea_all_params  <- if (exists("parameter_frequency_table"))
        paste(parameter_frequency_table$standard_parameter, collapse = "; ") else "Unknown"
      wea_all_eovs    <- if (exists("standardized_hits_mapped") && nrow(standardized_hits_mapped) > 0)
        paste(unique(standardized_hits_mapped$eov_group), collapse = "; ") else "Unknown"
      wea_param_count <- if (exists("parameter_frequency_table")) nrow(parameter_frequency_table) else 0L
      
      wea_hex_summary_res <- wea_pts_with_hex %>%
        group_by(hex_id) %>%
        summarise(
          programs              = program_name,
          parameters            = wea_all_params,
          eov_groups            = wea_all_eovs,
          parameter_count       = wea_param_count,
          first_year            = suppressWarnings(min(year_detected, na.rm = TRUE)),
          last_year             = suppressWarnings(max(year_detected, na.rm = TRUE)),
          year_count            = suppressWarnings(as.integer(max(year_detected, na.rm=TRUE) - min(year_detected, na.rm=TRUE) + 1L)),
          sample_location_count = n_distinct(paste(round(latitude_std,5), round(longitude_std,5))),
          gebco_mean_depth      = suppressWarnings(mean(gebco_depth_m, na.rm = TRUE)),
          source_files          = collapse_unique_text(source_file),
          .groups = "drop"
        )
    } else {
      wea_hex_summary_res <- wea_param_hits_res %>%
        group_by(hex_id) %>%
        summarise(
          programs        = program_name,
          parameters      = collapse_unique_text(standard_parameter),
          eov_groups      = collapse_unique_text(eov_group),
          parameter_count = n_distinct(standard_parameter),
          first_year      = suppressWarnings(if (all(is.na(year_detected))) NA_integer_ else min(year_detected, na.rm = TRUE)),
          last_year       = suppressWarnings(if (all(is.na(year_detected))) NA_integer_ else max(year_detected, na.rm = TRUE)),
          year_count      = suppressWarnings(as.integer(max(year_detected,na.rm=TRUE) - min(year_detected,na.rm=TRUE) + 1L)),
          .groups = "drop"
        ) %>%
        left_join(
          wea_pts_with_hex %>%
            group_by(hex_id) %>%
            summarise(
              sample_location_count = n_distinct(paste(round(latitude_std,5), round(longitude_std,5))),
              gebco_mean_depth      = suppressWarnings(mean(gebco_depth_m, na.rm = TRUE)),
              source_files          = collapse_unique_text(source_file),
              .groups = "drop"
            ),
          by = "hex_id"
        )
    }
    
    wea_hex_summary_res <- wea_hex_summary_res %>%
      mutate(
        first_year       = if_else(is.infinite(first_year), NA_integer_, as.integer(first_year)),
        last_year        = if_else(is.infinite(last_year),  NA_integer_, as.integer(last_year)),
        year_count       = if_else(is.infinite(year_count) | is.na(year_count), NA_integer_, as.integer(year_count)),
        gebco_mean_depth = if_else(is.nan(gebco_mean_depth), NA_real_, gebco_mean_depth)
      )
    
    wea_centroids_res <- st_centroid(st_geometry(wea_hex_grid_res)) %>%
      st_as_sf() %>% st_set_crs(st_crs(wea_hex_grid_res)) %>%
      mutate(hex_id = wea_hex_grid_res$hex_id) %>%
      st_transform(4326) %>%
      mutate(centroid_longitude = st_coordinates(.)[,1],
             centroid_latitude  = st_coordinates(.)[,2]) %>%
      st_drop_geometry() %>%
      select(hex_id, centroid_longitude, centroid_latitude)
    
    wea_freq_label <- if (exists("parameter_frequency_table") && nrow(parameter_frequency_table) > 0)
      paste(unique(parameter_frequency_table$frequency_label), collapse = "; ")
    else sampling_frequency
    
    wea_hex_sf_res <- wea_hex_grid_res %>%
      left_join(wea_hex_summary_res, by = "hex_id") %>%
      left_join(wea_centroids_res,   by = "hex_id") %>%
      filter(!is.na(programs)) %>%
      st_transform(4326) %>%
      st_make_valid() %>%
      mutate(
        `Program Name`       = program_name,
        `Full Program Name`  = program_full_name,
        `Monitoring Program` = program_name,
        `Parameters`         = parameters,
        `EOV Groups`         = eov_groups,
        `Parameter Count`    = parameter_count,
        `First Year`         = first_year,
        `Last Year`          = last_year,
        `Years Sampled`      = year_count,
        `Frequency`          = wea_freq_label,
        `Platform`           = program_platform,
        `Geometry Types`     = "Fixed Station",
        `Sample Locations`   = sample_location_count,
        `Centroid Latitude`  = centroid_latitude,
        `Centroid Longitude` = centroid_longitude,
        `Gebco Mean Depth`   = gebco_mean_depth,
        `Source Files`       = source_files,
        `Depth Range (m)`    = NA_character_
      ) %>%
      # Drop original lowercase columns to avoid duplicate name error in st_write
      select(
        `Program Name`, `Full Program Name`, `Monitoring Program`,
        `Parameters`, `EOV Groups`, `Parameter Count`,
        `First Year`, `Last Year`, `Years Sampled`,
        `Frequency`, `Platform`, `Geometry Types`,
        `Depth Range (m)`, `Sample Locations`,
        `Centroid Latitude`, `Centroid Longitude`,
        `Gebco Mean Depth`, `Source Files`,
        geometry
      )
    
    wea_geojson_out <- file.path(output_folder, paste0(display_name, "_wea_", hex_resolution, ".geojson"))
    suppressWarnings(st_write(wea_hex_sf_res, wea_geojson_out, delete_dsn = TRUE, quiet = TRUE))
    cat("WEA GeoJSON written:", basename(wea_geojson_out), "| Features:", nrow(wea_hex_sf_res), "\n")
  }
  
  ############################################
  # STANDALONE LEAFLET HTML MAP
  ############################################
  
  cat("\n=== Writing standalone HTML map ===\n")
  
  geojson_content <- readr::read_file(geojson_out)
  
  leaflet_html <- paste0(
    '<!DOCTYPE html>
<html>
<head>
  <meta charset="utf-8"/>
  <title>', display_name, ' \u2014 ', hex_resolution, ' \u2014 Ocean Monitoring Inventory</title>
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <link rel="stylesheet" href="https://unpkg.com/leaflet@1.9.4/dist/leaflet.css"/>
  <script src="https://unpkg.com/leaflet@1.9.4/dist/leaflet.js"></script>
  <style>
    * { box-sizing: border-box; margin: 0; padding: 0; }
    body { font-family: "Segoe UI", Arial, sans-serif; }
    #map { width: 100vw; height: 100vh; }
    #disclaimer {
      position: absolute; bottom: 24px; left: 50%; transform: translateX(-50%);
      z-index: 1000; background: rgba(0,0,0,0.65); color: #fff; font-size: 11px;
      padding: 6px 16px; border-radius: 20px; pointer-events: none;
      white-space: nowrap; letter-spacing: 0.02em;
    }
    #res-badge {
      position: absolute; top: 12px; right: 12px; z-index: 1000;
      background: #005e8c; color: #fff; font-size: 11px; font-weight: 700;
      padding: 5px 12px; border-radius: 4px; pointer-events: none;
    }
    .leaflet-popup-content-wrapper { border-radius: 6px; padding: 0; overflow: hidden;
      box-shadow: 0 4px 18px rgba(0,0,0,0.22); min-width: 300px; max-width: 420px; }
    .leaflet-popup-content { margin: 0; width: 100% !important; }
    .pp-header   { background: #005e8c; color: #fff; padding: 14px 18px 12px; }
    .pp-title    { font-size: 16px; font-weight: 700; line-height: 1.3; margin-bottom: 3px; }
    .pp-subtitle { font-size: 11px; opacity: 0.8; font-style: italic; }
    .pp-narrative { background: #f0f6fb; padding: 9px 18px; font-size: 12px; color: #333;
                    border-bottom: 1px solid #d6e6f0; line-height: 1.55; }
    .pp-narrative b { color: #005e8c; }
    .pp-table { width: 100%; border-collapse: collapse; font-size: 12px; }
    .pp-table tr:nth-child(even) td { background: #f7f7f7; }
    .pp-table td { padding: 6px 18px; vertical-align: top;
                   border-bottom: 1px solid #ececec; line-height: 1.4; }
    .pp-table td:first-child { font-weight: 600; color: #555; white-space: nowrap; width: 38%; }
    .pp-section-head td { background: #e8f0f5 !important; font-size: 10px; font-weight: 700;
      text-transform: uppercase; letter-spacing: 0.06em; color: #005e8c; padding: 5px 18px; }
    .polygon-label { font-size: 11px; font-weight: 600; background: rgba(255,255,255,0.85);
                     border: 1px solid #ccc; border-radius: 3px; padding: 2px 6px; }
  </style>
</head>
<body>
<div id="map"></div>
<div id="disclaimer">
  \u26a0\ufe0f This inventory reflects monitoring within 12 nautical miles of the CA coast \u2014 it does not include all offshore programs.
</div>
<div id="res-badge">Grid: ', hex_resolution, '</div>
<script>
var geojsonData = ', geojson_content, ';
var map = L.map("map").setView([37.5, -122.5], 6);
L.tileLayer(
  "https://server.arcgisonline.com/ArcGIS/rest/services/Ocean/World_Ocean_Base/MapServer/tile/{z}/{y}/{x}",
  { attribution: "Tiles &copy; Esri", maxZoom: 18 }
).addTo(map);

function val(v) {
  return (v===null||v===undefined||v==="NA"||v===""||v==="null")?null:v;
}
function row(label, value) {
  if (!val(value)) return "";
  return "<tr><td>" + label + "</td><td>" + value + "</td></tr>";
}
function sh(label) {
  return "<tr class=\'pp-section-head\'><td colspan=\'2\'>" + label + "</td></tr>";
}
function makePopup(p) {
  var narrative = val(p["Parameters"])
    ? "<div class=\'pp-narrative\'><b>Parameters:</b> " + p["Parameters"] + "</div>"
    : "";
  var depthRaw = val(p["Depth Range (m)"]);
  var depthDisplay = depthRaw;
  if (depthRaw) {
    var nums = depthRaw.match(/-?([\\d.]+)/g);
    if (nums && nums.length === 1)
      depthDisplay = "0\u2013" + Math.abs(parseFloat(nums[0])) + " m";
    else if (nums && nums.length >= 2)
      depthDisplay = Math.abs(parseFloat(nums[0])) + "\u2013" + Math.abs(parseFloat(nums[1])) + " m";
  }
  var tableRows =
    sh("Sampling") +
    row("Frequency",      p["Frequency"]) +
    row("Platform",       p["Platform"]) +
    row("First Year",     p["First Year"]) +
    row("Last Year",      p["Last Year"]) +
    row("Years Sampled",  p["Years Sampled"]) +
    row("Sample Locations", p["Sample Locations"]) +
    sh("Depth") +
    row("Overall Range",  depthDisplay) +
    row("Seafloor Depth", val(p["Gebco Mean Depth"])
      ? parseFloat(p["Gebco Mean Depth"]).toFixed(0) + " m" : null) +
    sh("Location") +
    row("Latitude",   val(p["Centroid Latitude"])  ? parseFloat(p["Centroid Latitude"]).toFixed(4)  : null) +
    row("Longitude",  val(p["Centroid Longitude"]) ? parseFloat(p["Centroid Longitude"]).toFixed(4) : null) +
    row("EOV Groups", p["EOV Groups"]);
  return "<div class=\'pp-header\'>" +
    "<div class=\'pp-title\'>"    + (val(p["Program Name"]) || "Station") + "</div>" +
    "<div class=\'pp-subtitle\'>" + (val(p["Full Program Name"]) || "")   + "</div>" +
    "</div>" + narrative + "<table class=\'pp-table\'>" + tableRows + "</table>";
}

L.geoJSON(geojsonData, {
  style: function() { return { color: "#005e8c", weight: 1, fillColor: "#0079c1", fillOpacity: 0.3 }; },
  onEachFeature: function(feature, layer) {
    layer.bindPopup(makePopup(feature.properties), { maxWidth: 420 });
    layer.on("mouseover", function() { this.setStyle({ fillOpacity: 0.55, weight: 2 }); });
    layer.on("mouseout",  function() { this.setStyle({ fillOpacity: 0.3,  weight: 1 }); });
  }
}).addTo(map);

', polygon_js_standalone, '
</script>
</body>
</html>')
  
  html_out <- file.path(output_folder, paste0(display_name, "_", hex_resolution, "_map.html"))
  writeLines(leaflet_html, html_out, useBytes = FALSE)
  cat("HTML map written:", html_out, "\n")
  
} # ===== END RESOLUTION LOOP =====

cat("\n===== ALL RESOLUTIONS COMPLETE =====\n")
cat("Program:           ", program_name, "\n")
cat("CSV files read:    ", length(csv_files), "\n")
cat("Polygon overlays:  ", length(spatial_files_overlay), "\n")
cat("Point SHP skipped: ", length(spatial_files_skip), "\n")
cat("Coastal rows:      ", nrow(all_filtered_coords_df), "\n")
cat("WEA-only rows:     ", nrow(wea_only_coords_df), "\n")
cat("Resolutions:        1km | 3km | 5km\n")
cat("Output folder:     ", output_folder, "\n")
cat("Main GeoJSONs:      coastal only (_1km/3km/5km.geojson)\n")
cat("WEA GeoJSONs:       WEA-only (_wea_1km/3km/5km.geojson)\n")
cat("=====================================\n")
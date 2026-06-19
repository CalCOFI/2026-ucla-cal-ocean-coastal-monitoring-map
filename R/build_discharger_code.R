###################################################################################
# build_discharger_code.R
#
# Reads one or more discharger/WWTP monitoring CSVs and outputs a
# point-marker GeoJSON for use as a separate map layer.
#
# INPUTS:
#   - One or more CSVs in discharger_folder
#   - Lat/lon columns auto-detected; DMS and decimal formats both supported
#   - Duplicate locations collapsed to one point per unique coordinate
#
# OUTPUT:
#   - Dischargers.geojson (point layer, one feature per unique location)
#
# Map display and popup styling handled by index.html, not this script.
#
# Run this BEFORE build_combine_map.R
###################################################################################

library(readr)
library(dplyr)
library(stringr)
library(purrr)
library(sf)
library(janitor)

# =============================================================================
# USER SETTINGS 
# =============================================================================

discharger_folder     <- "C:/Users/bhuan/Downloads/Monitoring Data/Dischargers"
discharger_layer_name <- "Dischargers"

output_root <- "C:/Users/bhuan/Downloads/Monitoring_Outputs"
output_folder <- file.path(output_root, discharger_layer_name)
dir.create(output_folder, showWarnings = FALSE, recursive = TRUE)

# =============================================================================
# CONSTANTS
# =============================================================================

PLACEHOLDER_VALUES <- c(
  "", "na", "n/a", "nan", "null", "none", "nd", "n.d.",
  "-999", "-9999", "999", "9999", "see figure", "tbd", "unknown"
)

# =============================================================================
# HELPER FUNCTIONS
# =============================================================================

clean_cols <- function(df) {
  df %>% mutate(across(where(is.character), str_squish))
}

collapse_unique <- function(x, sep = "; ") {
  x <- as.character(x)
  x <- x[!is.na(x) & str_trim(x) != ""]
  x <- unique(x)
  if (length(x) == 0) return(NA_character_)
  paste(sort(x), collapse = sep)
}

is_placeholder <- function(x) {
  str_trim(str_to_lower(as.character(x))) %in% PLACEHOLDER_VALUES
}

# Handles decimal OR degrees-minutes-seconds formats:
# e.g. 33.7366, "33°09'56\"", "33° 9' 56\""
parse_coord_value <- function(x) {
  x <- str_trim(as.character(x))
  if (is.na(x) || x == "") return(NA_real_)
  
  num <- suppressWarnings(as.numeric(x))
  if (!is.na(num)) return(num)
  
  # Normalize degree/apostrophe variants before DMS matching
  x_clean <- x %>%
    str_replace_all("\u00b0|\u00ba", "°") %>%
    str_replace_all('\u2019|\u2018|`', "'")
  
  dms <- str_match(x_clean,
                   "(\\d+)[°d]\\s*(\\d+)['''\u2032]\\s*([0-9.]+)[\"'\u2033s]?")
  if (!is.na(dms[1,1])) {
    deg <- as.numeric(dms[1,2])
    min <- as.numeric(dms[1,3])
    sec <- as.numeric(dms[1,4])
    return(deg + min/60 + sec/3600)
  }
  
  dm <- str_match(x_clean, "(\\d+)[°d]\\s*([0-9.]+)['''\u2032]")
  if (!is.na(dm[1,1])) {
    return(as.numeric(dm[1,2]) + as.numeric(dm[1,3])/60)
  }
  
  NA_real_
}

detect_lat_col <- function(df) {
  patterns <- c("lat.*mid|mid.*lat", "^latitude$|^lat$|lat_",
                "latitude.*n|lat.*n", "^lat")
  for (pat in patterns) {
    hits <- names(df)[str_detect(names(df), regex(pat, ignore_case = TRUE))]
    if (length(hits) > 0) return(hits[1])
  }
  NA_character_
}

detect_lon_col <- function(df) {
  patterns <- c("lon.*mid|long.*mid|mid.*lon", "^longitude$|^lon$|^long$|lon_",
                "longitude.*w|lon.*w", "^lon|^long")
  for (pat in patterns) {
    hits <- names(df)[str_detect(names(df), regex(pat, ignore_case = TRUE))]
    if (length(hits) > 0) return(hits[1])
  }
  NA_character_
}

# CA longitudes should be negative (~-117 to -124); flip if stored as positive
fix_longitude_sign <- function(vals, file_label = "") {
  med <- median(vals, na.rm = TRUE)
  if (!is.na(med) && med > 100 && med < 135) {
    message("  [", file_label, "] Longitude stored as positive — flipping to negative.")
    return(-abs(vals))
  }
  vals
}

# =============================================================================
# READ + CLEAN ONE CSV
# =============================================================================

read_discharger_file <- function(file_path) {
  message("Reading: ", basename(file_path))
  
  df_raw <- read_csv(file_path, show_col_types = FALSE,
                     col_types = cols(.default = col_character()))
  
  raw_names <- names(df_raw) %>%
    str_replace_all("\u00b5", "u") %>%
    str_replace_all("[^[:ascii:]]", "u") %>%
    str_replace_all("/", "_per_")
  names(df_raw) <- raw_names
  
  df <- df_raw %>%
    clean_names() %>%
    clean_cols() %>%
    mutate(source_file = basename(file_path),
           source_path = file_path)
  
  lat_col <- detect_lat_col(df)
  lon_col <- detect_lon_col(df)
  
  if (is.na(lat_col) || is.na(lon_col)) {
    warning("Could not detect lat/lon in ", basename(file_path), " — skipping.")
    return(NULL)
  }
  
  cat("  lat:", lat_col, "| lon:", lon_col, "\n")
  
  lat_vals <- sapply(df[[lat_col]], parse_coord_value, USE.NAMES = FALSE)
  lon_vals <- fix_longitude_sign(
    sapply(df[[lon_col]], parse_coord_value, USE.NAMES = FALSE),
    basename(file_path)
  )
  
  bad <- is_placeholder(df[[lat_col]]) | is_placeholder(df[[lon_col]]) |
    is.na(lat_vals) | is.na(lon_vals)
  
  if (any(bad)) message("  Dropping ", sum(bad), " row(s) with bad coordinates.")
  
  df %>%
    mutate(latitude_std  = lat_vals,
           longitude_std = lon_vals,
           coord_ok      = !bad) %>%
    filter(coord_ok) %>%
    select(-coord_ok)
}

# =============================================================================
# READ ALL CSVs IN FOLDER
# =============================================================================

csv_files <- list.files(discharger_folder, pattern = "\\.csv$",
                        full.names = TRUE, recursive = TRUE) %>%
  # Exclude any output subfolders
  .[!str_detect(., regex("(/|\\\\)(output[^/\\\\]*)(/|\\\\)", ignore_case = TRUE))]

if (length(csv_files) == 0) stop("No CSV files found in discharger_folder.")

cat("\nFound", length(csv_files), "CSV file(s):\n")
print(basename(csv_files))

all_df <- map(csv_files, read_discharger_file) %>%
  compact() %>%
  bind_rows()

if (nrow(all_df) == 0) stop("No plottable rows after reading all files.")
cat("\nTotal plottable rows:", nrow(all_df), "\n")

# =============================================================================
# COLLAPSE TO ONE ROW PER UNIQUE LOCATION
# =============================================================================

COORD_COLS <- c("latitude_std", "longitude_std", "source_file", "source_path")

# Re-scan files to identify original lat/lon column names to exclude
safe_read_header <- function(path) {
  tryCatch({
    read_csv(path, n_max = 1, show_col_types = FALSE,
             col_types = cols(.default = col_character())) %>% clean_names()
  }, error = function(e) NULL)
}

# grabs the original column names before clean_names() renamed them 
# so we can exclude the raw lat/lon columns from the final output attributes
lat_col_names <- map_chr(csv_files, ~ {
  df <- safe_read_header(.x)
  if (is.null(df)) NA_character_ else detect_lat_col(df)
}) %>% na.omit() %>% unique()

lon_col_names <- map_chr(csv_files, ~ {
  df <- safe_read_header(.x)
  if (is.null(df)) NA_character_ else detect_lon_col(df)
}) %>% na.omit() %>% unique()

exclude_cols <- unique(c(lat_col_names, lon_col_names, COORD_COLS))
attr_cols    <- setdiff(names(all_df), exclude_cols)

loc_df <- all_df %>%
  mutate(.loc_key = paste(round(latitude_std, 5), round(longitude_std, 5), sep = "_"))

# Columns that vary across rows sharing a coordinate get collapsed to a
# semicolon-separated string so no data is lost when rows are merged
multi_val_cols <- attr_cols[map_lgl(attr_cols, ~ {
  if (!.x %in% names(loc_df)) return(FALSE)
  loc_df %>%
    group_by(.loc_key) %>%
    summarise(n = n_distinct(.data[[.x]], na.rm = TRUE), .groups = "drop") %>%
    pull(n) %>%
    max(na.rm = TRUE) > 1
})]
single_val_cols <- setdiff(attr_cols, multi_val_cols)

grouped <- loc_df %>%
  group_by(.loc_key, latitude_std, longitude_std) %>%
  summarise(
    across(all_of(multi_val_cols),  ~ collapse_unique(.x)),
    across(all_of(single_val_cols), ~ {
      v <- unique(na.omit(as.character(.x)))
      if (length(v) == 0) NA_character_ else v[1]
    }),
    source_files = collapse_unique(source_file),
    n_rows       = n(),
    .groups      = "drop"
  ) %>%
  select(-.loc_key) %>%
  mutate(point_id = paste0("DISCH_", row_number()))

cat("\nUnique station points:", nrow(grouped), "\n")

# =============================================================================
# WRITE GEOJSON
# =============================================================================

points_sf <- grouped %>%
  st_as_sf(coords = c("longitude_std", "latitude_std"), crs = 4326, remove = FALSE)

geojson_path <- file.path(output_folder, paste0(discharger_layer_name, ".geojson"))
suppressWarnings(st_write(points_sf, geojson_path, delete_dsn = TRUE, quiet = TRUE))

cat("\n===== BUILD COMPLETE =====\n")
cat("CSV files read:        ", length(csv_files), "\n")
cat("Total rows parsed:     ", nrow(all_df), "\n")
cat("Unique station points: ", nrow(grouped), "\n")
cat("GeoJSON:               ", geojson_path, "\n")
cat("==========================\n")

# =============================================================================
# Title:       02_clean.R
# Author:      Tahir Arif, MPH
# Date:        March 2026
# Description: Clean raw NSSP influenza ED visit data. Standardizes column
#              names, parses dates, calculates MMWR week variables, classifies
#              respiratory seasons, and handles missing values.
# =============================================================================

# ---- Libraries ---------------------------------------------------------------
library(tidyverse)   # dplyr, tidyr, readr, stringr
library(janitor)     # clean_names(), remove_empty()
library(lubridate)   # Date parsing and manipulation
library(MMWRweek)    # MMWR epidemiologic week calculation
library(here)        # Relative paths

# ---- Paths -------------------------------------------------------------------
RAW_PATH    <- here("data", "raw_influenza_nssp.csv")
CLEAN_PATH  <- here("data", "clean_influenza.csv")

# ---- Load raw data -----------------------------------------------------------
message("Reading raw data from: ", RAW_PATH)
influenza_raw <- read_csv(RAW_PATH, show_col_types = FALSE)

message("Raw data dimensions: ", nrow(influenza_raw), " rows x ", ncol(influenza_raw), " cols")

# ---- Step 1: Standardize column names ----------------------------------------
# janitor::clean_names() converts to snake_case, removes special characters
influenza <- influenza_raw |>
  clean_names() |>
  remove_empty(which = c("rows", "cols"))

message("Column names after cleaning:")
print(names(influenza))

# ---- Step 2: Parse and standardize date columns ------------------------------
# NSSP typically provides week_end_date in ISO 8601 format (YYYY-MM-DDThh:mm:ss)
# We parse to Date, then derive week_start (Sunday), year, month

influenza <- influenza |>
  mutate(
    # Parse the week-ending date (strip time component if present)
    week_end_date  = as_date(parse_date_time(week_end_date, orders = c("ymdHMS", "ymd", "mdy"))),
    # NSSP week_end is Saturday; week_start is the preceding Sunday
    week_start_date = week_end_date - days(6),
    year            = year(week_end_date),
    month           = month(week_end_date, label = TRUE, abbr = TRUE),
    month_num       = month(week_end_date)
  )

# ---- Step 3: Calculate MMWR epidemiologic week -------------------------------
# MMWRweek package converts dates to MMWR year/week (CDC standard)
mmwr_vars <- MMWRweek(influenza$week_end_date)

influenza <- influenza |>
  mutate(
    mmwr_week = mmwr_vars$MMWRweek,
    mmwr_year = mmwr_vars$MMWRyear
  )

message("MMWR week range: ", min(influenza$mmwr_week, na.rm = TRUE), 
        " – ", max(influenza$mmwr_week, na.rm = TRUE))

# ---- Step 4: Classify respiratory seasons ------------------------------------
# Respiratory season: MMWR week 40 (early Oct) through week 20 (mid-May)
# of the *following* calendar year. Label as "YYYY–YY" (e.g., "2023-24").

classify_season <- function(mmwr_week, mmwr_year) {
  dplyr::case_when(
    mmwr_week >= 40 ~ paste0(mmwr_year,       "-", substr(mmwr_year + 1, 3, 4)),
    mmwr_week <= 20 ~ paste0(mmwr_year - 1,   "-", substr(mmwr_year,     3, 4)),
    TRUE            ~ "Off-season"
  )
}

# Peak season flag: weeks 40–20 (Oct–May) — broadened for influenza
influenza <- influenza |>
  mutate(
    season      = classify_season(mmwr_week, mmwr_year),
    peak_season = season != "Off-season"
  )

message("Seasons identified:")
print(sort(unique(influenza$season)))

# ---- Step 5: Standardize numeric columns -------------------------------------
# Percent-of-ED-visits columns typically come as character strings from Socrata

# Identify likely percentage columns (contain "percent" or "pct" in name)
pct_cols <- names(influenza)[str_detect(names(influenza), "percent|pct|rate|count")]

message("Numeric columns to coerce: ", paste(pct_cols, collapse = ", "))

influenza <- influenza |>
  mutate(across(all_of(pct_cols), ~ suppressWarnings(as.numeric(.x))))

# ---- Step 6: Handle missing values -------------------------------------------
# Report missingness before any imputation/removal
missing_summary <- influenza |>
  summarise(across(everything(), ~ sum(is.na(.)))) |>
  pivot_longer(everything(), names_to = "column", values_to = "n_missing") |>
  filter(n_missing > 0) |>
  arrange(desc(n_missing))

message("\nColumns with missing values:")
print(missing_summary)

# For percentage columns: NSSP suppresses values <0.01% — flag these
# We do NOT impute surveillance suppression; we flag it instead
if ("percent_visits" %in% names(influenza)) {
  influenza <- influenza |>
    mutate(suppressed = is.na(percent_visits))
  
  n_suppressed <- sum(influenza$suppressed)
  message("Suppressed records (NA percent_visits): ", n_suppressed)
}

# Remove rows where week_end_date is missing (cannot be placed in time)
influenza <- influenza |>
  filter(!is.na(week_end_date))

# ---- Step 7: Rename key columns for clarity ----------------------------------
# Ensure consistent naming regardless of Socrata field version
influenza <- influenza |>
  rename_with(~ str_replace_all(.x, "ed_visits_percent|percent_visits_weighted", "pct_ed_visits"),
              .cols = any_of(c("ed_visits_percent", "percent_visits_weighted")))

# ---- Step 8: Select and order final columns ----------------------------------
# Keep all original columns plus derived variables; reorder for readability
priority_cols <- c(
  "week_end_date", "week_start_date", "year", "month", "month_num",
  "mmwr_week", "mmwr_year", "season", "peak_season",
  "geography", "pathogen"
)

# Remaining columns (all others not in priority list)
other_cols <- setdiff(names(influenza), priority_cols)

influenza <- influenza |>
  select(all_of(priority_cols), all_of(other_cols))

# ---- Final validation --------------------------------------------------------
message("\nCleaned data dimensions: ", nrow(influenza), " rows x ", ncol(influenza), " cols")
message("Date range: ", min(influenza$week_end_date), " to ", max(influenza$week_end_date))
message("Seasons in data:")
print(table(influenza$season))

# ---- Save cleaned data -------------------------------------------------------
write_csv(influenza, CLEAN_PATH)
message("\nClean data saved to: ", CLEAN_PATH)

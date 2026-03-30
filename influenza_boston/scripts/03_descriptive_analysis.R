# =============================================================================
# Title:       03_descriptive_analysis.R
# Author:      Tahir Arif, MPH
# Date:        March 2026
# Description: Descriptive epidemiologic analysis of influenza ED visits in
#              Massachusetts. Computes weekly visit percentages, season-over-
#              season comparisons, summary statistics, and age-group
#              stratification where data permit.
# =============================================================================

# ---- Libraries ---------------------------------------------------------------
library(tidyverse)   # dplyr, tidyr, readr, stringr, purrr
library(janitor)     # tabyl(), adorn_*
library(lubridate)   # Date arithmetic
library(scales)      # Number formatting
library(here)        # Relative paths

# ---- Paths -------------------------------------------------------------------
CLEAN_PATH      <- here("data", "clean_influenza.csv")
OUT_WEEKLY      <- here("output", "table_weekly_summary.csv")
OUT_SEASON      <- here("output", "table_season_comparison.csv")
OUT_STATS       <- here("output", "table_summary_statistics.csv")
OUT_AGE         <- here("output", "table_age_stratification.csv")

# ---- Load cleaned data -------------------------------------------------------
message("Reading cleaned data from: ", CLEAN_PATH)
influenza <- read_csv(CLEAN_PATH, show_col_types = FALSE)

# Identify the primary ED visit percentage column
# NSSP may use different names depending on the dataset version
pct_col <- names(influenza)[str_detect(names(influenza), "pct_ed_visits|percent|visits_pct")][1]
message("Using percentage column: ", pct_col)

# ---- Analysis 1: Weekly ED visit percentages ---------------------------------
# Aggregate to one row per week (in case multiple age groups exist)
# If age groups are present, sum them or take the "all ages" row

weekly_summary <- influenza |>
  # If an "age_group" column exists, filter to "All Ages" for overall trend
  # Otherwise use all rows
  {
    if ("age_group" %in% names(influenza)) {
      filter(., str_detect(tolower(age_group), "all|overall|total"))
    } else {
      .
    }
  } |>
  group_by(week_end_date, mmwr_week, mmwr_year, season, peak_season) |>
  summarise(
    pct_ed_visits = mean(.data[[pct_col]], na.rm = TRUE),
    n_obs         = n(),
    .groups       = "drop"
  ) |>
  arrange(week_end_date)

message("Weekly summary rows: ", nrow(weekly_summary))
print(head(weekly_summary, 10))

write_csv(weekly_summary, OUT_WEEKLY)
message("Weekly summary saved to: ", OUT_WEEKLY)

# ---- Analysis 2: Season-over-season comparison --------------------------------
# For each season, compute: peak week, peak %, mean %, total weeks above threshold

# Define "elevated" as >1% of ED visits (standard public health threshold)
ELEVATED_THRESHOLD <- 1.0

season_comparison <- weekly_summary |>
  filter(peak_season) |>                       # Restrict to Oct–May weeks
  group_by(season) |>
  summarise(
    n_weeks             = n(),
    mean_pct            = round(mean(pct_ed_visits, na.rm = TRUE), 3),
    median_pct          = round(median(pct_ed_visits, na.rm = TRUE), 3),
    max_pct             = round(max(pct_ed_visits, na.rm = TRUE), 3),
    sd_pct              = round(sd(pct_ed_visits, na.rm = TRUE), 3),
    peak_week           = mmwr_week[which.max(pct_ed_visits)],
    peak_week_date      = as.character(week_end_date[which.max(pct_ed_visits)]),
    weeks_elevated      = sum(pct_ed_visits >= ELEVATED_THRESHOLD, na.rm = TRUE),
    pct_weeks_elevated  = round(weeks_elevated / n_weeks * 100, 1),
    .groups             = "drop"
  ) |>
  arrange(season)

message("\nSeason-over-season comparison:")
print(season_comparison)

write_csv(season_comparison, OUT_SEASON)
message("Season comparison saved to: ", OUT_SEASON)

# ---- Analysis 3: Overall summary statistics ----------------------------------
# Descriptive stats for the full dataset and by peak vs. off-season

summary_stats <- influenza |>
  group_by(season) |>
  summarise(
    n_weeks      = n_distinct(week_end_date),
    mean_pct     = round(mean(.data[[pct_col]], na.rm = TRUE), 3),
    sd_pct       = round(sd(.data[[pct_col]], na.rm = TRUE), 3),
    min_pct      = round(min(.data[[pct_col]], na.rm = TRUE), 3),
    p25_pct      = round(quantile(.data[[pct_col]], 0.25, na.rm = TRUE), 3),
    median_pct   = round(median(.data[[pct_col]], na.rm = TRUE), 3),
    p75_pct      = round(quantile(.data[[pct_col]], 0.75, na.rm = TRUE), 3),
    max_pct      = round(max(.data[[pct_col]], na.rm = TRUE), 3),
    .groups      = "drop"
  )

message("\nSummary statistics by season:")
print(summary_stats)

write_csv(summary_stats, OUT_STATS)
message("Summary statistics saved to: ", OUT_STATS)

# ---- Analysis 4: Age group stratification (if available) ---------------------
if ("age_group" %in% names(influenza)) {
  message("\nAge groups found in data: ", paste(unique(influenza$age_group), collapse = ", "))
  
  age_summary <- influenza |>
    filter(peak_season) |>
    group_by(age_group, season) |>
    summarise(
      n_weeks    = n_distinct(week_end_date),
      mean_pct   = round(mean(.data[[pct_col]], na.rm = TRUE), 3),
      max_pct    = round(max(.data[[pct_col]], na.rm = TRUE), 3),
      peak_week  = mmwr_week[which.max(.data[[pct_col]])],
      .groups    = "drop"
    ) |>
    arrange(age_group, season)
  
  message("Age-stratified summary:")
  print(age_summary)
  
  write_csv(age_summary, OUT_AGE)
  message("Age stratification table saved to: ", OUT_AGE)
  
} else {
  message("\nNo age_group column found in data — skipping age stratification.")
  message("Note: Age-stratified NSSP data may be available from a separate endpoint.")
  
  # Write placeholder file so downstream scripts don't error
  tibble(
    note = "Age-stratified data not available in this pull. ",
    recommendation = "Query NSSP with age_group parameter or use a supplemental dataset."
  ) |>
    write_csv(OUT_AGE)
}

# ---- Print overall findings summary ------------------------------------------
message("\n=== KEY FINDINGS SUMMARY ===")
message("Total weeks analyzed: ", nrow(weekly_summary))
message("Seasons covered: ", paste(sort(unique(weekly_summary$season)), collapse = ", "))

if (nrow(season_comparison) > 0) {
  peak_season_row <- season_comparison |> slice_max(max_pct, n = 1)
  message("Highest peak season: ", peak_season_row$season, 
          " (peak ", peak_season_row$max_pct, "% of ED visits)")
  message("Peak MMWR week: ", peak_season_row$peak_week)
}

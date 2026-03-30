# =============================================================================
# Title:       04_visualization.R
# Author:      Tahir Arif, MPH
# Date:        March 2026
# Description: Publication-quality ggplot2 visualizations of influenza ED
#              visit data in Massachusetts. Produces: (1) seasonal time series,
#              (2) season overlay comparison, (3) weekly heatmap by season.
# =============================================================================

# ---- Libraries ---------------------------------------------------------------
library(tidyverse)   # ggplot2, dplyr, tidyr, readr, forcats
library(lubridate)   # Date manipulation
library(viridis)     # Color-blind-safe color scales
library(scales)      # Axis label formatting
library(here)        # Relative paths

# ---- Shared plot theme -------------------------------------------------------
# Custom epidemiologic theme built on theme_minimal
theme_epi <- function(base_size = 12) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title        = element_text(face = "bold", size = base_size + 2, hjust = 0),
      plot.subtitle     = element_text(size = base_size - 1, color = "grey40", hjust = 0),
      plot.caption      = element_text(size = base_size - 3, color = "grey55", hjust = 0),
      axis.title        = element_text(size = base_size - 1, color = "grey30"),
      axis.text         = element_text(size = base_size - 2, color = "grey30"),
      panel.grid.minor  = element_blank(),
      panel.grid.major  = element_line(color = "grey92", linewidth = 0.4),
      legend.position   = "bottom",
      legend.title      = element_text(face = "bold", size = base_size - 2),
      legend.text       = element_text(size = base_size - 2),
      plot.margin       = margin(12, 16, 8, 12)
    )
}

# Set default theme globally
theme_set(theme_epi())

# ---- Paths -------------------------------------------------------------------
CLEAN_PATH     <- here("data", "clean_influenza.csv")
WEEKLY_PATH    <- here("output", "table_weekly_summary.csv")
SEASON_PATH    <- here("output", "table_season_comparison.csv")

OUT_TIMESERIES <- here("output", "fig_influenza_timeseries.png")
OUT_OVERLAY    <- here("output", "fig_influenza_season_overlay.png")
OUT_HEATMAP    <- here("output", "fig_influenza_heatmap.png")

# ---- Load data ---------------------------------------------------------------
message("Loading data...")
influenza    <- read_csv(CLEAN_PATH,  show_col_types = FALSE)
weekly_df    <- read_csv(WEEKLY_PATH, show_col_types = FALSE)
season_df    <- read_csv(SEASON_PATH, show_col_types = FALSE)

# Identify percentage column
pct_col <- names(influenza)[str_detect(names(influenza), "pct_ed_visits|percent|visits_pct")][1]
message("Percentage column: ", pct_col)

# Use weekly summary for most plots (one row per week)
# Exclude off-season for cleaner seasonal charts
weekly_peak <- weekly_df |>
  filter(peak_season) |>
  mutate(season = factor(season, levels = sort(unique(season))))

# ---- Figure 1: Full time series of influenza ED visits ----------------------
# Shows the complete temporal trend from the earliest to latest week in data

fig1 <- ggplot(weekly_df, aes(x = week_end_date, y = pct_ed_visits)) +
  # Shade respiratory seasons in alternating light gray
  geom_rect(
    data = weekly_df |>
      filter(peak_season) |>
      group_by(season) |>
      summarise(xmin = min(week_end_date), xmax = max(week_end_date), .groups = "drop"),
    aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, fill = "Respiratory Season"),
    inherit.aes = FALSE,
    alpha       = 0.10,
    color       = NA
  ) +
  # Main line
  geom_line(color = "#1B6CA8", linewidth = 0.8) +
  # Smooth trend
  geom_smooth(method = "loess", span = 0.2, color = "#E05C1B",
              se = FALSE, linewidth = 0.7, linetype = "dashed") +
  scale_fill_manual(values = c("Respiratory Season" = "#4A90D9"), name = NULL) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "6 months") +
  scale_y_continuous(labels = label_percent(scale = 1), limits = c(0, NA)) +
  labs(
    title    = "Influenza-Related ED Visits Are Sharply Seasonal in Massachusetts",
    subtitle = "Percentage of all ED visits flagged as influenza-related, by week",
    x        = "Week Ending Date",
    y        = "% of ED Visits",
    caption  = "Source: CDC NSSP BioSense Platform, 7xva-uux8 | Analysis: Tahir Arif, MPH"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(OUT_TIMESERIES, plot = fig1, width = 12, height = 5, dpi = 300, bg = "white")
message("Saved: ", OUT_TIMESERIES)

# ---- Figure 2: Season overlay — all seasons on the same MMWR week x-axis ----
# Each season is one colored line; x-axis = MMWR week (1–52)
# Only weeks 40–52 and 1–20 shown (peak season)

# Recode week to align season (week 40 = "week 1" of season)
weekly_peak <- weekly_peak |>
  mutate(
    season_week = if_else(mmwr_week >= 40, mmwr_week - 39, mmwr_week + 13)
  )

# Viridis color palette — one color per season
n_seasons  <- n_distinct(weekly_peak$season)
pal_colors <- viridis(n_seasons, option = "D", begin = 0.1, end = 0.9)

fig2 <- ggplot(weekly_peak, aes(x = season_week, y = pct_ed_visits,
                                 color = season, group = season)) +
  geom_line(linewidth = 0.9, alpha = 0.85) +
  geom_point(size = 1.2, alpha = 0.6) +
  scale_color_manual(values = pal_colors, name = "Respiratory Season") +
  scale_x_continuous(
    breaks = c(1, 5, 10, 15, 20, 25, 30),
    labels = c("Wk 40\n(Oct)", "Wk 44", "Wk 49\n(Dec)",
               "Wk 2\n(Jan)", "Wk 7\n(Feb)", "Wk 12\n(Mar)", "Wk 17\n(Apr)")
  ) +
  scale_y_continuous(labels = label_percent(scale = 1), limits = c(0, NA)) +
  labs(
    title    = "Peak Influenza Timing and Severity Vary Substantially Season to Season",
    subtitle = "Each line represents one respiratory season (MMWR weeks 40–20)",
    x        = "Respiratory Season Week",
    y        = "% of ED Visits",
    caption  = "Source: CDC NSSP BioSense Platform | Analysis: Tahir Arif, MPH"
  ) +
  guides(color = guide_legend(nrow = 2))

ggsave(OUT_OVERLAY, plot = fig2, width = 12, height = 6, dpi = 300, bg = "white")
message("Saved: ", OUT_OVERLAY)

# ---- Figure 3: Heatmap — season (y) × MMWR week (x) = % ED visits ----------
# Visualizes seasonal intensity pattern as a grid

heatmap_df <- weekly_peak |>
  select(season, season_week, pct_ed_visits) |>
  filter(!is.na(pct_ed_visits))

fig3 <- ggplot(heatmap_df, aes(x = season_week, y = fct_rev(season), fill = pct_ed_visits)) +
  geom_tile(color = "white", linewidth = 0.3) +
  scale_fill_viridis_c(
    option  = "C",         # "inferno" palette — intuitive hot = high
    name    = "% ED Visits",
    labels  = label_percent(scale = 1),
    limits  = c(0, max(heatmap_df$pct_ed_visits, na.rm = TRUE)),
    na.value = "grey90"
  ) +
  scale_x_continuous(
    breaks = c(1, 5, 10, 15, 20, 25, 30),
    labels = c("Wk 40\n(Oct)", "Wk 44", "Wk 49\n(Dec)",
               "Wk 2\n(Jan)", "Wk 7\n(Feb)", "Wk 12\n(Mar)", "Wk 17\n(Apr)")
  ) +
  labs(
    title    = "Influenza Activity Peaks Vary in Timing and Magnitude by Season",
    subtitle = "Heatmap of influenza-related ED visit percentage by season and week",
    x        = "Respiratory Season Week",
    y        = "Season",
    caption  = "Source: CDC NSSP BioSense Platform | Analysis: Tahir Arif, MPH"
  ) +
  theme(
    axis.text.y  = element_text(size = 9),
    legend.key.width = unit(1.5, "cm")
  )

ggsave(OUT_HEATMAP, plot = fig3, width = 12, height = 6, dpi = 300, bg = "white")
message("Saved: ", OUT_HEATMAP)

message("\nAll figures saved to output/")
message("Done.")

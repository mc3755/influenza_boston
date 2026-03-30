# Influenza Emergency Department Surveillance — Boston, MA

## Project Description

This repository analyzes influenza-associated emergency department (ED) visits in Massachusetts using CDC's National Syndromic Surveillance Program (NSSP) BioSense Platform data. The project tracks weekly influenza ED visit trends, performs season-over-season comparisons, and produces publication-quality visualizations suitable for public health reporting.

The analysis pipeline ingests real-time syndromic surveillance data from the CDC Socrata API, applies MMWR week classification, stratifies by respiratory season, and generates summary tables and charts for epidemiologic interpretation.

---

## Data Sources

| Source | Description | URL |
|--------|-------------|-----|
| CDC NSSP ED Visits | Weekly influenza-related ED visit percentages by state | https://data.cdc.gov/resource/7xva-uux8.json |
| CDC NSSP Data Dictionary | Variable definitions and coding | https://www.cdc.gov/nssp/biosense/index.html |

---

## Methods Summary

1. **Data acquisition**: CDC NSSP data queried via Socrata REST API, filtered for influenza pathogen and Massachusetts geography.
2. **Data cleaning**: Column names standardized, dates parsed, MMWR epidemiologic weeks calculated, respiratory seasons classified (October–March).
3. **Descriptive analysis**: Weekly ED visit percentages computed, season-over-season trends compared, age-group stratification applied where data permit.
4. **Visualization**: Time series overlays, season comparison charts, and weekly heat maps produced using ggplot2.

---

## Repository Structure

```
influenza_boston/
├── README.md
├── .gitignore
├── scripts/
│   ├── 01_data_pull.R          # Query CDC NSSP API and save raw data
│   ├── 02_clean.R              # Clean, standardize, add MMWR/season variables
│   ├── 03_descriptive_analysis.R  # Summary statistics and tables
│   └── 04_visualization.R      # ggplot2 charts saved to output/
├── data/                       # Raw and cleaned data (gitignored)
└── output/                     # Tables and figures (gitignored)
```

---

## How to Run

Run scripts **in order** from the project root directory:

```r
source("scripts/01_data_pull.R")
source("scripts/02_clean.R")
source("scripts/03_descriptive_analysis.R")
source("scripts/04_visualization.R")
```

Or from the terminal:

```bash
Rscript scripts/01_data_pull.R
Rscript scripts/02_clean.R
Rscript scripts/03_descriptive_analysis.R
Rscript scripts/04_visualization.R
```

---

## Required R Packages

```r
install.packages(c(
  "tidyverse",   # dplyr, ggplot2, tidyr, readr, stringr, forcats
  "janitor",     # clean_names(), tabyl()
  "httr",        # HTTP requests to Socrata API
  "jsonlite",    # JSON parsing
  "lubridate",   # Date manipulation
  "MMWRweek",    # MMWR epidemiologic week calculation
  "viridis",     # Color palettes
  "scales",      # Axis formatting
  "here"         # Relative paths
))
```

---

## Author

**Tahir Arif, MPH**  
Epidemiologist  
Date: March 2026

---

## Notes

- The CDC NSSP Socrata API does not require an API key for small queries (≤1,000 rows), but rate limits apply. For large pulls, register for a free Socrata account.
- NSSP data represent the percentage of ED visits flagged as influenza-related, not raw case counts; interpret accordingly.
- MMWR weeks begin on Sunday. The respiratory season is defined as MMWR weeks 40–20 (approximately October–May).

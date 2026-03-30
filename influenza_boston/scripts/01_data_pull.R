# =============================================================================
# Title:       01_data_pull.R
# Author:      Tahir Arif, MPH
# Date:        March 2026
# Description: Query CDC NSSP BioSense ED visit data for influenza in
#              Massachusetts via the Socrata REST API. Saves raw JSON
#              response as a CSV for downstream cleaning.
# =============================================================================

# ---- Libraries ---------------------------------------------------------------
library(httr)        # HTTP requests
library(jsonlite)    # JSON parsing
library(readr)       # write_csv
library(dplyr)       # Data manipulation
library(here)        # Relative paths

# ---- Configuration -----------------------------------------------------------

# Socrata API endpoint for NSSP ED visit data
# Dataset: Emergency Department Visits and Emergency Hospital Admissions for
#          Influenza, COVID-19, and RSV — Weekly, United States
NSSP_ENDPOINT <- "https://data.cdc.gov/resource/7xva-uux8.json"

# Query parameters
#   pathogen  = "Influenza" (case-sensitive per Socrata field values)
#   geography = "Massachusetts" (state-level filter)
#   $limit    = 5000 records (well above full Massachusetts dataset)
QUERY_PARAMS <- list(
  geography    = "Massachusetts",
  pathogen     = "Influenza",
  "$limit"     = 5000,
  "$order"     = "week_end_date DESC"
)

# Output path (relative to project root)
RAW_OUTPUT <- here("data", "raw_influenza_nssp.csv")

# ---- Helper: Safe API call ---------------------------------------------------

#' Pull data from Socrata REST API with error handling
#'
#' @param endpoint  Character. Full Socrata API URL.
#' @param params    Named list of query parameters.
#' @return          Data frame of parsed JSON results.
pull_socrata <- function(endpoint, params) {
  message("Querying Socrata API: ", endpoint)
  message("Parameters: ", paste(names(params), unlist(params), sep = "=", collapse = ", "))
  
  response <- GET(
    url   = endpoint,
    query = params,
    add_headers(
      "Accept"       = "application/json",
      "X-App-Token"  = ""   # Optionally set via Sys.getenv("SOCRATA_TOKEN")
    ),
    timeout(60)
  )
  
  # Check HTTP status
  if (http_error(response)) {
    stop(
      "API request failed with status ", status_code(response), ".\n",
      "URL: ", response$url, "\n",
      "Response: ", content(response, as = "text", encoding = "UTF-8")
    )
  }
  
  # Parse JSON content
  raw_text <- content(response, as = "text", encoding = "UTF-8")
  df       <- fromJSON(raw_text, flatten = TRUE)
  
  message("Records retrieved: ", nrow(df))
  return(df)
}

# ---- Pull data ---------------------------------------------------------------

influenza_raw <- pull_socrata(
  endpoint = NSSP_ENDPOINT,
  params   = QUERY_PARAMS
)

# ---- Basic validation --------------------------------------------------------

# Confirm the data frame is non-empty
if (nrow(influenza_raw) == 0) {
  stop(
    "API returned 0 records. Check filter values (geography, pathogen) ",
    "or verify the endpoint is still active."
  )
}

message("\nColumn names in raw data:")
print(names(influenza_raw))

message("\nDate range in raw data:")
if ("week_end_date" %in% names(influenza_raw)) {
  message(
    "  Min: ", min(influenza_raw$week_end_date, na.rm = TRUE),
    "  Max: ", max(influenza_raw$week_end_date, na.rm = TRUE)
  )
}

message("\nGeographies present:")
if ("geography" %in% names(influenza_raw)) {
  print(unique(influenza_raw$geography))
}

message("\nPathogens present:")
if ("pathogen" %in% names(influenza_raw)) {
  print(unique(influenza_raw$pathogen))
}

# ---- Save raw data -----------------------------------------------------------

write_csv(influenza_raw, RAW_OUTPUT)
message("\nRaw data saved to: ", RAW_OUTPUT)
message("Rows: ", nrow(influenza_raw), "  Cols: ", ncol(influenza_raw))

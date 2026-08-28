#  ==============================================================================
  # WISCONSIN WEATHER FORECAST ARCHIVE
  #
  # PROJECT LOCK
  #
  # OBJECTIVE
  # ---------
# Archive Open-Meteo weather forecasts for later comparison
# with NOAA observations.
#
# DAILY HIGH/LOW IS NON-NEGOTIABLE.
#
# Open-Meteo:
#   Use daily temperature_2m_max directly as High_Temp.
#   Use daily temperature_2m_min directly as Low_Temp.
#
# NEVER derive daily high/low from hourly forecasts.
#
#
# DAILY FORECAST WINDOW
# ---------------------
# Open-Meteo: Days 1-10
#
# Day 1 = TOMORROW.
# TODAY IS EXCLUDED.
#
#
# MISSING FORECASTS
# -----------------
# Missing forecasts are not fabricated.
# Dates with missing high or low are excluded.
#
#
# STATIONS
# --------
# KSBM  Sheboygan
# KMKE  Milwaukee
# KGRB  Green Bay
# KMSN  Madison
# KLSE  La Crosse
# KDLH  Duluth
#
# KCWA / Central Wisconsin Airport is NOT included.
#
#
# OUTPUT
# ------
# C:/Users/kwond/Documents/EMS/forecast_database/
#
# Filenames contain DATE ONLY.
#
# ==============================================================================


# ==============================================================================
# 1. PACKAGES
# ==============================================================================

rm(list = ls())

library(httr)
library(jsonlite)
library(dplyr)


# ==============================================================================
# 2. STATIONS
# ==============================================================================

stations <- data.frame(
  
  Station_ID = c(
    "KSBM",
    "KMKE",
    "KGRB",
    "KMSN",
    "KLSE",
    "KDLH"
  ),
  
  NOAA_ID = c(
    "USW00004841",
    "USW00014839",
    "USW00014898",
    "USW00014837",
    "USW00014920",
    "USW00014913"
  ),
  
  Location_Name = c(
    "Sheboygan",
    "Milwaukee",
    "Green Bay",
    "Madison",
    "La Crosse",
    "Duluth"
  ),
  
  latitude = c(
    43.76977,
    42.94720,
    44.49830,
    43.14060,
    43.87920,
    46.83690
  ),
  
  longitude = c(
    -87.85172,
    -87.89670,
    -88.11110,
    -89.34530,
    -91.25300,
    -92.20970
  ),
  
  Timezone = c(
    "America/Chicago",
    "America/Chicago",
    "America/Chicago",
    "America/Chicago",
    "America/Chicago",
    "America/Chicago"
  ),
  
  stringsAsFactors = FALSE
)


# ==============================================================================
# 3. KCWA SAFETY CHECK
# ==============================================================================

if (
  any(stations$Station_ID == "KCWA") ||
  any(grepl("Central Wisconsin", stations$Location_Name))
) {
  
  stop(
    "KCWA / Central Wisconsin Airport is present."
  )
  
}


# ==============================================================================
# 4. FORECAST HORIZON
# ==============================================================================

OPEN_METEO_DAYS <- 10


# ==============================================================================
# 5. OUTPUT DIRECTORY
# ==============================================================================

export_directory <-
  "C:/Users/kwond/Documents/EMS/forecast_database/"


dir.create(
  export_directory,
  recursive = TRUE,
  showWarnings = FALSE
)


# ==============================================================================
# 6. PULL DATE
# ==============================================================================

Pull_Timestamp <- Sys.time()

Pull_Date <- as.Date(
  Pull_Timestamp
)


# ==============================================================================
# 7. TARGET DATE WINDOW
# ==============================================================================
#
# Day 1 = tomorrow.
# Day 10 = ten days after the pull date.
#
# ==============================================================================

open_meteo_target_dates <- seq(
  Pull_Date + 1,
  Pull_Date + OPEN_METEO_DAYS,
  by = "day"
)


# ==============================================================================
# 8. START MESSAGE
# ==============================================================================

cat("\n")
cat("============================================================\n")
cat("WISCONSIN WEATHER FORECAST ARCHIVE\n")
cat("============================================================\n")

cat(
  "Pull timestamp: ",
  format(Pull_Timestamp),
  "\n",
  sep = ""
)

cat(
  "Pull date:      ",
  Pull_Date,
  "\n",
  sep = ""
)

cat("\n")

cat("Forecast source: Open-Meteo ONLY\n")
cat("Daily high/low:  DIRECT DAILY API FIELDS ONLY\n")
cat("High field:      temperature_2m_max\n")
cat("Low field:       temperature_2m_min\n")
cat("Hourly forecasts: NOT USED\n")
cat("Forecast window: Days 1-10\n")
cat("Day 1:           tomorrow\n")
cat("Today:           excluded\n")
cat("KCWA:            NOT INCLUDED\n")
cat("Missing forecasts: NOT FABRICATED\n")

cat("\n")
cat("============================================================\n")


# ==============================================================================
# 9. GENERIC API FUNCTION
# ==============================================================================

get_api <- function(
    url,
    headers = NULL,
    query = NULL
) {
  
  for (attempt in 1:3) {
    
    response <- tryCatch(
      
      GET(
        url,
        headers = headers,
        query = query,
        timeout(60)
      ),
      
      error = function(e) NULL
      
    )
    
    
    if (
      !is.null(response) &&
      status_code(response) == 200
    ) {
      
      return(response)
      
    }
    
    
    if (attempt < 3) {
      
      cat(
        "  API request failed. Retrying...\n"
      )
      
      Sys.sleep(3)
      
    }
    
  }
  
  
  stop(
    "API request failed after 3 attempts:\n",
    url
  )
  
}


# ==============================================================================
# 10. STORAGE
# ==============================================================================

open_meteo_list <- list()


# ==============================================================================
# 11. PROCESS EACH STATION
# ==============================================================================

for (
  i in seq_len(nrow(stations))
) {
  
  station_code <-
    stations$Station_ID[i]
  
  station_name <-
    stations$Location_Name[i]
  
  noaa_id <-
    stations$NOAA_ID[i]
  
  lat <-
    stations$latitude[i]
  
  lon <-
    stations$longitude[i]
  
  timezone <-
    stations$Timezone[i]
  
  
  cat("\n")
  cat("============================================================\n")
  
  cat(
    "STATION ",
    i,
    " OF ",
    nrow(stations),
    ": ",
    station_code,
    " - ",
    station_name,
    "\n",
    sep = ""
  )
  
  cat(
    "NOAA ID: ",
    noaa_id,
    "\n",
    sep = ""
  )
  
  cat("============================================================\n")
  
  
  # ============================================================================
  # A. OPEN-METEO DAILY FORECAST
  #
  # ONLY DAILY HIGH AND LOW FIELDS ARE REQUESTED.
  #
  # NO HOURLY FORECAST IS REQUESTED.
  # ============================================================================
  
  cat("\n")
  cat("Downloading Open-Meteo DAILY forecast...\n")
  
  
  om_url <-
    "https://api.open-meteo.com/v1/forecast"
  
  
  om_params <- list(
    
    latitude =
      lat,
    
    longitude =
      lon,
    
    daily =
      "temperature_2m_max,temperature_2m_min",
    
    forecast_days =
      OPEN_METEO_DAYS + 1,
    
    temperature_unit =
      "fahrenheit",
    
    timezone =
      timezone
    
  )
  
  
  om_response <- get_api(
    
    om_url,
    
    query =
      om_params
    
  )
  
  
  om_data <- content(
    
    om_response,
    
    as =
      "parsed",
    
    encoding =
      "UTF-8"
    
  )
  
  
  if (
    is.null(om_data$daily)
  ) {
    
    stop(
      "Open-Meteo daily data missing for ",
      station_code
    )
    
  }
  
  
  # ============================================================================
  # B. EXTRACT DIRECT DAILY FIELDS
  #
  # These are the actual forecast values archived.
  #
  # No hourly values are used.
  # ============================================================================
  
  om_dates <-
    as.Date(
      unlist(
        om_data$daily$time
      )
    )
  
  
  om_high <-
    as.numeric(
      unlist(
        om_data$daily$temperature_2m_max
      )
    )
  
  
  om_low <-
    as.numeric(
      unlist(
        om_data$daily$temperature_2m_min
      )
    )
  
  
  # ============================================================================
  # C. VALIDATE VECTOR LENGTHS
  # ============================================================================
  
  if (
    length(om_dates) != length(om_high) ||
    length(om_dates) != length(om_low)
  ) {
    
    stop(
      "Open-Meteo daily date/high/low vectors have different lengths for ",
      station_code
    )
    
  }
  
  
  # ============================================================================
  # D. CREATE DAILY ARCHIVE
  # ============================================================================
  
  om_daily <- data.frame(
    
    Target_Date =
      om_dates,
    
    High_Temp =
      om_high,
    
    Low_Temp =
      om_low,
    
    stringsAsFactors =
      FALSE
    
  ) %>%
    
    filter(
      
      Target_Date >=
        Pull_Date + 1,
      
      Target_Date <=
        Pull_Date + OPEN_METEO_DAYS
      
    ) %>%
    
    filter(
      
      !is.na(High_Temp),
      
      !is.na(Low_Temp)
      
    ) %>%
    
    mutate(
      
      Pull_Timestamp =
        Pull_Timestamp,
      
      Pull_Date =
        Pull_Date,
      
      Forecast_Day =
        as.integer(
          Target_Date - Pull_Date
        ),
      
      Station_ID =
        station_code,
      
      NOAA_ID =
        noaa_id,
      
      Location =
        station_name,
      
      Avg_Temp =
        (
          High_Temp +
            Low_Temp
        ) / 2,
      
      Source =
        "Open-Meteo"
      
    ) %>%
    
    select(
      
      Pull_Timestamp,
      Pull_Date,
      Forecast_Day,
      Station_ID,
      NOAA_ID,
      Location,
      Target_Date,
      High_Temp,
      Low_Temp,
      Avg_Temp,
      Source
      
    ) %>%
    
    arrange(
      Target_Date
    )
  
  
  # ============================================================================
  # E. VALIDATE HIGH / LOW
  # ============================================================================
  
  if (
    nrow(om_daily) > 0 &&
    any(
      om_daily$High_Temp <
      om_daily$Low_Temp,
      na.rm = TRUE
    )
  ) {
    
    stop(
      "Open-Meteo High < Low for ",
      station_code
    )
    
  }
  
  
  # ============================================================================
  # F. VALIDATE FORECAST DAYS
  # ============================================================================
  
  if (
    nrow(om_daily) > 0 &&
    any(
      om_daily$Forecast_Day < 1 |
      om_daily$Forecast_Day > OPEN_METEO_DAYS,
      na.rm = TRUE
    )
  ) {
    
    stop(
      "Invalid Open-Meteo forecast day for ",
      station_code
    )
    
  }
  
  
  # ============================================================================
  # G. VALIDATE TODAY IS EXCLUDED
  # ============================================================================
  
  if (
    nrow(om_daily) > 0 &&
    any(
      om_daily$Target_Date <=
      om_daily$Pull_Date,
      na.rm = TRUE
    )
  ) {
    
    stop(
      "Open-Meteo contains today or an earlier target date for ",
      station_code
    )
    
  }
  
  
  open_meteo_list[[i]] <-
    om_daily
  
  
  cat(
    "Open-Meteo daily days archived: ",
    nrow(om_daily),
    "\n",
    sep = ""
  )
  
  
  cat(
    "Station completed: ",
    station_code,
    "\n",
    sep = ""
  )
  
}


# ==============================================================================
# 12. COMBINE ALL STATIONS
# ==============================================================================

om_df <-
  bind_rows(
    open_meteo_list
  ) %>%
  
  arrange(
    Station_ID,
    Target_Date
  )


# ==============================================================================
# 13. FINAL KCWA CHECK
# ==============================================================================

if (
  any(om_df$Station_ID == "KCWA")
) {
  
  stop(
    "KCWA is present in the final archive."
  )
  
}


# ==============================================================================
# 14. FINAL DATE-RANGE CHECK
# ==============================================================================

if (
  nrow(om_df) > 0 &&
  any(
    om_df$Forecast_Day < 1 |
    om_df$Forecast_Day > OPEN_METEO_DAYS,
    na.rm = TRUE
  )
) {
  
  stop(
    "Invalid Open-Meteo forecast day detected."
  )
  
}


# ==============================================================================
# 15. FINAL TODAY CHECK
# ==============================================================================

if (
  nrow(om_df) > 0 &&
  any(
    om_df$Target_Date <=
    om_df$Pull_Date,
    na.rm = TRUE
  )
) {
  
  stop(
    "Open-Meteo contains today or an earlier target date."
  )
  
}


# ==============================================================================
# 16. FINAL HIGH/LOW VALIDATION
# ==============================================================================

if (
  nrow(om_df) > 0 &&
  any(
    om_df$High_Temp <
    om_df$Low_Temp,
    na.rm = TRUE
  )
) {
  
  stop(
    "A daily high is below its daily low."
  )
  
}


# ==============================================================================
# 17. FILE NAME
#
# DATE ONLY.
# ==============================================================================

file_date <-
  format(
    Pull_Date,
    "%Y%m%d"
  )


om_filename <-
  file.path(
    
    export_directory,
    
    paste0(
      "open_meteo_wisconsin_",
      file_date,
      ".csv"
    )
    
  )


# ==============================================================================
# 18. WRITE ARCHIVE
# ==============================================================================

write.csv(
  
  om_df,
  
  om_filename,
  
  row.names =
    FALSE,
  
  na =
    ""
  
)


# ==============================================================================
# 19. VERIFY FILE WAS ACTUALLY WRITTEN
# ==============================================================================

if (
  !file.exists(om_filename)
) {
  
  stop(
    "Open-Meteo file was not created: ",
    om_filename
  )
  
}


# ==============================================================================
# 20. SUMMARY
# ==============================================================================

cat("\n")
cat("============================================================\n")
cat("ARCHIVE WRITTEN SUCCESSFULLY\n")
cat("============================================================\n")

cat(
  "Open-Meteo rows: ",
  nrow(om_df),
  "\n",
  sep = ""
)

cat("\n")

cat(
  "Open-Meteo file:\n",
  om_filename,
  "\n",
  sep = ""
)


# ==============================================================================
# 21. STATION SUMMARY
# ==============================================================================

cat("\n")
cat("============================================================\n")
cat("STATION SUMMARY\n")
cat("============================================================\n")


station_summary <-
  om_df %>%
  
  group_by(
    Station_ID,
    NOAA_ID,
    Location
  ) %>%
  
  summarise(
    
    Forecast_Days =
      n(),
    
    Earliest_Target =
      as.character(
        min(Target_Date)
      ),
    
    Latest_Target =
      as.character(
        max(Target_Date)
      ),
    
    .groups =
      "drop"
    
  )


print(
  station_summary
)


# ==============================================================================
# 22. FINAL STATUS
# ==============================================================================

cat("\n")
cat("============================================================\n")
cat("PROJECT LOCK VALIDATION\n")
cat("============================================================\n")

cat("Forecast source:        Open-Meteo ONLY\n")
cat("Daily high/low fields:  DIRECTLY USED\n")
cat("Open-Meteo high:        temperature_2m_max\n")
cat("Open-Meteo low:         temperature_2m_min\n")
cat("Hourly forecasts:       NOT USED\n")
cat("Forecast window:        Days 1-10\n")
cat("Day 1:                  Tomorrow\n")
cat("Today:                  Excluded\n")
cat("Missing forecasts:      Not fabricated\n")
cat("KCWA:                   Not included\n")
cat("NOAA_ID:                Retained\n")
cat("Archive structure:      Preserved\n")
cat("============================================================\n")
cat("PIPELINE COMPLETE\n")
cat("============================================================\n")
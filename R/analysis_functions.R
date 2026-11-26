

# Acoustic Monitoring on Railways

# Analysis Functions

# Purpose: reusable functions for analysis bird and bat acoustic monitoring
# data across UK, Spain and Portugal

# Functions tested seperately before being added to this script



#------------------------------------------------------------------------------
#1. Recording effort
#------------------------------------------------------------------------------

recording_effort <- function(data, group_vars = c("Country", "device_id")) {
  required_columns <- c(
    "Country",
    "device_id",
    "recording_datetime",
    "duration_s"
  )
  missing_columns <- setdiff(required_columns, names(data))
  
  if (length(missing_columns) > 0) {
    stop(
      "Missing required columns: ",
      paste(missing_columns, collapse = ", ")
    )
  }
  recordings <- data %>%
    dplyr::distinct(
      Country,
      device_id,
      recording_datetime,
      duration_s
    )
  results <- recordings %>%
    dplyr::group_by(
      dplyr::across(dplyr::all_of(group_vars))
    ) %>%
    dplyr::summarise(
      Total_recordings = dplyr::n(),
      Total_hours = sum(duration_s) / 3600,
      .groups = "drop"
    )
  return(results)
}



#------------------------------------------------------------------------------














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
#2. Species richness
#------------------------------------------------------------------------------

species_richness <- function(data,
                             species_col,
                             group_vars = "Country") {
  required_columns <- c(group_vars, species_col)
  missing_columns <- setdiff(required_columns, names(data))
  
  if (length(missing_columns) > 0) {
    stop(
      "Missing required columns: ",
      paste(missing_columns, collapse = ", ")
    )
  }
  results <- data %>%
    dplyr::filter(!is.na(.data[[species_col]])) %>%
    dplyr::group_by(
      dplyr::across(dplyr::all_of(group_vars))
    ) %>%
    dplyr::summarise(
      Species_richness = dplyr::n_distinct(.data[[species_col]]),
      .groups = "drop"
    )
  return(results)
}



#------------------------------------------------------------------------------
#3. Species distribution
#------------------------------------------------------------------------------

species_distribution <- function(data, species_col) {
  required_columns <- c("Country", species_col)
  missing_columns <- setdiff(required_columns, names(data))
  
  if(length(missing_columns) > 0) {
    stop(
      "Missing required columns: ",
      paste(missing_columns, collapse = ", ")
    )
  }
  results <- data %>%
    dplyr::filter(
      !is.na(.data[[species_col]]),
      !is.na(Country)
    ) %>%
    dplyr::distinct(
      Country,
      Species = .data[[species_col]]
    ) %>%
    dplyr::mutate(Present = TRUE) %>%
    tidyr::pivot_wider(
      names_from = Country,
      values_from = Present,
      values_fill = FALSE
    )
  country_columns <- setdiff(names(results), "Species")
  results <- results %>% 
    dplyr::mutate(Number_of_countries = rowSums(
      dplyr::across(dplyr::all_of(country_columns))
    ))
  return(results)
}



#------------------------------------------------------------------------------
#4. Species dominance
#------------------------------------------------------------------------------

species_dominance <- function(data,
                              species_col,
                              group_vars = "Country") {
  required_columns <- c(group_vars, species_col)
  missing_columns <- setdiff(required_columns, names(data))
  
  if (length(missing_columns) > 0) {
    stop(
      "Missing required columns: ",
      paste(missing_columns, collapse = ", ")
    )
  }
  results <- data %>%
    dplyr::filter(!is.na(.data[[species_col]])) %>%
    dplyr::mutate(Species = .data[[species_col]]) %>%
    dplyr::group_by(
      dplyr::across(dplyr::all_of(c(group_vars, "Species")))
    ) %>%
    dplyr::summarise(
      Detections = dplyr::n(),
      .groups = "drop"
    )
  results <- results %>%
    dplyr::group_by(
      dplyr::across(dplyr::all_of(group_vars))
    ) %>%
    dplyr::mutate(
      Total_detections = sum(Detections),
      Percentage = (Detections / Total_detections) *100,
      Rank = dplyr::min_rank(dplyr::desc(Detections))
    ) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(
      dplyr::across(dplyr::all_of(group_vars)),
      Rank
    )
  return(results)
}



#------------------------------------------------------------------------------
# Confidence of model ID
#------------------------------------------------------------------------------

# confidence summary

confidence_summary <- function(data,
                               score_col,
                               group_vars = "Country") {
  required_columns <- c(group_vars, score_col)
  missing_columns <- setdiff(required_columns, names(data))
  
  if (length(missing_columns) > 0) {
    stop(
      "Missing required columns: ",
      paste(missing_columns, collapse = ", ")
    )
  }
  results <- data %>%
    dplyr::filter(!is.na(.data[[score_col]])) %>%
    dplyr::group_by(
      dplyr::across(dplyr::all_of(group_vars))
    ) %>%
    dplyr::summarise(
      Number_of_scores = dplyr::n(),
      Mean_score = mean(.data[[score_col]]),
      Median_score = median(.data[[score_col]]),
      SD_score = sd(.data[[score_col]]),
      Minimum_score = min(.data[[score_col]]),
      Maximum_score = max(.data[[score_col]]),
      .groups = "drop"
    )
  return(results)
}








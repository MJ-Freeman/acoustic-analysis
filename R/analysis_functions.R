

# Acoustic Monitoring on Railways

# Analysis Functions

# Purpose: reusable functions for analysis bird and bat acoustic monitoring
# data across UK, Spain and Portugal

# Functions tested seperately before being added to this script



#------------------------------------------------------------------------------
#1. Recording effort
#------------------------------------------------------------------------------

# looking at total recording instances and total hours of recordings


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

# looking at the number of unique species 


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

# looking at distribution of shared and unique species 
# across countries and devices


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

# looking at the most frequently detected species


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

# confidence summary - looking at numerical confidence scores

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



# confidence category - looking at "high", "low" 
# and "missing" categories where the models have been unable to identify 
# this category is unavailable in the Bat model output for Spain and Portugal

confidence_category <- function(data,
                                confidence_col, 
                                group_vars = "Country") {
  required_columns <- c(group_vars, confidence_col)
  missing_columns <- setdiff(required_columns, names(data))
  
  if (length(missing_columns) > 0) {
    stop(
      "Missing required columns: ",
      paste(missing_columns, collapse = ", ")
    )
  }
  results <- data %>%
    dplyr::mutate(
      Confidence_category = dplyr::if_else(
        is.na(.data[[confidence_col]]),
        "Missing",
        as.character(.data[[confidence_col]])
      )
    ) %>%
    dplyr::group_by(
      dplyr::across(
        dplyr::all_of(c(group_vars, "Confidence_category"))
      )
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
      Percentage = (Detections / Total_detections) * 100
    ) %>%
    dplyr::ungroup()
  return(results)
}



# dominant confidence - summarising confidence for most frequently detected sp.
# calculates: mean, median, sd, min and max scores 
# as well as % of detections above specified model confidence (0.5)

dominant_confidence <- function(data,
                                species_col,
                                score_col,
                                group_vars = "Country",
                                top_n = 3,
                                threshold = 0.5) {
  required_columns <- c(group_vars, species_col, score_col)
  missing_columns <- setdiff(required_columns, names(data))
  
  if (length(missing_columns) > 0) {
    stop(
      "Missing required columns: ",
      paste(missing_columns, collapse = ", ")
    )
  }
  
  dominant_species <- species_dominance(
    data,
    species_col = species_col,
    group_vars = group_vars
  ) %>%
    dplyr::filter(Rank <= top_n) %>%
    dplyr::select(
      dplyr::all_of(group_vars),
      Species,
      Rank
    )
  
  results <- data %>%
    dplyr::filter(
      !is.na(.data[[species_col]]),
      !is.na(.data[[score_col]])
    ) %>%
    dplyr::mutate(
      Species = as.character(.data[[species_col]]),
      Confidence_value = as.numeric(.data[[score_col]])
    ) %>%
    dplyr::semi_join(
      dominant_species, 
      by = c(group_vars, "Species")
    ) %>%
    dplyr::group_by(
      dplyr::across(dplyr::all_of(c(group_vars, "Species")))
    ) %>%
    dplyr::summarise(
      Total_detections = dplyr::n(),
      Mean_score = mean(Confidence_value),
      Median_score = median(Confidence_value),
      SD_score = sd(Confidence_value),
      Minimum_score = min(Confidence_value),
      Maximum_score = max(Confidence_value),
      Above_threshold = sum(Confidence_value >= threshold),
      Below_threshold = sum(Confidence_value < threshold),
      .groups = "drop")
  
  results <- results %>%
    dplyr::mutate(
      Above_percentage = (Above_threshold / Total_detections) * 100,
      Below_percentage = (Below_threshold / Total_detections) * 100
    ) %>%
    dplyr::left_join(
      dominant_species,
      by = c(group_vars, "Species")
    ) %>%
    dplyr::arrange(dplyr::across(dplyr::all_of(group_vars)),
                   Rank
    )
  return(results)
}



#------------------------------------------------------------------------------
# Hourly activity
#------------------------------------------------------------------------------

# hourly activity - looking at activity patterns and peak detection times
# optional start_hour & end_hour for specific windows (useful for bats)

hourly_activity <- function(data,
                            datetime_col,
                            group_vars = "Country",
                            start_hour = NULL,
                            end_hour = NULL) {
  required_columns <- c(group_vars, datetime_col)
  missing_columns <- setdiff(required_columns, names(data))
  
  if (length(missing_columns) > 0) {
    stop(
      "Missing required columns: ",
      paste(missing_columns, collapse = ", ")
    )
  }
  results <- data %>%
    dplyr::filter(!is.na(.data[[datetime_col]])) %>%
    dplyr::mutate(
      Hour = lubridate::hour(.data[[datetime_col]])
    )
  
  if(!is.null(start_hour) && !is.null(end_hour)) {
    if(start_hour <= end_hour) {
      results <- results %>%
        dplyr::filter(Hour >= start_hour, Hour <= end_hour)
    }else{
      results <- results %>%
        dplyr::filter(Hour >= start_hour | Hour <= end_hour)
    }
  }
  results <- results %>%
    dplyr::group_by(
      dplyr::across(dplyr::all_of(c(group_vars, "Hour")))
    ) %>%
    dplyr::summarise(
      Detections = dplyr::n(),
      .groups = "drop"
    ) %>%
    dplyr::arrange(
      dplyr::across(dplyr::all_of(group_vars)),
      Hour
    )
  return(results)
}


#dominant activity



























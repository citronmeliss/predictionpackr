#' Add combined Glascow Coma Scale function
#' This function adds a new column for combined GCS using the motoric, verbal and eye components of GSC.
#' @param study_data The study data frame.
#' @export

AddGCS <- function(study_data) {

  # Create new column
  study_data <- study_data %>%
    mutate(gcs = mgcs + vgcs + egcs)

  # Drop component variables
  study_data$mgcs <- NULL
  study_data$vgcs <- NULL
  study_data$egcs <- NULL

  return(study_data)

}

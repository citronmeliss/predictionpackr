#' Add iss15 function
#' This function uses iss to create a binary column of iss > 15
#' @param study_data The study dataframe.
#' @export
#'

AddISS <- function(study_data) {

  # Change iss to numeric
  study_data$iss <- as.numeric(as.character(study_data$iss))

  # Add column iss15 for alla observations with iss > 15
  study_data$iss15 <- ifelse(study_data$iss >= 15, 1,0)

  # Fix missing values
  study_data$iss15 <- with(study_data, ifelse(is.na(iss), 0, study_data$iss15)) # if missing in iss, also missing in iss15

  # Drop iss column
  study_data$iss <- NULL

  return(study_data)
}

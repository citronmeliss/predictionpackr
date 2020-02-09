#' Apply exclusion criteria function
#'
#' This function applies the study exclusion criteria and saves the results into
#' the parent environments results list
#' @param study_data The study data as a data frame. No default
#' @export
ApplyExclusionCriteria <- function(
  study_data
)
{
  ## Save number of patients enrolled during study period
  results$n_enrolled <<- nrow(study_data)
  ## Exclude observations with informed consent no
  study_data <- study_data[study_data$ic == "Yes", ]
  ## Drop ic column from data
  study_data$ic <- NULL
  ## Save number of observations remaining after those who did not provide
  ## informed consent were excluded
  results$n_after_excluding_no_consent <<- nrow(study_data)
  ## Exclude observations with missing clinicians' priority data
  study_data <- study_data[!is.na(study_data$tc), ]
  ## Save number of patients remaining after excluding those with missing
  ## clinicians' priority data
  results$n_after_excluding_missing_priority <<- nrow(study_data)

  ## Exclude observations with missing outcome e.g. RUN A COMPLETE CASE STUDY
  # study_data <- study_data[!is.na(study_data$s24h), ]
  # study_data <- study_data[!is.na(study_data$icu48h), ]

  study_data <- study_data[complete.cases(study_data),]

  ## Save number of patients remaining after excluding those with missing
  ## outcome
  results$n_after_excluding_incomplete_cases <<- nrow(study_data)

  return(study_data)
}

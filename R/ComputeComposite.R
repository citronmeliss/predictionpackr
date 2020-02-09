#' Create composite outcome function
#' This function creates a new binary variable composite outcome
#' @param study_data The study data frame.
#' @export

CreateComposite <- function(study_data) {

  study_data <- study_data %>%
    mutate(composite = ifelse(icu48h == 1 | s24h == "Yes" | majors24h == 1 | iss15 == 1, 1, 0))

}

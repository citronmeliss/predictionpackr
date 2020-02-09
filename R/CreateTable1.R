#' Create Table 1 function
#' This function creates a table of descriptive statistics for the patient cohort.
#' @param study_data The study data frame.
#' @export

CreateTable1 <- function(study_data) {

  # Define the variables to include
  listVars <- c("age", "sex", "moi", "sbp", "hr", "rr", "avpu", "gcs", "nsi",
                "s24h", "icu48h", "majors24h", "iss15", "composite")

  # Define which of the variables above are categorical
  catVars <- c("sex", "moi", "avpu",
               "s24h", "icu48h", "majors24h", "iss15", "composite")

  # Use the function CreateTableOne()
  table1 <- CreateTableOne(vars = listVars, data = study_data, factorVars = catVars, includeNA = FALSE)

  # Save as data frame and change
  # to non-normal distribution to measure in median[IQR]
  table1 <- print(table1, nonnormal = TRUE)

  # Change to data frame
  table1 <- as.data.frame(table1)

  return(table1)
}

#' Prepare study data function
#'
#' Prepares the study data using the data dictionary
#' @param study_data The study data frame. No default.
#' @param data_dictionary The data dictionary object. No default.
#' @export

PrepareStudyData <- function(
  study_data,
  data_dictionary
)
{
  ## Error handling
  if (!is.data.frame(study_data)) stop ("Study data has to be a data frame")
  ## If seqn in dataset, remove seqn to later bind it to the dataframe
  seqn_in_data <- FALSE
  if ("seqn" %in% names(study_data)) {
    seqn <- study_data$seqn # Get seqn as object
    seqn[is.na(seqn)] <- 999 # seqn = 999 should not be NA
    study_data$seqn <- NULL # Remove from study_data
    seqn_in_data <- TRUE
  }
  ## Prepare study data using the data dictionary
  study_data[] <- lapply(names(study_data), function(n) {
    vdd <- data_dictionary[[n]] # Get variable specific data dictionary and assign that to vdd
    data <- study_data[, n]
    if (vdd$t == "qual") {
      values <- vdd$vs # Get values
      if (values != ""){
        data <- as.factor(data)
        value_labels <- SplitLables(vdd$vls) # Get value labels
        value_label_list <- lapply(strsplit(value_labels, "="), trimws, "both") # Make a list of value labels without white space
        labels <- unlist(lapply(value_label_list, function(x) x[2])) # Get labels
        levels(data) <- labels # Assign those to data
      }
    }
    return(data)
  })
  ## If seqn is in study data, add seqn again
  if (seqn_in_data) study_data <- data.frame(study_data, seqn = seqn)

  return(study_data)
}

#' Split data function
#'
#' This function sets the data for analysis, i.e. splits into to two datasets for training and test.
#' @param cc_df Data frame. The complete case data. No default.
#' @param for_tbl_one Logic. If TRUE, merged table is return with strata variable "set". Defaults to TRUE.
#' @export

SplitData<- function(
  cc_df,
  for_tbl_one = FALSE
)
{
  ## Error handling
  if (!is.data.frame(cc_df)) stop("The study_data is not of data frame type.")

  n_events <- sum(cc_df$composite)
  cc_df <- cc_df[order(-cc_df$composite, cc_df$seqn), ] # Order by mortality and sequential int
  seqn_when_50 <- cc_df$seqn[100] # Seqn when 100 patients had died
  seqn_when_100 <- cc_df$seqn[200] # Seqn when 100 patients had died
  train <- cc_df[cc_df$seqn <= seqn_when_100, ] # For training
  test <- cc_df[cc_df$seqn > seqn_when_100 & cc_df$seqn <= seqn_when_200, ] # For testing

  ## Listify training and test set
  return_object <- list(train = train,
                        test = test)
  if (for_tbl_one) {
    train_w_strata <- cbind(train, set = rep("Gridsearch", nrow(train)))  # Add strata
    test_w_strata <- cbind(test, set = rep("Comparison", nrow(test)))     # Add strata
    merged <- rbind(train_w_strata, test_w_strata)                        # Merge tables
    merged$composite <- factor(merged$composite)                          # Coerce s30d to factor
    levels(merged$composite) <- c("No", "Yes")                            # And change levels
    return_object$merged <- merged                                        # Add to return
  }

  return (return_object)
}

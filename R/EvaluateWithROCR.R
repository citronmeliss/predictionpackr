#' Evaluate With Rocr
#'
#' Evaluate the classifier performance using ROCR.
#' @param predictions Numeric vector. Model predictions. No default.
#' @param outcome Numeric vector. The outcome of interest. No default.
#' @param measure Character vector of length 1. The ROCR measure to estimate. Defaults to "auc".
#' @param reverse Logical vector of length 1. If TRUE the functions returns the 1 - AUC rather than AUC. Defaults to FALSE.
#' @export
EvaluateWithRocr <- function(predictions, outcome, measure = "auc",
                             reverse = FALSE) {
  ## Error handling
  pred.rocr <- ROCR::prediction(predictions = predictions, labels = outcome)
  rocr.estimate <- ROCR::performance(pred.rocr, measure = measure, x.measure =  'cutoff')@y.values[[1]]
  if (reverse == TRUE)
    rocr.estimate <- 1 - rocr.estimate
  return (rocr.estimate)
}

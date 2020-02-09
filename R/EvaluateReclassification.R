#' EvaluateReclassification
#'
#' Calculates the NRI.
#' @param current.model.predictions Numeric vector. The risk groups that the current model predicted for subjects. No default.
#' @param new.model.predictions Numeric vector. The risk groups that the new model predicted for subjects. No default.
#' @param outcome.vector Numeric vector. The outcome of the subjects. No default.
#' @param reverse Logical. If TRUE model risk group levels are reversed, i.e. levels 1,2,3,4 are reversed to 4,3,2,1 - respectively. Defaults to FALSE.
#' @export
EvaluateReclassification <- function(current.model.predictions, new.model.predictions, outcome.vector,
                                     reverse = FALSE) {
  if (reverse) {
    preds.factor <- forcats::fct_rev(factor(new.model.predictions))
    new.model.predictions <- as.numeric(as.character(preds.factor))
  }
  log <- suppressMessages(capture.output({
    reclassification.estimates <- nricens::nribin(event = outcome.vector,
                                                  p.std = current.model.predictions,
                                                  p.new = new.model.predictions,
                                                  cut = sort(unique(current.model.predictions)),
                                                  niter = 0,
                                                  msg = FALSE)
  }))
  return (reclassification.estimates$nri)
}

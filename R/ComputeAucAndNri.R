#' Compute Auc And Nri
#'
#' Fit the model to the training data, predict on the test data, and evaluate the predictions.
#' @param data Data frame. The study data. No default.
#' @param indices Allows the boot function to pick indices.
#' @param boot Logical. If TRUE the sample is treated as a bootstrap sample. Then, indices are used by the boot package to resample the sample, and progress is logged as a bootstrap. Defaults to FALSE
#' @param log Logical. If TRUE progress is logged to a logfile. Defaults to TRUE
#' @param clean.start Logical. If TRUE logfile is removed and new information is logged. If FALSE information is appended to the logfile. Defaults to FALSE.
#' @export
ComputeAucAndNri <- function(data, indices, boot = FALSE,
                             log = TRUE, clean.start = TRUE,
                             ...) {
  ## Error handling
  if (!is.data.frame(data))
    stop ("data must be of type data frame")
  if (!IsLength1(boot) | !is.logical(boot))
    stop ("boot must be of a logical vector of length 1")
  d <- data
  if (boot)
    d <- data[indices, ]     ## Allow the boot function to pick indices
  ## Use GenerateModelPredictions instead
  predictions.outcome.and.tc <- GenerateModelPredictions(data = d, log = log,
                                                         boot = boot, clean_start = clean.start,
                                                         ...)
  ## Try with prediction models
  model.names <- c("GAP_CON", "KTS_CON", "gerdin_CON", "RTS_CON",
                   "GAP_CUT", "KTS_CUT", "gerdin_CUT", "RTS_CUT")
  ## Get those models whose AUCs are to be "reversed", i.e. 1 - AUC
  scores.to.reverse <- model.names[!grepl("gerdin", model.names)]
  model.aucs <- sapply(model.names, function(name) {
    reverse <- ifelse(name %in% scores.to.reverse, TRUE, FALSE)
    EvaluateWithRocr(predictions.outcome.and.tc[[name]],
                     outcome = predictions.outcome.and.tc[["composite"]],
                     reverse = reverse)
  })
  ## Get clinicians.auc
  clinicians.auc <- setNames(EvaluateWithRocr(predictions = predictions.outcome.and.tc$tc,
                                              outcome = predictions.outcome.and.tc$composite),
                             nm = "clinician.auc")

  ## Compare model auc to clinician auc
  model.clinicians.diff.labels <- paste0(model.names, ".diff.auc")
  model.clinician.difference <- setNames(model.aucs - clinicians.auc, model.clinicians.diff.labels)
  ## Check if model performance is worse with binning
  con.cat.auc.difference <- model.aucs[grepl("CON", model.names)] - model.aucs[grepl("CUT", model.names)]
  con.cat.auc.difference <- setNames(c(con.cat.auc.difference, -con.cat.auc.difference),
                                     nm = paste0(sub("\\_.*", "", model.names),
                                                 c(".con.cat.diff.auc", ".cat.con.diff.auc")))
  ## Compile aucs to one vector
  auc.vector <- c(model.aucs, clinicians.auc, model.clinician.difference, con.cat.auc.difference)
  ## Evaluate nri
  nri <- lapply(setNames(nm = grep("CUT", model.names, value = TRUE)), function(name) {
    reverse <- ifelse(name %in% grep("CUT", scores.to.reverse, value = TRUE), TRUE, FALSE)
    EvaluateReclassification(current.model.predictions = predictions.outcome.and.tc$tc,
                             new.model.predictions = predictions.outcome.and.tc[[name]],
                             outcome.vector = predictions.outcome.and.tc$composite,
                             reverse = reverse)
  })
  relevant.nris <- unlist(lapply(nri, function(nri.df)
    unlist(list(nri.plus = nri.df["NRI+", ], nri.minus = nri.df["NRI-", ]))))
  ## As vector of estimates for the boot package
  relevant.estimates <- c(auc.vector, relevant.nris)
  timestamp <- Sys.time()
  if (log) {
    analysis_name <- "Main"
    if (boot)
      analysis_name <- "Bootstrap"
    logline <- paste0(analysis_name, " analysis completed on ", timestamp)
    append <- ifelse(clean.start, FALSE, TRUE)
    write(logline, "logfile", append = append)
  }
  return (relevant.estimates)
}

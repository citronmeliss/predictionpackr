#' Generate model predictions function
#'
#' This function bins model predictions and converts them from character labels to numeric labels.
#' @param data The study data frame. No default
#' @param n_cores Number of cores to be used in parallel gridsearch. Passed to bin.models (which, in turn, passes to SupaLarna::gridsearch.breaks). As integer. Defaults to 2 (in gridsearch.breaks)
#' @param return_cps Logical. Function returns model cut_points if TRUE. Passed to bin.models. Defaults to TRUE.
#' @param log Logical. If TRUE progress is logged in logfile. Defaults to FALSE.
#' @param boot Logical. Affects only what is printed to logfile. If TRUE prepped_sample is assumed to be a bootstrap sample. Defaults to FALSE.
#' @param write_to_disk Logical. If TRUE the prediction data is saved as RDS to disk. Defaults to FALSE.
#' @param clean_start Logical. If TRUE the predictions directory and all files in it are removed before saving new stuff there. Defaults to FALSE.
#' @param gridsearch_parallel Logical. Passed to bin.models (which, in turn, passes to SupaLarnas gridsearch.breaks). If TRUE the gridsearch is performed in parallel. Defaults to FALSE.
#' @param is_sample Logical. Passed to bin.models. If TRUE, only a tenth of possible cut points is searched. Defaults to TRUE.
#' @param clean_start Logical. If TRUE the predictions directory and all files in it are removed before saving new stuff there. Defaults to FALSE.
#' @export
GenerateModelPredictions <- function(
  data,
  n_cores,
  return_cps = FALSE,
  log = FALSE,
  boot = FALSE,
  write_to_disk = FALSE,
  clean_start = FALSE,
  gridsearch_parallel = FALSE,
  is_sample = TRUE
)
{
  ## Split data
  split_data <- SplitDataset(study.sample = data,
                             events = NULL,
                             event.variable.name = NULL,
                             event.level = NULL,
                             split.proportions = c(0.5, 0.5),
                             remove.missing = FALSE,
                             sample.names = NULL,
                             return.data.frame = FALSE)

  ##split_data <- SplitData(data, for_tbl_one = FALSE)

  ## Extract outcome for gridsearch
  dataset_outcomes <- list(y_train =  split_data$training$composite,
                           y_test =  split_data$test$composite)

  ## Extract tc from test dataset and bind test outcome;
  ## may be extended for test-training model comparison
  tc_and_outcome <- list(tc = as.numeric(split_data$test$tc),
                         composite = split_data$test$composite)
  ## Define model_names
  model_names <- c("RTS", "GAP", "KTS", "gerdin")

  ## Define suffixes for later predictions
  suffixes <- setNames(nm = c("_CUT", "_CON"))

  ## Define model settings for gridsearch
  model_settings <- list(model_steps = setNames(as.list(c(0.5, 1, 1, 0.01)), # Steps for grid
                                                nm = model_names),
                         model_optimise = setNames(as.list(c(FALSE, FALSE, FALSE, TRUE)),
                                                   nm =  model_names))
  FirstUp <- function(x) {
    substr(x, 1, 1) <- toupper(substr(x, 1, 1))
    x
  }
  ## Generate predictions with models
  pred_data <- unlist(lapply(model_names, function(model_name){
    ## Get function from string
    model_func <- get(paste0("Model", FirstUp(model_name)))
    ## Make predictions on test and training set
    predictions <- lapply(setNames(nm = names(split_data)), function(train_or_test)
      model_func(split_data[[train_or_test]]))
    ## Define steps
    step <- model_settings$model_steps[[model_name]]
    ## Define grid with the predictions on the training set; for gridsearch
    grid <-  seq(min(predictions$training), max(predictions$test), by = step)
    ## Get the optimise setting corresponding to the model
    optimise <- model_settings$model_optimise[[model_name]]
    ## Gridsearch on traininging set, and bin predictions on test set
    binned_preds <- BinModels(predictions = predictions,
                              outcome = dataset_outcomes$y_train,
                              grid = grid,
                              n_cores = n_cores,
                              return_cps = return_cps,
                              gridsearch_parallel = gridsearch_parallel,
                              is_sample = is_sample,
                              maximise = optimise)
    ## Convert to numeric predictions
    binned_predictions <- lapply(binned_preds, function(preds){
      levels(preds) <- as.character(1:4)
      preds <- as.numeric(preds)
    })
    ## List prediction data, i.e. the continous predictions and binned predictions on test set
    pred_data <- setNames(list(binned_predictions$test,
                               predictions$test),
                          nm = sapply(suffixes, function(suffix)
                            paste0(model_name, suffix)))
    return (pred_data)}), recursive = FALSE)
  ## Bind outcome and tc to prediction data
  pred_data <- c(pred_data, tc_and_outcome)
  ## Write each prediction to disk
  if (write_to_disk) {
    ## Define timestamp
    timestamp <- Sys.time()
    if (!dir.exists("predictions"))
      dir.create("predictions")
    filenum <- as.character(round(as.numeric(timestamp)*1000))
    file_name_ext <- "main"
    if (boot)
      file_name_ext <- "boot"
    file_name <- paste0("./predictions/model_predictions_", file_name_ext, "_", filenum, ".rds")
    saveRDS(pred_data, file_name)
  }
  ## Return pred_data and cut_points
  return (pred_data)
}

#' Bootstrap Statistics
#'
#' Wrapper for boot::boot. For the log and clean.start arguments.
#' @param f Function. A function for the statistic argument in boot::boot. No default.
#' @param log Logical. If TRUE progress is logged to a logfile. Defaults to TRUE
#' @param clean.start Logical. If TRUE the predictions directory and all files in it are removed before saving new stuff there. Defaults to FALSE.
#' @param ... Arguments for f.
#' @export
BootstrapStatistics <- function(f, log = TRUE, clean.start = TRUE, ...) {
  if (log)
    if (file.exists("logfile")) {
      file.remove("logfile")
    }
  ## Define dir_name for write_to_disk
  if (clean.start) {
    if (dir.exists("predictions"))
      unlink("predictions", recursive = TRUE)
  }
  statistics <- boot::boot(statistic = f, log = log,
                           clean.start = clean.start,
                           ...)
  return (statistics)
}

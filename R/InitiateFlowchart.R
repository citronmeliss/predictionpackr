#' Initiate Flowchart
#'
#' Wrapper for the bengaltiger::CreateFlowchart to make the code cleaner
#' @param results List. The results list. No default.
#' @param ... Args for the bengaltiger::CreateFlowchart.
#' @export
InitiateFlowchart <- function(results, ...) {
  text <- c("patients were enrolled in this study",
            "patients did not give informed consent",
            "patients gave informed consent",
            "patients had missing clinical triage data",
            "patients had complete clinical triage data",
            "patients were incomplete cases",
            "patients were complete cases and included in final analyses")
  ns <- unlist(results)
  ## Rolling differences
  differences <- abs(diff(ns))
  inclusions.and.exclusions <- unlist(lapply(seq_along(ns), function(i) {
    if (i == length(ns))
      return (ns[i])
    else
      return (list(ns[i], differences[i]))
  }))
  flowchart <- CreateFlowchart(flowchart.elements = mapply(paste, inclusions.and.exclusions, text),
                               print.tikz = FALSE,
                               read.from.results = FALSE,
                               ...)
  return (flowchart)
}

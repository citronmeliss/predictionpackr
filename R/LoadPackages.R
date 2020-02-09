#' The Load Packages Function
#'
#' This function loads commonly used packages into your library.
#' @export

load.packages <- function(){
  ## List packages commonly used
  packages <- c("devtools",
                "roxygen2",
                "tidyverse")
  ## Load packages
  for (package in packages) require(package,
                                    character.only = TRUE)
  ## Print out the loaded packages
  paste("You have loaded", packages, "to your library.")
}

#' The Import Data Function
#'
#' This function imports study data from a csv file.
#' @param file.name is the name of your file while path is the path to reach it.
#' Don't forget to enclose the names as character strings in " ".
#' @export

ImportData <- function(file.name, path){
  file.path <- paste0(path, file.name)
  data <- read.csv(file.path, header = TRUE, sep = ";")
  return(data)
}

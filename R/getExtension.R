#' getExtension
#'
#' Get the file extension of a given file.
#' This function takes a file path as input and returns the file extension without the dot.
#'
#' @param file A character string containing the file path.
#'
#' @return A character vector containing the file extension without the dot.
#' @examples
#' getExtension("example.txt")
#'
#' @export

getExtension <- function(file){
  ex <- strsplit(basename(file), split="\\.")[[1]]
  return(ex[-1])
}

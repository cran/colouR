#' read_image_from_url
#'
#' Read either a PNG or JPG file from a URL. This function reads an
#' image file (PNG or JPG) from a URL and returns the image data.
#'
#' @param path A character string representing the URL of the image file.
#'
#' @return An object containing the image data. If the image is a JPG, the object will be of class "array".
#' If the image is a PNG, the object will be of class "matrix".
#'
#' @importFrom httr GET
#' @importFrom httr write_disk
#' @importFrom jpeg readJPEG
#' @importFrom png readPNG
#'
#' @export

read_image_from_url <- function(path) {
  # Get the file extension
  file_ext <- getExtension(file = path)

  # Read the image from the URL
  response <- httr::GET(path, httr::write_disk(tf <- tempfile(fileext = paste0(".", file_ext))))
  on.exit(unlink(tf))

  if (file_ext == "jpg" || file_ext == "jpeg") {
    image <- jpeg::readJPEG(tf)
  } else if (file_ext == "png") {
    image <- png::readPNG(tf)
  } else {
    stop("Unsupported file format. Only PNG and JPG files are supported.")
  }

  return(image)
}

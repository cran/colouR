#' Create a Colour Palette from an Image
#'
#' This function generates a colour palette based on the most prominent colors in an image..
#'
#' @param path Character string. The path to the image file.
#' @param n Integer. The number of most frequent colours in input image to consider.
#' @param avgCols Logical. Whether to average colours within clusters.
#' @param exclude Character vector. List of colours in HEX format to exclude from the palette.
#' @param n_clusters Integer. The number of clusters for colour grouping.
#' @param customExclude Character vector. Optional vector of custom color codes in HEX format to be excluded.
#'
#' @return A colour palette generated from input image.
#'
#' @examples
#' pal<-img2pal(path = "https://raw.githubusercontent.com/AlanInglis/colouR/master/images/bender.png",
#'              n = 10,
#'              avgCols = TRUE,
#'              exclude = TRUE,
#'              n_clusters = 15,
#'              customExclude = NULL)
#' pal
#'
#' @export

img2pal <- function(path,
                    n = NULL,
                    avgCols = FALSE,
                    exclude = FALSE,
                    n_clusters = NULL,
                    customExclude = NULL) {

  # Get colours from image
  suppressWarnings(
    colours <- colouR::getTopCol(path = path,
                                 n = n,
                                 avgCols = avgCols,
                                 exclude = exclude,
                                 n_clusters = n_clusters,
                                 customExclude = customExclude)
  )


  # Display palette
  if (avgCols == TRUE) {
    pal <-  colours$avg_color
    colpal <- colouR::plotPalette(colours, "avg_color")
  } else {
    pal <-  colours$hex
    colpal <- colouR::plotPalette(colours, "hex")
  }

  print(colpal)
 # myList <- list(pal = pal, palGraphic = colpal)
  return(pal = pal)
}

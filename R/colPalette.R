#' colPalette
#'
#' Create a Color Palette. This function generates a custom
#' color palette based on the specified 'palette' name.
#' The color palettes are sourced from two
#' predefined lists: 'taylor_palettes' and 'radiohead_palettes'.
#'
#' @param palette A character string specifying the name of the colour palette.
#' The palette must be present in either 'taylor_palettes' or 'radiohead_palettes'.
#'
#' @return A colorRampPalette function with the specified colour palette.
#'
#' @importFrom grDevices colorRampPalette
#'
#' @examples
#' my_palette <- colPalette(palette = "evermore")
#' colours <- my_palette(5)
#' print(colors)
#' @export

colPalette <- function(palette){

  if(palette %in% names(taylor_palettes)){
    pal <- taylor_palettes[[palette]]
  }else if(palette %in% names(radiohead_palettes)){
    pal <- radiohead_palettes[[palette]]
  }

  grDevices::colorRampPalette(pal)
}



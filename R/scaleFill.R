#' scaleFill
#'
#' Create a Custom Fill Scale for ggplot2. This function creates a
#' custom fill scale for ggplot2 plots based on the specified
#' 'palette' name. The color palettes are sourced from two
#' predefined lists: 'taylor_palettes' and 'radiohead_palettes'.
#' The function can create either a discrete or continuous fill scale,
#' depending on the 'discrete' parameter.
#' Additional arguments passed to the function are forwarded to the corresponding ggplot2 scale function.
#'
#' @param palette A character string specifying the name of the color palette.
#' The palette must be present in either 'taylor_palettes' or 'radiohead_palettes'.
#' @param discrete A logical value indicating whether to create a discrete (TRUE)
#'  or continuous (FALSE) fill scale. Default is TRUE.
#' @param ... Additional arguments to be passed to the ggplot2 scale function.
#'
#' @return A ggplot2 fill scale based on the specified color palette.
#'
#' @export


scaleFill <- function(palette,
                      discrete = TRUE,
                      ...){

  if(palette %in% names(taylor_palettes)){
    pal <- colPalette(palette = palette)
    nam <- 'TaylorSwift_'
  }else if(palette %in% names(radiohead_palettes)){
    pal <- colPalette(palette = palette)
    nam <- 'radiohead_'
  }


  if(discrete){
    ggplot2::discrete_scale("fill", paste0(nam, palette), palette = pal, ...)
  }else{
    ggplot2::scale_fill_gradientn(colours = pal(256),...)
  }
}

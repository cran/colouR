#' plotPalette
#'
#' Plot a color palette from a data frame.
#' This function takes a vector of colours or a
#' data frame with a column of colors and plots the colors as a color palette.
#'
#' @param df A vector of colours or a data frame containing a column with colors in hexadecimal format.
#' @param color_col A character string representing the name of the column containing the colors.
#'
#' @return A ggplot2 plot object displaying the color palette.
#'
#' @importFrom dplyr mutate
#' @import ggplot2
#'
#' @examples
#' # Create a sample data frame with colors
#' colors_df <- data.frame(
#'   color = c("#FF0000", "#00FF00", "#0000FF", "#FFFF00", "#FF00FF")
#' )
#'
#' # Plot the color palette with default bar height
#' plotPalette(colors_df, "color")
#'
#' @export


plotPalette <- function(df, color_col) {

  # Declare golbal vars
  index <- NULL

  cc <- class(df)

  if(cc == 'character'){
    df <- data.frame(cols = df)
    color_col <- 'cols'
  }

  # Ensure that the provided color_col exists in the data frame
  if (color_col %in% colnames(df)) {

    # Create a new data frame with an index column
    colIdx <- grep(color_col, colnames(df))


    df <- data.frame(col = df[,colIdx],
                     index = c(1:length(df[,colIdx])))

    # Plot the color palette using ggplot2
    ggplot(df, aes(x = index, y = 1, fill = col)) +
      geom_tile(height = 0.5) +
      scale_fill_manual(values=as.character(df$col)) +
      scale_x_continuous(expand = c(0, 0)) +
      scale_y_continuous(expand = c(0, 0)) +
      coord_cartesian(ylim = c(0, 1/0.5)) +
      theme_void() +
      theme(axis.text = element_blank(),
            axis.title = element_blank(),
            axis.ticks = element_blank(),
            legend.position = "none")
  } else {
    stop("The specified color column does not exist in the data frame.")
  }
}


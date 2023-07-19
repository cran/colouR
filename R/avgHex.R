#' avgHex
#'
#' Average Hex Colors per Group. This function takes a data frame
#' with two columns: one for the hex color values and another for
#' the group labels. It calculates the average color for each
#' group and returns a data frame with the group labels and their
#' corresponding average hex colors.
#'
#' @param df A data frame with columns for hex color values and group labels.
#' @param hex_col The name of the column containing hex color values. Default is "hex".
#' @param group_col The name of the column containing group labels. Default is "group".
#'
#' @return A data frame with the group labels and their corresponding average hex colors.
#'
#' @importFrom grDevices col2rgb
#' @importFrom grDevices rgb
#' @importFrom stats aggregate
#'
#' @examples
#' df <- data.frame(
#'   hex = c("#FF0000", "#00FF00", "#0000FF", "#FFFF00", "#FF00FF"),
#'   group = c(1, 1, 2, 2, 3)
#' )
#'
#' avgHex(df, hex_col = "hex", group_col = "group")
#' @export

avgHex <- function(df, hex_col, group_col){

  # Check if the specified columns exist in the data frame
  if (!(hex_col %in% colnames(df)) || !(group_col %in% colnames(df))) {
    stop("The specified hex or group column does not exist in the data frame.")
  }

  # Sum the number of rows within each group
  group_sums <- aggregate(df[[hex_col]] ~ df[[group_col]], data = df, FUN = length)

  # Rename
  colnames(group_sums) <- c("group", "row_sum")

  # Function to average hex colors
  average_hex_colors <- function(hex_colors) {
    # Convert the hex colors to RGB
    rgb_colors <- col2rgb(hex_colors)

    # Calculate the mean of each color channel (R, G, and B)
    mean_rgb <- rowMeans(rgb_colors)

    # Convert the mean RGB values to a single hex color
    mean_hex <- rgb(mean_rgb[1], mean_rgb[2], mean_rgb[3], maxColorValue = 255)

    return(mean_hex)
  }

  # Calculate the average color for each group
  avg_colors <- tapply(df[[hex_col]], df[[group_col]], average_hex_colors)

  # Convert the result into a data frame
  avg_colors_df <- data.frame(
    group = names(avg_colors),
    avg_color = unname(avg_colors),
    freq = group_sums$row_sum,
    stringsAsFactors = FALSE
  )

  return(avg_colors_df)
}

#' groupCols
#'
#' Group Hex Colors. This function takes a vector of hex color values
#' and groups them using k-means clustering in the RGB color space.
#' It returns a data frame with the original hex colors and their assigned group labels.
#'
#' @param hex_colors A character vector of hex color values.
#' @param n_clusters The number of clusters (groups) to use in the k-means clustering. Default is 5.
#'
#' @return A data frame with the original hex colors and their assigned group labels.
#'
#' @importFrom grDevices col2rgb
#' @importFrom stats kmeans
#'
#'
#'
#' @examples
#' hex_colors <- c("#FF0000", "#00FF00", "#0000FF", "#FFFF00", "#FF00FF")
#' groupCols(hex_colors, n_clusters = 2)
#' @export

groupCols <- function(hex_colors, n_clusters = 5){

  # Convert the hex colors to RGB
  rgb_colors <- col2rgb(hex_colors)

  # Transpose the RGB matrix and normalize the values to [0, 1]
  rgb_colors <- t(rgb_colors) / 255

  # Apply k-means clustering to the RGB colors
  clust_result <- kmeans(rgb_colors, centers = n_clusters)

  # Assign the cluster labels to the original hex colors
  grouped_colors <- data.frame(hex_color = hex_colors, group = clust_result$cluster)

  # Sort colours by group
  grouped_colors <- grouped_colors[order(grouped_colors$group, decreasing = TRUE), ]

  return(grouped_colors)
}


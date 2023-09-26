#' getTopCol
#'
#' Get top colors from an image. This function reads an image file, extracts the colors,
#' and returns the top n colors based on their frequency in the image.
#' Optionally, black and white shades can be excluded, and the colors can be grouped and averaged.
#'
#' @param path Character, the path to the image file (either jpg or png).
#' @param n Integer, the number of top colors to return. If NULL (default), return all colors.
#' @param exclude Logical, whether to exclude black and white shades. Default is TRUE.
#' @param sig Integer, the number of decimal places for the color percentage. Default is 4.
#' @param avgCols Logical, whether to average the colors by groups. Default is TRUE.
#' @param n_clusters Integer, the number of clusters to use for grouping colors. Default is 5.
#' @param customExclude Character vector. Optional vector of custom color codes in HEX format to be excluded.
#'
#' @return A data frame with the top colors, their frequency, and percentage in the image.
#'
#' @importFrom pixmap pixmapRGB
#' @importFrom jpeg readJPEG
#' @importFrom png readPNG
#' @importFrom grDevices rgb
#'
#' @export

getTopCol <- function(path,
                      n = NULL,
                      exclude = TRUE,
                      sig = 4,
                      avgCols = TRUE,
                      n_clusters = 5,
                      customExclude = NULL) {

  # Check if url
  isURL <- function(path){
    ex <- strsplit((path), split="\\:")[[1]]
    ex[-2]
  }

  checkURL <- isURL(path = path)


  if(checkURL == 'http' || checkURL == 'https'){
    image <- read_image_from_url(path)
  }else{
    file_ext <- getExtension(file = path)
    # read either jpg or png
    if (file_ext == "jpg" || file_ext == "jpeg") {
      image <- jpeg::readJPEG(path)
    } else if (file_ext == "png") {
      image <- png::readPNG(path)
    } else {
      stop("Unsupported file format. Only PNG and JPG files are supported.")
    }

  }

    pic <-  suppressWarnings(
      pixmap::pixmapRGB(image)
    )

  # get the RGB colours
  colours <- grDevices::rgb(pic@red, pic@green, pic@blue)

  # Create a sorted frequency table
  val_frequency <- sort(table(colours), decreasing = TRUE)

  # Create a data frame with the sorted frequency table
  df_col <- data.frame(
    hex = names(val_frequency),
    freq = as.vector(val_frequency)
  )

  # percentage of colour in image
  df_col$col_percent <- (df_col$freq / sum(df_col$freq)) * 100

  # round percentage
  df_col$col_percent <-  round(df_col$col_percent, sig)

  # exclude black and white shades
  if(exclude){
    df_col <- excludeCols(df_col, customExclude = customExclude)
  }

  # Average over colours
  if(avgCols){
    # First group the colours
    cols_grouped <- groupCols(df_col$hex,  n_clusters = n_clusters)
    df_col$group <- cols_grouped$group

    # Average colours
    df_col <- avgHex(df = df_col, hex_col = 'hex', group_col = 'group')

    # Remove group column
    df_col$group <- NULL

    df_col <- df_col[order(df_col$freq, decreasing = TRUE), ]
  }



  # Get the top n colors
  if(!is.null(n)){
    # Check if the number of items to return is less than number of rows
    if (nrow(df_col) < n) {
      stop("The the number of colors to return (n), must be less than number of clusters (n_cluster)")
    }
    top_n_colors <- df_col[1:n, ]
  }else{
    top_n_colors <- df_col
  }




  return(top_n_colors)
}



#' excludeCols
#' @description  A non-exhaustive list of white and black colour shades. For use when setting \code{exclude = TRUE}
#' in the \code{getTopCol} function. Setting  \code{exclude = TRUE} when calling the \code{getTopCol}
#' function will exclude the colours form the results.
#' @param data Data frame of colours
#' @param customExclude Character vector. Optional vector of custom color codes in HEX format to be excluded.
#' @return No return value, called for side effects.
#'
#' @export
excludeCols <- function(data, customExclude = NULL){

  # Vector of black shades
  blacks <- c(
    "#010101",
             "#000000",
             "#010001",
             "#010100",
             "#000001",
             "#010000",
             "#020101",
             "#000101",
             "#020201",
             "#010102",
             "#010201",
             "#000100",
             "#020202",
             "#020102",
             "#1D1916",
             "#1E1A19",
             "#1D1918",
             "#0F0400"
  )

  # Vector of white shades
  whites <- c(
    "#FCFCFC",
             "#FBFBFD",
             "#FAFAFC",
             "#F9F9FB",
             "#F8F9FB",
             "#FBFBFB",
             "#F9FAFC",
             "#F7F8FA",
             "#F8F8FA",
             "#F7F7F9",
             "#FAFBFD",
             "#F6F6F8",
             "#F6F7F9",
             "#F5F6F8",
             "#FCFCFE",
             "#F5F5F7",
             "#FAFAFA",
             "#FFFFFF",
             "#FEFEFE",
             "#FEFEFF",
             "#FDFFFE",
             "#FDFEFF",
             "#FDFDFD",
             "#FCFDFF",
             "#FBFCFE",
             "#F8FCFD",
             "#FCFEFD",
             "#FDFDFF",
             "#F9FDFE",
             "#F8FCFF",
             "#F7FBFE",
             "#FFFDFE",
             "#F5FAFD",
             "#FEFFFF",
             "#FFFFFE"
  )

    # Add custom colors to the exclusion list
    if (!is.null(customExclude)) {
      blacks <- c(blacks, customExclude)
    }

    colIdxBlack <- which(data$hex %in% blacks)
    colIdxWhite <- which(data$hex %in% whites)

    # Remove rows with values in blacks
    if (length(colIdxBlack) > 0) {
      res <- data[-colIdxBlack, , drop = FALSE]
    } else {
      res <- data
    }

    # Remove rows with values in whites
    if (length(colIdxWhite) > 0) {
      res_final <- res[-colIdxWhite, , drop = FALSE]
    } else {
      res_final <- res
    }

    return(res_final)
  }


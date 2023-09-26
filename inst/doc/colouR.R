## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
# install.packages("devtools")
#devtools::install_github("AlanInglis/colouR")

# Load the package
library(colouR)

# Load ggplot2 for making some plots
library(ggplot2)

## ---- out.height='30%', out.width='30%', fig.align="center"-------------------
knitr::include_graphics("https://raw.githubusercontent.com/AlanInglis/colouR/master/images/bender.png")

## ---- top10cols---------------------------------------------------------------
set.seed(1701) # for reproducability

top10 <- getTopCol(path = "https://raw.githubusercontent.com/AlanInglis/colouR/master/images/bender.png",
                   n = 10,
                   avgCols = FALSE,
                   exclude = FALSE)

# take a look at the top 100 most frequent colours in the image:
top10

## ---- top10colsPlot, out.height='70%', out.width='70%', fig.align="center"----
# order factors
top10$hex <- factor(top10$hex, levels = top10$hex)

# plot
ggplot(top10, aes(x = hex, y = freq)) +
  geom_bar(stat = 'identity', fill = top10$hex) +
  theme_dark() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  xlab('HEX colour code') +
  ylab('Frequency')

## ---- top10colsExclude--------------------------------------------------------
set.seed(1701) # for reproducability

top10exclude <- getTopCol(path = "https://raw.githubusercontent.com/AlanInglis/colouR/master/images/bender.png",
                          n = 10,
                          avgCols = FALSE,
                          exclude = TRUE,
                          customExclude = NULL)

## ---- top10colsPlotExclude, out.height='70%', out.width='70%', fig.align="center"----
# order factors
top10exclude$hex <- factor(top10exclude$hex, levels = top10exclude$hex)

# plot
ggplot(top10exclude, aes(x = hex, y = freq)) +
  geom_bar(stat = 'identity', fill = top10exclude$hex) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  xlab('HEX colour code') +
  ylab('Frequency')

## ---- top10colsAvg------------------------------------------------------------
set.seed(1701) # for reproducability

top10avg <- getTopCol(path = "https://raw.githubusercontent.com/AlanInglis/colouR/master/images/bender.png",
                      avgCols = TRUE,
                      exclude = TRUE,
                      n_clusters = 5)

## ---- top10colsPlotAvg, out.height='70%', out.width='70%', fig.align="center"----
# order factors
top10avg$avg_color <- factor(top10avg$avg_color, levels = top10avg$avg_color)

# plot
ggplot(top10avg, aes(x = avg_color, y = freq)) +
  geom_bar(stat = 'identity', fill = top10avg$avg_color) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  xlab('Average colour') +
  ylab('Frequency')

## ---- look_at_top10-----------------------------------------------------------
top10exclude

## ---- exclude_custom----------------------------------------------------------
coloursToExclude <- c("#A9C5DA", "#7CA5C1", "#C9E0F0", "#5A8595", "#FFFAC2")

top10exclude <- getTopCol(path = "https://raw.githubusercontent.com/AlanInglis/colouR/master/images/bender.png",
                          n = 10,
                          avgCols = FALSE,
                          exclude = TRUE,
                          customExclude = coloursToExclude)

## ---- exclude_view------------------------------------------------------------
top10exclude

## ---- palette,  out.height='70%', out.width='70%', fig.align="center"---------
hex_colors <- c("#FF0000", "#00FF00", "#0000FF", "#FFFF00", "#FF00FF", "#1050FF", "#ffff50")

plotPalette(hex_colors)

## ---- group-------------------------------------------------------------------
cols <- c("#FF0000", "#00FF00", "#0000FF", "#FFFF00", "#FF00FF", "#1050FF", "#ffff50")

set.seed(1701) # for reproducability
grCol <- groupCols(hex_colors = cols, n_clusters = 4)

## ---- groupPlot, out.height='70%', out.width='70%', fig.align="center"--------

# order factors
grCol$hex_color <- factor(grCol$hex_color, levels = grCol$hex_color)

# plot
ggplot(grCol, aes(x = hex_color, y = group)) +
  geom_bar(stat = 'identity', fill = grCol$hex_color) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  xlab('Average colour') +
  ylab('Group')

## ---- palette2, out.height='70%', out.width='70%', fig.align="center"---------
plotPalette(df = grCol, color_col = 'hex_color')

## ---- avg, out.height='70%', out.width='70%', fig.align="center"--------------
set.seed(1701)
avgCl <- avgHex(df = grCol, group_col = 'group', hex_col = 'hex_color')
avgCl
plotPalette(df = avgCl, color_col = 'avg_color')

## ---- img2palette, out.height='70%', out.width='70%', fig.align="center"------
pal <- img2pal(path = "https://raw.githubusercontent.com/AlanInglis/colouR/master/images/bender.png",
               n = 10,
               avgCols = TRUE,
               exclude = TRUE,
               n_clusters = 15,
               customExclude = NULL)

## ---- look_at_palette---------------------------------------------------------
pal

## ---- pals--------------------------------------------------------------------
radiohead_palettes$pabloHoney
taylor_palettes$red

## ---- rhpal, out.height='70%', out.width='70%', fig.align="center"------------
plotPalette(radiohead_palettes$okComputer)

## ---- typal, out.height='70%', out.width='70%', fig.align="center"------------
plotPalette(taylor_palettes$red)

## ---- typal20, out.height='70%', out.width='70%', fig.align="center"----------
# Create a color palette based on a Taylor Swift album cover
tswift_palette <- colPalette(palette = "evermore")
tpal <- tswift_palette(20)
plotPalette(tpal)

## ----rhpalgg, out.height='70%', out.width='70%', fig.align="center"-----------
library(dplyr)
# Apply a Radiohead color scale to a ggplot2 plot

# Create a summary data frame with counts per manufacturer for the mpg data
manufacturer_counts <- mpg %>%
  group_by(manufacturer) %>%
  summarize(count = n())

# sort the data
mpgsort <- manufacturer_counts[order(manufacturer_counts$count, decreasing = TRUE), ]

# order factors
mpgsort$manufacturer <-  factor(mpgsort$manufacturer, levels = mpgsort$manufacturer)

# Create the plot using a Radiohead palette
ggplot(mpgsort, aes(x = manufacturer, y= count, fill = manufacturer)) +
  geom_bar(stat = 'identity') +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scaleFill(palette = "pabloHoney", guide = "none")

## ---- typalgg,  out.height='70%', out.width='70%', fig.align="center"---------

# Create the plot using a Taylor Swift palette
ggplot(mpgsort, aes(x = manufacturer, y= count, fill = manufacturer)) +
  geom_bar(stat = 'identity') +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scaleFill(palette = "evermore", guide = "none")

## ---- rhpalgg2,  out.height='70%', out.width='100%', fig.align="center"-------

# Create the plot using a Radiohead palette
ggplot(mpg[1:122,],  aes(x = displ, y = cty, color = manufacturer)) +
  geom_point(size = 2) +
  scaleColor(palette = 'tkol') +
  theme_minimal()

## ---- typalgg2,  out.height='70%', out.width='100%', fig.align="center"-------

# Create the plot using a Taylor Swift palette
ggplot(mpg[1:122,],  aes(x = displ, y = cty, color = manufacturer)) +
  geom_point(size = 2) +
  scaleColor(palette = 'tSwift') +
  theme_minimal()

## ---- tyhm,  out.height='70%', out.width='100%', fig.align="center"-----------
# Dummy data
x <- LETTERS[1:20]
y <- paste0("var", seq(1,20))
data <- expand.grid(X=x, Y=y)
data$Z <- runif(400, 0, 5)

# Set a Taylor Swift palette of two colours
pal <- taylor_palettes$tSwift[c(6,5)]

# Create a heatmap 
ggplot(data, aes(x = X, y = Y)) +
  geom_tile(aes(fill = Z)) +
  scale_fill_gradientn(
    colors = pal,  name = "Z value",
    guide = guide_colorbar(
      order = 1,
      frame.colour = "black",
      ticks.colour = "black"
    ), oob = scales::squish
  ) +
  xlab('') + ylab('') +
  theme_bw()


## ---- ext---------------------------------------------------------------------
fileName <- "https://raw.githubusercontent.com/AlanInglis/colouR/master/images/bender.png"
getExtension(file = fileName)

# another example
getExtension(file = "example.txt")


## ---- url,   out.height='100%', out.width='100%', fig.align="center"----------
urlName <- "https://raw.githubusercontent.com/AlanInglis/colouR/master/images/bender.png"
image <- read_image_from_url(path = urlName)

# set up a plot 
plot(c(100, 250), c(300, 550), type = "n", xlab = "", ylab = "")
rasterImage(image,100,300,150,550)


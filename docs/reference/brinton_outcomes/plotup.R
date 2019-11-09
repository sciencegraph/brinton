
#' ---
#' title: 'plotup output'
#' author: 'by brinton R package'
#' date: ''
#' ---
#' <style>
#'   .main-container {
#'   max-width: 1200px !important;
#'   }
#'   h1.title {
#'   font-family: monospace;
#'   font-size: 20px;
#'   color: black;
#'   }
#'   h4.author{
#'   font-family: monospace;
#'   font-size: 10pt;
#'   }
#'   h4.date{
#'   font-family: monospace;
#'   font-size: 10pt;
#'   }
#' </style>
#+ preamble, echo=FALSE, message=FALSE
knitr::opts_chunk$set(echo=FALSE, message=FALSE, warning=FALSE, comment=NA, dev='png')

cat('A "color heatmap" produced from the "Petal.Width" variable(s) of the iris dataframe')
#+ plot, fig.width=6, fig.height=4
theme_set(theme_minimal())
ggplot(iris, aes(y=Petal.Width)) +
  stat_density_2d(aes(x=seq_along(Petal.Width), fill = stat(density)), geom = 'raster', contour = FALSE) +
  scale_fill_gradientn(colours = colorRampPalette(rev(RColorBrewer::brewer.pal(4, 'Spectral')))(3)) +
  labs(x='seq.') +
  theme(panel.grid = element_line(colour = NA),
    axis.ticks=element_line(color='black'),
    legend.position='none')

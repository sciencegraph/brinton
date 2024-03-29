
# output files

.onAttach <- function(libname, pkgname) {
  packageStartupMessage("M a G i C i N G R a P H S")
}

dataclass_v <- c('logical',
                 'ordered',
                 'factor',
                 'numeric',
                 'datetime',
                 'character')
logical_v   <- c(
  'blank',
  'line graph',
  'tile plot',
  'point graph',
  'point-to-point graph',
  'linerange graph',
  'bar graph',
  'bw bar graph',
  'color bar graph',
  'binned heatmap',
  'bw binned heatmap',
  'color binned heatmap'
)
ordered_v   <- c(
  'blank',
  'line graph',
  'tile plot',
  'point graph',
  'point-to-point graph',
  'linerange graph',
  'bar graph',
  'bw bar graph',
  'color bar graph',
  'binned heatmap',
  'bw binned heatmap',
  'color binned heatmap'
)
factor_v    <- c(
  'blank',
  'line graph',
  'freq. reordered line graph',
  'alphab. reordered line graph',
  'tile plot',
  'freq. reordered tile plot',
  'alphab. reordered tile plot',
  'point graph',
  'freq. reordered point graph',
  'alphab. reordered point graph',
  'binned heatmap',
  'bw binned heatmap',
  'color binned heatmap',
  'freq. reordered binned heatmap',
  'bw freq. reordered binned heatmap',
  'color freq. reordered binned heatmap',
  'alphab. reordered binned heatmap',
  'bw alphab. reordered binned heatmap',
  'color alphab. reordered binned heatmap',
  'point-to-point graph',
  'freq. reordered point-to-point graph',
  'alphab. reordered point-to-point graph',
  'linerange graph',
  'freq. reordered linerange graph',
  'alphab. reordered linerange graph',
  'bar graph',
  'bw bar graph',
  'color bar graph',
  'freq. reordered bar graph',
  'bw freq. reordered bar graph',
  'color freq. reordered bar graph',
  'alphab. reordered bar graph',
  'bw alphab. reordered bar graph',
  'color alphab. reordered bar graph'
)
datetime_v  <- c(
  'blank',
  'line graph',
  'stepped line graph',
  'point graph',
  'point-to-point graph',
  'stepped point-to-point graph',
  'binned heatmap',
  'bw binned heatmap',
  'color binned heatmap',
  'bw heatmap',
  'color heatmap'
)
numeric_v   <- c(
  'blank',
  'area graph',
  'stepped area graph',
  'bw stepped area graph',
  'color stepped area graph',
  'line graph',
  'stepped line graph',
  'stripe graph',
  'bw stripe graph',
  'color stripe graph',
  'binned stripe graph',
  'bw binned stripe graph',
  'color binned stripe graph',
  'seq. stripe graph',
  'bw seq. stripe graph',
  'color seq. stripe graph',
  'point graph',
  'bw point graph',
  'color point graph',
  'point graph with trend line',
  'bw point graph with trend line',
  'color point graph with trend line',
  'binned heatmap',
  'bw binned heatmap',
  'color binned heatmap',
  'bw heatmap',
  'color heatmap',
  'binned point graph',
  'bw binned point graph',
  'color binned point graph',
  'stepped point-to-point graph',
  'point-to-point graph',
  'bar graph',
  'bw bar graph',
  'color bar graph',
  'histogram',
  'bw histogram',
  'color histogram',
  'freq. polygon',
  'density plot',
  'filled density plot',
  'violin plot',
  'filled violin plot',
  'box plot',
  '3 uniaxial',
  'normal qq plot',
  'ecdf plot',
  'point ecdf plot',
  'stepped ecdf plot'
)
character_v <- c(
  'blank',
  'line graph',
  'freq. reordered line graph',
  'alphab. reordered line graph',
  'tile plot',
  'freq. reordered tile plot',
  'alphab. reordered tile plot',
  'point graph',
  'freq. reordered point graph',
  'alphab. reordered point graph',
  'binned heatmap',
  'bw binned heatmap',
  'color binned heatmap',
  'freq. reordered binned heatmap',
  'bw freq. reordered binned heatmap',
  'color freq. reordered binned heatmap',
  'alphab. reordered binned heatmap',
  'bw alphab. reordered binned heatmap',
  'color alphab. reordered binned heatmap',
  'point-to-point graph',
  'freq. reordered point-to-point graph',
  'alphab. reordered point-to-point graph',
  'linerange graph',
  'freq. reordered linerange graph',
  'alphab. reordered linerange graph',
  'bar graph',
  'bw bar graph',
  'color bar graph',
  'freq. reordered bar graph',
  'bw freq. reordered bar graph',
  'color freq. reordered bar graph',
  'alphab. reordered bar graph',
  'bw alphab. reordered bar graph',
  'color alphab. reordered bar graph'
)
group_v     <- c('sequence',
                 'scatter',
                 'bin',
                 'model',
                 'symbol',
                 'GOF',
                 'random')
label_v     <- c('TRUE',
                 'FALSE',
                 'T',
                 'F',
                 0,
                 1)
numeric2_v <- c(
  'blank',
  'scatter plot',
  'scatter plot with trend line',
  'bw scatter plot',
  'color scatter plot',
  'binned scatter plot',
  'bw binned scatter plot',
  'color binned scatter plot',
  'binned heatmap',
  'bw binned heatmap',
  'color binned heatmap',
  'hexagonal binned heatmap',
  'bw hexagonal binned heatmap',
  'color hexagonal binned heatmap',
  'bw heatmap',
  'color heatmap',
  'contour plot',
  'bw contour plot',
  'color contour plot',
  'contour plot with data points',
  'bw contour plot with data points',
  'color contour plot with data points',
  'parallel plot',
  'bw parallel plot',
  'color parallel plot',
  'unscaled parallel plot',
  'unscaled bw parallel plot',
  'unscaled color parallel plot',
  'path graph',
  'bw path graph',
  'color path graph',
  'point-to-point graph',
  'bw point-to-point graph',
  'color point-to-point graph',
  'point graph',
  'bw point graph',
  'color point graph',
  'line graph',
  'stepped line graph',
  'area graph',
  'stepped area graph',
  'bw stepped area graph',
  'color stepped area graph',
  'bw seq. heatmap',
  'color seq. heatmap',
  'bw seq. stripe graph',
  'color seq. stripe graph',
  'histogram',
  'bw histogram',
  'color histogram',
  'freq. polygon',
  'density plot',
  'color density plot',
  'filled density plot',
  'color filled density plot',
  'violin plot',
  'filled violin plot',
  'box plot',
  'ecdf plot',
  'point ecdf plot',
  'stepped ecdf plot',
  'tile plot',
  'bw stacked histogram',
  'color stacked histogram',
  'bw 100% stacked histogram',
  'color 100% stacked histogram'
)

matrixplot_2num <- c(
  'blank',
  'scatter plot',
  'scatter plot with trend line',
  'bw scatter plot',
  'color scatter plot',
  'binned scatter plot',
  'bw binned scatter plot',
  'color binned scatter plot',
  'binned heatmap',
  'bw binned heatmap',
  'color binned heatmap',
  'hexagonal binned heatmap',
  'bw hexagonal binned heatmap',
  'color hexagonal binned heatmap',
  'bw heatmap',
  'color heatmap',
  'contour plot',
  'bw contour plot',
  'color contour plot',
  'contour plot with data points',
  'bw contour plot with data points',
  'color contour plot with data points',
  'scatter plot with confidence ellipse',
  'bw scatter plot with confidence ellipse',
  'color scatter plot with confidence ellipse',
  'scatter plot with marginal rugs',
  'bw scatter plot with marginal rugs',
  'color scatter plot with marginal rugs',
  'path graph',
  'bw path graph',
  'color path graph',
  'point-to-point graph',
  'bw point-to-point graph',
  'color point-to-point graph'
)

matrixplot_2dt  <- c(
  'blank',
  'scatter plot',
  'scatter plot with trend line',
  'binned scatter plot',
  'bw binned scatter plot',
  'color binned scatter plot',
  'bw heatmap',
  'color heatmap',
  'contour plot',
  'bw contour plot',
  'color contour plot',
  'contour plot with data points',
  'path graph',
  'bw path graph',
  'color path graph',
  'point-to-point graph',
  'bw point-to-point graph',
  'color point-to-point graph'
)

datenum_v  <- c(
  'blank',
  'scatter plot',
  'scatter plot with trend line',
  'binned scatter plot',
  'bw binned scatter plot',
  'color binned scatter plot',
  'bw heatmap',
  'color heatmap',
  'contour plot with data points',
  'path graph',
  'bw path graph',
  'color path graph',
  'point-to-point graph',
  'bw point-to-point graph',
  'color point-to-point graph'
)

fac.num_v1 <- c(
  'blank',
  'path graph',
  'freq. reordered path graph',
  'alphab. reordered path graph',
  'point graph',
  'freq. reordered point graph',
  'alphab. reordered point graph',
  'tile plot',
  'freq. reordered tile plot',
  'alphab. reordered tile plot',
  'binned heatmap',
  'freq. reordered binned heatmap',
  'alphab. reordered binned heatmap',
  'bw binned heatmap',
  'bw freq. reordered binned heatmap',
  'bw alphab. reordered binned heatmap',
  'color binned heatmap',
  'color freq. reordered binned heatmap',
  'color alphab. reordered binned heatmap',
  'violin plot',
  'freq. reordered violin plot',
  'alphab. reordered violin plot',
  'filled violin plot',
  'freq. reordered filled violin plot',
  'alphab. reordered filled violin plot',
  'box plot',
  'freq. reordered box plot',
  'alphab. reordered box plot'
)

fac.num_v2 <- c(
  'blank',
  'bw stacked histogram',
  'color stacked histogram',
  'bw 100% stacked histogram',
  'color 100% stacked histogram',
  'density plot',
  'bw density plot',
  'color density plot',
  'filled density plot',
  'bw filled density plot',
  'color filled density plot'
)

matrixplot_fac.num_v1 <- c(
  'blank',
  'path graph',
  'point graph',
  'tile plot',
  'binned heatmap',
  'bw binned heatmap',
  'color binned heatmap',
  'violin plot',
  'filled violin plot',
  'box plot'
)

matrixplot_fac.num_v2 <- c(
  'blank',
  'bw stacked histogram',
  'color stacked histogram',
  'bw 100% stacked histogram',
  'color 100% stacked histogram',
  'density plot',
  'color density plot',
  'filled density plot',
  'color filled density plot'
)

matrixplot_ord.num_v1 <- c(
  'blank',
  'path graph',
  'point graph',
  'tile plot',
  'binned heatmap',
  'bw binned heatmap',
  'color binned heatmap',
  'violin plot',
  'filled violin plot',
  'box plot'
)

matrixplot_ord.num_v2 <- c(
  'blank',
  'bw stacked histogram',
  'color stacked histogram',
  'bw 100% stacked histogram',
  'color 100% stacked histogram',
  'density plot',
  'bw density plot',
  'color density plot',
  'bw filled density plot',
  'color filled density plot'
)

ord.num_v1 <- c(
  'blank',
  'path graph',
  'point graph',
  'tile plot',
  'binned heatmap',
  'bw binned heatmap',
  'color binned heatmap',
  'violin plot',
  'filled violin plot',
  'box plot'
)

ord.num_v2 <- c(
  'blank',
  'bw stacked histogram',
  'color stacked histogram',
  'bw 100% stacked histogram',
  'color 100% stacked histogram',
  'density plot',
  'bw density plot',
  'color density plot',
  'bw filled density plot',
  'color filled density plot'
)

ord.ord_v0 <- c(
  'blank',
  'bw stacked bar graph',
  'color stacked bar graph',
  'bw 100% stacked bar graph',
  'color 100% stacked bar graph'
)

ord.ord_v1 <- c(
  'blank',
  'transposed bw stacked bar graph',
  'transposed color stacked bar graph',
  'transposed bw 100% stacked bar graph',
  'transposed color 100% stacked bar graph'
  )

ord.ord_v2 <- c(
  'blank',
  'bw heatmap',
  'color heatmap',
  'color residuals heatmap',
  'bw contribution to x2 heatmap',
  'color contribution to x2 heatmap',
  'bw balloon plot',
  'color balloon plot',
  'color residuals balloon plot',
  'bw contribution to x2 balloon plot',
  'color contribution to x2 balloon plot'
)

fac.ord_v0 <- c(
  'blank',
  'bw stacked bar graph',
  'bw freq. reordered stacked bar graph',
  'bw alphab. reordered stacked bar graph',
  'color stacked bar graph',
  'color freq. reordered stacked bar graph',
  'color alphab. reordered stacked bar graph',
  'bw 100% stacked bar graph',
  'bw freq. reordered 100% stacked bar graph',
  'bw alphab. reordered 100% stacked bar graph',
  'color 100% stacked bar graph',
  'color freq. reordered 100% stacked bar graph',
  'color alphab. reordered 100% stacked bar graph'
)

fac.ord_v1 <- c(
  'blank',
  'transposed color stacked bar graph',
  'transposed color 100% stacked bar graph'
)

fac.ord_v2 <- c(
  'blank',
  'bw heatmap',
  'bw freq. reordered heatmap',
  'bw alphab. reordered heatmap',
  'color heatmap',
  'color freq. reordered heatmap',
  'color alphab. reordered heatmap',
  'color residuals heatmap',
  'color freq. reordered residuals heatmap',
  'color alphab. reordered residuals heatmap',
  'bw contribution to x2 heatmap',
  'bw freq. reordered contribution to x2 heatmap',
  'bw alphab. reordered contribution to x2 heatmap',
  'color contribution to x2 heatmap',
  'color freq. reordered contribution to x2 heatmap',
  'color alphab. reordered contribution to x2 heatmap',
  'bw balloon plot',
  'bw freq. reordered balloon plot',
  'bw alphab. reordered balloon plot',
  'color balloon plot',
  'color freq. reordered balloon plot',
  'color alphab. reordered balloon plot',
  'color residuals balloon plot',
  'color freq. reordered residuals balloon plot',
  'color alphab. reordered residuals balloon plot',
  'bw contribution to x2 balloon plot',
  'bw freq. reordered contribution to x2 balloon plot',
  'bw alphab. reordered contribution to x2 balloon plot',
  'color contribution to x2 balloon plot',
  'color freq. reordered contribution to x2 balloon plot',
  'color alphab. reordered contribution to x2 balloon plot'
)

fac.fac_v0 <- c(
  'blank',
  'color stacked bar graph',
  'color freq. reordered stacked bar graph',
  'color alphab. reordered stacked bar graph',
  'color 100% stacked bar graph',
  'color freq. reordered 100% stacked bar graph',
  'color alphab. reordered 100% stacked bar graph'
)

fac.fac_v1 <- c(
  'blank',
  'transposed color stacked bar graph',
  'transposed color freq. reordered stacked bar graph',
  'transposed color alphab. reordered stacked bar graph',
  'transposed color 100% stacked bar graph',
  'transposed color freq. reordered 100% stacked bar graph',
  'transposed color alphab. reordered 100% stacked bar graph'
)

fac.fac_v2 <- c(
  'blank',
  'bw heatmap',
  'color heatmap',
  'bw freq. reordered heatmap',
  'color freq. reordered heatmap',
  'bw alphab. reordered heatmap',
  'color alphab. reordered heatmap',
  'color residuals heatmap',
  'color freq. reordered residuals heatmap',
  'color alphab. reordered residuals heatmap',
  'bw contribution to x2 heatmap',
  'color contribution to x2 heatmap',
  'bw freq. reordered contribution to x2 heatmap',
  'color freq. reordered contribution to x2 heatmap',
  'bw alphab. reordered contribution to x2 heatmap',
  'color alphab. reordered contribution to x2 heatmap',
  'bw balloon plot', 'color balloon plot',
  'bw freq. reordered balloon plot', 'color freq. reordered balloon plot',
  'bw alphab. reordered balloon plot', 'color alphab. reordered balloon plot',
  'color residuals balloon plot',
  'color freq. reordered residuals balloon plot',
  'color alphab. reordered residuals balloon plot',
  'bw contribution to x2 balloon plot',
  'color contribution to x2 balloon plot',
  'bw freq. reordered contribution to x2 balloon plot',
  'color freq. reordered contribution to x2 balloon plot',
  'bw alphab. reordered contribution to x2 balloon plot',
  'color alphab. reordered contribution to x2 balloon plot'
  )

#' @noRd
output_up <- "#' ---
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
"

#' @noRd
output_wide <- "#' ---
#' title: 'wideplot graphic'
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
"

#' @noRd
output_long <- "#' ---
#' title: 'longplot graphic'
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
"

#' @noRd
output_matrix <- "#' ---
#' title: 'matrixplot graphic'
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
suppressPackageStartupMessages(library(patchwork))
knitr::opts_chunk$set(echo=FALSE, message=FALSE, warning=FALSE, comment=NA, dev='png')
"

# warnings
warning_tibble    <- "The vars argument expects a data.frame() class object. Please coerce with 'as.data.frame()'."
warning_bargraph  <- "You may prefer a 'histogram' instead of a 'bargraph'."
warning_general   <- "I'm so sorry because something went wrong :("
warning_pandoc    <- "'brinton' requires Pandoc v < 1.12.3 (https://pandoc.org/)."
warning_color     <- "Color argument accepts only 'black', 'bw' or 'color' values."
warning_coord     <- "Coord argument accepts only 'xy' or 'yx' values."
warning_long      <- "It seems that there are too many levels for some categorical variable."
warning_wp_dt     <- "Wideplot's 'datetime' available graphics are 'linegraph', 'pointgraph' and 'blank'."
warning_wp_lc     <- "Wideplot's 'logical' available graphics are 'linegraph', 'pointgraph' and 'blank'."
warning_wp_of     <- "Wideplot's 'ordered factor' available graphics are 'linegraph', 'pointgraph', 'bargraph' and 'blank'."
warning_wp_ft     <- "Wideplot's 'factor' available graphics are 'linegraph', 'pointgraph', 'bargraph', 'reorderedbargraph' and 'blank'."
warning_wp_nu     <- "Wideplot's 'numeric' available graphics are 'linegraph', 'pointgraph', 'histogram', 'densityplot', '3uniaxial', 'qqplot' and 'blank'."
warning_wp_ch     <- "Wideplot's 'character' available graphics are 'linegraph', 'pointgraph', 'bargraph', 'reorderedbargraph' and 'blank'."
warning_wp_dc     <- "Available datatypes are 'datetime', 'logical', 'factor', 'ordered', 'integer', 'double' and 'character'"

spmn1 <- "Better take a look at https://sciencegraph.github.io/brinton/articles/specimen.html"
spmn2 <- "Better take a look at https://sciencegraph.github.io/brinton/articles/specimen2.html"


# output files

.onAttach <- function(libname, pkgname) {
  packageStartupMessage("M a G i C i N G R a P H S")
}

dataclass_v <- c('logical',
                 'ordered',
                 'factor',
                 'numeric',
                 'datetime',
                 'character'
                 )
logical_v   <- c('blank',
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
ordered_v   <- c('blank',
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
factor_v    <- c('blank',
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
datetime_v  <- c('blank',
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
numeric_v   <- c('blank',
                 'area graph',
                 'stepped area graph',
                 'line graph',
                 'stepped line graph',
                 'stripe graph',
                 'bw stripe graph',
                 'color stripe graph',
                 'binned stripe graph',
                 'bw binned stripe graph',
                 'color binned stripe graph',
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
                 'normal qq plot'
                 )
character_v <- c('blank',
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
                 'random'
                 )
label_v     <- c('TRUE',
                 'FALSE',
                 'T',
                 'F',
                 0,
                 1
                 )

#' @noRd
output_up <- "
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
"

#' @noRd
output_wide <- "
#' ---
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
output_long <- "
#' ---
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

# warnings
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

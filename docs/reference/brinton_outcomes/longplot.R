
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

cat('Graphics from the "tobgp" variable(s) of the esoph dataframe')
#+ ordered, fig.width=12, fig.height=1.16666666666667
gridExtra::grid.arrange(of11, of12, of13, of14, of15, ncol=5)
gridExtra::grid.arrange(of21, of22, of23, ncol=5)
gridExtra::grid.arrange(of31, of32, of33, ncol=5)


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

cat('sleep dataframe')
#+ factor1, fig.width=13, fig.height=0.8
gridExtra::grid.arrange(ft1a, ft1b, ft1c, ft1d, ft1e, ft1f, ft1g, ncol=7)
#+ factor2, fig.width=13, fig.height=2.2
gridExtra::grid.arrange(ft2a, ft2b, ft2c, ft2d, ft2e, ft2f, ft2g, ncol=7)

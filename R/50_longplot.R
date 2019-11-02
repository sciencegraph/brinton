#' Presents a longplot in a html output.
#'
#' A longplot is a range of suitable graphics that represent the relationship
#' within the values of one, or a limited number, of variables in a dataset. Each
#' graphic relates the values of all the selected variables and eventually the
#' row number in which  they appear.
#'
#' In order to present the range of graphics, the user must define a dataset and
#' select one variable whitin it. Future work will include the ability to relate more
#' than one variable.
#'
#' @param data Data.frame. Default dataset to use for plot. If not already a
#' data.frame, it should be first coerced to by [as.data.frame()].
#' @param vars Character. A specific variable within the dataset. Future work will allow to include a vector of variables.
#' @param label Logical. If `TRUE` the output includes labels that show the names of the graphics that are being displayed.
#'
#' @return A html file that includes a range of graphics suitable for this particular combination of variables.
#' @export
#'
#' @examples
#' \dontrun{longplot(iris, "Petal.Length")}
longplot <- function(data,
                     vars,
                     # ncol = 5,
                     label = TRUE
)
{
  ## Format validation: function's object
  if(is.data.frame(data) == FALSE) {
    stop("I am so sorry, but this function only works with a data.frame input!\n",
         "You have provided an object of class ", class(data))
  }
  if(tibble::is_tibble(data) == TRUE) {
    # stop("The object must be coerced to a data frame.")
    data <- as.data.frame(data)
  }
  string      <- " argument expects a character vector"
  if(is.character(vars)  == FALSE) {
    stop(paste0("The 'vars'",  string))
    }
  if(length(vars) > 1) {
    stop("I am so sorry but, up to now, only one variable combinations have been considered.")
  } else {
  if (
    is.logical(unlist(data[, vars])) == TRUE |
    is.factor(unlist(data[, vars])) == TRUE |
    is.ordered(unlist(data[, vars])) == TRUE |
    is.character(unlist(data[, vars])) == TRUE
  ) {long <- length(unique(unlist(data[, vars])))/6 + 0.5}
  else if (
    is.numeric(unlist(data[, vars])) == TRUE |
    lubridate::is.instant(unlist(data[, vars])) == TRUE
  ) {long <- 2.4}
  else {stop("This type of variable has not been yet considered")}

  my_env <- new.env()
  ncol <- 5
  writeLines(output_long, "output.R")
  write(paste0("cat('Graphics from the ", deparse(substitute(vars)), " variable(s) of the ", deparse(substitute(data))," dataframe')"),
        file="output.R", append=TRUE)

  if (lubridate::is.instant(unlist(data[, vars])) == TRUE) {
    write(paste0("#+ datetime, fig.width=12, fig.height=", long), file="output.R", append=TRUE)  # gridExtra
    stripe <- c('line graph',
                'stepped line graph')
    dt11 <- pp_1DD_linegraph(data, colnames(data[vars]), pp_size = 1/ncol)
    dt12 <- pp_1DD_linegraph(data, colnames(data[vars]), 'yx', pp_size = 1/ncol, pp_trans = 'step')
    add_plots("dt1", 2)
    if (label == TRUE) {add_label("datetime", stripe)}
    stripe <- c('point graph',
                'point-to-point graph')
    dt21 <- pp_1DD_pointgraph(data, colnames(data[vars]), pp_size = 1/ncol)
    dt22 <- pp_1DD_linegraph(data, colnames(data[vars]), pp_size = 1/ncol, pp_points = TRUE)
    add_plots("dt2", 2)
    if (label == TRUE) {add_label("datetime", stripe)}
    stripe <- c('binned heatmap',
                'bw binned heatmap',
                'color binned heatmap')
    dt31 <- pp_1DD_heatmap(data, colnames(data[vars]), 'yx', 'black')
    dt32 <- pp_1DD_heatmap(data, colnames(data[vars]), 'yx', 'bw')
    dt33 <- pp_1DD_heatmap(data, colnames(data[vars]), 'yx', 'color')
    add_plots("dt3", 3)
    if (label == TRUE) {add_label("datetime", stripe)}
    stripe <- c('bw heatmap', 'color heatmap')
    p151 <- pp_1DD_raster(data, colnames(data[vars]), 'yx', 'bw')
    p152 <- pp_1DD_raster(data, colnames(data[vars]), 'yx', 'color')
    add_plots("p15", 2)
    if (label == TRUE) {add_label("datetime", stripe)}
    rmarkdown::render("output.R","html_document")
    pander::openFileInOS("output.html")

  } else if (is.logical(unlist(data[, vars])) == TRUE) {
    write(paste0("#+ logical, fig.width=12, fig.height=", long), file="output.R", append=TRUE)  # gridExtra
    stripe <- c('line graph',
                'point graph',
                'point-to-point graph',
                'tile plot',
                'linerange graph')
    lg11 <- pp_1DD_linegraph(data, colnames(data[vars]), 'yx', pp_size = 1/ncol)
    lg12 <- pp_1DD_pointgraph(data, colnames(data[vars]), 'yx', pp_size = 1/ncol)
    lg13 <- pp_1DD_linegraph(data, colnames(data[vars]), 'yx', pp_size = 1/ncol, pp_points = TRUE)
    lg14 <- pp_1DD_tileplot(data, colnames(data[vars]), 'yx')
    lg15 <- pp_1DD_linerange(data, colnames(data[vars]), 'yx', pp_size = 1/ncol)
    add_plots("lg1", 5)
    if (label == TRUE) {add_label("logical", stripe)}
    stripe <- c('binned heatmap',
                'bw binned heatmap',
                'color binned heatmap')
    lg21 <- pp_1DD_heatmap(data, colnames(data[vars]), 'yx', 'black')
    lg22 <- pp_1DD_heatmap(data, colnames(data[vars]), 'yx', 'bw')
    lg23 <- pp_1DD_heatmap(data, colnames(data[vars]), 'yx', 'color')
    add_plots("lg2", 3)
    if (label == TRUE) {add_label("logical", stripe)}
    stripe <- c('bar graph',
                'bw bar graph',
                'color bar graph')
    lg31 <- pp_bargraph(data, colnames(data[vars]), 'black')
    lg32 <- pp_bargraph(data, colnames(data[vars]), 'bw')
    lg33 <- pp_bargraph(data, colnames(data[vars]), 'color')
    add_plots("lg3", 3)
    if (label == TRUE) {add_label("logical", stripe)}
    rmarkdown::render("output.R","html_document")
    pander::openFileInOS("output.html")
  } else if (is.ordered(unlist(data[, vars])) == TRUE) {
    write(paste0("#+ ordered, fig.width=12, fig.height=", long), file="output.R", append=TRUE)  # gridExtra
    stripe <- c('line graph',
                'point graph',
                'point-to-point graph',
                'tile plot',
                'linerange graph')
    of11 <- pp_1DD_linegraph(data, colnames(data[vars]), 'yx', pp_size = 1/ncol)
    of12 <- pp_1DD_pointgraph(data, colnames(data[vars]), 'yx', pp_size = 1/ncol)
    of13 <- pp_1DD_linegraph(data, colnames(data[vars]), 'yx', pp_size = 1/ncol, pp_points = TRUE)
    of14 <- pp_1DD_tileplot(data, colnames(data[vars]), 'yx')
    of15 <- pp_1DD_linerange(data, colnames(data[vars]), 'yx', pp_size = 1/ncol)
    add_plots("of1", 5)
    if (label == TRUE) {add_label("ordered", stripe)}
    stripe <- c('binned heatmap',
                'bw binned heatmap',
                'color binned heatmap')
    of21 <- pp_1DD_heatmap(data, colnames(data[vars]), 'yx', 'black')
    of22 <- pp_1DD_heatmap(data, colnames(data[vars]), 'yx', 'bw')
    of23 <- pp_1DD_heatmap(data, colnames(data[vars]), 'yx', 'color')
    add_plots("of2", 3)
    if (label == TRUE) {add_label("ordered", stripe)}
    stripe <- c('bar graph',
                'bw bar graph',
                'color bar graph')
    of31 <- pp_bargraph(data, colnames(data[vars]), 'black')
    of32 <- pp_bargraph(data, colnames(data[vars]), 'bw')
    of33 <- pp_bargraph(data, colnames(data[vars]), 'color')
    add_plots("of3", 3)
    if (label == TRUE) {add_label("ordered", stripe)}
    rmarkdown::render("output.R","html_document")
    pander::openFileInOS("output.html")
  } else if (is.factor(unlist(data[, vars])) == TRUE & is.ordered(unlist(data[, vars])) == FALSE) {
    write(paste0("#+ factor, fig.width=12, fig.height=", long), file="output.R", append=TRUE)  # gridExtra
    data[[vars]] <- factor(data[[vars]], levels = unique(data[[vars]]))
    stripe <- c('line graph',
                'point graph',
                'point-to-point graph',
                'tile plot',
                'linerange graph')
    ft11 <- pp_1DD_linegraph(data, colnames(data[vars]), 'yx', pp_size = 1/ncol)
    ft12 <- pp_1DD_pointgraph(data, colnames(data[vars]), 'yx', pp_size = 1/ncol)
    ft13 <- pp_1DD_linegraph(data, colnames(data[vars]), 'yx', pp_size = 1/ncol, pp_points = TRUE)
    ft14 <- pp_1DD_tileplot(data, colnames(data[vars]), 'yx')
    ft15 <- pp_1DD_linerange(data, colnames(data[vars]), 'yx', pp_size = 1/ncol)
    add_plots("ft1", 5)
    if (label == TRUE) {add_label("factor", stripe)}
    data[[vars]] <- forcats::fct_infreq(data[[vars]], ordered = TRUE)
    stripe <- c('freq. reordered line graph',
                'freq. reordered point graph',
                'freq. reordered point-to-point graph',
                'freq. reordered tile plot',
                'freq. reordered linerange graph')
    ft21 <- pp_1DD_linegraph(data, colnames(data[vars]), 'yx', pp_size = 1/ncol)
    ft22 <- pp_1DD_pointgraph(data, colnames(data[vars]), 'yx', pp_size = 1/ncol)
    ft23 <- pp_1DD_linegraph(data, colnames(data[vars]), 'yx', pp_size = 1/ncol, pp_points = TRUE)
    ft24 <- pp_1DD_tileplot(data, colnames(data[vars]), 'yx')
    ft25 <- pp_1DD_linerange(data, colnames(data[vars]), 'yx', pp_size = 1/ncol)
    add_plots("ft2", 5)
    if (label == TRUE) {add_label("factor", stripe)}
    data[[vars]] <- as.character(data[[vars]])
    stripe <- c('alphab. reordered line graph',
                'alphab. reordered point graph',
                'alphab. reordered point-to-point graph',
                'alphab. reordered tile plot',
                'alphab. reordered linerange graph')
    ft31 <- pp_1DD_linegraph(data, colnames(data[vars]), 'yx', pp_size = 1/ncol)
    ft32 <- pp_1DD_pointgraph(data, colnames(data[vars]), 'yx', pp_size = 1/ncol)
    ft33 <- pp_1DD_linegraph(data, colnames(data[vars]), 'yx', pp_size = 1/ncol, pp_points = TRUE)
    ft34 <- pp_1DD_tileplot(data, colnames(data[vars]), 'yx')
    ft35 <- pp_1DD_linerange(data, colnames(data[vars]), 'yx', pp_size = 1/ncol)
    add_plots("ft3", 5)
    if (label == TRUE) {add_label("factor", stripe)}
    data[[vars]] <- factor(data[[vars]], levels = unique(data[[vars]]))
    stripe <- c('binned heatmap',
                'bw binned heatmap',
                'color binned heatmap')
    ft41 <- pp_1DD_heatmap(data, colnames(data[vars]), 'yx', 'black')
    ft42 <- pp_1DD_heatmap(data, colnames(data[vars]), 'yx', 'bw')
    ft43 <- pp_1DD_heatmap(data, colnames(data[vars]), 'yx', 'color')
    add_plots("ft4", 3)
    if (label == TRUE) {add_label("factor", stripe)}
    data[[vars]] <- forcats::fct_infreq(data[[vars]], ordered = TRUE)
    stripe <- c('freq. reordered binned heatmap',
                'bw freq. reordered binned heatmap',
                'color freq. reordered binned heatmap')
    ft51 <- pp_1DD_heatmap(data, colnames(data[vars]), 'yx', 'black')
    ft52 <- pp_1DD_heatmap(data, colnames(data[vars]), 'yx', 'bw')
    ft53 <- pp_1DD_heatmap(data, colnames(data[vars]), 'yx', 'color')
    add_plots("ft5", 3)
    if (label == TRUE) {add_label("factor", stripe)}
    data[[vars]] <- as.character(data[[vars]])
    stripe <- c('alphab. reordered binned heatmap',
                'bw alphab. reordered binned heatmap',
                'color alphab. reordered binned heatmap')
    ft61 <- pp_1DD_heatmap(data, colnames(data[vars]), 'yx', 'black')
    ft62 <- pp_1DD_heatmap(data, colnames(data[vars]), 'yx', 'bw')
    ft63 <- pp_1DD_heatmap(data, colnames(data[vars]), 'yx', 'color')
    add_plots("ft6", 3)
    if (label == TRUE) {add_label("factor", stripe)}
    data[[vars]] <- factor(data[[vars]], levels = unique(data[[vars]]))
    stripe <- c('bar graph',
                'bw bar graph',
                'color bar graph')
    ft71 <- pp_bargraph(data, colnames(data[vars]), 'black')
    ft72 <- pp_bargraph(data, colnames(data[vars]), 'bw')
    ft73 <- pp_bargraph(data, colnames(data[vars]), 'color')
    add_plots("ft7", 3)
    if (label == TRUE) {add_label("factor", stripe)}
    data[[vars]] <- forcats::fct_infreq(data[[vars]], ordered = TRUE)
    stripe <- c('freq. reordered bar graph',
                'bw freq. reordered bar graph',
                'color freq. reordered bar graph')
    ft81 <- pp_bargraph(data, colnames(data[vars]), 'black')
    ft82 <- pp_bargraph(data, colnames(data[vars]), 'bw')
    ft83 <- pp_bargraph(data, colnames(data[vars]), 'color')
    add_plots("ft8", 3)
    if (label == TRUE) {add_label("factor", stripe)}
    data[[vars]] <- as.character(data[[vars]])
    stripe <- c('alphab. reordered bar graph',
                'bw alphab. reordered bar graph',
                'color alphab. reordered bar graph')
    ft91 <- pp_bargraph(data, colnames(data[vars]), 'black')
    ft92 <- pp_bargraph(data, colnames(data[vars]), 'bw')
    ft93 <- pp_bargraph(data, colnames(data[vars]), 'color')
    add_plots("ft9", 3)
    if (label == TRUE) {add_label("factor", stripe)}
    rmarkdown::render("output.R","html_document")
    pander::openFileInOS("output.html")
  } else if (is.character(unlist(data[, vars])) == TRUE ) {
    write(paste0("#+ character, fig.width=12, fig.height=", long), file="output.R", append=TRUE)  # gridExtra
    data[[vars]] <- factor(data[[vars]], levels = unique(data[[vars]]))
    stripe <- c('line graph',
                'point graph',
                'point-to-point graph',
                'tile plot',
                'linerange graph')
    p011 <- pp_1DD_linegraph(data, colnames(data[vars]), 'yx', pp_size = 1/ncol)
    p012 <- pp_1DD_pointgraph(data, colnames(data[vars]), 'yx', pp_size = 1/ncol)
    p013 <- pp_1DD_linegraph(data, colnames(data[vars]), 'yx', pp_size = 1/ncol, pp_points = TRUE)
    p014 <- pp_1DD_tileplot(data, colnames(data[vars]), 'yx')
    p015 <- pp_1DD_linerange(data, colnames(data[vars]), 'yx', pp_size = 1/ncol)
    add_plots("p01", 5)
    if (label == TRUE) {add_label("character", stripe)}
    data[[vars]] <- forcats::fct_infreq(data[[vars]], ordered = TRUE)
    stripe <- c('freq. reordered line graph',
                'freq. reordered point graph',
                'freq. reordered point-to-point graph',
                'freq. reordered tile plot',
                'freq. reordered linerange graph')
    p021 <- pp_1DD_linegraph(data, colnames(data[vars]), 'yx', pp_size = 1/ncol)
    p022 <- pp_1DD_pointgraph(data, colnames(data[vars]), 'yx', pp_size = 1/ncol)
    p023 <- pp_1DD_linegraph(data, colnames(data[vars]), 'yx', pp_size = 1/ncol, pp_points = TRUE)
    p024 <- pp_1DD_tileplot(data, colnames(data[vars]), 'yx')
    p025 <- pp_1DD_linerange(data, colnames(data[vars]), 'yx', pp_size = 1/ncol)
    add_plots("p02", 5)
    if (label == TRUE) {add_label("character", stripe)}
    data[[vars]] <- as.character(data[[vars]])
    stripe <- c('alphab. reordered line graph',
                'alphab. reordered point graph',
                'alphab. reordered point-to-point graph',
                'alphab. reordered tile plot',
                'alphab. reordered linerange graph')
    p031 <- pp_1DD_linegraph(data, colnames(data[vars]), 'yx', pp_size = 1/ncol)
    p032 <- pp_1DD_pointgraph(data, colnames(data[vars]), 'yx', pp_size = 1/ncol)
    p033 <- pp_1DD_linegraph(data, colnames(data[vars]), 'yx', pp_size = 1/ncol, pp_points = TRUE)
    p034 <- pp_1DD_tileplot(data, colnames(data[vars]), 'yx')
    p035 <- pp_1DD_linerange(data, colnames(data[vars]), 'yx', pp_size = 1/ncol)
    add_plots("p03", 5)
    if (label == TRUE) {add_label("character", stripe)}
    data[[vars]] <- factor(data[[vars]], levels = unique(data[[vars]]))
    stripe <- c('binned heatmap',
                'bw binned heatmap',
                'color binned heatmap')
    p041 <- pp_1DD_heatmap(data, colnames(data[vars]), 'yx', 'black')
    p042 <- pp_1DD_heatmap(data, colnames(data[vars]), 'yx', 'bw')
    p043 <- pp_1DD_heatmap(data, colnames(data[vars]), 'yx', 'color')
    add_plots("p04", 3)
    if (label == TRUE) {add_label("character", stripe)}
    data[[vars]] <- forcats::fct_infreq(data[[vars]], ordered = TRUE)
    stripe <- c('freq. reordered binned heatmap',
                'bw freq. reordered binned heatmap',
                'color freq. reordered binned heatmap')
    p051 <- pp_1DD_heatmap(data, colnames(data[vars]), 'yx', 'black')
    p052 <- pp_1DD_heatmap(data, colnames(data[vars]), 'yx', 'bw')
    p053 <- pp_1DD_heatmap(data, colnames(data[vars]), 'yx', 'color')
    add_plots("p05", 3)
    if (label == TRUE) {add_label("character", stripe)}
    data[[vars]] <- as.character(data[[vars]])
    stripe <- c('alphab. reordered binned heatmap',
                'bw alphab. reordered binned heatmap',
                'color alphab. reordered binned heatmap')
    p061 <- pp_1DD_heatmap(data, colnames(data[vars]), 'yx', 'black')
    p062 <- pp_1DD_heatmap(data, colnames(data[vars]), 'yx', 'bw')
    p063 <- pp_1DD_heatmap(data, colnames(data[vars]), 'yx', 'color')
    add_plots("p06", 3)
    if (label == TRUE) {add_label("character", stripe)}
    data[[vars]] <- factor(data[[vars]], levels = unique(data[[vars]]))
    stripe <- c('bar graph',
                'bw bar graph',
                'color bar graph')
    p071 <- pp_bargraph(data, colnames(data[vars]), 'black')
    p072 <- pp_bargraph(data, colnames(data[vars]), 'bw')
    p073 <- pp_bargraph(data, colnames(data[vars]), 'color')
    add_plots("p07", 3)
    if (label == TRUE) {add_label("character", stripe)}
    data[[vars]] <- forcats::fct_infreq(data[[vars]], ordered = TRUE)
    stripe <- c('freq. reordered bar graph',
                'bw freq. reordered bar graph',
                'color freq. reordered bar graph')
    p081 <- pp_bargraph(data, colnames(data[vars]), 'black')
    p082 <- pp_bargraph(data, colnames(data[vars]), 'bw')
    p083 <- pp_bargraph(data, colnames(data[vars]), 'color')
    add_plots("p08", 3)
    if (label == TRUE) {add_label("character", stripe)}
    data[[vars]] <- as.character(data[[vars]])
    stripe <- c('alphab. reordered bar graph',
                'bw alphab. reordered bar graph',
                'color alphab. reordered bar graph')
    p091 <- pp_bargraph(data, colnames(data[vars]), 'black')
    p092 <- pp_bargraph(data, colnames(data[vars]), 'bw')
    p093 <- pp_bargraph(data, colnames(data[vars]), 'color')
    add_plots("p09", 3)
    if (label == TRUE) {add_label("character", stripe)}
    rmarkdown::render("output.R","html_document")
    pander::openFileInOS("output.html")
  } else if (is.numeric(unlist(data[, vars])) == TRUE) {
    my_binwidth <- (max(data[vars], na.rm=TRUE)-min(data[vars], na.rm=TRUE))/20
    write(paste0("#+ numeric, fig.width=12, fig.height=", long), file="output.R", append=TRUE)  # gridExtra
    stripe <- c('line graph',
                'stepped line graph')
    p011 <- pp_1DD_linegraph(data, colnames(data[vars]), 'yx', pp_size = 1/ncol)
    p012 <- pp_1DD_linegraph(data, colnames(data[vars]), 'yx', pp_size = 1/ncol, pp_trans = 'step')
    add_plots("p01", 2)
    if (label == TRUE) {add_label("numeric", stripe)}
    stripe <- c('point-to-point graph',
                'stepped point-to-point graph')
    p161 <- pp_1DD_linegraph(data, colnames(data[vars]), 'yx', pp_size = 1/ncol, pp_points = TRUE)
    p162 <- pp_1DD_linegraph(data, colnames(data[vars]), 'yx', pp_size = 1/ncol, pp_points = TRUE, pp_trans = 'step')
    add_plots("p16", 2)
    if (label == TRUE) {add_label("numeric", stripe)}
    stripe <- c('area graph',
                'stepped area graph')
    p131 <- pp_1DD_areagraph(data, colnames(data[vars]), 'yx')
    p132 <- pp_1DD_areagraph(data, colnames(data[vars]), 'yx', pp_trans = 'step')
    add_plots("p13", 2)
    if (label == TRUE) {add_label("numeric", stripe)}
    stripe <- c('stripe graph', 'bw stripe graph', 'color stripe graph')
    p021 <- pp_stripegraph(data, colnames(data[vars]), 'black')
    p022 <- pp_stripegraph(data, colnames(data[vars]), 'bw')
    p023 <- pp_stripegraph(data, colnames(data[vars]), 'color')
    add_plots("p02", 3)
    if (label == TRUE) {add_label("numeric", stripe)}
    stripe <- c('binned stripe graph',
                'bw binned stripe graph',
                'color binned stripe graph')
    p031 <- pp_binned_stripegraph(data, colnames(data[vars]), 'black', my_binwidth)
    p032 <- pp_binned_stripegraph(data, colnames(data[vars]), 'bw', my_binwidth)
    p033 <- pp_binned_stripegraph(data, colnames(data[vars]), 'color', my_binwidth)
    add_plots("p03", 3)
    if (label == TRUE) {add_label("numeric", stripe)}
    stripe <- c('point graph',
                'bw point graph',
                'color point graph')
    p041 <- pp_1DD_scatterplot(data, colnames(data[vars]), 'yx', pp_size = 3/ncol, 'black')
    p042 <- pp_1DD_scatterplot(data, colnames(data[vars]), 'yx', pp_size = 3/ncol, 'bw')
    p043 <- pp_1DD_scatterplot(data, colnames(data[vars]), 'yx', pp_size = 3/ncol, 'color')
    add_plots("p04", 3)
    if (label == TRUE) {add_label("numeric", stripe)}
    stripe <- c('point graph with trend line',
                'bw point graph with trend line',
                'color point graph with trend line')
    p121 <- pp_1DD_scatterplot(data, colnames(data[vars]), 'yx', pp_size = 3/ncol, 'black', 'true')
    p122 <- pp_1DD_scatterplot(data, colnames(data[vars]), 'yx', pp_size = 3/ncol, 'bw', 'true')
    p123 <- pp_1DD_scatterplot(data, colnames(data[vars]), 'yx', pp_size = 3/ncol, 'color', 'true')
    add_plots("p12", 3)
    if (label == TRUE) {add_label("numeric", stripe)}
    stripe <- c('binned point graph',
                'bw binned point graph',
                'color binned point graph')
    p051 <- pp_1DD_binnedpointgraph(data, colnames(data[vars]), 'yx', pp_size = 3/ncol, 'black')
    p052 <- pp_1DD_binnedpointgraph(data, colnames(data[vars]), 'yx', pp_size = 3/ncol, 'bw')
    p053 <- pp_1DD_binnedpointgraph(data, colnames(data[vars]), 'yx', pp_size = 3/ncol, 'color')
    add_plots("p05", 3)
    if (label == TRUE) {add_label("numeric", stripe)}
    stripe <- c('binned heatmap', 'bw binned heatmap', 'color binned heatmap')
    p061 <- pp_1DD_heatmap(data, colnames(data[vars]), 'yx', 'black')
    p062 <- pp_1DD_heatmap(data, colnames(data[vars]), 'yx', 'bw')
    p063 <- pp_1DD_heatmap(data, colnames(data[vars]), 'yx', 'color')
    add_plots("p06", 3)
    if (label == TRUE) {add_label("numeric", stripe)}
    stripe <- c('bw heatmap', 'color heatmap')
    p151 <- pp_1DD_raster(data, colnames(data[vars]), 'yx', 'bw')
    p152 <- pp_1DD_raster(data, colnames(data[vars]), 'yx', 'color')
    add_plots("p15", 2)
    if (label == TRUE) {add_label("numeric", stripe)}
    stripe <- c('bar graph',
                'bw bar graph',
                'color bar graph')
    p071 <- pp_bargraph(data, colnames(data[vars]), 'black', 'xy', pp_size = 0.2*my_binwidth)
    p072 <- pp_bargraph(data, colnames(data[vars]), 'bw', 'xy', pp_size = 0.2*my_binwidth)
    p073 <- pp_bargraph(data, colnames(data[vars]), 'color', 'xy', pp_size = 0.2*my_binwidth)
    add_plots("p07", 3)
    if (label == TRUE) {add_label("numeric", stripe)}
    stripe <- c('histogram',
                'bw histogram',
                'color histogram')
    p081 <- pp_histogram(data, colnames(data[vars]), 'black', pp_binwidth = my_binwidth)
    p082 <- pp_histogram(data, colnames(data[vars]), 'bw', pp_binwidth = my_binwidth)
    p083 <- pp_histogram(data, colnames(data[vars]), 'color', pp_binwidth = my_binwidth)
    add_plots("p08", 3)
    if (label == TRUE) {add_label("numeric", stripe)}
    stripe <- c('freq. polygon')
    p141 <- pp_histogram(data, colnames(data[vars]), 'black', 1, pp_geom = 'line', my_binwidth)
    add_plots("p14", 1)
    if (label == TRUE) {add_label("numeric", stripe)}
    stripe <- c('density plot',
                'filled density plot')
    p091 <- pp_density(data, colnames(data[vars]), pp_size = 1/ncol)
    p092 <- pp_density(data, colnames(data[vars]), pp_size = 1/ncol, pp_color='black')
    add_plots("p09", 2)
    if (label == TRUE) {add_label("numeric", stripe)}
    stripe <- c('violin plot',
                'filled violin plot')
    p101 <- pp_violin(data, colnames(data[vars]), pp_size = 1/ncol)
    p102 <- pp_violin(data, colnames(data[vars]), pp_size = 1/ncol, pp_color='black')
    add_plots("p10", 2)
    if (label == TRUE) {add_label("numeric", stripe)}
    stripe <- c('box plot',
                '3 uniaxial',
                'normal qq plot')
    p111 <- pp_boxplot(data, colnames(data[vars]), pp_size = 1/ncol)
    p112 <- pp_3uniaxial(data, colnames(data[vars]), pp_size = 4/ncol)
    p113 <- qqplot(data, colnames(data[vars]), pp_size = 1/ncol)
    add_plots("p11", 3)
    if (label == TRUE) {add_label("numeric", stripe)}
    rmarkdown::render("output.R","html_document")
    pander::openFileInOS("output.html")
  }
  }
}

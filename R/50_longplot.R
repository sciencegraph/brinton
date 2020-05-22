#' Presents a longplot in a html output.
#'
#' A longplot is a range of suitable graphics that represent the relationship
#' within the values of one, or a limited number, of variables in a dataset. Each
#' graphic relates the values of all the selected variables and eventually the
#' row number in which  they appear.
#'
#' In order to present the range of graphics, the user must define a dataset and
#' select at least one variable whitin it. Future work will include the ability to
#' relate more number and combinations of types of variables.
#'
#' @param data Data.frame. Default dataset to use for plot. If not already a
#' data.frame, it should be first coerced to by [as.data.frame()].
#' @param vars Character. A specific variable within the dataset. Future work will allow to include a vector of variables.
#' @param label Logical. If `TRUE` the output includes labels that show the names of the graphics that are being displayed.
#' @param dir Directory in which the files are stored.
#'
#' @return A html file that includes a range of graphics suitable for this particular combination of variables.
#' @export
#'
#' @examples
#' if (interactive()) {
#' longplot(esoph, "tobgp")
#' }
longplot <- function(data,
                     vars,
                     label = TRUE,
                     dir = tempdir()
                     )
{
  if (rmarkdown::pandoc_available("1.12.3") == FALSE) {print(warning_pandoc)}
  else if (rmarkdown::pandoc_available("1.12.3") == TRUE) {
  ## Auxiliary functions
  add_plots <- function(a, b) {
    write(paste0("gridExtra::grid.arrange(", paste0(a, 1:b, collapse = ", "), ", ncol=5)"), file.path(dir, "brinton_outcomes", "longplot.R"), append = TRUE)
  }

  add_label <- function(a, b) {
    char_types <- paste0(a, " = c('", paste0(b, collapse = "', '"), "')")
    write(paste0('cat("', char_types, '")'), file.path(dir, "brinton_outcomes", "longplot.R"), append=TRUE)
  }
  ## Format validation: function's object
  if(is.data.frame(data) == FALSE) {
    stop("I am so sorry, but this function only works with a data.frame input!\n",
         "You have provided an object of class ", class(data))
  }
  if(tibble::is_tibble(data) == TRUE) {
    stop(warning_tibble)
    # data <- as.data.frame(data)
  }
  string      <- " argument expects a character vector"
  if(is.character(vars)  == FALSE) {
    stop(paste0("The 'vars'",  string))
  }
  if(length(vars) > 2) {
    stop("I am so sorry but, up to now, only one and two variables combinations have been considered.")
  } else {
  if (length(vars) == 1 & (
    is.logical(unlist(data[, vars])) == TRUE |
    is.factor(unlist(data[, vars])) == TRUE |
    is.ordered(unlist(data[, vars])) == TRUE |
    is.character(unlist(data[, vars])) == TRUE)
  ) {long <- length(unique(unlist(data[, vars])))/6 + 0.5}
  else if (length(vars) == 1 & (
    is.numeric(unlist(data[, vars])) == TRUE |
    lubridate::is.instant(unlist(data[, vars])) == TRUE)
  ) {long <- 2}
  else if (length(vars) == 2 & (
    (lubridate::is.instant(unlist(data[, vars[1]])) == TRUE  &
     lubridate::is.instant(unlist(data[, vars[2]])) == TRUE) |
    (is.numeric(unlist(data[, vars[1]])) == TRUE  &
     lubridate::is.instant(unlist(data[, vars[2]])) == TRUE) |
    (lubridate::is.instant(unlist(data[, vars[1]])) == TRUE  &
     is.numeric(unlist(data[, vars[2]])) == TRUE))
  ) {long <- 2}
    else if (length(vars) == 2 &
             is.numeric(unlist(data[, vars[1]])) == TRUE &
             is.numeric(unlist(data[, vars[2]])) == TRUE) {
      long <- 2
    }
    else if (length(vars) == 2 & ((
    is.numeric(unlist(data[, vars[1]])) == TRUE &
    is.factor(unlist(data[, vars[2]])) == TRUE) |
    (
      is.numeric(unlist(data[, vars[2]])) == TRUE &
      is.factor(unlist(data[, vars[1]])) == TRUE))
  ) {long <- length(unique(unlist(sapply(data[, vars], is.factor))))/4 + 0.5}
  else {stop("This type of variable has not been yet considered")}

  my_env <- new.env()
  ncol <- 5
  dir.create(file.path(dir, "brinton_outcomes", fsep = .Platform$file.sep), showWarnings = FALSE)
  writeLines(output_long, file.path(dir, "brinton_outcomes", "longplot.R"))
  write(paste0("cat('Graphics from the ", deparse(substitute(vars)), " variable(s) of the ", deparse(substitute(data))," dataframe')"),
        file.path(dir, "brinton_outcomes", "longplot.R"), append=TRUE)

  if (length(vars) == 1 & lubridate::is.instant(unlist(data[, vars])) == TRUE) {
    write(paste0("#+ datetime, fig.width=12, fig.height=", long), file.path(dir, "brinton_outcomes", "longplot.R"), append=TRUE)  # gridExtra
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
    rmarkdown::render(file.path(dir, "brinton_outcomes", "longplot.R"),"html_document")
    pander::openFileInOS(file.path(dir, "brinton_outcomes", "longplot.html"))

  } else if (length(vars) == 1 & is.logical(unlist(data[, vars])) == TRUE) {
    write(paste0("#+ logical, fig.width=12, fig.height=", long), file.path(dir, "brinton_outcomes", "longplot.R"), append=TRUE)  # gridExtra
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
    rmarkdown::render(file.path(dir, "brinton_outcomes", "longplot.R"),"html_document")
    pander::openFileInOS(file.path(dir, "brinton_outcomes", "longplot.html"))
  } else if (length(vars) == 1 & is.ordered(unlist(data[, vars])) == TRUE) {
    write(paste0("#+ ordered, fig.width=12, fig.height=", long), file.path(dir, "brinton_outcomes", "longplot.R"), append=TRUE)  # gridExtra
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
    rmarkdown::render(file.path(dir, "brinton_outcomes", "longplot.R"),"html_document")
    pander::openFileInOS(file.path(dir, "brinton_outcomes", "longplot.html"))
  } else if (length(vars) == 1 & is.factor(unlist(data[, vars])) == TRUE & is.ordered(unlist(data[, vars])) == FALSE) {
    write(paste0("#+ factor, fig.width=12, fig.height=", long), file.path(dir, "brinton_outcomes", "longplot.R"), append=TRUE)  # gridExtra
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
    rmarkdown::render(file.path(dir, "brinton_outcomes", "longplot.R"),"html_document")
    pander::openFileInOS(file.path(dir, "brinton_outcomes", "longplot.html"))
  } else if (length(vars) == 1 & is.character(unlist(data[, vars])) == TRUE ) {
    write(paste0("#+ character, fig.width=12, fig.height=", long), file.path(dir, "brinton_outcomes", "longplot.R"), append=TRUE)  # gridExtra
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
    rmarkdown::render(file.path(dir, "brinton_outcomes", "longplot.R"),"html_document")
    pander::openFileInOS(file.path(dir, "brinton_outcomes", "longplot.html"))
  } else if (length(vars) == 1 & is.numeric(unlist(data[, vars])) == TRUE) {
    my_binwidth <- (max(data[vars], na.rm=TRUE)-min(data[vars], na.rm=TRUE))/20
    write(paste0("#+ numeric, fig.width=12, fig.height=", long), file.path(dir, "brinton_outcomes", "longplot.R"), append=TRUE)  # gridExtra
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
    stripe <- c('area graph')
    p131 <- pp_1DD_areagraph(data, colnames(data[vars]), 'yx')
    add_plots("p13", 1)
    if (label == TRUE) {add_label("numeric", stripe)}
    stripe <- c('stepped area graph',
                'bw stepped area graph',
                'color stepped area graph')
    p181 <- pp_1DD_areagraph(data, colnames(data[vars]), 'yx', pp_trans = 'step', pp_color = 'black')
    p182 <- pp_1DD_areagraph(data, colnames(data[vars]), 'yx', pp_trans = 'step', pp_color = 'bw')
    p183 <- pp_1DD_areagraph(data, colnames(data[vars]), 'yx', pp_trans = 'step', pp_color = 'color')
    add_plots("p18", 3)
    if (label == TRUE) {add_label("numeric", stripe)}
    stripe <- c('seq. stripe graph',
                'bw seq. stripe graph',
                'color seq. stripe graph')
    p191 <- pp_1DD_stripegraph(data, colnames(data[vars]), pp_color = 'black')
    p192 <- pp_1DD_stripegraph(data, colnames(data[vars]), pp_color = 'bw')
    p193 <- pp_1DD_stripegraph(data, colnames(data[vars]), pp_color = 'color')
    add_plots("p19", 3)
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
    stripe <- c('blank', 'bw heatmap', 'color heatmap')
    p151 <- blank(data, colnames(data[vars]))
    p152 <- pp_1DD_raster(data, colnames(data[vars]), 'yx', 'bw')
    p153 <- pp_1DD_raster(data, colnames(data[vars]), 'yx', 'color')
    add_plots("p15", 3)
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
    stripe <- c('ecfd plot',
                'dotted ecfd plot',
                'stepped ecfd plot')
    p171 <- pp_ecdf(data, colnames(data[vars]), pp_trans = "rect")
    p172 <- pp_ecdf(data, colnames(data[vars]), pp_trans = "point")
    p173 <- pp_ecdf(data, colnames(data[vars]), pp_trans = "step")
    add_plots("p17", 3)
    if (label == TRUE) {add_label("numeric", stripe)}
    rmarkdown::render(file.path(dir, "brinton_outcomes", "longplot.R"),"html_document")
    pander::openFileInOS(file.path(dir, "brinton_outcomes", "longplot.html"))
  } else if (length(vars) == 2 & (
             (lubridate::is.instant(unlist(data[, vars[1]])) == TRUE  &
             lubridate::is.instant(unlist(data[, vars[2]])) == TRUE) |
             (is.numeric(unlist(data[, vars[1]])) == TRUE  &
              lubridate::is.instant(unlist(data[, vars[2]])) == TRUE) |
             (lubridate::is.instant(unlist(data[, vars[1]])) == TRUE  &
              is.numeric(unlist(data[, vars[2]])) == TRUE))) {
    # my_binwidth <- (max(data[vars], na.rm=TRUE)-min(data[vars], na.rm=TRUE))/20
    write(paste0("#+ numeric_datetime, fig.width=12, fig.height=", long), file.path(dir, "brinton_outcomes", "longplot.R"), append=TRUE)  # gridExtra
    stripe <- c('scatter plot', 'scatter plot with trend line')
    p001 <- pp_scatterplot(data, colnames(data[vars][1]), colnames(data[vars][2]), pp_size = 3/ncol, 'black', 'false')
    p002 <- pp_scatterplot(data, colnames(data[vars][1]), colnames(data[vars][2]), pp_size = 3/ncol, 'black', 'true')
    add_plots("p00", 2)
    if (label == TRUE) {add_label("{date~num} OR {2date}", stripe)}
    stripe <- c('binned scatter plot', 'bw binned scatter plot', 'color binned scatter plot')
    p021 <- pp_binnedpointgraph(data, colnames(data[vars][1]), colnames(data[vars][2]), pp_size = 3/ncol, 'black')
    p022 <- pp_binnedpointgraph(data, colnames(data[vars][1]), colnames(data[vars][2]), pp_size = 3/ncol, 'bw')
    p023 <- pp_binnedpointgraph(data, colnames(data[vars][1]), colnames(data[vars][2]), pp_size = 3/ncol, 'color')
    add_plots("p02", 3)
    if (label == TRUE) {add_label("{date~num} OR {2date}", stripe)}
    stripe <- c('binned heatmap', 'bw binned heatmap', 'color binned heatmap')
    p031 <- pp_heatmap(data, colnames(data[vars][1]), colnames(data[vars][2]), 'black')
    p032 <- pp_heatmap(data, colnames(data[vars][1]), colnames(data[vars][2]), 'bw')
    p033 <- pp_heatmap(data, colnames(data[vars][1]), colnames(data[vars][2]), 'color')
    add_plots("p03", 3)
    if (label == TRUE) {add_label("{date~num} OR {2date}", stripe)}
    stripe <- c('blank', 'bw heatmap', 'color heatmap')
    p051 <- blank2(data, colnames(data[vars][1]), colnames(data[vars][2]))
    p052 <- pp_raster(data, colnames(data[vars][1]), colnames(data[vars][2]), 'bw')
    p053 <- pp_raster(data, colnames(data[vars][1]), colnames(data[vars][2]), 'color')
    add_plots("p05", 3)
    if (label == TRUE) {add_label("{date~num} OR {2date}", stripe)}
    stripe <- c('contour plot', 'bw contour plot', 'color contour plot')
    p061 <- pp_contourmap(data, colnames(data[vars][1]), colnames(data[vars][2]), pp_size = 3/ncol, 'black')
    p062 <- pp_contourmap(data, colnames(data[vars][1]), colnames(data[vars][2]), pp_size = 3/ncol, 'bw')
    p063 <- pp_contourmap(data, colnames(data[vars][1]), colnames(data[vars][2]), pp_size = 3/ncol, 'color')
    add_plots("p06", 3)
    if (label == TRUE) {add_label("{date~num} OR {2date}", stripe)}
    stripe <- c('contour plot with data points')
    p071 <- pp_contourmap(data, colnames(data[vars][1]), colnames(data[vars][2]), 'black', pp_size = 3/ncol, 'TRUE')
    add_plots("p07", 1)
    if (label == TRUE) {add_label("{date~num} OR {2date}", stripe)}
    stripe <- c('path graph', 'bw path graph', 'color path graph')
    p101 <- pp_pathgraph(data, colnames(data[vars][1]), colnames(data[vars][2]), FALSE, 'black', pp_size = 3/ncol)
    p102 <- pp_pathgraph(data, colnames(data[vars][1]), colnames(data[vars][2]), FALSE, 'bw', pp_size = 3/ncol)
    p103 <- pp_pathgraph(data, colnames(data[vars][1]), colnames(data[vars][2]), FALSE, 'color', pp_size = 3/ncol)
    add_plots("p10", 3)
    if (label == TRUE) {add_label("{date~num} OR {2date}", stripe)}
    stripe <- c('point-to-point graph', 'bw point-to-point graph', 'color point-to-point graph')
    p111 <- pp_pathgraph(data, colnames(data[vars][1]), colnames(data[vars][2]), TRUE, 'black', pp_size = 3/ncol)
    p112 <- pp_pathgraph(data, colnames(data[vars][1]), colnames(data[vars][2]), TRUE, 'bw', pp_size = 3/ncol)
    p113 <- pp_pathgraph(data, colnames(data[vars][1]), colnames(data[vars][2]), TRUE, 'color', pp_size = 3/ncol)
    add_plots("p11", 3)
    if (label == TRUE) {add_label("{date~num} OR {2date}", stripe)}
    rmarkdown::render(file.path(dir, "brinton_outcomes", "longplot.R"),"html_document")
    pander::openFileInOS(file.path(dir, "brinton_outcomes", "longplot.html"))
  } else if (length(vars) == 2 & is.numeric(unlist(data[, vars[1]])) == TRUE & is.numeric(unlist(data[, vars[2]])) == TRUE) {
    # my_binwidth <- (max(data[vars], na.rm=TRUE)-min(data[vars], na.rm=TRUE))/20
    write(paste0("#+ numeric, fig.width=12, fig.height=", long), file.path(dir, "brinton_outcomes", "longplot.R"), append=TRUE)  # gridExtra
    stripe <- c('scatter plot', 'bw scatter plot', 'color scatter plot')
    p001 <- pp_scatterplot(data, colnames(data[vars][1]), colnames(data[vars][2]), pp_size = 3/ncol, 'black', 'false')
    p002 <- pp_scatterplot(data, colnames(data[vars][1]), colnames(data[vars][2]), pp_size = 3/ncol, 'bw', 'false')
    p003 <- pp_scatterplot(data, colnames(data[vars][1]), colnames(data[vars][2]), pp_size = 3/ncol, 'color', 'false')
    add_plots("p00", 3)
    if (label == TRUE) {add_label("2num", stripe)}
    stripe <- c('binned scatter plot', 'bw binned scatter plot', 'color binned scatter plot')
    p021 <- pp_binnedpointgraph(data, colnames(data[vars][1]), colnames(data[vars][2]), pp_size = 3/ncol, 'black')
    p022 <- pp_binnedpointgraph(data, colnames(data[vars][1]), colnames(data[vars][2]), pp_size = 3/ncol, 'bw')
    p023 <- pp_binnedpointgraph(data, colnames(data[vars][1]), colnames(data[vars][2]), pp_size = 3/ncol, 'color')
    add_plots("p02", 3)
    if (label == TRUE) {add_label("2num", stripe)}
    stripe <- c('binned heatmap', 'bw binned heatmap', 'color binned heatmap')
    p031 <- pp_heatmap(data, colnames(data[vars][1]), colnames(data[vars][2]), 'black')
    p032 <- pp_heatmap(data, colnames(data[vars][1]), colnames(data[vars][2]), 'bw')
    p033 <- pp_heatmap(data, colnames(data[vars][1]), colnames(data[vars][2]), 'color')
    add_plots("p03", 3)
    if (label == TRUE) {add_label("2num", stripe)}
    stripe <- c('hexagonal binned heatmap', 'bw hexagonal binned heatmap', 'color hexagonal binned heatmap')
    p041 <- pp_heatmap(data, colnames(data[vars][1]), colnames(data[vars][2]), 'black', 6)
    p042 <- pp_heatmap(data, colnames(data[vars][1]), colnames(data[vars][2]), 'bw', 6)
    p043 <- pp_heatmap(data, colnames(data[vars][1]), colnames(data[vars][2]), 'color', 6)
    add_plots("p04", 3)
    if (label == TRUE) {add_label("2num", stripe)}
    stripe <- c('blank', 'bw heatmap', 'color heatmap')
    p051 <- blank2(data, colnames(data[vars][1]), colnames(data[vars][2]))
    p052 <- pp_raster(data, colnames(data[vars][1]), colnames(data[vars][2]), 'bw')
    p053 <- pp_raster(data, colnames(data[vars][1]), colnames(data[vars][2]), 'color')
    add_plots("p05", 3)
    if (label == TRUE) {add_label("2num", stripe)}
    stripe <- c('contour plot', 'bw contour plot', 'color contour plot')
    p061 <- pp_contourmap(data, colnames(data[vars][1]), colnames(data[vars][2]), pp_size = 3/ncol, 'black')
    p062 <- pp_contourmap(data, colnames(data[vars][1]), colnames(data[vars][2]), pp_size = 3/ncol, 'bw')
    p063 <- pp_contourmap(data, colnames(data[vars][1]), colnames(data[vars][2]), pp_size = 3/ncol, 'color')
    add_plots("p06", 3)
    if (label == TRUE) {add_label("2num", stripe)}
    stripe <- c('contour plot with data points', 'bw contour plot with data points', 'color contour plot with data points')
    p071 <- pp_contourmap(data, colnames(data[vars][1]), colnames(data[vars][2]), 'black', pp_size = 3/ncol, 'TRUE')
    p072 <- pp_contourmap(data, colnames(data[vars][1]), colnames(data[vars][2]), 'bw', pp_size = 3/ncol, 'TRUE')
    p073 <- pp_contourmap(data, colnames(data[vars][1]), colnames(data[vars][2]), 'color', pp_size = 3/ncol, 'TRUE')
    add_plots("p07", 3)
    if (label == TRUE) {add_label("2num", stripe)}
    stripe <- c('parallel plot', 'bw parallel plot', 'color parallel plot')
    p081 <- pp_parallel(data, colnames(data[vars][1]), colnames(data[vars][2]), TRUE, 'black', pp_size = 3/ncol)
    p082 <- pp_parallel(data, colnames(data[vars][1]), colnames(data[vars][2]), TRUE, 'bw', pp_size = 3/ncol)
    p083 <- pp_parallel(data, colnames(data[vars][1]), colnames(data[vars][2]), TRUE, 'color', pp_size = 3/ncol)
    add_plots("p08", 3)
    if (label == TRUE) {add_label("2num", stripe)}
    stripe <- c('unscaled parallel plot', 'unscaled bw parallel plot', 'unscaled color parallel plot')
    p091 <- pp_parallel(data, colnames(data[vars][1]), colnames(data[vars][2]), FALSE, 'black', pp_size = 3/ncol)
    p092 <- pp_parallel(data, colnames(data[vars][1]), colnames(data[vars][2]), FALSE, 'bw', pp_size = 3/ncol)
    p093 <- pp_parallel(data, colnames(data[vars][1]), colnames(data[vars][2]), FALSE, 'color', pp_size = 3/ncol)
    add_plots("p09", 3)
    if (label == TRUE) {add_label("2num", stripe)}
    stripe <- c('path graph', 'bw path graph', 'color path graph')
    p101 <- pp_pathgraph(data, colnames(data[vars][1]), colnames(data[vars][2]), FALSE, 'black', pp_size = 3/ncol)
    p102 <- pp_pathgraph(data, colnames(data[vars][1]), colnames(data[vars][2]), FALSE, 'bw', pp_size = 3/ncol)
    p103 <- pp_pathgraph(data, colnames(data[vars][1]), colnames(data[vars][2]), FALSE, 'color', pp_size = 3/ncol)
    add_plots("p10", 3)
    if (label == TRUE) {add_label("2num", stripe)}
    stripe <- c('point-to-point graph', 'bw point-to-point graph', 'color point-to-point graph')
    p111 <- pp_pathgraph(data, colnames(data[vars][1]), colnames(data[vars][2]), TRUE, 'black', pp_size = 3/ncol)
    p112 <- pp_pathgraph(data, colnames(data[vars][1]), colnames(data[vars][2]), TRUE, 'bw', pp_size = 3/ncol)
    p113 <- pp_pathgraph(data, colnames(data[vars][1]), colnames(data[vars][2]), TRUE, 'color', pp_size = 3/ncol)
    add_plots("p11", 3)
    if (label == TRUE) {add_label("2num", stripe)}
    stripe <- c('point graph', 'bw point graph', 'color point graph')
    p121 <- pp_unfolded(data, colnames(data[vars][1]), colnames(data[vars][2]), 'black', pp_size = 3/ncol, pp_geom = 'point')
    p122 <- pp_unfolded(data, colnames(data[vars][1]), colnames(data[vars][2]), 'bw', pp_size = 3/ncol, pp_geom = 'point')
    p123 <- pp_unfolded(data, colnames(data[vars][1]), colnames(data[vars][2]), 'color', pp_size = 3/ncol, pp_geom = 'point')
    add_plots("p12", 3)
    if (label == TRUE) {add_label("2num", stripe)}
    stripe <- c('line graph', 'stepped line graph')
    p131 <- pp_unfolded(data, colnames(data[vars][1]), colnames(data[vars][2]), 'black', pp_size = 3/ncol, pp_geom = 'line')
    p132 <- pp_unfolded(data, colnames(data[vars][1]), colnames(data[vars][2]), 'black', pp_size = 3/ncol, pp_geom = 'step')
    add_plots("p13", 2)
    if (label == TRUE) {add_label("numeric", stripe)}
    stripe <- c('area graph')
    p141 <- pp_unfolded(data, colnames(data[vars][1]), colnames(data[vars][2]), 'black', pp_size = 3/ncol, pp_geom = 'area')
    add_plots("p14", 1)
    if (label == TRUE) {add_label("2num", stripe)}
    stripe <- c('stepped area graph', 'bw stepped area graph', 'color stepped area graph')
    p151 <- pp_unfolded(data, colnames(data[vars][1]), colnames(data[vars][2]), 'black', pp_size = 3/ncol, pp_geom = 'bar')
    p152 <- pp_unfolded(data, colnames(data[vars][1]), colnames(data[vars][2]), 'color', pp_size = 3/ncol, pp_geom = 'bar')
    add_plots("p15", 2)
    if (label == TRUE) {add_label("2num", stripe)}
    stripe <- c('blank', 'bw seq. heatmap', 'color seq. heatmap')
    p161 <- blank2(data, colnames(data[vars][1]), colnames(data[vars][2]))
    p162 <- pp_unf_raster(data, colnames(data[vars][1]), colnames(data[vars][2]), 'bw', pp_size = 3/ncol, pp_geom = 'heat')
    p163 <- pp_unf_raster(data, colnames(data[vars][1]), colnames(data[vars][2]), 'color', pp_size = 3/ncol, pp_geom = 'heat')
    add_plots("p16", 3)
    if (label == TRUE) {add_label("2num", stripe)}
    stripe <- c('blank', 'bw seq. stripe graph', 'color seq. stripe graph')
    p171 <- blank2(data, colnames(data[vars][1]), colnames(data[vars][2]))
    p172 <- pp_unf_tile(data, colnames(data[vars][1]), colnames(data[vars][2]), 'bw', pp_size = 3/ncol, pp_geom = 'tile')
    p173 <- pp_unf_tile(data, colnames(data[vars][1]), colnames(data[vars][2]), 'color', pp_size = 3/ncol, pp_geom = 'tile')
    add_plots("p17", 3)
    if (label == TRUE) {add_label("2num", stripe)}
    stripe <- c('histogram', 'bw histogram', 'color histogram')
    p181 <- pp_unf_yuxt(data, colnames(data[vars][1]), colnames(data[vars][2]), 'black', pp_size = 3/ncol, pp_geom = 'hist')
    p182 <- pp_unf_yuxt(data, colnames(data[vars][1]), colnames(data[vars][2]), 'bw', pp_size = 3/ncol, pp_geom = 'hist')
    p183 <- pp_unf_yuxt(data, colnames(data[vars][1]), colnames(data[vars][2]), 'color', pp_size = 3/ncol, pp_geom = 'hist')
    add_plots("p18", 3)
    if (label == TRUE) {add_label("numeric", stripe)}
    stripe <- c('freq. polygon')
    p191 <- pp_unf_yuxt(data, colnames(data[vars][1]), colnames(data[vars][2]), 'black', pp_size = 3/ncol, pp_geom = 'freq')
    add_plots("p19", 1)
    if (label == TRUE) {add_label("2num", stripe)}
    stripe <- c('density plot', 'filled density plot')
    p201 <- pp_unf_yuxt(data, colnames(data[vars][1]), colnames(data[vars][2]), 'black', pp_size = 3/ncol, pp_geom = 'dens')
    p202 <- pp_unf_yuxt(data, colnames(data[vars][1]), colnames(data[vars][2]), 'fill', pp_size = 3/ncol, pp_geom = 'dens')
    add_plots("p20", 2)
    if (label == TRUE) {add_label("2num", stripe)}
    stripe <- c('violin plot', 'filled violin plot')
    p211 <- pp_unf_yuxt(data, colnames(data[vars][1]), colnames(data[vars][2]), 'black', pp_size = 3/ncol, pp_geom = 'viol')
    p212 <- pp_unf_yuxt(data, colnames(data[vars][1]), colnames(data[vars][2]), 'fill', pp_size = 3/ncol, pp_geom = 'viol')
    add_plots("p21", 2)
    if (label == TRUE) {add_label("2num", stripe)}
    stripe <- c('box plot')
    p221 <- pp_unf_yuxt(data, colnames(data[vars][1]), colnames(data[vars][2]), 'black', pp_size = 3/ncol, pp_geom = 'box')
    add_plots("p22", 1)
    if (label == TRUE) {add_label("2num", stripe)}
    stripe <- c('histogram', 'bw histogram', 'color histogram')
    p231 <- pp_unf_ecdf(data, colnames(data[vars][1]), colnames(data[vars][2]), pp_size = 1/ncol, pp_trans = 'line')
    p232 <- pp_unf_ecdf(data, colnames(data[vars][1]), colnames(data[vars][2]), pp_size = 1/ncol, pp_trans = 'point')
    p233 <- pp_unf_ecdf(data, colnames(data[vars][1]), colnames(data[vars][2]), pp_size = 1/ncol, pp_trans = 'step')
    add_plots("p23", 3)
    if (label == TRUE) {add_label("2num", stripe)}
    rmarkdown::render(file.path(dir, "brinton_outcomes", "longplot.R"),"html_document")
    pander::openFileInOS(file.path(dir, "brinton_outcomes", "longplot.html"))
  } else if (length(vars) == 2 & ((
    is.numeric(unlist(data[, vars[1]])) == TRUE &
    is.factor(unlist(data[, vars[2]])) == TRUE) |
    (
      is.numeric(unlist(data[, vars[2]])) == TRUE &
      is.factor(unlist(data[, vars[1]])) == TRUE))) {
    write(paste0("#+ factor_numeric, fig.width=12, fig.height=", long), file.path(dir, "brinton_outcomes", "longplot.R"), append=TRUE)  # gridExtra
    var1 <- colnames(data[vars][which(sapply(data[vars], is.numeric))])
    var2 <- colnames(data[vars][which(sapply(data[vars], is.factor))])
    stripe <- c('path graph',
                'point graph',
                'tile plot')
    ofnum11 <- pp_basicgraph(data, var1, var2, pp_size = 1/ncol, 'line')
    ofnum12 <- pp_basicgraph(data, var1, var2, pp_size = 1/ncol, 'point')
    ofnum13 <- pp_basicgraph(data, var1, var2, pp_size = 1/ncol, 'tile')
    add_plots("ofnum1", 3)
    if (label == TRUE) {add_label("fac-num", stripe)}
    stripe <- c('binned heatmap',
                'bw binned heatmap',
                'color binned heatmap')
    ofnum41 <- pp_basicgraph(data, var1, var2, pp_size = 1/ncol, 'bin', 'black')
    ofnum42 <- pp_basicgraph(data, var1, var2, pp_size = 1/ncol, 'bin', 'bw')
    ofnum43 <- pp_basicgraph(data, var1, var2, pp_size = 1/ncol, 'bin', 'color')
    add_plots("ofnum4", 3)
    if (label == TRUE) {add_label("fac-num", stripe)}
    stripe <- c('violin plot', 'filled violin plot')
    ofnum21 <- pp_basicgraph(data, var1, var2, pp_size = 1/ncol, 'violin')
    ofnum22 <- pp_basicgraph(data, var1, var2, pp_size = 1/ncol, 'violin filled')
    add_plots("ofnum2", 2)
    if (label == TRUE) {add_label("fac-num", stripe)}
    stripe <- c('box plot')
    ofnum31 <- pp_basicgraph(data, var1, var2, pp_size = 1/ncol, 'box')
    add_plots("ofnum3", 1)
    if (label == TRUE) {add_label("fac-num", stripe)}
    write(paste0("#+ factor_numeric2, fig.width=12, fig.height=2"), file.path(dir, "brinton_outcomes", "longplot.R"), append=TRUE)  # gridExtra
    stripe <- c('blank',
                'bw stacked histogram',
                'color stacked histogram')
    ofnum51 <- blank2(data, var1, var2)
    ofnum52 <- pp_histogram2(data, var1, var2)
    ofnum53 <- pp_histogram2(data, var1, var2, pp_color = "color")
    add_plots("ofnum5", 3)
    if (label == TRUE) {add_label("fac-num", stripe)}
    stripe <- c('blank',
                'bw 100% stacked histogram',
                'color 100% stacked histogram')
    ofnum61 <- blank2(data, var1, var2)
    ofnum62 <- pp_histogram2(data, var1, var2, pp_position = "fill")
    ofnum63 <- pp_histogram2(data, var1, var2, pp_color = "color", pp_position = "fill")
    add_plots("ofnum6", 3)
    if (label == TRUE) {add_label("fac-num", stripe)}
    stripe <- c('density plot',
                'color density plot',
                'filled density plot',
                'color filled density plot')
    ofnum71 <- pp_density2(data, var1, var2, 0.5, "line", "bw")
    ofnum72 <- pp_density2(data, var1, var2, 0.5, "line", "color")
    ofnum73 <- pp_density2(data, var1, var2, 0.5, "area", "bw")
    ofnum74 <- pp_density2(data, var1, var2, 0.5, "area", "color")
    add_plots("ofnum7", 4)
    if (label == TRUE) {add_label("fac-num", stripe)}
    rmarkdown::render(file.path(dir, "brinton_outcomes", "longplot.R"),"html_document")
    pander::openFileInOS(file.path(dir, "brinton_outcomes", "longplot.html"))
  }
  # unlink(file.path(dir, "brinton_outcomes", fsep = .Platform$file.sep), recursive = TRUE)
  }
  }
}

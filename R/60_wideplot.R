my_env <- new.env(parent = emptyenv())

#' Displays a wideplot in a html file.
#'
#' A wideplot is a grid of graphics where the graphics within each row
#' corresponds to graphical representations of each one of the variables
#' considered within a given dataset.
#'
#' @param data Data.frame. Default dataset to use for plot. Unquoted. If not
#' already a data.frame, it should be first coerced to by \emph{as.data.frame()}.
#' @param dataclass Character vector.
#' The types of data to be considered among the following:
#' \itemize{
#'   \item \emph{'logical'}
#'   \item \emph{'ordered'}
#'   \item \emph{'factor'}
#'   \item \emph{'numeric'}
#'   \item \emph{'datetime'}
#'   \item \emph{'character'}
#' }
#' @param logical Character vector. Graphics for logical variables among the following:
#' \itemize{
#'   \item \emph{'blank'}
#'   \item \emph{'line graph'}
#'   \item \emph{'tile plot'}
#'   \item \emph{'point graph'}
#'   \item \emph{'point-to-point graph'}
#'   \item \emph{'linerange graph'}
#'   \item \emph{'bar graph'}
#'   \item \emph{'bw bar graph'}
#'   \item \emph{'color bar graph'}
#'   \item \emph{'binned heatmap'}
#'   \item \emph{'bw binned heatmap'}
#'   \item \emph{'color binned heatmap'}
#' }
#' @param ordered Character vector. Graphics for ordered factor variables among the following:
#' \itemize{
#'   \item \emph{'blank'}
#'   \item \emph{'line graph'}
#'   \item \emph{'tile plot'}
#'   \item \emph{'point graph'}
#'   \item \emph{'point-to-point graph'}
#'   \item \emph{'linerange graph'}
#'   \item \emph{'bar graph'}
#'   \item \emph{'bw bar graph'}
#'   \item \emph{'color bar graph'}
#'   \item \emph{'binned heatmap'}
#'   \item \emph{'bw binned heatmap'}
#'   \item \emph{'color binned heatmap'}
#' }
#' @param factor Character vector. Graphics for Character variables among the following:
#' \itemize{
#'   \item \emph{'blank'}
#'   \item \emph{'line graph'}
#'   \item \emph{'freq. reordered line graph'}
#'   \item \emph{'alphab. reordered line graph'}
#'   \item \emph{'tile plot'}
#'   \item \emph{'freq. reordered tile plot'}
#'   \item \emph{'alphab. reordered tile plot'}
#'   \item \emph{'point graph'}
#'   \item \emph{'freq. reordered point graph'}
#'   \item \emph{'alphab. reordered point graph'}
#'   \item \emph{'binned heatmap'}
#'   \item \emph{'bw binned heatmap'}
#'   \item \emph{'color binned heatmap'}
#'   \item \emph{'freq. reordered binned heatmap'}
#'   \item \emph{'bw freq. reordered binned heatmap'}
#'   \item \emph{'color freq. reordered binned heatmap'}
#'   \item \emph{'alphab. reordered binned heatmap'}
#'   \item \emph{'bw alphab. reordered binned heatmap'}
#'   \item \emph{'color alphab. reordered binned heatmap'}
#'   \item \emph{'point-to-point graph'}
#'   \item \emph{'freq. reordered point-to-point graph'}
#'   \item \emph{'alphab. reordered point-to-point graph'}
#'   \item \emph{'linerange graph'}
#'   \item \emph{'freq. reordered linerange graph'}
#'   \item \emph{'alphab. reordered linerange graph'}
#'   \item \emph{'bar graph'}
#'   \item \emph{'bw bar graph'}
#'   \item \emph{'color bar graph'}
#'   \item \emph{'freq. reordered bar graph'}
#'   \item \emph{'bw freq. reordered bar graph'}
#'   \item \emph{'color freq. reordered bar graph'}
#'   \item \emph{'alphab. reordered bar graph'}
#'   \item \emph{'bw alphab. reordered bar graph'}
#'   \item \emph{'color alphab. reordered bar graph'}
#' }
#' @param datetime Character vector. Graphics for datetime variables among the following:
#' \itemize{
#'   \item \emph{'blank'}
#'   \item \emph{'line graph'}
#'   \item \emph{'stepped line graph'}
#'   \item \emph{'point graph'}
#'   \item \emph{'point-to-point graph'}
#'   \item \emph{'stepped point-to-point graph'}
#'   \item \emph{'binned heatmap'}
#'   \item \emph{'bw binned heatmap'}
#'   \item \emph{'color binned heatmap'}
#'   \item \emph{'bw heatmap'}
#'   \item \emph{'color heatmap'}
#' }
#' @param numeric Character vector. Graphics for numeric variables among the following:
#' \itemize{
#'   \item \emph{'blank'}
#'   \item \emph{'area graph'}
#'   \item \emph{'stepped area graph'}
#'   \item \emph{'bw stepped area graph'}
#'   \item \emph{'color stepped area graph'}
#'   \item \emph{'seq. stripe graph'}
#'   \item \emph{'bw seq. stripe graph'}
#'   \item \emph{'color seq. stripe graph'}
#'   \item \emph{'line graph'}
#'   \item \emph{'stepped line graph'}
#'   \item \emph{'stripe graph'}
#'   \item \emph{'bw stripe graph'}
#'   \item \emph{'color stripe graph'}
#'   \item \emph{'binned stripe graph'}
#'   \item \emph{'bw binned stripe graph'}
#'   \item \emph{'color binned stripe graph'}
#'   \item \emph{'point graph'}
#'   \item \emph{'bw point graph'}
#'   \item \emph{'color point graph'}
#'   \item \emph{'point graph with trend line'}
#'   \item \emph{'bw point graph with trend line'}
#'   \item \emph{'color point graph with trend line'}
#'   \item \emph{'binned heatmap'}
#'   \item \emph{'bw binned heatmap'}
#'   \item \emph{'color binned heatmap'}
#'   \item \emph{'bw heatmap'}
#'   \item \emph{'color heatmap'}
#'   \item \emph{'binned point graph'}
#'   \item \emph{'bw binned point graph'}
#'   \item \emph{'color binned point graph'}
#'   \item \emph{'point-to-point graph'}
#'   \item \emph{'stepped point-to-point graph'}
#'   \item \emph{'bar graph'}
#'   \item \emph{'bw bar graph'}
#'   \item \emph{'color bar graph'}
#'   \item \emph{'histogram'}
#'   \item \emph{'bw histogram'}
#'   \item \emph{'color histogram'}
#'   \item \emph{'density plot'}
#'   \item \emph{'filled density plot'}
#'   \item \emph{'violin plot'}
#'   \item \emph{'filled violin plot'}
#'   \item \emph{'box plot'}
#'   \item \emph{'3 uniaxial'}
#'   \item \emph{'normal qq plot'}
#'   \item \emph{'ecdf plot'}
#'   \item \emph{'dotted ecdf plot'}
#'   \item \emph{'stepped ecdf plot'}
#' }
#' @param character Character vector. Graphics for character variables among the following:
#' \itemize{
#'   \item \emph{'blank'}
#'   \item \emph{'line graph'}
#'   \item \emph{'freq. reordered line graph'}
#'   \item \emph{'alphab. reordered line graph'}
#'   \item \emph{'tile plot'}
#'   \item \emph{'freq. reordered tile plot'}
#'   \item \emph{'alphab. reordered tile plot'}
#'   \item \emph{'point graph'}
#'   \item \emph{'freq. reordered point graph'}
#'   \item \emph{'alphab. reordered point graph'}
#'   \item \emph{'binned heatmap'}
#'   \item \emph{'bw binned heatmap'}
#'   \item \emph{'color binned heatmap'}
#'   \item \emph{'freq. reordered binned heatmap'}
#'   \item \emph{'bw freq. reordered binned heatmap'}
#'   \item \emph{'color freq. reordered binned heatmap'}
#'   \item \emph{'alphab. reordered binned heatmap'}
#'   \item \emph{'bw alphab. reordered binned heatmap'}
#'   \item \emph{'color alphab. reordered binned heatmap'}
#'   \item \emph{'point-to-point graph'}
#'   \item \emph{'freq. reordered point-to-point graph'}
#'   \item \emph{'alphab. reordered point-to-point graph'}
#'   \item \emph{'linerange graph'}
#'   \item \emph{'freq. reordered linerange graph'}
#'   \item \emph{'alphab. reordered linerange graph'}
#'   \item \emph{'bar graph'}
#'   \item \emph{'bw bar graph'}
#'   \item \emph{'color bar graph'}
#'   \item \emph{'freq. reordered bar graph'}
#'   \item \emph{'bw freq. reordered bar graph'}
#'   \item \emph{'color freq. reordered bar graph'}
#'   \item \emph{'alphab. reordered bar graph'}
#'   \item \emph{'bw alphab. reordered bar graph'}
#'   \item \emph{'color alphab. reordered bar graph'}
#' }
#' @param group Quoted character. Group of prestablished graphics which marks represent:
#' \itemize{
#'   \item \emph{'sequence'}: the row number of the observations.
#'   \item \emph{'scatter'}: graphics which marks represent individual observations.
#'   \item \emph{'bin'}: observations into a series of intervals.
#'   \item \emph{'model'}: a model built from the observations.
#'   \item \emph{'symbol'}: different statistics through symbols.
#'   \item \emph{'GOF'}: the goodness of fit of a statistical model.
#'   \item \emph{'random'}: random graphics.
#' }
#' @param ncol Numeric. Number of columns. An integer between 3 and 7. The
#' fewer columns displayed, the larger the size of the resulting graphics, a
#' feature that is especially useful if the scale labels dwarf the graphics area.
#' @param label Logical. If `TRUE` the output includes labels that show the
#' names of the graphics that are being displayed.
#' @param dir Directory in which the files are stored.
#'
#' @return Cause the side-effect of creating and displaying a temporary html
#' file that includes a grid of graphics. The variables of a dataset are first
#' grouped by the type of data, then, each variable is graphically represented
#' into a range of different graphics in one row of the matrix.
#' @export
#'
#' @examples
#' if (interactive()) {
#' wideplot(sleep, dataclass = c("factor"),
#' factor=c("point graph", "line graph", "tile plot"),
#' numeric = c("point graph", "line graph", "stepped line graph"))
#' }
wideplot <- function(data,
                     dataclass = NULL,
                     logical = NULL,
                     ordered = NULL,
                     factor = NULL,
                     character = NULL,
                     datetime = NULL,
                     numeric = NULL,
                     group = NULL,
                     ncol = 7,
                     label = "FALSE",
                     dir = tempdir()
                     )
{
  ## Aux. function
  add_blank <- function(x) {
    while (length(x) < 7) {
      x <- append(x, c("blank"), after = length(x))
      i = length(x)
    }
    return(x)
  }
  if (rmarkdown::pandoc_available("1.12.3") == FALSE) {print(warning_pandoc)}
  else if (rmarkdown::pandoc_available("1.12.3") == TRUE) {
  ## Default types of data
  if (is.null(dataclass) == TRUE) {
    index <- c(length(data[sapply(data, is.logical)])>0,
               length(data[sapply(data, is.ordered)])>0,
               length(data[sapply(data, is.factor)][!sapply(data[sapply(data, is.factor)], is.ordered)])>0,
               length(data[sapply(data, is.character)])>0,
               length(data[sapply(data, lubridate::is.instant)])>0,
               length(data[sapply(data, is.numeric)])>0)
    dataclass <- c("logical", "ordered", "factor", "character", "datetime", "numeric")[index]
    }

  ## Default groups of graphics
  if (is.null(group) == TRUE & is.null(logical) == TRUE) {
    logical   <- c("line graph", "point graph", "tile plot", "blank", "bar graph", "color bar graph")
    }
  if (is.null(group) == TRUE & is.null(ordered) == TRUE) {
    ordered   <- c("line graph", "point graph", "tile plot", "blank", "bar graph", "color bar graph")
    }
  if (is.null(group) == TRUE & is.null(factor) == TRUE) {
    factor    <- c("line graph", "point graph", "tile plot", "blank", "bar graph", "color bar graph",
                   "freq. reordered bar graph", "alphab. reordered bar graph")
    }
  if (is.null(group) == TRUE & is.null(character) == TRUE) {
    character <- c("line graph", "point graph", "tile plot", "blank", "bar graph", "color bar graph",
                   "freq. reordered bar graph", "alphab. reordered bar graph")
    }
  if (is.null(group) == TRUE & is.null(datetime) == TRUE) {
    datetime  <- c("line graph", "point graph", "binned heatmap",
                   "color heatmap")
    }
  if (is.null(group) == TRUE & is.null(numeric) == TRUE) {
    numeric   <- c("line graph", "point graph", "binned heatmap",
                   "color heatmap", "histogram", "color bar graph", "filled density plot")
    }
  if (identical(group, "sequence") == TRUE & is.null(logical) == TRUE) {
    logical   <- c("point graph", "line graph", "tile plot", "point-to-point graph")
    }
  if (identical(group, "sequence") == TRUE & is.null(ordered) == TRUE) {
    ordered   <- c("point graph", "line graph", "tile plot", "point-to-point graph")
    }
  if (identical(group, "sequence") == TRUE & is.null(factor) == TRUE) {
    factor    <- c("point graph", "line graph", "tile plot", "point-to-point graph")
    }
  if (identical(group, "sequence") == TRUE & is.null(character) == TRUE) {
    character <- c("point graph", "line graph", "tile plot", "point-to-point graph")
    }
  if (identical(group, "sequence") == TRUE & is.null(datetime) == TRUE) {
    datetime  <- c("point graph", "line graph", "stepped line graph", "point-to-point graph",
                   "binned heatmap", "color binned heatmap", "color heatmap")
    }
  if (identical(group, "sequence") == TRUE & is.null(numeric) == TRUE) {
    numeric   <- c("point graph", "line graph", "stepped line graph", "point-to-point graph",
                   "area graph", "stepped area graph", "color heatmap")
    }

  if (identical(group, "scatter") == TRUE & is.null(logical) == TRUE) {
    logical   <- c("point graph", "blank", "blank", "tile plot")
    }
  if (identical(group, "scatter") == TRUE & is.null(ordered) == TRUE) {
    ordered   <- c("point graph", "blank", "blank", "tile plot")
    }
  if (identical(group, "scatter") == TRUE & is.null(factor) == TRUE) {
    factor    <- c('point graph', 'freq. reordered point graph', 'alphab. reordered point graph',
                  'tile plot', 'freq. reordered tile plot', 'alphab. reordered tile plot')
    }
  if (identical(group, "scatter") == TRUE & is.null(character) == TRUE) {
    character <- c('point graph', 'freq. reordered point graph', 'alphab. reordered point graph',
                     'tile plot', 'freq. reordered tile plot', 'alphab. reordered tile plot')
    }
  if (identical(group, "scatter") == TRUE & is.null(datetime) == TRUE) {
    datetime  <- c("point graph")
    }
  if (identical(group, "scatter") == TRUE & is.null(numeric) == TRUE) {
    numeric   <- c("point graph", "bw point graph", "color point graph",
                   "stripe graph", "bw stripe graph", "color stripe graph")
    }

  if (identical(group, "bin") == TRUE & is.null(logical) == TRUE) {
    logical   <- c('binned heatmap', 'bw binned heatmap', 'color binned heatmap')
    }
  if (identical(group, "bin") == TRUE & is.null(ordered) == TRUE) {
    ordered   <- c('binned heatmap', 'bw binned heatmap', 'color binned heatmap')
    }
  if (identical(group, "bin") == TRUE & is.null(factor) == TRUE) {
    factor   <- c('binned heatmap', 'bw binned heatmap', 'color binned heatmap')
    }
  if (identical(group, "bin") == TRUE & is.null(character) == TRUE) {
    character   <- c('binned heatmap', 'bw binned heatmap', 'color binned heatmap')
    }
  if (identical(group, "bin") == TRUE & is.null(datetime) == TRUE) {
    datetime   <- c('binned heatmap', 'bw binned heatmap', 'color binned heatmap')
    }
  if (identical(group, "bin") == TRUE & is.null(numeric) == TRUE) {
    numeric   <- c("binned heatmap", "bw binned heatmap", "color binned heatmap",
                   "bw binned stripe graph", "color binned stripe graph", "histogram")
    }

  if (identical(group, "model") == TRUE & is.null(numeric) == TRUE) {
    dataclass <- c("numeric")
    numeric   <- c("density plot", "violin plot")
  }

  if (identical(group, "symbol") == TRUE & is.null(numeric) == TRUE) {
    dataclass <- c("numeric")
    numeric   <- c("box plot")
  }

  if (identical(group, "GOF") == TRUE & is.null(numeric) == TRUE) {
    dataclass <- c("numeric")
    numeric   <- c("normal qq plot")
  }

  if (identical(group, "random") == TRUE & is.null(logical) == TRUE) {
    logical   <- sample(logical_v[2:length(logical_v)], 7, replace=F)
  }
  if (identical(group, "random") == TRUE & is.null(ordered) == TRUE) {
    ordered   <- sample(ordered_v[2:length(ordered_v)], 7, replace=F)
  }
  if (identical(group, "random") == TRUE & is.null(factor) == TRUE) {
    factor   <- sample(factor_v[2:length(factor_v)], 7, replace=F)
  }
  if (identical(group, "random") == TRUE & is.null(character) == TRUE) {
    character   <- sample(character_v[2:length(character_v)], 7, replace=F)
  }
  if (identical(group, "random") == TRUE & is.null(datetime) == TRUE) {
    datetime   <- sample(datetime_v[2:length(datetime_v)], 7, replace=F)
  }
  if (identical(group, "random") == TRUE & is.null(numeric) == TRUE) {
    numeric   <- sample(numeric_v[2:length(numeric_v)], 7, replace=F)
  }

## Addition of blank cells to the graphic matrix source

  datetime    <- add_blank(datetime)
  logical     <- add_blank(logical)
  ordered     <- add_blank(ordered)
  factor      <- add_blank(factor)
  numeric     <- add_blank(numeric)
  character   <- add_blank(character)

  my_env <- new.env()
  dir.create(file.path(dir, "brinton_outcomes", fsep = .Platform$file.sep), showWarnings = FALSE)
  writeLines(output_wide, file.path(dir, "brinton_outcomes", "wideplot.R"))
  write(paste0("cat('", deparse(substitute(data)), " dataframe')"), file.path(dir, "brinton_outcomes", "wideplot.R"), append=TRUE)

## Format validation: function's object

if(is.data.frame(data) == FALSE) {
  stop("I am so sorry, but this function only works with a data.frame input!\n",
       "You have provided an object of class ", class(data))
  }
if(tibble::is_tibble(data) == TRUE) {
  stop(warning_tibble)
  # data <- as.data.frame(data)
  }

## Format validation: function's parameters

string      <- " argument expects a character vector"

if(is.character(dataclass) == FALSE) {stop(paste0("The 'dataclass'", string))}
if(is.character(datetime)  == FALSE) {stop(paste0("The 'datetime'",  string))}
if(is.character(logical)   == FALSE) {stop(paste0("The 'logical'",   string))}
if(is.character(ordered)   == FALSE) {stop(paste0("The 'ordered'",   string))}
if(is.character(factor)    == FALSE) {stop(paste0("The 'factor'",    string))}
if(is.character(numeric)   == FALSE) {stop(paste0("The 'numeric'",   string))}
if(is.character(character) == FALSE) {stop(paste0("The 'character'", string))}
if(is.numeric(ncol)        == FALSE) {stop("The 'ncol' argument expects an integer between 3 and 10")}
if(ncol > 10 | ncol < 3) {stop("The 'ncol' argument must be a number between 3 and 10.")}

## Value validation: function's arguments
if(length(dataclass) != sum(dataclass %in% dataclass_v, na.rm = TRUE))
{stop(paste0("The 'dataclass'", string, " which values can be : '",
             paste0(dataclass_v, collapse = "', '"), "'"))
  }
if(length(datetime) != sum(datetime %in% datetime_v, na.rm = TRUE))
{stop(paste0("The 'datetime'", string, " which values can be : '",
             paste0(datetime_v, collapse = "', '"), "'"))
  }
if(length(logical) != sum(logical %in% logical_v, na.rm = TRUE))
{stop(paste0("The 'logical'", string, " which values can be : '",
             paste0(logical_v, collapse = "', '"), "'"))
  }
if(length(ordered) != sum(ordered %in% ordered_v, na.rm = TRUE))
{stop(paste0("The 'ordered'", string, " which values can be : '",
             paste0(ordered_v, collapse = "', '"), "'"))
  }
if(length(factor) != sum(factor %in% factor_v, na.rm = TRUE))
{stop(paste0("The 'factor'", string, " which values can be : '",
             paste0(factor_v, collapse = "', '"), "'"))
  }
if(length(numeric) != sum(numeric %in% numeric_v, na.rm = TRUE))
{stop(paste0("The 'numeric'", string, " which values can be : '",
             paste0(numeric_v, collapse = "', '"), "'"))
  }
if(length(character) != sum(character %in% character_v, na.rm = TRUE))
{stop(paste0("The 'character'", string, " which values can be : '",
             paste0(character_v, collapse = "', '"), "'"))
  }
if(length(label) != sum(label %in% label_v, na.rm = TRUE))
{stop(paste0("The 'label' argument expects a logical: '",
             paste0(label_v, collapse = "', '"), "'"))
}
if(length(group) != sum(group %in% group_v, na.rm = TRUE))
{stop(paste0("The 'group' argument expects a character which values can be : '",
             paste0(group_v, collapse = "', '"), "'"))
}

for (h in seq_along(dataclass)) {
  if (identical(dataclass[h], "logical") == TRUE) {

if (length(data[sapply(data, is.logical)])>0)
{
  data.logic <- data[sapply(data, is.logical)]
  rownames(data.logic) <- NULL
  out = NULL
  for (i in seq_along(data.logic))
  {
    for (j in 1:ncol) {eval(parse(text=paste0("lgi", letters[j], " <- paste0('lg', ", i, ", '", letters[j],"')")))}
    logic.plot <- function(pp)
    {
      long <- round(length(unique(pp[[i]]))/6 + 0.5, 1)
      # if (long > 20 ) {stop(warning_long)}
      for (j in 1:ncol) {eval(parse(text=paste0("
                                             if (logical[", j, "] == 'line graph') {
                                             assign(lgi", letters[j], ",
                                             pp_1DD_linegraph(pp, colnames(pp[i]), 'yx', pp_size = 1/ncol), envir=my_env)
                                             } else if (logical[", j, "] == 'point graph') {
                                             assign(lgi", letters[j], ",
                                             pp_1DD_pointgraph(pp, colnames(pp[i]), 'yx', pp_size = 1/ncol), envir=my_env)
                                             } else if (logical[", j, "] == 'tile plot') {
                                             assign(lgi", letters[j], ",
                                             pp_1DD_tileplot(pp, colnames(pp[i]), 'yx'), envir=my_env)
                                             } else if (logical[", j, "] == 'point-to-point graph') {
                                             assign(lgi", letters[j], ",
                                             pp_1DD_linegraph(pp, colnames(pp[i]), 'yx', pp_size = 1/ncol, pp_points = TRUE), envir=my_env)
                                             } else if (logical[", j, "] == 'linerange graph') {
                                             assign(lgi", letters[j], ",
                                             pp_1DD_linerange(pp, colnames(pp[i]), 'yx', pp_size = 1/ncol), envir=my_env)
                                             } else if (logical[", j, "] == 'bar graph') {
                                             assign(lgi", letters[j], ",
                                             pp_bargraph(pp, colnames(pp[i]), 'black'), envir=my_env)
                                             } else if (logical[", j, "] == 'bw bar graph') {
                                             assign(lgi", letters[j], ",
                                             pp_bargraph(pp, colnames(pp[i]), 'bw'), envir=my_env)
                                             } else if (logical[", j, "] == 'color bar graph') {
                                             assign(lgi", letters[j], ",
                                             pp_bargraph(pp, colnames(pp[i]), 'color'), envir=my_env)
                                             } else if (logical[", j, "] == 'binned heatmap') {
                                             assign(lgi", letters[j], ",
                                             pp_1DD_heatmap(pp, colnames(pp[i]), 'yx', 'black'), envir=my_env)
                                             } else if (logical[", j, "] == 'bw binned heatmap') {
                                             assign(lgi", letters[j], ",
                                             pp_1DD_heatmap(pp, colnames(pp[i]), 'yx', 'bw'), envir=my_env)
                                             } else if (logical[", j, "] == 'color binned heatmap') {
                                             assign(lgi", letters[j], ",
                                             pp_1DD_heatmap(pp, colnames(pp[i]), 'yx', 'color'), envir=my_env)
                                             } else if (logical[", j, "] == 'blank') {
                                             assign(lgi", letters[j], ",
                                             blank(pp, colnames(pp[i])), envir=my_env)
                                             } else {print(warning_wrong)}")))}
      line <- eval(parse(
        text=paste0("paste0('gridExtra::grid.arrange(' ,", paste0(" lgi", letters[1:ncol], collapse = ",', ',"), ",', ncol=", ncol, ")')")))
      write(paste0("#+ logical", i, ", fig.width=13, fig.height=", long), file.path(dir, "brinton_outcomes", "wideplot.R"), append=TRUE)  # gridExtra
      write(line, file.path(dir, "brinton_outcomes", "wideplot.R"), append=TRUE)  # gridExtra
    }
    logic.plot(data.logic)
  }
  if (label == TRUE) {
    char_types <- paste0("logical = c('", paste0(logical[1:ncol], collapse = "', '"), "')")
    write(paste0('cat("', char_types, '")'), file.path(dir, "brinton_outcomes", "wideplot.R"), append=TRUE)
  }
}

### ORDERED

  } else if (identical(dataclass[h], "ordered") == TRUE) {

if (length(data[sapply(data, is.ordered)])>0)
{
  data.ofac <- data[sapply(data, is.ordered)]
  rownames(data.ofac) <- NULL
  out = NULL
  for (i in seq_along(data.ofac))
  {
    for (j in 1:ncol) {eval(parse(text=paste0("ofi", letters[j], " <- paste0('of', ", i, ", '", letters[j],"')")))}
    ofac.plot <- function(pp)
    {
      long <- round(length(unique(pp[[i]]))/6 + 0.5, 1)
      # if (long > 20 ) {stop(warning_long)}
      for (j in 1:ncol) {eval(parse(text=paste0("
                                             if (ordered[", j, "] == 'line graph') {
                                             assign(ofi", letters[j], ",
                                             pp_1DD_linegraph(pp, colnames(pp[i]), 'yx', pp_size = 1/ncol), envir=my_env)
                                             } else if (ordered[", j, "] == 'tile plot') {
                                             assign(ofi", letters[j], ",
                                             pp_1DD_tileplot(pp, colnames(pp[i]), 'yx'), envir=my_env)
                                             } else if (ordered[", j, "] == 'point graph') {
                                             assign(ofi", letters[j], ",
                                             pp_1DD_pointgraph(pp, colnames(pp[i]), 'yx', pp_size = 1/ncol), envir=my_env)
                                             } else if (ordered[", j, "] == 'point-to-point graph') {
                                             assign(ofi", letters[j], ",
                                             pp_1DD_linegraph(pp, colnames(pp[i]), 'yx', pp_size = 1/ncol, pp_points = TRUE), envir=my_env)
                                             } else if (ordered[", j, "] == 'linerange graph') {
                                             assign(ofi", letters[j], ",
                                             pp_1DD_linerange(pp, colnames(pp[i]), 'yx', pp_size = 1/ncol), envir=my_env)
                                             } else if (ordered[", j, "] == 'bar graph') {
                                             assign(ofi", letters[j], ",
                                             pp_bargraph(pp, colnames(pp[i]), 'black'), envir=my_env)
                                             } else if (ordered[", j, "] == 'bw bar graph') {
                                             assign(ofi", letters[j], ",
                                             pp_bargraph(pp, colnames(pp[i]), 'bw'), envir=my_env)
                                             } else if (ordered[", j, "] == 'color bar graph') {
                                             assign(ofi", letters[j], ",
                                             pp_bargraph(pp, colnames(pp[i]), 'color'), envir=my_env)
                                             } else if (ordered[", j, "] == 'binned heatmap') {
                                             assign(ofi", letters[j], ",
                                             pp_1DD_heatmap(pp, colnames(pp[i]), 'yx', 'black'), envir=my_env)
                                             } else if (ordered[", j, "] == 'bw binned heatmap') {
                                             assign(ofi", letters[j], ",
                                             pp_1DD_heatmap(pp, colnames(pp[i]), 'yx', 'bw'), envir=my_env)
                                             } else if (ordered[", j, "] == 'color binned heatmap') {
                                             assign(ofi", letters[j], ",
                                             pp_1DD_heatmap(pp, colnames(pp[i]), 'yx', 'color'), envir=my_env)
                                             } else if (ordered[", j, "] == 'blank') {
                                             assign(ofi", letters[j], ",
                                             blank(pp, colnames(pp[i])), envir=my_env)
                                             } else {print(warning_wrong)}")))}

      line <- eval(parse(
        text=paste0("paste0('gridExtra::grid.arrange(' ,", paste0(" ofi", letters[1:ncol], collapse = ",', ',"), ",', ncol=", ncol, ")')")))
      write(paste0("#+ ordered", i, ", fig.width=13, fig.height=", long), file.path(dir, "brinton_outcomes", "wideplot.R"), append=TRUE)  # gridExtra
      write(line, file.path(dir, "brinton_outcomes", "wideplot.R"), append=TRUE)  # gridExtra
      }
    ofac.plot(data.ofac)
  }
  if (label == TRUE) {
    char_types <- paste0("ordered = c('", paste0(ordered[1:ncol], collapse = "', '"), "')")
    write(paste0('cat("', char_types, '")'), file.path(dir, "brinton_outcomes", "wideplot.R"), append=TRUE)
  }
}

### FACTOR

  } else if (identical(dataclass[h], "factor") == TRUE) {

if (length(data[sapply(data, is.factor)][!sapply(data[sapply(data, is.factor)], is.ordered)])>0)
{
  data.fac <- data[sapply(data, is.factor)][!sapply(data[sapply(data, is.factor)], is.ordered)]
  rownames(data.fac) <- NULL
  out = NULL
  for (i in seq_along(data.fac))
  {
    for (j in 1:ncol) {eval(parse(text=paste0("fti", letters[j], " <- paste0('ft', ", i, ", '", letters[j],"')")))}
    fac.plot <- function(pp)
    {
      long <- round(length(unique(pp[[i]]))/6 + 0.5, 1)
      # if (long > 20 ) {stop(warning_long)}
      for (j in 1:ncol) {eval(parse(text=paste0("
                                                if (factor[", j, "] == 'line graph') {
                                                pp[[i]] <- factor(pp[[i]], levels = unique(pp[[i]]))
                                                assign(fti", letters[j], ",
                                                pp_1DD_linegraph(pp, colnames(pp[i]), 'yx', pp_size = 1/ncol), envir=my_env)
                                                } else if (factor[", j, "] == 'freq. reordered line graph') {
                                                pp[[i]] <- forcats::fct_infreq(pp[[i]], ordered = TRUE)
                                                assign(fti", letters[j], ",
                                                pp_1DD_linegraph(pp, colnames(pp[i]), 'yx', pp_size = 1/ncol), envir=my_env)
                                                } else if (factor[", j, "] == 'alphab. reordered line graph') {
                                                pp[[i]] <- as.character(pp[[i]])
                                                assign(fti", letters[j], ",
                                                pp_1DD_linegraph(pp, colnames(pp[i]), 'yx', pp_size = 1/ncol), envir=my_env)
                                                } else if (factor[", j, "] == 'tile plot') {
                                                pp[[i]] <- factor(pp[[i]], levels = unique(pp[[i]]))
                                                assign(fti", letters[j], ",
                                                pp_1DD_tileplot(pp, colnames(pp[i]), 'yx'), envir=my_env)
                                                } else if (factor[", j, "] == 'freq. reordered tile plot') {
                                                pp[[i]] <- forcats::fct_infreq(pp[[i]], ordered = TRUE)
                                                assign(fti", letters[j], ",
                                                pp_1DD_tileplot(pp, colnames(pp[i]), 'yx'), envir=my_env)
                                                } else if (factor[", j, "] == 'alphab. reordered tile plot') {
                                                pp[[i]] <- as.character(pp[[i]])
                                                assign(fti", letters[j], ",
                                                pp_1DD_tileplot(pp, colnames(pp[i]), 'yx'), envir=my_env)
                                                } else if (factor[", j, "] == 'point graph') {
                                                pp[[i]] <- factor(pp[[i]], levels = unique(pp[[i]]))
                                                assign(fti", letters[j], ",
                                                pp_1DD_pointgraph(pp, colnames(pp[i]), 'yx', pp_size = 1/ncol), envir=my_env)
                                                } else if (factor[", j, "] == 'freq. reordered point graph') {
                                                pp[[i]] <- forcats::fct_infreq(pp[[i]], ordered = TRUE)
                                                assign(fti", letters[j], ",
                                                pp_1DD_pointgraph(pp, colnames(pp[i]), 'yx', pp_size = 1/ncol), envir=my_env)
                                                } else if (factor[", j, "] == 'alphab. reordered point graph') {
                                                pp[[i]] <- as.character(pp[[i]])
                                                assign(fti", letters[j], ",
                                                pp_1DD_pointgraph(pp, colnames(pp[i]), 'yx', pp_size = 1/ncol), envir=my_env)
                                                } else if (factor[", j, "] == 'binned heatmap') {
                                                pp[[i]] <- factor(pp[[i]], levels = unique(pp[[i]]))
                                                assign(fti", letters[j], ",
                                                pp_1DD_heatmap(pp, colnames(pp[i]), 'yx', 'black'), envir=my_env)
                                                } else if (factor[", j, "] == 'bw binned heatmap') {
                                                pp[[i]] <- factor(pp[[i]], levels = unique(pp[[i]]))
                                                assign(fti", letters[j], ",
                                                pp_1DD_heatmap(pp, colnames(pp[i]), 'yx', 'bw'), envir=my_env)
                                                } else if (factor[", j, "] == 'color binned heatmap') {
                                                pp[[i]] <- factor(pp[[i]], levels = unique(pp[[i]]))
                                                assign(fti", letters[j], ",
                                                pp_1DD_heatmap(pp, colnames(pp[i]), 'yx', 'color'), envir=my_env)
                                                } else if (factor[", j, "] == 'freq. reordered binned heatmap') {
                                                pp[[i]] <- forcats::fct_infreq(pp[[i]], ordered = TRUE)
                                                assign(fti", letters[j], ",
                                                pp_1DD_heatmap(pp, colnames(pp[i]), 'yx', 'black'), envir=my_env)
                                                } else if (factor[", j, "] == 'bw freq. reordered binned heatmap') {
                                                pp[[i]] <- forcats::fct_infreq(pp[[i]], ordered = TRUE)
                                                assign(fti", letters[j], ",
                                                pp_1DD_heatmap(pp, colnames(pp[i]), 'yx', 'bw'), envir=my_env)
                                                } else if (factor[", j, "] == 'color freq. reordered binned heatmap') {
                                                pp[[i]] <- forcats::fct_infreq(pp[[i]], ordered = TRUE)
                                                assign(fti", letters[j], ",
                                                pp_1DD_heatmap(pp, colnames(pp[i]), 'yx', 'color'), envir=my_env)
                                                } else if (factor[", j, "] == 'alphab. reordered binned heatmap') {
                                                pp[[i]] <- as.character(pp[[i]])
                                                assign(fti", letters[j], ",
                                                pp_1DD_heatmap(pp, colnames(pp[i]), 'yx', 'black'), envir=my_env)
                                                } else if (factor[", j, "] == 'bw alphab. reordered binned heatmap') {
                                                pp[[i]] <- as.character(pp[[i]])
                                                assign(fti", letters[j], ",
                                                pp_1DD_heatmap(pp, colnames(pp[i]), 'yx', 'bw'), envir=my_env)
                                                } else if (factor[", j, "] == 'color alphab. reordered binned heatmap') {
                                                pp[[i]] <- as.character(pp[[i]])
                                                assign(fti", letters[j], ",
                                                pp_1DD_heatmap(pp, colnames(pp[i]), 'yx', 'color'), envir=my_env)
                                                } else if (factor[", j, "] == 'point-to-point graph') {
                                                pp[[i]] <- factor(pp[[i]], levels = unique(pp[[i]]))
                                                assign(fti", letters[j], ",
                                                pp_1DD_linegraph(pp, colnames(pp[i]), 'yx', pp_size = 1/ncol, pp_points = TRUE), envir=my_env)
                                                } else if (factor[", j, "] == 'freq. reordered point-to-point graph') {
                                                pp[[i]] <- forcats::fct_infreq(pp[[i]], ordered = TRUE)
                                                assign(fti", letters[j], ",
                                                pp_1DD_linegraph(pp, colnames(pp[i]), 'yx', pp_size = 1/ncol, pp_points = TRUE), envir=my_env)
                                                } else if (factor[", j, "] == 'alphab. reordered point-to-point graph') {
                                                pp[[i]] <- as.character(pp[[i]])
                                                assign(fti", letters[j], ",
                                                pp_1DD_linegraph(pp, colnames(pp[i]), 'yx', pp_size = 1/ncol, pp_points = TRUE), envir=my_env)
                                                } else if (factor[", j, "] == 'linerange graph') {
                                                pp[[i]] <- factor(pp[[i]], levels = unique(pp[[i]]))
                                                assign(fti", letters[j], ",
                                                pp_1DD_linerange(pp, colnames(pp[i]), 'yx', pp_size = 1/ncol), envir=my_env)
                                                } else if (factor[", j, "] == 'freq. reordered linerange graph') {
                                                pp[[i]] <- forcats::fct_infreq(pp[[i]], ordered = TRUE)
                                                assign(fti", letters[j], ",
                                                pp_1DD_linerange(pp, colnames(pp[i]), 'yx', pp_size = 1/ncol), envir=my_env)
                                                } else if (factor[", j, "] == 'alphab. reordered linerange graph') {
                                                pp[[i]] <- as.character(pp[[i]])
                                                assign(fti", letters[j], ",
                                                pp_1DD_linerange(pp, colnames(pp[i]), 'yx', pp_size = 1/ncol), envir=my_env)
                                                } else if (factor[", j, "] == 'bar graph') {
                                                pp[[i]] <- factor(pp[[i]], levels = unique(pp[[i]]))
                                                assign(fti", letters[j], ",
                                                pp_bargraph(pp, colnames(pp[i]), 'black'), envir=my_env)
                                                } else if (factor[", j, "] == 'bw bar graph') {
                                                pp[[i]] <- factor(pp[[i]], levels = unique(pp[[i]]))
                                                assign(fti", letters[j], ",
                                                pp_bargraph(pp, colnames(pp[i]), 'bw'), envir=my_env)
                                                } else if (factor[", j, "] == 'color bar graph') {
                                                pp[[i]] <- factor(pp[[i]], levels = unique(pp[[i]]))
                                                assign(fti", letters[j], ",
                                                pp_bargraph(pp, colnames(pp[i]), 'color'), envir=my_env)
                                                } else if (factor[", j, "] == 'freq. reordered bar graph') {
                                                pp[[i]] <- forcats::fct_infreq(pp[[i]], ordered = TRUE)
                                                assign(fti", letters[j], ",
                                                pp_bargraph(subset(pp), colnames(pp[i]), 'black'), envir=my_env)
                                                } else if (factor[", j, "] == 'bw freq. reordered bar graph') {
                                                pp[[i]] <- forcats::fct_infreq(pp[[i]], ordered = TRUE)
                                                assign(fti", letters[j], ",
                                                pp_bargraph(subset(pp), colnames(pp[i]), 'bw'), envir=my_env)
                                                } else if (factor[", j, "] == 'color freq. reordered bar graph') {
                                                pp[[i]] <- forcats::fct_infreq(pp[[i]], ordered = TRUE)
                                                assign(fti", letters[j], ",
                                                pp_bargraph(subset(pp), colnames(pp[i]), 'color'), envir=my_env)
                                                } else if (factor[", j, "] == 'alphab. reordered bar graph') {
                                                pp[[i]] <- as.character(pp[[i]])
                                                assign(fti", letters[j], ",
                                                pp_bargraph(subset(pp), colnames(pp[i]), 'black'), envir=my_env)
                                                } else if (factor[", j, "] == 'bw alphab. reordered bar graph') {
                                                pp[[i]] <- as.character(pp[[i]])
                                                assign(fti", letters[j], ",
                                                pp_bargraph(subset(pp), colnames(pp[i]), 'bw'), envir=my_env)
                                                } else if (factor[", j, "] == 'color alphab. reordered bar graph') {
                                                pp[[i]] <- as.character(pp[[i]])
                                                assign(fti", letters[j], ",
                                                pp_bargraph(subset(pp), colnames(pp[i]), 'color'), envir=my_env)
                                                } else if (factor[", j, "] == 'blank') {
                                                assign(fti", letters[j], ",
                                                blank(pp, colnames(pp[i])), envir=my_env)
                                                } else {print(warning_wrong)}
                                                ")
                                    )
                              )
        }
      line <- eval(parse(
        text=paste0("paste0('gridExtra::grid.arrange(' ,", paste0(" fti", letters[1:ncol], collapse = ",', ',"), ",', ncol=", ncol, ")')")))
      write(paste0("#+ factor", i, ", fig.width=13, fig.height=", long), file.path(dir, "brinton_outcomes", "wideplot.R"), append=TRUE)  # gridExtra
      write(line, file.path(dir, "brinton_outcomes", "wideplot.R"), append=TRUE) # gridExtra
      }
    fac.plot(data.fac)
  }
  if (label == TRUE) {
    char_types <- paste0("factor = c('", paste0(factor[1:ncol], collapse = "', '"), "')")
    write(paste0('cat("', char_types, '")'), file.path(dir, "brinton_outcomes", "wideplot.R"), append=TRUE)
  }
}

  } else if (identical(dataclass[h], "datetime") == TRUE) {

    ### DATETIME

    if (length(data[sapply(data, lubridate::is.instant)])>0)
    {
      # select only numeric variables
      data.date <- data[sapply(data, lubridate::is.instant)]
      rownames(data.date) <- NULL
      out = NULL
      # plot a row of graphics for every column
      write(paste0("#+ datetime, fig.width=13, fig.height=", round(12/ncol, 1)), file.path(dir, "brinton_outcomes", "wideplot.R"), append=TRUE)  # gridExtra
      for (i in seq_along(data.date))
      {
        for (j in 1:ncol) {eval(parse(text=paste0("dti", letters[j], " <- paste0('dt', ", i, ", '", letters[j],"')")))}
        date.plot <- function(pp)
        {
          for (j in 1:ncol) {eval(parse(text=paste0("
                                                    if (datetime[", j, "] == 'line graph') {
                                                    assign(dti", letters[j], ",
                                                    pp_1DD_linegraph(pp, colnames(pp[i]), pp_size = 1/ncol), envir=my_env)
                                                    } else if (datetime[", j, "] == 'stepped line graph') {
                                                    assign(dti", letters[j], ",
                                                    pp_1DD_linegraph(pp, colnames(pp[i]), 'yx', pp_size = 1/ncol, pp_trans = 'step'), envir=my_env)
                                                    } else if (datetime[", j, "] == 'point graph') {
                                                    assign(dti", letters[j], ",
                                                    pp_1DD_pointgraph(pp, colnames(pp[i]), pp_size = 1/ncol), envir=my_env)
                                                    } else if (datetime[", j, "] == 'point-to-point graph') {
                                                    assign(dti", letters[j], ",
                                                    pp_1DD_linegraph(pp, colnames(pp[i]), pp_size = 1/ncol, pp_points = TRUE), envir=my_env)
                                                    } else if (datetime[", j, "] == 'stepped point-to-point graph') {
                                                    assign(dti", letters[j], ",
                                                    pp_1DD_linegraph(pp, colnames(pp[i]), pp_size = 1/ncol, pp_trans = 'step', pp_points = TRUE), envir=my_env)
                                                    } else if (datetime[", j, "] == 'binned heatmap') {
                                                    assign(dti", letters[j], ",
                                                    pp_1DD_heatmap(pp, colnames(pp[i]), 'yx', 'black'), envir=my_env)
                                                    } else if (datetime[", j, "] == 'bw binned heatmap') {
                                                    assign(dti", letters[j], ",
                                                    pp_1DD_heatmap(pp, colnames(pp[i]), 'yx', 'bw'), envir=my_env)
                                                    } else if (datetime[", j, "] == 'color binned heatmap') {
                                                    assign(dti", letters[j], ",
                                                    pp_1DD_heatmap(pp, colnames(pp[i]), 'yx', 'color'), envir=my_env)
                                                    } else if (datetime[", j, "] == 'bw heatmap') {
                                                    assign(dti", letters[j], ",
                                                    pp_1DD_raster(pp, colnames(pp[i]), 'yx', pp_color = 'bw'), envir=my_env)
                                                    } else if (datetime[", j, "] == 'color heatmap') {
                                                    assign(dti", letters[j], ",
                                                    pp_1DD_raster(pp, colnames(pp[i]), 'yx', pp_color = 'color'), envir=my_env)
                                                    } else if (datetime[", j, "] == 'blank') {
                                                    assign(dti", letters[j], ",
                                                    blank(pp, colnames(pp[i])), envir=my_env)
                                                    } else {print(warning_wrong)}")))}

          line <- eval(parse(
            text=paste0("paste0('gridExtra::grid.arrange(' ,", paste0(" dti", letters[1:ncol], collapse = ",', ',"), ",', ncol=", ncol, ")')")))
          write(line, file.path(dir, "brinton_outcomes", "wideplot.R"), append=TRUE)  # gridExtra
          }
        date.plot(data.date)
      }
      if (label == TRUE) {
        char_types <- paste0("datetime = c('", paste0(datetime[1:ncol], collapse = "', '"), "')")
        write(paste0('cat("', char_types, '")'), file.path(dir, "brinton_outcomes", "wideplot.R"), append=TRUE)
      }
    }

    ### NUMERIC

  } else if (identical(dataclass[h], "numeric") == TRUE) {

if (length(data[sapply(data, is.numeric)])>0)
{
  data.num <- data[sapply(data, is.numeric)]
  out = NULL
  write(paste0("#+ numeric, fig.width=13, fig.height=", round(12/ncol,1)), file.path(dir, "brinton_outcomes", "wideplot.R"), append=TRUE) # gridExtra
  for (i in seq_along(data.num))
  {
    for (j in 1:ncol) {eval(parse(text=paste0("nui", letters[j], " <- paste0('nu', ", i, ", '", letters[j],"')")))}
    num.plot <- function(pp)
    {
      my_binwidth <- (max(pp[i], na.rm=TRUE)-min(pp[i], na.rm=TRUE))/20
      for (j in 1:ncol) {eval(parse(text=paste0("
                                             if (numeric[", j, "] == 'line graph') {
                                             assign(nui", letters[j], ",
                                             pp_1DD_linegraph(pp, colnames(pp[i]), 'yx', pp_size = 1/ncol), envir=my_env)
                                             } else if (numeric[", j, "] == 'stepped line graph') {
                                             assign(nui", letters[j], ",
                                             pp_1DD_linegraph(pp, colnames(pp[i]), 'yx', pp_size = 1/ncol, pp_trans = 'step'), envir=my_env)
                                             } else if (numeric[", j, "] == 'point-to-point graph') {
                                             assign(nui", letters[j], ",
                                             pp_1DD_linegraph(pp, colnames(pp[i]), 'yx', pp_size = 1/ncol, pp_points = TRUE), envir=my_env)
                                             } else if (numeric[", j, "] == 'stepped point-to-point graph') {
                                             assign(nui", letters[j], ",
                                             pp_1DD_linegraph(pp, colnames(pp[i]), pp_size = 1/ncol, pp_trans = 'step', pp_points = TRUE), envir=my_env)
                                             } else if (numeric[", j, "] == 'area graph') {
                                             assign(nui", letters[j], ",
                                             pp_1DD_areagraph(pp, colnames(pp[i])), envir=my_env)
                                             } else if (numeric[", j, "] == 'stepped area graph') {
                                             assign(nui", letters[j], ",
                                             pp_1DD_areagraph(pp, colnames(pp[i]), pp_trans = 'step'), envir=my_env)
                                             } else if (numeric[", j, "] == 'stripe graph') {
                                             assign(nui", letters[j], ",
                                             pp_stripegraph(pp, colnames(pp[i])), envir=my_env)
                                             } else if (numeric[", j, "] == 'bw stripe graph') {
                                             assign(nui", letters[j], ",
                                             pp_stripegraph(pp, colnames(pp[i]), 'bw'), envir=my_env)
                                             } else if (numeric[", j, "] == 'color stripe graph') {
                                             assign(nui", letters[j], ",
                                             pp_stripegraph(pp, colnames(pp[i]), 'color'), envir=my_env)
                                             } else if (numeric[", j, "] == 'binned stripe graph') {
                                             assign(nui", letters[j], ",
                                             pp_binned_stripegraph(pp, colnames(pp[i]), 'black', my_binwidth), envir=my_env)
                                             } else if (numeric[", j, "] == 'bw binned stripe graph') {
                                             assign(nui", letters[j], ",
                                             pp_binned_stripegraph(pp, colnames(pp[i]), 'bw', my_binwidth), envir=my_env)
                                             } else if (numeric[", j, "] == 'color binned stripe graph') {
                                             assign(nui", letters[j], ",
                                             pp_binned_stripegraph(pp, colnames(pp[i]), 'color', my_binwidth), envir=my_env)
                                             } else if (numeric[", j, "] == 'point graph') {
                                             assign(nui", letters[j], ",
                                             pp_1DD_scatterplot(pp, colnames(pp[i]), 'yx', pp_size = 3/ncol), envir=my_env)
                                             } else if (numeric[", j, "] == 'bw point graph') {
                                             assign(nui", letters[j], ",
                                             pp_1DD_scatterplot(pp, colnames(pp[i]), 'yx', pp_size = 3/ncol, pp_color = 'bw'), envir=my_env)
                                             } else if (numeric[", j, "] == 'color point graph') {
                                             assign(nui", letters[j], ",
                                             pp_1DD_scatterplot(pp, colnames(pp[i]), 'yx', pp_size = 3/ncol, pp_color = 'color'), envir=my_env)
                                             } else if (numeric[", j, "] == 'point graph with trend line') {
                                             assign(nui", letters[j], ",
                                             pp_1DD_scatterplot(pp, colnames(pp[i]), 'yx', pp_size = 3/ncol, pp_smooth = 'true'), envir=my_env)
                                             } else if (numeric[", j, "] == 'bw point graph with trend line') {
                                             assign(nui", letters[j], ",
                                             pp_1DD_scatterplot(pp, colnames(pp[i]), 'yx', pp_size = 3/ncol, pp_color = 'bw', pp_smooth = 'true'), envir=my_env)
                                             } else if (numeric[", j, "] == 'color point graph with trend line') {
                                             assign(nui", letters[j], ",
                                             pp_1DD_scatterplot(pp, colnames(pp[i]), 'yx', pp_size = 3/ncol, pp_color = 'color', pp_smooth = 'true'), envir=my_env)
                                             } else if (numeric[", j, "] == 'binned point graph') {
                                             assign(nui", letters[j], ",
                                             pp_1DD_binnedpointgraph(pp, colnames(pp[i]), 'yx', pp_size = 3/ncol), envir=my_env)
                                             } else if (numeric[", j, "] == 'bw binned point graph') {
                                             assign(nui", letters[j], ",
                                             pp_1DD_binnedpointgraph(pp, colnames(pp[i]), 'yx', pp_size = 3/ncol, pp_color = 'bw'), envir=my_env)
                                             } else if (numeric[", j, "] == 'color binned point graph') {
                                             assign(nui", letters[j], ",
                                             pp_1DD_binnedpointgraph(pp, colnames(pp[i]), 'yx', pp_size = 3/ncol, pp_color = 'color'), envir=my_env)
                                             } else if (numeric[", j, "] == 'binned heatmap') {
                                             assign(nui", letters[j], ",
                                             pp_1DD_heatmap(pp, colnames(pp[i]), 'yx', 'black'), envir=my_env)
                                             } else if (numeric[", j, "] == 'bw binned heatmap') {
                                             assign(nui", letters[j], ",
                                             pp_1DD_heatmap(pp, colnames(pp[i]), 'yx', 'bw'), envir=my_env)
                                             } else if (numeric[", j, "] == 'color binned heatmap') {
                                             assign(nui", letters[j], ",
                                             pp_1DD_heatmap(pp, colnames(pp[i]), 'yx', 'color'), envir=my_env)
                                             } else if (numeric[", j, "] == 'bw heatmap') {
                                             assign(nui", letters[j], ",
                                             pp_1DD_raster(pp, colnames(pp[i]), 'yx', pp_color = 'bw'), envir=my_env)
                                             } else if (numeric[", j, "] == 'color heatmap') {
                                             assign(nui", letters[j], ",
                                             pp_1DD_raster(pp, colnames(pp[i]), 'yx', pp_color = 'color'), envir=my_env)
                                             } else if (numeric[", j, "] == 'bar graph') {
                                             assign(nui", letters[j], ",
                                             pp_bargraph(pp, colnames(pp[i]), 'black', 'xy', 0.2*my_binwidth), envir=my_env)
                                             warning(warning_bargraph)
                                             } else if (numeric[", j, "] == 'bw bar graph') {
                                             assign(nui", letters[j], ",
                                             pp_bargraph(pp, colnames(pp[i]), 'bw', 'xy', 0.2*my_binwidth), envir=my_env)
                                             } else if (numeric[", j, "] == 'color bar graph') {
                                             assign(nui", letters[j], ",
                                             pp_bargraph(pp, colnames(pp[i]), 'color' , 'xy', 0.2*my_binwidth), envir=my_env)
                                             } else if (numeric[", j, "] == 'histogram') {
                                             assign(nui", letters[j], ",
                                             pp_histogram(pp, colnames(pp[i]), 'black', 1, 'bar', my_binwidth), envir=my_env)
                                             } else if (numeric[", j, "] == 'bw histogram') {
                                             assign(nui", letters[j], ",
                                             pp_histogram(pp, colnames(pp[i]), 'bw', 1, 'bar', my_binwidth), envir=my_env)
                                             } else if (numeric[", j, "] == 'color histogram') {
                                             assign(nui", letters[j], ",
                                             pp_histogram(pp, colnames(pp[i]), 'color', 1, 'bar', my_binwidth), envir=my_env)
                                             } else if (numeric[", j, "] == 'freq. polygon') {
                                             assign(nui", letters[j], ",
                                             pp_histogram(pp, colnames(pp[i]), 'black', pp_size = 1, 'line', my_binwidth), envir=my_env)
                                             } else if (numeric[", j, "] == 'density plot') {
                                             assign(nui", letters[j], ",
                                             pp_density(pp, colnames(pp[i]), pp_size = 1/ncol), envir=my_env)
                                             } else if (numeric[", j, "] == 'filled density plot') {
                                             assign(nui", letters[j], ",
                                             pp_density(pp, colnames(pp[i]), pp_size = 1/ncol, pp_color='black'), envir=my_env)
                                             } else if (numeric[", j, "] == 'violin plot') {
                                             assign(nui", letters[j], ",
                                             pp_violin(pp, colnames(pp[i]), pp_size = 1/ncol), envir=my_env)
                                             } else if (numeric[", j, "] == 'filled violin plot') {
                                             assign(nui", letters[j], ",
                                             pp_violin(pp, colnames(pp[i]), pp_size = 1/ncol, pp_color='black'), envir=my_env)
                                             } else if (numeric[", j, "] == 'box plot') {
                                             assign(nui", letters[j], ",
                                             pp_boxplot(pp, colnames(pp[i]), pp_size = 1/ncol), envir=my_env)
                                             } else if (numeric[", j, "] == '3 uniaxial') {
                                             assign(nui", letters[j], ",
                                             pp_3uniaxial(pp, colnames(pp[i]), pp_size = 4/ncol), envir=my_env)
                                             } else if (numeric[", j, "] == 'normal qq plot') {
                                             assign(nui", letters[j], ",
                                             qqplot(pp, colnames(pp[i]), pp_size = 1/ncol), envir=my_env)
                                             } else if (numeric[", j, "] == 'blank') {
                                             assign(nui", letters[j], ",
                                             blank(pp, colnames(pp[i])), envir=my_env)
                                             } else {print(warning_wrong)}")))}

      line <- eval(parse(
        text=paste0("paste0('gridExtra::grid.arrange(' ,", paste0(" nui", letters[1:ncol], collapse = ",', ',"), ",', ncol=", ncol, ")')")))
      write(line, file.path(dir, "brinton_outcomes", "wideplot.R"), append=TRUE)  # gridExtra
    }
    num.plot(data.num)
  }
  if (label == TRUE) {
    char_types <- paste0("numeric = c('", paste0(numeric[1:ncol], collapse = "', '"), "')")
    write(paste0('cat("', char_types, '")'), file.path(dir, "brinton_outcomes", "wideplot.R"), append=TRUE)
  }
}

### CHARACTER

  } else if (identical(dataclass[h], "character") == TRUE) {

if (length(data[sapply(data, is.character)])>0)
{
  data.char <- data[sapply(data, is.character)]
  rownames(data.char) <- NULL
  out = NULL

  for (i in seq_along(data.char))
  {
    for (j in 1:ncol) {eval(parse(text=paste0("chi", letters[j], " <- paste0('ch', ", i, ", '", letters[j],"')")))}
    char.plot <- function(pp)
    {
      long <- round(length(unique(pp[[i]]))/6 + 0.5, 1)
      # if (long > 20 ) {stop(warning_long)}
      for (j in 1:ncol) {eval(parse(text=paste0("
                                             if (character[", j, "] == 'line graph') {
                                             pp[[i]] <- factor(pp[[i]], levels = unique(pp[[i]]))
                                             assign(chi", letters[j], ",
                                             pp_1DD_linegraph(pp, colnames(pp[i]), 'yx', pp_size = 1/ncol), envir=my_env)
                                             } else if (character[", j, "] == 'freq. reordered line graph') {
                                             pp[[i]] <- forcats::fct_infreq(pp[[i]], ordered = TRUE)
                                             assign(chi", letters[j], ",
                                             pp_1DD_linegraph(pp, colnames(pp[i]), 'yx', pp_size = 1/ncol), envir=my_env)
                                             } else if (character[", j, "] == 'alphab. reordered line graph') {
                                             pp[[i]] <- as.character(pp[[i]])
                                             assign(chi", letters[j], ",
                                             pp_1DD_linegraph(pp, colnames(pp[i]), 'yx', pp_size = 1/ncol), envir=my_env)
                                             } else if (character[", j, "] == 'point graph') {
                                             pp[[i]] <- factor(pp[[i]], levels = unique(pp[[i]]))
                                             assign(chi", letters[j], ",
                                             pp_1DD_pointgraph(pp, colnames(pp[i]), 'yx', pp_size = 1/ncol), envir=my_env)
                                             } else if (character[", j, "] == 'freq. reordered point graph') {
                                             pp[[i]] <- forcats::fct_infreq(pp[[i]], ordered = TRUE)
                                             assign(chi", letters[j], ",
                                             pp_1DD_pointgraph(pp, colnames(pp[i]), 'yx', pp_size = 1/ncol), envir=my_env)
                                             } else if (character[", j, "] == 'alphab. reordered point graph') {
                                             pp[[i]] <- as.character(pp[[i]])
                                             assign(chi", letters[j], ",
                                             pp_1DD_pointgraph(pp, colnames(pp[i]), 'yx', pp_size = 1/ncol), envir=my_env)
                                             } else if (character[", j, "] == 'binned heatmap') {
                                             pp[[i]] <- factor(pp[[i]], levels = unique(pp[[i]]))
                                             assign(chi", letters[j], ",
                                             pp_1DD_heatmap(pp, colnames(pp[i]), 'yx', 'black'), envir=my_env)
                                             } else if (character[", j, "] == 'bw binned heatmap') {
                                             pp[[i]] <- factor(pp[[i]], levels = unique(pp[[i]]))
                                             assign(chi", letters[j], ",
                                             pp_1DD_heatmap(pp, colnames(pp[i]), 'yx', 'bw'), envir=my_env)
                                             } else if (character[", j, "] == 'color binned heatmap') {
                                             pp[[i]] <- factor(pp[[i]], levels = unique(pp[[i]]))
                                             assign(chi", letters[j], ",
                                             pp_1DD_heatmap(pp, colnames(pp[i]), 'yx', 'color'), envir=my_env)
                                             } else if (character[", j, "] == 'freq. reordered binned heatmap') {
                                             pp[[i]] <- forcats::fct_infreq(pp[[i]], ordered = TRUE)
                                             assign(chi", letters[j], ",
                                             pp_1DD_heatmap(pp, colnames(pp[i]), 'yx', 'black'), envir=my_env)
                                             } else if (character[", j, "] == 'bw freq. reordered binned heatmap') {
                                             pp[[i]] <- forcats::fct_infreq(pp[[i]], ordered = TRUE)
                                             assign(chi", letters[j], ",
                                             pp_1DD_heatmap(pp, colnames(pp[i]), 'yx', 'bw'), envir=my_env)
                                             } else if (character[", j, "] == 'color freq. reordered binned heatmap') {
                                             pp[[i]] <- forcats::fct_infreq(pp[[i]], ordered = TRUE)
                                             assign(chi", letters[j], ",
                                             pp_1DD_heatmap(pp, colnames(pp[i]), 'yx', 'color'), envir=my_env)
                                             } else if (character[", j, "] == 'alphab. reordered binned heatmap') {
                                             pp[[i]] <- as.character(pp[[i]])
                                             assign(chi", letters[j], ",
                                             pp_1DD_heatmap(pp, colnames(pp[i]), 'yx', 'black'), envir=my_env)
                                             } else if (character[", j, "] == 'bw alphab. reordered binned heatmap') {
                                             pp[[i]] <- as.character(pp[[i]])
                                             assign(chi", letters[j], ",
                                             pp_1DD_heatmap(pp, colnames(pp[i]), 'yx', 'bw'), envir=my_env)
                                             } else if (character[", j, "] == 'color alphab. reordered binned heatmap') {
                                             pp[[i]] <- as.character(pp[[i]])
                                             assign(chi", letters[j], ",
                                             pp_1DD_heatmap(pp, colnames(pp[i]), 'yx', 'color'), envir=my_env)
                                             } else if (character[", j, "] == 'tile plot') {
                                             pp[[i]] <- factor(pp[[i]], levels = unique(pp[[i]]))
                                             assign(chi", letters[j], ",
                                             pp_1DD_tileplot(pp, colnames(pp[i]), 'yx'), envir=my_env)
                                             } else if (character[", j, "] == 'freq. reordered tile plot') {
                                             pp[[i]] <- forcats::fct_infreq(pp[[i]], ordered = TRUE)
                                             assign(chi", letters[j], ",
                                             pp_1DD_tileplot(pp, colnames(pp[i]), 'yx'), envir=my_env)
                                             } else if (character[", j, "] == 'alphab. reordered tile plot') {
                                             pp[[i]] <- as.character(pp[[i]])
                                             assign(chi", letters[j], ",
                                             pp_1DD_tileplot(pp, colnames(pp[i]), 'yx'), envir=my_env)
                                             } else if (character[", j, "] == 'point-to-point graph') {
                                             pp[[i]] <- factor(pp[[i]], levels = unique(pp[[i]]))
                                             assign(chi", letters[j], ",
                                             pp_1DD_linegraph(pp, colnames(pp[i]), 'yx', pp_size = 1/ncol, pp_points = TRUE), envir=my_env)
                                             } else if (character[", j, "] == 'freq. reordered point-to-point graph') {
                                             pp[[i]] <- forcats::fct_infreq(pp[[i]], ordered = TRUE)
                                             assign(chi", letters[j], ",
                                             pp_1DD_linegraph(pp, colnames(pp[i]), 'yx', pp_size = 1/ncol, pp_points = TRUE), envir=my_env)
                                             } else if (character[", j, "] == 'alphab. reordered point-to-point graph') {
                                             pp[[i]] <- as.character(pp[[i]])
                                             assign(chi", letters[j], ",
                                             pp_1DD_linegraph(pp, colnames(pp[i]), 'yx', pp_size = 1/ncol, pp_points = TRUE), envir=my_env)
                                             } else if (character[", j, "] == 'linerange graph') {
                                             pp[[i]] <- factor(pp[[i]], levels = unique(pp[[i]]))
                                             assign(chi", letters[j], ",
                                             pp_1DD_linerange(pp, colnames(pp[i]), 'yx', pp_size = 1/ncol), envir=my_env)
                                             } else if (character[", j, "] == 'freq. reordered linerange graph') {
                                             pp[[i]] <- forcats::fct_infreq(pp[[i]], ordered = TRUE)
                                             assign(chi", letters[j], ",
                                             pp_1DD_linerange(pp, colnames(pp[i]), 'yx', pp_size = 1/ncol), envir=my_env)
                                             } else if (character[", j, "] == 'alphab. reordered linerange graph') {
                                             pp[[i]] <- as.character(pp[[i]])
                                             assign(chi", letters[j], ",
                                             pp_1DD_linerange(pp, colnames(pp[i]), 'yx', pp_size = 1/ncol), envir=my_env)
                                             } else if (character[", j, "] == 'bar graph') {
                                             pp[[i]] <- factor(pp[[i]], levels = unique(pp[[i]]))
                                             assign(chi", letters[j], ",
                                             pp_bargraph(pp, colnames(pp[i]), 'black'), envir=my_env)
                                             } else if (character[", j, "] == 'bw bar graph') {
                                             pp[[i]] <- factor(pp[[i]], levels = unique(pp[[i]]))
                                             assign(chi", letters[j], ",
                                             pp_bargraph(pp, colnames(pp[i]), 'bw'), envir=my_env)
                                             } else if (character[", j, "] == 'color bar graph') {
                                             pp[[i]] <- factor(pp[[i]], levels = unique(pp[[i]]))
                                             assign(chi", letters[j], ",
                                             pp_bargraph(pp, colnames(pp[i]), 'color'), envir=my_env)
                                             } else if (character[", j, "] == 'freq. reordered bar graph') {
                                             pp[[i]] <- forcats::fct_infreq(pp[[i]], ordered = TRUE)
                                             assign(chi", letters[j], ",
                                             pp_bargraph(subset(pp), colnames(pp[i]), 'black'), envir=my_env)
                                             } else if (character[", j, "] == 'bw freq. reordered bar graph') {
                                             pp[[i]] <- forcats::fct_infreq(pp[[i]], ordered = TRUE)
                                             assign(chi", letters[j], ",
                                             pp_bargraph(subset(pp), colnames(pp[i]), 'bw'), envir=my_env)
                                             } else if (character[", j, "] == 'color freq. reordered bar graph') {
                                             pp[[i]] <- forcats::fct_infreq(pp[[i]], ordered = TRUE)
                                             assign(chi", letters[j], ",
                                             pp_bargraph(subset(pp), colnames(pp[i]), 'color'), envir=my_env)
                                             } else if (character[", j, "] == 'alphab. reordered bar graph') {
                                             pp[[i]] <- as.character(pp[[i]])
                                             assign(chi", letters[j], ",
                                             pp_bargraph(subset(pp), colnames(pp[i]), 'black'), envir=my_env)
                                             } else if (character[", j, "] == 'bw alphab. reordered bar graph') {
                                             pp[[i]] <- as.character(pp[[i]])
                                             assign(chi", letters[j], ",
                                             pp_bargraph(subset(pp), colnames(pp[i]), 'bw'), envir=my_env)
                                             } else if (character[", j, "] == 'color alphab. reordered bar graph') {
                                             pp[[i]] <- as.character(pp[[i]])
                                             assign(chi", letters[j], ",
                                             pp_bargraph(subset(pp), colnames(pp[i]), 'color'), envir=my_env)
                                             } else if (character[", j, "] == 'blank') {
                                             assign(chi", letters[j], ",
                                             blank(pp, colnames(pp[i])), envir=my_env)
                                             } else {print(warning_wrong)}")))}
      line <- eval(parse(
      text=paste0("paste0('gridExtra::grid.arrange(' ,", paste0(" chi", letters[1:ncol], collapse = ",', ',"), ",', ncol=", ncol, ")')")))
      write(paste0("#+ character", i, ", fig.width=13, fig.height=", long), file.path(dir, "brinton_outcomes", "wideplot.R"), append=TRUE)  # gridExtra
      write(line, file.path(dir, "brinton_outcomes", "wideplot.R"), append=TRUE)  # gridExtra
      }
    char.plot(data.char)
  }
  if (label == TRUE) {
    char_types <- paste0("character = c('", paste0(character[1:ncol], collapse = "', '"), "')")
    write(paste0('cat("', char_types, '")'), file.path(dir, "brinton_outcomes", "wideplot.R"), append=TRUE)
    }
}
    } else {stop(warning_wp_dc)}
}
rmarkdown::render(file.path(dir, "brinton_outcomes", "wideplot.R", fsep = .Platform$file.sep), "html_document", envir=my_env)
pander::openFileInOS(file.path(dir, "brinton_outcomes", "wideplot.html", fsep = .Platform$file.sep))
# unlink(file.path(dir, "brinton_outcomes", fsep = .Platform$file.sep), recursive = TRUE)
  }
}



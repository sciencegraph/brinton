my_env <- new.env(parent = emptyenv())

#' Displays a matrixplot of a particular type of graphic from those included in
#' the \href{https://sciencegraph.github.io/brinton/articles/specimen2.html}{specimen} for bivariate data in a html file.
#'
#' A matrixplot is a grid of a particular type of graphic showing bivariate relationships
#' between all pairs of variables of a certain(s) type(s) in a multivariate data set.
#'
#' @seealso Specimen for \href{https://sciencegraph.github.io/brinton/articles/specimen2.html}{bivariate} data.
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
#' @param diagram Character. A specific graphic to be presented within the ones
#' considered in the 2 input variables specimen available at
#' https://sciencegraph.github.io/brinton/articles/specimen2.html.
#' @param dir Directory in which the files are stored.
#'
#' @return Cause the side-effect of creating and displaying a temporary html
#' file that includes a grid of graphics. The variables of a dataset are first
#' grouped by the type of data, then, the variables matching the classes specified
#' in the dataclass parameter, are represented in each row and/or column of the matrix.
#' @export
#'
#' @examples
#' if (interactive()) {
#' matrixplot(iris, dataclass = c("numeric","numeric"),
#' diagram="bw contour plot with data points")
#' }
matrixplot <- function(data,
                       dataclass = NULL,
                       diagram = NULL,
                       dir = tempdir())
{
  if (rmarkdown::pandoc_available("1.12.3") == FALSE) {
    print(warning_pandoc)
  }
  else if (rmarkdown::pandoc_available("1.12.3") == TRUE) {
    if (is.data.frame(data) == FALSE) {
      stop(
        "I am so sorry, but this function only works with a data.frame input!\n",
        "You have provided an object of class ",
        class(data)
      )
    }
    if(tibble::is_tibble(data) == TRUE) {
      # stop(warning_tibble)
      data <- as.data.frame(data)
    }
    if(sum(as.vector(sapply(data, is.list) == TRUE)) > 0) {
      data <- data[sapply(data, is.list) == F]
    }

    data_list <- lapply(data, FUN=remove_attr)
    data <- as.data.frame(data_list)

    ## Value validation: function's arguments
    if (is.null(dataclass) == TRUE) {
      dataclass <- c("numeric", "numeric")
    }
    if (length(dataclass) == 1) {
      dataclass <- rep(dataclass, 2)
    }
    if (length(dataclass) != 2) {
      stop(paste0("The 'dataclass'", string, "vector of length 1 or 2"))
    }
    string  <- " argument expects a character"
    if (length(dataclass) != sum(dataclass %in% dataclass_v, na.rm = TRUE))
    {
      stop(paste0(
        "The 'dataclass'",
        string,
        " which values can be : '",
        paste0(dataclass_v, collapse = "', '"),
        "'"
      ))
    }
    if (is.null(diagram) == TRUE) {
      if (identical(dataclass, c("numeric", "numeric")) == TRUE) {
        diagram <- c("color binned heatmap")
      }
      if (identical(dataclass, c("datetime", "datetime")) == TRUE) {
        diagram <- c("path graph")
      }
      if (identical(dataclass, c("ordered", "ordered")) == TRUE) {
        diagram <- c("color heatmap")
      }
      if (identical(dataclass, c("factor", "factor")) == TRUE) {
        diagram <- c("color heatmap")
      }
      if (identical(dataclass, c("numeric", "datetime")) == TRUE |
          identical(dataclass, c("datetime", "numeric")) == TRUE) {
        diagram <- c("path graph")
      }
      if (identical(dataclass, c("numeric", "ordered")) == TRUE |
          identical(dataclass, c("ordered", "numeric")) == TRUE) {
        diagram <- c("color binned heatmap")
      }
      if (identical(dataclass, c("numeric", "factor")) == TRUE |
          identical(dataclass, c("factor", "numeric")) == TRUE) {
        diagram <- c("color binned heatmap")
      }
      if (identical(dataclass, c("factor", "ordered")) == TRUE |
          identical(dataclass, c("ordered", "factor")) == TRUE) {
        diagram <- c("color heatmap")
      }
    }
    if (length(diagram) != sum(diagram %in% c(matrixplot_2num, fac.num_v1, fac.num_v2, fac.fac_v1, fac.fac_v1, ord.ord_v0, ord.ord_v1, ord.ord_v2), na.rm = TRUE))
    {
      stop(paste0(
        "The 'diagram'",
        string,
        " which values can be : '",
        paste0(cbind(matrixplot_2num, fac.num_v1), collapse = "', '"),
        "'"
      ))
    }

    my_env <- new.env()
    dir.create(file.path(dir, "brinton_outcomes", fsep = .Platform$file.sep),
               showWarnings = FALSE)
    writeLines(output_matrix,
               file.path(dir, "brinton_outcomes", "matrixplot.R"))
    write(
      paste0(
        "cat('A matrix of ",
        deparse(substitute(diagram)),
        " produced from the " ,
        deparse(substitute(dataclass)),
        " dataclasses of the ",
        sys.call()[2],
        " dataframe')"
      ),
      file = file.path(dir, "brinton_outcomes", "matrixplot.R"),
      append = TRUE
    )
if (length(data[sapply(data, is.numeric)])>1 &&
    identical(dataclass, c("numeric", "numeric")) &&
    (diagram %in% matrixplot_2num) == TRUE)
{
  data.num <- data[sapply(data, is.numeric)]
  diagram <- rep(diagram, ncol(data.num))
  out = NULL
  write(paste0("#+ numeric, fig.width=", 2.4*(ncol(data.num)-1) ,", fig.height=", 2.4*(ncol(data.num)-1) ,""), file.path(dir, "brinton_outcomes", "matrixplot.R"), append=TRUE)
  for (i in seq_along(data.num)[2:ncol(data.num)])
  {
    for (j in seq_along(data.num)[c(1:i-1)]) {eval(parse(text=paste0("nui", letters[j], " <- paste0('nu', ", i, ", '", letters[j],"')")))}
    num.plot <- function(pp)
    {
      for (j in seq_along(data.num)[c(1:i-1)]) {eval(parse(text=paste0("
                                             if (diagram[", j, "] == 'scatter plot') {
                                             assign(nui", letters[j], ",
                                             pp_scatterplot(data, colnames(pp[j]), colnames(pp[i]), pp_size = 3/ncol(data.num), 'black', FALSE), envir=my_env)
                                             } else if (diagram[", j, "] == 'scatter plot with trend line') {
                                             assign(nui", letters[j], ",
                                             pp_scatterplot(data, colnames(pp[j]), colnames(pp[i]), pp_size = 3/ncol(data.num), 'black', TRUE), envir=my_env)
                                             } else if (diagram[", j, "] == 'bw scatter plot') {
                                             assign(nui", letters[j], ",
                                             pp_scatterplot(data, colnames(pp[j]), colnames(pp[i]), pp_size = 3/ncol(data.num), 'bw', FALSE), envir=my_env)
                                             } else if (diagram[", j, "] == 'color scatter plot') {
                                             assign(nui", letters[j], ",
                                             pp_scatterplot(data, colnames(pp[j]), colnames(pp[i]), pp_size = 3/ncol(data.num), 'color', FALSE), envir=my_env)
                                             } else if (diagram[", j, "] == 'binned scatter plot') {
                                             assign(nui", letters[j], ",
                                             pp_binnedpointgraph(data, colnames(pp[j]), colnames(pp[i]), pp_size = 3/ncol(data.num), 'black'), envir=my_env)
                                             } else if (diagram[", j, "] == 'bw binned scatter plot') {
                                             assign(nui", letters[j], ",
                                             pp_binnedpointgraph(data, colnames(pp[j]), colnames(pp[i]), pp_size = 3/ncol(data.num), 'bw'), envir=my_env)
                                             } else if (diagram[", j, "] == 'color binned scatter plot') {
                                             assign(nui", letters[j], ",
                                             pp_binnedpointgraph(data, colnames(pp[j]), colnames(pp[i]), pp_size = 3/ncol(data.num), 'color'), envir=my_env)
                                             } else if (diagram[", j, "] == 'binned heatmap') {
                                             assign(nui", letters[j], ",
                                             pp_heatmap(data, colnames(pp[j]), colnames(pp[i]), 'black'), envir=my_env)
                                             } else if (diagram[", j, "] == 'bw binned heatmap') {
                                             assign(nui", letters[j], ",
                                             pp_heatmap(data, colnames(pp[j]), colnames(pp[i]), 'bw'), envir=my_env)
                                             } else if (diagram[", j, "] == 'color binned heatmap') {
                                             assign(nui", letters[j], ",
                                             pp_heatmap(data, colnames(pp[j]), colnames(pp[i]), 'color'), envir=my_env)
                                             } else if (diagram[", j, "] == 'hexagonal binned heatmap') {
                                             assign(nui", letters[j], ",
                                             pp_heatmap(data, colnames(pp[j]), colnames(pp[i]), 'black', 6), envir=my_env)
                                             } else if (diagram[", j, "] == 'bw hexagonal binned heatmap') {
                                             assign(nui", letters[j], ",
                                             pp_heatmap(data, colnames(pp[j]), colnames(pp[i]), 'bw', 6), envir=my_env)
                                             } else if (diagram[", j, "] == 'color hexagonal binned heatmap') {
                                             assign(nui", letters[j], ",
                                             pp_heatmap(data, colnames(pp[j]), colnames(pp[i]), 'color', 6), envir=my_env)
                                             } else if (diagram[", j, "] == 'bw heatmap') {
                                             assign(nui", letters[j], ",
                                             pp_raster(data, colnames(pp[j]), colnames(pp[i]), 'bw'), envir=my_env)
                                             } else if (diagram[", j, "] == 'color heatmap') {
                                             assign(nui", letters[j], ",
                                             pp_raster(data, colnames(pp[j]), colnames(pp[i]), 'color'), envir=my_env)
                                             } else if (diagram[", j, "] == 'contour plot') {
                                             assign(nui", letters[j], ",
                                             pp_contourmap(data, colnames(pp[j]), colnames(pp[i]), pp_size = 3/ncol(data.num), 'black'), envir=my_env)
                                             } else if (diagram[", j, "] == 'bw contour plot') {
                                             assign(nui", letters[j], ",
                                             pp_contourmap(data, colnames(pp[j]), colnames(pp[i]), pp_size = 3/ncol(data.num), 'bw'), envir=my_env)
                                             } else if (diagram[", j, "] == 'color contour plot') {
                                             assign(nui", letters[j], ",
                                             pp_contourmap(data, colnames(pp[j]), colnames(pp[i]), pp_size = 3/ncol(data.num), 'color'), envir=my_env)
                                             } else if (diagram[", j, "] == 'contour plot with data points') {
                                             assign(nui", letters[j], ",
                                             pp_contourmap(data, colnames(pp[j]), colnames(pp[i]), pp_size = 3/ncol(data.num), 'black', TRUE), envir=my_env)
                                             } else if (diagram[", j, "] == 'bw contour plot with data points') {
                                             assign(nui", letters[j], ",
                                             pp_contourmap(data, colnames(pp[j]), colnames(pp[i]), pp_size = 3/ncol(data.num), 'bw', TRUE), envir=my_env)
                                             } else if (diagram[", j, "] == 'color contour plot with data points') {
                                             assign(nui", letters[j], ",
                                             pp_contourmap(data, colnames(pp[j]), colnames(pp[i]), pp_size = 3/ncol(data.num), 'color', TRUE), envir=my_env)
                                             } else if (diagram[", j, "] == 'scatter plot with confidence ellipse') {
                                             assign(nui", letters[j], ",
                                             pp_scatterplot(data, colnames(pp[j]), colnames(pp[i]), pp_size = 3/ncol(data.num), 'black', FALSE, TRUE, FALSE), envir=my_env)
                                             } else if (diagram[", j, "] == 'bw scatter plot with confidence ellipse') {
                                             assign(nui", letters[j], ",
                                             pp_scatterplot(data, colnames(pp[j]), colnames(pp[i]), pp_size = 3/ncol(data.num), 'bw', FALSE, TRUE, FALSE), envir=my_env)
                                             } else if (diagram[", j, "] == 'color scatter plot with confidence ellipse') {
                                             assign(nui", letters[j], ",
                                             pp_scatterplot(data, colnames(pp[j]), colnames(pp[i]), pp_size = 3/ncol(data.num), 'color', FALSE, TRUE, FALSE), envir=my_env)
                                             } else if (diagram[", j, "] == 'scatter plot with marginal rugs') {
                                             assign(nui", letters[j], ",
                                             pp_scatterplot(data, colnames(pp[j]), colnames(pp[i]), pp_size = 3/ncol(data.num), 'black', FALSE, FALSE, TRUE), envir=my_env)
                                             } else if (diagram[", j, "] == 'bw scatter plot with marginal rugs') {
                                             assign(nui", letters[j], ",
                                             pp_scatterplot(data, colnames(pp[j]), colnames(pp[i]), pp_size = 3/ncol(data.num), 'bw', FALSE, FALSE, TRUE), envir=my_env)
                                             } else if (diagram[", j, "] == 'color scatter plot with marginal rugs') {
                                             assign(nui", letters[j], ",
                                             pp_scatterplot(data, colnames(pp[j]), colnames(pp[i]), pp_size = 3/ncol(data.num), 'color', FALSE, FALSE, TRUE), envir=my_env)
                                             } else if (diagram[", j, "] == 'path graph') {
                                             assign(nui", letters[j], ",
                                             pp_pathgraph(data, colnames(pp[j]), colnames(pp[i]), FALSE, 'black', pp_size = 3/ncol(data.num)), envir=my_env)
                                             } else if (diagram[", j, "] == 'bw path graph') {
                                             assign(nui", letters[j], ",
                                             pp_pathgraph(data, colnames(pp[j]), colnames(pp[i]), FALSE, 'bw', pp_size = 3/ncol(data.num)), envir=my_env)
                                             } else if (diagram[", j, "] == 'color path graph') {
                                             assign(nui", letters[j], ",
                                             pp_pathgraph(data, colnames(pp[j]), colnames(pp[i]), FALSE, 'color', pp_size = 3/ncol(data.num)), envir=my_env)
                                             } else if (diagram[", j, "] == 'point-to-point graph') {
                                             assign(nui", letters[j], ",
                                             pp_pathgraph(data, colnames(pp[j]), colnames(pp[i]), TRUE, 'black', pp_size = 3/ncol(data.num)), envir=my_env)
                                             } else if (diagram[", j, "] == 'bw point-to-point graph') {
                                             assign(nui", letters[j], ",
                                             pp_pathgraph(data, colnames(pp[j]), colnames(pp[i]), TRUE, 'bw', pp_size = 3/ncol(data.num)), envir=my_env)
                                             } else if (diagram[", j, "] == 'color point-to-point graph') {
                                             assign(nui", letters[j], ",
                                             pp_pathgraph(data, colnames(pp[j]), colnames(pp[i]), TRUE, 'color', pp_size = 3/ncol(data.num)), envir=my_env)
                                             }")))
        eval(parse(text=paste0("assign('blank', blank(data, colnames(pp[i])), envir=my_env)
                                             ")))
        }
      line <- eval(parse(
      text=paste0("paste0( ", paste0(" nui", letters[1:i-1], collapse = ", ' + ', "), ", '  + ", paste0(rep("plot_spacer()", ncol(data.num) - i), collapse = " + "), "', ' + ')")))
      write(line, file.path(dir, "brinton_outcomes", "matrixplot.R"), append=TRUE)
    }
    num.plot(data.num)
  }
  lines <- readLines(file.path(dir, "brinton_outcomes", "matrixplot.R"))
  writeLines(paste0(c(lines[1:length(lines)-1], substr(lines[length(lines)], 1, nchar(lines[length(lines)]) - 5), "")),
             file.path(dir, "brinton_outcomes", "matrixplot.R"))
}
    else if (length(data[sapply(data, lubridate::is.instant)])>1 &&
             identical(dataclass, c("datetime", "datetime")) &&
        (diagram %in% matrixplot_2dt) == TRUE)
    {
      data.date <- data[sapply(data, lubridate::is.instant)]
      diagram <- rep(diagram, ncol(data.date))
      out = NULL
      write(paste0("#+ datetime, fig.width=", 3*(ncol(data.date)-1) ,", fig.height=", 3*(ncol(data.date)-1) ,""), file.path(dir, "brinton_outcomes", "matrixplot.R"), append=TRUE)
      for (i in seq_along(data.date)[2:ncol(data.date)])
      {
        for (j in seq_along(data.date)[c(1:i-1)]) {eval(parse(text=paste0("dti", letters[j], " <- paste0('dt', ", i, ", '", letters[j],"')")))}
        date.plot <- function(pp)
        {
          for (j in seq_along(data.date)[c(1:i-1)]) {eval(parse(text=paste0("
                                             if (diagram[", j, "] == 'scatter plot') {
                                             assign(dti", letters[j], ",
                                             pp_scatterplot(data, colnames(pp[j]), colnames(pp[i]), pp_size = 3/ncol(data.date), 'black', FALSE), envir=my_env)
                                             } else if (diagram[", j, "] == 'scatter plot with trend line') {
                                             assign(dti", letters[j], ",
                                             pp_scatterplot(data, colnames(pp[j]), colnames(pp[i]), pp_size = 3/ncol(data.date), 'black', TRUE), envir=my_env)
                                             } else if (diagram[", j, "] == 'binned scatter plot') {
                                             assign(dti", letters[j], ",
                                             pp_binnedpointgraph(data, colnames(pp[j]), colnames(pp[i]), pp_size = 3/ncol(data.date), 'black'), envir=my_env)
                                             } else if (diagram[", j, "] == 'bw binned scatter plot') {
                                             assign(dti", letters[j], ",
                                             pp_binnedpointgraph(data, colnames(pp[j]), colnames(pp[i]), pp_size = 3/ncol(data.date), 'bw'), envir=my_env)
                                             } else if (diagram[", j, "] == 'color binned scatter plot') {
                                             assign(dti", letters[j], ",
                                             pp_binnedpointgraph(data, colnames(pp[j]), colnames(pp[i]), pp_size = 3/ncol(data.date), 'color'), envir=my_env)
                                             } else if (diagram[", j, "] == 'bw heatmap') {
                                             assign(dti", letters[j], ",
                                             pp_raster(data, colnames(pp[j]), colnames(pp[i]), 'bw'), envir=my_env)
                                             } else if (diagram[", j, "] == 'color heatmap') {
                                             assign(dti", letters[j], ",
                                             pp_raster(data, colnames(pp[j]), colnames(pp[i]), 'color'), envir=my_env)
                                             } else if (diagram[", j, "] == 'contour plot') {
                                             assign(dti", letters[j], ",
                                             pp_contourmap(data, colnames(pp[j]), colnames(pp[i]), pp_size = 3/ncol(data.date), 'black'), envir=my_env)
                                             } else if (diagram[", j, "] == 'bw contour plot') {
                                             assign(dti", letters[j], ",
                                             pp_contourmap(data, colnames(pp[j]), colnames(pp[i]), pp_size = 3/ncol(data.date), 'bw'), envir=my_env)
                                             } else if (diagram[", j, "] == 'color contour plot') {
                                             assign(dti", letters[j], ",
                                             pp_contourmap(data, colnames(pp[j]), colnames(pp[i]), pp_size = 3/ncol(data.date), 'color'), envir=my_env)
                                             } else if (diagram[", j, "] == 'contour plot with data points') {
                                             assign(dti", letters[j], ",
                                             pp_contourmap(data, colnames(pp[j]), colnames(pp[i]), pp_size = 3/ncol(data.date), 'black', TRUE), envir=my_env)
                                             } else if (diagram[", j, "] == 'path graph') {
                                             assign(dti", letters[j], ",
                                             pp_pathgraph(data, colnames(pp[j]), colnames(pp[i]), FALSE, 'black', pp_size = 3/ncol(data.date)), envir=my_env)
                                             } else if (diagram[", j, "] == 'bw path graph') {
                                             assign(dti", letters[j], ",
                                             pp_pathgraph(data, colnames(pp[j]), colnames(pp[i]), FALSE, 'bw', pp_size = 3/ncol(data.date)), envir=my_env)
                                             } else if (diagram[", j, "] == 'color path graph') {
                                             assign(dti", letters[j], ",
                                             pp_pathgraph(data, colnames(pp[j]), colnames(pp[i]), FALSE, 'color', pp_size = 3/ncol(data.date)), envir=my_env)
                                             } else if (diagram[", j, "] == 'point-to-point graph') {
                                             assign(dti", letters[j], ",
                                             pp_pathgraph(data, colnames(pp[j]), colnames(pp[i]), TRUE, 'black', pp_size = 3/ncol(data.date)), envir=my_env)
                                             } else if (diagram[", j, "] == 'bw point-to-point graph') {
                                             assign(dti", letters[j], ",
                                             pp_pathgraph(data, colnames(pp[j]), colnames(pp[i]), TRUE, 'bw', pp_size = 3/ncol(data.date)), envir=my_env)
                                             } else if (diagram[", j, "] == 'color point-to-point graph') {
                                             assign(dti", letters[j], ",
                                             pp_pathgraph(data, colnames(pp[j]), colnames(pp[i]), TRUE, 'color', pp_size = 3/ncol(data.date)), envir=my_env)
                                             }")))
            eval(parse(text=paste0("assign('blank', blank(data, colnames(pp[i])), envir=my_env)
                                             ")))
            }

          line <- eval(parse(
            text=paste0("paste0( ", paste0(" dti", letters[1:i-1], collapse = ", ' + ', "), ", '  + ", paste0(rep("plot_spacer()", ncol(data.date) - i), collapse = " + "), "', ' + ')")))
            # text=paste0("paste0('gridExtra::grid.arrange(' ,", paste0(" dti", letters[1:i-1], collapse = ",', ',"), ",', ncol=", ncol(data.date)-1, ")')")))
          write(line, file.path(dir, "brinton_outcomes", "matrixplot.R"), append=TRUE)
        }
        date.plot(data.date)
      }
      lines <- readLines(file.path(dir, "brinton_outcomes", "matrixplot.R"))
      writeLines(paste0(c(lines[1:length(lines)-1], substr(lines[length(lines)], 1, nchar(lines[length(lines)]) - 5), "")),
                 file.path(dir, "brinton_outcomes", "matrixplot.R"))
    }
    else if (length(data[sapply(data, is.numeric)])>0 &&
             length(data[sapply(data, lubridate::is.instant)])>0 &&
             any(dataclass %in% "datetime") &&
             any(dataclass %in% "numeric") &&
             (diagram %in% datenum_v) == TRUE)
    {
      data.date <- data[sapply(data, lubridate::is.instant)]
      data.num <- data[sapply(data, is.numeric)]
      diagram <- rep(diagram, ncol(data.date))
      out = NULL
      for (i in seq_along(data.num))
      {
        write(paste0("#+ fig.width=", 3*ncol(data.date) ,", fig.height= 2.4"), file.path(dir, "brinton_outcomes", "matrixplot.R"), append=TRUE)
        for (j in seq_along(data.date)) {eval(parse(text=paste0("dni", letters[j], " <- paste0('dn', ", i, ", '", letters[j],"')")))}
        datenum.plot <- function(pp)
        {
          for (j in seq_along(data.date)) {eval(parse(text=paste0("
                                             if (diagram[", j, "] == 'scatter plot') {
                                             assign(dni", letters[j], ",
                                             pp_scatterplot(data, colnames(pp[sapply(pp, lubridate::is.instant)][j]), colnames(pp[sapply(pp, is.numeric)][i]), pp_size = 1/ncol(data.num), 'black', FALSE), envir=my_env)
                                             } else if (diagram[", j, "] == 'scatter plot with trend line') {
                                             assign(dni", letters[j], ",
                                             pp_scatterplot(data, colnames(pp[sapply(pp, lubridate::is.instant)][j]), colnames(pp[sapply(pp, is.numeric)][i]), pp_size = 1/ncol(data.num), 'black', TRUE), envir=my_env)
                                             } else if (diagram[", j, "] == 'binned scatter plot') {
                                             assign(dni", letters[j], ",
                                             pp_binnedpointgraph(data, colnames(pp[sapply(pp, lubridate::is.instant)][j]), colnames(pp[sapply(pp, is.numeric)][i]), pp_size = 1/ncol(data.num), 'black'), envir=my_env)
                                             } else if (diagram[", j, "] == 'bw binned scatter plot') {
                                             assign(dni", letters[j], ",
                                             pp_binnedpointgraph(data, colnames(pp[sapply(pp, lubridate::is.instant)][j]), colnames(pp[sapply(pp, is.numeric)][i]), pp_size = 1/ncol(data.num), 'bw'), envir=my_env)
                                             } else if (diagram[", j, "] == 'color binned scatter plot') {
                                             assign(dni", letters[j], ",
                                             pp_binnedpointgraph(data, colnames(pp[sapply(pp, lubridate::is.instant)][j]), colnames(pp[sapply(pp, is.numeric)][i]), pp_size = 1/ncol(data.num), 'color'), envir=my_env)
                                             } else if (diagram[", j, "] == 'bw heatmap') {
                                             assign(dni", letters[j], ",
                                             pp_raster(data, colnames(pp[sapply(pp, lubridate::is.instant)][j]), colnames(pp[sapply(pp, is.numeric)][i]), 'bw'), envir=my_env)
                                             } else if (diagram[", j, "] == 'color heatmap') {
                                             assign(dni", letters[j], ",
                                             pp_raster(data, colnames(pp[sapply(pp, lubridate::is.instant)][j]), colnames(pp[sapply(pp, is.numeric)][i]), 'color'), envir=my_env)
                                             } else if (diagram[", j, "] == 'contour plot with data points') {
                                             assign(dni", letters[j], ",
                                             pp_contourmap(data, colnames(pp[sapply(pp, lubridate::is.instant)][j]), colnames(pp[sapply(pp, is.numeric)][i]), pp_size = 1/ncol(data.num), 'black', TRUE), envir=my_env)
                                             } else if (diagram[", j, "] == 'path graph') {
                                             assign(dni", letters[j], ",
                                             pp_pathgraph(data, colnames(pp[sapply(pp, lubridate::is.instant)][j]), colnames(pp[sapply(pp, is.numeric)][i]), FALSE, 'black', pp_size = 0.5/ncol(data.num)), envir=my_env)
                                             } else if (diagram[", j, "] == 'bw path graph') {
                                             assign(dni", letters[j], ",
                                             pp_pathgraph(data, colnames(pp[sapply(pp, lubridate::is.instant)][j]), colnames(pp[sapply(pp, is.numeric)][i]), FALSE, 'bw', pp_size = 0.5/ncol(data.num)), envir=my_env)
                                             } else if (diagram[", j, "] == 'color path graph') {
                                             assign(dni", letters[j], ",
                                             pp_pathgraph(data, colnames(pp[sapply(pp, lubridate::is.instant)][j]), colnames(pp[sapply(pp, is.numeric)][i]), FALSE, 'color', pp_size = 0.5/ncol(data.num)), envir=my_env)
                                             } else if (diagram[", j, "] == 'point-to-point graph') {
                                             assign(dni", letters[j], ",
                                             pp_pathgraph(data, colnames(pp[sapply(pp, lubridate::is.instant)][j]), colnames(pp[sapply(pp, is.numeric)][i]), TRUE, 'black', pp_size = 1/ncol(data.num)), envir=my_env)
                                             } else if (diagram[", j, "] == 'bw point-to-point graph') {
                                             assign(dni", letters[j], ",
                                             pp_pathgraph(data, colnames(pp[sapply(pp, lubridate::is.instant)][j]), colnames(pp[sapply(pp, is.numeric)][i]), TRUE, 'bw', pp_size = 1/ncol(data.num)), envir=my_env)
                                             } else if (diagram[", j, "] == 'color point-to-point graph') {
                                             assign(dni", letters[j], ",
                                             pp_pathgraph(data, colnames(pp[sapply(pp, lubridate::is.instant)][j]), colnames(pp[sapply(pp, is.numeric)][i]), TRUE, 'color', pp_size = 1/ncol(data.num)), envir=my_env)
                                             }")))}

          line <- eval(parse(
            text=paste0("paste0( ", paste0(" dni", letters[1:ncol(data.date)], collapse = ", ' + ', "), ", '  + patchwork::plot_layout(ncol = ", ncol(data.date), ")')")))
            # text=paste0("paste0('gridExtra::grid.arrange(' ,", paste0(" dni", letters[1:ncol(data.date)], collapse = ",', ',"), ",', ncol=", ncol(data.date), ")')")))
          write(line, file.path(dir, "brinton_outcomes", "matrixplot.R"), append=TRUE)
          write("#' <br>", file.path(dir, "brinton_outcomes", "matrixplot.R"), append=TRUE)
        }
        datenum.plot(cbind(data.num, data.date))
      }
    }
    else if (length(data[sapply(data, is.numeric)])>0 &&
             length(data[sapply(data, is.factor)][!sapply(data[sapply(data, is.factor)], is.ordered)])>0 &&
             any(dataclass %in% "numeric") &&
             any(dataclass %in% c("factor")) &&
             (diagram %in% matrixplot_fac.num_v1) == TRUE)
{
  data.num <- data[sapply(data, is.numeric)]
  data.fac <- data[sapply(data, is.factor)][!sapply(data[sapply(data, is.factor)], is.ordered)]
  rownames(data.fac) <- NULL
  diagram <- rep(diagram, ncol(data.num))
  out = NULL
  for (i in seq_along(data.fac))
  {
    for (j in seq_along(data.num)) {eval(parse(text=paste0("fni", letters[j], " <- paste0('fn', ", i, ", '", letters[j],"')")))}
    facnum.plot <- function(pp)
    {
      long <- round(length(unique(pp[sapply(pp, is.factor)][[i]]))/4 + 0.6, 1)
      for (j in seq_along(data.num)) {eval(parse(text=paste0("
                                             if (diagram[", j, "] == 'path graph') {
                                             assign(fni", letters[j], ",
                                             pp_basicgraph(data, colnames(pp[sapply(pp, is.numeric)][j]), colnames(pp[sapply(pp, is.factor)][i]), pp_size = 1/ncol(data.num), 'line'), envir=my_env)
                                             } else if (diagram[", j, "] == 'point graph') {
                                             assign(fni", letters[j], ",
                                             pp_basicgraph(data, colnames(pp[sapply(pp, is.numeric)][j]), colnames(pp[sapply(pp, is.factor)][i]), pp_size = 1/ncol(data.num), 'point'), envir=my_env)
                                             } else if (diagram[", j, "] == 'tile plot') {
                                             assign(fni", letters[j], ",
                                             pp_basicgraph(data, colnames(pp[sapply(pp, is.numeric)][j]), colnames(pp[sapply(pp, is.factor)][i]), pp_size = 1/ncol(data.num), 'tile'), envir=my_env)
                                             } else if (diagram[", j, "] == 'binned heatmap') {
                                             assign(fni", letters[j], ",
                                             pp_basicgraph(data, colnames(pp[sapply(pp, is.numeric)][j]), colnames(pp[sapply(pp, is.factor)][i]), pp_size = 1/ncol(data.num), 'bin', 'black'), envir=my_env)
                                             } else if (diagram[", j, "] == 'bw binned heatmap') {
                                             assign(fni", letters[j], ",
                                             pp_basicgraph(data, colnames(pp[sapply(pp, is.numeric)][j]), colnames(pp[sapply(pp, is.factor)][i]), pp_size = 1/ncol(data.num), 'bin', 'bw'), envir=my_env)
                                             } else if (diagram[", j, "] == 'color binned heatmap') {
                                             assign(fni", letters[j], ",
                                             pp_basicgraph(data, colnames(pp[sapply(pp, is.numeric)][j]), colnames(pp[sapply(pp, is.factor)][i]), pp_size = 1/ncol(data.num), 'bin', 'color'), envir=my_env)
                                             } else if (diagram[", j, "] == 'violin plot') {
                                             assign(fni", letters[j], ",
                                             pp_basicgraph(data, colnames(pp[sapply(pp, is.factor)][i]), colnames(pp[sapply(pp, is.numeric)][j]), pp_size = 1/ncol(data.num), 'violin'), envir=my_env)
                                             } else if (diagram[", j, "] == 'filled violin plot') {
                                             assign(fni", letters[j], ",
                                             pp_basicgraph(data, colnames(pp[sapply(pp, is.factor)][i]), colnames(pp[sapply(pp, is.numeric)][j]), pp_size = 1/ncol(data.num), 'violin filled'), envir=my_env)
                                             } else if (diagram[", j, "] == 'box plot') {
                                             assign(fni", letters[j], ",
                                             pp_basicgraph(data, colnames(pp[sapply(pp, is.factor)][i]), colnames(pp[sapply(pp, is.numeric)][j]), pp_size = 1/ncol(data.num), 'box'), envir=my_env)
                                             }")))}

      line <- eval(parse(
        text=paste0("paste0( ", paste0(" fni", letters[1:ncol(data.num)], collapse = ", ' + ', "), ", ' + patchwork::plot_layout(ncol = ", ncol(data.num), ")')")))
        # text=paste0("paste0('gridExtra::grid.arrange(' ,", paste0(" fni", letters[1:ncol(data.num)], collapse = ",', ',"), ",', ncol=", ncol(data.num), ")')")))
      write(paste0("#+ fig.width=", 2.4*ncol(data.num) ,", fig.height=", long), file.path(dir, "brinton_outcomes", "matrixplot.R"), append=TRUE) # gridExtra
      write(line, file.path(dir, "brinton_outcomes", "matrixplot.R"), append=TRUE)
      write("#' <br>", file.path(dir, "brinton_outcomes", "matrixplot.R"), append=TRUE)
    }
    facnum.plot(cbind(data.num, data.fac))
  }
    }
    else if (length(data[sapply(data, is.numeric)])>0 &&
             length(data[sapply(data, is.factor)][!sapply(data[sapply(data, is.factor)], is.ordered)])>0 &&
             any(dataclass %in% "numeric") &&
             any(dataclass %in% c("factor")) &&
             (diagram %in% matrixplot_fac.num_v2) == TRUE)
    {
      data.num <- data[sapply(data, is.numeric)]
      data.fac <- data[sapply(data, is.factor)][!sapply(data[sapply(data, is.factor)], is.ordered)]
      rownames(data.fac) <- NULL
      diagram <- rep(diagram, ncol(data.num))
      out = NULL
      for (i in seq_along(data.fac))
      {
        write(paste0("#+ fig.width=", 2.4*ncol(data.num) ,", fig.height=2.4"), file.path(dir, "brinton_outcomes", "matrixplot.R"), append=TRUE)
        for (j in seq_along(data.num)) {eval(parse(text=paste0("fni", letters[j], " <- paste0('fn', ", i, ", '", letters[j],"')")))}
        facnum.plot <- function(pp)
        {
          long <- round(length(unique(pp[sapply(pp, is.factor)][[i]]))/6 + 1.4, 1)
          for (j in seq_along(data.num)) {eval(parse(text=paste0("
                                             if (diagram[", j, "] == 'bw stacked histogram') {
                                             assign(fni", letters[j], ",
                                             pp_histogram2(data, colnames(pp[sapply(pp, is.numeric)][j]), colnames(pp[sapply(pp, is.factor)][i])), envir=my_env)
                                             } else if (diagram[", j, "] == 'color stacked histogram') {
                                             assign(fni", letters[j], ",
                                             pp_histogram2(data, colnames(pp[sapply(pp, is.numeric)][j]), colnames(pp[sapply(pp, is.factor)][i]), pp_color = 'color'), envir=my_env)
                                             } else if (diagram[", j, "] == 'bw 100% stacked histogram') {
                                             assign(fni", letters[j], ",
                                             pp_histogram2(data, colnames(pp[sapply(pp, is.numeric)][j]), colnames(pp[sapply(pp, is.factor)][i]), pp_position = 'fill'), envir=my_env)
                                             } else if (diagram[", j, "] == 'color 100% stacked histogram') {
                                             assign(fni", letters[j], ",
                                             pp_histogram2(data, colnames(pp[sapply(pp, is.numeric)][j]), colnames(pp[sapply(pp, is.factor)][i]), pp_position = 'fill', pp_color='color'), envir=my_env)
                                             } else if (diagram[", j, "] == 'density plot') {
                                             assign(fni", letters[j], ",
                                             pp_density2(data, colnames(pp[sapply(pp, is.numeric)][j]), colnames(pp[sapply(pp, is.factor)][i]), pp_size = 0.5, 'line', 'black'), envir=my_env)
                                             } else if (diagram[", j, "] == 'color density plot') {
                                             assign(fni", letters[j], ",
                                             pp_density2(data, colnames(pp[sapply(pp, is.numeric)][j]), colnames(pp[sapply(pp, is.factor)][i]), pp_size = 0.5, 'line', 'color'), envir=my_env)
                                             } else if (diagram[", j, "] == 'filled density plot') {
                                             assign(fni", letters[j], ",
                                             pp_density2(data, colnames(pp[sapply(pp, is.factor)][i]), colnames(pp[sapply(pp, is.numeric)][j]), pp_size = 0.5, 'area', 'bw'), envir=my_env)
                                             } else if (diagram[", j, "] == 'color filled density plot') {
                                             assign(fni", letters[j], ",
                                             pp_density2(data, colnames(pp[sapply(pp, is.factor)][i]), colnames(pp[sapply(pp, is.numeric)][j]), pp_size = 0.5, 'area', 'color'), envir=my_env)
                                             }")))}

          line <- eval(parse(
            text=paste0("paste0( ", paste0(" fni", letters[1:ncol(data.num)], collapse = ", ' + ', "), ", ' + patchwork::plot_layout(ncol = ", ncol(data.num), ")')")))
          write(paste0(line, " + patchwork::plot_layout(guides = 'collect') & theme(legend.position = 'bottom', legend.key.size = unit(0.5,'line'), legend.title = element_blank())"), file.path(dir, "brinton_outcomes", "matrixplot.R"), append=TRUE)
          write("#' <br>", file.path(dir, "brinton_outcomes", "matrixplot.R"), append=TRUE)
        }
        facnum.plot(cbind(data.num, data.fac))
      }
    }
    else if (length(data[sapply(data, is.numeric)])>0 &&
           length(data[sapply(data, is.ordered)])>0 &&
           any(dataclass %in% "numeric") &&
           any(dataclass %in% c("ordered")) &&
           (diagram %in% matrixplot_ord.num_v1) == TRUE)
  {
    data.num <- data[sapply(data, is.numeric)]
    data.ord <- data[sapply(data, is.ordered)]
    rownames(data.ord) <- NULL
    diagram <- rep(diagram, ncol(data.num))
    out = NULL
    for (i in seq_along(data.ord))
    {
      for (j in seq_along(data.num)) {eval(parse(text=paste0("oni", letters[j], " <- paste0('on', ", i, ", '", letters[j],"')")))}
      ordnum.plot <- function(pp)
      {
        long <- round(length(unique(pp[sapply(pp, is.ordered)][[i]]))/4 + 0.6, 1)
        for (j in seq_along(data.num)) {eval(parse(text=paste0("
                                             if (diagram[", j, "] == 'path graph') {
                                             assign(oni", letters[j], ",
                                             pp_basicgraph(data, colnames(pp[sapply(pp, is.numeric)][j]), colnames(pp[sapply(pp, is.factor)][i]), pp_size = 1/ncol(data.num), 'line'), envir=my_env)
                                             } else if (diagram[", j, "] == 'point graph') {
                                             assign(oni", letters[j], ",
                                             pp_basicgraph(data, colnames(pp[sapply(pp, is.numeric)][j]), colnames(pp[sapply(pp, is.factor)][i]), pp_size = 1/ncol(data.num), 'point'), envir=my_env)
                                             } else if (diagram[", j, "] == 'tile plot') {
                                             assign(oni", letters[j], ",
                                             pp_basicgraph(data, colnames(pp[sapply(pp, is.numeric)][j]), colnames(pp[sapply(pp, is.factor)][i]), pp_size = 1/ncol(data.num), 'tile'), envir=my_env)
                                             } else if (diagram[", j, "] == 'binned heatmap') {
                                             assign(oni", letters[j], ",
                                             pp_basicgraph(data, colnames(pp[sapply(pp, is.numeric)][j]), colnames(pp[sapply(pp, is.factor)][i]), pp_size = 1/ncol(data.num), 'bin', 'black'), envir=my_env)
                                             } else if (diagram[", j, "] == 'bw binned heatmap') {
                                             assign(oni", letters[j], ",
                                             pp_basicgraph(data, colnames(pp[sapply(pp, is.numeric)][j]), colnames(pp[sapply(pp, is.factor)][i]), pp_size = 1/ncol(data.num), 'bin', 'bw'), envir=my_env)
                                             } else if (diagram[", j, "] == 'color binned heatmap') {
                                             assign(oni", letters[j], ",
                                             pp_basicgraph(data, colnames(pp[sapply(pp, is.numeric)][j]), colnames(pp[sapply(pp, is.factor)][i]), pp_size = 1/ncol(data.num), 'bin', 'color'), envir=my_env)
                                             } else if (diagram[", j, "] == 'violin plot') {
                                             assign(oni", letters[j], ",
                                             pp_basicgraph(data, colnames(pp[sapply(pp, is.factor)][i]), colnames(pp[sapply(pp, is.numeric)][j]), pp_size = 1/ncol(data.num), 'violin'), envir=my_env)
                                             } else if (diagram[", j, "] == 'filled violin plot') {
                                             assign(oni", letters[j], ",
                                             pp_basicgraph(data, colnames(pp[sapply(pp, is.factor)][i]), colnames(pp[sapply(pp, is.numeric)][j]), pp_size = 1/ncol(data.num), 'violin filled'), envir=my_env)
                                             } else if (diagram[", j, "] == 'box plot') {
                                             assign(oni", letters[j], ",
                                             pp_basicgraph(data, colnames(pp[sapply(pp, is.factor)][i]), colnames(pp[sapply(pp, is.numeric)][j]), pp_size = 1/ncol(data.num), 'box'), envir=my_env)
                                             }")))}

        line <- eval(parse(
          # text=paste0("paste0('gridExtra::grid.arrange(' ,", paste0(" oni", letters[1:ncol(data.num)], collapse = ",', ',"), ",', ncol=", ncol(data.num), ")')")))
        text=paste0("paste0( ", paste0(" oni", letters[1:ncol(data.num)], collapse = ", ' + ', "), ", ' + patchwork::plot_layout(ncol = ", ncol(data.num), ")')")))
        write(paste0("#+ fig.width=", 2.4*ncol(data.num) ,", fig.height=", long), file.path(dir, "brinton_outcomes", "matrixplot.R"), append=TRUE) # gridExtra
        write(line, file.path(dir, "brinton_outcomes", "matrixplot.R"), append=TRUE)
        write("#' <br>", file.path(dir, "brinton_outcomes", "matrixplot.R"), append=TRUE)
      }
      ordnum.plot(cbind(data.num, data.ord))
    }
  }
    else if (length(data[sapply(data, is.numeric)])>0 &&
             length(data[sapply(data, is.ordered)])>0 &&
             any(dataclass %in% "numeric") &&
             any(dataclass %in% c("ordered")) &&
             (diagram %in% matrixplot_ord.num_v2) == TRUE)
    {
      data.num <- data[sapply(data, is.numeric)]
      data.ord <- data[sapply(data, is.ordered)]
      rownames(data.ord) <- NULL
      diagram <- rep(diagram, ncol(data.num))
      out = NULL
      for (i in seq_along(data.ord))
      {
        write(paste0("#+ fig.width=", 2.4*ncol(data.num) ,", fig.height=2.4"), file.path(dir, "brinton_outcomes", "matrixplot.R"), append=TRUE)
        for (j in seq_along(data.num)) {eval(parse(text=paste0("oni", letters[j], " <- paste0('on', ", i, ", '", letters[j],"')")))}
        ordnum.plot <- function(pp)
        {
          long <- round(length(unique(pp[sapply(pp, is.ordered)][[i]]))/6 + 0.6, 1)
          for (j in seq_along(data.num)) {eval(parse(text=paste0("
                                             if (diagram[", j, "] == 'bw stacked histogram') {
                                             assign(oni", letters[j], ",
                                             pp_histogram2(data, colnames(pp[sapply(pp, is.numeric)][j]), colnames(pp[sapply(pp, is.factor)][i])), envir=my_env)
                                             } else if (diagram[", j, "] == 'color stacked histogram') {
                                             assign(oni", letters[j], ",
                                             pp_histogram2(data, colnames(pp[sapply(pp, is.numeric)][j]), colnames(pp[sapply(pp, is.factor)][i]), pp_color = 'color', pp_scale = 'ordinal'), envir=my_env)
                                             } else if (diagram[", j, "] == 'bw 100% stacked histogram') {
                                             assign(oni", letters[j], ",
                                             pp_histogram2(data, colnames(pp[sapply(pp, is.numeric)][j]), colnames(pp[sapply(pp, is.factor)][i]), pp_position = 'fill'), envir=my_env)
                                             } else if (diagram[", j, "] == 'color 100% stacked histogram') {
                                             assign(oni", letters[j], ",
                                             pp_histogram2(data, colnames(pp[sapply(pp, is.numeric)][j]), colnames(pp[sapply(pp, is.factor)][i]), pp_position = 'fill', pp_color='color', pp_scale = 'ordinal'), envir=my_env)
                                             } else if (diagram[", j, "] == 'density plot') {
                                             assign(oni", letters[j], ",
                                             pp_density2(data, colnames(pp[sapply(pp, is.numeric)][j]), colnames(pp[sapply(pp, is.factor)][i]), pp_size = 0.5, 'line', 'black'), envir=my_env)
                                             } else if (diagram[", j, "] == 'bw density plot') {
                                             assign(oni", letters[j], ",
                                             pp_density2(data, colnames(pp[sapply(pp, is.numeric)][j]), colnames(pp[sapply(pp, is.factor)][i]), pp_size = 0.5, 'line', 'bw'), envir=my_env)
                                             } else if (diagram[", j, "] == 'color density plot') {
                                             assign(oni", letters[j], ",
                                             pp_density2(data, colnames(pp[sapply(pp, is.numeric)][j]), colnames(pp[sapply(pp, is.factor)][i]), pp_size = 0.5, 'line', 'viridis'), envir=my_env)
                                             } else if (diagram[", j, "] == 'bw filled density plot') {
                                             assign(oni", letters[j], ",
                                             pp_density2(data, colnames(pp[sapply(pp, is.factor)][i]), colnames(pp[sapply(pp, is.numeric)][j]), pp_size = 0.5, 'area', 'bw'), envir=my_env)
                                             } else if (diagram[", j, "] == 'color filled density plot') {
                                             assign(oni", letters[j], ",
                                             pp_density2(data, colnames(pp[sapply(pp, is.factor)][i]), colnames(pp[sapply(pp, is.numeric)][j]), pp_size = 0.5, 'area', 'viridis'), envir=my_env)
                                             }")))}

          line <- eval(parse(
            text=paste0("paste0( ", paste0(" oni", letters[1:ncol(data.num)], collapse = ", ' + ', "), ", ' + patchwork::plot_layout(ncol = ", ncol(data.num), ")')")))
            # text=paste0("paste0('gridExtra::grid.arrange(' ,", paste0(" oni", letters[1:ncol(data.num)], collapse = ",', ',"), ",', ncol=", ncol(data.num), ")')")))
          # write(paste0("#+ fig.width=", 2.4*ncol(data.num) ,", fig.height=", long), file.path(dir, "brinton_outcomes", "matrixplot.R"), append=TRUE) # gridExtra
          # write(line, file.path(dir, "brinton_outcomes", "matrixplot.R"), append=TRUE)
          write(paste0(line, " + patchwork::plot_layout(guides = 'collect') & theme(legend.position = 'bottom', legend.key.size = unit(0.5,'line'), legend.title = element_blank())"), file.path(dir, "brinton_outcomes", "matrixplot.R"), append=TRUE)
          write("#' <br>", file.path(dir, "brinton_outcomes", "matrixplot.R"), append=TRUE)
        }
        ordnum.plot(cbind(data.num, data.ord))
      }
    }
    else if (length(data[sapply(data, is.ordered)])>1 &&
             identical(dataclass, c("ordered", "ordered")) &&
             (diagram %in% ord.ord_v0) == TRUE)
  {
    data.ord <- data[sapply(data, is.ordered)]
    diagram <- rep(diagram, ncol(data.ord))
    out = NULL
    for (i in seq_along(data.ord)[2:ncol(data.ord)])
    {
      GAheight <- length(unique(data.ord[,(i)]))/5+0.5
      write(paste0("#+ fig.width=", 4*(ncol(data.ord)-1) ,", fig.height=", GAheight), file.path(dir, "brinton_outcomes", "matrixplot.R"), append=TRUE) # gridExtra
      for (j in seq_along(data.ord)[c(1:i-1)]) {eval(parse(text=paste0("ooi", letters[j], " <- paste0('oo', ", i, ", '", letters[j],"')")))}
      ord.plot <- function(pp)
      {
        for (j in seq_along(data.ord)[c(1:i-1)]) {eval(parse(text=paste0("
                                             if (diagram[", j, "] == 'bw stacked bar graph') {
                                             assign(ooi", letters[j], ",
                                             pp_stackedbar(data, colnames(pp[i]), colnames(pp[j]), 'bw', 'stack', 'ordinal'), envir=my_env)
                                             } else if (diagram[", j, "] == 'color stacked bar graph') {
                                             assign(ooi", letters[j], ",
                                             pp_stackedbar(data, colnames(pp[i]), colnames(pp[j]), 'color', 'stack', 'ordinal'), envir=my_env)
                                             } else if (diagram[", j, "] == 'bw 100% stacked bar graph') {
                                             assign(ooi", letters[j], ",
                                             pp_stackedbar(data, colnames(pp[i]), colnames(pp[j]), 'bw', 'fill', 'ordinal'), envir=my_env)
                                             } else if (diagram[", j, "] == 'color 100% stacked bar graph') {
                                             assign(ooi", letters[j], ",
                                             pp_stackedbar(data, colnames(pp[i]), colnames(pp[j]), 'color', 'fill', 'ordinal'), envir=my_env)
                                             }")))
          eval(parse(text=paste0("assign('blank', blank(data, colnames(pp[i])), envir=my_env)
                                             ")))
          }

        line <- eval(parse(
          # text=paste0("paste0( ", paste0(" ooi", letters[1:i-1], collapse = ", ' + ', "), ", '", paste0(rep(" + plot_spacer()", ncol(data.ord) - i), collapse = ""), "')")))
          # write(paste(line, " + plot_layout(widths = unit(rep(5,", ncol(data.ord), "), c(rep('cm',", ncol(data.ord) -1,"), 'null')))"), file.path(dir, "brinton_outcomes", "matrixplot.R"), append=TRUE)
          text=paste0("paste0('gridExtra::grid.arrange(' ,", paste0(" ooi", letters[1:i-1], collapse = ",', ',"), ",', ncol=", ncol(data.ord)-1, ")')")))
          write(line, file.path(dir, "brinton_outcomes", "matrixplot.R"), append=TRUE)
      }
      ord.plot(data.ord)
    }
    # lines <- readLines(file.path(dir, "brinton_outcomes", "matrixplot.R"))
    # writeLines(paste0(c(lines[1:length(lines)-1], substr(lines[length(lines)], 1, nchar(lines[length(lines)])), "")),
    #            file.path(dir, "brinton_outcomes", "matrixplot.R"))
    }
    else if (length(data[sapply(data, is.ordered)])>1 &&
             identical(dataclass, c("ordered", "ordered")) &&
             (diagram %in% ord.ord_v1) == TRUE)
    {
      data.ord <- data[sapply(data, is.ordered)]
      data.ord <- data.ord[, c(ncol(data.ord), 1:ncol(data.ord)-1)]
      diagram <- rep(diagram, ncol(data.ord))
      out = NULL
      for (i in seq_along(data.ord)[2:ncol(data.ord)])
      {
        GAheight <- length(unique(data.ord[,(i)]))/5+0.5
        write(paste0("#+ fig.width=", 4*(ncol(data.ord)-1) ,", fig.height=", GAheight), file.path(dir, "brinton_outcomes", "matrixplot.R"), append=TRUE) # gridExtra
        for (j in seq_along(data.ord)[c(1:i-1)]) {eval(parse(text=paste0("ooi", letters[j], " <- paste0('oo', ", i, ", '", letters[j],"')")))}
        ord.plot <- function(pp)
        {
          for (j in seq_along(data.ord)[c(1:i-1)]) {eval(parse(text=paste0("
                                             if (diagram[", j, "] == 'transposed bw stacked bar graph') {
                                             assign(ooi", letters[j], ",
                                             pp_stackedbar(data, colnames(pp[i]), colnames(pp[j]), 'bw', 'stack', 'ordinal'), envir=my_env)
                                             } else if (diagram[", j, "] == 'transposed color stacked bar graph') {
                                             assign(ooi", letters[j], ",
                                             pp_stackedbar(data, colnames(pp[i]), colnames(pp[j]), 'color', 'stack', 'ordinal'), envir=my_env)
                                             } else if (diagram[", j, "] == 'transposed bw 100% stacked bar graph') {
                                             assign(ooi", letters[j], ",
                                             pp_stackedbar(data, colnames(pp[i]), colnames(pp[j]), 'bw', 'fill', 'ordinal'), envir=my_env)
                                             } else if (diagram[", j, "] == 'transposed color 100% stacked bar graph') {
                                             assign(ooi", letters[j], ",
                                             pp_stackedbar(data, colnames(pp[i]), colnames(pp[j]), 'color', 'fill', 'ordinal'), envir=my_env)
                                             }")))
            eval(parse(text=paste0("assign('blank', blank(data, colnames(pp[i])), envir=my_env)
                                             ")))
          }

          line <- eval(parse(
            text=paste0("paste0('gridExtra::grid.arrange(' ,", paste0(" ooi", letters[1:i-1], collapse = ",', ',"), ",', ncol=", ncol(data.ord)-1, ")')")))
          write(line, file.path(dir, "brinton_outcomes", "matrixplot.R"), append=TRUE)
        }
        ord.plot(data.ord)
      }
    }
    else if (length(data[sapply(data, is.ordered)])>1 &&
             identical(dataclass, c("ordered", "ordered")) &&
             (diagram %in% ord.ord_v2) == TRUE)
    {
      data.ord <- data[sapply(data, is.ordered)]
      diagram <- rep(diagram, ncol(data.ord))
      out = NULL
      for (i in seq_along(data.ord)[2:ncol(data.ord)])
      {
        GAheight <- length(unique(data.ord[,(i)]))/5+1.5
        write(paste0("#+ fig.width=", 4*(ncol(data.ord)-1) ,", fig.height=", GAheight), file.path(dir, "brinton_outcomes", "matrixplot.R"), append=TRUE) # gridExtra
        for (j in seq_along(data.ord)[c(1:i-1)]) {eval(parse(text=paste0("ooi", letters[j], " <- paste0('oo', ", i, ", '", letters[j],"')")))}
        ord.plot <- function(pp)
        {
          for (j in seq_along(data.ord)[c(1:i-1)]) {eval(parse(text=paste0("
                                             if (diagram[", j, "] == 'bw heatmap') {
                                             assign(ooi", letters[j], ",
                                             pp_contingency(data, colnames(pp[j]), colnames(pp[i]), 'bw', 'observed'), envir=my_env)
                                             } else if (diagram[", j, "] == 'color heatmap') {
                                             assign(ooi", letters[j], ",
                                             pp_contingency(data, colnames(pp[j]), colnames(pp[i]), 'color', 'observed'), envir=my_env)
                                             } else if (diagram[", j, "] == 'color residuals heatmap') {
                                             assign(ooi", letters[j], ",
                                             pp_contingency(data, colnames(pp[j]), colnames(pp[i]), 'color', 'residuals'), envir=my_env)
                                             } else if (diagram[", j, "] == 'bw contribution to x2 heatmap') {
                                             assign(ooi", letters[j], ",
                                             pp_contingency(data, colnames(pp[j]), colnames(pp[i]), 'bw', 'contrib'), envir=my_env)
                                             } else if (diagram[", j, "] == 'color contribution to x2 heatmap') {
                                             assign(ooi", letters[j], ",
                                             pp_contingency(data, colnames(pp[j]), colnames(pp[i]), 'color', 'contrib'), envir=my_env)
                                             } else if (diagram[", j, "] == 'bw balloon plot') {
                                             assign(ooi", letters[j], ",
                                             pp_contingency(data, colnames(pp[j]), colnames(pp[i]), 'bw', 'observed', pp_geom = 'point'), envir=my_env)
                                             } else if (diagram[", j, "] == 'color balloon plot') {
                                             assign(ooi", letters[j], ",
                                             pp_contingency(data, colnames(pp[j]), colnames(pp[i]), 'color', 'observed', pp_geom = 'point'), envir=my_env)
                                             } else if (diagram[", j, "] == 'color residuals balloon plot') {
                                             assign(ooi", letters[j], ",
                                             pp_contingency(data, colnames(pp[j]), colnames(pp[i]), 'color', 'residuals', pp_geom = 'point'), envir=my_env)
                                             } else if (diagram[", j, "] == 'bw contribution to x2 balloon plot') {
                                             assign(ooi", letters[j], ",
                                             pp_contingency(data, colnames(pp[j]), colnames(pp[i]), 'bw', 'contrib', pp_geom = 'point'), envir=my_env)
                                             } else if (diagram[", j, "] == 'color contribution to x2 balloon plot') {
                                             assign(ooi", letters[j], ",
                                             pp_contingency(data, colnames(pp[j]), colnames(pp[i]), 'color', 'contrib', pp_geom = 'point'), envir=my_env)
                                             }")))
            eval(parse(text=paste0("assign('blank', blank(data, colnames(pp[i])), envir=my_env)
                                             ")))
          }

          line <- eval(parse(
            text=paste0("paste0('gridExtra::grid.arrange(' ,", paste0(" ooi", letters[1:i-1], collapse = ",', ',"), ",', ncol=", ncol(data.ord)-1, ")')")))
          write(line, file.path(dir, "brinton_outcomes", "matrixplot.R"), append=TRUE)
        }
        ord.plot(data.ord)
      }
    }
    else if (length(data[sapply(data, is.factor)][!sapply(data[sapply(data, is.factor)], is.ordered)])>1 &&
             identical(dataclass, c("factor", "factor")) &&
             (diagram %in% fac.fac_v0) == TRUE)
    {
      data.fac <- data[sapply(data, is.factor)][!sapply(data[sapply(data, is.factor)], is.ordered)]
      diagram <- rep(diagram, ncol(data.fac))
      out = NULL
      for (i in seq_along(data.fac)[2:ncol(data.fac)])
      {
        GAheight <- length(unique(data.fac[,(i)]))/5+0.5
        write(paste0("#+ fig.width=", 4*(ncol(data.fac)-1) ,", fig.height=", GAheight), file.path(dir, "brinton_outcomes", "matrixplot.R"), append=TRUE) # gridExtra
        for (j in seq_along(data.fac)[c(1:i-1)]) {eval(parse(text=paste0("ffi", letters[j], " <- paste0('ff', ", i, ", '", letters[j],"')")))}
        fac.plot <- function(pp)
        {
          for (j in seq_along(data.fac)[c(1:i-1)]) {eval(parse(text=paste0("
                                             if (diagram[", j, "] == 'color stacked bar graph') {
                                             assign(ffi", letters[j], ",
                                             pp_stackedbar(data, colnames(pp[i]), colnames(pp[j]), 'color', 'stack'), envir=my_env)
                                             } else if (diagram[", j, "] == 'color 100% stacked bar graph') {
                                             assign(ffi", letters[j], ",
                                             pp_stackedbar(data, colnames(pp[i]), colnames(pp[j]), 'color', 'fill'), envir=my_env)
                                             }")))
            eval(parse(text=paste0("assign('blank', blank(data, colnames(pp[i])), envir=my_env)
                                             ")))
          }
          line <- eval(parse(
          # text=paste0("paste0( ", paste0(" ffi", letters[1:i-1], collapse = ", ' + ', "), ", '", paste0(rep(" + plot_spacer()", ncol(data.fac) - i), collapse = ""), "')")))
          # write(paste(line, " + plot_layout(widths = c(rep(1,", ncol(data.fac) -1, ")))"), file.path(dir, "brinton_outcomes", "matrixplot.R"), append=TRUE)
          text=paste0("paste0('gridExtra::grid.arrange(' ,", paste0(" ffi", letters[1:i-1], collapse = ",', ',"), ",', ncol=", ncol(data.fac)-1, ")')")))
          write(line, file.path(dir, "brinton_outcomes", "matrixplot.R"), append=TRUE)
        }
        fac.plot(data.fac)
      }
    }
    else if (length(data[sapply(data, is.factor)][!sapply(data[sapply(data, is.factor)], is.ordered)])>1 &&
             identical(dataclass, c("factor", "factor")) &&
             (diagram %in% fac.fac_v1) == TRUE)
    {
      data.fac <- data[sapply(data, is.factor)][!sapply(data[sapply(data, is.factor)], is.ordered)]
      data.fac <- data.fac[, c(ncol(data.fac), 1:ncol(data.fac)-1)]
      diagram <- rep(diagram, ncol(data.fac))
      out = NULL
      for (i in seq_along(data.fac)[2:ncol(data.fac)])
      {
        GAheight <- length(unique(data.fac[,(i)]))/5+0.5
        write(paste0("#+ fig.width=", 4*(ncol(data.fac)-1) ,", fig.height=", GAheight), file.path(dir, "brinton_outcomes", "matrixplot.R"), append=TRUE) # gridExtra
        for (j in seq_along(data.fac)[c(1:i-1)]) {eval(parse(text=paste0("ffi", letters[j], " <- paste0('ff', ", i, ", '", letters[j],"')")))}
        fac.plot <- function(pp)
        {
          for (j in seq_along(data.fac)[c(1:i-1)]) {eval(parse(text=paste0("
                                             if (diagram[", j, "] == 'transposed color stacked bar graph') {
                                             assign(ffi", letters[j], ",
                                             pp_stackedbar(data, colnames(pp[i]), colnames(pp[j]), 'color', 'stack'), envir=my_env)
                                             } else if (diagram[", j, "] == 'transposed color 100% stacked bar graph') {
                                             assign(ffi", letters[j], ",
                                             pp_stackedbar(data, colnames(pp[i]), colnames(pp[j]), 'color', 'fill'), envir=my_env)
                                             }")))
            eval(parse(text=paste0("assign('blank', blank(data, colnames(pp[i])), envir=my_env)
                                             ")))
          }

          line <- eval(parse(
            text=paste0("paste0('gridExtra::grid.arrange(' ,", paste0(" ffi", letters[1:i-1], collapse = ",', ',"), ",', ncol=", ncol(data.fac)-1, ")')")))
          write(line, file.path(dir, "brinton_outcomes", "matrixplot.R"), append=TRUE)
        }
        fac.plot(data.fac)
      }
    }
    else if (length(data[sapply(data, is.factor)][!sapply(data[sapply(data, is.factor)], is.ordered)])>1 &&
             identical(dataclass, c("factor", "factor")) &&
             (diagram %in% fac.fac_v2) == TRUE)
    {
      data.fac <- data[sapply(data, is.factor)][!sapply(data[sapply(data, is.factor)], is.ordered)]
      data.fac <- data.fac[, c(ncol(data.fac), 1:ncol(data.fac)-1)]
      diagram <- rep(diagram, ncol(data.fac))
      out = NULL
      for (i in seq_along(data.fac)[2:ncol(data.fac)])
      {
        GAheight <- length(unique(data.fac[,(i)]))/5+0.9
        write(paste0("#+ fig.width=", 4*(ncol(data.fac)-1) ,", fig.height=", GAheight), file.path(dir, "brinton_outcomes", "matrixplot.R"), append=TRUE) # gridExtra
        for (j in seq_along(data.fac)[c(1:i-1)]) {eval(parse(text=paste0("ffi", letters[j], " <- paste0('ff', ", i, ", '", letters[j],"')")))}
        fac.plot <- function(pp)
        {
          for (j in seq_along(data.fac)[c(1:i-1)]) {eval(parse(text=paste0("
                                             if (diagram[", j, "] == 'bw heatmap') {
                                             assign(ffi", letters[j], ",
                                             pp_contingency(data, colnames(pp[j]), colnames(pp[i]), 'bw', 'observed'), envir=my_env)
                                             } else if (diagram[", j, "] == 'color heatmap') {
                                             assign(ffi", letters[j], ",
                                             pp_contingency(data, colnames(pp[j]), colnames(pp[i]), 'color', 'observed'), envir=my_env)
                                             } else if (diagram[", j, "] == 'color residuals heatmap') {
                                             assign(ffi", letters[j], ",
                                             pp_contingency(data, colnames(pp[j]), colnames(pp[i]), 'color', 'residuals'), envir=my_env)
                                             } else if (diagram[", j, "] == 'bw contribution to x2 heatmap') {
                                             assign(ffi", letters[j], ",
                                             pp_contingency(data, colnames(pp[j]), colnames(pp[i]), 'bw', 'contrib'), envir=my_env)
                                             } else if (diagram[", j, "] == 'color contribution to x2 heatmap') {
                                             assign(ffi", letters[j], ",
                                             pp_contingency(data, colnames(pp[j]), colnames(pp[i]), 'color', 'contrib'), envir=my_env)
                                             } else if (diagram[", j, "] == 'bw balloon plot') {
                                             assign(ffi", letters[j], ",
                                             pp_contingency(data, colnames(pp[j]), colnames(pp[i]), 'bw', 'observed', pp_geom = 'point'), envir=my_env)
                                             } else if (diagram[", j, "] == 'color balloon plot') {
                                             assign(ffi", letters[j], ",
                                             pp_contingency(data, colnames(pp[j]), colnames(pp[i]), 'color', 'observed', pp_geom = 'point'), envir=my_env)
                                             } else if (diagram[", j, "] == 'color residuals balloon plot') {
                                             assign(ffi", letters[j], ",
                                             pp_contingency(data, colnames(pp[j]), colnames(pp[i]), 'color', 'residuals', pp_geom = 'point'), envir=my_env)
                                             } else if (diagram[", j, "] == 'bw contribution to x2 balloon plot') {
                                             assign(ffi", letters[j], ",
                                             pp_contingency(data, colnames(pp[j]), colnames(pp[i]), 'bw', 'contrib', pp_geom = 'point'), envir=my_env)
                                             } else if (diagram[", j, "] == 'color contribution to x2 balloon plot') {
                                             assign(ffi", letters[j], ",
                                             pp_contingency(data, colnames(pp[j]), colnames(pp[i]), 'color', 'contrib', pp_geom = 'point'), envir=my_env)
                                             }")))
            eval(parse(text=paste0("assign('blank', blank(data, colnames(pp[i])), envir=my_env)
                                             ")))
          }

          line <- eval(parse(
            text=paste0("paste0('gridExtra::grid.arrange(' ,", paste0(" ffi", letters[1:i-1], collapse = ",', ',"), ",', ncol=", ncol(data.fac)-1, ")')")))
          write(line, file.path(dir, "brinton_outcomes", "matrixplot.R"), append=TRUE)
        }
        fac.plot(data.fac)
      }
    }
    else if (length(data[sapply(data, is.ordered)])>0 &&
             length(data[sapply(data, is.factor)][!sapply(data[sapply(data, is.factor)], is.ordered)])>0 &&
             any(dataclass %in% "ordered") &&
             any(dataclass %in% "factor") &&
             (diagram %in% fac.ord_v0) == TRUE)
    {
      data.ord <- data[sapply(data, is.ordered)]
      data.fac <- data[sapply(data, is.factor)][!sapply(data[sapply(data, is.factor)], is.ordered)]
      rownames(data.fac) <- NULL
      diagram <- rep(diagram, ncol(data.ord))
      out = NULL
      for (i in seq_along(data.fac))
      {
        GAheight <- length(unique(data.fac[,(i)]))/5+0.5
        write(paste0("#+ fig.width=", 4*ncol(data.ord) ,", fig.height=", GAheight), file.path(dir, "brinton_outcomes", "matrixplot.R"), append=TRUE) # gridExtra
        for (j in seq_along(data.ord)) {eval(parse(text=paste0("ofi", letters[j], " <- paste0('of', ", i, ", '", letters[j],"')")))}
        facord.plot <- function(pp)
        {
          long <- round(length(unique(pp[sapply(pp, is.factor)][[i]]))/6 + 0.6, 1)
          for (j in seq_along(data.ord)) {eval(parse(text=paste0("
                                             if (diagram[", j, "] == 'bw stacked bar graph') {
                                             assign(ofi", letters[j], ",
                                             pp_stackedbar(data, colnames(pp[sapply(pp, is.factor)][!sapply(pp[sapply(pp, is.factor)], is.ordered)][i]), colnames(pp[sapply(pp, is.ordered)][j]), 'bw', 'stack', 'ordinal'), envir=my_env)
                                             } else if (diagram[", j, "] == 'color stacked bar graph') {
                                             assign(ofi", letters[j], ",
                                             pp_stackedbar(data, colnames(pp[sapply(pp, is.factor)][!sapply(pp[sapply(pp, is.factor)], is.ordered)][i]), colnames(pp[sapply(pp, is.ordered)][j]), 'color', 'stack', 'ordinal'), envir=my_env)
                                             } else if (diagram[", j, "] == 'bw 100% stacked bar graph') {
                                             assign(ofi", letters[j], ",
                                             pp_stackedbar(data, colnames(pp[sapply(pp, is.factor)][!sapply(pp[sapply(pp, is.factor)], is.ordered)][i]), colnames(pp[sapply(pp, is.ordered)][j]), 'bw', 'fill', 'ordinal'), envir=my_env)
                                             } else if (diagram[", j, "] == 'color 100% stacked bar graph') {
                                             assign(ofi", letters[j], ",
                                             pp_stackedbar(data, colnames(pp[sapply(pp, is.factor)][!sapply(pp[sapply(pp, is.factor)], is.ordered)][i]), colnames(pp[sapply(pp, is.ordered)][j]), 'color', 'fill', 'ordinal'), envir=my_env)
                                             }")))}

          line <- eval(parse(
            text=paste0("paste0('gridExtra::grid.arrange(' ,", paste0(" ofi", letters[1:ncol(data.ord)], collapse = ",', ',"), ",', ncol=", ncol(data.ord), ")')")))
          write(line, file.path(dir, "brinton_outcomes", "matrixplot.R"), append=TRUE)
          write("#' <br>", file.path(dir, "brinton_outcomes", "matrixplot.R"), append=TRUE)
        }
        facord.plot(cbind(data.ord, data.fac))
      }
    }
    else if (length(data[sapply(data, is.ordered)])>0 &&
             length(data[sapply(data, is.factor)][!sapply(data[sapply(data, is.factor)], is.ordered)])>0 &&
             any(dataclass %in% "ordered") &&
             any(dataclass %in% "factor") &&
             (diagram %in% fac.ord_v1) == TRUE)
    {
      data.ord <- data[sapply(data, is.ordered)]
      data.fac <- data[sapply(data, is.factor)][!sapply(data[sapply(data, is.factor)], is.ordered)]
      rownames(data.ord) <- NULL
      diagram <- rep(diagram, ncol(data.fac))
      out = NULL
      for (i in seq_along(data.ord))
      {
        GAheight <- length(unique(data.ord[,(i)]))/5+0.5
        write(paste0("#+ fig.width=", 4*ncol(data.fac) ,", fig.height=", GAheight), file.path(dir, "brinton_outcomes", "matrixplot.R"), append=TRUE) # gridExtra
        for (j in seq_along(data.fac)) {eval(parse(text=paste0("ofi", letters[j], " <- paste0('of', ", i, ", '", letters[j],"')")))}
        facord.plot <- function(pp)
        {
          long <- round(length(unique(pp[sapply(pp, is.factor)][[i]]))/6 + 0.6, 1)
          for (j in seq_along(data.fac)) {eval(parse(text=paste0("
                                             if (diagram[", j, "] == 'transposed color stacked bar graph') {
                                             assign(ofi", letters[j], ",
                                             pp_stackedbar(data, colnames(pp[sapply(pp, is.ordered)][i]), colnames(pp[sapply(pp, is.factor)][!sapply(pp[sapply(pp, is.factor)], is.ordered)][j]), 'color', 'stack'), envir=my_env)
                                             } else if (diagram[", j, "] == 'transposed color 100% stacked bar graph') {
                                             assign(ofi", letters[j], ",
                                             pp_stackedbar(data, colnames(pp[sapply(pp, is.ordered)][i]), colnames(pp[sapply(pp, is.factor)][!sapply(pp[sapply(pp, is.factor)], is.ordered)][j]), 'color', 'fill'), envir=my_env)
                                             }")))}

          line <- eval(parse(
            text=paste0("paste0('gridExtra::grid.arrange(' ,", paste0(" ofi", letters[1:ncol(data.fac)], collapse = ",', ',"), ",', ncol=", ncol(data.fac), ")')")))
          write(line, file.path(dir, "brinton_outcomes", "matrixplot.R"), append=TRUE)
          write("#' <br>", file.path(dir, "brinton_outcomes", "matrixplot.R"), append=TRUE)
        }
        facord.plot(cbind(data.ord, data.fac))
      }
    }
    else if (length(data[sapply(data, is.ordered)])>0 &&
             length(data[sapply(data, is.factor)][!sapply(data[sapply(data, is.factor)], is.ordered)])>0 &&
             any(dataclass %in% "ordered") &&
             any(dataclass %in% "factor") &&
             (diagram %in% fac.ord_v2) == TRUE)
    {
      data.ord <- data[sapply(data, is.ordered)]
      data.fac <- data[sapply(data, is.factor)][!sapply(data[sapply(data, is.factor)], is.ordered)]
      rownames(data.fac) <- NULL
      diagram <- rep(diagram, ncol(data.ord))
      out = NULL
      for (i in seq_along(data.fac))
      {
        GAheight <- length(unique(data.fac[,(i)]))/5+1.5
        write(paste0("#+ fig.width=", 4*ncol(data.ord) ,", fig.height=", GAheight), file.path(dir, "brinton_outcomes", "matrixplot.R"), append=TRUE) # gridExtra
        for (j in seq_along(data.ord)) {eval(parse(text=paste0("ofi", letters[j], " <- paste0('of', ", i, ", '", letters[j],"')")))}
        facord.plot <- function(pp)
        {
          long <- round(length(unique(pp[sapply(pp, is.factor)][[i]]))/6 + 0.6, 1)
          for (j in seq_along(data.ord)) {eval(parse(text=paste0("
                                             if (diagram[", j, "] == 'bw heatmap') {
                                             assign(ofi", letters[j], ",
                                             pp_contingency(data, colnames(pp[sapply(pp, is.ordered)][j]), colnames(pp[sapply(pp, is.factor)][!sapply(pp[sapply(pp, is.factor)], is.ordered)][i]), 'bw', 'observed'), envir=my_env)
                                             } else if (diagram[", j, "] == 'color heatmap') {
                                             assign(ofi", letters[j], ",
                                             pp_contingency(data, colnames(pp[sapply(pp, is.ordered)][j]), colnames(pp[sapply(pp, is.factor)][!sapply(pp[sapply(pp, is.factor)], is.ordered)][i]), 'color', 'observed'), envir=my_env)
                                             } else if (diagram[", j, "] == 'color residuals heatmap') {
                                             assign(ofi", letters[j], ",
                                             pp_contingency(data, colnames(pp[sapply(pp, is.ordered)][j]), colnames(pp[sapply(pp, is.factor)][!sapply(pp[sapply(pp, is.factor)], is.ordered)][i]), 'color', 'residuals'), envir=my_env)
                                             } else if (diagram[", j, "] == 'bw contribution to x2 heatmap') {
                                             assign(ofi", letters[j], ",
                                             pp_contingency(data, colnames(pp[sapply(pp, is.ordered)][j]), colnames(pp[sapply(pp, is.factor)][!sapply(pp[sapply(pp, is.factor)], is.ordered)][i]), 'bw', 'contrib'), envir=my_env)
                                             } else if (diagram[", j, "] == 'color contribution to x2 heatmap') {
                                             assign(ofi", letters[j], ",
                                             pp_contingency(data, colnames(pp[sapply(pp, is.ordered)][j]), colnames(pp[sapply(pp, is.factor)][!sapply(pp[sapply(pp, is.factor)], is.ordered)][i]), 'color', 'contrib'), envir=my_env)
                                             } else if (diagram[", j, "] == 'bw balloon plot') {
                                             assign(ofi", letters[j], ",
                                             pp_contingency(data, colnames(pp[sapply(pp, is.ordered)][j]), colnames(pp[sapply(pp, is.factor)][!sapply(pp[sapply(pp, is.factor)], is.ordered)][i]), 'bw', 'observed', pp_geom = 'point'), envir=my_env)
                                             } else if (diagram[", j, "] == 'color balloon plot') {
                                             assign(ofi", letters[j], ",
                                             pp_contingency(data, colnames(pp[sapply(pp, is.ordered)][j]), colnames(pp[sapply(pp, is.factor)][!sapply(pp[sapply(pp, is.factor)], is.ordered)][i]), 'color', 'observed', pp_geom = 'point'), envir=my_env)
                                             } else if (diagram[", j, "] == 'color residuals balloon plot') {
                                             assign(ofi", letters[j], ",
                                             pp_contingency(data, colnames(pp[sapply(pp, is.ordered)][j]), colnames(pp[sapply(pp, is.factor)][!sapply(pp[sapply(pp, is.factor)], is.ordered)][i]), 'color', 'residuals', pp_geom = 'point'), envir=my_env)
                                             } else if (diagram[", j, "] == 'bw contribution to x2 balloon plot') {
                                             assign(ofi", letters[j], ",
                                             pp_contingency(data, colnames(pp[sapply(pp, is.ordered)][j]), colnames(pp[sapply(pp, is.factor)][!sapply(pp[sapply(pp, is.factor)], is.ordered)][i]), 'bw', 'contrib', pp_geom = 'point'), envir=my_env)
                                             } else if (diagram[", j, "] == 'color contribution to x2 balloon plot') {
                                             assign(ofi", letters[j], ",
                                             pp_contingency(data, colnames(pp[sapply(pp, is.ordered)][j]), colnames(pp[sapply(pp, is.factor)][!sapply(pp[sapply(pp, is.factor)], is.ordered)][i]), 'color', 'contrib', pp_geom = 'point'), envir=my_env)
                                             }")))}

          line <- eval(parse(
            text=paste0("paste0('gridExtra::grid.arrange(' ,", paste0(" ofi", letters[1:ncol(data.ord)], collapse = ",', ',"), ",', ncol=", ncol(data.ord), ")')")))
          write(line, file.path(dir, "brinton_outcomes", "matrixplot.R"), append=TRUE)
          write("#' <br>", file.path(dir, "brinton_outcomes", "matrixplot.R"), append=TRUE)
        }
        facord.plot(cbind(data.ord, data.fac))
      }
    }





    else {stop("The combination of these data and graphic type has not been still considered, or
             maybe the data does not include the types of data considered by the 'dataclass' argument, or
             maybe the data does not have two columns of the dataclass set.")}

  } else {stop(warning_wp_dc)}

rmarkdown::render(file.path(dir, "brinton_outcomes", "matrixplot.R", fsep = .Platform$file.sep), "html_document", envir=my_env)
pander::openFileInOS(file.path(dir, "brinton_outcomes", "matrixplot.html", fsep = .Platform$file.sep))
# unlink(file.path(dir, "brinton_outcomes", fsep = .Platform$file.sep), recursive = TRUE)
  }

my_env <- new.env(parent = emptyenv())

#' Displays a matrix of a particular graphic for bivariate data in a html file.
#'
#' A matrixplot is a grid of a particular graphic showing bivariate relationships
#' between all pairs of variables of a certain(s) type(s) in a multivariate data set.
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
    if (tibble::is_tibble(data) == TRUE) {
      stop(warning_tibble)
      # data <- as.data.frame(data)
    }

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
      diagram <- c("scatter plot")
    }
    if (length(diagram) != sum(diagram %in% c(numeric2_v2, fac.num_v1, fac.num_v2), na.rm = TRUE))
    {
      stop(paste0(
        "The 'diagram'",
        string,
        " which values can be : '",
        paste0(cbind(numeric2_v2, fac.num_v1), collapse = "', '"),
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
        deparse(substitute(data)),
        " dataframe')"
      ),
      file = file.path(dir, "brinton_outcomes", "matrixplot.R"),
      append = TRUE
    )


if (length(data[sapply(data, is.numeric)])>0 &&
    dataclass[1] == "numeric" &&
    dataclass[2] == "numeric" &&
    (diagram %in% numeric2_v2) == TRUE)
{
  data.num <- data[sapply(data, is.numeric)]
  diagram <- rep(diagram, ncol(data.num))
  out = NULL
  write(paste0("#+ numeric, fig.width=", 2.4*ncol(data.num) ,", fig.height=2.4"), file.path(dir, "brinton_outcomes", "matrixplot.R"), append=TRUE) # gridExtra
  for (i in seq_along(data.num))
  {
    for (j in seq_along(data.num)) {eval(parse(text=paste0("nui", letters[j], " <- paste0('nu', ", i, ", '", letters[j],"')")))}
    num.plot <- function(pp)
    {
      for (j in seq_along(data.num)) {eval(parse(text=paste0("
                                             if (diagram[", j, "] == 'scatter plot') {
                                             assign(nui", letters[j], ",
                                             pp_scatterplot(data, colnames(pp[i]), colnames(pp[j]), pp_size = 3/ncol(data.num), 'black', FALSE), envir=my_env)
                                             } else if (diagram[", j, "] == 'bw scatter plot') {
                                             assign(nui", letters[j], ",
                                             pp_scatterplot(data, colnames(pp[i]), colnames(pp[j]), pp_size = 3/ncol(data.num), 'bw', FALSE), envir=my_env)
                                             } else if (diagram[", j, "] == 'color scatter plot') {
                                             assign(nui", letters[j], ",
                                             pp_scatterplot(data, colnames(pp[i]), colnames(pp[j]), pp_size = 3/ncol(data.num), 'color', FALSE), envir=my_env)
                                             } else if (diagram[", j, "] == 'binned scatter plot') {
                                             assign(nui", letters[j], ",
                                             pp_binnedpointgraph(data, colnames(pp[i]), colnames(pp[j]), pp_size = 3/ncol(data.num), 'black'), envir=my_env)
                                             } else if (diagram[", j, "] == 'bw binned scatter plot') {
                                             assign(nui", letters[j], ",
                                             pp_binnedpointgraph(data, colnames(pp[i]), colnames(pp[j]), pp_size = 3/ncol(data.num), 'bw'), envir=my_env)
                                             } else if (diagram[", j, "] == 'color binned scatter plot') {
                                             assign(nui", letters[j], ",
                                             pp_binnedpointgraph(data, colnames(pp[i]), colnames(pp[j]), pp_size = 3/ncol(data.num), 'color'), envir=my_env)
                                             } else if (diagram[", j, "] == 'binned heatmap') {
                                             assign(nui", letters[j], ",
                                             pp_heatmap(data, colnames(pp[i]), colnames(pp[j]), 'black'), envir=my_env)
                                             } else if (diagram[", j, "] == 'bw binned heatmap') {
                                             assign(nui", letters[j], ",
                                             pp_heatmap(data, colnames(pp[i]), colnames(pp[j]), 'bw'), envir=my_env)
                                             } else if (diagram[", j, "] == 'color binned heatmap') {
                                             assign(nui", letters[j], ",
                                             pp_heatmap(data, colnames(pp[i]), colnames(pp[j]), 'color'), envir=my_env)
                                             } else if (diagram[", j, "] == 'hexagonal binned heatmap') {
                                             assign(nui", letters[j], ",
                                             pp_heatmap(data, colnames(pp[i]), colnames(pp[j]), 'black', 6), envir=my_env)
                                             } else if (diagram[", j, "] == 'bw hexagonal binned heatmap') {
                                             assign(nui", letters[j], ",
                                             pp_heatmap(data, colnames(pp[i]), colnames(pp[j]), 'bw', 6), envir=my_env)
                                             } else if (diagram[", j, "] == 'color hexagonal binned heatmap') {
                                             assign(nui", letters[j], ",
                                             pp_heatmap(data, colnames(pp[i]), colnames(pp[j]), 'color', 6), envir=my_env)
                                             } else if (diagram[", j, "] == 'bw heatmap') {
                                             assign(nui", letters[j], ",
                                             pp_raster(data, colnames(pp[i]), colnames(pp[j]), 'bw'), envir=my_env)
                                             } else if (diagram[", j, "] == 'color heatmap') {
                                             assign(nui", letters[j], ",
                                             pp_raster(data, colnames(pp[i]), colnames(pp[j]), 'color'), envir=my_env)
                                             } else if (diagram[", j, "] == 'contour plot') {
                                             assign(nui", letters[j], ",
                                             pp_contourmap(data, colnames(pp[i]), colnames(pp[j]), pp_size = 3/ncol(data.num), 'black'), envir=my_env)
                                             } else if (diagram[", j, "] == 'bw contour plot') {
                                             assign(nui", letters[j], ",
                                             pp_contourmap(data, colnames(pp[i]), colnames(pp[j]), pp_size = 3/ncol(data.num), 'bw'), envir=my_env)
                                             } else if (diagram[", j, "] == 'color contour plot') {
                                             assign(nui", letters[j], ",
                                             pp_contourmap(data, colnames(pp[i]), colnames(pp[j]), pp_size = 3/ncol(data.num), 'color'), envir=my_env)
                                             } else if (diagram[", j, "] == 'contour plot with data points') {
                                             assign(nui", letters[j], ",
                                             pp_contourmap(data, colnames(pp[i]), colnames(pp[j]), pp_size = 3/ncol(data.num), 'black', TRUE), envir=my_env)
                                             } else if (diagram[", j, "] == 'bw contour plot with data points') {
                                             assign(nui", letters[j], ",
                                             pp_contourmap(data, colnames(pp[i]), colnames(pp[j]), pp_size = 3/ncol(data.num), 'bw', TRUE), envir=my_env)
                                             } else if (diagram[", j, "] == 'color contour plot with data points') {
                                             assign(nui", letters[j], ",
                                             pp_contourmap(data, colnames(pp[i]), colnames(pp[j]), pp_size = 3/ncol(data.num), 'color', TRUE), envir=my_env)
                                             } else if (diagram[", j, "] == 'scatter plot with confidence ellipse') {
                                             assign(nui", letters[j], ",
                                             pp_scatterplot(data, colnames(pp[i]), colnames(pp[j]), pp_size = 3/ncol(data.num), 'black', FALSE, TRUE, FALSE), envir=my_env)
                                             } else if (diagram[", j, "] == 'bw scatter plot with confidence ellipse') {
                                             assign(nui", letters[j], ",
                                             pp_scatterplot(data, colnames(pp[i]), colnames(pp[j]), pp_size = 3/ncol(data.num), 'bw', FALSE, TRUE, FALSE), envir=my_env)
                                             } else if (diagram[", j, "] == 'color scatter plot with confidence ellipse') {
                                             assign(nui", letters[j], ",
                                             pp_scatterplot(data, colnames(pp[i]), colnames(pp[j]), pp_size = 3/ncol(data.num), 'color', FALSE, TRUE, FALSE), envir=my_env)
                                             } else if (diagram[", j, "] == 'scatter plot with marginal rugs') {
                                             assign(nui", letters[j], ",
                                             pp_scatterplot(data, colnames(pp[i]), colnames(pp[j]), pp_size = 3/ncol(data.num), 'black', FALSE, FALSE, TRUE), envir=my_env)
                                             } else if (diagram[", j, "] == 'bw scatter plot with marginal rugs') {
                                             assign(nui", letters[j], ",
                                             pp_scatterplot(data, colnames(pp[i]), colnames(pp[j]), pp_size = 3/ncol(data.num), 'bw', FALSE, FALSE, TRUE), envir=my_env)
                                             } else if (diagram[", j, "] == 'color scatter plot with marginal rugs') {
                                             assign(nui", letters[j], ",
                                             pp_scatterplot(data, colnames(pp[i]), colnames(pp[j]), pp_size = 3/ncol(data.num), 'color', FALSE, FALSE, TRUE), envir=my_env)
                                             } else if (diagram[", j, "] == 'path graph') {
                                             assign(nui", letters[j], ",
                                             pp_pathgraph(data, colnames(pp[i]), colnames(pp[j]), FALSE, 'black', pp_size = 3/ncol(data.num)), envir=my_env)
                                             } else if (diagram[", j, "] == 'bw path graph') {
                                             assign(nui", letters[j], ",
                                             pp_pathgraph(data, colnames(pp[i]), colnames(pp[j]), FALSE, 'bw', pp_size = 3/ncol(data.num)), envir=my_env)
                                             } else if (diagram[", j, "] == 'color path graph') {
                                             assign(nui", letters[j], ",
                                             pp_pathgraph(data, colnames(pp[i]), colnames(pp[j]), FALSE, 'color', pp_size = 3/ncol(data.num)), envir=my_env)
                                             } else if (diagram[", j, "] == 'point-to-point graph') {
                                             assign(nui", letters[j], ",
                                             pp_pathgraph(data, colnames(pp[i]), colnames(pp[j]), TRUE, 'black', pp_size = 3/ncol(data.num)), envir=my_env)
                                             } else if (diagram[", j, "] == 'bw point-to-point graph') {
                                             assign(nui", letters[j], ",
                                             pp_pathgraph(data, colnames(pp[i]), colnames(pp[j]), TRUE, 'bw', pp_size = 3/ncol(data.num)), envir=my_env)
                                             } else if (diagram[", j, "] == 'color point-to-point graph') {
                                             assign(nui", letters[j], ",
                                             pp_pathgraph(data, colnames(pp[i]), colnames(pp[j]), TRUE, 'color', pp_size = 3/ncol(data.num)), envir=my_env)
                                             }")))}

      line <- eval(parse(
        text=paste0("paste0('gridExtra::grid.arrange(' ,", paste0(" nui", letters[1:ncol(data.num)], collapse = ",', ',"), ",', ncol=", ncol(data.num), ")')")))
      write(line, file.path(dir, "brinton_outcomes", "matrixplot.R"), append=TRUE)  # gridExtra
    }
    num.plot(data.num)
    }
}
    else if (length(data[sapply(data, lubridate::is.instant)])>0 &&
        dataclass[1] == "datetime" &&
        dataclass[2] == "datetime" &&
        (diagram %in% datetime2_v) == TRUE)
    {
      data.date <- data[sapply(data, lubridate::is.instant)]
      diagram <- rep(diagram, ncol(data.date))
      out = NULL
      write(paste0("#+ datetime, fig.width=", 2.4*ncol(data.date) ,", fig.height=2.4"), file.path(dir, "brinton_outcomes", "matrixplot.R"), append=TRUE) # gridExtra
      for (i in seq_along(data.date))
      {
        for (j in seq_along(data.date)) {eval(parse(text=paste0("dti", letters[j], " <- paste0('dt', ", i, ", '", letters[j],"')")))}
        num.plot <- function(pp)
        {
          for (j in seq_along(data.date)) {eval(parse(text=paste0("
                                             if (diagram[", j, "] == 'scatter plot') {
                                             assign(dti", letters[j], ",
                                             pp_scatterplot(data, colnames(pp[i]), colnames(pp[j]), pp_size = 3/ncol(data.num), 'black', FALSE), envir=my_env)
                                             } else if (diagram[", j, "] == 'scatter plot with trend line') {
                                             assign(dti", letters[j], ",
                                             pp_scatterplot(data, colnames(pp[i]), colnames(pp[j]), pp_size = 3/ncol(data.num), 'black', TRUE), envir=my_env)
                                             } else if (diagram[", j, "] == 'binned scatter plot') {
                                             assign(dti", letters[j], ",
                                             pp_binnedpointgraph(data, colnames(pp[i]), colnames(pp[j]), pp_size = 3/ncol(data.num), 'black'), envir=my_env)
                                             } else if (diagram[", j, "] == 'bw binned scatter plot') {
                                             assign(dti", letters[j], ",
                                             pp_binnedpointgraph(data, colnames(pp[i]), colnames(pp[j]), pp_size = 3/ncol(data.num), 'bw'), envir=my_env)
                                             } else if (diagram[", j, "] == 'color binned scatter plot') {
                                             assign(dti", letters[j], ",
                                             pp_binnedpointgraph(data, colnames(pp[i]), colnames(pp[j]), pp_size = 3/ncol(data.num), 'color'), envir=my_env)
                                             } else if (diagram[", j, "] == 'binned heatmap') {
                                             assign(dti", letters[j], ",
                                             pp_heatmap(data, colnames(pp[i]), colnames(pp[j]), 'black'), envir=my_env)
                                             } else if (diagram[", j, "] == 'bw binned heatmap') {
                                             assign(dti", letters[j], ",
                                             pp_heatmap(data, colnames(pp[i]), colnames(pp[j]), 'bw'), envir=my_env)
                                             } else if (diagram[", j, "] == 'color binned heatmap') {
                                             assign(dti", letters[j], ",
                                             pp_heatmap(data, colnames(pp[i]), colnames(pp[j]), 'color'), envir=my_env)
                                             } else if (diagram[", j, "] == 'bw heatmap') {
                                             assign(dti", letters[j], ",
                                             pp_raster(data, colnames(pp[i]), colnames(pp[j]), 'bw'), envir=my_env)
                                             } else if (diagram[", j, "] == 'color heatmap') {
                                             assign(dti", letters[j], ",
                                             pp_raster(data, colnames(pp[i]), colnames(pp[j]), 'color'), envir=my_env)
                                             } else if (diagram[", j, "] == 'contour plot') {
                                             assign(dti", letters[j], ",
                                             pp_contourmap(data, colnames(pp[i]), colnames(pp[j]), pp_size = 3/ncol(data.num), 'black'), envir=my_env)
                                             } else if (diagram[", j, "] == 'bw contour plot') {
                                             assign(dti", letters[j], ",
                                             pp_contourmap(data, colnames(pp[i]), colnames(pp[j]), pp_size = 3/ncol(data.num), 'bw'), envir=my_env)
                                             } else if (diagram[", j, "] == 'color contour plot') {
                                             assign(dti", letters[j], ",
                                             pp_contourmap(data, colnames(pp[i]), colnames(pp[j]), pp_size = 3/ncol(data.num), 'color'), envir=my_env)
                                             } else if (diagram[", j, "] == 'contour plot with data points') {
                                             assign(dti", letters[j], ",
                                             pp_contourmap(data, colnames(pp[i]), colnames(pp[j]), pp_size = 3/ncol(data.num), 'black', TRUE), envir=my_env)
                                             } else if (diagram[", j, "] == 'path graph') {
                                             assign(dti", letters[j], ",
                                             pp_pathgraph(data, colnames(pp[i]), colnames(pp[j]), FALSE, 'black', pp_size = 3/ncol(data.num)), envir=my_env)
                                             } else if (diagram[", j, "] == 'bw path graph') {
                                             assign(dti", letters[j], ",
                                             pp_pathgraph(data, colnames(pp[i]), colnames(pp[j]), FALSE, 'bw', pp_size = 3/ncol(data.num)), envir=my_env)
                                             } else if (diagram[", j, "] == 'color path graph') {
                                             assign(dti", letters[j], ",
                                             pp_pathgraph(data, colnames(pp[i]), colnames(pp[j]), FALSE, 'color', pp_size = 3/ncol(data.num)), envir=my_env)
                                             } else if (diagram[", j, "] == 'point-to-point graph') {
                                             assign(dti", letters[j], ",
                                             pp_pathgraph(data, colnames(pp[i]), colnames(pp[j]), TRUE, 'black', pp_size = 3/ncol(data.num)), envir=my_env)
                                             } else if (diagram[", j, "] == 'bw point-to-point graph') {
                                             assign(dti", letters[j], ",
                                             pp_pathgraph(data, colnames(pp[i]), colnames(pp[j]), TRUE, 'bw', pp_size = 3/ncol(data.num)), envir=my_env)
                                             } else if (diagram[", j, "] == 'color point-to-point graph') {
                                             assign(dti", letters[j], ",
                                             pp_pathgraph(data, colnames(pp[i]), colnames(pp[j]), TRUE, 'color', pp_size = 3/ncol(data.num)), envir=my_env)
                                             }")))}

          line <- eval(parse(
            text=paste0("paste0('gridExtra::grid.arrange(' ,", paste0(" dti", letters[1:ncol(data.date)], collapse = ",', ',"), ",', ncol=", ncol(data.date), ")')")))
          write(line, file.path(dir, "brinton_outcomes", "matrixplot.R"), append=TRUE)  # gridExtra
        }
        num.plot(data.date)
      }
    }

    else if (length(data[sapply(data, is.numeric)])>0 &&
             length(data[sapply(data, lubridate::is.instant)])>0 &&
             ((dataclass[1] == "datetime" && dataclass[2] == "numeric") |
             (dataclass[1] == "numeric" && dataclass[2] == "datetime")) &&
             (diagram %in% datenum_v) == TRUE)
    {
      data.date <- data[sapply(data, lubridate::is.instant)]
      data.num <- data[sapply(data, is.numeric)]
      diagram <- rep(diagram, ncol(data.date))
      out = NULL
      write(paste0("#+ datetime, fig.width=", 2.4*ncol(data.date) ,", fig.height=2.4"), file.path(dir, "brinton_outcomes", "matrixplot.R"), append=TRUE) # gridExtra
      for (i in seq_along(data.num))
      {
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
                                             } else if (diagram[", j, "] == 'binned heatmap') {
                                             assign(dni", letters[j], ",
                                             pp_heatmap(data, colnames(pp[sapply(pp, lubridate::is.instant)][j]), colnames(pp[sapply(pp, is.numeric)][i]), 'black'), envir=my_env)
                                             } else if (diagram[", j, "] == 'bw binned heatmap') {
                                             assign(dni", letters[j], ",
                                             pp_heatmap(data, colnames(pp[sapply(pp, lubridate::is.instant)][j]), colnames(pp[sapply(pp, is.numeric)][i]), 'bw'), envir=my_env)
                                             } else if (diagram[", j, "] == 'color binned heatmap') {
                                             assign(dni", letters[j], ",
                                             pp_heatmap(data, colnames(pp[sapply(pp, lubridate::is.instant)][j]), colnames(pp[sapply(pp, is.numeric)][i]), 'color'), envir=my_env)
                                             } else if (diagram[", j, "] == 'bw heatmap') {
                                             assign(dni", letters[j], ",
                                             pp_raster(data, colnames(pp[sapply(pp, lubridate::is.instant)][j]), colnames(pp[sapply(pp, is.numeric)][i]), 'bw'), envir=my_env)
                                             } else if (diagram[", j, "] == 'color heatmap') {
                                             assign(dni", letters[j], ",
                                             pp_raster(data, colnames(pp[sapply(pp, lubridate::is.instant)][j]), colnames(pp[sapply(pp, is.numeric)][i]), 'color'), envir=my_env)
                                             } else if (diagram[", j, "] == 'contour plot') {
                                             assign(dni", letters[j], ",
                                             pp_contourmap(data, colnames(pp[sapply(pp, lubridate::is.instant)][j]), colnames(pp[sapply(pp, is.numeric)][i]), pp_size = 1/ncol(data.num), 'black'), envir=my_env)
                                             } else if (diagram[", j, "] == 'bw contour plot') {
                                             assign(dni", letters[j], ",
                                             pp_contourmap(data, colnames(pp[sapply(pp, lubridate::is.instant)][j]), colnames(pp[sapply(pp, is.numeric)][i]), pp_size = 1/ncol(data.num), 'bw'), envir=my_env)
                                             } else if (diagram[", j, "] == 'color contour plot') {
                                             assign(dni", letters[j], ",
                                             pp_contourmap(data, colnames(pp[sapply(pp, lubridate::is.instant)][j]), colnames(pp[sapply(pp, is.numeric)][i]), pp_size = 1/ncol(data.num), 'color'), envir=my_env)
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
            text=paste0("paste0('gridExtra::grid.arrange(' ,", paste0(" dni", letters[1:ncol(data.date)], collapse = ",', ',"), ",', ncol=", ncol(data.date), ")')")))
          write(line, file.path(dir, "brinton_outcomes", "matrixplot.R"), append=TRUE)  # gridExtra
        }
        datenum.plot(cbind(data.num, data.date))
      }
    }
    else if (length(data[sapply(data, is.numeric)])>0 &&
             length(data[sapply(data, is.factor)])>0 &&
             (dataclass == c("factor", "numeric") |
              dataclass == c("numeric", "factor"))  &&
             (diagram %in% fac.num_v1) == TRUE)
{
  data.num <- data[sapply(data, is.numeric)]
  data.fac <- data[sapply(data, is.factor)]
  rownames(data.fac) <- NULL
  diagram <- rep(diagram, ncol(data.num))
  out = NULL
  for (i in seq_along(data.fac))
  {
    for (j in seq_along(data.num)) {eval(parse(text=paste0("fni", letters[j], " <- paste0('fn', ", i, ", '", letters[j],"')")))}
    fac.plot <- function(pp)
    {
      long <- round(length(unique(pp[sapply(pp, is.factor)][[i]]))/6 + 0.5, 1)
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
        text=paste0("paste0('gridExtra::grid.arrange(' ,", paste0(" fni", letters[1:ncol(data.num)], collapse = ",', ',"), ",', ncol=", ncol(data.num), ")')")))
      write(paste0("#+ fig.width=", 2.4*ncol(data.num) ,", fig.height=", long), file.path(dir, "brinton_outcomes", "matrixplot.R"), append=TRUE) # gridExtra
      write(line, file.path(dir, "brinton_outcomes", "matrixplot.R"), append=TRUE)  # gridExtra
    }
    fac.plot(cbind(data.num, data.fac))
  }
}
    else if (length(data[sapply(data, is.numeric)])>0 &&
             length(data[sapply(data, is.factor)])>0 &&
             (dataclass == c("factor", "numeric") |
              dataclass == c("numeric", "factor"))  &&
             (diagram %in% fac.num_v2) == TRUE)
    {
      data.num <- data[sapply(data, is.numeric)]
      data.fac <- data[sapply(data, is.factor)]
      rownames(data.fac) <- NULL
      diagram <- rep(diagram, ncol(data.num))
      out = NULL
      write(paste0("#+ fig.width=", 2.4*ncol(data.num) ,", fig.height=2.4"), file.path(dir, "brinton_outcomes", "matrixplot.R"), append=TRUE) # gridExtra
      for (i in seq_along(data.fac))
      {
        for (j in seq_along(data.num)) {eval(parse(text=paste0("fni", letters[j], " <- paste0('fn', ", i, ", '", letters[j],"')")))}
        fac.plot <- function(pp)
        {
          long <- round(length(unique(pp[sapply(pp, is.factor)][[i]]))/6 + 0.5, 1)
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
                                             pp_histogram2(data, colnames(pp[sapply(pp, is.numeric)][j]), colnames(pp[sapply(pp, is.factor)][i]), pp_color = 'color', pp_position = 'fill'), envir=my_env)
                                             } else if (diagram[", j, "] == 'density plot') {
                                             assign(fni", letters[j], ",
                                             pp_density2(data, colnames(pp[sapply(pp, is.numeric)][j]), colnames(pp[sapply(pp, is.factor)][i]), 0.5, 'line', 'bw'), envir=my_env)
                                             } else if (diagram[", j, "] == 'color density plot') {
                                             assign(fni", letters[j], ",
                                             pp_density2(data, colnames(pp[sapply(pp, is.numeric)][j]), colnames(pp[sapply(pp, is.factor)][i]), 0.5, 'line', 'color'), envir=my_env)
                                             } else if (diagram[", j, "] == 'filled density plot') {
                                             assign(fni", letters[j], ",
                                             pp_density2(data, colnames(pp[sapply(pp, is.factor)][i]), colnames(pp[sapply(pp, is.numeric)][j]), 0.5, 'area', 'bw'), envir=my_env)
                                             } else if (diagram[", j, "] == 'color filled density plot') {
                                             assign(fni", letters[j], ",
                                             pp_density2(data, colnames(pp[sapply(pp, is.factor)][i]), colnames(pp[sapply(pp, is.numeric)][j]), 0.5, 'area', 'color'), envir=my_env)
                                             }")))}

          line <- eval(parse(
            text=paste0("paste0('gridExtra::grid.arrange(' ,", paste0(" fni", letters[1:ncol(data.num)], collapse = ",', ',"), ",', ncol=", ncol(data.num), ")')")))
          write(line, file.path(dir, "brinton_outcomes", "matrixplot.R"), append=TRUE)  # gridExtra
        }
        fac.plot(cbind(data.num, data.fac))
      }
    }
    else {stop("The combination of these data and graphic type has not been still considered or
             maybe the data does not include the types of data considered by the 'dataclass' argument.")}

  } else {stop(warning_wp_dc)}

rmarkdown::render(file.path(dir, "brinton_outcomes", "matrixplot.R", fsep = .Platform$file.sep), "html_document", envir=my_env)
pander::openFileInOS(file.path(dir, "brinton_outcomes", "matrixplot.html", fsep = .Platform$file.sep))
# unlink(file.path(dir, "brinton_outcomes", fsep = .Platform$file.sep), recursive = TRUE)
  }



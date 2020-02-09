#' Presents a specific graphic explicitly called by name.
#'
#' In order to present the graphic, the user must define a dataset, a
#' variable whitin this dataset and a compatible type of graphic.
#' Future work will include graphics that involve more than one variable.
#'
#' @param data Data.frame. Default dataset to use for plot. If not already a
#' data.frame, it should be first coerced to by [as.data.frame()].
#' @param vars Character. A variable within the dataset.
#' @param diagram Character. A specific graphic to be presented within the ones
#' considered by the 'logical', 'ordered', 'factor', 'character', 'datetime' and 'numeric'
#' arguments of the 'wideplot()' function.
#' @param output Character. Type of output.
#' \itemize{
#'   \item \emph{'html'}: default output is a html webpage.
#'   \item \emph{'plots pane'}: output in RStudio's plots pane.
#'   \item \emph{'console'}: shows the code that produces a particular graphic.
#' }
#' @param dir Directory in which the files are stored.
#'
#' @return This function can have three outputs: by default it produces a particular graphic,
#' but it can also be represented into the RStudio's plots pane, or can return the code to produce it.
#' @export
#'
#' @examples
#' if (interactive()) {
#' plotup(iris, "Petal.Width", "color heatmap")
#' }
#' plotup(iris, "Petal.Width", "color heatmap", output = "console")
plotup <- function(data,
                   vars,
                   diagram,
                   output = 'html',
                   dir = tempdir()
                   )
{
  if (rmarkdown::pandoc_available("1.12.3") == FALSE) {print(warning_pandoc)}
  else if (rmarkdown::pandoc_available("1.12.3") == TRUE) {
  my_env <- new.env()
  if(is.data.frame(data) == FALSE) {
    stop("I am so sorry, but this function only works with a data.frame input!\n",
         "You have provided an object of class ", class(data))
  }
  if(tibble::is_tibble(data) == TRUE) {
    stop(warning_tibble)
    # data <- as.data.frame(data)
    # envir=my_env
  }
  string      <- " argument expects a character vector"
  if(is.character(vars)  == FALSE) {
    stop(paste0("The 'vars'",  string))
  }
    if(length(vars) > 2) {
    stop("I am so sorry but, up to now, only one and two variables combinations have been considered.")
  }
  ## Value validation: function's argument
  ### dataset
  ### variable
  if (length(vars) == 1 &
      (
        is.logical(unlist(data[, vars])) == TRUE |
        is.factor(unlist(data[, vars])) == TRUE |
        is.ordered(unlist(data[, vars])) == TRUE |
        is.character(unlist(data[, vars])) == TRUE
      )) {
    long <- length(unique(unlist(data[, vars]))) / 6 + 0.5
  }
  else if (length(vars) == 1 &
           (is.numeric(unlist(data[, vars])) == TRUE |
            lubridate::is.instant(unlist(data[, vars])) == TRUE)) {
    long <- 4
  }
  else if (length(vars) == 2 &
           is.numeric(unlist(data[, vars[1]])) == TRUE &
           is.numeric(unlist(data[, vars[2]])) == TRUE) {
    long <- 4
  }
  else {stop("This type of variable has not been yet considered")}
    ### diagram
  string      <- " argument expects a character string"
  if(length(vars) == 1 & lubridate::is.instant(unlist(data[, vars])) == TRUE & (diagram %in% datetime_v) == FALSE)
  {stop(paste0("The 'diagram'", string, " which values can be :\n '",
               paste0(head(datetime_v, 10), collapse = "'\n '"), "'\n  ...\n ", spmn1))
  }
  if(length(vars) == 1 & is.logical(unlist(data[, vars])) == TRUE & (diagram %in% logical_v) == FALSE)
  {stop(paste0("The 'diagram'", string, " which values can be :\n '",
               paste0(head(logical_v, 10), collapse = "'\n '"), "'\n  ...\n ", spmn1))
  }
  if(length(vars) == 1 & is.ordered(unlist(data[, vars])) == TRUE & (diagram %in% ordered_v) == FALSE)
  {stop(paste0("The 'diagram'", string, " which values can be :\n '",
               paste0(head(ordered_v, 10), collapse = "'\n '"), "'\n  ...\n ", spmn1))
  }
  if(length(vars) == 1 & is.factor(unlist(data[, vars])) == TRUE & is.ordered(unlist(data[, vars])) == FALSE & (diagram %in% factor_v) == FALSE)
  {stop(paste0("The 'diagram'", string, " which values can be :\n '",
               paste0(head(factor_v, 10), collapse = "'\n '"), "'\n  ...\n ", spmn1))
  }
  if(length(vars) == 1 & is.numeric(unlist(data[, vars])) == TRUE & (diagram %in% numeric_v == FALSE))
  {stop(paste0("The 'diagram'", string, " which values can be :\n '",
               paste0(head(numeric_v, 10), collapse = "'\n '"), "'\n  ...\n ", spmn1))
  }
  if(length(vars) == 2 & is.numeric(unlist(data[, vars])) == TRUE & (diagram %in% numeric2_v == FALSE))
  {stop(paste0("The 'diagram'", string, " which values can be :\n '",
               paste0(head(numeric2_v, 10), collapse = "'\n '"), "'\n  ...\n ", spmn2))
  }
  if(length(vars) == 1 & is.character(unlist(data[, vars])) == TRUE & (diagram %in% character_v) == FALSE)
  {stop(paste0("The 'diagram'", string, " which values can be :\n '",
               paste0(head(character_v, 10), collapse = "'\n '"), "'\n  ...\n ", spmn1))
  }
  ### output
  output_v <- c('html',
                'plots pane',
                'console'
  )
  if(length(output) != sum(output %in% output_v, na.rm = TRUE))
  {stop(paste0("The 'output' argument expects a value that can be : '",
               paste0(output_v, collapse = "', '"), "'"))
  }
  theme <- "theme_set(theme_minimal())\n"
  theme_detail <- "theme(panel.grid = element_line(colour = NA),
  \x20\x20axis.ticks = element_line(color = 'black'))\n"
  theme_detail_y <- "theme(panel.grid = element_line(colour = NA),
  \x20\x20axis.text.y = element_text(color = NA),
  \x20\x20axis.title.y = element_text(color = NA),
  \x20\x20axis.ticks.x = element_line(color = 'black'))\n"
  theme_detail_z <- "theme(panel.grid = element_line(colour = NA),
  \x20\x20axis.ticks = element_line(color = 'black'),
  \x20\x20legend.position='none')\n"
  theme_detail_yz <- "theme(panel.grid = element_line(colour = NA),
  \x20\x20axis.text.y =element_text(color = NA),
  \x20\x20axis.title.y =element_text(color = NA),
  \x20\x20axis.ticks.x =element_line(color = 'black'),
  \x20\x20legend.position='none')\n"
  getdens2 <- "get_density <- function(x, y, ...) {
  dens <- MASS::kde2d(x, y, ...)
  ix <- findInterval(x, dens$x)
  iy <- findInterval(y, dens$y)
  ii <- cbind(ix, iy)
  return(dens$z[ii])
}\n"
  scale_bw_l <- "scale_color_gradientn(colours = colorRampPalette(c('#E5E5E5', '#000000'))(2))"
  scale_color_l <- "scale_color_gradientn(colours = colorRampPalette(rev(RColorBrewer::brewer.pal(4, 'Spectral')))(3))"
  scale_bw_a <- "scale_fill_gradientn(colours = colorRampPalette(c('#E5E5E5', '#000000'))(2))"
  scale_color_a <- "scale_fill_gradientn(colours = colorRampPalette(rev(RColorBrewer::brewer.pal(4, 'Spectral')))(3))"
  getdens1 <- "add_density_1D <- function(a, b) {
  a$b <- unlist(a[, b])
  if (length(unique(na.omit(a$b))) == 1) {
    dens <- 1 / length(a$b)
  } else if (any(is.na(a$b)) == FALSE) {
    dens <-
      sm::sm.density(a$b, eval.points = a$b, display = 'none')$estimate
  } else {
    ind <- which(is.na(a$b) == TRUE)
    dens <-
      sm::sm.density(na.omit(a$b),
                     eval.points = na.omit(a$b),
                     display = 'none')$estimate
    for (i in 1:(length(ind)))
      dens <- append(dens, NA, after = (ind[i] + 1) - 2)
  }
  return(dens)
}\n"
  unfold <- eval("foo_df <- reshape(
  \x20\x20data = {deparse(substitute(data))},
  \x20\x20direction = 'long',
  \x20\x20v.names = 'measure',
  \x20\x20timevar = 'variable',
  \x20\x20idvar   = 'foo_id',
  \x20\x20varying = c('{as.character(substitute(vars1))}', '{as.character(substitute(vars2))}'),
  \x20\x20times   = c('{as.character(substitute(vars1))}', '{as.character(substitute(vars2))}')
  )\n")
  reorder_freq <- paste0(deparse(substitute(data)),
                         "[['",
                         as.character(substitute(vars)),
                         "']] <- forcats::fct_infreq(",
                         deparse(substitute(data)),
                         "[['",
                         as.character(substitute(vars)),
                         "']], ordered = TRUE)")
  reorder_alphab <- paste0(deparse(substitute(data)),
                         "[['",
                         as.character(substitute(vars)),
                         "']] <- as.character(",
                         deparse(substitute(data)),
                         "[['",
                         as.character(substitute(vars)),
                         "']])")
  reorder_observ <- paste0(deparse(substitute(data)),
                         "[['",
                         as.character(substitute(vars)),
                         "']] <- factor(",
                         deparse(substitute(data)),
                         "[['",
                         as.character(substitute(vars)),
                         "']], levels = unique(",
                         deparse(substitute(data)),
                         "[['",
                         as.character(substitute(vars)),
                             "']]))")
  if (diagram == "blank") {
    vars <- vars[1]
    p <- glue::glue(
      "{theme}
      ggplot({deparse(substitute(data))}, aes(x=seq_along({as.character(substitute(vars))}), y={as.character(substitute(vars))})) +
      \x20\x20geom_blank() +
      \x20\x20labs(x='seq') +
      \x20\x20theme(axis.title=element_blank(),
      \x20\x20\x20\x20axis.text=element_blank(),
      \x20\x20\x20\x20axis.ticks=element_blank(),
      \x20\x20\x20\x20panel.grid = element_line(colour = NA))"
    )
  }
  else if (length(vars) == 1 &
           diagram == "normal qq plot" &
           is.numeric(unlist(data[, vars])) == TRUE
  ) {
    vars <- vars[1]
    p <- paste0("
                qqplot <- function (pp_df,
                pp_var,
                pp_size = 0.5) {
                pp_df$pp_var <- unlist(pp_df[, pp_var])
                y <- stats::quantile(pp_df$pp_var[!is.na(pp_df$pp_var)], c(0.25, 0.75))
                x <- stats::qnorm(c(0.25, 0.75))
                slope <- diff(y)/diff(x)
                int <- y[1L] - slope * x[1L]
                d <- data.frame(resids = pp_df$pp_var)
                ggplot(d, aes_(sample = ~resids)) +
                stat_qq(size=pp_size) +
                geom_abline(slope = slope, intercept = int, size=pp_size) +
                labs(y=names(pp_df[pp_var])) +
                pp_theme()
                }
                qqplot(",
                deparse(substitute(data)),
                ", '",
                as.character(substitute(vars)),
                "')
                ")
  }
  else if (length(vars) == 1 &
           diagram == "3 uniaxial" &
           is.numeric(unlist(data[, vars])) == TRUE
  ) {
    vars <- vars[1]
    p <- paste0("
pp_3uniaxial <- function(data,
                         variable,
                         pp_size = 2)  {
  data$variable <- unlist(data[, variable])
  pp_plot <- ggplot(data,
    aes_string(y=variable),
    environment = environment()) +
    labs(x=names(data[variable])) +
    geom_boxplot(aes(x=1), width = 0.5, size=pp_size/4) +
    geom_point(aes(x=2), size=pp_size, alpha=.1) +
    geom_violin(aes(x=3), size=pp_size/4) +
    scale_x_continuous(breaks = c(1, 2, 3), labels = c('box', 'dot', 'violin')) +
    theme(axis.title.y=element_blank()) +
    coord_flip() +
    pp_theme()
    pp_plot
  }
pp_3uniaxial(",
                deparse(substitute(data)),
                ", '",
                as.character(substitute(vars)),
                "')
  ")
  }
  else if (length(vars) == 1 &
           diagram == "freq. reordered line graph") {
    vars <- vars[1]
    p <- glue::glue(
      "{theme}{reorder_freq}
      ggplot({deparse(substitute(data))}, aes(x=seq_along({as.character(substitute(vars))}), y={as.character(substitute(vars))})) +
      \x20\x20geom_path(aes(group=1)) +
      \x20\x20labs(x='seq') +
      \x20\x20{theme_detail}"
    )
  }
  else if (length(vars) == 1 &
           diagram == "alphab. reordered line graph") {
    vars <- vars[1]
    p <- glue::glue(
      "{theme}{reorder_alphab}
      ggplot({deparse(substitute(data))}, aes(x=seq_along({as.character(substitute(vars))}), y={as.character(substitute(vars))})) +
      \x20\x20geom_path(aes(group=1)) +
      \x20\x20labs(x='seq') +
      \x20\x20{theme_detail}"
    )
  }
  else if (length(vars) == 1 &
           diagram == "point-to-point graph") {
    vars <- vars[1]
    p <- glue::glue(
      "{theme}
      ggplot({deparse(substitute(data))}, aes(x=seq_along({as.character(substitute(vars))}), y={as.character(substitute(vars))})) +
      \x20\x20geom_path(aes(group=1)) +
      \x20\x20geom_point() +
      \x20\x20labs(x='seq') +
      \x20\x20{theme_detail}"
    )
  }
  else if (length(vars) == 1 &
           diagram == "freq. reordered point-to-point graph") {
    vars <- vars[1]
    p <- glue::glue(
      "{theme}{reorder_freq}
      ggplot({deparse(substitute(data))}, aes(x=seq_along({as.character(substitute(vars))}), y={as.character(substitute(vars))})) +
      \x20\x20geom_path(aes(group=1)) +
      \x20\x20geom_point() +
      \x20\x20labs(x='seq') +
      \x20\x20{theme_detail}"
    )
  }
  else if (length(vars) == 1 &
           diagram == "alphab. reordered point-to-point graph") {
    vars <- vars[1]
    p <- glue::glue(
      "{theme}{reorder_alphab}
      ggplot({deparse(substitute(data))}, aes(x=seq_along({as.character(substitute(vars))}), y={as.character(substitute(vars))})) +
      \x20\x20geom_path(aes(group=1)) +
      \x20\x20geom_point() +
      \x20\x20labs(x='seq') +
      \x20\x20{theme_detail}"
    )
  }
  else if (length(vars) == 1 &
           diagram == "stepped point-to-point graph" &
           (is.numeric(unlist(data[, vars])) == TRUE |
            lubridate::is.instant(unlist(data[, vars])) == TRUE)) {
    vars <- vars[1]
    p <- glue::glue(
      "{theme}
      ggplot({deparse(substitute(data))}, aes(x=seq_along({as.character(substitute(vars))}), y={as.character(substitute(vars))})) +
      \x20\x20geom_step(aes(group=1)) +
      \x20\x20geom_point() +
      \x20\x20labs(x='seq') +
      \x20\x20{theme_detail}"
    )
  }
  else if (length(vars) == 1 &
           diagram == "point graph") {
    vars <- vars[1]
    p <- glue::glue(
      "{theme}{reorder_observ}
      ggplot({deparse(substitute(data))}, aes(x=seq_along({as.character(substitute(vars))}), y={as.character(substitute(vars))})) +
      \x20\x20geom_point() +
      \x20\x20labs(x='seq') +
      \x20\x20{theme_detail}"
    )
  }
  else if (length(vars) == 1 &
           diagram == "freq. reordered point graph") {
    vars <- vars[1]
    p <- glue::glue(
      "{theme}{reorder_freq}
      ggplot({deparse(substitute(data))}, aes(x=seq_along({as.character(substitute(vars))}), y={as.character(substitute(vars))})) +
      \x20\x20geom_point() +
      \x20\x20labs(x='seq') +
      \x20\x20{theme_detail}"
    )
  }
  else if (length(vars) == 1 &
           diagram == "alphab. reordered point graph") {
    vars <- vars[1]
    p <- glue::glue(
      "{theme}{reorder_alphab}
      ggplot({deparse(substitute(data))}, aes(x=seq_along({as.character(substitute(vars))}), y={as.character(substitute(vars))})) +
      \x20\x20geom_point() +
      \x20\x20labs(x='seq') +
      \x20\x20{theme_detail}"
    )
  }
  else if (length(vars) == 1 &
           diagram == "point graph with trend line" &
           is.numeric(unlist(data[, vars])) == TRUE) {
    vars <- vars[1]
    p <- glue::glue(
      "{theme}
      ggplot({deparse(substitute(data))}, aes(x=seq_along({as.character(substitute(vars))}), y={as.character(substitute(vars))})) +
      \x20\x20geom_point() +
      \x20\x20geom_smooth(method = 'loess', size=0.5) +
      \x20\x20labs(x='seq') +
      \x20\x20{theme_detail}"
    )
  }
  else if (length(vars) == 1 &
           diagram == "line graph" & (
             is.logical(unlist(data[, vars])) == TRUE |
             is.factor(unlist(data[, vars])) == TRUE |
             is.ordered(unlist(data[, vars])) == TRUE |
             is.character(unlist(data[, vars])) == TRUE
           )) {
    vars <- vars[1]
    p <- glue::glue(
      "{theme}{reorder_observ}
      ggplot({deparse(substitute(data))}, aes(x=seq_along({as.character(substitute(vars))}), y={as.character(substitute(vars))})) +
      \x20\x20geom_path(aes(group=1)) +
      \x20\x20labs(x='seq') +
      \x20\x20{theme_detail}"
    )
  }
  else if (length(vars) == 1 &
           diagram == "tile plot" & (
             is.logical(unlist(data[, vars])) == TRUE |
             is.factor(unlist(data[, vars])) == TRUE |
             is.ordered(unlist(data[, vars])) == TRUE |
             is.character(unlist(data[, vars])) == TRUE
           )) {
    vars <- vars[1]
    p <- glue::glue(
      "{theme}{reorder_observ}
      ggplot({deparse(substitute(data))}, aes(x=seq_along({as.character(substitute(vars))}), y={as.character(substitute(vars))})) +
      \x20\x20geom_tile() +
      \x20\x20labs(x='seq') +
      \x20\x20{theme_detail}"
    )
  }
  else if (length(vars) == 1 &
           diagram == "freq. reordered tile plot" & (
             is.logical(unlist(data[, vars])) == TRUE |
             is.factor(unlist(data[, vars])) == TRUE |
             is.ordered(unlist(data[, vars])) == TRUE |
             is.character(unlist(data[, vars])) == TRUE
           )) {
    vars <- vars[1]
    p <- glue::glue(
      "{theme}{reorder_freq}
      ggplot({deparse(substitute(data))}, aes(x=seq_along({as.character(substitute(vars))}), y={as.character(substitute(vars))})) +
      \x20\x20geom_tile() +
      \x20\x20labs(x='seq') +
      \x20\x20{theme_detail}"
    )
  }
  else if (length(vars) == 1 &
           diagram == "alphab. reordered tile plot" & (
             is.logical(unlist(data[, vars])) == TRUE |
             is.factor(unlist(data[, vars])) == TRUE |
             is.ordered(unlist(data[, vars])) == TRUE |
             is.character(unlist(data[, vars])) == TRUE
           )) {
    vars <- vars[1]
    p <- glue::glue(
      "{theme}{reorder_alphab}
      ggplot({deparse(substitute(data))}, aes(x=seq_along({as.character(substitute(vars))}), y={as.character(substitute(vars))})) +
      \x20\x20geom_tile() +
      \x20\x20labs(x='seq') +
      \x20\x20{theme_detail}"
    )
  }
  else if (length(vars) == 1 &
           diagram == "bar graph" &
           (is.numeric(unlist(data[, vars])) == TRUE |
            lubridate::is.instant(unlist(data[, vars])) == TRUE)) {
    vars <- vars[1]
    p <- glue::glue(
      "{theme}
      binwidth <- (max({substitute(data)}['{substitute(vars)}'], na.rm=TRUE)-min({substitute(data)}['{substitute(vars)}'], na.rm=TRUE))/100
      ggplot({deparse(substitute(data))}, aes(x={as.character(substitute(vars))})) +
      \x20\x20geom_bar(stat='count', width=binwidth, fill='black', color='black', position = 'identity') +
      \x20\x20{theme_detail}"
    )
  }
  else if (length(vars) == 1 &
           diagram == "bw bar graph" &
           (is.numeric(unlist(data[, vars])) == TRUE |
            lubridate::is.instant(unlist(data[, vars])) == TRUE)) {
    vars <- vars[1]
    p <- glue::glue(
      "{theme}
      binwidth <- (max({substitute(data)}['{substitute(vars)}'], na.rm=TRUE)-min({substitute(data)}['{substitute(vars)}'], na.rm=TRUE))/100
      ggplot({deparse(substitute(data))}, aes(x={as.character(substitute(vars))}, fill=..count..)) +
      \x20\x20geom_bar(stat='count', width=binwidth, position = 'identity') +
      \x20\x20{scale_bw_a} +
      \x20\x20{theme_detail_z}"
    )
  }
  else if (length(vars) == 1 &
           diagram == "color bar graph" &
           (is.numeric(unlist(data[, vars])) == TRUE |
            lubridate::is.instant(unlist(data[, vars])) == TRUE)) {
    vars <- vars[1]
    p <- glue::glue(
      "{theme}
      binwidth <- (max({substitute(data)}['{substitute(vars)}'], na.rm=TRUE)-min({substitute(data)}['{substitute(vars)}'], na.rm=TRUE))/100
      ggplot({deparse(substitute(data))}, aes(x={as.character(substitute(vars))}, fill=..count..)) +
      \x20\x20geom_bar(stat='count', width=binwidth, position = 'identity') +
      \x20\x20{scale_color_a} +
      \x20\x20{theme_detail_z}"
    )
  }
  else if (length(vars) == 1 &
           diagram == "bar graph" & (
             is.logical(unlist(data[, vars])) == TRUE |
             is.factor(unlist(data[, vars])) == TRUE |
             is.ordered(unlist(data[, vars])) == TRUE |
             is.character(unlist(data[, vars])) == TRUE
           )) {
    vars <- vars[1]
    p <- glue::glue(
      "{theme}{reorder_observ}
      ggplot({deparse(substitute(data))}, aes(x={as.character(substitute(vars))})) +
      \x20\x20geom_bar(stat='count', width=0.75, fill='black') +
      \x20\x20coord_flip() +
      \x20\x20{theme_detail}"
    )
  }
  else if (length(vars) == 1 &
           diagram == "bw bar graph" & (
             is.logical(unlist(data[, vars])) == TRUE |
             is.factor(unlist(data[, vars])) == TRUE |
             is.ordered(unlist(data[, vars])) == TRUE |
             is.character(unlist(data[, vars])) == TRUE
           )) {
    vars <- vars[1]
    p <- glue::glue(
      "{theme}{reorder_observ}
      ggplot({deparse(substitute(data))}, aes(x={as.character(substitute(vars))}, fill=..count..)) +
      \x20\x20geom_bar(stat='count', width=0.75) +
      \x20\x20{scale_bw_a} +
      \x20\x20coord_flip() +
      \x20\x20{theme_detail_z}"
    )
  }
  else if (length(vars) == 1 &
           diagram == "color bar graph" & (
             is.logical(unlist(data[, vars])) == TRUE |
             is.factor(unlist(data[, vars])) == TRUE |
             is.ordered(unlist(data[, vars])) == TRUE |
             is.character(unlist(data[, vars])) == TRUE
           )) {
    vars <- vars[1]
    p <- glue::glue(
      "{theme}{reorder_observ}
      ggplot({deparse(substitute(data))}, aes(x={as.character(substitute(vars))}, fill=..count..)) +
      \x20\x20geom_bar(stat='count', width=0.75) +
      \x20\x20{scale_color_a} +
      \x20\x20coord_flip() +
      \x20\x20{theme_detail_z}"
    )
  }
  else if (length(vars) == 1 &
           diagram == "freq. reordered bar graph" &
           (is.factor(unlist(data[, vars])) == TRUE |
            is.character(unlist(data[, vars])) == TRUE)) {
    vars <- vars[1]
    p <- glue::glue(
      "{theme}{reorder_freq}
      ggplot({deparse(substitute(data))}, aes(x={as.character(substitute(vars))}, fill=..count..)) +
      \x20\x20geom_bar(stat='count', width=0.75) +
      \x20\x20{scale_bw_a} +
      \x20\x20coord_flip() +
      \x20\x20{theme_detail_z}"
    )
  }
  else if (length(vars) == 1 &
           diagram == "bw freq. reordered bar graph" &
           (is.factor(unlist(data[, vars])) == TRUE |
            is.character(unlist(data[, vars])) == TRUE)) {
    vars <- vars[1]
    p <- glue::glue(
      "{theme}{reorder_freq}
      ggplot({deparse(substitute(data))}, aes(x={as.character(substitute(vars))}, fill=..count..)) +
      \x20\x20geom_bar(stat='count', width=0.75) +
      \x20\x20{scale_bw_a} +
      \x20\x20coord_flip() +
      \x20\x20{theme_detail_z}"
    )
  }
  else if (length(vars) == 1 &
           diagram == "color freq. reordered bar graph" &
           (is.factor(unlist(data[, vars])) == TRUE |
            is.character(unlist(data[, vars])) == TRUE)) {
    vars <- vars[1]
    p <- glue::glue(
      "{theme}{reorder_freq}
      ggplot({deparse(substitute(data))}, aes(x={as.character(substitute(vars))}, fill=..count..)) +
      \x20\x20geom_bar(stat='count', width=0.75) +
      \x20\x20{scale_color_a} +
      \x20\x20coord_flip() +
      \x20\x20{theme_detail_z}"
    )
  }
  else if (length(vars) == 1 &
           diagram == "alphab. reordered bar graph" &
           (is.factor(unlist(data[, vars])) == TRUE |
            is.character(unlist(data[, vars])) == TRUE)) {
    vars <- vars[1]
    p <- glue::glue(
      "{theme}{reorder_alphab}
      ggplot({deparse(substitute(data))}, aes(x={as.character(substitute(vars))}, fill=..count..)) +
      \x20\x20geom_bar(stat='count', width=0.75) +
      \x20\x20{scale_bw_a} +
      \x20\x20coord_flip() +
      \x20\x20{theme_detail_z}"
    )
  }
  else if (length(vars) == 1 &
           diagram == "bw alphab. reordered bar graph" &
           (is.factor(unlist(data[, vars])) == TRUE |
            is.character(unlist(data[, vars])) == TRUE)) {
    vars <- vars[1]
    p <- glue::glue(
      "{theme}{reorder_alphab}
      ggplot({deparse(substitute(data))}, aes(x={as.character(substitute(vars))}, fill=..count..)) +
      \x20\x20geom_bar(stat='count', width=0.75) +
      \x20\x20{scale_bw_a} +
      \x20\x20coord_flip() +
      \x20\x20{theme_detail_z}"
    )
  }
  else if (length(vars) == 1 &
           diagram == "color alphab. reordered bar graph" &
           (is.factor(unlist(data[, vars])) == TRUE |
            is.character(unlist(data[, vars])) == TRUE)) {
    vars <- vars[1]
    p <- glue::glue(
      "{theme}{reorder_alphab}
      ggplot({deparse(substitute(data))}, aes(x={as.character(substitute(vars))}, fill=..count..)) +
      \x20\x20geom_bar(stat='count', width=0.75) +
      \x20\x20{scale_color_a} +
      \x20\x20coord_flip() +
      \x20\x20{theme_detail_z}"
    )
  }
  else if (length(vars) == 1 &
           diagram == "linerange graph" & (
             is.logical(unlist(data[, vars])) == TRUE |
             is.factor(unlist(data[, vars])) == TRUE |
             is.ordered(unlist(data[, vars])) == TRUE |
             is.character(unlist(data[, vars])) == TRUE
           )) {
    vars <- vars[1]
    p <- glue::glue(
      "{theme}{reorder_observ}
      ggplot({deparse(substitute(data))}, aes(y={as.character(substitute(vars))})) +
      \x20\x20geom_path(aes(x=seq_along({substitute(vars)})), size=0.5) +
      \x20\x20geom_point(aes(x=seq_along({substitute(vars)})), size=1) +
      \x20\x20labs(x='seq.') +
      \x20\x20{theme_detail_z}"
    )
  }
  else if (length(vars) == 1 &
           diagram == "freq. reordered linerange graph" & (
             is.logical(unlist(data[, vars])) == TRUE |
             is.factor(unlist(data[, vars])) == TRUE |
             is.ordered(unlist(data[, vars])) == TRUE |
             is.character(unlist(data[, vars])) == TRUE
           )) {
    vars <- vars[1]
    p <- glue::glue(
      "{theme}{reorder_freq}
      ggplot({deparse(substitute(data))}, aes(y={as.character(substitute(vars))})) +
      \x20\x20geom_path(aes(x=seq_along({substitute(vars)})), size=0.5) +
      \x20\x20geom_point(aes(x=seq_along({substitute(vars)})), size=1) +
      \x20\x20labs(x='seq.') +
      \x20\x20{theme_detail_z}"
    )
  }
  else if (length(vars) == 1 &
           diagram == "alphab. reordered linerange graph" & (
             is.logical(unlist(data[, vars])) == TRUE |
             is.factor(unlist(data[, vars])) == TRUE |
             is.ordered(unlist(data[, vars])) == TRUE |
             is.character(unlist(data[, vars])) == TRUE
           )) {
    vars <- vars[1]
    p <- glue::glue(
      "{theme}{reorder_alphab}
      ggplot({deparse(substitute(data))}, aes(y={as.character(substitute(vars))})) +
      \x20\x20geom_path(aes(x=seq_along({substitute(vars)})), size=0.5) +
      \x20\x20geom_point(aes(x=seq_along({substitute(vars)})), size=1) +
      \x20\x20labs(x='seq.') +
      \x20\x20{theme_detail_z}"
    )
  }
  else if (length(vars) == 1 &
           diagram == "binned heatmap" &
           (
             is.factor(unlist(data[, vars])) == TRUE |
             is.character(unlist(data[, vars])) == TRUE |
             is.logical(unlist(data[, vars])) == TRUE |
             is.ordered(unlist(data[, vars])) == TRUE
           )) {
    vars <- vars[1]
    p <- glue::glue(
      "{theme}{reorder_observ}
      ggplot({deparse(substitute(data))}, aes(y={as.character(substitute(vars))})) +
      \x20\x20geom_bin2d(aes(x=seq_along({substitute(vars)})), fill = 'black') +
      \x20\x20labs(x='seq.') +
      \x20\x20{theme_detail_z}"
    )
  }
  else if (length(vars) == 1 &
           diagram == "binned heatmap" &
           (is.numeric(unlist(data[, vars])) == TRUE |
            lubridate::is.instant(unlist(data[, vars])) == TRUE)) {
    vars <- vars[1]
    p <- glue::glue(
      "{theme}
      ggplot({deparse(substitute(data))}, aes(y={as.character(substitute(vars))})) +
      \x20\x20geom_bin2d(aes(x=seq_along({substitute(vars)})), fill = 'black') +
      \x20\x20labs(x='seq.') +
      \x20\x20{theme_detail_z}"
    )
  }
  else if (length(vars) == 1 &
           diagram == "bw binned heatmap" &
           (
             is.factor(unlist(data[, vars])) == TRUE |
             is.character(unlist(data[, vars])) == TRUE |
             is.logical(unlist(data[, vars])) == TRUE |
             is.ordered(unlist(data[, vars])) == TRUE
           )) {
    vars <- vars[1]
    p <- glue::glue(
      "{theme}
      ggplot({deparse(substitute(data))}, aes(y={as.character(substitute(vars))})) +
      \x20\x20geom_bin2d(aes(x=seq_along({substitute(vars)}))) +
      \x20\x20{scale_bw_a}
      \x20\x20labs(x='seq.') +
      \x20\x20{theme_detail_z}"
    )
  }
  else if (length(vars) == 1 &
           diagram == "bw heatmap" &
           (is.numeric(unlist(data[, vars])) == TRUE |
            lubridate::is.instant(unlist(data[, vars])) == TRUE)) {
    vars <- vars[1]
    p <- glue::glue(
      "{theme}
      ggplot({deparse(substitute(data))}, aes(y={as.character(substitute(vars))})) +
      \x20\x20stat_density_2d(aes(x=seq_along({substitute(vars)}), fill = stat(density)), geom = 'raster', contour = FALSE) +
      \x20\x20{scale_bw_a} +
      \x20\x20labs(x='seq.') +
      \x20\x20{theme_detail_z}"
    )
  }
  else if (length(vars) == 1 &
           diagram == "color heatmap" &
           (is.numeric(unlist(data[, vars])) == TRUE |
            lubridate::is.instant(unlist(data[, vars])) == TRUE)) {
    vars <- vars[1]
    p <- glue::glue(
      "{theme}
      ggplot({deparse(substitute(data))}, aes(y={as.character(substitute(vars))})) +
      \x20\x20stat_density_2d(aes(x=seq_along({substitute(vars)}), fill = stat(density)), geom = 'raster', contour = FALSE) +
      \x20\x20{scale_color_a} +
      \x20\x20labs(x='seq.') +
      \x20\x20{theme_detail_z}"
    )
  }
  else if (length(vars) == 1 &
           diagram == "bw binned heatmap" &
           (is.numeric(unlist(data[, vars])) == TRUE |
            lubridate::is.instant(unlist(data[, vars])) == TRUE)) {
    vars <- vars[1]
    p <- glue::glue(
      "{theme}
      ggplot({deparse(substitute(data))}, aes(y={as.character(substitute(vars))})) +
      \x20\x20geom_bin2d(aes(x=seq_along({substitute(vars)}))) +
      \x20\x20{scale_bw_a} +
      \x20\x20labs(x='seq.') +
      \x20\x20{theme_detail_z}"
    )
  }
  else if (length(vars) == 1 &
           diagram == "color binned heatmap" &
           (
             is.factor(unlist(data[, vars])) == TRUE |
             is.character(unlist(data[, vars])) == TRUE |
             is.logical(unlist(data[, vars])) == TRUE |
             is.ordered(unlist(data[, vars])) == TRUE
           )) {
    vars <- vars[1]
    p <- glue::glue(
      "{theme}{reorder_observ}
      ggplot({deparse(substitute(data))}, aes(y={as.character(substitute(vars))})) +
      \x20\x20geom_bin2d(aes(x=seq_along({substitute(vars)}))) +
      \x20\x20{scale_color_a} +
      \x20\x20labs(x='seq.') +
      \x20\x20{theme_detail_z}"
    )
  }
  else if (length(vars) == 1 &
           diagram == "color binned heatmap" &
           (is.numeric(unlist(data[, vars])) == TRUE |
            lubridate::is.instant(unlist(data[, vars])) == TRUE)) {
    vars <- vars[1]
    p <- glue::glue(
      "{theme}
      ggplot({deparse(substitute(data))}, aes(y={as.character(substitute(vars))})) +
      \x20\x20geom_bin2d(aes(x=seq_along({substitute(vars)}))) +
      \x20\x20{scale_color_a} +
      \x20\x20labs(x='seq.') +
      \x20\x20{theme_detail_z}"
    )
  }
  else if (length(vars) == 1 &
           diagram == "freq. reordered binned heatmap" &
           (is.factor(unlist(data[, vars])) == TRUE |
            is.character(unlist(data[, vars])) == TRUE)) {
    vars <- vars[1]
    p <- glue::glue(
      "{theme}{reorder_freq}
      ggplot({deparse(substitute(data))}, aes(y={as.character(substitute(vars))})) +
      \x20\x20geom_bin2d(aes(x=seq_along({substitute(vars)})), fill = 'black') +
      \x20\x20labs(x='seq.') +
      \x20\x20{theme_detail_z}"
    )
  }
  else if (length(vars) == 1 &
           diagram == "bw freq. reordered binned heatmap" &
           (is.factor(unlist(data[, vars])) == TRUE |
            is.character(unlist(data[, vars])) == TRUE)) {
    vars <- vars[1]
    p <- glue::glue(
      "{theme}{reorder_freq}
      ggplot({deparse(substitute(data))}, aes(y={as.character(substitute(vars))})) +
      \x20\x20geom_bin2d(aes(x=seq_along({substitute(vars)}))) +
      \x20\x20labs(x='seq.') +
      \x20\x20{scale_bw_a} +
      \x20\x20{theme_detail_z}"
    )
  }
  else if (length(vars) == 1 &
           diagram == "color freq. reordered binned heatmap" &
           (is.factor(unlist(data[, vars])) == TRUE |
            is.character(unlist(data[, vars])) == TRUE)) {
    vars <- vars[1]
    p <- glue::glue(
      "{theme}{reorder_freq}
      ggplot({deparse(substitute(data))}, aes(y={as.character(substitute(vars))})) +
      \x20\x20geom_bin2d(aes(x=seq_along({substitute(vars)}))) +
      \x20\x20labs(x='seq.') +
      \x20\x20{scale_color_a} +
      \x20\x20{theme_detail_z}"
    )
  }
  else if (length(vars) == 1 &
           diagram == "alphab. reordered binned heatmap" &
           (is.factor(unlist(data[, vars])) == TRUE |
            is.character(unlist(data[, vars])) == TRUE)) {
    vars <- vars[1]
    p <- glue::glue(
      "{theme}{reorder_alphab}
      ggplot({deparse(substitute(data))}, aes(y={as.character(substitute(vars))})) +
      \x20\x20geom_bin2d(aes(x=seq_along({substitute(vars)})), fill = 'black') +
      \x20\x20labs(x='seq.') +
      \x20\x20{theme_detail_z}"
    )
  }
  else if (length(vars) == 1 &
           diagram == "bw alphab. reordered binned heatmap" &
           (is.factor(unlist(data[, vars])) == TRUE |
            is.character(unlist(data[, vars])) == TRUE)) {
    vars <- vars[1]
    p <- glue::glue(
      "{theme}{reorder_alphab}
      ggplot({deparse(substitute(data))}, aes(y={as.character(substitute(vars))})) +
      \x20\x20geom_bin2d(aes(x=seq_along({substitute(vars)}))) +
      \x20\x20labs(x='seq.') +
      \x20\x20{scale_bw_a} +
      \x20\x20{theme_detail_z}"
    )
  }
  else if (length(vars) == 1 &
           diagram == "color alphab. reordered binned heatmap" &
           (is.factor(unlist(data[, vars])) == TRUE |
            is.character(unlist(data[, vars])) == TRUE)) {
    vars <- vars[1]
    p <- glue::glue(
      "{theme}{reorder_alphab}
      ggplot({deparse(substitute(data))}, aes(y={as.character(substitute(vars))})) +
      \x20\x20geom_bin2d(aes(x=seq_along({substitute(vars)}))) +
      \x20\x20labs(x='seq.') +
      \x20\x20{scale_color_a} +
      \x20\x20{theme_detail_z}"
    )
  }
  else if (length(vars) == 1 &
           diagram == "stepped line graph" &
           (is.numeric(unlist(data[, vars])) == TRUE |
           lubridate::is.instant(unlist(data[, vars])) == TRUE)) {
    vars <- vars[1]
    p <- glue::glue(
      "{theme}
      ggplot({deparse(substitute(data))}, aes(x=seq_along({as.character(substitute(vars))}), y={as.character(substitute(vars))})) +
      \x20\x20geom_step(direction = 'hv') +
      \x20\x20labs(x='seq') +
      \x20\x20{theme_detail}"
    )
  }
  else if (length(vars) == 1 &
           diagram == "line graph" &
           (is.numeric(unlist(data[, vars])) == TRUE |
           lubridate::is.instant(unlist(data[, vars])) == TRUE)) {
    vars <- vars[1]
    p <- glue::glue(
      "{theme}
      ggplot({deparse(substitute(data))}, aes(x=seq_along({as.character(substitute(vars))}), y={as.character(substitute(vars))})) +
      \x20\x20geom_line() +
      \x20\x20labs(x='seq') +
      \x20\x20{theme_detail}"
    )
  }
  else if (length(vars) == 1 &
           diagram == "area graph" &
           is.numeric(unlist(data[, vars])) == TRUE) {
    vars <- vars[1]
    p <- glue::glue(
      "{theme}
      ggplot({deparse(substitute(data))}, aes(x=seq_along({as.character(substitute(vars))}), y={as.character(substitute(vars))})) +
      \x20\x20geom_area(fill = 'black') +
      \x20\x20labs(x='seq') +
      \x20\x20{theme_detail}"
    )
  }
  else if (length(vars) == 1 &
           diagram == "stepped area graph" &
           is.numeric(unlist(data[, vars])) == TRUE) {
    vars <- vars[1]
    p <- glue::glue(
      "{theme}
      ggplot({deparse(substitute(data))}, aes(x=seq_along({as.character(substitute(vars))}), y={as.character(substitute(vars))})) +
      \x20\x20geom_bar(fill = 'black', width = 1, stat = 'identity') +
      \x20\x20labs(x='seq') +
      \x20\x20{theme_detail}"
    )
  }
  else if (length(vars) == 1 &
           diagram == "bw stepped area graph" &
           is.numeric(unlist(data[, vars])) == TRUE) {
    vars <- vars[1]
    p <- glue::glue(
      "{theme}
      ggplot({deparse(substitute(data))}, aes(x=seq_along({as.character(substitute(vars))}), y={as.character(substitute(vars))}, fill={as.character(substitute(vars))})) +
      \x20\x20geom_bar(width = 1, stat = 'identity') +
      \x20\x20{scale_bw_a} +
      \x20\x20labs(x='seq') +
      \x20\x20{theme_detail_z}"
    )
  }
  else if (length(vars) == 1 &
           diagram == "color stepped area graph" &
           is.numeric(unlist(data[, vars])) == TRUE) {
    vars <- vars[1]
    p <- glue::glue(
      "{theme}
      ggplot({deparse(substitute(data))}, aes(x=seq_along({as.character(substitute(vars))}), y={as.character(substitute(vars))}, fill={as.character(substitute(vars))})) +
      \x20\x20geom_bar(width = 1, stat = 'identity') +
      \x20\x20{scale_color_a} +
      \x20\x20labs(x='seq') +
      \x20\x20{theme_detail_z}"
    )
  }
  else if (length(vars) == 1 &
           diagram == "histogram" &
           is.numeric(unlist(data[, vars])) == TRUE) {
    vars <- vars[1]
    p <- glue::glue(
      "{theme}
      ggplot({deparse(substitute(data))}, aes(x={as.character(substitute(vars))})) +
      \x20\x20geom_histogram(bins = 20, fill='black') +
      \x20\x20{theme_detail}"
    )
  }
  else if (length(vars) == 1 &
           diagram == "bw histogram" &
           is.numeric(unlist(data[, vars])) == TRUE) {
    vars <- vars[1]
    p <- glue::glue(
      "{theme}
      ggplot({deparse(substitute(data))}, aes(x={as.character(substitute(vars))}, fill=..count..)) +
      \x20\x20geom_histogram(bins = 20) +
      \x20\x20{scale_bw_a} +
      \x20\x20{theme_detail_z}"
    )
  }
  else if (length(vars) == 1 &
           diagram == "color histogram" &
           is.numeric(unlist(data[, vars])) == TRUE) {
    vars <- vars[1]
    p <- glue::glue(
      "{theme}
      ggplot({deparse(substitute(data))}, aes(x={as.character(substitute(vars))}, fill=..count..)) +
      \x20\x20geom_histogram(bins = 20) +
      \x20\x20{scale_color_a} +
      \x20\x20{theme_detail_z}"
    )
  }
  else if (length(vars) == 1 &
           diagram == "freq. polygon" &
           is.numeric(unlist(data[, vars])) == TRUE) {
    vars <- vars[1]
    p <- glue::glue(
      "{theme}
      ggplot({deparse(substitute(data))}, aes(x={as.character(substitute(vars))})) +
      \x20\x20geom_line(stat = 'bin', bins = 20, center = 0, size = 0.5) +
      \x20\x20{theme_detail}"
    )
  }
  else if (length(vars) == 1 &
           diagram == "density plot" &
           is.numeric(unlist(data[, vars])) == TRUE) {
    vars <- vars[1]
    p <- glue::glue(
      "{theme}
      ggplot({deparse(substitute(data))},
      \x20\x20aes(x = {as.character(substitute(vars))})) +
      \x20\x20geom_density(size = 0.5) +
      \x20\x20{theme_detail}
      "
    )
  }
  else if (length(vars) == 1 &
           diagram == "filled density plot" &
           is.numeric(unlist(data[, vars])) == TRUE) {
    vars <- vars[1]
    p <- glue::glue(
      "{theme}
      ggplot({deparse(substitute(data))},
      \x20\x20aes(x = {as.character(substitute(vars))})) +
      \x20\x20geom_density(fill = 'black') +
      \x20\x20{theme_detail}
      "
    )
  }
  else if (length(vars) == 1 &
           diagram == "violin plot" &
           is.numeric(unlist(data[, vars])) == TRUE) {
    vars <- vars[1]
    p <- glue::glue(
      "{theme}
      ggplot({deparse(substitute(data))},
      \x20\x20aes(x = 0, y = {as.character(substitute(vars))})) +
      \x20\x20geom_violin(size = 0.5) +
      \x20\x20coord_flip() +
      \x20\x20{theme_detail_y}
      "
    )
  }  else if (length(vars) == 1 &
              diagram == "filled violin plot" &
              is.numeric(unlist(data[, vars])) == TRUE) {
    vars <- vars[1]
    p <- glue::glue(
      "{theme}
      ggplot({deparse(substitute(data))},
      \x20\x20aes(x = 0, y = {as.character(substitute(vars))})) +
      \x20\x20geom_violin(fill = 'black') +
      \x20\x20coord_flip() +
      \x20\x20{theme_detail_y}
      "
    )
  }
  # else if (length(vars) == 1 &
  #          diagram == "bw point graph" &
  #          is.numeric(unlist(data[, vars])) == TRUE) {
  #   vars <- vars[1]
  #   p <- glue::glue(
  #     "{theme}{getdens1}
  #     ggplot({deparse(substitute(data))},
  #     \x20\x20aes(x = seq_along({as.character(substitute(vars))}), y = {as.character(substitute(vars))})) +
  #     \x20\x20geom_point(aes(color=add_density_1D({deparse(substitute(data))}, '{as.character(substitute(vars))}'))) +
  #     \x20\x20{scale_bw_l} +
  #     \x20\x20labs(x='seq') +
  #     \x20\x20{theme_detail_z}"
  #   )
  # }
  # else if (length(vars) == 1 &
  #          diagram == "bw point graph with trend line" &
  #          is.numeric(unlist(data[, vars])) == TRUE) {
  #   vars <- vars[1]
  #   p <- glue::glue(
  #     "{theme}{getdens1}
  #     ggplot({deparse(substitute(data))},
  #     \x20\x20aes(x = seq_along({as.character(substitute(vars))}), y = {as.character(substitute(vars))})) +
  #     \x20\x20geom_point(aes(color=add_density_1D({deparse(substitute(data))}, '{as.character(substitute(vars))}'))) +
  #     \x20\x20geom_smooth(method = 'loess', size=0.5) +
  #     \x20\x20{scale_bw_l} +
  #     \x20\x20labs(x='seq') +
  #     \x20\x20{theme_detail_z}"
  #   )
  # }
  # else if (length(vars) == 1 &
  #          diagram == "color point graph" &
  #          is.numeric(unlist(data[, vars])) == TRUE) {
  #   vars <- vars[1]
  #   p <- glue::glue(
  #     "{theme}{getdens1}
  #     ggplot({deparse(substitute(data))},
  #     \x20\x20aes(x = seq_along({as.character(substitute(vars))}), y = {as.character(substitute(vars))})) +
  #     \x20\x20geom_point(aes(color=add_density_1D({deparse(substitute(data))}, '{as.character(substitute(vars))}'))) +
  #     \x20\x20{scale_color_l} +
  #     \x20\x20labs(x='seq') +
  #     \x20\x20{theme_detail_z}"
  #   )
  # }
  # else if (length(vars) == 1 &
  #          diagram == "color point graph with trend line" &
  #          is.numeric(unlist(data[, vars])) == TRUE) {
  #   vars <- vars[1]
  #   p <- glue::glue(
  #     "{theme}{getdens1}
  #     ggplot({deparse(substitute(data))},
  #     \x20\x20aes(x = seq_along({as.character(substitute(vars))}), y = {as.character(substitute(vars))})) +
  #     \x20\x20geom_point(aes(color=add_density_1D({deparse(substitute(data))}, '{as.character(substitute(vars))}'))) +
  #     \x20\x20geom_smooth(method = 'loess', size=0.5) +
  #     \x20\x20{scale_color_l} +
  #     \x20\x20labs(x='seq') +
  #     \x20\x20{theme_detail_z}"
  #   )
  # }
  else if (length(vars) == 1 &
           diagram == "bw point graph" &
           is.numeric(unlist(data[, vars])) == TRUE) {
    vars <- vars[1]
    p <- glue::glue(
      "{theme}
      ggplot({deparse(substitute(data))},
      \x20\x20aes(x = seq_along({as.character(substitute(vars))}), y = {as.character(substitute(vars))})) +
      \x20\x20geom_point(aes(color={as.character(substitute(vars))})) +
      \x20\x20{scale_bw_l} +
      \x20\x20labs(x='seq') +
      \x20\x20{theme_detail_z}"
    )
  }
  else if (length(vars) == 1 &
           diagram == "bw point graph with trend line" &
           is.numeric(unlist(data[, vars])) == TRUE) {
    vars <- vars[1]
    p <- glue::glue(
      "{theme}
      ggplot({deparse(substitute(data))},
      \x20\x20aes(x = seq_along({as.character(substitute(vars))}), y = {as.character(substitute(vars))})) +
      \x20\x20geom_point(aes(color={as.character(substitute(vars))})) +
      \x20\x20geom_smooth(method = 'loess', size=0.5) +
      \x20\x20{scale_bw_l} +
      \x20\x20labs(x='seq') +
      \x20\x20{theme_detail_z}"
    )
  }
  else if (length(vars) == 1 &
           diagram == "color point graph" &
           is.numeric(unlist(data[, vars])) == TRUE) {
    vars <- vars[1]
    p <- glue::glue(
      "{theme}
      ggplot({deparse(substitute(data))},
      \x20\x20aes(x = seq_along({as.character(substitute(vars))}), y = {as.character(substitute(vars))})) +
      \x20\x20geom_point(aes(color={as.character(substitute(vars))})) +
      \x20\x20{scale_color_l} +
      \x20\x20labs(x='seq') +
      \x20\x20{theme_detail_z}"
    )
  }
  else if (length(vars) == 1 &
           diagram == "color point graph with trend line" &
           is.numeric(unlist(data[, vars])) == TRUE) {
    vars <- vars[1]
    p <- glue::glue(
      "{theme}
      ggplot({deparse(substitute(data))},
      \x20\x20aes(x = seq_along({as.character(substitute(vars))}), y = {as.character(substitute(vars))})) +
      \x20\x20geom_point(aes(color={as.character(substitute(vars))})) +
      \x20\x20geom_smooth(method = 'loess', size=0.5) +
      \x20\x20{scale_color_l} +
      \x20\x20labs(x='seq') +
      \x20\x20{theme_detail_z}"
    )
  }
  else if (length(vars) == 1 &
           diagram == "binned point graph" &
           (is.factor(unlist(data[, vars])) == TRUE |
            is.numeric(unlist(data[, vars])) == TRUE)) {
    vars <- vars[1]
    p <- glue::glue(
      "{theme}
      ggplot({deparse(substitute(data))},
      \x20\x20aes(x = seq_along({as.character(substitute(vars))}), y = {as.character(substitute(vars))})) +
      \x20\x20geom_point(color='black', stat= 'bin2d') +
      \x20\x20labs(x='seq') +
      \x20\x20{theme_detail_z}"
    )
  }
  else if (length(vars) == 1 &
           diagram == "bw binned point graph" &
           (is.factor(unlist(data[, vars])) == TRUE |
            is.numeric(unlist(data[, vars])) == TRUE)) {
    vars <- vars[1]
    p <- glue::glue(
      "{theme}
      ggplot({deparse(substitute(data))},
      \x20\x20aes(x = seq_along({as.character(substitute(vars))}), y = {as.character(substitute(vars))})) +
      \x20\x20geom_point(aes(color=..count..), stat= 'bin2d') +
      \x20\x20{scale_bw_l} +
      \x20\x20labs(x='seq') +
      \x20\x20{theme_detail_z}"
    )
  }
  else if (length(vars) == 1 &
           diagram == "color binned point graph" &
           (is.factor(unlist(data[, vars])) == TRUE |
            is.numeric(unlist(data[, vars])) == TRUE)) {
    vars <- vars[1]
    p <- glue::glue(
      "{theme}
      ggplot({deparse(substitute(data))},
      \x20\x20aes(x = seq_along({as.character(substitute(vars))}), y = {as.character(substitute(vars))})) +
      \x20\x20geom_point(aes(color=..count..), stat= 'bin2d') +
      \x20\x20{scale_color_l} +
      \x20\x20labs(x='seq') +
      \x20\x20{theme_detail_z}"
    )
  }
  else if (length(vars) == 1 &
           diagram == "box plot" &
           is.numeric(unlist(data[, vars])) == TRUE) {
    vars <- vars[1]
    p <- glue::glue(
      "{theme}
      ggplot({deparse(substitute(data))},
      \x20\x20aes(x = 0, y = {as.character(substitute(vars))})) +
      \x20\x20geom_boxplot() +
      \x20\x20coord_flip() +
      \x20\x20{theme_detail_y}"
    )
  }
  else if (length(vars) == 1 &
           diagram == "ecdf plot" &
           is.numeric(unlist(data[, vars])) == TRUE) {
    vars <- vars[1]
    p <- glue::glue(
      "{theme}
      ggplot({deparse(substitute(data))}, aes(x = {as.character(substitute(vars))})) +
      \x20\x20stat_ecdf(geom = 'line', size=0.1) +
      \x20\x20labs(x = '{as.character(substitute(vars))}', y = 'p') +
      \x20\x20{theme_detail}"
    )
  }
  else if (length(vars) == 1 &
           diagram == "point ecdf plot" &
           is.numeric(unlist(data[, vars])) == TRUE) {
    vars <- vars[1]
    p <- glue::glue(
      "{theme}
      ggplot({deparse(substitute(data))}, aes(x = {as.character(substitute(vars))})) +
      \x20\x20stat_ecdf(geom = 'point', size=0.1) +
      \x20\x20labs(x = '{as.character(substitute(vars))}', y = 'p') +
      \x20\x20{theme_detail}"
    )
  }
  else if (length(vars) == 1 &
           diagram == "stepped ecdf plot" &
           is.numeric(unlist(data[, vars])) == TRUE) {
    vars <- vars[1]
    p <- glue::glue(
      "{theme}
      ggplot({deparse(substitute(data))}, aes(x = {as.character(substitute(vars))})) +
      \x20\x20stat_ecdf(geom = 'step', size=0.1) +
      \x20\x20labs(x = '{as.character(substitute(vars))}', y = 'p') +
      \x20\x20{theme_detail}"
    )
  }
  else if (length(vars) == 1 &
           diagram == "stripe graph" &
           is.numeric(unlist(data[, vars])) == TRUE) {
    vars <- vars[1]
    p <- glue::glue(
      "{theme}
      ggplot({deparse(substitute(data))}, aes(x={as.character(substitute(vars))})) +
      \x20\x20geom_linerange(aes(ymin=0, ymax=1)) +
      \x20\x20{theme_detail_y}"
    )
  }
  else if (length(vars) == 1 &
           diagram == "bw stripe graph" &
           is.numeric(unlist(data[, vars])) == TRUE) {
    vars <- vars[1]
    p <- glue::glue(
      "{theme}{getdens1}
      ggplot({deparse(substitute(data))}, aes(x={as.character(substitute(vars))}, color=add_density_1D({deparse(substitute(data))}, '{as.character(substitute(vars))}'))) +
      \x20\x20geom_linerange(aes(ymin=0, ymax=1)) +
      \x20\x20{scale_bw_l} +
      \x20\x20{theme_detail_yz}"
    )
  }
  else if (length(vars) == 1 &
           diagram == "color stripe graph" &
           is.numeric(unlist(data[, vars])) == TRUE) {
    vars <- vars[1]
    p <- glue::glue(
      "{theme}{getdens1}
      ggplot({deparse(substitute(data))}, aes(x={as.character(substitute(vars))}, color=add_density_1D({deparse(substitute(data))}, '{as.character(substitute(vars))}'))) +
      \x20\x20geom_linerange(aes(ymin=0, ymax=1)) +
      \x20\x20{scale_color_l} +
      \x20\x20{theme_detail_yz}"
    )
  }
  else if (length(vars) == 1 &
           diagram == "binned stripe graph" &
           is.numeric(unlist(data[, vars])) == TRUE) {
    vars <- vars[1]
    p <- glue::glue(
      "{theme}
      binwidth <- (max({substitute(data)}['{substitute(vars)}'], na.rm=TRUE)-min({substitute(data)}['{substitute(vars)}'], na.rm=TRUE))/20
      ggplot({deparse(substitute(data))}, aes(x={as.character(substitute(vars))})) +
      \x20\x20geom_bin2d(aes(y=1), binwidth = c(binwidth, 1), fill='black') +
      \x20\x20{theme_detail_y}"
    )
  }
  else if (length(vars) == 1 &
           diagram == "bw binned stripe graph" &
           is.numeric(unlist(data[, vars])) == TRUE) {
    vars <- vars[1]
    p <- glue::glue(
      "{theme}
      binwidth <- (max({substitute(data)}['{substitute(vars)}'], na.rm=TRUE)-min({substitute(data)}['{substitute(vars)}'], na.rm=TRUE))/20
      ggplot({deparse(substitute(data))}, aes(x={as.character(substitute(vars))})) +
      \x20\x20geom_bin2d(aes(y=1), binwidth = c(binwidth, 1)) +
      \x20\x20{scale_bw_a} +
      \x20\x20{theme_detail_yz}"
    )
  }
  else if (length(vars) == 1 &
           diagram == "color binned stripe graph" &
           is.numeric(unlist(data[, vars])) == TRUE) {
    vars <- vars[1]
    p <- glue::glue(
      "{theme}
      binwidth <- (max({substitute(data)}['{substitute(vars)}'], na.rm=TRUE)-min({substitute(data)}['{substitute(vars)}'], na.rm=TRUE))/20
      ggplot({deparse(substitute(data))}, aes(x={as.character(substitute(vars))})) +
      \x20\x20geom_bin2d(aes(y=1), binwidth = c(binwidth, 1)) +
      \x20\x20{scale_color_a} +
      \x20\x20{theme_detail_yz}"
    )
  }
  else if (length(vars) == 1 &
           diagram == "seq. stripe graph" &
           is.numeric(unlist(data[, vars])) == TRUE) {
    vars <- vars[1]
    p <- glue::glue(
      "{theme}
      ggplot({deparse(substitute(data))}, aes(x=seq_along({as.character(substitute(vars))}))) +
      \x20\x20geom_tile(aes(y=1), fill = 'black') +
      \x20\x20{theme_detail_y}"
    )
  }
  else if (length(vars) == 1 &
           diagram == "bw seq. stripe graph" &
           is.numeric(unlist(data[, vars])) == TRUE) {
    vars <- vars[1]
    p <- glue::glue(
      "{theme}{getdens1}
      ggplot({deparse(substitute(data))}, aes(x=seq_along({as.character(substitute(vars))}))) +
      \x20\x20geom_tile(aes(y=1, fill={as.character(substitute(vars))})) +
      \x20\x20{scale_bw_a} +
      \x20\x20{theme_detail_yz}"
    )
  }
  else if (length(vars) == 1 &
           diagram == "color seq. stripe graph" &
           is.numeric(unlist(data[, vars])) == TRUE) {
    vars <- vars[1]
    p <- glue::glue(
      "{theme}{getdens1}
      ggplot({deparse(substitute(data))}, aes(x=seq_along({as.character(substitute(vars))}))) +
      \x20\x20geom_tile(aes(y=1, fill={as.character(substitute(vars))})) +
      \x20\x20{scale_color_a} +
      \x20\x20{theme_detail_yz}"
    )
  }




  else if (length(vars) == 2 &
           diagram == "scatter plot" &
           is.numeric(unlist(data[, vars[1]])) == TRUE &
           is.numeric(unlist(data[, vars[2]])) == TRUE) {
    vars1 <- vars[1]
    vars2 <- vars[2]
    p <- glue::glue(
      "{theme}
      ggplot({deparse(substitute(data))}, aes(x={as.character(substitute(vars1))}, y={as.character(substitute(vars2))})) +
      \x20\x20geom_point() +
      \x20\x20{theme_detail}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "bw scatter plot" &
           is.numeric(unlist(data[, vars[1]])) == TRUE &
           is.numeric(unlist(data[, vars[2]])) == TRUE) {
    vars1 <- vars[1]
    vars2 <- vars[2]
    p <- glue::glue(
      "{theme}{getdens2}
      ggplot({deparse(substitute(data))}, aes(x={as.character(substitute(vars1))}, y={as.character(substitute(vars2))}, color=get_density({deparse(substitute(data))}${as.character(substitute(vars1))}, {deparse(substitute(data))}${as.character(substitute(vars2))}))) +
      \x20\x20geom_point() +
      \x20\x20{scale_bw_l} +
      \x20\x20{theme_detail_z}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "color scatter plot" &
           is.numeric(unlist(data[, vars[1]])) == TRUE &
           is.numeric(unlist(data[, vars[2]])) == TRUE) {
    vars1 <- vars[1]
    vars2 <- vars[2]
    p <- glue::glue(
      "{theme}{getdens2}
      ggplot({deparse(substitute(data))}, aes(x={as.character(substitute(vars1))}, y={as.character(substitute(vars2))}, color=get_density({deparse(substitute(data))}${as.character(substitute(vars1))}, {deparse(substitute(data))}${as.character(substitute(vars2))}))) +
      \x20\x20geom_point() +
      \x20\x20{scale_color_l} +
      \x20\x20{theme_detail_z}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "binned scatter plot" &
           is.numeric(unlist(data[, vars[1]])) == TRUE &
           is.numeric(unlist(data[, vars[2]])) == TRUE) {
    vars1 <- vars[1]
    vars2 <- vars[2]
    p <- glue::glue(
      "{theme}
      ggplot({deparse(substitute(data))}, aes(x={as.character(substitute(vars1))}, y={as.character(substitute(vars2))})) +
      \x20\x20geom_point(stat= 'bin2d') +
      \x20\x20{theme_detail_z}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "bw binned scatter plot" &
           is.numeric(unlist(data[, vars[1]])) == TRUE &
           is.numeric(unlist(data[, vars[2]])) == TRUE) {
    vars1 <- vars[1]
    vars2 <- vars[2]
    p <- glue::glue(
      "{theme}
      ggplot({deparse(substitute(data))}, aes(x={as.character(substitute(vars1))}, y={as.character(substitute(vars2))}, color=..count..)) +
      \x20\x20geom_point(stat= 'bin2d') +
      \x20\x20{scale_bw_l} +
      \x20\x20{theme_detail_z}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "color binned scatter plot" &
           is.numeric(unlist(data[, vars[1]])) == TRUE &
           is.numeric(unlist(data[, vars[2]])) == TRUE) {
    vars1 <- vars[1]
    vars2 <- vars[2]
    p <- glue::glue(
      "{theme}
      ggplot({deparse(substitute(data))}, aes(x={as.character(substitute(vars1))}, y={as.character(substitute(vars2))}, color=..count..)) +
      \x20\x20geom_point(stat= 'bin2d') +
      \x20\x20{scale_color_l} +
      \x20\x20{theme_detail_z}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "binned heatmap" &
           is.numeric(unlist(data[, vars[1]])) == TRUE &
           is.numeric(unlist(data[, vars[2]])) == TRUE) {
    vars1 <- vars[1]
    vars2 <- vars[2]
    p <- glue::glue(
      "{theme}
      ggplot({deparse(substitute(data))}, aes(x={as.character(substitute(vars1))}, y={as.character(substitute(vars2))})) +
      \x20\x20geom_bin2d(fill = 'black') +
      \x20\x20{theme_detail}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "bw binned heatmap" &
           is.numeric(unlist(data[, vars[1]])) == TRUE &
           is.numeric(unlist(data[, vars[2]])) == TRUE) {
    vars1 <- vars[1]
    vars2 <- vars[2]
    p <- glue::glue(
      "{theme}
      ggplot({deparse(substitute(data))}, aes(x={as.character(substitute(vars1))}, y={as.character(substitute(vars2))})) +
      \x20\x20geom_bin2d() +
      \x20\x20{scale_bw_a} +
      \x20\x20{theme_detail_z}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "color binned heatmap" &
           is.numeric(unlist(data[, vars[1]])) == TRUE &
           is.numeric(unlist(data[, vars[2]])) == TRUE) {
    vars1 <- vars[1]
    vars2 <- vars[2]
    p <- glue::glue(
      "{theme}
      ggplot({deparse(substitute(data))}, aes(x={as.character(substitute(vars1))}, y={as.character(substitute(vars2))})) +
      \x20\x20geom_bin2d() +
      \x20\x20{scale_color_a} +
      \x20\x20{theme_detail_z}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "hexagonal binned heatmap" &
           is.numeric(unlist(data[, vars[1]])) == TRUE &
           is.numeric(unlist(data[, vars[2]])) == TRUE) {
    vars1 <- vars[1]
    vars2 <- vars[2]
    p <- glue::glue(
      "{theme}
      ggplot({deparse(substitute(data))}, aes(x={as.character(substitute(vars1))}, y={as.character(substitute(vars2))})) +
      \x20\x20stat_binhex(fill = 'black') +
      \x20\x20{theme_detail}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "bw hexagonal binned heatmap" &
           is.numeric(unlist(data[, vars[1]])) == TRUE &
           is.numeric(unlist(data[, vars[2]])) == TRUE) {
    vars1 <- vars[1]
    vars2 <- vars[2]
    p <- glue::glue(
      "{theme}
      ggplot({deparse(substitute(data))}, aes(x={as.character(substitute(vars1))}, y={as.character(substitute(vars2))})) +
      \x20\x20stat_binhex(aes(fill=..count..,  color=..count..)) +
      \x20\x20{scale_bw_l} +
      \x20\x20{scale_bw_a} +
      \x20\x20{theme_detail_z}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "color hexagonal binned heatmap" &
           is.numeric(unlist(data[, vars[1]])) == TRUE &
           is.numeric(unlist(data[, vars[2]])) == TRUE) {
    vars1 <- vars[1]
    vars2 <- vars[2]
    p <- glue::glue(
      "{theme}
      ggplot({deparse(substitute(data))}, aes(x={as.character(substitute(vars1))}, y={as.character(substitute(vars2))})) +
      \x20\x20stat_binhex(aes(fill=..count..,  color=..count..)) +
      \x20\x20{scale_color_l} +
      \x20\x20{scale_color_a} +
      \x20\x20{theme_detail_z}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "bw heatmap" &
           is.numeric(unlist(data[, vars[1]])) == TRUE &
           is.numeric(unlist(data[, vars[2]])) == TRUE) {
    vars1 <- vars[1]
    vars2 <- vars[2]
    p <- glue::glue(
      "{theme}
      ggplot({deparse(substitute(data))}, aes(x={as.character(substitute(vars1))}, y={as.character(substitute(vars2))})) +
      \x20\x20stat_density_2d(aes(fill=stat(density)), geom = 'raster', contour = FALSE) +
      \x20\x20{scale_bw_a} +
      \x20\x20{theme_detail_z}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "color heatmap" &
           is.numeric(unlist(data[, vars[1]])) == TRUE &
           is.numeric(unlist(data[, vars[2]])) == TRUE) {
    vars1 <- vars[1]
    vars2 <- vars[2]
    p <- glue::glue(
      "{theme}
      ggplot({deparse(substitute(data))}, aes(x={as.character(substitute(vars1))}, y={as.character(substitute(vars2))})) +
      \x20\x20stat_density_2d(aes(fill=stat(density)), geom = 'raster', contour = FALSE) +
      \x20\x20{scale_color_a} +
      \x20\x20{theme_detail_z}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "contour plot" &
           is.numeric(unlist(data[, vars[1]])) == TRUE &
           is.numeric(unlist(data[, vars[2]])) == TRUE) {
    vars1 <- vars[1]
    vars2 <- vars[2]
    p <- glue::glue(
      "{theme}
      ggplot({deparse(substitute(data))}, aes(x={as.character(substitute(vars1))}, y={as.character(substitute(vars2))})) +
      \x20\x20stat_density_2d(color = 'black', size=0.2) +
      \x20\x20{theme_detail}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "bw contour plot" &
           is.numeric(unlist(data[, vars[1]])) == TRUE &
           is.numeric(unlist(data[, vars[2]])) == TRUE) {
    vars1 <- vars[1]
    vars2 <- vars[2]
    p <- glue::glue(
      "{theme}
      ggplot({deparse(substitute(data))}, aes(x={as.character(substitute(vars1))}, y={as.character(substitute(vars2))})) +
      \x20\x20stat_density_2d(aes(color=..level..), size=0.2) +
      \x20\x20{scale_bw_l} +
      \x20\x20{theme_detail_z}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "color contour plot" &
           is.numeric(unlist(data[, vars[1]])) == TRUE &
           is.numeric(unlist(data[, vars[2]])) == TRUE) {
    vars1 <- vars[1]
    vars2 <- vars[2]
    p <- glue::glue(
      "{theme}
      ggplot({deparse(substitute(data))}, aes(x={as.character(substitute(vars1))}, y={as.character(substitute(vars2))})) +
      \x20\x20stat_density_2d(aes(color=..level..), size=0.2) +
      \x20\x20{scale_color_l} +
      \x20\x20{theme_detail_z}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "contour plot with data points" &
           is.numeric(unlist(data[, vars[1]])) == TRUE &
           is.numeric(unlist(data[, vars[2]])) == TRUE) {
    vars1 <- vars[1]
    vars2 <- vars[2]
    p <- glue::glue(
      "{theme}
      ggplot({deparse(substitute(data))}, aes(x={as.character(substitute(vars1))}, y={as.character(substitute(vars2))})) +
      \x20\x20stat_density_2d(color = 'black', size=0.2) +
      \x20\x20geom_point() +
      \x20\x20{theme_detail}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "bw contour plot with data points" &
           is.numeric(unlist(data[, vars[1]])) == TRUE &
           is.numeric(unlist(data[, vars[2]])) == TRUE) {
    vars1 <- vars[1]
    vars2 <- vars[2]
    p <- glue::glue(
      "{theme}
      ggplot({deparse(substitute(data))}, aes(x={as.character(substitute(vars1))}, y={as.character(substitute(vars2))})) +
      \x20\x20stat_density_2d(aes(color=..level..), size=0.2) +
      \x20\x20geom_point(aes(color=get_density({deparse(substitute(data))}${as.character(substitute(vars1))}, {deparse(substitute(data))}${as.character(substitute(vars2))}))) +
      \x20\x20{scale_bw_l} +
      \x20\x20{theme_detail_z}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "color contour plot with data points" &
           is.numeric(unlist(data[, vars[1]])) == TRUE &
           is.numeric(unlist(data[, vars[2]])) == TRUE) {
    vars1 <- vars[1]
    vars2 <- vars[2]
    p <- glue::glue(
      "{theme}
      ggplot({deparse(substitute(data))}, aes(x={as.character(substitute(vars1))}, y={as.character(substitute(vars2))})) +
      \x20\x20stat_density_2d(aes(color=..level..), size=0.2) +
      \x20\x20geom_point(aes(color=get_density({deparse(substitute(data))}${as.character(substitute(vars1))}, {deparse(substitute(data))}${as.character(substitute(vars2))}))) +
      \x20\x20{scale_color_l} +
      \x20\x20{theme_detail_z}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "parallel plot" &
           is.numeric(unlist(data[, vars[1]])) == TRUE &
           is.numeric(unlist(data[, vars[2]])) == TRUE) {
    vars1 <- vars[1]
    vars2 <- vars[2]
    p <- glue::glue(
      "{theme}
      GGally::ggparcoord({deparse(substitute(data))}, columns = 1:2, scale = 'uniminmax', alphaLines = 0.2, order = 'skewness') +
      \x20\x20scale_x_discrete(expand = c(.1, 0)) +
      \x20\x20{theme_detail}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "bw parallel plot" &
           is.numeric(unlist(data[, vars[1]])) == TRUE &
           is.numeric(unlist(data[, vars[2]])) == TRUE) {
    vars1 <- vars[1]
    vars2 <- vars[2]
    p <- glue::glue(
      "{theme}{getdens2}
      GGally::ggparcoord(data.frame('{as.character(substitute(vars1))}' = {deparse(substitute(data))}${as.character(substitute(vars1))},
      \x20\x20'{as.character(substitute(vars2))}' = {deparse(substitute(data))}${as.character(substitute(vars2))},
      \x20\x20'dens' = get_density({deparse(substitute(data))}${as.character(substitute(vars1))},
      \x20\x20{deparse(substitute(data))}${as.character(substitute(vars2))})), columns = 1:2,
      \x20\x20scale = 'uniminmax', alphaLines = 0.5, mapping=aes(color=dens), order = 'skewness') +
      \x20\x20{scale_bw_l} +
      \x20\x20scale_x_discrete(expand = c(.1, 0)) +
      \x20\x20{theme_detail_z}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "color parallel plot" &
           is.numeric(unlist(data[, vars[1]])) == TRUE &
           is.numeric(unlist(data[, vars[2]])) == TRUE) {
    vars1 <- vars[1]
    vars2 <- vars[2]
    p <- glue::glue(
      "{theme}{getdens2}
      GGally::ggparcoord(data.frame('{as.character(substitute(vars1))}' = {deparse(substitute(data))}${as.character(substitute(vars1))},
      \x20\x20'{as.character(substitute(vars2))}' = {deparse(substitute(data))}${as.character(substitute(vars2))},
      \x20\x20'dens' = get_density({deparse(substitute(data))}${as.character(substitute(vars1))},
      \x20\x20{deparse(substitute(data))}${as.character(substitute(vars2))})), columns = 1:2,
      \x20\x20scale = 'uniminmax', alphaLines = 0.5, mapping=aes(color=dens), order = 'skewness') +
      \x20\x20{scale_color_l} +
      \x20\x20scale_x_discrete(expand = c(.1, 0)) +
      \x20\x20{theme_detail_z}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "unscaled parallel plot" &
           is.numeric(unlist(data[, vars[1]])) == TRUE &
           is.numeric(unlist(data[, vars[2]])) == TRUE) {
    vars1 <- vars[1]
    vars2 <- vars[2]
    p <- glue::glue(
      "{theme}
      GGally::ggparcoord({deparse(substitute(data))}, columns = 1:2, scale = 'globalminmax', alphaLines = 0.2, order = 'skewness') +
      \x20\x20scale_x_discrete(expand = c(.1, 0)) +
      \x20\x20{theme_detail}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "unscaled bw parallel plot" &
           is.numeric(unlist(data[, vars[1]])) == TRUE &
           is.numeric(unlist(data[, vars[2]])) == TRUE) {
    vars1 <- vars[1]
    vars2 <- vars[2]
    p <- glue::glue(
      "{theme}{getdens2}
      GGally::ggparcoord(data.frame('{as.character(substitute(vars1))}' = {deparse(substitute(data))}${as.character(substitute(vars1))},
      \x20\x20'{as.character(substitute(vars2))}' = {deparse(substitute(data))}${as.character(substitute(vars2))},
      \x20\x20'dens' = get_density({deparse(substitute(data))}${as.character(substitute(vars1))},
      \x20\x20{deparse(substitute(data))}${as.character(substitute(vars2))})), columns = 1:2,
      \x20\x20scale = 'globalminmax', alphaLines = 0.5, mapping=aes(color=dens), order = 'skewness') +
      \x20\x20{scale_bw_l} +
      \x20\x20scale_x_discrete(expand = c(.1, 0)) +
      \x20\x20{theme_detail_z}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "unscaled color parallel plot" &
           is.numeric(unlist(data[, vars[1]])) == TRUE &
           is.numeric(unlist(data[, vars[2]])) == TRUE) {
    vars1 <- vars[1]
    vars2 <- vars[2]
    p <- glue::glue(
      "{theme}{getdens2}
      GGally::ggparcoord(data.frame('{as.character(substitute(vars1))}' = {deparse(substitute(data))}${as.character(substitute(vars1))},
      \x20\x20'{as.character(substitute(vars2))}' = {deparse(substitute(data))}${as.character(substitute(vars2))},
      \x20\x20'dens' = get_density({deparse(substitute(data))}${as.character(substitute(vars1))},
      \x20\x20{deparse(substitute(data))}${as.character(substitute(vars2))})), columns = 1:2,
      \x20\x20scale = 'globalminmax', alphaLines = 0.5, mapping=aes(color=dens), order = 'skewness') +
      \x20\x20{scale_color_l} +
      \x20\x20scale_x_discrete(expand = c(.1, 0)) +
      \x20\x20{theme_detail_z}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "path graph" &
           is.numeric(unlist(data[, vars[1]])) == TRUE &
           is.numeric(unlist(data[, vars[2]])) == TRUE) {
    vars1 <- vars[1]
    vars2 <- vars[2]
    p <- glue::glue(
      "{theme}
      ggplot({deparse(substitute(data))}, aes(x={as.character(substitute(vars1))}, y={as.character(substitute(vars2))})) +
      \x20\x20geom_path() +
      \x20\x20{theme_detail}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "bw path graph" &
           is.numeric(unlist(data[, vars[1]])) == TRUE &
           is.numeric(unlist(data[, vars[2]])) == TRUE) {
    vars1 <- vars[1]
    vars2 <- vars[2]
    p <- glue::glue(
      "{theme}
      ggplot({deparse(substitute(data))}, aes(x={as.character(substitute(vars1))}, y={as.character(substitute(vars2))}, color=seq_along({as.character(substitute(vars1))}))) +
      \x20\x20geom_path() +
      \x20\x20{scale_bw_l} +
      \x20\x20{theme_detail_z}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "color path graph" &
           is.numeric(unlist(data[, vars[1]])) == TRUE &
           is.numeric(unlist(data[, vars[2]])) == TRUE) {
    vars1 <- vars[1]
    vars2 <- vars[2]
    p <- glue::glue(
      "{theme}
      ggplot({deparse(substitute(data))}, aes(x={as.character(substitute(vars1))}, y={as.character(substitute(vars2))}, color=seq_along({as.character(substitute(vars1))}))) +
      \x20\x20geom_path() +
      \x20\x20{scale_color_l} +
      \x20\x20{theme_detail_z}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "point-to-point graph" &
           is.numeric(unlist(data[, vars[1]])) == TRUE &
           is.numeric(unlist(data[, vars[2]])) == TRUE) {
    vars1 <- vars[1]
    vars2 <- vars[2]
    p <- glue::glue(
      "{theme}
      ggplot({deparse(substitute(data))}, aes(x={as.character(substitute(vars1))}, y={as.character(substitute(vars2))})) +
      \x20\x20geom_path() +
      \x20\x20geom_point() +
      \x20\x20{theme_detail}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "bw point-to-point graph" &
           is.numeric(unlist(data[, vars[1]])) == TRUE &
           is.numeric(unlist(data[, vars[2]])) == TRUE) {
    vars1 <- vars[1]
    vars2 <- vars[2]
    p <- glue::glue(
      "{theme}
      ggplot({deparse(substitute(data))}, aes(x={as.character(substitute(vars1))}, y={as.character(substitute(vars2))}, color=seq_along({as.character(substitute(vars1))}))) +
      \x20\x20geom_path() +
      \x20\x20geom_point() +
      \x20\x20{scale_bw_l} +
      \x20\x20{theme_detail_z}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "color point-to-point graph" &
           is.numeric(unlist(data[, vars[1]])) == TRUE &
           is.numeric(unlist(data[, vars[2]])) == TRUE) {
    vars1 <- vars[1]
    vars2 <- vars[2]
    p <- glue::glue(
      "{theme}
      ggplot({deparse(substitute(data))}, aes(x={as.character(substitute(vars1))}, y={as.character(substitute(vars2))}, color=seq_along({as.character(substitute(vars1))}))) +
      \x20\x20geom_path() +
      \x20\x20geom_point() +
      \x20\x20{scale_color_l} +
      \x20\x20{theme_detail_z}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "point graph" &
           is.numeric(unlist(data[, vars[1]])) == TRUE &
           is.numeric(unlist(data[, vars[2]])) == TRUE) {
    vars1 <- vars[1]
    vars2 <- vars[2]
    unfold2 <- paste0("{theme}", unfold)
    p <- glue::glue(
      paste0(unfold2),
      "ggplot(foo_df, aes_string(x='foo_id')) +
      geom_point(aes_string(y='measure'), size=1) +
      labs(y='', x='seq') +
      facet_grid(variable~., switch = 'both') +
      {theme_detail}
      "
      )
  }
  else if (length(vars) == 2 &
           diagram == "bw point graph" &
           is.numeric(unlist(data[, vars[1]])) == TRUE &
           is.numeric(unlist(data[, vars[2]])) == TRUE) {
    vars1 <- vars[1]
    vars2 <- vars[2]
    unfold2 <- paste0("{theme}", unfold)
    p <- glue::glue(
      paste0(unfold2),
      "ggplot(foo_df, aes_string(x='foo_id')) +
      geom_point(aes_string(y='measure', color = 'measure'), size=1) +
      labs(y='', x='seq') +
      {scale_bw_l} +
      facet_grid(variable~., switch = 'both') +
      {theme_detail_z}
      "
    )
  }
  else if (length(vars) == 2 &
           diagram == "color point graph" &
           is.numeric(unlist(data[, vars[1]])) == TRUE &
           is.numeric(unlist(data[, vars[2]])) == TRUE) {
    vars1 <- vars[1]
    vars2 <- vars[2]
    unfold2 <- paste0("{theme}", unfold)
    p <- glue::glue(
      paste0(unfold2),
      "ggplot(foo_df, aes_string(x = 'foo_id')) +
      geom_point(aes_string(y = 'measure', color = 'measure'), size = 1) +
      labs(y = '', x = 'seq') +
      {scale_color_l} +
      facet_grid(variable ~ ., switch = 'both') +
      {theme_detail_z}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "line graph" &
           is.numeric(unlist(data[, vars[1]])) == TRUE &
           is.numeric(unlist(data[, vars[2]])) == TRUE) {
    vars1 <- vars[1]
    vars2 <- vars[2]
    unfold2 <- paste0("{theme}", unfold)
    p <- glue::glue(
      paste0(unfold2),
      "ggplot(foo_df, aes_string(x = 'foo_id', y = 'measure')) +
      geom_line(color = 'black') +
      labs(y = '', x = 'seq') +
      facet_grid(variable ~ ., switch = 'both') +
      {theme_detail}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "stepped line graph" &
           is.numeric(unlist(data[, vars[1]])) == TRUE &
           is.numeric(unlist(data[, vars[2]])) == TRUE) {
    vars1 <- vars[1]
    vars2 <- vars[2]
    unfold2 <- paste0("{theme}", unfold)
    p <- glue::glue(
      paste0(unfold2),
      "ggplot(foo_df, aes_string(x = 'foo_id', y = 'measure')) +
      geom_step(color = 'black') +
      labs(y = '', x = 'seq') +
      facet_grid(variable ~ ., switch = 'both') +
      {theme_detail}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "area graph" &
           is.numeric(unlist(data[, vars[1]])) == TRUE &
           is.numeric(unlist(data[, vars[2]])) == TRUE) {
    vars1 <- vars[1]
    vars2 <- vars[2]
    unfold2 <- paste0("{theme}", unfold)
    p <- glue::glue(
      paste0(unfold2),
      "ggplot(foo_df, aes_string(x = 'foo_id', y = 'measure')) +
      geom_area(fill = 'black') +
      labs(y = '', x = 'seq') +
      facet_grid(variable ~ ., switch = 'both') +
      {theme_detail}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "stepped area graph" &
           is.numeric(unlist(data[, vars[1]])) == TRUE &
           is.numeric(unlist(data[, vars[2]])) == TRUE) {
    vars1 <- vars[1]
    vars2 <- vars[2]
    unfold2 <- paste0("{theme}", unfold)
    p <- glue::glue(
      paste0(unfold2),
      "ggplot(foo_df, aes_string(x = 'foo_id')) +
      geom_bar(aes_string(y='measure'), fill = 'black', width = 1, stat = 'identity') +
      labs(y = '', x = 'seq') +
      facet_grid(variable ~ ., switch = 'both') +
      {theme_detail}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "bw stepped area graph" &
           is.numeric(unlist(data[, vars[1]])) == TRUE &
           is.numeric(unlist(data[, vars[2]])) == TRUE) {
    vars1 <- vars[1]
    vars2 <- vars[2]
    unfold2 <- paste0("{theme}", unfold)
    p <- glue::glue(
      paste0(unfold2),
      "ggplot(foo_df, aes_string(x = 'foo_id')) +
      geom_bar(aes_string(y='measure', fill = 'measure'), width = 1, stat = 'identity') +
      {scale_bw_a} +
      labs(y = '', x = 'seq') +
      facet_grid(variable ~ ., switch = 'both') +
      {theme_detail_z}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "color stepped area graph" &
           is.numeric(unlist(data[, vars[1]])) == TRUE &
           is.numeric(unlist(data[, vars[2]])) == TRUE) {
    vars1 <- vars[1]
    vars2 <- vars[2]
    unfold2 <- paste0("{theme}", unfold)
    p <- glue::glue(
      paste0(unfold2),
      "ggplot(foo_df, aes_string(x = 'foo_id')) +
      geom_bar(aes_string(y='measure', fill = 'measure'), width = 1, stat = 'identity') +
      {scale_color_a} +
      labs(y = '', x = 'seq') +
      facet_grid(variable ~ ., switch = 'both') +
      {theme_detail_z}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "bw heatmap" &
           is.numeric(unlist(data[, vars[1]])) == TRUE &
           is.numeric(unlist(data[, vars[2]])) == TRUE) {
    vars1 <- vars[1]
    vars2 <- vars[2]
    unfold2 <- paste0("{theme}", unfold)
    p <- glue::glue(
      paste0(unfold2),
      "ggplot(foo_df, aes_string(x = 'foo_id')) +
      stat_density_2d(aes(fill = stat(density)), geom = 'raster', contour = FALSE) +
      {scale_bw_a} +
      labs(y = '', x = 'seq') +
      facet_grid(variable ~ ., switch = 'both') +
      {theme_detail_z}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "color heatmap" &
           is.numeric(unlist(data[, vars[1]])) == TRUE &
           is.numeric(unlist(data[, vars[2]])) == TRUE) {
    vars1 <- vars[1]
    vars2 <- vars[2]
    unfold2 <- paste0("{theme}", unfold)
    p <- glue::glue(
      paste0(unfold2),
      "ggplot(foo_df, aes_string(x = 'foo_id')) +
      stat_density_2d(aes(fill = stat(density)), geom = 'raster', contour = FALSE) +
      {scale_color_a} +
      labs(y = '', x = 'seq') +
      facet_grid(variable ~ ., switch = 'both') +
      {theme_detail_z}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "bw seq. stripe graph" &
           is.numeric(unlist(data[, vars[1]])) == TRUE &
           is.numeric(unlist(data[, vars[2]])) == TRUE) {
    vars1 <- vars[1]
    vars2 <- vars[2]
    unfold2 <- paste0("{theme}", unfold)
    p <- glue::glue(
      paste0(unfold2),
      "ggplot(foo_df, aes_string(x = 'foo_id', y = 1)) +
      geom_tile(aes_string(fill = 'measure')) +
      {scale_bw_a} +
      labs(y = '', x = 'seq') +
      facet_grid(variable ~ ., switch = 'both') +
      {theme_detail_z}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "color seq. stripe graph" &
           is.numeric(unlist(data[, vars[1]])) == TRUE &
           is.numeric(unlist(data[, vars[2]])) == TRUE) {
    vars1 <- vars[1]
    vars2 <- vars[2]
    unfold2 <- paste0("{theme}", unfold)
    p <- glue::glue(
      paste0(unfold2),
      "ggplot(foo_df, aes_string(x = 'foo_id', y = 1)) +
      geom_tile(aes_string(fill = 'measure')) +
      {scale_color_a} +
      labs(y = '', x = 'seq') +
      facet_grid(variable ~ ., switch = 'both') +
      {theme_detail_z}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "histogram" &
           is.numeric(unlist(data[, vars[1]])) == TRUE &
           is.numeric(unlist(data[, vars[2]])) == TRUE) {
    vars1 <- vars[1]
    vars2 <- vars[2]
    unfold2 <- paste0("{theme}", unfold)
    p <- glue::glue(
      paste0(unfold2),
      "ggplot(foo_df, aes_string(x = 'measure')) +
      geom_histogram(fill = 'black') +
      labs(x = '') +
      facet_grid(variable ~ ., switch = 'both') +
      {theme_detail}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "bw histogram" &
           is.numeric(unlist(data[, vars[1]])) == TRUE &
           is.numeric(unlist(data[, vars[2]])) == TRUE) {
    vars1 <- vars[1]
    vars2 <- vars[2]
    unfold2 <- paste0("{theme}", unfold)
    p <- glue::glue(
      paste0(unfold2),
      "ggplot(foo_df, aes_string(x = 'measure')) +
      geom_histogram(aes_(fill=~..count..)) +
      {scale_bw_a} +
      labs(x = '') +
      facet_grid(variable ~ ., switch = 'both') +
      {theme_detail_z}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "color histogram" &
           is.numeric(unlist(data[, vars[1]])) == TRUE &
           is.numeric(unlist(data[, vars[2]])) == TRUE) {
    vars1 <- vars[1]
    vars2 <- vars[2]
    unfold2 <- paste0("{theme}", unfold)
    p <- glue::glue(
      paste0(unfold2),
      "ggplot(foo_df, aes_string(x = 'measure')) +
      geom_histogram(aes_(fill=~..count..)) +
      {scale_color_a} +
      labs(x = '') +
      facet_grid(variable ~ ., switch = 'both') +
      {theme_detail_z}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "freq. polygon" &
           is.numeric(unlist(data[, vars[1]])) == TRUE &
           is.numeric(unlist(data[, vars[2]])) == TRUE) {
    vars1 <- vars[1]
    vars2 <- vars[2]
    unfold2 <- paste0("{theme}", unfold)
    p <- glue::glue(
      paste0(unfold2),
      "ggplot(foo_df, aes_string(x = 'measure')) +
      geom_line(stat = 'bin', bins = 20, center = 0, color = 'black', size = 0.1) +
      labs(x = '') +
      facet_grid(variable ~ ., switch = 'both') +
      {theme_detail}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "density plot" &
           is.numeric(unlist(data[, vars[1]])) == TRUE &
           is.numeric(unlist(data[, vars[2]])) == TRUE) {
    vars1 <- vars[1]
    vars2 <- vars[2]
    unfold2 <- paste0("{theme}", unfold)
    p <- glue::glue(
      paste0(unfold2),
      "ggplot(foo_df, aes_string(x = 'measure')) +
      geom_density(size = 0.1) +
      labs(x = '') +
      facet_grid(variable ~ ., switch = 'both') +
      {theme_detail}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "filled density plot" &
           is.numeric(unlist(data[, vars[1]])) == TRUE &
           is.numeric(unlist(data[, vars[2]])) == TRUE) {
    vars1 <- vars[1]
    vars2 <- vars[2]
    unfold2 <- paste0("{theme}", unfold)
    p <- glue::glue(
      paste0(unfold2),
      "ggplot(foo_df, aes_string(x = 'measure')) +
      geom_density(fill = 'black', size = 0.1) +
      labs(x = '') +
      facet_grid(variable ~ ., switch = 'both') +
      {theme_detail}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "violin plot" &
           is.numeric(unlist(data[, vars[1]])) == TRUE &
           is.numeric(unlist(data[, vars[2]])) == TRUE) {
    vars1 <- vars[1]
    vars2 <- vars[2]
    unfold2 <- paste0("{theme}", unfold)
    p <- glue::glue(
      paste0(unfold2),
      "ggplot(foo_df, aes_string(x = 0, y = 'measure')) +
      geom_violin(size = 0.1) +
      labs(x = '') +
      coord_flip() +
      facet_grid(variable ~ ., switch = 'both') +
      {theme_detail}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "filled violin plot" &
           is.numeric(unlist(data[, vars[1]])) == TRUE &
           is.numeric(unlist(data[, vars[2]])) == TRUE) {
    vars1 <- vars[1]
    vars2 <- vars[2]
    unfold2 <- paste0("{theme}", unfold)
    p <- glue::glue(
      paste0(unfold2),
      "ggplot(foo_df, aes_string(x = 0, y = 'measure')) +
      geom_violin(fill = 'black', size = 0.1) +
      labs(x = '') +
      coord_flip() +
      facet_grid(variable ~ ., switch = 'both') +
      {theme_detail}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "box plot" &
           is.numeric(unlist(data[, vars[1]])) == TRUE &
           is.numeric(unlist(data[, vars[2]])) == TRUE) {
    vars1 <- vars[1]
    vars2 <- vars[2]
    unfold2 <- paste0("{theme}", unfold)
    p <- glue::glue(
      paste0(unfold2),
      "ggplot(foo_df) +
      geom_boxplot(aes_string(x=0, y='measure'), size = 0.1) +
      labs(x = '') +
      coord_flip() +
      facet_grid(variable ~ ., switch = 'both') +
      {theme_detail_y}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "filled violin plot" &
           is.numeric(unlist(data[, vars[1]])) == TRUE &
           is.numeric(unlist(data[, vars[2]])) == TRUE) {
    vars1 <- vars[1]
    vars2 <- vars[2]
    unfold2 <- paste0("{theme}", unfold)
    p <- glue::glue(
      paste0(unfold2),
      "ggplot(foo_df, aes_string(x = 'measure')) +
      geom_violin(fill = 'black', size = 0.1) +
      labs(x = '') +
      facet_grid(variable ~ ., switch = 'both') +
      {theme_detail_y}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "ecdf plot" &
           is.numeric(unlist(data[, vars[1]])) == TRUE &
           is.numeric(unlist(data[, vars[2]])) == TRUE) {
    vars1 <- vars[1]
    vars2 <- vars[2]
    unfold2 <- paste0("{theme}", unfold)
    p <- glue::glue(
      paste0(unfold2),
      "ggplot(foo_df, aes_string(x = 'measure')) +
      stat_ecdf(geom = 'point', size=0.1) +
      labs(x = '', y = 'p') +
      facet_grid(variable ~ ., switch = 'both') +
      {theme_detail}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "point ecdf plot" &
           is.numeric(unlist(data[, vars[1]])) == TRUE &
           is.numeric(unlist(data[, vars[2]])) == TRUE) {
    vars1 <- vars[1]
    vars2 <- vars[2]
    unfold2 <- paste0("{theme}", unfold)
    p <- glue::glue(
      paste0(unfold2),
      "ggplot(foo_df, aes_string(x = 'measure')) +
      stat_ecdf(geom = 'point', size=1) +
      labs(x = '', y = 'p') +
      facet_grid(variable ~ ., switch = 'both') +
      {theme_detail}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "stepped ecdf plot" &
           is.numeric(unlist(data[, vars[1]])) == TRUE &
           is.numeric(unlist(data[, vars[2]])) == TRUE) {
    vars1 <- vars[1]
    vars2 <- vars[2]
    unfold2 <- paste0("{theme}", unfold)
    p <- glue::glue(
      paste0(unfold2),
      "ggplot(foo_df, aes_string(x = 'measure')) +
      stat_ecdf(geom = 'step', size=0.1) +
      labs(x = '', y = 'p') +
      facet_grid(variable ~ ., switch = 'both') +
      {theme_detail}"
    )
  }
  else {stop("The combination of these data and graphic type has not been still considered.")}
    if (output == 'console') {
      cat(p)}
    else if (output == 'plots pane') {
      eval(parse(text=p))}
    else if (output == 'html') {
      dir.create(file.path(dir, "brinton_outcomes", fsep = .Platform$file.sep), showWarnings = FALSE)
      writeLines(output_up, file.path(dir, "brinton_outcomes", "plotup.R"))
      write(paste0("cat('A ", deparse(substitute(diagram)), " produced from the " ,deparse(substitute(vars)), " variable(s) of the ", deparse(substitute(data))," dataframe')"),
            file=file.path(dir, "brinton_outcomes", "plotup.R"), append=TRUE)
      write(paste0("#+ plot, fig.width=6, fig.height=", long), file.path(dir, "brinton_outcomes", "plotup.R"), append = TRUE)
      write(p, file.path(dir, "brinton_outcomes", "plotup.R"), append = TRUE)
      rmarkdown::render(file.path(dir, "brinton_outcomes", "plotup.R"),"html_document", envir=my_env)
      pander::openFileInOS(file.path(dir, "brinton_outcomes", "plotup.html"))
      # unlink(file.path(dir, "brinton_outcomes", fsep = .Platform$file.sep), recursive = TRUE)
      }
    }
}

geom_tile(aes_string(x='pp_id', fill = 'measure'))

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
#'
#' @return This function can have three outputs: by default it produces a particular graphic,
#' but it can also be represented into the RStudio's plots pane, or can return the code to produce it.
#' @export
#'
#' @examples
#' plotup(esoph, "ncases", "line graph")
plotup <- function(data,
                   vars,
                   diagram,
                   output = 'html'
                   )
{
  ## Auxiliary constant
  dir <- tempdir()
  my_env <- new.env()
  # dirtemp <- getwd()
  # file.path(dir, "output.R") <- paste0(dirtemp, "\\plotup.R")
  # plotupHTML <- paste0(dirtemp, "\\plotup.html")
  ## Value validation: function's argument
  ### dataset
  ### variable
  if (
    is.logical(unlist(data[, vars])) == TRUE |
    is.factor(unlist(data[, vars])) == TRUE |
    is.ordered(unlist(data[, vars])) == TRUE |
    is.character(unlist(data[, vars])) == TRUE
  ) {long <- length(unique(unlist(data[, vars])))/6 + 0.5}
  else if (
    is.numeric(unlist(data[, vars])) == TRUE |
    lubridate::is.instant(unlist(data[, vars])) == TRUE
  ) {long <- 4}
  else {stop("This type of variable has not been yet considered")}

  ### diagram
  string      <- " argument expects a character string"
  if(lubridate::is.instant(unlist(data[, vars])) == TRUE & (diagram %in% datetime_v) == FALSE)
  {stop(paste0("The 'diagram'", string, " which values can be : '",
               paste0(datetime_v, collapse = "', '"), "'"))
  }
  if(is.logical(unlist(data[, vars])) == TRUE & (diagram %in% logical_v) == FALSE)
  {stop(paste0("The 'logical'", string, " which values can be : '",
               paste0(logical_v, collapse = "', '"), "'"))
  }
  if(is.ordered(unlist(data[, vars])) == TRUE & (diagram %in% ordered_v) == FALSE)
  {stop(paste0("The 'ordered'", string, " which values can be : '",
               paste0(ordered_v, collapse = "', '"), "'"))
  }
  if(is.factor(unlist(data[, vars])) == TRUE & is.ordered(unlist(data[, vars])) == FALSE & (diagram %in% factor_v) == FALSE)
  {stop(paste0("The 'factor'", string, " which values can be : '",
               paste0(factor_v, collapse = "', '"), "'"))
  }
  if(is.numeric(unlist(data[, vars])) == TRUE & (diagram %in% numeric_v == FALSE))
  {stop(paste0("The 'numeric'", string, " which values can be : '",
               paste0(numeric_v, collapse = "', '"), "'"))
  }
  if(is.character(unlist(data[, vars])) == TRUE & (diagram %in% character_v) == FALSE)
  {stop(paste0("The 'character'", string, " which values can be : '",
               paste0(character_v, collapse = "', '"), "'"))
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

  theme <- "theme_set(theme_minimal())"
  theme_detail <- "theme(panel.grid = element_line(colour = NA),
    axis.ticks=element_line(color='black'))"
  theme_detail_y <- "theme(panel.grid = element_line(colour = NA),
    axis.text.y=element_text(color=NA),
    axis.title.y=element_text(color=NA),
    axis.ticks.x=element_line(color='black'))"
  theme_detail_z <- "theme(panel.grid = element_line(colour = NA),
    axis.ticks=element_line(color='black'),
    legend.position='none')"
  theme_detail_yz <- "theme(panel.grid = element_line(colour = NA),
    axis.text.y=element_text(color=NA),
    axis.title.y=element_text(color=NA),
    axis.ticks.x=element_line(color='black'),
    legend.position='none')"
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
  if (vars %in% names(data) == FALSE ) {
    stop(paste0("The '", as.character(substitute(data)),
                "' dataset does not include the variable '",
                as.character(substitute(vars)), "'."))
  }
  if (diagram == "blank") {
    p <- paste0(theme, "
ggplot(",
  deparse(substitute(data)),
  ", aes(x=seq_along(",
  as.character(substitute(vars)),
  "), y=",
  as.character(substitute(vars)),
  ")) +
  geom_blank() +
  labs(x='seq') +
  theme(axis.title=element_blank(),
    axis.text=element_blank(),
    axis.ticks=element_blank(),
    panel.grid = element_line(colour = NA))")
  }
  else if (diagram == "freq. reordered line graph") {
    p <- paste0(theme, "\n", reorder_freq, "
ggplot(",
  deparse(substitute(data)),
  ", aes(x=seq_along(",
  as.character(substitute(vars)),
  "), y=",
  as.character(substitute(vars)),
    ")) +
  geom_path(aes(group=1)) +
  labs(x='seq') +
  ", theme_detail)
  }
  else if (diagram == "alphab. reordered line graph") {
    p <- paste0(theme, "\n", reorder_alphab, "
ggplot(",
  deparse(substitute(data)),
  ", aes(x=seq_along(",
  as.character(substitute(vars)),
  "), y=",
  as.character(substitute(vars)),
  ")) +
  geom_path(aes(group=1)) +
  labs(x='seq') +
  ", theme_detail)
  }
  else if (diagram == "point-to-point graph") {
    p <- paste0(theme, "
ggplot(",
  deparse(substitute(data)),
  ", aes(x=seq_along(",
  as.character(substitute(vars)),
  "), y=",
  as.character(substitute(vars)),
  ")) +
  geom_path(aes(group=1)) +
  geom_point() +
  labs(x='seq') +
  ", theme_detail)
  }
  else if (diagram == "freq. reordered point-to-point graph") {
    p <- paste0(theme, "\n", reorder_freq, "
ggplot(",
  deparse(substitute(data)),
  ", aes(x=seq_along(",
  as.character(substitute(vars)),
  "), y=",
  as.character(substitute(vars)),
  ")) +
  geom_path(aes(group=1)) +
  geom_point() +
  labs(x='seq') +
  ", theme_detail)
  }
  else if (diagram == "alphab. reordered point-to-point graph") {
    p <- paste0(theme, "\n", reorder_alphab, "
ggplot(",
  deparse(substitute(data)),
  ", aes(x=seq_along(",
  as.character(substitute(vars)),
  "), y=",
  as.character(substitute(vars)),
  ")) +
  geom_path(aes(group=1)) +
  geom_point() +
  labs(x='seq') +
  ", theme_detail)
  }
  else if (diagram == "stepped point-to-point graph" & (
    is.numeric(unlist(data[, vars])) == TRUE |
    lubridate::is.instant(unlist(data[, vars])) == TRUE
  )) {
    p <- paste0(theme, "
                ggplot(",
  deparse(substitute(data)),
  ", aes(x=seq_along(",
  as.character(substitute(vars)),
  "), y=",
  as.character(substitute(vars)),
  ")) +
  geom_step(aes(group=1)) +
  geom_point() +
  labs(x='seq') +
  ", theme_detail)
  }
  else if (diagram == "point graph") {
    p <- paste0(theme, "
ggplot(",
  deparse(substitute(data)),
  ", aes(x=seq_along(",
  as.character(substitute(vars)),
  "), y=",
  as.character(substitute(vars)),
  ")) +
  geom_point() +
  labs(x='seq') +
  ", theme_detail)
  }
  else if (diagram == "freq. reordered point graph") {
    p <- paste0(theme, "\n", reorder_freq, "
ggplot(",
  deparse(substitute(data)),
  ", aes(x=seq_along(",
  as.character(substitute(vars)),
  "), y=",
  as.character(substitute(vars)),
  ")) +
  geom_point() +
  labs(x='seq') +
  ", theme_detail)
  }
  else if (diagram == "alphab. reordered point graph") {
    p <- paste0(theme, "\n", reorder_alphab, "
ggplot(",
  deparse(substitute(data)),
  ", aes(x=seq_along(",
  as.character(substitute(vars)),
  "), y=",
  as.character(substitute(vars)),
  ")) +
  geom_point() +
  labs(x='seq') +
  ", theme_detail)
  }
  else if (diagram == "point graph with trend line" &
    is.numeric(unlist(data[, vars])) == TRUE) {
    p <- paste0(theme, "
ggplot(",
  deparse(substitute(data)),
  ", aes(x=seq_along(",
  as.character(substitute(vars)),
  "), y=",
  as.character(substitute(vars)),
  ")) +
  geom_point() +
  geom_smooth(method = 'loess', size=0.5) +
  labs(x='seq') +
  ", theme_detail)
  }
  else if (diagram == "line graph" & (
    is.logical(unlist(data[, vars])) == TRUE |
    is.factor(unlist(data[, vars])) == TRUE |
    is.ordered(unlist(data[, vars])) == TRUE |
    is.character(unlist(data[, vars])) == TRUE
  )) {
    reorder_observ
    p <- paste0(theme, "\n", reorder_observ, "
ggplot(",
  deparse(substitute(data)),
  ", aes(x=seq_along(",
  as.character(substitute(vars)),
  "), y=",
  as.character(substitute(vars)),
  ")) +
  geom_path(aes(group=1)) +
  labs(x='seq') +
  ", theme_detail)
  }
  else if (diagram == "line graph" & (
    is.logical(unlist(data[, vars])) == FALSE |
    is.factor(unlist(data[, vars])) == FALSE |
    is.ordered(unlist(data[, vars])) == FALSE |
    is.character(unlist(data[, vars])) == FALSE
  )) {
    reorder_observ
    p <- paste0(theme, "
ggplot(",
  deparse(substitute(data)),
  ", aes(x=seq_along(",
  as.character(substitute(vars)),
  "), y=",
  as.character(substitute(vars)),
  ")) +
  geom_path(aes(group=1)) +
  labs(x='seq') +
  ", theme_detail)
  }
  else if (diagram == "tile plot" & (
    is.logical(unlist(data[, vars])) == TRUE |
    is.factor(unlist(data[, vars])) == TRUE |
    is.ordered(unlist(data[, vars])) == TRUE |
    is.character(unlist(data[, vars])) == TRUE
    )) {
    p <- paste0(theme, "\n", reorder_observ, "
ggplot(",
  deparse(substitute(data)),
  ", aes(x=seq_along(",
  as.character(substitute(vars)),
  "), y=",
  as.character(substitute(vars)),
  ")) +
  geom_tile() +
  labs(x='seq') +
  ", theme_detail)
  }
  else if (diagram == "freq. reordered tile plot" & (
    is.logical(unlist(data[, vars])) == TRUE |
    is.factor(unlist(data[, vars])) == TRUE |
    is.ordered(unlist(data[, vars])) == TRUE |
    is.character(unlist(data[, vars])) == TRUE
  )) {
    p <- paste0(theme, "\n", reorder_freq, "
ggplot(",
  deparse(substitute(data)),
  ", aes(x=seq_along(",
  as.character(substitute(vars)),
  "), y=",
  as.character(substitute(vars)),
  ")) +
  geom_tile() +
  labs(x='seq') +
  ", theme_detail)
  }
  else if (diagram == "alphab. reordered tile plot" & (
    is.logical(unlist(data[, vars])) == TRUE |
    is.factor(unlist(data[, vars])) == TRUE |
    is.ordered(unlist(data[, vars])) == TRUE |
    is.character(unlist(data[, vars])) == TRUE
  )) {
    p <- paste0(theme, "\n", reorder_alphab, "
ggplot(",
  deparse(substitute(data)),
  ", aes(x=seq_along(",
  as.character(substitute(vars)),
  "), y=",
  as.character(substitute(vars)),
  ")) +
  geom_tile() +
  labs(x='seq') +
  ", theme_detail)
  }
  else if (diagram == "bar graph" & (
    is.numeric(unlist(data[, vars])) == TRUE |
    lubridate::is.instant(unlist(data[, vars])) == TRUE
    )) {
    p <- paste0(theme, "
binwidth <- (max(",
substitute(data),
"['",
substitute(vars),
"'], na.rm=TRUE)-min(",
substitute(data),
"['",
substitute(vars),
"'], na.rm=TRUE))/100
ggplot(",
  deparse(substitute(data)),
  ", aes(x=",
  as.character(substitute(vars)),
  ")) +
  geom_bar(stat='count', width=binwidth, fill='black', color='black', position = 'identity') +
  ", theme_detail)
  }
  else if (diagram == "bw bar graph" & (
    is.numeric(unlist(data[, vars])) == TRUE |
    lubridate::is.instant(unlist(data[, vars])) == TRUE
    )) {
    p <- paste0(theme, "
binwidth <- (max(",
substitute(data),
"['",
substitute(vars),
"'], na.rm=TRUE)-min(",
substitute(data),
"['",
substitute(vars),
"'], na.rm=TRUE))/100
ggplot(",
  deparse(substitute(data)),
  ", aes(x=",
  as.character(substitute(vars)),
  ", fill=..count..)) +
  geom_bar(width=binwidth, position = 'identity') +
  scale_fill_gradientn(colours = colorRampPalette(c('#E5E5E5', '#000000'))(2)) +
  ", theme_detail_z)
  }
  else if (diagram == "color bar graph" & (
    is.numeric(unlist(data[, vars])) == TRUE |
    lubridate::is.instant(unlist(data[, vars])) == TRUE
    )) {
    p <- paste0(theme, "
binwidth <- (max(",
substitute(data),
"['",
substitute(vars),
"'], na.rm=TRUE)-min(",
substitute(data),
"['",
substitute(vars),
"'], na.rm=TRUE))/100
ggplot(",
  deparse(substitute(data)),
  ", aes(x=",
  as.character(substitute(vars)),
  ", fill=..count..)) +
  geom_bar(width=binwidth, position = 'identity') +
  scale_fill_gradientn(colours = colorRampPalette(rev(RColorBrewer::brewer.pal(4, 'Spectral')))(3)) +
  ", theme_detail_z)
  }
  else if (diagram == "bar graph" & (
           is.logical(unlist(data[, vars])) == TRUE |
           is.factor(unlist(data[, vars])) == TRUE |
           is.ordered(unlist(data[, vars])) == TRUE |
           is.character(unlist(data[, vars])) == TRUE
           )) {
    p <- paste0(theme, "\n", reorder_observ, "
ggplot(",
  deparse(substitute(data)),
  ", aes(x=",
  as.character(substitute(vars)),
  ")) +
  geom_bar(stat='count', width=0.75, fill='black') +
  coord_flip() +
  ", theme_detail)
  }
  else if (diagram == "bw bar graph" & (
    is.logical(unlist(data[, vars])) == TRUE |
    is.factor(unlist(data[, vars])) == TRUE |
    is.ordered(unlist(data[, vars])) == TRUE |
    is.character(unlist(data[, vars])) == TRUE
  )) {
    p <- paste0(theme, "\n", reorder_observ, "
ggplot(",
  deparse(substitute(data)),
  ", aes(x=",
  as.character(substitute(vars)),
  ", fill=..count..)) +
  geom_bar(stat='count', width=0.75) +
  scale_fill_gradientn(colours = colorRampPalette(c('#E5E5E5', '#000000'))(2)) +
  coord_flip() +
  ", theme_detail_z)
  }
  else if (diagram == "color bar graph" & (
    is.logical(unlist(data[, vars])) == TRUE |
    is.factor(unlist(data[, vars])) == TRUE |
    is.ordered(unlist(data[, vars])) == TRUE |
    is.character(unlist(data[, vars])) == TRUE
  )) {
    p <- paste0(theme, "\n", reorder_observ, "
ggplot(",
  deparse(substitute(data)),
  ", aes(x=",
  as.character(substitute(vars)),
  ", fill=..count..)) +
  geom_bar(stat='count', width=0.75) +
  scale_fill_gradientn(colours = colorRampPalette(rev(RColorBrewer::brewer.pal(4, 'Spectral')))(3)) +
  coord_flip() +
  ", theme_detail_z)
  }
  else if (diagram == "freq. reordered bar graph" & (
    is.factor(unlist(data[, vars])) == TRUE |
    is.character(unlist(data[, vars])) == TRUE
  )) {
    p <- paste0(theme, "\n", reorder_freq, "
ggplot(",
  deparse(substitute(data)),
  ", aes(x=",
  as.character(substitute(vars)),
  ")) +
  geom_bar(aes(",
  as.character(substitute(vars)),
  "),stat='count', width=0.75, fill='black') +
  coord_flip() +
  ", theme_detail)
  }
  else if (diagram == "bw freq. reordered bar graph" & (
    is.factor(unlist(data[, vars])) == TRUE |
    is.character(unlist(data[, vars])) == TRUE
  )) {
    # variable <- forcats::fct_infreq(variable, ordered = TRUE)
    p <- paste0(theme, "\n", reorder_freq, "
ggplot(",
  deparse(substitute(data)),
  ", aes(x=",
  as.character(substitute(vars)),
  ", fill=..count..)) +
  geom_bar(aes(",
  as.character(substitute(vars)),
  "),stat='count', width=0.75) +
  scale_fill_gradientn(colours = colorRampPalette(c('#E5E5E5', '#000000'))(2)) +
  coord_flip() +
  ", theme_detail_z)
  }
  else if (diagram == "color freq. reordered bar graph" & (
    is.factor(unlist(data[, vars])) == TRUE |
    is.character(unlist(data[, vars])) == TRUE
  )) {
    # variable <- forcats::fct_infreq(variable, ordered = TRUE)
    p <- paste0(theme, "\n", reorder_freq, "
ggplot(",
  deparse(substitute(data)),
  ", aes(x=",
  as.character(substitute(vars)),
  ", fill=..count..)) +
  geom_bar(aes(",
  as.character(substitute(vars)),
  "),stat='count', width=0.75) +
  scale_fill_gradientn(colours = colorRampPalette(rev(RColorBrewer::brewer.pal(4, 'Spectral')))(3)) +
  coord_flip() +
  ", theme_detail_z)
  }
  else if (diagram == "alphab. reordered bar graph" & (
    is.factor(unlist(data[, vars])) == TRUE |
    is.character(unlist(data[, vars])) == TRUE
  )) {
    p <- paste0(theme, "\n", reorder_alphab, "
ggplot(",
  deparse(substitute(data)),
  ", aes(x=",
  as.character(substitute(vars)),
  ")) +
  geom_bar(aes(",
  as.character(substitute(vars)),
  "),stat='count', width=0.75, fill='black') +
  coord_flip() +
  ", theme_detail)
  }
  else if (diagram == "bw alphab. reordered bar graph" & (
    is.factor(unlist(data[, vars])) == TRUE |
    is.character(unlist(data[, vars])) == TRUE
  )) {
    # variable <- forcats::fct_infreq(variable, ordered = TRUE)
    p <- paste0(theme, "\n", reorder_alphab, "
ggplot(",
  deparse(substitute(data)),
  ", aes(x=",
  as.character(substitute(vars)),
  ", fill=..count..)) +
  geom_bar(aes(",
  as.character(substitute(vars)),
  "),stat='count', width=0.75) +
  scale_fill_gradientn(colours = colorRampPalette(c('#E5E5E5', '#000000'))(2)) +
  coord_flip() +
  ", theme_detail_z)
  }
  else if (diagram == "color alphab. reordered bar graph" & (
    is.factor(unlist(data[, vars])) == TRUE |
    is.character(unlist(data[, vars])) == TRUE
  )) {
    # variable <- forcats::fct_infreq(variable, ordered = TRUE)
    p <- paste0(theme, "\n", reorder_alphab, "
ggplot(",
  deparse(substitute(data)),
  ", aes(x=",
  as.character(substitute(vars)),
  ", fill=..count..)) +
  geom_bar(aes(",
  as.character(substitute(vars)),
  "),stat='count', width=0.75) +
  scale_fill_gradientn(colours = colorRampPalette(rev(RColorBrewer::brewer.pal(4, 'Spectral')))(3)) +
  coord_flip() +
  ", theme_detail_z)
  }
  else if (diagram == "linerange graph" & (
    is.logical(unlist(data[, vars])) == TRUE |
    is.factor(unlist(data[, vars])) == TRUE |
    is.ordered(unlist(data[, vars])) == TRUE |
    is.character(unlist(data[, vars])) == TRUE
  )) {
    p <- paste0(theme, "\n", reorder_observ, "
ggplot(",
  deparse(substitute(data)),
  ", aes(y=",
  as.character(substitute(vars)),
  ")) +
  geom_path(aes(x=seq_along(", substitute(vars), ")), size=0.5) +
  geom_point(aes(x=seq_along(", substitute(vars), ")), size=1) +
  labs(x='seq.') +
  ", theme_detail)
  }
  else if (diagram == "freq. reordered linerange graph" & (
    is.logical(unlist(data[, vars])) == TRUE |
    is.factor(unlist(data[, vars])) == TRUE |
    is.ordered(unlist(data[, vars])) == TRUE |
    is.character(unlist(data[, vars])) == TRUE
  )) {
    p <- paste0(theme, "\n", reorder_freq, "
ggplot(",
  deparse(substitute(data)),
  ", aes(y=",
  as.character(substitute(vars)),
  ")) +
  geom_path(aes(x=seq_along(", substitute(vars), ")), size=1) +
  geom_point(aes(x=seq_along(", substitute(vars), ")), size=1) +
  labs(x='seq.') +
  ", theme_detail)
  }
  else if (diagram == "alphab. reordered linerange graph" & (
    is.logical(unlist(data[, vars])) == TRUE |
    is.factor(unlist(data[, vars])) == TRUE |
    is.ordered(unlist(data[, vars])) == TRUE |
    is.character(unlist(data[, vars])) == TRUE
  )) {
    p <- paste0(theme, "\n", reorder_alphab, "
ggplot(",
  deparse(substitute(data)),
  ", aes(y=",
  as.character(substitute(vars)),
  ")) +
  geom_path(aes(x=seq_along(", substitute(vars), ")), size=1) +
  geom_point(aes(x=seq_along(", substitute(vars), ")), size=1) +
  labs(x='seq.') +
  ", theme_detail)
  }
  else if (diagram == "binned heatmap" &
    (is.factor(unlist(data[, vars])) == TRUE |
     is.character(unlist(data[, vars])) == TRUE |
     is.logical(unlist(data[, vars])) == TRUE |
     is.ordered(unlist(data[, vars])) == TRUE)) {
    p <- paste0(theme, "\n", reorder_observ, "
ggplot(",
  deparse(substitute(data)),
  ", aes(y=",
  as.character(substitute(vars)),
  ")) +
  geom_bin2d(aes(x=seq_along(", substitute(vars), ")), fill = 'black') +
  labs(x='seq.') +
  ", theme_detail_z)
  }
  else if (diagram == "binned heatmap" & (
    is.numeric(unlist(data[, vars])) == TRUE |
    lubridate::is.instant(unlist(data[, vars])) == TRUE
  )) {
    p <- paste0(theme, "
ggplot(",
  deparse(substitute(data)),
  ", aes(y=",
  as.character(substitute(vars)),
  ")) +
  geom_bin2d(aes(x=seq_along(", substitute(vars), ")), fill = 'black') +
  labs(x='seq.') +
  ", theme_detail_z)
  }
  else if (diagram == "bw binned heatmap" &
    (is.factor(unlist(data[, vars])) == TRUE |
    is.character(unlist(data[, vars])) == TRUE |
    is.logical(unlist(data[, vars])) == TRUE |
    is.ordered(unlist(data[, vars])) == TRUE)) {
    p <- paste0(theme, "\n", reorder_observ, "
ggplot(",
  deparse(substitute(data)),
  ", aes(y=",
  as.character(substitute(vars)),
  ")) +
  geom_bin2d(aes(x=seq_along(", substitute(vars), "))) +
  scale_fill_gradientn(colours = colorRampPalette(c('#E5E5E5', '#000000'))(2)) +
  labs(x='seq.') +
  ", theme_detail_z)
  }
  else if (diagram == "bw heatmap" & (
    is.numeric(unlist(data[, vars])) == TRUE |
    lubridate::is.instant(unlist(data[, vars])) == TRUE
  )) {
    p <- paste0(theme, "
ggplot(",
  deparse(substitute(data)),
  ", aes(y=",
  as.character(substitute(vars)),
  ")) +
  stat_density_2d(aes(x=seq_along(", substitute(vars), "), fill = stat(density)), geom = 'raster', contour = FALSE) +
  scale_fill_gradientn(colours = colorRampPalette(c('#E5E5E5', '#000000'))(2)) +
  labs(x='seq.') +
  ", theme_detail_z)
  }
  else if (diagram == "color heatmap" & (
    is.numeric(unlist(data[, vars])) == TRUE |
    lubridate::is.instant(unlist(data[, vars])) == TRUE
  )) {
    p <- paste0(theme, "
ggplot(",
  deparse(substitute(data)),
  ", aes(y=",
  as.character(substitute(vars)),
  ")) +
  stat_density_2d(aes(x=seq_along(", substitute(vars), "), fill = stat(density)), geom = 'raster', contour = FALSE) +
  scale_fill_gradientn(colours = colorRampPalette(rev(RColorBrewer::brewer.pal(4, 'Spectral')))(3)) +
  labs(x='seq.') +
  ", theme_detail_z)
  }
  else if (diagram == "bw binned heatmap" & (
    is.numeric(unlist(data[, vars])) == TRUE |
    lubridate::is.instant(unlist(data[, vars])) == TRUE
  )) {
    p <- paste0(theme, "
ggplot(",
  deparse(substitute(data)),
  ", aes(y=",
  as.character(substitute(vars)),
  ")) +
  geom_bin2d(aes(x=seq_along(", substitute(vars), "))) +
  scale_fill_gradientn(colours = colorRampPalette(c('#E5E5E5', '#000000'))(2)) +
  labs(x='seq.') +
  ", theme_detail_z)
  }
  else if (diagram == "color binned heatmap" &
    (is.factor(unlist(data[, vars])) == TRUE |
    is.character(unlist(data[, vars])) == TRUE |
    is.logical(unlist(data[, vars])) == TRUE |
    is.ordered(unlist(data[, vars])) == TRUE)) {
    p <- paste0(theme, "\n", reorder_observ, "
ggplot(",
  deparse(substitute(data)),
  ", aes(y=",
  as.character(substitute(vars)),
  ")) +
  geom_bin2d(aes(x=seq_along(", substitute(vars), "))) +
  scale_fill_gradientn(colours = colorRampPalette(rev(RColorBrewer::brewer.pal(4, 'Spectral')))(3)) +
  labs(x='seq.') +
  ", theme_detail_z)
  }
  else if (diagram == "color binned heatmap" & (
    is.numeric(unlist(data[, vars])) == TRUE |
    lubridate::is.instant(unlist(data[, vars])) == TRUE
  )) {
    p <- paste0(theme, "
ggplot(",
  deparse(substitute(data)),
  ", aes(y=",
  as.character(substitute(vars)),
  ")) +
  geom_bin2d(aes(x=seq_along(", substitute(vars), "))) +
  scale_fill_gradientn(colours = colorRampPalette(rev(RColorBrewer::brewer.pal(4, 'Spectral')))(3)) +
  labs(x='seq.') +
  ", theme_detail_z)
  }
  else if (diagram == "freq. reordered binned heatmap" &
    (is.factor(unlist(data[, vars])) == TRUE |
    is.character(unlist(data[, vars])) == TRUE)) {
    p <- paste0(theme, "\n", reorder_freq, "
ggplot(",
  deparse(substitute(data)),
  ", aes(y=",
  as.character(substitute(vars)),
  ")) +
  geom_bin2d(aes(x=seq_along(", substitute(vars), ")), fill = 'black') +
  labs(x='seq.') +
  ", theme_detail_z)
  }
  else if (diagram == "bw freq. reordered binned heatmap" &
    (is.factor(unlist(data[, vars])) == TRUE |
    is.character(unlist(data[, vars])) == TRUE)) {
    p <- paste0(theme, "\n", reorder_freq, "
ggplot(",
  deparse(substitute(data)),
  ", aes(y=",
  as.character(substitute(vars)),
  ")) +
  geom_bin2d(aes(x=seq_along(", substitute(vars), "))) +
  scale_fill_gradientn(colours = colorRampPalette(c('#E5E5E5', '#000000'))(2)) +
  labs(x='seq.') +
  ", theme_detail_z)
  }
  else if (diagram == "color freq. reordered binned heatmap" &
    (is.factor(unlist(data[, vars])) == TRUE |
    is.character(unlist(data[, vars])) == TRUE)) {
    p <- paste0(theme, "\n", reorder_freq, "
ggplot(",
  deparse(substitute(data)),
  ", aes(y=",
  as.character(substitute(vars)),
  ")) +
  geom_bin2d(aes(x=seq_along(", substitute(vars), "))) +
  scale_fill_gradientn(colours = colorRampPalette(rev(RColorBrewer::brewer.pal(4, 'Spectral')))(3)) +
  labs(x='seq.') +
  ", theme_detail_z)
  }
  else if (diagram == "alphab. reordered binned heatmap" &
    (is.factor(unlist(data[, vars])) == TRUE |
    is.character(unlist(data[, vars])) == TRUE)) {
    p <- paste0(theme, "\n", reorder_alphab, "
ggplot(",
  deparse(substitute(data)),
  ", aes(y=",
  as.character(substitute(vars)),
  ")) +
  geom_bin2d(aes(x=seq_along(", substitute(vars), ")), fill = 'black') +
  labs(x='seq.') +
  ", theme_detail_z)
  }
  else if (diagram == "bw alphab. reordered binned heatmap" &
    (is.factor(unlist(data[, vars])) == TRUE |
    is.character(unlist(data[, vars])) == TRUE)) {
    p <- paste0(theme, "\n", reorder_alphab, "
ggplot(",
  deparse(substitute(data)),
  ", aes(y=",
  as.character(substitute(vars)),
  ")) +
  geom_bin2d(aes(x=seq_along(", substitute(vars), "))) +
  scale_fill_gradientn(colours = colorRampPalette(c('#E5E5E5', '#000000'))(2)) +
  labs(x='seq.') +
  ", theme_detail_z)
  }
  else if (diagram == "color alphab. reordered binned heatmap" &
    (is.factor(unlist(data[, vars])) == TRUE |
    is.character(unlist(data[, vars])) == TRUE)) {
    p <- paste0(theme, "\n", reorder_alphab, "
ggplot(",
  deparse(substitute(data)),
  ", aes(y=",
  as.character(substitute(vars)),
  ")) +
  geom_bin2d(aes(x=seq_along(", substitute(vars), "))) +
  scale_fill_gradientn(colours = colorRampPalette(rev(RColorBrewer::brewer.pal(4, 'Spectral')))(3)) +
  labs(x='seq.') +
  ", theme_detail_z)
  }
  else if (diagram == "stepped line graph" &
    is.numeric(unlist(data[, vars])) == TRUE |
    lubridate::is.instant(unlist(data[, vars])) == TRUE
  ) {
    p <- paste0(theme, "
ggplot(",
  deparse(substitute(data)),
  ", aes(x=seq_along(",
  as.character(substitute(vars)),
  "), y=",
  as.character(substitute(vars)),
  ")) +
  geom_step(direction = 'hv') +
  labs(x='seq') +
  ", theme_detail)
  }
  else if (diagram == "area graph" &
           is.numeric(unlist(data[, vars])) == TRUE
  ) {
    p <- paste0(theme, "
ggplot(",
  deparse(substitute(data)),
  ", aes(x=seq_along(",
  as.character(substitute(vars)),
  "), y=",
  as.character(substitute(vars)),
  ")) +
  geom_area(, fill = 'black') +
  labs(x='seq') +
  ", theme_detail)
  }
  else if (diagram == "stepped area graph" &
           is.numeric(unlist(data[, vars])) == TRUE
  ) {
    p <- paste0(theme, "
ggplot(",
  deparse(substitute(data)),
  ", aes(x=seq_along(",
  as.character(substitute(vars)),
  "), y=",
  as.character(substitute(vars)),
  ")) +
  geom_bar(fill = 'black', width = 1, stat = 'identity') +
  labs(x='seq') +
  ", theme_detail)
  }
  else if (diagram == "histogram" &
           is.numeric(unlist(data[, vars])) == TRUE
  ) {
    p <- paste0(theme, "
ggplot(",
  deparse(substitute(data)),
  ", aes(x=",
  as.character(substitute(vars)),
  ")) +
  geom_histogram(bins = 20, fill='black') +
  ", theme_detail)
  }
  else if (diagram == "bw histogram" &
           is.numeric(unlist(data[, vars])) == TRUE
  ) {
    p <- paste0(theme, "
ggplot(",
  deparse(substitute(data)),
  ", aes(x=",
  as.character(substitute(vars)),
  ", fill=..count..)) +
  geom_histogram(bins = 20) +
  scale_fill_gradientn(colours = colorRampPalette(c('#E5E5E5', '#000000'))(2)) +
  ", theme_detail_z)
  }
  else if (diagram == "color histogram" &
           is.numeric(unlist(data[, vars])) == TRUE
  ) {
    p <- paste0(theme, "
ggplot(",
  deparse(substitute(data)),
  ", aes(x=",
  as.character(substitute(vars)),
  ", fill=..count..)) +
  geom_histogram(bins = 20) +
  scale_fill_gradientn(colours = colorRampPalette(rev(RColorBrewer::brewer.pal(4, 'Spectral')))(3)) +
  ", theme_detail_z)
  }
  else if (diagram == "freq. polygon" &
           is.numeric(unlist(data[, vars])) == TRUE
  ) {
    p <- paste0(theme, "
ggplot(",
  deparse(substitute(data)),
  ", aes(x=",
  as.character(substitute(vars)),
  ")) +
  geom_line(stat = 'bin', bins = 20, center = 0, size = 0.5) +
  ", theme_detail)
  }
  else if (diagram == "density plot" &
           is.numeric(unlist(data[, vars])) == TRUE
  ) {
    p <- paste0(theme, "
ggplot(",
  deparse(substitute(data)),
  ", aes(x=",
  as.character(substitute(vars)),
  ")) +
  geom_density(size=0.5) +
  ", theme_detail)
  }
  else if (diagram == "filled density plot" &
           is.numeric(unlist(data[, vars])) == TRUE
  ) {
    p <- paste0(theme, "
ggplot(",
  deparse(substitute(data)),
  ", aes(x=",
  as.character(substitute(vars)),
  ")) +
  geom_density(fill = 'black') +
  ", theme_detail)
  }
  else if (diagram == "violin plot" &
           is.numeric(unlist(data[, vars])) == TRUE
  ) {
    p <- paste0(theme, "
ggplot(",
  deparse(substitute(data)),
  ", aes(x=0, y=",
  as.character(substitute(vars)),
  ")) +
  geom_violin(size=0.5) +
  coord_flip() +
  ", theme_detail_y)
  }
  else if (diagram == "filled violin plot") {
    p <- paste0(theme, "
ggplot(",
  deparse(substitute(data)),
  ", aes(x=0, y=",
  as.character(substitute(vars)),
  ")) +
  geom_violin(fill = 'black') +
  coord_flip() +
  ", theme_detail)
  }
  else if (diagram == "bw point graph" &
    is.numeric(unlist(data[, vars])) == TRUE
  ) {
    p <- paste0(theme, "
add_density_1D <- function(a, b) {
  a$b <- unlist(a[, b])
                if (length(unique(na.omit(a$b))) == 1) {
                dens <- 1/length(a$b)
                }
                else if (any(is.na(a$b)) == FALSE) {
                dens <- sm::sm.density(a$b, eval.points=a$b, display='none')$estimate
                }
                else {
                ind <- which(is.na(a$b) == TRUE)
                dens <- sm::sm.density(na.omit(a$b), eval.points=na.omit(a$b), display='none')$estimate
                for(i in 1:(length(ind))) dens <- append(dens, NA, after=(ind[i]+1)-2)
                }
                return(dens)
}
ggplot(",
  deparse(substitute(data)),
  ", aes(x=seq_along(",
  as.character(substitute(vars)),
  "), y=",
  as.character(substitute(vars)),
  ")) +
  geom_point(aes(color=add_density_1D(",
  deparse(substitute(data)),
  ", '",
  as.character(substitute(vars)),
  "'))) +
  scale_color_gradientn(colours = colorRampPalette(c('#E5E5E5', '#000000'))(2)) +
  labs(x='seq') +
  ", theme_detail_z)
  }
  else if (diagram == "bw point graph with trend line" &
         is.numeric(unlist(data[, vars])) == TRUE
  ) {
    p <- paste0(theme, "
  add_density_1D <- function(a, b) {
    a$b <- unlist(a[, b])
                 if (length(unique(na.omit(a$b))) == 1) {
                 dens <- 1/length(a$b)
                 }
                 else if (any(is.na(a$b)) == FALSE) {
                 dens <- sm::sm.density(a$b, eval.points=a$b, display='none')$estimate
                 }
                 else {
                 ind <- which(is.na(a$b) == TRUE)
                 dens <- sm::sm.density(na.omit(a$b), eval.points=na.omit(a$b), display='none')$estimate
                  for(i in 1:(length(ind))) dens <- append(dens, NA, after=(ind[i]+1)-2)
                  }
                  return(dens)
}
ggplot(",
  deparse(substitute(data)),
  ", aes(x=seq_along(",
  as.character(substitute(vars)),
  "), y=",
  as.character(substitute(vars)),
  ")) +
  geom_point(aes(color=add_density_1D(",
  deparse(substitute(data)),
  ", '",
  as.character(substitute(vars)),
  "'))) +
  geom_smooth(method = 'loess', size=0.5) +
  scale_color_gradientn(colours = colorRampPalette(c('#E5E5E5', '#000000'))(2)) +
  labs(x='seq') +
  ", theme_detail_z)
}
  else if (diagram == "color point graph" &
           is.numeric(unlist(data[, vars])) == TRUE
  ) {
    p <- paste0(theme, "
add_density_1D <- function(a, b) {
  a$b <- unlist(a[, b])
                if (length(unique(na.omit(a$b))) == 1) {
                dens <- 1/length(a$b)
                }
                else if (any(is.na(a$b)) == FALSE) {
                dens <- sm::sm.density(a$b, eval.points=a$b, display='none')$estimate
                }
                else {
                ind <- which(is.na(a$b) == TRUE)
                dens <- sm::sm.density(na.omit(a$b), eval.points=na.omit(a$b), display='none')$estimate
                for(i in 1:(length(ind))) dens <- append(dens, NA, after=(ind[i]+1)-2)
                }
                return(dens)
  }
ggplot(",
  deparse(substitute(data)),
  ", aes(x=seq_along(",
  as.character(substitute(vars)),
  "), y=",
  as.character(substitute(vars)),
  ")) +
  geom_point(aes(color=add_density_1D(",
  deparse(substitute(data)),
  ", '",
  as.character(substitute(vars)),
  "'))) +
  scale_color_gradientn(colours = colorRampPalette(rev(RColorBrewer::brewer.pal(4, 'Spectral')))(3)) +
  labs(x='seq') +
  ", theme_detail_z)
  }
else if (diagram == "color point graph with trend line" &
         is.numeric(unlist(data[, vars])) == TRUE
) {
  p <- paste0(theme, "
add_density_1D <- function(a, b) {
  a$b <- unlist(a[, b])
                if (length(unique(na.omit(a$b))) == 1) {
                dens <- 1/length(a$b)
                }
                else if (any(is.na(a$b)) == FALSE) {
                dens <- sm::sm.density(a$b, eval.points=a$b, display='none')$estimate
                }
                else {
                ind <- which(is.na(a$b) == TRUE)
                dens <- sm::sm.density(na.omit(a$b), eval.points=na.omit(a$b), display='none')$estimate
                for(i in 1:(length(ind))) dens <- append(dens, NA, after=(ind[i]+1)-2)
                }
                return(dens)
  }
ggplot(",
  deparse(substitute(data)),
  ", aes(x=seq_along(",
  as.character(substitute(vars)),
  "), y=",
  as.character(substitute(vars)),
  ")) +
  geom_point(aes(color=add_density_1D(",
  deparse(substitute(data)),
  ", '",
  as.character(substitute(vars)),
  "'))) +
  geom_smooth(method = 'loess', size=0.5) +
  scale_color_gradientn(colours = colorRampPalette(rev(RColorBrewer::brewer.pal(4, 'Spectral')))(3)) +
  labs(x='seq') +
  ", theme_detail_z)
}
  else if (diagram == "binned point graph" & (
    is.factor(unlist(data[, vars])) == TRUE |
    is.numeric(unlist(data[, vars])) == TRUE
  )) {
    p <- paste0(theme, "
ggplot(",
  deparse(substitute(data)),
  ", aes(y=",
  as.character(substitute(vars)),
  ")) +
  geom_point(aes(x=seq_along(", substitute(vars), ")), color='black', stat= 'bin2d') +
  labs(x='seq.') +
  ", theme_detail_z)
  }
  else if (diagram == "bw binned point graph" & (
    is.factor(unlist(data[, vars])) == TRUE |
    is.numeric(unlist(data[, vars])) == TRUE
  )) {
    p <- paste0(theme, "
ggplot(",
  deparse(substitute(data)),
  ", aes(y=",
  as.character(substitute(vars)),
  ")) +
  geom_point(aes(x=seq_along(", substitute(vars), "), color=..count..), stat= 'bin2d') +
  scale_color_gradientn(colours = colorRampPalette(c('#E5E5E5', '#000000'))(2)) +
  labs(x='seq.') +
  ", theme_detail_z)
  }
  else if (diagram == "color binned point graph" & (
    is.factor(unlist(data[, vars])) == TRUE |
    is.numeric(unlist(data[, vars])) == TRUE
  )) {
    p <- paste0(theme, "
ggplot(",
  deparse(substitute(data)),
  ", aes(y=",
  as.character(substitute(vars)),
  ")) +
  geom_point(aes(x=seq_along(", substitute(vars), "), color=..count..), stat= 'bin2d') +
  scale_color_gradientn(colours = colorRampPalette(rev(RColorBrewer::brewer.pal(4, 'Spectral')))(3)) +
  labs(x='seq.') +
  ", theme_detail_z)
  }
  else if (diagram == "box plot" &
           is.numeric(unlist(data[, vars])) == TRUE
  ) {
    p <- paste0(theme, "
ggplot(",
  deparse(substitute(data)),
  ", aes(x=0, y=",
  as.character(substitute(vars)),
  ")) +
  geom_boxplot() +
  coord_flip() +
  ", theme_detail_y)
  }
  else if (diagram == "normal qq plot" &
           is.numeric(unlist(data[, vars])) == TRUE
  ) {

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
  else if (diagram == "3 uniaxial" &
           is.numeric(unlist(data[, vars])) == TRUE
  ) {

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
  else if (diagram == "stripe graph" &
           is.numeric(unlist(data[, vars])) == TRUE) {
    p <- paste0(theme, "
ggplot(",
  deparse(substitute(data)),
  ", aes(x=",
  as.character(substitute(vars)),
  ")) +
  geom_linerange(aes(ymin=0, ymax=1)) +
  ", theme_detail_y)
  }
  else if (diagram == "bw stripe graph" &
           is.numeric(unlist(data[, vars])) == TRUE) {
    p <- paste0(theme, "
add_density_1D <- function(a, b) {
  a$b <- unlist(a[, b])
                if (length(unique(na.omit(a$b))) == 1) {
                dens <- 1/length(a$b)
                }
                else if (any(is.na(a$b)) == FALSE) {
                dens <- sm::sm.density(a$b, eval.points=a$b, display='none')$estimate
                }
                else {
                ind <- which(is.na(a$b) == TRUE)
                dens <- sm::sm.density(na.omit(a$b), eval.points=na.omit(a$b), display='none')$estimate
                for(i in 1:(length(ind))) dens <- append(dens, NA, after=(ind[i]+1)-2)
                }
                return(dens)
  }
ggplot(",
  deparse(substitute(data)),
  ", aes(x=",
  as.character(substitute(vars)),
  ")) +
  geom_linerange(aes(ymin=0, ymax=1, color=add_density_1D(",
  deparse(substitute(data)),
  ", '",
  as.character(substitute(vars)),
  "'))) +
  scale_color_gradientn(colours = colorRampPalette(c('#E5E5E5', '#000000'))(2)) +
  ", theme_detail_yz)
  }
  else if (diagram == "color stripe graph" &
           is.numeric(unlist(data[, vars])) == TRUE) {
    p <- paste0(theme, "
add_density_1D <- function(a, b) {
  a$b <- unlist(a[, b])
                if (length(unique(na.omit(a$b))) == 1) {
                dens <- 1/length(a$b)
                }
                else if (any(is.na(a$b)) == FALSE) {
                dens <- sm::sm.density(a$b, eval.points=a$b, display='none')$estimate
                }
                else {
                ind <- which(is.na(a$b) == TRUE)
                dens <- sm::sm.density(na.omit(a$b), eval.points=na.omit(a$b), display='none')$estimate
                for(i in 1:(length(ind))) dens <- append(dens, NA, after=(ind[i]+1)-2)
                }
                return(dens)
  }
ggplot(",
  deparse(substitute(data)),
  ", aes(x=",
  as.character(substitute(vars)),
  ")) +
  geom_linerange(aes(ymin=0, ymax=1, color=add_density_1D(",
  deparse(substitute(data)),
  ", '",
  as.character(substitute(vars)),
  "'))) +
  scale_color_gradientn(colours = colorRampPalette(rev(RColorBrewer::brewer.pal(4, 'Spectral')))(3)) +
  ", theme_detail_yz)
  }
  else if (diagram == "binned stripe graph" &
           is.numeric(unlist(data[, vars])) == TRUE) {
    p <- paste0(theme, "
binwidth <- (max(",
substitute(data),
"['",
substitute(vars),
"'], na.rm=TRUE)-min(",
substitute(data),
"['",
substitute(vars),
"'], na.rm=TRUE))/20
ggplot(",
  deparse(substitute(data)),
  ", aes(x=",
  as.character(substitute(vars)),
  ")) +
  geom_bin2d(aes(y=1), binwidth = c(binwidth, 1), fill='black') +
  ", theme_detail_y)
  }
  else if (diagram == "bw binned stripe graph" &
           is.numeric(unlist(data[, vars])) == TRUE) {
    p <- paste0(theme, "
binwidth <- (max(",
substitute(data),
"['",
substitute(vars),
"'], na.rm=TRUE)-min(",
substitute(data),
"['",
substitute(vars),
"'], na.rm=TRUE))/20
ggplot(",
  deparse(substitute(data)),
  ", aes(x=",
  as.character(substitute(vars)),
  ")) +
  geom_bin2d(aes(y=1), binwidth = c(binwidth, 1)) +
  scale_fill_gradientn(colours = colorRampPalette(c('#E5E5E5', '#000000'))(2)) +
  ", theme_detail_yz)
  }
  else if (diagram == "color binned stripe graph" &
           is.numeric(unlist(data[, vars])) == TRUE) {
    p <- paste0(theme, "
binwidth <- (max(",
substitute(data),
"['",
substitute(vars),
"'], na.rm=TRUE)-min(",
substitute(data),
"['",
substitute(vars),
"'], na.rm=TRUE))/20
ggplot(",
  deparse(substitute(data)),
  ", aes(x=",
  as.character(substitute(vars)),
  ")) +
  geom_bin2d(aes(y=1), binwidth = c(binwidth, 1)) +
  scale_fill_gradientn(colours = colorRampPalette(rev(RColorBrewer::brewer.pal(4, 'Spectral')))(3)) +
  ", theme_detail_yz)
  }
  else {stop("The combination of these data and graphic type has not been still considered.")}
  if (output == 'console') {
    cat(p)}
  else if (output == 'plots pane') {
    eval(parse(text=p))}
  else if (output == 'html') {
    writeLines(output_up, file.path(dir, "output.R"))
    write(paste0("cat('A ", deparse(substitute(diagram)), " produced from the " ,deparse(substitute(vars)), " variable(s) of the ", deparse(substitute(data))," dataframe')"),
          file=file.path(dir, "output.R"), append=TRUE)
    write(paste0("#+ plot, fig.width=6, fig.height=", long), file.path(dir, "output.R"), append = TRUE)
    write(p, file.path(dir, "output.R"), append = TRUE)
    rmarkdown::render(file.path(dir, "output.R"),"html_document", envir=my_env)
    pander::openFileInOS(file.path(dir, "output.html"))
    }
  }

#' Returns a ggplot object of a specific graphic explicitly called by name.
#'
#' In order to present the graphic, the user must define a dataset, at
#' least one variable whitin this dataset and a compatible type of graphic.
#' Future work will include graphics that can combine up to three variables.
#'
#' @param data Data.frame. Default dataset to use for plot. If not already a
#' data.frame, it should be first coerced to by [as.data.frame()].
#' @param vars Character. A variable within the dataset.
#' @param diagram Character. A specific graphic to be presented within the ones
#' considered by the 'logical', 'ordered', 'factor', 'character', 'datetime' and 'numeric'
#' arguments of the 'wideplot()' function.
#' @param output Character. Type of output.
#' \itemize{
#'   \item \emph{'html'}: Creates and displays a html file with the specific graphic.
#'   \item \emph{'plots pane'}: Default output, a ggplot2 object in RStudio's plots pane.
#'   \item \emph{'console'}: Prints the code that produces the specific graphic.
#' }
#' @param dir Directory in which the files are stored.
#'
#' @return This function returns a c('gg', 'ggplot') object, but if the 'output' argument
#' is set to it 'html' or 'console', the function cause a side-effect: either creating and
#' displaying a temporary html file, or printing the ggplot2 code to the console.
#'
#' @export
#'
#' @examples
#' plotup(iris, "Petal.Width", "color heatmap")
#' plotup(iris, "Petal.Width", "color heatmap", output = "console")
#' if (interactive()) {
#' plotup(iris, "Petal.Width", "color heatmap", output = "html")
#' }
plotup <- function(data,
                   vars,
                   diagram,
                   output = 'plots pane',
                   dir = tempdir()
                   )
{

# check pandoc ------------------------------------------------------------


  if (rmarkdown::pandoc_available("1.12.3") == FALSE) {print(warning_pandoc)}
  else if (rmarkdown::pandoc_available("1.12.3") == TRUE) {
  my_env <- new.env()

# check class -------------------------------------------------------------


    if(is.data.frame(data) == FALSE) {
    stop("This function only works with a data.frame input!\n",
         "You have provided an object of class ", class(data))
  }
  if(tibble::is_tibble(data) == TRUE) {
    stop(warning_tibble)
    # data <- as.data.frame(data)
    # envir=my_env
  }

# trans vars --------------------------------------------------------------

  if (length(vars) == 1) {
    if (is.logical(unlist(data[, vars])) == TRUE) {v <- "L"}
    else if (is.character(unlist(data[, vars])) == TRUE) {v <- "C"}
    else if (is.ordered(unlist(data[, vars])) == FALSE &
             is.factor(unlist(data[, vars])) == TRUE) {v <- "F"}
    else if (is.ordered(unlist(data[, vars])) == TRUE) {v <- "O"}
    else if (lubridate::is.instant(unlist(data[, vars])) == TRUE) {v <- "D"}
    else if (is.numeric(unlist(data[, vars])) == TRUE) {v <- "N"}
  }
  else if (length(vars) == 2) {
    if (is.logical(unlist(data[, vars[1]])) == TRUE) {v <- "L"}
    else if (is.character(unlist(data[, vars[1]])) == TRUE) {v <- "C"}
    else if (is.ordered(unlist(data[, vars[1]])) == FALSE &
             is.factor(unlist(data[, vars[1]])) == TRUE) {v <- "F"}
    else if (is.ordered(unlist(data[, vars[1]])) == TRUE) {v <- "O"}
    else if (lubridate::is.instant(unlist(data[, vars[1]])) == TRUE) {v <- "D"}
    else if (is.numeric(unlist(data[, vars[1]])) == TRUE) {v <- "N"}
    if (is.logical(unlist(data[, vars[2]])) == TRUE) {v[2] <- "L"}
    else if (is.character(unlist(data[, vars[2]])) == TRUE) {v[2] <- "C"}
    else if (is.ordered(unlist(data[, vars[2]])) == FALSE &
             is.factor(unlist(data[, vars[2]])) == TRUE) {v[2] <- "F"}
    else if (is.ordered(unlist(data[, vars[2]])) == TRUE) {v[2] <- "O"}
    else if (lubridate::is.instant(unlist(data[, vars[2]])) == TRUE) {v[2] <- "D"}
    else if (is.numeric(unlist(data[, vars[2]])) == TRUE) {v[2] <- "N"}
  }

# check vars --------------------------------------------------------------


  string      <- " argument expects a character vector"
  if(is.character(vars)  == FALSE) {
    stop(paste0("The 'vars'",  string))
  }
    if(length(vars) > 2) {
    stop("Up to now, only one and two variables combinations have been considered.")
    }

# check diagrams ----------------------------------------------------------

  if (length(vars) == 1) {
    trythis_1 <- paste0("
  This diagram's name is not available for a '", class(unlist(data[, vars])) ,"' input variable.

  Please take a look at the specimen:
  https://sciencegraph.github.io/brinton/articles/specimen.html

  Or try, for instance, one of these diagrams: ")

      if (v == "L" & any(!diagram %in% logical_v)) {
        stop(paste0(trythis_1, paste0(logical_v[c(2:length(logical_v))], collapse = ", ")))
        }
      else if (v == "C" & any(!diagram %in% character_v)) {
        stop(paste0(trythis_1, paste0(character_v[c(2:length(character_v))], collapse = ", ")))
        }
      else if (v == "F" &
               any(!diagram %in% factor_v)) {
        stop(paste0(trythis_1, paste0(factor_v[c(2:length(factor_v))], collapse = ", ")))
      }
      else if (v == "O" &
               any(!diagram %in% ordered_v)) {
        stop(paste0(trythis_1, paste0(ordered_v[c(2:length(ordered_v))], collapse = ", ")))
      }
      else if (v == "D" &
               any(!diagram %in% datetime_v)) {
        stop(paste0(trythis_1, paste0(datetime_v[c(2:length(datetime_v))], collapse = ", ")))
      }
      else if ( v == "N" & any(!diagram %in% numeric_v)) {
        stop(paste0(trythis_1, paste0(datetime_v[c(2:length(datetime_v))], collapse = ", ")))
      }
  }


  if (length(vars) == 2) {
    trythis_2 <- paste0("
  This diagram's name is not available for the combination of c('", class(unlist(data[, vars[1]]))[1], "', '", class(unlist(data[, vars[2]]))[1], "') input variables.

  Please take a look at the specimen:
  https://sciencegraph.github.io/brinton/articles/specimen2.html

  Or try, for instance, one of these diagrams: ")

    if (all(v == "N") &
        any(!diagram %in% numeric2_v))
    {stop(paste0(trythis_2, paste0(numeric2_v[c(2:length(numeric2_v))],
                                   collapse = ", ")))}
    else if (all(v == "D") &
      any(!diagram %in% datenum_v))
    {stop(paste0(trythis_2, paste0(
      datenum_v[c(2:length(datenum_v))], collapse = ", ")))}
    else if (all(v == "F") &
             any(!diagram %in% c(fac.fac_v0, fac.fac_v1, fac.fac_v2)))
    {stop(paste0(trythis_2, paste0(
      c(fac.fac_v0[c(2:length(fac.fac_v0))],
        fac.fac_v1[c(2:length(fac.fac_v1))],
        fac.fac_v2[c(2:length(fac.fac_v2))]), collapse = ", ")))}
    else if (all(v == "O") &
      any(!diagram %in% c(ord.ord_v0, ord.ord_v1, ord.ord_v2)))
    {stop(paste0(trythis_2, paste0(
      c(ord.ord_v0[c(2:length(ord.ord_v0))],
        ord.ord_v1[c(2:length(ord.ord_v1))],
        ord.ord_v2[c(2:length(ord.ord_v2))]), collapse = ", ")))}
    else if (("F" %in% v) & ("O" %in% v) &
             any(!diagram %in% c(fac.ord_v0, fac.ord_v1, fac.ord_v2)))
    {stop(paste0(trythis_2, paste0(
      c(fac.ord_v0[c(2:length(fac.ord_v0))],
        fac.ord_v1[c(2:length(fac.ord_v1))],
        fac.ord_v2[c(2:length(fac.ord_v2))]), collapse = ", ")))}
    else if (("D" %in% v) & ("N" %in% v) &
             any(!diagram %in% datenum_v))
    {stop(paste0(trythis_2, paste0(datenum_v, collapse = ", ")))}
    else if (("O" %in% v) & ("N" %in% v) &
             any(!diagram %in% c(ord.num_v1, ord.num_v2)))
    {stop(paste0(trythis_2, paste0(
      c(ord.num_v1[c(2:length(ord.num_v1))],
        ord.num_v2[c(2:length(ord.num_v2))]), collapse = ", ")))}
    else if (("F" %in% v) & ("N" %in% v) &
             any(!diagram %in% c(fac.num_v1, fac.num_v2)))
    {stop(paste0(trythis_2, paste0(
      c(fac.num_v1[c(2:length(fac.num_v1))],
        fac.num_v2[c(2:length(fac.num_v2))]), collapse = ", ")))}
  }


# figure dims -------------------------------------------------------------
  GAwidth <- 10

  if (length(vars) == 1 &
      any(v %in% c("L", "F", "O", "C"))
      ) {
    GAheight <- length(unique(unlist(data[, vars]))) / 2
  } else if (length(vars) == 1 &
           any(v %in% c("N", "D"))
           ) {
    GAheight <- 6.2
  } else if (length(vars) == 2 &
           any(v[1] == c("N", "D")) &
           any(v[2] == c("N", "D"))
           ) {
    GAheight <- 6.2
  } else if (length(vars) == 2 &
           any(fac.num_v1 == diagram) &
           any(v == c("N")) &
           any(v == c("F"))
           ) {
    GAheight <- length(unique(unlist(data[, vars][sapply(data[, vars], is.factor)])))/2
  } else if (length(vars) == 2 &
           any(fac.num_v2 == diagram) &
           any(v == c("N")) &
           any(v == c("F"))
  ) {
    GAheight <- 6.2
  } else if (length(vars) == 2 &
             any(ord.num_v1 == diagram) &
             any(v == c("N")) &
             any(v == c("O"))
  ) {
    GAheight <- length(unique(unlist(data[, vars][sapply(data[, vars], is.ordered)])))/2
  } else if (length(vars) == 2 &
             any(ord.num_v2 == diagram) &
             any(v == c("N")) &
             any(v == c("O"))
  ) {
    GAheight <- 6.2
  } else if (length(vars) == 2 &
             any(ord.ord_v0 == diagram) &
             identical(v, c("O", "O"))
  ) {
    GAheight <- length(unique(unlist(data[, vars[2]])))/1
  } else if (length(vars) == 2 &
             any(ord.ord_v1 == diagram) &
             identical(v, c("O", "O"))
  ) {
    GAheight <- length(unique(unlist(data[, vars[1]])))/1
  } else if (length(vars) == 2 &
             any(ord.ord_v2 == diagram) &
             identical(v, c("O", "O"))
  ) {
    GAheight <- length(unique(unlist(data[, vars[2]])))/1
    GAwidth  <- length(unique(unlist(data[, vars[1]])))/1
  } else if (length(vars) == 2 &
             any(fac.fac_v0 == diagram) &
             identical(v, c("F", "F"))
  ) {
    GAheight <- length(unique(unlist(data[, vars[2]])))/1
  } else if (length(vars) == 2 &
             any(fac.fac_v1 == diagram) &
             identical(v, c("F", "F"))
  ) {
    GAheight <- length(unique(unlist(data[, vars[1]])))/1
  } else if (length(vars) == 2 &
             any(fac.fac_v2 == diagram) &
             identical(v, c("F", "F"))
  ) {
    GAheight <- length(unique(unlist(data[, vars[2]])))/1
    GAwidth  <- length(unique(unlist(data[, vars[1]])))/1
  } else if (length(vars) == 2 &
             any(fac.ord_v0 == diagram) &
             any(v == c("F")) &
             any(v == c("O"))
  ) {
    GAheight <- length(unique(unlist(data[, vars][!sapply(data[, vars], is.ordered)])))/1
  } else if (length(vars) == 2 &
             any(fac.ord_v1 == diagram) &
             any(v == c("F")) &
             any(v == c("O"))
  ) {
    GAheight <- length(unique(unlist(data[, vars][sapply(data[, vars], is.ordered)])))/1
  } else if (length(vars) == 2 &
             any(fac.ord_v2 == diagram) &
             any(v == c("F")) &
             any(v == c("O"))
  ) {
    GAheight <- length(unique(unlist(data[, vars][!sapply(data[, vars], is.ordered)])))/1
    GAwidth  <- length(unique(unlist(data[, vars][sapply(data[, vars], is.ordered)])))/1
  } else {
    stop("This data and graphic type does not match or has not been yet included in the brinton's specimen.")
  }

# check diagram -----------------------------------------------------------


  # string      <- " argument expects a character string"
  # if (length(vars) == 1 &
  #     lubridate::is.instant(unlist(data[, vars])) == TRUE &
  #     (diagram %in% datetime_v) == FALSE)
  # {
  #   stop(
  #     paste0(
  #       "The 'diagram'",
  #       string,
  #       " which values can be :\n '",
  #       paste0(head(datetime_v, 10), collapse = "'\n '"),
  #       "'\n  ...\n ",
  #       spmn1
  #     )
  #   )
  # }
  # if (length(vars) == 1 &
  #     is.logical(unlist(data[, vars])) == TRUE &
  #     (diagram %in% logical_v) == FALSE)
  # {
  #   stop(
  #     paste0(
  #       "The 'diagram'",
  #       string,
  #       " which values can be :\n '",
  #       paste0(head(logical_v, 10), collapse = "'\n '"),
  #       "'\n  ...\n ",
  #       spmn1
  #     )
  #   )
  # }
  # if (length(vars) == 1 &
  #     is.ordered(unlist(data[, vars])) == TRUE &
  #     (diagram %in% ordered_v) == FALSE)
  # {
  #   stop(
  #     paste0(
  #       "The 'diagram'",
  #       string,
  #       " which values can be :\n '",
  #       paste0(head(ordered_v, 10), collapse = "'\n '"),
  #       "'\n  ...\n ",
  #       spmn1
  #     )
  #   )
  # }
  # if (length(vars) == 1 &
  #     is.factor(unlist(data[, vars])) == TRUE &
  #     is.ordered(unlist(data[, vars])) == FALSE &
  #     (diagram %in% factor_v) == FALSE)
  # {
  #   stop(
  #     paste0(
  #       "The 'diagram'",
  #       string,
  #       " which values can be :\n '",
  #       paste0(head(factor_v, 10), collapse = "'\n '"),
  #       "'\n  ...\n ",
  #       spmn1
  #     )
  #   )
  # }
  # if (length(vars) == 1 &
  #     is.numeric(unlist(data[, vars])) == TRUE &
  #     (diagram %in% numeric_v == FALSE))
  # {
  #   stop(
  #     paste0(
  #       "The 'diagram'",
  #       string,
  #       " which values can be :\n '",
  #       paste0(head(numeric_v, 10), collapse = "'\n '"),
  #       "'\n  ...\n ",
  #       spmn1
  #     )
  #   )
  # }
  # if (length(vars) == 2 &
  #     sum(sapply(unlist(data[, vars]), is.numeric) == TRUE) == 2 &
  #     (diagram %in% numeric2_v == FALSE))
  # {
  #   stop(
  #     paste0(
  #       "The 'diagram'",
  #       string,
  #       " which values can be :\n '",
  #       paste0(head(numeric2_v, 10), collapse = "'\n '"),
  #       "'\n  ...\n ",
  #       spmn2
  #     )
  #   )
  # }
  # if (length(vars) == 2 &
  #     any(sapply(unlist(data[, vars]), is.numeric) == TRUE) &
  #     any(sapply(unlist(data[, vars]), is.factor) == TRUE) &
  #     (diagram %in% fac.num_v1 == FALSE))
  # {
  #   stop(
  #     paste0(
  #       "The 'diagram'",
  #       string,
  #       " which values can be :\n '",
  #       paste0(head(fac.num_v1, 10), collapse = "'\n '"),
  #       "'\n  ...\n ",
  #       spmn2
  #     )
  #   )
  # }
  # if (length(vars) == 1 &
  #     is.character(unlist(data[, vars])) == TRUE &
  #     (diagram %in% character_v) == FALSE)
  # {
  #   stop(
  #     paste0(
  #       "The 'diagram'",
  #       string,
  #       " which values can be :\n '",
  #       paste0(head(character_v, 10), collapse = "'\n '"),
  #       "'\n  ...\n ",
  #       spmn1
  #     )
  #   )
  # }

# check output ------------------------------------------------------------


  output_v <- c('html',
                'plots pane',
                'console')
  if (length(output) != sum(output %in% output_v, na.rm = TRUE))
  {
    stop(paste0(
      "The 'output' argument expects a value that can be : '",
      paste0(output_v, collapse = "', '"),
      "'"
    ))
  }

# strings -----------------------------------------------------------------


  theme <- "theme_minimal()"
  theme_basic <- "theme_minimal() +
  theme(panel.grid = element_line(colour = NA),
  \x20\x20axis.ticks = element_line(color = 'black'))\n"
  theme_basic_y <- "theme_minimal() +
  theme(panel.grid = element_line(colour = NA),
  \x20\x20axis.text.y = element_text(color = NA),
  \x20\x20axis.title.y = element_text(color = NA),
  \x20\x20axis.ticks.x = element_line(color = 'black'))\n"
  theme_basic_z <- "theme_minimal() +
  theme(panel.grid = element_line(colour = NA),
  \x20\x20axis.ticks = element_line(color = 'black'),
  \x20\x20legend.position='none')\n"
  theme_basic_yz <- "theme_minimal() +
  theme(panel.grid = element_line(colour = NA),
  \x20\x20axis.text.y = element_text(color = NA),
  \x20\x20axis.title.y = element_text(color = NA),
  \x20\x20axis.ticks.x = element_line(color = 'black'),
  \x20\x20legend.position='none')\n"
  theme_basic_stack <- "theme_minimal() +
  theme(panel.grid = element_line(colour = NA),
  \x20\x20axis.ticks.x = element_line(color = 'black'))"
  theme_basic_grid <- "theme_minimal() +
  theme(axis.ticks = element_blank(),
  \x20\x20axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
  \x20\x20panel.grid = element_line(colour = NA))"
  getdens2 <- "get_density <- function(x, y, ...) {
  dens <- MASS::kde2d(x, y, ...)
  ix <- findInterval(x, dens$x)
  iy <- findInterval(y, dens$y)
  ii <- cbind(ix, iy)
  return(dens$z[ii])
}\n"
  scale_bw_l        <- "scale_color_gradientn(colours = colorRampPalette(c('#E5E5E5', '#000000'))(2))"
  scale_bw_l_p      <- "scale_color_gradientn(colours = colorRampPalette(c('#E5E5E5', '#000000'))(2), labels = scales::label_percent())"
  scale_color_l     <- "scale_color_gradientn(colours = colorRampPalette(rev(RColorBrewer::brewer.pal(4, 'Spectral')))(3))"
  scale_value_l     <- "scale_color_gradientn(colours = colorRampPalette(rev(RColorBrewer::brewer.pal(4, 'BrBG')))(2))"
  scale_value_sym   <- "scale_fill_gradientn(colours = colorRampPalette(rev(RColorBrewer::brewer.pal(11, 'RdYlGn')))(3), limits = c(-abs(max(df[,3])), abs(max(df[,3]))))"
  scale_value_sym_l <- "scale_color_gradientn(colours = colorRampPalette(rev(RColorBrewer::brewer.pal(11, 'RdYlGn')))(3), limits = c(-abs(max(df[,3])), abs(max(df[,3]))))"
  scale_seq_l       <- "viridis::scale_color_viridis(direction = -1)"
  scale_bw_a        <- "scale_fill_gradientn(colours = colorRampPalette(c('#E5E5E5', '#000000'))(2))"
  scale_bw_a_p      <- "scale_fill_gradientn(colours = colorRampPalette(c('#E5E5E5', '#000000'))(2), labels = scales::label_percent())"
  scale_color_a     <- "scale_fill_gradientn(colours = colorRampPalette(rev(RColorBrewer::brewer.pal(4, 'Spectral')))(3))"
  scale_value_a     <- "scale_fill_gradientn(colours = colorRampPalette(rev(RColorBrewer::brewer.pal(4, 'BrBG')))(2))"
  scale_seq_a       <- "scale_color_gradientn(colours = colorRampPalette(RColorBrewer::brewer.pal(3, 'YlOrBr'))(3))"
  p_scale_value_a_p <- "scale_fill_gradientn(colours = colorRampPalette(rev(RColorBrewer::brewer.pal(11, 'RdYlGn')))(3), labels = scales::label_percent())"
  scl_gray_disc_l   <- "scale_color_grey(start = 0.8, end = 0.2)"
  scl_gray_disc_a   <- "scale_fill_grey(start = 0.8, end = 0.2)"
  scl_color_disc_l  <- "scale_color_brewer(type = 'qual', palette = 'Set1')"
  scl_color_disc_a  <- "scale_fill_brewer(type = 'qual', palette = 'Set1')"
  scl_viridis_a     <- "viridis::scale_fill_viridis(discrete=TRUE, direction = -1)"
  scl_viridis_ad    <- "viridis::scale_fill_viridis(direction = -1)"
  scl_viridis_l     <- "viridis::scale_color_viridis(discrete=TRUE, direction = -1)"
  p_legend          <- "guides(fill=guide_legend(title=as.character({as.character(substitute(vars1))}), keyheight = unit(0.4, 'cm'),
                                        title.theme = element_text(size = 9, colour = 'gray20'),
                                        reverse = TRUE))"
  p_legendt          <- "guides(fill=guide_legend(title=as.character({as.character(substitute(vars2))}), keyheight = unit(0.4, 'cm'),
                                        title.theme = element_text(size = 9, colour = 'gray20'),
                                        reverse = TRUE))"
  p_labs_fill       <- "labs(x =  names(df)[1], y = names(df)[2], fill = names(df)[3])"
  p_labs_color      <- "labs(x =  names(df)[1], y = names(df)[2], color = names(df)[3])"
  p_guides          <- "guides(fill = guide_colorbar(barwidth = unit(2, 'mm'), title.theme = element_text(size = 9, colour = 'gray30')),
                      color = guide_colorbar(barwidth = unit(2, 'mm'), title.theme = element_text(size = 9, colour = 'gray30')),
                      size = FALSE)"
  getdens1          <- "add_density_1D <- function(a, b) {
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
  unfold <- eval(
    "foo_df <- reshape(
  data = {deparse(substitute(data))},
  direction = 'long',
  v.names = 'measure',
  timevar = 'variable',
  idvar   = 'foo_id',
  varying = c('{as.character(substitute(vars1))}', '{as.character(substitute(vars2))}'),
  times   = c('{as.character(substitute(vars1))}', '{as.character(substitute(vars2))}')
  )\n"
  )

# reorder -----------------------------------------------------------------


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
reorder_freq2 <- "
data_aux <- {deparse(substitute(data))}[, c('{as.character(substitute(vars1))}', '{as.character(substitute(vars2))}')]
data_aux_y <- as.data.frame(ftable(data_aux${as.character(substitute(vars2))}, useNA = 'no'))
data_aux[,2]  <- factor(data_aux[,2], levels = data_aux_y[order(-data_aux_y$Freq),][,1], ordered = TRUE)"

# 1var diagrams -----------------------------------------------------------


  q <- ""
  if (diagram == "blank") {
    vars <- vars[1]
    p <- glue::glue(
    "
    ggplot({deparse(substitute(data))}, aes(x=seq_along({as.character(substitute(vars))}), y={as.character(substitute(vars))})) +
    \x20\x20geom_blank() +
    \x20\x20labs(x='seq') +
    \x20\x20{theme} +
    \x20\x20theme(axis.title=element_blank(),
    \x20\x20\x20\x20axis.text=element_blank(),
    \x20\x20\x20\x20axis.ticks=element_blank(),
    \x20\x20\x20\x20panel.grid = element_line(colour = NA))"
    )
  }

  else if (length(vars) == 1 &
           diagram == "normal qq plot" &
           identical(v, "N")
  ) {
    vars <- vars[1]
    q <- paste0(
                "
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
                theme_minimal() +
                theme(axis.ticks=element_line(color='black'),
                panel.grid = element_line(colour = NA),
                axis.title = element_text(colour = '#333333'))}")
    p <- paste0(
                "
                qqplot(",
                deparse(substitute(data)),
                ", '",
                as.character(substitute(vars)),
                "')
                ")
  }
  else if (length(vars) == 1 &
           diagram == "3 uniaxial" &
           identical(v, "N")) {
    vars <- vars[1]
    q <- paste0("
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
    coord_flip() +
    theme_minimal() +
    theme(axis.ticks=element_line(color='black'),
      panel.grid = element_line(colour = NA),
      axis.title.x = element_text(colour = '#333333'),
      axis.title.y=element_blank())
    pp_plot
  }"
    )
    p <- paste0("pp_3uniaxial(",
                deparse(substitute(data)),
                ", '",
                as.character(substitute(vars)),
                "')
  ")
  }
  else if (length(vars) == 1 &
           diagram == "freq. reordered line graph" &
           any(v == c("C", "F"))) {
    vars <- vars[1]
    q <- glue::glue(
      "{reorder_freq}"
    )
    p <- glue::glue(
      "
      ggplot({deparse(substitute(data))}, aes(x=seq_along({as.character(substitute(vars))}), y={as.character(substitute(vars))})) +
      \x20\x20geom_path(aes(group=1)) +
      \x20\x20labs(x='seq') +
      \x20\x20{theme_basic}"
    )
  }
  else if (length(vars) == 1 &
           diagram == "alphab. reordered line graph" &
           any(v == c("C", "F"))) {
    vars <- vars[1]
    q <- reorder_alphab
    p <- glue::glue(
      "
      ggplot({deparse(substitute(data))}, aes(x=seq_along({as.character(substitute(vars))}), y={as.character(substitute(vars))})) +
      \x20\x20geom_path(aes(group=1)) +
      \x20\x20labs(x='seq') +
      \x20\x20{theme_basic}"
      )
  }
  else if (length(vars) == 1 &
           diagram == "point-to-point graph" &
           any(v == c("D", "N"))) {
    vars <- vars[1]
    p <- glue::glue(
      "
      ggplot({deparse(substitute(data))}, aes(x=seq_along({as.character(substitute(vars))}), y={as.character(substitute(vars))})) +
      \x20\x20geom_path(aes(group=1)) +
      \x20\x20geom_point() +
      \x20\x20labs(x='seq') +
      \x20\x20{theme_basic}"
    )
  }
  else if (length(vars) == 1 &
           diagram == "point-to-point graph" &
           any(v == c("L", "C", "F", "O"))) {
    vars <- vars[1]
    q <- glue::glue(
      "{reorder_observ}"
    )
    p <- glue::glue(
      "
      ggplot({deparse(substitute(data))}, aes(x=seq_along({as.character(substitute(vars))}), y={as.character(substitute(vars))})) +
      \x20\x20geom_path(aes(group=1)) +
      \x20\x20geom_point() +
      \x20\x20labs(x='seq') +
      \x20\x20{theme_basic}"
    )
  }
  else if (length(vars) == 1 &
           diagram == "freq. reordered point-to-point graph" &
           any(v == c("C", "F"))) {
    vars <- vars[1]
    q <- glue::glue(
      "{reorder_freq}"
    )
    p <- glue::glue(
      "
      ggplot({deparse(substitute(data))}, aes(x=seq_along({as.character(substitute(vars))}), y={as.character(substitute(vars))})) +
      \x20\x20geom_path(aes(group=1)) +
      \x20\x20geom_point() +
      \x20\x20labs(x='seq') +
      \x20\x20{theme_basic}"
    )
  }
  else if (length(vars) == 1 &
           diagram == "alphab. reordered point-to-point graph" &
           any(v == c("C", "F"))) {
    vars <- vars[1]
    q <- glue::glue(
      "{reorder_alphab}"
    )
    p <- glue::glue(
      "
      ggplot({deparse(substitute(data))}, aes(x=seq_along({as.character(substitute(vars))}), y={as.character(substitute(vars))})) +
      \x20\x20geom_path(aes(group=1)) +
      \x20\x20geom_point() +
      \x20\x20labs(x='seq') +
      \x20\x20{theme_basic}"
    )
  }
  else if (length(vars) == 1 &
           diagram == "stepped point-to-point graph" &
           any(v == c("D", "N"))) {
    vars <- vars[1]
    p <- glue::glue(
      "
      ggplot({deparse(substitute(data))}, aes(x=seq_along({as.character(substitute(vars))}), y={as.character(substitute(vars))})) +
      \x20\x20geom_step(aes(group=1)) +
      \x20\x20geom_point() +
      \x20\x20labs(x='seq') +
      \x20\x20{theme_basic}"
    )
  }
  else if (length(vars) == 1 &
           diagram == "point graph" &
           any(v == c("D", "N"))) {
    vars <- vars[1]
    p <- glue::glue(
      "
      ggplot({deparse(substitute(data))}, aes(x=seq_along({as.character(substitute(vars))}), y={as.character(substitute(vars))})) +
      \x20\x20geom_point() +
      \x20\x20labs(x='seq') +
      \x20\x20{theme_basic}"
    )
  }
  else if (length(vars) == 1 &
           diagram == "point graph" &
           any(v == c("L", "C", "F", "O"))) {
    vars <- vars[1]
    q <- glue::glue(
      "{reorder_observ}"
    )
    p <- glue::glue(
      "
      ggplot({deparse(substitute(data))}, aes(x=seq_along({as.character(substitute(vars))}), y={as.character(substitute(vars))})) +
      \x20\x20geom_point() +
      \x20\x20labs(x='seq') +
      \x20\x20{theme_basic}"
    )
  }
  else if (length(vars) == 1 &
           diagram == "freq. reordered point graph" &
           any(v == c("C", "F"))) {
    vars <- vars[1]
    q <- glue::glue(
      "{reorder_freq}"
    )
    p <- glue::glue(
      "
      ggplot({deparse(substitute(data))}, aes(x=seq_along({as.character(substitute(vars))}), y={as.character(substitute(vars))})) +
      \x20\x20geom_point() +
      \x20\x20labs(x='seq') +
      \x20\x20{theme_basic}"
    )
  }
  else if (length(vars) == 1 &
           diagram == "alphab. reordered point graph" &
           any(v == c("C", "F"))) {
    vars <- vars[1]
    q <- glue::glue(
      "{reorder_alphab}"
    )
    p <- glue::glue(
      "
      ggplot({deparse(substitute(data))}, aes(x=seq_along({as.character(substitute(vars))}), y={as.character(substitute(vars))})) +
      \x20\x20geom_point() +
      \x20\x20labs(x='seq') +
      \x20\x20{theme_basic}"
    )
  }
  else if (length(vars) == 1 &
           diagram == "point graph with trend line" &
           any(v == c("N"))) {
    vars <- vars[1]
    p <- glue::glue(
      "
      ggplot({deparse(substitute(data))}, aes(x=seq_along({as.character(substitute(vars))}), y={as.character(substitute(vars))})) +
      \x20\x20geom_point() +
      \x20\x20geom_smooth(method = 'loess', size=0.5) +
      \x20\x20labs(x='seq') +
      \x20\x20{theme_basic}"
    )
  }
  else if (length(vars) == 1 &
           diagram == "line graph" &
           any(v == c("L", "C", "F", "O"))) {
    vars <- vars[1]
    q <- glue::glue(
      "{reorder_observ}"
    )
    p <- glue::glue(
      "
      ggplot({deparse(substitute(data))}, aes(x=seq_along({as.character(substitute(vars))}), y={as.character(substitute(vars))})) +
      \x20\x20geom_path(aes(group=1)) +
      \x20\x20labs(x='seq') +
      \x20\x20{theme_basic}"
    )
  }
  else if (length(vars) == 1 &
           diagram == "tile plot" &
           any(v == c("L", "C", "F", "O"))) {
    vars <- vars[1]
    q <- glue::glue(
      "{reorder_observ}"
    )
    p <- glue::glue(
      "
      ggplot({deparse(substitute(data))}, aes(x=seq_along({as.character(substitute(vars))}), y={as.character(substitute(vars))})) +
      \x20\x20geom_tile() +
      \x20\x20labs(x='seq') +
      \x20\x20{theme_basic}"
    )
  }
  else if (length(vars) == 1 &
           diagram == "freq. reordered tile plot" &
           any(v == c("C", "F"))) {
    vars <- vars[1]
    q <- glue::glue(
      "{reorder_freq}"
    )
    p <- glue::glue(
      "
      ggplot({deparse(substitute(data))}, aes(x=seq_along({as.character(substitute(vars))}), y={as.character(substitute(vars))})) +
      \x20\x20geom_tile() +
      \x20\x20labs(x='seq') +
      \x20\x20{theme_basic}"
    )
  }
  else if (length(vars) == 1 &
           diagram == "alphab. reordered tile plot" &
           any(v == c("C", "F"))) {
    vars <- vars[1]
    q <- glue::glue(
      "{reorder_alphab}"
    )
    p <- glue::glue(
      "
      ggplot({deparse(substitute(data))}, aes(x=seq_along({as.character(substitute(vars))}), y={as.character(substitute(vars))})) +
      \x20\x20geom_tile() +
      \x20\x20labs(x='seq') +
      \x20\x20{theme_basic}"
    )
  }
  else if (length(vars) == 1 &
           diagram == "bar graph" &
           any(v == c("N"))) {
    vars <- vars[1]
    q <- glue::glue(
      "binwidth <- (max({substitute(data)}['{substitute(vars)}'], na.rm=TRUE) -
                        min({substitute(data)}['{substitute(vars)}'], na.rm=TRUE))/100"
    )
    p <- glue::glue(
      "
      ggplot({deparse(substitute(data))}, aes(x={as.character(substitute(vars))})) +
      \x20\x20geom_bar(stat='count', width=binwidth, fill='black', color='black', position = 'identity') +
      \x20\x20{theme_basic}"
    )
  }
  else if (length(vars) == 1 &
           diagram == "bw bar graph" &
           any(v == c("N"))) {
    vars <- vars[1]
    q <- glue::glue(
      "binwidth <- (max({substitute(data)}['{substitute(vars)}'], na.rm=TRUE) -
                        min({substitute(data)}['{substitute(vars)}'], na.rm=TRUE))/100"
    )
    p <- glue::glue(
      "
      ggplot({deparse(substitute(data))}, aes(x={as.character(substitute(vars))}, fill=..count..)) +
      \x20\x20geom_bar(stat='count', width=binwidth, position = 'identity') +
      \x20\x20{scale_bw_a} +
      \x20\x20{theme_basic_z}"
    )
  }
  else if (length(vars) == 1 &
           diagram == "color bar graph" &
           any(v == c("N"))) {
    vars <- vars[1]
    q <- glue::glue(
      "binwidth <- (max({substitute(data)}['{substitute(vars)}'], na.rm=TRUE) -
                        min({substitute(data)}['{substitute(vars)}'], na.rm=TRUE))/100"
    )
    p <- glue::glue(
      "
      ggplot({deparse(substitute(data))}, aes(x={as.character(substitute(vars))}, fill=..count..)) +
      \x20\x20geom_bar(stat='count', width=binwidth, position = 'identity') +
      \x20\x20{scale_color_a} +
      \x20\x20{theme_basic_z}"
    )
  }
  else if (length(vars) == 1 &
           diagram == "bar graph" &
           any(v == c("L", "C", "F", "O"))) {
    vars <- vars[1]
    q <- glue::glue(
      "{reorder_observ}"
    )
    p <- glue::glue(
      "
      ggplot({deparse(substitute(data))}, aes(x={as.character(substitute(vars))})) +
      \x20\x20geom_bar(stat='count', width=0.75, fill='black') +
      \x20\x20coord_flip() +
      \x20\x20{theme_basic}"
    )
  }
  else if (length(vars) == 1 &
           diagram == "bw bar graph" &
           any(v == c("L", "C", "F", "O"))) {
    vars <- vars[1]
    q <- glue::glue(
      "{reorder_observ}"
    )
    p <- glue::glue(
      "
      ggplot({deparse(substitute(data))}, aes(x={as.character(substitute(vars))}, fill=..count..)) +
      \x20\x20geom_bar(stat='count', width=0.75) +
      \x20\x20{scale_bw_a} +
      \x20\x20coord_flip() +
      \x20\x20{theme_basic_z}"
    )
  }
  else if (length(vars) == 1 &
           diagram == "color bar graph" &
           any(v == c("L", "C", "F", "O"))) {
    vars <- vars[1]
    q <- glue::glue(
      "{reorder_observ}"
    )
    p <- glue::glue(
      "
      ggplot({deparse(substitute(data))}, aes(x={as.character(substitute(vars))}, fill=..count..)) +
      \x20\x20geom_bar(stat='count', width=0.75) +
      \x20\x20{scale_color_a} +
      \x20\x20coord_flip() +
      \x20\x20{theme_basic_z}"
    )
  }
  else if (length(vars) == 1 &
           diagram == "freq. reordered bar graph" &
           any(v == c("C", "F"))) {
    vars <- vars[1]
    q <- glue::glue(
      "{reorder_freq}"
    )
    p <- glue::glue(
      "
      ggplot({deparse(substitute(data))}, aes(x={as.character(substitute(vars))})) +
      \x20\x20geom_bar(stat='count', width=0.75, fill='black') +
      \x20\x20{scale_bw_a} +
      \x20\x20coord_flip() +
      \x20\x20{theme_basic_z}"
    )
  }
  else if (length(vars) == 1 &
           diagram == "bw freq. reordered bar graph" &
           any(v == c("C", "F"))) {
    vars <- vars[1]
    q <- glue::glue(
      "{reorder_freq}"
    )
    p <- glue::glue(
      "
      ggplot({deparse(substitute(data))}, aes(x={as.character(substitute(vars))}, fill=..count..)) +
      \x20\x20geom_bar(stat='count', width=0.75) +
      \x20\x20{scale_bw_a} +
      \x20\x20coord_flip() +
      \x20\x20{theme_basic_z}"
    )
  }
  else if (length(vars) == 1 &
           diagram == "color freq. reordered bar graph" &
           any(v == c("C", "F"))) {
    vars <- vars[1]
    q <- glue::glue(
      "{reorder_freq}"
    )
    p <- glue::glue(
      "
      ggplot({deparse(substitute(data))}, aes(x={as.character(substitute(vars))}, fill=..count..)) +
      \x20\x20geom_bar(stat='count', width=0.75) +
      \x20\x20{scale_color_a} +
      \x20\x20coord_flip() +
      \x20\x20{theme_basic_z}"
    )
  }
  else if (length(vars) == 1 &
           diagram == "alphab. reordered bar graph" &
           any(v == c("C", "F"))) {
    vars <- vars[1]
    q <- glue::glue(
      "{reorder_alphab}"
    )
    p <- glue::glue(
      "
      ggplot({deparse(substitute(data))}, aes(x={as.character(substitute(vars))})) +
      \x20\x20geom_bar(stat='count', width=0.75, fill='black') +
      \x20\x20coord_flip() +
      \x20\x20{theme_basic_z}"
    )
  }
  else if (length(vars) == 1 &
           diagram == "bw alphab. reordered bar graph" &
           any(v == c("C", "F"))) {
    vars <- vars[1]
    q <- glue::glue(
      "{reorder_alphab}"
    )
    p <- glue::glue(
      "
      ggplot({deparse(substitute(data))}, aes(x={as.character(substitute(vars))}, fill=..count..)) +
      \x20\x20geom_bar(stat='count', width=0.75) +
      \x20\x20{scale_bw_a} +
      \x20\x20coord_flip() +
      \x20\x20{theme_basic_z}"
    )
  }
  else if (length(vars) == 1 &
           diagram == "color alphab. reordered bar graph" &
           any(v == c("C", "F"))) {
    vars <- vars[1]
    q <- glue::glue(
      "{reorder_alphab}"
    )
    p <- glue::glue(
      "
      ggplot({deparse(substitute(data))}, aes(x={as.character(substitute(vars))}, fill=..count..)) +
      \x20\x20geom_bar(stat='count', width=0.75) +
      \x20\x20{scale_color_a} +
      \x20\x20coord_flip() +
      \x20\x20{theme_basic_z}"
    )
  }
  else if (length(vars) == 1 &
           diagram == "linerange graph" &
           any(v == c("L", "C", "F", "O"))) {
    vars <- vars[1]
    q <- glue::glue(
      "{reorder_observ}"
    )
    p <- glue::glue(
      "
      ggplot({deparse(substitute(data))}, aes(y={as.character(substitute(vars))})) +
      \x20\x20geom_path(aes(x=seq_along({substitute(vars)})), size=0.5) +
      \x20\x20geom_point(aes(x=seq_along({substitute(vars)})), size=1) +
      \x20\x20labs(x='seq.') +
      \x20\x20{theme_basic_z}"
    )
  }
  else if (length(vars) == 1 &
           diagram == "freq. reordered linerange graph" &
           any(v == c("C", "F"))) {
    vars <- vars[1]
    q <- glue::glue(
      "{reorder_freq}"
    )
    p <- glue::glue(
      "
      ggplot({deparse(substitute(data))}, aes(y={as.character(substitute(vars))})) +
      \x20\x20geom_path(aes(x=seq_along({substitute(vars)})), size=0.5) +
      \x20\x20geom_point(aes(x=seq_along({substitute(vars)})), size=1) +
      \x20\x20labs(x='seq.') +
      \x20\x20{theme_basic_z}"
    )
  }
  else if (length(vars) == 1 &
           diagram == "alphab. reordered linerange graph" &
           any(v == c("C", "F"))) {
    vars <- vars[1]
    q <- glue::glue(
      "{reorder_alphab}"
    )
    p <- glue::glue(
      "
      ggplot({deparse(substitute(data))}, aes(y={as.character(substitute(vars))})) +
      \x20\x20geom_path(aes(x=seq_along({substitute(vars)})), size=0.5) +
      \x20\x20geom_point(aes(x=seq_along({substitute(vars)})), size=1) +
      \x20\x20labs(x='seq.') +
      \x20\x20{theme_basic_z}"
    )
  }
  else if (length(vars) == 1 &
           diagram == "binned heatmap" &
           any(v == c("L", "C", "F", "O"))) {
    vars <- vars[1]
    q <- glue::glue(
      "{reorder_observ}"
    )
    p <- glue::glue(
      "
      ggplot({deparse(substitute(data))}, aes(y={as.character(substitute(vars))})) +
      \x20\x20geom_bin2d(aes(x=seq_along({substitute(vars)})), fill = 'black') +
      \x20\x20labs(x='seq.') +
      \x20\x20{theme_basic_z}"
    )
  }
  else if (length(vars) == 1 &
           diagram == "bw binned heatmap" &
           any(v == c("L", "C", "F", "O"))) {
    vars <- vars[1]
    q <- glue::glue(
      "{reorder_observ}"
    )
    p <- glue::glue(
      "
      ggplot({deparse(substitute(data))}, aes(y={as.character(substitute(vars))})) +
      \x20\x20geom_bin2d(aes(x=seq_along({substitute(vars)}))) +
      \x20\x20{scale_bw_a} +
      \x20\x20labs(x='seq.') +
      \x20\x20{theme_basic_z}"
    )
  }
  else if (length(vars) == 1 &
           diagram == "binned heatmap" &
           any(v == c("D", "N"))) {
    vars <- vars[1]
    p <- glue::glue(
      "
      ggplot({deparse(substitute(data))}, aes(y={as.character(substitute(vars))})) +
      \x20\x20geom_bin2d(aes(x=seq_along({substitute(vars)})), fill = 'black') +
      \x20\x20labs(x='seq.') +
      \x20\x20{theme_basic_z}"
    )
  }
  else if (length(vars) == 1 &
           diagram == "bw heatmap" &
           any(v == c("D", "N"))) {
    vars <- vars[1]
    p <- glue::glue(
      "
      ggplot({deparse(substitute(data))}, aes(y={as.character(substitute(vars))})) +
      \x20\x20stat_density_2d(aes(x=seq_along({substitute(vars)}), fill = stat(density)), geom = 'raster', contour = FALSE) +
      \x20\x20{scale_bw_a} +
      \x20\x20labs(x='seq.') +
      \x20\x20{theme_basic_z}"
    )
  }
  else if (length(vars) == 1 &
           diagram == "color heatmap" &
           any(v == c("D", "N"))) {
    vars <- vars[1]
    p <- glue::glue(
      "
      ggplot({deparse(substitute(data))}, aes(y={as.character(substitute(vars))})) +
      \x20\x20stat_density_2d(aes(x=seq_along({substitute(vars)}), fill = stat(density)), geom = 'raster', contour = FALSE) +
      \x20\x20{scale_color_a} +
      \x20\x20labs(x='seq.') +
      \x20\x20{theme_basic_z}"
    )
  }
  else if (length(vars) == 1 &
           diagram == "bw binned heatmap" &
           any(v == c("D", "N"))) {
    vars <- vars[1]
    p <- glue::glue(
      "
      ggplot({deparse(substitute(data))}, aes(y={as.character(substitute(vars))})) +
      \x20\x20geom_bin2d(aes(x=seq_along({substitute(vars)}))) +
      \x20\x20{scale_bw_a} +
      \x20\x20labs(x='seq.') +
      \x20\x20{theme_basic_z}"
    )
  }
  else if (length(vars) == 1 &
           diagram == "color binned heatmap" &
           any(v == c("L", "C", "F", "O"))) {
    vars <- vars[1]
    q <- glue::glue(
      "{reorder_observ}"
    )
    p <- glue::glue(
      "
      ggplot({deparse(substitute(data))}, aes(y={as.character(substitute(vars))})) +
      \x20\x20geom_bin2d(aes(x=seq_along({substitute(vars)}))) +
      \x20\x20{scale_color_a} +
      \x20\x20labs(x='seq.') +
      \x20\x20{theme_basic_z}"
    )
  }
  else if (length(vars) == 1 &
           diagram == "color binned heatmap" &
           any(v == c("D", "N"))) {
    vars <- vars[1]
    p <- glue::glue(
      "
      ggplot({deparse(substitute(data))}, aes(y={as.character(substitute(vars))})) +
      \x20\x20geom_bin2d(aes(x=seq_along({substitute(vars)}))) +
      \x20\x20{scale_color_a} +
      \x20\x20labs(x='seq.') +
      \x20\x20{theme_basic_z}"
    )
  }
  else if (length(vars) == 1 &
           diagram == "freq. reordered binned heatmap" &
           any(v == c("C", "F"))) {
    vars <- vars[1]
    q <- glue::glue(
      "{reorder_freq}"
    )
    p <- glue::glue(
      "
      ggplot({deparse(substitute(data))}, aes(y={as.character(substitute(vars))})) +
      \x20\x20geom_bin2d(aes(x=seq_along({substitute(vars)})), fill = 'black') +
      \x20\x20labs(x='seq.') +
      \x20\x20{theme_basic_z}"
    )
  }
  else if (length(vars) == 1 &
           diagram == "bw freq. reordered binned heatmap" &
           any(v == c("C", "F"))) {
    vars <- vars[1]
    q <- glue::glue(
      "{reorder_freq}"
    )
    p <- glue::glue(
      "
      ggplot({deparse(substitute(data))}, aes(y={as.character(substitute(vars))})) +
      \x20\x20geom_bin2d(aes(x=seq_along({substitute(vars)}))) +
      \x20\x20labs(x='seq.') +
      \x20\x20{scale_bw_a} +
      \x20\x20{theme_basic_z}"
    )
  }
  else if (length(vars) == 1 &
           diagram == "color freq. reordered binned heatmap" &
           any(v == c("C", "F"))) {
    vars <- vars[1]
    q <- glue::glue(
      "{reorder_freq}"
    )
    p <- glue::glue(
      "
      ggplot({deparse(substitute(data))}, aes(y={as.character(substitute(vars))})) +
      \x20\x20geom_bin2d(aes(x=seq_along({substitute(vars)}))) +
      \x20\x20labs(x='seq.') +
      \x20\x20{scale_color_a} +
      \x20\x20{theme_basic_z}"
    )
  }
  else if (length(vars) == 1 &
           diagram == "alphab. reordered binned heatmap" &
           any(v == c("C", "F"))) {
    vars <- vars[1]
    q <- glue::glue(
      "{reorder_alphab}"
    )
    p <- glue::glue(
      "
      ggplot({deparse(substitute(data))}, aes(y={as.character(substitute(vars))})) +
      \x20\x20geom_bin2d(aes(x=seq_along({substitute(vars)})), fill = 'black') +
      \x20\x20labs(x='seq.') +
      \x20\x20{theme_basic_z}"
    )
  }
  else if (length(vars) == 1 &
           diagram == "bw alphab. reordered binned heatmap" &
           any(v == c("C", "F"))) {
    vars <- vars[1]
    q <- glue::glue(
      "{reorder_alphab}"
    )
    p <- glue::glue(
      "
      ggplot({deparse(substitute(data))}, aes(y={as.character(substitute(vars))})) +
      \x20\x20geom_bin2d(aes(x=seq_along({substitute(vars)}))) +
      \x20\x20labs(x='seq.') +
      \x20\x20{scale_bw_a} +
      \x20\x20{theme_basic_z}"
    )
  }
  else if (length(vars) == 1 &
           diagram == "color alphab. reordered binned heatmap" &
           any(v == c("C", "F"))) {
    vars <- vars[1]
    q <- glue::glue(
      "{reorder_alphab}"
    )
    p <- glue::glue(
      "
      ggplot({deparse(substitute(data))}, aes(y={as.character(substitute(vars))})) +
      \x20\x20geom_bin2d(aes(x=seq_along({substitute(vars)}))) +
      \x20\x20labs(x='seq.') +
      \x20\x20{scale_color_a} +
      \x20\x20{theme_basic_z}"
    )
  }
  else if (length(vars) == 1 &
           diagram == "stepped line graph" &
           any(v == c("D", "N"))) {
    vars <- vars[1]
    p <- glue::glue(
      "
      ggplot({deparse(substitute(data))}, aes(x=seq_along({as.character(substitute(vars))}), y={as.character(substitute(vars))})) +
      \x20\x20geom_step(direction = 'hv') +
      \x20\x20labs(x='seq') +
      \x20\x20{theme_basic}"
    )
  }
  else if (length(vars) == 1 &
           diagram == "line graph" &
           any(v == c("D", "N"))) {
    vars <- vars[1]
    p <- glue::glue(
      "
      ggplot({deparse(substitute(data))}, aes(x=seq_along({as.character(substitute(vars))}), y={as.character(substitute(vars))})) +
      \x20\x20geom_line() +
      \x20\x20labs(x='seq') +
      \x20\x20{theme_basic}"
    )
  }
  else if (length(vars) == 1 &
           diagram == "area graph" &
           any(v == c("N"))) {
    vars <- vars[1]
    p <- glue::glue(
      "
      ggplot({deparse(substitute(data))}, aes(x=seq_along({as.character(substitute(vars))}), y={as.character(substitute(vars))})) +
      \x20\x20geom_area(fill = 'black') +
      \x20\x20labs(x='seq') +
      \x20\x20{theme_basic}"
    )
  }
  else if (length(vars) == 1 &
           diagram == "stepped area graph" &
           any(v == c("N"))) {
    vars <- vars[1]
    p <- glue::glue(
      "
      ggplot({deparse(substitute(data))}, aes(x=seq_along({as.character(substitute(vars))}), y={as.character(substitute(vars))})) +
      \x20\x20geom_bar(fill = 'black', width = 1, stat = 'identity') +
      \x20\x20labs(x='seq') +
      \x20\x20{theme_basic}"
    )
  }
  else if (length(vars) == 1 &
           diagram == "bw stepped area graph" &
           any(v == c("N"))) {
    vars <- vars[1]
    p <- glue::glue(
      "
      ggplot({deparse(substitute(data))}, aes(x=seq_along({as.character(substitute(vars))}), y={as.character(substitute(vars))}, fill={as.character(substitute(vars))})) +
      \x20\x20geom_bar(width = 1, stat = 'identity') +
      \x20\x20{scale_bw_a} +
      \x20\x20labs(x='seq') +
      \x20\x20{theme_basic_z}"
    )
  }
  else if (length(vars) == 1 &
           diagram == "color stepped area graph" &
           any(v == c("N"))) {
    vars <- vars[1]
    p <- glue::glue(
      "
      ggplot({deparse(substitute(data))}, aes(x=seq_along({as.character(substitute(vars))}), y={as.character(substitute(vars))}, fill={as.character(substitute(vars))})) +
      \x20\x20geom_bar(width = 1, stat = 'identity') +
      \x20\x20{scale_value_a} +
      \x20\x20labs(x='seq') +
      \x20\x20{theme_basic_z}"
    )
  }
  else if (length(vars) == 1 &
           diagram == "histogram" &
           any(v == c("N"))) {
    vars <- vars[1]
    p <- glue::glue(
      "
      ggplot({deparse(substitute(data))}, aes(x={as.character(substitute(vars))})) +
      \x20\x20geom_histogram(bins = 20, fill='black') +
      \x20\x20{theme_basic}"
    )
  }
  else if (length(vars) == 1 &
           diagram == "bw histogram" &
           any(v == c("N"))) {
    vars <- vars[1]
    p <- glue::glue(
      "
      ggplot({deparse(substitute(data))}, aes(x={as.character(substitute(vars))}, fill=..count..)) +
      \x20\x20geom_histogram(bins = 20) +
      \x20\x20{scale_bw_a} +
      \x20\x20{theme_basic_z}"
    )
  }
  else if (length(vars) == 1 &
           diagram == "color histogram" &
           any(v == c("N"))) {
    vars <- vars[1]
    p <- glue::glue(
      "
      ggplot({deparse(substitute(data))}, aes(x={as.character(substitute(vars))}, fill=..count..)) +
      \x20\x20geom_histogram(bins = 20) +
      \x20\x20{scale_color_a} +
      \x20\x20{theme_basic_z}"
    )
  }
  else if (length(vars) == 1 &
           diagram == "freq. polygon" &
           any(v == c("N"))) {
    vars <- vars[1]
    p <- glue::glue(
      "
      ggplot({deparse(substitute(data))}, aes(x={as.character(substitute(vars))})) +
      \x20\x20geom_line(stat = 'bin', bins = 20, center = 0, size = 0.5) +
      \x20\x20{theme_basic}"
    )
  }
  else if (length(vars) == 1 &
           diagram == "density plot" &
           any(v == c("N"))) {
    vars <- vars[1]
    p <- glue::glue(
      "
      ggplot({deparse(substitute(data))},
      \x20\x20aes(x = {as.character(substitute(vars))})) +
      \x20\x20geom_density(size = 0.5) +
      \x20\x20{theme_basic}
      "
    )
  }
  else if (length(vars) == 1 &
           diagram == "filled density plot" &
           any(v == c("N"))) {
    vars <- vars[1]
    p <- glue::glue(
      "
      ggplot({deparse(substitute(data))},
      \x20\x20aes(x = {as.character(substitute(vars))})) +
      \x20\x20geom_density(fill = 'black') +
      \x20\x20{theme_basic}
      "
    )
  }
  else if (length(vars) == 1 &
           diagram == "violin plot" &
           any(v == c("N"))) {
    vars <- vars[1]
    p <- glue::glue(
      "
      ggplot({deparse(substitute(data))},
      \x20\x20aes(x = 0, y = {as.character(substitute(vars))})) +
      \x20\x20geom_violin(size = 0.5) +
      \x20\x20coord_flip() +
      \x20\x20{theme_basic_y}
      "
    )
  }  else if (length(vars) == 1 &
              diagram == "filled violin plot" &
              any(v == c("N"))) {
    vars <- vars[1]
    p <- glue::glue(
      "
      ggplot({deparse(substitute(data))},
      \x20\x20aes(x = 0, y = {as.character(substitute(vars))})) +
      \x20\x20geom_violin(fill = 'black') +
      \x20\x20coord_flip() +
      \x20\x20{theme_basic_y}
      "
    )
  }
  else if (length(vars) == 1 &
           diagram == "bw point graph" &
           any(v == c("N"))) {
    vars <- vars[1]
    p <- glue::glue(
      "
      ggplot({deparse(substitute(data))},
      \x20\x20aes(x = seq_along({as.character(substitute(vars))}), y = {as.character(substitute(vars))})) +
      \x20\x20geom_point(aes(color={as.character(substitute(vars))})) +
      \x20\x20{scale_bw_l} +
      \x20\x20labs(x='seq') +
      \x20\x20{theme_basic_z}"
    )
  }
  else if (length(vars) == 1 &
           diagram == "bw point graph with trend line" &
           any(v == c("N"))) {
    vars <- vars[1]
    p <- glue::glue(
      "
      ggplot({deparse(substitute(data))},
      \x20\x20aes(x = seq_along({as.character(substitute(vars))}), y = {as.character(substitute(vars))})) +
      \x20\x20geom_point(aes(color={as.character(substitute(vars))})) +
      \x20\x20geom_smooth(method = 'loess', size=0.5) +
      \x20\x20{scale_bw_l} +
      \x20\x20labs(x='seq') +
      \x20\x20{theme_basic_z}"
    )
  }
  else if (length(vars) == 1 &
           diagram == "color point graph" &
           any(v == c("N"))) {
    vars <- vars[1]
    p <- glue::glue(
      "
      ggplot({deparse(substitute(data))},
      \x20\x20aes(x = seq_along({as.character(substitute(vars))}), y = {as.character(substitute(vars))})) +
      \x20\x20geom_point(aes(color={as.character(substitute(vars))})) +
      \x20\x20{scale_value_l} +
      \x20\x20labs(x='seq') +
      \x20\x20{theme_basic_z}"
    )
  }
  else if (length(vars) == 1 &
           diagram == "color point graph with trend line" &
           any(v == c("N"))) {
    vars <- vars[1]
    p <- glue::glue(
      "
      ggplot({deparse(substitute(data))},
      \x20\x20aes(x = seq_along({as.character(substitute(vars))}), y = {as.character(substitute(vars))})) +
      \x20\x20geom_point(aes(color={as.character(substitute(vars))})) +
      \x20\x20geom_smooth(method = 'loess', size=0.5) +
      \x20\x20{scale_value_l} +
      \x20\x20labs(x='seq') +
      \x20\x20{theme_basic_z}"
    )
  }
  else if (length(vars) == 1 &
           diagram == "binned point graph" &
           any(v == c("N"))) {
    vars <- vars[1]
    p <- glue::glue(
      "
      ggplot({deparse(substitute(data))},
      \x20\x20aes(x = seq_along({as.character(substitute(vars))}), y = {as.character(substitute(vars))})) +
      \x20\x20geom_point(color='black', stat= 'bin2d') +
      \x20\x20labs(x='seq') +
      \x20\x20{theme_basic_z}"
    )
  }
  else if (length(vars) == 1 &
           diagram == "bw binned point graph" &
           any(v == c("N"))) {
    vars <- vars[1]
    p <- glue::glue(
      "
      ggplot({deparse(substitute(data))},
      \x20\x20aes(x = seq_along({as.character(substitute(vars))}), y = {as.character(substitute(vars))})) +
      \x20\x20geom_point(aes(color=..count..), stat= 'bin2d') +
      \x20\x20{scale_bw_l} +
      \x20\x20labs(x='seq') +
      \x20\x20{theme_basic_z}"
    )
  }
  else if (length(vars) == 1 &
           diagram == "color binned point graph" &
           any(v == c("N"))) {
    vars <- vars[1]
    p <- glue::glue(
      "
      ggplot({deparse(substitute(data))},
      \x20\x20aes(x = seq_along({as.character(substitute(vars))}), y = {as.character(substitute(vars))})) +
      \x20\x20geom_point(aes(color=..count..), stat= 'bin2d') +
      \x20\x20{scale_color_l} +
      \x20\x20labs(x='seq') +
      \x20\x20{theme_basic_z}"
    )
  }
  else if (length(vars) == 1 &
           diagram == "box plot" &
           any(v == c("N"))) {
    vars <- vars[1]
    p <- glue::glue(
      "
      ggplot({deparse(substitute(data))},
      \x20\x20aes(x = 0, y = {as.character(substitute(vars))})) +
      \x20\x20geom_boxplot() +
      \x20\x20coord_flip() +
      \x20\x20{theme_basic_y}"
    )
  }
  else if (length(vars) == 1 &
           diagram == "ecdf plot" &
           any(v == c("N"))) {
    vars <- vars[1]
    p <- glue::glue(
      "
      ggplot({deparse(substitute(data))}, aes(x = {as.character(substitute(vars))})) +
      \x20\x20stat_ecdf(geom = 'line', size=0.1) +
      \x20\x20labs(x = '{as.character(substitute(vars))}', y = 'p') +
      \x20\x20{theme_basic}"
    )
  }
  else if (length(vars) == 1 &
           diagram == "point ecdf plot" &
           any(v == c("N"))) {
    vars <- vars[1]
    p <- glue::glue(
      "
      ggplot({deparse(substitute(data))}, aes(x = {as.character(substitute(vars))})) +
      \x20\x20stat_ecdf(geom = 'point', size=0.1) +
      \x20\x20labs(x = '{as.character(substitute(vars))}', y = 'p') +
      \x20\x20{theme_basic}"
    )
  }
  else if (length(vars) == 1 &
           diagram == "stepped ecdf plot" &
           any(v == c("N"))) {
    vars <- vars[1]
    p <- glue::glue(
      "
      ggplot({deparse(substitute(data))}, aes(x = {as.character(substitute(vars))})) +
      \x20\x20stat_ecdf(geom = 'step', size=0.1) +
      \x20\x20labs(x = '{as.character(substitute(vars))}', y = 'p') +
      \x20\x20{theme_basic}"
    )
  }
  else if (length(vars) == 1 &
           diagram == "stripe graph" &
           any(v == c("N"))) {
    vars <- vars[1]
    p <- glue::glue(
      "
      ggplot({deparse(substitute(data))}, aes(x={as.character(substitute(vars))})) +
      \x20\x20geom_linerange(aes(ymin=0, ymax=1)) +
      \x20\x20{theme_basic_y}"
    )
  }
  else if (length(vars) == 1 &
           diagram == "bw stripe graph" &
           any(v == c("N"))) {
    vars <- vars[1]
    q <- glue::glue(
      "{getdens1}"
    )
    p <- glue::glue(
      "
      ggplot({deparse(substitute(data))}, aes(x={as.character(substitute(vars))}, color=add_density_1D({deparse(substitute(data))}, '{as.character(substitute(vars))}'))) +
      \x20\x20geom_linerange(aes(ymin=0, ymax=1)) +
      \x20\x20{scale_bw_l} +
      \x20\x20{theme_basic_yz}"
    )
  }
  else if (length(vars) == 1 &
           diagram == "color stripe graph" &
           any(v == c("N"))) {
    vars <- vars[1]
    q <- glue::glue(
      "{getdens1}"
    )
    p <- glue::glue(
      "
      ggplot({deparse(substitute(data))}, aes(x={as.character(substitute(vars))}, color=add_density_1D({deparse(substitute(data))}, '{as.character(substitute(vars))}'))) +
      \x20\x20geom_linerange(aes(ymin=0, ymax=1)) +
      \x20\x20{scale_color_l} +
      \x20\x20{theme_basic_yz}"
    )
  }
  else if (length(vars) == 1 &
           diagram == "binned stripe graph" &
           any(v == c("N"))) {
    vars <- vars[1]
    q <- glue::glue(
      "binwidth <- (max({substitute(data)}['{substitute(vars)}'], na.rm=TRUE) -
                        min({substitute(data)}['{substitute(vars)}'], na.rm=TRUE))/20"
    )
    p <- glue::glue(
      "
      ggplot({deparse(substitute(data))}, aes(x={as.character(substitute(vars))})) +
      \x20\x20geom_bin2d(aes(y=1), binwidth = binwidth, fill='black') +
      \x20\x20{theme_basic_y}"
    )
  }
  else if (length(vars) == 1 &
           diagram == "bw binned stripe graph" &
           any(v == c("N"))) {
    vars <- vars[1]
    q <- glue::glue(
      "binwidth <- (max({substitute(data)}['{substitute(vars)}'], na.rm=TRUE) -
                        min({substitute(data)}['{substitute(vars)}'], na.rm=TRUE))/20"
    )
    p <- glue::glue(
      "
      ggplot({deparse(substitute(data))}, aes(x={as.character(substitute(vars))})) +
      \x20\x20geom_bin2d(aes(y=1), binwidth = binwidth) +
      \x20\x20{scale_bw_a} +
      \x20\x20{theme_basic_yz}"
    )
  }
  else if (length(vars) == 1 &
           diagram == "color binned stripe graph" &
           any(v == c("N"))) {
    vars <- vars[1]
    q <- glue::glue(
      "binwidth <- (max({substitute(data)}['{substitute(vars)}'], na.rm=TRUE) -
                        min({substitute(data)}['{substitute(vars)}'], na.rm=TRUE))/20"
    )
    p <- glue::glue(
      "
      ggplot({deparse(substitute(data))}, aes(x={as.character(substitute(vars))})) +
      \x20\x20geom_bin2d(aes(y=1), binwidth = binwidth) +
      \x20\x20{scale_color_a} +
      \x20\x20{theme_basic_yz}"
    )
  }
  else if (length(vars) == 1 &
           diagram == "seq. stripe graph" &
           any(v == c("N"))) {
    vars <- vars[1]
    p <- glue::glue(
      "
      ggplot({deparse(substitute(data))}, aes(x=seq_along({as.character(substitute(vars))}))) +
      \x20\x20geom_tile(aes(y=1), fill = 'black') +
      \x20\x20labs(y = '', x = 'seq', subtitle = '{as.character(substitute(vars))}') +
      \x20\x20{theme_basic_y}"
    )
  }
  else if (length(vars) == 1 &
           diagram == "bw seq. stripe graph" &
           any(v == c("N"))) {
    vars <- vars[1]
    q <- glue::glue(
      "{getdens1}"
    )
    p <- glue::glue(
      "
      ggplot({deparse(substitute(data))}, aes(x=seq_along({as.character(substitute(vars))}))) +
      \x20\x20geom_tile(aes(y=1, fill={as.character(substitute(vars))})) +
      \x20\x20{scale_bw_a} +
      \x20\x20labs(y = '', x = 'seq', subtitle = '{as.character(substitute(vars))}') +
      \x20\x20{theme_basic_yz}"
    )
  }
  else if (length(vars) == 1 &
           diagram == "color seq. stripe graph" &
           any(v == c("N"))) {
    vars <- vars[1]
    q <- glue::glue(
      "{getdens1}"
    )
    p <- glue::glue(
      "
      ggplot({deparse(substitute(data))}, aes(x=seq_along({as.character(substitute(vars))}))) +
      \x20\x20geom_tile(aes(y=1, fill={as.character(substitute(vars))})) +
      \x20\x20{scale_value_a} +
      \x20\x20labs(y = '', x = 'seq', subtitle = '{as.character(substitute(vars))}') +
      \x20\x20{theme_basic_yz}"
    )
  }

# 2var diagrams -----------------------------------------------------------


  else if (length(vars) == 2 &
           diagram == "scatter plot" &
           any(v[1] == c("N", "D")) &
           any(v[2] == c("N", "D"))) {
    vars1 <- vars[1]
    vars2 <- vars[2]
    p <- glue::glue(
      "
      ggplot({deparse(substitute(data))}, aes(x={as.character(substitute(vars1))}, y={as.character(substitute(vars2))})) +
      \x20\x20geom_point() +
      \x20\x20{theme_basic}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "scatter plot with trend line" &
           any(v[1] == c("N", "D")) &
           any(v[2] == c("N", "D"))) {
    vars1 <- vars[1]
    vars2 <- vars[2]
    p <- glue::glue(
      "
      ggplot({deparse(substitute(data))}, aes(x={as.character(substitute(vars1))}, y={as.character(substitute(vars2))})) +
      \x20\x20geom_point() +
      \x20\x20geom_smooth() +
      \x20\x20{theme_basic}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "bw scatter plot" &
           identical(v, c("N", "N"))) {
    vars1 <- vars[1]
    vars2 <- vars[2]
    q <- glue::glue(
      "{getdens2}"
    )
    p <- glue::glue(
      "
      ggplot({deparse(substitute(data))}, aes(x={as.character(substitute(vars1))}, y={as.character(substitute(vars2))}, color=get_density({as.character(substitute(vars1))}, {as.character(substitute(vars2))}))) +
      \x20\x20geom_point() +
      \x20\x20{scale_bw_l} +
      \x20\x20{theme_basic_z}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "color scatter plot" &
           identical(v, c("N", "N"))) {
    vars1 <- vars[1]
    vars2 <- vars[2]
    q <- glue::glue(
      "{getdens2}"
    )
    p <- glue::glue(
      "
      ggplot({deparse(substitute(data))}, aes(x={as.character(substitute(vars1))}, y={as.character(substitute(vars2))}, color=get_density({as.character(substitute(vars1))}, {as.character(substitute(vars2))}))) +
      \x20\x20geom_point() +
      \x20\x20{scale_color_l} +
      \x20\x20{theme_basic_z}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "binned scatter plot" &
           any(v[1] == c("N", "D")) &
           any(v[2] == c("N", "D"))) {
    vars1 <- vars[1]
    vars2 <- vars[2]
    p <- glue::glue(
      "
      ggplot({deparse(substitute(data))}, aes(x={as.character(substitute(vars1))}, y={as.character(substitute(vars2))})) +
      \x20\x20geom_point(stat= 'bin2d') +
      \x20\x20{theme_basic_z}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "bw binned scatter plot" &
           any(v[1] == c("N", "D")) &
           any(v[2] == c("N", "D"))) {
    vars1 <- vars[1]
    vars2 <- vars[2]
    p <- glue::glue(
      "
      ggplot({deparse(substitute(data))}, aes(x={as.character(substitute(vars1))}, y={as.character(substitute(vars2))}, color=..count..)) +
      \x20\x20geom_point(stat= 'bin2d') +
      \x20\x20{scale_bw_l} +
      \x20\x20{theme_basic_z}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "color binned scatter plot" &
           any(v[1] == c("N", "D")) &
           any(v[2] == c("N", "D"))) {
    vars1 <- vars[1]
    vars2 <- vars[2]
    p <- glue::glue(
      "
      ggplot({deparse(substitute(data))}, aes(x={as.character(substitute(vars1))}, y={as.character(substitute(vars2))}, color=..count..)) +
      \x20\x20geom_point(stat= 'bin2d') +
      \x20\x20{scale_color_l} +
      \x20\x20{theme_basic_z}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "binned heatmap" &
           any(v[1] == c("N", "D")) &
           any(v[2] == c("N", "D"))) {
    vars1 <- vars[1]
    vars2 <- vars[2]
    p <- glue::glue(
      "
      ggplot({deparse(substitute(data))}, aes(x={as.character(substitute(vars1))}, y={as.character(substitute(vars2))})) +
      \x20\x20geom_bin2d(fill = 'black') +
      \x20\x20{theme_basic}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "bw binned heatmap" &
           any(v[1] == c("N", "D")) &
           any(v[2] == c("N", "D"))) {
    vars1 <- vars[1]
    vars2 <- vars[2]
    p <- glue::glue(
      "
      ggplot({deparse(substitute(data))}, aes(x={as.character(substitute(vars1))}, y={as.character(substitute(vars2))})) +
      \x20\x20geom_bin2d() +
      \x20\x20{scale_bw_a} +
      \x20\x20{theme_basic_z}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "color binned heatmap" &
           any(v[1] == c("N", "D")) &
           any(v[2] == c("N", "D"))) {
    vars1 <- vars[1]
    vars2 <- vars[2]
    p <- glue::glue(
      "
      ggplot({deparse(substitute(data))}, aes(x={as.character(substitute(vars1))}, y={as.character(substitute(vars2))})) +
      \x20\x20geom_bin2d() +
      \x20\x20{scale_color_a} +
      \x20\x20{theme_basic_z}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "hexagonal binned heatmap" &
           identical(v, c("N", "N"))) {
    vars1 <- vars[1]
    vars2 <- vars[2]
    p <- glue::glue(
      "
      ggplot({deparse(substitute(data))}, aes(x={as.character(substitute(vars1))}, y={as.character(substitute(vars2))})) +
      \x20\x20stat_binhex(fill = 'black') +
      \x20\x20{theme_basic}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "bw hexagonal binned heatmap" &
           identical(v, c("N", "N"))) {
    vars1 <- vars[1]
    vars2 <- vars[2]
    p <- glue::glue(
      "
      ggplot({deparse(substitute(data))}, aes(x={as.character(substitute(vars1))}, y={as.character(substitute(vars2))})) +
      \x20\x20stat_binhex(aes(fill=..count..,  color=..count..)) +
      \x20\x20{scale_bw_l} +
      \x20\x20{scale_bw_a} +
      \x20\x20{theme_basic_z}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "color hexagonal binned heatmap" &
           identical(v, c("N", "N"))) {
    vars1 <- vars[1]
    vars2 <- vars[2]
    p <- glue::glue(
      "
      ggplot({deparse(substitute(data))}, aes(x={as.character(substitute(vars1))}, y={as.character(substitute(vars2))})) +
      \x20\x20stat_binhex(aes(fill=..count..,  color=..count..)) +
      \x20\x20{scale_color_l} +
      \x20\x20{scale_color_a} +
      \x20\x20{theme_basic_z}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "bw heatmap" &
           any(v[1] == c("N", "D")) &
           any(v[2] == c("N", "D"))) {
    vars1 <- vars[1]
    vars2 <- vars[2]
    p <- glue::glue(
      "
      ggplot({deparse(substitute(data))}, aes(x={as.character(substitute(vars1))}, y={as.character(substitute(vars2))})) +
      \x20\x20stat_density_2d(aes(fill=stat(density)), geom = 'raster', contour = FALSE) +
      \x20\x20{scale_bw_a} +
      \x20\x20{theme_basic_z}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "color heatmap" &
           any(v[1] == c("N", "D")) &
           any(v[2] == c("N", "D"))) {
    vars1 <- vars[1]
    vars2 <- vars[2]
    p <- glue::glue(
      "
      ggplot({deparse(substitute(data))}, aes(x={as.character(substitute(vars1))}, y={as.character(substitute(vars2))})) +
      \x20\x20stat_density_2d(aes(fill=stat(density)), geom = 'raster', contour = FALSE) +
      \x20\x20{scale_color_a} +
      \x20\x20{theme_basic_z}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "contour plot" &
           any(v[1] == c("N", "D")) &
           any(v[2] == c("N", "D"))) {
    vars1 <- vars[1]
    vars2 <- vars[2]
    p <- glue::glue(
      "
      ggplot({deparse(substitute(data))}, aes(x={as.character(substitute(vars1))}, y={as.character(substitute(vars2))})) +
      \x20\x20stat_density_2d(color = 'black', size=0.2) +
      \x20\x20{theme_basic}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "bw contour plot" &
           any(v[1] == c("N", "D")) &
           any(v[2] == c("N", "D"))) {
    vars1 <- vars[1]
    vars2 <- vars[2]
    p <- glue::glue(
      "
      ggplot({deparse(substitute(data))}, aes(x={as.character(substitute(vars1))}, y={as.character(substitute(vars2))})) +
      \x20\x20stat_density_2d(aes(color=..level..), size=0.2) +
      \x20\x20{scale_bw_l} +
      \x20\x20{theme_basic_z}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "color contour plot" &
           any(v[1] == c("N", "D")) &
           any(v[2] == c("N", "D"))) {
    vars1 <- vars[1]
    vars2 <- vars[2]
    p <- glue::glue(
      "
      ggplot({deparse(substitute(data))}, aes(x={as.character(substitute(vars1))}, y={as.character(substitute(vars2))})) +
      \x20\x20stat_density_2d(aes(color=..level..), size=0.2) +
      \x20\x20{scale_color_l} +
      \x20\x20{theme_basic_z}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "contour plot with data points" &
           any(v[1] == c("N", "D")) &
           any(v[2] == c("N", "D"))) {
    vars1 <- vars[1]
    vars2 <- vars[2]
    p <- glue::glue(
      "
      ggplot({deparse(substitute(data))}, aes(x={as.character(substitute(vars1))}, y={as.character(substitute(vars2))})) +
      \x20\x20stat_density_2d(color = 'black', size=0.2) +
      \x20\x20geom_point() +
      \x20\x20{theme_basic}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "bw contour plot with data points" &
           identical(v, c("N", "N"))) {
    vars1 <- vars[1]
    vars2 <- vars[2]
    q <- glue::glue(
      "{getdens2}"
    )
    p <- glue::glue(
      "
      ggplot({deparse(substitute(data))}, aes(x={as.character(substitute(vars1))}, y={as.character(substitute(vars2))})) +
      \x20\x20stat_density_2d(aes(color=..level..), size=0.2) +
      \x20\x20geom_point(aes(color=get_density({as.character(substitute(vars1))}, {as.character(substitute(vars2))}))) +
      \x20\x20{scale_bw_l} +
      \x20\x20{theme_basic_z}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "color contour plot with data points" &
           identical(v, c("N", "N"))) {
    vars1 <- vars[1]
    vars2 <- vars[2]
    q <- glue::glue(
      "{getdens2}"
    )
    p <- glue::glue(
      "
      ggplot({deparse(substitute(data))}, aes(x={as.character(substitute(vars1))}, y={as.character(substitute(vars2))})) +
      \x20\x20stat_density_2d(aes(color=..level..), size=0.2) +
      \x20\x20geom_point(aes(color=get_density({as.character(substitute(vars1))}, {as.character(substitute(vars2))}))) +
      \x20\x20{scale_color_l} +
      \x20\x20{theme_basic_z}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "parallel plot" &
           identical(v, c("N", "N"))) {
    vars1 <- vars[1]
    vars2 <- vars[2]
    p <- glue::glue(
      "
      GGally::ggparcoord({deparse(substitute(data))}, columns = 1:2, scale = 'uniminmax', alphaLines = 0.2, order = 'skewness') +
      \x20\x20scale_x_discrete(expand = c(.1, 0)) +
      \x20\x20{theme_basic}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "bw parallel plot" &
           identical(v, c("N", "N"))) {
    vars1 <- vars[1]
    vars2 <- vars[2]
    q <- glue::glue(
      "{getdens2}"
    )
    p <- glue::glue(
      "
      GGally::ggparcoord(data.frame('{as.character(substitute(vars1))}' = {deparse(substitute(data))}${as.character(substitute(vars1))},
      \x20\x20'{as.character(substitute(vars2))}' = {deparse(substitute(data))}${as.character(substitute(vars2))},
      \x20\x20'dens' = get_density({deparse(substitute(data))}${as.character(substitute(vars1))},
      \x20\x20{deparse(substitute(data))}${as.character(substitute(vars2))})), columns = 1:2,
      \x20\x20scale = 'uniminmax', alphaLines = 0.5, mapping=aes(color=dens), order = 'skewness') +
      \x20\x20{scale_bw_l} +
      \x20\x20scale_x_discrete(expand = c(.1, 0)) +
      \x20\x20{theme_basic_z}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "color parallel plot" &
           identical(v, c("N", "N"))) {
    vars1 <- vars[1]
    vars2 <- vars[2]
    q <- glue::glue(
      "{getdens2}"
    )
    p <- glue::glue(
      "
      GGally::ggparcoord(data.frame('{as.character(substitute(vars1))}' = {deparse(substitute(data))}${as.character(substitute(vars1))},
      \x20\x20'{as.character(substitute(vars2))}' = {deparse(substitute(data))}${as.character(substitute(vars2))},
      \x20\x20'dens' = get_density({deparse(substitute(data))}${as.character(substitute(vars1))},
      \x20\x20{deparse(substitute(data))}${as.character(substitute(vars2))})), columns = 1:2,
      \x20\x20scale = 'uniminmax', alphaLines = 0.5, mapping=aes(color=dens), order = 'skewness') +
      \x20\x20{scale_color_l} +
      \x20\x20scale_x_discrete(expand = c(.1, 0)) +
      \x20\x20{theme_basic_z}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "unscaled parallel plot" &
           identical(v, c("N", "N"))) {
    vars1 <- vars[1]
    vars2 <- vars[2]
    p <- glue::glue(
      "
      GGally::ggparcoord({deparse(substitute(data))}, columns = 1:2, scale = 'globalminmax', alphaLines = 0.2, order = 'skewness') +
      \x20\x20scale_x_discrete(expand = c(.1, 0)) +
      \x20\x20{theme_basic}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "unscaled bw parallel plot" &
           identical(v, c("N", "N"))) {
    vars1 <- vars[1]
    vars2 <- vars[2]
    q <- glue::glue(
      "{getdens2}"
    )
    p <- glue::glue(
      "
      GGally::ggparcoord(data.frame('{as.character(substitute(vars1))}' = {deparse(substitute(data))}${as.character(substitute(vars1))},
      \x20\x20'{as.character(substitute(vars2))}' = {deparse(substitute(data))}${as.character(substitute(vars2))},
      \x20\x20'dens' = get_density({deparse(substitute(data))}${as.character(substitute(vars1))},
      \x20\x20{deparse(substitute(data))}${as.character(substitute(vars2))})), columns = 1:2,
      \x20\x20scale = 'globalminmax', alphaLines = 0.5, mapping=aes(color=dens), order = 'skewness') +
      \x20\x20{scale_bw_l} +
      \x20\x20scale_x_discrete(expand = c(.1, 0)) +
      \x20\x20{theme_basic_z}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "unscaled color parallel plot" &
           identical(v, c("N", "N"))) {
    vars1 <- vars[1]
    vars2 <- vars[2]
    q <- glue::glue(
      "{getdens2}"
    )
    p <- glue::glue(
      "
      GGally::ggparcoord(data.frame('{as.character(substitute(vars1))}' = {deparse(substitute(data))}${as.character(substitute(vars1))},
      \x20\x20'{as.character(substitute(vars2))}' = {deparse(substitute(data))}${as.character(substitute(vars2))},
      \x20\x20'dens' = get_density({deparse(substitute(data))}${as.character(substitute(vars1))},
      \x20\x20{deparse(substitute(data))}${as.character(substitute(vars2))})), columns = 1:2,
      \x20\x20scale = 'globalminmax', alphaLines = 0.5, mapping=aes(color=dens), order = 'skewness') +
      \x20\x20{scale_color_l} +
      \x20\x20scale_x_discrete(expand = c(.1, 0)) +
      \x20\x20{theme_basic_z}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "path graph" &
           any(v[1] == c("N", "D")) &
           any(v[2] == c("N", "D"))) {
    vars1 <- vars[1]
    vars2 <- vars[2]
    p <- glue::glue(
      "
      ggplot({deparse(substitute(data))}, aes(x={as.character(substitute(vars1))}, y={as.character(substitute(vars2))})) +
      \x20\x20geom_path() +
      \x20\x20geom_point(data = {deparse(substitute(data))}[1,], aes(x={as.character(substitute(vars1))}, y={as.character(substitute(vars2))}), size=1.5, shape = 1) +
      \x20\x20geom_point(data = {deparse(substitute(data))}[nrow({deparse(substitute(data))}),], aes(x={as.character(substitute(vars1))}, y={as.character(substitute(vars2))}), size = 1.5, shape = 19) +
      \x20\x20{theme_basic}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "bw path graph" &
           any(v[1] == c("N", "D")) &
           any(v[2] == c("N", "D"))) {
    vars1 <- vars[1]
    vars2 <- vars[2]
    p <- glue::glue(
      "
      ggplot({deparse(substitute(data))}, aes(x={as.character(substitute(vars1))}, y={as.character(substitute(vars2))}, color=seq_along({as.character(substitute(vars1))}))) +
      \x20\x20geom_path() +
      \x20\x20geom_point(data = {deparse(substitute(data))}[1,], aes(x={as.character(substitute(vars1))}, y={as.character(substitute(vars2))}), color = 'black', size = 1.5, shape = 1) +
      \x20\x20geom_point(data = {deparse(substitute(data))}[nrow({deparse(substitute(data))}),], aes(x={as.character(substitute(vars1))}, y={as.character(substitute(vars2))}), color = 'black', size = 1.5, shape = 19) +
      \x20\x20{scale_bw_l} +
      \x20\x20{theme_basic_z}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "color path graph" &
           any(v[1] == c("N", "D")) &
           any(v[2] == c("N", "D"))) {
    vars1 <- vars[1]
    vars2 <- vars[2]
    p <- glue::glue(
      "
      ggplot({deparse(substitute(data))}, aes(x={as.character(substitute(vars1))}, y={as.character(substitute(vars2))}, color=seq_along({as.character(substitute(vars1))}))) +
      \x20\x20geom_path() +
      \x20\x20geom_point(data = {deparse(substitute(data))}[1,], aes(x={as.character(substitute(vars1))}, y={as.character(substitute(vars2))}), color = 'black', size=1.5, shape = 1) +
      \x20\x20geom_point(data = {deparse(substitute(data))}[nrow({deparse(substitute(data))}),], aes(x={as.character(substitute(vars1))}, y={as.character(substitute(vars2))}), color = 'black', size=1.5, shape = 19) +
      \x20\x20{scale_seq_l} +
      \x20\x20{theme_basic_z}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "point-to-point graph" &
           any(v[1] == c("N", "D")) &
           any(v[2] == c("N", "D"))) {
    vars1 <- vars[1]
    vars2 <- vars[2]
    p <- glue::glue(
      "
      ggplot({deparse(substitute(data))}, aes(x={as.character(substitute(vars1))}, y={as.character(substitute(vars2))})) +
      \x20\x20geom_path() +
      \x20\x20geom_point() +
      \x20\x20{theme_basic}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "bw point-to-point graph" &
           any(v[1] == c("N", "D")) &
           any(v[2] == c("N", "D"))) {
    vars1 <- vars[1]
    vars2 <- vars[2]
    p <- glue::glue(
      "
      ggplot({deparse(substitute(data))}, aes(x={as.character(substitute(vars1))}, y={as.character(substitute(vars2))}, color=seq_along({as.character(substitute(vars1))}))) +
      \x20\x20geom_path() +
      \x20\x20geom_point() +
      \x20\x20{scale_bw_l} +
      \x20\x20{theme_basic_z}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "color point-to-point graph" &
           any(v[1] == c("N", "D")) &
           any(v[2] == c("N", "D"))) {
    vars1 <- vars[1]
    vars2 <- vars[2]
    p <- glue::glue(
      "
      ggplot({deparse(substitute(data))}, aes(x={as.character(substitute(vars1))}, y={as.character(substitute(vars2))}, color=seq_along({as.character(substitute(vars1))}))) +
      \x20\x20geom_path() +
      \x20\x20geom_point() +
      \x20\x20{scale_seq_l} +
      \x20\x20{theme_basic_z}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "point graph" &
           identical(v, c("N", "N"))) {
    vars1 <- vars[1]
    vars2 <- vars[2]
    q <- glue::glue(
      paste0(unfold)
    )
    p <- glue::glue(
      "
      ggplot(foo_df, aes_string(x='foo_id')) +
      geom_point(aes_string(y='measure'), size=1) +
      labs(y='', x='seq') +
      facet_grid(variable~., switch = 'both') +
      {theme_basic}
      "
      )
  }
  else if (length(vars) == 2 &
           diagram == "bw point graph" &
           identical(v, c("N", "N"))) {
    vars1 <- vars[1]
    vars2 <- vars[2]
    q <- glue::glue(
      paste0(unfold)
    )
    p <- glue::glue(
      "
      ggplot(foo_df, aes_string(x='foo_id')) +
      geom_point(aes_string(y='measure', color = 'measure'), size=1) +
      labs(y='', x='seq') +
      {scale_bw_l} +
      facet_grid(variable~., switch = 'both') +
      {theme_basic_z}
      "
    )
  }
  else if (length(vars) == 2 &
           diagram == "color point graph" &
           identical(v, c("N", "N"))) {
    vars1 <- vars[1]
    vars2 <- vars[2]
    q <- glue::glue(
      paste0(unfold)
    )
    p <- glue::glue(
      "
      ggplot(foo_df, aes_string(x = 'foo_id')) +
      geom_point(aes_string(y = 'measure', color = 'measure'), size = 1) +
      labs(y = '', x = 'seq') +
      {scale_value_l} +
      facet_grid(variable ~ ., switch = 'both') +
      {theme_basic_z}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "line graph" &
           identical(v, c("N", "N"))) {
    vars1 <- vars[1]
    vars2 <- vars[2]
    q <- glue::glue(
      paste0(unfold)
    )
    p <- glue::glue(
      "
      ggplot(foo_df, aes_string(x = 'foo_id', y = 'measure')) +
      geom_line(color = 'black') +
      labs(y = '', x = 'seq') +
      facet_grid(variable ~ ., switch = 'both') +
      {theme_basic}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "stepped line graph" &
           identical(v, c("N", "N"))) {
    vars1 <- vars[1]
    vars2 <- vars[2]
    q <- glue::glue(
      paste0(unfold)
    )
    p <- glue::glue(
      "
      ggplot(foo_df, aes_string(x = 'foo_id', y = 'measure')) +
      geom_step(color = 'black') +
      labs(y = '', x = 'seq') +
      facet_grid(variable ~ ., switch = 'both') +
      {theme_basic}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "area graph" &
           identical(v, c("N", "N"))) {
    vars1 <- vars[1]
    vars2 <- vars[2]
    q <- glue::glue(
      paste0(unfold)
    )
    p <- glue::glue(
      "
      ggplot(foo_df, aes_string(x = 'foo_id', y = 'measure')) +
      geom_area(fill = 'black') +
      labs(y = '', x = 'seq') +
      facet_grid(variable ~ ., switch = 'both') +
      {theme_basic}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "stepped area graph" &
           identical(v, c("N", "N"))) {
    vars1 <- vars[1]
    vars2 <- vars[2]
    q <- glue::glue(
      paste0(unfold)
    )
    p <- glue::glue(
      "
      ggplot(foo_df, aes_string(x = 'foo_id')) +
      geom_bar(aes_string(y='measure'), fill = 'black', width = 1, stat = 'identity') +
      labs(y = '', x = 'seq') +
      facet_grid(variable ~ ., switch = 'both') +
      {theme_basic}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "bw stepped area graph" &
           identical(v, c("N", "N"))) {
    vars1 <- vars[1]
    vars2 <- vars[2]
    q <- glue::glue(
      paste0(unfold)
    )
    p <- glue::glue(
      "
      ggplot(foo_df, aes_string(x = 'foo_id')) +
      geom_bar(aes_string(y='measure', fill = 'measure'), width = 1, stat = 'identity') +
      {scale_bw_a} +
      labs(y = '', x = 'seq') +
      facet_grid(variable ~ ., switch = 'both') +
      {theme_basic_z}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "color stepped area graph" &
           identical(v, c("N", "N"))) {
    vars1 <- vars[1]
    vars2 <- vars[2]
    q <- glue::glue(
      paste0(unfold)
    )
    p <- glue::glue(
      "
      ggplot(foo_df, aes_string(x = 'foo_id')) +
      geom_bar(aes_string(y='measure', fill = 'measure'), width = 1, stat = 'identity') +
      {scale_value_a} +
      labs(y = '', x = 'seq') +
      facet_grid(variable ~ ., switch = 'both') +
      {theme_basic_z}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "bw seq. heatmap" &
           any(v[1] == c("N", "D")) &
           any(v[2] == c("N", "D"))) {
    vars1 <- vars[1]
    vars2 <- vars[2]
    q <- glue::glue(
      paste0(unfold)
    )
    p <- glue::glue(
      "
      ggplot(foo_df, aes_string(x = 'foo_id', y='measure')) +
      stat_density_2d(aes(fill = stat(density)), geom = 'raster', contour = FALSE) +
      {scale_bw_a} +
      labs(y = '', x = 'seq') +
      facet_grid(variable ~ ., switch = 'both') +
      {theme_basic_z}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "color seq. heatmap" &
           any(v[1] == c("N", "D")) &
           any(v[2] == c("N", "D"))) {
    vars1 <- vars[1]
    vars2 <- vars[2]
    q <- glue::glue(
      paste0(unfold)
    )
    p <- glue::glue(
      "
      ggplot(foo_df, aes_string(x = 'foo_id', y='measure')) +
      stat_density_2d(aes(fill = stat(density)), geom = 'raster', contour = FALSE) +
      {scale_color_a} +
      labs(y = '', x = 'seq') +
      facet_grid(variable ~ ., switch = 'both') +
      {theme_basic_z}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "bw seq. stripe graph" &
           identical(v, c("N", "N"))) {
    vars1 <- vars[1]
    vars2 <- vars[2]
    q <- glue::glue(
      paste0(unfold)
    )
    p <- glue::glue(
      "
      ggplot(foo_df, aes_string(x = 'foo_id', y = 1)) +
      geom_tile(aes_string(fill = 'measure')) +
      {scale_bw_a} +
      labs(y = '', x = 'seq') +
      facet_grid(variable ~ ., switch = 'both') +
      {theme_basic_z}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "color seq. stripe graph" &
           identical(v, c("N", "N"))) {
    vars1 <- vars[1]
    vars2 <- vars[2]
    q <- glue::glue(
      paste0(unfold)
    )
    p <- glue::glue(
      "
      ggplot(foo_df, aes_string(x = 'foo_id', y = 1)) +
      geom_tile(aes_string(fill = 'measure')) +
      {scale_value_a} +
      labs(y = '', x = 'seq') +
      facet_grid(variable ~ ., switch = 'both') +
      {theme_basic_z}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "histogram" &
           identical(v, c("N", "N"))) {
    vars1 <- vars[1]
    vars2 <- vars[2]
    q <- glue::glue(
      paste0(unfold)
    )
    p <- glue::glue(
      "
      ggplot(foo_df, aes_string(x = 'measure')) +
      geom_histogram(fill = 'black') +
      labs(x = '') +
      facet_grid(variable ~ ., switch = 'both') +
      {theme_basic}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "bw histogram" &
           identical(v, c("N", "N"))) {
    vars1 <- vars[1]
    vars2 <- vars[2]
    q <- glue::glue(
      paste0(unfold)
    )
    p <- glue::glue(
      "
      ggplot(foo_df, aes_string(x = 'measure')) +
      geom_histogram(aes_(fill=~..count..)) +
      {scale_bw_a} +
      labs(x = '') +
      facet_grid(variable ~ ., switch = 'both') +
      {theme_basic_z}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "color histogram" &
           identical(v, c("N", "N"))) {
    vars1 <- vars[1]
    vars2 <- vars[2]
    q <- glue::glue(
      paste0(unfold)
    )
    p <- glue::glue(
      "
      ggplot(foo_df, aes_string(x = 'measure')) +
      geom_histogram(aes_(fill=~..count..)) +
      {scale_color_a} +
      labs(x = '') +
      facet_grid(variable ~ ., switch = 'both') +
      {theme_basic_z}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "freq. polygon" &
           identical(v, c("N", "N"))) {
    vars1 <- vars[1]
    vars2 <- vars[2]
    q <- glue::glue(
      paste0(unfold)
    )
    p <- glue::glue(
      "
      ggplot(foo_df, aes_string(x = 'measure')) +
      geom_line(stat = 'bin', bins = 20, center = 0, color = 'black', size = 0.1) +
      labs(x = '') +
      facet_grid(variable ~ ., switch = 'both') +
      {theme_basic}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "density plot" &
           identical(v, c("N", "N"))) {
    vars1 <- vars[1]
    vars2 <- vars[2]
    q <- glue::glue(
      paste0(unfold)
    )
    p <- glue::glue(
      "
      ggplot(foo_df, aes_string(x = 'measure')) +
      geom_density(size = 0.1) +
      labs(x = '') +
      facet_grid(variable ~ ., switch = 'both') +
      {theme_basic}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "filled density plot" &
           identical(v, c("N", "N"))) {
    vars1 <- vars[1]
    vars2 <- vars[2]
    q <- glue::glue(
      paste0(unfold)
    )
    p <- glue::glue(
      "
      ggplot(foo_df, aes_string(x = 'measure')) +
      geom_density(fill = 'black', size = 0.1) +
      labs(x = '') +
      facet_grid(variable ~ ., switch = 'both') +
      {theme_basic}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "violin plot" &
           identical(v, c("N", "N"))) {
    vars1 <- vars[1]
    vars2 <- vars[2]
    q <- glue::glue(
      paste0(unfold)
    )
    p <- glue::glue(
      "
      ggplot(foo_df, aes_string(x = 0, y = 'measure')) +
      geom_violin(size = 0.1) +
      labs(x = '') +
      coord_flip() +
      facet_grid(variable ~ ., switch = 'both') +
      {theme_basic}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "filled violin plot" &
           identical(v, c("N", "N"))) {
    vars1 <- vars[1]
    vars2 <- vars[2]
    q <- glue::glue(
      paste0(unfold)
    )
    p <- glue::glue(
      "
      ggplot(foo_df, aes_string(x = 0, y = 'measure')) +
      geom_violin(fill = 'black', size = 0.1) +
      labs(x = '') +
      coord_flip() +
      facet_grid(variable ~ ., switch = 'both') +
      {theme_basic}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "box plot" &
           identical(v, c("N", "N"))) {
    vars1 <- vars[1]
    vars2 <- vars[2]
    q <- glue::glue(
      paste0(unfold)
    )
    p <- glue::glue(
      "
      ggplot(foo_df) +
      geom_boxplot(aes_string(x=0, y='measure'), size = 0.1) +
      labs(x = '') +
      coord_flip() +
      facet_grid(variable ~ ., switch = 'both') +
      {theme_basic_y}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "filled violin plot" &
           identical(v, c("N", "N"))) {
    vars1 <- vars[1]
    vars2 <- vars[2]
    q <- glue::glue(
      paste0(unfold)
    )
    p <- glue::glue(
      "
      ggplot(foo_df, aes_string(x = 'measure')) +
      geom_violin(fill = 'black', size = 0.1) +
      labs(x = '') +
      facet_grid(variable ~ ., switch = 'both') +
      {theme_basic_y}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "ecdf plot" &
           identical(v, c("N", "N"))) {
    vars1 <- vars[1]
    vars2 <- vars[2]
    q <- glue::glue(
      paste0(unfold)
    )
    p <- glue::glue(
      "
      ggplot(foo_df, aes_string(x = 'measure')) +
      stat_ecdf(geom = 'point', size=0.1) +
      labs(x = '', y = 'p') +
      facet_grid(variable ~ ., switch = 'both') +
      {theme_basic}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "point ecdf plot" &
           identical(v, c("N", "N"))) {
    vars1 <- vars[1]
    vars2 <- vars[2]
    q <- glue::glue(
      paste0(unfold)
    )
    p <- glue::glue(
      "
      ggplot(foo_df, aes_string(x = 'measure')) +
      stat_ecdf(geom = 'point', size=1) +
      labs(x = '', y = 'p') +
      facet_grid(variable ~ ., switch = 'both') +
      {theme_basic}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "stepped ecdf plot" &
           identical(v, c("N", "N"))) {
    vars1 <- vars[1]
    vars2 <- vars[2]
    q <- glue::glue(
      paste0(unfold)
    )
    p <- glue::glue(
      "
      ggplot(foo_df, aes_string(x = 'measure')) +
      stat_ecdf(geom = 'step', size=0.1) +
      labs(x = '', y = 'p') +
      facet_grid(variable ~ ., switch = 'both') +
      {theme_basic}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "path graph" &
           ((any(v == c("N")) & any(v == c("O"))) |
           (any(v == c("N")) & any(v == c("F"))))) {
    vars1 <- colnames(data[vars][which(sapply(data[vars], is.numeric))])
    vars2 <- colnames(data[vars][which(sapply(data[vars], is.factor))])
    p <- glue::glue(
      "
      ggplot({deparse(substitute(data))}, aes(x={as.character(substitute(vars1))}, y={as.character(substitute(vars2))})) +
      \x20\x20geom_path(aes(group=1), size=0.5) +
      \x20\x20{theme_basic}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "freq. reordered path graph" &
           any(v == c("N")) &
           any(v == c("F"))) {
    vars1 <- colnames(data[vars][which(sapply(data[vars], is.numeric))])
    vars2 <- colnames(data[vars][which(sapply(data[vars], is.factor))])
    q <- paste0(paste0("data_mod <- ",
                       deparse(substitute(data)),
                       "[ , c('",
                       as.character(substitute(vars1)),
                       "', '",
                       as.character(substitute(vars2)),
                       "')]\n"),
                glue::glue(
      "
      data_aux <- as.data.frame(ftable(data_mod[2], useNA = 'no'))
      data_mod[,2]  <- factor(data_mod[,2], levels = data_aux[order(-data_aux$Freq),][,1], ordered = TRUE)"))
    p <- glue::glue(
      "
      ggplot({deparse(substitute(data_mod))}, aes(x={as.character(substitute(vars1))}, y={as.character(substitute(vars2))})) +
      \x20\x20geom_path(aes(group=1), size=0.5) +
      \x20\x20{theme_basic}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "alphab. reordered path graph" &
           any(v == c("N")) &
           any(v == c("F"))) {
    vars1 <- colnames(data[vars][which(sapply(data[vars], is.numeric))])
    vars2 <- colnames(data[vars][which(sapply(data[vars], is.factor))])
    q <- paste0(paste0("data_mod <- ",
                       deparse(substitute(data)),
                       "[ , c('",
                       as.character(substitute(vars1)),
                       "', '",
                       as.character(substitute(vars2)),
                       "')]\n"),
                glue::glue(
                  "
      data_mod[,2]  <- as.character(data_mod[,2])"))
    p <- glue::glue(
      "
      ggplot({deparse(substitute(data_mod))}, aes(x={as.character(substitute(vars1))}, y={as.character(substitute(vars2))})) +
      \x20\x20geom_path(aes(group=1), size=0.5) +
      \x20\x20{theme_basic}"
                )
  }
  else if (length(vars) == 2 &
           diagram == "point graph" &
           ((any(v == c("N")) & any(v == c("O"))) |
            (any(v == c("N")) & any(v == c("F"))))) {
    vars1 <- colnames(data[vars][which(sapply(data[vars], is.numeric))])
    vars2 <- colnames(data[vars][which(sapply(data[vars], is.factor))])
    p <- glue::glue(
      "
      ggplot({deparse(substitute(data))}, aes(x={as.character(substitute(vars1))}, y={as.character(substitute(vars2))})) +
      \x20\x20geom_point(aes(group=1)) +
      \x20\x20{theme_basic}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "freq. reordered point graph" &
           any(v == c("N")) &
           any(v == c("F"))) {
    vars1 <- colnames(data[vars][which(sapply(data[vars], is.numeric))])
    vars2 <- colnames(data[vars][which(sapply(data[vars], is.factor))])
    q <- paste0(paste0("data_mod <- ",
                       deparse(substitute(data)),
                       "[ , c('",
                       as.character(substitute(vars1)),
                       "', '",
                       as.character(substitute(vars2)),
                       "')]\n"),
                glue::glue(
                  "
      data_aux <- as.data.frame(ftable(data_mod[2], useNA = 'no'))
      data_mod[,2]  <- factor(data_mod[,2], levels = data_aux[order(-data_aux$Freq),][,1], ordered = TRUE)"))
    p <- glue::glue(
      "
      ggplot({deparse(substitute(data_mod))}, aes(x={as.character(substitute(vars1))}, y={as.character(substitute(vars2))})) +
      \x20\x20geom_point(aes(group=1)) +
      \x20\x20{theme_basic}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "alphab. reordered point graph" &
           any(v == c("N")) &
           any(v == c("F"))) {
    vars1 <- colnames(data[vars][which(sapply(data[vars], is.numeric))])
    vars2 <- colnames(data[vars][which(sapply(data[vars], is.factor))])
    q <- paste0(paste0("data_mod <- ",
                       deparse(substitute(data)),
                       "[ , c('",
                       as.character(substitute(vars1)),
                       "', '",
                       as.character(substitute(vars2)),
                       "')]\n"),
                glue::glue(
                  "
      data_mod[,2]  <- as.character(data_mod[,2])"))
    p <- glue::glue(
      "
      ggplot({deparse(substitute(data_mod))}, aes(x={as.character(substitute(vars1))}, y={as.character(substitute(vars2))})) +
      \x20\x20geom_point(aes(group=1)) +
      \x20\x20{theme_basic}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "tile plot" &
           ((any(v == c("N")) & any(v == c("O"))) |
            (any(v == c("N")) & any(v == c("F"))))) {
    vars1 <- colnames(data[vars][which(sapply(data[vars], is.numeric))])
    vars2 <- colnames(data[vars][which(sapply(data[vars], is.factor))])
    p <- glue::glue(
      "
      ggplot({deparse(substitute(data))}, aes(x={as.character(substitute(vars1))}, y={as.character(substitute(vars2))})) +
      \x20\x20geom_tile() +
      \x20\x20{theme_basic}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "freq. reordered tile plot" &
           any(v == c("N")) &
           any(v == c("F"))) {
    vars1 <- colnames(data[vars][which(sapply(data[vars], is.numeric))])
    vars2 <- colnames(data[vars][which(sapply(data[vars], is.factor))])
    q <- paste0(paste0("data_mod <- ",
                       deparse(substitute(data)),
                       "[ , c('",
                       as.character(substitute(vars1)),
                       "', '",
                       as.character(substitute(vars2)),
                       "')]\n"),
                glue::glue(
                  "
      data_aux <- as.data.frame(ftable(data_mod[2], useNA = 'no'))
      data_mod[,2]  <- factor(data_mod[,2], levels = data_aux[order(-data_aux$Freq),][,1], ordered = TRUE)"))
    p <- glue::glue(
      "
      ggplot({deparse(substitute(data_mod))}, aes(x={as.character(substitute(vars1))}, y={as.character(substitute(vars2))})) +
      \x20\x20geom_tile() +
      \x20\x20{theme_basic}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "alphab. reordered tile plot" &
           any(v == c("N")) &
           any(v == c("F"))) {
    vars1 <- colnames(data[vars][which(sapply(data[vars], is.numeric))])
    vars2 <- colnames(data[vars][which(sapply(data[vars], is.factor))])
    q <- paste0(paste0("data_mod <- ",
                       deparse(substitute(data)),
                       "[ , c('",
                       as.character(substitute(vars1)),
                       "', '",
                       as.character(substitute(vars2)),
                       "')]\n"),
                glue::glue(
                  "
      data_mod[,2]  <- as.character(data_mod[,2])"))
    p <- glue::glue(
      "
      ggplot({deparse(substitute(data_mod))}, aes(x={as.character(substitute(vars1))}, y={as.character(substitute(vars2))})) +
      \x20\x20geom_tile() +
      \x20\x20{theme_basic}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "binned heatmap" &
           ((any(v == c("N")) & any(v == c("O"))) |
            (any(v == c("N")) & any(v == c("F"))))) {
    vars1 <- colnames(data[vars][which(sapply(data[vars], is.numeric))])
    vars2 <- colnames(data[vars][which(sapply(data[vars], is.factor))])
    p <- glue::glue(
      "
      ggplot({deparse(substitute(data))}, aes(x={as.character(substitute(vars1))}, y={as.character(substitute(vars2))})) +
      \x20\x20geom_bin2d(fill = 'black') +
      \x20\x20{theme_basic}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "freq. reordered binned heatmap" &
           any(v == c("N")) &
           any(v == c("F"))) {
    vars1 <- colnames(data[vars][which(sapply(data[vars], is.numeric))])
    vars2 <- colnames(data[vars][which(sapply(data[vars], is.factor))])
    q <- paste0(paste0("data_mod <- ",
                       deparse(substitute(data)),
                       "[ , c('",
                       as.character(substitute(vars1)),
                       "', '",
                       as.character(substitute(vars2)),
                       "')]\n"),
                glue::glue(
                  "
      data_aux <- as.data.frame(ftable(data_mod[2], useNA = 'no'))
      data_mod[,2]  <- factor(data_mod[,2], levels = data_aux[order(-data_aux$Freq),][,1], ordered = TRUE)"))
    p <- glue::glue(
      "
      ggplot({deparse(substitute(data_mod))}, aes(x={as.character(substitute(vars1))}, y={as.character(substitute(vars2))})) +
      \x20\x20geom_bin2d(fill = 'black') +
      \x20\x20{theme_basic}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "alphab. reordered binned heatmap" &
           any(v == c("N")) &
           any(v == c("F"))) {
    vars1 <- colnames(data[vars][which(sapply(data[vars], is.numeric))])
    vars2 <- colnames(data[vars][which(sapply(data[vars], is.factor))])
    q <- paste0(paste0("data_mod <- ",
                       deparse(substitute(data)),
                       "[ , c('",
                       as.character(substitute(vars1)),
                       "', '",
                       as.character(substitute(vars2)),
                       "')]\n"),
                glue::glue(
                  "
      data_mod[,2]  <- as.character(data_mod[,2])"))
    p <- glue::glue(
      "
      ggplot({deparse(substitute(data_mod))}, aes(x={as.character(substitute(vars1))}, y={as.character(substitute(vars2))})) +
      \x20\x20geom_bin2d(fill = 'black') +
      \x20\x20{theme_basic}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "bw binned heatmap" &
           ((any(v == c("N")) & any(v == c("O"))) |
            (any(v == c("N")) & any(v == c("F"))))) {
    vars1 <- colnames(data[vars][which(sapply(data[vars], is.numeric))])
    vars2 <- colnames(data[vars][which(sapply(data[vars], is.factor))])
    p <- glue::glue(
      "
      ggplot({deparse(substitute(data))}, aes(x={as.character(substitute(vars1))}, y={as.character(substitute(vars2))})) +
      \x20\x20geom_bin2d() +
      \x20\x20{scale_bw_a} +
      \x20\x20{theme_basic_z}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "bw freq. reordered binned heatmap" &
           any(v == c("N")) &
           any(v == c("F"))) {
    vars1 <- colnames(data[vars][which(sapply(data[vars], is.numeric))])
    vars2 <- colnames(data[vars][which(sapply(data[vars], is.factor))])
    q <- paste0(paste0("data_mod <- ",
                       deparse(substitute(data)),
                       "[ , c('",
                       as.character(substitute(vars1)),
                       "', '",
                       as.character(substitute(vars2)),
                       "')]\n"),
                glue::glue(
                  "
      data_aux <- as.data.frame(ftable(data_mod[2], useNA = 'no'))
      data_mod[,2]  <- factor(data_mod[,2], levels = data_aux[order(-data_aux$Freq),][,1], ordered = TRUE)"))
    p <- glue::glue(
      "
      ggplot({deparse(substitute(data_mod))}, aes(x={as.character(substitute(vars1))}, y={as.character(substitute(vars2))})) +
      \x20\x20geom_bin2d() +
      \x20\x20{scale_bw_a} +
      \x20\x20{theme_basic_z}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "bw alphab. reordered binned heatmap" &
           any(v == c("N")) &
           any(v == c("F"))) {
    vars1 <- colnames(data[vars][which(sapply(data[vars], is.numeric))])
    vars2 <- colnames(data[vars][which(sapply(data[vars], is.factor))])
    q <- paste0(paste0("data_mod <- ",
                       deparse(substitute(data)),
                       "[ , c('",
                       as.character(substitute(vars1)),
                       "', '",
                       as.character(substitute(vars2)),
                       "')]\n"),
                glue::glue(
                  "
      data_mod[,2]  <- as.character(data_mod[,2])"))
    p <- glue::glue(
      "
      ggplot({deparse(substitute(data_mod))}, aes(x={as.character(substitute(vars1))}, y={as.character(substitute(vars2))})) +
      \x20\x20geom_bin2d() +
      \x20\x20{scale_bw_a} +
      \x20\x20{theme_basic_z}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "color binned heatmap" &
           ((any(v == c("N")) & any(v == c("O"))) |
            (any(v == c("N")) & any(v == c("F"))))) {
    vars1 <- colnames(data[vars][which(sapply(data[vars], is.numeric))])
    vars2 <- colnames(data[vars][which(sapply(data[vars], is.factor))])
    p <- glue::glue(
      "
      ggplot({deparse(substitute(data))}, aes(x={as.character(substitute(vars1))}, y={as.character(substitute(vars2))})) +
      \x20\x20geom_bin2d() +
      \x20\x20{scale_color_a} +
      \x20\x20{theme_basic_z}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "color freq. reordered binned heatmap" &
           any(v == c("N")) &
           any(v == c("F"))) {
    vars1 <- colnames(data[vars][which(sapply(data[vars], is.numeric))])
    vars2 <- colnames(data[vars][which(sapply(data[vars], is.factor))])
    q <- paste0(paste0("data_mod <- ",
                       deparse(substitute(data)),
                       "[ , c('",
                       as.character(substitute(vars1)),
                       "', '",
                       as.character(substitute(vars2)),
                       "')]\n"),
                glue::glue(
                  "
      data_aux <- as.data.frame(ftable(data_mod[2], useNA = 'no'))
      data_mod[,2]  <- factor(data_mod[,2], levels = data_aux[order(-data_aux$Freq),][,1], ordered = TRUE)"))
    p <- glue::glue(
      "
      ggplot({deparse(substitute(data_mod))}, aes(x={as.character(substitute(vars1))}, y={as.character(substitute(vars2))})) +
      \x20\x20geom_bin2d() +
      \x20\x20{scale_color_a} +
      \x20\x20{theme_basic_z}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "color alphab. reordered binned heatmap" &
           any(v == c("N")) &
           any(v == c("F"))) {
    vars1 <- colnames(data[vars][which(sapply(data[vars], is.numeric))])
    vars2 <- colnames(data[vars][which(sapply(data[vars], is.factor))])
    q <- paste0(paste0("data_mod <- ",
                       deparse(substitute(data)),
                       "[ , c('",
                       as.character(substitute(vars1)),
                       "', '",
                       as.character(substitute(vars2)),
                       "')]\n"),
                glue::glue(
                  "
      data_mod[,2]  <- as.character(data_mod[,2])"))
    p <- glue::glue(
      "
      ggplot({deparse(substitute(data_mod))}, aes(x={as.character(substitute(vars1))}, y={as.character(substitute(vars2))})) +
      \x20\x20geom_bin2d() +
      \x20\x20{scale_color_a} +
      \x20\x20{theme_basic_z}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "bw stacked histogram" &
           ((any(v == c("N")) & any(v == c("O"))) |
            (any(v == c("N")) & any(v == c("F"))))) {
    vars1 <- colnames(data[vars][which(sapply(data[vars], is.numeric))])
    vars2 <- colnames(data[vars][which(sapply(data[vars], is.factor))])
    p <- glue::glue(
      "
      ggplot({deparse(substitute(data))}, aes(x={as.character(substitute(vars1))}, color={as.character(substitute(vars2))}, fill={as.character(substitute(vars2))})) +
      \x20\x20geom_histogram(center = 0, position = 'stack') +
      \x20\x20{scl_gray_disc_l} +
      \x20\x20{scl_gray_disc_a} +
      \x20\x20labs(subtitle = '{as.character(substitute(vars2))}') +
      \x20\x20{theme_basic_z}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "color stacked histogram" &
           any(v == c("N")) &
           any(v == c("O"))) {
    vars1 <- colnames(data[vars][which(sapply(data[vars], is.numeric))])
    vars2 <- colnames(data[vars][which(sapply(data[vars], is.ordered))])
    p <- glue::glue(
      "
      ggplot({deparse(substitute(data))}, aes(x={as.character(substitute(vars1))}, color={as.character(substitute(vars2))}, fill={as.character(substitute(vars2))})) +
      \x20\x20geom_histogram(center = 0, position = 'stack') +
      \x20\x20{scl_viridis_l} +
      \x20\x20{scl_viridis_a} +
      \x20\x20labs(subtitle = '{as.character(substitute(vars2))}') +
      \x20\x20{theme_basic_z}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "color stacked histogram" &
           any(v == c("N")) &
           any(v == c("F"))) {
    vars1 <- colnames(data[vars][which(sapply(data[vars], is.numeric))])
    vars2 <- colnames(data[vars][which(sapply(data[vars], is.factor))])
    p <- glue::glue(
      "
      ggplot({deparse(substitute(data))}, aes(x={as.character(substitute(vars1))}, color={as.character(substitute(vars2))}, fill={as.character(substitute(vars2))})) +
      \x20\x20geom_histogram(center = 0, position = 'stack') +
      \x20\x20{scl_color_disc_l} +
      \x20\x20{scl_color_disc_a} +
      \x20\x20labs(subtitle = '{as.character(substitute(vars2))}') +
      \x20\x20{theme_basic_z}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "bw 100% stacked histogram" &
           ((any(v == c("N")) & any(v == c("O"))) |
            (any(v == c("N")) & any(v == c("F"))))) {
    vars1 <- colnames(data[vars][which(sapply(data[vars], is.numeric))])
    vars2 <- colnames(data[vars][which(sapply(data[vars], is.factor))])
    p <- glue::glue(
      "
      ggplot({deparse(substitute(data))}, aes(x={as.character(substitute(vars1))}, color={as.character(substitute(vars2))}, fill={as.character(substitute(vars2))})) +
      \x20\x20geom_histogram(center = 0, position = 'fill') +
      \x20\x20scale_y_continuous(breaks = c(0, 1), labels = c('0%', '100%')) +
      \x20\x20{scl_gray_disc_l} +
      \x20\x20{scl_gray_disc_a} +
      \x20\x20labs(subtitle = '{as.character(substitute(vars2))}') +
      \x20\x20{theme_basic_z}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "color 100% stacked histogram" &
           any(v == c("N")) &
           any(v == c("O"))) {
    vars1 <- colnames(data[vars][which(sapply(data[vars], is.numeric))])
    vars2 <- colnames(data[vars][which(sapply(data[vars], is.ordered))])
    p <- glue::glue(
      "
      ggplot({deparse(substitute(data))}, aes(x={as.character(substitute(vars1))}, color={as.character(substitute(vars2))}, fill={as.character(substitute(vars2))})) +
      \x20\x20geom_histogram(center = 0, position = 'fill') +
      \x20\x20scale_y_continuous(breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1), labels = c('0%', '20%', '40%', '60%', '80%', '100%')) +
      \x20\x20{scl_viridis_l} +
      \x20\x20{scl_viridis_a} +
      \x20\x20labs(subtitle = '{as.character(substitute(vars2))}') +
      \x20\x20{theme_basic_z}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "color 100% stacked histogram" &
           any(v == c("N")) &
           any(v == c("F"))) {
    vars1 <- colnames(data[vars][which(sapply(data[vars], is.numeric))])
    vars2 <- colnames(data[vars][which(sapply(data[vars], is.factor))])
    p <- glue::glue(
      "
      ggplot({deparse(substitute(data))}, aes(x={as.character(substitute(vars1))}, color={as.character(substitute(vars2))}, fill={as.character(substitute(vars2))})) +
      \x20\x20geom_histogram(center = 0, position = 'fill') +
      \x20\x20scale_y_continuous(breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1), labels = c('0%', '20%', '40%', '60%', '80%', '100%')) +
      \x20\x20{scl_color_disc_l} +
      \x20\x20{scl_color_disc_a} +
      \x20\x20labs(subtitle = '{as.character(substitute(vars2))}') +
      \x20\x20{theme_basic_z}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "density plot" &
           ((any(v == c("N")) & any(v == c("O"))) |
            (any(v == c("N")) & any(v == c("F"))))) {
    vars1 <- colnames(data[vars][which(sapply(data[vars], is.numeric))])
    vars2 <- colnames(data[vars][which(sapply(data[vars], is.factor))])
    p <- glue::glue(
      "
      ggplot({deparse(substitute(data))}, aes(x={as.character(substitute(vars1))}, group={as.character(substitute(vars2))})) +
      \x20\x20geom_density(size=0.5) +
      \x20\x20labs(subtitle = '{as.character(substitute(vars2))}') +
      \x20\x20{theme_basic}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "bw density plot" &
           any(v == c("N")) &
           any(v == c("O"))) {
    vars1 <- colnames(data[vars][which(sapply(data[vars], is.numeric))])
    vars2 <- colnames(data[vars][which(sapply(data[vars], is.ordered))])
    p <- glue::glue(
      "
      ggplot({deparse(substitute(data))}, aes(x={as.character(substitute(vars1))}, color={as.character(substitute(vars2))})) +
      \x20\x20geom_density(size=0.5) +
      \x20\x20{scl_gray_disc_l} +
      \x20\x20labs(subtitle = '{as.character(substitute(vars2))}') +
      \x20\x20{theme_basic_z}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "color density plot" &
           any(v == c("N")) &
           any(v == c("O"))) {
    vars1 <- colnames(data[vars][which(sapply(data[vars], is.numeric))])
    vars2 <- colnames(data[vars][which(sapply(data[vars], is.ordered))])
    p <- glue::glue(
      "
      ggplot({deparse(substitute(data))}, aes(x={as.character(substitute(vars1))}, color={as.character(substitute(vars2))})) +
      \x20\x20geom_density(size=0.5) +
      \x20\x20{scl_viridis_l} +
      \x20\x20labs(subtitle = '{as.character(substitute(vars2))}') +
      \x20\x20{theme_basic_z}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "color density plot" &
           any(v == c("N")) &
           any(v == c("F"))) {
    vars1 <- colnames(data[vars][which(sapply(data[vars], is.numeric))])
    vars2 <- colnames(data[vars][which(sapply(data[vars], is.factor))])
    p <- glue::glue(
      "
      ggplot({deparse(substitute(data))}, aes(x={as.character(substitute(vars1))}, color={as.character(substitute(vars2))})) +
      \x20\x20geom_density(size=0.5) +
      \x20\x20{scl_color_disc_l} +
      \x20\x20labs(subtitle = '{as.character(substitute(vars2))}') +
      \x20\x20{theme_basic_z}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "bw filled density plot" &
           any(v == c("N")) &
           any(v == c("O"))) {
    vars1 <- colnames(data[vars][which(sapply(data[vars], is.numeric))])
    vars2 <- colnames(data[vars][which(sapply(data[vars], is.ordered))])
    p <- glue::glue(
      "
      ggplot({deparse(substitute(data))}, aes(x={as.character(substitute(vars1))}, fill={as.character(substitute(vars2))})) +
      \x20\x20geom_density(size=0.5, alpha = 0.7, color = 'black') +
      \x20\x20{scl_gray_disc_a} +
      \x20\x20labs(subtitle = '{as.character(substitute(vars2))}') +
      \x20\x20{theme_basic_z}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "filled density plot" &
           ((any(v == c("N")) & any(v == c("O"))) |
            (any(v == c("N")) & any(v == c("F"))))) {
    vars1 <- colnames(data[vars][which(sapply(data[vars], is.numeric))])
    vars2 <- colnames(data[vars][which(sapply(data[vars], is.factor))])
    p <- glue::glue(
      "
      ggplot({deparse(substitute(data))}, aes(x={as.character(substitute(vars1))}, group={as.character(substitute(vars2))})) +
      \x20\x20geom_density(size=0.5, alpha = 0.3, color = 'black', fill = 'black') +
      \x20\x20labs(subtitle = '{as.character(substitute(vars2))}') +
      \x20\x20{theme_basic_z}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "color filled density plot" &
           any(v == c("N")) &
           any(v == c("O"))) {
    vars1 <- colnames(data[vars][which(sapply(data[vars], is.numeric))])
    vars2 <- colnames(data[vars][which(sapply(data[vars], is.ordered))])
    p <- glue::glue(
      "
      ggplot({deparse(substitute(data))}, aes(x={as.character(substitute(vars1))}, fill={as.character(substitute(vars2))})) +
      \x20\x20geom_density(size=0.5, alpha = 0.3, color = 'white') +
      \x20\x20{scl_viridis_a} +
      \x20\x20labs(subtitle = '{as.character(substitute(vars2))}') +
      \x20\x20{theme_basic_z}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "color filled density plot" &
           any(v == c("N")) &
           any(v == c("F"))) {
    vars1 <- colnames(data[vars][which(sapply(data[vars], is.numeric))])
    vars2 <- colnames(data[vars][which(sapply(data[vars], is.factor))])
    p <- glue::glue(
      "
      ggplot({deparse(substitute(data))}, aes(x={as.character(substitute(vars1))}, fill={as.character(substitute(vars2))})) +
      \x20\x20geom_density(size=0.5, alpha = 0.3, color = 'white') +
      \x20\x20{scl_color_disc_a} +
      \x20\x20labs(subtitle = '{as.character(substitute(vars2))}') +
      \x20\x20{theme_basic_z}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "violin plot" &
           ((any(v == c("N")) & any(v == c("O"))) |
            (any(v == c("N")) & any(v == c("F"))))) {
    vars1 <- colnames(data[vars][which(sapply(data[vars], is.factor))])
    vars2 <- colnames(data[vars][which(sapply(data[vars], is.numeric))])
    p <- glue::glue(
      "
      ggplot({deparse(substitute(data))}, aes(x={as.character(substitute(vars1))}, y={as.character(substitute(vars2))})) +
      \x20\x20geom_violin(size=0.5) +
      \x20\x20coord_flip() +
      \x20\x20{theme_basic}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "freq. reordered violin plot" &
           any(v == c("N")) &
           any(v == c("F"))) {
    vars1 <- colnames(data[vars][which(sapply(data[vars], is.factor))])
    vars2 <- colnames(data[vars][which(sapply(data[vars], is.numeric))])
    q <- paste0(paste0("data_mod <- ",
                       deparse(substitute(data)),
                       "[ , c('",
                       as.character(substitute(vars2)),
                       "', '",
                       as.character(substitute(vars1)),
                       "')]\n"),
                glue::glue(
                  "
      data_aux <- as.data.frame(ftable(data_mod[2], useNA = 'no'))
      data_mod[,2]  <- factor(data_mod[,2], levels = data_aux[order(-data_aux$Freq),][,1], ordered = TRUE)"))
    p <- glue::glue(
      "
      ggplot({deparse(substitute(data_mod))}, aes(x={as.character(substitute(vars1))}, y={as.character(substitute(vars2))})) +
      \x20\x20geom_violin(size=0.5) +
      \x20\x20coord_flip() +
      \x20\x20{theme_basic}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "alphab. reordered violin plot" &
           any(v == c("N")) &
           any(v == c("F"))) {
    vars1 <- colnames(data[vars][which(sapply(data[vars], is.factor))])
    vars2 <- colnames(data[vars][which(sapply(data[vars], is.numeric))])
    q <- paste0(paste0("data_mod <- ",
                       deparse(substitute(data)),
                       "[ , c('",
                       as.character(substitute(vars2)),
                       "', '",
                       as.character(substitute(vars1)),
                       "')]\n"),
                glue::glue(
                  "
      data_mod[,2]  <- as.character(data_mod[,2])"))
    p <- glue::glue(
      "
      ggplot({deparse(substitute(data_mod))}, aes(x={as.character(substitute(vars1))}, y={as.character(substitute(vars2))})) +
      \x20\x20geom_violin(size=0.5) +
      \x20\x20coord_flip() +
      \x20\x20{theme_basic}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "filled violin plot" &
           ((any(v == c("N")) & any(v == c("O"))) |
            (any(v == c("N")) & any(v == c("F"))))) {
    vars1 <- colnames(data[vars][which(sapply(data[vars], is.factor))])
    vars2 <- colnames(data[vars][which(sapply(data[vars], is.numeric))])
    p <- glue::glue(
      "
      ggplot({deparse(substitute(data))}, aes(x={as.character(substitute(vars1))}, y={as.character(substitute(vars2))})) +
      \x20\x20geom_violin(fill='black') +
      \x20\x20coord_flip() +
      \x20\x20{theme_basic}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "freq. reordered filled violin plot" &
           any(v == c("N")) &
           any(v == c("F"))) {
    vars1 <- colnames(data[vars][which(sapply(data[vars], is.factor))])
    vars2 <- colnames(data[vars][which(sapply(data[vars], is.numeric))])
    q <- paste0(paste0("data_mod <- ",
                       deparse(substitute(data)),
                       "[ , c('",
                       as.character(substitute(vars2)),
                       "', '",
                       as.character(substitute(vars1)),
                       "')]\n"),
                glue::glue(
                  "
      data_aux <- as.data.frame(ftable(data_mod[2], useNA = 'no'))
      data_mod[,2]  <- factor(data_mod[,2], levels = data_aux[order(-data_aux$Freq),][,1], ordered = TRUE)"))
    p <- glue::glue(
      "
      ggplot({deparse(substitute(data_mod))}, aes(x={as.character(substitute(vars1))}, y={as.character(substitute(vars2))})) +
      \x20\x20geom_violin(fill='black') +
      \x20\x20coord_flip() +
      \x20\x20{theme_basic}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "alphab. reordered filled violin plot" &
           any(v == c("N")) &
           any(v == c("F"))) {
    vars1 <- colnames(data[vars][which(sapply(data[vars], is.factor))])
    vars2 <- colnames(data[vars][which(sapply(data[vars], is.numeric))])
    q <- paste0(paste0("data_mod <- ",
                       deparse(substitute(data)),
                       "[ , c('",
                       as.character(substitute(vars2)),
                       "', '",
                       as.character(substitute(vars1)),
                       "')]\n"),
                glue::glue(
                  "
      data_mod[,2]  <- as.character(data_mod[,2])"))
    p <- glue::glue(
      "
      ggplot({deparse(substitute(data_mod))}, aes(x={as.character(substitute(vars1))}, y={as.character(substitute(vars2))})) +
      \x20\x20geom_violin(fill='black') +
      \x20\x20coord_flip() +
      \x20\x20{theme_basic}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "box plot" &
           ((any(v == c("N")) & any(v == c("O"))) |
            (any(v == c("N")) & any(v == c("F"))))) {
    vars1 <- colnames(data[vars][which(sapply(data[vars], is.factor))])
    vars2 <- colnames(data[vars][which(sapply(data[vars], is.numeric))])
    p <- glue::glue(
      "
      ggplot({deparse(substitute(data))}, aes(x={as.character(substitute(vars1))}, y={as.character(substitute(vars2))})) +
      \x20\x20geom_boxplot(size=0.5) +
      \x20\x20coord_flip() +
      \x20\x20{theme_basic}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "freq. reordered box plot" &
           any(v == c("N")) &
           any(v == c("F"))) {
    vars1 <- colnames(data[vars][which(sapply(data[vars], is.factor))])
    vars2 <- colnames(data[vars][which(sapply(data[vars], is.numeric))])
    q <- paste0(paste0("data_mod <- ",
                       deparse(substitute(data)),
                       "[ , c('",
                       as.character(substitute(vars2)),
                       "', '",
                       as.character(substitute(vars1)),
                       "')]\n"),
                glue::glue(
                  "
      data_aux <- as.data.frame(ftable(data_mod[2], useNA = 'no'))
      data_mod[,2]  <- factor(data_mod[,2], levels = data_aux[order(-data_aux$Freq),][,1], ordered = TRUE)"))
    p <- glue::glue(
      "
      ggplot({deparse(substitute(data_mod))}, aes(x={as.character(substitute(vars1))}, y={as.character(substitute(vars2))})) +
      \x20\x20geom_boxplot(size=0.5) +
      \x20\x20coord_flip() +
      \x20\x20{theme_basic}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "alphab. reordered box plot" &
           any(v == c("N")) &
           any(v == c("F"))) {
    vars1 <- colnames(data[vars][which(sapply(data[vars], is.factor))])
    vars2 <- colnames(data[vars][which(sapply(data[vars], is.numeric))])
    q <- paste0(paste0("data_mod <- ",
                       deparse(substitute(data)),
                       "[ , c('",
                       as.character(substitute(vars2)),
                       "', '",
                       as.character(substitute(vars1)),
                       "')]\n"),
                glue::glue(
                  "
      data_mod[,2]  <- as.character(data_mod[,2])"))
    p <- glue::glue(
      "
      ggplot({deparse(substitute(data_mod))}, aes(x={as.character(substitute(vars1))}, y={as.character(substitute(vars2))})) +
      \x20\x20geom_boxplot(size=0.5) +
      \x20\x20coord_flip() +
      \x20\x20{theme_basic}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "bw stacked bar graph" &
           ((any(v == c("F")) & any(v == c("O"))) |
            (any(v == c("O")) & any(v == c("O"))))) {
    vars1 <- vars[1]
    vars2 <- vars[2]
    p <- glue::glue(
      "
      ggplot({deparse(substitute(data))}, aes(x={as.character(substitute(vars2))}, fill={as.character(substitute(vars1))})) +
      \x20\x20geom_bar(key_glyph = draw_key_dotplot, position = 'stack') +
      \x20\x20{scl_gray_disc_a} +
      \x20\x20{p_legend} +
      \x20\x20coord_flip() +
      \x20\x20{theme_basic_stack}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "bw freq. reordered stacked bar graph" &
           any(v == c("F")) &
           any(v == c("O"))) {
    vars1 <- colnames(data[vars][which(sapply(data[vars], is.ordered))])
    vars2 <- colnames(data[vars][,sapply(data[vars], function(x) is.factor(x) & !is.ordered(x)), drop = FALSE])
    q <- glue::glue(
      "
      data_aux <- {deparse(substitute(data))}[, c('{as.character(substitute(vars1))}', '{as.character(substitute(vars2))}')]
      data_aux_y <- as.data.frame(ftable(data_aux${as.character(substitute(vars2))}, useNA = 'no'))
      data_aux[,2]  <- factor(data_aux[,2], levels = data_aux_y[order(-data_aux_y$Freq),][,1], ordered = TRUE)"
      )
    p <- glue::glue(
      "
      ggplot(data_aux, aes(x={as.character(substitute(vars2))}, fill={as.character(substitute(vars1))})) +
      \x20\x20geom_bar(key_glyph = draw_key_dotplot, position = 'stack') +
      \x20\x20{scl_gray_disc_a} +
      \x20\x20{p_legend} +
      \x20\x20coord_flip() +
      \x20\x20{theme_basic_stack}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "bw alphab. reordered stacked bar graph" &
           any(v == c("F")) &
           any(v == c("O"))) {
    vars1 <- colnames(data[vars][which(sapply(data[vars], is.ordered))])
    vars2 <- colnames(data[vars][,sapply(data[vars], function(x) is.factor(x) & !is.ordered(x)), drop = FALSE])
    q <- glue::glue(
      "
      data_aux <- data[, vars]
      data_aux[[vars[2]]] <- as.character(data_aux[[vars[2]]])"
      )
    p <- glue::glue(
      "
      ggplot(data_aux, aes(x={as.character(substitute(vars2))}, fill={as.character(substitute(vars1))})) +
      \x20\x20geom_bar(key_glyph = draw_key_dotplot, position = 'stack') +
      \x20\x20{scl_gray_disc_a} +
      \x20\x20{p_legend} +
      \x20\x20coord_flip() +
      \x20\x20{theme_basic_stack}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "color stacked bar graph" &
           ((any(v == c("F")) & any(v == c("O"))) |
            (any(v == c("O")) & any(v == c("O"))))) {
    vars1 <- vars[1]
    vars2 <- vars[2]
    p <- glue::glue(
      "
      ggplot({deparse(substitute(data))}, aes(x={as.character(substitute(vars2))}, fill={as.character(substitute(vars1))})) +
      \x20\x20geom_bar(key_glyph = draw_key_dotplot, position = 'stack') +
      \x20\x20{scl_viridis_a} +
      \x20\x20{p_legend} +
      \x20\x20coord_flip() +
      \x20\x20{theme_basic_stack}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "color freq. reordered stacked bar graph" &
           (any(v == c("F")) &
           any(v == c("O")))) {
    vars1 <- colnames(data[vars][which(sapply(data[vars], is.ordered))])
    vars2 <- colnames(data[vars][,sapply(data[vars], function(x) is.factor(x) & !is.ordered(x)), drop = FALSE])
    q <- glue::glue(
      "
      data_aux <- {deparse(substitute(data))}[, c('{as.character(substitute(vars1))}', '{as.character(substitute(vars2))}')]
      data_aux_y <- as.data.frame(ftable(data_aux${as.character(substitute(vars2))}, useNA = 'no'))
      data_aux[,2]  <- factor(data_aux[,2], levels = data_aux_y[order(-data_aux_y$Freq),][,1], ordered = TRUE)"
      )
    p <- glue::glue(
      "
      ggplot(data_aux, aes(x={as.character(substitute(vars2))}, fill={as.character(substitute(vars1))})) +
      \x20\x20geom_bar(key_glyph = draw_key_dotplot, position = 'stack') +
      \x20\x20{scl_viridis_a} +
      \x20\x20{p_legend} +
      \x20\x20coord_flip() +
      \x20\x20{theme_basic_stack}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "color alphab. reordered stacked bar graph" &
           (any(v == c("F")) &
            any(v == c("O")))) {
    vars1 <- colnames(data[vars][which(sapply(data[vars], is.ordered))])
    vars2 <- colnames(data[vars][,sapply(data[vars], function(x) is.factor(x) & !is.ordered(x)), drop = FALSE])
    q <- glue::glue(
      "
      data_aux <- data[, vars]
      data_aux[[vars[2]]] <- as.character(data_aux[[vars[2]]])"
      )
    p <- glue::glue(
      "
      ggplot(data_aux, aes(x={as.character(substitute(vars2))}, fill={as.character(substitute(vars1))})) +
      \x20\x20geom_bar(key_glyph = draw_key_dotplot, position = 'stack') +
      \x20\x20{scl_viridis_a} +
      \x20\x20{p_legend} +
      \x20\x20coord_flip() +
      \x20\x20{theme_basic_stack}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "color stacked bar graph" &
           identical(v, c("F", "F"))) {
    vars1 <- vars[1]
    vars2 <- vars[2]
    p <- glue::glue(
      "
      ggplot({deparse(substitute(data))}, aes(x={as.character(substitute(vars2))}, fill={as.character(substitute(vars1))})) +
      \x20\x20geom_bar(key_glyph = draw_key_dotplot, position = 'stack') +
      \x20\x20{scl_color_disc_a} +
      \x20\x20{p_legend} +
      \x20\x20coord_flip() +
      \x20\x20{theme_basic_stack}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "transposed color stacked bar graph" &
           identical(v, c("F", "F"))) {
    vars1 <- vars[1]
    vars2 <- vars[2]
    p <- glue::glue(
      "
      ggplot({deparse(substitute(data))}, aes(x={as.character(substitute(vars1))}, fill={as.character(substitute(vars2))})) +
      \x20\x20geom_bar(key_glyph = draw_key_dotplot, position = 'stack') +
      \x20\x20{scl_color_disc_a} +
      \x20\x20{p_legendt} +
      \x20\x20coord_flip() +
      \x20\x20{theme_basic_stack}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "transposed color stacked bar graph" &
           any(v == c("F")) &
           any(v == c("O"))) {
    vars1 <- colnames(data[vars][which(sapply(data[vars], is.ordered))])
    vars2 <- colnames(data[vars][,sapply(data[vars], function(x) is.factor(x) & !is.ordered(x)), drop = FALSE])
    p <- glue::glue(
      "
      ggplot({deparse(substitute(data))}, aes(x={as.character(substitute(vars1))}, fill={as.character(substitute(vars2))})) +
      \x20\x20geom_bar(key_glyph = draw_key_dotplot, position = 'stack') +
      \x20\x20{scl_color_disc_a} +
      \x20\x20{p_legendt} +
      \x20\x20coord_flip() +
      \x20\x20{theme_basic_stack}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "color freq. reordered stacked bar graph" &
           identical(v, c("F", "F"))) {
    vars1 <- vars[1]
    vars2 <- vars[2]
    q <- glue::glue(
      "
      data_aux <- {deparse(substitute(data))}[, c('{as.character(substitute(vars1))}', '{as.character(substitute(vars2))}')]
      data_aux_x <- as.data.frame(ftable(data_aux${as.character(substitute(vars1))}, useNA = 'no'))
      data_aux_y <- as.data.frame(ftable(data_aux${as.character(substitute(vars2))}, useNA = 'no'))
      data_aux[,1]  <- factor(data_aux[,1], levels = data_aux_x[order(-data_aux_x$Freq),][,1], ordered = TRUE)
      data_aux[,2]  <- factor(data_aux[,2], levels = data_aux_y[order(-data_aux_y$Freq),][,1], ordered = TRUE)"
      )
    p <- glue::glue(
      "
      ggplot(data_aux, aes(x={as.character(substitute(vars2))}, fill={as.character(substitute(vars1))})) +
      \x20\x20geom_bar(key_glyph = draw_key_dotplot, position = 'stack') +
      \x20\x20{scl_color_disc_a} +
      \x20\x20{p_legend} +
      \x20\x20coord_flip() +
      \x20\x20{theme_basic_stack}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "transposed color freq. reordered stacked bar graph" &
           identical(v, c("F", "F"))) {
    vars1 <- vars[1]
    vars2 <- vars[2]
    q <- glue::glue(
      "
      data_aux <- {deparse(substitute(data))}[, c('{as.character(substitute(vars1))}', '{as.character(substitute(vars2))}')]
      data_aux_x <- as.data.frame(ftable(data_aux${as.character(substitute(vars1))}, useNA = 'no'))
      data_aux_y <- as.data.frame(ftable(data_aux${as.character(substitute(vars2))}, useNA = 'no'))
      data_aux[,1]  <- factor(data_aux[,1], levels = data_aux_x[order(-data_aux_x$Freq),][,1], ordered = TRUE)
      data_aux[,2]  <- factor(data_aux[,2], levels = data_aux_y[order(-data_aux_y$Freq),][,1], ordered = TRUE)"
      )
    p <- glue::glue(
      "
      ggplot({deparse(substitute(data_aux))}, aes(x={as.character(substitute(vars1))}, fill={as.character(substitute(vars2))})) +
      \x20\x20geom_bar(key_glyph = draw_key_dotplot, position = 'stack') +
      \x20\x20{scl_color_disc_a} +
      \x20\x20{p_legendt} +
      \x20\x20coord_flip() +
      \x20\x20{theme_basic_stack}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "color alphab. reordered stacked bar graph" &
           identical(v, c("F", "F"))) {
    vars1 <- vars[1]
    vars2 <- vars[2]
    q <- glue::glue(
      "
      data_aux <- data[, vars]
      data_aux[[vars[1]]] <- as.character(data_aux[[vars[1]]])
      data_aux[[vars[2]]] <- as.character(data_aux[[vars[2]]])"
      )
    p <- glue::glue(
      "
      ggplot({deparse(substitute(data_aux))}, aes(x={as.character(substitute(vars2))}, fill={as.character(substitute(vars1))})) +
      \x20\x20geom_bar(key_glyph = draw_key_dotplot, position = 'stack') +
      \x20\x20{scl_color_disc_a} +
      \x20\x20{p_legend} +
      \x20\x20coord_flip() +
      \x20\x20{theme_basic_stack}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "transposed color alphab. reordered stacked bar graph" &
           identical(v, c("F", "F"))) {
    vars1 <- vars[1]
    vars2 <- vars[2]
    q <- glue::glue(
      "
      data_aux <- data[, vars]
      data_aux[[vars[1]]] <- as.character(data_aux[[vars[1]]])
      data_aux[[vars[2]]] <- as.character(data_aux[[vars[2]]])"
      )
    p <- glue::glue(
      "
      ggplot({deparse(substitute(data_aux))}, aes(x={as.character(substitute(vars1))}, fill={as.character(substitute(vars2))})) +
      \x20\x20geom_bar(key_glyph = draw_key_dotplot, position = 'stack') +
      \x20\x20{scl_color_disc_a} +
      \x20\x20{p_legendt} +
      \x20\x20coord_flip() +
      \x20\x20{theme_basic_stack}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "bw 100% stacked bar graph" &
           ((any(v == c("F")) & any(v == c("O"))) |
            (any(v == c("O")) & any(v == c("O"))))) {
    vars1 <- vars[1]
    vars2 <- vars[2]
    p <- glue::glue(
      "
      ggplot({deparse(substitute(data))}, aes(x={as.character(substitute(vars2))}, fill={as.character(substitute(vars1))})) +
      \x20\x20geom_bar(key_glyph = draw_key_dotplot, position = 'fill') +
      \x20\x20{scl_gray_disc_a} +
      \x20\x20scale_y_continuous(breaks = c(0, 0.5, 1), labels = c('0%', '50%', '100%')) +
      \x20\x20labs(y='percentage') +
      \x20\x20{p_legend} +
      \x20\x20coord_flip() +
      \x20\x20{theme_basic_stack}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "color 100% stacked bar graph" &
           ((any(v == c("F")) & any(v == c("O"))) |
            (any(v == c("O")) & any(v == c("O"))))) {
    vars1 <- vars[1]
    vars2 <- vars[2]
    p <- glue::glue(
      "
      ggplot({deparse(substitute(data))}, aes(x={as.character(substitute(vars2))}, fill={as.character(substitute(vars1))})) +
      \x20\x20geom_bar(key_glyph = draw_key_dotplot, position = 'fill') +
      \x20\x20{scl_viridis_a} +
      \x20\x20scale_y_continuous(breaks = c(0, 0.5, 1), labels = c('0%', '50%', '100%')) +
      \x20\x20labs(y='percentage') +
      \x20\x20{p_legend} +
      \x20\x20coord_flip() +
      \x20\x20{theme_basic_stack}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "color freq. reordered 100% stacked bar graph" &
           any(v == c("F")) &
           any(v == c("O"))) {
    vars1 <- colnames(data[vars][which(sapply(data[vars], is.ordered))])
    vars2 <- colnames(data[vars][,sapply(data[vars], function(x) is.factor(x) & !is.ordered(x)), drop = FALSE])
    q <- glue::glue(
      "
      data_aux <- {deparse(substitute(data))}[, c('{as.character(substitute(vars1))}', '{as.character(substitute(vars2))}')]
      data_aux_y <- as.data.frame(ftable(data_aux${as.character(substitute(vars2))}, useNA = 'no'))
      data_aux[,2]  <- factor(data_aux[,2], levels = data_aux_y[order(-data_aux_y$Freq),][,1], ordered = TRUE)"
      )
    p <- glue::glue(
      "
      ggplot(data_aux, aes(x={as.character(substitute(vars2))}, fill={as.character(substitute(vars1))})) +
      \x20\x20geom_bar(key_glyph = draw_key_dotplot, position = 'fill') +
      \x20\x20{scl_viridis_a} +
      \x20\x20scale_y_continuous(breaks = c(0, 0.5, 1), labels = c('0%', '50%', '100%')) +
      \x20\x20labs(y='percentage') +
      \x20\x20{p_legend} +
      \x20\x20coord_flip() +
      \x20\x20{theme_basic_stack}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "color alphab. reordered 100% stacked bar graph" &
           any(v == c("F")) &
           any(v == c("O"))) {
    vars1 <- colnames(data[vars][which(sapply(data[vars], is.ordered))])
    vars2 <- colnames(data[vars][,sapply(data[vars], function(x) is.factor(x) & !is.ordered(x)), drop = FALSE])
    q <- glue::glue(
      "
      data_aux <- data[, vars]
      data_aux[[vars[2]]] <- as.character(data_aux[[vars[2]]])"
      )
    p <- glue::glue(
      "
      ggplot(data_aux, aes(x={as.character(substitute(vars2))}, fill={as.character(substitute(vars1))})) +
      \x20\x20geom_bar(key_glyph = draw_key_dotplot, position = 'fill') +
      \x20\x20{scl_viridis_a} +
      \x20\x20scale_y_continuous(breaks = c(0, 0.5, 1), labels = c('0%', '50%', '100%')) +
      \x20\x20labs(y='percentage') +
      \x20\x20{p_legend} +
      \x20\x20coord_flip() +
      \x20\x20{theme_basic_stack}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "transposed color 100% stacked bar graph" &
           ((any(v == c("F")) & any(v == c("O"))) |
            (any(v == c("O")) & any(v == c("O"))))) {
    vars1 <- vars[1]
    vars2 <- vars[2]
    p <- glue::glue(
      "
      ggplot({deparse(substitute(data))}, aes(x={as.character(substitute(vars1))}, fill={as.character(substitute(vars2))})) +
      \x20\x20geom_bar(key_glyph = draw_key_dotplot, position = 'fill') +
      \x20\x20{scl_viridis_a} +
      \x20\x20scale_y_continuous(breaks = c(0, 0.5, 1), labels = c('0%', '50%', '100%')) +
      \x20\x20labs(y='percentage') +
      \x20\x20{p_legendt} +
      \x20\x20coord_flip() +
      \x20\x20{theme_basic_stack}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "color 100% stacked bar graph" &
           identical(v, c("F", "F"))) {
    vars1 <- vars[1]
    vars2 <- vars[2]
    p <- glue::glue(
      "
      ggplot({deparse(substitute(data))}, aes(x={as.character(substitute(vars2))}, fill={as.character(substitute(vars1))})) +
      \x20\x20geom_bar(key_glyph = draw_key_dotplot, position = 'fill') +
      \x20\x20{scl_color_disc_a} +
      \x20\x20scale_y_continuous(breaks = c(0, 0.5, 1), labels = c('0%', '50%', '100%')) +
      \x20\x20labs(y='percentage') +
      \x20\x20{p_legend} +
      \x20\x20coord_flip() +
      \x20\x20{theme_basic_stack}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "transposed color 100% stacked bar graph" &
           identical(v, c("F", "F"))) {
    vars1 <- vars[1]
    vars2 <- vars[2]
    p <- glue::glue(
      "
      ggplot({deparse(substitute(data))}, aes(x={as.character(substitute(vars1))}, fill={as.character(substitute(vars2))})) +
      \x20\x20geom_bar(key_glyph = draw_key_dotplot, position = 'fill') +
      \x20\x20{scl_color_disc_a} +
      \x20\x20scale_y_continuous(breaks = c(0, 0.5, 1), labels = c('0%', '50%', '100%')) +
      \x20\x20labs(y='percentage') +
      \x20\x20{p_legendt} +
      \x20\x20coord_flip() +
      \x20\x20{theme_basic_stack}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "transposed color stacked bar graph" &
           any(v == c("F")) &
           any(v == c("O"))) {
    vars1 <- colnames(data[vars][which(sapply(data[vars], is.ordered))])
    vars2 <- colnames(data[vars][,sapply(data[vars], function(x) is.factor(x) & !is.ordered(x)), drop = FALSE])
    p <- glue::glue(
      "
      ggplot({deparse(substitute(data))}, aes(x={as.character(substitute(vars1))}, fill={as.character(substitute(vars2))})) +
      \x20\x20geom_bar(key_glyph = draw_key_dotplot, position = 'fill') +
      \x20\x20{scl_color_disc_a} +
      \x20\x20{p_legendt} +
      \x20\x20coord_flip() +
      \x20\x20{theme_basic_stack}"
    )
  }

  else if (length(vars) == 2 &
           diagram == "color freq. reordered 100% stacked bar graph" &
           identical(v, c("F", "F"))) {
    vars1 <- vars[1]
    vars2 <- vars[2]
    q <- glue::glue(
      "
      data_aux <- {deparse(substitute(data))}[, c('{as.character(substitute(vars1))}', '{as.character(substitute(vars2))}')]
      data_aux_x <- as.data.frame(ftable(data_aux${as.character(substitute(vars1))}, useNA = 'no'))
      data_aux_y <- as.data.frame(ftable(data_aux${as.character(substitute(vars2))}, useNA = 'no'))
      data_aux[,1]  <- factor(data_aux[,1], levels = data_aux_x[order(-data_aux_x$Freq),][,1], ordered = TRUE)
      data_aux[,2]  <- factor(data_aux[,2], levels = data_aux_y[order(-data_aux_y$Freq),][,1], ordered = TRUE)"
      )
    p <- glue::glue(
      "
      ggplot(data_aux, aes(x={as.character(substitute(vars2))}, fill={as.character(substitute(vars1))})) +
      \x20\x20geom_bar(key_glyph = draw_key_dotplot, position = 'fill') +
      \x20\x20{scl_color_disc_a} +
      \x20\x20scale_y_continuous(breaks = c(0, 0.5, 1), labels = c('0%', '50%', '100%')) +
      \x20\x20labs(y='percentage') +
      \x20\x20{p_legend} +
      \x20\x20coord_flip() +
      \x20\x20{theme_basic_stack}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "transposed color freq. reordered 100% stacked bar graph" &
           identical(v, c("F", "F"))) {
    vars1 <- vars[1]
    vars2 <- vars[2]
    q <- glue::glue(
      "
      data_aux <- {deparse(substitute(data))}[, c('{as.character(substitute(vars1))}', '{as.character(substitute(vars2))}')]
      data_aux_x <- as.data.frame(ftable(data_aux${as.character(substitute(vars1))}, useNA = 'no'))
      data_aux_y <- as.data.frame(ftable(data_aux${as.character(substitute(vars2))}, useNA = 'no'))
      data_aux[,1]  <- factor(data_aux[,1], levels = data_aux_x[order(-data_aux_x$Freq),][,1], ordered = TRUE)
      data_aux[,2]  <- factor(data_aux[,2], levels = data_aux_y[order(-data_aux_y$Freq),][,1], ordered = TRUE)"
      )
    p <- glue::glue(
      "
      ggplot(data_aux, aes(x={as.character(substitute(vars1))}, fill={as.character(substitute(vars2))})) +
      \x20\x20geom_bar(key_glyph = draw_key_dotplot, position = 'fill') +
      \x20\x20{scl_color_disc_a} +
      \x20\x20scale_y_continuous(breaks = c(0, 0.5, 1), labels = c('0%', '50%', '100%')) +
      \x20\x20labs(y='percentage') +
      \x20\x20{p_legendt} +
      \x20\x20coord_flip() +
      \x20\x20{theme_basic_stack}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "color alphab. reordered 100% stacked bar graph" &
           identical(v, c("F", "F"))) {
    vars1 <- vars[1]
    vars2 <- vars[2]
    q <- glue::glue(
      "
      data_aux <- data[, vars]
      data_aux[[vars[1]]] <- as.character(data_aux[[vars[1]]])
      data_aux[[vars[2]]] <- as.character(data_aux[[vars[2]]])"
      )
    p <- glue::glue(
      "
      ggplot(data_aux, aes(x={as.character(substitute(vars2))}, fill={as.character(substitute(vars1))})) +
      \x20\x20geom_bar(key_glyph = draw_key_dotplot, position = 'fill') +
      \x20\x20{scl_color_disc_a} +
      \x20\x20scale_y_continuous(breaks = c(0, 0.5, 1), labels = c('0%', '50%', '100%')) +
      \x20\x20labs(y='percentage') +
      \x20\x20{p_legend} +
      \x20\x20coord_flip() +
      \x20\x20{theme_basic_stack}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "transposed color alphab. reordered 100% stacked bar graph" &
           identical(v, c("F", "F"))) {
    vars1 <- vars[1]
    vars2 <- vars[2]
    q <- glue::glue(
      "
      data_aux <- data[, vars]
      data_aux[[vars[1]]] <- as.character(data_aux[[vars[1]]])
      data_aux[[vars[2]]] <- as.character(data_aux[[vars[2]]])"
      )
    p <- glue::glue(
      "
      ggplot(data_aux, aes(x={as.character(substitute(vars1))}, fill={as.character(substitute(vars2))})) +
      \x20\x20geom_bar(key_glyph = draw_key_dotplot, position = 'fill') +
      \x20\x20{scl_color_disc_a} +
      \x20\x20scale_y_continuous(breaks = c(0, 0.5, 1), labels = c('0%', '50%', '100%')) +
      \x20\x20labs(y='percentage') +
      \x20\x20{p_legendt} +
      \x20\x20coord_flip() +
      \x20\x20{theme_basic_stack}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "transposed bw stacked bar graph" &
           identical(v, c("O", "O"))) {
    vars1 <- vars[1]
    vars2 <- vars[2]
    p <- glue::glue(
      "
      ggplot({deparse(substitute(data))}, aes(x={as.character(substitute(vars1))}, fill={as.character(substitute(vars2))})) +
      \x20\x20geom_bar(key_glyph = draw_key_dotplot, position = 'stack') +
      \x20\x20{scl_gray_disc_a} +
      \x20\x20{p_legendt} +
      \x20\x20coord_flip() +
      \x20\x20{theme_basic_stack}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "transposed color stacked bar graph" &
           identical(v, c("O", "O"))) {
    vars1 <- vars[1]
    vars2 <- vars[2]
    p <- glue::glue(
      "
      ggplot({deparse(substitute(data))}, aes(x={as.character(substitute(vars1))}, fill={as.character(substitute(vars2))})) +
      \x20\x20geom_bar(key_glyph = draw_key_dotplot, position = 'stack') +
      \x20\x20{scl_viridis_a} +
      \x20\x20{p_legendt} +
      \x20\x20coord_flip() +
      \x20\x20{theme_basic_stack}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "transposed color stacked bar graph" &
           (identical(v, c("F", "F")) |
            (any(v == c("F")) & any(v == c("O"))))) {
    vars1 <- vars[1]
    vars2 <- vars[2]
    p <- glue::glue(
      "
      ggplot({deparse(substitute(data))}, aes(x={as.character(substitute(vars1))}, fill={as.character(substitute(vars2))})) +
      \x20\x20geom_bar(key_glyph = draw_key_dotplot, position = 'stack') +
      \x20\x20{scl_color_disc_a} +
      \x20\x20{p_legendt} +
      \x20\x20coord_flip() +
      \x20\x20{theme_basic_stack}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "transposed bw 100% stacked bar graph" &
           identical(v, c("O", "O"))) {
    vars1 <- vars[1]
    vars2 <- vars[2]
    p <- glue::glue(
      "
      ggplot({deparse(substitute(data))}, aes(x={as.character(substitute(vars1))}, fill={as.character(substitute(vars2))})) +
      \x20\x20geom_bar(key_glyph = draw_key_dotplot, position = 'fill') +
      \x20\x20{scl_gray_disc_a} +
      \x20\x20scale_y_continuous(breaks = c(0, 0.5, 1), labels = c('0%', '50%', '100%')) +
      \x20\x20labs(y='percentage') +
      \x20\x20{p_legendt} +
      \x20\x20coord_flip() +
      \x20\x20{theme_basic_stack}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "transposed color 100% stacked bar graph" &
           identical(v, c("O", "O"))) {
    vars1 <- vars[1]
    vars2 <- vars[2]
    p <- glue::glue(
      "
      ggplot({deparse(substitute(data))}, aes(x={as.character(substitute(vars1))}, fill={as.character(substitute(vars2))})) +
      \x20\x20geom_bar(key_glyph = draw_key_dotplot, position = 'fill') +
      \x20\x20{scl_viridis_a} +
      \x20\x20scale_y_continuous(breaks = c(0, 0.5, 1), labels = c('0%', '50%', '100%')) +
      \x20\x20labs(y='percentage') +
      \x20\x20{p_legendt} +
      \x20\x20coord_flip() +
      \x20\x20{theme_basic_stack}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "bw heatmap" &
           (identical(v, c("O", "O")) |
           identical(v, c("F", "F")) |
           (any(v == c("F")) & any(v == c("O")))
           )
           ) {
    vars1 <- vars[1]
    vars2 <- vars[2]
    q <- glue::glue(
      "
      tb <- ftable({deparse(substitute(data))}[,c('{as.character(substitute(vars1))}', '{as.character(substitute(vars2))}')], useNA = 'no')
      df <- as.data.frame(tb)
      names(df)[3] <- 'freq'"
      )
    p <- glue::glue(
      "
      ggplot(df, aes_string(x=df[,1], y=df[,2], fill=df[,3])) +
      \x20\x20geom_tile() +
      \x20\x20{scale_bw_a} +
      \x20\x20{p_labs_fill} +
      \x20\x20{p_guides} +
      \x20\x20{theme_basic_grid}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "bw freq. reordered heatmap" &
           identical(v, c("F", "F"))) {
    vars1 <- vars[1]
    vars2 <- vars[2]
    q <- glue::glue(
      "
      data_aux <- {deparse(substitute(data))}[, c('{as.character(substitute(vars1))}', '{as.character(substitute(vars2))}')]
      data_aux_x <- as.data.frame(ftable(data_aux${as.character(substitute(vars1))}, useNA = 'no'))
      data_aux_y <- as.data.frame(ftable(data_aux${as.character(substitute(vars2))}, useNA = 'no'))
      data_aux[,1]  <- factor(data_aux[,1], levels = data_aux_x[order(-data_aux_x$Freq),][,1], ordered = TRUE)
      data_aux[,2]  <- factor(data_aux[,2], levels = data_aux_y[order(-data_aux_y$Freq),][,1], ordered = TRUE)

      tb <- ftable(data_aux[,c('{as.character(substitute(vars1))}', '{as.character(substitute(vars2))}')], useNA = 'no')
      df <- as.data.frame(tb)
      names(df)[3] <- 'freq'"
      )
    p <- glue::glue(
      "
      ggplot(df, aes_string(x=df[,1], y=df[,2], fill=df[,3])) +
      \x20\x20geom_tile() +
      \x20\x20{scale_bw_a} +
      \x20\x20{p_labs_fill} +
      \x20\x20{p_guides} +
      \x20\x20{theme_basic_grid}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "bw freq. reordered heatmap" &
           any(v == c("F")) &
           any(v == c("O"))) {
    vars1 <- colnames(data[vars][which(sapply(data[vars], is.ordered))])
    vars2 <- colnames(data[vars][,sapply(data[vars], function(x) is.factor(x) & !is.ordered(x)), drop = FALSE])
    q <- glue::glue(
      "
      data_aux <- {deparse(substitute(data))}[, c('{as.character(substitute(vars1))}', '{as.character(substitute(vars2))}')]
      data_aux_y <- as.data.frame(ftable(data_aux${as.character(substitute(vars2))}, useNA = 'no'))
      data_aux[,2]  <- factor(data_aux[,2], levels = data_aux_y[order(-data_aux_y$Freq),][,1], ordered = TRUE)

      tb <- ftable(data_aux[,c('{as.character(substitute(vars1))}', '{as.character(substitute(vars2))}')], useNA = 'no')
      df <- as.data.frame(tb)
      names(df)[3] <- 'freq'"
      )
    p <- glue::glue(
      "
      ggplot(df, aes_string(x=df[,1], y=df[,2], fill=df[,3])) +
      \x20\x20geom_tile() +
      \x20\x20{scale_bw_a} +
      \x20\x20{p_labs_fill} +
      \x20\x20{p_guides} +
      \x20\x20{theme_basic_grid}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "bw alphab. reordered heatmap" &
           any(v == c("F")) &
           any(v == c("O"))) {
    vars1 <- colnames(data[vars][which(sapply(data[vars], is.ordered))])
    vars2 <- colnames(data[vars][,sapply(data[vars], function(x) is.factor(x) & !is.ordered(x)), drop = FALSE])
    q <- glue::glue(
      "
      data_aux <- data[, vars]
      data_aux[[vars[2]]] <- as.character(data_aux[[vars[2]]])

      tb <- ftable(data_aux[,c('{as.character(substitute(vars1))}', '{as.character(substitute(vars2))}')], useNA = 'no')
      df <- as.data.frame(tb)
      names(df)[3] <- 'freq'"
      )
    p <- glue::glue(
      "
      ggplot(df, aes_string(x=df[,1], y=df[,2], fill=df[,3])) +
      \x20\x20geom_tile() +
      \x20\x20{scale_bw_a} +
      \x20\x20{p_labs_fill} +
      \x20\x20{p_guides} +
      \x20\x20{theme_basic_grid}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "bw alphab. reordered heatmap" &
           identical(v, c("F", "F"))) {
    vars1 <- vars[1]
    vars2 <- vars[2]
    q <- glue::glue(
      "
      data_aux <- data[, vars]
      data_aux[[vars[1]]] <- as.character(data_aux[[vars[1]]])
      data_aux[[vars[2]]] <- as.character(data_aux[[vars[2]]])

      tb <- ftable(data_aux[,c('{as.character(substitute(vars1))}', '{as.character(substitute(vars2))}')], useNA = 'no')
      df <- as.data.frame(tb)
      names(df)[3] <- 'freq'"
      )
    p <- glue::glue(
      "
      ggplot(df, aes_string(x=df[,1], y=df[,2], fill=df[,3])) +
      \x20\x20geom_tile() +
      \x20\x20{scale_bw_a} +
      \x20\x20{p_labs_fill} +
      \x20\x20{p_guides} +
      \x20\x20{theme_basic_grid}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "color heatmap" &
           (identical(v, c("O", "O")) |
            identical(v, c("F", "F")) |
            (any(v == c("F")) & any(v == c("O")))
           )) {
    vars1 <- vars[1]
    vars2 <- vars[2]
    q <- glue::glue(
      "
      tb <- ftable({deparse(substitute(data))}[,c('{as.character(substitute(vars1))}', '{as.character(substitute(vars2))}')], useNA = 'no')
      df <- as.data.frame(tb)
      names(df)[3] <- 'freq'"
      )
    p <- glue::glue(
      "
      ggplot(df, aes_string(x=df[,1], y=df[,2], fill=df[,3])) +
      \x20\x20geom_tile() +
      \x20\x20{scale_color_a} +
      \x20\x20{p_labs_fill} +
      \x20\x20{p_guides} +
      \x20\x20{theme_basic_grid}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "color freq. reordered heatmap" &
           identical(v, c("F", "F"))) {
    vars1 <- vars[1]
    vars2 <- vars[2]
    q <- glue::glue(
      "
      data_aux <- {deparse(substitute(data))}[, c('{as.character(substitute(vars1))}', '{as.character(substitute(vars2))}')]
      data_aux_x <- as.data.frame(ftable(data_aux${as.character(substitute(vars1))}, useNA = 'no'))
      data_aux_y <- as.data.frame(ftable(data_aux${as.character(substitute(vars2))}, useNA = 'no'))
      data_aux[,1]  <- factor(data_aux[,1], levels = data_aux_x[order(-data_aux_x$Freq),][,1], ordered = TRUE)
      data_aux[,2]  <- factor(data_aux[,2], levels = data_aux_y[order(-data_aux_y$Freq),][,1], ordered = TRUE)

      tb <- ftable(data_aux[,c('{as.character(substitute(vars1))}', '{as.character(substitute(vars2))}')], useNA = 'no')
      df <- as.data.frame(tb)
      names(df)[3] <- 'freq'"
      )
    p <- glue::glue(
      "
      ggplot(df, aes_string(x=df[,1], y=df[,2], fill=df[,3])) +
      \x20\x20geom_tile() +
      \x20\x20{scale_color_a} +
      \x20\x20{p_labs_fill} +
      \x20\x20{p_guides} +
      \x20\x20{theme_basic_grid}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "color freq. reordered heatmap" &
           any(v == c("F")) &
           any(v == c("O"))) {
    vars1 <- colnames(data[vars][which(sapply(data[vars], is.ordered))])
    vars2 <- colnames(data[vars][,sapply(data[vars], function(x) is.factor(x) & !is.ordered(x)), drop = FALSE])
    q <- glue::glue(
      "
      data_aux <- {deparse(substitute(data))}[, c('{as.character(substitute(vars1))}', '{as.character(substitute(vars2))}')]
      data_aux_y <- as.data.frame(ftable(data_aux${as.character(substitute(vars2))}, useNA = 'no'))
      data_aux[,2]  <- factor(data_aux[,2], levels = data_aux_y[order(-data_aux_y$Freq),][,1], ordered = TRUE)

      tb <- ftable(data_aux[,c('{as.character(substitute(vars1))}', '{as.character(substitute(vars2))}')], useNA = 'no')
      df <- as.data.frame(tb)
      names(df)[3] <- 'freq'"
      )
    p <- glue::glue(
      "
      ggplot(df, aes_string(x=df[,1], y=df[,2], fill=df[,3])) +
      \x20\x20geom_tile() +
      \x20\x20{scale_color_a} +
      \x20\x20{p_labs_fill} +
      \x20\x20{p_guides} +
      \x20\x20{theme_basic_grid}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "color alphab. reordered heatmap" &
           any(v == c("F")) &
           any(v == c("O"))) {
    vars1 <- colnames(data[vars][which(sapply(data[vars], is.ordered))])
    vars2 <- colnames(data[vars][,sapply(data[vars], function(x) is.factor(x) & !is.ordered(x)), drop = FALSE])
    q <- glue::glue(
      "
      data_aux <- data[, vars]
      data_aux[[vars[2]]] <- as.character(data_aux[[vars[2]]])

      tb <- ftable(data_aux[,c('{as.character(substitute(vars1))}', '{as.character(substitute(vars2))}')], useNA = 'no')
      df <- as.data.frame(tb)
      names(df)[3] <- 'freq'"
      )
    p <- glue::glue(
      "
      ggplot(df, aes_string(x=df[,1], y=df[,2], fill=df[,3])) +
      \x20\x20geom_tile() +
      \x20\x20{scale_color_a} +
      \x20\x20{p_labs_fill} +
      \x20\x20{p_guides} +
      \x20\x20{theme_basic_grid}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "color alphab. reordered heatmap" &
           identical(v, c("F", "F"))) {
    vars1 <- vars[1]
    vars2 <- vars[2]
    q <- glue::glue(
      "
      data_aux <- data[, vars]
      data_aux[[vars[1]]] <- as.character(data_aux[[vars[1]]])
      data_aux[[vars[2]]] <- as.character(data_aux[[vars[2]]])

      tb <- ftable(data_aux[,c('{as.character(substitute(vars1))}', '{as.character(substitute(vars2))}')], useNA = 'no')
      df <- as.data.frame(tb)
      names(df)[3] <- 'freq'"
      )
    p <- glue::glue(
      "
      ggplot(df, aes_string(x=df[,1], y=df[,2], fill=df[,3])) +
      \x20\x20geom_tile() +
      \x20\x20{scale_color_a} +
      \x20\x20{p_labs_fill} +
      \x20\x20{p_guides} +
      \x20\x20{theme_basic_grid}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "color residuals heatmap" &
           (identical(v, c("O", "O")) |
            identical(v, c("F", "F")) |
            (any(v == c("F")) & any(v == c("O")))
           )) {
    vars1 <- vars[1]
    vars2 <- vars[2]
    q <- glue::glue(
      "
      tb <- ftable({deparse(substitute(data))}[,c('{as.character(substitute(vars1))}', '{as.character(substitute(vars2))}')], useNA = 'no')
      df <- as.data.frame(tb)
      df[,3]     <- as.data.frame(chisq.test(tb)$residuals)$Freq
      names(df)[3] <- 'resid'"
      )
    p <- glue::glue(
      "
      ggplot(df, aes_string(x=df[,1], y=df[,2], fill=df[,3])) +
      \x20\x20geom_tile() +
      \x20\x20{scale_value_sym} +
      \x20\x20{p_labs_fill} +
      \x20\x20{p_guides} +
      \x20\x20{theme_basic_grid}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "color freq. reordered residuals heatmap" &
           identical(v, c("F", "F"))) {
    vars1 <- vars[1]
    vars2 <- vars[2]
    q <- glue::glue(
      "
      data_aux <- {deparse(substitute(data))}[, c('{as.character(substitute(vars1))}', '{as.character(substitute(vars2))}')]
      data_aux_x <- as.data.frame(ftable(data_aux${as.character(substitute(vars1))}, useNA = 'no'))
      data_aux_y <- as.data.frame(ftable(data_aux${as.character(substitute(vars2))}, useNA = 'no'))
      data_aux[,1]  <- factor(data_aux[,1], levels = data_aux_x[order(-data_aux_x$Freq),][,1], ordered = TRUE)
      data_aux[,2]  <- factor(data_aux[,2], levels = data_aux_y[order(-data_aux_y$Freq),][,1], ordered = TRUE)

      tb <- ftable(data_aux[,c('{as.character(substitute(vars1))}', '{as.character(substitute(vars2))}')], useNA = 'no')
      df <- as.data.frame(tb)
      df[,3]     <- as.data.frame(chisq.test(tb)$residuals)$Freq
      names(df)[3] <- 'resid'"
      )
    p <- glue::glue(
      "
      ggplot(df, aes_string(x=df[,1], y=df[,2], fill=df[,3])) +
      \x20\x20geom_tile() +
      \x20\x20{scale_value_sym} +
      \x20\x20{p_labs_fill} +
      \x20\x20{p_guides} +
      \x20\x20{theme_basic_grid}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "color freq. reordered residuals heatmap" &
           any(v == c("F")) &
           any(v == c("O"))) {
    vars1 <- colnames(data[vars][which(sapply(data[vars], is.ordered))])
    vars2 <- colnames(data[vars][,sapply(data[vars], function(x) is.factor(x) & !is.ordered(x)), drop = FALSE])
    q <- glue::glue(
      "
      data_aux <- {deparse(substitute(data))}[, c('{as.character(substitute(vars1))}', '{as.character(substitute(vars2))}')]
      data_aux_y <- as.data.frame(ftable(data_aux${as.character(substitute(vars2))}, useNA = 'no'))
      data_aux[,2]  <- factor(data_aux[,2], levels = data_aux_y[order(-data_aux_y$Freq),][,1], ordered = TRUE)

      tb <- ftable(data_aux[,c('{as.character(substitute(vars1))}', '{as.character(substitute(vars2))}')], useNA = 'no')
      df <- as.data.frame(tb)
      df[,3]     <- as.data.frame(chisq.test(tb)$residuals)$Freq
      names(df)[3] <- 'resid'"
      )
    p <- glue::glue(
      "
      ggplot(df, aes_string(x=df[,1], y=df[,2], fill=df[,3])) +
      \x20\x20geom_tile() +
      \x20\x20{scale_value_sym} +
      \x20\x20{p_labs_fill} +
      \x20\x20{p_guides} +
      \x20\x20{theme_basic_grid}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "color alphab. reordered residuals heatmap" &
           any(v == c("F")) &
           any(v == c("O"))) {
    vars1 <- colnames(data[vars][which(sapply(data[vars], is.ordered))])
    vars2 <- colnames(data[vars][,sapply(data[vars], function(x) is.factor(x) & !is.ordered(x)), drop = FALSE])
    q <- glue::glue(
      "
      data_aux <- data[, vars]
      data_aux[[vars[2]]] <- as.character(data_aux[[vars[2]]])

      tb <- ftable(data_aux[,c('{as.character(substitute(vars1))}', '{as.character(substitute(vars2))}')], useNA = 'no')
      df <- as.data.frame(tb)
      df[,3]     <- as.data.frame(chisq.test(tb)$residuals)$Freq
      names(df)[3] <- 'resid'"
      )
    p <- glue::glue(
      "
      ggplot(df, aes_string(x=df[,1], y=df[,2], fill=df[,3])) +
      \x20\x20geom_tile() +
      \x20\x20{scale_value_sym} +
      \x20\x20{p_labs_fill} +
      \x20\x20{p_guides} +
      \x20\x20{theme_basic_grid}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "color alphab. reordered residuals heatmap" &
           identical(v, c("F", "F"))) {
    vars1 <- vars[1]
    vars2 <- vars[2]
    q <- glue::glue(
      "
      data_aux <- data[, vars]
      data_aux[[vars[1]]] <- as.character(data_aux[[vars[1]]])
      data_aux[[vars[2]]] <- as.character(data_aux[[vars[2]]])

      tb <- ftable(data_aux[,c('{as.character(substitute(vars1))}', '{as.character(substitute(vars2))}')], useNA = 'no')
      df <- as.data.frame(tb)
      df[,3]     <- as.data.frame(chisq.test(tb)$residuals)$Freq
      names(df)[3] <- 'resid'"
      )
    p <- glue::glue(
      "
      ggplot(df, aes_string(x=df[,1], y=df[,2], fill=df[,3])) +
      \x20\x20geom_tile() +
      \x20\x20{scale_value_sym} +
      \x20\x20{p_labs_fill} +
      \x20\x20{p_guides} +
      \x20\x20{theme_basic_grid}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "bw contribution to x2 heatmap" &
           (identical(v, c("O", "O")) |
            identical(v, c("F", "F")) |
            (any(v == c("F")) & any(v == c("O")))
           )) {
    vars1 <- vars[1]
    vars2 <- vars[2]
    q <- glue::glue(
      "
      tb <- ftable({deparse(substitute(data))}[,c('{as.character(substitute(vars1))}', '{as.character(substitute(vars2))}')], useNA = 'no')
      df <- as.data.frame(tb)
      chisq <- chisq.test(tb, simulate.p.value = TRUE)
      df[,3]     <- as.data.frame(chisq$residuals^2/chisq$statistic)$Freq
      names(df)[3] <- 'contrib'"
      )
    p <- glue::glue(
      "
      ggplot(df, aes_string(x=df[,1], y=df[,2], fill=df[,3])) +
      \x20\x20geom_tile() +
      \x20\x20{scale_bw_a_p} +
      \x20\x20{p_labs_fill} +
      \x20\x20{p_guides} +
      \x20\x20{theme_basic_grid}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "bw freq. reordered contribution to x2 heatmap" &
           identical(v, c("F", "F"))) {
    vars1 <- vars[1]
    vars2 <- vars[2]
    q <- glue::glue(
      "
      data_aux <- {deparse(substitute(data))}[, c('{as.character(substitute(vars1))}', '{as.character(substitute(vars2))}')]
      data_aux_x <- as.data.frame(ftable(data_aux${as.character(substitute(vars1))}, useNA = 'no'))
      data_aux_y <- as.data.frame(ftable(data_aux${as.character(substitute(vars2))}, useNA = 'no'))
      data_aux[,1]  <- factor(data_aux[,1], levels = data_aux_x[order(-data_aux_x$Freq),][,1], ordered = TRUE)
      data_aux[,2]  <- factor(data_aux[,2], levels = data_aux_y[order(-data_aux_y$Freq),][,1], ordered = TRUE)

      tb <- ftable(data_aux[,c('{as.character(substitute(vars1))}', '{as.character(substitute(vars2))}')], useNA = 'no')
      df <- as.data.frame(tb)
      chisq <- chisq.test(tb, simulate.p.value = TRUE)
      df[,3]     <- as.data.frame(chisq$residuals^2/chisq$statistic)$Freq
      names(df)[3] <- 'contrib'"
      )
    p <- glue::glue(
      "
      ggplot(df, aes_string(x=df[,1], y=df[,2], fill=df[,3])) +
      \x20\x20geom_tile() +
      \x20\x20{scale_bw_a_p} +
      \x20\x20{p_labs_fill} +
      \x20\x20{p_guides} +
      \x20\x20{theme_basic_grid}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "bw freq. reordered contribution to x2 heatmap" &
           any(v == c("F")) &
           any(v == c("O"))) {
    vars1 <- colnames(data[vars][which(sapply(data[vars], is.ordered))])
    vars2 <- colnames(data[vars][,sapply(data[vars], function(x) is.factor(x) & !is.ordered(x)), drop = FALSE])
    q <- glue::glue(
      "
      data_aux <- {deparse(substitute(data))}[, c('{as.character(substitute(vars1))}', '{as.character(substitute(vars2))}')]
      data_aux_y <- as.data.frame(ftable(data_aux${as.character(substitute(vars2))}, useNA = 'no'))
      data_aux[,2]  <- factor(data_aux[,2], levels = data_aux_y[order(-data_aux_y$Freq),][,1], ordered = TRUE)

      tb <- ftable(data_aux[,c('{as.character(substitute(vars1))}', '{as.character(substitute(vars2))}')], useNA = 'no')
      df <- as.data.frame(tb)
      chisq <- chisq.test(tb, simulate.p.value = TRUE)
      df[,3]     <- as.data.frame(chisq$residuals^2/chisq$statistic)$Freq
      names(df)[3] <- 'contrib'"
      )
    p <- glue::glue(
      "
      ggplot(df, aes_string(x=df[,1], y=df[,2], fill=df[,3])) +
      \x20\x20geom_tile() +
      \x20\x20{scale_bw_a_p} +
      \x20\x20{p_labs_fill} +
      \x20\x20{p_guides} +
      \x20\x20{theme_basic_grid}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "bw alphab. reordered contribution to x2 heatmap" &
           any(v == c("F")) &
           any(v == c("O"))) {
    vars1 <- colnames(data[vars][which(sapply(data[vars], is.ordered))])
    vars2 <- colnames(data[vars][,sapply(data[vars], function(x) is.factor(x) & !is.ordered(x)), drop = FALSE])
    q <- glue::glue(
      "
      data_aux <- data[, vars]
      data_aux[[vars[2]]] <- as.character(data_aux[[vars[2]]])

      tb <- ftable(data_aux[,c('{as.character(substitute(vars1))}', '{as.character(substitute(vars2))}')], useNA = 'no')
      df <- as.data.frame(tb)
      chisq <- chisq.test(tb, simulate.p.value = TRUE)
      df[,3]     <- as.data.frame(chisq$residuals^2/chisq$statistic)$Freq
      names(df)[3] <- 'contrib'"
      )
    p <- glue::glue(
      "
      ggplot(df, aes_string(x=df[,1], y=df[,2], fill=df[,3])) +
      \x20\x20geom_tile() +
      \x20\x20{scale_bw_a_p} +
      \x20\x20{p_labs_fill} +
      \x20\x20{p_guides} +
      \x20\x20{theme_basic_grid}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "bw alphab. reordered contribution to x2 heatmap" &
           identical(v, c("F", "F"))) {
    vars1 <- vars[1]
    vars2 <- vars[2]
    q <- glue::glue(
      "
      data_aux <- data[, vars]
      data_aux[[vars[1]]] <- as.character(data_aux[[vars[1]]])
      data_aux[[vars[2]]] <- as.character(data_aux[[vars[2]]])

      tb <- ftable(data_aux[,c('{as.character(substitute(vars1))}', '{as.character(substitute(vars2))}')], useNA = 'no')
      df <- as.data.frame(tb)
      chisq <- chisq.test(tb, simulate.p.value = TRUE)
      df[,3]     <- as.data.frame(chisq$residuals^2/chisq$statistic)$Freq
      names(df)[3] <- 'contrib'"
      )
    p <- glue::glue(
      "
      ggplot(df, aes_string(x=df[,1], y=df[,2], fill=df[,3])) +
      \x20\x20geom_tile() +
      \x20\x20{scale_bw_a_p} +
      \x20\x20{p_labs_fill} +
      \x20\x20{p_guides} +
      \x20\x20{theme_basic_grid}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "color contribution to x2 heatmap" &
           (identical(v, c("O", "O")) |
            identical(v, c("F", "F")) |
            (any(v == c("F")) & any(v == c("O")))
           )) {
    vars1 <- vars[1]
    vars2 <- vars[2]
    q <- glue::glue(
      "
      tb <- ftable({deparse(substitute(data))}[,c('{as.character(substitute(vars1))}', '{as.character(substitute(vars2))}')], useNA = 'no')
      df <- as.data.frame(tb)
      chisq <- chisq.test(tb, simulate.p.value = TRUE)
      df[,3]     <- as.data.frame(chisq$residuals^2/chisq$statistic)$Freq
      names(df)[3] <- 'contrib'"
      )
    p <- glue::glue(
      "
      ggplot(df, aes_string(x=df[,1], y=df[,2], fill=df[,3])) +
      \x20\x20geom_tile() +
      \x20\x20{p_scale_value_a_p} +
      \x20\x20{p_labs_fill} +
      \x20\x20{p_guides} +
      \x20\x20{theme_basic_grid}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "color freq. reordered contribution to x2 heatmap" &
           identical(v, c("F", "F"))) {
    vars1 <- vars[1]
    vars2 <- vars[2]
    q <- glue::glue(
      "
      data_aux <- {deparse(substitute(data))}[, c('{as.character(substitute(vars1))}', '{as.character(substitute(vars2))}')]
      data_aux_x <- as.data.frame(ftable(data_aux${as.character(substitute(vars1))}, useNA = 'no'))
      data_aux_y <- as.data.frame(ftable(data_aux${as.character(substitute(vars2))}, useNA = 'no'))
      data_aux[,1]  <- factor(data_aux[,1], levels = data_aux_x[order(-data_aux_x$Freq),][,1], ordered = TRUE)
      data_aux[,2]  <- factor(data_aux[,2], levels = data_aux_y[order(-data_aux_y$Freq),][,1], ordered = TRUE)

      tb <- ftable(data_aux[,c('{as.character(substitute(vars1))}', '{as.character(substitute(vars2))}')], useNA = 'no')
      df <- as.data.frame(tb)
      chisq <- chisq.test(tb, simulate.p.value = TRUE)
      df[,3]     <- as.data.frame(chisq$residuals^2/chisq$statistic)$Freq
      names(df)[3] <- 'contrib'"
      )
    p <- glue::glue(
      "
      ggplot(df, aes_string(x=df[,1], y=df[,2], fill=df[,3])) +
      \x20\x20geom_tile() +
      \x20\x20{p_scale_value_a_p} +
      \x20\x20{p_labs_fill} +
      \x20\x20{p_guides} +
      \x20\x20{theme_basic_grid}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "color freq. reordered contribution to x2 heatmap" &
           any(v == c("F")) &
           any(v == c("O"))) {
    vars1 <- colnames(data[vars][which(sapply(data[vars], is.ordered))])
    vars2 <- colnames(data[vars][,sapply(data[vars], function(x) is.factor(x) & !is.ordered(x)), drop = FALSE])
    q <- glue::glue(
      "
      data_aux <- {deparse(substitute(data))}[, c('{as.character(substitute(vars1))}', '{as.character(substitute(vars2))}')]
      data_aux_y <- as.data.frame(ftable(data_aux${as.character(substitute(vars2))}, useNA = 'no'))
      data_aux[,2]  <- factor(data_aux[,2], levels = data_aux_y[order(-data_aux_y$Freq),][,1], ordered = TRUE)

      tb <- ftable(data_aux[,c('{as.character(substitute(vars1))}', '{as.character(substitute(vars2))}')], useNA = 'no')
      df <- as.data.frame(tb)
      chisq <- chisq.test(tb, simulate.p.value = TRUE)
      df[,3]     <- as.data.frame(chisq$residuals^2/chisq$statistic)$Freq
      names(df)[3] <- 'contrib'"
      )
    p <- glue::glue(
      "
      ggplot(df, aes_string(x=df[,1], y=df[,2], fill=df[,3])) +
      \x20\x20geom_tile() +
      \x20\x20{p_scale_value_a_p} +
      \x20\x20{p_labs_fill} +
      \x20\x20{p_guides} +
      \x20\x20{theme_basic_grid}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "color alphab. reordered contribution to x2 heatmap" &
           any(v == c("F")) &
           any(v == c("O"))) {
    vars1 <- colnames(data[vars][which(sapply(data[vars], is.ordered))])
    vars2 <- colnames(data[vars][,sapply(data[vars], function(x) is.factor(x) & !is.ordered(x)), drop = FALSE])
    q <- glue::glue(
      "
      data_aux <- data[, vars]
      data_aux[[vars[2]]] <- as.character(data_aux[[vars[2]]])

      tb <- ftable(data_aux[,c('{as.character(substitute(vars1))}', '{as.character(substitute(vars2))}')], useNA = 'no')
      df <- as.data.frame(tb)
      chisq <- chisq.test(tb, simulate.p.value = TRUE)
      df[,3]     <- as.data.frame(chisq$residuals^2/chisq$statistic)$Freq
      names(df)[3] <- 'contrib'"
      )
    p <- glue::glue(
      "
      ggplot(df, aes_string(x=df[,1], y=df[,2], fill=df[,3])) +
      \x20\x20geom_tile() +
      \x20\x20{p_scale_value_a_p} +
      \x20\x20{p_labs_fill} +
      \x20\x20{p_guides} +
      \x20\x20{theme_basic_grid}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "color alphab. reordered contribution to x2 heatmap" &
           identical(v, c("F", "F"))) {
    vars1 <- vars[1]
    vars2 <- vars[2]
    q <- glue::glue(
      "
      data_aux <- data[, vars]
      data_aux[[vars[1]]] <- as.character(data_aux[[vars[1]]])
      data_aux[[vars[2]]] <- as.character(data_aux[[vars[2]]])

      tb <- ftable(data_aux[,c('{as.character(substitute(vars1))}', '{as.character(substitute(vars2))}')], useNA = 'no')
      df <- as.data.frame(tb)
      chisq <- chisq.test(tb, simulate.p.value = TRUE)
      df[,3]     <- as.data.frame(chisq$residuals^2/chisq$statistic)$Freq
      names(df)[3] <- 'contrib'"
      )
    p <- glue::glue(
      "
      ggplot(df, aes_string(x=df[,1], y=df[,2], fill=df[,3])) +
      \x20\x20geom_tile() +
      \x20\x20{p_scale_value_a_p} +
      \x20\x20{p_labs_fill} +
      \x20\x20{p_guides} +
      \x20\x20{theme_basic_grid}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "bw balloon plot" &
           (identical(v, c("O", "O")) |
            identical(v, c("F", "F")) |
            (any(v == c("F")) & any(v == c("O")))
           )) {
    vars1 <- vars[1]
    vars2 <- vars[2]
    q <- glue::glue(
      "
      tb <- ftable({deparse(substitute(data))}[,c('{as.character(substitute(vars1))}', '{as.character(substitute(vars2))}')], useNA = 'no')
      df <- as.data.frame(tb)
      names(df)[3] <- 'freq'"
      )
    p <- glue::glue(
      "
      ggplot(df, aes_string(x=df[,1], y=df[,2], color=df[,3], size=df[,3])) +
      \x20\x20geom_point() +
      \x20\x20{scale_bw_l} +
      \x20\x20{p_labs_color} +
      \x20\x20{p_guides} +
      \x20\x20{theme_basic_grid}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "bw freq. reordered balloon plot" &
           identical(v, c("F", "F"))) {
    vars1 <- vars[1]
    vars2 <- vars[2]
    q <- glue::glue(
      "
      data_aux <- {deparse(substitute(data))}[, c('{as.character(substitute(vars1))}', '{as.character(substitute(vars2))}')]
      data_aux_x <- as.data.frame(ftable(data_aux${as.character(substitute(vars1))}, useNA = 'no'))
      data_aux_y <- as.data.frame(ftable(data_aux${as.character(substitute(vars2))}, useNA = 'no'))
      data_aux[,1]  <- factor(data_aux[,1], levels = data_aux_x[order(-data_aux_x$Freq),][,1], ordered = TRUE)
      data_aux[,2]  <- factor(data_aux[,2], levels = data_aux_y[order(-data_aux_y$Freq),][,1], ordered = TRUE)

      tb <- ftable(data_aux[,c('{as.character(substitute(vars1))}', '{as.character(substitute(vars2))}')], useNA = 'no')
      df <- as.data.frame(tb)
      names(df)[3] <- 'freq'"
      )
    p <- glue::glue(
      "
      ggplot(df, aes_string(x=df[,1], y=df[,2], color=df[,3], size=df[,3])) +
      \x20\x20geom_point() +
      \x20\x20{scale_bw_l} +
      \x20\x20{p_labs_color} +
      \x20\x20{p_guides} +
      \x20\x20{theme_basic_grid}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "bw freq. reordered balloon plot" &
           any(v == c("F")) & any(v == c("O"))) {
    vars1 <- colnames(data[vars][which(sapply(data[vars], is.ordered))])
    vars2 <- colnames(data[vars][,sapply(data[vars], function(x) is.factor(x) & !is.ordered(x)), drop = FALSE])
    q <- glue::glue(
      "
      data_aux <- {deparse(substitute(data))}[, c('{as.character(substitute(vars1))}', '{as.character(substitute(vars2))}')]
      data_aux_y <- as.data.frame(ftable(data_aux${as.character(substitute(vars2))}, useNA = 'no'))
      data_aux[,2]  <- factor(data_aux[,2], levels = data_aux_y[order(-data_aux_y$Freq),][,1], ordered = TRUE)

      tb <- ftable(data_aux[,c('{as.character(substitute(vars1))}', '{as.character(substitute(vars2))}')], useNA = 'no')
      df <- as.data.frame(tb)
      names(df)[3] <- 'freq'"
      )
    p <- glue::glue(
      "
      ggplot(df, aes_string(x=df[,1], y=df[,2], color=df[,3], size=df[,3])) +
      \x20\x20geom_point() +
      \x20\x20{scale_bw_l} +
      \x20\x20{p_labs_color} +
      \x20\x20{p_guides} +
      \x20\x20{theme_basic_grid}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "bw alphab. reordered balloon plot" &
           any(v == c("F")) & any(v == c("O"))) {
    vars1 <- colnames(data[vars][which(sapply(data[vars], is.ordered))])
    vars2 <- colnames(data[vars][,sapply(data[vars], function(x) is.factor(x) & !is.ordered(x)), drop = FALSE])
    q <- glue::glue(
      "
      data_aux <- data[, vars]
      data_aux[[vars[2]]] <- as.character(data_aux[[vars[2]]])

      tb <- ftable(data_aux[,c('{as.character(substitute(vars1))}', '{as.character(substitute(vars2))}')], useNA = 'no')
      df <- as.data.frame(tb)
      names(df)[3] <- 'freq'"
      )
    p <- glue::glue(
      "
      ggplot(df, aes_string(x=df[,1], y=df[,2], color=df[,3], size=df[,3])) +
      \x20\x20geom_point() +
      \x20\x20{scale_bw_l} +
      \x20\x20{p_labs_color} +
      \x20\x20{p_guides} +
      \x20\x20{theme_basic_grid}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "bw alphab. reordered balloon plot" &
           identical(v, c("F", "F"))) {
    vars1 <- vars[1]
    vars2 <- vars[2]
    q <- glue::glue(
      "
      data_aux <- data[, vars]
      data_aux[[vars[1]]] <- as.character(data_aux[[vars[1]]])
      data_aux[[vars[2]]] <- as.character(data_aux[[vars[2]]])

      tb <- ftable(data_aux[,c('{as.character(substitute(vars1))}', '{as.character(substitute(vars2))}')], useNA = 'no')
      df <- as.data.frame(tb)
      names(df)[3] <- 'freq'"
      )
    p <- glue::glue(
      "
      ggplot(df, aes_string(x=df[,1], y=df[,2], color=df[,3], size=df[,3])) +
      \x20\x20geom_point() +
      \x20\x20{scale_bw_l} +
      \x20\x20{p_labs_color} +
      \x20\x20{p_guides} +
      \x20\x20{theme_basic_grid}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "color balloon plot" &
           (identical(v, c("O", "O")) |
            identical(v, c("F", "F")) |
            (any(v == c("F")) & any(v == c("O")))
           )) {
    vars1 <- vars[1]
    vars2 <- vars[2]
    q <- glue::glue(
      "
      tb <- ftable({deparse(substitute(data))}[,c('{as.character(substitute(vars1))}', '{as.character(substitute(vars2))}')], useNA = 'no')
      df <- as.data.frame(tb)
      names(df)[3] <- 'freq'"
      )
    p <- glue::glue(
      "
      ggplot(df, aes_string(x=df[,1], y=df[,2], color=df[,3], size=df[,3])) +
      \x20\x20geom_point() +
      \x20\x20{scale_color_l} +
      \x20\x20{p_labs_color} +
      \x20\x20{p_guides} +
      \x20\x20{theme_basic_grid}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "color freq. reordered balloon plot" &
           identical(v, c("F", "F"))) {
    vars1 <- vars[1]
    vars2 <- vars[2]
    q <- glue::glue(
      "
      data_aux <- {deparse(substitute(data))}[, c('{as.character(substitute(vars1))}', '{as.character(substitute(vars2))}')]
      data_aux_x <- as.data.frame(ftable(data_aux${as.character(substitute(vars1))}, useNA = 'no'))
      data_aux_y <- as.data.frame(ftable(data_aux${as.character(substitute(vars2))}, useNA = 'no'))
      data_aux[,1]  <- factor(data_aux[,1], levels = data_aux_x[order(-data_aux_x$Freq),][,1], ordered = TRUE)
      data_aux[,2]  <- factor(data_aux[,2], levels = data_aux_y[order(-data_aux_y$Freq),][,1], ordered = TRUE)

      tb <- ftable(data_aux[,c('{as.character(substitute(vars1))}', '{as.character(substitute(vars2))}')], useNA = 'no')
      df <- as.data.frame(tb)
      names(df)[3] <- 'freq'"
      )

    p <- glue::glue(
      "
      ggplot(df, aes_string(x=df[,1], y=df[,2], color=df[,3], size=df[,3])) +
      \x20\x20geom_point() +
      \x20\x20{scale_color_l} +
      \x20\x20{p_labs_color} +
      \x20\x20{p_guides} +
      \x20\x20{theme_basic_grid}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "color freq. reordered balloon plot" &
           any(v == c("F")) &
           any(v == c("O"))) {
    vars1 <- colnames(data[vars][which(sapply(data[vars], is.ordered))])
    vars2 <- colnames(data[vars][,sapply(data[vars], function(x) is.factor(x) & !is.ordered(x)), drop = FALSE])
    q <- glue::glue(
      "
      data_aux <- {deparse(substitute(data))}[, c('{as.character(substitute(vars1))}', '{as.character(substitute(vars2))}')]
      data_aux_y <- as.data.frame(ftable(data_aux${as.character(substitute(vars2))}, useNA = 'no'))
      data_aux[,2]  <- factor(data_aux[,2], levels = data_aux_y[order(-data_aux_y$Freq),][,1], ordered = TRUE)

      tb <- ftable(data_aux[,c('{as.character(substitute(vars1))}', '{as.character(substitute(vars2))}')], useNA = 'no')
      df <- as.data.frame(tb)
      names(df)[3] <- 'freq'")
    p <- glue::glue(
      "
      ggplot(df, aes_string(x=df[,1], y=df[,2], color=df[,3], size=df[,3])) +
      \x20\x20geom_point() +
      \x20\x20{scale_color_l} +
      \x20\x20{p_labs_color} +
      \x20\x20{p_guides} +
      \x20\x20{theme_basic_grid}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "color alphab. reordered balloon plot" &
           any(v == c("F")) &
           any(v == c("O"))) {
    vars1 <- colnames(data[vars][which(sapply(data[vars], is.ordered))])
    vars2 <- colnames(data[vars][,sapply(data[vars], function(x) is.factor(x) & !is.ordered(x)), drop = FALSE])
    q <- glue::glue(
      "
      data_aux <- data[, vars]
      data_aux[[vars[2]]] <- as.character(data_aux[[vars[2]]])

      tb <- ftable(data_aux[,c('{as.character(substitute(vars1))}', '{as.character(substitute(vars2))}')], useNA = 'no')
      df <- as.data.frame(tb)
      names(df)[3] <- 'freq'")
    p <- glue::glue(
      "
      ggplot(df, aes_string(x=df[,1], y=df[,2], color=df[,3], size=df[,3])) +
      \x20\x20geom_point() +
      \x20\x20{scale_color_l} +
      \x20\x20{p_labs_color} +
      \x20\x20{p_guides} +
      \x20\x20{theme_basic_grid}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "color alphab. reordered balloon plot" &
           identical(v, c("F", "F"))) {
    vars1 <- vars[1]
    vars2 <- vars[2]
    q <- glue::glue(
      "
      data_aux <- data[, vars]
      data_aux[[vars[1]]] <- as.character(data_aux[[vars[1]]])
      data_aux[[vars[2]]] <- as.character(data_aux[[vars[2]]])

      tb <- ftable(data_aux[,c('{as.character(substitute(vars1))}', '{as.character(substitute(vars2))}')], useNA = 'no')
      df <- as.data.frame(tb)
      names(df)[3] <- 'freq'"
      )
    p <- glue::glue(
      "
      ggplot(df, aes_string(x=df[,1], y=df[,2], color=df[,3], size=df[,3])) +
      \x20\x20geom_point() +
      \x20\x20{scale_color_l} +
      \x20\x20{p_labs_color} +
      \x20\x20{p_guides} +
      \x20\x20{theme_basic_grid}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "color residuals balloon plot" &
           (identical(v, c("O", "O")) |
            identical(v, c("F", "F")) |
            (any(v == c("F")) & any(v == c("O")))
           )) {
    vars1 <- vars[1]
    vars2 <- vars[2]
    q <- glue::glue(
      "
      tb <- ftable({deparse(substitute(data))}[,c('{as.character(substitute(vars1))}', '{as.character(substitute(vars2))}')], useNA = 'no')
      df <- as.data.frame(tb)
      df[,3]     <- as.data.frame(chisq.test(tb)$residuals)$Freq
      names(df)[3] <- 'resid'"
      )
    p <- glue::glue(
      "
      ggplot(df, aes_string(x=df[,1], y=df[,2], color=df[,3], size=df[,3])) +
      \x20\x20geom_point() +
      \x20\x20{scale_value_sym_l} +
      \x20\x20{p_labs_color} +
      \x20\x20{p_guides} +
      \x20\x20{theme_basic_grid}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "color freq. reordered residuals balloon plot" &
           identical(v, c("F", "F"))) {
    vars1 <- vars[1]
    vars2 <- vars[2]
    q <- glue::glue(
      "
      data_aux <- {deparse(substitute(data))}[, c('{as.character(substitute(vars1))}', '{as.character(substitute(vars2))}')]
      data_aux_x <- as.data.frame(ftable(data_aux${as.character(substitute(vars1))}, useNA = 'no'))
      data_aux_y <- as.data.frame(ftable(data_aux${as.character(substitute(vars2))}, useNA = 'no'))
      data_aux[,1]  <- factor(data_aux[,1], levels = data_aux_x[order(-data_aux_x$Freq),][,1], ordered = TRUE)
      data_aux[,2]  <- factor(data_aux[,2], levels = data_aux_y[order(-data_aux_y$Freq),][,1], ordered = TRUE)

      tb <- ftable(data_aux[,c('{as.character(substitute(vars1))}', '{as.character(substitute(vars2))}')], useNA = 'no')
      df <- as.data.frame(tb)
      df[,3]     <- as.data.frame(chisq.test(tb)$residuals)$Freq
      names(df)[3] <- 'resid'"
      )
    p <- glue::glue(
      "
      ggplot(df, aes_string(x=df[,1], y=df[,2], color=df[,3], size=df[,3])) +
      \x20\x20geom_point() +
      \x20\x20{scale_value_sym_l} +
      \x20\x20{p_labs_color} +
      \x20\x20{p_guides} +
      \x20\x20{theme_basic_grid}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "color freq. reordered residuals balloon plot" &
           any(v == c("F")) & any(v == c("O"))) {
    vars1 <- colnames(data[vars][which(sapply(data[vars], is.ordered))])
    vars2 <- colnames(data[vars][,sapply(data[vars], function(x) is.factor(x) & !is.ordered(x)), drop = FALSE])
    q <- glue::glue(
      "
      data_aux <- {deparse(substitute(data))}[, c('{as.character(substitute(vars1))}', '{as.character(substitute(vars2))}')]
      data_aux_y <- as.data.frame(ftable(data_aux${as.character(substitute(vars2))}, useNA = 'no'))
      data_aux[,2]  <- factor(data_aux[,2], levels = data_aux_y[order(-data_aux_y$Freq),][,1], ordered = TRUE)

      tb <- ftable(data_aux[,c('{as.character(substitute(vars1))}', '{as.character(substitute(vars2))}')], useNA = 'no')
      df <- as.data.frame(tb)
      df[,3]     <- as.data.frame(chisq.test(tb)$residuals)$Freq
      names(df)[3] <- 'resid'"
      )
    p <- glue::glue(
      "
      ggplot(df, aes_string(x=df[,1], y=df[,2], color=df[,3], size=df[,3])) +
      \x20\x20geom_point() +
      \x20\x20{scale_value_sym_l} +
      \x20\x20{p_labs_color} +
      \x20\x20{p_guides} +
      \x20\x20{theme_basic_grid}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "color alphab. reordered residuals balloon plot" &
           any(v == c("F")) & any(v == c("O"))) {
    vars1 <- colnames(data[vars][which(sapply(data[vars], is.ordered))])
    vars2 <- colnames(data[vars][,sapply(data[vars], function(x) is.factor(x) & !is.ordered(x)), drop = FALSE])
    q <- glue::glue(
      "
      data_aux <- data[, vars]
      data_aux[[vars[2]]] <- as.character(data_aux[[vars[2]]])

      tb <- ftable(data_aux[,c('{as.character(substitute(vars1))}', '{as.character(substitute(vars2))}')], useNA = 'no')
      df <- as.data.frame(tb)
      df[,3]     <- as.data.frame(chisq.test(tb)$residuals)$Freq
      names(df)[3] <- 'resid'"
      )
    p <- glue::glue(
      "
      ggplot(df, aes_string(x=df[,1], y=df[,2], color=df[,3], size=df[,3])) +
      \x20\x20geom_point() +
      \x20\x20{scale_value_sym_l} +
      \x20\x20{p_labs_color} +
      \x20\x20{p_guides} +
      \x20\x20{theme_basic_grid}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "color alphab. reordered residuals balloon plot" &
           identical(v, c("F", "F"))) {
    vars1 <- vars[1]
    vars2 <- vars[2]
    q <- glue::glue(
      "
      data_aux <- data[, vars]
      data_aux[[vars[1]]] <- as.character(data_aux[[vars[1]]])
      data_aux[[vars[2]]] <- as.character(data_aux[[vars[2]]])

      tb <- ftable(data_aux[,c('{as.character(substitute(vars1))}', '{as.character(substitute(vars2))}')], useNA = 'no')
      df <- as.data.frame(tb)
      df[,3]     <- as.data.frame(chisq.test(tb)$residuals)$Freq
      names(df)[3] <- 'resid'"
      )
    p <- glue::glue(
      "
      ggplot(df, aes_string(x=df[,1], y=df[,2], color=df[,3], size=df[,3])) +
      \x20\x20geom_point() +
      \x20\x20{scale_value_sym_l} +
      \x20\x20{p_labs_color} +
      \x20\x20{p_guides} +
      \x20\x20{theme_basic_grid}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "bw contribution to x2 balloon plot" &
           (identical(v, c("O", "O")) |
            identical(v, c("F", "F")) |
            (any(v == c("F")) & any(v == c("O")))
           )) {
    vars1 <- vars[1]
    vars2 <- vars[2]
    q <- glue::glue(
      "
      tb <- ftable({deparse(substitute(data))}[,c('{as.character(substitute(vars1))}', '{as.character(substitute(vars2))}')], useNA = 'no')
      df <- as.data.frame(tb)
      chisq <- chisq.test(tb, simulate.p.value = TRUE)
      df[,3]     <- as.data.frame(chisq$residuals^2/chisq$statistic)$Freq
      names(df)[3] <- 'contrib'"
      )
    p <- glue::glue(
      "
      ggplot(df, aes_string(x=df[,1], y=df[,2], color=df[,3], size=df[,3])) +
      \x20\x20geom_point() +
      \x20\x20{scale_bw_l_p} +
      \x20\x20{p_labs_color} +
      \x20\x20{p_guides} +
      \x20\x20{theme_basic_grid}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "bw freq. reordered contribution to x2 balloon plot" &
           identical(v, c("F", "F"))) {
    vars1 <- vars[1]
    vars2 <- vars[2]
    q <- glue::glue(
      "
      data_aux <- {deparse(substitute(data))}[, c('{as.character(substitute(vars1))}', '{as.character(substitute(vars2))}')]
      data_aux_x <- as.data.frame(ftable(data_aux${as.character(substitute(vars1))}, useNA = 'no'))
      data_aux_y <- as.data.frame(ftable(data_aux${as.character(substitute(vars2))}, useNA = 'no'))
      data_aux[,1]  <- factor(data_aux[,1], levels = data_aux_x[order(-data_aux_x$Freq),][,1], ordered = TRUE)
      data_aux[,2]  <- factor(data_aux[,2], levels = data_aux_y[order(-data_aux_y$Freq),][,1], ordered = TRUE)

      tb <- ftable(data_aux[,c('{as.character(substitute(vars1))}', '{as.character(substitute(vars2))}')], useNA = 'no')
      df <- as.data.frame(tb)
      chisq <- chisq.test(tb, simulate.p.value = TRUE)
      df[,3]     <- as.data.frame(chisq$residuals^2/chisq$statistic)$Freq
      names(df)[3] <- 'contrib'"
      )
    p <- glue::glue(
      "
      ggplot(df, aes_string(x=df[,1], y=df[,2], color=df[,3], size=df[,3])) +
      \x20\x20geom_point() +
      \x20\x20{scale_bw_l_p} +
      \x20\x20{p_labs_color} +
      \x20\x20{p_guides} +
      \x20\x20{theme_basic_grid}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "bw freq. reordered contribution to x2 balloon plot" &
           any(v == c("F")) & any(v == c("O"))) {
    vars1 <- colnames(data[vars][which(sapply(data[vars], is.ordered))])
    vars2 <- colnames(data[vars][,sapply(data[vars], function(x) is.factor(x) & !is.ordered(x)), drop = FALSE])
    q <- glue::glue(
      "
      data_aux <- {deparse(substitute(data))}[, c('{as.character(substitute(vars1))}', '{as.character(substitute(vars2))}')]
      data_aux_y <- as.data.frame(ftable(data_aux${as.character(substitute(vars2))}, useNA = 'no'))
      data_aux[,2]  <- factor(data_aux[,2], levels = data_aux_y[order(-data_aux_y$Freq),][,1], ordered = TRUE)

      tb <- ftable(data_aux[,c('{as.character(substitute(vars1))}', '{as.character(substitute(vars2))}')], useNA = 'no')
      df <- as.data.frame(tb)
      chisq <- chisq.test(tb, simulate.p.value = TRUE)
      df[,3]     <- as.data.frame(chisq$residuals^2/chisq$statistic)$Freq
      names(df)[3] <- 'contrib'"
      )
    p <- glue::glue(
      "
      ggplot(df, aes_string(x=df[,1], y=df[,2], color=df[,3], size=df[,3])) +
      \x20\x20geom_point() +
      \x20\x20{scale_bw_l_p} +
      \x20\x20{p_labs_color} +
      \x20\x20{p_guides} +
      \x20\x20{theme_basic_grid}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "bw alphab. reordered contribution to x2 balloon plot" &
           any(v == c("F")) & any(v == c("O"))) {
    vars1 <- colnames(data[vars][which(sapply(data[vars], is.ordered))])
    vars2 <- colnames(data[vars][,sapply(data[vars], function(x) is.factor(x) & !is.ordered(x)), drop = FALSE])
    q <- glue::glue(
      "
      data_aux <- data[, vars]
      data_aux[[vars[2]]] <- as.character(data_aux[[vars[2]]])

      tb <- ftable(data_aux[,c('{as.character(substitute(vars1))}', '{as.character(substitute(vars2))}')], useNA = 'no')
      df <- as.data.frame(tb)
      chisq <- chisq.test(tb, simulate.p.value = TRUE)
      df[,3]     <- as.data.frame(chisq$residuals^2/chisq$statistic)$Freq
      names(df)[3] <- 'contrib'"
      )
    p <- glue::glue(
      "
      ggplot(df, aes_string(x=df[,1], y=df[,2], color=df[,3], size=df[,3])) +
      \x20\x20geom_point() +
      \x20\x20{scale_bw_l_p} +
      \x20\x20{p_labs_color} +
      \x20\x20{p_guides} +
      \x20\x20{theme_basic_grid}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "bw alphab. reordered contribution to x2 balloon plot" &
           identical(v, c("F", "F"))) {
    vars1 <- vars[1]
    vars2 <- vars[2]
    q <- glue::glue(
      "
      data_aux <- data[, vars]
      data_aux[[vars[1]]] <- as.character(data_aux[[vars[1]]])
      data_aux[[vars[2]]] <- as.character(data_aux[[vars[2]]])

      tb <- ftable(data_aux[,c('{as.character(substitute(vars1))}', '{as.character(substitute(vars2))}')], useNA = 'no')
      df <- as.data.frame(tb)
      chisq <- chisq.test(tb, simulate.p.value = TRUE)
      df[,3]     <- as.data.frame(chisq$residuals^2/chisq$statistic)$Freq
      names(df)[3] <- 'contrib'"
      )
    p <- glue::glue(
      "
      ggplot(df, aes_string(x=df[,1], y=df[,2], color=df[,3], size=df[,3])) +
      \x20\x20geom_point() +
      \x20\x20{scale_bw_l_p} +
      \x20\x20{p_labs_color} +
      \x20\x20{p_guides} +
      \x20\x20{theme_basic_grid}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "color contribution to x2 balloon plot" &
           (identical(v, c("O", "O")) |
            identical(v, c("F", "F")) |
            (any(v == c("F")) & any(v == c("O")))
           )) {
    vars1 <- vars[1]
    vars2 <- vars[2]
    q <- glue::glue(
      "
      tb <- ftable({deparse(substitute(data))}[,c('{as.character(substitute(vars1))}', '{as.character(substitute(vars2))}')], useNA = 'no')
      df <- as.data.frame(tb)
      chisq <- chisq.test(tb, simulate.p.value = TRUE)
      df[,3]     <- as.data.frame(chisq$residuals^2/chisq$statistic)$Freq
      names(df)[3] <- 'contrib'"
      )
    p <- glue::glue(
      "
      ggplot(df, aes_string(x=df[,1], y=df[,2], color=df[,3], size=df[,3])) +
      \x20\x20geom_point() +
      \x20\x20p_scale_value_l_p +
      \x20\x20{p_labs_color} +
      \x20\x20{p_guides} +
      \x20\x20{theme_basic_grid}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "color freq. reordered contribution to x2 balloon plot" &
           identical(v, c("F", "F"))) {
    vars1 <- vars[1]
    vars2 <- vars[2]
    q <- glue::glue(
      "
      data_aux <- {deparse(substitute(data))}[, c('{as.character(substitute(vars1))}', '{as.character(substitute(vars2))}')]
      data_aux_x <- as.data.frame(ftable(data_aux${as.character(substitute(vars1))}, useNA = 'no'))
      data_aux_y <- as.data.frame(ftable(data_aux${as.character(substitute(vars2))}, useNA = 'no'))
      data_aux[,1]  <- factor(data_aux[,1], levels = data_aux_x[order(-data_aux_x$Freq),][,1], ordered = TRUE)
      data_aux[,2]  <- factor(data_aux[,2], levels = data_aux_y[order(-data_aux_y$Freq),][,1], ordered = TRUE)

      tb <- ftable(data_aux[,c('{as.character(substitute(vars1))}', '{as.character(substitute(vars2))}')], useNA = 'no')
      df <- as.data.frame(tb)
      chisq <- chisq.test(tb, simulate.p.value = TRUE)
      df[,3]     <- as.data.frame(chisq$residuals^2/chisq$statistic)$Freq
      names(df)[3] <- 'contrib'"
      )
    p <- glue::glue(
      "
      ggplot(df, aes_string(x=df[,1], y=df[,2], color=df[,3], size=df[,3])) +
      \x20\x20geom_point() +
      \x20\x20p_scale_value_l_p +
      \x20\x20{p_labs_color} +
      \x20\x20{p_guides} +
      \x20\x20{theme_basic_grid}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "color freq. reordered contribution to x2 balloon plot" &
           any(v == c("F")) & any(v == c("O"))) {
    vars1 <- colnames(data[vars][which(sapply(data[vars], is.ordered))])
    vars2 <- colnames(data[vars][,sapply(data[vars], function(x) is.factor(x) & !is.ordered(x)), drop = FALSE])
    q <- glue::glue(
      "
      data_aux <- {deparse(substitute(data))}[, c('{as.character(substitute(vars1))}', '{as.character(substitute(vars2))}')]
      data_aux_y <- as.data.frame(ftable(data_aux${as.character(substitute(vars2))}, useNA = 'no'))
      data_aux[,2]  <- factor(data_aux[,2], levels = data_aux_y[order(-data_aux_y$Freq),][,1], ordered = TRUE)

      tb <- ftable(data_aux[,c('{as.character(substitute(vars1))}', '{as.character(substitute(vars2))}')], useNA = 'no')
      df <- as.data.frame(tb)
      chisq <- chisq.test(tb, simulate.p.value = TRUE)
      df[,3]     <- as.data.frame(chisq$residuals^2/chisq$statistic)$Freq
      names(df)[3] <- 'contrib'"
      )
    p <- glue::glue(
      "
      ggplot(df, aes_string(x=df[,1], y=df[,2], color=df[,3], size=df[,3])) +
      \x20\x20geom_point() +
      \x20\x20p_scale_value_l_p +
      \x20\x20{p_labs_color} +
      \x20\x20{p_guides} +
      \x20\x20{theme_basic_grid}"
    )
  }
  else if (length(vars) == 2 &
           diagram == "color alphab. reordered contribution to x2 balloon plot" &
           any(v == c("F")) & any(v == c("O"))) {
    vars1 <- colnames(data[vars][which(sapply(data[vars], is.ordered))])
    vars2 <- colnames(data[vars][,sapply(data[vars], function(x) is.factor(x) & !is.ordered(x)), drop = FALSE])
    q <- glue::glue(
      "
      data_aux <- data[, vars]
      data_aux[[vars[2]]] <- as.character(data_aux[[vars[2]]])

      tb <- ftable(data_aux[,c('{as.character(substitute(vars1))}', '{as.character(substitute(vars2))}')], useNA = 'no')
      df <- as.data.frame(tb)
      chisq <- chisq.test(tb, simulate.p.value = TRUE)
      df[,3]     <- as.data.frame(chisq$residuals^2/chisq$statistic)$Freq
      names(df)[3] <- 'contrib'"
      )
    p <- glue::glue(
      "
      ggplot(df, aes_string(x=df[,1], y=df[,2], color=df[,3], size=df[,3])) +
      \x20\x20geom_point() +
      \x20\x20p_scale_value_l_p +
      \x20\x20{p_labs_color} +
      \x20\x20{p_guides} +
      \x20\x20{theme_basic_grid}"
    )
  }
  else if (length(vars) == 2 &
             diagram == "color alphab. reordered contribution to x2 balloon plot" &
             identical(v, c("F", "F"))) {
    vars1 <- vars[1]
    vars2 <- vars[2]
    q <- glue::glue(
      "
      data_aux <- data[, vars]
      data_aux[[vars[1]]] <- as.character(data_aux[[vars[1]]])
      data_aux[[vars[2]]] <- as.character(data_aux[[vars[2]]])

      tb <- ftable(data_aux[,c('{as.character(substitute(vars1))}', '{as.character(substitute(vars2))}')], useNA = 'no')
      df <- as.data.frame(tb)
      chisq <- chisq.test(tb, simulate.p.value = TRUE)
      df[,3]     <- as.data.frame(chisq$residuals^2/chisq$statistic)$Freq
      names(df)[3] <- 'contrib'"
      )
    p <- glue::glue(
      "
      ggplot(df, aes_string(x=df[,1], y=df[,2], color=df[,3], size=df[,3])) +
      \x20\x20geom_point() +
      \x20\x20p_scale_value_l_p +
      \x20\x20{p_labs_color} +
      \x20\x20{p_guides} +
      \x20\x20{theme_basic_grid}"
    )
  }


# clousure ----------------------------------------------------------------


  else {stop("The combination of these data and graphic type has not been still considered.")}
    if (output == 'console') {
      cat(paste0(q, "\n", p))}
    else if (output == 'plots pane') {
      eval(parse(text=paste0(q, "\n", p)))}
    else if (output == 'html') {
      dir.create(file.path(dir, "brinton_outcomes", fsep = .Platform$file.sep), showWarnings = FALSE)
      writeLines(output_up, file.path(dir, "brinton_outcomes", "plotup.R"))
      write(paste0("cat('A ", deparse(substitute(diagram)), " produced from the " ,deparse(substitute(vars)), " variable(s) of the ", deparse(substitute(data))," dataframe')"),
            file=file.path(dir, "brinton_outcomes", "plotup.R"), append=TRUE)
      write(paste0("#+ plot"), file.path(dir, "brinton_outcomes", "plotup.R"), append = TRUE)
      write(paste0(q , "\nplot <- ", p), file.path(dir, "brinton_outcomes", "plotup.R"), append = TRUE)
      write(paste0("grid::grid.draw(egg::set_panel_size(plot, width  = unit(", GAwidth,", 'cm'), height = unit(", GAheight, ", 'cm')))"), file.path(dir, "brinton_outcomes", "plotup.R"), append = TRUE)
      rmarkdown::render(file.path(dir, "brinton_outcomes", "plotup.R"),"html_document", envir=my_env)
      pander::openFileInOS(file.path(dir, "brinton_outcomes", "plotup.html"))
      }
    }
}

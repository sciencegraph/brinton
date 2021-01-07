#' @noRd
add_density_1D <- function(a, b) {
  a$b <- unlist(a[, b])
  if (length(unique(stats::na.omit(a$b))) == 1) {
    dens <- 1/length(a$b)
  }
  else if (any(is.na(a$b)) == FALSE) {
    dens <- sm::sm.density(a$b, eval.points=a$b, display='none')$estimate
  }
  else if (all(is.na(a$b)) == FALSE) {
    dens <- as.numeric(NA)
  }
  else {
    ind <- which(is.na(a$b) == TRUE)
    dens <- sm::sm.density(stats::na.omit(a$b), eval.points=stats::na.omit(a$b), display='none')$estimate
    for(i in 1:(length(ind))) dens <- append(dens, NA, after=(ind[i]+1)-2)
  }
  return(dens)
}

add_plots <- function(a, b) {
  write(
    paste0(
      "gridExtra::grid.arrange(",
      paste0(a, 1:b, collapse = ", "),
      ", ncol=5)"
    ),
    file.path(dir, "brinton_outcomes", "longplot.R"),
    append = TRUE
  )
}

add_label <- function(a, b) {
  char_types <-
    paste0(a, " = c('", paste0(b, collapse = "', '"), "')")
  write(
    paste0('cat("', char_types, '")'),
    file.path(dir, "brinton_outcomes", "longplot.R"),
    append = TRUE
  )
}

short_label <- function(df, var, n, m) {
  df$var <- unlist(df[,var])
  classes <- class(df$var)
  nivells <- levels(df$var)
  recoded <-ifelse(nchar(as.vector(df$var)) > n, paste0(substring(as.vector(df$var), 1, m), "..."), as.vector(df$var))
  if (is.ordered(df$var) == TRUE) {df$var <- factor(recoded, levels = ifelse(nchar(as.vector(nivells)) > n, paste0(substring(as.vector(nivells), 1, m), "..."), as.vector(nivells)), ordered = TRUE)}
  if (is.ordered(df$var) == FALSE & is.factor(df$var) == TRUE) {df$var <- factor(recoded, levels = ifelse(nchar(as.vector(nivells)) > n, paste0(substring(as.vector(nivells), 1, m), "..."), as.vector(nivells)), ordered = FALSE)}
  if (is.character(df$var) == TRUE) {df$var <- recoded}
  # df$var <- recoded
  # class(df$var) <- classes[1]
  return(df$var)
}

# borrowed from: https://slowkow.com/notes/ggplot2-color-by-density/
get_density <- function(x, y, ...) {
  dens <- MASS::kde2d(x, y, ...)
  ix <- findInterval(x, dens$x)
  iy <- findInterval(y, dens$y)
  ii <- cbind(ix, iy)
  return(dens$z[ii])
}

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

# borrowed from: https://slowkow.com/notes/ggplot2-color-by-density/
get_density <- function(x, y, ...) {
  dens <- MASS::kde2d(x, y, ...)
  ix <- findInterval(x, dens$x)
  iy <- findInterval(y, dens$y)
  ii <- cbind(ix, iy)
  return(dens$z[ii])
}

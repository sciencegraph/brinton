#' @noRd
add_blank <- function(x) {
  while (length(x) < 7) {
    x <- append(x, c("blank"), after = length(x))
    i = length(x)
    }
  return(x)
  }

add_plots <- function(a, b) {
  write(paste0("gridExtra::grid.arrange(", paste0(a, 1:b, collapse = ", "), ", ncol=5)"), file=paste0(getwd(), "\\longplot.R"), append = TRUE)
  }

add_label <- function(a, b) {
  char_types <- paste0(a, " = c('", paste0(b, collapse = "', '"), "')")
  write(paste0('cat("', char_types, '")'), file=paste0(getwd(), "\\longplot.R"), append=TRUE)
  }

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

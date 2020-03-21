#' @import ggplot2
#' @import glue
#' @importFrom gridExtra grid.arrange
#' @importFrom rmarkdown render
#' @importFrom RColorBrewer brewer.pal
#' @importFrom stats density
#' @importFrom stats reshape
#' @importFrom forcats fct_infreq
#' @importFrom lubridate is.instant
#' @importFrom pander openFileInOS
#' @importFrom utils head
utils::globalVariables(c("..level..", "..count..", "pp_dens", "reshape", "pp_id", "head"))

pp_theme <- function(base_size = 11,
                     base_family = "",
                     base_line_size = base_size / 22,
                     base_rect_size = base_size / 22){
  theme_minimal(base_size = base_size,
                base_family = base_family,
                base_line_size = base_line_size) %+replace%
    theme(
      axis.ticks=element_line(color="black"),
      panel.grid = element_line(colour = NA),
      axis.title = element_text(colour = "#333333", size=base_size/1.2),
      complete = TRUE
    )
}

theme_blank <-  theme(axis.text= element_blank(),
                      axis.ticks= element_blank(),
                      axis.title = element_blank(),
                      panel.border = element_rect(colour = NA),
                      panel.grid.major = element_line(colour = NA),
                      panel.grid.minor = element_line(colour = NA),
                      legend.key = element_rect(fill = NA, colour = NA),
                      strip.background = element_rect(fill = NA, colour = NA)
                      )

amb.x <- theme(axis.text.x  = element_blank(),
               axis.title.x = element_blank(), #element_text(color=NA),
               axis.ticks.x = element_blank() #element_line(color=NA)
               )

amb.y <- theme(axis.text.y  = element_blank(),
               axis.title.y = element_blank(), #element_text(color=NA),
               axis.ticks.y = element_blank() #element_line(color=NA)
               )

amb.z <- theme(legend.position="none")

# custom palettes

scl_gry <- colorRampPalette(c("#E5E5E5", "#000000"))
scl_col_cont <- colorRampPalette(rev(RColorBrewer::brewer.pal(4, "Spectral")))
scl_col_disc <- colorRampPalette(rev(RColorBrewer::brewer.pal(9, "Set1")))
p_scale_gray_l <- scale_color_gradientn(colours = scl_gry(2))
p_scale_color_l <- scale_color_gradientn(colours = scl_col_cont(3))
p_scale_gray_a <- scale_fill_gradientn(colours = scl_gry(2))
p_scale_color_a <- scale_fill_gradientn(colours = scl_col_cont(3))

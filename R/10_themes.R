#' @import ggplot2
#' @import glue
#' @import patchwork
#' @importFrom gridExtra grid.arrange
#' @importFrom rmarkdown render
#' @importFrom RColorBrewer brewer.pal
#' @importFrom stats density
#' @importFrom stats reshape
#' @importFrom stats chisq.test
#' @importFrom stats ftable
#' @importFrom forcats fct_infreq
#' @importFrom lubridate is.instant
#' @importFrom pander openFileInOS
#' @importFrom utils head
#' @importFrom grDevices colorRampPalette
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
      legend.text = element_text(size = 9, face = "plain", color = "gray30"),
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

rot.x <- theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

# custom palettes

scl_gry         <- colorRampPalette(c("#E5E5E5", "#000000"))
scl_col_cont    <- colorRampPalette(rev(RColorBrewer::brewer.pal(4, "Spectral")))
scl_col_seq     <- colorRampPalette(RColorBrewer::brewer.pal(3, "YlOrBr"))
scl_col_value   <- colorRampPalette(rev(RColorBrewer::brewer.pal(11, "RdYlGn")))
# scl_col_disc  <- colorRampPalette(rev(RColorBrewer::brewer.pal(9, "Set1")))

# discontinuous palettes
scl_col_disc_l  <- scale_color_brewer(type = "qual", palette = "Set1")
scl_col_disc_a  <- scale_fill_brewer(type = "qual", palette = "Set1")
scl_viridis_l  <- scale_colour_viridis_d(direction = -1)
scl_viridis_ld  <- scale_colour_viridis_c(direction = -1)
# scl_col_seq_l  <- scale_color_brewer(type = "seq", palette = "YlOrBr")
scl_viridis_a  <- scale_fill_viridis_d(direction = -1)
scl_viridis_ad  <- scale_fill_viridis_c(direction = -1)
# scl_col_seq_a  <- scale_fill_brewer(type = "seq", palette = "YlOrBr")
scl_gray_disc_l <- scale_color_grey(start = 0.8, end = 0.2) #scale_color_brewer(type = "seq", palette = "Greys")
scl_gray_disc_a <- scale_fill_grey(start = 0.8, end = 0.2) #scale_fill_brewer(type = "seq", palette = "Greys")

# scl_guid_disc_a  <- scale_fill_brewer(type = "qual", palette = "Set1")
scale_fill_brewer(type = "seq", palette = "YlOrBr")

# continuous grayscale palettes
p_scale_gray_l  <- scale_color_gradientn(colours = scl_gry(2))
p_scale_gray_l_p  <- scale_color_gradientn(colours = scl_gry(2), labels = scales::label_percent())
p_scale_gray_a  <- scale_fill_gradientn(colours = scl_gry(2))
p_scale_gray_a_p  <- scale_fill_gradientn(colours = scl_gry(2), labels = scales::label_percent())

# continuous color palettes for density
p_scale_color_l <- scale_color_gradientn(colours = scl_col_cont(3))
p_scale_color_a <- scale_fill_gradientn(colours = scl_col_cont(3))

# continuous color palettes for sequences
p_scale_seq_l   <- scale_color_gradientn(colours = scl_col_seq(2))
p_scale_seq_a   <- scale_fill_gradientn(colours = scl_col_seq(2))

# continuous color palettes for values
p_scale_value_l <- scale_color_gradientn(colours = colorRampPalette(rev(RColorBrewer::brewer.pal(4, 'BrBG')))(2))
p_scale_value_l_p <- scale_color_gradientn(colours = colorRampPalette(rev(RColorBrewer::brewer.pal(4, 'BrBG')))(2), labels = scales::label_percent())
p_scale_value_a <- scale_fill_gradientn(colours = colorRampPalette(rev(RColorBrewer::brewer.pal(4, 'BrBG')))(2))
p_scale_value_a_p <- scale_fill_gradientn(colours = colorRampPalette(rev(RColorBrewer::brewer.pal(4, 'BrBG')))(2), labels = scales::label_percent())
# p_scale_value_a <- scale_fill_gradientn(colours = scl_col_value(3))
# p_scale_limits_l <- scale_color_gradientn(colours = scl_col_value(3), limits = c(-abs(max(df[,1])), abs(max(df[,2]))))
# p_scale_limits_a <- scale_fill_gradientn(colours = scl_col_value(3), limits = c(-abs(max(df[,1])), abs(max(df[,2]))))




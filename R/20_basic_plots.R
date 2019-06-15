
# basic plots


blank <- function(pp_df,
                  pp_var)  {
  pp_df$pp_var <- unlist(pp_df[, pp_var])

  p_plot <- ggplot(pp_df,
                   aes_string(x=pp_var),
                   environment = environment()) +
    labs(x=names(pp_df[pp_var])) +
    pp_theme() +
    theme(axis.title=element_blank(),
          axis.text=element_blank(),
          axis.ticks=element_blank(),
          panel.grid = element_line(colour = NA)) +
    geom_blank()
}

pp_3uniaxial <- function(pp_df,
                         pp_var,
                         pp_size = 0.5)  {
  pp_df$pp_var <- unlist(pp_df[, pp_var])

  p_plot <- ggplot(pp_df,
                   aes_string(y=pp_var),
                   environment = environment()) +
    labs(x=names(pp_df[pp_var])) +
    geom_boxplot(aes(x=1), width = 0.5, size=0.3*pp_size) +
    geom_point(aes(x=2), size=pp_size*4, alpha=.1) +
    geom_violin(aes(x=3), size=0.3*pp_size) +
    scale_x_continuous(breaks = c(1, 2, 3), labels = c("box", "dot", "violin")) +
    theme(axis.title.y=element_blank()) +
    coord_flip() +
    pp_theme()
}

pp_density <- function(pp_df,
                       pp_var,
                       pp_size = 0.5,
                       pp_color = NULL)  {
  pp_df$pp_var <- unlist(pp_df[, pp_var])

  p_labs <- labs(x=names(pp_df[pp_var]))
  p_plot <- ggplot(pp_df, aes_string(x=pp_var), environment = environment()) + p_labs + pp_theme()
  if (is.null(pp_color)) {
    p_plot + geom_density(size=pp_size)
  } else if (pp_color == "black") {
    p_plot + geom_density(size=pp_size, fill = "black")
  } else {stop(warning_general)}
}


pp_violin <- function(pp_df,
                      pp_var,
                      pp_size = 0.5,
                      pp_color = NULL, ...)  {
  pp_df$pp_var <- unlist(pp_df[, pp_var])

  p_labs <- labs(x=names(pp_df[pp_var]))
  p_plot <- ggplot(pp_df, aes_string(x=0, y=pp_var), environment = environment()) + p_labs + pp_theme() + amb.y + coord_flip()
  if (is.null(pp_color)) {
    p_plot + geom_violin(size=pp_size)
  } else if (pp_color == "black") {
    p_plot + geom_violin(fill = "black", size=pp_size)
  } else {stop(warning_general)}
}

pp_boxplot <- function(pp_df,
                       pp_var,
                       pp_size = 0.5,
                       pp_color = NULL, ...)  {
  pp_df$pp_var <- unlist(pp_df[, pp_var])

  p_labs  <- labs(x=names(pp_df[pp_var]))
  p_plot  <- ggplot(pp_df, aes_string(x=0, y=pp_var), environment = environment()) + p_labs + pp_theme()
  p_box   <- geom_boxplot(size=pp_size)
  p_box_f <- geom_boxplot(size=pp_size, fill = "black", alpha = 0.5)

  if (is.null(pp_color)) {
    p_plot + p_box + amb.y + coord_flip()
  } else if (pp_color == "black") {
    p_plot + p_box_f + amb.y + coord_flip()
  } else {stop(warning_general)}
}


qqplot <- function (vec,
                    pp_size = 0.5) {
  y <- stats::quantile(vec[!is.na(vec)], c(0.25, 0.75))
  x <- stats::qnorm(c(0.25, 0.75))
  slope <- diff(y)/diff(x)
  int <- y[1L] - slope * x[1L]
  d <- data.frame(resids = vec)

  ggplot(d, aes_(sample = ~resids)) +
    stat_qq(size=pp_size) +
    geom_abline(slope = slope, intercept = int, size=pp_size) +
    pp_theme()
}

pp_1DD_scatterplot <- function(pp_df,
                               pp_var,
                               pp_size = 1,
                               pp_coord = "xy",
                               pp_color = "black",
                               pp_smooth = "false"
)
{
  pp_df$pp_var <- unlist(pp_df[, pp_var])

  p_labs          <- labs(y=names(pp_df[pp_var]), x="seq")
  p_plot          <- ggplot(pp_df, aes_string(y=pp_var), environment = environment()) + p_labs + pp_theme()
  pp_df$pp_dens   <- add_density_1D(pp_df, pp_var)
  p_point         <- geom_point(data = pp_df, aes(x=seq_along(pp_var)), size=pp_size)
  p_point_c       <- geom_point(data = pp_df, aes_(x=~seq_along(pp_var), color=~pp_dens), size=pp_size)
  p_smooth        <- geom_smooth(aes(x=seq_along(pp_var)), method = "loess", size=0.5)

  if (pp_coord == "yx" & pp_color == "black" & pp_smooth == "false") {
    p_plot + p_point
  } else if (pp_coord == "yx" & pp_color == "black" & pp_smooth == "true") {
    p_plot + p_point + p_smooth
  } else if (pp_coord == "yx" & pp_color == "bw" & pp_smooth == "false") {
    p_plot + p_point_c + p_scale_gray_l + amb.z
  } else if (pp_coord == "yx" & pp_color == "bw" & pp_smooth == "true") {
    p_plot + p_point_c + p_smooth + p_scale_gray_l + amb.z
  } else if (pp_coord == "yx" & pp_color == "color" & pp_smooth == "false") {
    p_plot + p_point_c + p_scale_color_l + amb.z
  } else if (pp_coord == "yx" & pp_color == "color" & pp_smooth == "true") {
    p_plot + p_point_c + p_smooth + p_scale_color_l + amb.z
  } else if (pp_coord == "xy" & pp_color == "black") {
    p_plot + p_point + coord_flip()
  } else if (pp_coord == "xy" & pp_color == "bw") {
    p_plot + p_point_c + p_scale_gray_l + coord_flip() + amb.z
  } else if (pp_coord == "xy" & pp_color == "color") {
    p_plot + p_point_c + p_scale_color_l + coord_flip() + amb.z
  } else {stop(warning_general)}
}

pp_histogram <- function(pp_df,
                         pp_var,
                         pp_color = "black",
                         pp_binwidth = 1)  {
  pp_df$pp_var <- unlist(pp_df[, pp_var])

  p_labs   <- labs(x=names(pp_df[pp_var]))
  p_plot   <- ggplot(pp_df, aes_string(x=pp_var), environment = environment()) + p_labs + pp_theme()
  p_hist   <- geom_histogram(fill="black", binwidth = pp_binwidth, center = 0)
  p_hist_c <- geom_histogram(aes_(fill=~..count..), binwidth = pp_binwidth, center = 0)

  if (pp_color == "black") {
    p_plot + p_hist
  } else if (pp_color == "bw") {
    p_plot + p_hist_c + p_scale_gray_a + amb.z
  } else if (pp_color == "color") {
    p_plot + p_hist_c + p_scale_color_a + amb.z
  } else {stop(warning_general)}
}

pp_bargraph <- function(pp_df,
                        pp_var,
                        pp_color = "black",
                        pp_coord = "yx",
                        pp_size = 0.75
)  {
  pp_df$pp_var <- unlist(pp_df[, pp_var])

  p_labs     <- labs(x=names(pp_df[pp_var]))
  p_plot     <- ggplot(pp_df, aes_string(x=pp_var), environment = environment()) + p_labs + pp_theme()
  p_bar      <- geom_bar(stat="count", width=pp_size, fill="black", color="black", position = "identity")
  p_bar_c    <- geom_bar(aes_(fill=~..count..), stat="count", width=pp_size, position = "identity")
  p_bar2     <- geom_bar(stat="count", width=pp_size, fill="black")
  p_bar2_c   <- geom_bar(aes_(fill=~..count..), stat="count", width=pp_size)

  if (pp_coord == "xy" & pp_color == "black") {
    p_plot + p_bar
  } else if (pp_coord == "xy" & pp_color == "bw") {
    p_plot + p_bar_c + p_scale_gray_a + amb.z
  } else if (pp_coord == "xy" & pp_color == "color") {
    p_plot + p_bar_c + p_scale_color_a + amb.z
  } else if (pp_coord == "yx" & pp_color == "black") {
    p_plot + p_bar2 + coord_flip()
  } else if (pp_coord == "yx" & pp_color == "bw") {
    p_plot + p_bar2_c + p_scale_gray_a + coord_flip() + amb.z
  } else if (pp_coord == "yx" & pp_color == "color") {
    p_plot + p_bar2_c + p_scale_color_a + coord_flip() + amb.z
  } else {stop(warning_general)}
}



pp_1DD_binnedpointgraph <- function(pp_df,
                                    pp_var,
                                    pp_size = 1,
                                    pp_coord = "xy",
                                    pp_color = "black")
{
  pp_df$pp_var <- unlist(pp_df[, pp_var])

  p_labs    <- labs(y=names(pp_df[pp_var]), x="seq")
  p_plot    <- ggplot(pp_df, aes_string(y=pp_var), environment = environment()) + p_labs + pp_theme()
  p_point   <- geom_point(data = pp_df, aes(x=seq_along(pp_var)), size=pp_size, stat= "bin2d")
  p_point_c <- geom_point(data = pp_df, aes_(x=~seq_along(pp_var), color=~..count..), size=pp_size, stat= "bin2d")

  if (pp_coord == "yx" & pp_color == "black") {
    p_plot + p_point + amb.z
  } else if (pp_coord == "yx" & pp_color == "bw") {
    p_plot + p_point_c + p_scale_gray_l + amb.z
  } else if (pp_coord == "yx" & pp_color == "color") {
    p_plot + p_point_c + p_scale_color_l + amb.z
  } else if (pp_coord == "xy" & pp_color == "black") {
    p_plot + p_point + amb.z + coord_flip()
  } else if (pp_coord == "xy" & pp_color == "bw") {
    p_plot + p_point_c + p_scale_gray_l + coord_flip() + amb.z
  } else if (pp_coord == "xy" & pp_color == "color") {
    p_plot + p_point_c + p_scale_color_l + coord_flip() + amb.z
  } else {stop(warning_general)}
}

pp_1DD_areagraph <- function(pp_df,
                             pp_var,
                             ...,
                             pp_size = 1,
                             pp_trans = "rect",
                             pp_coord = "xy")
{
  pp_df$pp_var <- unlist(pp_df[, pp_var])

  p_labs <- labs(y=names(pp_df[pp_var]), x="seq")
  p_plot <- ggplot(pp_df, aes_string(y=pp_var), environment = environment()) + p_labs + pp_theme()
  p_area <- geom_area(aes(x=seq_along(pp_var)), fill = "black")
  p_areastep <- geom_bar(aes(x=seq_along(pp_var)), fill = "black", width = 1, stat = "identity")

  if (pp_coord == "xy" & pp_trans == "rect") {
    p_plot + p_area
  } else if (pp_coord == "yx" & pp_trans == "rect") {
    p_plot + p_area + coord_flip()
  } else if (pp_coord == "xy" & pp_trans == "step") {
    p_plot + p_areastep
  } else if (pp_coord == "yx" & pp_trans == "step") {
    p_plot + p_areastep + coord_flip()
  } else {stop(warning_general)}
}

pp_1DD_linegraph <- function(pp_df,
                             pp_var,
                             ...,
                             pp_size = 1,
                             pp_coord = "xy",
                             pp_trans = "rect",
                             pp_points = FALSE)
{
  pp_df$pp_var <- unlist(pp_df[, pp_var])

  p_labs    <- labs(y=names(pp_df[pp_var]), x="seq")
  p_plot    <- ggplot(pp_df, aes_string(y=pp_var), environment = environment()) + p_labs + pp_theme()
  p_path    <- geom_path(aes(x=seq_along(pp_var), group=1), size=0.5*pp_size)
  p_path_s  <- geom_step(aes(x=seq_along(pp_var), group=1), size=0.5*pp_size)
  p_point   <- geom_point(data = pp_df, aes(x=seq_along(pp_var), group=1), size=3*pp_size)

  if (pp_coord == "xy" & pp_trans == "rect" & pp_points == FALSE) {
    p_plot + p_path
  } else if (pp_coord == "yx" & pp_trans == "rect" & pp_points == FALSE) {
    p_plot + p_path + coord_flip()
  } else if (pp_coord == "xy" & pp_trans == "step" & pp_points == FALSE) {
    p_plot + p_path_s
  } else if (pp_coord == "yx" & pp_trans == "step" & pp_points == FALSE) {
    p_plot + p_path_s + coord_flip()
  } else if (pp_coord == "xy" & pp_trans == "rect" & pp_points == TRUE) {
    p_plot + p_path + p_point
  } else if (pp_coord == "yx" & pp_trans == "rect" & pp_points == TRUE) {
    p_plot + p_path + p_point + coord_flip()
  } else if (pp_coord == "xy" & pp_trans == "step" & pp_points == TRUE) {
    p_plot + p_path_s + p_point
  } else if (pp_coord == "yx" & pp_trans == "step" & pp_points == TRUE) {
    p_plot + p_path_s + p_point + coord_flip()
  } else {stop(warning_general)}
}

pp_1DD_linerange <- function(pp_df,
                             pp_var,
                             ...,
                             pp_size = 1,
                             pp_coord = "xy")
{
  pp_df$pp_var <- unlist(pp_df[, pp_var])

  p_labs      <- labs(y=names(pp_df[pp_var]), x="seq")
  p_plot      <- ggplot(pp_df, aes_string(y=pp_var), environment = environment()) + p_labs + pp_theme()
  p_path      <- geom_path(aes(x=seq_along(pp_var)), size=0.2*pp_size)
  p_point     <- geom_point(data = pp_df, aes(x=seq_along(pp_var)), size=3*pp_size)

  if (pp_coord == "xy") {
    p_plot + p_path + p_point
  } else if (pp_coord == "yx") {
    p_plot + p_path + p_point + coord_flip()
  } else {stop(warning_general)}
}

pp_1DD_pointgraph <- function(pp_df,
                              pp_var,
                              ...,
                              pp_size = 1,
                              pp_coord = "xy")
{
  pp_df$pp_var <- unlist(pp_df[, pp_var])

  p_labs <- labs(y=names(pp_df[pp_var]), x="seq")
  p_plot <- ggplot(pp_df, aes_string(y=pp_var), environment = environment()) + p_labs + pp_theme()
  p_point <- geom_point(aes(x=seq_along(pp_var)), size=3*pp_size)

  if (pp_coord == "xy") {
    p_plot + p_point
  } else if (pp_coord == "yx") {
    p_plot + p_point + coord_flip()
  } else {stop(warning_general)}
}

pp_1DD_tileplot <- function(pp_df,
                            pp_var,
                            ...,
                            pp_size = 0.5,
                            pp_coord = "xy")
{
  pp_df$pp_var <- unlist(pp_df[, pp_var])

  p_labs  <- labs(y=names(pp_df[pp_var]), x="seq")
  p_plot <- ggplot(pp_df, aes_string(y=pp_var), environment = environment()) + p_labs + pp_theme()
  p_tile  <- geom_tile(aes(x=seq_along(pp_var)), size=pp_size)

  if (pp_coord == "xy") {
    p_plot + p_tile
  } else if (pp_coord == "yx") {
    p_plot + p_tile + coord_flip()
  } else {stop(warning_general)}
}


pp_1DD_heatmap <- function(pp_df,
                           pp_var,
                           pp_coord = "xy",
                           pp_color = "black")
{
  pp_df$pp_var <- unlist(pp_df[, pp_var])

  p_labs    <- labs(y=names(pp_df[pp_var]), x="seq")
  p_plot    <- ggplot(pp_df, aes_string(y=pp_var), environment = environment()) + p_labs + pp_theme() + amb.z
  p_bin2d   <- geom_bin2d(data = pp_df, aes(x=seq_along(pp_var)), fill = "black")
  p_bin2d_c <- geom_bin2d(data = pp_df, aes(x=seq_along(pp_var)))

  if (pp_coord == "yx" & pp_color == "black") {
    p_plot + p_bin2d
  } else if (pp_coord == "yx" & pp_color == "bw") {
    p_plot + p_bin2d_c + p_scale_gray_a
  } else if (pp_coord == "yx" & pp_color == "color") {
    p_plot + p_bin2d_c + p_scale_color_a
  } else if (pp_coord == "xy" & pp_color == "black") {
    p_plot + p_bin2d + coord_flip()
  } else if (pp_coord == "xy" & pp_color == "bw") {
    p_plot + p_bin2d_c + p_scale_gray_a + coord_flip()
  } else if (pp_coord == "xy" & pp_color == "color") {
    p_plot + p_bin2d_c + p_scale_color_a + coord_flip()
  } else {stop(warning_general)}

}


pp_binned_stripegraph <- function(pp_df,
                                  pp_var,
                                  pp_color = "black",
                                  pp_binwidth = 1)  {
  pp_df$pp_var <- unlist(pp_df[, pp_var])

  p_labs    <- labs(x=names(pp_df[pp_var]))
  p_plot    <- ggplot(pp_df, aes_string(x=pp_var), environment = environment()) + pp_theme() + amb.y
  p_bin2d   <- geom_bin2d(aes(y=1), binwidth = c(pp_binwidth, 1), fill="black")
  p_bin2d_c <- geom_bin2d(aes(y=1), binwidth = c(pp_binwidth, 1))

  if (pp_color == "black") {
    p_plot + p_bin2d
  } else if (pp_color == "bw") {
    p_plot + p_bin2d_c + p_scale_gray_a + amb.z
  } else if (pp_color == "color") {
    p_plot + p_bin2d_c + p_scale_color_a + amb.z
  } else {stop(warning_general)}
}

pp_stripegraph <- function(pp_df,
                           pp_var,
                           pp_color = "black")  {
  pp_df$pp_var <- unlist(pp_df[, pp_var])
  pp_df$pp_dens <- add_density_1D(pp_df, pp_var)

  p_labs <- labs(x=names(pp_df[pp_var]))
  p_plot <- ggplot(pp_df, aes_string(x=pp_var), environment = environment()) + scale_y_continuous(limits = c(0,1)) + pp_theme() + amb.y
  p_lrange <- geom_linerange(aes(ymin=0, ymax=1))
  p_lrange_c <- geom_linerange(aes_(ymin=0, ymax=1, color=~pp_dens))

  if (pp_color == "black") {
    p_plot + p_lrange
  } else if (pp_color == "bw") {
    p_plot + p_lrange_c + p_scale_gray_l + amb.z
  } else if (pp_color == "color") {
    p_plot + p_lrange_c + p_scale_color_l + amb.z
  } else {stop(warning_general)}
}




# pp_1DD_stripegraph <- function(pp_df,
#                               pp_var,
#                               ...,
#                               pp_coord = "xy")
# {
#   pp_df$pp_var <- unlist(pp_df[, pp_var])
#   if (pp_coord == "xy") {
#
#     pp_plot <- ggplot(pp_df,
#                       aes_string(y=pp_var),
#                       environment = environment()) +
#       labs(y=names(pp_df[pp_var]), x="seq") +
#       geom_tile(aes(x=seq_along(pp_var))) +
#       pp_theme()
#   }
#   else if (pp_coord == "yx") {
#
#     pp_plot <- ggplot(pp_df,
#                       aes_string(y=pp_var),
#                       environment = environment()) +
#       labs(y=names(pp_df[pp_var]), x="seq") +
#       geom_tile(aes(x=seq_along(pp_var))) +
#       coord_flip() +
#       pp_theme()
#   }
#   else {stop(warning_coord)}
# }
#
#
#
#
# pp_dotchart <- function(pp_df,
#                         pp_var,
#                         pp_color = "black", ...)  {
#   pp_df$pp_var <- unlist(pp_df[, pp_var])
#
#   pp_plot <- ggplot(pp_df,
#                     aes_string(x=pp_var),
#                     environment = environment()) +
#     labs(x=names(pp_df[pp_var])) +
#     pp_theme()
#   if (pp_color == "black") {
#     pp_plot +
#       geom_point(stat="count",
#                  size=3,
#                  fill="black") +
#       coord_flip()
#   } else if (pp_color == "bw") {
#     pp_plot +
#       geom_point(aes(color=..count..),
#                  stat="count",
#                  size=3) +
#       scale_color_gradientn(colours = scl_gry(2)) +
#       coord_flip() +
#       amb.z
#   } else if (pp_color == "color") {
#     pp_plot +
#       geom_point(aes(color=..count..),
#                  stat="count",
#                  size=3) +
#       scale_color_gradientn(colours = scl_col_cont(3)) +
#       coord_flip() +
#       amb.z
#   } else {stop(warning_color)}
# }
#
# pp_bubblechart <- function(pp_df,
#                         pp_var,
#                         pp_color = "black", ...)  {
#   pp_df$pp_var <- unlist(pp_df[, pp_var])
#
#   pp_plot <- ggplot(pp_df,
#                     aes_string(x=pp_var),
#                     environment = environment()) +
#     labs(x=names(pp_df[pp_var])) +
#     pp_theme()
#   if (pp_color == "black") {
#     pp_plot +
#       geom_point(aes(size=..count..),
#                  stat="count",
#                  fill="black") +
#       coord_flip() +
#       amb.z
#   } else if (pp_color == "bw") {
#     pp_plot +
#       geom_point(aes(color=..count..,
#                      size=..count..),
#                  stat="count") +
#       scale_color_gradientn(colours = scl_gry(2)) +
#       coord_flip() +
#       amb.z
#   } else if (pp_color == "color") {
#     pp_plot +
#       geom_point(aes(color=..count..,
#                      size=..count..),
#                  stat="count") +
#       scale_color_gradientn(colours = scl_col_cont(3)) +
#       coord_flip() +
#       amb.z
#   } else {stop(warning_color)}
# }
#
# pp_lollipopchart <- function(pp_df,
#                              pp_var,
#                              pp_color = "black", ...)  {
#   pp_df$pp_var <- unlist(pp_df[, pp_var])
#
#   pp_plot <- ggplot(pp_df,
#                     aes_string(x=pp_var),
#                     environment = environment()) +
#     labs(x=names(pp_df[pp_var])) +
#     pp_theme()
#   if (pp_color == "black") {
#     pp_plot +
#       geom_bar(stat="count",
#                fill="black",
#                width=0.1) +
#       geom_point(stat="count",
#                  size=3,
#                  fill="black") +
#       coord_flip()
#   } else if (pp_color == "bw") {
#     pp_plot +
#       geom_bar(aes(fill=..count..),
#                stat="count",
#                width=0.1) +
#       geom_point(aes(color=..count..),
#                  stat="count",
#                  size=3) +
#       scale_color_gradientn(colours = scl_gry(2)) +
#       scale_fill_gradientn(colours = scl_gry(2)) +
#       coord_flip() +
#       amb.z
#   } else if (pp_color == "color") {
#     pp_plot +
#       geom_bar(aes(fill=..count..),
#                stat="count",
#                width=0.1) +
#       geom_point(aes(color=..count..),
#                  stat="count",
#                  size=3) +
#       scale_color_gradientn(colours = scl_col_cont(3)) +
#       scale_fill_gradientn(colours = scl_col_cont(3)) +
#       coord_flip() +
#       amb.z
#   } else {stop(warning_color)}
# }
#
# pp_freq_stripegraph <- function(pp_df,
#                                   pp_var,
#                                   pp_color = "black",
#                                   pp_binwidth = 1, ...)  {
#   pp_df$pp_var <- unlist(pp_df[, pp_var])
#
#   pp_plot <- ggplot(pp_df,
#                     aes_string(x=pp_var),
#                     environment = environment()) +
#     labs(x=names(pp_df[pp_var])) +
#     pp_theme()
#   if (pp_color == "black") {
#     pp_plot +
#       geom_bar(aes(fill=..count..),
#                position = "fill",
#                width=0.75,
#                fill="black") +
#       coord_flip() +
#       amb.x
#   } else if (pp_color == "bw") {
#     pp_plot +
#       geom_bar(aes(fill=..count..),
#                position = "fill", width=0.75) +
#       scale_fill_gradientn(colours = scl_gry(2)) +
#       coord_flip() +
#       amb.x +
#       amb.z
#   } else if (pp_color == "color") {
#     pp_plot +
#       geom_bar(aes(fill=..count..),
#                position = "fill",
#                width=0.75) +
#       scale_fill_gradientn(colours = scl_col_cont(3)) +
#       coord_flip() +
#       amb.x +
#       amb.z
#   } else {stop(warning_color)}
# }
#
# pp_bargraph_char <- function(pp_df,
#                         pp_var,
#                         pp_color = "black", ...)  {
#   pp_df$pp_var <- unlist(pp_df[, pp_var])
#
#   trans_df <- as.data.frame(sort(table(pp_df$pp_var), decreasing = TRUE))
#   # names(trans_df) <- c("Var1", "pp_freq")
#   pp_plot <- ggplot(trans_df,
#                     aes_string(x=trans_df[,1], y=trans_df[,2]),
#                     environment = environment()
#                     ) +
#     labs(y="count") +
#     amb.y +
#     pp_theme()
#   if (pp_color == "black") {
#     pp_plot +
#       geom_bar(stat="identity",
#                width=0.75,
#                fill="black") +
#       coord_flip()
#   } else if (pp_color == "bw") {
#     pp_plot +
#       geom_bar(aes(fill=trans_df[,2]),
#                stat="identity",
#                width=0.75) +
#       scale_fill_gradientn(colours = scl_gry(2)) +
#       coord_flip() +
#       amb.z
#   } else if (pp_color == "color") {
#     pp_plot +
#       geom_bar(aes(fill=trans_df[,2]),
#                stat="identity",
#                width=0.75) +
#       scale_fill_gradientn(colours = scl_col_cont(3)) +
#       coord_flip() +
#       amb.z
#   } else {stop(warning_color)}
# }
#
# pp_bargraph_date <- function(pp_df,
#                              pp_var,
#                              ...,
#                              pp_color = "black")  {
#   pp_df$pp_var <- unlist(pp_df[, pp_var])
#
#   trans_df <- as.data.frame(table(pp_df$pp_var))
#   # names(trans_df) <- c("Var1", "pp_freq")
#   pp_plot <- ggplot(trans_df,
#                     aes_string(x=trans_df[,1], y=trans_df[,2]),
#                     environment = environment()
#   ) +
#     labs(y="count") +
#     amb.y +
#     pp_theme()
#   if (pp_color == "black") {
#     pp_plot +
#       geom_bar(stat="identity",
#                width=0.75,
#                fill="black") +
#       coord_flip()
#   } else if (pp_color == "bw") {
#     pp_plot +
#       geom_bar(aes(fill=trans_df[,2]),
#                stat="identity",
#                width=0.75) +
#       scale_fill_gradientn(colours = scl_gry(2)) +
#       coord_flip() +
#       amb.z
#   } else if (pp_color == "color") {
#     pp_plot +
#       geom_bar(aes(fill=trans_df[,2]),
#                stat="identity",
#                width=0.75) +
#       scale_fill_gradientn(colours = scl_col_cont(3)) +
#       coord_flip() +
#       amb.z
#   } else {stop(warning_color)}
# }
#
# pp_1D_pointgraph <- function(pp_df,
#                              pp_var,
#                              pp_color = "black", ...)  {
#   pp_df$pp_var <- unlist(pp_df[, pp_var])
#   pp_df$pp_dens <- sm::sm.density(pp_df$pp_var,eval.points=pp_df$pp_var, display="none")$estimate
#
#   pp_plot <- ggplot(pp_df,
#                     aes_string(x=pp_var),
#                     environment = environment()) +
#     labs(x=names(pp_df[pp_var])) +
#     pp_theme()
#   if (pp_color == "black") {
#     pp_plot +
#       geom_point(aes(y=0)) +
#       amb.y
#   } else if (pp_color == "bw") {
#     pp_plot + geom_point(aes(y=0,
#                              color=pp_dens)) +
#       scale_color_gradientn(colours = scl_gry(2)) +
#       amb.y +
#       amb.z
#   } else if (pp_color == "color") {
#     pp_plot + geom_point(aes(y=0,
#                              color=pp_dens)) +
#       scale_color_gradientn(colours = scl_col_cont(3)) +
#       amb.y +
#       amb.z
#   } else {stop(warning_color)}
# }
#
# pp_matrix <- function(pp_df,
#                       pp_var)  {
#   pp_df$pp_var <- unlist(pp_df[, pp_var])
#
#   pp_plot <- ggplot(pp_df,
#                     aes_string(y=pp_var),
#                     environment = environment()) +
#     labs(y=names(pp_df[pp_var]), x="seq") +
#     geom_tile(aes(x=seq_along(pp_var)), size=0.5) +
#     # theme(axis.title.y=element_blank()) +
#     coord_flip() +
#     pp_theme()
# }

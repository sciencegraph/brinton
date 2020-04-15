
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

blank2 <- function(pp_df,
                  pp_var1,
                  pp_var2)  {
  pp_df$pp_var1 <- unlist(pp_df[, pp_var1])
  pp_df$pp_var2 <- unlist(pp_df[, pp_var2])

  p_plot <- ggplot(pp_df,
                   aes_string(x=pp_var1),
                   environment = environment()) +
    labs(x=names(pp_df[pp_var1])) +
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

pp_ecdf <- function(pp_df,
                      pp_var,
                      pp_size = 0.5,
                      pp_trans = "point")  {
  pp_df$pp_var <- unlist(pp_df[, pp_var])

  p_labs <- labs(x=names(pp_df[pp_var]), y="p")
  p_plot <- ggplot(pp_df, aes_string(x=pp_var), environment = environment()) + p_labs + pp_theme()
  if (pp_trans == "point") {
    p_plot + stat_ecdf(geom = "point")
  } else if (pp_trans == "rect") {
    p_plot + stat_ecdf(geom = "line")
  } else if (pp_trans == "step") {
    p_plot + stat_ecdf(geom = "step")
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
    labs(y=names(pp_df[pp_var]), x="theoretical") +
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
  # p_point_c       <- geom_point(data = pp_df, aes_(x=~seq_along(pp_var), color=~pp_dens), size=pp_size)
  p_point_c       <- geom_point(data = pp_df, aes_(x=~seq_along(pp_var), color=~pp_var), size=pp_size)
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
                         pp_size = 1,
                         pp_geom = "bar",
                         pp_binwidth = 1)  {
  pp_df$pp_var <- unlist(pp_df[, pp_var])

  p_labs   <- labs(x=names(pp_df[pp_var]))
  p_plot   <- ggplot(pp_df, aes_string(x=pp_var), environment = environment()) + p_labs + pp_theme()
  p_freq   <- geom_line(stat = "bin", binwidth = pp_binwidth, center = 0, size = 0.5*pp_size)
  p_dots   <- geom_dotplot(binwidth = pp_binwidth, dotsize = 0.85, stackratio = 1/0.85)
  p_hist   <- geom_histogram(fill="black", color="black", binwidth = pp_binwidth, center = 0)
  p_hist_c <- geom_histogram(aes_(fill=~..count.., color=~..count..), binwidth = pp_binwidth, center = 0)

  if (pp_color == "black" & pp_geom == "bar") {
    p_plot + p_hist
  } else if (pp_color == "bw" & pp_geom == "bar") {
    p_plot + p_hist_c + p_scale_gray_a + p_scale_gray_l + amb.z
  } else if (pp_color == "color" & pp_geom == "bar") {
    p_plot + p_hist_c + p_scale_color_a + p_scale_color_l + amb.z
  } else if (pp_color == "black" & pp_geom == "line") {
    p_plot + p_freq
  } else if (pp_color == "black" & pp_geom == "dot") {
    p_plot + p_dots + amb.y
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
                             pp_coord = "xy",
                             pp_color = "black")
{
  pp_df$pp_var <- unlist(pp_df[, pp_var])

  p_labs <- labs(y=names(pp_df[pp_var]), x="seq")
  p_plot <- ggplot(pp_df, aes_string(y=pp_var), environment = environment()) + p_labs + pp_theme()
  p_area <- geom_area(aes(x=seq_along(pp_var)), fill = "black")
  p_areastep <- geom_bar(aes(x=seq_along(pp_var)), fill = "black", width = 1, stat = "identity")
  p_areastep_c <- geom_bar(aes(x=seq_along(pp_var), fill = pp_var), width = 1, stat = "identity")

  if (pp_coord == "xy" & pp_trans == "rect" & pp_color == "black") {
    p_plot + p_area
  } else if (pp_coord == "yx" & pp_trans == "rect" & pp_color == "black") {
    p_plot + p_area + coord_flip()
  } else if (pp_coord == "xy" & pp_trans == "step" & pp_color == "black") {
    p_plot + p_areastep
  } else if (pp_coord == "yx" & pp_trans == "step" & pp_color == "black") {
    p_plot + p_areastep + coord_flip()
  } else if (pp_coord == "xy" & pp_trans == "step" & pp_color == "bw") {
    p_plot + p_areastep_c + p_scale_gray_a + amb.z
    } else if (pp_coord == "yx" & pp_trans == "step" & pp_color == "bw") {
    p_plot + p_areastep_c + p_scale_gray_a + coord_flip() + amb.z
  } else if (pp_coord == "xy" & pp_trans == "step" & pp_color == "color") {
    p_plot + p_areastep_c + p_scale_color_a + amb.z
  } else if (pp_coord == "yx" & pp_trans == "step" & pp_color == "color") {
    p_plot + p_areastep_c + p_scale_color_a + coord_flip() + amb.z
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

pp_1DD_raster   <- function(pp_df,
                            pp_var,
                            pp_coord = "xy",
                            pp_color = "bw")
{
  pp_df$pp_var <- unlist(pp_df[, pp_var])

  p_labs  <- labs(y=names(pp_df[pp_var]), x="seq")
  p_plot <- ggplot(pp_df, aes_string(y=pp_var), environment = environment()) + p_labs + pp_theme()
  p_raster  <- stat_density_2d(aes(x=seq_along(pp_var), fill = stat(density)), geom = 'raster', contour = FALSE)

  if (pp_coord == "xy" & pp_color == "bw") {
    p_plot + p_raster + p_scale_gray_a + coord_flip() + amb.z
  } else if (pp_coord == "xy" & pp_color == "color") {
    p_plot + p_raster + p_scale_color_a + coord_flip() + amb.z
  } else if (pp_coord == "yx" & pp_color == "bw") {
    p_plot + p_raster + p_scale_gray_a + amb.z
  } else if (pp_coord == "yx" & pp_color == "color") {
    p_plot + p_raster + p_scale_color_a + amb.z
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

pp_1DD_stripegraph <- function(pp_df,
                               pp_var,
                               pp_color = "bw")
{
  pp_df$pp_var <- unlist(pp_df[, pp_var])
  pp_df$pp_dens <- add_density_1D(pp_df, pp_var)

  p_labs <- labs(y=names(pp_df[pp_var]), x="seq")
  p_plot <- ggplot(pp_df, aes(x=seq_along(pp_var)), environment = environment()) + p_labs + pp_theme() + amb.y
  p_tile <- geom_tile(aes_string(y=1), fill = "black")
  p_tile_c <- geom_tile(aes_string(y=1, fill=pp_var))

  if (pp_color == "black") {
    p_plot + p_tile
  } else if (pp_color == "bw") {
    p_plot + p_tile_c + p_scale_gray_a + amb.z
  } else if (pp_color == "color") {
    p_plot + p_tile_c + p_scale_color_a + amb.z
  } else {stop(warning_general)}
}

# p_plot    <- ggplot(pp_df, aes_string(y=1), environment = environment()) +
#   p_labs +
#   pp_theme() +
#   facet_grid(variable~., switch = "both")
# p_tile    <- geom_tile(aes_string(x='pp_id', fill = 'measure'))

pp_binned_stripegraph <- function(pp_df,
                                  pp_var,
                                  pp_color = "black",
                                  pp_binwidth = 1)  {
  pp_df$pp_var <- unlist(pp_df[, pp_var])

  p_labs    <- labs(x=names(pp_df[pp_var]))
  p_plot    <- ggplot(pp_df, aes_string(x=pp_var), environment = environment()) + pp_theme() + amb.y
  p_bin2d   <- geom_bin2d(aes(y=1), binwidth = c(pp_binwidth, 1), fill = "black")
  p_bin2d_c <- geom_bin2d(aes(y=1), binwidth = c(pp_binwidth, 1))

  if (pp_color == "black") {
    p_plot + p_bin2d
  } else if (pp_color == "bw") {
    p_plot + p_bin2d_c + p_scale_gray_a + p_scale_gray_l + amb.z
  } else if (pp_color == "color") {
    p_plot + p_bin2d_c + p_scale_color_a + p_scale_color_l + amb.z
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

pp_scatterplot <- function(pp_df,
                           pp_var1,
                           pp_var2,
                           pp_size = 1,
                           pp_color = "black",
                           pp_smooth = "false") {
  pp_df$pp_var1 <- unlist(pp_df[, pp_var1])
  pp_df$pp_var2 <- unlist(pp_df[, pp_var2])

  p_labs          <- labs(y=names(pp_df[pp_var2]), x=names(pp_df[pp_var1]))
  p_plot          <- ggplot(pp_df, aes_string(y=pp_var2), environment = environment()) + p_labs + pp_theme()
  if (is.numeric(pp_df$pp_var1) == TRUE & is.numeric(pp_df$pp_var2) == TRUE) {pp_df$pp_dens   <- get_density(pp_df$pp_var1, pp_df$pp_var2, n = 100)}
  p_point         <- geom_point(data = pp_df, aes(x=pp_var1), size=pp_size)
  p_point_c       <- geom_point(data = pp_df, aes_(x=~pp_var1, color=~pp_dens), size=pp_size)
  p_smooth        <- geom_smooth(aes(x=pp_var1), method = "loess", size=0.5)

  if (pp_color == "black" & pp_smooth == "false") {
    p_plot + p_point
  } else if (pp_color == "black" & pp_smooth == "true") {
    p_plot + p_point + p_smooth
  } else if (pp_color == "bw" & pp_smooth == "false") {
    p_plot + p_point_c + p_scale_gray_l + amb.z
  } else if (pp_color == "color" & pp_smooth == "false") {
    p_plot + p_point_c + p_scale_color_l + amb.z
  } else {stop(warning_general)}
}

pp_binnedpointgraph <- function(pp_df,
                                pp_var1,
                                pp_var2,
                                pp_size = 1,
                                pp_color = "black")
{
  pp_df$pp_var1 <- unlist(pp_df[, pp_var1])
  pp_df$pp_var2 <- unlist(pp_df[, pp_var2])

  p_labs    <- labs(y=names(pp_df[pp_var2]), x=names(pp_df[pp_var1]))
  p_plot    <- ggplot(pp_df, aes_string(y=pp_var2), environment = environment()) + pp_theme()
  p_point   <- geom_point(data = pp_df, aes(x=pp_var1), size=pp_size, stat= "bin2d")
  p_point_c <- geom_point(data = pp_df, aes_(x=~pp_var1, color=~..count..), size=pp_size, stat= "bin2d")

  if (pp_color == "black") {
    p_plot + p_point + amb.z + p_labs
  } else if (pp_color == "bw") {
    p_plot + p_point_c + p_scale_gray_l + amb.z + p_labs
  } else if (pp_color == "color") {
    p_plot + p_point_c + p_scale_color_l + amb.z + p_labs
  } else {stop(warning_general)}
}

pp_heatmap <- function(pp_df,
                       pp_var1,
                       pp_var2,
                       pp_color = "black",
                       edges = 4)
{
  pp_df$pp_var1 <- unlist(pp_df[, pp_var1])
  pp_df$pp_var2 <- unlist(pp_df[, pp_var2])

  p_labs        <- labs(y=names(pp_df[pp_var2]), x=names(pp_df[pp_var1]))
  p_plot        <- ggplot(pp_df, aes_string(y=pp_var2), environment = environment()) + p_labs + pp_theme() + amb.z
  p_bin2d       <- geom_bin2d(data = pp_df, aes(x=pp_var1), fill = "black")
  p_bin2d_c     <- geom_bin2d(data = pp_df, aes(x=pp_var1))
  p_bin2dhex    <- stat_binhex(data = pp_df, aes(x=pp_var1), fill = "black")
  p_bin2dhex_c  <- stat_binhex(data = pp_df, aes(x=pp_var1, fill=..count..,  color=..count..))

  if (pp_color == "black" & edges == 4) {
    p_plot + p_bin2d
  } else if (pp_color == "bw" & edges == 4) {
    p_plot + p_bin2d_c + p_scale_gray_a
  } else if (pp_color == "color" & edges == 4) {
    p_plot + p_bin2d_c + p_scale_color_a
  } else if (pp_color == "black" & edges == 6) {
    p_plot + p_bin2dhex
  } else if (pp_color == "bw" & edges == 6) {
    p_plot + p_bin2dhex_c + p_scale_gray_a + p_scale_gray_l
  } else if (pp_color == "color" & edges == 6) {
    p_plot + p_bin2dhex_c + p_scale_color_a + p_scale_color_l
  } else {stop(warning_general)}
}

pp_raster   <- function(pp_df,
                            pp_var1,
                            pp_var2,
                            pp_color = "bw")
{
  pp_df$pp_var1 <- unlist(pp_df[, pp_var1])
  pp_df$pp_var2 <- unlist(pp_df[, pp_var2])

  p_labs  <- labs(y=names(pp_df[pp_var2]), x=names(pp_df[pp_var1]))
  p_plot <- ggplot(pp_df, aes_string(y=pp_var2), environment = environment()) + p_labs + pp_theme()
  p_raster  <- stat_density_2d(aes(x=pp_var1, fill = stat(density)), geom = 'raster', contour = FALSE)

  if (pp_color == "bw") {
    p_plot + p_raster + p_scale_gray_a + amb.z
  } else if (pp_color == "color") {
    p_plot + p_raster + p_scale_color_a + amb.z
  } else {stop(warning_general)}
}

pp_contourmap <- function(pp_df,
                       pp_var1,
                       pp_var2,
                       pp_color = "black",
                       pp_size = 1,
                       pp_points = FALSE)
{
  pp_df$pp_var1 <- unlist(pp_df[, pp_var1])
  pp_df$pp_var2 <- unlist(pp_df[, pp_var2])
  if (is.numeric(pp_df$pp_var1) == TRUE & is.numeric(pp_df$pp_var2) == TRUE) {pp_df$pp_dens   <- get_density(pp_df$pp_var1, pp_df$pp_var2, n = 100)}

  p_labs        <- labs(y=names(pp_df[pp_var2]), x=names(pp_df[pp_var1]))
  p_plot        <- ggplot(pp_df, aes_string(x=pp_var1, y=pp_var2), environment = environment()) + p_labs + pp_theme() + amb.z
  p_contour2d   <- geom_density_2d(color = "black", size=pp_size*0.2)
  p_contour2d_c <- geom_density_2d(aes(color=..level..), size=pp_size*0.2)
  p_point       <- geom_point(size=pp_size)
  p_point_c     <- geom_point(aes_(color=~pp_dens), size=pp_size)

  if (pp_color == "black" & pp_points == FALSE) {
    p_plot + p_contour2d
  } else if (pp_color == "bw" & pp_points == FALSE) {
    p_plot + p_contour2d_c + p_scale_gray_l
  } else if (pp_color == "color" & pp_points == FALSE) {
    p_plot + p_contour2d_c + p_scale_color_l
  } else if (pp_color == "black" & pp_points == TRUE) {
    p_plot + p_contour2d  + p_point
  } else if (pp_color == "bw" & pp_points == TRUE) {
    p_plot + p_contour2d_c + p_point_c + p_scale_gray_l
  } else if (pp_color == "color" & pp_points == TRUE) {
    p_plot + p_contour2d_c + p_point_c + p_scale_color_l
  } else {stop(warning_general)}
}

pp_parallel <- function(pp_df,
                        pp_var1,
                        pp_var2,
                        pp_size = 1,
                        pp_relative = TRUE,
                        pp_color = "black")
{
  pp_df$pp_var1 <- unlist(pp_df[, pp_var1])
  pp_df$pp_var2 <- unlist(pp_df[, pp_var2])
  pp_df$pp_dens   <-
    get_density(pp_df$pp_var1, pp_df$pp_var2, n = 100)

  # p_labs          <- labs(y='', x='')
  p_expand        <- scale_x_discrete(expand = c(.1, 0))
  p_parallel      <-
    GGally::ggparcoord(
      pp_df,
      columns = c(grep(pp_var1, colnames(pp_df)),
                  grep(pp_var2, colnames(pp_df))),
      scale = 'uniminmax',
      alphaLines = ifelse(pp_size > 1 || pp_size < 0, 1, pp_size)
    ) +
    p_expand + pp_theme()
  p_parallel_c    <-
    GGally::ggparcoord(
      pp_df,
      columns = c(grep(pp_var1, colnames(pp_df)),
                  grep(pp_var2, colnames(pp_df))),
      scale = 'uniminmax',
      alphaLines = ifelse(pp_size > 1 || pp_size < 0, 1, pp_size),
      mapping = aes(color = pp_dens)
    ) +
    p_expand + pp_theme()
  p_parallelg     <-
    GGally::ggparcoord(
      pp_df,
      columns = c(grep(pp_var1, colnames(pp_df)),
                  grep(pp_var2, colnames(pp_df))),
      scale = 'globalminmax',
      alphaLines = ifelse(pp_size > 1 || pp_size < 0, 1, pp_size)
    ) +
    p_expand + pp_theme()
  p_parallelg_c   <-
    GGally::ggparcoord(
      pp_df,
      columns = c(grep(pp_var1, colnames(pp_df)),
                  grep(pp_var2, colnames(pp_df))),
      scale = 'globalminmax',
      alphaLines = ifelse(pp_size > 1 || pp_size < 0, 1, pp_size),
      mapping = aes(color = pp_dens)
    ) +
    p_expand + pp_theme()

  if (pp_relative == FALSE & pp_color == "black") {
    p_parallelg + amb.z
  } else if (pp_relative == FALSE & pp_color == "bw") {
    p_parallelg_c + p_scale_gray_l + amb.z
  } else if (pp_relative == FALSE & pp_color == "color") {
    p_parallelg_c + p_scale_color_l + amb.z
  } else if (pp_relative == TRUE & pp_color == "black") {
    p_parallel + amb.z
  } else if (pp_relative == TRUE & pp_color == "bw") {
    p_parallel_c + p_scale_gray_l + amb.z
  } else if (pp_relative == TRUE & pp_color == "color") {
    p_parallel_c + p_scale_color_l + amb.z
  } else {
    stop(warning_general)
  }
}

pp_pathgraph <- function(pp_df,
                         pp_var1,
                         pp_var2,
                         pp_size = 1,
                         pp_points = FALSE,
                         pp_color = "black")
{
  pp_df$pp_var1 <- unlist(pp_df[, pp_var1])
  pp_df$pp_var2 <- unlist(pp_df[, pp_var2])
  # pp_df$pp_dens   <- get_density(pp_df$pp_var1, pp_df$pp_var2, n = 100)

  p_labs    <- labs(y=names(pp_df[pp_var2]), x=names(pp_df[pp_var1]))
  p_plot    <- ggplot(pp_df, aes_string(y=pp_var2), environment = environment()) + p_labs + pp_theme()
  p_path    <- geom_path(aes(x=pp_var1, group=1), size=0.5*pp_size)
  p_path_c  <- geom_path(aes(x=pp_var1, group=1, color=seq_along(pp_var1)))
  p_point   <- geom_point(data = pp_df, aes(x=pp_var1, group=1), size=1.5*pp_size)
  p_point_c   <- geom_point(data = pp_df, aes(x=pp_var1, color=seq_along(pp_var1), group=1), size=1.5*pp_size)

  if (pp_points == FALSE & pp_color == "black") {
    p_plot + p_path
  } else if (pp_points == FALSE & pp_color == "bw") {
    p_plot + p_path_c + p_scale_gray_l + amb.z
  } else if (pp_points == FALSE & pp_color == "color") {
    p_plot + p_path_c + p_scale_color_l + amb.z
  } else if (pp_points == TRUE & pp_color == "black") {
    p_plot + p_path + p_point
  } else if (pp_points == TRUE & pp_color == "bw") {
    p_plot + p_path_c + p_point_c + p_scale_gray_l + amb.z
  } else if (pp_points == TRUE & pp_color == "color") {
    p_plot + p_path_c + p_point_c + p_scale_color_l + amb.z
  } else {stop(warning_general)}
}

pp_unfolded <- function(pp_df,
                        pp_var1,
                        pp_var2,
                        pp_size = 0.5,
                        pp_geom = "line",
                        pp_color = "black")
  {
  pp_df    <- reshape(data = pp_df,
                      direction = "long",
                      varying = c(pp_var1, pp_var2),
                      times   = c(pp_var1, pp_var2),
                      v.names = "measure",
                      timevar = "variable",
                      idvar   = "pp_id"
                      )
  p_labs    <- labs(y='', x='seq')
  p_plot    <- ggplot(pp_df, aes_string(x='pp_id'), environment = environment()) +
    p_labs +
    pp_theme() +
    facet_grid(variable~., switch = "both")
  p_line      <- geom_line(aes_string(y='measure'), color = 'black', size=0.5*pp_size)
  p_line_c    <- geom_line(aes_string(y='measure', color = 'pp_id'), size=0.5*pp_size)
  p_step      <- geom_step(aes_string(y='measure'), color = 'black', size=0.5*pp_size)
  p_step_c    <- geom_step(aes_string(y='measure', color = 'pp_id'), size=0.5*pp_size)
  p_point     <- geom_point(aes_string(y='measure'), size=pp_size)
  p_point_c   <- geom_point(aes_string(y='measure', color = 'measure'), size=pp_size)
  p_bar       <- geom_linerange(aes_string(ymin=0, ymax='measure'), color = 'black', size=pp_size)
  p_bar_c     <- geom_linerange(aes_string(ymin=0, ymax='measure', color = 'measure'), size=pp_size)
  p_area      <- geom_area(aes_string(y='measure'), fill = 'black')

  if (pp_geom == "line" & pp_color == "black") {
    p_plot + p_line
  } else if (pp_geom ==  "line" & pp_color == "bw") {
    p_plot + p_line_c + p_scale_gray_l + amb.z
  } else if (pp_geom ==  "line" & pp_color == "color") {
    p_plot + p_line_c + p_scale_color_l + amb.z
  } else if (pp_geom == "step" & pp_color == "black") {
    p_plot + p_step
  } else if (pp_geom ==  "step" & pp_color == "bw") {
    p_plot + p_step_c + p_scale_gray_l + amb.z
  } else if (pp_geom ==  "step" & pp_color == "color") {
    p_plot + p_step_c + p_scale_color_l + amb.z
  } else if (pp_geom ==  "point" & pp_color == "black") {
    p_plot + p_point
  } else if (pp_geom ==  "point" & pp_color == "bw") {
    p_plot + p_point_c + p_scale_gray_l + amb.z
  } else if (pp_geom ==  "point" & pp_color == "color") {
    p_plot + p_point_c + p_scale_color_l + amb.z
  } else if (pp_geom ==  "bar" & pp_color == "black") {
    p_plot + p_bar
  } else if (pp_geom ==  "bar" & pp_color == "bw") {
    p_plot + p_bar_c + p_scale_gray_l + amb.z
  } else if (pp_geom ==  "bar" & pp_color == "color") {
    p_plot + p_bar_c + p_scale_color_l + amb.z
  } else if (pp_geom ==  "area" & pp_color == "black") {
    p_plot + p_area
  } else {stop(warning_general)}
}

pp_unf_raster <- function(pp_df,
                          pp_var1,
                          pp_var2,
                          pp_size = 0.5,
                          pp_geom = "heat",
                          pp_color = "bw")
{
  pp_df    <- reshape(
    data = pp_df,
    direction = "long",
    varying = c(pp_var1, pp_var2),
    times   = c(pp_var1, pp_var2),
    v.names = "measure",
    timevar = "variable",
    idvar   = "pp_id"
  )
  p_labs    <- labs(y = '', x = 'seq')
  p_plot    <-
    ggplot(pp_df, aes_string(y = 'measure'), environment = environment()) +
    p_labs +
    pp_theme() +
    facet_grid(variable ~ ., switch = "both")
  p_raster    <-
    stat_density_2d(aes(x = pp_id, fill = stat(density)),
                    geom = 'raster',
                    contour = FALSE)

  if (pp_geom ==  "heat" & pp_color == "bw") {
    p_plot + p_raster + p_scale_gray_a + amb.z
  } else if (pp_geom ==  "heat" & pp_color == "color") {
    p_plot + p_raster + p_scale_color_a + amb.z
  } else {
    stop(warning_general)
  }
}

pp_unf_tile <- function(pp_df,
                        pp_var1,
                        pp_var2,
                        pp_size = 0.5,
                        pp_geom = "tile",
                        pp_color = "bw")
{
  pp_df    <- reshape(
    data = pp_df,
    direction = "long",
    varying = c(pp_var1, pp_var2),
    times   = c(pp_var1, pp_var2),
    v.names = "measure",
    timevar = "variable",
    idvar   = "pp_id"
  )
  p_labs    <- labs(y = '', x = 'seq')
  p_plot    <-
    ggplot(pp_df, aes_string(y = 1), environment = environment()) +
    p_labs +
    pp_theme() +
    facet_grid(variable ~ ., switch = "both")
  p_tile    <- geom_tile(aes_string(x = 'pp_id', fill = 'measure'))

  if (pp_geom ==  "tile" & pp_color == "bw") {
    p_plot + p_tile + p_scale_gray_a + amb.z + amb.y
  } else if (pp_geom ==  "tile" & pp_color == "color") {
    p_plot + p_tile + p_scale_color_a + amb.z + amb.y
  } else {
    stop(warning_general)
  }
}

pp_unf_yuxt <- function(pp_df,
                        pp_var1,
                        pp_var2,
                        pp_size = 0.5,
                        pp_geom = "hist",
                        pp_color = "bw")
{
  pp_df    <- reshape(data = pp_df,
                      direction = "long",
                      varying = c(pp_var1, pp_var2),
                      times   = c(pp_var1, pp_var2),
                      v.names = "measure",
                      timevar = "variable",
                      idvar   = "pp_id"
  )
  p_labs    <- labs(y='', x='')
  p_plot    <- ggplot(pp_df, aes_string(x='measure'), environment = environment()) +
    p_labs +
    pp_theme() +
    facet_grid(variable~., switch = "both")
  p_histo    <- geom_histogram(fill="black")
  p_histo_c  <- geom_histogram(aes_(fill=~..count..))
  p_freq     <- geom_line(stat = 'bin', bins = 20, center = 0, color = "black", size = 0.1)
  p_dens     <- geom_density(size = 0.1)
  p_dens_f   <- geom_density(fill = "black")
  p_viol     <- geom_violin(aes_string(x=0, y='measure'), size = 0.1)
  p_viol_f   <- geom_violin(aes_string(x=0, y='measure'), fill = "black")
  p_box      <- geom_boxplot(aes_string(x=0, y='measure'), size = 0.1)


  if (pp_geom ==  "hist" & pp_color == "black") {
    p_plot + p_histo
  } else if (pp_geom ==  "hist" & pp_color == "bw") {
    p_plot + p_histo_c + p_scale_gray_a + amb.z
  } else if (pp_geom ==  "hist" & pp_color == "color") {
    p_plot + p_histo_c + p_scale_color_a + amb.z
  } else if (pp_geom ==  "freq" & pp_color == "black") {
    p_plot + p_freq
  } else if (pp_geom ==  "dens" & pp_color == "black") {
    p_plot + p_dens
  } else if (pp_geom ==  "dens" & pp_color == "fill") {
    p_plot + p_dens_f
  } else if (pp_geom ==  "viol" & pp_color == "black") {
    p_plot + p_viol + coord_flip() + amb.y
  } else if (pp_geom ==  "viol" & pp_color == "fill") {
    p_plot + p_viol_f + coord_flip() + amb.y
  } else if (pp_geom ==  "box" & pp_color == "black") {
    p_plot + p_box + coord_flip() + amb.y
  } else {stop(warning_general)}
}

pp_unf_ecdf <- function(pp_df,
                          pp_var1,
                          pp_var2,
                          pp_size = 0.5,
                          pp_trans = "point")
{
  pp_df    <- reshape(data = pp_df,
                      direction = "long",
                      varying = c(pp_var1, pp_var2),
                      times   = c(pp_var1, pp_var2),
                      v.names = "measure",
                      timevar = "variable",
                      idvar   = "pp_id"
  )
  p_labs    <- labs(x='', y='p')
  p_plot    <- ggplot(pp_df, aes_string(x='measure'), environment = environment()) +
    pp_theme() +
    facet_grid(variable~., switch = "both") +
    p_labs

  if (pp_trans ==  "point") {
    p_plot + stat_ecdf(geom = "point", size=pp_size)
  } else if (pp_trans ==  "line") {
    p_plot + stat_ecdf(geom = "line", size=pp_size)
  } else if (pp_trans ==  "step") {
    p_plot + stat_ecdf(geom = "step", size=pp_size)
  } else {stop(warning_general)}
}

pp_basicgraph <- function(pp_df,
                          pp_var1,
                          pp_var2,
                          pp_size = 1,
                          pp_geom,
                          pp_color = "black")
{
  pp_df$pp_var1 <- unlist(pp_df[, pp_var1])
  pp_df$pp_var2 <- unlist(pp_df[, pp_var2])

  p_labs    <- labs(y=names(pp_df[pp_var2]), x=names(pp_df[pp_var1]))
  p_plot    <- ggplot(pp_df, aes_string(x=pp_var1, y=pp_var2), environment = environment()) + p_labs + pp_theme()
  p_path    <- geom_path(aes(group=1), size=0.5*pp_size)
  p_point   <- geom_point(aes(group=1), size=3*pp_size)
  p_tile    <- geom_tile()
  p_box     <- geom_boxplot(size=0.5*pp_size)
  p_violin  <- geom_violin(aes(group=pp_var2), size=0.5*pp_size)
  p_violi_f <- geom_violin(aes(group=pp_var2), fill="black")
  p_bin2d   <- geom_bin2d(data = pp_df, fill = "black")
  p_bin2d_c <- geom_bin2d(data = pp_df)

  if (pp_geom == "line") {
    p_plot + p_path
  } else if (pp_geom == "point") {
    p_plot + p_point
  } else if (pp_geom == "tile") {
    p_plot + p_tile
  } else if (pp_geom == "box") {
    p_plot + p_box
  } else if (pp_geom == "violin") {
    p_plot + p_violin
  } else if (pp_geom == "violin filled") {
    p_plot + p_violi_f
  } else if (pp_geom == "bin" & pp_color == "black") {
    p_plot + p_bin2d
  } else if (pp_geom == "bin" & pp_color == "bw") {
    p_plot + p_bin2d_c + p_scale_gray_a + amb.z
  } else if (pp_geom == "bin" & pp_color == "color") {
    p_plot + p_bin2d_c + p_scale_color_a + amb.z
  } else {stop(warning_general)}
}

pp_density2 <- function(pp_df,
                        pp_var1,
                        pp_var2,
                        pp_size = 0.5,
                        pp_aes = "line",
                        pp_color = "color")  {
  pp_df$pp_var1 <- unlist(pp_df[, pp_var1])
  pp_df$pp_var2 <- unlist(pp_df[, pp_var2])

  p_labs  <- labs(x=names(pp_df[pp_var1]))
  p_plot_b <- ggplot(pp_df, aes_string(x=pp_var1, group = pp_var2), environment = environment()) + p_labs + pp_theme()
  p_plot_c <- ggplot(pp_df, aes_string(x=pp_var1, color = pp_var2), environment = environment()) + p_labs + pp_theme()
  p_plot_f <- ggplot(pp_df, aes_string(x=pp_var1, fill = pp_var2), environment = environment()) + p_labs + pp_theme()
  if (pp_aes == "line" & pp_color == "bw") {
    p_plot_b + geom_density(size=pp_size, color = "black")
  } else if (pp_aes == "area" & pp_color == "bw") {
    p_plot_b + geom_density(size=0.5*pp_size, alpha = 0.3, fill = "black", color = "white")
  } else if (pp_aes == "line" & pp_color == "color") {
    p_plot_c + geom_density(size=pp_size, alpha = 0.5) + scl_col_disc_l + amb.z
  } else if (pp_aes == "area" & pp_color == "color") {
    p_plot_f + geom_density(size=0.5*pp_size, alpha = 0.5, color = "white") + scl_col_disc_a + amb.z
  } else {stop(warning_general)}
}

pp_histogram2 <- function(pp_df,
                          pp_var1,
                          pp_var2,
                          pp_color = "bw",
                          pp_position = "stack")  {
  pp_df$pp_var1 <- unlist(pp_df[, pp_var1])
  pp_df$pp_var2 <- unlist(pp_df[, pp_var2])

  p_labs    <- labs(x=names(pp_df[pp_var1]))
  p_plot    <- ggplot(pp_df, aes_string(x=pp_var1, color=pp_var2, fill=pp_var2), environment = environment()) + p_labs + pp_theme()
  p_hist_s  <- geom_histogram(center = 0, position = "stack")
  p_hist_f  <- geom_histogram(center = 0, position = "fill")

  if (pp_color == "bw" & pp_position == "stack") {
    p_plot + p_hist_s + scl_gray_disc_a + scl_gray_disc_l + amb.z
  } else if (pp_color == "color" & pp_position == "stack") {
    p_plot + p_hist_s + scl_col_disc_a + scl_col_disc_l + amb.z
  } else if (pp_color == "bw" & pp_position == "fill") {
    p_plot + p_hist_f + scl_gray_disc_a + scl_gray_disc_l + amb.z + scale_y_continuous(breaks = c(0, 1), labels = c("0%", "100%"))
  } else if (pp_color == "color" & pp_position == "fill") {
    p_plot + p_hist_f + scl_col_disc_a + scl_col_disc_l + amb.z + scale_y_continuous(breaks = c(0, 1), labels = c("0%", "100%"))
  } else {stop(warning_general)}
}

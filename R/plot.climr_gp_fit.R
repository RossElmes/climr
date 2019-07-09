#' Plot climr output
#' @param x Output from the \code{\link{gp_fit}} function
#' @param ... Other arguments to plot (not currently implemented)
#'
#' @return Nothing: just a nice plot
#' @seealso \code{\link{load_clim}}, \code{\link{gp_fit}}
#' @export
#' @import ggplot2
#' @importFrom viridis "scale_color_viridis"
#'
#' @examples
#' ans1 = load_clim('SH')
#' ans2 = gp_fit(ans1, optim_method = "BFGS")
#' plot(ans2)
plot.climr_gp_fit = function(x,...) {

  # Create global variables to avoid annoying CRAN notes
  temp = year = pred2_unscale = x_g_unscale = NULL

  # Get the data set out
  df <- x$data
  fit <- x$fit

  # Create a nice plot from the output of gp_fit.climr

  # Finally create the plot
  ggplot(df, aes(year, temp)) +
    geom_point(aes(colour = temp)) +
    theme_bw() +
    xlab('Year') +
    ylab('Temperature anomaly') +
    geom_line(data = fit, aes(x = x_g_unscale, y = pred2_unscale, colour = pred2_unscale)) +
    theme(legend.position = 'None') +
    scale_color_viridis()

}

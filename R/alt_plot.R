#' @author Chen Lin, Kevin Wang, Samuel Mueller
#' @title Alternative (ggplot) visualisation for mcvis method
#' @description
#' The ggplot_mcvis function first orders the MC-index matrix columns by
#' the magnitude of the MC-index for the tau1, which is the inverse of the smallest eigenvalue
#' Under this ordering,
#' the first entry of the matrix is classified as a "category 5" thickness in plotting
#' Subsequently, other lines are sequentially put in the categories of
#' \itemize{
#'  \item category 5: 0.5 or above
#'  \item category 4: 0.3 - 0.5
#'  \item category 3: 0.2 - 0.3
#'  \item category 2: 0.1 - 0.2
#'  \item category 1: 0.0 - 0.1
#' }
#' @param x Output of the mcvis function
#' @param eig_max The maximum number of eigenvalues to be displayed on the plot.
#' @param var_max The maximum number of variables (i.e. columns) to be displayed on the plot.
#' @import ggplot2
#' @importFrom reshape2 melt
#' @importFrom rlang .data
#' @return A ggplot
#' @export
#' @examples
#' library(ggplot2)
#' set.seed(1)
#' p = 10
#' n = 100
#' X = matrix(rnorm(n*p, 0, 5), ncol = p)
#' X[,1] = X[,2] + rnorm(n, 0, 1)
#' mcvis_result = mcvis(X)
#' alt_plot(mcvis_result)
#'
#' X[,1] = X[,2] + rnorm(n, 0, 100)
#' mcvis_result2 = mcvis(X)
#' alt_plot(mcvis_result2)

alt_plot = function(x,
                        eig_max = 1L,
                        var_max = ncol(x$MC))
  ##if eig_max==1 or var_max==1, the function fails to give an output.
{
  MC_ordered = make_MC_ordered(
    mcvis_result = x,
    eig_max = eig_max,
    var_max = var_max)

  taup = rownames(MC_ordered)[1]
  p = ncol(x$MC)
  ############################################
  melt_MC = reshape2::melt(
    MC_ordered,
    varnames = c("taus", "cols"),
    value.name = "weights")

  thickness = 1 - melt_MC$weights
  thickness = thickness - (1/p)

  ggplot_size_cat = dplyr::case_when(
    thickness <= 0.1 ~ "Not expected to cause MC",
    thickness <= 0.2 ~ "Small chance to cause MC",
    thickness <= 0.3 ~ "Fair chance to cause MC",
    TRUE ~ "Strong chance to cause MC")

  ggplot_size_cat = factor(
    ggplot_size_cat,
    levels = c("Not expected to cause MC",
               "Small chance to cause MC",
               "Fair chance to cause MC",
               "Strong chance to cause MC"))

  plotdf = dplyr::mutate(melt_MC, thickness, ggplot_size_cat)

  plotdf$cols_norm = rangeTransform(as.integer(plotdf$cols))
  plotdf$taus_norm = rangeTransform(as.integer(plotdf$taus))

  plotdf$y1 = 0
  plotdf$y2 = 1

  plotdf$linetype = ifelse(plotdf$taus == rownames(MC_ordered)[1], rownames(MC_ordered)[1], "others")
  #################  ggplot #######################
  ggplot_size_manual = c(0, 0.5, 1, 2)
  ggplot_alpha_manual = c(0, 0.5, 1, 1)
  axis_1 = data.frame(x=rangeTransform(as.integer(unique(plotdf$cols))),
                      y=0, label=as.character(unique(plotdf$cols)))

  axis_2 = data.frame(x=rangeTransform(as.integer(unique(plotdf$taus))),
                      y=1, label=as.character(unique(plotdf$taus)))

  linetype_manual = c("dotted","solid")
  names(linetype_manual) = c("others", taup)

  gg = ggplot2::ggplot(data=plotdf) +
    geom_segment(aes(
      x=.data$cols_norm, xend=.data$taus_norm,
      y=.data$y1, yend=.data$y2,
      colour = .data$ggplot_size_cat,
      size = .data$ggplot_size_cat,
      alpha = .data$ggplot_size_cat,
      linetype = .data$linetype)) +
    geom_text(data=axis_1, aes(label=.data$label, x=.data$x, y=.data$y - 0.075)) +
    geom_text(data=axis_2, aes(label=.data$label, x=.data$x, y=.data$y + 0.075)) +
    geom_segment(data=axis_1, aes(x=.data$x, xend=.data$x, y=.data$y, yend=.data$y-0.025), size=0.7) +
    geom_segment(data=axis_2, aes(x=.data$x, xend=.data$x, y=.data$y, yend=.data$y+0.025), size=0.7) +
    geom_segment(x=0, xend=1, y=0, yend=0, size=0.7) +
    geom_segment(x=0, xend=1, y=1, yend=1, size=0.7) +
    scale_colour_brewer(palette = "Set1", drop = FALSE, direction = -1) +
    scale_size_manual(values = ggplot_size_manual, drop = FALSE) +
    scale_alpha_manual(values = ggplot_alpha_manual, drop = FALSE) +
    scale_linetype_manual(values = linetype_manual, drop = FALSE) +
    scale_y_continuous(limits=c(-0.2, 1.2), expand=c(0, 0)) +
    labs(title = "Multi-collinearity plot") +
    guides(
      colour = guide_legend(title = "MC categories"),
      size = FALSE,
      linetype = FALSE,
      alpha = FALSE) +
    theme_bw() +
    theme(axis.title=element_blank(),
          axis.text=element_blank(),
          axis.ticks=element_blank(),
          panel.grid=element_blank())
  gg
  return(gg)
}

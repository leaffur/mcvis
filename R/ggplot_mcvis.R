#' @author Chen Lin
#' @title Multi-collinearity Visualization
#' @param mcvis_result Output of the mcvis function
#' @param eig.max The maximum number of eigenvalues to be displayed on the plot.
#' @param vol.max The maximum number of variables to be displayed on the plot.
#' @import ggplot2
#' @import reshape2
#' @export
#' @examples
#' library(mplot)
#' data("artificialeg")
#' p=dim(artificialeg)[2]-1
#' X=artificialeg[,1:p]
#' mcvis_result = mcvis2(X)
#' ggplot_mcvis(mcvis_result)

ggplot_mcvis = function(mcvis_result,
                        eig.max = ncol(g), 
                        vol.max = ncol(g)){
  g = mcvis_result$g
  col.names = mcvis_result$col.names
  
  eig.max = min(p, eig.max)
  vol.max = min(p, vol.max)
  or = order(g[p,]) ## Order the columns of g by the smallest eigen value
  or = or[1:vol.max]
  g.or = g[,or]
  if (vol.max > 1) {g.or = g.or[p:(p-eig.max+1),]} else {g.or = as.matrix(g.or[p:(p-eig.max+1)])}
  if (eig.max == 1) {g.or = t(g.or)}

  # ###############  ggplot #######################
  dat = reshape2::melt(g.or,
                       varnames = c("X2", "X1"),
                       value.name = "weights") %>%
    dplyr::mutate(
      thickness = 1-weights,
      # thicknessCategory = cut(thickness, breaks = seq(0, 1, by = 0.2),
      #                         include.lowest = T,
      #                         ordered_result = T)

      thicknessCategory = cut(thickness, breaks = quantile(thickness, probs = seq(0, 1, by = 0.2)),
                              labels = paste0("category", 1:5),
                              include.lowest = T,
                              ordered_result = T)
    )

  dat$x1_norm = rangeTransform(as.integer(dat$X1))
  dat$x2_norm = rangeTransform(as.integer(dat$X2))

  dat$y1 = 0
  dat$y2 = 1


  axis_1 = data.frame(x=rangeTransform(as.integer(unique(dat$X1))),
                      y=0, label=as.character(unique(dat$X1)))

  axis_2 = data.frame(x=rangeTransform(as.integer(unique(dat$X2))),
                      y=1, label=as.character(unique(dat$X2)))

  gg = ggplot2::ggplot(data=dat) +
    theme_bw() +
    theme(axis.title=element_blank()) +
    theme(axis.text=element_blank()) +
    theme(axis.ticks=element_blank()) +
    theme(panel.grid=element_blank()) +
    geom_segment(aes(x=x1_norm, xend=x2_norm, y=y1, yend=y2, colour = X2,
                     size = thicknessCategory, alpha = thickness)) +
    scale_size_manual(values = c(0, 0.1, 0.2, 0.3, 1.5)) +
    geom_segment(x=0, xend=1, y=0, yend=0, size=0.7) +
    geom_segment(x=0, xend=1, y=1, yend=1, size=0.7) +
    # scale_colour_grey() +
    # scale_colour_brewer(palette="Set1") +
    scale_y_continuous(limits=c(-0.2, 1.2), expand=c(0, 0)) +
    geom_segment(data=axis_1, aes(x=x, xend=x, y=y, yend=y-0.025), size=0.7) +
    geom_segment(data=axis_2, aes(x=x, xend=x, y=y, yend=y+0.025), size=0.7) +
    geom_text(data=axis_1, aes(label=label, x=x, y=y - 0.075)) +
    geom_text(data=axis_2, aes(label=label, x=x, y=y + 0.075)) +
    labs(caption = "Largest Eigen = smallest Eigenvalue")
  
  return(gg)
}

##################
rangeTransform = function(x){ (x - min(x)) / (max(x) - min(x))}
##################
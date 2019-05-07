#' @author Chen Lin, Kevin Wang
#' @title ggplot visualisation for mcvis method
#' @param mcvis_result Output of the mcvis function
#' @param eig.max The maximum number of eigenvalues to be displayed on the plot.
#' @param vol.max The maximum number of variables to be displayed on the plot.
#' @import ggplot2
#' @importFrom reshape2 melt
#' @export
#' @examples
#' library(mplot)
#' data("artificialeg")
#' X=artificialeg[,1:9]
#' mcvis_result = mcvis(X)
#' ggplot_mcvis(mcvis_result)

ggplot_mcvis = function(mcvis_result,
                        eig.max = ncol(mcvis_result$MC),
                        vol.max = ncol(mcvis_result$MC))
##if eig.max==1 or vol.max==1, the function fails to give an output.
{
  g = 1-mcvis_result$MC
  col.names = mcvis_result$col.names
  p = ncol(g)
  eig.max = min(p, eig.max)
  vol.max = min(p, vol.max)
  or = order(g[p,]) ## Order the columns of g by the smallest eigen value
  or = or[1:vol.max]
  g.or = g[,or]
  if (vol.max > 1) {g.or = g.or[p:(p-eig.max+1),]} else {g.or = as.matrix(g.or[p:(p-eig.max+1)])}
  if (eig.max == 1) {g.or = t(g.or)}

  #################  ggplot #######################
  preDat = reshape2::melt(g.or,
                          varnames = c("X2", "X1"),
                          value.name = "weights")

  thickness = 1 - preDat$weights


  ## Size category is the scale of the g-matrix.
  # sizeCategory5 = 1
  # sizeCategory5 = max(thickness)
  sizeCategory5 = thickness[1]
  sizeCategory1 = 0.01*sizeCategory5
  sizeCategory2 = 0.5*sizeCategory5
  sizeCategory3 = 0.7*sizeCategory5
  sizeCategory4 = 0.9*sizeCategory5

  ggplotSizeManual = c(0, 0.1, 0.5, 1, 2)

  ggplotSizeCategory = dplyr::case_when(
    thickness <= sizeCategory1 ~ "category1",
    thickness <= sizeCategory2 ~ "category2",
    thickness <= sizeCategory3 ~ "category3",
    thickness <= sizeCategory4 ~ "category4",
    thickness <= sizeCategory5 ~ "category5",
    thickness > sizeCategory5 ~ "category5"
  )

  ggplotAlphaManual = c(0, 0.2, 0.5, 0.8, 1.0)

  dat = preDat %>%
    dplyr::mutate(
      thickness,
      ggplotSizeCategory
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
                     size = ggplotSizeCategory, alpha = ggplotSizeCategory)) +
    scale_size_manual(values = ggplotSizeManual, drop = FALSE) +
    # scale_alpha_continuous(range = c(0, 1)) +
    scale_alpha_manual(values = ggplotAlphaManual, drop = FALSE) +
    geom_segment(x=0, xend=1, y=0, yend=0, size=0.7) +
    geom_segment(x=0, xend=1, y=1, yend=1, size=0.7) +
    # scale_colour_grey() +
    # scale_colour_brewer(palette="Set1") +
    scale_y_continuous(limits=c(-0.2, 1.2), expand=c(0, 0)) +
    geom_segment(data=axis_1, aes(x=x, xend=x, y=y, yend=y-0.025), size=0.7) +
    geom_segment(data=axis_2, aes(x=x, xend=x, y=y, yend=y+0.025), size=0.7) +
    geom_text(data=axis_1, aes(label=label, x=x, y=y - 0.075)) +
    geom_text(data=axis_2, aes(label=label, x=x, y=y + 0.075)) +
    labs(title = "Visualise tau against original variables",
         caption = "Largest Eigen = smallest Eigenvalue") +
    guides(
      colour = FALSE,
      size = guide_legend(title = ""),
      alpha = guide_legend(title = "")
      )
  return(gg)

}

##################
#' @author Chen Lin, Kevin Wang
#' @title Range transform
#' @param x numeric vector
#' @export
rangeTransform = function(x){ (x - min(x)) / (max(x) - min(x))}
##################

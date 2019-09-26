#' @author Chen Lin, Kevin Wang, Samuel Mueller
#' @title ggplot visualisation for mcvis method
#' @description
#' The ggplot_mcvis function first orders the MC-index matrix columns by
#' the magnitude of the MC-index for the tau1, which is the inverse of the smallest eigenvalue
#' Under this ordering,
#' the first entry of the matrix is classified as a "category 5" thickness in plotting
#' Subsequently, other lines are sequentially put in the categories of
#' \itemize{
#'  \item category 5: anything equal or above the first entry of the matrix
#'  \item category 4: anything above 90 \% of the first entry of the matrix
#'  \item category 3: anything above 70 \% of the first entry of the matrix
#'  \item category 2: anything above 50 \% of the first entry of the matrix
#'  \item category 1: anything above 10 \% of the first entry of the matrix
#' }
#' @param mcvis_result Output of the mcvis function
#' @param eig_max The maximum number of eigenvalues to be displayed on the plot.
#' @param var_max The maximum number of variables (i.e. columns) to be displayed on the plot.
#' @import ggplot2
#' @importFrom reshape2 melt
#' @return A ggplot
#' @export
#' @examples
#' set.seed(1)
#' p = 10
#' n = 100
#' X = matrix(rnorm(n*p), ncol = p)
#' X[,1] = X[,2] + rnorm(n, 0, 0.1)
#' mcvis_result = mcvis(X)
#' ggplot_mcvis(mcvis_result)

ggplot_mcvis = function(mcvis_result,
                        eig_max = ncol(mcvis_result$MC),
                        var_max = ncol(mcvis_result$MC))
  ##if eig_max==1 or var_max==1, the function fails to give an output.
{
  MC = 1 - mcvis_result$MC
  col_names = mcvis_result$col_names
  p = length(col_names)
  eig_max = min(p, eig_max)
  var_max = min(p, var_max)
  or = order(MC[p,]) ## Order the columns of g by the smallest eigen value
  or = or[1:var_max]
  MC_ordered = MC[,or]
  if (var_max > 1) {
    MC_ordered = MC_ordered[p:(p-eig_max+1),]
  } else {
    MC_ordered = as.matrix(MC_ordered[p:(p-eig_max+1)])
  }
  if (eig_max == 1) {MC_ordered = t(MC_ordered)}

  #################  ggplot #######################
  preDat = reshape2::melt(MC_ordered,
                          varnames = c("X2", "X1"),
                          value.name = "weights")

  thickness = 1 - preDat$weights


  ## Size category is the scale of the g-matrix.
  # sizeCategory5 = 1
  # sizeCategory5 = max(thickness)
  sizeCategory5 = thickness[1]
  sizeCategory1 = 0.1*sizeCategory5
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
  ggplotSizeCategory = factor(ggplotSizeCategory,
                              levels = paste0("category", 1:5))

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
    labs(title = "Multi-collinearity plot") +
    guides(
      colour = FALSE,
      size = guide_legend(title = "MC categories"),
      alpha = FALSE
    )
  return(gg)

}

##################
#' @author Chen Lin, Kevin Wang
#' @title Range transform
#' @param x numeric vector
rangeTransform = function(x){ (x - min(x)) / (max(x) - min(x))}
##################

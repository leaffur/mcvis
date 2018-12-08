#' @author Chen Lin
#' @title Multi-collinearity Visualization
#' @param X A matrix of regressors.
#' @param tau A parameter for plotting line thickness
#' @param firstcol Logical; if the first column of X is constant of ones.
#' @param col.names The name of variables.
#' @param eig.max The maximum number of eigenvalues to be displayed on the plot.
#' @param vol.max The maximum number of variables to be displayed on the plot.
#' @param method The resampling method for the data. Current supports "bootstrap" or "cv"(cross-validation).
#' @param steps Number of sampling runs we perform. Default to 1000.
#' @import igraph
#' @import ggplot2
#' @export
#' @examples
#' library(mplot)
#' data("artificialeg")
#' p=dim(artificialeg)[2]-1
#' X=artificialeg[,1:p]
#' mcvis(X)


mcvis <- function(X,
                  tau = 1.5,
                  firstcol = FALSE,
                  eig.max = dim(X)[2]-firstcol,
                  vol.max = dim(X)[2]-firstcol,
                  method = "bootstrap",
                  steps = 1000L
)
{
  n = dim(X)[1]
  # n1 = as.matrix(rep(1,n)) ## Kevin: I don't think it was used until redefined
  p = dim(X)[2] - firstcol ## If firstcol is FALSE, then p is p, otherwise we have an extra intercept term to consider
  col.names = colnames(X)[(2-firstcol):(p+1-firstcol)]
  eig.max = min(p, eig.max)
  vol.max = min(p, vol.max)
  ## One can choose the max variables and eigenvectors in the plot

  ## Initialise the matrice
  vif = v2 = matrix(0, p, steps)
  if (method=="bootstrap") {method = 1}
  if (method=="cv") {method = 2}

  for (i in 1:steps) {
    ## Use bootstrap or cross-validation to determine indices
    index.b = switch(
      method,
      sample(n, replace = TRUE),
      sample(n, replace = FALSE)[1:(floor(sqrt(p*n)))]
    )

    ## Sampling on the rows of the design matrix
    X.b = as.matrix(X[index.b,])
    # n1 = as.matrix(rep(1, nrow(X.b)))


    ## If the input X does not have an intercept, then, we will use X as is.Otherwise, we will remove it.
    if (firstcol == FALSE) { X1 = X.b } else {
      X1 = X.b[ ,2:(p+1)]
    }

    # X2 = X1 - n1 %*% colMeans(X1)
    X2 = sweep(x = X1, MARGIN = 2, STATS = colMeans(X1), FUN = "-")
    # s = as.matrix(sqrt(diag(crossprod(X2, X2))))
    s = as.matrix(sqrt(colSums(X2^2)))

    Z = sweep(x = X2, MARGIN = 2, STATS = as.vector(s), FUN = "/")


    # Z = X2[,1]/s[1,]
    #
    # for (j in 2:p){
    #   Z = as.matrix(cbind(Z, X2[,j]/s[j, ]))
    # }
    # kevinZ = sweep(x = X2, MARGIN = 2, STATS = as.vector(s), FUN = "/")
    # identical(unname(kevinZ), unname(Z))



    ## Z is the centering and standarding of X1 (Kevin: I think there is an easy way to do this.)
    # x.norm = as.matrix(sqrt(diag(t(X1) %*% X1)))

    # x.norm = as.matrix(sqrt(diag(crossprod(X1, X1))))
    x.norm = as.matrix(sqrt(colSums(X1^2)))

    v = as.vector(s/x.norm)
    D = diag(v)
    Z1 = Z %*% D
    ## Chen: note! I use Z*D rather than D to calculate the eigenvalue and variance inflation factors.

    ## Kevin This part could potentially be faster...
    # v2[,i] = 1/t(eigen(crossprod(Z1,Z1))$values)

    crossprodZ1 = crossprod(Z1, Z1)

    v2[,i] = 1/t(svd(crossprodZ1)$d)
    ## Inverse of vif of d
    vif[,i] = t(diag(solve(crossprodZ1)))
  } ## End the i for loop
  ##############################
  ## Kevin: When we have 1000 steps (i.e. number of resamples)
  ## We will always split the number of resamples into 10 groups (indexList). For each of the 10 groups, we
  ## will perform a linear regression against the inverse of the eigenvalues (v2) against the VIF (vif).
  ## We will store the t-statistics of the regressions into tstatList and tstatMat is the matrix version
  ## Each column of tstatMat correspond to one of the 10 groups.
  ## Each row of the tstatMat correspond to each of the original regressor, including an Intercept term.
  ## When calculating tor, we use the squared t value and remove the intercept term
  ## The matrix g is the row-proportion of tor.

  indexList = unname(base::split(1:steps, sort((1:steps) %% 10)))
  tor = matrix(0, p, p)



  for (j in 1:p){

    tstatList = lapply(
      indexList,
      function(thisIndex){
        lmObj = lm(v2[j, thisIndex] ~ t(vif[,thisIndex]))
        tstat = coef(summary(lmObj))[, "t value"]

      }
    )

    tstatMat = unname(do.call(cbind, tstatList))
    tor[j, ] = rowMeans(tstatMat^2)[-1]
  }

  g = 1 - sweep(tor, 1, rowSums(tor), "/")
  # ## g[j,i]: jth small eigenvalue with ith column vector
  # rownames(g) = paste0("eigen", 1:p)
  # colnames(g) = paste0("variable", 1:p)
  # ####################################################################
  # or = order(g[p,]) ## Order the columns of g by the smallest eigen value
  # or = or[1:vol.max]
  # g.or = g[,or]
  # if (vol.max > 1) {g.or = g.or[p:(p-eig.max+1),]} else {g.or = as.matrix(g.or[p:(p-eig.max+1)])}
  # if (eig.max == 1) {g.or = t(g.or)}
  #
  # ###############  ggplot #######################
  # dat = reshape2::melt(g.or,
  #                      varnames = c("X2", "X1"),
  #                      value.name = "weights") %>%
  #   dplyr::mutate(
  #     thickness = 1-weights,
  #     # thicknessCategory = cut(thickness, breaks = seq(0, 1, by = 0.2),
  #     #                         include.lowest = T,
  #     #                         ordered_result = T)
  #
  #     thicknessCategory = cut(thickness, breaks = quantile(thickness, probs = seq(0, 1, by = 0.2)),
  #                             labels = paste0("category", 1:5),
  #                             include.lowest = T,
  #                             ordered_result = T)
  #   )
  #
  # dat$x1_norm = rangeTransform(as.integer(dat$X1))
  # dat$x2_norm = rangeTransform(as.integer(dat$X2))
  #
  # dat$y1 = 0
  # dat$y2 = 1
  #
  #
  # axis_1 = data.frame(x=rangeTransform(as.integer(unique(dat$X1))),
  #                     y=0, label=as.character(unique(dat$X1)))
  #
  # axis_2 = data.frame(x=rangeTransform(as.integer(unique(dat$X2))),
  #                     y=1, label=as.character(unique(dat$X2)))
  #
  # gg = ggplot2::ggplot(data=dat) +
  #   theme_bw() +
  #   theme(axis.title=element_blank()) +
  #   theme(axis.text=element_blank()) +
  #   theme(axis.ticks=element_blank()) +
  #   theme(panel.grid=element_blank()) +
  #   geom_segment(aes(x=x1_norm, xend=x2_norm, y=y1, yend=y2, colour = X2,
  #                    size = thicknessCategory, alpha = thickness)) +
  #   scale_size_manual(values = c(0, 0.1, 0.2, 0.3, 1.5)) +
  #   geom_segment(x=0, xend=1, y=0, yend=0, size=0.7) +
  #   geom_segment(x=0, xend=1, y=1, yend=1, size=0.7) +
  #   # scale_colour_grey() +
  #   # scale_colour_brewer(palette="Set1") +
  #   scale_y_continuous(limits=c(-0.2, 1.2), expand=c(0, 0)) +
  #   geom_segment(data=axis_1, aes(x=x, xend=x, y=y, yend=y-0.025), size=0.7) +
  #   geom_segment(data=axis_2, aes(x=x, xend=x, y=y, yend=y+0.025), size=0.7) +
  #   geom_text(data=axis_1, aes(label=label, x=x, y=y - 0.075)) +
  #   geom_text(data=axis_2, aes(label=label, x=x, y=y + 0.075)) +
  #   labs(caption = "Largest Eigen = smallest Eigenvalue")
  #
  # ############################
  # ## igraph plotting
  #
  # g.or[g.or > 1-tau/p] = 1
  # ## For plotting purpose, if the values of g.or is above a certain threshold, then we set it to 1
  # col.names = col.names[or]
  # ## reorder the variables by the connection with the smallest eigenvalue
  #
  # M = matrix(1,eig.max,vol.max)
  # G = igraph::graph_from_incidence_matrix(M)
  #
  # vec = as.vector(t(g.or))
  #
  #
  # graph_attr(G,'weight') = vec
  # par(bg="white")
  # G.text<-paste('x',or," -- ", col.names,sep="")
  # val<-paste('v',1:eig.max,sep="")
  # col<-paste('x',or,sep="")
  #
  # plot(G,
  #      edge.color = grey(graph_attr(G,'weight')),
  #      vertex.size=20,
  #      vertex.label=c(val,col),
  #      vertex.color=c("yellow","cyan")[V(G)$type+1],
  #      edge.width=(rep(1,length(vec))-graph_attr(G,'weight'))*10,
  #      layout=layout_as_bipartite,
  #      main="mcvis"
  # )
  #
  # if (vol.max>1)
  # {text(x=rep(2,vol.max),y=(1:vol.max)*2/(1-vol.max)+(vol.max+1)/(vol.max-1),G.text)}
  #
  # if (vol.max==1) {text(x=2,y=0,G.text)}
  # text(x=-2,y=0,"v1: the smallest eigenvalue")
  #
  # ## No returns??
  # return(gg)
}
##################
rangeTransform = function(x){ (x - min(x)) / (max(x) - min(x))}
##################

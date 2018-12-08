#' @author Chen Lin
#' @title Multi-collinearity Visualization
#' @param X A matrix of regressors.
#' @param tau A parameter for plotting line thickness
#' @param eig.max The maximum number of eigenvalues to be displayed on the plot.
#' @param vol.max The maximum number of variables to be displayed on the plot.
#' @param method The resampling method for the data. Current supports "bootstrap" or "cv"(cross-validation).
#' @param steps Number of sampling runs we perform. Default to 1000.
#' @import igraph
#' @import ggplot2
#' @import purrr
#' @import magrittr
#' @export
#' @examples
#' library(mplot)
#' data("artificialeg")
#' p=dim(artificialeg)[2]-1
#' X=artificialeg[,1:p]
#' mcvis2(X)


mcvis2 <- function(X,
                   eig.max = dim(X)[2],
                   vol.max = dim(X)[2],
                   method = "bootstrap",
                   steps = 1000L
)
{
  n = dim(X)[1]
  p = dim(X)[2] ## We now enforce no intercept terms
  col.names = colnames(X)
  
  ## One can choose the max variables and eigenvectors in the plot
  
  ## Initialise the matrice
  vif = v2 = matrix(0, p, steps)
  
  X = as.matrix(X)
  
  if (method == "bootstrap") {
    index.b = replicate(steps, sample(n, replace = TRUE), simplify = FALSE)
  } 
  
  if (method == "cv") {
    index.b = replicate(steps, sample(n, replace = FALSE)[1:(floor(sqrt(p*n)))], simplify = FALSE)
  } 
  
  X1 = purrr::map(index.b, ~ X[.x, ])
  X2 = purrr::map(X1, ~ sweep(x = .x, MARGIN = 2, STATS = colMeans(.x), FUN = "-"))
  s = purrr::map(X2, ~ as.matrix(sqrt(colSums(.x^2))))
  Z = purrr::map2(
    .x = X2, 
    .y = s,
    .f = ~ sweep(x = .x, MARGIN = 2, STATS = as.vector(.y), FUN = "/")
  )
  x.norm = purrr::map(X1, ~as.matrix(sqrt(colSums(.x^2))))
  v = purrr::map2(.x = x.norm, 
                  .y = s, 
                  ~ as.vector(.y/.x))
  D = purrr::map(.x = v, .f = diag)
  Z1 = purrr::map2(Z, D, ~ .x %*% .y)
  crossprodZ1 = purrr::map(Z1, ~ crossprod(.x, .x))
  v2 = purrr::map(.x = crossprodZ1, ~ 1/svd(.x)$d) %>% 
    do.call(cbind, .)
  vif = purrr::map(.x = crossprodZ1, ~ diag(solve(.x))) %>% 
    do.call(cbind, .)
  ##############################
  
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
  rownames(g) = paste0("eigen", 1:p)
  colnames(g) = paste0("variable", 1:p)
  ####################################################################
  result = list(
    g = g, 
    col.names = col.names
  )
  return(result)
}


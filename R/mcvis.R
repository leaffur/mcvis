#' @author Chen Lin, Kevin Wang
#' @title Multi-collinearity Visualization
#' @param X A matrix of regressors (without intercept terms).
#' @param method The resampling method for the data. Current supports "bootstrap" or "cv"(cross-validation).
#' @param steps Number of sampling runs we perform. Default to 1000.
#' @import igraph
#' @importFrom magrittr %>%
#' @importFrom purrr map map2
#' @importFrom stats coef lm
#' @importFrom graphics par plot text
#' @export
#' @examples
#' library(mplot)
#' data("artificialeg")
#' X=artificialeg[,1:9]
#' mcvis(X)


mcvis <- function(X,
                   method = "bootstrap",
                   steps = 1000L)
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


  X1 = purrr::map(index.b, ~ X[.x, ]) ##Resampling
  X2 = purrr::map(X1, ~ sweep(x = .x, MARGIN = 2, STATS = colMeans(.x), FUN = "-")) ##Centering
  s = purrr::map(X2, ~ as.matrix(sqrt(colSums(.x^2))))
  Z = purrr::map2(
    .x = X2,
    .y = s,
    .f = ~ sweep(x = .x, MARGIN = 2, STATS = as.vector(.y), FUN = "/")
  ) ## Standardizing

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

  MC = 1 - sweep(tor, 1, rowSums(tor), "/")
  ## MC[j,i]: jth smallest eigenvalue with ith variable
  rownames(MC) = paste0("tau", p:1)
  colnames(MC) = paste0("col", 1:p)
  ####################################################################
  result = list(
    X = X,
    t_sqaure = tor,
    MC = 1-MC,
    col.names = col.names
  )
  return(result)
}


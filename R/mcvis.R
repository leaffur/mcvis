#' @author Chen Lin, Kevin Wang, Samuel Mueller
#' @title Multi-collinearity Visualization
#' @param X A matrix of regressors (without intercept terms).
#' @param method The resampling method for the data.
#' Currently supports "bootstrap" or "cv" (cross-validation).
#' @param steps Number of resampling runs we perform. Default is set to 1000.
#' @param k Number of partitions in averaging theMC index. Default is set to 10.
#' @return A list of outputs:
#' \itemize{
#' \item{X: }{The original matrix of regressors (for plotting purposes)}
#' \item{t_square: }{The t^2 statistics for the regression between the VIFs and the tau's.}
#' \item{MC: }{The MC indices}
#' \item{col.names: }{Column names (for plotting purposes)}
#' }
#' @import igraph
#' @importFrom magrittr %>%
#' @importFrom purrr map map2
#' @importFrom stats coef lm
#' @importFrom graphics par plot text
#' @export
#' @examples
#' set.seed(1)
#' p = 10
#' n = 100
#' X = matrix(rnorm(n*p), ncol = p)
#' X[,1] = X[,2] + X[,3] + rnorm(n, 0, 0.1)
#' mcvis_result = mcvis(X)
#' mcvis_result$MC


mcvis <- function(X,
                  method = "bootstrap",
                  steps = 1000L,
                  k = 10L)
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

  indexList = unname(base::split(1:steps, sort((1:steps) %% k)))
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

  MC = sweep(tor, 1, rowSums(tor), "/")
  ## MC[j,i]: jth smallest eigenvalue with ith variable
  rownames(MC) = paste0("tau", p:1)
  colnames(MC) = paste0("col", 1:p)
  ####################################################################
  result = list(
    X = X,
    t_square = tor,
    MC = MC,
    col.names = col.names
  )
  return(result)
}

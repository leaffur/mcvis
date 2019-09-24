#' @author Chen Lin, Kevin Wang, Samuel Mueller
#' @title Multi-collinearity Visualization
#' @param X A matrix of regressors (without intercept terms).
#' @param method The resampling method for the data.
#' Currently supports "bootstrap" or "cv" (cross-validation).
#' @param times Number of resampling runs we perform. Default is set to 1000.
#' @param k Number of partitions in averaging theMC index. Default is set to 10.
#' @return A list of outputs:
#' \itemize{
#' \item{X: }{The original matrix of regressors (for plotting purposes)}
#' \item{t_square: }{The t^2 statistics for the regression between the VIFs and the tau's.}
#' \item{MC: }{The MC indices}
#' \item{col.names: }{Column names (for plotting purposes)}
#' }
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
#' X[,1] = X[,2] + rnorm(n, 0, 0.1)
#' mcvis_result = mcvis(X)
#' mcvis_result$MC

mcvis <- function(X,
                   method = "bootstrap",
                   times = 1000L,
                   k = 10L)
{
  n = nrow(X)
  p = ncol(X) ## We now enforce no intercept terms

  if(is.null(colnames(X))){
    col_names = sprintf("col_%02d", seq_len(p))
  } else {
    col_names = colnames(X)
  }


  ## One can choose the max variables and eigenvectors in the plot

  ## Initialise the matrice

  X = as.matrix(X)

  if (method == "bootstrap") {
    index = replicate(times, sample(n, replace = TRUE), simplify = FALSE)
  }

  if (method == "cv") {
    index = replicate(times, sample(n, replace = FALSE)[1:(floor(sqrt(p*n)))], simplify = FALSE)
  }

  list_mcvis_result = purrr::map(.x = index,
                                 .f = ~ one_mcvis(X = X, index = .x))
  list_tau = purrr::map(list_mcvis_result, "tau") %>%
    do.call(cbind, .)
  list_vif = purrr::map(list_mcvis_result, "vif") %>%
    do.call(cbind, .)
  ##############################

  list_index_block = unname(base::split(1:times, sort((1:times) %% k)))
  t_square = matrix(0, p, p)

  for (j in 1:p){

    list_tstat = lapply(
      list_index_block,
      function(this_index){
        lm_obj = lm(list_tau[j, this_index] ~ t(list_vif[,this_index]))
        tstat = coef(summary(lm_obj))[, "t value"]
      }
    )

    tstat_mat = unname(do.call(cbind, list_tstat))
    t_square[j, ] = rowMeans(tstat_mat^2)[-1]
  }

  MC = t_square/rowSums(t_square)
  ## MC[j,i]: jth smallest eigenvalue with ith variable
  rownames(MC) = paste0("tau", p:1)
  colnames(MC) = paste0("col", 1:p)
  ####################################################################
  result = list(
    t_square = t_square,
    MC = MC,
    col_names = col_names
  )
  return(result)
}

one_mcvis = function(X, index){
  X1 = X[index, ] ## Resampling on the rows
  X2 = sweep(x = X1, MARGIN = 2, STATS = colMeans(X1), FUN = "-")
  s = as.matrix(sqrt(colSums(X2^2)))
  Z = sweep(x = X2, MARGIN = 2, STATS = as.vector(s), FUN = "/") ## Standardizing
  x_norm = as.matrix(sqrt(colSums(X1^2)))
  v = as.vector(s/x_norm)
  D = diag(v)
  Z1 = Z %*% D
  crossprodZ1 = crossprod(Z1, Z1)
  tau = 1/svd(crossprodZ1)$d
  vif = diag(solve(crossprodZ1))
  result = list(tau = tau,
                vif = vif)
  return(result)
}

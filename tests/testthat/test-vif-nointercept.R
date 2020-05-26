# set.seed(1)
# p = 10
# n = 100
# X = matrix(rnorm(n*p), ncol = p)
# X[,1] = X[,2] + rnorm(n, 0, 0.1)
# r2 = vector("numeric", p)
#
# for(j in 1:p){
#   y = X[,j,drop = FALSE]
#   r2[j] = summary(lm(y ~ X[,-j]))$r.squared
# }
#
# (vif_mcvis = 1/(1-r2))
#
# y = rnorm(n)
# df = data.frame(y, X)
# vif_car = car::vif(lm(y ~ ., data = df))
# vif_car
#
# expect_equal(unname(vif_car),
#              unname(vif_mcvis))
#
# X2 = sweep(x = X, MARGIN = 2, STATS = colMeans(X), FUN = "-")
# s = as.matrix(sqrt(colSums(X2^2)))
# Z = sweep(x = X2, MARGIN = 2, STATS = as.vector(s), FUN = "/") ## Standardizing
#
# crossprodZ = crossprod(Z, Z)
# svd_obj = svd(crossprodZ)
# tau = 1/svd_obj$d
# g = svd_obj$u
# head((g*g) %*% tau)

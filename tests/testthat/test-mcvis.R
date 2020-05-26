set.seed(5)
p = 4
n = 50
X = matrix(rnorm(n*p), ncol = p)
X[,1] = X[,2] + rnorm(n, 0, 0.1)

## studentise
mcvis_result = mcvis(X, standardise_method = "studentise")

## euclidean
mcvis_result = mcvis(X, standardise_method = "euclidean")

## none
mcvis_result = mcvis(X, standardise_method = "none")

## Test sampling methods
mcvis(X, sampling_method = "cv")
testthat::expect_error(mcvis(X, sampling_method = "abc"))

## Test error inputs
X[,1] = X[,2]
testthat::expect_warning(mcvis(X))
X[,1] = "abc"
testthat::expect_error(mcvis(X))

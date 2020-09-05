set.seed(5)
p = 4
n = 50
X = matrix(rnorm(n*p), ncol = p)
X[,1] = X[,2] + rnorm(n, 0, 0.1)

## default plotting
mcvis_result = mcvis(X, standardise_method = "studentise")
plot(mcvis_result, type = "ggplot")
plot(mcvis_result, type = "igraph")
plot(mcvis_result, type = "alt")

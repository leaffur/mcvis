set.seed(1)
p = 10
n = 100
X = cbind(Int = 1,
          matrix(rnorm(n*p), ncol = p))
X[,2] = X[,3] + rnorm(n, 0, 0.1)
r2 = vector("numeric", p + 1)

for(j in 1:(p+1)){
  y = X[,j,drop = FALSE]
  r2[j] = summary(lm(y ~ X[,-j]))$r.squared
}

vif_mcvis = 1/(1-r2)

y = rnorm(n)
df = data.frame(X, y)
vif_car = car::vif(lm(y ~ . -1, data = df))
vif_car

# expect_equal(unname(vif_car),
#              unname(vif_mcvis))

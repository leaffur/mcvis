test_that("basic mcvis works", {
  library(mplot)
  data("artificialeg")
  X = artificialeg[,1:9]
  mcvis(X)
})

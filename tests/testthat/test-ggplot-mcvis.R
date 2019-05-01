test_that("test ggplot mcvis", {
  library(mplot)
  data("artificialeg")
  X=artificialeg[,1:9]
  mcvis_result = mcvis(X)
  set.seed(1)
  vdiffr::expect_doppelganger(
    title = "Visualise tau against original variables",
    ggplot_mcvis(mcvis_result))
})

test_that("basic mcvis works", {
  library(mplot)
  data("artificialeg")
  X = artificialeg[,1:9]
  mcvis(X)
})


# set.seed(5)
# p = 4
# n = 50
# X = matrix(rnorm(n*p), ncol = p)
# X[,1] = X[,2] + rnorm(n, 0, 0.1)
# mcvis_result = mcvis(X)
# ggplot_mcvis(mcvis_result) +
#   ggsci::scale_color_d3() +
#   scale_size_discrete(c(5,4,3,2,1))
#   # scale_colour_brewer(palette = "Set1")
#
#
# imgurl <- "~/Desktop/logo1.png"
# hexSticker::sticker(imgurl, package = "mcvis",
#         s_x=1, s_y=.8, s_width=.6,
#         p_size=8, p_color = "#3E3E3B",
#         h_fill="white", h_color = "#2A93D5",
#         filename="inst/mcvis_logo.png")

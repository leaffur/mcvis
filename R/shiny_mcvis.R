#' @author Chen Lin, Kevin Wang
#' @title Shiny app for mcvis exploration
#' @param mcvis_result Output of the mcvis function
#' @param eig.max The maximum number of eigenvalues to be displayed on the plot.
#' @param vol.max The maximum number of variables to be displayed on the plot.
#' @import shiny
#' @importFrom DT renderDataTable
#' @export
#' @examples
#' library(mplot)
#' data("artificialeg")
#' p=dim(artificialeg)[2]-1
#' X=artificialeg[,1:p]
#' mcvis_result = mcvis2(X)
#' shiny_mcvis()

shiny_mcvis <- function() {

  appDir <- system.file("shiny", package = "mcvis")

  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `mcvis`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}

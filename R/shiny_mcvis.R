#' @author Chen Lin, Kevin Wang
#' @title Shiny app for mcvis exploration
#' @param mcvis_result Output of the mcvis function.
#' @import shiny
#' @import ggplot2
#' @importFrom dplyr case_when
#' @importFrom dplyr mutate
#' @importFrom DT renderDataTable
#' @importFrom psych describe
#' @export
#' @examples
#' library(mplot)
#' data("artificialeg")
#' p = dim(artificialeg)[2]-1
#' X = artificialeg[,1:p]
#' mcvis_result = mcvis(X)
#' shiny_mcvis(mcvis_result)

shiny_mcvis <- function(mcvis_result) {
  .GlobalEnv$mcvis_result <- mcvis_result
  # on.exit(rm(X, envir = .GlobalEnv))
  appDir <- system.file("shiny", package = "mcvis")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `mcvis`.", call. = FALSE)
  }
  shiny::runApp(appDir, display.mode = "normal")
}

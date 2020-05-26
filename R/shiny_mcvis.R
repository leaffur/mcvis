#' @author Chen Lin, Kevin Wang, Samuel Mueller
#' @title Shiny app for mcvis exploration
#' @param mcvis_result Output of the mcvis function.
#' @param X The original X matrix
#' @import ggplot2
#' @importFrom dplyr case_when
#' @importFrom dplyr mutate
#' @importFrom DT renderDataTable
#' @importFrom psych describe
#' @export
#' @examples
#' \dontrun{
#' set.seed(1)
#' library(mplot)
#' data('artificialeg', package = 'mplot')
#' p = dim(artificialeg)[2]-1
#' X = artificialeg[,1:p]
#' mcvis_result = mcvis(X)
#' shiny_mcvis(mcvis_result, X)
#' }

shiny_mcvis <- function(mcvis_result, X) {
    .GlobalEnv$shiny_mcvis_result <- c(mcvis_result, X = list(X))
    # on.exit(rm(X, envir = .GlobalEnv))
    appDir <- system.file("shiny", package = "mcvis")
    if (appDir == "") {
        stop("Could not find example directory. Try re-installing `mcvis`.", call. = FALSE)
    }
    shiny::runApp(appDir, display.mode = "normal")
}

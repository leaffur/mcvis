#' @author Chen Lin, Kevin Wang, Samuel Mueller
#' @title Shiny app for mcvis exploration
#' @param mcvis_result Output of the mcvis function.
#' @param X The original X matrix
#' @import ggplot2
#' @import shiny
#' @importFrom dplyr case_when
#' @importFrom dplyr mutate
#' @importFrom psych describe
#' @export
#' @examples
#' if(interactive()){
#' set.seed(1)
#' p = 10
#' n = 100
#' X = matrix(rnorm(n*p), ncol = p)
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

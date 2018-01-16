#' This is a helper function to launch the stminsights app
#'
#' \code{run_stminsights} launches the app to analyze Structural Topic models.
#' It requires uploading an R image containing an arbitrary number of stm models and effect estimates.
#' In addition, the image should contain the \code{out} object which was used to fit the models.
#' See the description for additional information.
#'
#' @import stm
#' @import igraph
#' @import tidygraph
#' @import ggraph
#' @import shiny
#' @import shinyBS
#' @import shinydashboard
#' @import scales
#' @import ggplot2
#' @import shinyjs
#' @import ggrepel
#' @import purrr
#' @import stringr
#' @import dplyr
#' @import tibble
#' @details To be written, sorry =D.
#' @export
#'
run_stminsights <- function() {
  appDir <- system.file("app", package = "stminsights")
  if (appDir == "") {
    stop("Could not find directory. Try re-installing `stminsights`.",
         call. = FALSE)
  }
  runApp(appDir, display.mode = "normal",
                launch.browser = TRUE)

  }

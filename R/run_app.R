#' @title launch the stminsights shiny app
#' @name run_stminsights
#' @description
#' \code{run_stminsights} launches the app to analyze Structural Topic models.
#' It requires an .RData file with stm objects as illustrated in the example below.
#'
#' @param use_browser Choose whether you want to launch the shiny app in your browser.
#' Defaults to \code{TRUE}.
#' @examples
#' \dontrun{
#'


#' library(stm)
#'
#' # required out object
#' out <- list(documents = poliblog5k.docs,
#'             vocab = poliblog5k.voc,
#'             meta = poliblog5k.meta)
#'
#' # one or several stm models and effect estimates
#' poli <- stm(documents = out$documents,
#'             vocab = out$vocab,
#'             data = out$meta,
#'             prevalence = ~ rating * s(day),
#'             K = 20)
#' prep_poli <- estimateEffect(1:20 ~ rating * s(day), poli,
#'                             meta = out$meta)
#'
#' poli_content <-  stm(documents = out$documents,
#'                      vocab = out$vocab,
#'                      data = out$meta,
#'                      prevalence = ~ rating + s(day),
#'                      content = ~ rating,
#'                      K = 15)
#' prep_poli_content <- estimateEffect(1:15 ~ rating + s(day), poli_content,
#'                                     meta = out$meta)
#'
#' # all objects stored in an .RData file
#' save.image('stm_poliblog5k.RData')
#'
#' # launch the app
#' if(interactive()){
#' run_stminsights()
#' }
#' }
#'
#'
#' @import stm
#' @import tidygraph
#' @import ggraph
#' @import shiny
#' @import shinyBS
#' @import shinydashboard
#' @import ggplot2
#' @import ggrepel
#' @import stringr
#' @import dplyr
#' @import tibble
#'
#' @export
#'
run_stminsights <- function(use_browser = TRUE) {
  appDir <- system.file("app", package = "stminsights")
  if (appDir == "") {
    stop("Could not find directory. Try re-installing `stminsights`.",
         call. = FALSE)
  }

  if (use_browser == TRUE)
  runApp(appDir, display.mode = "normal",
                launch.browser = TRUE)
  else runApp(appDir, display.mode = "normal")

  }

#' @title launch the stminsights shiny app
#' @name run_stminsights
#' @description
#' \code{run_stminsights} launches the app to analyze Structural Topic models.
#' It requires an .RData file with stm objects as illustrated in the example below.
#'
#' @param use_browser Choose whether you want to launch the shiny app in your browser.
#' Defaults to \code{TRUE}.
#' @examples
#'
#'  \dontrun{
#'
#' library(stm)
#' library(quanteda)
#'
#' # prepare data
#' data <- corpus(gadarian, text_field = 'open.ended.response')
#' docvars(data)$text <- texts(data)
#' data <- dfm(data, stem = TRUE, remove = stopwords('english'),
#'             remove_punct = TRUE) %>% dfm_trim(min_count = 2)
#' out <- convert(data, to = 'stm')
#'
#' # fit models and effect estimates
#' gadarian_3 <- stm(documents = out$documents,
#'                   vocab = out$vocab,
#'                   data = out$meta,
#'                   prevalence = ~ treatment + s(pid_rep),
#'                   K = 3,
#'                   max.em.its = 1, # reduce computation time for example
#'                   verbose = FALSE)
#'
#' prep_3 <- estimateEffect(1:3 ~ treatment + s(pid_rep), gadarian_3,
#'                          meta = out$meta)
#'
#' gadarian_5 <- stm(documents = out$documents,
#'                   vocab = out$vocab,
#'                   data = out$meta,
#'                   prevalence = ~ treatment + s(pid_rep),
#'                   K = 5,
#'                   max.em.its = 1, # reduce computation time for example
#'                   verbose = FALSE)
#'
#' prep_5 <- estimateEffect(1:5 ~ treatment + s(pid_rep), gadarian_5,
#'                          meta = out$meta)
#'
#' # save objects in .RData file
#' save.image(paste0(tempdir(), '/stm_gadarian.RData'))
#'
#' # launch the app
#' if(interactive()){
#'   run_stminsights()
#' }
#'
#' }
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

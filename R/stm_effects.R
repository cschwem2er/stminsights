#' @title extract stm effect estimates
#' @name get_effects
#' @description
#' \code{ stm_effects()} is a helper function to store effect estimates from
#' stm in a data frame.
#' @param estimates The object containing estimates calculated with
#' \code{\link[stm]{estimateEffect}}.
#'
#' @param variable The variable for which estimates should be extracted.
#'
#' @param type The estimate type. Can either be \code{'pointestimate'}
#' or  \code{'continuous'}.
#'
#' @param ci The confidence interval for uncertainty estimates.
#'  Defaults to  \code{0.95}.
#'
#' @param moderator The moderator variable in case you want to include
#'   an interaction effect.
#'
#' @param modval The value of the moderator variable for an interaction effect.
#'  See examples for combining data for multiple values.
#' @return
#'   Returns effect estimates in a tidy data frame.
#'
#' @examples
#'\dontrun{
#'
#' library(stm)
#' library(dplyr)
#' library(ggplot2)
#'
#' # store effects
#' prep <- estimateEffect(1:3 ~ treatment, gadarianFit, gadarian)
#'
#' effects <- get_effects(estimates = prep,
#'                       variable = 'treatment',
#'                       type = 'pointestimate')
#'
#' # plot effects
#' effects %>% filter(topic == 3) %>%
#' ggplot(aes(x = value, y = proportion)) +
#'  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.1, size = 1) +
#'  coord_flip() + theme_light() + labs(x = 'Treatment', y = 'Topic Proportion')
#'
#' # combine estimates for interaction effects
#' prep_int <- estimateEffect(1:3 ~ treatment*s(pid_rep), gadarianFit, gadarian)
#'
#' effects_int <- stm_effects(estimates = prep_int,
#'                           variable = 'pid_rep',
#'                           type = 'continuous',
#'                           moderator = 'treatment',
#'                           modval = 1) %>%
#'  bind_rows(
#'    get_effects(estimates = prep_int,
#'                variable = 'pid_rep',
#'                type = 'continuous',
#'                moderator = 'treatment',
#'                modval = 0)
#'  )
#'}
#'
#' @import stm
#' @import dplyr
#'
#' @export

get_effects <- function(estimates,
                        # estimates object
                        variable,
                        # variable for estimates
                        type ,
                        # continuous or pointestimate
                        ci = 0.95,
                        # confidence interval
                        moderator = NULL,
                        # moderator for interaction
                        modval = NULL) {

  if (!type %in% c('pointestimate', 'continuous'))
    stop("type must be set to pointestimate or continuous")
  # moderator value for interaction
  data <- plot.estimateEffect(
    estimates,
    variable,
    type,
    ci.level = ci,
    omit.plot = TRUE,
    moderator = moderator,
    moderator.value = modval
  )
  names(data$cis) <- data$topics
  names(data$means) <- data$topics
  names(data$uvals) <- data$topics

  tidy_stm <- data$topics %>% purrr::map(function(x) {
    x <- as.character(x)
    cis <- t(data$cis[[x]]) %>% as.data.frame() %>%
      purrr::set_names(c('lower', 'upper'))
    props <-
      tibble(
        value = data$uvals,
        proportion = data$means[[x]],
        topic = x
      ) %>%
      bind_cols(cis)
  })
  tidy_stm <- tidy_stm %>% bind_rows() %>%
    mutate(topic = factor(topic))
  if (type == 'pointestimate') {
    tidy_stm$value <- as.factor(tidy_stm$value)
  }
  if (!is.null(moderator)) {
    tidy_stm$moderator <- modval
  }
  return(tidy_stm)
}

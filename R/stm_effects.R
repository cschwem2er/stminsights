#' @title extract stm effect estimates
#' @name get_effects
#' @description
#' \code{ get_effects()} is a helper function to store effect estimates from
#' stm in a data frame.
#' @param estimates The object containing estimates calculated with
#' \code{\link[stm]{estimateEffect}}.
#'
#' @param variable The variable for which estimates should be extracted.
#'
#' @param type The estimate type. Must be either \code{'pointestimate'},
#' \code{'continuous'}, or \code{'difference'}.
#'
#' @param ci The confidence interval for uncertainty estimates.
#'  Defaults to  \code{0.95}.
#'
#' @param moderator The moderator variable in case you want to include
#'   an interaction effect.
#'
#' @param modval The value of the moderator variable for an interaction effect.
#'  See examples for combining data for multiple values.
#' @param cov_val1 The first value of a covariate for type \code{'difference'}.
#'
#' @param cov_val2 The second value of a covariate for type \code{'difference'}.
#' The topic proportion of \code{'cov_val2'} will be subtracted from the
#'  proportion of \code{'cov_val1'}.
#' @return
#'   Returns effect estimates in a tidy data frame.
#'
#' @examples
#'
#' library(stm)
#' library(dplyr)
#' library(ggplot2)
#'
#' # store effects
#' prep <- estimateEffect(1:3 ~ treatment + pid_rep, gadarianFit, gadarian)
#'
#' effects <- get_effects(estimates = prep,
#'                       variable = 'treatment',
#'                       type = 'pointestimate')
#'
#'
#' # plot effects
#' effects %>% filter(topic == 3) %>%
#' ggplot(aes(x = value, y = proportion)) +
#'  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.1, size = 1) +
#'  geom_point(size = 3) +
#'  coord_flip() + theme_light() + labs(x = 'Treatment', y = 'Topic Proportion')
#'
#'
#' # combine estimates for interaction effects
#' prep_int <- estimateEffect(1:3 ~ treatment * s(pid_rep),
#'  gadarianFit, gadarian)
#'
#' effects_int <- get_effects(estimates = prep_int,
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
#'
#' # plot interaction effects
#' effects_int %>% filter(topic == 2) %>%
#'  mutate(moderator = as.factor(moderator)) %>%
#'  ggplot(aes(x = value, y = proportion, color = moderator,
#'  group = moderator, fill = moderator)) +
#'  geom_line() +
#'  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2)  +
#'  theme_light() + labs(x = 'PID Rep.', y = 'Topic Proportion',
#'  color = 'Treatment', group = 'Treatment', fill = 'Treatment')
#'
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
                        modval = NULL,
                        # cov values for difference
                        cov_val1 = NULL,
                        cov_val2 = NULL) {
  data <- plot.estimateEffect(
    x = estimates,
    covariate = variable,
    method = type,
    ci.level = ci,
    moderator = moderator,
    moderator.value = modval,
    cov.value1 = cov_val1,
    cov.value2 = cov_val2,
    omit.plot = TRUE
  )


  # catching inconsistent stm naming conventions
  if (!'cis' %in% names(data)) {
    data$cis <- data$ci
  }

  if ('x' %in% names(data)) {
    data$uvals <- data$x
  }
  names(data$cis) <- data$topics
  names(data$means) <- data$topics

  tidy_stm <- data$topics %>% purrr::map(function(top) {
    top <- as.character(top)

    if (type == 'difference') {
      props <- tibble(
        difference = data$means[[top]],
        topic = top,
        lower = data$cis[[top]][[1]],
        upper = data$cis[[top]][[2]]
      )
    }

    else {
      cis <- t(data$cis[[top]]) %>% as_tibble() %>%
        purrr::set_names(c('lower', 'upper'))

      props <-
        tibble(
          value = data$uvals,
          proportion = data$means[[top]],
          topic = top
        ) %>%
        bind_cols(cis)
    }
  })

  tidy_stm <- tidy_stm %>% bind_rows()
  tidy_stm$topic <- as.factor(tidy_stm$topic)
  if (type == 'pointestimate') {
    tidy_stm$value <- as.factor(tidy_stm$value)
  }
  if (!is.null(moderator)) {
    tidy_stm$moderator <- modval
  }
  return(tidy_stm)
}

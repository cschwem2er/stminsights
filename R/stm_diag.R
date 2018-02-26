#' @title computes stm model diagnostics
#' @name get_diag
#' @description
#' \code{ get_diag()} is a helper function to compute average and median
#' \code{\link[stm]{semanticCoherence}} and \code{\link[stm]{exclusivity}} for
#' a number of  \code{\link[stm]{stm}} models. The function does not work for
#' models with content covariates.
#' @param models A list of stm models.
#'
#' @param outobj The \code{out} object containing documents for all stm models.
#' @return
#'   Returns model diagnostics in a tidy data frame.
#'
#' @examples
#'
#' library(stm)
#' library(dplyr)
#' library(ggplot2)
#' library(quanteda)
#'
#' # prepare data
#' data <- corpus(gadarian, text_field = 'open.ended.response')
#' docvars(data)$text <- texts(data)
#' data <- dfm(data, stem = TRUE, remove = stopwords('english'),
#'             remove_punct = TRUE)
#' out <- convert(data, to = 'stm')
#'
#' # fit models
#' gadarian_3 <- stm(documents = out$documents,
#'                   vocab = out$vocab,
#'                   data = out$meta,
#'                   prevalence = ~ treatment + s(pid_rep),
#'                   K = 3,
#'                   max.em.its = 2, # reduce computation time for example
#'                   verbose = FALSE)
#'
#' gadarian_5 <- stm(documents = out$documents,
#'                   vocab = out$vocab,
#'                   data = out$meta,
#'                   prevalence = ~ treatment + s(pid_rep),
#'                   K = 5,
#'                   max.em.its = 2, # reduce computation time for example
#'                   verbose = FALSE)
#'
#' # get diagnostics
#' diag <- get_diag(models = list(
#'                  model_3 = gadarian_3,
#'                  model_5 = gadarian_5),
#'                  outobj = out)
#' \dontrun{
#' # plot diagnostics
#' diag %>%
#' ggplot(aes(x = coherence, y = exclusivity, color = statistic))  +
#'   geom_text(aes(label = name), nudge_x = 5) + geom_point() +
#'   labs(x = 'Semantic Coherence', y = 'Exclusivity') + theme_light()
#' }
#'
#' @import stm
#' @import dplyr
#' @export

get_diag <- function(models, # list of stm models
                     outobj) # out object containing documents
{

  model_dfs <-  purrr::map2(models, names(models), function(x, y) {
    exclusivity_mod <- exclusivity(x)
    coherence_mod <- semanticCoherence(x,  outobj$documents)
    nr_topics_mod <- ncol(x$theta)

    model_df_mean <- tibble(
      exclusivity = mean(exclusivity_mod),
      coherence = mean(coherence_mod),
      statistic = 'mean',
      nr_topics = nr_topics_mod)
    model_df_median <- tibble(
      exclusivity = stats::median(exclusivity_mod),
      coherence = stats::median(coherence_mod),
      statistic = 'median',
      nr_topics = nr_topics_mod)

    model_df <- bind_rows(model_df_mean, model_df_median) %>%
      mutate(name = y)
    model_df
  })

  model_dfs <- bind_rows(model_dfs)
  return(model_dfs)

}



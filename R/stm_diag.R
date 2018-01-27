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
#' \dontrun{
#' library(stm)
#' library(dplyr)
#' library(ggplot2)
#'
#' # out object
#' out <- list(documents = poliblog5k.docs,
#'             vocab = poliblog5k.voc,
#'             meta = poliblog5k.meta)
#'
#' # one or several stm models
#' poli10 <- stm(documents = out$documents,
#'             vocab = out$vocab,
#'             data = out$meta,
#'             prevalence = ~ rating * s(day),
#'             K = 10)
#' poli20 <- stm(documents = out$documents,
#'             vocab = out$vocab,
#'             data = out$meta,
#'             prevalence = ~ rating * s(day),
#'             K = 20)
#' poli30 <- stm(documents = out$documents,
#'             vocab = out$vocab,
#'             data = out$meta,
#'             prevalence = ~ rating * s(day),
#'             K = 30)
#'
#' # get diagnostics
#' diag <- get_diag(models = list(
#'                  model_10 = poli10,
#'                  model_20 = poli20,
#'                  model_30 = poli30),
#'                  outobj = out)
#'
#' # plot diagnostics
#' diag %>%
#' ggplot(aes(x = coherence, y = exclusivity, color = statistic))  +
#'   geom_text(aes(label = name), nudge_x = 0.8) + geom_point() +
#'   labs(x = 'Semantic Coherence', y = 'Exclusivity') + theme_light()
#'}
#' @import stm
#' @import dplyr
#' @export

get_diag <- function(models, # list of stm models
                     outobj) # out object containing documents
{

  model_dfs <-  purrr::map2(models, names(models), function(x, y) {
    exclusivity_mod<- exclusivity(x)
    coherence_mod <- semanticCoherence(x,  outobj$documents)
    nr_topics_mod <- ncol(x$theta)

    model_df_mean <- tibble(
      exclusivity = mean(exclusivity_mod),
      coherence = mean(coherence_mod),
      statistic = 'mean',
      topics = nr_topics_mod)
    model_df_median <- tibble(
      exclusivity = stats::median(exclusivity_mod),
      coherence = stats::median(coherence_mod),
      statistic = 'median',
      topics = nr_topics_mod)

    model_df <- bind_rows(model_df_mean, model_df_median) %>%
      mutate(name = y)
    model_df
  })

  model_dfs <- bind_rows(model_dfs) %>%
    mutate(topics = as.factor(topics))

  return(model_dfs)

}



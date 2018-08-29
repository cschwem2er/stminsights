#' @title extract topic correlation network
#' @name get_network
#' @description
#' \code{ get_network()} is a helper function to extract stm topic correlation networks
#' as tidygraph objects and add labels and topic proportions.
#' @param model The stm model for computing the correlation network.
#'
#' @param method The method for determining edges. Can be either  \code{'simple'} or  \code{'huge'}.
#'
#' @param cutoff The correlation cutoff criterion for \code{method = 'cutoff'}. Defaults to \code{0.05}.
#'
#' @param labels An optional vector of topic labels. Must include a label for each topic of the model.
#'
#' @param cutiso Remove isolated notes without any edges from the network. Defaults to \code{FALSE}.
#' @return
#'   Returns tidygraph network of topic correlations.
#'
#' @examples
#'
#' library(stm)
#' library(ggraph)
#' library(quanteda)
#'
#' # prepare data
#' data <- corpus(gadarian, text_field = 'open.ended.response')
#' docvars(data)$text <- texts(data)
#' data <- dfm(data, stem = TRUE, remove = stopwords('english'),
#'             remove_punct = TRUE)
#' out <- convert(data, to = 'stm')
#'
#' # fit model
#' gadarian_10 <- stm(documents = out$documents,
#'                    vocab = out$vocab,
#'                    data = out$meta,
#'                    prevalence = ~ treatment + s(pid_rep),
#'                    K = 10,
#'                    max.em.its = 1, # reduce computation time for example
#'                    verbose = FALSE)
#'
#' # extract network
#' stm_corrs <- get_network(model = gadarian_10,
#'                          method = 'simple',
#'                          labels = paste('Topic', 1:10),
#'                          cutoff = 0.001,
#'                          cutiso = TRUE)
#'
#' \dontrun{
#' # plot network
#' ggraph(stm_corrs, layout = 'fr') +
#'   geom_edge_link(
#'     aes(edge_width = weight),
#'     label_colour = '#fc8d62',
#'     edge_colour = '#377eb8') +
#'   geom_node_point(size = 4, colour = 'black')  +
#'   geom_node_label(
#'     aes(label = name, size = props),
#'     colour = 'black',  repel = TRUE, alpha = 0.85) +
#'   scale_size(range = c(2, 10), labels = scales::percent) +
#'   labs(size = 'Topic Proportion',  edge_width = 'Topic Correlation') +
#'   scale_edge_width(range = c(1, 3)) +
#'   theme_graph()
#' }
#'
#' @import stm
#' @import dplyr
#' @import tidygraph
#'
#' @export
#'
#'
## quiets concerns of R CMD check for non standard evaluation
utils::globalVariables(c( "edges", "weight", "nodes", "degree", "." ))

get_network <- function(model,
                        method = 'simple',
                        cutoff = 0.05,
                        labels = NULL,
                        cutiso = FALSE) {

    # calculate topic correlation graph
    if (method == "simple") {
      cormat <-
        topicCorr(model, method = 'simple', cutoff = cutoff)$poscor
    }
    else {
      cormat <- topicCorr(model, method = 'huge')$poscor
    }

    g <- igraph::simplify(igraph::graph.adjacency(cormat,
                                               mode = 'undirected',
                                               weighted = TRUE))

    if (length(igraph::E(g)) == 0) {
      stop(
        "There are no (sufficiently high) correlations between the topics of this STM model."
      )
    }

    if (!is.null(labels)) {
    igraph::V(g)$name <- labels
    }

    igraph::V(g)$props <- colMeans(model$theta)

    graph_tidy <- as_tbl_graph(g) %>%
      mutate(degree = centrality_degree(loops = FALSE)) %>%
      activate(edges) %>%
      filter(!edge_is_loop()) %>%
      mutate(weight = round(weight, 2),
             edge_label = as.character(weight))

    if (cutiso == TRUE) {
      graph_tidy <- graph_tidy %>% activate(nodes) %>%
        filter(degree > 0)
    }
    return(graph_tidy)
  }




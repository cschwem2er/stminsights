######################
# imports & settings #
######################

library(stm)
library(dplyr)
library(igraph)
library(ggnet)
library(ggplot2)
library(plotly)
library(shiny)
library(shinyBS)
library(shinydashboard)
library(scales)
library(LDAvis)
library(Rtsne)
library(stringr)
library(WriteXLS)
# library(largeVis) not implemented at the moment


options(scipen=999)


####################
# loading stm data #
####################

load('data/path_to_image.RData') # load R environment

# The environment needs to include three objects:
# 1) "model" -> the estimated stm model
# 2) "out" -> stm meta data generated with stm.prepDocuments
# 3) "prep" -> effect estimates for all topics generated with stm.estimateEffect


####################
# helper functions #
####################

props <- model$theta # topic proportions
colnames(props) <- 1:ncol(props)  # topic names
columns <- names(out$meta) # meta columns
theme_set(theme_light(base_size = 16)) # adjust graph settings


plotGraph <- function(g, eweight,  labels) {
  # generate topic correlation graph
  
  if(length(V(g)) == 0) return()
  ew <- eweight * 5
  gnet <- intergraph::asNetwork(g)
  if (labels==F) {e.labels <- NULL}
  else e.labels <- ifelse(rep(labels, length(E(g))), round(E(g)$weight, 2), "")
  
  ggnet2(gnet, mode = "fruchtermanreingold", 
         edge.size = E(g)$weight * ew + 0.1, label = T, 
         size= V(g)$props , max_size=25,
         label.color = "grey20", label.size= 4,    
        edge.lty = 1, 
         edge.alpha = 0.9,
         color="#377eb8", 
         edge.color="grey10",
         node.alpha=0.95,
         edge.label = e.labels,
         edge.label.size = 4,
         edge.label.color = "#fb8072", edge.label.alpha = 0.9) +
    guides(size = F, color=F) 
    #+ labs(title="Topic Correlation Graph") 
   
}

prepLDAvis <- function (model, docs, nrwords, method,
                        perp) {
  # generate output for LDAvis  
  

  
  svd_tsne <- function(x) Rtsne(x,
                                perplexity=perp)$Y

  if (method == "t-SNE") {
   
    scaling <- svd_tsne
    axlabs <- list(xlab="tSNE1", ylab="tSNE2")
  }
  
  #largevis <- function(x) {
  #  dat <- scale(x)
  #  dat <- t(dat)
  #  lvis <- vis(dat)$coords
  #} 
  
  #if (method == "largeVis") {
  #
  #  scaling <- largevis
  #  axlabs <- list(xlab="largeVis1", ylab="largeVis2")
  #}
  
  else  {
    scaling <- jsPCA
    axlabs <- list(xlab="PCA1", ylab="PCA2")
  }
  
  theta<-model$theta
  phi <- exp(model$beta$logbeta[[1]])
  if(any(phi==0)){
    phi<-phi + .Machine$double.eps
    phi<-phi/rowSums(phi)
  }
  vocab <- model$vocab
  doc.length <- as.integer(unlist(lapply(docs, function(x) sum(x[2,]))))
  term.frequency <- model$settings$dim$wcounts$x
  f<-createJSON(phi=phi,theta=theta,doc.length=doc.length,vocab=vocab,
                        term.frequency=term.frequency,
                plot.opts = axlabs,
                R=nrwords,
                mds.method = scaling)
}


filter_terms <- function(inputstring){
  # takes string with keywords and creates regex
  
  low_str <- str_to_lower(inputstring)
  terms <- low_str %>% 
    str_split(.,',') 
  regex <- str_c(terms[[1]], '|', collapse="") %>% 
    str_sub(., end= -2)
  return(regex)
}


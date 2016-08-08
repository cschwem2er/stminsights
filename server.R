  # loads helper functions and datasets



shinyServer(function(input,output, session){



  
  # Dynamically create text inputs for topic labels  
  output$textInputs <- renderUI({
    w <- 1:ncol(props)
    uis <- vector("list", length(w))
    for(i in seq(w)) {
     
       if(nchar(as.character(i) ) == 1) {
         uis[[i]] <- textInput(paste("TL", "0", i, sep = ""), paste("Topic ", i, sep = ""),
                               value = as.character(paste0('0', w[i])))
      
      }
      else uis[[i]] <- textInput(paste("TL", i, sep = ""), paste("Topic ", i, sep = ""),
                           value = as.character(w[i]))
      
    }
    uis
  })
  
  
  
  

  # dynamically update topic labels
  tlabels <- reactive({
    tnames <- str_subset(names(input), '^TL')
    topiclabels <-  list()
    #tlabs <- str_extract_all('^TL', names(input))
    for (i in tnames) {
      topiclabels[[i]] <- input[[i]]
    }
   print(unlist(topiclabels, use.names=F))
   print(unlist(names(topiclabels)))
   return(unlist(topiclabels, use.names=F))

  })
 
  
  
  modvalues <- reactive({
    modvar <-out$meta[[input$modvar]]
    #return(unique(modvar))
    return(levels(modvar))
    
  })
  
  observe({
    
    updateSelectInput(session, "topic", choices = tlabels(),
                      selected=tlabels()[1])
    updateSelectInput(session, "effectTopic", choices = tlabels(),
                      selected=tlabels()[1])
    updateSelectInput(session, "perspTopic1", choices = tlabels(),
                      selected=tlabels()[1])
    updateSelectInput(session, "perspTopic2", choices = tlabels(),
                      selected=tlabels()[2])
    updateSelectInput(session, "modval1", choices = modvalues(),
                      selected=modvalues()[1])
    updateSelectInput(session, "modval2", choices = modvalues(),
                      selected=modvalues()[2])
  })
  


  # Find topic documents
  topicDocs <- reactive({

    
    validate(
      need(length(tlabels()) == ncol(props), "Loading, please wait..")
    )

    t <- which(input$topic == tlabels())
 

    nrt <- as.numeric(input$nrthoughts)
    thoughts <- findThoughts(model, n=nrt, topics=c(t),
                             texts=out$meta[[input$doccol]])$docs[[1]]
    
    topicThoughts <- thoughts[1:nrt]
    thoughtdf <-data.frame(topicThoughts, stringsAsFactors=F)
    names(thoughtdf) <- " "

    return(thoughtdf)
  }) 
  
  # Find topic terms
  topicTerms <- reactive({
    
    validate(
      need(length(tlabels()) == ncol(props), "Loading, please wait..")
    )

    
    t <- which(input$topic == tlabels())
    nrterms <- as.numeric(input$nrwords)
    labels <- labelTopics(model, n=nrterms)
    
    labList <- list()
    if (1 %in% input$labtypes) {
      prob <- str_c(labels$prob[t,][1:nrterms], collapse=', ')
      labList$Probability <- prob

    }
    if (2 %in% input$labtypes) {
      frex<- str_c(labels$frex[t,][1:nrterms], collapse=', ')
      labList$FREX <- frex
    }
    
    if (3 %in% input$labtypes) {
      lift<- str_c(labels$lift[t,][1:nrterms], collapse=', ')
      labList$Lift <- lift
    }
    
    if (4 %in% input$labtypes) {
      score<- str_c(labels$score[t,][1:nrterms], collapse=', ')
      labList$Score <- score
    }
    
    labDf <- data.frame(labList)


    return(labDf)
  }) 
  
  # Dataframe for topic docs
  output$tlabel <- renderDataTable(
    topicDocs(), 
    options = list(
      
      pageLength = 1,
      searching = FALSE,
      autoWidth=TRUE,
      scrollX = TRUE,
      info=FALSE,
      lengthMenu=c(1,2,5),
      lengthChange = T)
  )
  
  # Dataframe for topic terms
  output$tterms <- renderDataTable(
    topicTerms(), 
    options = list(
      
      pageLength = 1,
      searching = FALSE,
      autoWidth=TRUE,
      scrollX = TRUE,
      lengthChange = FALSE,
      info = FALSE,
      paging=FALSE)
  )
  
  # Topic proportions plot          
  plotTopicProps <- function(proportions) {
    if (length(tlabels()) > 0) {
      
      names_ <- tlabels()
    }
    else {names_ <- colnames(proportions)}
    frequency <-colMeans(proportions)
    order <- order(frequency, decreasing = F)
    percentage <- frequency[order]
    names_ <- names_[order]
    topic <- factor(names_, levels=names_)
    combined <- data.frame(percentage, topic)
    p <- ggplot(combined, aes(x=topic, y=percentage)) +
      geom_bar(stat="identity",
               fill="#377eb8", color="#000000") +
      coord_flip() + scale_y_continuous(labels=percent ) +
      labs(y = "Relative Frequency", x = "Topics") +
      
      theme(axis.text=element_text(size=14))
    return(ggplotly(p))
  }
  
  
  output$topicprops <- renderPlotly({
    plotTopicProps(props)
  })
  
  
  
  # generate effectplot
  output$effectplot <- renderPlot({
    
    # inputs
    scalemax <- input$scalemax
    scalemin <- input$scalemin
    plotV <- input$plotVar
    plotT <- which(input$effectTopic == tlabels())
    plotPers1 <- which(input$perspTopic1 == tlabels())
    plotPers2 <- which(input$perspTopic2 == tlabels())
    plotM <- input$plotType
    type <- input$plotType
    interaction <- input$moderator
    
    # differences
    if (type == "difference") {
      covalue1 <- input$covar1
      covalue2 <- input$covar2
    }
    else {
      covalue1 <- NULL
      covalue2 <- NULL
    }
    

    
    # wordclouds
    if (type == "wordcloud") {
      return(cloud(model, topic=plotT,
                   max.words=input$cloud_words,
                   scale = c(scalemax, scalemin))
      )
    }
    
    # catching continous variables for pointestimate selection
    if (type == "pointestimate" & length(table(out$meta[[plotV]])) > 10) {
      stop("Please only use categorical variables for pointestimate plots.")
      
    }
    if (type == "pointestimate") {
    return(plot.estimateEffect(x=prep, 
                        covariate= plotV,
                        topics=plotT,
                        
                        method="pointestimate",
                        main=paste('Topic: ', as.character(input$effectTopic)),
                        printlegend=F,
                        ci.level=input$effectci))
    }
    # perspectives
    if (type == "perspectives") {
      return(plot.STM(model, topics=c(plotPers1, plotPers2),
                      type="perspectives",
                      plabels=c(input$perspTopic1, input$perspTopic2),
                      n=input$persp_words,
                      text.cex = input$persp_cex)
      )
    }
    
    # interaction effects
    if (input$moderator ==T) {
      moderator <- input$modvar
      modval1 <- input$modval1
      modval2 <- input$modval2
    }
    else {
      moderator <- NULL
      modval1  <- NULL
      modval2 <- NULL
    }
    
    
    if (interaction == F & type=="continuous") {
      
     return(plot.estimateEffect(x=prep, 
                                  covariate= plotV,
                                  topics=plotT,
                                  
                                  method="continuous",
                                  main=paste('Topic: ', as.character(input$effectTopic)),
                                  printlegend=F,
                                  ci.level=input$effectci,
                                    xlab=plotV)
     )
      
      
    }
    
    
    
    
    if (interaction == T & type=="continuous") {

      

      
      x <- plot.estimateEffect(prep, covariate = plotV, model = model,
                             method = "continuous",  moderator = moderator,
                             moderator.value = modval1, linecol = "blue", ylim = c(0, input$ylim),
                             printlegend = F,
                             xlab=plotV)
      
      y <-plot.estimateEffect(prep, covariate = plotV, model = model,
                                method = "continuous", moderator = moderator,
                                moderator.value = modval2, linecol = "red", add = T,
                                printlegend = F)
      legend("bottom", legend= c(modval1, modval2), lwd = 2, col = c("blue", "red"))
      
      return(y)
      
    }

    
    
    
  
    
  }, res=90)  
  
  
  

  
  # Generating LDAvis output
  withProgress(message = 'Preparing data..',
               value = 0, 
               {
                 output$stmVis <- renderVis({ prepLDAvis(model, out$documents, 
                                                         input$visTerms,
                                                         input$scaling,
                                                         input$perplexity)})
                 
               })
  
  # Generating correlation graph
  withProgress(message = 'Creating graph..',
               value = 0, 
               {
                 
                 calcGraph <- function(model, method, cutoff=0.05) {
                   # calculate topic correlation graph
                   if (method=="cutoff") {
                     cormat <- topicCorr(model, method = 'simple', cutoff=cutoff)$poscor
                   }
                   else { cormat <- topicCorr(method = 'huge', model)$poscor}
                   
                   
                   g<- simplify(graph.adjacency(cormat, mode='undirected', weighted=TRUE))
                   V(g)$name <- tlabels()
                   V(g)$props <- colMeans(model$theta) * 100
                   isolated <- V(g)[igraph::degree(g) == 0] # dropping nodes without any edges
                   g <- delete_vertices(g, isolated)
                   
                 }
                 
                 
                 
                    graph <- reactive({
                      calcGraph(model, input$graphmethod, input$cutoff) 
                       })
               })

                 output$graphplot <- renderPlot({
                   
                   plotGraph(graph(),eweight=input$eWeight, 
                             labels=input$eLabels)
 
                
                  }, res=90)
  
                 #output$downloadNetwork <- downloadHandler(
                 #  filename = 'networkplot.png',
                  # content = function(file) {
                  #   ggsave(file,
                   #         plot =  plotGraph(graph(),eweight=input$eWeight, 
                    #                          labels=input$eLabels), 
                   #         device = "png")
                  # }) 



  
  
  
  # Filter for meta data
  meta_filtered <- reactive({
    m <- out$meta
    
    if (! "All columns" %in% input$columns) {
      
      m <- select(m, match(input$columns, names(m)))
    }
    
    if (input$filter != "" & input$fcol != "None") {
      fcolumn <- input$fcol
      if (str_sub(input$filter, -1L) == ",")
      {nocomma <- str_sub(input$filter, 1L,-2L)
      fterms <- filter_terms(nocomma)
      }
      else {fterms <- filter_terms(input$filter)}
      
      m$termfilter <- str_detect(str_to_lower(m[[fcolumn]]), fterms)
      m <- filter(m,  termfilter == T)
      m <- subset(m, select = -c(termfilter) )
    }
    
    
    if (input$regfilter != ""& input$fcol != "None") {
      fcolumn <- input$fcol
      m$filter_reg <- str_detect(m[[fcolumn]], input$regfilter)
      m <- filter(m,  filter_reg == T)
      m <- subset(m, select = -c(filter_reg) )
    }
    
    if (input$exfilter != ""& input$fcol != "None") {
      fcolumn <- input$fcol
      if (str_sub(input$exfilter, -1L) == ",")
      {nocomma_x <- str_sub(input$exfilter, 1L,-2L)
      exterms <- filter_terms(nocomma_x)
      }
      else {exterms <- filter_terms(input$exfilter)}
      m$filter_ex <- str_detect(str_to_lower(m[[fcolumn]]), exterms)
      m <- filter(m,  filter_ex == F)
      m <- subset(m, select = -c(filter_ex) )
    }
    
    return(m)
  })
  
  
  # Dataframe for metadata
  output$metadata <- renderDataTable(
    meta_filtered(), 
    options = list(
      
      pageLength = 3,
      searching = FALSE,
      autoWidth=TRUE,
      scrollX = TRUE,
      lengthMenu=c(1,2,5,10,25))
  )
  
  
  ### model infos
  
  modelcall <- model$settings$call[ 2:8]
  modelcall$estimateEffects <- gsub("  ", "", paste(format(prep$formula), collapse = "")) 
  modelframe <- data.frame(names(modelcall), as.character(modelcall)) 
  names(modelframe) <- c("Attribute", "Value")
  
  output$modelinfo <- renderDataTable(
    modelframe, 
    options = list(
      
      pageLength = 1,
      searching = FALSE,
      autoWidth=TRUE,
      scrollX = FALSE,
      lengthChange = FALSE,
      info = FALSE,
      paging=FALSE)
  )
  
  ### label dataframe
  
  labelframe <- reactive({
    tnames <- str_subset(names(input), '^TL')
    topiclabels <-  list()

    for (i in tnames) {
      topiclabels[[i]] <- input[[i]]
    }
    labels <- unlist(topiclabels, use.names=F)
    ids <- unlist(names(topiclabels))
    ids <- str_replace(ids, "TL", "")
    
    frequency <- round(colMeans(props), 3)
    return(data.frame(Topic=ids, Label=labels, Proportion=frequency))
    
  })
  
  
  output$labelframe <- renderDataTable(
    labelframe(), 
    options = list(
      
      pageLength = 1,
      searching = FALSE,
      autoWidth=TRUE,
      scrollX = FALSE,
      lengthChange = FALSE,
      info = FALSE,
      paging=FALSE)
  )
  
  
  output$downloadLabels <- downloadHandler(
    filename = function() { 'stmInsights_topiclabels.xlsx' },
    content = function(file) {
      WriteXLS(labelframe(), file,
               Encoding='UTF-8')
      
    }
  )
  

})


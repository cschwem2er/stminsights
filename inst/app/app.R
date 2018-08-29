library(shiny)
library(shinyBS)
library(shinydashboard)
library(stm)
library(tidygraph)
library(ggraph)
library(stringr)
library(ggplot2)
library(ggrepel)
library(tibble)
library(dplyr)
library(readr)




ui <- dashboardPage(
  header = dashboardHeader(title = 'stminsights'),

  #### sidebar ####

  sidebar = dashboardSidebar(
    conditionalPanel(
      condition = "input.tabvals == 1",





      h4("Topic"),
      selectInput(
        "topic",
        label = NULL,
        choices = c(1),
        selected = 1
      ),
      bsTooltip('topic',
                "Select the topic to inspect.",
                placement = "right"),

      bsTooltip(
        "plotVar",
        "Select the variable. Cannot be categorical for continuous plots. ",
        placement = "right"
      ),



      h4("Terms"),
      checkboxGroupInput(
        "labtypes",
        "Label types",
        c(
          "Probability" = 1,
          "FREX" = 2,
          "Lift" = 3,
          "Score" = 4
        ),
        selected = c(1, 2)
      ),
      bsTooltip('labtypes',
                "Check which labeltypes you want to use.",
                placement = "right"),

      sliderInput(
        'nrwords',
        'Nr. of Terms',
        min = 1,
        max = 50,
        value = 10
      ),
      bsTooltip('nrwords', "The number of terms to be displayed",
                placement = "right"),


      h4("Documents"),


      uiOutput('doccol'),


      bsTooltip(
        'doccol',
        "Select the column of your meta dataframe to be displayed."
      )

    ),



    conditionalPanel(
      condition = "input.tabvals == 7",

      bsTooltip(
        'effectschoice',
        "Select the correct effect estimates for your STM model.",
        placement = "right"
      ),
      selectInput(
        'plotType',
        # type "difference" is not implemented at the moment
        label = "Type",
        choices = c("wordcloud", "perspectives", "pointestimate", "continuous"),
        selected = "wordcloud"
      ),

      bsTooltip('plotType', "Choose the plot type to be displayed.",
                placement = "right")



    ),


    conditionalPanel(
      condition = "input.tabvals == 7 && input.plotType != 'perspectives'",

      htmlOutput("effectTopic"),


      bsTooltip(
        'effectTopic',
        "Select the topic for the plot creation.",
        placement = "right"
      )
    ),




    conditionalPanel(
      condition = "input.tabvals == 7 && (input.plotType == 'continuous' || input.plotType == 'pointestimate')",

      htmlOutput("plotVar"),
      htmlOutput("plotLabel"),
      htmlOutput("plotLabel2"),

      sliderInput(
        'effectci',
        'Confidence Intervall',
        min = 0.55,
        max = 0.99,
        value = 0.95
      ),
      bsTooltip(
        'effectci',
        "Width of confidence intervalls can be adjusted here.",
        placement = "right"
      )



    ),


    conditionalPanel(
      condition = "input.tabvals == 7 && (input.plotType == 'continuous')",
      #|| input.plotType == 'pointestimate' not implemented for now
      checkboxInput('moderator',
                    label = "Interaction Effect"),
      bsTooltip(
        'moderator',
        "Plot an interaction effect for a categorical variable.",
        placement = "right"
      )
    ),





    conditionalPanel(
      condition = "input.tabvals == 7 && input.plotType == 'wordcloud'",
      sliderInput(
        'cloud_words',
        'Maximum nr. of words',
        min = 10,
        max = 100,
        value = 50
      ),
      bsTooltip(
        'cloud_words',
        "The maximum number of words to be included in the cloud.",
        placement = "right"
      ),

      sliderInput(
        'scalemax',
        'Maximum word scaling',
        min = 1,
        max = 10,
        value = 3
      ),
      bsTooltip('scalemax', "Adjust the maximum word size.",
                placement = "right"),

      sliderInput(
        'scalemin',
        'Minimum word scaling',
        min = .2,
        max = 2,
        value = .5
      ),
      bsTooltip('scalemin', "Adjust the minimum word size.",
                placement = "right")
    ),




    conditionalPanel(
      condition = "input.tabvals == 7 && input.plotType == 'perspectives'",



      htmlOutput("perspTopic1"),
      bsTooltip(
        'perspTopic1',
        "Choose the first topic for the perspective plot.",
        placement = "right"
      ),

      htmlOutput("perspTopic2"),
      bsTooltip(
        'perspTopic2',
        "Choose the second topic for the perspective plot.",
        placement = "right"
      ),

      sliderInput(
        'persp_words',
        'Number of words',
        min = 5,
        max = 50,
        value = 25
      ),
      bsTooltip(
        'persp_words',
        "Number of words to be displayed in the perspective plot.",
        placement = "right"
      ),

      sliderInput(
        'persp_cex',
        'Text Scaling',
        min = 0.1,
        max = 2,
        value = 1
      ),
      bsTooltip(
        'persp_cex',
        "Controls the scaling constant on text size.",
        placement = "right"
      ),
     checkboxInput(
        'contLabels',
        'Change content categories',
        value = FALSE
      ),
      bsTooltip(
        'contLabels',
        "Check if you want to change the categories to plot. Only affects content models.",
        placement = "right"
      )


    ),



    conditionalPanel(
      condition = "input.tabvals == 7 && input.plotType == 'perspectives' && input.contLabels == true",

      htmlOutput("perspCat1"),
      bsTooltip(
        'perspCat1',
        "Choose the first category for the content perspective plot.",
        placement = "right"
      ),

      htmlOutput("perspCat2"),
      bsTooltip(
        'perspCat2',
        "Choose the second category for the content perspective plot.",
        placement = "right"
      )
    ),



    conditionalPanel(
      condition = "input.tabvals == 7 && input.moderator == true && (input.plotType == 'continuous')",
      # || input.plotType == 'pointestimate') not implemented for now

      htmlOutput("modvar"),
      bsTooltip('modvar',
                "Select a categorical moderator variable.",
                placement = "right"),

      selectInput("modval1",
                  choices = "None",
                  label = "Interaction value 1"),

      selectInput("modval2",
                  choices = "None",
                  label = "Interaction value 2")

    ),



    conditionalPanel(
      condition = "input.tabvals == 4",

      selectInput(
        "graphmethod",
        label = "Method",
        choices = c("cutoff", "huge")
      ),
      bsTooltip(
        'graphmethod',
        'Choose edges are determined algorithmically\\
        or by a correlation cutoff criterium.',
        placement = "right"
      )
    ),

    conditionalPanel(
      condition = "input.tabvals == 4 && input.graphmethod == 'cutoff'",

      sliderInput(
        "cutoff",
        label = h4("cutoff"),
        min = 0.01,
        max = 0.5 ,
        value = 0.05
      ),
      bsTooltip('cutoff', 'Minimum correlation between topic pairs.',
                placement = "right")
    ),

    conditionalPanel(
      condition = "input.tabvals == 4",
      checkboxInput(
        "cutiso",
        label = h4("Remove isolated Nodes"),
        value = FALSE
      ),
      bsTooltip(
        'cutiso',
        'Based on the correlation criteria isolated topics have no connections to other topics.',
        placement = "right"
      ),
      checkboxInput("eLabels", label = h4("Show weights"),
                    value = FALSE),
      bsTooltip(
        'eLabels',
        'Edge labels display correlations between topics.',
        placement = "right"
      ),

      checkboxInput(
        "includelegend",
        label = h4("Include legend"),
        value = FALSE
      ),
      bsTooltip(
        'includelegend',
        'Include scaling guides for the plot.',
        placement = "right"
      )
    )

    ),

  #### body ####

  body = dashboardBody(fluidRow(column(
    width = 12,
    tabBox(
      id = 'tabvals',
      width = NULL,



      tabPanel(
        'Info & Topics',
        shinyjs::useShinyjs(),

        # tags$a(href="javascript:history.go(0)",
        #        popify(tags$i(class="fa fa-refresh fa-5x"),
        #               title = "Reload",
        #               content = "Click here to restart the Shiny session",
        #               placement = "right")),

        tags$style(
          type = "text/css",
          ".shiny-output-error { visibility: hidden; }",
          ".shiny-output-error:before { visibility: hidden; }"
        ),

        h1("stminsights"),
        # p(
        #   "This app enables interactive exploration of",
        #   a("Structural Topic Models.",
        #     href = "http://structuraltopicmodel.com/", target = "_blank"),
        #   "For most of the sidebar inputs a short description is available if you
        #   hover over them. In case you are not familiar with STM, the authors describe
        #   the model in the corresponding ",
        #   a("package vignette.",
        #     href = "https://cran.r-project.org/web/packages/stm/vignettes/stmVignette.pdf",
        #     target = "_blank")
        # ),


        fileInput('rfile', 'Read in STM data', accept = c('.RData', '.Rda')),


        bsTooltip(
          'rfile',
          "Upload your.Rdata file with STM models, effect estimates and an 'out' object here.",
          placement = 'bottom'
        ),


        h3('Model Selection'),
        p('Select the STM model and corresponding effect estimates here.'),

        uiOutput('modelchoice'),

        bsTooltip('modelchoice',
                  "Select the STM model.",
                  placement = "top"),
        uiOutput('effectschoice'),
        bsTooltip(
          'effectschoice',
          "Select the correct effect estimates for your model.",
          placement = "top"
        ),


        h3('Topics'),
        p('Inspect the most important words for the selected topic.'),

        h4('Words'),
        dataTableOutput('tterms'),
        h4('Documents'),
        p(
          'Inspect the documents with the highest proportions for the selected topic.'
        ),

        dataTableOutput('tlabel'),

        h4("Labels"),
        p(
          "Use the boxes to enter labels for your topics. It is suggested
          to keep the labels short."
        ),
        uiOutput("textInputs"),

        value = 1
        ),



      tabPanel(
        'Proportions',

        p('This plot shows the topic proportions over all documents.'),
        plotOutput('topicprops',
                   height = '600px',
                   #             height = paste(as.character((ncol(props()) * 25), 'px')),
                   width = "70%"),
        downloadButton("download_prop", "Download plot"),

        value = 6
      ),



      tabPanel(
        'Plots',
        p('Plot results and effect estimates from the model.'),
        actionButton("effectsubmit", label = "Generate plot") ,
        plotOutput('effectplot',
                   height = "500px",
                   width = "70%") ,
        downloadButton("download_plot", "Download plot"),
        value = 7
      ),



      tabPanel(
        'Correlation Graph',
        p(
          'Create a network based upon correlations between topics. Node labels are scaled by topic proportions.',
          "Edge width is determined by pairwise topic correlations."
        ),
        actionButton("graphsubmit", label = "Generate plot") ,

        plotOutput('graphplot',
                   height = "600px",
                   width = "80%"),
        downloadButton("download_graph", "Download plot"),
        value = 4
      ),



      tabPanel(
        'Misc.',

        h2('Model Info'),
        p(
          "Parameters for the selected model and effect estimates are listed below."
        ),
        dataTableOutput('modelinfo'),

        h2('Model Diagnostics'),
        p(
          'Plot Semantic Coherence and Exclusivity for all models in your environment without a content variable.'
        ),
        actionButton("diagsubmit", label = "Generate plot") ,
        plotOutput("stm_diag", width = "70%"),
        downloadButton("download_diag", "Download plot"),
        h2('Topic Labels'),
        p(
          "If you assigned labels to your topics you can download a corresponding table here."
        ),
        dataTableOutput('labelframe'),
        downloadButton('downloadLabels', 'Download topic labels'),


        value = 2
      )
  )
  )))

  )


server <- function(input, output, session) {
  # The environment needs to include three kinds of objects:
  # 1) one or multiple stm models
  # 2) an object"out" -> stm meta data generated with stm.prepDocuments
  # 3) one or multiple effect estimates for all topics generated with stm.estimateEffect


  options(shiny.maxRequestSize = 5000 * 1024 ^ 2)
  options(shiny.reactlog = TRUE)

  #### read and parse input file ####


  stm_data <- reactive({
    infile <- input$rfile
    if (is.null(infile)) {
      # User has not uploaded a file yet
      return(NULL)
    }


    load(infile$datapath, .GlobalEnv)
    rm(infile)



    stm_data <- list(
      model_names = NULL,
      effect_names = NULL,
      columns = NULL,
      models = NULL,
      effects = NULL,
      out = NULL
    )


    stm_data$model_names <-
      Filter(function(x)
        'STM' %in% class(get(x)), ls(envir = .GlobalEnv))

    stm_data$models <- purrr::map(stm_data$model_names, get)
    names(stm_data$models) <- stm_data$model_names
    stm_data$effect_names <-
      Filter(function(x)
        'estimateEffect' %in% class(get(x)),
        ls(envir =  .GlobalEnv))
    stm_data$effects <- purrr::map(stm_data$effect_names, get)
    names(stm_data$effects) <- stm_data$effect_names
    stm_data$out <- out
    stm_data$columns <- names(stm_data$out$meta)
    return(stm_data)
  })


  #### update ui inputs ####

  observeEvent(stm_data(), {
    output$doccol = renderUI({
      selectInput(
        "doccol",
        label = "Document column",
        choices = stm_data()$columns,
        selected = "text"
      )
    })

    output$modelchoice = renderUI({
      selectInput("modelchoice",
                  label = "Model",
                  choices = stm_data()$model_names)
    })
    output$effectschoice = renderUI({
      selectInput("effectschoice",
                  label = "Effect Estimates",
                  choices = stm_data()$effect_names)
    })



  })

  observeEvent(input$rfile, {
    shinyjs::delay(800, shinyjs::hide('rfile'))
  }, priority = 10)


  observeEvent(input$plotType, {
    if(input$plotType %in% c('wordcloud', 'perspectives')) {
      shinyjs::hide('download_plot')
    }
    else {shinyjs::show('download_plot')}

  }, priority = 10)



  model <- reactive({
    req(stm_data())
    req(input$modelchoice)
    model <- stm_data()$models[[input$modelchoice]]

    return(model)
  })
  stm_effect_estimates <- reactive({
    req(stm_data()$effects)
    stm_effect_estimates <-
      stm_data()$effects[[input$effectschoice]]
    return(stm_effect_estimates)
  })

  props <-  reactive({
    req(model())
    props <- model()[['theta']]
    colnames(props) <- 1:ncol(props)
    return(props)
  })




  observeEvent(model(), {
    removeUI(
      selector = "div.form-group shiny-input-container",
      multiple = TRUE,
      immediate = TRUE,
      session = session
    )
  }, priority = -2)


  # Dynamically create text inputs for topic labels
  observeEvent(model(), {
    output$textInputs <- renderUI({
      w <- 1:ncol(props())
      uis <- vector("list", length(w))
      for (i in seq(w)) {
        if (nchar(as.character(i)) == 1) {
          uis[[i]] <-
            textInput(paste("TL", "0", i, sep = ""),
                      paste("Topic ", i, sep = ""),
                      value = as.character(paste0('0', w[i])))
        }
        else
          uis[[i]] <-
            textInput(paste("TL", i, sep = ""),
                      paste("Topic ", i, sep = ""),
                      value = as.character(w[i]))

      }
      uis
    })
  }, priority = -1)

  # dynamically update topic labels

  tlabels <- reactive({
    req(model())
    nr_topics <- ncol(props())
    tnames <- str_subset(names(input), '^TL')[1:nr_topics]
    topiclabels <-  list()
    #tlabs <- str_extract_all('^TL', names(input))
    for (i in tnames) {
      topiclabels[[i]] <- input[[i]]
    }
    to_return <- unlist(topiclabels, use.names = F)
    #print(to_return)
    return(to_return)

  })



  plot_pointestimate <- function(estobj,
                                 variable,
                                 topic,
                                 xlab = input$plotLabel,
                                 ylab = input$plotLabel2,
                                 ci = 0.95) {
    # function for plotting point estimates for estimated effects
    data <- plot(
      x = estobj,
      covariate = variable,
      topic = topic,
      method = 'pointestimate',
      ci.level = ci,
      omit.plot = TRUE
    )
    means <- tibble(
      mean  = data$means[[1]],
      value = data$uvals,
      labels = data$labels
    )
    cis <- t(data$cis[[1]])
    colnames(cis) <- c('lower', 'upper')
    comb <- cbind(means, cis)
    comb$value <- as.factor(comb$value)

    plot <-
      ggplot(comb, aes(y = reorder(value, mean), x = mean)) + geom_point() +
      geom_line() +  guides(fill = F,
                            color = F,
                            group = F)  +
      geom_errorbarh(aes(xmin = lower, xmax = upper),
                     height = 0.1) +
      scale_x_continuous(labels = scales::percent,
                         breaks = scales::pretty_breaks(n = 8)) +
      labs(x = ylab , y  = xlab) + theme_bw(base_size = 14) +
      theme(
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major = element_line(
          size = 0.2,
          linetype = 'solid',
          colour = "grey95"
        )
      )
    # ggsave(
    #   "plot.png",
    #   plot,
    #   dpi = 300,
    #   width = 9,
    #   height = 6
    # )
    return(plot)
  }


  plot_continuous <- function(estobj,
                              variable,
                              topic,
                              xlab = input$plotLabel,
                              ylab = input$plotLabel2,
                              ci = 0.95) {
    # function for plotting estimated effects of continuous variables
    data <- plot(
      x = estobj,
      covariate = variable,
      topic = topic,
      method = 'continuous',
      ci.level = ci,
      omit.plot = TRUE
    )
    means <- tibble(mean  = data$means[[1]], value = data$x)
    cis <- t(data$ci[[1]])
    colnames(cis) <- c('lower', 'upper')
    comb <- cbind(means, cis)
    comb$lower <- ifelse(comb$lower < 0, 0, comb$lower)
    comb$mean <- ifelse(comb$mean < 0, 0, comb$mean)
    #comb$value <- get_date(comb$value)

    plot <- ggplot(comb, aes(x = value, y = mean)) +
      geom_line(color = '#778899') +
      geom_ribbon(aes(ymin = lower, ymax = upper),
                  alpha = 0.2,
                  fill = '#778899') +
      scale_x_continuous(breaks = scales::pretty_breaks(n = 8), expand = c(0.00, 0)) +
      scale_y_continuous(
        breaks = scales::pretty_breaks(n = 8),
        expand = c(0.00, 0),
        labels = scales::percent
      ) +
      guides(fill = F,
             color = F,
             group = F)  +
      coord_cartesian(ylim = c(-0.001, max(comb$upper))) +
      labs(x = xlab , y  = ylab) + theme_bw(base_size = 14) +
      theme(
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(
          size = 0.2,
          linetype = 'solid',
          colour = "grey95"
        )
      )
    # ggsave(
    #   "plot.png",
    #   plot,
    #   dpi = 300,
    #   width = 9,
    #   height = 6
    # )
    return(plot)
  }


  plot_continuous_int <- function(estobj,
                                  variable,
                                  topic,
                                  xlab = input$plotLabel,
                                  ylab =  input$plotLabel2,
                                  ci = 0.95,
                                  modvar = modvar,
                                  modval1 =  modval1,
                                  modval2 =  modval2) {
    dat1 <-  plot(
      x = estobj,
      covariate = variable,
      topic = topic,
      method = "continuous",
      moderator = modvar,
      moderator.value = modval1,
      ci.level = ci,
      omit.plot = TRUE,
      printlegend = FALSE
    )

    dat2 <-  plot(
      x = estobj,
      covariate = variable,
      topic = topic,
      method = "continuous",
      moderator = modvar,
      moderator.value = modval2,
      ci.level = ci,
      omit.plot = TRUE,
      printlegend = FALSE
    )

    get_int_data <- function(data, label) {
      means <- tibble(mean  = data$means[[1]], value = data$x)
      cis <- t(data$ci[[1]])
      colnames(cis) <- c('lower', 'upper')
      comb <- cbind(means, cis)
      comb$lower <- ifelse(comb$lower < 0, 0, comb$lower)
      comb$mean <- ifelse(comb$mean < 0, 0, comb$mean)
      comb$Moderator <- label
      return(comb)

    }

    mod1 <- get_int_data(dat1, modval1)
    mod2 <- get_int_data(dat2, modval2)
    both <- bind_rows(mod1, mod2)

    plot <-
      ggplot(both, aes(x = value, y = mean, group = Moderator)) +
      geom_line(aes(color = Moderator)) +
      geom_ribbon(aes(
        ymin = lower,
        ymax = upper,
        fill = Moderator
      ),
      alpha = 0.2) +
      scale_x_continuous(breaks = scales::pretty_breaks(n = 8), expand = c(0.00, 0)) +
      scale_y_continuous(
        breaks = scales::pretty_breaks(n = 8),
        expand = c(0.00, 0),
        labels = scales::percent
      ) +
      guides(fill = FALSE, group = FALSE)  +
      coord_cartesian(ylim = c(-0.001, max(both$upper))) +
      theme_bw(base_size = 14) +
      theme(
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(
          size = 0.2,
          linetype = 'solid',
          colour = "grey95"
        )
      )   +
      scale_fill_brewer(palette = 'Set1')  +
      scale_color_brewer(palette = 'Set1') +
      labs(x = xlab , y  = ylab)

    # ggsave(
    #   "plot.png",
    #   plot,
    #   dpi = 300,
    #   width = 9,
    #   height = 6
    # )
    return(plot)

  }

  #### diagnostics graph ####


  get_diags <- function() {
    len_models <- length(stm_data()$models)

    df <- tibble(
      exclusivity = numeric(),
      coherence = numeric(),
      statistic = character(),
      topics = numeric()
    )
    withProgress(message = 'Diagnostic calculations in progress...', {
      for (model in seq_along(1:len_models)) {
        incProgress(1 / len_models)

        modelcall <- stm_data()$models[[model]]$settings$call[2:8]
        if ('content' %in% names(modelcall)) {
          next()
        }

        stm_obj <- stm_data()$models[[model]]
        obj_name <- stm_data()$model_names[[model]]

        exclusivity_mod <- exclusivity(stm_obj)
        coherence_mod <- semanticCoherence(stm_obj,  out$documents)
        nr_topics_m <- ncol(stm_obj$theta)

        model_df_mean <- tibble(
          model_name = obj_name,
          exclusivity = mean(exclusivity_mod),
          coherence = mean(coherence_mod),
          statistic = 'mean',
          topics = nr_topics_m
        )

        model_df_median <- tibble(
          model_name = obj_name,
          exclusivity = median(exclusivity_mod),
          coherence = median(coherence_mod),
          statistic = 'median',
          topics = nr_topics_m
        )

        df <- bind_rows(df, model_df_mean, model_df_median)

      }
    })
    df$topics <- as.factor(df$topics)

    dplot <- ggplot(df,
                    aes(
                      y = exclusivity,
                      x = coherence,
                      color = statistic,
                      shape = topics
                    )) +
      geom_point(size = 3) +  geom_text_repel(
        aes(
          x = coherence,
          label = model_name,
          size = 3
        ),
        show.legend = FALSE,
        box.padding = 0.5
      ) + theme_bw(base_size = 15) +
      labs(
        x = 'Semantic Coherence',
        y = 'Exclusivity',
        color = 'Measure',
        shape = 'Nr. topics'
      ) +
      theme(
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(
          size = 0.2,
          linetype = 'solid',
          colour = "grey95"
        )
      ) +
      scale_color_brewer(palette = 'Set1') +
      scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
      scale_x_continuous(breaks = scales::pretty_breaks(n = 5))

    # ggsave(
    #   "plot.png",
    #   dplot,
    #   dpi = 300,
    #   width = 9,
    #   height = 6
    # )

    return(dplot)
  }

  plot_stm_diag <- eventReactive(input$diagsubmit, {
    get_diags()

  })




  output$stm_diag <- renderPlot({
    plot_stm_diag()
  })


  #### update plot input UI ####

  output$perspTopic2 <- renderUI({
    selectInput(
      "perspTopic2",
      label = "Perspectives - Topic 2",
      choices = tlabels(),
      selected = tlabels()[2]
    )
  })

  output$perspTopic1 <- renderUI({
    selectInput(
      "perspTopic1",
      label = "Perspectives - Topic 1",
      choices = tlabels(),
      selected = tlabels()[1]
    )
  })

  output$perspCat1 <- renderUI({
    selectInput(
      "perspCat1",
      label = "Content - Category 1",
      choices = model()$settings$covariates$yvarlevels,
      selected = model()$settings$covariates$yvarlevels[1]
    )
  })

  output$perspCat2 <- renderUI({
    selectInput(
      "perspCat2",
      label = "Content - Category 2",
      choices = model()$settings$covariates$yvarlevels,
      selected = model()$settings$covariates$yvarlevels[2]
    )
  })




  output$effectTopic <- renderUI({
    selectInput(
      "effectTopic",
      label = "Topic",
      choices = tlabels(),
      selected = tlabels()[1]
    )

  })
  output$plotVar <- renderUI({
    selectInput(
      "plotVar",
      label = "Plot variable",
      choices = stm_effect_estimates()$varlist,
      selected = stm_effect_estimates()$varlist[3]
    )
  })

  output$plotLabel <- renderUI({
    textInput("plotLabel",
              label = "Axis label",
              value = input$plotVar)
  })
  output$plotLabel2 <- renderUI({
  textInput("plotLabel2",
            label = "Axis label 2",
            value = 'Topic Proportion')
})

  output$modvar <- renderUI({
    selectInput(
      "modvar",
      label = "Interaction variable",
      choices = stm_effect_estimates()$varlist,
      selected = stm_effect_estimates()$varlist[1]
    )
  })



  # moderator values for plots
  modvalues <- reactive({
    req(input$modvar)
    modvar <- out$meta[[input$modvar]]
    modvar <- as.factor(modvar)
    levels <- levels(modvar)

    return(levels)

  })
  #
  #### update plotting inputs ####

  observe({
    updateSelectInput(session,
                      "topic",
                      choices = tlabels(),
                      selected = tlabels()[1])
    updateSelectInput(session,
                      "effectTopic",
                      choices = tlabels(),
                      selected = tlabels()[1])
    updateSelectInput(session,
                      "perspTopic1",
                      choices = tlabels(),
                      selected = tlabels()[1])
    updateSelectInput(session,
                      "perspTopic2",
                      choices = tlabels(),
                      selected = tlabels()[2])
    updateSelectInput(session,
                      "modval1",
                      choices = modvalues(),
                      selected = modvalues()[1])
    updateSelectInput(session,
                      "modval2",
                      choices = modvalues(),
                      selected = modvalues()[2])
  }, priority = -10)


  #### find topic documents ####

  topicDocs <- reactive({
    validate(
      need(!is.null(model()), "Loading, please wait.."),
      need(!is.null(tlabels()), "Loading, please wait.."),
      need(!is.null(stm_data()), "Loading, please wait.."),
      need(length(tlabels()) == ncol(props()), "Loading, please wait.."))

    t <- which(input$topic == tlabels())
    validate(need(length(t) > 0, "Loading, please wait.."))



    #nrt <- as.numeric(input$nrthoughts)
   # if(is.null(model())) {return(tibble(docs = ''))}
  #  else {
    thoughts <- reactive({

      findThoughts(
        model(),
        n = 100,
        # set to 100
        topics = c(t),
        texts = stm_data()$out$meta[[input$doccol]]
      )

    })


    topicThoughts <- thoughts()$docs[[1]][1:100]
    thoughtdf <- data.frame(topicThoughts, stringsAsFactors = FALSE)
    names(thoughtdf) <- " "

    return(thoughtdf)
   # }


  })

  #### Find topic terms ####

  topicTerms <- reactive({
    req(stm_data())
    req(model())
    req(input$topic)
    req(tlabels())

    t <- which(input$topic == tlabels())
    nrterms <- as.numeric(input$nrwords)
    labels <- labelTopics(model(), n = nrterms)

    labList <- list()


    if ("content" %in% modelcall()$Attribute) {
      labels <- sageLabels(model(), n = nrterms)
      contentList <- list()

      if (1 %in% input$labtypes) {
        prob <- str_c(labels$marginal$prob[t,], collapse = ', ')
        labList$Probability <- prob

        names(labels$cov.betas) <-
          str_c('Prob_', labels$covnames)
        contentList <- c(contentList, labels$cov.betas %>% purrr::map(function (x) {
          x$problabels[t,] %>% str_c(collapse = ', ')
        }))

      }
      if (2 %in% input$labtypes) {
        frex <- str_c(labels$marginal$frex[t,], collapse = ', ')
        labList$FREX <- frex

        names(labels$cov.betas) <-
          str_c('FREX_', labels$covnames)
        contentList <- c(contentList, labels$cov.betas %>% purrr::map(function (x) {
          x$frexlabels[t,] %>% str_c(collapse = ', ')
        }))
      }

      if (3 %in% input$labtypes) {
        lift <- str_c(labels$marginal$lift[t,], collapse = ', ')
        labList$Lift <- lift

        names(labels$cov.betas) <-
          str_c('Lift_', labels$covnames)
        contentList <- c(contentList, labels$cov.betas %>% purrr::map(function (x) {
          x$liftlabels[t,] %>% str_c(collapse = ', ')
        }))
      }

      if (4 %in% input$labtypes) {
        score <- str_c(labels$marginal$score[t,], collapse = ', ')
        labList$Score <- score

        names(labels$cov.betas) <-
          str_c('Score_', labels$covnames)
        contentList <- c(contentList, labels$cov.betas %>% purrr::map(function (x) {
          x$scorelabels[t,] %>% str_c(collapse = ', ')
        }))
      }



      labList <- c(labList, contentList)

      labDf <- data.frame(labList)
      return(labDf)

    }

    else{
      if (1 %in% input$labtypes) {
        prob <- str_c(labels$prob[t,][1:nrterms], collapse = ', ')
        labList$Probability <- prob

      }
      if (2 %in% input$labtypes) {
        frex <- str_c(labels$frex[t,][1:nrterms], collapse = ', ')
        labList$FREX <- frex
      }

      if (3 %in% input$labtypes) {
        lift <- str_c(labels$lift[t,][1:nrterms], collapse = ', ')
        labList$Lift <- lift
      }

      if (4 %in% input$labtypes) {
        score <- str_c(labels$score[t,][1:nrterms], collapse = ', ')
        labList$Score <- score
      }

      labDf <- data.frame(labList)


      return(labDf)

    }


  })

  #### dataframes for docs and terms ####

  # Dataframe for topic docs
  output$tlabel <- renderDataTable(
    topicDocs(),
    escape = FALSE,
    options = list(
      pageLength = 1,
      searching = FALSE,
      autoWidth = TRUE,
      scrollX = TRUE,
      info = FALSE,
      lengthMenu = c(1, 2, 5, 10),
      lengthChange = T
    )
  )

  # Dataframe for topic terms
  output$tterms <- renderDataTable(
    topicTerms(),
    options = list(
      pageLength = 1,
      searching = FALSE,
      autoWidth = TRUE,
      scrollX = TRUE,
      lengthChange = FALSE,
      info = FALSE,
      paging = FALSE
    )
  )

  #### Topic proportions plot  ####

  plotTopicProps <- function(proportions) {
    req(input$modelchoice)
    req(stm_data())
    if (length(tlabels()) > 0) {
      names_ <- tlabels()
    }
    else {
      names_ <- colnames(proportions)
    }
    frequency <- colMeans(proportions)
    order <- order(frequency, decreasing = F)
    percentage <- frequency[order]
    names_ <- names_[order]
    topic <- factor(names_, levels = names_)
    combined <- data.frame(percentage, topic)
    p <- ggplot(combined, aes(x = topic, y = percentage)) +
      geom_bar(stat = "identity",
               fill = "#377eb8",
               color = "#000000") +
      coord_flip() + scale_y_continuous(labels = scales::percent,
                                        expand = c(0, 0)) +
      labs(y = "Proportion", x = "Topic") +
      theme_light(base_size = 14) +
      theme(axis.text = element_text(size = 14),
            panel.grid.major.y = element_blank())

    # ggsave("plot.png",
    #        p,
    #        dpi = 300,
    #        width = 9,
    #        height = 6)
    return(p)
  }



  output$topicprops <- renderPlot({
    plotTopicProps(props())
  })



  #### effectplot ####


  plot_effect_graph <- eventReactive(input$effectsubmit, {
    # inputs
    scalemax <- input$scalemax
    scalemin <- input$scalemin
    plotT <- which(input$effectTopic == tlabels())
    plotPers1 <- which(input$perspTopic1 == tlabels())
    plotPers2 <- which(input$perspTopic2 == tlabels())
    plotM <- input$plotType
    type <- input$plotType


    # differences (not implemented at the moment)
    # if (type == "difference") {
    #   covalue1 <- input$covar1
    #   covalue2 <- input$covar2
    # }
    # else {
    #   covalue1 <- NULL
    #   covalue2 <- NULL
    # }


    #### wordcloud ####
    if (type == "wordcloud") {
      # png(
      #   'plot.png',
      #   width = 9,
      #   height = 6,
      #   units = 'in',
      #   res = 120
      # )
      #
      # #  return(
      # cloud(
      #   model(),
      #   topic = plotT,
      #   random.order = FALSE,
      #   max.words = input$cloud_words,
      #   scale = c(scalemax, scalemin)
      # )
      # dev.off()


        cloud(
          model(),
          topic = plotT,
          random.order = FALSE,
          max.words = input$cloud_words,
          scale = c(scalemax, scalemin)
        )

    }

    #### pointestimate ####
    if (type == "pointestimate") {
      return(
        plot_pointestimate(
          estobj = stm_effect_estimates(),
          variable = input$plotVar,
          topic = plotT,
          ci = input$effectci
        )
      )
    }

    #### perspectives ####
    if (type == "perspectives") {
      if (("content" %in% modelcall()$Attribute) &
          (plotPers1 == plotPers2)) {

        plabels <- model()$settings$covariates$yvarlevels

        # png(
        #   'plot.png',
        #   width = 8,
        #   height = 6,
        #   units = 'in',
        #   res = 150
        # )
#
#         plot(
#           model(),
#           topics = plotPers1,
#           type = "perspectives",
#           n = input$persp_words,
#           main = plotPers1,
#           covarlevels = c(input$perspCat1, input$perspCat2),
#           plabels = c(input$perspCat1, input$perspCat2),
#           text.cex = input$persp_cex
#         )

        #dev.off()

          plot(
            model(),
            topics = plotPers1,
            type = "perspectives",
            n = input$persp_words,
            plabels = model()$settings$covariates$yvarlevels,
            text.cex = input$persp_cex
          )


      }
      else {
        # png(
        #   'plot.png',
        #   width = 8,
        #   height = 6,
        #   units = 'in',
        #   res = 150
        # )
        #
        # plot(
        #   model(),
        #   topics = c(plotPers1, plotPers2),
        #   type = "perspectives",
        #   plabels = c(input$perspTopic1, input$perspTopic2),
        #   n = input$persp_words,
        #   text.cex = input$persp_cex
        # )
        # dev.off()
        #


        p <-   plot(
            model(),
            topics = c(plotPers1, plotPers2),
            type = "perspectives",
            plabels = c(input$perspTopic1, input$perspTopic2),
            n = input$persp_words,
            text.cex = input$persp_cex
          )

      }
    }
    #

    #### continuous ####

    interaction <- reactive({
      input$moderator
    })

    if (interaction() == FALSE & type == "continuous") {
      return(
        plot_continuous(
          estobj = stm_effect_estimates(),
          variable = input$plotVar,
          topic = plotT,
          ci = input$effectci
        )
      )


    }


    ####  interaction  ####

    if (interaction() == TRUE & type == "continuous") {
      modvar <- input$modvar
      modval1 <- input$modval1
      modval2 <- input$modval2

      return(
        plot_continuous_int(
          estobj = stm_effect_estimates(),
          variable = input$plotVar,
          topic = plotT,
          ci = input$effectci,
          modvar = modvar,
          modval1 =  modval1,
          modval2 =  modval2
        )
      )
    }


  })

  output$effectplot <- renderPlot({
    withProgress(message = 'Creating graph..', value = 0,
                 {
                   incProgress(1)
                   plot_effect_graph()

                 })
  }, res = 90)



  #### correlation graph ####

  # Generating correlation graph

  calcGraph <-
    function(model, method,  cutoff = 0.05) {
      # calculate topic correlation graph
      if (method == "cutoff") {
        cormat <-
          topicCorr(model(), method = 'simple', cutoff = cutoff)$poscor
      }
      else {
        cormat <- topicCorr(model(), method = 'huge')$poscor
      }


      g <-
        igraph::simplify(igraph::graph.adjacency(cormat, mode = 'undirected', weighted = TRUE))

      if (length(igraph::E(g)) == 0) {
        stop(
          "There are no (sufficiently high) correlations between the topics of this STM model."
        )
      }
     igraph::V(g)$name <- tlabels()
      igraph::V(g)$props <- colMeans(model()$theta)

      return(g)
    }



  graph <- reactive({
    calcGraph(model(), input$graphmethod, input$cutoff)

  })



  plot_corr_graph <- eventReactive(input$graphsubmit, {
    plotGraph(graph(),
              labels = input$eLabels,
              cutiso = input$cutiso)

  })

  output$graphplot <- renderPlot({
    withProgress(message = 'Creating graph..', value = 0,
                 {
                   incProgress(1)
                   plot_corr_graph()

                 })
  }, res = 90)


  output$stm_diag <- renderPlot({
    plot_stm_diag()
  })


  plotGraph <- function(g,  labels, cutiso) {
    # generate topic correlation graph

    #if (length(V(g)) == 0)
    #  return()

    toplot <- as_tbl_graph(g) %>%
      mutate(degree = centrality_degree(loops = FALSE)) %>%
      activate(edges) %>%
      filter(!edge_is_loop()) %>%
      mutate(weight = round(weight, 2),
             edge_label = '')


    if (labels == TRUE) {
      toplot <- toplot %>% activate(edges) %>%
        mutate(edge_label = as.character(weight))
    }

    if (cutiso == TRUE) {
      toplot <- toplot %>% activate(nodes) %>%
        filter(degree > 0)
    }



    plot <- toplot %>% ggraph(layout = 'fr') +
      geom_edge_link(
        aes(edge_width = weight, label =  edge_label),
        label_colour = '#fc8d62',
        edge_colour = '#377eb8',
        alpha = 0.5,
        label_size = 4,
        angle_calc = 'along',
        label_dodge = unit(3, "mm")
      ) +
      geom_node_point(size = 4, colour = 'black')  +
      geom_node_label(
        aes(label = name, size = props),
        colour = 'black',
        repel = TRUE,
        alpha = 0.85
      ) +
      theme_graph() +
      scale_size(range = c(2, 10), labels = scales::percent) +
      labs(size = 'Topic Proportion',
           edge_width = 'Topic Correlation') +
      scale_edge_width(range = c(0, 3))
    if (input$includelegend == FALSE) {
      plot <- plot +  guides(size = FALSE, edge_width = FALSE)
    }


    # ggsave(
    #   "plot.png",
    #   plot,
    #   dpi = 300,
    #   width = 9,
    #   height = 6
    # )
    return(plot)
  }



  ### model infos

  modelcall <- reactive({
    modelcall <- model()$settings$call[2:8]
    modelcall$estimateEffects <-
      gsub("  ", "", paste(format(stm_effect_estimates()$formula), collapse = ""))
    modelframe <-
      data.frame(names(modelcall), as.character(modelcall))
    names(modelframe) <- c("Attribute", "Value")

    modelframe <-
      modelframe %>%
      filter(!Attribute %in% c('documents', 'vocab', 'data', 'verbose')) %>%
      filter(Attribute != '')
    return(modelframe)
  })

  output$modelinfo <- renderDataTable(
    modelcall(),
    options = list(
      pageLength = 1,
      searching = FALSE,
      autoWidth = TRUE,
      scrollX = FALSE,
      lengthChange = FALSE,
      info = FALSE,
      paging = FALSE
    )
  )

  #### label dataframe ####

  labelframe <- reactive({
    labels <- tlabels()
    ids <- seq_along(labels)
    frequency <- round(colMeans(props()), 3)
    return(data.frame(
      Topic = ids,
      Label = labels,
      Proportion = frequency
    ))

  })


  output$labelframe <- renderDataTable(
    labelframe(),
    options = list(
      pageLength = 1,
      searching = FALSE,
      autoWidth = TRUE,
      scrollX = FALSE,
      lengthChange = FALSE,
      info = FALSE,
      paging = FALSE
    )
  )

  #### download handlers ####

  output$downloadLabels <- downloadHandler(
    filename = function() {
      'stmInsights_topiclabels.tsv'
    },
    content = function(file) {
      readr::write_tsv(labelframe(), file)

    }
  )

  output$download_plot <- downloadHandler(
    filename = 'plot.pdf',
    content = function(file) {
      pdf(file = file, width = 9, height = 6)
   print(plot_effect_graph())
      dev.off()
    },
   contentType = "image/png"
  )


  output$download_prop <- downloadHandler(
    filename = 'plot.pdf',
    content = function(file) {
      pdf(file = file, width = 9, height = 6)
      print(plotTopicProps(props()))
      dev.off()
    },
    contentType = "image/png"
  )




  output$download_graph <- downloadHandler(
    filename = 'plot.pdf',
    content = function(file) {
      pdf(file = file, width = 9, height = 6)
      print(plot_corr_graph())
      dev.off()
    },
    contentType = "image/png"
  )



  output$download_diag <- downloadHandler(
    filename = 'plot.pdf',
    content = function(file) {
      pdf(file = file, width = 9, height = 6)
      print(get_diags())
      dev.off()
    },
    contentType = "image/png"
  )






}



shinyApp(ui = ui, server = server)

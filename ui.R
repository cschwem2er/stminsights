source('helper.R') # load helper functions and stm data

header <- dashboardHeader(title = 'stmInsights')

body <- dashboardBody(fluidRow(column(
  width = 12,
  tabBox(
    id = 'tabvals',
    width = NULL,
    
    
    
    tabPanel('Info & Labels',
             h1("stmInsights"),
             p("This app enables interactive exploration of",
               a("Structural Topic Models.", 
                 href="http://structuraltopicmodel.com/"),
               "For most of the sidebar inputs a short info is available if you
               hover over them. In case you are not familiar with STM the authors describe
                 the model in the corresponding ",
               a("package vignette.", 
                 href="https://cran.r-project.org/web/packages/stm/vignettes/stmVignette.pdf")),
             p("The model for this example was fitted on a sample corpus of
               100.000 parliamentary written questions for the UK legislative term 2010-2015.
               Topic prevalence is estimated with covariates for date, party and an
               identifier for Citizens of Immigrant Origin."),
              p("You should inspect the", strong('Topics'), "tab first to make 
                sense about what the topics actually stand for. You can navigate back to 
                this tab afterwards and enter topic labels.
               Outputs in other tabs will then adjust labels accordingly."),
           
            
             
             h3("Topic Labels"),
             p("Use the boxes to enter labels for your topics. It is suggested 
        to keep the labels short, such that plot outputs will not be flooded
        with text."),
             uiOutput("textInputs"), value=6 ),
    
    tabPanel('Topics',

             h3('Topic Terms'),
             dataTableOutput('tterms'),
             h3('Topic Documents'),
             dataTableOutput('tlabel'),
    

             
             h3('All Topics - Proportions'),
             plotlyOutput('topicprops',
                        height = paste(as.character((ncol(props) * 20), 'px')), 
                        width="80%"), value = 1),
 

    
  
    
    tabPanel('Plots',
             plotOutput('effectplot',
                        width="70%") ,
             value=7 ),
    
            
    


    tabPanel('LDAvis',
             
             visOutput('stmVis')
             , value = 3),
    
    
    
    tabPanel(
      'Correlation Graph',
      plotOutput('graphplot',
                 height = "600px",
                 width = "80%"),
      value = 4),
    
    
    tabPanel('Misc.',
             h2('Metadata'),
             p("This dataframe includes metadata for the documents used to fit the STM model."),
             dataTableOutput('metadata'), 
             h2('Model Info'),
             p("Parameters for model and effect estimation are listed below."),
             dataTableOutput('modelinfo'),
             value = 2)
  )
)))



dashboardPage(
  header,
  dashboardSidebar(
    
    
    
    #conditionalPanel(
    #  condition = "input.tabvals ==6",
    #  h4("Topic Labels"),
    # 
    #), does not contain anything relevant at the moment
    
    conditionalPanel(

      condition = "input.tabvals == 1",
      
      h4("Topic"),
      selectInput(
        "topic",
        label = NULL,
        choices = as.numeric(colnames(props)),
        selected = 1
      ),
      bsTooltip('topic', "The topic for which you want to display documents and labels."),
      
      h4("Terms"),
      checkboxGroupInput("labtypes", "Label types",
                         c("Probability" = 1,
                           "FREX" = 2,
                           "Lift" = 3,
                           "Score" = 4),
                         selected=c(1,2)),
      bsTooltip('labtypes', "Check which labeltypes you want to use."),
      
      sliderInput(
        'nrwords',
        'Nr. of Terms',
        min = 2,
        max = 50,
        value = 10
        #animate=T
      ),
      bsTooltip('nrwords', "The number of terms to be displayed"),
      
      
      h4("Documents"),
      
      selectInput(
        "doccol",
        label = "Document column",
        choices = columns
      ),
      bsTooltip('doccol', "Select the column of your meta dataframe which includes the documents."),
      

      sliderInput(
        'nrthoughts',
        'Nr. of documents',
        min = 1,
        max = 50,
        value = 5
      ),
      bsTooltip('nrthoughts', "The number of documents to be retrieved for the selected topic.")
    ),
    
    
    
    conditionalPanel(
      condition = "input.tabvals == 2",
      selectInput(
        "columns",
        label = h4("Meta Columns:"),
        choices = c('All columns', columns),
        selected = "All columns",
        multiple = T
      ),
      bsTooltip('columns', "Select metadata columns to be displayed."),
      selectInput(
        "fcol",
        label = h4("Filter column:"),
        choices = c('None', columns),
        selected = "None"
      ),
      bsTooltip('fcol', "Select a column to filter textual meta data."),
      textInput(
        "filter",
        label = h4("Filter Terms:"),
        value = ""
      ),
      bsTooltip('filter', "Enter a list of filter terms. Example: migration, black, asian"),
      # regex filter
      textInput(
        "regfilter",
        label = h4("Regular Expression:"),
        value = ""
      ),
      bsTooltip(
        'regfilter',
        "Advanced users can optionally filter by regular expressions."),
      # exclusion filter
      textInput(
        "exfilter",
        label = h4("Exclusion Terms:"),
        value = ""
      ),
      bsTooltip('exfilter', "You can also remove rows containing exclusion terms.")
    ),
    
    
    conditionalPanel(
      condition = "input.tabvals == 7",
      
      selectInput(
        'plotType', # type "difference" is not implemented at the moment
        label = "Type",
        choices= c("continuous", "pointestimate", "wordcloud", "perspectives"),
        selected="continuous"),
  
      bsTooltip('plotType', "Choose the plot type to be displayed.")
   

    
    ),
      
    
    conditionalPanel(
      condition = "input.tabvals == 7 && input.plotType != 'perspectives'",
      
    selectInput(
      "effectTopic",
      label = "Topic",
      choices = as.numeric(colnames(props)),
      selected = 1),
    bsTooltip('effectTopic', "Select the topic for the plot creation.")
    ),
    
    

    
    conditionalPanel(
      condition = "input.tabvals == 7 && (input.plotType == 'continuous' || input.plotType == 'pointestimate')",
      
      selectInput(
        "plotVar",
        label = "Plot variable",
        choices = prep$varlist,
        selected=prep$varlist[3]),
      bsTooltip('plotVar', "Select the variable for the plot."),
    
    sliderInput(
      'effectci',
      'Confidence Intervall',
      min = 0.55,
      max = 0.99,
      value = 0.95
    ),
    bsTooltip('effectci', "Width of confidence intervalls can be adjusted here.")
    

  
    ),
    
    
    conditionalPanel(
      condition = "input.tabvals == 7 && input.plotType == 'continuous'",
      checkboxInput('moderator',
                    label= "Interaction Effect"),
      bsTooltip('moderator', "Check this box if you want to display an interaction effect.")
    ),
      

      
      
      
    conditionalPanel(
      condition = "input.tabvals == 7 && input.plotType == 'wordcloud'",
      sliderInput(
        'cloud_words',
        'Maximum nr. of words',
        min = 10,
        max = 150,
        value = 50
      ),
      bsTooltip('cloud_words', "The maximum number of words to be included in the cloud."),
      
      sliderInput(
        'scalemax',
        'Maximum word scaling',
        min = 1,
        max = 10,
        value = 5
      ),
      bsTooltip('scalemax', "Adjust the maximum word size."),
      
      sliderInput(
        'scalemin',
        'Minimum word scaling',
        min = .2,
        max = 2,
        value = .8
      ),
      bsTooltip('scalemin', "Adjust the minimum word size.")
    ),
      
      
    
    
    conditionalPanel(
      condition = "input.tabvals == 7 && input.plotType == 'perspectives'",
      
      
      
      selectInput(
        "perspTopic1",
        label = "Perspectives - Topic 1",
        choices = as.numeric(colnames(props)),
        selected = 1),
      bsTooltip('perspTopic1', "Choose the first topic for the perspective plot."),
      
      selectInput(
        "perspTopic2",
        label = "Perspectives - Topic 2",
        choices = as.numeric(colnames(props)),
        selected = 2),
      bsTooltip('perspTopic2', "Choose the second topic for the perspective plot."),
      
      sliderInput(
        'persp_words',
        'Number of words',
        min = 5,
        max = 50,
        value = 25
      ),
      bsTooltip('persp_words', "Number of words to be displayed in the perspective plot.")
    ),
    

      
       conditionalPanel(
        condition = "input.tabvals == 7 && input.moderator == true && input.plotType == 'continuous'",
      
      selectInput(
        "modvar",
        label = "Interaction variable",
        choices = prep$varlist,
        selected=prep$varlist[1]),
      bsTooltip('modvar', "Select the moderator variable."),
      
      selectInput(
        "modval1",
        choices = "None",
        label = "Interaction value 1"),
      
      selectInput(
        "modval2",
        choices = "None",
        label = "Interaction value 2"),
      sliderInput(
        "ylim",
        'Y-Axis Cutoff',
        min = 0.00,
        max = 0.50,
        value = 0.10,
        step=0.01
      ),
      bsTooltip('ylim', "For interaction plots it is advisable to adjust the y-axis manually.")
       ),
      
    # difference plots are not implented at the moment
    # conditionalPanel(
    #    condition = "input.tabvals == 7 && input.plotType == 'difference'",
    #  
    #  textInput(
    #   "covar1",
    #    label = "Covariate Value 1"),
    #  textInput(
    #   "covar2",
    #  label = "Covariate Value 2")
    #),


    
    conditionalPanel(
      condition = "input.tabvals == 3",
      selectInput(
        "scaling",
        label = h4("Scaling method"),
        selected = F,
        choices = list("t-SNE", "PCA") # largeVis may be added later
      ),
      bsTooltip("scaling", "The scaling method for the 2d Intertopic Distance Map"), 
      sliderInput(
        'visTerms',
        h4('Number of terms'),
        min = 5,
        max = 50,
        value = 25),
      bsTooltip("visTerms", "The number of terms to be displayed.")
    
    ),
    
    conditionalPanel(
      condition = "input.tabvals == 3 && input.scaling == 't-SNE'",
      
      sliderInput(
        "perplexity",
        label = h4("Perplexity (t-SNE)"),
        min = 1,
        max = round((ncol(props) - 2) / 3, 0) ,
        value = 5
      ),
      bsPopover("perplexity", title="Info", 
            content="Perplexity is a measure for information\\
            and may be viewed as a knob that sets the number \\
            of effective nearest neighbors. The most appropriate \\
            value depends on the density of your data. \\
            Loosely speaking, one could say that a \\
            larger / denser dataset requires a larger perplexity. \\
            Typical values for the perplexity range between 5 and 50.",
                options = list(container = "body"))
      
      
    ),
    
    
    
    conditionalPanel(
      condition = "input.tabvals == 4",
      helpText("Node sizes are scaled by topic proportions."),
      helpText("Edge thickness is determined by pairwise topic correlations."),
      selectInput(
        "graphmethod",
        label = "Method",
        choices = c("huge", "cutoff")
          ),
      bsTooltip('graphmethod','Choose whether the graph connections are computed algorithmically\\
 or by a simple correlation cutoff criterium.')
      ),
    
    conditionalPanel(
      condition = "input.tabvals == 4 && input.graphmethod == 'cutoff'",
      
      sliderInput(
        "cutoff",
        label = h4("cutoff"),
        min = 0.01,
        max = 0.5 ,
        value = 0.05),
      bsTooltip('cutoff','Minimum correlation between topic pairs.')
    ),
    
    conditionalPanel(
      condition = "input.tabvals == 4",
      
      checkboxInput("eLabels", label = h4("Edge Labels:"),
                    value = FALSE),
      bsTooltip('eLabels','Edge labels display correlations between topics.'),
      
      sliderInput(
        'eWeight',
        h4('Edge weight:'),
        min = 1,
        max = 5,
        value = 1
      ),
      bsTooltip('eWeight','Edge weights adjust thickness based upon topic correlations.')
    )
    
  ),
  body
)
#Author - Bac Tran
#This is the user-interface definition of a Shiny web application. You can
#run the application by clicking 'Run App' above.
#

 shinyUI(
      fluidPage(
        useShinyjs(),
        titlePanel("Covid data - Bac Tran"),
        tabsetPanel(
          tabPanel("Data Summary",
                         htmlOutput(outputId = "Summary")),
            
          tabPanel("Raw Data",
                  DT::dataTableOutput(outputId = "Rawdata")),
          
          tabPanel("Missingness",
                   p("Note:Data was processed through PreProcessing subTab of glmnet Model Tab. By default it uses Raw data."),
                   plotOutput(outputId = "Missing"),
                   checkboxInput(inputId = "cluster", 
                                 label = "Cluster missingness", value = FALSE),
          ),
          
          tabPanel("Missing_Tree",
                   plotOutput(outputId = "Tree")
          ),
          
          tabPanel("gg_miss_upset chart",
                   h3("A gg_miss_upset visualisation of missing data", align ="center"),
                   plotOutput(outputId = "Missupset")
          ),

          tabPanel("MixedPairs Numeric - Categorical",
                   selectizeInput(inputId = "VariablesD",
                                  label = "Choose Categorical Variables to chart:",
                                  choices = cat_list, multiple = TRUE,
                                  selected = "HEALTHCARE_BASIS"),
                   selectizeInput(inputId = "VariablesF",
                                  label = "Choose Numeric Variables to chart:",
                                  choices = num_list, multiple = TRUE,
                                  selected = num_list),
                   selectizeInput(inputId = "VariablesE",
                                  label = "Choose Categorical Variable to colour:",
                                  choices = cat_list, multiple = FALSE,
                                  selected = "HEALTHCARE_BASIS"),
                   actionButton(inputId = "Go_pair", label = "Plot", icon = icon("play")),
                   plotOutput(outputId = "MixedPairs")
          ),
          
          tabPanel("glmnet Model",
              tabsetPanel(
                tabPanel("Prepocessing",
                         h3("Partial Deleting", align ="center"),
                         h4("Variable deleting"),
                         sliderInput(inputId = "VarThresh","Variable missing Threshold (%):",
                                     min = 1,
                                     max = 100,
                                     value = 100),
                         verbatimTextOutput(outputId = "RemoveVar"),
                         br(),
                         h4("Observation deleting"),
                         br(),
                         p("Please choose following options to remove observation with missingness."),
                         sliderInput(inputId = "ObsThresh","Observation missing Threshold (%):",
                                     min = 1,
                                     max = 100,
                                     value = 100),
                         verbatimTextOutput(outputId = "RemoveObs1"),
                         checkboxInput(inputId = "outlierdelete", 
                                       label = "Remove detected outliers of first cleaning strategy", 
                                       value = FALSE),
                         verbatimTextOutput(outputId = "RemoveObs2"),
                         checkboxInput(inputId = "top5delete", 
                                       label = "Remove Observation with missingness in most 5 important variables", 
                                       value = FALSE),
                         verbatimTextOutput(outputId = "RemoveObs3"),
                         checkboxInput(inputId = "top2delete", 
                                       label = "Remove Observation with missingness in most 2 important variables", 
                                       value = FALSE),
                         verbatimTextOutput(outputId = "RemoveObs4"),
                         h3("Imputation", align ="center"),
                         checkboxInput(inputId = "Impuhealthcost", 
                                       label = "Impute missing value of Healcare_cost to 0 and create a shadow varible", 
                                       value = FALSE),
                         checkboxInput(inputId = "ImpuGovern", 
                                       label = 'Impute missing value of Govern_Type to "none"', value = FALSE),
                         h3("Data summary after first PreProcessing", align ="center"),
                         verbatimTextOutput(outputId = "newdata")
                ),
                
                tabPanel("Train model",
                   h1("Glmnet Model", align ="center"),
                   p("Note:Data was processed through PreProcessing Tab"),
                   p("Then the cleaned data is processed through the recipe:"),
                   p("cleandata %>% step_impute_knn %>% step_center %>% step_scale %>% step_dummy"),
                   h3("Data Recipe"),
                   p("Adjust the recipe in order from top to bottom:"),
                   checkboxInput(inputId = "knn",
                                 label = "KNN Imputation", value = TRUE),
                   sliderInput(inputId = "knnnumber","Choose the number of neighbors.:",
                               min = 1,
                               max = 100,
                               value = 5),
                   checkboxInput(inputId = "Center",
                                 label = "Centering numeric data", value = TRUE),
                   checkboxInput(inputId = "Scale",
                                 label = "Scaling numeric data", value = TRUE),
                   actionButton(inputId = "Go", label = "Train Model and Visualize
                                the result", icon = icon("play")),
                   h3("Model Summary"),
                   verbatimTextOutput(outputId = "gmlnetprocess"),
                   h3("Variable Importance"),
                   plotOutput(outputId = "Importance"),
                   h3("Model Visualization"),
                   plotOutput(outputId = "glmnetmodel"),
                   verbatimTextOutput(outputId = "TestRMSE"),
                   h3("Residual Boxplot"),
                   p("Please check the Residual Table for residual and prediction data"),
                   sliderInput(inputId = "re_range", label = "IQR Multiplier", 
                               min = 0, max = 5, step = 0.1, value = 1.5),
                   plotOutput(outputId = "residualboxplot")
     
          ),
          
          tabPanel("Residual Table",
                   p("Note: The result of model prediction and residuals was joined with Data after first PreProcessing Tab"),
                   DT::dataTableOutput(outputId = "Residualdata"))
          ))
          
          
                   
)))
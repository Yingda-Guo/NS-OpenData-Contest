## app.R ##

library(shinydashboard)
library(shiny)
library(data.table)
library(e1071)
library(caret)
library(MASS)
library(rpart)
library(kernlab)
library(randomForest)
library(plotly)
library(ggplot2)
library(shinyWidgets)

# Load data
DT <- fread("data.csv")

# Get unique disease
unique_disease <- unique(DT$Disease)

# ML algorithm
ML <- list("K-Nearest Neighbors" = "knn", 
     "Support Vector Machine" = "svmRadial",
     "Random Forest" = "rf",
     "Generalized Linear Model" = "glm",
     "Neural Network" = "nnet")

# Load metric explanation
metric_ex <- fread("metric_ex.csv")
setkey(metric_ex, Weather_Factor)

ui <- dashboardPage(
  skin = "black",
  dashboardHeader(title = "Disease VS Weather"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Summary", tabName = "summary", icon = icon("th"))
    )
  ),
  dashboardBody(
    
    tabItems(
      # First tab content
      tabItem(tabName = "dashboard",
              # Boxes need to be put in a row (or column)
              fluidRow(
                box(width = 12, 
                    selectInput("select1", label = "Choose a disease to explore:", 
                                choices = unique_disease, 
                                selected = unique_disease[3]),
                    # Copy the line below to make a select box 
                    selectInput("select2", label = "Choose learning algorithm(s):", 
                                choices = ML, 
                                multiple = T,
                                selected = ML)
                    )
              ),
              br(),
              fluidRow(
                box(width = 12,
                    title = "Machine Learning Algorithms Comparison",
                    plotlyOutput("plot1"))
              ),
              br(),
              fluidRow(
                box(width = 12,
                    title = "Weather Factor Importane",
                    plotlyOutput("plot2")
                    )
              ),
              br(),
              fluidRow(
                box(width = 12,
                    title = "Prediction",
                    uiOutput("weatherfactor"),
                    actionButton("bt", label = "Predict")
                )
              )
      ),
      
      # Second tab content
      tabItem(tabName = "summary",
              DT::dataTableOutput('summary')
      )
    )
  )
)

server <- function(input, output, session) {
  
  # Doing calculation
  learning_result <- reactive({
    # Get all column names 
    all_columns <- names(DT)
    
    # Get all data
    dataset <- DT[Disease == input$select1,][, all_columns[which(!all_columns %in% c("Year","Disease","Number","Med_case","Avg_case","Med_case", "Above_Med"))], with =F]
    
    # For each disease, we do a prediction
    validation_index <- createDataPartition(dataset$Above_Avg, p=0.70, list=FALSE)
    
    # select 30% of the data for validation
    validation <- dataset[-validation_index,]
    
    # use the remaining 80% of data to training and testing the models
    dataset <- dataset[validation_index,]
    
    # Run algorithms using 10-fold cross validation
    control <- trainControl(method="cv", number=3)
    metric <- "Accuracy"
    
    withProgress(message = 'Runing Machine Learn Algorithm', value = 0, {
      # Get Number of algorithm
      algorithms <- input$select2
      
      # Number of times we'll go through the loop
      n <- length(algorithms)
      
      # Get a result list
      result_list <- list()
      
      for (i in 1:n) {
        # GLM
        set.seed(7)
        result_list[[algorithms[i]]] <- train(Above_Avg~., data = dataset, method = algorithms[i], metric = metric, trControl = control)
        
        # Increment the progress bar, and update the detail text.
        incProgress(1/n, detail = paste("Runing ", algorithms[i]))
        
      }
      
      # Return result list
      result_list
    })
  })
  
  output$plot1 <- renderPlotly({
    
    # Get the learning result
    result_list <- learning_result()
    
    # Create a result table
    acc_dt_list <- list()
    
    # Get the most accurate prediction
    model_names <- names(result_list)
    
    # Get accuacy of each method 
    for(model in model_names){
      accuracy <- max(result_list[[model]]$results$Accuracy)
      acc_dt_list[[model]] <- list(Algorithm = model, Accuarcy_real = accuracy)
    }
    
    plot_dt <- rbindlist(acc_dt_list)
    plot_dt[, Accuarcy := Accuarcy_real / min(Accuarcy_real)]
    plot_dt$Algorithm <- factor(plot_dt$Algorithm, levels = unique(plot_dt$Algorithm)[order(plot_dt$Accuarcy, decreasing = F)])
    p <- plot_ly(data = plot_dt, x = ~Accuarcy, y = ~Algorithm, text = ~ paste0("Prediction ",100 * round(Accuarcy_real, 2), " %") ,type = 'bar', orientation = 'h')
    
    p %>% layout(yaxis = list(title = ""))
  })
  
  
  output$plot2 <- renderPlotly({
    
    var_importance <- varImp(learning_result()[["glm"]], scale = T)
    
    plot_dt <- data.table(Weather_Factor = rownames(var_importance$importance),
                          Ranking =  var_importance$importance) 
    names(plot_dt) <- c("Weather_Factor", "Ranking")
    setkey(plot_dt, Weather_Factor)
    plot_dt <- plot_dt[metric_ex]
    plot_dt$Weather_Factor <- factor(plot_dt$Weather_Factor, levels = unique(plot_dt$Weather_Factor)[order(plot_dt$Ranking, decreasing = F)])
    p <- plot_ly(data = plot_dt, x = ~Ranking, y = ~Weather_Factor ,text = ~Explanation, type = 'bar', orientation = 'h')
    
    p %>% layout(yaxis = list(title = ""))
  })
  
  # Create a click event
  # Create a new pop-up window when people 
  observeEvent(event_data("plotly_click"),{
    
    data <- event_data("plotly_click")
    
    showModal(modalDialog(
      verbatimTextOutput("value"),
      footer = tagList(
        modalButton("Cancel")
      )
    ))
    
    output$value <- renderPrint({
      learning_result()[[data$y]]
    })
    
  })
  
  output$weatherfactor <- renderUI({
    # Get all unique widgets 
    factor_names <- metric_ex$Weather_Factor
    lapply(factor_names, function(x){
      n <- DT[Disease == input$select1,][[x]]
      numericInput(x, metric_ex[Weather_Factor == x, Explanation],round(mean(n)), min = round(min(n)), max = round(max(n)))
    })
  })
  
  observeEvent(input$bt, {
    
    # Get predicted result
    result_list <- learning_result()
    
    # Get the most accurate prediction
    model_names <- names(result_list)
    most_accurate_model <- model_names[1]
    
    for(model in model_names[2:length(model_names)]){
      if(max(result_list[[model]]$results$Accuracy) > max(result_list[[most_accurate_model]]$results$Accuracy)){
        most_accurate_model <- model
      }
    }
    
    #Get all input data and doing prediction
    pre_list <- list()
    
    for(i in metric_ex$Weather_Factor){
      pre_list[[i]] <- isolate(input[[i]])
    }
    
    pre_dt <- data.frame(pre_list)
    names(pre_dt) <- metric_ex$Weather_Factor
    
    prediction <- predict(result_list[[most_accurate_model]], pre_dt)
    
    if(prediction == "No"){
      sendSweetAlert(
        session = session,
        title = paste0(input$select1, " is within the safe range!"),
        text = NULL,
        type = "success"
      )
    }else{
      sendSweetAlert(
        session = session,
        title = paste0(input$select1, " is within the danger range!"),
        text = NULL,
        type = "warning"
      )
    }
    
  })
  
  # Render summary  data table
  output$summary <- DT::renderDataTable({
    DT <- fread("summary.csv")
    # Predict the missing value
    numeric_columns <- names(DT)[sapply(DT, is.numeric)]
    DT[,(numeric_columns) := round(.SD,2), .SDcols=numeric_columns]
    }, 
                                                 selection = 'single', 
                                                 options = list(scrollX = TRUE, pageLength = 20))
}

shinyApp(ui, server)
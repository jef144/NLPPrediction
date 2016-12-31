library(shiny)
library(datasets)

# Define server logic required to summarize and view the selected dataset
shinyServer(function(input, output) {
  
  # Return the requested dataset
  datasetInput <- reactive({
   input$ngram   
  })
  

  
  # Show the first "n" observations
  source("PredictionMethod.R")
  
  #dfResult <- data.frame(post_gram =  "my stuff", prob = .2)

  
  
  
  output$view <- renderTable({
    dfResult <- predictWord(input$ngram)
    dfResult
  })
}) 
library(shiny)

# Define UI for textual prediction application
# Project for final Capstone course in Coursera Data Sciences
shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("Textual Prediction Application"),
  
  # Sidebar with controls to select a dataset and specify the number
  # of observations to view
  sidebarPanel(
  textInput("ngram", "Enter a phrase or partial sentence to see the eventual prediction" )
  ),
  
  # Show a summary of the dataset and an HTML table with the requested
  # number of observations
  mainPanel(
  #verbatimTextOutput("summary"),
    tableOutput("view")
  )
))
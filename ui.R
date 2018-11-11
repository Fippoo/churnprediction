library(shiny)

# Define UI for miles per gallon application
shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("Customer Churn Prediction"),
  
  sidebarPanel(
    selectInput("variable", "Variable:",
                list("Churn for Dependents" = "plt1", 
                     "Churn for Tenure" = "plt2", 
                     "Churn for Contract" = "plt3",
                     "Churn for PaperlessBilling" = "plt4",
                     "Decision Tree Plot" = "plt5",
                     "Decision Tree ROC Plot" = "plt6"))
  ),
  
  mainPanel(
    plotOutput("churnPlots")
  )
))

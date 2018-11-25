library(shiny)

# Define UI for miles per gallon application
shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("Customer Churn Prediction"),
  
  sidebarPanel(
    width=0.1
  ),
  
  mainPanel(
    titlePanel("Telco Customer's Data"),
    
    h3(textOutput("TEXT0")),
    textOutput("TEXT01"),
    
    h3(textOutput("TEXT1")),
    textOutput("TEXT2"),
    
    h3(textOutput("TEXT3")),
    htmlOutput("TEXT4"),
    
    h3(textOutput("TEXT5")),
    htmlOutput("TEXT6"),
    
    DT::dataTableOutput("render_dataframe"),

    #selectInput("variable", "Select different plots:",
    #            list("Customer Demographic Plots" = "plt1", 
    #                 "Telco Services Plots" = "plt2", 
    #                 "Customer's Account Plots" = "plt3",
    #                 "Decision Tree Plot" = "plt5",
    #                 "Decision Tree ROC Plot" = "plt6")),
    htmlOutput("TEXT7"),
    plotOutput("demographicPlots"),
    
    htmlOutput("TEXT8"),
    plotOutput("servicePlots"),
    
    htmlOutput("TEXT9"),
    plotOutput("accountPlots"),
    
    htmlOutput("TEXT10"),
    plotOutput("dtreePlots"),
    
    htmlOutput("TEXT11"),
    plotOutput("aucPlots")
    
  )
))

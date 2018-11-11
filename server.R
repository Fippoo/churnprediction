# loading libraries

library(rpart)      # Creating decision tree
library(rpart.plot) # Plot decision tree
library(caret)      # Claculate model performance
library(e1071)      # Claculate model accuracy
library(shiny)      # For ShinyApp
library(pROC)       # Show performance plot of the model


# Define server logic required to plot various variables against mpg
shinyServer(function(input, output) {
  ### Read the dataset
  filepath <- "C:/Users/psmai/Documents/Wissen/ISO HS Aalen/Business Analytics - Anwendungsentwicklung/TelcoCustomerChurn/data/telco-customer-churn/dataset.csv"
  df <- read.csv(file = filepath)
  
  ### PREPARATION of the dataset for modeling
  # Feature engineering to count number of services each customer has
  df$ART_countOfService <- 0
  serviceColumns <- c("PhoneService", "MultipleLines", "InternetService",
                      "OnlineSecurity", "OnlineBackup", "DeviceProtection",
                      "TechSupport", "StreamingTV", "StreamingMovies")
  df_subset <- df[, serviceColumns]
  for(customer in 1:nrow(df_subset)){
    row <- as.numeric(df_subset[customer, ])
    df$ART_countOfService[customer] <- sum(row != 1)
  }
  
  # Feature engineering to find tenure
  df$ART_tenure1To2 <- 0
  df$ART_tenure2To3 <- 0
  df$ART_tenure3To4 <- 0
  df$ART_tenure4To5 <- 0
  df$ART_tenure5To6 <- 0
  for(customer in 1:nrow(df)){
    if(df$tenure[customer] < 13){
      df$ART_tenure1To2[customer] <- 1
    }else if(df$tenure[customer] < 25){
      df$ART_tenure2To3[customer] <- 1
    }else if(df$tenure[customer] < 37){
      df$ART_tenure3To4[customer] <- 1
    }else if(df$tenure[customer] < 49){
      df$ART_tenure3To4[customer] <- 1
    }else if(df$tenure[customer] < 61){
      df$ART_tenure4To5[customer] <- 1
    }else{
      df$ART_tenure5To6[customer] <- 1
    }
  }
  
  
  ### MODELING using machine learning
  # Dropping unnecessary or least correlated features to target
  drop_columns <- c("customerID", "gender", "Partner", "TotalCharges", "MonthlyCharges")
  df <- df[, !(colnames(df) %in% drop_columns)]
  
  # Split into train and test sets
  set.seed(123)
  smp_size <- floor(0.75 * nrow(df))
  ind <- sample(seq_len(nrow(df)), size = smp_size)
  df_train <- df[ind, ]
  df_test <- df[-ind, ]
  
  colnames(df)
  
  # CART model
  decisionTree = rpart(Churn ~ SeniorCitizen+SeniorCitizen+tenure+PhoneService+
                         MultipleLines+InternetService+OnlineSecurity+OnlineBackup+
                         DeviceProtection+TechSupport+StreamingTV+StreamingMovies+
                         Contract+PaperlessBilling+PaymentMethod+ART_countOfService+
                         ART_tenure1To2+ART_tenure1To2+ART_tenure3To4+
                         ART_tenure4To5+ART_tenure5To6, data=df_train)
  
  # Performance of the classifier
  original <- df_test$Churn
  original <- as.numeric(original)
  predictions = predict(decisionTree, newdata = df_test, type="class")
  
  formulaText <- reactive({
    paste("variable ~", "var1")
  })
  
  # Generate a plot of the requested variable against mpg and only 
  # include outliers if requested
  output$churnPlots <- renderPlot({
    if(input$variable == "plt1"){
      df$Dependents <- as.factor(df$Dependents)
      plot(x = df$Dependents, 
           y = df$Churn,
           col = c("#3399ff", "#ff9966")
      )
      legend("topright", c("Retained", "Churned"), 
             fill = c("#3399ff", "#ff9966"), bty = "n")
      title("Impact of Dependents of customer")
    }
    if(input$variable == "plt2"){
      df$tenure <- as.factor(df$tenure)
      plot(x = df$tenure, y = df$Churn)
      title("Impact of Tenure of the contract")
    }
    if(input$variable == "plt3"){
      df$Contract <- as.factor(df$Contract)
      plot(x = df$Contract, y = df$Churn, col = c("#3399ff", "#ff9966"))
      legend("topright", c("Retained", "Churned"), 
             fill = c("#3399ff", "#ff9966"), bty = "n")
      title("Impact of Contract Type")
    }
    if(input$variable == "plt4"){
      df$PaperlessBilling <- as.factor(df$PaperlessBilling)
      plot(x = df$PaperlessBilling, y = df$Churn, col = c("#3399ff", "#ff9966"))
      legend("topright", c("Retained", "Churned"), 
             fill = c("#3399ff", "#ff9966"), bty = "n")
      title("Impact of PaperlessBilling")
    }
    if(input$variable == "plt5"){
      prp(decisionTree)
    }
    if(input$variable == "plt6"){
      plot(roc(as.numeric(original),as.numeric(predictions)), legacy.axes = TRUE)
    }
  })
  
})

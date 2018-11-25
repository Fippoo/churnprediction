# loading libraries
library(ggplot2)
library(rpart)      # Creating decision tree
library(rpart.plot) # Plot decision tree
library(caret)      # Claculate model performance
library(e1071)      # Claculate model accuracy
library(shiny)      # For ShinyApp
library(ggpubr)     # For grid plots
library(scales)     # Show percentages on ggplot
library(pROC)       # Show performance plot of the model


# Define server logic required to plot various variables against mpg
shinyServer(function(input, output) {
  ### Read the dataset
  filepath <- "C:/test/dataset.csv"
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
  #drop_columns <- c("customerID", "gender", "Partner", "TotalCharges", "MonthlyCharges")
  #df <- df[, !(colnames(df) %in% drop_columns)]
  
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
  output$TEXT0 <- renderText({
    "Source:"
  })
  
  output$TEXT01 <- renderText({
    "https://www.kaggle.com/blastchar/telco-customer-churn/home"
  })
  
  output$TEXT1 <- renderText({
    "Context:"
  })
  
  output$TEXT2 <- renderText({
    "Predict behavior to retain customers. You can analyze all relevant customer data and develop focused customer retention programs. [IBM Sample Data Sets]"
  })
  
  output$TEXT3 <- renderText({
    "Content:"
  })
  
  output$TEXT4 <- renderUI({
    HTML(
    paste0("Each row represents a customer, each column contains customer’s attributes described on the column Metadata.", "<br/>",

    "The data set includes information about: ", "<br/>",
    
    " + Customers who left within the last month – the column is called Churn", "<br/>",
    " + Services that each customer has signed up for – phone, multiple lines, internet, online security, online backup, device protection, tech support, and streaming TV and movies", "<br/>",
    " + Customer account information – how long they’ve been a customer, contract, payment method, paperless billing, monthly charges, and total charges", "<br/>",
    " + Demographic info about customers – gender, age range, and if they have partners and dependents"
    , "<br/>")
    )
  })
  
  output$TEXT5 <- renderText({
    "Columns description (relevant):"
  })
  
  output$TEXT6 <- renderUI({
    HTML(
      paste0(
        " - SeniorCitizen: Whether the customer is a senior citizen or not (1, 0)", "<br/>",
        " - Partner: Whether the customer has a partner or not (Yes, No)", "<br/>",
        " - Dependents: Whether the customer has dependents or not (Yes, No)", "<br/>",
        " - tenure: Number of months the customer has stayed with the company", "<br/>",
        " - PhoneService: Whether the customer has a phone service or not (Yes, No)", "<br/>",
        " - Contract: The contract term of the customer (Month-to-month, One year, Two year)", "<br/>",
        " - PaperlessBilling: Whether the customer has paperless billing or not (Yes, No)", "<br/>",
        " - PaymentMethod: The customer’s payment method (Electronic check, Mailed check, Bank transfer (automatic), Credit card (automatic))",
        " - Churn: Whether the customer churned or not (Yes or No)", "<br/>"
        , "<br/>")
    )
  })
  
  
  # Render the dataframe
  output$render_dataframe <- DT::renderDataTable({
    df
  })
  
  output$TEXT7 <- renderUI({
    HTML(
      "<br/>",
      "<br/>",
      "<br/>",
        "<h3>DEMOGRAPHIC FEATURES:</h3>",
        "<br/>",
        "+ Gender: Equal distribution of males and females.","<br/>",
        "+ Partner: Customers with a partner do churn with a lower likelihood. (as compared to customers who are single)","<br/>",
        "+ Dependents: Customers with a dependents do churn with a lower likelihood. (as compared to having a partner)","<br/>",
        "+ SeniorCitizen: Senior citizen customers do churn with higher likelihood."
      ,"<br/>"
      ,"<br/>")
  })
  
  # Generate a plot of the requested variable against mpg and only 
  # include outliers if requested
  output$demographicPlots <- renderPlot({
    # Look on demographic features
    # Gender, Partner, Dependents, SeniorCItizens
    plt1 <- ggplot(df, aes(x=gender,fill=Churn)) + 
      geom_bar(position = 'fill') + 
      ggtitle("Churn per Gender") +
      ylab("Percentage") +
      scale_y_continuous(labels = scales::percent) + 
      scale_fill_manual(values = c("steelblue", "grey"))
    plt2 <- ggplot(df, aes(x=Partner,fill=Churn)) + 
      geom_bar(position = 'fill') + 
      ggtitle("Churn per Partner") +
      ylab("Percentage") +
      scale_y_continuous(labels = scales::percent) + 
      scale_fill_manual(values = c("steelblue", "grey"))
    plt3 <- ggplot(df, aes(x=Dependents,fill=Churn)) + 
      geom_bar(position = 'fill') + 
      ggtitle("Churn per Dependents") +
      ylab("Percentage") +
      scale_y_continuous(labels = scales::percent) + 
      scale_fill_manual(values = c("steelblue", "grey"))
    plt4 <- ggplot(df, aes(x=as.factor(SeniorCitizen),fill=Churn)) + 
      geom_bar(position = 'fill') + 
      ggtitle("Churn per SeniorCitizen") +
      ylab("Percentage") +
      scale_y_continuous(labels = scales::percent) + 
      scale_fill_manual(values = c("steelblue", "grey"))
    gg_plt1 <- ggarrange(plt1, plt2, plt3, plt4, ncol = 2, nrow = 2)
    gg_plt1
    annotate_figure(gg_plt1)
  })
  
  output$TEXT8 <- renderUI({
    HTML(
      "<br/>",
      "<br/>",
      "<br/>",
      "<h3>SERVICE FEATURES:</h3>",
      "<br/>",
      "+ PhoneService: It has no significant difference for churn prediction.","<br/>",
      "+ StreamingTV: Customers with StreamingTV but without having internet service has lowest likelihood to churn.","<br/>",
      "+ InterService: Customers with fiber optic have the highest likelihood to churn.","<br/>",
      "+ TechSupport: Customers with fiber optic have the highest likelihood to churn."
      ,"<br/>"
      ,"<br/>")
  })
  
  output$servicePlots <- renderPlot({
    # Look into services features
    # phone, multiple lines, internet, online security, online backup,
    # device protection, tech support, and streaming TV and movies
    plt1 <- ggplot(df, aes(x=PhoneService,fill=Churn)) + 
      geom_bar(position = 'fill') + 
      ggtitle("Churn per PhoneService") +
      ylab("Percentage") +
      scale_y_continuous(labels = scales::percent) + 
      scale_fill_manual(values = c("steelblue", "grey")) 
    plt2 <- ggplot(df, aes(x=StreamingTV,fill=Churn)) + 
      geom_bar(position = 'fill') + 
      ggtitle("Churn per StreamingTV") +
      ylab("Percentage") +
      scale_y_continuous(labels = scales::percent) + 
      scale_fill_manual(values = c("steelblue", "grey"))
    plt3 <- ggplot(df, aes(x=InternetService,fill=Churn)) + 
      geom_bar(position = 'fill') + 
      ggtitle("Churn per InternetService") +
      ylab("Percentage") +
      scale_y_continuous(labels = scales::percent) + 
      scale_fill_manual(values = c("steelblue", "grey"))
    plt4 <- ggplot(df, aes(x=TechSupport,fill=Churn)) + 
      geom_bar(position = 'fill') + 
      ggtitle("Churn per TechSupport") +
      ylab("Percentage") +
      scale_y_continuous(labels = scales::percent) + 
      scale_fill_manual(values = c("steelblue", "grey"))
    gg_plt2 <- ggarrange(plt1, plt2, plt3, plt4, ncol = 2, nrow = 2)
    gg_plt2
    annotate_figure(gg_plt2)
  })
  
  output$TEXT9 <- renderUI({
    HTML(
      "<br/>",
      "<br/>",
      "<br/>",
      "<h3>CUSTOMER'S ACCOUNT FEATURES & TENURE:</h3>",
      "<br/>",
      "+ Contract: The longer duration of contract shows lower likelihood to churn.","<br/>",
      "+ PaperlessBilling: Customers with PaperlessBilling do churn with higher likelihood.","<br/>",
      "+ Tenure: Customers with lower tenure do churn with higher likelihood."
      ,"<br/>"
      ,"<br/>"
      ,"<br/>")
  })
  
  output$accountPlots <- renderPlot({
    # Look into Customer account information & tenure features
    plt1 <- ggplot(df, aes(x=Contract,fill=Churn)) + 
      geom_bar(position = 'fill') + 
      ggtitle("Churn per Contract") +
      ylab("Percentage") +
      scale_y_continuous(labels = scales::percent) + 
      scale_fill_manual(values = c("steelblue", "grey"))
    plt2 <- ggplot(df, aes(x=PaperlessBilling,fill=Churn)) + 
      geom_bar(position = 'fill') + 
      ggtitle("Churn per PaperlessBilling") +
      ylab("Percentage") +
      scale_y_continuous(labels = scales::percent) + 
      scale_fill_manual(values = c("steelblue", "grey"))
    plt3 <- ggplot(df, aes(x=tenure,fill=Churn)) + 
      geom_bar(position = 'fill') + 
      ggtitle("Churn per Tenure") +
      ylab("Percentage") +
      scale_y_continuous(labels = scales::percent) + 
      scale_fill_manual(values = c("steelblue", "grey"))
    gg_plt2 <- ggarrange(plt1, plt2, plt3, ncol = 2, nrow = 2)
    gg_plt2
    annotate_figure(gg_plt2)
  })
  
  output$TEXT10 <- renderUI({
    HTML(
      "<br/>",
      "<br/>",
      "<br/>",
      "<h3>DECISION TREE MODEL:</h3>",
      "<br/>",
      "+ What is a decision Tree: Decision tree learning is the construction of a decision tree from class-labeled training tuples. A decision tree is a flow-chart-like structure, where each internal (non-leaf) node denotes a test on an attribute, each branch represents the outcome of a test, and each leaf (or terminal) node holds a class label. The topmost node in a tree is the root node. ","<br/>",
      "+ Source: https://en.wikipedia.org/wiki/Decision_tree_learning","<br/>",
      "+ Conclusion: Decision tree model shows that contract duration is the most relevant column, then internet service, then tenure and then tech support."
      ,"<br/>")
  })
    
  output$dtreePlots <- renderPlot({
    prp(decisionTree)
  })
  
  output$TEXT11 <- renderUI({
    HTML(
      "<br/>",
      "<br/>",
      "<br/>",
      "<h3>DECISION TREE PERFORMANCE:</h3>",
      "<br/>",
      "+ What is model performance: Model performance can be understood by ROC curve and area under the curve. Higher the area under the curve, better the model is performing on the test data set.","<br/>",
      "+ Source: https://en.wikipedia.org/wiki/Receiver_operating_characteristic","<br/>",
      "+ Conclusion: The model performs very well with roughly 80% of the area under the curve."
      ,"<br/>"
      ,"<br/>"
      ,"<br/>")
  })
  
  output$aucPlots <- renderPlot({
    plot(roc(as.numeric(original),as.numeric(predictions)), legacy.axes = TRUE)
  })

  
})# END of shinyServer()

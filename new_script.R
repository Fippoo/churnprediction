.libPaths(.libPaths()[2])

# loading libraries
library(ggplot2)
library(data.table)
library(rpart)      # Creating decision tree
library(rpart.plot) # Plot decision tree
library(caret)      # Claculate model performance
library(e1071)      # Claculate model accuracy
library(shiny)      # For ShinyApp
library(ggpubr)     # For grid plots
library(scales)     # Show percentages on ggplot

# Hello world example for shiny app
runExample("01_hello")


### [1] Read the dataset
filepath <- "dataset.csv"
df <- read.csv(file = filepath)

### [2] UNDERSTANDING the dataset
# View the dataset
View(df)
# Structure of the dataset
str(df)
# Look on the target variable
df$Churn <- as.factor(df$Churn)
# Does a customer have more than 1 contract?
length(unique(df$customerID))

# Look on demographic features
# Gender, Partner, Dependents, SeniorCItizens
plt1 <- ggplot(df, aes(x=gender,fill=Churn)) + 
  geom_bar() + 
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
plt4 <- ggplot(df, aes(x=SeniorCitizen,fill=Churn)) + 
  geom_bar(position = 'fill') + 
  ggtitle("Churn per SeniorCitizen") +
  ylab("Percentage") +
  scale_y_continuous(labels = scales::percent) + 
  scale_fill_manual(values = c("steelblue", "grey"))
gg_plt1 <- ggarrange(plt1, plt2, plt3, plt4, ncol = 2, nrow = 2)
gg_plt1
annotate_figure(gg_plt1,   top = "DEMOGRAPHIC FEATURES")

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
plt3 <- ggplot(df, aes(x=OnlineSecurity,fill=Churn)) + 
  geom_bar(position = 'fill') + 
  ggtitle("Churn per OnlineSecurity") +
  ylab("Percentage") +
  scale_y_continuous(labels = scales::percent) + 
  scale_fill_manual(values = c("steelblue", "grey"))
plt4 <- ggplot(df, aes(x=DeviceProtection,fill=Churn)) + 
  geom_bar(position = 'fill') + 
  ggtitle("Churn per DeviceProtection") +
  ylab("Percentage") +
  scale_y_continuous(labels = scales::percent) + 
  scale_fill_manual(values = c("steelblue", "grey"))
gg_plt2 <- ggarrange(plt1, plt2, plt3, plt4, ncol = 2, nrow = 2)
gg_plt2
annotate_figure(gg_plt2,   top = "SERVICE FEATURES")

# Look into Customer account information features
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
gg_plt2 <- ggarrange(plt1, plt2, ncol = 2, nrow = 1)
gg_plt2
annotate_figure(gg_plt2,   top = "CUSTOMER'S ACCOUNT FEATURES")

### [3] CLEANING THE DATASET
# Looking for missing values in the dataset
sum(is.na(df$customerID))
sum(is.na(df$tenure))
sum(is.na(df$Contract))
sum(is.na(df$Dependents))
sum(is.na(df$TotalCharges))
sum(is.na(df$Churn))
# So totalCharges has 11 missing values
# These 11 customers are as following
View(df[is.na(df$TotalCharges), ])

# Since these have tenure 0, that means they just started
# a contract & have not yet recieved a bill yet
# So, we should not delete them and keep them as it is


### [4] PREPARATION of the dataset for modeling
# Feature engineering to count number of services each customer has
df$ART_countOfService <- 0
serviceColumns <- c("PhoneService", "MultipleLines", "InternetService",
                    "OnlineSecurity", "OnlineBackup", "DeviceProtection",
                    "TechSupport", "StreamingTV", "StreamingMovies")
df_subset <- df[, serviceColumns]
View(df_subset)

# Loop to calculate values for ART_countOfService
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


### [5] MODELING using machine learning
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
decisionTree = rpart(Churn ~ SeniorCitizen+tenure+PhoneService+
                       MultipleLines+InternetService+OnlineSecurity+OnlineBackup+
                       DeviceProtection+TechSupport+StreamingTV+StreamingMovies+
                       Contract+PaperlessBilling+PaymentMethod+ART_countOfService+
                       ART_tenure1To2+ART_tenure1To2+ART_tenure3To4+
                       ART_tenure4To5+ART_tenure5To6, data=df_train)


# Plot the tree using prp command defined in rpart.plot package
prp(decisionTree)
printcp(decisionTree) # See results of training

### [6] PREDICTING using machine learning model
original <- df_test$Churn
summary(original)
predictions = predict(decisionTree, newdata = df_test, type="class")
table(predictions)
cm <- confusionMatrix(original, predictions)
print(paste0("Model accuracy is ", 100*cm$overall[1], "%"))

# Command to run shiny app
location_ui_server = "shinyapp/"
runApp(location_ui_server)

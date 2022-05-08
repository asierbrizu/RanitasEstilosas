# Clear plots
if(!is.null(dev.list())) dev.off()
# Clear console
cat("\014") 
# Clean workspace
rm(list=ls())
# Set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Install required packages
library(lattice)
library(ggplot2)
library(caret)
library(rpart)
library(rpart.plot)

# Read data from CSV
filename = "../data/covid-19-symptoms.tab"
data <- read.csv(file = filename, sep =" ", header = TRUE)

# Convert columns to factors
index <- 1:ncol(data)
data[ , index] <- lapply(data[ , index], as.factor)

# Percentaje of training examples
training_p <- 0.8

# Generate data partition 80% training / 20% test. The result is a vector with the indexes 
# of the examples that will be used for the training of the model.
training_indexes <- createDataPartition(y = data$TARGET, p = training_p, list = FALSE)

# Split training and test data
training_data <- data[training_indexes, ]  # Extract training data using training_indexes
test_data     <- data[-training_indexes, ] # Extract data with the indexes not included in training_indexes 

# Create Linear Model using training data. Formula = all the columns except Salary
model <- rpart(formula = TARGET ~., data = training_data)

# Make the prediction using the model and test data
prediction <- predict(model, test_data, type = "class")

# Calculate accuracy using Confusion Matrix
prediction_results <- table(test_data$TARGET, prediction)
matrix <- confusionMatrix(prediction_results)
accuracy <- matrix$overall[1]

# Print the accuracy
accuracy <- paste0("Accuracy = ", round(100*accuracy, digits = 2), "%")
print(accuracy, quote = FALSE)

# Print attributes in descending relevance
attrs <- names(model$variable.importance)

print("Attributes in descending order of relevance")

for (i in 1:length(attrs)) {
  print(paste0("  ", attrs[i]), quote = FALSE)
}

# Plot tree (this method is slow, wait until pot is completed)
rpart.plot(model, 
           type = 2,
           extra = 102,
           tweak = 1.1,
           box.palette = "GnYlRd",
           shadow.col = "darkgray",
           main = "Go to hospital or stay at home?", 
           sub = accuracy)

# Print the rules that represent the Decision Tree
rpart.rules(model, 
            style="wide", 
            cover = TRUE, 
            eq = "=", 
            when = "IF", 
            and = "&&", 
            extra = 4)
initialize = function(fileCsv){
  # (STEP 1) Save the Csv file into a data frame.
  data <- read.csv(file = "CAR DETAILS FROM CAR DEKHO.csv")
  
  # (STEP 2) Remove the column name from the data frame.
  col.rmv <- "name"
  data <- data[, ! names(data) %in% col.rmv, drop = F]
  
  # (STEP 3) Remove the row where the fuel is "Electric".
  data <- data[!grepl("Electric", data$fuel), ]
  
  # (STEP 4) Improve visualy the data frame.
  knitr::kable(head(data), caption = paste0("A glipmse of the data from ", fileCsv))
  
  return(data)
}

creationDataPartiton = function(perc, target){
  
  # (STEP 1) Create the data partition with the percentage and target wanted.
  dataPartition <- createDataPartition(
    y = target,
    p = perc,
    list = FALSE
  )
  
  return(dataPartition)
}

# (STEP 1) Read the provided file and store in a data.frame.
data <- initialize("CAR DETAILS FROM CAR DEKHO.csv")

# SELLER_TYPE #
i <- 0
while(i<=10){
  
  # (STEP 2) Randomly divide data in train (80%) and test (20%).
  index.train <- creationDataPartiton(0.8, data$seller_type)
  index.test <- creationDataPartiton(0.2, data$seller_type)
  
  # (STEP 3) Get partitions.
  data.train <- data[index.train, ]
  data.test <- data[-index.train, ]
  
  print(glue(""))
  print(glue("Size of the training set: {nrow(data.train)}"))
  print(glue("Size of the test set: {nrow(data.test)}")) 
  
  # (STEP 4) Train a decision tree to predict the seller_type.
  modelSellerType = rpart(formula = seller_type ~., data = data.train)
  print(modelSellerType)
  
  # (STEP 5) Make predictions over the test data.
  predictionSellerType = predict(modelSellerType, data.test, type = "class")
  print(glue(""))
  
  print("")
  
  # (STEP 6) Create and print confusion matrix.
  matSellerType = caret::confusionMatrix(as.factor(data.test$seller_type), predictionSellerType)
  print(matSellerType$table)
  
  # (STEP 7) Calculate and print the accuracy.
  print(glue(""))
  print(glue("The model of the seller_type hits in {round(100*matSellerType$overall[[1]],2)}"))
  
  i <- i + 1
}

# (STEP 8) Print the top-5 most relevant columns.
rel_cols <- names(modelSellerType$variable.importance)
print("TOP-5 MOST RELEVANT COLUMNS:")
for(w in 1:length(rel_cols) - 1){
  print(glue("Number {w}: {rel_cols[w]} "))
}

# OWNER #
j <- 0
while(j<=10){
  
  # (STEP 2) Randomly divide data in train (80%) and test (20%).
  index.train <- creationDataPartiton(0.8, data$owner)
  index.test <- creationDataPartiton(0.2, data$owner)
  
  # (STEP 3) Get partitions.
  data.train <- data[index.train, ]
  data.test <- data[-index.train, ]
  
  print(glue(""))
  print(glue("Size of the training set: {nrow(data.train)}"))
  print(glue("Size of the test set: {nrow(data.test)}")) 
  
  # (STEP 4) Train a decision tree to predict the owner.
  modelOwner = rpart(formula = owner ~., data = data.train)
  print(modelSellerType)
  
  # (STEP 5) Make predictions over the test data.
  predictionOwner = predict(modelOwner, data.test, type = "class")
  print(glue(""))
  
  print("")
  
  # (STEP 6) Create and print confusion matrix.
  matOwner = caret::confusionMatrix(as.factor(data.test$owner), predictionOwner)
  print(matOwner$table)
  
  # (STEP 7) Calculate and print the accuracy.
  print(glue(""))
  print(glue("The model of the owner hits in {round(100*matOwner$overall[[1]],2)}"))
  
  j <- j + 1
}

# (STEP 8) Print the top-5 most relevant columns.
rel_cols <- names(modelOwner$variable.importance)
print("TOP-5 MOST RELEVANT COLUMNS:")
for(w in 1:length(rel_cols) - 1){
  print(glue("Number {w}: {rel_cols[w]} "))
}
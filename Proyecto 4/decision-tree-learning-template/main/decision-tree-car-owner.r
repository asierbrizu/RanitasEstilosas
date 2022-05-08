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
filename = "../data/CAR DETAILS FROM CAR DEKHO.csv"
data <- read.csv(file = filename, sep =",", header = TRUE)

# Transform and delete data
data$name = NULL
data <- transform(data, fuel = ifelse(fuel == "Petrol", 0, 
                                      ifelse(fuel == "Diesel", 1, 
                                             ifelse(fuel == "Electric", 2, 
                                                    ifelse(fuel == "LPG", 3, 4)))))

# Percentaje of training examples
training_p <- 0.8

best_model    <- NULL
best_accuracy <- 0

for (i in 1:10){
  # Generate data partition 80% training / 20% test. The result is a vector with the indexes 
  # of the examples that will be used for the training of the model.
  training_indexes <- createDataPartition(y = data$owner, p = training_p, list = FALSE)
  
  # Split training and test data
  training_data <- data[training_indexes, ]  # Extract training data using training_indexes
  test_data     <- data[-training_indexes, ] # Extract data with the indexes not included in training_indexes 
  
  # Create Linear Model using training data. Formula = all the columns except Salary
  #model <- rpart(formula = owner ~., data = training_data)
  model <- rpart(formula = owner ~year + km_driven + selling_price, data = training_data)
  
  # Make the prediction using the model and test data
  prediction <- predict(model, test_data, type = "class")
  
  # Calculate accuracy using Confusion Matrix
  prediction_results <- table(test_data$owner, prediction)
  matrix <- confusionMatrix(prediction_results)
  accuracy <- matrix$overall[1]
  
  if (best_accuracy < accuracy){
    best_accuracy <- accuracy
    best_model    <- model
  }
}

# Print the accuracy
accuracy <- paste0("Accuracy = ", round(100*best_accuracy, digits = 2), "%")
print(accuracy, quote = FALSE)

# Print attributes in descending relevance
attrs <- names(best_model$variable.importance)

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
           main = "owner", 
           sub = accuracy)

# Print the rules that represent the Decision Tree
rpart.rules(model, 
            style="wide", 
            cover = TRUE, 
            eq = "==", 
            when = "IF", 
            and = "&&", 
            extra = 4)


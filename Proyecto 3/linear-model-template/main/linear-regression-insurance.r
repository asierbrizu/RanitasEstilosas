# Clear Environment
rm(list=ls())
# Set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# Clear plots & console
if(!is.null(dev.list())) dev.off()
cat("\014") 

# Install required packages
library(ggplot2)
library(lattice)
library(caret)

# Read data
filename = "../data/insurance.csv"
data <- read.csv(file=filename, sep=",", header = TRUE)

#Transform variables to numerical
data <- transform(data, sex = ifelse(sex == "male", 1, 0))
data <- transform(data, smoker = ifelse(smoker == "yes", 1, 0))

# Remove non-numerical columns
data$region = NULL

par(mfrow = c(2,3), mar=c(2,2,2,2))

# Scatter Plot - Check linear relationships
for (col_name in colnames(data)) {
  if (col_name != "charges") {
    scatter.smooth(x=data[[col_name]], y=data$charges, main=col_name, col="lightgreen")
  }
}

# Correlation between variables
print("Correlation between each attribute and charges: A low correlation (-0.2 < x < 0.2)", quote=FALSE)

for (col_name in colnames(data)) {
  print(paste0(col_name, ": ", cor(data$charges, data[[col_name]])), quote=FALSE)
}

training.models <- function(nTrainings, data) {
  result            <- list()
  result$best_model <- NULL
  result$avg_error  <- 0
  result$best_error <- Inf
  
  # Percentage of training examples
  training_p <- 0.8
  
  for (i in 1:nTrainings){
    # Generate data partition 80% training / 20% test. The result is a vector with 
    # the indexes of the examples that will be used for the training of the model.
    training_samples <- createDataPartition(y = data$charges, p = training_p, list = FALSE)
    
    # Split training and test data
    training_data <- data[training_samples, ]
    test_data     <- data[-training_samples, ]
    
    # Create Linear Model using training data. Formula = all the columns except charges
    model <- lm(formula = training_data$charges ~., data = training_data)
    
    # Make the prediction using the model and test data
    prediction <- predict(model, test_data)
    
    # Calculate Mean Average Error
    mean_avg_error <- mean(abs(prediction - test_data$charges))
    
    result$avg_error <- result$avg_error + mean_avg_error
    
    if(mean_avg_error < result$best_error){
      result$best_error <- mean_avg_error
      result$best_model <- model
    }
  }
  return(result)
}

nTrainings <- 10

result <- training.models(nTrainings, data)

print(paste0("- Mean average error of all training models: ", (result$avg_error/nTrainings)))
print(paste0("- Mean average error of the best model: ", result$best_error))

# Print model summary
summary(result$best_model)

print("--------------------------------------------------------------")
print("Pregunta 1:")

data_ns        <- data[20,]
data_ns$smoker <- 0

prediction <- predict(result$best_model, data_ns)
data_ns    <- transform(data_ns, charges = prediction - charges)

print(data[20,])
print(data_ns)

print("--------------------------------------------------------------")
print("Pregunta 2:")

data_ys        <- data[3,]
data_ys$smoker <- 1

prediction <- predict(result$best_model, data_ys)
data_ys    <- transform(data_ys, charges = prediction - charges)

print(data[3,])
print(data_ys)

print("--------------------------------------------------------------")
print("Pregunta 3:")

data_5 <- transform(data, age = age + 5)

prediction <- predict(result$best_model, data_5)
data_5     <- transform(data_5, charges = prediction - charges)
data_5_or  <- data_5[order(data_5$charges, decreasing=TRUE),]

print("3 Perosnas con mayor coste:")
print(data_5_or[1:3,])


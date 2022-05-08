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

for(i in 1:10){
  # Generate data partition 80% training / 20% test. The result is a vector with 
  # the indexes of the examples that will be used for the training of the model.
  training_indexes <- createDataPartition(y = data$charges, p = 0.8, list = FALSE)
  
  # Split training and test data
  training_data <- data[training_indexes, ]  # Extract training data using training_indexes
  test_data     <- data[-training_indexes, ] # Extract data with the indexes not included in training_indexes 
  
  # Create Linear Model using training data. Formula = all the columns except Salary
  model <- lm(formula = charges ~., data = training_data)
  print(model)
  
  # Make the prediction using the model and test data
  prediction <- predict(model, test_data)
}

# Correlacion
print("Correlación entre cada atributo y coste: Baja correlacion (-0.2 < x < 0.2)", quote=FALSE)

for (col_name in colnames(data)) {
  print(paste0(col_name, ": ", cor(data$charges, data[[col_name]])), quote=FALSE)
}

training.models <- function(numTrainings, data) {
  result            <- list()
  result$best_model <- NULL
  result$avg_error  <- 0
  result$best_error <- Inf
  
  # Percentage of training examples
  training_p <- 0.8
  
  for (i in 1:numTrainings){
    training_samples <- createDataPartition(y = data$charges, p = training_p, list = FALSE)
    training_data <- data[training_samples, ]
    test_data     <- data[-training_samples, ]
    model <- lm(formula = training_data$charges ~., data = training_data)
    prediction <- predict(model, test_data)
    mean_avg_error <- mean(abs(prediction - test_data$charges))
    result$avg_error <- result$avg_error + mean_avg_error
    if(mean_avg_error < result$best_error){
      result$best_error <- mean_avg_error
      result$best_model <- model
    }
  }
  return(result)
}

numTrainings <- 10

result <- training.models(numTrainings, data)

print(paste0("- Error medio medio de todos los modelos: ", (result$avg_error/numTrainings)))
print(paste0("- Error medio medio del mejor modelo: ", result$best_error))

# Resumen mejor modelo
summary(result$best_model)

solucion <- data.frame(i1 = 0, c1 = 0,
                       i2 = 0, c2 = 0,
                       i3 = 0, c3 = 0)


for(i in 1:length(data$smoker)){
  aumento <- transform(data, age = age+5)
  aumentos <- predict(model, aumento)
  aumento <- transform(aumento, charges = aumentos - charges)
  resta <- (data[i,]$charges - aumento[i,]$charges)*(-1)
  
  if(solucion$c1 < resta){
    solucion$c3 <- solucion$c2
    solucion$i3 <- solucion$i2
    
    solucion$c2 <- solucion$c1
    solucion$i2 <- solucion$i1
    
    solucion$c1 <- resta
    solucion$i1 <- i
    
  }else if(solucion$c2 < resta){
    solucion$c3 <- solucion$c2
    solucion$i3 <- solucion$i2
    
    solucion$c2 <- resta
    solucion$i2 <- i
    
  }else if(solucion$c2 < resta){
    solucion$c3 <- resta
    solucion$i3 <- i
    
  }
}

#Esto es lo referente a la primera cuestion
data_ns        <- data[20,]
data_ns$smoker <- 0
prediction <- predict(result$best_model, data_ns)
data_ns    <- transform(data_ns, charges = prediction - charges)
print(data[20,])
print(data_ns)

#Esto es lo referente a la segunda cuestion
data_ys        <- data[3,]
data_ys$smoker <- 1
prediction <- predict(result$best_model, data_ys)
data_ys    <- transform(data_ys, charges = prediction - charges)
print(data[3,])
print(data_ys)

#Esto es lo referente a la tercera cuestion

#Primera persona
print(data[solucion$i1,])
print(solucion$c1)

#Segunda persona
print(data[solucion$i2,])
print(solucion$c2)

#Tercera persona
print(data[solucion$i3,])
print(solucion$c3)





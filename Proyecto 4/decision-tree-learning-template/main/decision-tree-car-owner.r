if(!is.null(dev.list())) dev.off()
cat("\014") 
rm(list=ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(ggplot2)
library(rpart.plot)
library(lattice)
library(caret)
library(rpart)

archivo = "../data/CAR DETAILS FROM CAR DEKHO.csv"
data <- read.csv(file = archivo, sep =",", header = TRUE)

data$name = NULL
data <- transform(data, fuel = ifelse(fuel == "Petrol", 0, 
                                      ifelse(fuel == "Diesel", 1, 
                                             ifelse(fuel == "Electric", 2, 
                                                    ifelse(fuel == "LPG", 3, 4)))))
precisionOptima <- 0
training_p <- 0.8
modeloOptimo    <- NULL


for (i in 1:10){
  training_indexes <- createDataPartition(y = data$owner, p = training_p, list = FALSE)
  training_data <- data[training_indexes, ]
  test_data     <- data[-training_indexes, ]
  modelo <- rpart(formula = owner ~year + km_driven + selling_price, data = training_data)
  prediccion <- predict(modelo, test_data, type = "class")
  resPrediccion <- table(test_data$owner, prediccion)
  matrix <- confusionMatrix(resPrediccion)
  precision <- matrix$overall[1]
  
  if (precisionOptima < precision){
    modeloOptimo    <- modelo
    precisionOptima <- precision
  }
}

precision <- paste0("Accuracy = ", round(100*precisionOptima, digits = 2), "%")
print(precision, quote = FALSE)
attrs <- names(modeloOptimo$variable.importance)
print("Attributes in descending order of relevance")

for (i in 1:length(attrs)) {
  print(paste0("  ", attrs[i]), quote = FALSE)
}

rpart.plot(modelo, 
           type = 2,
           extra = 102,
           tweak = 1.1,
           box.palette = "GnYlRd",
           shadow.col = "darkgray",
           main = "owner", 
           sub = precision)

rpart.rules(modelo, 
            style="wide", 
            cover = TRUE, 
            eq = "==", 
            when = "IF", 
            and = "&&", 
            extra = 4)


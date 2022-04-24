# Clear environment and console
rm(list=ls())
cat("\014")
graphics.off()
# Set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#Import the libraries needed to display the results
library(kableExtra)
library(magrittr)

# Include algorithm functions
source("../algorithms/blind/expand-node.R")
source("../algorithms/informed/hill-climbing-search.R")
source("../algorithms/informed/random-restart-hill-climbing.R")
source("../algorithms/informed/local-beam-search.R")

# Include functions for data analysis and result plot
source("../algorithms/results-analysis/analyze-results.R")

# Include the problem
source("../problem/p-hub-problem.R")

# Clear console
cat("\014")
graphics.off()

all_results  <- matrix(list(), nrow=7, ncol=10)
best_results <- vector(mode = "list", length = 7)

filename <- "../data/p-hub/AP40.txt"

problem <- initialize.problem(p = 4, filename = filename)

for(i in 1:10){
  #Hill Climbing Search
  all_results[[1,i]] <- hill.climbing.search(problem = problem)
  #Random Restart Hill Climbing
  all_results[[2,i]] <- random.restart.hill.climbing(filename, 4, 10)
  all_results[[3,i]] <- random.restart.hill.climbing(filename, 4, 20)
  all_results[[4,i]] <- random.restart.hill.climbing(filename, 4, 50)
  #Local Beam Search
  all_results[[5,i]] <- local.beam.search(problem, 3)
  all_results[[6,i]] <- local.beam.search(problem, 5)
  all_results[[7,i]] <- local.beam.search(problem, 10)
  
  if(i==10){ #Tarda mucho, es para saber cuando acaba y seguir ejecutando.
    print("Aqui termina.")
  }
}

for (i in 1:7) {
  results <- local.analyze.results(all_results[i,], problem)
  
  print(all_results[[i,1]]$name)
  print(paste0("Best evaluation: ", round(min(results$Evaluation), 2), 
               " - Mean: ", round(mean(results$Evaluation), 2), 
               " - SD: ", round(sd(results$Evaluation), 2)), quote = FALSE)
  print(paste0("Best runtime: ", round(min(results$Runtime), 2), 
               " - Mean: ", round(mean(results$Runtime), 2), 
               " - SD: ", round(sd(results$Runtime), 2)), quote = FALSE)
  print("---------------------------------------------------------------------")
  
  best_result_index <- match(min(results$Evaluation), results$Evaluation)
  best_results[[i]]   <- all_results[[i,best_result_index]]
}

# Print results in an HTML Table
results_df  <- local.analyze.results(best_results, problem)
kable_material(kbl(results_df),  c("striped", "hover", "condensed", "responsive"))

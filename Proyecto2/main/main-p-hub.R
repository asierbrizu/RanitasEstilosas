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

test <- 7

all_results  <- matrix(list(), nrow=test, ncol=10)
best_results <- vector(mode = "list", length = test)

file <- "../data/p-hub/AP50.txt"
p    <- 4

problem <- initialize.problem(p = p, filename = file)

for(i in 1:10){
  
  #Hill Climbing Search
  all_results[[1,i]] <- hill.climbing.search(problem = problem)
  
  #Random Restart Hill Climbing
  times              <- 10
  all_results[[2,i]] <- random.restart.hill.climbing(file, p, times)
  times              <- 20
  all_results[[3,i]] <- random.restart.hill.climbing(file, p, times)
  times              <- 50
  all_results[[4,i]] <- random.restart.hill.climbing(file, p, times)
  eae
  #Local Beam Search
  beams              <- 10
  all_results[[5,i]] <- local.beam.search(problem, beams)
  beams              <- 20
  all_results[[6,i]] <- local.beam.search(problem, beams)
  beams              <- 50
  all_results[[7,i]] <- local.beam.search(problem, beams)
  
  print(paste0(i, " loop"))
}

for (i in 1:test) {
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

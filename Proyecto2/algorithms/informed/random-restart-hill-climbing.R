random.restart.hill.climbing = function(file, p, iterations) {
  
  # Get Start time
  start_time       <- Sys.time()
  
  #Initialization of information for further analysis
  report <- data.frame(iteration = numeric())
  
  results <- vector(mode = "list", length = iterations)
  
  problem <- initialize.problem(p = p, filename = file)
  
  count <- 1
  while (count <= iterations) {
    results[[count]] <- hill.climbing.search(problem = problem)
    report <- rbind(report, data.frame(iteration = count))
    count <- count + 1
  }
  
  # Analyze results
  results_df        <- local.analyze.results(results, problem)
  best_result_index <- match(min(results_df$Evaluation), results_df$Evaluation)
  
  # Get runtime
  end_time <- Sys.time()
  
  result             <- list()
  result$name        <- paste0("Random Restart Hill Climbing")
  result$runtime     <- end_time - start_time
  result$state_final <- results[[best_result_index]]$state_final
  result$report      <- report
  
  return(result)
}
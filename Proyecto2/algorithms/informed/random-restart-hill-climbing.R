random.restart.hill.climbing = function(file, p, iterations) {
  inicio       <- Sys.time()
  resultados <- vector(mode = "list", length = iterations)
  problem <- initialize.problem(p = p, filename = file)
  count <- 1
  report <- data.frame(iteration = numeric())
  
  while (count <= iterations) {
    resultados[[count]] <- hill.climbing.search(problem = problem)
    report <- rbind(report, data.frame(iteration = count))
    count <- count + 1
  }
  
  results_df        <- local.analyze.results(resultados, problem)
  best_result_index <- match(min(results_df$Evaluation), results_df$Evaluation)
  resultado             <- list()
  resultado$name        <- paste0("Random Restart Hill Climbing")
  resultado$runtime     <- Sys.time() - inicio
  resultado$report      <- report
  resultado$state_final <- resultados[[best_result_index]]$state_final
  return(resultado)
}
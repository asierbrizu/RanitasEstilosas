local.beam.search = function(problem, beams, max_iterations = 10) {
  
  state_initial    <- problem$state_initial
  actions_possible <- problem$actions_possible
  
  inicio       <- Sys.time()
  nodes <- vector(mode = "list")
  nodes[[1]] <- list(parent = c(),
                       state = state_initial,
                       actions = c(),
                       depth = 1,
                       cost = get.cost(state = state_initial, problem = problem),
                       evaluation = get.evaluation(state_initial, problem))
  node_best <- nodes[[1]]
  report <- data.frame(iteration = numeric(),
                       nodes_frontier = numeric(),
                       depth_of_expanded = numeric(),
                       nodes_added_frontier = numeric())
  count <- 1
  
  while (count <= max_iterations) {
    sucessor_nodes <- local.expand.node(nodes[[1]], actions_possible, problem)
    sucessor_nodes <- sucessor_nodes[order(sapply(sucessor_nodes,function (x) x$evaluation))]
    node_best_successor <- sucessor_nodes[[1]]
    
    if (node_best_successor$evaluation <= node_best$evaluation) {
      node_best <- node_best_successor
    }
    
    nodes <- nodes[- 1]
    nodes <- c(nodes, sucessor_nodes)
    nodes <- nodes[order(sapply(nodes,function (x) x$evaluation))]
    
    if(length(nodes) > beams) nodes <- nodes[1:beams]
    report <- rbind(report, data.frame(iteration = count,
                                       nodes_frontier = length(nodes),
                                       depth_of_expanded = node_best$depth,
                                       nodes_added_frontier = length(nodes) - 1))
    count <- count + 1
  }
  
  resultado             <- list()
  resultado$name        <- paste0("Local Beam Search")
  resultado$report      <- report
  resultado$runtime     <- Sys.time() - inicio
  resultado$state_final <- node_best
  return(resultado)
}
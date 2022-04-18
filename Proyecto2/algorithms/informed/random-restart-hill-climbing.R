RRHC = function(problem, iterations, repetitions){
  result = list(method = "RRHC", final.state = c(),status = "",
                max.frontier = 0, max.depth = 0, iterations = 0)
 n = floor(runif(1, min=0, max=40));
  node = list(parent=c(), state=n, actions=c(), depth=0, cost=0,
              evaluation = get.evaluation(n, problem))
  frontier = list(node)
 
  count = 1
  count.limit = iterations

  while (TRUE){
    if (length(frontier)==0){result$status = "No nodes in the frontier. No solution possible.";break}
    if (count==count.limit){result$status = "Maximum number of iterations reached";break}
    
   
    firstnode = frontier[[1]]; 
    frontier[[1]] = NULL  

    if (is.final.state(firstnode$state,problem)){result$status="Solution Found";break}

    newnodes = expand.node(firstnode, problem) 
  
    newnodes = newnodes[order(sapply(newnodes,function (x) x$evaluation))]
    newnode = newnodes[[1]]
    
    print(glue("The current node is {firstnode$evaluation}, the best successor is {newnode$evaluation}"))
    if (firstnode$evaluation > newnode$evaluation){
      frontier = list(newnode)
    } else{
      result$status="Local Optimum Found";break
    }

    result$max.frontier = max(result$max.frontier,length(frontier))
    result$max.depth = max(result$max.depth,firstnode$depth)
    result$iterations = count

    count = count+1
  }

  result$final.state = firstnode
  return(result)
}

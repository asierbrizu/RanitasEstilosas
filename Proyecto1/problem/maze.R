# This function must return a list with the information needed to solve the problem.
initialize.problem <- function(file) {
  file2="feet-maze-1a.txt"
  print(file2)
  problem <- list()
  # Compulsory attributes
  problem$name              <- "Laberinto"
  problem$maze              <- read.table(file2,header=FALSE,sep="\n")
#  problem$maze              <- read.csv(file2,header=FALSE)
  print(problem$maze[1,])
  filas=strtoi(strsplit(problem$maze[1,],split=";")[1])
  print(problem$maze[1,])
  filas=strtoi(unlist(strsplit(problem$maze[1,],split=";"))[1])
  columnas=strtoi(unlist(strsplit(problem$maze[1,],split=";"))[2])
  filaInicio=strtoi(unlist(strsplit(problem$maze[filas+2,],split=";"))[1])
  colInicio=strtoi(unlist(strsplit(problem$maze[filas+2,],split=";"))[2])
  filaFin=strtoi(unlist(strsplit(problem$maze[filas+3,],split=";"))[1])
  colFin=strtoi(unlist(strsplit(problem$maze[filas+3,],split=";"))[2])
  laberinto=rep(NA,filas)
  for(i in filas){
    laberinto[i]<-strsplit(problem$maze[i+1],split=";")
    print(laberinto[i])
  }
  barIzda=strsplit(problem$maze[filas+4],split=";")
  barDrcha=strsplit(problem$maze[filas+5],split=";")
  barAbj=strsplit(problem$maze[filas+6],split=";")
  barArr=strsplit(problem$maze[filas+7],split=";")
  
  

  problem$state_initial     <- problem$maze[filas+2]
  problem$state_final       <- problem$maze[filas+3]

  
  # There are 4 actions that move: UP, DOWN, LEFT, RIGHT
  problem$actions_possible  <- data.frame(direction = c("Up", "Down", "Left", "Right"), stringsAsFactors = FALSE)
  
  
  return(problem)
}

# Analyzes if an action can be applied in a state.
# There is an IF for each action.
is.applicable <- function (state, action, problem) {
  print(problem$name)
  filaActual=strsplit(state,split=",")[1]
  columnaActual=strsplit(state,split=",")[2]
  if (action == "Up") {
    #Comprobar que no hay vacio
    if(filaActual==0){
      #PROBLEMA
    }else{
      #Comprobar que sean pies distintos
      if(laberinto[filaActual,columnaActual]==laberinto[filaActual-1,columnaActual]){
        #PROBLEMA
      }else{
        #Comprobar barreras
        barrerasIzda=strsplit(maze[filas+4],split=";")
        for(barrera in barrerasIzda){
          if(state==barrera){
            #PROBLEMON
          }
          split(maze[fila[2]+5],barrerasDrcha,drop=FALSE,sep=";",lex.order=FALSE)
          barrerasDrcha=strsplit(maze[filas+5],split=";")
          
            for(barrera in barrerasDrcha){
            
            if(paste(filaActual,columnaActual-1,sep=",")==barrera){####HAY QUE COMPROBARLO
              #PROBLEMON
            }
          }
        }
      }}}
  
  if (action == "Down") {
    return(row != problem$rows)
  }
  
  if (action == "Left") {
    return(col != 1)
  }
  
  if (action == "Right") {
    return(col != problem$columns)
  }
  
  return(FALSE)
}

# Returns the state resulting on applying the action over the state.
# There is an IF for each action.
effect = function (state, action, problem) {
  # Get the location of "blank" in the matrix
  where <- which(state == 0, arr.ind = TRUE)
  row <- where[1]
  col <- where[2]
  result <- state  
  
  if (action == "Up") {
    result[row-1, col] <- state[row, col]
    result[row, col]   <- state[row-1, col]
    return(result)
  }
  
  if (action == "Down") {
    result[row+1, col] <- state[row, col]
    result[row, col]   <- state[row+1, col]
    return(result)
  }
  
  if (action == "Left") {
    result[row, col-1] <- state[row, col]
    result[row, col]   <- state[row, col-1]
    return(result)
  }
  
  if (action == "Right") {
    result[row, col+1] <- state[row, col]
    result[row, col]   <- state[row, col+1]
    return(result)
  }
}

# Analyzes if a state is final or not
is.final.state <- function (state, final_state, problem) {
  return(sum(state == final_state) == (nrow(state) * ncol(state)))
}

# Transforms a state into a string
to.string <- function (state, problem) {
  for (i in 1:nrow(state)) {
    print(state[i, ])
  }
}

# Returns the cost of applying an action over a state
get.cost = function (action, state, problem) {
  return(1)
}

# Heuristic function used by Informed Search Algorithms
get.evaluation = function(state, problem) {
  return(sum(state != problem$state_final))
}


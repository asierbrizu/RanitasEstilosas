# This function must return a list with the information needed to solve the problem.
initialize.problem <- function(file) {
  problem <- list()
  # Compulsory attributes
  problem$name              <- "Laberinto"
  print(problem$name)
  problem$maze              <- read.table(file,header=FALSE,sep="\n")
  print(problem$maze[1,])
  problem$filas=strtoi(strsplit(problem$maze[1,],split=";")[1])
  print(problem$maze[1,])
  problem$filas=strtoi(unlist(strsplit(problem$maze[1,],split=";"))[1])
  problem$columnas=strtoi(unlist(strsplit(problem$maze[1,],split=";"))[2])
  problem$filaInicio=strtoi(unlist(strsplit(problem$maze[filas+2,],split=";"))[1])+1
  problem$colInicio=strtoi(unlist(strsplit(problem$maze[filas+2,],split=";"))[2])+1
  problem$filaFin=strtoi(unlist(strsplit(problem$maze[filas+3,],split=";"))[1])+1
  problem$colFin=strtoi(unlist(strsplit(problem$maze[filas+3,],split=";"))[2])+1
  problem$laberinto=rep(NA,filas)
  for(i in filas){
    problem$laberinto[i]<-strsplit(problem$maze[i+1,],split=";")
    print(problem$laberinto[i])
  }
  
  #Hay que terminar esto
  temp=unmap(strsplit(problem$maze[filas+4,],split=";"))
  for(actual in temp){
    problem$barIzda=paste(strtoi(temp[i,1])+1,strtoi(temp[i,2])+1,sep=",")
  }
  
  temp=unmap(strsplit(problem$maze[filas+5,],split=";"))
  problem$barDrcha=paste(strtoi(temp[1])+1,strtoi(temp[2])+1,sep=",")
  
  temp=unmap(strsplit(problem$maze[filas+6,],split=";"))
  problem$barAbj=paste(strtoi(temp[1])+1,strtoi(temp[2])+1,sep=",")
  
  temp=unmap(strsplit(problem$maze[filas+7,],split=";"))
  problem$barArr=paste(strtoi(temp[1])+1,strtoi(temp[2])+1,sep=",")
  
  

  problem$state_initial     <- problem$maze[filas+2,]
  problem$state_final       <- problem$maze[filas+3,]

  
  # There are 4 actions that move: UP, DOWN, LEFT, RIGHT
  problem$actions_possible  <- data.frame(direction = c("Up", "Down", "Left", "Right"), stringsAsFactors = FALSE)
  
  
  return(problem)
}

# Analyzes if an action can be applied in a state.
# There is an IF for each action.
is.applicable <- function (state, action, problem) {
  #Para probar
  #state="2,1"
  #action="Left"
  
  print(problem$name)
  filaActual=strtoi(unlist(strsplit(state,split=","))[1])
  columnaActual=strtoi(unlist(strsplit(state,split=","))[2])
  print("Empezamos")
  if (action == "Up") {
    print("Arriba")
    #Comprobar que no hay vacio
    if(filaActual==1){
      return(FALSE)
    }else{
      #Comprobar que sean pies distintos
      if(problem$laberinto[filaActual,columnaActual]==problem$laberinto[filaActual-1,columnaActual]){
        return(FALSE)
      }else{
        #Comprobar barreras
        for(barrera in problem$barArr){
          if(state==barrera){
            return(FALSE)
          }
          
            for(barrera in barAbj){
            
            if(paste(filaActual-1,columnaActual,sep=",")==barrera){####HAY QUE COMPROBARLO
              return(FALSE)
            }
          }
        }
      }}
    }
  
  if (action == "Down") {
    print("Abajo")
    #Comprobar que no hay vacio
    if(filaActual==problem$filas){
      return(FALSE)
    }else{
      #Comprobar que sean pies distintos
      if(problem$laberinto[filaActual,columnaActual]==problem$laberinto[filaActual+1,columnaActual]){
        return(FALSE)
      }else{
        #Comprobar barreras
        for(barrera in problem$barAbj){
          if(state==barrera){
            return(FALSE)
          }
          
          for(barrera in problem$barArr){
            
            if(paste(filaActual+1,columnaActual,sep=",")==barrera){####HAY QUE COMPROBARLO
              return(FALSE)
            }
          }
        }
      }}
  }
  
  if (action == "Left") {
    print("Izquierda")
    #Comprobar que no hay vacio
    if(columnaActual==1){
      return(FALSE)
    }else{
      #Comprobar que sean pies distintos
      if(problem$laberinto[filaActual,columnaActual]==problem$laberinto[filaActual,columnaActual-1]){
        return(FALSE)
      }else{
        #Comprobar barreras
        for(barrera in problem$barIzda){
          if(state==barrera){
            return(FALSE)
          }
          
          for(barrera in barDrcha){
            
            if(paste(filaActual,columnaActual-1,sep=",")==barrera){####HAY QUE COMPROBARLO
              return(FALSE)
            }
          }
        }
      }}
  }
  
  if (action == "Right") {
    print("Derecha")
    #Comprobar que no hay vacio
    if(columnaActual==problem$columnas){
      return(FALSE)
    }else{
      #Comprobar que sean pies distintos
      if(problem$laberinto[filaActual,columnaActual]==problem$laberinto[filaActual,columnaActual+1]){
        return(FALSE)
      }else{
        #Comprobar barreras
        for(barrera in barDrcha){
          if(state==barrera){
            return(FALSE)
          }
          
        for(barrera in barIzda){
            
            if(paste(filaActual,columnaActual+1,sep=",")==barrera){####HAY QUE COMPROBARLO
              return(FALSE)
            }
          }
        }
      }}
  }
  
  return(FALSE)
}

# Returns the state resulting on applying the action over the state.
# There is an IF for each action.
effect = function (state, action, problem) {
  
  filaActual=strtoi(unlist(strsplit(state,split=","))[1])
  columnaActual=strtoi(unlist(strsplit(state,split=","))[2])
  
  if (action == "Up") {
    result=paste(filaActual-1,columnaActual,sep=",")
    return(result)
  }
  
  if (action == "Down") {
    result=paste(filaActual+1,columnaActual,sep=",")
    return(result)
  }
  
  if (action == "Left") {
    result=paste(filaActual,columnaActual-1,sep=",")
    return(result)
  }
  
  if (action == "Right") {
    result=paste(filaActual,columnaActual+1,sep=",")
    return(result)
  }
}

# Analyzes if a state is final or not
is.final.state <- function (state, final_state, problem) {
  return(state == final_state) 
}

# Transforms a state into a string
to.string <- function (state, problem) {
  filaActual=strtoi(unlist(strsplit(state,split=","))[1])
  columnaActual=strtoi(unlist(strsplit(state,split=","))[2])
  
  print(paste("Fila actual: ",filaActual,", columna actual: ",columnaActual,"."))
}

# Returns the cost of applying an action over a state
get.cost = function (action, state, problem) {
  return(1)
}

# Heuristic function used by Informed Search Algorithms
get.evaluation = function(state, problem) {
  return(sum(state != problem$state_final))
}


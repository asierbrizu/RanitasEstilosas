# This function must return a list with the information needed to solve the problem.
initialize.problem <- function(file) {
  problem <- list()
  # Compulsory attributes
  problem$name              <- "Laberinto"
  problem$maze              <- read.table(file,header=FALSE,sep="\n")
  problem$filas=strtoi(unlist(strsplit(problem$maze[1,],split=";"))[2])
  problem$columnas=strtoi(unlist(strsplit(problem$maze[1,],split=";"))[1])
  problem$filaInicio=strtoi(unlist(strsplit(problem$maze[problem$filas+2,],split=","))[2])+1
  problem$colInicio=strtoi(unlist(strsplit(problem$maze[problem$filas+2,],split=","))[1])+1
  problem$filaFin=strtoi(unlist(strsplit(problem$maze[problem$filas+3,],split=","))[2])+1
  problem$colFin=strtoi(unlist(strsplit(problem$maze[problem$filas+3,],split=","))[1])+1
  problem$laberinto=rep(NA,problem$filas)
  #j=2
  contFilas=1
  for(i in problem$laberinto){
    problem$laberinto[contFilas]=rep(NA,problem$columnas)
    contCols=1
    filita<-unlist(strsplit(problem$maze[contFilas+1,],split=";"))
    
    for(j in problem$laberinto[i]){
    problem$laberinto[contFilas,contCols]=filita[contCols]   
     }
    
    #print(paste("Filita sin unlist ",filita))
    #filita=unlist(filita)
    #print(paste("Filita con unlist ",filita))
    #problem$laberinto[j-1]=filita
    #j=j+1 
    #print(paste("Mostrando laberinto antes de salir del for",problem$laberinto))
    
    
    
  }
  print(paste("Mostrando laberinto al final del for",problem$laberinto))
  
  #Barreras
  temp=unlist(strsplit(problem$maze[problem$filas+4,],split=";"))
  i=1
  for(actual in temp){
    problem$barIzda[i]=paste(strtoi(actual[2])+1,strtoi(actual[1])+1,sep=",")
    i=i+1
    }
  
  temp=unlist(strsplit(problem$maze[problem$filas+5,],split=";"))
  i=1
  for(actual in temp){
    problem$barDrcha[i]=paste(strtoi(actual[2])+1,strtoi(actual[1])+1,sep=",")
    i=i+1
  }
  
  temp=unlist(strsplit(problem$maze[problem$filas+6,],split=";"))
  i=1
  for(actual in temp){
    problem$barAbj[i]=paste(strtoi(actual[2])+1,strtoi(actual[1])+1,sep=",")
    i=i+1
  }
  
  temp=unlist(strsplit(problem$maze[problem$filas+7,],split=";"))
  i=1
  for(actual in temp){
    problem$barArr[i]=paste(strtoi(actual[2])+1,strtoi(actual[1])+1,sep=",")
    i=i+1
  }
  
  

  problem$state_initial     <- paste(problem$colInicio,problem$filaInicio,sep=",")
  problem$state_final       <- paste(problem$colFin,problem$filaFin,sep=",")

  
  # There are 4 actions that move: UP, DOWN, LEFT, RIGHT
  problem$actions_possible  <- data.frame(direction = c("Up", "Down", "Left", "Right"), stringsAsFactors = FALSE)
  
  print(paste("Mostrando laberinto al final del initialice",problem$laberinto))
  return(problem)
}

# Analyzes if an action can be applied in a state.
# There is an IF for each action.
is.applicable <- function (state, action, problem) {
  print(paste("Mostrando laberinto al principio del isAplicable",problem$laberinto))
  #Para probar
  #state="2,1"
  #action="Left"
  
  print(state)
  filaActual=strtoi(unlist(strsplit(state,split=","))[2])
  columnaActual=strtoi(unlist(strsplit(state,split=","))[1])
  print("Empezamos")
  if (action == "Up") {
    print("Arriba")
    #Comprobar que no hay vacio
    if(filaActual==1){
      return(FALSE)
    }else{
      #Comprobar que sean pies distintos
      print(paste("La fila actual es ",filaActual," la columna actual es ",columnaActual))
      print(problem$laberinto)
      print(problem$laberinto[filaActual,columnaActual])
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


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
  vacio=rep(NA,problem$filas*problem$columnas)
  problem$laberinto=matrix(c(vacio), nrow=problem$filas, ncol=problem$columnas, byrow=TRUE)
  for(contFilas in 1:problem$filas){
    filita<-unlist(strsplit(problem$maze[contFilas+1,],split=";"))
    for(contCols in 1:problem$columnas){
    #print(paste("Lo que le vamos a meter es: ",filita[contCols]," contFilas: ",contFilas," contCols: ",contCols))
    problem$laberinto[contFilas,contCols]<-filita[contCols]   
    }
    
    #print(paste("Laberinto[1,2]: ",problem$laberinto[1,2]))
    #print(paste("Laberinto[3,3]: ",problem$laberinto[3,3]))
    #print(paste("Laberinto[5,1]: ",problem$laberinto[5,1]))
    #print(paste("Laberinto[7,7]: ",problem$laberinto[7,7]))
    #print(paste("Laberinto[4,6]: ",problem$laberinto[4,6]))
    #print(paste("Laberinto entero: ", problem$laberinto))

  }

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
  
  #print(paste("Mostrando laberinto al final del initialice",problem$laberinto))
  return(problem)
}

# Analyzes if an action can be applied in a state.
# There is an IF for each action.
is.applicable <- function (state, action, problem) {
  #Para probar
  #state="2,1"
  #action="Left"
  
  filaActual=strtoi(unlist(strsplit(state,split=","))[2])
  columnaActual=strtoi(unlist(strsplit(state,split=","))[1])
  if (action == "Up") {
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
          
            for(barrera in problem$barAbj){
            
            if(paste(filaActual-1,columnaActual,sep=",")==barrera){####HAY QUE COMPROBARLO
              return(FALSE)
            }
          }
        }
      }}
    }
  
  if (action == "Down") {
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
          
          for(barrera in problem$barDrcha){
            
            if(paste(filaActual,columnaActual-1,sep=",")==barrera){####HAY QUE COMPROBARLO
              return(FALSE)
            }
          }
        }
      }}
  }
  
  if (action == "Right") {
    #Comprobar que no hay vacio
    if(columnaActual==problem$columnas){
      return(FALSE)
    }else{
      #Comprobar que sean pies distintos
      if(problem$laberinto[filaActual,columnaActual]==problem$laberinto[filaActual,columnaActual+1]){
        return(FALSE)
      }else{
        #Comprobar barreras
        for(barrera in problem$barDrcha){
          if(state==barrera){
            return(FALSE)
          }
          
        for(barrera in problem$barIzda){
            
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


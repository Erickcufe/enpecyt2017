enpecyt_translate_dicc <- function(encuesta, diccionario, cambio){

  diccionario <- diccionario[,c(1,5,6)]
  colnames(diccionario) <- c("columna","valor","respuesta")

  seleccion <- list()
  for (i in 1:nrow(diccionario)) {
    if(sum(!is.na(diccionario[i,]))==3){
      seleccion[[i]] <- i
    }
  }
  index <- unlist(seleccion)
  for (i in index){

    if(diccionario[i,1]==cambio){

      a <- which(index==i)
      b <- index[a+1]-1
      aswers <- diccionario[i:b,c(2,3)]
      print(aswers)
    }
  }
  aswers[,1] <- as.numeric(aswers[,1])
  aswers <- na.omit(aswers)
  sleep_2 <- encuesta[!is.na(encuesta[,cambio]), ]
  for(i in 1:nrow(sleep_2)){
    for(j in 1:nrow(aswers)){
      if(sleep_2[i,cambio]==as.numeric(aswers[j,1])){
        sleep_2[i,cambio] <- aswers[j,2]
        next
      } else {
        next
      }
    }
  }
  return(sleep_2)
}

#Robot detection function
detectarRobot <- function(unaCadena){
  listaBots <- c("googlebot",
                 "slurp",
                 "YandexBot",
                 "ia_archiver",
                 "bingbot",
                 "Baiduspider",
                 "yacybot",
                 "YodaoBot",
                 "Ezooms",
                 "MJ12bot",
                 "OpenindexSpider",
                 "Ask Jeeves",
                 "DuckDuckBot",
                 "msnbot",
                 "robot"
  )
  for(bot in listaBots){
    rgrep <- any(grep(bot, unaCadena, ignore.case = TRUE))
    if (rgrep == TRUE){
      return(1)
    }
  }
  return(0)
}

#Regex matches caputre function
matches<-function(x, m) {
  Map(function(u, mm) {
    so<-attr(mm,"capture.start")
    ml<-attr(mm,"capture.length")
    if (length(so) == 1L) {
      if (is.na(so) || (so == -1L)) 
        return(character())
    }
    substring(u, so, so + ml - 1L)
    
  }, x, m, USE.NAMES = FALSE)
}

#Log loading function
cargarTabla <- function(unArchivo){
  datos<-matrix(nrow=0,ncol=11)
  colnames(datos) <- c("session","ip","user","frank","date", "method", "resource", "status", "bytes", "referer", "userAgent")
  con  <- file(unArchivo, open = "r")
  contador <- 0
  while (length(linea <- readLines(con, n = 1, warn = FALSE)) > 0) {
    vector <- gregexpr('(\\S+)(, )?(\\S+)? \\S+ \\S+ \\[([^\\]]+)\\] "(\\w+)\\s([^"]*)" (\\d+)\\s?(\\d+)?\\s?("[^"]*")?\\s?("[^"]*")?', linea, perl=TRUE)
    resultado <- matches(linea, vector)
    ip <- resultado[[1]][1]
    user <- resultado[[1]][2]
    frank <- resultado[[1]][3]
    date <- resultado[[1]][4]
    method <- resultado[[1]][5]
    resource <- resultado[[1]][6]
    status <- resultado[[1]][7]
    bytes <- resultado[[1]][8]
    referer <- resultado[[1]][9]
    userAgent <- resultado[[1]][10]
    contador <- contador + 1
    print(paste("Processing line:", contador))
    if (!detectarRobot(linea)){
      datos <- rbind(datos, c(0, ip, user, frank, date, method, resource,
                              status, bytes, referer, userAgent
                              )
                    )
    } else 
    {
      print(paste("Robot found:", userAgent));
    }
  }
  close(con)
  return(datos)
}

#Session recognition function
identficarSesiones<-function(unaMatriz, unTiempo) {
  unaMatriz[order(unaMatriz[,"ip"], unaMatriz[,"userAgent"]),]
  for(i in 1:(nrow(unaMatriz)-1)) {
    fecha1 <- strptime(unaMatriz[i,5], "%d/%b/%Y:%H:%M:%S %z", tz="UTC")
    fecha2 <- strptime(unaMatriz[i+1,5], "%d/%b/%Y:%H:%M:%S %z", tz="UTC")
    
    #Custom resource timeout:
    #unTiempo <- determinarTimeout(unaMatriz[,"resource"])
    
    if(unaMatriz[i+1,2] == unaMatriz[i,2] & unaMatriz[i+1,11] == unaMatriz[i,11] & difftime(fecha2, fecha1, units="mins") < unTiempo)
    {      
      unaMatriz[i+1,1] <- unaMatriz[i,1]
    }
    else
    {
      unaMatriz[i+1,1] <- as.integer(unaMatriz[i,1]) + 1 
    }
  }
  return(unaMatriz)
}

#Custom resource timeout function
#determinarTimeout <- function(unRecurso){
#  
#  rgrep <- any(grep("/a/", unRecurso, ignore.case = TRUE))
#  if (rgrep == TRUE){
#    return(10) #10 minutos
#  }
#  
#  rgrep <- any(grep("/b/", unRecurso, ignore.case = TRUE))
#  if (rgrep == TRUE){
#    return(5) #5 minutos
#  }
#  
#  rgrep <- any(grep("/c/", unRecurso, ignore.case = TRUE))
#  if (rgrep == TRUE){
#    return(3) #5 minutos
#  }
#  
#  rgrep <- any(grep("/d/", unRecurso, ignore.case = TRUE))
#  if (rgrep == TRUE){
#    return(1)  #1 minuto
#  }
#  
#  return(8) #Tiemeout por defecto
#}

#Main function
sessionizer<-function(unArchivoEntrada, unArchivoSalida, timeout) {
  print(paste("Loading File:", unArchivoEntrada))
  matriz <- cargarTabla(unArchivoEntrada);
  print("Creating sessions ...")
  resultado <- identficarSesiones(matriz, timeout);
  print(paste("Writing file:", unArchivoSalida))
  write.table(resultado, unArchivoSalida, col.names = TRUE, row.names = FALSE, sep = " ", quote = FALSE)
  print("Finished!")
}

#Demo
archivoEntrada <- "D:/100.log"
archivoSalida <- "D:/output.txt"
timeout <- 5 #5 min timeout
sessionizer(archivoEntrada, archivoSalida, timeout)









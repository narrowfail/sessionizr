#Robot detection function
detectRobots <- function(someString){
  botsList <- c("googlebot", "slurp", "YandexBot", "ia_archiver", "bingbot",
                 "Baiduspider", "yacybot", "YodaoBot", "Ezooms", "MJ12bot",
                 "OpenindexSpider", "Ask Jeeves", "DuckDuckBot", "msnbot",
                 "robot")
  for(bot in botsList){
    rgrep <- any(grep(bot, someString, ignore.case = TRUE))
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
loadData <- function(fileIn){
  data <- matrix(nrow=0,ncol=11)
  colnames(data) <- c("session","ip","user","frank","date", "method", 
                       "resource", "status", "bytes", "referer", "userAgent")
  con <- file(fileIn, open = "r")
  counter <- 0
  while (length(line <- readLines(con, n = 1, warn = FALSE)) > 0) {
    vector <- gregexpr('(\\S+)(, )?(\\S+)? \\S+ \\S+ \\[([^\\]]+)\\] "(\\w+)\\s([^"]*)" (\\d+)\\s?(\\d+)?\\s?("[^"]*")?\\s?("[^"]*")?', line, perl=TRUE)
    result <- matches(line, vector)
    ip <- result[[1]][1]
    user <- result[[1]][2]
    frank <- result[[1]][3]
    date <- result[[1]][4]
    method <- result[[1]][5]
    resource <- result[[1]][6]
    status <- result[[1]][7]
    bytes <- result[[1]][8]
    referer <- result[[1]][9]
    userAgent <- result[[1]][10]
    counter <- counter + 1
    print(paste("Processing line:", counter))
    if (!detectRobots(line)){
      data <- rbind(data, c(0, ip, user, frank, date, method, resource,
                              status, bytes, referer, userAgent
                              )
                    )
    } else 
    {
      print(paste("Robot found:", userAgent));
    }
  }
  close(con)
  return(data)
}

#Session recognition function
findSessions<-function(data, timeout) {
  data[order(data[,"ip"], data[,"userAgent"]),]
  for(i in 1:(nrow(data)-1)) {
    date1 <- strptime(data[i,5], "%d/%b/%Y:%H:%M:%S %z", tz="UTC")
    date2 <- strptime(data[i+1,5], "%d/%b/%Y:%H:%M:%S %z", tz="UTC")
    
    #Custom resource timeout:
    #timeout <- determinarTimeout(data[,"resource"])
    
    if(data[i+1,2] == data[i,2] & data[i+1,11] == data[i,11]
       & difftime(date2, date1, units="mins") < timeout)
    {      
      data[i+1,1] <- data[i,1]
    }
    else
    {
      data[i+1,1] <- as.integer(data[i,1]) + 1 
    }
  }
  return(data)
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
sessionizer<-function(inputFile, outputFile, timeout) {
  print(paste("Loading File:", inputFile))
  matrix <- loadData(inputFile);
  print("Creating sessions ...")
  result <- findSessions(matrix, timeout);
  print(paste("Writing file:", outputFile))
  write.table(result, outputFile, col.names = TRUE, row.names = FALSE,
              sep = " ", quote = FALSE)
  print("Finished!")
}

#Demo
inputFile <- "D:/100.log"
outputFile <- "D:/output.txt"
timeout <- 5 #5 min timeout
sessionizer(inputFile, outputFile, timeout)

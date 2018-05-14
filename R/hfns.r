#assume no NA values
#moved to myscale.r
# myscale <- function(x){
#   (x - mean(x)) / sd(x)
# }

sanitize <- function(txt) {
  #list of bad chars from http://gavinmiller.io/2016/creating-a-secure-sanitization-function/
  #c('/', '+', '\\', '?', '%', '*', ':', '|', '"', '<', '>', '.', ' ')
  
  #convert this list to regex form
  badCharsRegex <- c('/', '\\+', '\\\\','\\?','%','\\*',':','\\|','\\"','<','>','\\.')
  #create a character class
  badCharClass <- paste(c('[',badCharsRegex,']'),collapse='')
  txt <- gsub(badCharClass,'_',txt) #replace all bad characters but space
  txt <- gsub('\\s','_',txt) #replace space (needs to be outside character class)
  return(gsub('__+','_',txt)) #replace repeated instances of _ with a single instance
}

writeDF <- function(df,prefix) {
  filename <- paste(prefix,"_",Sys.Date(),".csv",sep="")
  wd<-getwd() #store the cwd for housekeeping activity at the end - just so your script plays nice
  setwd (tempdir())
  write.csv(df, file=filename, row.names=FALSE)
  if(.Platform$OS.type=='unix') { #also returns "unix" for mac
    system(paste("open", filename))
  } else if(.Platform$OS.type=='windows') { #haven't tested this on windows
    shell.exec(filename) #opens the file in excel
  }
  
  setwd(wd) #return to original cwd when done
}
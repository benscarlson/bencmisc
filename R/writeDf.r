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

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

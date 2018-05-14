#' copied from rMOL/r/shared.r
makeQueryString <- function(paramList) {
  attribs <- vector(length=0)
  for(key in names(paramList)) {
    attribs <- append(attribs,paste(key,paramList[[key]],sep='='))
  }
  qs <- paste(attribs,collapse='&')
  return(qs)
}
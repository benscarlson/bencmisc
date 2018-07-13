#Usage:
# ptm <- proc.time()
# elapsedMinSec(ptm,proc.time())
# elapsedMinSec(ptm)
#' @export
elapsedMinSec <- function(start,end=proc.time()) {

  mins <- ((end - start)[3])/60 #decimal
  minPart <- floor(mins)
  names(minPart) <- 'minutes'
  secPart <- (mins - minPart)*60
  names(secPart) <- 'seconds'

  return(c(minPart,secPart))

}

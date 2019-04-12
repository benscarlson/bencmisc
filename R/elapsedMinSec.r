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

  el <- c(minPart,secPart)
  class(el) <- 'elapsedMinSec'

  return(el)

}

#' @export
format.elapsedMinSec <- function(x,...) {
  return(glue::glue('Elapsed time is {x[1]} min and {round(x[2])} sec'))
}

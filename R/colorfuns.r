#' Accpets rbg tuple string e.g. '165 239 210' and returns hex string '#A5EFD2'
#' @param x \code{string} vector of rgb tuples in the form 'r g b'.
#' @examples
#' tuple2hex(c("165 239 210", "111 45 93"))
#' @export
#'

tuple2hex <- function(x) {
  #See https://gist.github.com/mbannert/e9fcfa86de3b06068c83
  hex <- sapply(strsplit(x, " "), function(x) {
    rgb(x[1], x[2], x[3], maxColorValue=255)
  })

  return(hex)
}

#TODO: make these into function later when I need them.
# rgb2hex <- function(r,g,b) rgb(r, g, b, maxColorValue = 255)#rgb2hex(255,0,0). https://gist.github.com/mbannert/e9fcfa86de3b06068c83
#
# #turn colorname into hex. https://gist.github.com/mbannert/e9fcfa86de3b06068c83
# col2hex <- function(col, alpha) rgb(t(col2rgb(col)), alpha=alpha, maxColorValue=255)
# col2hex('firebrick') # [1] "#B22222"
# col2hex('firebrick', 204) # [1] "#B22222CC"

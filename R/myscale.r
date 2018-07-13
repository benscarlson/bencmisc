#assume no NA values
#' @export
myscale <- function(x){
  (x - mean(x)) / sd(x)
}

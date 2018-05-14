#assume no NA values
myscale <- function(x){
  (x - mean(x)) / sd(x)
}
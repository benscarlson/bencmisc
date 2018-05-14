moveToDf <- function(mv) {
  require(move)
  require(dplyr)
  
  df <- tibble(x=coordinates(mv)[,1],
               y=coordinates(mv)[,2],
               timestamp=timestamps(mv),
               short_name=tolower(as.character(mv@idData[1,1])))
  
  if(proj4string(mv)=='+proj=longlat +ellps=WGS84') {
    df <- df %>% rename(lon=x, lat=y)
  }
  
  return(df)
}


#testdat <- storks0 %>% group_by(short_name) %>% sample_n(3) %>% filter(short_name %in% c('Carolin','Elfie'))

#---- convert a dataframe containing a single animal to a move object ----
# requires lon, lat, timestamp, short_name fields
# assumes that points are in WGS84 format
#
dfToMove <- function(dat, animalName=NULL) {
  require(move)
  require(dplyr)
  
  if(is.null(animalName)) {
    stop('Supply animal name')
  }
  
  message('Assuming proj="+proj=longlat +ellps=WGS84"')
  #TODO: make sure data is in WGS84 format
  
  dat <- dat %>% arrange(timestamp) #move function requires this
  df <- data.frame(dat)
  
  # if(is.null(animalName)) {
  #   animalName = df$short_name #TODO: should be more general!
  #   #animalName = df$niche_group
  # }
  
  mv <- move(x=df$lon,y=df$lat,time=df$timestamp,
             proj=CRS("+proj=longlat +ellps=WGS84"),
             animal=rep(animalName,nrow(df)),
             data=df %>% dplyr::select(-c(lon,lat,timestamp)))
  
  return(mv)
}
#----

#---- convert dataframe with multiple individuals to a moveStack object ----
# requires short_name as the grouping variables
dfToMoveStack <- function(dat) {
  require(dplyr)
  require(purrr)
  require(tidyr)
  
  dat1 <- dat %>% 
    mutate(short_name2=short_name) %>% #hack to get short_name into the function call, since group_by removes it
    group_by(short_name2) %>% 
    nest() %>% 
    mutate(moveObj = data %>% map(dfToMove))
  
  return(moveStack(dat1$moveObj))
}
#----

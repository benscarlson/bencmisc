# Converts a dataframe with coordinate columns to a dataframe with target crs x,y values
# Can accept a dataframe with either lon/lat or x/y columns
# If fromCRS or toCRS not suplied, assumes WGS84
spTransDf <- function(dat, fromCRS=CRS("+proj=longlat +datum=WGS84"), toCRS=CRS("+proj=longlat +datum=WGS84")) {
  require(sp)
  require(raster)
  
  #TODO: at option: if converting to wgs84, change columns to lon/lat
  
  if(is.null(fromCRS)) {
    message('No fromCRS, assuming input is: +proj=longlat +datum=WGS84')
  }
  if(is.null(toCRS)) {
    message('No toCRS, assuming output should be: +proj=longlat +datum=WGS84')
  }
  
  #TODO: could somehow parse CRS to see if projected or geographic?
  if(all(c('lon','lat') %in% colnames(dat))) {
    dat <- dat %>% rename(x=lon,y=lat)
  }
  
  #TODO: exit if don't have x/y columns
  
  transDat <-  SpatialPointsDataFrame(
    coords=dplyr::select(dat,x,y),
    data=dplyr::select(dat,-x,-y),
    proj4string=fromCRS) %>%
    spTransform(CRSobj=toCRS)
  
  transDat2 <- as.data.frame(transDat) %>% 
    dplyr::select(x,y,everything()) %>%
    as_tibble()
  
  return(transDat2)
}
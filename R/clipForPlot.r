#polys is a polygon object to clip
#bounds is a bounding box (use bbox())
# clipForPlot <- function (polys,bounds,lonPad,latPad) {
#   if(FALSE) {polys<-statesPoly;bounds<-bbox(islandsPoly);lonPad<-0;latPad<-0}
#   #map background
#   require(dplyr)
#   require(PBSmapping)
#   require(rgdal)
  
#   polys_df <- fortify(polys)
  
#   #http://cameron.bracken.bz/finally-an-easy-way-to-fix-the-horizontal-lines-in-ggplot2-maps
#   pset <- polys_df %>% dplyr::rename(X=long,Y=lat,PID=group,POS=order)
#   pset$PID <- as.numeric(pset$PID)
#   xlim <- c(bounds['x','min']-lonPad-1, bounds['x','max']+lonPad+1)
#   ylim <- c(bounds['y','min']-latPad-1, bounds['x','max']+latPad+1)
#   polys_df_clip <- clipPolys(pset, xlim=xlim,ylim=ylim, keepExtra=TRUE)
#   polys_df_clip <- polys_df_clip %>% 
#     dplyr::rename(long=X,lat=Y,group=PID,order=POS) #rename back
#   return(polys_df_clip)
# }

#dat should have columns named lon, lat
clipForPlot <- function (wmap,dat,lonPad,latPad) {
  #map background
  suppressPackageStartupMessages(require(PBSmapping))
  suppressPackageStartupMessages(require(rgdal))
  
  wmap_df <- fortify(wmap)
  
  #http://cameron.bracken.bz/finally-an-easy-way-to-fix-the-horizontal-lines-in-ggplot2-maps
  pset <- wmap_df %>% rename(X=long,Y=lat,PID=group,POS=order)
  pset$PID <- as.numeric(pset$PID)
  xlim = c(min(dat$lon)-lonPad-1, max(dat$lon)+lonPad+1)
  ylim=c(min(dat$lat)-latPad-1, max(dat$lat)+latPad+1)
  wmap_df_clip = clipPolys(pset, xlim=xlim,ylim=ylim, keepExtra=TRUE)
  wmap_df_clip = wmap_df_clip %>% rename(long=X,lat=Y,group=PID,order=POS) #rename back
  return(wmap_df_clip)
}
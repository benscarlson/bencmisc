# library(dplyr)
# library(raster)
#
# randPts <- function(bb,num,crs) {
#
#   df <- tibble(
#     x=runif(num,bb@xmin,bb@xmax),
#     y=runif(num,bb@ymin,bb@ymax)
#   )
#
#   pts <- SpatialPoints(
#     coords=df,
#     proj4string=crs)
#
#   return(pts)
#
# }
#
# npts <- 1000
#
# r1 <- raster('~/projects/misc/utmtest/2018-07-16_utm32.tif') #original projection
#
# r1pts <- randPts(extent(r1),npts,crs(r1))
#
# r2 <- raster('~/projects/misc/utmtest/2018-07-16_utm33.tif') #RMSE: 38.85377
# #r2 <- raster('~/projects/misc/utmtest/2018-07-16_utm33_gdaltap.tif') #RSME: 37.97987
# #r2 <- raster('~/projects/misc/utmtest/2018-07-16_utm33_gdalnotap.tif') #RSME: 38.78215
#
#
# #plot(r1)
# #points(r1pts)
#
# vls <- list()
# vls$r1 <- extract(r1, r1pts)
#
# r2pts <- spTransform(r1pts,crs(r2))
#
# #plot(r2)
# #points(r2pts)
#
# vls$r2 <- extract(r2,r2pts)
#
# vls <- data.frame(vls)
# vls <- vls[complete.cases(vls),]
#
# err <- vls$r1 - vls$r2
# absdev <- abs(vls$r1 - vls$r2)
#
# hist(vls$r1, breaks=30)
# hist(vls$r2, breaks=30)
# hist(err,breaks=30)
# hist(absdev,breaks=30)
#
# apply(vls,2,summary)
# apply(vls,2,mean)
# length(err)
# length(err[err==0])
# #RSME
# sqrt(sum((err)^2)/(length(err)-1))

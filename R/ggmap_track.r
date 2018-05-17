#quick and dirty centroid function.
#does not take spherical coords into account.
centroidXYdf <- function(df) {
  require(dplyr)

  if('lon' %in% names(df) & 'lat' %in% colnames(df)) {
    df <- rename(df,x=lon,y=lat)
  }
  x <- (min(df$x) + max(df$x))/2
  y <- (min(df$y) + max(df$y))/2
  return(c(x=x,y=y))
}

getMapRetry <- function(centroid, zoom, maptype) {
  mp <- NULL
  attempt <- 0
  message('Getting map...')
  while( is.null(mp) && attempt <= 3 ) {
    attempt <- attempt + 1
    try(
      mp <- get_map(location = c(lon = centroid['x'], lat = centroid['y']),
                    zoom = zoom, maptype = maptype, scale = 2, messaging=FALSE)
    )
  }

  if(is.null(mp)) {
    stop(sprintf('Failed to get map after %s tries.',attempt))
  } else {
    message(sprintf('Got map on try #%s.',attempt))
  }

  return(mp) #should not reach here if we hit stop() condition above.
}

#----------
# Assumes lon, lat, timestamp columns
#TODO: catch and retry error:
## Error in download.file(url, destfile = tmp, quiet = !messaging, mode = "wb"):  cannot open URL ...
ggmapTrack <- function(gdat,centroid=NULL,zoom=NULL,indivName=NULL, maptype='satellite', drawPath=TRUE) {
  require(ggmap)
  #require(ggsn)
  require(lubridate)

  if(is.null(centroid)) {
    centroid=centroidXYdf(gdat)
  }

  if(is.null(zoom)) {
    zoom <- calc_zoom(make_bbox(lon,lat,data=gdat,f=0.2))-1
  }

  # gdat$behav <- factor(
  #   as.integer(gdat$behav),
  #   levels=c(2,3,4,5,6),
  #   labels=c('Flight (active)','Flight (passive)','Walking/Pecking','Standing/Preening','Sitting'))

  mp <- getMapRetry(centroid,zoom,maptype)

  # mp <- get_map(location = c(lon = centroid['x'], lat = centroid['y']), zoom = zoom,
  #               maptype = maptype, scale = 2, messaging=FALSE)

  title <- sprintf('Animal name: %s',ifelse(is.null(indivName),'<Unspecified>',indivName))
  st <- sprintf('%s to %s, zoom=%s, n=%s',
    lubridate::date(min(gdat$timestamp)),
    lubridate::date(max(gdat$timestamp)),
    zoom,
    formatC(nrow(gdat),big.mark=',',format='f',digits=0))

  p <- ggmap(mp) +
    labs(title=title, subtitle=st)

  if(drawPath) {
    p <- p + geom_path(data=gdat, aes(x = lon, y=lat))
  }

  #make sure to add geom_point on top of geom_path.
  p <- p + geom_point(data = gdat, size = 3, shape = 21,
                          aes(x = lon, y = lat, fill=timestamp))
  #Color by timestamp
  rng <- range(as.numeric(gdat$timestamp))

  if(diff(rng) != 0) { #can't color if there is no difference between timestamps
    #this is how ggplot internally figures out breaks
    #https://stackoverflow.com/questions/38486102/how-does-ggplot-calculate-its-default-breaks
    breaks <-labeling::extended(rng[1], rng[2], m = 5) #note won't display the first and last items
    labels <- as.Date(as.POSIXct(breaks,origin='1970-01-01',tz='UTC'))

    p <- p + scale_fill_gradient('Timestamp',low = "grey", high = "blue", breaks=breaks, labels=labels)
  } else {
    p <- p + guides(fill=FALSE)
  }

  #issue with adding scalebar is location is based on data extent, not on map
  # extent. might be able to fix, see: http://oswaldosantos.github.io/ggsn/
  #ggsn::scalebar(data=rename(gdat,long=lon,lat=lat),
  #  dist = 10, dd2km = TRUE, model = 'WGS84',location='bottomleft', st.size=3) +

  return(p)
}

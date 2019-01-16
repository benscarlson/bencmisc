#' quick and dirty centroid function.
#' does not take spherical coords into account.
#' @export
centroidXYdf <- function(df) {

  if('lon' %in% names(df) & 'lat' %in% colnames(df)) {
    df <- dplyr::rename(df,x=lon,y=lat)
  }
  x <- (min(df$x) + max(df$x))/2
  y <- (min(df$y) + max(df$y))/2
  return(c(x=x,y=y))
}

#' Gets a google map, retries if fails.
#'
#' @param centroid \code{numeric vector} A named vector with two elements (x and y)
#' @param zoom \code{integer}
#' @param maptype \code{string} Passed to ggmap maptype argument.
#' @return a ggmap object (a classed raster object with a bounding box attribute)
#' @examples
#' getMapRetry(c(lon=12,lat=45),11,'Satellite')
#' @export
#'
getMapRetry <- function(centroid, zoom, maptype) {
  #TODO: accept dataframe, use if centiod is not supplied
  # default or allow zoom to be null, then calculate zoom
  # default maptype

  mp <- NULL
  attempt <- 0

  if(!ggmap::has_goog_key()) {
    #key <- keyring::key_get('google_maps',keyring='api_keys')
    key <- stringr::str_replace_all(readr::read_file('~/.secrets/api_key_google_maps.txt'), "[\r\n]" , "")
    if(key=='') {
      #stop('Google api key was not found in google_maps keyring. Set it with key_set_with_value()')
      stop('Google maps api key was not found')
    }

    message('registering api key...')
    ggmap::register_google(key=key) #need to set this with key_set_with_value('gmaps_api',password=<key>)

    #bug in ggmap, need to specifically set signature to NA
    #see https://github.com/dkahle/ggmap/issues/205
    option <- getOption('ggmap')
    option$google$signature <- NA
    options(ggmap = option)

  }

  message('Getting map...')
  while( is.null(mp) && attempt <= 3 ) {
    attempt <- attempt + 1
    try(
      mp <- ggmap::get_map(location = c(lon = centroid['x'], lat = centroid['y']),
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
#TODO: should be able to remove these imports
#' @import ggplot2
#' @export
ggmapTrack <- function(gdat,centroid=NULL,zoom=NULL,indivName=NULL, maptype='satellite', drawPath=TRUE) {

  if(FALSE) {
    centroid <- c(x=12,y=52); zoom <- 10; maptype <- 'satellite'
  }

  if(is.null(centroid)) {
    centroid=centroidXYdf(gdat)
  }

  if(is.null(zoom)) {
    zoom <- ggmap::calc_zoom(make_bbox(lon,lat,data=gdat,f=0.2))-1
  }

  mp <- getMapRetry(centroid,zoom,maptype)

  title <- sprintf('Animal name: %s',ifelse(is.null(indivName),'<Unspecified>',indivName))

  st <- sprintf('%s to %s, zoom=%s, n=%s',
    lubridate::date(min(gdat$timestamp)),
    lubridate::date(max(gdat$timestamp)),
    zoom,
    formatC(nrow(gdat),big.mark=',',format='f',digits=0))

  p <- ggmap::ggmap(mp) +
    labs(title=title, subtitle=st)

  if(drawPath) {
    p <- p + geom_path(data=gdat, aes(x = lon, y=lat))
  }

  #make sure to add geom_point on top of geom_path.
  p <- p + geom_point(data = gdat, size = 2, shape = 21,
                          aes(x = lon, y = lat, fill=timestamp))
  #Color by timestamp
  rng <- range(as.numeric(gdat$timestamp))

  #can't figure out why, but this code is now giving error. worked earlier!
  # Error in as.POSIXct.numeric(value) : 'origin' must be supplied
  # Might have something to do with 'fill' above

  # if(diff(rng) != 0) { #can't color if there is no difference between timestamps
  #   #this is how ggplot internally figures out breaks
  #   #https://stackoverflow.com/questions/38486102/how-does-ggplot-calculate-its-default-breaks
  #   breaks <-labeling::extended(rng[1], rng[2], m = 5) #note won't display the first and last items
  #
  #   labels <- as.Date(as.POSIXct(breaks,origin='1970-01-01',tz='UTC'))
  #
  #   p <- p + scale_fill_gradient('Timestamp',low = "grey", high = "blue", breaks=breaks, labels=labels)
  #
  #   #taking out breaks and labels does not fix the problem
  #   #p <- p + scale_fill_gradient('Timestamp',low = "grey", high = "blue")
  # } else {
    p <- p + guides(fill=FALSE)
  # }

  #issue with adding scalebar is location is based on data extent, not on map
  # extent. might be able to fix, see: http://oswaldosantos.github.io/ggsn/
  #ggsn::scalebar(data=rename(gdat,long=lon,lat=lat),
  #  dist = 10, dd2km = TRUE, model = 'WGS84',location='bottomleft', st.size=3) +

  return(p)
}

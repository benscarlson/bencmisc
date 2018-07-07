kmlPoints <- function(dat,outFile) {

  kml_open(outFile, overwrite=TRUE) #check out: kml_visibility

  dat %>%
    group_by(short_name) %>%
    nest() %>% #makes list of data frames based on groups, store in 'data' column
    by_row(..f=function(row) {

      pts <- as.data.frame(row$data)
      coordinates(pts) <- ~ lon + lat
      proj4string(pts) <- CRS("+proj=longlat +datum=WGS84") #need to have +datum=WGS84 for kml_layer() to work

      #print(pts)
      kml_layer.SpatialPoints(
        obj=pts,
        subfolder.name=row$short_name,
        size=0.9,
        colour='red',
        #colour=color,
        #colour_scale=rep("#FFFF00", 2),  #LabelScale=0.6,\
        balloon=TRUE,
        shape='http://maps.google.com/mapfiles/kml/shapes/placemark_circle.png',
        points_names='',
        labels='')

      return(TRUE)
    })

  kml_close(outFile)
}

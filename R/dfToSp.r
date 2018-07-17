#accepts a df (for now with lon/lat columns)
#creates a SP object
#transforms coordinate system if requested.
# @import glue #shouldn't need this import anymore
# @import sp #shouldn't need this import anymore
#' @export
#'
dfToSp <- function(dat,proj="+proj=longlat +datum=WGS84 +no_defs",toProj=NULL) {
  pts <- dat
  sp::coordinates(pts) <- ~ lon + lat
  message('Assuming dataframe lon/lat are WGS84 coordinates.')
  sp::proj4string(pts) <- proj
  if(!is.null(toProj)) {
    message(glue::glue('Transforming coordinates to: {toProj}'))
    pts <- sp::spTransform(pts,toProj)
  }
  return(pts)
}

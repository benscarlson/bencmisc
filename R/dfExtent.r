#' accepts a dataframe (assuming lon/lat columns)
#' returns a list with values cooresponding to an extent object
#' @export
#'
dfExtent <- function(dat) {
  #assumes lon/lat columns

  #xMin, yMin, xMax, yMax
  bbox <- list(xMin=min(dat$lon),
               yMin=min(dat$lat),
               xMax=max(dat$lon),
               yMax=max(dat$lat))

  return(bbox)
}

#' accepts a dataframe (assuming lon/lat columns)
#' returns a string specifying a gee bounding box
#' @param digits \code{integer} Number of digits to round coordinates
#' @export
#'
geeExtentStr <- function(dat,digits=7) {
  bbox <- dfExtent(dat)

  bbox <- lapply(bbox,round,digits)
  #Example: [11.92218, 52.03254, 12.26966, 52.23553]
  return(as.character(glue::glue('[{bbox$xMin}, {bbox$yMin}, {bbox$xMax}, {bbox$yMax}]')))

}

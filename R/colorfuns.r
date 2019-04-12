#TODO: this file should have all function related to colors, palettes, colortables, etc.
# see files geePalette.r, legendToSLD.r, palettes.r

#TODO: for color tables, try to have a single unified csv format that can be translated to and
# from every other format.
#
# Start with fields:
# value - the numeric value of the class
# label - the name of the class
# color_rgb - a three digit tuple of rgb values

#' Creates a color table that can be used by gdaldem
#' Write the return to disk using:
#' write.table(ret,'myfile.txt', row.names=FALSE,quote=FALSE,col.names=FALSE)
#' @export
gdaldemColorTable <- function(dat) {
  #dat <- read_csv('/Users/benc/projects/gis-data/dfd_lulc/DFD-LULC_DE2014_subset_legend.csv')

  ret <- dat %>%
    mutate(rgb=gsub(' ', ',', color_rgb)) %>%
    mutate(line=as.character(glue('{value}: {rgb}'))) %>%
    select(line)

  return(ret)
}

#' Accpets rbg tuple string e.g. '165 239 210' and returns hex string '#A5EFD2'
#' @param x \code{string} vector of rgb tuples in the form 'r g b'.
#' @examples
#' tuple2hex(c("165 239 210", "111 45 93"))
#' @export
#'

tuple2hex <- function(x) {
  #See https://gist.github.com/mbannert/e9fcfa86de3b06068c83
  hex <- sapply(strsplit(x, " "), function(x) {
    rgb(x[1], x[2], x[3], maxColorValue=255)
  })

  return(hex)
}

#TODO: make these into function later when I need them.
# rgb2hex <- function(r,g,b) rgb(r, g, b, maxColorValue = 255)#rgb2hex(255,0,0). https://gist.github.com/mbannert/e9fcfa86de3b06068c83
#
# #turn colorname into hex. https://gist.github.com/mbannert/e9fcfa86de3b06068c83
# col2hex <- function(col, alpha) rgb(t(col2rgb(col)), alpha=alpha, maxColorValue=255)
# col2hex('firebrick') # [1] "#B22222"
# col2hex('firebrick', 204) # [1] "#B22222CC"

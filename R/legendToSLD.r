library(dplyr)
library(glue)
library(readr)

ColorMapEntry <- '<ColorMapEntry color="{color}" quantity="{value}" label="{label}"/>'

ColorMap <- function(cme) {
  return (
    c('<ColorMap type="intervals" extended="false">',
    cme,
    '</ColorMap>'))
}

RasterSymbolizer <- function(colorMap) {
  return (
    c('<RasterSymbolizer>',
      colorMap,
      '</RasterSymbolizer>'))
}


rgbToHex <- function(rgbvec, split=' ') {
  hexvec <- sapply(strsplit(rgbvec, split=split), function(x) {
    rgb(x[1],x[2],x[3], maxColorValue=255)
  })

  return(hexvec)
}

singlequote <- function(vals) {
#  names(vals) <- NULL
  return(sapply(vals,USE.NAMES=FALSE,function(val) {return(glue("\'{val}\'"))}))
}

#data frame with 'value', 'label', 'color'
#If colors are in Hex, set rgb=FALSE
#formatJS returns as a formatted javascript string. Can paste directly into GEE.
legendSLD <- function(legend,rgb=TRUE,formatJS=TRUE) {

  if(rgb) {
    legend <- legend %>%
      mutate(color=rgbToHex(color))
  }

  cme <- legend %>%
    glue_data(ColorMapEntry)

  cm <- ColorMap(cme)
  rs <- RasterSymbolizer(cm)

  if(formatJS) {
    cmeqte <- singlequote(rs)
    #sapply(cme[1],function(val) {return(glue("\'{val}\'"))})

    rs <- paste(singlequote(rs),collapse=' +\n')
  }

  return(rs)
}


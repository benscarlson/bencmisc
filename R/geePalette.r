library(clipr)
library(dplyr)
library(glue)
library(readr)
library(raster)

geePalette <- function(palDf) {
  lst <- apply(palDf,1,function(x) {glue("'{x['hex_col']}', //{x['value']} - {x['label']}")})
  lst <- paste(lst,collapse='\n')
  lst <- glue("var myPal = [\n{lst}\n];")
  message('adding to clipboard')
  write_clip(lst)
  return(lst)
}

#This is all code to get data into common format required to use geePalette
# Should clean this up and put it somewhere else

# #---- clc12 ----
# lcClasses <- read_delim('~/projects/whitestork/data/gis/g100_clc12_V18_5a/Legend/clc_legend.csv',delim=';',
#                         col_types=list(GRID_CODE=col_integer()))
# lcClasses[21,'LABEL3'] #Super long name
# lcClasses[21,'LABEL3'] <- 'Ag with natural vegetation'
# lcClasses <- lcClasses %>% mutate(RGB=ifelse(is.na(RGB),'255-255-255',RGB))
#
# rgb2hex <- function(r,g,b) rgb(r, g, b, maxColorValue = 255)
#
# lcClasses$hex_col <- sapply(strsplit(lcClasses$RGB, "-"), function(x) {rgb2hex(x[1], x[2], x[3])})
#
# View(lcClasses)
# range(lcClasses$GRID_CODE)
# lc2 <- lcClasses %>% select(value=GRID_CODE,label=LABEL3,hex_col) %>%
#   filter(value <= 44) #classes above these are just unclassified, and no data
#
# geePalette(lc2)
#
# #---- gl30
#
# #TODO: maybe this turns into a function called "attachColorTable". pass in a table of
# #   labels and a raster, and it returns the labeled color table
#
# gl30 <- raster('/Users/benc/projects/gis-data/globeland30/mosiac/gl30mosiac.tif')
# #gl30rat <- read_csv('~/projects/gis-data/globeland30/rat.csv')
# labs <- read_csv('~/projects/gis-data/globeland30/labels.csv')
#
# #didn't need this code
# # gl30 <- ratify(gl30)
# # rat <- levels(gl30)[[1]]
# # rat <- rat %>% left_join(gl30rat,by='ID')
#
# rast.ct <- colortable(gl30)
# rast.ct <- data.frame(ID=0:(length(rast.ct)-1),hex_col=rast.ct) #since R is 1-based, need to assign 0-based ID column
#
# ct <- labs %>% left_join(rast.ct,by='ID') %>%
#   rename(value=ID)
#
# geePalette(ct)

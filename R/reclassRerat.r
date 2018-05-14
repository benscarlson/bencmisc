

#reclass table should have columns:
# ID, ID_rcl, label_rcl, col_rcl
reclassRerat <- function(rast, rclTable) {
  if(FALSE) {rclTable <- read_csv('scratch/reclass3.csv') }
  
  layerName <- names(rast) #need to preserve layer name, reclassify() does not
  
  rcl <- rclTable %>%
    dplyr::select(ID,ID_rcl) %>%
    as.matrix()
  
  rastrcl <- reclassify(rast, rcl=rcl)
  
  #now ratify reclassified raster
  rastrcl <- ratify(rastrcl, count=TRUE)
  
  # Join in labels and colors from reclass table
  # first, remove duplicates from rclTable in order to 
  # add labels and colors to rat
  rclNodup <- rclTable %>%
    dplyr::select(ID=ID_rcl,label_rcl,col_rcl) %>%
    distinct(ID,label_rcl,col_rcl)
  
  rat <- as.data.frame(levels(rastrcl)) %>% 
    left_join(rclNodup,by='ID') %>%
    rename(label=label_rcl,col=col_rcl)
  
  levels(rastrcl) <- rat
  names(rastrcl) <- layerName
  
  return(rastrcl)
}
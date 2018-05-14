#Given a longitude, find the proper UTM zone
# https://stackoverflow.com/questions/9186496/determining-utm-zone-to-convert-from-longitude-latitude
lon2utm <- function(lon) {
  (floor((lon + 180)/6) %% 60) + 1
}
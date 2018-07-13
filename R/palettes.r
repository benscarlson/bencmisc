#Color palettes from Cory

#' @describeIn cm.cols1 basic color scheme
#' @export
cm.cols1 <- function(x,bias=1) {
  colorRampPalette(c('grey90','steelblue4','steelblue1','gold','red1','red4'),bias=bias)(x)
}

#' @describeIn cm.cols1 more highs and lows
#' @export
cm.cols1.1=function(x,bias=1) {
  colorRampPalette(c('grey90','grey90','grey90','grey90','grey60','grey60','grey60','steelblue4','steelblue1','gold','gold','red1','red1','red4','red4'),bias=bias)(x)
}

#' @describeIn cm.cols1 no grey
#' @export
cm.cols1.2=function(x,bias=1) {
  colorRampPalette(c('steelblue4','steelblue1','gold','red1','red4'),bias=bias)(x)
}

#' @describeIn cm.cols1 faded cm.cols1
#' @export
#for sd
cm.cols.sd=function(x,bias=1) {
  colorRampPalette(c('slategray3','slategray2','khaki','indianred1','indianred4'),bias=bias)(x)
}

#' @describeIn cm.cols1 for plotting differences
#' @export
# for differences
cm.cols.dif=function(x,bias=1){
  colorRampPalette(c('steelblue4','steelblue1','steelblue1', 'grey70', 'red1','red1','red4'))(x)
}

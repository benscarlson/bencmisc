#mostly based on this post:
# http://www.win-vector.com/blog/2014/05/trimming-the-fat-from-glm-models-in-r/
#' @export
slimGlm <- function(fit) {

  # just in case we forgot to set
  # y=FALSE and model=FALSE
  fit$y = c()
  fit$model = c()

  #fit$residuals = c() #required
  fit$fitted.values = c()
  fit$effects = c()
  #fit$qr = c()  #required
  #fit$qr$qr = c() #required
  #fit$linear.predictors = c() #required
  #fit$weights = c() #required
  fit$prior.weights = c()
  fit$data = c()
  fit$na.action = c()

  fit$family$variance = c()
  # fit$family$dev.resids = c() #required
  fit$family$aic = c()
  fit$family$validmu = c()
  fit$family$simulate = c()
  attr(fit$terms,".Environment") = c()
  attr(fit$formula,".Environment") = c()

  return(fit)
}

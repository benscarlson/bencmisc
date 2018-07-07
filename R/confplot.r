# Usage:
#   p <- confplot(fit) + ggtitle('Confidence Intervals', subtitle ='Mangus May 2013')
#   print(p)
#   ggsave(filename='figures/fit4.png',plot=p)

confplot <- function(fit,intercept=FALSE) {
  require(ggplot2)

  #confint fails for some reason, but confint.default works
  #confint uses the 'profile-likelihood' limits
  #confint.default uses Wald confidence limits
  #profile-likelihood is thought be be superior but there is no practical difference
  #https://www.r-bloggers.com/example-9-14-confidence-intervals-for-logistic-regression-models/

  #ci1 <- confint(fit, level = 0.95)[-1, ]
  ci1 <- confint.default(fit)
  est <- coef(fit)

  if(!intercept) {
    ci1 <- ci1[-1, ]
    est <- est[-1]
  }

  dat <- data.frame(var=rownames(ci1),
                    est = est,
                    lb=ci1[,1],
                    ub=ci1[,2])

  p<-ggplot(dat, aes(x=var, y=est)) +
    geom_point() +
    geom_errorbar(aes(ymin = lb, ymax = ub), width = 0.1) +
    geom_hline(yintercept = 0, lty = 2, color = "red") +
    #geom_hline(yintercept = 1, lty = 2, color = "red") +
    coord_flip() +
    theme_classic() +
    ylab('Coefficient') +
    xlab('Variable')

  return(p)
}


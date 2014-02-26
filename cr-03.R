#
#
#

# sampling distribution of sampling variance 
sample.variance <- rep(0, 10000)
for (i in 1:10000) {
  tmp <- rnorm(2)
  sample.variance[i] <- sd(tmp)^2
}

sample.variance <- data.frame(sample.variance)
names(sample.variance) <- 'var'
qplot(var, data=sample.variance, geom="density", xlim = c(0, 10),
      fill = as.factor(1), alpha = I(0.2))
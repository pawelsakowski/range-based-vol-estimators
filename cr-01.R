#
#
#


n       <- 100
max.i   <- 100000
r       <- rep(0, max.i)
r2      <- rep(0, max.i)
l       <- rep(0, max.i)
absr    <- rep(0, max.i)
open    <- rep(0, max.i)
high    <- rep(0, max.i)
low     <- rep(0, max.i)
close   <- rep(0, max.i)

for (i in 1:max.i) {
  x       <- cumsum(rnorm(n, mean = 0, sd = 0.1))
  x       <- exp(x)
  r2[i]   <- ((log(x[n]) - log(x[1])))^2
  r[i]    <- ((log(x[n]) - log(x[1])))
  absr[i] <- abs((log(x[n]) - log(x[1])))
  l[i]    <- log(max(x) / min(x))  
  open[i] <- log(x[1])
  high[i] <- log(max(x))
  low[i]  <- log(min(x))
  close[i]<- log(x[n])
}

# 1.
# klasyczny estymator wariancji
mean(r2/n)
hist(r2/n, 100)
sd1 <- sd(r2/n)
sd1

# 2.
# rangowy estymator wariancji
mean(l^2 * (1/(4*log(2))/n))
hist(l^2 * (1/(4*log(2))/n), 100)
sd2 <- sd(l^2 * (1/(4*log(2))/n))
sd2

# 3.
# estymator znaleziony w Osband "Rootless vol"
mean(((0.84 * l - 0.39 * absr)/sqrt(n))^2)
hist(((0.84 * l - 0.39 * absr)/sqrt(n))^2, 100)

depth_dist <- ggplot(diamonds, aes(depth))
depth_dist +
  geom_histogram(aes(y = ..density..), binwidth = 0.1)

sd3 <- sd(((0.84 * l - 0.39 * absr)/sqrt(n))^2)
sd3



# histogram
data <- c(r2/n,
l^2 * (1/(4*log(2))/n),
((0.84 * l - 0.39 * absr)/sqrt(n))^2
)

data <- data.frame(data)
names(data) <- 'x'
data$est <- 1
data$est[100001:200000] <- 2
data$est[200001:300000] <- 3
str(data)
library(ggplot2)
qplot(x, data=data, geom="density", xlim = c(0, 0.05),
      fill = as.factor(est), alpha = I(0.2))
head(data)
tail(data)


# 4.
# Garman-Klass
mean(0.5 * l^2 - 
      (2 * log(2) - 1) * r2
     )

mean((0.511 * l^2 - 
        0.19 * ((close - open) * (high + low - 2 * open) -
                  2 * ((high - open) * (low - open))
        ) -
        0.383 * r2)
)

# 5.
# Rogers-Satchell

# 6.
# Yang-Zhang



#######
sd1/sd2
sd1/sd3




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



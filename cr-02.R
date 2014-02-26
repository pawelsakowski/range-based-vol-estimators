#
#
#
library(ggplot2)
library(xts)
library(reshape)

rm(list=ls())

n         <- 10000
max.i     <- 10000
classic   <- rep(0, max.i)
parkinson <- rep(0, max.i)
osband    <- rep(0, max.i)
gk        <- rep(0, max.i)
rs        <- rep(0, max.i)


for (i in 1:max.i) {
  x            <- cumsum(rnorm(n, mean = 0.0000, sd = 0.001))
  x            <- exp(x)
  classic[i]   <- (log(x[n]) - log(x[1])) ^ 2 * 1 / n
  parkinson[i] <- (log(max(x) / min(x))) ^ 2 * (1 / (4 * log(2)) / n)
  osband[i]    <- ((0.84 * log(max(x) / min(x)) - 
                      0.39 * abs((log(x[n]) - log(x[1])))) / sqrt(n)) ^ 2
  gk[i]        <- (0.5 * (log(max(x) / min(x))) ^ 2 -
                     (2 * log(2) - 1) * log(x[n] / x[1]) ^ 2) / n
  rs[i]        <- (
    log(max(x) / x[1]) * 
    (log(max(x) / x[1] ) - log(x[n] / x[1])) +
    log(min(x) / x[1]) * 
    (log(min(x) / x[1] ) - log(x[n] / x[1]))
  ) / n
}

# 1.
# klasyczny estymator wariancji
mean(classic)
hist(classic, 100)
sd.classic <- sd(classic)
sd.classic

# 2.
# rangowy estymator wariancji
mean(parkinson)
hist(parkinson, 100)
sd.parkinson <- sd(parkinson)
sd.parkinson

# 3.
# estymator znaleziony w Osband "Rootless vol"
mean(osband)
hist(osband, 100)
sd.osband <- sd(osband)
sd.osband

# 4.
# estymator Garmana-Klassa (1980)
mean(gk)
hist(gk, 100)
sd.gk <- sd(gk)
sd.gk

# 5.
# estymator Rogersa-Stachella (1991)
mean(rs)
hist(rs, 100)
sd.rs <- sd(rs)
sd.rs


# efficiency
(sd.classic ^ 2 - sd.parkinson ^ 2) / sd.classic ^ 2
(sd.classic ^ 2 - sd.osband ^ 2)    / sd.classic ^ 2
(sd.classic ^ 2 - sd.gk ^ 2)        / sd.classic ^ 2
(sd.classic ^ 2 - sd.rs ^ 2)        / sd.classic ^ 2

sd.parkinson/sd.classic
sd.osband/sd.classic
sd.gk/sd.classic
sd.rs/sd.classic



mean(classic)
mean(parkinson)
mean(osband)
mean(gk)
mean(rs)

sd.classic
sd.parkinson
sd.osband
sd.gk
sd.rs


# densities
xmax <- 0.000005
ymax <- 1200000

data <- c(classic, parkinson, osband, gk, rs)
data <- data.frame(data)
str(data)
names(data) <- 'x'
data$estymator[(0 * max.i + 1) : (1 * max.i)] <- 1
data$estymator[(1 * max.i + 1) : (2 * max.i)] <- 2
data$estymator[(2 * max.i + 1) : (3 * max.i)] <- 3
data$estymator[(3 * max.i + 1) : (4 * max.i)] <- 4
data$estymator[(4 * max.i + 1) : (5 * max.i)] <- 5
data$estymator <- factor(data$estymator, levels = c(1:5), 
                   labels = c("classic        ",
                              "parkinson      ",
                              "osband         ",
                              "garman-klass   ",
                              "rogers-satchell"))

tmp.data <- data[as.numeric(data$estymator) %in% c(1),]
p <- qplot(x, data=tmp.data, geom="density", 
           xlim = c(0, xmax),
           ylim = c(0, ymax),
           fill = estymator, alpha = I(0.2)) +
  geom_vline(aes(xintercept = 0.000001))
#print(p)
ggsave(filename = "docs/moja.prezentacja/img/plot1.pdf", plot = p, 
       width = 12, height = 6)

tmp.data <- data[as.numeric(data$estymator) %in% c(1, 2),]
p <- qplot(x, data=tmp.data, geom="density", 
           xlim = c(0, xmax),
           ylim = c(0, ymax),
           fill = estymator, alpha = I(0.2)) +
  geom_vline(aes(xintercept = 0.000001))
#print(p)
ggsave(filename = "docs/moja.prezentacja/img/plot2.pdf", plot = p, 
       width = 12, height = 6)

tmp.data <- data[as.numeric(data$estymator) %in% c(1, 2, 3),]
p <- qplot(x, data=tmp.data, geom="density", 
           xlim = c(0, xmax),
           ylim = c(0, ymax),
           fill = estymator, alpha = I(0.2)) +
  geom_vline(aes(xintercept = 0.000001))
#print(p)
ggsave(filename = "docs/moja.prezentacja/img/plot3.pdf", plot = p, 
       width = 12, height = 6)



tmp.data <- data[as.numeric(data$estymator) %in% c(1, 2, 3, 4),]
p <- qplot(x, data=tmp.data, geom="density", 
           xlim = c(0, xmax),
           ylim = c(0, ymax),
           fill = estymator, alpha = I(0.2)) +
  geom_vline(aes(xintercept = 0.000001))
#print(p)
ggsave(filename = "docs/moja.prezentacja/img/plot4.pdf", plot = p, 
       width = 12, height = 6)


tmp.data <- data[as.numeric(data$estymator) %in% c(1, 2, 3, 4, 5),]
p <- qplot(x, data=tmp.data, geom="density", 
           xlim = c(0, xmax),
           ylim = c(0, ymax),
           fill = estymator, alpha = I(0.2)) +
  geom_vline(aes(xintercept = 0.000001))
#
print(p)
ggsave(filename = "docs/moja.prezentacja/img/plot5.pdf", plot = p, 
       width = 12, height = 6)


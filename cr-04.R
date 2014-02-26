
# plot of random walks
n <- 1000
x1 <- exp(cumsum(rnorm(n, mean = 0.00001, sd = 0.001)))
x2 <- exp(cumsum(rnorm(n, mean = 0.00001, sd = 0.001)))
x3 <- exp(cumsum(rnorm(n, mean = 0.00001, sd = 0.001)))
x4 <- exp(cumsum(rnorm(n, mean = 0.00001, sd = 0.001)))
x5 <- exp(cumsum(rnorm(n, mean = 0.00001, sd = 0.001)))
x6 <- exp(cumsum(rnorm(n, mean = 0.00001, sd = 0.001)))
x7 <- exp(cumsum(rnorm(n, mean = 0.00001, sd = 0.001)))
x8 <- exp(cumsum(rnorm(n, mean = 0.00001, sd = 0.001)))
x9 <- exp(cumsum(rnorm(n, mean = 0.00001, sd = 0.001)))
x10<- exp(cumsum(rnorm(n, mean = 0.00001, sd = 0.001)))

ymin <- 0.9
ymax <- 1.1

data <- data.frame(cbind(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10))
data$obs <- 1:1000

tmp.data <- data[, c(1:1,11)]
tmp.data  <- melt(tmp.data, 
                  id = "obs", 
                  measure = c("x1")
)
p <- qplot(obs, value, ylim = c(ymin, ymax), 
           data = tmp.data, 
           geom = c("line"),
           colour = variable) 
ggsave(filename = "docs/moja.prezentacja/img/rw1.pdf", plot = p, 
       width = 12, height = 6)

tmp.data <- data[, c(1:2,11)]
tmp.data  <- melt(tmp.data, 
                  id = "obs", 
                  measure = c("x1","x2")
)
p <- qplot(obs, value, ylim = c(ymin, ymax),
           data = tmp.data, 
           geom = c("line"),
           colour = variable) 
ggsave(filename = "docs/moja.prezentacja/img/rw2.pdf", plot = p, 
       width = 12, height = 6)

tmp.data <- data[, c(1:3,11)]
tmp.data  <- melt(tmp.data, 
                  id = "obs", 
                  measure = c("x1","x2","x3")
)
p <- qplot(obs, value, ylim = c(ymin, ymax),
           data = tmp.data, 
           geom = c("line"),
           colour = variable) 
ggsave(filename = "docs/moja.prezentacja/img/rw3.pdf", plot = p, 
       width = 12, height = 6)

tmp.data <- data[, c(1:4,11)]
tmp.data  <- melt(tmp.data, 
                  id = "obs", 
                  measure = c("x1","x2","x3", "x4")
)
p <- qplot(obs, value, ylim = c(ymin, ymax), 
           data = tmp.data, 
           geom = c("line"),
           colour = variable) 
ggsave(filename = "docs/moja.prezentacja/img/rw4.pdf", plot = p, 
       width = 12, height = 6)

tmp.data <- data[, c(1:5, 11)]
tmp.data  <- melt(tmp.data, 
                  id = "obs", 
                  measure = c("x1","x2","x3","x4","x5")
)
p <- qplot(obs, value, ylim = c(ymin, ymax), 
           data = tmp.data, 
           geom = c("line"),
           colour = variable) 
ggsave(filename = "docs/moja.prezentacja/img/rw5.pdf", plot = p, 
       width = 12, height = 6)


tmp.data <- data[, c(1:6, 11)]
tmp.data  <- melt(tmp.data, 
                  id = "obs", 
                  measure = c("x1","x2","x3","x4","x5", "x6")
)
p <- qplot(obs, value, ylim = c(ymin, ymax), 
           data = tmp.data, 
           geom = c("line"),
           colour = variable) 
ggsave(filename = "docs/moja.prezentacja/img/rw6.pdf", plot = p, 
       width = 12, height = 6)


tmp.data <- data[, c(1:7, 11)]
tmp.data  <- melt(tmp.data, 
                  id = "obs", 
                  measure = c("x1","x2","x3","x4","x5", "x6", "x7")
)
p <- qplot(obs, value, ylim = c(ymin, ymax),
           data = tmp.data, 
           geom = c("line"),
           colour = variable) 
ggsave(filename = "docs/moja.prezentacja/img/rw7.pdf", plot = p, 
       width = 12, height = 6)



tmp.data <- data[, c(1:8, 11)]
tmp.data  <- melt(tmp.data, 
                  id = "obs", 
                  measure = c("x1","x2","x3","x4","x5", "x6", "x7", "x8")
)
p <- qplot(obs, value, ylim = c(ymin, ymax), 
           data = tmp.data, 
           geom = c("line"),
           colour = variable) 
ggsave(filename = "docs/moja.prezentacja/img/rw8.pdf", plot = p, 
       width = 12, height = 6)



tmp.data <- data[, c(1:8, 11)]
tmp.data  <- melt(tmp.data, 
                  id = "obs", 
                  measure = c("x1","x2","x3","x4","x5", "x6", "x7", "x8")
)
p <- qplot(obs, value, ylim = c(ymin, ymax), 
           data = tmp.data, 
           geom = c("line"),
           colour = variable) 
ggsave(filename = "docs/moja.prezentacja/img/rw8.pdf", plot = p, 
       width = 12, height = 6)




tmp.data <- data[, c(1:8, 11)]
tmp.data  <- melt(tmp.data, 
                  id = "obs", 
                  measure = c("x1","x2","x3","x4","x5", "x6", "x7", "x8")
)
p <- qplot(obs, value, ylim = c(ymin, ymax), 
           data = tmp.data, 
           geom = c("line"),
           colour = variable) 
ggsave(filename = "docs/moja.prezentacja/img/rw8.pdf", plot = p, 
       width = 12, height = 6)




tmp.data <- data[, c(1:9, 11)]
tmp.data  <- melt(tmp.data, 
                  id = "obs", 
                  measure = c("x1","x2","x3","x4","x5", "x6", "x7", "x8", "x9")
)
p <- qplot(obs, value, ylim = c(ymin, ymax), 
           data = tmp.data, 
           geom = c("line"),
           colour = variable) 
ggsave(filename = "docs/moja.prezentacja/img/rw9.pdf", plot = p, 
       width = 12, height = 6)



tmp.data <- data[, c(1:10, 11)]
tmp.data  <- melt(tmp.data, 
                  id = "obs", 
                  measure = c("x1","x2","x3","x4","x5", 
                              "x6", "x7", "x8", "x9", "x10")
)
p <- qplot(obs, value, ylim = c(ymin, ymax), 
           data = tmp.data, 
           geom = c("line"),
           colour = variable) 
ggsave(filename = "docs/moja.prezentacja/img/rw10.pdf", plot = p, 
       width = 12, height = 6)

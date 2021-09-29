# normal sigma = 2
x = rnorm(10000, 0, 2)
sd(x)


# uniform sigma = 2
x2 = runif(100000, -sqrt(12), sqrt(12))
mean(x2)
sd(x2)


# mixed normal sigma = 2
library(LaplacesDemon)
sigma <- c(sqrt(3), sqrt(53))
p <- c(0.98, 0.02)
mu <- c(0,0)
s2 = rnormm(100000, p, mu, sigma)
mean(s2)
sd(s2)




##########
# normal sigma = 4
x = rnorm(10000, 0, 4)
sd(x)


# uniform sigma = 4
x2 = runif(100000, -sqrt(48), sqrt(48))
mean(x2)
sd(x2)


# mixed normal sigma = 4
library(LaplacesDemon)
sigma <- c(sqrt(12), sqrt(212))
p <- c(0.98, 0.02)
mu <- c(0,0)
s2 = rnormm(100000, p, mu, sigma)
mean(s2)
sd(s2)





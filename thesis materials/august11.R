### Other populations
install.packages("LaplacesDemon")
library(LaplacesDemon)

# Mix-normal distribution
# -> has outliers
sigma <- c(sqrt(0.75), sqrt(13.25))
p <- c(0.98, 0.02)
mu <- c(0,0)

n = 1000
x1 = rnormm(n, p, mu, sigma)
sd(x1)
mean(x1)

boxplot(x1)
hist(x1, breaks = 100)



# Uniform Distribution
n = 10000
s1 = runif(n, -sqrt(3), sqrt(3))
hist(s1)

mean(s1)
sd(s1)

# Uniform Distribution with different mean
n = 10000
s1 = runif(n, 10, 20)
hist(s1)
mean(s1)





# Mix-normal distribution again
# -> has outliers
sigma <- c(sqrt(0.75), sqrt(13.25))
p <- c(0.25, 0.75)
mu <- c(0,10)

n = 1000
x1 = rnormm(n, p, mu, sigma)
hist(x1, breaks = 25)

sd(x1)
mean(x1)



library(LaplacesDemon)

my.dist1 <- readline(prompt="Enter distribution 1: ")
#my.dist2 <- readline(prompt="Enter distribution 2: ")
#my.dist3 <- readline(prompt="Enter distribution 3: ")
my.stand1 <- readline(prompt="Enter standard deviation 1: ")
#my.stand2 <- readline(prompt="Enter standard deviation 2: ")
#my.stand3 <- readline(prompt="Enter standard deviation 3: ")
my.sample1 <- (readline(prompt="Enter sample size 1: "))
#my.sample2 <- readline(prompt="Enter sample size 2: ")
#my.sample3 <- readline(prompt="Enter sample size 3: ")

my.sample1 <- as.integer(my.sample1)


sig1 <- c(sqrt(0.75), sqrt(13.25))
p <- c(0.98, 0.02)
mu <- c(0,0)

sig2 <- c(sqrt(3), sqrt(53))
p <- c(0.98, 0.02)
mu <- c(0,0)
s2 = rnormm(100000, p, mu, sigma)
mean(s2)
sd(s2)

sig4 <- c(sqrt(12), sqrt(212))
p <- c(0.98, 0.02)
mu <- c(0,0)
s2 = rnormm(100000, p, mu, sigma)
mean(s2)
sd(s2)

unif.sig1 <- (-sqrt(3), sqrt(3))
if (my.dist1 == "uniform") {
  w.dist = runif
} else if (my.dist1 == "normal") {
  w.dist = rnorm 
} else if (my.dist1 == "mix") {
  w.dist = rnormm
}

if(my.stand1 == "1") {
  my.stand1 = unif.sig1
} else if (my.stand1 == "2") {
  my.stand1 = unif.sig2
} else if(my.stand1 == "4") {
  my.stand1 = unif.sig4
}

if (my.sample1 == 10) {
  n1 = my.sample1
} else if (my.sample1 == 20) {
  n1 = my.sample1
} else if (my.sample1 == 30) {
  n1 = my.sample1
}


unif.sig1 <- -sqrt(3), sqrt(3)
s1 <- w.dist(n1, my.stand1)
            
n1=10
n2=20
n3=30

seq_a = rep("a",n1)
seq_b = rep("b",n2)
seq_c = rep("c", n3)
seq_total = c(seq_a, seq_b, seq_c)
sequence <- factor(seq_total)
totalTrials <- 0
error <- 0

for(i in 1:10000) {
  s1 = stand1
  s2 = rnorm(n1,0,4)
  s3 = rnormm(n2,p,mu,sigma2)
  nums= c(s1, s2, s3)
  
  results = oneway.test(nums ~ sequence, var.equal = TRUE)
  pvalue = results[[3]]
  if(pvalue <= 0.05){
    error = error +1
  }
  totalTrials = totalTrials +1
}

error/totalTrials

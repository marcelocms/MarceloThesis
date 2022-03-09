# Uniform Distribution


# Uniform Distribution with different mean


seq_a = rep("a",10)
seq_b = rep("b",10)
seq_c = rep("c", 10)
seq_total = c(seq_a, seq_b, seq_c)
sequence <- factor(seq_total)
totalTrials <- 0
error <- 0


# Aug 18 Uniform Dist analysis

for(i in 1:10000) {
  s1 = runif(n, -sqrt(3), sqrt(3))
  s2 = runif(n, -sqrt(12), sqrt(12))
  s3 = runif(n, -sqrt(24), sqrt(24))
  nums= c(s1, s2, s3)
  norm_results1 = shapiro.test(s1)
  norm_results2 = shapiro.test(s2)
  norm_results3 = shapiro.test(s3)
  
  pvalue_s1 = norm_results1[[2]]
  pvalue_s2 = norm_results2[[2]]
  pvalue_s3 = norm_results3[[2]]
  p_min = min(pvalue_s1, pvalue_s2, pvalue_s3)
  if(p_min <= 0.05){
    results = kruskal.test(nums ~ sequence)
    pvalue = results[[3]]
  }
  else{
    results = oneway.test(nums ~ sequence, var.equal = TRUE)
    pvalue = results[[3]]
  }
  
  if(pvalue <= 0.05){
    error = error +1
  }
  totalTrials = totalTrials +1
}

error/totalTrials




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
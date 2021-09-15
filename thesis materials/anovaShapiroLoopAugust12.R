# Todo
#1) Make a for loop (1:1000) to automatically pick random samples and do the ANOVA test
#2) if statement to check the p-value
#3) count how many p-values are <= 0.05


n = 10
# Start for loop here
seq_a = rep("a",10)
seq_b = rep("b",10)
seq_c = rep("c", 10)
seq_total = c(seq_a, seq_b, seq_c)
sequence <- factor(seq_total)
totalTrials <- 0
error <- 0

for(i in 1:1000){
  s1 = rnorm(n, mean = 0, sd = 1)
  s2 = rnorm(n, mean = 0, sd = 1)
  s3 = rnorm(n, mean = 0, sd = 1)
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







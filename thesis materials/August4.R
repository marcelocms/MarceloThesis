# Todo
# 1) Add if-statement to check normality (Shapiro test)
# 2) If all samples are normal, use ANOVA
# 3) If one or more samples are non-normal, use Kruskal-Wallis test
# 4) Record number of times ANOVA was used and number of numbers KW test was used
# 5) Calculate the overall probability of a type 1 error 


n = 10
s1 = rnorm(n, mean = 0, sd = 1)
s2 = rnorm(n, mean = 0, sd = 1)
s3 = rnorm(n, mean = 0, sd = 1)

nums = c(s1, s2, s3)
groups = c('a','a','a','a','a','a','a','a','a','a','b','b','b','b','b','b','b','b','b','b','c','c','c','c','c','c','c','c','c','c')
groups = factor(groups)


norm_results1 = shapiro.test(s1)
norm_results2 = shapiro.test(s2)
norm_results3 = shapiro.test(s3)

norm_results1[[2]]
norm_results2[[2]]
norm_results3[[2]]

# Shapiro-Wilks Test 
# H0: The sample is from a normally distributed population
# H1: The sample is not from a normally distributed population


# Uniform distribution
u1 = runif(50, -sqrt(3), sqrt(3))
norm_results = shapiro.test(u1)
norm_results[[2]]




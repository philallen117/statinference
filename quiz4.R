# quiz4
# clear environment between questions

# q1
setwd("C:/Users/Phil/repos/statinference")
df1 <- read.table("q1.tsv", header = TRUE, sep="")
t.test(df1$End, df1$Baseline, paired=TRUE) # what it asks for
t.test(x=df1$End, y=df1$Baseline, paired=TRUE, alternative="less") # what wording suggests

# q2
# 95% interval, so need t-quantile 0.975 
n <- 9
t95 <- qt(0.975,n-1)
s <- 30
mbar <- 1100
round(mbar + c(-1,1) * t95 * s / sqrt(n), 0)

# q3
# one-sided exact. h0: prob of preferring coke = 0.5; h1: > 0.5
# fiddle number preferring coke by 1,
# because pbinom(v, lower.tail=FALSE) is P(variable strictly greater than v)
pbinom(3-1, size=4, prob=0.5, lower.tail = FALSE)

# q4
# Assume arrivals are Markov so number of arrivals ~ Poisson(lambda * t)
arr <- 10
t <- 1787 # days
#h0: benchmark rate 1 per 100 days
l0 <- 1/ 100
# p-value for 1-sided below = P(Arr <= arr; h0)
ppois(10, lambda = t * l0, lower.tail = TRUE)

# q5
# t(reatment) and c(control placebo) - 9 participants each, assuming common variance
nn <-9
m_gain_t <- -3
m_gain_c <- 1
sd_gain_t <- 1.5
sd_gain_c <- 1.8
pooled_se <- sqrt((sd_gain_t^2 + sd_gain_c^2)/2) #weights cancel for equal group sizes
# 99% interval
m_gain_t - m_gain_c + c(-1,1) * qt(0.995, df=2*nn-2) * pooled_se * sqrt(2/nn)
# does not contain 0
# so p < 0.01 (two-sided)

# q6
ci <- c(1027,1123)
xbar <- mean(ci)
halfCIwidth <- (1123 - 1027) / 2
mu0 <- 1028 # (just within half an interval of xbar)
# non-rejection region of mu0
mu0 + c(-1,1)* halfCIwidth
# of course, that includes xbar (just within half an interval of mu0)

# q7 - this is the question from homework, but solving for power instead of n; here, n = 100
sd <- 0.4 # given by magic
# power = P(Xbar > Xcrit ; X ~ N(0.1, 0.4))
xcrit <- sd * qnorm(0.95) / sqrt(100) # one-sided 5% test
# X ~ N(0.1, 0.4) => Xbar ~ N(0.1, 0.4/sqrt(100))
power <- pnorm(xcrit, mean=0.1, sd=sd/sqrt(100), lower.tail=FALSE)
# Simplify to standard normal to show "effect size" (xcrit-mu1)/sd. Here mu1 = 0.1
power <- pnorm(sqrt(100)*(xcrit-0.1)/sd, lower.tail=FALSE)

# q8 - this is the homework, solving for n
sd <- 0.4 # given by magic
mu0 <- 0.0
mu1 < 0.1
zalpha <- qnorm(0.05) # one-sided, mu1 > mu0
zpower <- qnorm(0.9) # target 90%
# power = P(Xbar > Xcrit ; X ~ N(0.1, sd))
# xcrit given by alpha:
# xcrit = mu0 - sd * zalpha / sqrt(n) # one-sided 5% test, note that zalpha < 0
# X ~ N(0.1, 0.4) => Xbar ~ N(mu1, sd/sqrt(n)) => (Xbar - mu1) * sqrt(n) / sd ~ N0.1)
# So power = 1- pnorm(sqrt(n)*(xcrit - mu1)/sd)
# So 1 - power = pnorm(...)
# Apply quantile function both sides
# qnorm(1-power) = sqrt(n) * (xcrit - mu1) / sd
# By symmetry of N(0,1) and negating, zpower = sqrt(n) * (mu1 - xcrit) / sd
# Substituting and taking zalpha out, zpower = sqrt(n) * (mu1 - mu0) / sd + zalpha
# So sqrt(n) = (zpower - zalpha) * (sd / (mu1 - mu0))
# So we need n at least
((0.4/0.1) * (zpower - zalpha))^2

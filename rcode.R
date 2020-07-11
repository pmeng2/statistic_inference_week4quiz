## 1. A pharmaceutical company is interested in testing a potential blood 
## pressure lowering medication. Their first examination considers only subjects
## that received the medication at baseline then two weeks later. The data are 
## as follows (SBP in mmHg)
## Consider testing the hypothesis that there was a mean reduction in blood 
## pressure? Give the P-value for the associated two sided T test.
## (Hint, consider that the observations are paired.)

base <- c(140, 138, 150, 148, 135)
week2 <- c(132, 135, 151, 146, 130)
t.test(base, week2, alternative = "two.sided", paired = TRUE)$p.value


## 2. A sample of 9 men yielded a sample average brain volume of 1,100cc and a 
## standard deviation of 30cc. What is the complete set of values of μ0 that a 
## test of H_0:μ = μ0 H would fail to reject the null hypothesis in a two sided 
## 5% Students t-test?

mean1 <- 1100
sd1 <- 30
n1 <- 9
mean1 +c(-1,1)*qt(.975,n1-1)*sd1/sqrt(n1)


## 3. Researchers conducted a blind taste test of Coke versus Pepsi. Each of 
## four people was asked which of two blinded drinks given in random order that 
## they preferred. The data was such that 3 of the 4 people chose Coke. Assuming 
## that this sample is representative, report a P-value for a test of the
## hypothesis that Coke is preferred to Pepsi using a one sided exact test.

pbinom(2, size=4, prob=0.5, lower.tail=FALSE)


## 4. Infection rates at a hospital above 1 infection per 100 person days at 
## risk are believed to be too high and are used as a benchmark. A hospital that
## had previously been above the benchmark recently had 10 infections over the
## last 1,787 person days at risk. About what is the one sided P-value for the 
## relevant test of whether the hospital is *below* the standard?

lamda1 <- 1/100
t <- 1787
ppois(10, lamda1*t)


## 5. Suppose that 18 obese subjects were randomized, 9 each, to a new diet pill 
## and a placebo. Subjects’ body mass indices (BMIs) were measured at a baseline 
## and again after having received the treatment or placebo for four weeks. The 
## average difference from follow-up to the baseline (followup - baseline) was
## −3 kg/m2 for the treated group and 1 kg/m2 for the placebo group. The 
## corresponding standard deviations of the differences was 1.5 kg/m2 for the 
## treatment group and 1.8 kg/m2 for the placebo group. Does the change in BMI 
## appear to differ between the treated and placebo groups? Assuming normality 
## of the underlying data and a common population variance, give a pvalue for a 
## two sided t test.

t <- -3
sdt <- 1.5
nt <- 9
p <- 1
sdp <- 1.8
np <- 9
df1 <- nt + np -2
sp1 <- sqrt(((nt-1)*sdt^2+(np-1)*sdp^2)/df1)
t1 <- (t-p)/(sp1*sqrt(1/nt+1/np))
2*pt(t1, df1)


## 6. Brain volumes for 9 men yielded a 90% confidence interval of 1,077 cc to 
## 1,123 cc. Would you reject in a two sided 5% hypothesis test of
## Ho : μ = 1078

## 7. Researchers would like to conduct a study of 100 healthy adults to 
## detect a four year mean brain volume loss of 0.01 mm^3.  Assume that the 
## standard deviation of four year volume loss in this population is .04mm^3.
## About what would be the power of the study for a 5% one sided test versus 
## a null hypothesis of no volume loss?

mean7 <- 0.01
sd7 <- 0.04
n7 <- 100
power.t.test(n7, mean7, sd7, type="one.sample", alternative = "one.sided")$power

## 8. Researchers would like to conduct a study of n healthy adults to detect 
## a four year mean brain volume loss of .01~mm^3.Assume that the standard 
## deviation of four year volume loss in this population is .04~mm^3. About what 
## would be the value of n needed for 90% power of type one error rate of 5% 
## one sided test versus a null hypothesis of no volume loss?
power.t.test(delta = 0.01, sd = 0.04, type = "one.sample", power = 0.9, alternative = "one.sided")$n
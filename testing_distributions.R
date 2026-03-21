library(tidyr)
library(readxl)
library(stringr)
library(purrr)

data <- read.csv("Data_File_2818482.csv", header = FALSE, sep = "\t")

# Clean up the nonsense format of the file
ncols <- ncol(data)

data2 <- data.frame("interarrival_times" = unlist(data[1,2:ncols]),
                    "service_times_for_initial_phase" = unlist(data[2, 2:ncols]),
                    "service_times_for_placing_keyboard_and_mouse" = unlist(data[3, 2:ncols]),
                    "service_times_for_assembling_the_case" = unlist(data[4, 2:ncols])) %>% subset(complete.cases(.))
  

# starting with interarrival times...
iat <- data2$interarrival_times
iat_hist <- hist(iat, breaks = "FD")
# looks roughly exponential, with what seems like an outlier
# MLE would be n/sum(iat)
n_iat <- length(iat)
sum_iat <- sum(iat)
mle_iat <- n_iat/sum_iat
# test if the interarrival times follow an exponential(mle_iat) distribution
iat_test <- ks.test(iat, "pexp", rate=mle_iat, alternative = "two.sided")
# will need to correct the test statistic given we've estimated the distribution parameter from the data
# test stat will have a 95% sig level critical value of 1.094
iat_test_stat_corrected <- (iat_test$statistic - (0.2/n_iat))*(sqrt(n_iat) + 0.26 + (0.5/(sqrt(n_iat))))
# giving a corrected test statistics of 0.513 - Therefore, we can conclude that the interarrival times follow
# an exponential distribution with rate parameter =  0.138428

# looking at service times for initial phase
stfip <- data2$service_times_for_initial_phase
stfip_hist <- hist(stfip, breaks = "FD")
# not entirely clear what this is - possibly exponential, possible lognormal
#
# trying an exponential distribution
n_stfip <- length(stfip)
sum_stfip <- sum(stfip)
mle_exponential_stfip <- n_stfip / sum_stfip
# testing to see if data fits an exponential(mle_exponential_stfip) distribution
# again will need to correct the test statistic
stfip_exponential_test <- ks.test(stfip, "pexp", rate = mle_exponential_stfip, alternative = "two.sided")
# again, our critical value 95% sig level is 1.094
stfip_exponential_test_stat_corrected <- (stfip_exponential_test$statistic - (0.2/n_stfip))*(sqrt(n_stfip) + 0.26 + (0.5/(sqrt(n_stfip))))
# giving a test stat of 0.585.
# Therefore, we can conclude that the service times for the initial phase follow an exponential distribution 
# with rate parameter 0.2952633


# looking at service times for placing the keyboard and mouse
stfpkam <- data2$service_times_for_placing_keyboard_and_mouse
stfpkam_hist <- hist(stfpkam, breaks = "FD")
# this looks like a mix of rv's
# take the max and min and add more bins to the histogram
stfpkam_min <- min(stfpkam)
#
stfpkam_max <- max(stfpkam)
#
#
stfpkam_breaks <- seq(4.4, 9.3, 0.1)
#
stfpkam_hist2 <- hist(stfpkam, breaks = stfpkam_breaks)
#
# 1. looks very roughly, like there's a uniform distribution for x <= 6.7
# 2. another uniform distrubiton for 6.7 < x <= 7.1
# 3. a third uniform distribution for x > 7.1
#
# examining these in turn...
# 1.
stfpkam_dist1 <- stfpkam[stfpkam <= 6.7]
#
# have 91 obs, so if we want to keep the expected values above 5
# will want a max of 18 bins
#
stfpkam_dist1_hist <- hist(stfpkam_dist1, breaks = seq(4.4, 6.7, (6.7-4.4)/18))
#
# running a chi-square test...
stfpkam_dist1_expected <- length(stfpkam_dist1) * (1/18)
#
stfpkam_dist1_test <- sum((stfpkam_dist1_hist$counts - stfpkam_dist1_expected)**2 / stfpkam_dist1_expected)
#
# test will have 18 - 3 = 15 degrees of freedom, given we've estimated 2 parameters from the distribution
#
pchisq(stfpkam_dist1_test, 15, lower.tail = FALSE)
#
# which is clearly non-significant - so we look ok for this bit
#
# 2.
stfpkam_dist2 <- stfpkam[stfpkam > 6.7 & stfpkam <= 7.1]
# have 33 observations here, so will only manage 6 bins
#
stfpkam_dist2_hist <- hist(stfpkam_dist2, breaks = seq(6.7, 7.1, (7.1-6.7)/6))
#
# running a chi-square test...
stfpkam_dist2_expected <- length(stfpkam_dist2) * (1/6)
#
stfpkam_dist2_test <- sum((stfpkam_dist2_hist$counts - stfpkam_dist2_expected)**2 / stfpkam_dist2_expected)
#
# out test here would have 3 degrees of freedom, 6 bins, estimated two parameters.
pchisq(stfpkam_dist2_test, 3, lower.tail=FALSE)
# which is clearly non-significant - so we look ok here!

# 3.
 stfpkam_dist3 <- stfpkam[stfpkam > 7.1]
# 76 obs - max of 15 bins
 stfpkam_dist3_hist <- hist(stfpkam_dist3, breaks = seq(7.1, max(stfpkam_dist3), (max(stfpkam_dist3)-7.1)/15))
 #
 # running a chi-square test...
 #
 stfpkam_dist3_expected <- length(stfpkam_dist3) * (1/15)
 #
 stfpkam_dist3_test <- sum((stfpkam_dist3_hist$counts - stfpkam_dist3_expected)**2 / stfpkam_dist3_expected)
 #
 # test will have 12 degrees of freedom; 15 bins, two parameters estimated.
 pchisq(stfpkam_dist3_test, 12, lower.tail=FALSE)
 # 
 # again non-significant, so we look ok here!
 
# looking at service times for assembling the case
 
stfatc <- data2$service_times_for_assembling_the_case
stfatc_hist <- hist(stfatc, breaks = "FD")
# looks logNormal

log_stfatc <- log(stfatc)
log_stfatc_hist <- hist(log_stfatc, breaks = "FD")
#
# critical value at the 95% sig level for the KS test here will be 0.895
log_stfatc_test <- ks.test(log_stfatc, "pnorm", mean = mean(log_stfatc), sd = sd(log_stfatc), alternative = "two.sided")
#
n <- length(log_stfatc)
#
log_stfatc_corrected_test <- (sqrt(n) - 0.01 + (0.85 / sqrt(n)))*log_stfatc_test$statistic
#
# yielding a correct test statistic of 0.622 - Therefore we can conclude the data comes from a log normal distribution
mean_log_stfatc = mean(log_stfatc)
#
var_log_stfatc = var(log_stfatc)
#

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
# this looks roughly as if it could be uniformly distributed
stfpkam_min <- min(stfpkam)
#
stfpkam_max <- max(stfpkam)
#
#
stfpkam_breaks <- seq(4.4, 9.3, (9.3-4.4)/40)
#
stfpkam_hist2 <- hist(stfpkam, breaks = stfpkam_breaks)

stfpkam_expected <- length(stfpkam) * (1/40)

stfpkam_test <- sum((stfpkam_hist2$counts - stfpkam_expected)**2 / stfpkam_expected)

# Chi square test will have 40-3 = 37 degrees of freedom

pchisq(stfpkam_test, 37, lower.tail = FALSE)

# which is clearly non-significant, so we look ok for this bit!

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

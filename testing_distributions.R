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
# this looks roughly uniformly distributed - albeit very roughly
# mle's here would be the min and max values
stfpkam_mle <- c(min(stfpkam), max(stfpkam))
# will need to bin and Chi-Square test here.
# will take the bins from the histogram
stfpkam_breaks <- stfpkam_hist$breaks
# bins are 0.5 apart - will try something more granular
stfpkam_breaks2 <- seq(4, 9.5, 0.15)
# get probs for expected values, assuming data comes from a uniform(4, 9.5) distribution
stfpkam_probs <- punif(stfpkam_breaks2, min = 4, max = 9.5)
#want the probability of a value falling within each bin
stfpkam_probs2 <- stfpkam_probs[2:length(stfpkam_probs)]
stfpkam_probs3 <- stfpkam_probs[1:length(stfpkam_probs)-1]
stfpkam_probs4 <- stfpkam_probs2 - stfpkam_probs3
#
stfpkam_hist2 <- hist(stfpkam, breaks = stfpkam_breaks2)
# get observed vals in each bin
stfpkam_observed <- stfpkam_hist2$counts
# expected vals
# all above 5
stfpkam_expected <- length(stfpkam) * stfpkam_probs4
#
stfpkam_test_stat <- sum((stfpkam_observed - stfpkam_expected)**2 / stfpkam_expected)
# degrees of freedom here are 34, there are 37 breaks, need to minus 3 for estimating two parameters
pchisq(stfpkam_test_stat, 34, lower.tail=FALSE)
# alright, this is clearly wrong



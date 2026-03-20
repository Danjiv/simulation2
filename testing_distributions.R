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

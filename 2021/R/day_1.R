library(dplyr)
if(Sys.info()[["user"]] %in% c("eidearob", "EIDEAROB"))
  setwd("C:/Users/eidearob/Documents/GitHub/advent-of-code/")
dt <- read.table("2021/Input/1/data.txt")
# First part: number of times the difference is above 0
(diff(dt[,1]) > 0) %>% sum %>% print

# Second part: 
# 1/ Create a matrix where each row corresponds to a bloc of 3
# 2/ Compute the sum of each row
# 3/ Compute the number of times the difference of this sum is above 0
(cbind(dt[-c(nrow(dt) - 1, nrow(dt)), 1], 
      dt[-c(1, nrow(dt)), 1], 
      dt[-c(1, 2), 1]) %>% rowSums() %>% diff > 0) %>% sum %>% print

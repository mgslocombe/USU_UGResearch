#Load packages and data ----
library(tidyverse)

pct <- read.csv("./Data/clean_perc_covertimeAM.csv", header = TRUE)

#Create matrix w/ mean and sd for each plant category ----
#Fineleaf
unique(pct$tms)

tms <- seq(1,6)
fl_perc <- matrix(NA, nrow = 6, ncol=3)
fl_perc[,1] <- seq(1,6)
fl_perc [1,2:3] <- 0

colnames(fl_perc) <- c("tms", "mean", "sd")

for(i in 2:length(tms)){
  
  temp <- pct %>% filter(tms == i & trt == "5") %>%
    filter(pl_code == "FL")
  
  fl_perc[i,2] <- (sum(temp$pro))/20
  fl_perc[i,3] <- sd(temp$pro)
  
}

#Whorled
unique (pct$tms)

wl_perc <- matrix(NA, nrow = 6, ncol=3)
wl_perc[,1] <- seq(1,6)
wl_perc [1,2:3] <- 0

colnames(wl_perc) <- c("tms", "mean", "sd")

for(i in 2:length(tms)){
  
  temp <- pct %>% filter(tms == i & trt == "5") %>%
    filter(pl_code == "WL")
  
  wl_perc[i,2] <- (sum(temp$pro))/20
  wl_perc[i,3] <- sd(temp$pro)
  
}

#Only one observation of whorled for timestep 2, no sd calculated

#Terrestrial
t_perc <- matrix(NA, nrow = 6, ncol=3)
t_perc[,1] <- seq(1,6)
t_perc [1,2:3] <- 0

colnames(t_perc) <- c("tms", "mean", "sd")

for(i in 2:length(tms)){
  
  temp <- pct %>% filter(tms == i & trt == "5") %>%
    filter(pl_code == "T")
  
  t_perc[i,2] <- (sum(temp$pro))/20
  t_perc[i,3] <- sd(temp$pro)
  
}

#No observations of T plants in tms 1-5, replaced with zeros
t_perc [2:5,2:3] <- 0 

#Duckweed
dw_perc <- matrix(NA, nrow = 6, ncol=3)
dw_perc[,1] <- seq(1,6)
dw_perc [1,2:3] <- 0

colnames(dw_perc) <- c("tms", "mean", "sd")

for(i in 2:length(tms)){
  
  temp <- pct %>% filter(tms == i & trt == "5") %>%
    filter(pl_code == "DW")
  
  dw_perc[i,2] <- (sum(temp$pro))/20
  dw_perc[i,3] <- sd(temp$pro)
  
}


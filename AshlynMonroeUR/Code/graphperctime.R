#Load packages and data ----
library(tidyverse)

pctdownload <- read.csv("./Data/clean_perc_covertimeAM.csv", header = TRUE)

#Add rows of data for all 0s ----
#Edit pct to only include plant codes of interest (FL, WL, T, DW) and no PRD
pctcl <- pctdownload %>% filter(pl_code == "FL" | 
                          pl_code == "WL" | 
                          pl_code == "T" | 
                          pl_code == "DW") %>%
  filter(loc != "PRD")

pctfill <- crossing(1:6, pctcl$block, pctcl$loc, pctcl$trt, pctcl$pl_code)
colnames(pctfill) <- c("tms", "block", "loc", "trt", "pl_code")

pct <- pctfill %>%
  left_join(pctcl, by = c("tms", "block", "loc", "trt", "pl_code")) %>%
  select(-X, -tank)

pct$pro[is.na(pct$pro)] <- 0

#Create matrix w/ mean and sd for each plant category ----
#Fineleaf
unique(pct$tms)

tms <- seq(1,6)
fl_perc <- matrix(NA, nrow = 6, ncol=3)
fl_perc[,1] <- seq(1,6)

colnames(fl_perc) <- c("tms", "mean", "sd")

for(i in 1:length(tms)){
  
  temp <- pct %>% filter(tms == i & trt == "5") %>%
    filter(pl_code == "FL")
  
  fl_perc[i,2] <- mean(temp$pro)
  fl_perc[i,3] <- sd(temp$pro)
  
}

#Whorled
wl_perc <- matrix(NA, nrow = 6, ncol=3)
wl_perc[,1] <- seq(1,6)

colnames(wl_perc) <- c("tms", "mean", "sd")

for(i in 1:length(tms)){
  
  temp <- pct %>% filter(tms == i & trt == "5") %>%
    filter(pl_code == "WL")
  
  wl_perc[i,2] <- mean(temp$pro)
  wl_perc[i,3] <- sd(temp$pro)
  
}

#Terrestrial
t_perc <- matrix(NA, nrow = 6, ncol=3)
t_perc[,1] <- seq(1,6)

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


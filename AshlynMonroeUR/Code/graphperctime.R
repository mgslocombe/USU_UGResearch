#Load packages and data ----
library(tidyverse)
library(Hmisc)

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

#Create matrix w/ mean and sd for each plant category for trt 5cm ----
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

fl_perc <- as.data.frame(fl_perc)

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

wl_perc <- as.data.frame(wl_perc)

#Terrestrial
t_perc <- matrix(NA, nrow = 6, ncol=3)
t_perc[,1] <- seq(1,6)

colnames(t_perc) <- c("tms", "mean", "sd")

for(i in 1:length(tms)){
  
  temp <- pct %>% filter(tms == i & trt == "5") %>%
    filter(pl_code == "T")
  
  t_perc[i,2] <- mean(temp$pro)
  t_perc[i,3] <- sd(temp$pro)
  
}

t_perc <- as.data.frame(t_perc)

#Duckweed
dw_perc <- matrix(NA, nrow = 6, ncol=3)
dw_perc[,1] <- seq(1,6)

colnames(dw_perc) <- c("tms", "mean", "sd")

for(i in 1:length(tms)){
  
  temp <- pct %>% filter(tms == i & trt == "5") %>%
    filter(pl_code == "DW")
  
  dw_perc[i,2] <- mean(temp$pro)
  dw_perc[i,3] <- sd(temp$pro)
  
}

dw_perc <- as.data.frame(dw_perc)

#Create graph of cover over time for pl groups, trt 5cm ----

plot(x = seq(1,6), fl_perc$mean, type="n", ylim=c(0,1), xlab="Timesteps", 
     ylab = "Proportional Cover", main = "5 cm of water")
with (
  data = fl_perc
  , expr = errbar(tms-0.2, mean, mean+sd, mean-sd, add=T, pch=16, cap=.02, lwd=2, 
                  col="blue", errbar.col="blue")
)

with (
  data = wl_perc
  , expr = errbar(tms-0.1, mean, mean+sd, mean-sd, add=T, pch=16, cap=.02, lwd =2,
                  col="lightblue", errbar.col="lightblue")
)

with (
  data = t_perc
  , expr = errbar(tms, mean, mean+sd, mean-sd, add=T, pch=16, cap=.02, lwd =2,
                  col="gold1", errbar.col="gold1")
)

with (
  data = dw_perc
  , expr = errbar(tms+0.1, mean, mean+sd, mean-sd, add=T, pch=16, cap=.02, lwd =2,
                  col="olivedrab", errbar.col="olivedrab")
)

legend("topleft", legend=c("Fineleaf", "Whorled", "Terrestrial", "Duckweed"), 
       col=c("blue", "lightblue", "gold1", "olivedrab"),pch=16, bty="n")

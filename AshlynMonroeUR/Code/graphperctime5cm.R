#Load packages and data ----
library(tidyverse)
library(Hmisc)
library(plotrix)
library(lubridate)

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

pct <- pct %>%
  mutate(perc = pro * 100)

date <- c("2024-01-12", "2024-01-22", "2024-02-02", "2024-02-16", "2024-03-01", 
          "2024-03-08") %>%
  ymd()

date2 <- c("2024-01-05","2024-01-12", "2024-01-22", "2024-02-02", "2024-02-16", "2024-03-01", 
           "2024-03-08", "2024-03-15") %>%
  ymd()

#Create matrix w/ mean and sd for each plant category for trt 5cm ----
#Fineleaf

tms <- seq(1,6)
fl_perc <- matrix(NA, nrow = 6, ncol=3)
fl_perc[,1] <- seq(1,6)

colnames(fl_perc) <- c("tms", "mean", "se")

for(i in 1:length(tms)){
  
  temp <- pct %>% filter(tms == i & trt == "5") %>%
    filter(pl_code == "FL")
  
  fl_perc[i,2] <- mean(temp$perc)
  fl_perc[i,3] <- std.error(temp$perc)
  
}

fl_perc <- as.data.frame(cbind(fl_perc, date))
fl_perc <- as.data.frame(cbind(fl_perc, date))

fl_perc <- fl_perc[,c(1:3,5)]

#Whorled
wl_perc <- matrix(NA, nrow = 6, ncol=3)
wl_perc[,1] <- seq(1,6)

colnames(wl_perc) <- c("tms", "mean", "se")

for(i in 1:length(tms)){
  
  temp <- pct %>% filter(tms == i & trt == "5") %>%
    filter(pl_code == "WL")
  
  wl_perc[i,2] <- mean(temp$perc)
  wl_perc[i,3] <- std.error(temp$perc)
  
}

wl_perc <- as.data.frame(cbind(wl_perc, date))
wl_perc <- as.data.frame(cbind(wl_perc, date))

wl_perc <- wl_perc[,c(1:3,5)]

#Terrestrial
t_perc <- matrix(NA, nrow = 6, ncol=3)
t_perc[,1] <- seq(1,6)

colnames(t_perc) <- c("tms", "mean", "se")

for(i in 1:length(tms)){
  
  temp <- pct %>% filter(tms == i & trt == "5") %>%
    filter(pl_code == "T")
  
  t_perc[i,2] <- mean(temp$perc)
  t_perc[i,3] <- std.error(temp$perc)
  
}

t_perc <- as.data.frame(cbind(t_perc, date))
t_perc <- as.data.frame(cbind(t_perc, date))

t_perc <- t_perc[,c(1:3,5)]

#Duckweed
dw_perc <- matrix(NA, nrow = 6, ncol=3)
dw_perc[,1] <- seq(1,6)

colnames(dw_perc) <- c("tms", "mean", "se")

for(i in 1:length(tms)){
  
  temp <- pct %>% filter(tms == i & trt == "5") %>%
    filter(pl_code == "DW")
  
  dw_perc[i,2] <- mean(temp$perc)
  dw_perc[i,3] <- std.error(temp$perc)
  
}

dw_perc <- as.data.frame(cbind(dw_perc, date))
dw_perc <- as.data.frame(cbind(dw_perc, date))

dw_perc <- dw_perc[,c(1:3,5)]

#Create graph of cover over time for pl groups, trt 5cm ----
plot(x = fl_perc$date, y = fl_perc$mean, type="n", ylim=c(0,100), xlab="", 
     ylab = "Absolute Percent Cover", main = "5 cm of water", frame = FALSE, 
     xaxt = "n")
axis(1, fl_perc$date, format(fl_perc$date, "%b %d"), cex.axis = .7)
with (
  data = fl_perc
  , expr = errbar(date-1, mean, mean+se, mean-se, add=T, pch=19, cex=1.5, cap=.0, lwd=2, 
                  col="blue", errbar.col="blue", type = "b")
)

with (
  data = wl_perc
  , expr = errbar(date, mean, mean+se, mean-se, add=T, pch=19, cex=1.5, cap=.0, lwd =2,
                  col="lightblue", errbar.col="lightblue", type = "b")
)

with (
  data = t_perc
  , expr = errbar(date+1, mean, mean+se, mean-se, add=T, pch=19, cex=1.5, cap=.0, lwd =2,
                  col="gold1", errbar.col="gold1", type = "b")
)

with (
  data = dw_perc
  , expr = errbar(date+2, mean, mean+se, mean-se, add=T, pch=19, cex=1.5, cap=.0, lwd =2,
                  col="olivedrab", errbar.col="olivedrab", type = "b")
)

legend("topleft", legend=c("Fineleaf", "Whorled", "Terrestrial", "Duckweed"), 
       col=c("blue", "lightblue", "gold1", "olivedrab"),pch=16, bty="n")

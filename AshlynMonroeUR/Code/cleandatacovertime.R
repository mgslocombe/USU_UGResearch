#Load data ----
getwd()
covertime <- read.csv("./Data/perc_covertimeAM.csv", header=TRUE)

library(tidyverse)
#Clean data, covertime ----

#Run summary to ensure there are not unreasonable values
summary(covertime)

#Review unique values for each column (looking for data entry mistakes)
unique(covertime$date)
unique(covertime$tank)
unique(covertime$block)
unique(covertime$loc)
unique(covertime$trt)
unique(covertime$pl_code)
unique(covertime$perc)

#Reassign values that were entered incorrectly
covertime$trt[covertime$trt == "sat"] <- "drought"

#Add column for ordinal data collection
covertime <- covertime %>%
  mutate(tms = ifelse(date == "1/12", 1, ifelse(
    date == "1/22", 2, ifelse(
      date == "2/2", 3, ifelse(
        date == "2/16", 4, ifelse(
          date == "3/1", 5, ifelse(
            date == "3/8", 6, NA
          )
        )
      )
    )
  )))

#Add column for percent cover from cover class data
covertime <- covertime %>%
  mutate(pro = ifelse(perc == 2, ((0+5)/2)/100, ifelse(
    perc == 3, ((5+10)/2)/100, ifelse(
      perc == 4, ((10+20)/2)/100, ifelse(
        perc == 5, ((20+30)/2)/100, ifelse(
          perc == 6, ((30+40)/2)/100, ifelse(
            perc == 7, ((40+50)/2)/100, ifelse(
              perc == 8, ((50+60)/2)/100, ifelse(
                perc == 9, ((60+70)/2)/100, ifelse(
                  perc == 10, ((70+80)/2)/100, ifelse(
                    perc == 11, ((80+90)/2)/100, ifelse(
                      perc == 12, ((90+95)/2)/100, ifelse(
                        perc == 13, (95+100)/2/100, NA
                      )))))))))))))

covertimecl <- covertime[,c(3:7,11:12)]
covertimecl$block <- as.factor(covertimecl$block)
covertimecl$loc <- as.factor(covertimecl$loc)
covertimecl$trt <- as.factor(covertimecl$trt)
covertimecl$pl_code <- as.factor(covertimecl$pl_code)
covertimecl$tms <- as.factor(covertimecl$tms)
summary(covertimecl)

#Confirm data types are correct
#as.factor()
#as.numeric()
#as.character()
#Reference above functions as needed

#Save clean data ----
write.csv(covertimecl,"./Data/clean_perc_covertimeAM.csv", row.names=TRUE)

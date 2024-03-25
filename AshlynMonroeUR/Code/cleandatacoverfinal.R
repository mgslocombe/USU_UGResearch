#Load data ----
getwd()
coverfinal <- read.csv("./Data/perc_speciesfinalAM.csv")

library(tidyverse)

#Clean data, coverfinal ----

#Run summary to ensure there are not unreasonable values
summary(coverfinal)

#Review unique values for each column (looking for data entry mistakes)
unique(coverfinal$date)
unique(coverfinal$tank)
length(unique(coverfinal$tank))
unique(coverfinal$block)
unique(coverfinal$loc)
unique(coverfinal$trt)
unique(coverfinal$pl_code)
unique(coverfinal$perc)

#Reassign values that were entered incorrectly
coverfinal$trt[coverfinal$trt == "sat"] <- "drought"
coverfinal$pl_code[coverfinal$pl_code == "unid"] <- "unidentified"
coverfinal$pl_code [coverfinal$pl_code == "unid moss"] <- "unidentified2"
coverfinal$pl_code [coverfinal$pl_code == "POFO"] <- "POFL"


#Add column for percent cover from cover class data
coverfinal <- coverfinal %>%
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

coverfinalcl <- coverfinal[,c(3:7,9)]
#could also use tidy to select the columns you want 
#coverfinalcl <- select(coverfinal, date, tank, block, loc, trt, pl_code, pro)
#coverfinalcl <- select(coverfinal, -percID, -perc)
#2 lines above have the advantage of still working even if order of col changed

coverfinalcl$block <- as.factor(coverfinalcl$block)
coverfinalcl$loc <- as.factor(coverfinalcl$loc)
coverfinalcl$trt <- as.factor(coverfinalcl$trt)
coverfinalcl$pl_code <- as.factor(coverfinalcl$pl_code)
summary(coverfinalcl)

#Confirm data types are correct
#as.factor()
#as.numeric()
#as.character()
#Reference above functions as needed

#Save clean data ----
write.csv(coverfinalcl,"./Data/clean_perc_coverfinalAM.csv", row.names=TRUE)

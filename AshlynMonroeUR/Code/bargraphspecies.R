#loading packages and data
library(tidyvers)
finalcover <- read.csv("./Data/clean_perc_coverfinalAM.csv", header = TRUE)

#Cleaning data
finalcovercl <- select(finalcover, -X, -tank)
finalcovercl <- finalcovercl %>% 
  filter(loc != "PRD") %>%
  filter(pl_code == "FLPO"|
           pl_code == "STFI"|
           pl_code == "ZAPA"|
           pl_code == "CHARA"|
           pl_code == "STPE"|
           pl_code == "LEMNA"|
           pl_code == "coonstail"|
           pl_code == "terrestrial")

#adding zeros to get cover average
scfill <- crossing(finalcovercl$block, finalcovercl$loc, finalcovercl$trt,
                    finalcovercl$pl_code)
colnames(scfill) <- c("block", "loc", "trt", "pl_code")

sc <- scfill %>%
  left_join(finalcovercl, by = c("block", "loc", "trt", "pl_code"))

sc$pro[is.na(sc$pro)] <- 0

sc <- sc %>%
  mutate(perc = pro * 100) %>%
  select(-pro)
#deleting pro column

#grouping by block then finding average
scmean <- sc %>%
  group_by(loc, trt, pl_code) %>%
  mutate(meanperc = mean(perc)) %>%
  ungroup()%>%
  select(-perc,-block)

unique(scmean)
scfin <- unique(scmean)

scfin$loc <- as.character(scfin$loc)

scfin$loc[scfin$loc == "FB"] <- "Farmington Bay"
scfin$loc[scfin$loc == "OB"] <- "Ogden Bay"
scfin$loc[scfin$loc == "PS"] <- "Public Shooting"
scfin$loc[scfin$loc == "SC"] <- "Salt Creek"

scfin$loc <- as.factor(scfin$loc)

scfin$trt <- as.character(scfin$trt)

scfin$trt[scfin$trt == "5"] <- "5cm"
scfin$trt[scfin$trt == "15"] <- "15cm"
scfin$trt[scfin$trt == "drought"] <- "Drought"

scfin$trt <- as.factor(scfin$trt)

#Graphing
scfin$loc <- factor(scfin$loc, levels = c('Farmington Bay','Ogden Bay',
                                          'Public Shooting','Salt Creek'))
scfin$trt <- factor(scfin$trt, levels = c('Drought','5cm','15cm'))

ggplot(scfin, aes(x = trt, y = meanperc, fill = pl_code)) + 
  geom_bar(stat = 'identity') + xlab("Treatment") + ylab("Absolute Percent Cover") +
  labs(fill = "Plant Catagory") +
  scale_fill_manual(values=c("#82cbed","red","yellow","green","gold1","olivedrab","blue","brown")) +
  facet_wrap("loc",nrow = 1) +
  theme_bw()


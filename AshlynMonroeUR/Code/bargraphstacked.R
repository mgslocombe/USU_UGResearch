#loading packages and data
library(tidyverse)
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

scfin$pl_code <- as.character(scfin$pl_code)

scfin$pl_code[scfin$pl_code == "CHARA"] <- "Muskgrass"
scfin$pl_code[scfin$pl_code == "FLPO"] <- "Fineleaf Potamogeton spp." 
#added spp to help clarify that this is multiple species
scfin$pl_code[scfin$pl_code == "LEMNA"] <- "Duckweed"
scfin$pl_code[scfin$pl_code == "STFI"] <- "Fineleaf Pondweed"
scfin$pl_code[scfin$pl_code == "STPE"] <- "Sago Pondweed"
scfin$pl_code[scfin$pl_code == "ZAPA"] <- "Horned Pondweed"
scfin$pl_code[scfin$pl_code == "coonstail"] <- "Coonstail"
scfin$pl_code[scfin$pl_code == "terrestrial"] <- "Terrestrial"

scfin$pl_code <- as.factor(scfin$pl_code)

#Graphing
scfin$loc <- factor(scfin$loc, levels = c('Farmington Bay','Ogden Bay',
                                          'Public Shooting','Salt Creek'))
scfin$trt <- factor(scfin$trt, levels = c('Drought','5cm','15cm'))

#Setting levels for pl_code so that similar species are are stacked on one another
scfin$pl_code <- factor(scfin$pl_code, 
                        levels = c('Fineleaf Pondweed','Fineleaf Potamogeton spp.',
                                   'Horned Pondweed', 'Sago Pondweed', 'Coonstail',
                                   'Muskgrass', 'Duckweed', 'Terrestrial'))

ggplot(scfin, aes(x = trt, y = meanperc, fill = pl_code)) + 
  geom_bar(stat = 'identity') + xlab(" ") + ylab("Absolute Percent Cover") +
  labs(fill = "Plant Catagory") +
  scale_fill_manual(values=c("#b6dee0","#7dd5db","#2f85ad","#2f4aad","#eaed9a","#e3b886","#5f048a","#e8bcdb")) +
  facet_wrap("loc",nrow = 1) +
  theme_bw() +
  theme(axis.line = element_line(color='black'),
        plot.background = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank())+
  theme(strip.background = element_blank())+
  theme(legend.text = element_text(size=6.5))

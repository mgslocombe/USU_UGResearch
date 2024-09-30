#Load packages
library(DBI)
library(tidyverse)

#Load data
deltavegdb<-dbConnect(RSQLite::SQLite(),"./Data/deltavegsurvey.db")

#Plot data
plot<- dbGetQuery(deltavegdb, "SELECT * FROM plot;")
site<- dbGetQuery(deltavegdb, "SELECT * FROM site;")

head(site)
head(plot)

#filter to 2023 and format data
plot23 <- plot %>%
  left_join(site, by = "siteID_date") %>%
  select(siteID_date:perc_cov, survey_date) %>%
  filter(survey_date=="23/08/01" | survey_date=="23/08/02") %>%
  filter(plt_code=="" | plt_code=="POCR") %>%
  mutate(percent = ifelse(perc_cov==1, 0.05/2, ifelse(
    perc_cov==2, (.25+.05)/2, ifelse(
      perc_cov==3, (.5+.25)/2, ifelse(
        perc_cov==4, (.75+.5)/2, ifelse(
          perc_cov==5, (.95+.75)/2, ifelse(
            perc_cov==6, (1+.95)/2, NA
            )))))))
unique(plot23$perc_cov)

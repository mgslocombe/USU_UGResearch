##
##ArcGIS##
##

#Load packages
library(DBI)
library(tidyverse)
library(ggplot2)
library(glmmTMB)
library(DHARMa)

# Connect to SQLite database
deltavegdb<-dbConnect(RSQLite::SQLite(),"./Data/deltavegsurvey.db")

# Load csv data
plot_data<- dbGetQuery(deltavegdb, "SELECT * FROM plot;")
site_data<- dbGetQuery(deltavegdb, "SELECT * FROM site;")

head(site_data)
head(plot_data)

# Filter site data fro 2023-August and 2024-Sept
site_filtered <- site_data %>%
  filter(survey_date %in% c('23/08/01', '23/08/02', '24/09/09', '24/09/11')) %>%
  mutate(year = case_when(
    survey_date %in% c('23/08/01', '23/08/02') ~ 2023,
    survey_date %in% c('24/09/09', '24/09/11') ~ 2024
  ))

view(site_filtered)
view(plot_data)

# Join plot data with site data on site ID and siteID_date
joined_data <- site_filtered %>%
  left_join(plot_data %>%
              filter(plt_code == "POCR" | plt_code == ""),
            by = "siteID_date") %>%
  mutate(pres = ifelse(!is.na(perc_cov) & perc_cov > 0,1,0)) #1 for presence, 0 for absence

str(joined_data)
view(joined_data)

plot_summary <- joined_data %>%
  group_by(siteID.x, year) %>%
  summarise(
    presence = sum(pres, na.rm = TRUE), #Sum of presence across all subplots
    absence = 5*15 - presence, #Total possible subplots (15*5) minus presence
    .groups = 'drop'
  )

view(plot_summary)

# Manually create a data frame with GPS coordinates for each site
gps_data <- tibble(
  siteID.x = c("DZ1_3", "DZ1_4", "DZ1_6", "DZ2_1", "DZ2_2", "DZ2_3", 
               "DZ3_2", "DZ3_3", "DZ3_4", "DZ4_1", "DZ4_2", "DZ4_5", 
               "RZ_1", "RZ_5", "RZ_6"),
  GPS_lat = c(40.244436, 40.246814, 40.243978, 40.247054, 40.245785, 40.2454157,
              40.2461248, 40.248428, 40.248686, 40.2500143, 40.250287, 40.25091,
              40.240766, 40.242833, 40.243769),
  GPS_long = c(-111.719664, -111.718824, -111.718342, -111.729827, -111.730208, -111.7283928,
               -111.7238676, -111.72588, -111.726989, -111.7288555, -111.726631, -111.723676,
               -111.718471, -111.719109, -111.718547)
)

plot_summary_with_gps <- plot_summary %>%
  left_join(gps_data, by = "siteID.x")

view(plot_summary_with_gps)

# Format data
formatted_data <- plot_summary_with_gps %>%
  pivot_wider(
    names_from = year,
    values_from = c(presence, absence),
    names_prefix = "year_"
  )
view(plot_summary_reshaped)


print(formatted_data)
view(formatted_data)

#Export as CSV for ArcGIS
# 30-rows (two rows per plot, 2023 and 2024 as rows)
write.csv(plot_summary_with_gps, "plot_summary_30_rows.csv", row.names = FALSE)

# 15-row format (cone row per plot, 2023 and 2024 as columns)
write.csv(formatted_data, "plot_summary_15_rows.csv", row.names = FALSE)



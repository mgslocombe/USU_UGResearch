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

# Filter site data fro 2023-August and 2024-July
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
  mutate(percent = ifelse(is.na(perc_cov), 0,
                          ifelse(perc_cov==0,0,
                                 ifelse(perc_cov==1, 0.05/2,
                                        ifelse(perc_cov==2, (.25+.05)/2,
                                               ifelse(perc_cov==3, (.5+.25)/2,
                                                      ifelse(perc_cov==4, (.75+.5)/2,
                                                             ifelse(perc_cov==5, (.95+.75)/2,
                                                                    ifelse(perc_cov==6, (1+.95)/2, NA)))))))))

str(joined_data)
view(joined_data)

# Calculate mean cover by plot and site, accounting for NAs and 0s
mean_cover <- joined_data %>%
  group_by(siteID.x, year) %>%
  summarise(mean_cover = sum(percent, na.rm = TRUE)/5, .groups = 'drop')

print(mean_cover)
view(mean_cover)

unique(joined_data$siteID.x)
unique(site_filtered$siteID)

# Format data
formatted_data <- mean_cover %>%
  pivot_wider(names_from = year, values_from = mean_cover) %>%
  filter(!is.na('2023') & !is.na('2024'))

print(formatted_data)
view(formatted_data)

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

# Join the manually inputted GPS coordinates to the mean_cover data
mean_cover_with_gps <- mean_cover %>%
  left_join(gps_data, by = "siteID.x")

# View the resulting data with GPS coordinates
print(mean_cover_with_gps)
view(mean_cover_with_gps)

# Pivot the data and include GPS_lat and GPS_long
final_formatted_data <- mean_cover_with_gps %>%
  pivot_wider(names_from = year, values_from = mean_cover) %>%
  filter(!is.na(`2023`) & !is.na(`2024`)) %>%
  select(siteID.x, GPS_lat, GPS_long, everything())  # Keep GPS_lat and GPS_long along with the years

# View the final formatted data
print(final_formatted_data)
view(final_formatted_data)


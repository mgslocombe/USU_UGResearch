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
  filter(survey_date %in% c('23/08/01', '23/08/02', '24/07/16', '24/07/17')) %>%
  mutate(year = case_when(
    survey_date %in% c('23/08/01', '23/08/02') ~ 2023,
    survey_date %in% c('24/07/16', '24/07/17') ~ 2024
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

###############
# Create histograms to decide if the data has a normal distribution

ggplot(joined_data %>%
         filter(year==2023),
       aes(x=percent *100)) +
  geom_histogram(binwidth = 5, fill = "orange", alpha = 0.7, color = "black") +
  labs(title = "Percent Cover Histogram for 2023", 
       x = "Percent Cover (%)", 
       y = "Frequency") +
  theme_minimal()

ggplot(joined_data %>%
        filter(year==2024),
      aes(x=percent *100)) +
  geom_histogram(binwidth = 5, fill = "blue", alpha = 0.7, color = "black") +
  labs(title = "Percent Cover Histogram for 2024", 
       x = "Percent Cover (%)", 
       y = "Frequency") +
  theme_minimal()

ggplot(joined_data,
       aes(x = percent *100, fill = as.factor(year))) +
  geom_histogram(binwidth = 5, alpha = 0.7, position = "dodge", color = "black") +
  labs(title = "Side-by-Side Percent Cover Histogram for 2023-2024", 
       x = "Percent Cover (%)", 
       y = "Frequency",
       fill = "Year") +
  theme_minimal() +
  scale_fill_manual(values = c("orange", "blue"))

# There is not a normal distribution, so apply a log transformation to the data
# Then check again

log_joined_data <- joined_data %>%
  mutate(log_percent = log1p(percent *100))

ggplot(log_joined_data,
       aes(x = percent *100, fill = as.factor(year))) +
  geom_histogram(binwidth = 5, alpha = 0.7, position = "dodge", color = "black") +
  labs(title = "Log-Transformed Percent Cover Histogram for 2023-2024", 
       x = "Log-Transformed Percent Cover (%)", 
       y = "Frequency",
       fill = "Year") +
  theme_minimal()+
  scale_fill_manual(values = c("pink", "green"))

summary(joined_data$percent *100)
summary(log_joined_data$log_percent)

# It looks like there is maybe too many 0's for the log transformation to work property for these data...

# Generalized linear model
# Make 0's a little higher (values for this model must be between 0 and 1)
# Calculate mean cover by plot and site, accounting for NAs and 0s

mean_cover$mean_cover[mean_cover$mean_cover == 0] <- 0.0001

view(mean_cover)

mod <- glmmTMB(mean_cover ~ year,
               data = mean_cover,
               family = beta_family)

summary(mod)
# There is no change in mean between years

simulateResiduals(mod, plot = T)
# Too many 0's

# For undergrad data, use count of POCR instead. Sites where POCR was in the plot vs not.
# Bar plot:


##############
#PAUSE
# --------------------------------------------------------------------------------------------------------------------

# Run paired t-test comparing mean cover between 2023 and 2024
t_test_result <- t.test(formatted_data$'2023', formatted_data$'2024', paired = TRUE)

print(t_test_result)

# We can be 95% confident (or there is strong evidence)
# that the mean cover of Potamogeton crispus in the Provo River Delta in 2024 is between
# -1.2491 and 1.7157 units different from the  mean cover in 2023.
# The mean difference indicates that, on average, the cover in 2024 is approximately
# 0.2333 units higher than in 2023,
# although this difference is not statistically significant (p-value = 0.2952).


# 
# Add rows where POCR = 0.

# # OLD!!!!!!!!!!!!Join plot data with site data on site ID and siteID_date
# joined_data <- plot_data %>%
#   filter(plt_code == "POCR" | plt_code == "") %>%
#   inner_join(site_filtered, by = "siteID_date") %>%
#   mutate(percent = ifelse(perc_cov==0,0,
#               ifelse(perc_cov==1, 0.05/2,
#               ifelse(perc_cov==2, (.25+.05)/2,
#               ifelse(perc_cov==3, (.5+.25)/2,
#               ifelse(perc_cov==4, (.75+.5)/2,
#               ifelse(perc_cov==5, (.95+.75)/2,
#               ifelse(perc_cov==6, (1+.95)/2, NA)))))))) %>%
#   filter(!siteID.x %in% "RZ_1")

# Add the two plots with no POCR

# missing_rows <- tibble(
#   siteID.x = c("RZ_1", "DZ1_6"),
#   '2023' = c(0,0),
#   '2024' = c(0,0)
# )
# 
# formatted_data <- formatted_data %>%
#   bind_rows(missing_rows)
# 
# view(formatted_data)



###### This is now data for August 2023 and September 2024
# Filter site data fro 2023-August and 2024-July
site_filtered2 <- site_data %>%
  filter(survey_date %in% c('23/08/01', '23/08/02', '24/09/09', '24/09/11')) %>%
  mutate(year = case_when(
    survey_date %in% c('23/08/01', '23/08/02') ~ 2023,
    survey_date %in% c('24/09/09', '24/09/11') ~ 2024
  ))

view(site_filtered2)
view(plot_data)

# Join plot data with site data on site ID and siteID_date
joined_data2 <- site_filtered2 %>%
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

## mutate 0 and 1 for presence absence for each subplot. And then find the sum for each plot.

str(joined_data2)
view(joined_data2)

# Calculate mean cover by plot and site, accounting for NAs and 0s
mean_cover2 <- joined_data2 %>%
  group_by(siteID.x, year) %>%
  summarise(mean_cover = sum(percent, na.rm = TRUE)/5, .groups = 'drop')

print(mean_cover2)
view(mean_cover2)

unique(joined_data2$siteID.x)
unique(site_filtered2$siteID)

# Format data
formatted_data2 <- mean_cover2 %>%
  pivot_wider(names_from = year, values_from = mean_cover) %>%
  filter(!is.na('2023') & !is.na('2024'))

print(formatted_data2)
view(formatted_data2)

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




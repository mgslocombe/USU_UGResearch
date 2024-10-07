#Load packages
library(DBI)
library(tidyverse)

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
         
# Join plot data with site data on site ID and siteID_date
joined_data <- plot_data %>%
  filter(plt_code == "POCR" | plt_code == "") %>%
  inner_join(site_filtered, by = "siteID_date") %>%
  mutate(percent = ifelse(perc_cov==1, 0.05/2,
            ifelse(perc_cov==2, (.25+.05)/2,
            ifelse(perc_cov==3, (.5+.25)/2,
            ifelse(perc_cov==4, (.75+.5)/2,
            ifelse(perc_cov==5, (.95+.75)/2,
            ifelse(perc_cov==6, (1+.95)/2, NA)))))))
str(joined_data)
view(joined_data)

# Add 0's to ifelse code

# Filter out RZ_1 because of NA and no other POCR observations for the site.

# Group by site_id and year and add together percent and divide by 5.

# select for columns you want

# unique to only keep the rows unique from one another (format data) it should be a 15 x 2.

# make a histogram of each years data to see if it's normally distributed. (it probably won't be), so do a
# log transformation on data. transform percent data into percent (*100).

# Calculate mean cover by plot and site, accounting for NAs and 0s
mean_cover <- joined_data %>%
  group_by(siteID.x, year) %>%
  summarise(mean_cover = mean(percent, na.rm = TRUE), .groups = 'drop')

print(mean_cover)

# Format data
formatted_data <- mean_cover %>%
  pivot_wider(names_from = year, values_from = mean_cover) %>%
  filter(!is.na('2023') & !is.na('2024'))

print(formatted_data)

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

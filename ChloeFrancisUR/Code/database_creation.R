#Creating a database for full delta survey

# Load SQL package----
library(DBI)

# Establish a database connection----
delvegsur <- dbConnect(RSQLite :: SQLite(), "./Data/deltavegsurvey.db")

# Read in table csv file----
plot <- read.csv("./Data/plot.csv", header =T)
site <- read.csv("./Data/site.csv", header =T)

# Create tables in database----
dbExecute(delvegsur, "CREATE TABLE site (
          siteID_date varchar(12),
          siteID varchar(5),
          GPS_lat varchar(12),
          GPS_long varchar(12),
          depth_cm varchar(3),
          flow varchar(6) CHECK (flow IN ('medium', 'low', 'high', 'none')),
          survey_date varchar(8),
          survey_by varchar(6),
          weather varchar(14) CHECK (weather IN ('clear and sunny', 'partly cloudy', 'cloudy', 'rainy', 'smoggy')),
          notes tinytext,
          PRIMARY KEY (siteID_date)
);")

dbExecute(delvegsur, "CREATE TABLE plot (
          siteID_date varchar(12),
          siteID varchar(5),
          plotID varchar(7) PRIMARY KEY,
          plot varchar(6) CHECK (plot IN ('center', 'north', 'east', 'south', 'west')),
          plt_code varchar(14) CHECK (plt_code IN ('', 'STPE', 'POCR', 'ZAPA', 'PHCA', 'rushspp', 'filalg', 'lemnaspp', 'RUCI', 'POAM', 'STFI', 'unkgrass', 'spirodelaspp', 'AZMI', 'RAAQ', 'CEDE', 'algae', 'fineleafpotamogeton')),
          height_cm varchar(3),
          perc_cov varchar(1) CHECK (perc_cov IN('', '0', '1', '2', '3', '4', '5', '6')),
          FOREIGN KEY (siteID_date) REFERENCES site(siteID_date)
);")

# Load data into tables----
dbWriteTable(delvegsur, "site", site, append = TRUE)
dbWriteTable(delvegsur, "plot", plot, append = TRUE)

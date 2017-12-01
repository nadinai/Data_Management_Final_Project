### Data Management with R
### Final project

# Packages

library(tidyverse)
library(dbplyr)
library(DBI)
library(RSQLite)
library(stringr)


## Part 1: Reading the data and putting it in a database  ---------------------------

# Reading the data  ---------------------------

# UNHCR description of all data: each row of data represents the information about 
# UNHCR's populations of concern for a given year and country of residence and/or 
# origin. In the 2016 data, figures between 1 and 4 have been replaced with an 
# asterisk (*). These represent situations where the figures are being kept 
# confidential to protect the anonymity of individuals. Such figures are not 
# included in any totals.

asylum_all <- read.csv("data/unhcr_popstats_export_asylum_seekers_all_data.csv",
                       skip = 4)
time_series <- read.csv("data/unhcr_popstats_export_time_series_all_data.csv", 
                       skip = 3)
pers_concern <- read.csv("data/unhcr_popstats_export_persons_of_concern_all_data.csv",
                         skip = 3)

# asylum_month specfic description: this dataset presents information about asylum a
# applications lodged in 38 European and 6 non-European countries. 
# Data are broken down by month and origin. Where possible, figures exclude 
# repeat/re-opened asylum applications and applications lodged on appeal or 
# with courts. For some countries, the monthly data are available since 1999 
#while for others at a later period.

asylum_month <- read.csv("data/unhcr_popstats_export_asylum_seekers_monthly_all_data.csv", 
                         skip = 3)

demographics <- read.csv("data/unhcr_popstats_export_demographics_all_data.csv",
                         skip = 3)

# Resettlement specific description: This page presents information on resettlement 
# arrivals of refugees, with or without UNHCR assistance. This dataset is based 
# on Government statistics and, in principle, excludes humanitarian admissions.

resettlement <- read.csv("data/unhcr_popstats_export_resettlement_all_data.csv",
                         skip = 3)


# Integrating the 5 tables into a databse  --------------------------

unhcr <- dbConnect(SQLite(), dbname = "data/UNHRC.sqlite")
dbWriteTable(unhcr, "asylum_all", asylum_all)
dbWriteTable(unhcr, "asylum_month", asylum_month)
dbWriteTable(unhcr, "demographics", demographics)
dbWriteTable(unhcr, "resettlement", resettlement)
dbWriteTable(unhcr, "time_series", time_series)
dbWriteTable(unhcr, "pers_concern", pers_concern)
dbListTables(unhcr)

# Tidying the demographics dataframe: creating a column for age and one for gender, 
  # ensuring the format is consistent, and changing column names.

demographicsTidy <- 
  demographics %>% 
  select(-F..Total, -M..Total) %>% 
  gather(key = gender_age, value = value,
         -Location.Name,-Country...territory.of.asylum.residence, -Year) %>% 
  separate(col=gender_age, into=c("gender", "age"), sep="\\.", extra = "merge") %>% 
  rename(country_asylum_residence=Country...territory.of.asylum.residence, 
         location_asylum_residence=Location.Name, year=Year)

demographicsTidy$age <- str_replace_all(demographicsTidy$age, "\\.", "-")
demographicsTidy$age <- str_replace_all(demographicsTidy$age, "-$|^-", "")

demographicsTidy$value <- as.numeric(demographicsTidy$value) 

# Merging time_series and resettlement

resettlement$Population.type <- rep("Resettled",nrow(resettlement))
merged <- rbind(time_series, resettlement)

# To check that the two dataframes were joined correctly, we perform a count of the
# observations contained in the unmerged datasets and in the merged one:
sum(nrow(resettlement), nrow(time_series))==nrow(merged)


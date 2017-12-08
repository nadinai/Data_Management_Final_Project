#FINAL PROJECT

# Packages
library(tidyverse)
library(stringr)
library(dbplyr)
library(DBI)
library(RSQLite)

setwd("C:/Users/admin/Desktop/Mestrado/3rd semester/Data Management with R/Final project")

# -----------------------------------------------------------------------------
# --------------------------- TIDYING DATA ------------------------------------
# -----------------------------------------------------------------------------

## Part 1: Reading the data and putting it in a database 

# Reading the data  

# UNHCR description of all data: each row of data represents the information about 
# UNHCR's populations of concern for a given year and country of residence and/or 
# origin. In the 2016 data, figures between 1 and 4 have been replaced with an 
# asterisk (*). These represent situations where the figures are being kept 
# confidential to protect the anonymity of individuals. Such figures are not 
# included in any totals.
asylum_all <- read.csv("data/unhcr_popstats_export_asylum_seekers_all_data.csv",
                       skip = 4,stringsAsFactors = FALSE)
time_series <- read.csv("data/unhcr_popstats_export_time_series_all_data.csv", 
                        skip = 3, stringsAsFactors = FALSE)
pers_concern <- read.csv("data/unhcr_popstats_export_persons_of_concern_all_data.csv",
                         skip = 3, stringsAsFactors = FALSE)

# asylum_month specfic description: this dataset presents information about asylum a
# applications lodged in 38 European and 6 non-European countries. 
# Data are broken down by month and origin. Where possible, figures exclude 
# repeat/re-opened asylum applications and applications lodged on appeal or 
# with courts. For some countries, the monthly data are available since 1999 
#while for others at a later period.

asylum_month <- read.csv("data/unhcr_popstats_export_asylum_seekers_monthly_all_data.csv", 
                         skip = 3, stringsAsFactors = FALSE)

demographics <- read.csv("data/unhcr_popstats_export_demographics_all_data.csv", 
                         skip = 3, stringsAsFactors = FALSE)

# Resettlement specific description: This page presents information on resettlement 
# arrivals of refugees, with or without UNHCR assistance. This dataset is based 
# on Government statistics and, in principle, excludes humanitarian admissions.

resettlement <- read.csv("data/unhcr_popstats_export_resettlement_all_data.csv",
                         skip = 3, stringsAsFactors = FALSE)


# Part 2: Integrating the 5 tables into a databse  --------------------------

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

demo_tidy <- 
  demographics %>% 
  select(-F..Total, -M..Total) %>% 
  gather(key = gender_age, value = value,
         -Location.Name,-Country...territory.of.asylum.residence, -Year) %>% 
  separate(col=gender_age, into=c("gender", "age"), sep="\\.", extra = "merge") %>% 
  rename(country_res=Country...territory.of.asylum.residence, 
         location_asylum_residence=Location.Name, year=Year)
demo_tidy$age <- str_replace_all(demo_tidy$age, "\\.", "-")
demo_tidy$age <- str_replace_all(demo_tidy$age, "-$|^-", "")
demo_tidy$age <- str_replace_all(demo_tidy$age, "60", "over 60")

demo_tidy$value <- as.integer(demo_tidy$value) 


# Merging time_series and resettlement
  
resettlement$Population.type <- rep("Resettled",nrow(resettlement))
pop_time <- rbind(time_series, resettlement)
  
# To check that the two dataframes were joined correctly, we performed a count of the
# observations contained in the unmerged datasets and in the merged one ("pop_time"):

sum(nrow(resettlement), nrow(time_series))==nrow(pop_time)
unique(pop_time$Population.type)

# Changing the names of the variables in the merged data frame "pop_time".

pop_time <- pop_time %>% 
  rename(country_res=Country...territory.of.asylum.residence, 
         year=Year, pop_type=Population.type, origin=Origin, value=Value)

# Part 3: Creating a continent variable (geographical) --------------------------

# In the in the data frame "pop_time"

# To discover which and how countries were listed by the UN

unique(pop_time$country_res) 

# Creating the new variable "continent_res"
# by assigning each country to its respective continent

attach(pop_time)

pop_time$continent_res[country_res=='Austria'] <- "Europe"
pop_time$continent_res[country_res=='Belgium'] <- "Europe"
pop_time$continent_res[country_res=='Switzerland'] <- "Europe"
pop_time$continent_res[country_res=='Germany'] <- "Europe"
pop_time$continent_res[country_res=='Denmark'] <- "Europe"
pop_time$continent_res[country_res=='Spain'] <- "Europe"
pop_time$continent_res[country_res=='France'] <- "Europe"
pop_time$continent_res[country_res=='United Kingdom'] <- "Europe"
pop_time$continent_res[country_res=='Greece'] <- "Europe"
pop_time$continent_res[country_res=='Italy'] <- "Europe"
pop_time$continent_res[country_res=='Luxembourg'] <- "Europe"
pop_time$continent_res[country_res=='Netherlands'] <- "Europe"
pop_time$continent_res[country_res=='Norway'] <- "Europe"
pop_time$continent_res[country_res=='Sweden'] <- "Europe"
pop_time$continent_res[country_res=='Serbia and Kosovo (S/RES/1244 (1999))'] <- "Europe"
pop_time$continent_res[country_res=='Cyprus'] <- "Europe"
pop_time$continent_res[country_res=='Portugal'] <- "Europe"
pop_time$continent_res[country_res=='Romania'] <- "Europe"
pop_time$continent_res[country_res=='Finland'] <- "Europe"
pop_time$continent_res[country_res=='Ireland'] <- "Europe"
pop_time$continent_res[country_res=='Hungary'] <- "Europe"
pop_time$continent_res[country_res=='Iceland'] <- "Europe"
pop_time$continent_res[country_res=='Poland'] <- "Europe"
pop_time$continent_res[country_res=='Czech Rep.'] <- "Europe"
pop_time$continent_res[country_res=='Croatia'] <- "Europe"
pop_time$continent_res[country_res=='Bulgaria'] <- "Europe"
pop_time$continent_res[country_res=='Albania'] <- "Europe"
pop_time$continent_res[country_res=='The former Yugoslav Republic of Macedonia'] <- "Europe"
pop_time$continent_res[country_res=='Slovenia'] <- "Europe"
pop_time$continent_res[country_res=='Bosnia and Herzegovina'] <- "Europe"
pop_time$continent_res[country_res=='Georgia'] <- "Europe"
pop_time$continent_res[country_res=='Malta'] <- "Europe"
pop_time$continent_res[country_res=='Slovakia'] <- "Europe"
pop_time$continent_res[country_res=='Belarus'] <- "Europe"
pop_time$continent_res[country_res=='Ukraine'] <- "Europe"
pop_time$continent_res[country_res=='Lithuania'] <- "Europe"
pop_time$continent_res[country_res=='Latvia'] <- "Europe"
pop_time$continent_res[country_res=='Rep. of Moldova'] <- "Europe"
pop_time$continent_res[country_res=='Estonia'] <- "Europe"
pop_time$continent_res[country_res=='Liechtenstein'] <- "Europe"
pop_time$continent_res[country_res=='Montenegro'] <- "Europe"
pop_time$continent_res[country_res=='Monaco'] <- "Europe"

pop_time$continent_res[country_res=='Canada'] <- "Americas"
pop_time$continent_res[country_res=='United States of America'] <- "Americas"
pop_time$continent_res[country_res=='Argentina'] <- "Americas"
pop_time$continent_res[country_res=='Brazil'] <- "Americas"
pop_time$continent_res[country_res=='Chile'] <- "Americas"
pop_time$continent_res[country_res=='Paraguay'] <- "Americas"
pop_time$continent_res[country_res=='Venezuela (Bolivarian Republic of)'] <- "Americas"
pop_time$continent_res[country_res=='Colombia'] <- "Americas"
pop_time$continent_res[country_res=='Dominican Rep.'] <- "Americas"
pop_time$continent_res[country_res=='Mexico'] <- "Americas"
pop_time$continent_res[country_res=='Peru'] <- "Americas"
pop_time$continent_res[country_res=='Uruguay'] <- "Americas"
pop_time$continent_res[country_res=='Bolivia (Plurinational State of)'] <- "Americas"
pop_time$continent_res[country_res=='Costa Rica'] <- "Americas"
pop_time$continent_res[country_res=='Ecuador'] <- "Americas"                                  
pop_time$continent_res[country_res=='Guatemala'] <- "Americas"
pop_time$continent_res[country_res=='Guyana'] <- "Americas"
pop_time$continent_res[country_res=='Honduras'] <- "Americas"
pop_time$continent_res[country_res=='Nicaragua'] <- "Americas"
pop_time$continent_res[country_res=='Panama'] <- "Americas"
pop_time$continent_res[country_res=='El Salvador'] <- "Americas"
pop_time$continent_res[country_res=='Cuba'] <- "Americas"
pop_time$continent_res[country_res=='Belize'] <- "Americas"
pop_time$continent_res[country_res=='French Guiana'] <- "Americas"
pop_time$continent_res[country_res=='Jamaica'] <- "Americas"
pop_time$continent_res[country_res=='Bahamas']<- "Americas"
pop_time$continent_res[country_res=='Suriname']<- "Americas"
pop_time$continent_res[country_res=='Saint Lucia']<- "Americas"
pop_time$continent_res[country_res=='Haiti']<- "Americas"
pop_time$continent_res[country_res=='Antigua and Barbuda']<- "Americas"
pop_time$continent_res[country_res=='Trinidad and Tobago']<- "Americas"
pop_time$continent_res[country_res=='Saint Kitts and Nevis']<- "Americas"
pop_time$continent_res[country_res=='Saint Vincent and the Grenadines']<- "Americas"
pop_time$continent_res[country_res=='Cayman Islands']<- "Americas"
pop_time$continent_res[country_res=='Aruba']<- "Americas"
pop_time$continent_res[country_res=='British Virgin Islands']<- "Americas"
pop_time$continent_res[country_res=='Bonaire']<- "Americas"
pop_time$continent_res[country_res=='CuraÃ§ao']<- "Americas"
pop_time$continent_res[country_res=='Grenada'] <- "Americas"
pop_time$continent_res[country_res=='Montserrat'] <- "Americas"
pop_time$continent_res[country_res=='Sint Maarten (Dutch part)'] <- "Americas"
pop_time$continent_res[country_res=='Anguilla'] <- "Americas"
pop_time$continent_res[country_res=='Turks and Caicos Islands'] <- "Americas"

pop_time$continent_res[country_res=='Morocco'] <- "Africa"
pop_time$continent_res[country_res=='Tunisia'] <- "Africa"
pop_time$continent_res[country_res=='Dem. Rep. of the Congo'] <- "Africa"
pop_time$continent_res[country_res=='Burundi'] <- "Africa"
pop_time$continent_res[country_res=='Togo'] <- "Africa"
pop_time$continent_res[country_res=='United Rep. of Tanzania'] <- "Africa"
pop_time$continent_res[country_res=='Uganda'] <- "Africa"
pop_time$continent_res[country_res=='Senegal'] <- "Africa"
pop_time$continent_res[country_res=='Central African Rep.'] <- "Africa"
pop_time$continent_res[country_res=='Egypt'] <- "Africa"
pop_time$continent_res[country_res=='Rwanda'] <- "Africa"
pop_time$continent_res[country_res=='Zambia'] <- "Africa"
pop_time$continent_res[country_res=='Sudan'] <- "Africa"
pop_time$continent_res[country_res=='Botswana'] <- "Africa"
pop_time$continent_res[country_res=='Ethiopia'] <- "Africa"
pop_time$continent_res[country_res=='Equatorial Guinea'] <- "Africa"
pop_time$continent_res[country_res=='Benin'] <- "Africa"
pop_time$continent_res[country_res=='Congo'] <- "Africa"                         
pop_time$continent_res[country_res=='Gabon'] <- "Africa"                                    
pop_time$continent_res[country_res=='Ghana'] <- "Africa"                                     
pop_time$continent_res[country_res=='Liberia'] <- "Africa"
pop_time$continent_res[country_res=='Sierra Leone'] <- "Africa"
pop_time$continent_res[country_res=='Cameroon'] <- "Africa"
pop_time$continent_res[country_res=='Algeria'] <- "Africa"
pop_time$continent_res[country_res=='Kenya'] <- "Africa"
pop_time$continent_res[country_res=='Lesotho'] <- "Africa"
pop_time$continent_res[country_res=='Swaziland'] <- "Africa"
pop_time$continent_res[country_res=='Mali'] <- "Africa"
pop_time$continent_res[country_res=='Gambia'] <- "Africa"
pop_time$continent_res[country_res=='Mozambique'] <- "Africa"
pop_time$continent_res[country_res=='Angola'] <- "Africa"
pop_time$continent_res[country_res=='Nigeria'] <- "Africa"
pop_time$continent_res[country_res=='Djibouti'] <- "Africa"
pop_time$continent_res[country_res=='Niger'] <- "Africa"
pop_time$continent_res[country_res=='Somalia'] <- "Africa"
pop_time$continent_res[country_res=='Zimbabwe'] <- "Africa"
pop_time$continent_res[country_res=='Guinea-Bissau'] <- "Africa"
pop_time$continent_res[country_res=='Burkina Faso'] <- "Africa"
pop_time$continent_res[country_res=='Malawi'] <- "Africa"
pop_time$continent_res[country_res=='Mauritania'] <- "Africa"
pop_time$continent_res[country_res=='Comoros'] <- "Africa"
pop_time$continent_res[country_res=='Guinea'] <- "Africa"
pop_time$continent_res[country_res=='Cabo Verde'] <- "Africa"
pop_time$continent_res[country_res=='Namibia'] <- "Africa"
pop_time$continent_res[country_res=='Chad'] <- "Africa"
pop_time$continent_res[country_res=='Eritrea'] <- "Africa"
pop_time$continent_res[country_res=='Libya'] <- "Africa"
pop_time$continent_res[country_res=='South Africa'] <- "Africa"
pop_time$continent_res[country_res=='Madagascar'] <- "Africa"
pop_time$continent_res[country_res=='Mauritius'] <- "Africa"
pop_time$continent_res[country_res=='South Sudan'] <- "Africa"
pop_time$continent_res[country_res=='Barbados'] <- "Africa"
pop_time$continent_res[country_res=='Seychelles'] <- "Africa"
pop_time$continent_res[country_res=="CÃ´te d'Ivoire"] <- "Africa"

pop_time$continent_res[country_res=='China'] <- "Asia"
pop_time$continent_res[country_res=='Hong Kong SAR'] <- "Asia"
pop_time$continent_res[country_res=='China, Hong Kong SAR'] <- "Asia"                   
pop_time$continent_res[country_res=='China, Macao SAR'] <- "Asia" 
pop_time$continent_res[country_res=='Turkey'] <- "Asia"
pop_time$continent_res[country_res=='Iran (Islamic Rep. of)'] <- "Asia" 
pop_time$continent_res[country_res=='India'] <- "Asia"
pop_time$continent_res[country_res=='Cambodia'] <- "Asia"
pop_time$continent_res[country_res=='Lebanon'] <- "Asia"
pop_time$continent_res[country_res=='Macao SAR'] <- "Asia"
pop_time$continent_res[country_res=='Nepal'] <- "Asia"
pop_time$continent_res[country_res=='Bhutan'] <- "Asia"
pop_time$continent_res[country_res=='United Arab Emirates'] <- "Asia" 
pop_time$continent_res[country_res=='Jordan'] <- "Asia"
pop_time$continent_res[country_res=='Viet Nam'] <- "Asia"
pop_time$continent_res[country_res=='Bangladesh'] <- "Asia"
pop_time$continent_res[country_res=='Bahrain'] <- "Asia"
pop_time$continent_res[country_res=='Saudi Arabia'] <- "Asia"
pop_time$continent_res[country_res=='Philippines'] <- "Asia"
pop_time$continent_res[country_res=='Thailand'] <- "Asia"
pop_time$continent_res[country_res=='Yemen'] <- "Asia"
pop_time$continent_res[country_res=='Rep. of Korea'] <- "Asia"
pop_time$continent_res[country_res=='Indonesia'] <- "Asia"
pop_time$continent_res[country_res=='Singapore'] <- "Asia"
pop_time$continent_res[country_res=='Iraq'] <- "Asia"
pop_time$continent_res[country_res=='Pakistan'] <- "Asia"
pop_time$continent_res[country_res=='Qatar'] <- "Asia"
pop_time$continent_res[country_res=='Syrian Arab Rep.'] <- "Asia"
pop_time$continent_res[country_res=='Kuwait'] <- "Asia"
pop_time$continent_res[country_res=='Afghanistan'] <- "Asia"
pop_time$continent_res[country_res=='Sri Lanka'] <- "Asia"
pop_time$continent_res[country_res=='Armenia'] <- "Asia"
pop_time$continent_res[country_res=='Azerbaijan'] <- "Asia"
pop_time$continent_res[country_res=='Tajikistan'] <- "Asia"
pop_time$continent_res[country_res=='Kazakhstan'] <- "Asia"
pop_time$continent_res[country_res=='Japan'] <- "Asia"
pop_time$continent_res[country_res=='Malaysia'] <- "Asia"
pop_time$continent_res[country_res=='Kyrgyzstan'] <- "Asia"
pop_time$continent_res[country_res=='Russian Federation'] <- "Asia"
pop_time$continent_res[country_res=='Turkmenistan'] <- "Asia"
pop_time$continent_res[country_res=='Uzbekistan'] <- "Asia"
pop_time$continent_res[country_res=='Israel'] <- "Asia"
pop_time$continent_res[country_res=='Oman'] <- "Asia"
pop_time$continent_res[country_res=='State of Palestine'] <- "Asia"
pop_time$continent_res[country_res=='Timor-Leste'] <- "Asia"
pop_time$continent_res[country_res=='Myanmar'] <- "Asia"
pop_time$continent_res[country_res=='Mongolia'] <- "Asia"
pop_time$continent_res[country_res=='Brunei Darussalam'] <- "Asia"
pop_time$continent_res[country_res=="Dem. People's Rep. of Korea"] <- "Asia"
pop_time$continent_res[country_res=="Lao People's Dem. Rep."] <- "Asia"

pop_time$continent_res[country_res=='Australia'] <- "Oceania"
pop_time$continent_res[country_res=='New Zealand'] <- "Oceania"
pop_time$continent_res[country_res=='Papua New Guinea'] <- "Oceania"
pop_time$continent_res[country_res=='Fiji'] <- "Oceania"
pop_time$continent_res[country_res=='Vanuatu'] <- "Oceania"
pop_time$continent_res[country_res=='Solomon Islands'] <- "Oceania"
pop_time$continent_res[country_res=='Micronesia (Federated States of)'] <- "Oceania"
pop_time$continent_res[country_res=='Palau'] <- "Oceania"
pop_time$continent_res[country_res=='Samoa'] <- "Oceania"
pop_time$continent_res[country_res=='Tonga'] <- "Oceania"
pop_time$continent_res[country_res=='Nauru'] <- "Oceania"

detach(pop_time)

# To check if all the countries were included

unique(pop_time$continent_res) 

# To change the order of the columns and put continent before country

pop_time<-pop_time[,c(1,6,2,3,4,5)]
head(pop_time)


# In the data frame "demo_tidy" 

# To discover which and how countries were listed by the UN

unique(demo_tidy$country_res) 

# To compare the countries from the dataframe "pop_time" and "demo_tidy"

unique(pop_time[!pop_time$country_res %in% demo_tidy$country_res,]) 

# "demo_tidy" has 8 countries less than "pop_time", so we can use the same coding as before
               
# Creating the new variable "continent_res"
# by assigning each country to its respective continent

attach(demo_tidy)

demo_tidy$continent_res[country_res=='Austria'] <- "Europe"
demo_tidy$continent_res[country_res=='Belgium'] <- "Europe"
demo_tidy$continent_res[country_res=='Switzerland'] <- "Europe"
demo_tidy$continent_res[country_res=='Germany'] <- "Europe"
demo_tidy$continent_res[country_res=='Denmark'] <- "Europe"
demo_tidy$continent_res[country_res=='Spain'] <- "Europe"
demo_tidy$continent_res[country_res=='France'] <- "Europe"
demo_tidy$continent_res[country_res=='United Kingdom'] <- "Europe"
demo_tidy$continent_res[country_res=='Greece'] <- "Europe"
demo_tidy$continent_res[country_res=='Italy'] <- "Europe"
demo_tidy$continent_res[country_res=='Luxembourg'] <- "Europe"
demo_tidy$continent_res[country_res=='Netherlands'] <- "Europe"
demo_tidy$continent_res[country_res=='Norway'] <- "Europe"
demo_tidy$continent_res[country_res=='Sweden'] <- "Europe"
demo_tidy$continent_res[country_res=='Serbia and Kosovo (S/RES/1244 (1999))'] <- "Europe"
demo_tidy$continent_res[country_res=='Cyprus'] <- "Europe"
demo_tidy$continent_res[country_res=='Portugal'] <- "Europe"
demo_tidy$continent_res[country_res=='Romania'] <- "Europe"
demo_tidy$continent_res[country_res=='Finland'] <- "Europe"
demo_tidy$continent_res[country_res=='Ireland'] <- "Europe"
demo_tidy$continent_res[country_res=='Hungary'] <- "Europe"
demo_tidy$continent_res[country_res=='Iceland'] <- "Europe"
demo_tidy$continent_res[country_res=='Poland'] <- "Europe"
demo_tidy$continent_res[country_res=='Czech Rep.'] <- "Europe"
demo_tidy$continent_res[country_res=='Croatia'] <- "Europe"
demo_tidy$continent_res[country_res=='Bulgaria'] <- "Europe"
demo_tidy$continent_res[country_res=='Albania'] <- "Europe"
demo_tidy$continent_res[country_res=='The former Yugoslav Republic of Macedonia'] <- "Europe"
demo_tidy$continent_res[country_res=='Slovenia'] <- "Europe"
demo_tidy$continent_res[country_res=='Bosnia and Herzegovina'] <- "Europe"
demo_tidy$continent_res[country_res=='Georgia'] <- "Europe"
demo_tidy$continent_res[country_res=='Malta'] <- "Europe"
demo_tidy$continent_res[country_res=='Slovakia'] <- "Europe"
demo_tidy$continent_res[country_res=='Belarus'] <- "Europe"
demo_tidy$continent_res[country_res=='Ukraine'] <- "Europe"
demo_tidy$continent_res[country_res=='Lithuania'] <- "Europe"
demo_tidy$continent_res[country_res=='Latvia'] <- "Europe"
demo_tidy$continent_res[country_res=='Rep. of Moldova'] <- "Europe"
demo_tidy$continent_res[country_res=='Estonia'] <- "Europe"
demo_tidy$continent_res[country_res=='Liechtenstein'] <- "Europe"
demo_tidy$continent_res[country_res=='Montenegro'] <- "Europe"
demo_tidy$continent_res[country_res=='Monaco'] <- "Europe"

demo_tidy$continent_res[country_res=='Canada'] <- "Americas"
demo_tidy$continent_res[country_res=='United States of America'] <- "Americas"
demo_tidy$continent_res[country_res=='Argentina'] <- "Americas"
demo_tidy$continent_res[country_res=='Brazil'] <- "Americas"
demo_tidy$continent_res[country_res=='Chile'] <- "Americas"
demo_tidy$continent_res[country_res=='Paraguay'] <- "Americas"
demo_tidy$continent_res[country_res=='Venezuela (Bolivarian Republic of)'] <- "Americas"
demo_tidy$continent_res[country_res=='Colombia'] <- "Americas"
demo_tidy$continent_res[country_res=='Dominican Rep.'] <- "Americas"
demo_tidy$continent_res[country_res=='Mexico'] <- "Americas"
demo_tidy$continent_res[country_res=='Peru'] <- "Americas"
demo_tidy$continent_res[country_res=='Uruguay'] <- "Americas"
demo_tidy$continent_res[country_res=='Bolivia (Plurinational State of)'] <- "Americas"
demo_tidy$continent_res[country_res=='Costa Rica'] <- "Americas"
demo_tidy$continent_res[country_res=='Ecuador'] <- "Americas"                                  
demo_tidy$continent_res[country_res=='Guatemala'] <- "Americas"
demo_tidy$continent_res[country_res=='Guyana'] <- "Americas"
demo_tidy$continent_res[country_res=='Honduras'] <- "Americas"
demo_tidy$continent_res[country_res=='Nicaragua'] <- "Americas"
demo_tidy$continent_res[country_res=='Panama'] <- "Americas"
demo_tidy$continent_res[country_res=='El Salvador'] <- "Americas"
demo_tidy$continent_res[country_res=='Cuba'] <- "Americas"
demo_tidy$continent_res[country_res=='Belize'] <- "Americas"
demo_tidy$continent_res[country_res=='French Guiana'] <- "Americas"
demo_tidy$continent_res[country_res=='Jamaica'] <- "Americas"
demo_tidy$continent_res[country_res=='Bahamas']<- "Americas"
demo_tidy$continent_res[country_res=='Suriname']<- "Americas"
demo_tidy$continent_res[country_res=='Saint Lucia']<- "Americas"
demo_tidy$continent_res[country_res=='Haiti']<- "Americas"
demo_tidy$continent_res[country_res=='Antigua and Barbuda']<- "Americas"
demo_tidy$continent_res[country_res=='Trinidad and Tobago']<- "Americas"
demo_tidy$continent_res[country_res=='Saint Kitts and Nevis']<- "Americas"
demo_tidy$continent_res[country_res=='Saint Vincent and the Grenadines']<- "Americas"
demo_tidy$continent_res[country_res=='Cayman Islands']<- "Americas"
demo_tidy$continent_res[country_res=='Aruba']<- "Americas"
demo_tidy$continent_res[country_res=='British Virgin Islands']<- "Americas"
demo_tidy$continent_res[country_res=='Bonaire']<- "Americas"
demo_tidy$continent_res[country_res=='CuraÃ§ao']<- "Americas"
demo_tidy$continent_res[country_res=='Grenada'] <- "Americas"
demo_tidy$continent_res[country_res=='Montserrat'] <- "Americas"
demo_tidy$continent_res[country_res=='Sint Maarten (Dutch part)'] <- "Americas"
demo_tidy$continent_res[country_res=='Anguilla'] <- "Americas"
demo_tidy$continent_res[country_res=='Turks and Caicos Islands'] <- "Americas"

demo_tidy$continent_res[country_res=='Morocco'] <- "Africa"
demo_tidy$continent_res[country_res=='Tunisia'] <- "Africa"
demo_tidy$continent_res[country_res=='Dem. Rep. of the Congo'] <- "Africa"
demo_tidy$continent_res[country_res=='Burundi'] <- "Africa"
demo_tidy$continent_res[country_res=='Togo'] <- "Africa"
demo_tidy$continent_res[country_res=='United Rep. of Tanzania'] <- "Africa"
demo_tidy$continent_res[country_res=='Uganda'] <- "Africa"
demo_tidy$continent_res[country_res=='Senegal'] <- "Africa"
demo_tidy$continent_res[country_res=='Central African Rep.'] <- "Africa"
demo_tidy$continent_res[country_res=='Egypt'] <- "Africa"
demo_tidy$continent_res[country_res=='Rwanda'] <- "Africa"
demo_tidy$continent_res[country_res=='Zambia'] <- "Africa"
demo_tidy$continent_res[country_res=='Sudan'] <- "Africa"
demo_tidy$continent_res[country_res=='Botswana'] <- "Africa"
demo_tidy$continent_res[country_res=='Ethiopia'] <- "Africa"
demo_tidy$continent_res[country_res=='Equatorial Guinea'] <- "Africa"
demo_tidy$continent_res[country_res=='Benin'] <- "Africa"
demo_tidy$continent_res[country_res=='Congo'] <- "Africa"                         
demo_tidy$continent_res[country_res=='Gabon'] <- "Africa"                                    
demo_tidy$continent_res[country_res=='Ghana'] <- "Africa"                                     
demo_tidy$continent_res[country_res=='Liberia'] <- "Africa"
demo_tidy$continent_res[country_res=='Sierra Leone'] <- "Africa"
demo_tidy$continent_res[country_res=='Cameroon'] <- "Africa"
demo_tidy$continent_res[country_res=='Algeria'] <- "Africa"
demo_tidy$continent_res[country_res=='Kenya'] <- "Africa"
demo_tidy$continent_res[country_res=='Lesotho'] <- "Africa"
demo_tidy$continent_res[country_res=='Swaziland'] <- "Africa"
demo_tidy$continent_res[country_res=='Mali'] <- "Africa"
demo_tidy$continent_res[country_res=='Gambia'] <- "Africa"
demo_tidy$continent_res[country_res=='Mozambique'] <- "Africa"
demo_tidy$continent_res[country_res=='Angola'] <- "Africa"
demo_tidy$continent_res[country_res=='Nigeria'] <- "Africa"
demo_tidy$continent_res[country_res=='Djibouti'] <- "Africa"
demo_tidy$continent_res[country_res=='Niger'] <- "Africa"
demo_tidy$continent_res[country_res=='Somalia'] <- "Africa"
demo_tidy$continent_res[country_res=='Zimbabwe'] <- "Africa"
demo_tidy$continent_res[country_res=='Guinea-Bissau'] <- "Africa"
demo_tidy$continent_res[country_res=='Burkina Faso'] <- "Africa"
demo_tidy$continent_res[country_res=='Malawi'] <- "Africa"
demo_tidy$continent_res[country_res=='Mauritania'] <- "Africa"
demo_tidy$continent_res[country_res=='Comoros'] <- "Africa"
demo_tidy$continent_res[country_res=='Guinea'] <- "Africa"
demo_tidy$continent_res[country_res=='Cabo Verde'] <- "Africa"
demo_tidy$continent_res[country_res=='Namibia'] <- "Africa"
demo_tidy$continent_res[country_res=='Chad'] <- "Africa"
demo_tidy$continent_res[country_res=='Eritrea'] <- "Africa"
demo_tidy$continent_res[country_res=='Libya'] <- "Africa"
demo_tidy$continent_res[country_res=='South Africa'] <- "Africa"
demo_tidy$continent_res[country_res=='Madagascar'] <- "Africa"
demo_tidy$continent_res[country_res=='Mauritius'] <- "Africa"
demo_tidy$continent_res[country_res=='South Sudan'] <- "Africa"
demo_tidy$continent_res[country_res=='Barbados'] <- "Africa"
demo_tidy$continent_res[country_res=='Seychelles'] <- "Africa"
demo_tidy$continent_res[country_res=="CÃ´te d'Ivoire"] <- "Africa"

demo_tidy$continent_res[country_res=='China'] <- "Asia"
demo_tidy$continent_res[country_res=='Hong Kong SAR'] <- "Asia"
demo_tidy$continent_res[country_res=='China, Hong Kong SAR'] <- "Asia"                   
demo_tidy$continent_res[country_res=='China, Macao SAR'] <- "Asia" 
demo_tidy$continent_res[country_res=='Turkey'] <- "Asia"
demo_tidy$continent_res[country_res=='Iran (Islamic Rep. of)'] <- "Asia" 
demo_tidy$continent_res[country_res=='India'] <- "Asia"
demo_tidy$continent_res[country_res=='Cambodia'] <- "Asia"
demo_tidy$continent_res[country_res=='Lebanon'] <- "Asia"
demo_tidy$continent_res[country_res=='Macao SAR'] <- "Asia"
demo_tidy$continent_res[country_res=='Nepal'] <- "Asia"
demo_tidy$continent_res[country_res=='Bhutan'] <- "Asia"
demo_tidy$continent_res[country_res=='United Arab Emirates'] <- "Asia" 
demo_tidy$continent_res[country_res=='Jordan'] <- "Asia"
demo_tidy$continent_res[country_res=='Viet Nam'] <- "Asia"
demo_tidy$continent_res[country_res=='Bangladesh'] <- "Asia"
demo_tidy$continent_res[country_res=='Bahrain'] <- "Asia"
demo_tidy$continent_res[country_res=='Saudi Arabia'] <- "Asia"
demo_tidy$continent_res[country_res=='Philippines'] <- "Asia"
demo_tidy$continent_res[country_res=='Thailand'] <- "Asia"
demo_tidy$continent_res[country_res=='Yemen'] <- "Asia"
demo_tidy$continent_res[country_res=='Rep. of Korea'] <- "Asia"
demo_tidy$continent_res[country_res=='Indonesia'] <- "Asia"
demo_tidy$continent_res[country_res=='Singapore'] <- "Asia"
demo_tidy$continent_res[country_res=='Iraq'] <- "Asia"
demo_tidy$continent_res[country_res=='Pakistan'] <- "Asia"
demo_tidy$continent_res[country_res=='Qatar'] <- "Asia"
demo_tidy$continent_res[country_res=='Syrian Arab Rep.'] <- "Asia"
demo_tidy$continent_res[country_res=='Kuwait'] <- "Asia"
demo_tidy$continent_res[country_res=='Afghanistan'] <- "Asia"
demo_tidy$continent_res[country_res=='Sri Lanka'] <- "Asia"
demo_tidy$continent_res[country_res=='Armenia'] <- "Asia"
demo_tidy$continent_res[country_res=='Azerbaijan'] <- "Asia"
demo_tidy$continent_res[country_res=='Tajikistan'] <- "Asia"
demo_tidy$continent_res[country_res=='Kazakhstan'] <- "Asia"
demo_tidy$continent_res[country_res=='Japan'] <- "Asia"
demo_tidy$continent_res[country_res=='Malaysia'] <- "Asia"
demo_tidy$continent_res[country_res=='Kyrgyzstan'] <- "Asia"
demo_tidy$continent_res[country_res=='Russian Federation'] <- "Asia"
demo_tidy$continent_res[country_res=='Turkmenistan'] <- "Asia"
demo_tidy$continent_res[country_res=='Uzbekistan'] <- "Asia"
demo_tidy$continent_res[country_res=='Israel'] <- "Asia"
demo_tidy$continent_res[country_res=='Oman'] <- "Asia"
demo_tidy$continent_res[country_res=='State of Palestine'] <- "Asia"
demo_tidy$continent_res[country_res=='Timor-Leste'] <- "Asia"
demo_tidy$continent_res[country_res=='Myanmar'] <- "Asia"
demo_tidy$continent_res[country_res=='Mongolia'] <- "Asia"
demo_tidy$continent_res[country_res=='Brunei Darussalam'] <- "Asia"
demo_tidy$continent_res[country_res=="Dem. People's Rep. of Korea"] <- "Asia"
demo_tidy$continent_res[country_res=="Lao People's Dem. Rep."] <- "Asia"

demo_tidy$continent_res[country_res=='Australia'] <- "Oceania"
demo_tidy$continent_res[country_res=='New Zealand'] <- "Oceania"
demo_tidy$continent_res[country_res=='Papua New Guinea'] <- "Oceania"
demo_tidy$continent_res[country_res=='Fiji'] <- "Oceania"
demo_tidy$continent_res[country_res=='Vanuatu'] <- "Oceania"
demo_tidy$continent_res[country_res=='Solomon Islands'] <- "Oceania"
demo_tidy$continent_res[country_res=='Micronesia (Federated States of)'] <- "Oceania"
demo_tidy$continent_res[country_res=='Palau'] <- "Oceania"
demo_tidy$continent_res[country_res=='Samoa'] <- "Oceania"
demo_tidy$continent_res[country_res=='Tonga'] <- "Oceania"
demo_tidy$continent_res[country_res=='Nauru'] <- "Oceania"

detach(demo_tidy)

# To check if all the countries were included

unique(demo_tidy$continent_res) 

# To change the order of the columns and put continent before country

demo_tidy<-demo_tidy[,c(1,7,2,3,4,5,6)]
head(demo_tidy)


# Part 4: treating missing data --------------------------

summary(pop_time) 
summary(demo_tidy)

# The variables "origin" and "pop_type" don't exist in the data frame "demo_tidy",
# neither does the variable "continent_res", created specifically for "pop_time".
# The variables "gender", "age" and "location_asylum_residence" don't exist in the data frame "pop_time".
# These two data frames shall not be merged, otherwise information will be lost.

unique(demo_tidy$year)
unique(pop_time$Year)

# We only have information about the demographics (of asylum seekers) from 2001, 
# while the information about persons of concern started being collected since 1951.
# Thus, we will focus our analysis in the last 15 years of collected data (2001-2016).
# There are no missing years from 2001 to 2016 in both data frames.

unique(demo_tidy$country_res)
# 191 countries, no missing values

# Adjusting some country names that are mixed with characters that are not properly read:
demo_tidy$country_res <- as.character(demo_tidy$country_res)
demo_tidy$country_res[demo_tidy$country_res=="CÃ´te d'Ivoire"] <- "Ivory Coast"
demo_tidy$country_res[demo_tidy$country_res=="CuraÃ§ao"] <- "Curacao"
demo_tidy$country_res[demo_tidy$country_res=="Serbia and Kosovo (S/RES/1244 (1999))"] <- "Serbia and Kosovo"
 
unique(pop_time$country_res) 
#199 countries, no missing values

# Adjusting some country names that are mixed with characters that are not properly read:
pop_time$country_res <- as.character(pop_time$country_res)
pop_time$country_res[pop_time$country_res=="CÃ´te d'Ivoire"] <- "Ivory Coast"
pop_time$country_res[pop_time$country_res=="CuraÃ§ao"] <- "Curacao"
pop_time$country_res[pop_time$country_res=="Serbia and Kosovo (S/RES/1244 (1999))"] <- "Serbia and Kosovo"

unique(pop_time$origin)
# Missing values are coded as "Various/unknown".
# We opt for not recoding these missing values as "NA",
# because using any command to authomatically remove "NA's" 
# could result in missing relevant information.

unique(pop_time$pop_type) 
# "[5] Others of concern" is not specific, but also not a missing value,
# so we kept it in the data frame.

unique(pop_time$value) 
# Values range from 10480 Levels: 
# -1 * 0 1 10 100 1000 10000 100000 1000000 100003 100027 10005 1001 10010 10011 10013 1002 ... 9999

# We let "-1" for lack of information about it (maybe a correction on previous years?)

# Acording the UN, "In the 2016 data, figures between 1 and 4 have been replaced with an asterisk (*). 
# These represent situations where the figures are being kept confidential to protect the 
# anonymity of individuals. Such figures are not included in any totals."
# To be sure that these figures are treated as missing values, we recoded them as "NA":
pop_time$value[pop_time$value=='\\*'] <- NA
unique(pop_time$value) 
# The "*" still appears as a unique value, but it's just a double coding:
nrow(pop_time[pop_time$value=="*",])
nrow(pop_time[pop_time$value=="NA",])
# Also, as these values ranged from 1 to 4 (acording the UN), excluding the missing values of the 
# variable "value" won't bring considerable consequences to the final analysis.
              
unique(demo_tidy$location_asylum_residence) 
# This identified no missing values.
# Looking at the data frame, we saw that the missing values are blank,
# we the recoded them as "NA":
demo_tidy$location_asylum_residence[demo_tidy$location_asylum_residence==''] <- NA
# Results could be seen on the dataframe

unique(demo_tidy$gender) 
# Female, Male, F and M, no missing values.
# Standardizing the observations:
demo_tidy$gender[demo_tidy$gender=='F'] <- "Female"
demo_tidy$gender[demo_tidy$gender=='M'] <- "Male"
unique(demo_tidy$gender)

unique(demo_tidy$age) 
# Missing values are coded as "Unknwon".
# Recoding the missing values as "NA":
demo_tidy$age[demo_tidy$age=='Unknown'] <- NA
demo_tidy$age

unique(demo_tidy$value)
# Missing values already coded as "NA"


# -----------------------------------------------------------------------------
# --------------------------- ANALYSIS ----------------------------------------
# -----------------------------------------------------------------------------

# N: How is the distribution of the populations of concern across the continents?
typeof(pop_time$value)
pop_time$value <- as.integer(as.factor(pop_time$value))

# Time trends in refugee waves across continents starting in 1980
library(scales)
pop_time %>% 
  filter(year > 1980) %>% 
  na.omit(value) %>% 
  group_by(year, continent_res) %>%
  mutate(total = sum(value, na.rm=TRUE)) %>%
  ggplot(aes(x = year, y = total)) +
  geom_line() +
  facet_wrap(~ continent_res, ncol=3) +
  scale_y_continuous(labels = comma)

# Time-trends in refugee waves across continents, including the countries
pop_time %>% 
  filter(year > 1999) %>% 
  na.omit(value) %>% 
  group_by(country_res, year) %>%
  mutate(total = sum(value, na.rm=TRUE)) %>%
  ggplot(aes(x = year, y = total)) +
  geom_line(aes(group = country_res), alpha = 0.3) +
  geom_smooth() +
  facet_wrap(~ continent_res, ncol=3)



# A closer look at the evolution of the number of refugees in Europe
pop_time %>% 
  filter(year > 1999, continent_res == "Europe") %>% 
  group_by(year) %>%
  mutate(total = sum(value, na.rm=TRUE)) %>%
  ggplot(aes(x = year, y = total)) +
  geom_line() 

# How does that look relative to the countries?
pop_time %>% 
  filter(year > 1999, continent_res == "Europe") %>% 
  group_by(year, country_res) %>%
  mutate(total = sum(value, na.rm=TRUE)) %>%
  ggplot(aes(x = year, y = total)) +
  geom_line(aes(group = country_res)) 


# Which are the countries that have had over the last 17 years over 1000000 people 
# recorded under UNHCR's populations of concern?

pop_time %>% 
  filter(year > 1999, continent_res == "Europe") %>% 
  group_by(year, country_res) %>%
  mutate(total = sum(value, na.rm=TRUE)) %>%
  filter(total  > 1000000) %>% 
  ggplot(aes(x = year, y = total, colour=country_res)) +
  geom_line(aes(group = country_res)) 


# N: How did the populations of concern distribution varied over time?
#- histograms 

# N: Is there any gender-based differemce

# From which countries do most refugees come from?

# And which one is the most preferred destination?

# How is the distribution of refugess across gender and age?
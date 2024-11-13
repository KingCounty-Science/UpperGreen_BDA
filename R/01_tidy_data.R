#=== === === === === === === ===
# Started September 30, 2022
# Rebekah Stiling, rstiling@kingcounty.gov
# This script explores a preliminary set of vegetation data
#=== === === === === === === ===

# load relevant packages ####
library(Microsoft365R) #for accessing sharepoint
library(readxl) #for reading excel files
library(janitor) #for cleaning names
library(tidyverse) #for wrangling and plotting
library(patchwork) # for multipanel plotting
library(lubridate) #for transitioning posix to date

# load data from Microsoft Teams/Sharepoint ####

#identify which site I need data from
site <- get_sharepoint_site("DNRP Beavers")

# default is the document library
drv <- site$get_drive()

#This downloads the file to the project folder (I had to know the pathway from Teams/Sharepoint)
drv$download_file("Upper Green/03_Data/vegetation/CharleyCreek_Vegetation_2022_2023.xlsx")

#Because this downloads to my project working directory, I will read in the data, then delete the raw file shortly. I do not want any versions of the data stored in my project. They need to stay in sharepoint.

# Review the sheet names in order to select the correct one.  
excel_sheets("CharleyCreek_Vegetation_2022_2023.xlsx")

# read in the data, all sheets
spcov_df<-read_excel("CharleyCreek_Vegetation_2022_2023.xlsx", 
                     sheet = "DATA_SPCOVER", 
                     .name_repair = make_clean_names)

totcov_df<-read_excel("CharleyCreek_Vegetation_2022_2023.xlsx", 
                       sheet = "DATA_TOTALCOVER", 
                       .name_repair = make_clean_names)

trnk_df<-read_excel("CharleyCreek_Vegetation_2022_2023.xlsx", 
                        sheet = "DATA_TRUNK", 
                        .name_repair = make_clean_names)

veglst_df<-read_excel("CharleyCreek_Vegetation_2022_2023.xlsx", 
                    sheet = "VegetationList", 
                    .name_repair = make_clean_names)

#Removes the file from my working directory. 
file.remove("CharleyCreek_Vegetation_2022_2023.xlsx")

# check files with quick peek ####
head(spcov_df)
head(totcov_df)
head(trnk_df)
head(veglst_df)

# create usable tidy dataset ####
# join data tables to get species common and scientific names 
spcover <- left_join(spcov_df, veglst_df, by = "species_code")

#convert trace to 0.5% for plotting
spcover$percent_cover_num<-str_replace(spcover$percent_cover, "trace", ".5" ) 

#convert column to numeric
spcover$percent_cover_num <- as.numeric(spcover$percent_cover_num) 

#create an extra column with the transect/dist combo
spcover$circle_name <- str_sub(spcover$plot_id, start = 6, end = 9)

#drop time from the observation data
spcover$observation_date.only <-as_date(spcover$observation_date)

##write to csv for visualizations and quarto ####
write_csv(x = spcover,file = "data/spcover.csv")

# join data tables to get species common and scientific names for trees
trnk <- left_join(trnk_df, veglst_df, by = "species_code")

#create an extra column with the transect/dist combo
trnk$circle_name <- str_sub(trnk$plot_id, start = 6, end = 9)

#convert sapling to 0.25 for plotting
trnk$dbh_inches_num<-str_replace(trnk$dbh_inches, "sapling", ".25" ) 

#convert column to numeric
trnk$dbh_inches_num <- as.numeric(trnk$dbh_inches_num) 

#get dist from spcover
plotinfo <-spcover %>% select(plot_id, dist, reach, question, treatment, transect) %>% unique()

trnk <- left_join(trnk, plotinfo, by = "plot_id")

##write to csv for visualizations and quarto ####
write_csv(x = trnk,file = "data/trnk.csv")


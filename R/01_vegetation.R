#=== === === === === === === ===
# Started September 30, 2022
# Rebekah Stiling, rstiling@kingcounty.gov
# This script explores a preliminary set of vegetation data
#=== === === === === === === ===

# load relevant packages
library(Microsoft365R) #for accessing sharepoint
library(readxl) #for reading excel files
library(janitor) #for cleaning names
library(tidyverse) #for wrangling and plotting

#identify which site I need data from
site <- get_sharepoint_site("DNRP Beavers")

# default is the document library
drv <- site$get_drive()

#This downloads the file to the project folder (I had to know the pathway from Teams/Sharepoint)
drv$download_file("Upper Green/03_Data/vegetation/Vegetation Monitoring Data.xlsx")

#Because this downloads to my project working directory, I will read in the data, then delete the raw file shortly. I do not want any versions of the data stored in my project. They need to stay in sharepoint.

# Review the sheet names in order to select the correct one.  
excel_sheets("Vegetation Monitoring Data.xlsx")

# read in the data, all sheets
spcov_df<-read_excel("Vegetation Monitoring Data.xlsx", 
                     sheet = "DATA_SPCOVER", 
                     .name_repair = make_clean_names)

totcov_df<-read_excel("Vegetation Monitoring Data.xlsx", 
                       sheet = "DATA_TOTALCOVER", 
                       .name_repair = make_clean_names)

trnk_df<-read_excel("Vegetation Monitoring Data.xlsx", 
                        sheet = "DATA_TRUNK", 
                        .name_repair = make_clean_names)

veglst_df<-read_excel("Vegetation Monitoring Data.xlsx", 
                    sheet = "VegetationList", 
                    .name_repair = make_clean_names)

#Removes the file from my working directory. 
file.remove("Vegetation Monitoring Data.xlsx")

head(spcov_df)
head(totcov_df)
head(trnk_df)
head(veglst_df)

spcover <- left_join(spcov_df, veglst_df, by = "species_code")

head(spcover) #check info.

spcover %>% filter(is.na(common_name)) %>% pull(species_code) %>% unique()

# QA/QC
# Do species code groups and common name groups match?
spcover %>% 
  group_by(species_code) %>% 
  summarise(freq = n()) 

spcover %>% 
  group_by(common_name) %>% 
  summarise(freq = n()) 

#what species don't have a common name?
spcover %>% 
  select(resort, plot_id, scientific_name, common_name) %>% 
  filter(is.na(common_name))

#plot the frequency of occurrence by code (#code from )
spcover %>% 
  group_by(species_code) %>%
  summarise(freq = n()) %>% 
  ggplot(aes(x = reorder(species_code, freq), 
             y = freq)) + 
  geom_col(alpha = 0.85, show.legend = FALSE) +     
  labs(y = "count of observations",
       x = NULL) + 
  theme_minimal() +  
  theme(axis.text.x = element_text(size = 8)) + 
  coord_flip()

#plot the frequency of occurrence with common names (Beka's code (difference is geom_bar vs geom_col)
p1<-spcover %>% 
  group_by(common_name) %>%
  summarise(freq = n()) %>% 
  drop_na() %>% 
  ggplot(aes(x = reorder(common_name, freq), y = freq)) +
  geom_bar(stat = "identity") + 
  labs(y = "count of observations",
       x = NULL) + 
  scale_y_continuous(breaks=seq(0,28,by=2))+
  coord_flip() + 
  theme_minimal() 
p1

ggsave("figs/plant observation frequency.png",p1,  width = 6, height = 8, units = "in" )

#Obtain list of plants for next data sheet.
observations<-spcover %>% 
  group_by(species_code) %>%
  summarise(freq = n())

obs_fullnames <-left_join(observations, veglst_df, by = "species_code")
write_csv(obs_fullnames, "tables/common plant species-2023-11-29.csv")


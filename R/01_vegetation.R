#=== === === === === === === ===
# Started September 30, 2022
# Rebekah Stiling, rstiling@kingcounty.gov
# This script explores a preliminary set of vegetation data
#=== === === === === === === ===

# Load relevant packages
library(tidyverse)  # for data wrangling, analysis, and plotting
library(readxl) #To read the excel file

xcelfilename <- "data/Vegetation Monitoring Data_2023-10-24.xlsx"

# Review the sheet names in order to select the correct one.  
excel_sheets(xcelfilename)

# read in the worksheet with the isotope data
cover <- read_excel(xcelfilename, sheet = "DATA_SPCOVER")
vegnames <- read_excel(xcelfilename, sheet = "VegetationList")

spcover <- left_join(cover, vegnames, by = "species_code")

head(spcover) #check info.

spcover %>% filter(is.na(common_name)) %>% pull(species_code)

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

obs_fullnames <-left_join(observations, vegnames, by = "species_code")
write_csv(obs_fullnames, "tables/common plant species-2023-10-24.csv")


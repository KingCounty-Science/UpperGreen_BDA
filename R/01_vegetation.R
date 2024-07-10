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
library(patchwork) # for multipanel plotting
library(lubridate) #for transitioning posix to date

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

spcover$percent_cover_num<-str_replace(spcover$percent_cover, "trace", "1" ) #convert trace to 1% for plotting
spcover$percent_cover_num <- as.numeric(spcover$percent_cover_num) #convert column to numeric

#create an extra column with the transect/dist combo
spcover$circle_name <- str_sub(spcover$plot_id, start = 6, end = 9)

spcover$observation_date.only <-as_date(spcover$observation_date.x)

## WRITE TO CSV FOR QUARTO ####
write_csv(x = spcover,file = "data/spcover.csv")

# summary of events
spcover  %>%  
  select(reach, plot_id)   %>% 
  unique() %>% 
  group_by( reach) %>% 
  summarise(freq = n()) %>% 
  filter(freq != 24) %>% 
  pull(reach)

#plot the frequency of occurrence by code 
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

#plot the frequency of occurrence with common names (difference is geom_bar vs geom_col)
p1<-spcover %>% 
  group_by(common_name) %>%
  summarise(freq = n()) %>% 
  drop_na() %>% 
  ggplot(aes(x = reorder(common_name, freq), y = freq)) +
  geom_bar(stat = "identity") + 
  labs(y = "count of observations",
       x = NULL) + 
  scale_y_continuous(breaks=seq(0,100,by=5))+
  coord_flip() + 
  theme_minimal() 
p1

ggsave("figs/plant observation frequency_common.tiff",p1,  width = 7.5, height = 8, units = "in" )

p2<-spcover %>% 
  group_by(scientific_name) %>%
  summarise(freq = n()) %>% 
  drop_na() %>% 
  ggplot(aes(x = reorder(scientific_name, freq), y = freq)) +
  geom_bar(stat = "identity") + 
  labs(y = "count of observations",
       x = NULL) + 
  scale_y_continuous(breaks=seq(0,100,by=5))+
  coord_flip() + 
  theme_minimal() 
p2

ggsave("figs/plant observation frequency_sci.tiff",p2,  width = 7.5, height = 8, units = "in" )

#capture the reordered list:
spec_arranged<-spcover %>% 
  group_by(scientific_name) %>%
  summarise(freq = n()) %>% 
  drop_na() %>%
  arrange(freq) %>% 
  pull(scientific_name)

#Obtain list of plants for next data sheet.
observations<-spcover %>% 
  group_by(species_code) %>%
  summarise(freq = n())

obs_fullnames <-left_join(observations, veglst_df, by = "species_code")
write_csv(obs_fullnames, "tables/common plant species-2024-07-03.csv")


ggplot(data = spcover %>% 
         group_by(reach, common_name) %>% 
         summarise(freq = n()) %>% 
         drop_na(),
       aes(x = reorder(common_name, freq), y = freq)) +
  geom_bar(stat = "identity") + 
  labs(y = "count of observations",
       x = NULL) + 
  scale_y_continuous(breaks=seq(0,28,by=2))+
  coord_flip() + 
  theme_minimal() +
  facet_grid(cols = vars(reach))
ggsave("figs/plant observation frequency_reach_common.tiff",  width = 10, height = 8, units = "in" )


ggplot(data = spcover %>% 
         group_by(reach, scientific_name) %>% 
         summarise(freq = n()) %>% 
         drop_na(),
       aes(x = reorder(scientific_name, freq), y = freq)) +
  geom_bar(stat = "identity") + 
  labs(y = "count of observations",
       x = NULL) + 
  scale_y_continuous(breaks=seq(0,28,by=2))+
  coord_flip() + 
  theme_minimal() +
  facet_grid(cols = vars(reach))
ggsave("figs/plant observation frequency_reach_sci.tiff",  width = 10, height = 8, units = "in" )

#what about distance from water's edge
ggplot(data = spcover %>% 
         group_by(dist, scientific_name) %>% 
         summarise(freq = n()) %>% 
         drop_na(),
       aes(x = reorder(scientific_name, freq), y = freq)) +
  geom_bar(stat = "identity") + 
  labs(y = "count of observations",
       x = NULL) + 
  scale_y_continuous(breaks=seq(0,28,by=2))+
  coord_flip() + 
  theme_minimal() +
  facet_grid(cols = vars(dist))
ggsave("figs/plant observation frequency_dist_sci.tiff",  width = 10, height = 8, units = "in" )

##The histograms track the frequently occurring species.
## What about the most abundant species? How often do species cover a lot of the plot?

spcover %>% 
  filter(vegetation_type != "tree" | is.na(vegetation_type)) %>% 
  filter(percent_cover_num > 30) %>% 
  group_by(common_name) %>% 
  summarise(freq = n()) %>% 
  drop_na() %>% 
  ggplot(aes(x = reorder(common_name, freq), y = freq)) +
  geom_bar(stat = "identity") + 
  labs(y = "count of observations",
       x = NULL) + 
  scale_y_continuous(breaks=seq(0,100,by=5))+
  coord_flip() + 
  theme_minimal() 

# what is the average amount of cover for a common species ####

#first, what are the most commonly occuring species that are trees and that are not trees
#trees 
treelist<-spcover %>% 
  filter(vegetation_type == "tree") %>% 
  group_by(scientific_name) %>%
  summarise(freq = n()) %>% 
  drop_na() %>% 
  pull(scientific_name )
#there are only 8 trees, so we could filter for these 
spcover_trees<-spcover %>% 
  filter(scientific_name %in% treelist)

#what about making a percent cover plot that mirrors the frequency of the occurance
A<-ggplot(spcover %>% 
         drop_na(scientific_name) %>% 
         mutate(scientific_name = fct_relevel(scientific_name, spec_arranged)),
       aes(x = scientific_name,  y = percent_cover_num)) + 
  geom_boxplot() +
  coord_flip() +
  labs(y = "percent cover (%)",
       x = "species") +
  theme_bw()
A
ggsave("figs/avecov_sci.tiff", A, width = 10, height = 8, units = "in" )

B <-p2 + scale_y_continuous(breaks=seq(0,100,by=10)) + labs(y = "count of observations",
                                                              x = "species")
C <- A + labs(y = "percent cover (%)",
                x = NULL)
B + C + plot_annotation(tag_levels = "A")
ggsave("figs/freq_ave_sci.tiff", width = 10, height = 10, units = "in" )

ggplot(spcover_trees, aes(x = reorder(scientific_name, percent_cover_num),  y = percent_cover_num)) + 
  geom_boxplot() +
  facet_grid(~dist) + 
  coord_flip() +
  labs(y = "percent cover (%)",
       x = "tree species") +
  theme_bw()

ggplot(spcover, aes(x = reorder(scientific_name, percent_cover_num),  y = percent_cover_num)) + 
  geom_boxplot() +
  facet_grid(~dist) + 
  coord_flip() +
  labs(y = "percent cover (%)",
       x = "tree species") +
  theme_bw()


#start looking at the breakdown of species on the ground
ggplot(spcover, aes(x = "", y = percent_cover_num, fill = common_name)) + 
  geom_bar(stat = "identity", width = 1, position = position_fill()) +
  coord_polar(theta = "y") + 
  facet_grid(cols = vars(reach), vars(dist))

ggplot(spcover, aes(x = "", y = percent_cover_num, fill = common_name)) + 
  geom_bar(stat = "identity", width = 1, position = position_fill()) +
  coord_polar(theta = "y") + 
  facet_grid(cols = vars(reach), vars(transect))

ggplot(spcover, aes(x = "", y = percent_cover_num, fill = common_name)) + 
  geom_bar(stat = "identity", width = 1, position = position_fill()) +
  coord_polar(theta = "y") + 
  facet_grid(vars(reach), vars(circle_name))

ggplot(spcover, aes(x = "", y = percent_cover_num, fill = common_name)) + 
  geom_bar(stat = "identity", width = 1, position = position_fill()) +
  coord_polar(theta = "y") + 
  facet_wrap(~plot_id)

#geom_col adds up th
ggplot(spcover, aes(x = "", y = percent_cover_num, fill = common_name)) + 
  geom_col() +
  facet_grid(cols = vars(reach), vars(transect))

#geom_col adds up th
ggplot(spcover, aes(x = plot_id, y = percent_cover_num, fill = common_name)) + 
  geom_col() +
  facet_grid(cols = vars(reach), vars(dist()))

#what if we converted the things that we see in trace amounts to "other" and then highlighted the most common things


# I think this could be a good place for patchwork. I make 4 sets of 24 charts then combine.

ggplot(data = spcover %>% filter(reach == "WillowNorth"), 
       aes(x = common_name, y = percent_cover_num)) +
  geom_col() +
  facet_wrap(~circle_name, ncol = 6)


### Total Cover ####
plot_info <-spcover %>% select(plot_id, region, reach, question, treatment, transect, dist, circle_name) %>% unique()

totcov_df %>%
  group_by_all() %>%
  filter(n()>1)

totcov<-left_join(x = totcov_df, y = plot_info, by = "plot_id")

head(totcov_df)

plot_info %>%
  group_by(plot_id) %>%
  filter(n()>1)

ggplot(totcov, 
       aes(x = dist,  y = total_cover )) + 
  geom_boxplot(aes(group = cut_width(dist, 50))) + # I need this line otherwise it blends the numeric data
  labs(y = "overal canopy cover (%)",
       x = "distance from water's edge (ft) ") +
  scale_x_continuous(breaks=(c(20, 75, 130))) +
  theme_bw()

ggsave("figs/canopy_cover_dist.tiff", width = 6, height = 6, units = "in" )

ggplot(totcov, 
       aes(x = dist,  y = total_cover )) + 
  geom_boxplot(aes(group = cut_width(dist, 50))) + # I need this line otherwise it blends the numeric data
  labs(y = "overal canopy cover (%)",
       x = "distance from water's edge (ft) ") +
  scale_x_continuous(breaks=(c(20, 75, 130))) +
  facet_grid(~reach)+
  theme_bw()

ggplot(totcov, 
       aes(x = dist,  y = total_cover )) + 
  geom_point() + # I need this line otherwise it blends the numeric data
  labs(y = "overal canopy cover (%)",
       x = "distance from water's edge (ft) ") +
  scale_x_continuous(breaks=(c(20, 75, 130))) +
  facet_grid(~reach)+
  theme_bw()


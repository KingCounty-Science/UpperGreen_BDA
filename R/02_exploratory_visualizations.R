#=== === === === === === === ===
# Started September 30, 2022
# Rebekah Stiling, rstiling@kingcounty.gov
# This script explores a preliminary set of vegetation data.
# It also contains the plots that were included in the vegetation report submitted to WA Dept of Ecology
#=== === === === === === === ===

# load relevant packages ####
library(tidyverse) #for wrangling and plotting
library(patchwork) # for multipanel plotting


## read in the tidy data ####
spcover<-read_csv(file = "data/spcover.csv")

# Sampling Events ####
# summary of events
spcover  %>%  
  select(reach, plot_id)   %>% 
  unique() %>% 
  group_by( reach) %>% 
  summarise(freq = n()) 

# Richness ####
# How many species were observed?
unique_species<-spcover %>% 
  select(species_code) %>%
  unique() %>% 
  count() %>% 
  pull()
unique_species

# How many species occurred in 75% of the plots or more?
spcover %>% 
  group_by(species_code) %>%
  summarise(freq = n()) %>% 
  filter(freq >= (unique_species*.75)) 

# Howe many species occurred only once?
spcover %>% 
  group_by(species_code) %>%
  summarise(freq = n()) %>% 
  filter(freq <= (1)) 

# How many species were in each plot on average, min, and max?
n_plotspecies<- spcover %>% 
  select(plot_id, species_code) %>% 
  group_by(plot_id) %>% 
  summarise(n_plotspecies = n_distinct(species_code))

n_plotspecies

mean(n_plotspecies$n_plotspecies)
range(n_plotspecies$n_plotspecies)

# How often was salmonberry observed and where?
spcover %>% 
  filter(scientific_name == "Rubus spectabilis") %>%
  group_by(reach) %>% 
  summarise(freq = n()) 


# Plots for write up. ####
#capture a list ordered by frequency for plotting.
spec_arranged<-spcover %>% 
  group_by(scientific_name) %>%
  summarise(freq = n()) %>% 
  drop_na() %>%
  arrange(freq) %>% 
  pull(scientific_name)

#isolate the tree/shrub etc lables
vegtypelist <- spcover %>% 
  select(scientific_name, vegetation_type) %>% 
  unique()

# joined frequency and percent cover plots.
A<-spcover %>% 
  group_by(scientific_name) %>%
  summarise(freq = n()) %>% 
  drop_na() %>%
  left_join(vegtypelist) %>% 
  ggplot(aes(x = reorder(scientific_name, freq), y = freq, fill = vegetation_type)) +
  geom_bar(stat = "identity") + 
  scale_fill_manual(values = "#0A6522") +
  labs(y = "count of observations",
       x = NULL) + 
  scale_y_continuous(breaks=seq(0,100,by=5))+
  coord_flip() + 
  theme_minimal() + 
  scale_y_continuous(breaks=seq(0,100,by=10)) + 
  theme(text=element_text(size = 9),
        legend.position="none",#drop legend
        axis.text.y = element_text(face = "italic")) + 
  labs(y = "count of observations",
       x = "species")
A
B<-ggplot(spcover %>% 
            drop_na(scientific_name) %>% 
            mutate(scientific_name = fct_relevel(scientific_name, spec_arranged)),
          aes(x = scientific_name,  y = percent_cover_num)) + 
  geom_boxplot() +
  coord_flip() +
  labs(y = "percent cover (%)",
       x = "species") +
  theme_minimal() + 
  theme(text=element_text(size = 9),
        axis.title.y=element_blank(),
        axis.text.y=element_blank()) +
  labs(y = "percent cover (%)",
                    x = NULL)
B

A + B + plot_annotation(tag_levels = "A") 
ggsave("figs/freq_ave_sci.tiff", width = 6.5, height = 8, units = "in" )

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
#ggsave("figs/plant observation frequency_reach_common.tiff",  width = 10, height = 8, units = "in" )

# additional plots for exploration. ####
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
#ggsave("figs/plant observation frequency_reach_sci.tiff",  width = 10, height = 8, units = "in" )

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
#ggsave("figs/plant observation frequency_dist_sci.tiff",  width = 10, height = 8, units = "in" )

##The histograms track the frequently occurring species.

## What about the most abundant species? How often do species cover a lot of the plot?
spcover %>% 
  filter(vegetation_type != "tree" | is.na(vegetation_type)) %>% 
  filter(percent_cover_num > 30) %>% 
  group_by(scientific_name) %>% 
  summarise(freq = n()) %>% 
  drop_na() %>% 
  ggplot(aes(x = reorder(scientific_name, freq), y = freq)) +
  geom_bar(stat = "identity") + 
  labs(y = "count of observations where % cover exceeding 50%",
       x = NULL) + 
  scale_y_continuous(breaks=seq(0,100,by=5))+
  coord_flip() + 
  theme_minimal() 

# what is the average amount of cover for a common species ####

#first, what are the most commonly occurring species that are trees and that are not trees
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

#geom_col adds up th
ggplot(spcover, aes(x = "", y = percent_cover_num, fill = common_name)) + 
  geom_col() +
  facet_grid(cols = vars(reach), vars(transect))

#geom_col adds up th
ggplot(spcover, aes(x = plot_id, y = percent_cover_num, fill = common_name)) + 
  geom_col() +
  facet_grid(cols = vars(reach), vars(dist()))

# I think this could be a good place for patchwork. I make 4 sets of 24 charts then combine.

ggplot(data = spcover %>% filter(reach == "WillowNorth"), 
       aes(x = common_name, y = percent_cover_num)) +
  geom_col() +
  facet_wrap(~circle_name, ncol = 6)

# Tree Sizes ####
## read in the tidy tree data ####
trnk <-read_csv(file = "data/trnk.csv")

trnk %>% select(scientific_name, common_name) %>% unique() 

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

cov_max <- spcover %>% 
  select(percent_cover_num) %>% 
  max()

spcover %>% 
  filter(percent_cover == "trace") %>% 
  select(scientific_name) %>% 
  distinct()

spcover %>% 
  filter(percent_cover == "1") %>% 
  select(scientific_name) %>% 
  distinct()



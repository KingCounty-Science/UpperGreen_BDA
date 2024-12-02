#=== === === === === === === ===
# Started September 30, 2022
# Rebekah Stiling, rstiling@kingcounty.gov
# This script explores a preliminary set of vegetation data.
# It also contains the plots that were included in the vegetation report submitted to WA Dept of Ecology
#=== === === === === === === ===

# load relevant packages ####
library(tidyverse) #for wrangling and plotting
library(patchwork) # for multipanel plotting
library(viridis) # for colors


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

# If we only look at the plots in the 20 and 75 foot plots, what is the richness?

close_unique_species <- spcover %>% 
  filter(dist != 130) %>% 
  select(scientific_name) %>%
  unique() %>% 
  count() %>% 
  pull()
close_unique_species

closest_unique_species <- spcover %>% 
  filter(dist == 20) %>% 
  select(scientific_name) %>%
  unique() %>% 
  count() %>% 
  pull()
closest_unique_species

#What are the species that were in the 130 plots that were not in the 75+20 foot plots?
specieslist_20_75 <-spcover %>% 
  filter(dist != 130) %>% 
  select(scientific_name) %>%
  unique()

specieslist_all <-spcover %>% 
  select(scientific_name) %>%
  unique()

anti_join(specieslist_all, specieslist_20_75) %>%  pull()

#which were down to sp.?
sp_species<-spcover %>% 
  select(scientific_name) %>%
  unique() %>% 
  pull() %>% 
  str_subset(pattern = "sp.")

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

spcover %>% 
  filter(scientific_name == "Thuja plicata") %>%
  group_by(reach) %>% 
  summarise(freq = n()) 

spcover %>% 
  filter(scientific_name == "Alnus rubra") %>%
  group_by(reach) %>% 
  summarise(freq = n()) 

bevpref_tbl<-spcover %>% 
  filter(scientific_name == "Alnus rubra"|
           scientific_name == "Thuja plicata" |
           scientific_name == "Rubus spectabilis") %>%
  group_by(reach, scientific_name) %>% 
  summarise(freq = n()) %>%
  pivot_wider(names_from = scientific_name, values_from = freq)

overlap_tle <-spcover %>% 
  filter(scientific_name == "Alnus rubra"|
           scientific_name == "Thuja plicata" |
           scientific_name == "Rubus spectabilis") %>%
  group_by(reach, plot_id) %>% 
  summarise(freq = n()) %>% 
  group_by(reach, freq) %>% 
  summarise(freq2 = n()) %>% 
  pivot_wider(names_from = freq, values_from = freq2) %>% 
  rename("one" = "1","two" = "2","three" = "3") %>% 
  mutate(tot = one + two + !is.na(three))
  

bevpref <- (left_join(bevpref_tbl, overlap_tle, by = "reach" ))

write_csv(bevpref, "tables/prefered_sitefrequency.csv")

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

#first, what are the most commonly occurring species that are trees and that are not trees

#how many trees were in each plot? 
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

spcover_trees %>% 
  group_by(plot_id) %>% 
  summarise(freq = n()) %>% 
  filter(freq <= 3) 

#what species are in the groups of four?
foursp_list<-spcover_trees %>% 
  group_by(plot_id) %>% 
  summarise(freq = n()) %>% 
  filter(freq == 4) %>% 
  pull(plot_id)

spcover_trees %>% 
  filter(plot_id %in% foursp_list) %>% 
  group_by(plot_id) %>% 
  select(plot_id, scientific_name) %>% 
  filter(scientific_name == "Thuja plicata")

spcover_trees %>% 
  filter(plot_id == "CCEBCF075") %>% 
  select(scientific_name)

#Coverage


ggplot(spcover_trees, aes(x = reorder(scientific_name, percent_cover_num),  y = percent_cover_num)) + 
  geom_boxplot() +
  geom_jitter(width =.1, alpha = .3) +
  facet_grid(~dist) + 
  coord_flip() +
  labs(y = "Percent cover (%)",
       x = "Tree species") +
  theme_bw() +
  theme(text=element_text(size = 9),
        axis.text.y = element_text(face = "italic"))

ggsave("figs/treecover_by dist.tiff",  width = 6.5, height = 4, units = "in" )



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

#what were the trunk sizes for plots in the 20-ft area?
#excluding saplings
near20<- trnk %>% select(scientific_name, dbh_inches, dbh_inches_num, dist) %>% filter(dist == "20" & dbh_inches != "sapling")

# number of saplings along water's edge
trnk %>% 
  select(scientific_name, dbh_inches, dbh_inches_num, sapling_count, dist) %>% 
  filter(dist == "20" & dbh_inches == "sapling") %>% 
  group_by(scientific_name) %>% 
  summarise(saps = sum(sapling_count))

near20 %>% 
  summarise(frq = n(), 
            mean = mean(dbh_inches_num),
            median_trunk = median(dbh_inches_num))

ggplot(near20, aes(x = dbh_inches_num, fill = scientific_name)) +
  geom_histogram(binwidth = 2, boundary = 0, closed = "left") +
  theme_minimal() +
  scale_fill_viridis(option="viridis", discrete = TRUE, direction = -1) +
  labs(x = "d.b.h. (inches)",
       y = "number of trees",
       fill = "Scientific name") +
  scale_y_continuous(breaks = seq(0, 10, 2), limits = c(0, 10))

ggsave("figs/treeDBH_20m_hist.tiff",  width = 6.5, height = 2.5, units = "in" )
range(trnk$dbh_inches_num) 
trunk_sizes<-trnk %>% 
  filter(dbh_inches != "sapling") %>% 
  group_by(scientific_name) %>% 
  summarise(frq = n(),
            min_trunk = min(dbh_inches_num),
            max_trunk = max(dbh_inches_num),
            mean_trunk = mean(dbh_inches_num),
            median_trunk = median(dbh_inches_num))
trunk_sizes
write_csv(trunk_sizes, "data/trunk_summary.csv")

trnk$dist_fact <- as.factor(trnk$dist)

ggplot(trnk %>% 
         filter(dbh_inches != "sapling"), 
       aes(x = scientific_name, y = dbh_inches_num)) +
  geom_boxplot() +
  geom_jitter(width =.2, 
              alpha = .5, 
              size = 2,
              aes(color = dist_fact)) +
  labs(y = "DBH (inches)",
       x = "Tree species",
       color = "Plot center (ft)") +
  theme_minimal() +
  theme(text=element_text(size = 9),
        axis.text.x = element_text(face = "italic",
                                   angle = 15)) +
  scale_color_manual(values = c("#7570b3", "#d95f02", "#1b9e77"))

ggsave("figs/treeDBH_by species.tiff",  width = 6.5, height = 4, units = "in" )

#tree dbh by reach and distance from water's edge
ggplot(trnk %>% 
         filter(dbh_inches != "sapling"), 
       aes(x = dist,  y = dbh_inches_num)) + 
  geom_boxplot(aes(group = cut_width(dist, 50))) + # I need this line otherwise it blends the numeric data
  geom_jitter(width =3, alpha = .3, color = "#0A6522") +
  labs(y = "d.b.h. (inches)",
       x = "Distance from water's edge (ft) ") +
  scale_x_continuous(breaks=(c(20, 75, 130))) +
  facet_grid(~reach)+
  theme_bw()
ggsave("figs/dbh_dist_reach.tiff", width = 6.5, height = 4, units = "in" )


saplings<-trnk %>% 
  filter(dbh_inches == "sapling") %>% 
  group_by(scientific_name) %>% 
  summarise(tot_saps = sum(sapling_count))
saplings

trnk %>% 
  filter(dbh_inches == "sapling") %>% 
  group_by(plot_id) %>% 
  summarise(tot_saps = sum(sapling_count)) %>% 
  filter(tot_saps > 5)

## Trunk diameter statistics ####

#did the trunk diameters vary according to distance from water's edge overall?
#single factor anova
# I need to turn the distance into a factor. 
fact_dbh_dist <- trnk %>% 
  filter(dbh_inches != "sapling") %>% 
  select(dist, dbh_inches_num, scientific_name, reach) %>% 
  mutate(dist_fct = as_factor(dist))

#rename as factor terms
fact_dbh_dist$dist_fct_name <- recode(fact_dbh_dist$dist_fct, "20" = "near", 
                       "75" = "mid",
                       "130" = "far")

#anova with distance as numeric
aov_dist <- aov(formula = dbh_inches_num ~ dist, data = fact_dbh_dist )
summary(aov_dist)

#anova with distance as factor
aov_dist_fact <- aov(formula = dbh_inches_num ~ dist_fct_name, data = fact_dbh_dist)
summary(aov_dist_fact)

#anova with reach as a factor
aov_reach <- aov(formula = dbh_inches_num ~ reach, data = fact_dbh_dist )
summary(aov_reach)
## Tukey to compare
TukeyHSD(aov_reach, conf.level = .95)

# Total Cover ####
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

totcov %>% 
  group_by(dist ) %>% 
  summarise(avecov = mean(total_cover),
            medcov = median(total_cover))

totcov %>% select(total_cover) %>% pull() %>% mean()
totcov %>% select(total_cover) %>% pull() %>% median()
totcov %>% select(total_cover) %>% pull() %>% min()
totcov %>% select(total_cover) %>% pull() %>% max()

ggplot(totcov, 
       aes(x = dist,  y = total_cover )) + 
  geom_boxplot(aes(group = cut_width(dist, 50))) + # I need this line otherwise it blends the numeric data
  geom_jitter(width =3, alpha = .3, color = "#0A6522") +
  labs(y = "Overall canopy cover (%)",
       x = "Distance from water's edge (ft) ") +
  scale_x_continuous(breaks=(c(20, 75, 130))) +
  facet_grid(~reach)+
  theme_bw()

ggsave("figs/canopy_cover_dist_reach.tiff", width = 6.5, height = 4, units = "in" )

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

## Ideas for later
# Are there metrics that we can calculate for each plot? If so, then we can plot on an x-y graph. Such as: total species and total canopy cover. 





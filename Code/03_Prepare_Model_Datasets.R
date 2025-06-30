#### CONTACT ####
# Courtney Stuart (courtney.e.stuart@gmail.com; courtney.stuart@mansfield.ox.ac.uk)

#### LIBRARIES ####
# install packages (first run only)
# install.packages(c("easypackages", "conflicted", "dplyr", "here", "forcats", "lubridate",
#                    "stringr", "tidyr", "ggplot2", "RColorBrewer", "lme4", "lmerTest",
#                    "raster", "sf", "sp", "terra", "PNWColors", "usdm", "corrplot",
#                    "purrr", "MuMIn", "ggplot2", "car", "spdep", "lmtest", "MASS"))

# load all libraries at once with easypackages
library(easypackages)
libraries("easypackages", "conflicted", "dplyr", "here", "forcats", "lubridate",
          "stringr", "tidyr", "ggplot2", "RColorBrewer", "raster", "sf", "sp",
          "terra", "PNWColors", "ggplot2","spdep", "purrr")
conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")
conflict_prefer("vifcor", "usdm")
conflict_prefer("vifstep", "usdm")
conflict_prefer("vif", "usdm")

#### DIRECTORIES ####
# working directory and relative folder path
setwd("E:/Data/StuartC_DPhil_Ch2/")
#set_here("E:/Data/StuartC_DPhil_Ch2/") #set first-time only
here::i_am(".here")
here::here() # verify where we are according to the here package

#### COLOR PALETTES ####
lake = pnw_palette(name = "Lake", n = 8, type = "discrete")
bioeroder_col = lake[6]
scraper_col = lake[5]
# browser_col = lake[2]
browser_col = "#7F4F24"
grazer_col = lake[1]
territorial_col = lake[3]

#### LOAD FISH DATA ####
load(here("Code", "01_Combine_Fish_Data.RData"))

#### SIMPLE HERBIVORE SUMMARY STATS ####
# how many herbivorous fish did we record?
sum(herbs$Abundance)

# how many species did we record?
length(unique(herbs$Species))

# how many families did we record and what were they?
length(unique(herbs$Family))
unique(herbs$Family)

# who were the top five most abundant species?
herbs %>%
  count(Species, sort = TRUE) %>%
  slice_max(n = 5, order_by = n)

# how many transects was each functional group spotted on?
herbs %>%
  filter(Functional_Group == "Territorial algae/detritus feeders") %>%
  summarise(n_transects = n_distinct(Transect_ID))

herbs %>%
  filter(Functional_Group == "Small excavators/scrapers") %>%
  summarise(n_transects = n_distinct(Transect_ID))

herbs %>%
  filter(Functional_Group == "Grazers/detritivores") %>%
  summarise(n_transects = n_distinct(Transect_ID))

#### LOAD TRANSECT CONDITIONS ####
transects_150m = read.csv(here("Data", "Transects", "Conditions_150m2_Transects.csv"))
transects_60m = read.csv(here("Data", "Transects", "Conditions_60m2_Transects.csv"))

# add motu info
transects_150m = transects_150m %>%
  mutate(
  Motu = case_when(
    # special cases where Motu is not the first segment
    grepl("^Hiraanae_Auroa", Transect_ID) ~ "Auroa",
    grepl("^Tauini_Auroa", Transect_ID) ~ "Tauini",
    # default case: take the first segment before the first underscore
    TRUE ~ sub("_.*", "", Transect_ID))) %>%
  relocate(Motu, .before = Transect_ID)

transects_60m = transects_60m %>%
  mutate(
    Motu = case_when(
      # special cases where Motu is not the first segment
      grepl("^Hiraanae_Auroa", Transect_ID) ~ "Auroa",
      grepl("^Tauini_Auroa", Transect_ID) ~ "Tauini",
      # default case: take the first segment before the first underscore
      TRUE ~ sub("_.*", "", Transect_ID))) %>%
  relocate(Motu, .before = Transect_ID)

# how many unique functional groups were observed on each transect?
functional_diversity =
  herbs %>%
  group_by(Transect_ID) %>%
  summarise(Functional_Diversity = n_distinct(Functional_Group))

functional_diversity = left_join(transects_150m, functional_diversity)

# replace NAs with 0 so we know where no herbivores were seen
functional_diversity = functional_diversity %>%
  mutate(Functional_Diversity = replace_na(Functional_Diversity, 0))

# plot functional diversity across the transects
diversity_plot = 
  ggplot(functional_diversity, aes(x = factor(Transect_ID), y = Functional_Diversity)) +
  geom_bar(stat = "identity", fill = "#5dcece", color = "#20a7a7", alpha = 0.7) +  
  labs(x = "Transect ID",
       y = "Herbivore functional diversity") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        axis.line = element_line(color = "black", linewidth = 0.5),
        panel.grid = element_blank()) 
diversity_plot

# how many Grazers/detritivores were observed on each transect?
grazer_abundance =
  herbs %>%
  filter(Functional_Group == "Grazers/detritivores") %>%
  group_by(Transect_ID) %>%
  summarise(Grazer_Abundance = n())

# how many unique Grazers/detritivore species were observed on each transect?
grazer_richness = 
  herbs %>%
  filter(Functional_Group == "Grazers/detritivores") %>%
  group_by(Transect_ID) %>%
  summarise(Grazer_Richness = n_distinct(Species))

# combine the Grazers/detritivore response data with the transect data
grazer_data = left_join(transects_150m, grazer_abundance)
grazer_data = left_join(grazer_data, grazer_richness)

# replace NAs with 0 so we know where no Grazers/detritivores were seen
grazer_data = grazer_data %>%
  mutate(Grazer_Abundance = replace_na(Grazer_Abundance, 0),
         Grazer_Richness = replace_na(Grazer_Richness, 0))

# remove temp items
rm(list = c("grazer_abundance", "grazer_richness"))

# plot grazer abundance and richness across the transects
grazer_abundance_plot = 
  ggplot(grazer_data, aes(x = factor(Transect_ID), y = Grazer_Abundance)) +
  geom_bar(stat = "identity", fill = "#5dcece", color = "#20a7a7", alpha = 0.7) +  
  labs(x = "Transect ID",
       y = "Grazer/detritivore abundance") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        axis.line = element_line(color = "black", linewidth = 0.5),
        panel.grid = element_blank()) 
grazer_abundance_plot

grazer_richness_plot =
  ggplot(grazer_data, aes(x = factor(Transect_ID), y = Grazer_Richness)) +
  geom_bar(stat = "identity", fill = "#5dcece", color = "#20a7a7", alpha = 0.7) +  
  labs(x = "Transect ID",
       y = "Grazer/detritivore richness") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        axis.line = element_line(color = "black", linewidth = 0.5),
        panel.grid = element_blank()) 
grazer_richness_plot

# how many Browsers were observed on each transect?
browser_abundance =
  herbs %>%
  filter(Functional_Group == "Browsers") %>%
  group_by(Transect_ID) %>%
  summarise(Browser_Abundance = n())

# how many unique Browser species were observed on each transect?
browser_richness = 
  herbs %>%
  filter(Functional_Group == "Browsers") %>%
  group_by(Transect_ID) %>%
  summarise(Browser_Richness = n_distinct(Species))

# combine the Browser response data with the transect data
browser_data = left_join(transects_150m, browser_abundance)
browser_data = left_join(browser_data, browser_richness)

# replace NAs with 0 so we know where no Browsers were seen
browser_data = browser_data %>%
  mutate(Browser_Abundance = replace_na(Browser_Abundance, 0),
         Browser_Richness = replace_na(Browser_Richness, 0))

# remove temp items
rm(list = c("browser_abundance", "browser_richness"))

# plot browser abundance and richness across the transects
browser_abundance_plot = 
  ggplot(browser_data, aes(x = factor(Transect_ID), y = Browser_Abundance)) +
  geom_bar(stat = "identity", fill = "#5dcece", color = "#20a7a7", alpha = 0.7) +  
  labs(x = "Transect ID",
       y = "Browser abundance") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        axis.line = element_line(color = "black", linewidth = 0.5),
        panel.grid = element_blank()) 
browser_abundance_plot

browser_richness_plot =
  ggplot(browser_data, aes(x = factor(Transect_ID), y = Browser_Richness)) +
  geom_bar(stat = "identity", fill = "#5dcece", color = "#20a7a7", alpha = 0.7) +  
  labs(x = "Transect ID",
       y = "Browser richness") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        axis.line = element_line(color = "black", linewidth = 0.5),
        panel.grid = element_blank()) 
browser_richness_plot

# how many Small excavators/scrapers were observed on each transect?
scraper_abundance =
  herbs %>%
  filter(Functional_Group == "Small excavators/scrapers") %>%
  group_by(Transect_ID) %>%
  summarise(Scraper_Abundance = n())

# how many unique Small excavators/scraper species were observed on each transect?
scraper_richness = 
  herbs %>%
  filter(Functional_Group == "Small excavators/scrapers") %>%
  group_by(Transect_ID) %>%
  summarise(Scraper_Richness = n_distinct(Species))

# combine the Small excavators/scraper response data with the transect data
scraper_data = left_join(transects_150m, scraper_abundance)
scraper_data = left_join(scraper_data, scraper_richness)

# replace NAs with 0 so we know where no Small excavators/scrapers were seen
scraper_data = scraper_data %>%
  mutate(Scraper_Abundance = replace_na(Scraper_Abundance, 0),
         Scraper_Richness = replace_na(Scraper_Richness, 0))

# remove temp items
rm(list = c("scraper_abundance", "scraper_richness"))

# plot scraper abundance and richness across the transects
scraper_abundance_plot = 
  ggplot(scraper_data, aes(x = factor(Transect_ID), y = Scraper_Abundance)) +
  geom_bar(stat = "identity", fill = "#5dcece", color = "#20a7a7", alpha = 0.7) +  
  labs(x = "Transect ID",
       y = "Small excavator/scraper abundance") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        axis.line = element_line(color = "black", linewidth = 0.5),
        panel.grid = element_blank()) 
scraper_abundance_plot

scraper_richness_plot =
  ggplot(scraper_data, aes(x = factor(Transect_ID), y = Scraper_Richness)) +
  geom_bar(stat = "identity", fill = "#5dcece", color = "#20a7a7", alpha = 0.7) +  
  labs(x = "Transect ID",
       y = "Small excavator/scraper richness") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        axis.line = element_line(color = "black", linewidth = 0.5),
        panel.grid = element_blank()) 
scraper_richness_plot

# how many Large excavators/bioeroders were observed on each transect?
bioeroder_abundance =
  herbs %>%
  filter(Functional_Group == "Large excavators/bioeroders") %>%
  group_by(Transect_ID) %>%
  summarise(Bioeroder_Abundance = n())

# how many unique Large excavators/bioeroder species were observed on each transect?
bioeroder_richness = 
  herbs %>%
  filter(Functional_Group == "Large excavators/bioeroders") %>%
  group_by(Transect_ID) %>%
  summarise(Bioeroder_Richness = n_distinct(Species))

# combine the Large excavators/bioeroder response data with the transect data
bioeroder_data = left_join(transects_150m, bioeroder_abundance)
bioeroder_data = left_join(bioeroder_data, bioeroder_richness)

# replace NAs with 0 so we know where no Large excavators/bioeroders were seen
bioeroder_data = bioeroder_data %>%
  mutate(Bioeroder_Abundance = replace_na(Bioeroder_Abundance, 0),
         Bioeroder_Richness = replace_na(Bioeroder_Richness, 0))

# remove temp items
rm(list = c("bioeroder_abundance", "bioeroder_richness"))

# plot bioeroder abundance and richness across the transects
bioeroder_abundance_plot = 
  ggplot(bioeroder_data, aes(x = factor(Transect_ID), y = Bioeroder_Abundance)) +
  geom_bar(stat = "identity", fill = "#5dcece", color = "#20a7a7", alpha = 0.7) +  
  labs(x = "Transect ID",
       y = "Large excavator/bioeroder abundance") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        axis.line = element_line(color = "black", linewidth = 0.5),
        panel.grid = element_blank()) 
bioeroder_abundance_plot

bioeroder_richness_plot =
  ggplot(bioeroder_data, aes(x = factor(Transect_ID), y = Bioeroder_Richness)) +
  geom_bar(stat = "identity", fill = "#5dcece", color = "#20a7a7", alpha = 0.7) +  
  labs(x = "Transect ID",
       y = "Large excavator/bioeroder richness") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        axis.line = element_line(color = "black", linewidth = 0.5),
        panel.grid = element_blank()) 
bioeroder_richness_plot

# first find Casey's 60m^2 transects that included territorial fish observations
territorial_abundance_60m = herbs %>%
  filter(Functional_Group == "Territorial algae/detritus feeders" & 
           Year == 2021 & Area_m2 == 60 & Observer == "CEB") %>%
  group_by(Transect_ID) %>%
  summarise(Territorial_Abundance = n(), .groups = "drop")

# now make a list of ALL of Casey's 60m^2 transects
casey_transects = herbs %>%
  filter(Year == 2021 & Area_m2 == 60 & Observer == "CEB") %>%
  select(Transect_ID) %>%
  distinct()

# use left join to keep all of Casey's 60m^2 transects, fill any missing Territorial_Abundance 
# rows with 0 (these are transects where territorials were not observed)
territorial_abundance_60m = casey_transects %>%
  left_join(territorial_abundance_60m, by = "Transect_ID") %>%
  mutate(Territorial_Abundance = ifelse(is.na(Territorial_Abundance), 0, Territorial_Abundance))

# first find Kosta's 150m^2 transects that included territorial fish observations
territorial_abundance_150m = herbs %>%
  filter(Functional_Group == "Territorial algae/detritus feeders" & 
           Year == 2023 & Area_m2 == 150 & Observer == "KAS") %>%
  group_by(Transect_ID) %>%
  summarise(Territorial_Abundance = n(), .groups = "drop")

# now make a list of ALL of Kosta's 150m^2 transects
kosta_transects = transects_150m %>%
  filter(!Transect_ID %in% transects_60m$Transect_ID) %>%
  select(Transect_ID) %>%
  distinct()

# use left join to keep all of Kosta's 150m^2 transects, fill any missing Territorial_Abundance 
# rows with 0 (these are transects where territorials were not observed)
territorial_abundance_150m = kosta_transects %>%
  left_join(territorial_abundance_150m, by = "Transect_ID") %>%
  mutate(Territorial_Abundance = ifelse(is.na(Territorial_Abundance), 0, Territorial_Abundance))

# combine the territorial response data with the transect data
territorial_abundance_60m = left_join(territorial_abundance_60m, transects_60m)
territorial_abundance_150m = left_join(territorial_abundance_150m, transects_150m)

# combine all territorial abundance data, regardless of transect area
territorial_abundance = rbind(territorial_abundance_60m, territorial_abundance_150m) %>%
  relocate(Territorial_Abundance, .after = Lat_UTM6S)

# how many unique Territorial algae/detritus feeder species were observed on each 60m^2 transect?
territorial_richness_60m =
  herbs %>%
  filter(Functional_Group == "Territorial algae/detritus feeders" & 
           Year == 2021 & Area_m2 == 60 & Observer == "CEB") %>%
  group_by(Transect_ID) %>%  group_by(Transect_ID) %>%
  summarise(Territorial_Richness = n_distinct(Species))

# left join to keep all transects, filling missing Territorial_Richness with 0
territorial_richness_60m = casey_transects %>%
  left_join(territorial_richness_60m, by = "Transect_ID") %>%
  mutate(Territorial_Richness = ifelse(is.na(Territorial_Richness), 0, Territorial_Richness))

# how many unique Territorial algae/detritus feeder species were observed on each 150m^2 transect?
territorial_richness_150m =
  herbs %>%
  filter(Functional_Group == "Territorial algae/detritus feeders" & 
           Year == 2023 & Area_m2 == 150 & Observer == "KAS") %>%
  group_by(Transect_ID) %>%  group_by(Transect_ID) %>%
  summarise(Territorial_Richness = n_distinct(Species))

# left join to keep all transects, filling missing Territorial_Richness with 0
territorial_richness_150m = kosta_transects %>%
  left_join(territorial_richness_150m, by = "Transect_ID") %>%
  mutate(Territorial_Richness = ifelse(is.na(Territorial_Richness), 0, Territorial_Richness))

# combine the territorial response data with the transect data
territorial_richness_60m = left_join(territorial_richness_60m, transects_60m)
territorial_richness_150m = left_join(territorial_richness_150m, transects_150m)

# combine all territorial richness data, regardless of transect area
territorial_richness = rbind(territorial_richness_60m, territorial_richness_150m) %>%
  relocate(Territorial_Richness, .after = Lat_UTM6S)

# combine all territorial response data
territorial_data = left_join(territorial_abundance, territorial_richness)

# remove temp items
rm(list = c("territorial_abundance", "territorial_richness",
            "territorial_abundance_60m", "territorial_richness_60m",
            "territorial_abundance_150m", "territorial_richness_150m"))

# plot territorial abundance and richness across the transects
territorial_abundance_plot = 
  ggplot(territorial_data, aes(x = factor(Transect_ID), y = Territorial_Abundance)) +
  geom_bar(stat = "identity", fill = "#5dcece", color = "#20a7a7", alpha = 0.7) +  
  labs(x = "Transect ID",
       y = "Territorial algae/detritus feeder abundance") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        axis.line = element_line(color = "black", linewidth = 0.5),
        panel.grid = element_blank()) 
territorial_abundance_plot

territorial_richness_plot =
  ggplot(territorial_data, aes(x = factor(Transect_ID), y = Territorial_Richness)) +
  geom_bar(stat = "identity", fill = "#5dcece", color = "#20a7a7", alpha = 0.7) +  
  labs(x = "Transect ID",
       y = "Territorial algae/detritus feeder richness") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        axis.line = element_line(color = "black", linewidth = 0.5),
        panel.grid = element_blank()) 
territorial_richness_plot

##### STACKED BARPLOTS OF FUNCTIONAL GROUP ABUNDANCE AND RICHNESS ACROSS TRANSECTS ####
# combine functional group abundance data into a single long-format dataframe
fga_data = bind_rows(
  grazer_abundance_plot$data %>% mutate(Functional_Group = "Grazers/detritivores",
                                        Abundance = Grazer_Abundance),
  browser_abundance_plot$data %>% mutate(Functional_Group = "Browsers",
                                         Abundance = Browser_Abundance),
  scraper_abundance_plot$data %>% mutate(Functional_Group = "Small excavators/scrapers",
                                         Abundance = Scraper_Abundance),
  bioeroder_abundance_plot$data %>% mutate(Functional_Group = "Large excavators/bioeroders",
                                           Abundance = Bioeroder_Abundance),
  territorial_abundance_plot$data %>% mutate(Functional_Group = "Territorial algae/detritus feeders",
                                             Abundance = Territorial_Abundance)) %>%
  select(Transect_ID, Functional_Group, Abundance)

# create the stacked bar plot
fga_stacked_barplot =
  ggplot(fga_data, aes(x = Transect_ID, y = Abundance, fill = Functional_Group)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = c(
    "Browsers" = browser_col, 
    "Grazers/detritivores" = grazer_col, 
    "Small excavators/scrapers" = scraper_col, 
    "Large excavators/bioeroders" = bioeroder_col, 
    "Territorial algae/detritus feeders" = territorial_col)) +
  ylab("Functional Group Abundance") +
  xlab(NULL) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        axis.line = element_line(color = "black", linewidth = 0.5),
        panel.grid = element_blank(),
        legend.position = "top",
        legend.justification = "center",
        legend.title = element_blank(),
        legend.text = element_text(size = 6)) 
 fga_stacked_barplot
 
 # combine functional group richness data into a single long-format dataframe
 fgr_data = bind_rows(
   grazer_richness_plot$data %>% mutate(Functional_Group = "Grazers/detritivores",
                                         Richness = Grazer_Richness),
   browser_richness_plot$data %>% mutate(Functional_Group = "Browsers",
                                          Richness = Browser_Richness),
   scraper_richness_plot$data %>% mutate(Functional_Group = "Small excavators/scrapers",
                                          Richness = Scraper_Richness),
   bioeroder_richness_plot$data %>% mutate(Functional_Group = "Large excavators/bioeroders",
                                            Richness = Bioeroder_Richness),
   territorial_richness_plot$data %>% mutate(Functional_Group = "Territorial algae/detritus feeders",
                                              Richness = Territorial_Richness)) %>%
   select(Transect_ID, Functional_Group, Richness)
 
 # create the stacked bar plot
 fgr_stacked_barplot =
   ggplot(fgr_data, aes(x = Transect_ID, y = Richness, fill = Functional_Group)) +
   geom_bar(stat = "identity", position = "stack") +
   scale_fill_manual(values = c(
     "Browsers" = browser_col, 
     "Grazers/detritivores" = grazer_col, 
     "Small excavators/scrapers" = scraper_col, 
     "Large excavators/bioeroders" = bioeroder_col, 
     "Territorial algae/detritus feeders" = territorial_col)) +
   ylab("Functional Group Richness") +
   xlab(NULL) +
   theme_minimal() +
   theme(axis.text.x = element_text(angle = 90, hjust = 1),
         axis.line = element_line(color = "black", linewidth = 0.5),
         panel.grid = element_blank(),
         legend.position = "top",
         legend.justification = "center",
         legend.title = element_blank(),
         legend.text = element_text(size = 6)) 
 fgr_stacked_barplot
 
 # convert abundance data into presence/absence (1 if present, 0 if absent) to represent functional diversity
 fgd_data = fga_data %>%
   mutate(Present = ifelse(Abundance > 0, 1, 0)) %>%
   group_by(Transect_ID, Functional_Group) %>%
   summarise(Present = max(Present), .groups = "drop")  # Ensure each group is counted once
 
 # create a stacked bar plot where height represents the number of unique functional groups
 fgd_stacked_barplot =
   ggplot(fgd_data, aes(x = Transect_ID, y = Present, fill = Functional_Group)) +
   geom_bar(stat = "identity", position = "stack") +
   scale_fill_manual(values = c(
     "Browsers" = browser_col, 
     "Grazers/detritivores" = grazer_col, 
     "Small excavators/scrapers" = scraper_col, 
     "Large excavators/bioeroders" = bioeroder_col, 
     "Territorial algae/detritus feeders" = territorial_col)) +
   ylab("Functional Diversity") +
   xlab(NULL) +
   theme_minimal() +
   theme(axis.text.x = element_text(angle = 90, hjust = 1),
         axis.line = element_line(color = "black", linewidth = 0.5),
         panel.grid = element_blank(),
         legend.position = "top",
         legend.justification = "center",
         legend.title = element_blank(),
         legend.text = element_text(size = 6))
 fgd_stacked_barplot
 
 # save the plots
 ggsave(fga_stacked_barplot,
        dpi = 600, width = 10, height = 6, units = "in", bg = "white",
        filename = here("Figures", "Functional_Group_Abundance_Stacked_Barplot.png"))
 
 ggsave(fgr_stacked_barplot,
        dpi = 600, width = 10, height = 6, units = "in", bg = "white",
        filename = here("Figures", "Functional_Group_Richness_Stacked_Barplot.png"))
 
 ggsave(fgd_stacked_barplot,
        dpi = 600, width = 10, height = 6, units = "in", bg = "white",
        filename = here("Figures", "Functional_Diversity_Stacked_Barplot.png"))

#### PLOTTING BIVARIATE RELATIONSHIPS ####
# convert the dataframe to long format, keeping only numeric environmental variables
grazer_data_long = grazer_data %>%
  select(Depth_Mean:Seabird_N15_Max, Live_Coral_m2:Grazer_Abundance) %>% 
  pivot_longer(cols = -Grazer_Abundance, names_to = "Environmental_Variable", values_to = "Value")
# create the ggplot
ggplot(grazer_data_long, aes(x = Value, y = Grazer_Abundance)) +
  geom_point(alpha = 0.6) +  # Scatterplot with transparency
  geom_smooth(method = "lm", se = TRUE, color = "blue") +  # Add linear regression line
  facet_wrap(~Environmental_Variable, scales = "free_x") +  # Facet by environmental variable
  theme_minimal() +  # Clean theme
  labs(x = "Environmental Value", y = "Grazer Abundance", title = "Grazer Abundance vs. Environmental Conditions")

# convert the dataframe to long format, keeping only numeric environmental variables
scraper_data_long = scraper_data %>%
  select(Depth_Mean:Seabird_N15_Max, Live_Coral_m2:Scraper_Abundance) %>% 
  pivot_longer(cols = -Scraper_Abundance, names_to = "Environmental_Variable", values_to = "Value")
# create the ggplot
ggplot(scraper_data_long, aes(x = Value, y = Scraper_Abundance)) +
  geom_point(alpha = 0.6) +  # Scatterplot with transparency
  geom_smooth(method = "lm", se = TRUE, color = "blue") +  # Add linear regression line
  facet_wrap(~Environmental_Variable, scales = "free_x") +  # Facet by environmental variable
  theme_minimal() +  # Clean theme
  labs(x = "Environmental Value", y = "Scraper Abundance", title = "Scraper Abundance vs. Environmental Conditions")

# convert the dataframe to long format, keeping only numeric environmental variables
territorial_data_long = territorial_data %>%
  select(Depth_Mean:Seabird_N15_Max, Live_Coral_m2:Territorial_Abundance) %>% 
  pivot_longer(cols = -Territorial_Abundance, names_to = "Environmental_Variable", values_to = "Value")
# create the ggplot
ggplot(territorial_data_long, aes(x = Value, y = Territorial_Abundance)) +
  geom_point(alpha = 0.6) +  # Scatterplot with transparency
  geom_smooth(method = "lm", se = TRUE, color = "blue") +  # Add linear regression line
  facet_wrap(~Environmental_Variable, scales = "free_x") +  # Facet by environmental variable
  theme_minimal() +  # Clean theme
  labs(x = "Environmental Value", y = "Territorial Abundance", title = "Territorial Abundance vs. Environmental Conditions")

#### BIVARIATE CORRELATIONS ####
predictors = c("Depth_Mean", "Depth_SD", "Depth_Min", "Depth_Max",
               "Slope_Mean", "Slope_SD", "Slope_Min", "Slope_Max",
               "Rugosity_Mean", "Rugosity_SD", "Rugosity_Min", "Rugosity_Max",
               "Seabird_N15_Mean", "Seabird_N15_SD", "Seabird_N15_Min", "Seabird_N15_Max",
               "Live_Coral_m2", "Grazing_Surface_Area", "REI",
               "Fishing_Proximity", "Native_Vegetation")

# Create a data frame to view the results
require(purrr)
grazer_cor = data.frame(
  predictor = predictors,
  correlation = map_dbl(predictors, ~cor(grazer_data$Grazer_Abundance, grazer_data[[.]],
                                         method = "pearson", use = "complete.obs")))

grazer_top10 = grazer_cor %>%
  mutate(abs_correlation = abs(correlation)) %>%
  arrange(desc(abs_correlation)) %>%
  head(10)
grazer_top10

scraper_cor = data.frame(
  predictor = predictors,
  correlation = map_dbl(predictors, ~cor(scraper_data$Scraper_Abundance, scraper_data[[.]],
                                         method = "pearson", use = "complete.obs")))
scraper_top10 = scraper_cor %>%
  mutate(abs_correlation = abs(correlation)) %>%
  arrange(desc(abs_correlation)) %>%
  head(10)
scraper_top10

territorial_cor = data.frame(
  predictor = predictors,
  correlation = map_dbl(predictors, ~cor(territorial_data$Territorial_Abundance, territorial_data[[.]],
                                         method = "pearson", use = "complete.obs")))

territorial_top10 = territorial_cor %>%
  mutate(abs_correlation = abs(correlation)) %>%
  arrange(desc(abs_correlation)) %>%
  head(10)
territorial_top10

top_predictors = unique(c(grazer_top10$predictor, 
                          scraper_top10$predictor, 
                          territorial_top10$predictor))
top_predictors

##### SAVE MODEL DATASETS ####
save(functional_diversity, file = here("Data", "Model_Datasets", "Functional_Diversity_Data.RData"))
save(grazer_data, file = here("Data", "Model_Datasets", "Grazer_Data.RData"))
save(scraper_data, file = here("Data", "Model_Datasets", "Scraper_Data.RData"))
save(territorial_data, file = here("Data", "Model_Datasets", "Territorial_Data.RData"))

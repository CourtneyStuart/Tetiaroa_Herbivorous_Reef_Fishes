#### CONTACT ####
# Courtney Stuart (courtney.e.stuart@gmail.com; courtney.stuart@mansfield.ox.ac.uk)

#### LIBRARIES ####
# install packages (first run only)
# install.packages(c("easypackages", "conflicted", "dplyr", "here",
#                    "lubridate", "stringr", "tidyr", "ggplot2", "RColorBrewer",
#                    "sp", "sf", "raster", "terra", "PNWColors", "colorspace"))

# load all libraries at once with easypackages
library(easypackages)
libraries("easypackages", "conflicted", "dplyr", "here", "forcats",
          "lubridate", "stringr", "tidyr", "ggplot2", "RColorBrewer",
          "sp", "sf", "raster", "terra", "PNWColors", "colorspace")
conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")

#### DIRECTORIES ####
# working directory and relative folder path
setwd("E:/Data/StuartC_DPhil_Ch2/")
#set_here("E:/Data/StuartC_DPhil_Ch2/") # set first-time only
here::i_am(".here")
here::here() # verify where we are according to the here package

#### COLOR PALETTES ####
lake = pnw_palette(name = "Lake", n = 8, type = "discrete")
bioeroder_col = lake[6]
scraper_col = lake[5]
browser_col = lake[2]
grazer_col = lake[1]
territorial_col = lake[3]

#### DATA CLEANING AND PREP ####
# read in the 2021 data from Casey Benkwitt
data_2021 = read.csv(here("Data", "Fish", "BenkwittC_Tetiaroa_2021_Fish_Raw.csv"))

# read in the 2023 data from Courtney Stuart and Kosta Stamoulis
data_2023 = read.csv(here("Data", "Fish", "StuartC_Tetiaroa_2023_Fish_Raw.csv"))

# read in the table of taxonomic information for Pacific reef fishes
taxa_info = read.csv(here("Data", "Fish", "Pacific_Fish_List.csv"))

# read in Table 1 of the Electronic Supplementary Materials from Cure et al. 2021
# Cure, K., Currey-Randall, L., Galaiduk, R., Radford, B., Wakeford, M., & Heyward, A.
# (2021). Depth gradients in abundance and functional roles suggest limited depth refuges
# for herbivorous fishes. Coral Reefs, 40, 365-379. https://doi.org/10.1007/s00338-021-02060-7
cure_2021 = read.csv(here("Data", "Fish", "Cure_et_al_2021_ESM_TableS1.csv"))

# clean the 2021 data
data_2021 = left_join(data_2021,
                       select(taxa_info, 
                              TAXONNAME, FAMILY, COMMON_FAMILY, CONSUMER_GROUP),
                       by = c("Species" = "TAXONNAME"))

data_2021 = data_2021 %>%
  select(Date, Time, Motu, Site, Transect, FAMILY, COMMON_FAMILY,
         Species, CONSUMER_GROUP, Size_TL_cm, Abundance, Area_m2) %>%
  rename(Family = FAMILY,
         Common_Family = COMMON_FAMILY,
         Consumer_Group = CONSUMER_GROUP) %>%
  mutate(Date = format(dmy(Date), "%d/%m/%Y"),
         Start_Time = str_extract(Time, "^[^-]+"),
         Year = "2021",
         Observer = "CEB") %>%
  relocate(Start_Time, .after = Date) %>%
  relocate(Year, .before = Start_Time) %>%
  relocate(Observer, .after = Transect) %>%
  mutate(Density_m2 = Abundance/Area_m2) %>%
  select(-Time)

# Pomacentridae were only recorded on Casey's 60m passes, fix an typos / transcription errors!
data_2021 = data_2021 %>%
  mutate(
    Area_m2 = ifelse(Family == "Pomacentridae", 60, Area_m2),
    Density_m2 = ifelse(Family == "Pomacentridae", Abundance / Area_m2, Density_m2))

# clean the 2023 data
data_2023 = data_2023 %>%
  select(Date, Time_st, Transect, Family, Common_family, Taxon_name, Consumer_group,
         Size_TL_cm, Number) %>%
  rename(Common_Family = Common_family,
         Species = Taxon_name,
         Consumer_Group = Consumer_group,
         Abundance = Number) %>%
  mutate(Date = format(mdy(Date), "%d/%m/%Y"),
         Start_Time = str_replace(Time_st, ":\\d{2}$", ""),
         Year = "2023",
         Observer = "KAS") %>%
  relocate(Start_Time, .after = Date) %>%
  relocate(Year, .before = Start_Time) %>%
  relocate(Observer, .after = Transect) %>%
  separate(Transect, into = c("Motu", "Site", "Transect"), 
           sep = "(?<=^[A-Za-z]{3})(?=[a-zA-Z]+)|(?<=\\D)(?=\\d)", 
           remove = FALSE) %>%
  mutate(Motu = case_when(
    Motu == "AIE" ~ "Aie",
    Motu == "RIM" ~ "Rimatuu",
    Motu == "REI" ~ "Reiono",
    Motu == "HIR" ~ "Hiraanae/Auroa",
    Motu == "TAU" ~ "Tauini/Auroa",
    Motu == "ONE" ~ "Onetahi",
    TRUE ~ Motu)) %>%
  mutate(Site = case_when(
    Site == "wind" ~ "Windward",
    Site == "lee" ~ "Leeward",
    Site == "AURlee" ~ "Leeward")) %>%
  mutate(Area_m2 = 150) %>%
  select(-Time_st) 

# expand dataframe such that each row represents a single fish
data_2023 = data_2023 %>%
  uncount(Abundance) %>%
  mutate(Abundance = 1) %>%
  relocate(Abundance, .after = Size_TL_cm) %>%
  mutate(Density_m2 = Abundance/Area_m2)

# combine the 2021 and 2023 fish data
data_2123 = rbind(data_2021, data_2023)

data_2123 = data_2123 %>%
  mutate(Motu = str_replace_all(
    Motu, c("Hiraanae/Auroa" = "Hiraanae_Auroa",
            "Tauini/Auroa" = "Tauini_Auroa")),
    Site = str_replace_all(
      Site, c("Lagoon \\(southeast\\)" = "Lagoon_Southeast",
              "Lagoon side \\(southwest\\)" = "Lagoon_Side_Southwest",
              "Protected \\(southwest\\)" = "Protected_Southwest",
              "Southwest point" = "Southwest_Point",
              "Exposed \\(southeast\\)" = "Exposed_Southeast",
              "Airstrip/resort" = "Airstrip_Resort",
              "North \\(Site 2\\)" = "North_Site_2",
              "South \\(Site 1\\)" = "South_Site_1")),
    Transect_ID = paste(Motu, Site, Transect, sep = "_")) %>%
  relocate(Transect_ID, .after = Transect) %>%
  # explicitly define column types
  mutate(Date = as.character(Date),
         Year = as.factor(Year),
         Start_Time = as.character(Start_Time),
         Motu = as.factor(Motu),
         Site = as.character(Site),
         Transect = as.character(Transect),
         Transect_ID = as.factor(Transect_ID),
         Observer = as.factor(Observer),
         Family = as.character(Family),
         Common_Family = as.character(Common_Family),
         Species = as.character(Species),
         Consumer_Group = as.character(Consumer_Group),
         Size_TL_cm = as.integer(Size_TL_cm),
         Abundance = as.integer(Abundance),
         Area_m2 = as.numeric(Area_m2),
         Density_m2 = as.numeric(Density_m2))

#  save the info for all transects for later
transect_info = data_2123 %>%
  select(Date, Year, Start_Time, Motu, Site, Transect, Transect_ID, Observer) %>%
  distinct(Transect_ID, .keep_all = TRUE)

# isolate herbivores and add the functional group info from Cure et al. 2021
herbs = data_2123 %>%
  filter(Consumer_Group == "PRIMARY")

# exclude contentious species (those debated/not recognised as nominal herbivores)
herbs = herbs %>%
  filter(!Species %in% 
           c("Chromis iomelas", #planktivore
             "Crenimugil crenilabrus", #omnivore/invertivore
             "Ostracion cubicus", #omnivore/microcarnivore
             "Canthigaster solandri", #omnivore/microcarnivore
             "Canthigaster amboinensis")) #omnivore/microcarnivore

# first save the list of unique herbivore fish species seen in Tetiaroa
# herbs_list = herbs %>%
#   select(Family, Common_Family, Species, Consumer_Group) %>%
#   distinct()
# write.csv(herbs_list,
#           here("Data", "Fish", "Tetiaroa_Herbivore_Fish_List.csv"),
#           row.names = FALSE)

# assign functional groups
func_groups = data.frame(
    Species = c(
      # Grazers/Detritivores
      "Acanthurus guttatus", "Acanthurus lineatus", "Acanthurus nigricans", 
      "Acanthurus nigricauda", "Acanthurus nigrofuscus", "Acanthurus olivaceus", 
      "Acanthurus sp", "Acanthurus triostegus", "Centropyge flavissima", 
      "Ctenochaetus striatus", "Zebrasoma scopas", "Zebrasoma veliferum",
      # Browsers
      "Kyphosus vaigiensis",
      # Small excavators/Scrapers
      # "Chlorurus microrhinos", #we don't have any small (<35 cm) C. microrhinos of this type
      "Chlorurus sordidus", "Hipposcarus longiceps", "Scarus altipinnis", "Scarus frenatus", 
      "Scarus niger", "Scarus oviceps", "Scarus psittacus",
      # Large excavators/Bioeroders
      "Chlorurus microrhinos",
      # Territorial algae/Detritus feeders
      "Abudefduf septemfasciatus", "Chrysiptera brownriggii", 
      "Chrysiptera glauca", "Stegastes albifasciatus", "Stegastes nigricans"),
    Common_Name = c(
      # Grazers/Detritivores
      "Whitespotted surgeonfish", "Lined surgeonfish", "Whitecheek surgeonfish", 
      "Epaulette surgeonfish", "Brown surgeonfish", "Orangespot surgeonfish", 
      "Unidentified surgeonfish", "Convict surgeonfish", "Lemonpeel angelfish", 
      "Striated surgeonfish", "Twotone tang", "Sailfin tang",
      # Browsers
      "Brassy chub",
      # Small excavators/Scrapers
      # "Pacific steephead parrotfish", #we don't have any small (<35 cm) C. microrhinos of this type
      "Pacific bullethead parrotfish", "Pacific longnose parrotfish", 
      "Filament-finned parrotfish", "Bridled parrotfish", "Dusky parrotfish", 
      "Dark capped parrotfish", "Common parrotfish",
      # Large excavators/Bioeroders
      "Pacific steephead parrotfish",
      # Territorial algae/Detritus feeders
      "Banded sergeant", "Surge damselfish", "Grey demoiselle", 
      "Whitebar gregory", "Dusky farmerfish"),
    Functional_Group = c(
      # Grazers/Detritivores
      rep("Grazers/detritivores", 12),
      # Browsers
      "Browsers",
      # Small excavators/Scrapers
      rep("Small excavators/scrapers", 7),
      # Large excavators/Bioeroders
      "Large excavators/bioeroders",
      # Territorial algae/Detritus feeders
      rep("Territorial algae/detritus feeders", 5))
  )

# now add more detailed herbivore functional group info
herbs = herbs %>%
  left_join(func_groups, by = "Species", relationship = "many-to-many") %>%
  mutate(
    Functional_Group = case_when(
      Species == "Chlorurus microrhinos" & Size_TL_cm >= 35 ~ "Large excavators/bioeroders",
      Species == "Chlorurus microrhinos" & Size_TL_cm < 35 ~ "Small excavators/scrapers",
      TRUE ~ Functional_Group),
    Consumer_Group = str_to_title(tolower(Consumer_Group))) %>%
  relocate(Common_Name, .after = Species) %>%
  relocate(Functional_Group, .after = Consumer_Group)

# calculate the total abundance by functional group
fg_abundance = herbs %>%
  group_by(Functional_Group) %>%
  summarise(Functional_Abundance = sum(Abundance)) %>%
  arrange(Functional_Abundance) %>%
  mutate(Functional_Group = factor(Functional_Group, levels = Functional_Group))

# calculate richness by functional group
fg_richness = herbs %>%
  group_by(Functional_Group) %>%
  summarise(Functional_Richness = n_distinct(Species)) %>%
  arrange(Functional_Richness) %>%
  mutate(Functional_Group = factor(Functional_Group, levels = Functional_Group))

# custom colors for each functional group
small_excavators_colors = c(
  scraper_col,
  lighten(scraper_col, amount = 0.1),
  lighten(scraper_col, amount = 0.2),
  lighten(scraper_col, amount = 0.3),
  lighten(scraper_col, amount = 0.4),
  lighten(scraper_col, amount = 0.5),
  lighten(scraper_col, amount = 0.6))

grazers_colors = c(
  grazer_col,
  lighten(grazer_col, amount = 0.05),
  lighten(grazer_col, amount = 0.1),
  lighten(grazer_col, amount = 0.15),
  lighten(grazer_col, amount = 0.2),
  lighten(grazer_col, amount = 0.25),
  lighten(grazer_col, amount = 0.3),
  lighten(grazer_col, amount = 0.35),
  lighten(grazer_col, amount = 0.4),
  lighten(grazer_col, amount = 0.45),
  lighten(grazer_col, amount = 0.5),
  lighten(grazer_col, amount = 0.55))

territorial_colors = c(
  territorial_col,
  lighten(territorial_col, amount = 0.1),
  lighten(territorial_col, amount = 0.2),
  lighten(territorial_col, amount = 0.3),
  lighten(territorial_col, amount = 0.4))

large_excavators_colors = bioeroder_col
browsers_colors = browser_col

# apply color mapping based on functional group
herbs$species_color = mapply(function(group, species) {
  if (group == "Browsers") {
    return(browsers_colors)
  } else if (group == "Large excavators/bioeroders") {
    return(large_excavators_colors)
  } else if (group == "Small excavators/scrapers") {
    return(small_excavators_colors[which(species == unique(herbs$Species[herbs$Functional_Group == "Small excavators/scrapers"]))])
  } else if (group == "Grazers/detritivores") {
    return(grazers_colors[which(species == unique(herbs$Species[herbs$Functional_Group == "Grazers/detritivores"]))])
  } else if (group == "Territorial algae/detritus feeders") {
    return(territorial_colors[which(species == unique(herbs$Species[herbs$Functional_Group == "Territorial algae/detritus feeders"]))])
  }
}, herbs$Functional_Group, herbs$Species)

# check the color assignment for your data
table(herbs$Functional_Group, herbs$species_color)

herbs$Functional_Group  = factor(herbs$Functional_Group, 
                                 levels = fg_abundance$Functional_Group)

fg_abundance_plot = 
  ggplot(herbs, aes(x = Functional_Group, fill = species_color)) +
  geom_bar() + 
  labs(
    title = NULL,
    x = NULL,
    y = "Total Abundance",
    fill = "Species"
  ) +
  scale_fill_identity() +  
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1),
        axis.line = element_line(color = "black", linewidth = 0.5),
        plot.title = element_blank(),
        panel.grid = element_blank()) +
  # add text labels manually for each functional group showing only N
  geom_text(aes(x = "Browsers", y = 1, label = "n = 1"), vjust = -0.5, size = 4) +
  geom_text(aes(x = "Large excavators/bioeroders", y = 2, label = "n = 2"), vjust = -0.5, size = 4) +
  geom_text(aes(x = "Small excavators/scrapers", y = 657, label = "n = 657"), vjust = -0.5, size = 4) +
  geom_text(aes(x = "Grazers/detritivores", y = 883, label = "n = 883"), vjust = -0.5, size = 4) +
  geom_text(aes(x = "Territorial algae/detritus feeders", y = 1310, label = "n = 1310"), vjust = -0.5, size = 4) +
  theme(
    axis.title.x = element_text(size = 14, face = "bold"),  # x-axis label
    axis.title.y = element_text(size = 14, face = "bold"),  # y-axis label
    axis.text.x = element_text(size = 12, face = "bold"),   # x-axis tick text
    axis.text.y = element_text(size = 12),    # y-axis tick text
    legend.text = element_text(size = 11),
    legend.title = element_text(size = 11))

plot(fg_abundance_plot)

ggsave(fg_abundance_plot,
       dpi = 600,
       width = 8,
       height = 6,
       units = "in",
       bg = "white",
       filename = here("Figures", "Abundance_by_Functional_Group.png"))

# reorder Species factor by total abundance
herbs = herbs %>%
  group_by(Species) %>%
  mutate(Total_Abundance = n()) %>% # calculate total abundance for each species
  ungroup() %>%
  mutate(Species = fct_reorder(Species, Total_Abundance, .desc = FALSE)) # reorder factor

# plot with reordered stacking
sp_abundance_plot = 
  ggplot(herbs, 
       aes(x = Functional_Group, fill = Species)) +
  geom_bar() + 
  labs(
    title = NULL,
    x = NULL,
    y = "Total Abundance",
    fill = "Species") +
  # use manual fill scale to map colors
  scale_fill_manual(
    values = setNames(herbs$species_color, herbs$Species), # map species to colors
    breaks = c(
      "Kyphosus vaigiensis",
      "Chlorurus microrhinos",
      "Hipposcarus longiceps",
      "Scarus altipinnis",
      "Scarus frenatus",
      "Scarus oviceps",
      "Scarus psittacus",
      "Scarus niger",
      "Chlorurus sordidus",
      "Acanthurus guttatus",
      "Acanthurus lineatus",
      "Acanthurus nigricans",
      "Acanthurus nigricauda",
      "Acanthurus nigrofuscus",
      "Acanthurus olivaceus",
      "Acanthurus sp",
      "Acanthurus triostegus",
      "Centropyge flavissima",
      "Ctenochaetus striatus",
      "Zebrasoma scopas",
      "Zebrasoma veliferum",
      "Abudefduf septemfasciatus",
      "Chrysiptera brownriggii",
      "Chrysiptera glauca",
      "Stegastes albifasciatus",
      "Stegastes nigricans")) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) + 
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 30, hjust = 1),
    axis.line = element_line(color = "black", linewidth = 0.5),
    plot.title = element_blank(),
    panel.grid = element_blank(),
    legend.position = "top", 
    legend.title = element_blank(),
    legend.text = element_text(size = 11, face = "italic")) +
  # add text labels manually for each functional group showing only N
  geom_text(aes(x = "Browsers", y = 1, label = "n = 1"), vjust = -0.5, size = 4) +
  geom_text(aes(x = "Large excavators/bioeroders", y = 2, label = "n = 2"), vjust = -0.5, size = 4) +
  geom_text(aes(x = "Small excavators/scrapers", y = 657, label = "n = 657"), vjust = -0.5, size = 4) +
  geom_text(aes(x = "Grazers/detritivores", y = 883, label = "n = 883"), vjust = -0.5, size = 4) +
  geom_text(aes(x = "Territorial algae/detritus feeders", y = 1310, label = "n = 1310"), vjust = -0.5, size = 4) +
theme(
  axis.title.x = element_text(size = 14, face = "bold"),  
  axis.title.y = element_text(size = 14, face = "bold"),  
  axis.text.x = element_text(size = 12, face = "bold"),  
  axis.text.y = element_text(size = 12))

plot(sp_abundance_plot)

ggsave(sp_abundance_plot,
       dpi = 600,
       width = 11,
       height = 8,
       units = "in",
       bg = "white",
       filename = here("Figures", "Abundance_by_Species.png"))

# removing temp items
rm(list = c("data_2021", "data_2023"))

# take an overall/summary look at the herbivores
herbivore_summary = herbs %>%
  group_by(Species, Functional_Group) %>%
  summarise(Total_Abundance = n(), .groups = "drop")

summary_plot =
  ggplot(herbivore_summary, 
       aes(x = Total_Abundance, y = Species, fill = Functional_Group)) +
  geom_bar(stat = "identity") +  
  theme_minimal() +  
  labs(x = "Total Abundance", y = "") +  
  theme(axis.text.y = element_text(size = 8, face = "italic"),
        axis.line = element_line(color = "black", linewidth = 0.5),
        plot.title = element_blank(),
        panel.grid = element_blank(),
        legend.position = "top",
        legend.justification = "center",
        legend.title = element_blank(),
        legend.text = element_text(size = 6)) + 
  scale_fill_manual(values = c(
    "Browsers" = "#d3ceba", 
    "Grazers/detritivores" = "#30c67c", 
    "Small excavators/scrapers" = "#0061ff", 
    "Large excavators/bioeroders" = "#c3e1fc", 
    "Territorial algae/detritus feeders" = "#392d69")) +
  guides(fill = guide_legend(nrow = 2, byrow = FALSE))  # wrap legend into 2 rows

summary_plot

ggsave(summary_plot,
       dpi = 600,
       width = 6,
       height = 4,
       units = "in",
       bg = "white",
       filename = here("Figures", "Herbivore_Summary.png"))

# summary look at the herbivores by year
herbivore_summary_by_year = herbs %>%
  group_by(Species, Functional_Group, Year) %>%
  summarise(Total_Abundance = n(), .groups = "drop") %>%
  pivot_wider(names_from = Year, values_from = Total_Abundance, 
              names_prefix = "Abundance_", values_fill = list(Total_Abundance = 0))
# browsers
browsers = filter(herbs, Functional_Group == "Browsers")

# large excavators/bioeroders
bioeroders = filter(herbs, Functional_Group == "Large excavators/bioeroders")

# small excavators/scrapers
scrapers = filter(herbs, Functional_Group == "Small excavators/scrapers")

# grazers/detritivores
grazers = filter(herbs, Functional_Group == "Grazers/detritivores")

# territorial algae/detritus feeders
territorials = filter(herbs, Functional_Group == "Territorial algae/detritus feeders" )

# save PROJ.4 string for Tetiaroa projection before reading in spatial data
# Projected Coordinate System	WGS 1984 UTM Zone 6S (EPSG WKID	32706)
# with unit meters
my_crs = CRS("+proj=utm +zone=6 +south +datum=WGS84 +units=m +no_defs +type=crs")

# save PROJ.4 string for the standard geographic coordinate system used by
# Garmin GPS - WGS84 - World Geodetic System 1984 (EPSG WKID 4326)
# with unit decimal degrees 
gcs = CRS("+proj=longlat +datum=WGS84 +no_defs +type=crs")

# read in the transect belt shapefiles (150-m^2 and 60-m^2)
belts_150m2_2021 = st_read(here("Data", "GIS",
                                "Transect_Belts_150m2_2021_CEB.shp"))
compareCRS(belts_150m2_2021, my_crs)

belts_60m2_2021 = st_read(here("Data", "GIS",
                               "Transect_Belts_60m2_2021_CEB.shp"))
compareCRS(belts_60m2_2021, my_crs)

belts_150m2_2023 = st_read(here("Data", "GIS",
                                "Transect_Belts_150m2_2023_CES.shp"))
compareCRS(belts_150m2_2021, my_crs)

# update the transect names/unique IDs to match the fish data prepped above
belts_150m2_2021 = belts_150m2_2021 %>%
  select(-ORIG_FID) %>%
  rename(Transect_ID = Transect_n) %>%
  rename(Width_per_Side_m = Width_per_) %>%
  rename(Belt_Perimeter_m = Shape_Leng) %>%
  rename(Belt_Area_m2 = Shape_Area) %>%
  mutate(Transect_ID = str_replace_all(
    Transect_ID, c("Lagoon \\(southeast\\)" = "Lagoon_Southeast",
            "Lagoon side \\(southwest\\)" = "Lagoon_Side_Southwest",
            "Protected \\(southwest\\)" = "Protected_Southwest",
            "Southwest point" = "Southwest_Point",
            "Exposed \\(southeast\\)" = "Exposed_Southeast",
            "Airstrip/resort" = "Airstrip_Resort",
            "Aie_North" = "Aie_North_Site_2",
            "Aie_South" = "Aie_South_Site_1",
            "Hiraanae/Auroa" = "Hiraanae_Auroa")))

belts_60m2_2021 = belts_60m2_2021 %>%
 select(-ORIG_FID) %>%
  rename(Transect_ID = Transect_n) %>%
  rename(Width_per_Side_m = BUFF_DIST) %>%
  rename(Belt_Perimeter_m = Shape_Leng) %>%
  rename(Belt_Area_m2 = Shape_Area) %>%
  mutate(Transect_ID = str_replace_all(
    Transect_ID, c("Lagoon \\(southeast\\)" = "Lagoon_Southeast",
                   "Lagoon side \\(southwest\\)" = "Lagoon_Side_Southwest",
                   "Protected \\(southwest\\)" = "Protected_Southwest",
                   "Southwest point" = "Southwest_Point",
                   "Exposed \\(southeast\\)" = "Exposed_Southeast",
                   "Airstrip/resort" = "Airstrip_Resort",
                   "Aie_North" = "Aie_North_Site_2",
                   "Aie_South" = "Aie_South_Site_1",
                   "Hiraanae/Auroa" = "Hiraanae_Auroa")))

belts_150m2_2023 = belts_150m2_2023 %>%
  select(-ORIG_FID) %>%
  rename(Transect_ID = Transect_n) %>%
  rename(Width_per_Side_m = Width_per_) %>%
  rename(Belt_Perimeter_m = Shape_Leng) %>%
  rename(Belt_Area_m2 = Shape_Area) %>%
  mutate(
    Transect_ID = str_replace_all(Transect_ID, c(
      "AIE" = "Aie",
      "RIM" = "Rimatuu",
      "REI" = "Reiono",
      "HIRAUR" = "Hiraanae_Auroa",
      "TAUAUR" = "Tauini_Auroa",
      "ONE" = "Onetahi")),
    Transect_ID = str_replace_all(Transect_ID, c(
      "lee" = "Leeward",
      "wind" = "Windward")),
    Transect_ID = str_replace_all(Transect_ID, 
      "(.*?)(Leeward|Windward)(\\d{2}[A-Z])", "\\1_\\2_\\3")
  )

# combine the 150-m^2 transects into a single object
belts_150m2 = rbind(belts_150m2_2021, belts_150m2_2023)

# rename the 60-m^2 transects for ease
belts_60m2 = belts_60m2_2021

# associate the transect info with all functional groups EXCEPT TERRITORIAL ALGAE/DETRITUS FEEDERS
browsers = left_join(browsers, belts_150m2,
                     by = "Transect_ID")

bioeroders = left_join(bioeroders, belts_150m2,
                     by = "Transect_ID")

scrapers = left_join(scrapers, belts_150m2,
                     by = "Transect_ID")

grazers = left_join(grazers, belts_150m2,
                    by = "Transect_ID")

# now deal with the territorial observations, based on transect area
territorials_60m = left_join(filter(territorials, Area_m2 == 60), belts_60m2,
                             by = "Transect_ID")

territorials_150m = left_join(filter(territorials, Area_m2 == 150), belts_150m2,
                             by = "Transect_ID")

# I only want to keep territorials recorded on the first 60m2 pass by CEB, not those recorded on the second 150m2 
# pass to avoid double counting fish by accident
territorials_150m = territorials_150m %>%
  filter(!Observer == "CEB")

# combining the data back together
territorials = rbind(territorials_60m, territorials_150m)

# remove items no longer needed 
rm(list = c("belts_150m2_2021", "belts_150m2_2023", "belts_60m2_2021",
            "territorials_150m", "territorials_60m"))

# how many transects was each functional group recorded on?
length(unique(browsers$Transect_ID))
length(unique(bioeroders$Transect_ID))
length(unique(scrapers$Transect_ID))
length(unique(grazers$Transect_ID))
length(unique(territorials$Transect_ID))

# read in the motu shapefile 
motu = st_read(here("Data", "GIS", "Motus.shp"))
compareCRS(motu, my_crs)

# simple map to confirm general transect locations
ggplot() +
  geom_sf(data = motu, fill = "gray70", color = NA) + 
  geom_sf(data = st_as_sf(grazers), fill = "black", color = NA) + # plot grazers layer as example
  theme_minimal()

# save workspace
save.image(here("Code", "01_Combine_Fish_Data.RData"))

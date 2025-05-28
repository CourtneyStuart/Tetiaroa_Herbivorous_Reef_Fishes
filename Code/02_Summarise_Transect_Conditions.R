#### CONTACT ####
# Courtney Stuart (courtney.e.stuart@gmail.com; courtney.stuart@mansfield.ox.ac.uk)

#### LIBRARIES ####
#### libraries ####
# install required packages (first run only)
# install.packages(c("easypackages", "conflicted", "dplyr", "here",
#                    "lubridate", "stringr", "tidyr", "ggplot2",
#                    "RColorBrewer", "sp", "sf", "raster", "terra",
#                    "exactextractr", "MultiscaleDTM", "forcats",
#                    "geosphere", "purrr", "waver", "openair", "Cairo"))

# load all libraries at once with easypackages
library(easypackages)
libraries("easypackages", "conflicted", "dplyr", "here", "lubridate", 
          "stringr", "tidyr", "ggplot2", "RColorBrewer", "sp", "sf", 
          "raster", "terra", "exactextractr", "MultiscaleDTM", "forcats", 
          "geosphere", "purrr", "waver", "openair", "Cairo")
conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")
rasterOptions(progress = 'text') # progress info for processing large rasters
options(terra.progress = 1) # progress info for processing large rasters
rasterOptions(tmpdir = "E:/temp_raster_directory") # custom directory for temporary files
terraOptions(tempdir = "E:/temp_raster_directory") # custom directory for temporary files

#### DIRECTORIES ####
# working directory and relative folder path
setwd("E:/Data/StuartC_DPhil_Ch2/")
#set_here("E:/Data/StuartC_DPhil_Ch2/") #set first-time only
here::i_am(".here")
here::here() # verify where we are according to the here package

# save PROJ.4 string for Tetiaroa projection before reading in spatial data
# Projected Coordinate System	WGS 1984 UTM Zone 6S (EPSG WKID	32706)
# with unit meters
my_crs = CRS("+proj=utm +zone=6 +south +datum=WGS84 +units=m +no_defs +type=crs")

# save PROJ.4 string for the standard geographic coordinate system used by
# Garmin GPS - WGS84 - World Geodetic System 1984 (EPSG WKID 4326)
# with unit decimal degrees 
gcs = CRS("+proj=longlat +datum=WGS84 +no_defs +type=crs")

#### TRANSECT BELTS ####
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

#### BATHYMETRIC DERIVATIVES & SEABIRD N15 ####
# read in the rasters that characterize the seafloor
depth = rast(here("Data", "Rasters", "Depth_400m.tif"))
slope = rast(here("Data", "Rasters", "Slope_400m.tif"))
rugosity = rast(here("Data", "Rasters", "SAPARugosity_400m.tif"))
seabird_N15 = rast(here("Data", "Rasters", "Nutrientscape_GAM1_Clamped.tif"))
crs(depth) = st_crs(belts_150m2)$wkt
crs(slope) = st_crs(belts_150m2)$wkt
crs(rugosity) = st_crs(belts_150m2)$wkt
crs(seabird_N15) = st_crs(belts_150m2)$wkt

##### 150m^2 belts #####
# ensure matching projected coordinate systems
identical(st_crs(belts_150m2)$wkt, crs(depth))
identical(st_crs(belts_150m2)$wkt, crs(slope))
identical(st_crs(belts_150m2)$wkt, crs(rugosity))
identical(st_crs(belts_150m2)$wkt, crs(seabird_N15))

# define raster names
rasters = c("depth", "slope", "rugosity", "seabird_N15")
conditions_150m2 = data.frame()

# loop through rasters
for (i in seq_along(rasters)) {
  raster_name = rasters[i]
  raster_layer = get(raster_name) # load raster by name
  
  # extract values from raster
  extracted = terra::extract(raster_layer, belts_150m2, fun = NULL, cells = FALSE)
  
  # map Transect_ID to extracted data
  extracted$Transect_ID = belts_150m2$Transect_ID[extracted$ID]
  
  # rename raster values column
  colnames(extracted)[2] = "value" # second column contains raster values
  
  # calculate statistics for each raster
  stats = extracted %>%
    group_by(Transect_ID) %>%
    summarise(
      min_value = min(value, na.rm = TRUE),
      max_value = max(value, na.rm = TRUE),
      mean_value = mean(value, na.rm = TRUE),
      sd_value = sd(value, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(raster = raster_name)
  
  # append to results
  conditions_150m2 = bind_rows(conditions_150m2, stats)
  
  # print progress
  print(paste("Completed raster", i, "of", length(rasters)))
}

# reshape data to wide format, only keeping the required stats
conditions_150m2_wide = conditions_150m2 %>%
  pivot_wider(
    id_cols = Transect_ID,
    names_from = raster,
    values_from = c(mean_value, sd_value, min_value, max_value),
    names_glue = "{raster}_{.value}") %>%
  rename_with(~ str_replace_all(., "_value$", ""), everything()) %>%       # remove '_value'
  rename_with(~ str_to_title(str_replace_all(., "_", " ")), everything()) %>% # capitalise each word (with spaces)
  rename_with(~ str_replace_all(., " ", "_"), everything()) %>%            # replace spaces with underscores
  rename_with(~ str_replace_all(., "Sd", "SD"), everything())  %>% # replace "Sd" with "SD"
  rename(Transect_ID = Transect_Id) 

# join the summary statistics back to the belts_150m2 object
belts_150m2_with_stats = belts_150m2 %>%
  left_join(conditions_150m2_wide, by = "Transect_ID")  # merge by Transect_ID

##### 60m^2 belts #####
# define raster names
rasters = c("depth", "slope", "rugosity", "seabird_N15")
conditions_60m2 = data.frame()

# loop through rasters
for (i in seq_along(rasters)) {
  raster_name = rasters[i]
  raster_layer = get(raster_name) # load raster by name
  
  # extract values from raster
  extracted = terra::extract(raster_layer, belts_60m2, fun = NULL, cells = FALSE)
  
  # map Transect_ID to extracted data
  extracted$Transect_ID = belts_60m2$Transect_ID[extracted$ID]
  
  # rename raster values column
  colnames(extracted)[2] = "value" # second column contains raster values
  
  # calculate statistics for each raster
  stats = extracted %>%
    group_by(Transect_ID) %>%
    summarise(
      min_value = min(value, na.rm = TRUE),
      max_value = max(value, na.rm = TRUE),
      mean_value = mean(value, na.rm = TRUE),
      sd_value = sd(value, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(raster = raster_name)
  
  # append to results
  conditions_60m2 = bind_rows(conditions_60m2, stats)
  
  # print progress
  print(paste("Completed raster", i, "of", length(rasters)))
}

conditions_60m2_wide = conditions_60m2 %>%
  pivot_wider(
    id_cols = Transect_ID,
    names_from = raster,
    values_from = c(mean_value, sd_value, min_value, max_value),
    names_glue = "{raster}_{.value}") %>%
  rename_with(~ str_replace_all(., "_value$", ""), everything()) %>%       # remove '_value'
  rename_with(~ str_to_title(str_replace_all(., "_", " ")), everything()) %>% # capitalise each word (with spaces)
  rename_with(~ str_replace_all(., " ", "_"), everything()) %>% # replace spaces with underscores
  rename_with(~ str_replace_all(., "Sd", "SD"), everything())  %>% # replace "Sd" with "SD"
  rename(Transect_ID = Transect_Id) 

# join the summary statistics back to the belts_60m2 object
belts_60m2_with_stats = belts_60m2 %>%
  left_join(conditions_60m2_wide, by = "Transect_ID")  # merge by Transect_ID

# exploring some of the conditions
depth_plot = 
ggplot(belts_150m2_with_stats, aes(x = factor(Transect_ID), y = Depth_Mean)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "skyblue3", alpha = 0.7) +  
  geom_errorbar(aes(ymin = Depth_Mean - Depth_SD, ymax = Depth_Mean + Depth_SD), 
                width = 0.2, color = "navy") +  # add error bars to show stdev
  labs(x = expression("Transect ID (150 m"^2*" pass)"),
       y = "Depth below surface (m)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        axis.line = element_line(color = "black", linewidth = 0.5),
        panel.grid = element_blank()) 
depth_plot

ggsave(depth_plot,
       dpi = 600,
       width = 10,
       height = 8,
       units = "in",
       bg = "white",
       filename = here("Figures", "Depth_Across_150m2_Transects.png"))

seabird_N15_plot = 
ggplot(belts_150m2_with_stats, aes(x = factor(Transect_ID), y = Seabird_N15_Max)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "skyblue3", alpha = 0.7) + 
  labs(x = expression("Transect ID (150 m"^2*" pass)"),
       y = expression("Max modelled seabird nutrients (delta"^15*"N ‰)")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        axis.line = element_line(color = "black", linewidth = 0.5),
        panel.grid = element_blank()) 
seabird_N15_plot

ggsave(seabird_N15_plot,
       dpi = 600,
       width = 10,
       height = 8,
       units = "in",
       bg = "white",
       filename = here("Figures", "N15_Across_150m2_Transects.png"))

gc() # free up any available space

#### KSLOF HABITAT DATA ####
habitat = st_read(here("Data", "Habitat", "Khaled_bin_Sultan_Living_Oceans_Foundation", 
                       "FPSOTE_habitats_final.shp"))
compareCRS(habitat, my_crs)

# dissolve polygons that share boundaries for the same class
habitat = habitat %>%
  group_by(Habitat) %>%  # group by habitat type
  summarise(geometry = st_union(geometry)) %>%  # dissolve polygons within each class
  ungroup()

##### 150m^2 belts #####
# perform spatial intersection to get areas of habitat classes within transect belts
intersected_150m2 = st_intersection(belts_150m2_with_stats, habitat) %>%
  mutate(area = st_area(.))  # calculate the area of each intersected polygon

# reclassify the habitat types based on biological cover using
# Table 2 Hierarchy of habitat classes utilized for satellite mapping
# Purkis, S.J., Gleason, A.C.R., Purkis, C.R. et al. High-resolution habitat 
# and bathymetry maps for 65,000 sq. km of Earth’s remotest coral reefs. 
# Coral Reefs 38, 467–488 (2019).
intersected_150m2 = intersected_150m2 %>%
  mutate(Biological_Cover = case_when(
    Habitat == "Back reef coral bommies" ~ "Live Coral",
    Habitat == "Back reef coral framework" ~ "Live Coral",
    Habitat == "Back reef rubble dominated" ~ "Algae",
    Habitat == "Back reef sediment dominated" ~ "Bare",
    Habitat == "Beach sand" ~ "Bare",
    Habitat == "Lagoonal floor barren" ~ "Bare",
    Habitat == "Lagoonal patch reefs" ~ "Live Coral",
    Habitat == "Lagoonal pinnacle reefs massive coral dominated" ~ "Live Coral",
    Habitat == "Rock" ~ "Bare",
    TRUE ~ "Unclassified")) # catch-all for unexpected values

# confirm changes
unique(intersected_150m2$Biological_Cover) 

# group by transect and biological cover, then sum areas
dominant_habitat_150m2 = intersected_150m2 %>%
  group_by(Transect_ID, Biological_Cover) %>%
  summarise(total_area = sum(area), .groups = "drop") %>%  # sum areas of the same habitat class
  group_by(Transect_ID) %>%
  slice_max(total_area, n = 1, with_ties = FALSE) %>%  # keep only the habitat with the largest area per transect
  rename(Dominant_Habitat = Biological_Cover) %>%
  ungroup() %>%
  st_drop_geometry()

# join the dominant habitat class back to the original belts
belts_150m2_with_stats = left_join(
  belts_150m2_with_stats,
  select(dominant_habitat_150m2, -total_area),
  by = join_by(Transect_ID))

# filter for Biological_Cover = Live Coral and summarise the area covered by it for each transect
coral_area_150m2 = intersected_150m2 %>%
  mutate(Biological_Cover = as.character(Biological_Cover)) %>%  # ensure it's character
  filter(Biological_Cover == "Live Coral") %>%
  group_by(Transect_ID) %>%  # group by transect ID
  summarise(Live_Coral_m2 = as.numeric(sum(area, na.rm = TRUE)), .groups = "drop") %>%  # drop units and sum areas
  complete(Transect_ID = unique(intersected_150m2$Transect_ID),
           fill = list(Live_Coral_m2 = 0)) %>% # add 0 for transects that had no live coral
  mutate(Live_Coral_m2 = as.numeric(Live_Coral_m2)) %>%
  st_drop_geometry() %>%
  select(Transect_ID, Live_Coral_m2)

# join the coral area back to the original belts
belts_150m2_with_stats = left_join(
  belts_150m2_with_stats, coral_area_150m2, by = join_by(Transect_ID))

# define the habitat types to include in grazing surface area calculations
grazing_habitats = c("Back reef coral bommies", 
                      "Back reef coral framework", 
                      "Back reef rubble dominated", 
                      "Lagoonal patch reefs", 
                      "Lagoonal pinnacle reefs massive coral dominated", 
                      "Rock")

# calculate total grazing surface area for each transect based on the Habitat column
grazing_area_150m2 = intersected_150m2 %>%
  mutate(Habitat = as.character(Habitat)) %>%  # ensure Habitat is a character
  filter(Habitat %in% grazing_habitats) %>%  # filter for specified habitats
  group_by(Transect_ID) %>%  # group by Transect_ID
  summarise(Grazing_Surface_Area = sum(as.numeric(area), na.rm = TRUE), .groups = "drop") %>%  # sum areas for each transect
  complete(Transect_ID = unique(intersected_150m2$Transect_ID), 
           fill = list(Grazing_Surface_Area = 0)) %>%  # fill missing transects with 0
  mutate(Grazing_Surface_Area = as.numeric(Grazing_Surface_Area)) %>%  # convert to numeric
  st_drop_geometry() %>%  # drop geometry if not needed
  select(Transect_ID, Grazing_Surface_Area)  # select relevant columns

# join the grazig surface area back to the original belts
belts_150m2_with_stats = left_join(
  belts_150m2_with_stats, grazing_area_150m2, by = join_by(Transect_ID))

##### 60m^2 belts #####
# perform spatial intersection to get areas of habitat classes within transect belts
intersected_60m2 = st_intersection(belts_60m2_with_stats, habitat) %>%
  mutate(area = st_area(.))  # calculate the area of each intersected polygon

# reclassify the habitat types based on biological cover using
# Table 2 Hierarchy of habitat classes utilized for satellite mapping
# Purkis, S.J., Gleason, A.C.R., Purkis, C.R. et al. High-resolution habitat 
# and bathymetry maps for 65,000 sq. km of Earth’s remotest coral reefs. 
# Coral Reefs 38, 467–488 (2019).
intersected_60m2 = intersected_60m2 %>%
  mutate(Biological_Cover = case_when(
    Habitat == "Back reef coral bommies" ~ "Live Coral",
    Habitat == "Back reef coral framework" ~ "Live Coral",
    Habitat == "Back reef rubble dominated" ~ "Algae",
    Habitat == "Back reef sediment dominated" ~ "Bare",
    Habitat == "Beach sand" ~ "Bare",
    Habitat == "Lagoonal floor barren" ~ "Bare",
    Habitat == "Lagoonal patch reefs" ~ "Live Coral",
    Habitat == "Lagoonal pinnacle reefs massive coral dominated" ~ "Live Coral",
    Habitat == "Rock" ~ "Bare",
    TRUE ~ "Unclassified")) # catch-all for unexpected values

# confirm changes
unique(intersected_150m2$Biological_Cover) 

# group by transect and biological cover, then sum areas
dominant_habitat_60m2 = intersected_60m2 %>%
  group_by(Transect_ID, Biological_Cover) %>%
  summarise(total_area = sum(area), .groups = "drop") %>%  # sum areas of the same habitat class
  group_by(Transect_ID) %>%
  slice_max(total_area, n = 1, with_ties = FALSE) %>%  # keep only the habitat with the largest area per transect
  rename(Dominant_Habitat = Biological_Cover) %>%
  ungroup() %>%
  st_drop_geometry()

# join the dominant habitat class back to the original belts
belts_60m2_with_stats = left_join(
  belts_60m2_with_stats,
  select(dominant_habitat_60m2, -total_area),
  by = join_by(Transect_ID))

# filter for Biological_Cover = Live Coral and summarise the area covered by it for each transect
coral_area_60m2 = intersected_60m2 %>%
  mutate(Biological_Cover = as.character(Biological_Cover)) %>%  # ensure it's character
  filter(Biological_Cover == "Live Coral") %>%
  group_by(Transect_ID) %>%  # group by transect ID
  summarise(Live_Coral_m2 = as.numeric(sum(area, na.rm = TRUE)), .groups = "drop") %>%  # drop units and sum areas
  complete(Transect_ID = unique(intersected_60m2$Transect_ID),
           fill = list(Live_Coral_m2 = 0)) %>% # add 0 for transects that had no live coral
  mutate(Live_Coral_m2 = as.numeric(Live_Coral_m2)) %>%
  st_drop_geometry() %>%
  select(Transect_ID, Live_Coral_m2)

# join the coral area back to the original belts
belts_60m2_with_stats = left_join(
  belts_60m2_with_stats, coral_area_60m2, by = join_by(Transect_ID))

# calculate total grazing surface area for each transect based on the Habitat column
grazing_area_60m2 = intersected_60m2 %>%
  mutate(Habitat = as.character(Habitat)) %>%  # ensure Habitat is a character
  filter(Habitat %in% grazing_habitats) %>%  # filter for specified habitats
  group_by(Transect_ID) %>%  # group by Transect_ID
  summarise(Grazing_Surface_Area = sum(as.numeric(area), na.rm = TRUE), .groups = "drop") %>%  # sum areas for each transect
  complete(Transect_ID = unique(intersected_60m2$Transect_ID), 
           fill = list(Grazing_Surface_Area = 0)) %>%  # fill missing transects with 0
  mutate(Grazing_Surface_Area = as.numeric(Grazing_Surface_Area)) %>%  # convert to numeric
  st_drop_geometry() %>%  # drop geometry if not needed
  select(Transect_ID, Grazing_Surface_Area)  # select relevant columns

# join the grazing surface area back to the original belts
belts_60m2_with_stats = left_join(
  belts_60m2_with_stats, grazing_area_60m2, by = join_by(Transect_ID))

gc() # free up any available space

#### RELATIVE EXPOSURE INDEX ####
# load Meteo France hourly weather data for Tetiaroa (2020-2023)
weather_data = read.csv(here("Data", "Weather", "H_987_previous-2020-2023.csv")) %>%
  rename(station = NOM_USUEL,
         yyyymmddhh = AAAAMMJJHH,
         wind_direction = DD,
         wind_speed = FF) %>%
  filter(station == "TETIAROA 1") %>%
  filter(!is.na(wind_direction), !is.na(wind_speed),
         yyyymmddhh >= 2020110100 & yyyymmddhh <= 2023110123) %>%
  mutate(wind_direction_bin = floor(wind_direction / 10) * 10)  # ensure bins align

#  plot a wind rose
windRose(weather_data, ws = "wind_speed", wd = "wind_direction",
         angle = 10, paddle = FALSE,
         cols = c("#90e0ef", "#00b4d8", "#0077b6", "#03045e"))

# save the wind rose plot
Cairo(file = here("Figures", "Wind_Rose_Nov2021_Nov2023.png"), 
      bg = "white", type = "png", units = "in", width = 6, height = 6, 
      pointsize = 12,  dpi = 600)
windRose(weather_data, ws = "wind_speed", wd = "wind_direction",
         angle = 10, paddle = FALSE,
         cols = c("#90e0ef", "#00b4d8", "#0077b6", "#03045e"))
dev.off()

#### PREPARE TRANSECT & MOTU DATA ####
transects = st_zm(belts_150m2)  # remove Z-axis if present
motu = st_zm(st_read(here("Data", "GIS", "Motus.shp")))

##### CALCULATE FETCH #####
wind_directions = seq(0, 350, by = 10)  # 10° intervals

fetch_results = fetch_len_multi(
  pts = st_centroid(st_geometry(transects)),  # ensure centroid geometry
  bearings = wind_directions,
  shoreline = motu,
  dmax = 10000)  # max fetch in meters

# convert fetch matrix to tidy dataframe
fetch_df = as.data.frame(fetch_results) %>%
  setNames(as.character(wind_directions)) %>%
  mutate(Transect_ID = transects$Transect_ID) %>%
  pivot_longer(cols = -Transect_ID, names_to = "Wind_Direction", values_to = "Fetch_Distance") %>%
  mutate(Wind_Direction = as.numeric(Wind_Direction))  # ensure numeric type

#### CALCULATE REI ####
wind_freq = weather_data %>%
  group_by(wind_direction_bin) %>%
  summarise(frequency = n() / nrow(weather_data))

wind_speed_summary = weather_data %>%
  group_by(wind_direction_bin) %>%
  summarise(mean_wind_speed = mean(wind_speed, na.rm = TRUE))

rei_results = fetch_df %>%
  left_join(wind_freq, by = c("Wind_Direction" = "wind_direction_bin")) %>%
  left_join(wind_speed_summary, by = c("Wind_Direction" = "wind_direction_bin")) %>%
  mutate(rei_component = Fetch_Distance * frequency * mean_wind_speed) %>%
  group_by(Transect_ID) %>%
  summarise(REI = sum(rei_component, na.rm = TRUE))

# join the REI back to the transect data
belts_150m2_with_stats = left_join(
  belts_150m2_with_stats, rei_results, by = join_by(Transect_ID)) 

belts_60m2_with_stats = left_join(
  belts_60m2_with_stats, rei_results, by = join_by(Transect_ID)) 

gc() # free up any available space

#### PROXIMITY TO SPEARFISHING ####
# here are the locations of spearfishing in Tetiaroa according to the Tetiaroa Society
fishing_pressure = st_read(here("Data", "GIS", "Spear_Fishing_UTM6S.shp"))
plot(fishing_pressure$geometry)
compareCRS(fishing_pressure, belts_150m2_with_stats)

# starting with the 150m2 transects

# calculate the distance between each belt transect and the nearest spearfishing site
proximity_to_fishing_150m2 = st_distance(belts_150m2_with_stats, fishing_pressure)

# find the minimum distance to any polygon in fishing_pressure for each row in belts_150m2_with_stats
min_distances_150m2 = apply(proximity_to_fishing_150m2, 1, min)

# create a dataframe with Transect_ID and proximity_to_fishing
distance_df_150m2 = data.frame(
  Transect_ID = belts_150m2_with_stats$Transect_ID,
  proximity_to_fishing_150m2 = min_distances_150m2)

# use left_join to merge the distance dataframe into belts_150m2_with_stats by Transect_ID
belts_150m2_with_stats = belts_150m2_with_stats %>%
  left_join(distance_df_150m2, by = "Transect_ID") %>%
  rename(Fishing_Proximity = proximity_to_fishing_150m2)

# repeat for the 60m2 transects

# calculate the distance between each belt transect and the nearest spearfishing site
proximity_to_fishing_60m2 = st_distance(belts_60m2_with_stats, fishing_pressure)

# find the minimum distance to any polygon in fishing_pressure for each row in belts_60m2_with_stats
min_distances_60m2 = apply(proximity_to_fishing_60m2, 1, min)

# create a dataframe with Transect_ID and proximity_to_fishing
distance_df_60m2 = data.frame(
  Transect_ID = belts_60m2_with_stats$Transect_ID,
  proximity_to_fishing_60m2 = min_distances_60m2)

# use left_join to merge the distance dataframe into belts_60m2_with_stats by Transect_ID
belts_60m2_with_stats = belts_60m2_with_stats %>%
  left_join(distance_df_60m2, by = "Transect_ID") %>%
  rename(Fishing_Proximity = proximity_to_fishing_60m2)

# calculate the centroids of each transect and add these as new Lat/Long columns
belts_150m2_with_stats = belts_150m2_with_stats %>%
  mutate(
    Long_UTM6S = st_coordinates(st_centroid(geometry))[,1], # extract longitude from centroid
    Lat_UTM6S = st_coordinates(st_centroid(geometry))[,2])  # extract latitude from centroid

belts_60m2_with_stats = belts_60m2_with_stats %>%
  mutate(
    Long_UTM6S = st_coordinates(st_centroid(geometry))[,1], # extract longitude from centroid
    Lat_UTM6S = st_coordinates(st_centroid(geometry))[,2])  # extract latitude from centroid

# ggplot() +
#   geom_sf(data = belts_150m2_with_stats, fill = "lightblue", color = "black", alpha = 0.5) +  # plot polygons
#   geom_point(data = belts_150m2_with_stats, aes(x = Long_UTM6S, y = Lat_UTM6S), color = "red", size = 2) +  # plot centroids
#   labs(title = "Polygon Centroids Verification", x = "Longitude", y = "Latitude") +
#   theme_minimal()

#### MOTU VEGETATION INFO ####
# finally, add the information from Eleanor on the cover of native flora vs. coconut monocrop on the motu,
# which will influence accretion/erosion processes and potential sedimentation on the nearby reefs. 
veg = read.csv(here("Data", "Terrestrial_Vegetation", 
                    "Habitat_Cover_Chagos_Tetiaroa_Seychelles_CES.csv"))

# keep only the Tetiaroa observations
veg = veg %>%
  filter(Region == "Tetiaroa")

# create a named vector of Native_Percent values for each island
require(stringr)
require(tibble)
island_means = veg %>%
  group_by(Island) %>%
  summarise(mean_native = mean(Native_percent, na.rm = TRUE)) %>%
  deframe()

# precompute the means for the hoas
mean_tauini_auroa = veg %>%
  filter(Island %in% c("Tauini", "Auroa")) %>%
  summarise(mean_val = mean(Native_percent, na.rm = TRUE)) %>%
  pull(mean_val)

mean_hiraanae_auroa = veg %>%
  filter(Island %in% c("Hiraanae", "Auroa")) %>%
  summarise(mean_val = mean(Native_percent, na.rm = TRUE)) %>%
  pull(mean_val)

# assign values to belts_150m2_with_stats
belts_150m2_with_stats = belts_150m2_with_stats %>%
  mutate(Native_Vegetation = case_when(
    str_detect(Transect_ID, "Onetahi") ~ island_means["Onetahi"],
    str_detect(Transect_ID, "Honuea") ~ island_means["Honuea"],
    str_detect(Transect_ID, "Reiono") ~ island_means["Reiono"],
    str_detect(Transect_ID, "Rimatuu") ~ island_means["Rimatuu"],
    str_detect(Transect_ID, "Hiraanae") & !str_detect(Transect_ID, "Hiraanae_Auroa") ~ island_means["Hiraanae"],
    str_detect(Transect_ID, "Oroatera") ~ island_means["Oroatera"],
    str_detect(Transect_ID, "Aie") ~ island_means["Aie"],
    str_detect(Transect_ID, "Tiaraunu") ~ island_means["Tiaraunu"],
    str_detect(Transect_ID, "Tauini_Auroa|Tauini_Hoa") ~ mean_tauini_auroa,
    str_detect(Transect_ID, "Hiraanae_Auroa") ~ mean_hiraanae_auroa,
    TRUE ~ NA_real_
  )) %>%
  relocate(Native_Vegetation, .before = "Long_UTM6S")

# repeat for the 60 m2 belts
belts_60m2_with_stats = belts_60m2_with_stats %>%
  mutate(Native_Vegetation = case_when(
    str_detect(Transect_ID, "Onetahi") ~ island_means["Onetahi"],
    str_detect(Transect_ID, "Honuea") ~ island_means["Honuea"],
    str_detect(Transect_ID, "Reiono") ~ island_means["Reiono"],
    str_detect(Transect_ID, "Rimatuu") ~ island_means["Rimatuu"],
    str_detect(Transect_ID, "Hiraanae") & !str_detect(Transect_ID, "Hiraanae_Auroa") ~ island_means["Hiraanae"],
    str_detect(Transect_ID, "Oroatera") ~ island_means["Oroatera"],
    str_detect(Transect_ID, "Aie") ~ island_means["Aie"],
    str_detect(Transect_ID, "Tiaraunu") ~ island_means["Tiaraunu"],
    str_detect(Transect_ID, "Tauini_Auroa|Tauini_Hoa") ~ mean_tauini_auroa,
    str_detect(Transect_ID, "Hiraanae_Auroa") ~ mean_hiraanae_auroa,
    TRUE ~ NA_real_
  )) %>%
  relocate(Native_Vegetation, .before = "Long_UTM6S")

#### SAVE CSVs WITH BELT CONDITIONS ####
write.csv(st_drop_geometry(belts_150m2_with_stats),
          here("Data", "Transects", "Conditions_150m2_Transects.csv"),
          row.names = FALSE)
write.csv(st_drop_geometry(belts_60m2_with_stats),
          here("Data", "Transects", "Conditions_60m2_Transects.csv"),
          row.names = FALSE)

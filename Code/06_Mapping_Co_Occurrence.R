#### CONTACT ####
# Courtney Stuart (courtney.e.stuart@gmail.com; courtney.stuart@mansfield.ox.ac.uk)

#### LIBRARIES ####
# install packages (first run only)
# install.packages(c("easypackages", "dplyr", "tidyr", "here", "purrr", "PNWColors", 
#                    "ggplot2", "stringr", "cowplot", "patchwork"))

# load all libraries at once with easypackages
library(easypackages)
libraries("dplyr", "tidyr", "here", "purrr", "PNWColors", "ggplot2", "stringr",
          "cowplot", "patchwork")

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

##### LOAD MODEL DATASETS ####
load(here("Data", "Model_Datasets", "Grazer_Data.RData"))
load(here("Data", "Model_Datasets", "Scraper_Data.RData"))
load(here("Data", "Model_Datasets", "Territorial_Data.RData"))
load(here("Data", "Model_Datasets", "Functional_Diversity_Data.RData"))
tmp_env = new.env()
load(here("Code", "01_Combine_Fish_Data.RData"), envir = tmp_env)
herbs = tmp_env$herbs

#### PREPARE MAPPING DATA #### 
# create a browser_data frame
browser_data = herbs %>%
  # keep only Browsers
  filter(Functional_Group == "Browsers") %>%
  # group by Transect_ID
  group_by(Transect_ID) %>%
  # summarise total abundance and species richness (number of unique species)
  summarise(
    Browser_Abundance = sum(Abundance, na.rm = TRUE),
    Browser_Richness = n_distinct(Species)) %>%
  # make sure to include all transects, even those with 0 Browsers
  right_join(
    herbs %>% distinct(Transect_ID),
    by = "Transect_ID") %>%
  # replace NAs with 0s for transects without Browsers
  mutate(
    Browser_Abundance = replace_na(Browser_Abundance, 0),
    Browser_Richness = replace_na(Browser_Richness, 0))
# find the one transect that is missing and add it back in 
missing_from_browser = anti_join(grazer_data, browser_data, by = "Transect_ID")
missing_from_browser = missing_from_browser %>%
  mutate(Browser_Abundance = 0,
         Browser_Richness = 0) %>%
  select(Transect_ID, Browser_Abundance, Browser_Richness)
browser_data = bind_rows(browser_data, missing_from_browser)

# create a large excavators/bioeroders frame
bioeroder_data = herbs %>%
  # keep only bioeroders
  filter(Functional_Group == "Large excavators/bioeroders") %>%
  # group by Transect_ID
  group_by(Transect_ID) %>%
  # summarise total abundance and species richness (number of unique species)
  summarise(
    Bioeroder_Abundance = sum(Abundance, na.rm = TRUE),
    Bioeroder_Richness = n_distinct(Species)) %>%
  # make sure to include all transects, even those with 0 Browsers
  right_join(
    herbs %>% distinct(Transect_ID),
    by = "Transect_ID") %>%
  # replace NAs with 0s for transects without Browsers
  mutate(
    Bioeroder_Abundance = replace_na(Bioeroder_Abundance, 0),
    Bioeroder_Richness = replace_na(Bioeroder_Richness, 0))
# find the one transect that is missing and add it back in 
missing_from_bioeroder = anti_join(grazer_data, bioeroder_data, by = "Transect_ID")
missing_from_bioeroder = missing_from_bioeroder %>%
  mutate(Bioeroder_Abundance = 0,
         Bioeroder_Richness = 0) %>%
  select(Transect_ID, Bioeroder_Abundance, Bioeroder_Richness)
bioeroder_data = bind_rows(bioeroder_data, missing_from_bioeroder)

# keep only the relevant rows from grazers, scrapers, and territorials
grazer_data = grazer_data %>% select(Transect_ID, Grazer_Abundance, Grazer_Richness)
scraper_data = scraper_data %>% select(Transect_ID, Scraper_Abundance, Scraper_Richness)
territorial_data = territorial_data %>% select(Transect_ID, Territorial_Abundance, Territorial_Richness)

# list all herbivore groups to be joined
herbivore_data_list = list(
  bioeroder_data,
  browser_data,
  grazer_data,
  scraper_data,
  territorial_data)

# check to make sure that Onetahi_Leeward_08D is included properly, as this site had NO herbivores at all
sapply(herbivore_data_list, function(df) "Onetahi_Leeward_08D" %in% df$Transect_ID)
lapply(herbivore_data_list, function(df) df %>% filter(Transect_ID == "Onetahi_Leeward_08D"))

# run the join
herbivore_data = reduce(herbivore_data_list, function(x, y) {
  full_join(x, y, by = "Transect_ID")})

# add the transect metadata
transect_metadata = herbs %>%
  select(Transect_ID, Date, Year, Start_Time, Motu, Site, Transect, Observer) %>%
  distinct()

# metadata for "Onetahi_Leeward_08D" is missing, we need to add it
transect_metadata = bind_rows(
  transect_metadata,
  tibble(
    Transect_ID = factor("Onetahi_Leeward_08D", levels = levels(transect_metadata$Transect_ID)),
    Date = "23/09/2023",
    Year = factor("2023", levels = levels(transect_metadata$Year)),
    Start_Time = "15:49",
    Motu = factor("Onetahi", levels = levels(transect_metadata$Motu)),
    Site = "Leeward",
    Transect = "08D",
    Observer = factor("KAS", levels = levels(transect_metadata$Observer))))

# pull together the herbivore community data across all transects
herbivore_data = left_join(transect_metadata, herbivore_data, by = "Transect_ID")

# bring in the coordinates of the transect centroids
centroids = read.csv(here("Data", "Transects", "Conditions_150m2_Transects.csv")) %>% 
  select(Transect_ID, Long_UTM6S, Lat_UTM6S)

# pull everything together
herbivore_data = left_join(herbivore_data, centroids) %>%
  mutate(
    Total_Abundance = Bioeroder_Abundance + Browser_Abundance + Grazer_Abundance + 
      Scraper_Abundance + Territorial_Abundance,
    Total_Richness = Bioeroder_Richness + Browser_Richness + Grazer_Richness + 
      Scraper_Richness + Territorial_Richness)

# ensure that there are no special characters in the column names
names(herbivore_data) = make.names(names(herbivore_data))

# finally, save the data for mapping
write.csv(herbivore_data,
          file = here("Data", "GIS", "Herbivore_Community_Mapping_Data.csv"), 
          row.names = FALSE)

#### EXPLORE CO-OCCURRENCE ####
# what can we say about co-occurrence? let's quantify these patterns as well as visualise them
# spatially on a map

# calculate binary presence-absence matrix
presence_absence = herbivore_data[, grep("_Abundance$", names(herbivore_data))] > 0
colnames(presence_absence) = gsub("_Abundance", "", colnames(presence_absence))

# ensure it's a matrix
presence_absence = as.matrix(presence_absence)

# pairwise co-occurrence counts
co_occurrence_counts = t(presence_absence) %*% presence_absence

# row totals: number of transects where each group is present
group_totals = diag(co_occurrence_counts)

# co-occurrence proportions (proportion of group A transects that also had group B)
co_occurrence_proportions = sweep(co_occurrence_counts, 1, group_totals, FUN = "/")

# round and set diagonals to NA for readability
diag(co_occurrence_proportions) = NA
round(co_occurrence_proportions, 2)

# interpretation: 
# row = "given that this group is present"
# column = "what proportion of those transects also had this group"

#### EXPLORE GENERAL SPATIAL PATTERNS ####
herbivore_data2 = herbivore_data %>%
  rowwise() %>%
  mutate(Total_Abundance = sum(c_across(ends_with("_Abundance")), na.rm = TRUE)) %>%
  mutate(Total_Richness = sum(c_across(ends_with("_Richness")), na.rm = TRUE)) %>%
  ungroup()

# calculate the 90th percentile (top 10%)
abundance_threshold = quantile(herbivore_data2$Total_Abundance, 0.90, na.rm = TRUE)

# filter for top 10% of transects
top_10_abundance = herbivore_data2 %>%
  filter(Total_Abundance >= abundance_threshold)

# calculate the 10th percentile (bottom 10%)
abundance_threshold = quantile(herbivore_data2$Total_Abundance, 0.10, na.rm = TRUE)

# filter for bottom 10% of transects
bottom_10_abundance = herbivore_data2 %>%
  filter(Total_Abundance <= abundance_threshold)

# calculate the 90th percentile (top 10%) for richness
richness_threshold_top = quantile(herbivore_data2$Total_Richness, 0.90, na.rm = TRUE)

# filter for top 10% of transects based on richness
top_10_richness = herbivore_data2 %>%
  filter(Total_Richness >= richness_threshold_top)

# calculate the 10th percentile (bottom 10%) for richness
richness_threshold_bottom = quantile(herbivore_data2$Total_Richness, 0.10, na.rm = TRUE)

# filter for bottom 10% of transects based on richness
bottom_10_richness = herbivore_data2 %>%
  filter(Total_Richness <= richness_threshold_bottom)

#### PLOT ALL RESPONSE VARIABLES ACROSS ALL TRANSECTS ####
# now plot functional diversity, total herbivore abundance and richness across all transects using
# stacked bar charts to show relative functional group contributions

# we lost the functional diversity info somewhere along the way...add it back using unique transect info
plotting_data = left_join(herbivore_data, 
                           (functional_diversity %>%
                              select(Transect_ID, 
                                     Functional_Diversity,
                                     Long_UTM6S, Lat_UTM6S)))

# arrange alphabetically by motu, group, and create new, simple transect IDs for plotting. then,
# calculate new columns that tell us whether each functional group was present (1) or not (0) on
# each transect
plotting_data = plotting_data %>%
  arrange(Motu) %>%
  group_by(Motu) %>%
  mutate(
    # Split Motu by underscore into parts
    parts = str_split(Motu, pattern = "_"),
    # Take first 3 letters of first two parts, uppercase, and join by underscore
    prefix = map_chr(parts, ~ {
      p <- .x
      first_part  <- ifelse(length(p) >= 1, str_to_upper(str_sub(p[1], 1, 3)), "")
      second_part <- ifelse(length(p) >= 2, str_to_upper(str_sub(p[2], 1, 3)), "")
      if (second_part != "") {
        paste0(first_part, "_", second_part)
      } else {
        first_part
      }
    }),
    Transect_ID_Simple = paste0(prefix, "_", sprintf("%02d", row_number()))
  ) %>%
  ungroup() %>%
  select(-parts) %>%  # remove intermediate column
  relocate(Transect_ID_Simple, .after = Transect_ID) %>%
  mutate(
    Browser_Present     = as.integer(Browser_Abundance     > 0),
    Bioeroder_Present   = as.integer(Bioeroder_Abundance   > 0),
    Scraper_Present     = as.integer(Scraper_Abundance     > 0),
    Grazer_Present      = as.integer(Grazer_Abundance      > 0),
    Territorial_Present = as.integer(Territorial_Abundance > 0)
  ) %>%
  relocate(Browser_Present, .before = Browser_Abundance) %>%
  relocate(Bioeroder_Present, .before = Bioeroder_Abundance) %>%
  relocate(Grazer_Present, .before = Grazer_Abundance) %>%
  relocate(Scraper_Present, .before = Scraper_Abundance) %>%
  relocate(Territorial_Present, .before = Territorial_Abundance)

##### Functional Diversity #####
# get all transects (unique IDs)
all_transects = plotting_data %>%
  distinct(Transect_ID_Simple)

# pivot to long format
diversity_long = plotting_data %>%
  pivot_longer(
    cols = ends_with("_Present"),
    names_to = "Functional_Group",
    values_to = "Presence") %>%
  mutate(
    Functional_Group = case_when(
      Functional_Group == "Browser_Present"    ~ "Browsers",
      Functional_Group == "Grazer_Present"     ~ "Grazers/detritivores",
      Functional_Group == "Scraper_Present"    ~ "Small excavators/scrapers",
      Functional_Group == "Bioeroder_Present"  ~ "Large excavators/bioeroders",
      Functional_Group == "Territorial_Present"~ "Territorial algae/detritus feeders",
      TRUE ~ Functional_Group)) %>%
  filter(Presence == 1)

# join all transects with functional groups present (long format)
diversity_long_all = all_transects %>%
  left_join(diversity_long, by = "Transect_ID_Simple") %>%
  mutate(Presence = ifelse(is.na(Presence), 0, Presence))  %>% 
  mutate(Transect_ID_Simple = factor(Transect_ID_Simple, levels = sort(unique(Transect_ID_Simple))))

max_diversity = as.integer(max(diversity_long$Functional_Diversity))

# plot the results
diversity_plot = 
  ggplot(diversity_long_all, 
         aes(x = Transect_ID_Simple, y = Presence, fill = Functional_Group)) +
  geom_bar(stat = "identity") +  
  theme_minimal() +  
  labs(x = "Transect ID", y = "Functional diversity") +  
  theme(
    axis.title.y = element_text(size = 14),
    axis.title.x = element_blank(),
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 12),
    axis.line = element_line(color = "black", linewidth = 0.5),
    plot.title = element_blank(),
    panel.grid = element_blank(),
    legend.position = "top",
    legend.justification = "center",
    legend.direction = "horizontal",
    legend.title = element_blank(),
    legend.text = element_text(size = 12)) +
  scale_fill_manual(
    values = c(
      "Browsers" = browser_col, 
      "Grazers/detritivores" = grazer_col, 
      "Small excavators/scrapers" = scraper_col, 
      "Large excavators/bioeroders" = bioeroder_col, 
      "Territorial algae/detritus feeders" = territorial_col),
    na.translate = FALSE) +  # remove grey NA legend box
  scale_y_continuous(breaks = seq(0, max_diversity, length.out = 3), expand = c(0, 0)) +
  guides(fill = guide_legend(nrow = 1))   # single-row legend

diversity_plot

##### Abundance #####
# pivot functional group abundances into long format
abundance_long = plotting_data %>%
  select(Transect_ID_Simple, 
         Grazer_Abundance, 
         Browser_Abundance, 
         Bioeroder_Abundance, 
         Scraper_Abundance, 
         Territorial_Abundance) %>%
  pivot_longer(
    cols = ends_with("_Abundance"),
    names_to = "Functional_Group",
    values_to = "Abundance") %>%
  mutate(
    Functional_Group = case_when(
      Functional_Group == "Grazer_Abundance"     ~ "Grazers/detritivores",
      Functional_Group == "Browser_Abundance"    ~ "Browsers",
      Functional_Group == "Bioeroder_Abundance"  ~ "Large excavators/bioeroders",
      Functional_Group == "Scraper_Abundance"    ~ "Small excavators/scrapers",
      Functional_Group == "Territorial_Abundance"~ "Territorial algae/detritus feeders")) %>%
  complete(
    Transect_ID_Simple = unique(plotting_data$Transect_ID_Simple),
    Functional_Group,
    fill = list(Abundance = 0)) %>%
  mutate(Transect_ID_Simple = factor(Transect_ID_Simple, levels = sort(unique(plotting_data$Transect_ID_Simple))))

# calculate the max abundance ever recorded
max_abundance = plotting_data %>%
  rowwise() %>%
  mutate(Total_Abundance = sum(c_across(c(Grazer_Abundance, Browser_Abundance, Bioeroder_Abundance, Scraper_Abundance, Territorial_Abundance)))) %>%
  ungroup() %>%
  summarise(Max_Abundance = max(Total_Abundance, na.rm = TRUE)) %>%
  pull(Max_Abundance)

abundance_plot =
  ggplot(abundance_long, 
         aes(x = Transect_ID_Simple, y = Abundance, fill = Functional_Group)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(x = "Transect ID", y = "Total herbivore abundance") +
  scale_fill_manual(
    values = c(
      "Browsers" = browser_col,
      "Grazers/detritivores" = grazer_col,
      "Small excavators/scrapers" = scraper_col,
      "Large excavators/bioeroders" = bioeroder_col,
      "Territorial algae/detritus feeders" = territorial_col),
    na.translate = FALSE) +
  scale_y_continuous(breaks = seq(0, max_abundance, length.out = 3), expand = c(0, 0)) +
  theme(
    axis.title = element_text(size = 14),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_blank(),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 12),
    axis.line = element_line(color = "black", linewidth = 0.5),
    panel.grid = element_blank(),
    legend.position = "top",
    legend.justification = "center",
    legend.direction = "horizontal",
    legend.title = element_blank(),
    legend.text = element_text(size = 6)) +
  guides(fill = guide_legend(nrow = 1))

abundance_plot

##### Richness #####
# repeat the same process as above, but for richness instead of abundance
richness_long = plotting_data %>%
  select(Transect_ID_Simple, 
         Grazer_Richness, 
         Browser_Richness, 
         Bioeroder_Richness, 
         Scraper_Richness, 
         Territorial_Richness) %>%
  pivot_longer(
    cols = ends_with("_Richness"),
    names_to = "Functional_Group",
    values_to = "Richness") %>%
  mutate(
    Functional_Group = case_when(
      Functional_Group == "Grazer_Richness"     ~ "Grazers/detritivores",
      Functional_Group == "Browser_Richness"    ~ "Browsers",
      Functional_Group == "Bioeroder_Richness"  ~ "Large excavators/bioeroders",
      Functional_Group == "Scraper_Richness"    ~ "Small excavators/scrapers",
      Functional_Group == "Territorial_Richness"~ "Territorial algae/detritus feeders")) %>%
  complete(
    Transect_ID_Simple = unique(plotting_data$Transect_ID_Simple),
    Functional_Group,
    fill = list(Richness = 0)) %>%
  mutate(Transect_ID_Simple = factor(Transect_ID_Simple, levels = sort(unique(plotting_data$Transect_ID_Simple))))

# calculate the max richness ever recorded
max_richness = plotting_data %>%
  rowwise() %>%
  mutate(Total_Richness = sum(c_across(c(Grazer_Richness, Browser_Richness, Bioeroder_Richness, Scraper_Richness, Territorial_Richness)))) %>%
  ungroup() %>%
  summarise(Max_Richness = max(Total_Richness, na.rm = TRUE)) %>%
  pull(Max_Richness)

richness_plot =
  ggplot(richness_long, 
         aes(x = Transect_ID_Simple, y = Richness, fill = Functional_Group)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(x = "Transect ID", y = "Total herbivore richness") +
  scale_fill_manual(
    values = c(
      "Browsers" = browser_col,
      "Grazers/detritivores" = grazer_col,
      "Small excavators/scrapers" = scraper_col,
      "Large excavators/bioeroders" = bioeroder_col,
      "Territorial algae/detritus feeders" = territorial_col),
    na.translate = FALSE) +
  scale_y_continuous(
    breaks = round(seq(0, max_richness, length.out = 3)),
    limits = c(0, max_richness),
    expand = c(0, 0)) +
  theme(
    axis.title = element_text(size = 14),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_blank(),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 12),
    axis.line = element_line(color = "black", linewidth = 0.5),
    panel.grid = element_blank(),
    legend.position = "top",
    legend.justification = "center",
    legend.direction = "horizontal",
    legend.title = element_blank(),
    legend.text = element_text(size = 6)) +
  guides(fill = guide_legend(nrow = 1))

richness_plot

##### Stacked Plots #####
# remove x-axis texts and titles from plots A and B, keep only in C:
diversity_plot_no_x = diversity_plot + theme(
  axis.title.x = element_blank(),
  axis.text.x = element_blank(),
  axis.ticks.x = element_blank())

abundance_plot_no_x = abundance_plot + theme(
  axis.title.x = element_blank(),
  axis.text.x = element_blank(),
  axis.ticks.x = element_blank())

# remove legends from all plots
diversity_plot_no_legend = diversity_plot_no_x + theme(legend.position = "none")
abundance_plot_no_legend = abundance_plot_no_x + theme(legend.position = "none")
richness_plot_no_legend = richness_plot + theme(legend.position = "none")

# combine plots, collect legends — but since all have legend.position = "none", no legend will appear by default
combined_plot = (diversity_plot_no_legend + abundance_plot_no_legend + richness_plot_no_legend) +
  plot_layout(ncol = 1, heights = c(1, 1, 1), guides = "collect") +  # this collects legends but none exist now
  plot_annotation(tag_levels = "A")

# manually add legend back **only once** from the original diversity_plot
# but patchwork won't add legends if all are removed, so you *need* at least one plot with a legend.

# so better approach: keep legend only in diversity_plot_no_x, remove from others
diversity_plot_no_x_legend = diversity_plot_no_x  # keep legend here
abundance_plot_no_legend = abundance_plot_no_x + theme(legend.position = "none")
richness_plot_no_legend = richness_plot + theme(legend.position = "none")

combined_plot = (diversity_plot_no_x_legend + abundance_plot_no_legend + richness_plot_no_legend) +
  plot_layout(ncol = 1, heights = c(1, 1, 1), guides = "collect") +
  plot_annotation(tag_levels = "A")

combined_plot = combined_plot & theme(
  legend.position = "top",
  legend.direction = "horizontal",
  legend.box.just = "center",
  legend.title = element_blank(),
  legend.key.size = unit(1, "lines"),
  legend.spacing.x = unit(0.3, "cm"),
  legend.text = element_text(size = 12),
  plot.margin = margin(5, 5, 5, 5)
)

combined_plot

# finally! save the plot
ggsave(combined_plot,
       dpi = 600,
       width = 14,
       height = 10,
       units = "in",
       bg = "white",
       filename = here("Figures", "Transect_Level_Responses.png"))

#### PLOT ABUNDANCE & RICHNESS AT THE MOTU LEVEL ####
# make sure Motu labels are correct
plotting_data_motu =  plotting_data %>%
  mutate(
    Motu = as.character(Motu),  # Convert from factor to character
    Motu = case_when(
      Motu %in% c("Tauini", "Tauini_Auroa") ~ "Tauini-Ahuroa Channel",
      Motu == "Hiraanae_Auroa" ~ "Hīra‘a‘ānae-Ahuroa Channel",
      Motu == "Hiraanae" ~ "Hīra‘a‘ānae", 
      Motu == "Aie" ~ "'Ă'ie",
      Motu == "Tiaraunu" ~ "Ti'ara'aunu",
      Motu == "Rimatuu" ~ "Rimatu'u",
      TRUE ~ Motu))

# summarize total abundance by Motu and functional group
abundance_motu = plotting_data_motu %>%
  group_by(Motu) %>%
  summarise(
    Territorial = sum(Territorial_Abundance),
    Scraper = sum(Scraper_Abundance),
    Grazer = sum(Grazer_Abundance),
    Browser = sum(Browser_Abundance),
    Bioeroder = sum(Bioeroder_Abundance)) %>%
  pivot_longer(cols = -Motu, names_to = "Functional_Group", values_to = "Abundance") %>%
  group_by(Motu) %>%
  mutate(Relative_Abundance = Abundance / sum(Abundance) * 100) %>%
  ungroup()

# summarize total richness by Motu and functional group
richness_motu = plotting_data_motu %>%
  group_by(Motu) %>%
  summarise(
    Territorial = sum(Territorial_Richness),
    Scraper = sum(Scraper_Richness),
    Grazer = sum(Grazer_Richness),
    Browser = sum(Browser_Richness),
    Bioeroder = sum(Bioeroder_Richness)) %>%
  pivot_longer(cols = -Motu, names_to = "Functional_Group", values_to = "Richness") %>%
  group_by(Motu) %>%
  mutate(Relative_Richness = Richness / sum(Richness) * 100) %>%
  ungroup()

# define colors (using those previously created)
functional_colors = c(
  Territorial = territorial_col,
  Scraper = scraper_col,
  Grazer = grazer_col,
  Browser = browser_col,
  Bioeroder = bioeroder_col)

# plot relative contributions of each functional group to absolute herbivore abundance across motu
motu_abundance_plot = 
  ggplot(abundance_motu, aes(x = Motu, y = Relative_Abundance, fill = Functional_Group)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = functional_colors,
                    labels = c(
                      Grazer = "Grazers/detritivores",
                      Scraper = "Small excavators/scrapers",
                      Browser = "Browsers",
                      Bioeroder = "Large excavators/Bioeroders",
                      Territorial = "Territorial detritus/algae feeders")) +
  labs(
    y = "Relative Abundance (%)",
    x = "Motu",
    fill = "Functional Group") +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size = 16, color = "black"),
        axis.text.x = element_text(size = 14, color = "black", angle = 30, hjust = 1),
        axis.text.y = element_text(size = 14, color = "black"),
        axis.line = element_line(color = "black", linewidth = 0.5),
        panel.grid = element_blank())
motu_abundance_plot

# plot relative contributions of each functional group to absolute herbivore richness across motu
motu_richness_plot =
  ggplot(richness_motu, aes(x = Motu, y = Relative_Richness, fill = Functional_Group)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = functional_colors,
                    labels = c(
                      Grazer = "Grazers/detritivores",
                      Scraper = "Small excavators/scrapers",
                      Browser = "Browsers",
                      Bioeroder = "Large excavators/Bioeroders",
                      Territorial = "Territorial detritus/algae feeders")) +
  labs(
    y = "Relative Richness (%)",
    x = "Motu",
    fill = "Functional Group") +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size = 16, color = "black"),
        axis.text.x = element_text(size = 14, color = "black", angle = 30, hjust = 1),
        axis.text.y = element_text(size = 14, color = "black"),
        axis.line = element_line(color = "black", linewidth = 0.5),
        panel.grid = element_blank())

# remove x-axis text from top plot (abundance) and add panel A label
motu_abundance_plot2 = motu_abundance_plot +
  theme(axis.text.x = element_blank()) +
  labs(tag = "A")

# add panel B label
motu_richness_plot2 = motu_richness_plot +
  labs(tag = "B")

# combine panels and place legend on top
combined_plot = motu_abundance_plot2 / motu_richness_plot2 +
  plot_layout(guides = "collect", heights = c(1, 1)) &
  theme(legend.position = "top",
        legend.direction = "horizontal",
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        plot.tag = element_text(face = "bold", size = 14)) &
  scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 10))

combined_plot

# finally! save the plot
ggsave(combined_plot,
       dpi = 600,
       width = 14,
       height = 10,
       units = "in",
       bg = "white",
       filename = here("Figures", "Motu_Level_Responses.png"))

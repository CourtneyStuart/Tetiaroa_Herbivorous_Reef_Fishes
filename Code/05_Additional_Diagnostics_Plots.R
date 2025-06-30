#### CONTACT ####
# Courtney Stuart (courtney.e.stuart@gmail.com; courtney.stuart@mansfield.ox.ac.uk)

#### LIBRARIES ####
# install packages (first run only)
# install.packages(c("easypackages", "conflicted", "dplyr", "here", "forcats", "lubridate",
#                    "stringr", "tidyr", "ggplot2", "RColorBrewer", "lme4", "lmerTest",
#                    "raster", "sf", "sp", "terra", "PNWColors", "usdm", "corrplot",
#                    "purrr", "MuMIn", "ggplot2", "car", "spdep", "broom", "fishualize", "cowplot"))

# load all libraries at once with easypackages
library(easypackages)
libraries("easypackages", "conflicted", "dplyr", "here", "forcats", "lubridate",
          "stringr", "tidyr", "ggplot2", "RColorBrewer", "lme4", "lmerTest",
          "raster", "sf", "sp", "terra", "PNWColors", "usdm", "corrplot", "purrr", 
          "MuMIn", "ggplot2", "car", "spdep", "broom", "fishualize", "cowplot")
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

#### LOAD DATA ####
# load the model datasets
load(here("Data", "Model_Datasets", "Functional_Diversity_Data.RData"))
load(here("Data", "Model_Datasets", "Grazer_Data.RData"))
load(here("Data", "Model_Datasets", "Scraper_Data.RData"))
load(here("Data", "Model_Datasets", "Territorial_Data.RData"))

# center and scale the explanatory variables, as we did ahead of modelling
vars_to_scale = c("Depth_Max", "Rugosity_Max", "Seabird_N15_Max", 
                  "Grazing_Surface_Area", "REI", "Fishing_Proximity", 
                  "Native_Vegetation")
pretty_labels = c(
  "Depth_Max" = "Depth below the surface (−m)",
  "Seabird_N15_Max" = "Seabird nutrient enrichment (δ¹⁵N)",
  "Grazing_Surface_Area" = "Grazing area",
  "REI" = "Relative exposure index",
  "Rugosity_Max" = "Maximum rugosity",
  "Native_Vegetation" = "Native vegetation cover",
  "Fishing_Proximity" = "Distance to spearfishing")

# apply across all datasets
functional_diversity = functional_diversity %>%
  mutate(across(all_of(vars_to_scale), ~ scale(.)[,1]))

grazer_data = grazer_data %>%
  mutate(across(all_of(vars_to_scale), ~ scale(.)[,1]))

scraper_data = scraper_data %>%
  mutate(across(all_of(vars_to_scale), ~ scale(.)[,1]))

territorial_data = territorial_data %>%
  mutate(across(all_of(vars_to_scale), ~ scale(.)[,1]))

# load the fitted models
load(here("Data", "Fitted_Models", "Functional_Diversity_Poisson_Model.RData"))
load(here("Data", "Fitted_Models", "Grazer_Abundance_Negative_Binomial_Model.RData"))
load(here("Data", "Fitted_Models", "Grazer_Richness_Poisson_Model.RData"))
load(here("Data", "Fitted_Models", "Scraper_Abundance_Negative_Binomial_Model.RData"))
load(here("Data", "Fitted_Models", "Scraper_Richness_Poisson_Model.RData"))
load(here("Data", "Fitted_Models", "Territorial_Abundance_Negative_Binomial_Model.RData"))
load(here("Data", "Fitted_Models", "Territorial_Richness_Poisson_Model.RData"))

#### GRAZERS ####
##### ABUNDANCE #####
# extract tidy coefficients with confidence intervals
ga_tidy = tidy(ga_negbin, conf.int = TRUE)

# plot all predictors (excluding intercept)
ga_tidy %>%
  filter(term != "(Intercept)") %>%
  ggplot(aes(x = term, y = estimate)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  coord_flip() +
  labs(
    x = "Predictor (Standardised)",
    y = "Coefficient Estimate",
    title = "Effect Sizes in Negative Binomial GLM") +
  scale_x_discrete(labels = pretty_labels) +
  theme_bw() +
  theme(panel.grid = element_blank())

##### RICHNESS #####
# extract tidy coefficients with confidence intervals
gr_tidy = tidy(gr_poisson, conf.int = TRUE)

# plot all predictors (excluding intercept)
gr_tidy %>%
  filter(term != "(Intercept)") %>%
  ggplot(aes(x = term, y = estimate)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  coord_flip() +
  labs(
    x = "Predictor (Standardised)",
    y = "Coefficient Estimate",
    title = "Effect Sizes in Poisson GLM") +
  scale_x_discrete(labels = pretty_labels) +
  theme_bw() +
  theme(panel.grid = element_blank())

#### SCRAPERS ####
##### ABUNDANCE #####
# extract tidy coefficients with confidence intervals
sa_tidy = tidy(sa_negbin, conf.int = TRUE)

# plot all predictors (excluding intercept)
sa_tidy %>%
  filter(term != "(Intercept)") %>%
  ggplot(aes(x = term, y = estimate)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  coord_flip() +
  labs(
    x = "Predictor (Standardised)",
    y = "Coefficient Estimate",
    title = "Effect Sizes in Negative Binomial GLM") +
  scale_x_discrete(labels = pretty_labels) +
  theme_bw() +
  theme(panel.grid = element_blank())

##### RICHNESS #####
# extract tidy coefficients with confidence intervals
sr_tidy = tidy(sr_poisson, conf.int = TRUE)

# plot all predictors (excluding intercept)
sr_tidy %>%
  filter(term != "(Intercept)") %>%
  ggplot(aes(x = term, y = estimate)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  coord_flip() +
  labs(
    x = "Predictor (Standardised)",
    y = "Coefficient Estimate",
    title = "Effect Sizes in Poisson GLM") +
  scale_x_discrete(labels = pretty_labels) +
  theme_bw() +
  theme(panel.grid = element_blank())

#### TERRITORIALS ####
##### ABUNDANCE #####
# extract tidy coefficients with confidence intervals
ta_tidy = tidy(ta_negbin, conf.int = TRUE)

# plot all predictors (excluding intercept)
ta_tidy %>%
  filter(term != "(Intercept)") %>%
  ggplot(aes(x = term, y = estimate)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  coord_flip() +
  labs(
    x = "Predictor (Standardised)",
    y = "Coefficient Estimate",
    title = "Effect Sizes in Negative Binomial GLM") +
  scale_x_discrete(labels = pretty_labels) +
  theme_bw() +
  theme(panel.grid = element_blank())

##### RICHNESS #####
# extract tidy coefficients with confidence intervals
tr_tidy = tidy(tr_poisson, conf.int = TRUE)

# plot all predictors (excluding intercept)
tr_tidy %>%
  filter(term != "(Intercept)") %>%
  ggplot(aes(x = term, y = estimate)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  coord_flip() +
  labs(
    x = "Predictor (Standardised)",
    y = "Coefficient Estimate",
    title = "Effect Sizes in Poisson GLM") +
  scale_x_discrete(labels = pretty_labels) +
  theme_bw() +
  theme(panel.grid = element_blank())

#### COMPOSITE ABUNDANCE FIGURE ####
# tidy each model and add a 'group' column
ga_tidy = tidy(ga_negbin, conf.int = TRUE) %>% 
  filter(term != "(Intercept)") %>%
  mutate(group = "Grazers/detritivores")

sa_tidy = tidy(sa_negbin, conf.int = TRUE) %>% 
  filter(term != "(Intercept)") %>%
  mutate(group = "Small excavators/scrapers")

ta_tidy = tidy(ta_negbin, conf.int = TRUE) %>% 
  filter(term != "(Intercept)") %>%
  mutate(group = "Territorial algae/detritus feeders")

# combine all into one dataframe
all_abund = bind_rows(ga_tidy, sa_tidy, ta_tidy)

# ensure terms are ordered alphabetically
all_abund = all_abund %>%
  mutate(term = factor(term, levels = sort(unique(term))))

# plot
functional_group_abundance = 
  ggplot(all_abund, aes(x = term, y = estimate, colour = group)) +
  geom_point(position = position_dodge(width = 0.6), size = 3) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                position = position_dodge(width = 0.6), width = 0.2) +
  coord_flip() +
  labs(
    x = "Predictor (Standardised)",
    y = "Coefficient Estimate",
    colour = "Group",
    title = "") +
  scale_color_manual(values = c(
    "Grazers/detritivores" = grazer_col, 
    "Small excavators/scrapers" = scraper_col, 
    "Territorial algae/detritus feeders" = territorial_col)) +
  scale_x_discrete(labels = pretty_labels) +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", linewidth = 0.5) +
  theme(
    axis.title.x = element_text(size = 14),  # x-axis label
    axis.title.y = element_text(size = 14),  # y-axis label
    axis.text.x = element_text(size = 12),   # x-axis tick text
    axis.text.y = element_text(size = 12),    # y-axis tick text
    legend.text = element_text(size = 11),
    legend.title = element_text(size = 11))

functional_group_abundance

#### COMPOSITE RICHNESS FIGURE ####
# tidy each model and add a 'group' column
gr_tidy = tidy(gr_poisson, conf.int = TRUE) %>% 
  filter(term != "(Intercept)") %>%
  mutate(group = "Grazers/detritivores")

sr_tidy = tidy(sr_poisson, conf.int = TRUE) %>% 
  filter(term != "(Intercept)") %>%
  mutate(group = "Small excavators/scrapers")

tr_tidy = tidy(tr_poisson, conf.int = TRUE) %>% 
  filter(term != "(Intercept)") %>%
  mutate(group = "Territorial algae/detritus feeders")

# combine all into one dataframe
all_rich = bind_rows(gr_tidy, sr_tidy, tr_tidy)

# ensure terms are ordered alphabetically
all_rich = all_rich %>%
  mutate(term = factor(term, levels = sort(unique(term))))

# plot
functional_group_richness = 
ggplot(all_rich, aes(x = term, y = estimate, colour = group)) +
  geom_point(position = position_dodge(width = 0.6), size = 3) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                position = position_dodge(width = 0.6), width = 0.2) +
  coord_flip() +
  labs(
    x = "Predictor (Standardised)",
    y = "Coefficient Estimate",
    colour = "Group",
    title = "") +
  scale_color_manual(values = c(
    "Grazers/detritivores" = grazer_col, 
    "Small excavators/scrapers" = scraper_col, 
    "Territorial algae/detritus feeders" = territorial_col)) +
  scale_x_discrete(labels = pretty_labels) +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", linewidth = 0.5) +
  theme(
    axis.title.x = element_text(size = 14),  # x-axis label
    axis.title.y = element_text(size = 14),  # y-axis label
    axis.text.x = element_text(size = 12),   # x-axis tick text
    axis.text.y = element_text(size = 12),    # y-axis tick text
    legend.text = element_text(size = 11),
    legend.title = element_text(size = 11))

functional_group_richness

require(cowplot)
plot_grid(
  (functional_group_abundance + theme(legend.position = "none")),
  (functional_group_richness + theme(legend.position = "none")),
  labels = c("A", "B"),
  nrow = 1, rel_widths = c(1, 1), align = "h")

# extract legend from one of the plots
shared_legend = ggplotGrob(functional_group_abundance + theme(legend.position = "bottom"))$grobs[[which(sapply(ggplotGrob(functional_group_abundance + theme(legend.position = "bottom"))$grobs, function(x) x$name) == "guide-box")]]
shared_legend

# combine the two plots horizontally
landscape_plots = plot_grid(
  (functional_group_abundance + theme(legend.position = "none")),
  (functional_group_richness + theme(legend.position = "none")),
  labels = c("(A) Functional Group Abundance",
             "(B) Functional Group Richness"),
  nrow = 1, rel_widths = c(1, 1), align = "h")

# add the shared legend below
landscape_plots = plot_grid(
  landscape_plots, shared_legend,
  ncol = 1,
  rel_heights = c(1, 0.05))
landscape_plots

ggsave(plot = landscape_plots,
       filename = here("Figures", "Standardised_Effects_on_Abundance_and_Richness_Landscape.png"),
       width = 12, height = 6, units = "in", dpi = 600, bg = "white")

# combine the two plots vertically 
portrait_plots = plot_grid(
  (functional_group_abundance + theme(legend.position = "none")),
  (functional_group_richness + theme(legend.position = "none")),
  labels = c("(A) Functional Group Abundance",
             "(B) Functional Group Richness"),
  ncol = 1, rel_widths = c(1, 1), align = "v")

# add the shared legend below
portrait_plots = plot_grid(
  portrait_plots, shared_legend,
  ncol = 1,
  rel_heights = c(1, 0.05))
portrait_plots

ggsave(plot = portrait_plots,
       filename = here("Figures", "Standardised_Effects_on_Abundance_and_Richness_Portrait.png"),
       width = 8, height = 10, units = "in", dpi = 600, bg = "white")


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

transects_150m = read.csv(here("Data", "Transects", "Conditions_150m2_Transects.csv"))
transects_60m = read.csv(here("Data", "Transects", "Conditions_60m2_Transects.csv"))

herbs_transects = left_join(herbs, transects_150m)
herbs_transects = left_join(herbs_transects, transects_60m)

# step 1: filter and summarize the data
all_territorials = herbs_transects %>%
  filter(Functional_Group == "Territorial algae/detritus feeders") %>%
  group_by(Species, Transect_ID) %>%
  summarize(
    Total_Abundance = sum(Abundance), 
    REI = first(REI),  # use the first REI for each Species-Transect_ID combination
    Rugosity_Max = first(Rugosity_Max),
    .groups = 'drop'
  )

# step 2: define high REI and Rugosity_Max thresholds (top 25%)
high_REI_threshold = quantile(all_territorials$REI, 0.75)  # Top 25% of REI
high_Rugosity_Max_threshold = quantile(all_territorials$Rugosity_Max, 0.75)  # Top 25% of Rugosity_Max

# step 3: find the top 3 species based on high REI
top_species_high_REI = all_territorials %>%
  filter(REI >= high_REI_threshold) %>%
  group_by(Species) %>%
  tally() %>%
  arrange(desc(n)) %>%
  slice_head(n = 3)

# step 4: find the top 3 species based on high Rugosity_Max
top_species_high_Rugosity = all_territorials %>%
  filter(Rugosity_Max >= high_Rugosity_Max_threshold) %>%
  group_by(Species) %>%
  tally() %>%
  arrange(desc(n)) %>%
  slice_head(n = 3)

# view the results
top_species_high_REI
top_species_high_Rugosity

# model summaries
summary(fd_poisson)
summary(ga_negbin)
summary(sa_negbin)
summary(ta_negbin)
summary(gr_poisson)
summary(sr_poisson)
summary(tr_poisson)

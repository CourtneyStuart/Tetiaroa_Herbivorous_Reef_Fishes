#### CONTACT ####
# Courtney Stuart (courtney.e.stuart@gmail.com; courtney.stuart@mansfield.ox.ac.uk)

#### LIBRARIES ####
# install packages (first run only)
# install.packages(c("easypackages", "conflicted", "dplyr", "here", "forcats", "lubridate",
#                    "stringr", "tidyr", "ggplot2", "RColorBrewer", "lme4", "lmerTest",
#                    "raster", "sf", "sp", "terra", "PNWColors", "usdm", "corrplot",
#                    "purrr", "MuMIn", "ggplot2", "car", "spdep", "lmtest", "MASS",
#                    "Cairo", "DHARMa", "pscl"))

# load all libraries at once with easypackages
library(easypackages)
libraries("easypackages", "conflicted", "dplyr", "here", "forcats", "lubridate",
          "stringr", "tidyr", "ggplot2", "RColorBrewer", "lme4", "lmerTest",
          "raster", "sf", "sp", "terra", "PNWColors", "usdm", "corrplot",
          "purrr", "MuMIn", "ggplot2", "car", "spdep", "lmtest", "MASS", 
          "Cairo", "DHARMa", "pscl")
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

##### LOAD MODEL DATASETS ####
load(here("Data", "Model_Datasets", "Functional_Diversity_Data.RData"))
load(here("Data", "Model_Datasets", "Grazer_Data.RData"))
load(here("Data", "Model_Datasets", "Scraper_Data.RData"))
load(here("Data", "Model_Datasets", "Territorial_Data.RData"))

# check for NAs in any of the datasets
anyNA(functional_diversity)
anyNA(grazer_data)
anyNA(scraper_data)
anyNA(territorial_data)

#### CORRELATION/MULTICOLLINEARITY CHECK ####
# keep only predictor columns
corrdata = functional_diversity %>%
  select(Depth_Mean, Depth_SD, Depth_Max, Depth_Min,
         Slope_Mean, Slope_SD, Slope_Max, Slope_Min,
         Rugosity_Mean, Rugosity_SD, Rugosity_Max, Rugosity_Min,
         Seabird_N15_Mean, Seabird_N15_SD, Seabird_N15_Max, Seabird_N15_Max,
         Grazing_Surface_Area, REI, Fishing_Proximity, Native_Vegetation)

# check for correlation among the predictors
cormat = cor(corrdata, use = "complete.obs")

# save color palette
palette = pnw_palette("Shuksan2", 200, type = "continuous")

# set plotting margins
par(mar = c(0,0,0,0))

# plot correlation
corrplot(cormat, method = "color", col = palette, type = "upper",
         order = "original", addCoef.col = "black", number.cex = 0.50, 
         number.digits = 2, tl.col = "black", tl.srt = 40, tl.cex = 0.8)
Cairo(file = here("Figures", "Full_Correlation_Matrix.png"), 
      bg = "white", type = "png", units = "in", width = 6, height = 6, 
      pointsize = 12,  dpi = 600)
corrplot(cormat, method = "color", col = palette, type = "upper",
         order = "original", addCoef.col = "black", number.cex = 0.50, 
         number.digits = 2, tl.col = "black", tl.srt = 40, tl.cex = 0.8)
dev.off()

# we could run through the correlation matrix manually to find problematic
# variables...but instead we'll automate the process using the "usdm" package. 
# the vifcor function runs through all pairs of variables in the algae data 
# frame and checks whether any exceed the r threshold we define. if so, it will
# tell us where the problems are.
usdm::vifcor(corrdata, th = 0.7)

# the vifstep function runs through all pairs of variables in the algae
# data frame and checks whether any exceed the VIF threshold. we define. if so, 
# it willtell us where the problems are.
usdm::vifstep(corrdata, th = 5)

# take a closer look at the data
hist(corrdata$Depth_Max)
hist(corrdata$Depth_SD)
hist(corrdata$Slope_Max)
hist(corrdata$Rugosity_Max)
hist(corrdata$Seabird_N15_Max)
hist(corrdata$Grazing_Surface_Area)
hist(corrdata$REI)
hist(corrdata$Fishing_Proximity)
hist(corrdata$Native_Vegetation)

# reduce the number of predictors
corrdata2 = corrdata %>%
  select(Depth_Max, Rugosity_Max, Seabird_N15_Max, Grazing_Surface_Area, 
         REI, Fishing_Proximity, Native_Vegetation)

# check for correlation among the retained predictors
cormat2 = cor(corrdata2, use = "complete.obs")

# create and save the restricted correlation plot
corrplot(cormat2, method = "color", col = palette, type = "upper",
         order = "original", addCoef.col = "black", number.cex = 0.50, 
         number.digits = 2, tl.col = "black", tl.srt = 40, tl.cex = 0.8)
Cairo(file = here("Figures", "Restricted_Correlation_Matrix.png"), 
      bg = "white", type = "png", units = "in", width = 6, height = 6, 
      pointsize = 12,  dpi = 600)
corrplot(cormat2, method = "color", col = palette, type = "upper",
         order = "original", addCoef.col = "black", number.cex = 0.50, 
         number.digits = 2, tl.col = "black", tl.srt = 40, tl.cex = 0.8)
dev.off()

usdm::vifcor(corrdata2, th = 0.7)
usdm::vifstep(corrdata2, th = 5)

# reset plotting margins
par(mar = c(2,2,2,2))

#### CENTER AND SCALE ALL EXPLANATORY VARIABLES ####
vars_to_scale = c("Depth_Max", "Rugosity_Max", "Seabird_N15_Max", 
                   "Grazing_Surface_Area", "REI", "Fishing_Proximity", 
                   "Native_Vegetation")

# apply across all datasets
functional_diversity = functional_diversity %>%
  mutate(across(all_of(vars_to_scale), ~ scale(.)[,1]))

grazer_data = grazer_data %>%
  mutate(across(all_of(vars_to_scale), ~ scale(.)[,1]))

scraper_data = scraper_data %>%
  mutate(across(all_of(vars_to_scale), ~ scale(.)[,1]))

territorial_data = territorial_data %>%
  mutate(across(all_of(vars_to_scale), ~ scale(.)[,1]))

#### FUNCTIONAL DIVERSITY ####
# examine the distribution of the functional diversity response data
hist(functional_diversity$Functional_Diversity)

# the response variable does not follow a normal distribution (negative skewness, right-modal),
# is bounded by 0 below, and can take only positive integer values (here 0-4).

# fit a Poisson model to the functional diversity data
fd_poisson = glm(Functional_Diversity ~ 
                   Depth_Max + Rugosity_Max + Seabird_N15_Max + 
                   Grazing_Surface_Area + REI + Fishing_Proximity + 
                   Native_Vegetation,
                 data = functional_diversity,
                 family = poisson)
summary(fd_poisson) # check the results generally

# perform model diagnostics
dev.off()
par(mfrow = c(2,2))
plot(fd_poisson, which = 1:4, main = NULL)

# conduct an overdispersion test
fd_res_dev = deviance(fd_poisson)
fd_res_df = df.residual(fd_poisson)
overdispersion_ratio = fd_res_dev / fd_res_df
cat("Overdispersion ratio:", round(overdispersion_ratio, 2), "\n")

# simulate residuals
fd_sim_res = simulateResiduals(fd_poisson)
par(mfrow = c(1,1))
par(mar = c(4,4,3,1))
# plot(fd_sim_res, title = NULL, pch = 19, col = "black", cex = 1)
plotQQunif(fd_sim_res, pch = 19, col = "black", cex = 1) # left plot in plot.DHARMa()

# check for spatial autocorrelation
testSpatialAutocorrelation(fd_sim_res, x = functional_diversity$Long_UTM6S, y = functional_diversity$Lat_UTM6S)

# The DHARMa QQ plot suggests issues with the uniformity of the residual distribution 
# (as indicated by the Kolmogorov-Smirnov Test) and dispersion (evidence of underdispersion), 
# likely due to the limited variability in the response data and the small sample size. 

# try a negative binomial model as an alternative
fd_negbin = glm.nb(Functional_Diversity ~ 
                     Depth_Max + Rugosity_Max + Seabird_N15_Max + 
                     Grazing_Surface_Area + REI + Fishing_Proximity + 
                     Native_Vegetation,
                   data = functional_diversity,
                   na.action = na.omit)

# the negative binomial model does not converge well for our functional diversity data - the additional
# model complexity was not viable given the data limitations (small sample size, few unique response values).
summary(fd_negbin)

# conduct an overdispersion test
fd_res_dev = deviance(fd_negbin)
fd_res_df = df.residual(fd_negbin)
fd_overdispersion_ratio = fd_res_dev / fd_res_df
cat("Overdispersion ratio:", round(fd_overdispersion_ratio, 2), "\n")

# simulate residuals
fd_sim_res = simulateResiduals(fd_negbin)
par(mfrow = c(1,1))
par(mar = c(4,4,3,1))
# plot(fd_sim_res, title = NULL, pch = 19, col = "black", cex = 1)
plotQQunif(fd_sim_res, pch = 19, col = "black", cex = 1) # left plot in plot.DHARMa()

# check for spatial autocorrelation
testSpatialAutocorrelation(fd_sim_res, x = functional_diversity$Long_UTM6S, y = functional_diversity$Lat_UTM6S)

# the DHARMa QQ plot suggests that there are now issues with distribution (KS test) and dispersion.

# compare AICs
AIC(fd_poisson, fd_negbin)

# perform a likelihood ratio test
lrtest(fd_poisson, fd_negbin)
# the high p-value (0.9804) suggests that there is no significant difference between the two models.
# this implies that the additional parameter in the negative binomial model does not significantly 
# improve the fit compared to the poisson model.

# proceed with interpreting the results of the poisson model of functional diversity

#### GRAZERS ####
##### ABUNDANCE #####
# examine the distribution of the grazer abundance response data
hist(grazer_data$Grazer_Abundance)

# the response variable does not follow a normal distribution (positive skew, left-modal),
# is bounded by 0 below, and can take only positive integer values (here 0-50).

# fit Poisson model
ga_poisson = glm(Grazer_Abundance ~ 
                   Depth_Max + Rugosity_Max + Seabird_N15_Max + 
                   Grazing_Surface_Area + REI + Fishing_Proximity + 
                   Native_Vegetation,
                 data = grazer_data,
                 family = poisson)
summary(ga_poisson)

# perform model diagnostics
par(mfrow = c(2,2))
plot(ga_poisson, which = 1:4)

# conduct an overdispersion test
ga_res_dev = deviance(ga_poisson)
ga_res_df = df.residual(ga_poisson)
ga_overdispersion_ratio = ga_res_dev / ga_res_df
cat("Overdispersion ratio:", round(overdispersion_ratio, 2), "\n")

# simulate residuals
ga_sim_res = simulateResiduals(ga_poisson)
par(mfrow = c(1,1))
par(mar = c(4,4,3,1))
# plot(ga_sim_res, title = NULL, pch = 19, col = "black", cex = 1)
plotQQunif(ga_sim_res, pch = 19, col = "black", cex = 1) # left plot in plot.DHARMa()

# check for spatial autocorrelation
testSpatialAutocorrelation(ga_sim_res, x = grazer_data$Long_UTM6S, y = grazer_data$Lat_UTM6S)

# the DHARMa QQ plot suggests that there are issues with distribution (KS test), dispersion, and outliers.

# try a negative binomial model as an alternative
ga_negbin = glm.nb(Grazer_Abundance ~ 
                     Depth_Max + Rugosity_Max + Seabird_N15_Max + 
                     Grazing_Surface_Area + REI + Fishing_Proximity + 
                     Native_Vegetation,
                   data = grazer_data)
summary(ga_negbin)

# conduct an overdispersion test
ga_res_dev = deviance(ga_negbin)
ga_res_df = df.residual(ga_negbin)
ga_overdispersion_ratio = ga_res_dev / ga_res_df
cat("Overdispersion ratio:", round(ga_overdispersion_ratio, 2), "\n")

# simulate residuals
ga_sim_res = simulateResiduals(ga_negbin)
par(mfrow = c(1,1))
par(mar = c(4,4,3,1))
# plot(ga_sim_res, title = NULL, pch = 19, col = "black", cex = 1)
plotQQunif(ga_sim_res, pch = 19, col = "black", cex = 1) # left plot in plot.DHARMa()

# check for spatial autocorrelation
testSpatialAutocorrelation(ga_sim_res, x = grazer_data$Long_UTM6S, y = grazer_data$Lat_UTM6S)

# the DHARMa QQ plot suggests that there are no issues with distribution (KS test), dispersion, or outliers.

# compare AICs
AIC(ga_poisson, ga_negbin)

# perform a likelihood ratio test
lrtest(ga_poisson, ga_negbin)
# the low p-value (<0.05) suggests that there is a significant difference between the two models.
# this implies that the additional parameter in the negative binomial model does significantly 
# improve the fit compared to the poisson model.

# proceed with interpreting the results of the negative binomial model of grazer abundance.

##### RICHNESS #####
# examine the distribution of the grazer richness response data
hist(grazer_data$Grazer_Richness)

# the response variable does not follow a normal distribution (positive skew,left-modal), 
# is bounded by 0 below, and can take only positive integer values (here 0-6).

# fit Poisson model
gr_poisson = glm(Grazer_Richness ~ 
                   Depth_Max + Rugosity_Max + Seabird_N15_Max + 
                   Grazing_Surface_Area + REI + Fishing_Proximity + 
                   Native_Vegetation,
                 data = grazer_data,
                 family = poisson)
summary(gr_poisson)

# perform model diagnostics
par(mfrow = c(2,2))
plot(gr_poisson, which = 1:4, main = NULL)

# conduct an overdispersion test
gr_res_dev = deviance(gr_poisson)
gr_res_df = df.residual(gr_poisson)
gr_overdispersion_ratio = gr_res_dev / gr_res_df
cat("Overdispersion ratio:", round(gr_overdispersion_ratio, 2), "\n")

# simulate resiudals
gr_sim_res = simulateResiduals(gr_poisson)
par(mfrow = c(1,1))
par(mar = c(4,4,3,1))
# plot(ga_sim_res, title = NULL, pch = 19, col = "black", cex = 1)
plotQQunif(gr_sim_res, pch = 19, col = "black", cex = 1) # left plot in plot.DHARMa()

# check for spatial autocorrelation
testSpatialAutocorrelation(gr_sim_res, x = grazer_data$Long_UTM6S, y = grazer_data$Lat_UTM6S)

# the DHARMa QQ plot suggests that there is an issue with dispersion

# try a negative binomial model as an alternative
gr_negbin = glm.nb(Grazer_Richness ~ 
                     Depth_Max + Rugosity_Max + Seabird_N15_Max + 
                     Grazing_Surface_Area + REI + Fishing_Proximity + 
                     Native_Vegetation,
                   data = grazer_data)

# the negative binomial model does not converge well for our grazer richness data - the additional
# model complexity was not viable given the data limitations (small sample size, few unique response values).
summary(gr_negbin)

# perform model diagnostics
par(mfrow = c(2,2))
plot(gr_negbin, which = 1:4)

# conduct an overdispersion test
gr_res_dev = deviance(gr_negbin)
gr_res_df = df.residual(gr_negbin)
gr_overdispersion_ratio = gr_res_dev / gr_res_df
cat("Overdispersion ratio:", round(gr_overdispersion_ratio, 2), "\n")

# simulate resiudals
gr_sim_res = simulateResiduals(gr_negbin)
par(mfrow = c(1,1))
par(mar = c(4,4,3,1))
# plot(ga_sim_res, title = NULL, pch = 19, col = "black", cex = 1)
plotQQunif(gr_sim_res, pch = 19, col = "black", cex = 1) # left plot in plot.DHARMa()

# check for spatial autocorrelation
testSpatialAutocorrelation(gr_sim_res, x = grazer_data$Long_UTM6S, y = grazer_data$Lat_UTM6S)

# the DHARMa QQ plot suggests that there is still an issue with dispersion.

# compare AICs
AIC(gr_negbin, gr_poisson)

# perform a likelihood ratio test
lrtest(gr_negbin, gr_poisson)
# the high p-value (0.9693) suggests that there is no significant difference between the two models.
# this implies that the additional parameter in the negative binomial model does not significantly 
# improve the fit compared to the poisson model.

# proceed with interpreting the results of the poisson model of grazer richness

#### SCRAPERS ####
##### ABUNDANCE #####
# examine the distribution of the scraper abundance response data
hist(scraper_data$Scraper_Abundance)

# the response variable does not follow a normal distribution (positive skewness, left-modal),
# is bounded by 0 below, and can take only positive integer values (here 0-50).

# fit Poisson model
sa_poisson = glm(Scraper_Abundance ~ 
                   Depth_Max + Rugosity_Max + Seabird_N15_Max + 
                   Grazing_Surface_Area + REI + Fishing_Proximity + 
                   Native_Vegetation,
                 data = scraper_data,
                 family = poisson)
summary(sa_poisson)

# perform model diagnostics
par(mfrow = c(2,2))
plot(sa_poisson, which = 1:4)

# conduct an overdispersion test
sa_res_dev = deviance(sa_poisson)
sa_res_df = df.residual(sa_poisson)
sa_overdispersion_ratio = sa_res_dev / sa_res_df
cat("Overdispersion ratio:", round(overdispersion_ratio, 2), "\n")

# simulate residuals
sa_sim_res = simulateResiduals(sa_poisson)
par(mfrow = c(1,1))
par(mar = c(4,4,3,1))
# plot(sa_sim_res, title = NULL, pch = 19, col = "black", cex = 1)
plotQQunif(sa_sim_res, pch = 19, col = "black", cex = 1) # left plot in plot.DHARMa()

# check for spatial autocorrelation
testSpatialAutocorrelation(sa_sim_res, x = scraper_data$Long_UTM6S, y = scraper_data$Lat_UTM6S)

# the DHARMa QQ plot suggests that there are issues with distribution (KS test), dispersion, and outliers.

# try a negative binomial model as an alternative
sa_negbin = glm.nb(Scraper_Abundance ~ 
                     Depth_Max + Rugosity_Max + Seabird_N15_Max + 
                     Grazing_Surface_Area + REI + Fishing_Proximity + 
                     Native_Vegetation,
                   data = scraper_data)
summary(sa_negbin)

# conduct an overdispersion test
sa_res_dev = deviance(sa_negbin)
sa_res_df = df.residual(sa_negbin)
sa_overdispersion_ratio = sa_res_dev / sa_res_df
cat("Overdispersion ratio:", round(sa_overdispersion_ratio, 2), "\n")

# simulate residuals
sa_sim_res = simulateResiduals(sa_negbin)
par(mfrow = c(1,1))
par(mar = c(4,4,3,1))
# plot(sa_sim_res, title = NULL, pch = 19, col = "black", cex = 1)
plotQQunif(sa_sim_res, pch = 19, col = "black", cex = 1) # left plot in plot.DHARMa()

# check for spatial autocorrelation
testSpatialAutocorrelation(sa_sim_res, x = scraper_data$Long_UTM6S, y = scraper_data$Lat_UTM6S)

# the DHARMa QQ plot suggests that there are no issues with distribution (KS test), dispersion, or outliers.

# compare AICs
AIC(sa_poisson, sa_negbin)

# perform a likelihood ratio test
lrtest(sa_poisson, sa_negbin)
# the low p-value (<0.05) suggests that there is a significant difference between the two models.
# this implies that the additional parameter in the negative binomial model does significantly 
# improve the fit compared to the poisson model.

# proceed with interpreting the results of the negative binomial model of scraper abundance

##### RICHNESS #####
# examine the distribution of the scraper richness response data
hist(scraper_data$Scraper_Richness)

# the response variable does not follow a normal distribution (positive skew,left-modal), 
# is bounded by 0 below, and can take only positive integer values (here 0-6).

# fit Poisson model
sr_poisson = glm(Scraper_Richness ~ 
                   Depth_Max + Rugosity_Max + Seabird_N15_Max + 
                   Grazing_Surface_Area + REI + Fishing_Proximity + 
                   Native_Vegetation,
                 data = scraper_data,
                 family = poisson)
summary(sr_poisson)

# perform model diagnostics
par(mfrow = c(2,2))
plot(sr_poisson, which = 1:4, main = NULL)

# conduct an overdispersion test
sr_res_dev = deviance(sr_poisson)
sr_res_df = df.residual(sr_poisson)
sr_overdispersion_ratio = sr_res_dev / sr_res_df
cat("Overdispersion ratio:", round(sr_overdispersion_ratio, 2), "\n")

# simulate resiudals
sr_sim_res = simulateResiduals(sr_poisson)
par(mfrow = c(1,1))
par(mar = c(4,4,3,1))
# plot(ga_sim_res, title = NULL, pch = 19, col = "black", cex = 1)
plotQQunif(sr_sim_res, pch = 19, col = "black", cex = 1) # left plot in plot.DHARMa()

# check for spatial autocorrelation
testSpatialAutocorrelation(sr_sim_res, x = scraper_data$Long_UTM6S, y = scraper_data$Lat_UTM6S)

# the DHARMa QQ plot suggests that there is an issue with dispersion

# try a negative binomial model as an alternative
sr_negbin = glm.nb(Scraper_Richness ~ 
                     Depth_Max + Rugosity_Max + Seabird_N15_Max + 
                     Grazing_Surface_Area + REI + Fishing_Proximity + 
                     Native_Vegetation,
                   data = scraper_data)
# the negative binomial model does not converge well for our scraper richness data - the additional
# model complexity was not viable given the data limitations (small sample size, few unique response values).
summary(sr_negbin)

# perform model diagnostics
par(mfrow = c(2,2))
plot(sr_negbin, which = 1:4, main = NULL)

# conduct an overdispersion test
sr_res_dev = deviance(sr_negbin)
sr_res_df = df.residual(sr_negbin)
sr_overdispersion_ratio = sr_res_dev / sr_res_df
cat("Overdispersion ratio:", round(sr_overdispersion_ratio, 2), "\n")

# simulate resiudals
sr_sim_res = simulateResiduals(sr_negbin)
par(mfrow = c(1,1))
par(mar = c(4,4,3,1))
# plot(ga_sim_res, title = NULL, pch = 19, col = "black", cex = 1)
plotQQunif(sr_sim_res, pch = 19, col = "black", cex = 1) # left plot in plot.DHARMa()

# check for spatial autocorrelation
testSpatialAutocorrelation(sr_sim_res, x = scraper_data$Long_UTM6S, y = scraper_data$Lat_UTM6S)

# the DHARMa QQ plot suggests that there is an issue with dispersion

# compare AIC
AIC(sr_poisson, sr_negbin)

# perform a likelihood ratio test
lrtest(sr_negbin, sr_poisson)
# the high p-value (0.967) suggests that there is no significant difference between the two models.
# this implies that the additional parameter in the negative binomial model does not significantly 
# improve the fit compared to the poisson model.

# proceed with interpreting the poisson model of scraper richness.

#### TERRITORIAL ####
##### ABUNDANCE #####
# examine the distribution of the territorial abundance response data
hist(territorial_data$Territorial_Abundance)

# the response variable does not follow a normal distribution (positive skew, left-modal),
# is bounded by 0 below, and can take only positive integer values (here 0-50).

# fit Poisson model
ta_poisson = glm(Territorial_Abundance ~ 
                   Depth_Max + Rugosity_Max + Seabird_N15_Max + 
                   Grazing_Surface_Area + REI + Fishing_Proximity + 
                   Native_Vegetation,
                 data = territorial_data,
                 family = poisson)
summary(ta_poisson)

# perform model diagnostics
par(mfrow = c(2,2))
plot(ta_poisson, which = 1:4)

# conduct an overdispersion test
ta_res_dev = deviance(ta_poisson)
ta_res_df = df.residual(ta_poisson)
ta_overdispersion_ratio = ta_res_dev / ta_res_df
cat("Overdispersion ratio:", round(overdispersion_ratio, 2), "\n")

# simulate residuals
ta_sim_res = simulateResiduals(ta_poisson)
par(mfrow = c(1,1))
par(mar = c(4,4,3,1))
# plot(ta_sim_res, title = NULL, pch = 19, col = "black", cex = 1)
plotQQunif(ta_sim_res, pch = 19, col = "black", cex = 1) # left plot in plot.DHARMa()

# check for spatial autocorrelation
testSpatialAutocorrelation(ta_sim_res, x = territorial_data$Long_UTM6S, y = territorial_data$Lat_UTM6S)

# the DHARMa QQ plot suggests that there are issues with distribution (KS test), dispersion, and outliers.

# try a negative binomial model as an alternative
ta_negbin = glm.nb(Territorial_Abundance ~ 
                     Depth_Max + Rugosity_Max + Seabird_N15_Max + 
                     Grazing_Surface_Area + REI + Fishing_Proximity + 
                     Native_Vegetation,
                   data = territorial_data)
summary(ta_negbin)

# conduct an overdispersion test
ta_res_dev = deviance(ta_negbin)
ta_res_df = df.residual(ta_negbin)
ta_overdispersion_ratio = ta_res_dev / ta_res_df
cat("Overdispersion ratio:", round(ta_overdispersion_ratio, 2), "\n")

# simulate residuals
ta_sim_res = simulateResiduals(ta_negbin)
par(mfrow = c(1,1))
par(mar = c(4,4,3,1))
# plot(ta_sim_res, title = NULL, pch = 19, col = "black", cex = 1)
plotQQunif(ta_sim_res, pch = 19, col = "black", cex = 1) # left plot in plot.DHARMa()

# check for spatial autocorrelation
testSpatialAutocorrelation(ta_sim_res, x = territorial_data$Long_UTM6S, y = territorial_data$Lat_UTM6S)

# the DHARMa QQ plot suggests that there are no issues with distribution (KS test), dispersion, or outliers.

# compare AICs
AIC(ta_poisson, ta_negbin)

# perform a likelihood ratio test
lrtest(ta_poisson, ta_negbin)
# the low p-value (<0.05) suggests that there is a significant difference between the two models.
# this implies that the additional parameter in the negative binomial model does significantly 
# improve the fit compared to the poisson model.

# proceed with interpreting the results of the negative binomial model of territorial abundance

##### RICHNESS #####
# examine the distribution of the territorial richness response data
hist(territorial_data$Territorial_Richness)

# the response variable follows a roughly normal distribution, 
# is bounded by 0 below, and can take only positive integer values (here 0-6).

# fit Poisson model
tr_poisson = glm(Territorial_Richness ~ 
                   Depth_Max + Rugosity_Max + Seabird_N15_Max + 
                   Grazing_Surface_Area + REI + Fishing_Proximity + 
                   Native_Vegetation,
                 data = territorial_data,
                 family = poisson)
summary(tr_poisson)

# perform model diagnostics
par(mfrow = c(2,2))
plot(tr_poisson, which = 1:4, main = NULL)

# conduct an overdispersion test
tr_res_dev = deviance(tr_poisson)
tr_res_df = df.residual(tr_poisson)
tr_overdispersion_ratio = tr_res_dev / tr_res_df
cat("Overdispersion ratio:", round(tr_overdispersion_ratio, 2), "\n")

# simulate resiudals
tr_sim_res = simulateResiduals(tr_poisson)
par(mfrow = c(1,1))
par(mar = c(4,4,3,1))
# plot(ga_sim_res, title = NULL, pch = 19, col = "black", cex = 1)
plotQQunif(tr_sim_res, pch = 19, col = "black", cex = 1) # left plot in plot.DHARMa()

# check for spatial autocorrelation
testSpatialAutocorrelation(tr_sim_res, x = territorial_data$Long_UTM6S, y = territorial_data$Lat_UTM6S)

# the DHARMa QQ plot suggests that there is an issues with dispersion

# try a negative binomial model as an alternative
tr_negbin = glm.nb(Territorial_Richness ~ 
                     Depth_Max + Rugosity_Max + Seabird_N15_Max + 
                     Grazing_Surface_Area + REI + Fishing_Proximity + 
                     Native_Vegetation,
                   data = territorial_data)
# the negative binomial model does not converge well for our territorial richness data - the additional
# model complexity was not viable given the data limitations (small sample size, few unique response values).
summary(tr_negbin)

# perform model diagnostics
par(mfrow = c(2,2))
plot(tr_negbin, which = 1:4)

# conduct an overdispersion test
tr_res_dev = deviance(tr_negbin)
tr_res_df = df.residual(tr_negbin)
tr_overdispersion_ratio = tr_res_dev / tr_res_df
cat("Overdispersion ratio:", round(tr_overdispersion_ratio, 2), "\n")

# simulate resiudals
tr_sim_res = simulateResiduals(tr_negbin)
par(mfrow = c(1,1))
par(mar = c(4,4,3,1))
# plot(ga_sim_res, title = NULL, pch = 19, col = "black", cex = 1)
plotQQunif(tr_sim_res, pch = 19, col = "black", cex = 1) # left plot in plot.DHARMa()

# check for spatial autocorrelation
testSpatialAutocorrelation(tr_sim_res, x = territorial_data$Long_UTM6S, y = territorial_data$Lat_UTM6S)

# the DHARMa QQ plot suggests that there is still an issue with dispersion and now one with distribution (KS test)

# compare AIC
AIC(tr_poisson, tr_negbin)

# perform a likelihood ratio test
lrtest(tr_negbin, tr_poisson)
# the high p-value (0.9709) suggests that there is no significant difference between the two models.
# this implies that the additional parameter in the negative binomial model does not significantly 
# improve the fit compared to the poisson model.

# proceed with interpreting the results of the poisson model of territorial richness

#### SAVE FINAL MODEL OBJECTS ####
save(fd_poisson, file = here("Data", "Fitted_Models", "Functional_Diversity_Poisson_Model.RData"))
save(ga_negbin, file = here("Data", "Fitted_Models", "Grazer_Abundance_Negative_Binomial_Model.RData"))
save(gr_poisson, file = here("Data", "Fitted_Models", "Grazer_Richness_Poisson_Model.RData"))
save(sa_negbin, file = here("Data", "Fitted_Models", "Scraper_Abundance_Negative_Binomial_Model.RData"))
save(sr_poisson, file = here("Data", "Fitted_Models", "Scraper_Richness_Poisson_Model.RData"))
save(ta_negbin, file = here("Data", "Fitted_Models", "Territorial_Abundance_Negative_Binomial_Model.RData"))
save(tr_poisson, file = here("Data", "Fitted_Models", "Territorial_Richness_Poisson_Model.RData"))

#### SAVE TABLES OF SUMMARY MODEL RESULTS ####
fd_poisson_table = as.data.frame(summary(fd_poisson)$coefficients)
fd_poisson_table$Lower_95CI = fd_poisson_table$Estimate - 1.96 * fd_poisson_table$`Std. Error`
fd_poisson_table$Upper_95CI = fd_poisson_table$Estimate + 1.96 * fd_poisson_table$`Std. Error`
write.csv(fd_poisson_table, here("Data", "Fitted_Models", "Functional_Diversity_Poisson_Model_Summary.csv"),
          row.names = TRUE)
ga_negbin_table = as.data.frame(summary(ga_negbin)$coefficients)
ga_negbin_table$Lower_95CI = ga_negbin_table$Estimate - 1.96 * ga_negbin_table$`Std. Error`
ga_negbin_table$Upper_95CI = ga_negbin_table$Estimate + 1.96 * ga_negbin_table$`Std. Error`
write.csv(ga_negbin_table, here("Data", "Fitted_Models", "Grazer_Abundance_Negative_Binomial_Model_Summary.csv"),
          row.names = TRUE)
gr_poisson_table = as.data.frame(summary(gr_poisson)$coefficients)
gr_poisson_table$Lower_95CI = gr_poisson_table$Estimate - 1.96 * gr_poisson_table$`Std. Error`
gr_poisson_table$Upper_95CI = gr_poisson_table$Estimate + 1.96 * gr_poisson_table$`Std. Error`
write.csv(gr_poisson_table, here("Data", "Fitted_Models", "Grazer_Richness_Poisson_Model_Summary.csv"),
          row.names = TRUE)
sa_negbin_table = as.data.frame(summary(sa_negbin)$coefficients)
sa_negbin_table$Lower_95CI = sa_negbin_table$Estimate - 1.96 * sa_negbin_table$`Std. Error`
sa_negbin_table$Upper_95CI = sa_negbin_table$Estimate + 1.96 * sa_negbin_table$`Std. Error`
write.csv(sa_negbin_table, here("Data", "Fitted_Models", "Scraper_Abundance_Negative_Binomial_Model_Summary.csv"),
          row.names = TRUE)
sr_poisson_table = as.data.frame(summary(sr_poisson)$coefficients)
sr_poisson_table$Lower_95CI = sr_poisson_table$Estimate - 1.96 * sr_poisson_table$`Std. Error`
sr_poisson_table$Upper_95CI = sr_poisson_table$Estimate + 1.96 * sr_poisson_table$`Std. Error`
write.csv(sr_poisson_table, here("Data", "Fitted_Models", "Scraper_Richness_Poisson_Model_Summary.csv"),
          row.names = TRUE)
ta_negbin_table = as.data.frame(summary(ta_negbin)$coefficients)
ta_negbin_table$Lower_95CI = ta_negbin_table$Estimate - 1.96 * ta_negbin_table$`Std. Error`
ta_negbin_table$Upper_95CI = ta_negbin_table$Estimate + 1.96 * ta_negbin_table$`Std. Error`
write.csv(ta_negbin_table, here("Data", "Fitted_Models", "Territorial_Abundance_Negative_Binomial_Model_Summary.csv"),
          row.names = TRUE)
tr_poisson_table = as.data.frame(summary(tr_poisson)$coefficients)
tr_poisson_table$Lower_95CI = tr_poisson_table$Estimate - 1.96 * tr_poisson_table$`Std. Error`
tr_poisson_table$Upper_95CI = tr_poisson_table$Estimate + 1.96 * tr_poisson_table$`Std. Error`
write.csv(tr_poisson_table, here("Data", "Fitted_Models", "Territorial_Richness_Poisson_Model_Summary.csv"),
          row.names = TRUE)

#### VARIATION EXPLAINED BY THE FINAL MODELS ####
# load(here("Data", "Fitted_Models", "Functional_Diversity_Poisson_Model.RData"))
# load(here("Data", "Fitted_Models", "Grazer_Abundance_Negative_Binomial_Model.RData"))
# load(here("Data", "Fitted_Models", "Grazer_Richness_Poisson_Model.RData"))
# load(here("Data", "Fitted_Models", "Scraper_Abundance_Negative_Binomial_Model.RData"))
# load(here("Data", "Fitted_Models", "Scraper_Richness_Poisson_Model.RData"))
# load(here("Data", "Fitted_Models", "Territorial_Abundance_Negative_Binomial_Model.RData"))
# load(here("Data", "Fitted_Models", "Territorial_Richness_Poisson_Model.RData"))

require(pscl)
round(pscl::pR2(fd_poisson), digits = 2)
round(pscl::pR2(ga_negbin), digits = 2)
round(pscl::pR2(sa_negbin), digits = 2)
round(pscl::pR2(ta_negbin), digits = 2)
round(pscl::pR2(gr_poisson), digits = 2)
round(pscl::pR2(sr_poisson), digits = 2)
round(pscl::pR2(tr_poisson), digits = 2)

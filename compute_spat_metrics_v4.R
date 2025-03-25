#compute_spat_metrics_v4


library(raster)
library(ggplot2)
library(landscapemetrics)
library(terra)
library(RColorBrewer)
library(lubridate)
library(gridExtra)
#install.packages("remotes")
#remotes::install_github("VANatHeritage/nhSDM")
library(nhSDM)
library(igraph)
library(ggpubr)
library(zoo)
library(multcompView)
library(GoFKernel)
library(poweRlaw)
library(dplyr)

###### READ DATA #######

mask = raster("~/Thèse/Analyse_Spatiale/mask_v1.tiff")
premask <- is.na(mask)
mask = mask(premask, premask, inverse = F, maskvalue = 1)
maskinv = mask(premask, premask, inverse = T, maskvalue = 1)
plot(mask, colNA = "grey")
plot(maskinv, colNA = "grey")


################ Buffer if needed ? ################


b <- buffer(maskinv, width=10) 
plot(b)


## Saison biologique 2017 - 2018 ##

fs1718 <- c(list.files(path="~/Thèse/Resultats/2017_RF_4bands_V2/rasters", pattern = "tif$", full.names = TRUE),
            list.files(path="~/Thèse/Resultats/2018_RF_4bands_V2/rasters", pattern = "tif$", full.names = TRUE))
fs1718 = fs1718[-c(1:2,36:63)]
raster_stack_1718 = raster::stack(fs1718)
vec_names_1718 = names(raster_stack_1718)
date_values_1718 <- as.Date(names(raster_stack_1718), format="X%Y%m%d")
vec_day_1718 = yday(date_values_1718)
vec_day_1718[28] = vec_day_1718[28] + 365 # 2018
vec_day_1718[29] = vec_day_1718[29] + 365
vec_day_1718[30] = vec_day_1718[30] + 365
vec_day_1718[31] = vec_day_1718[31] + 365
vec_day_1718[32] = vec_day_1718[32] + 365
vec_day_1718[33] = vec_day_1718[33] + 365
crs(raster_stack_1718) <- "+proj=utm +zone=31 +datum=WGS84 +units=m +no_defs"
raster_stack_1718_lambert93 = projectRaster(raster_stack_1718, crs = "+init=epsg:2154", method = "ngb")


crs(raster_stack_1718_lambert93)
crs(mask) = crs(raster_stack_1718_lambert93)


mask <- projectRaster(mask,raster_stack_1718_lambert93,method = 'ngb')

raster_stack_1718_lambert93_masked = mask(raster_stack_1718_lambert93, mask)


## Saison biologique 2018 - 2019 ##

fs1819 <- c(list.files(path="~/Thèse/Resultats/2018_RF_4bands_V2/rasters", pattern = "tif$", full.names = TRUE),
            list.files(path="~/Thèse/Resultats/2019_RF_4bands_V2/rasters", pattern = "tif$", full.names = TRUE))
fs1819 = fs1819[-c(1:6,36:63)]
raster_stack_1819 = raster::stack(fs1819)
vec_names_1819 = names(raster_stack_1819)
date_values_1819 <- as.Date(names(raster_stack_1819), format="X%Y%m%d")
vec_day_1819 = yday(date_values_1819)
vec_day_1819[29] = vec_day_1819[29] + 365 # 2019
crs(raster_stack_1819) <- "+proj=utm +zone=31 +datum=WGS84 +units=m +no_defs"
raster_stack_1819_lambert93 = projectRaster(raster_stack_1819, crs = "+init=epsg:2154", method = "ngb")

raster_stack_1819_lambert93_masked = mask(raster_stack_1819_lambert93, mask)
## Saison biologique 2019 - 2020 ##

fs1920 <- c(list.files(path="~/Thèse/Resultats/2019_RF_4bands_V2/rasters", pattern = "tif$", full.names = TRUE),
            list.files(path="~/Thèse/Resultats/2020_RF_4bands_V2/rasters", pattern = "tif$", full.names = TRUE))
fs1920 = fs1920[-c(35:60)]
raster_stack_1920 = raster::stack(fs1920)
vec_names_1920 = names(raster_stack_1920)
date_values_1920 <- as.Date(names(raster_stack_1920), format="X%Y%m%d")
vec_day_1920 = yday(date_values_1920)
vec_day_1920[30] = vec_day_1920[30] + 365 # 2020
vec_day_1920[31] = vec_day_1920[31] + 365
vec_day_1920[32] = vec_day_1920[32] + 365
vec_day_1920[33] = vec_day_1920[33] + 365
vec_day_1920[34] = vec_day_1920[34] + 365
crs(raster_stack_1920) <- "+proj=utm +zone=31 +datum=WGS84 +units=m +no_defs"
raster_stack_1920_lambert93 = projectRaster(raster_stack_1920, crs = "+init=epsg:2154", method = "ngb")

raster_stack_1920_lambert93_masked = mask(raster_stack_1920_lambert93, mask)
## Saison biologique 2020 - 2021 ##

fs2021 <- c(list.files(path="~/Thèse/Resultats/2020_RF_4bands_V2/rasters", pattern = "tif$", full.names = TRUE),
            list.files(path="~/Thèse/Resultats/2021_RF_4bands_V2/rasters", pattern = "tif$", full.names = TRUE))
fs2021 = fs2021[-c(1:4, 35:62)]
raster_stack_2021 = raster::stack(fs2021)
vec_names_2021 = names(raster_stack_2021)
date_values_2021 <- as.Date(names(raster_stack_2021), format="X%Y%m%d")
vec_day_2021 = yday(date_values_2021)
vec_day_2021[28] = vec_day_2021[28] + 366 # 2021 (année 2020 bisextile)
vec_day_2021[29] = vec_day_2021[29] + 366
vec_day_2021[30] = vec_day_2021[30] + 366
crs(raster_stack_2021) <- "+proj=utm +zone=31 +datum=WGS84 +units=m +no_defs"
raster_stack_2021_lambert93 = projectRaster(raster_stack_2021, crs = "+init=epsg:2154", method = "ngb")

raster_stack_2021_lambert93_masked = mask(raster_stack_2021_lambert93, mask)
## Saison biologique 2021 - 2022 ##

fs2122 <- c(list.files(path="~/Thèse/Resultats/2021_RF_4bands_V2/rasters", pattern = "tif$", full.names = TRUE),
            list.files(path="~/Thèse/Resultats/2022_RF_4bands_V2/rasters", pattern = "tif$", full.names = TRUE))
fs2122 = fs2122[-c(1:2, 41:64)]
raster_stack_2122 = raster::stack(fs2122)
vec_names_2122 = names(raster_stack_2122)
date_values_2122 <- as.Date(names(raster_stack_2122), format="X%Y%m%d")
vec_day_2122 = yday(date_values_2122)
vec_day_2122[30] = vec_day_2122[30] + 365 # 2022
vec_day_2122[31] = vec_day_2122[31] + 365
vec_day_2122[32] = vec_day_2122[32] + 365
vec_day_2122[33] = vec_day_2122[33] + 365
vec_day_2122[34] = vec_day_2122[34] + 365
vec_day_2122[35] = vec_day_2122[35] + 365
vec_day_2122[36] = vec_day_2122[36] + 365
vec_day_2122[37] = vec_day_2122[37] + 365
vec_day_2122[38] = vec_day_2122[38] + 365
crs(raster_stack_2122) <- "+proj=utm +zone=31 +datum=WGS84 +units=m +no_defs"
raster_stack_2122_lambert93 = projectRaster(raster_stack_2122, crs = "+init=epsg:2154", method = "ngb")

raster_stack_2122_lambert93_masked = mask(raster_stack_2122_lambert93, mask)
## Saison biologique 2022 ##

fs22 <- list.files(path="~/Thèse/Resultats/2022_RF_4bands_V2/rasters", pattern = "tif$", full.names = TRUE)
fs22 = fs22[-c(1:8)]
raster_stack_22 = raster::stack(fs22)
vec_names_22 = names(raster_stack_22)
date_values_22 <- as.Date(names(raster_stack_22), format="X%Y%m%d")
vec_day_22 = yday(date_values_22)
crs(raster_stack_22) <- "+proj=utm +zone=31 +datum=WGS84 +units=m +no_defs"
raster_stack_22_lambert93 = projectRaster(raster_stack_22, crs = "+init=epsg:2154", method = "ngb")

raster_stack_22_lambert93_masked = mask(raster_stack_22_lambert93, mask)

## Saison biologique 2023 ##

fs23 <- list.files(path="~/Thèse/Resultats/2023_RF_4bands_V2/rasters", pattern = "tif$", full.names = TRUE)
#fs23 = fs23[-c(1:8)]
raster_stack_23 = raster::stack(fs23)
vec_names_23 = names(raster_stack_23)
date_values_23 <- as.Date(names(raster_stack_23), format="X%Y%m%d")
vec_day_23 = yday(date_values_23)
crs(raster_stack_23) <- "+proj=utm +zone=31 +datum=WGS84 +units=m +no_defs"
raster_stack_23_lambert93 = projectRaster(raster_stack_23, crs = "+init=epsg:2154", method = "ngb")

raster_stack_23_lambert93_masked = mask(raster_stack_23_lambert93, mask)

#################################################

# one big raster brick


full_rasterstack_lambert93_masked = raster::stack(raster_stack_1718_lambert93_masked, raster_stack_1819_lambert93_masked,
                                                  raster_stack_1920_lambert93_masked, raster_stack_2021_lambert93_masked,
                                                  raster_stack_2122_lambert93_masked, raster_stack_22_lambert93_masked,
                                                  raster_stack_23_lambert93_masked)

all_dates = c(date_values_1718, date_values_1819, date_values_1920, date_values_2021, date_values_2122, date_values_22, date_values_23)
all_names = c(vec_names_1718, vec_names_1819,vec_names_1920,vec_names_2021, vec_names_2122, vec_names_22, vec_names_23)

dup = duplicated(all_dates)
ind = which(dup)

full_rasterstack_lambert93_masked = dropLayer(full_rasterstack_lambert93_masked, i = ind) # dates en double

all_dates = all_dates[-ind]# dates en double
all_names = all_names[-ind]# dates en double




#### drop small patches

full_rasterstack_lambert93_masked_clean = raster::stack()
size_thr = 400

for (i in c(1:length(all_names))) {
  
  input_r = raster(full_rasterstack_lambert93_masked, layer=i) 
  input_r_sp =  as(input_r, "SpatRaster")
  
  y <- terra::patches(input_r_sp, zeroAsNA = TRUE)
  rz <- zonal(cellSize(y, unit="m"), y, sum, as.raster=TRUE)
  output_r_sp <- ifel(rz >= size_thr, 1, ifel(is.na(input_r_sp),NA,0))
  
  output_r = raster(output_r_sp)
  #plot(output_r_sp, colNA = "orange")
  
  
  #if (i == 1) {full_rasterstack_lambert93_masked_clean = output_r}
  
  #if (i !=1) {full_rasterstack_lambert93_masked_clean = raster::addLayer(full_rasterstack_lambert93_masked_clean, output_r)}
  
  full_rasterstack_lambert93_masked_clean = raster::addLayer(full_rasterstack_lambert93_masked_clean, output_r)
  
}

#plot(full_rasterstack_lambert93_masked_clean$X20170717)

##################################################
##################################################
##################################################
##################################################

# all
check = check_landscape(full_rasterstack_lambert93_masked) # every landscape is ok

spatial_metrics = NULL

#area (as a test)
total_area_1 = lsm_c_ca(full_rasterstack_lambert93_masked, directions = 4) #works directly on the brick
total_area_1 = subset(total_area_1, total_area_1$class == 1)
total_area_1$date = all_dates[total_area_1$layer]

spatial_metrics = total_area_1

#total_area_1 = total_area_1[-2,] # NA in metrics

colnames(spatial_metrics) = c("layer","level","class","id","metric","area","date")
spatial_metrics = spatial_metrics[,-c(4,5)]

#nb patches

patch_n_1 = lsm_c_np(full_rasterstack_lambert93_masked, directions = 4)
patch_n_1 = subset(patch_n_1, patch_n_1$class == 1)

spatial_metrics$np = patch_n_1$value

# patch complexity (Mean fractal dimension index)
patch_complexity_1 = lsm_c_frac_mn(full_rasterstack_lambert93_masked, directions = 4)
patch_complexity_1 = subset(patch_complexity_1, patch_complexity_1$class == 1)

patch_complexity_1_SD = lsm_c_frac_sd(full_rasterstack_lambert93_masked, directions = 4)
patch_complexity_1_SD = subset(patch_complexity_1_SD, patch_complexity_1_SD$class == 1)

patch_complexity_1_cv = lsm_c_frac_cv(full_rasterstack_lambert93_masked, directions = 4)
patch_complexity_1_cv = subset(patch_complexity_1_cv, patch_complexity_1_cv$class == 1)

spatial_metrics$frac_mn = patch_complexity_1$value
spatial_metrics$frac_sd = patch_complexity_1_SD$value
spatial_metrics$frac_cv = patch_complexity_1_cv$value




# Mean perimeter-area ratio

patch_para_1 = lsm_c_para_mn(full_rasterstack_lambert93_masked, directions = 4)
patch_para_1 = subset(patch_para_1, patch_para_1$class == 1)

patch_para_1_SD = lsm_c_para_sd(full_rasterstack_lambert93_masked, directions = 4)
patch_para_1_SD = subset(patch_para_1_SD, patch_para_1_SD$class == 1)

patch_para_1_cv = lsm_c_para_cv(full_rasterstack_lambert93_masked, directions = 4)
patch_para_1_cv = subset(patch_para_1_cv, patch_para_1_cv$class == 1)

spatial_metrics$para_mn = patch_para_1$value
spatial_metrics$para_sd = patch_para_1_SD$value
spatial_metrics$para_cv = patch_para_1_cv$value



# pafrac
patch_pafrac_1 = lsm_c_pafrac(full_rasterstack_lambert93_masked, directions = 4)
patch_pafrac_1 = subset(patch_pafrac_1, patch_pafrac_1$class == 1)

spatial_metrics$pafrac = patch_pafrac_1$value


# shape (Mean shape index)
patch_shape_1 = lsm_c_shape_mn(full_rasterstack_lambert93_masked, directions = 4)
patch_shape_1 = subset(patch_shape_1, patch_shape_1$class == 1)

patch_shape_1_SD = lsm_c_shape_sd(full_rasterstack_lambert93_masked, directions = 4)
patch_shape_1_SD = subset(patch_shape_1_SD, patch_shape_1_SD$class == 1)

patch_shape_1_cv = lsm_c_shape_cv(full_rasterstack_lambert93_masked, directions = 4)
patch_shape_1_cv = subset(patch_shape_1_cv, patch_shape_1_cv$class == 1)


spatial_metrics$shape_mn = patch_shape_1$value
spatial_metrics$shape_sd = patch_shape_1_SD$value
spatial_metrics$shape_cv = patch_shape_1_cv$value




# ENN (Mean of euclidean nearest-neighbor distance)

patch_enn_1 = lsm_c_enn_mn(full_rasterstack_lambert93_masked_clean, directions = 4)
patch_enn_1 = subset(patch_enn_1, patch_enn_1$class == 1)

patch_enn_1_SD = lsm_c_enn_sd(full_rasterstack_lambert93_masked_clean, directions = 4)
patch_enn_1_SD = subset(patch_enn_1_SD, patch_enn_1_SD$class == 1)

patch_enn_1_cv = lsm_c_enn_cv(full_rasterstack_lambert93_masked_clean, directions = 4)
patch_enn_1_cv = subset(patch_enn_1_cv, patch_enn_1_cv$class == 1)


spatial_metrics$enn_mn = rep(NA, length(spatial_metrics$np))
spatial_metrics$enn_mn[patch_enn_1$layer] = patch_enn_1$value

spatial_metrics$enn_sd = rep(NA, length(spatial_metrics$np))
spatial_metrics$enn_sd[patch_enn_1$layer] = patch_enn_1_SD$value

spatial_metrics$enn_cv = rep(NA, length(spatial_metrics$np))
spatial_metrics$enn_cv[patch_enn_1$layer] = patch_enn_1_cv$value


# number of patches for enn

patch_n_enn_1 = lsm_c_np(full_rasterstack_lambert93_masked_clean, directions = 4)
patch_n_enn_1 = subset(patch_n_enn_1, patch_n_enn_1$class == 1)

spatial_metrics$enn_np = rep(NA, length(spatial_metrics$np))
spatial_metrics$enn_np[patch_n_enn_1$layer] = patch_n_enn_1$value




################# Patch scale metrics #################



spatial_metrics_patch = NULL

all_patch_area = lsm_p_area(full_rasterstack_lambert93_masked_clean, directions = 4)
all_patch_area = subset(all_patch_area, all_patch_area$class == 1)
all_patch_area$date = rep(NA, length(all_patch_area$value))
all_patch_area$date = all_dates[all_patch_area$layer]

spatial_metrics_patch = all_patch_area



colnames(spatial_metrics_patch) = c("layer","level","class","id","metric","area","date")
spatial_metrics_patch = spatial_metrics_patch[,-c(4,5)]


all_patch_enn = lsm_p_enn(full_rasterstack_lambert93_masked_clean, directions = 4)
all_patch_enn = subset(all_patch_enn, all_patch_enn$class == 1)


spatial_metrics_patch$enn = rep(NA, length(spatial_metrics_patch$date))
spatial_metrics_patch$enn= all_patch_enn$value


#############################################################
#############################################################
#############################################################
#############################################################






############## PLOTS #############


spatial_metrics_plot = spatial_metrics[c(1:32),] # select dates to plot
spatial_metrics_plot = spatial_metrics[c(35:62),] # select dates to plot
spatial_metrics_plot = spatial_metrics[c(62:96),] # select dates to plot
spatial_metrics_plot = spatial_metrics[c(97:125),] # select dates to plot
spatial_metrics_plot = spatial_metrics[c(126:163),] # select dates to plot
spatial_metrics_plot = spatial_metrics[c(164:188),] # select dates to plot
spatial_metrics_plot = spatial_metrics[c(189:222),] # select dates to plot


spatial_metrics_plot = spatial_metrics # select dates to plot

# area 

plot_area = ggplot()+
  ggtitle("Total area") +
  xlab("Day") + ylab("Hectares")+
  
  geom_point(data = spatial_metrics_plot, aes(x = date, y = area, color = "class1"), shape = 1, col = "darkgreen")+
  geom_path(data = spatial_metrics_plot, aes(x = date, y = area, color = "class1"), col = "darkgreen")


plot_area


# np

plot_np = ggplot()+
  ggtitle("Number of patches") +
  xlab("Day") + ylab("Value")+
  
  geom_point(data = spatial_metrics_plot, aes(x = date, y = np, color = "class1"), shape = 1, col = "black")+
  geom_path(data = spatial_metrics_plot, aes(x = date, y = np, color = "class1"), col = "black")


plot_np


# frac

plot_frac = ggplot()+
  ggtitle("Mean patch fractal dim index") +
  xlab("Day") + ylab("Value")+
  
  geom_point(data = spatial_metrics_plot, aes(x = date, y = frac_mn, color = "class1"), shape = 1, col = "red2")+
  geom_errorbar(data = spatial_metrics_plot, aes(x = date, y = frac_mn, ymin = frac_mn-frac_sd, ymax = frac_mn+frac_sd), width=.1)+
  geom_path(data = spatial_metrics_plot, aes(x = date, y = frac_mn, color = "class1"), col = "red2")


plot_frac
grid.arrange(plot_frac, plot_area, nrow=2)

# para

plot_para = ggplot()+
  ggtitle("Mean patch area to perimeter ratio") +
  xlab("Day") + ylab("Value")+
  
  geom_point(data = spatial_metrics_plot, aes(x = date, y = para_mn, color = "class1"), shape = 1, col = "blue2")+
  geom_errorbar(data = spatial_metrics_plot, aes(x = date, y = para_mn, ymin = para_mn-para_sd, ymax = para_mn+para_sd), width=.1)+
  geom_path(data = spatial_metrics_plot, aes(x = date, y = para_mn, color = "class1"), col = "blue2")

plot_para


# pafrac 

plot_pafrac = ggplot()+
  ggtitle("Perimeter-Area Fractal Dimension of the Seagrass class ()") +
  xlab("Day") + ylab("Value")+
  
  geom_point(data = na.omit(spatial_metrics_plot), aes(x = date, y = pafrac, color = "class1"), shape = 1, col = "orange2")+
  geom_path(data = na.omit(spatial_metrics_plot), aes(x = date, y = pafrac, color = "class1"), col = "orange2")

plot_pafrac

grid.arrange(plot_pafrac, plot_area, nrow=2)


# shape
plot_shape = ggplot()+
  ggtitle("Mean shape index") +
  xlab("Day") + ylab("Value")+
  
  geom_point(data = spatial_metrics_plot, aes(x = date, y = shape_mn, color = "class1"), shape = 1, col = "purple2")+
  geom_errorbar(data = spatial_metrics_plot, aes(x = date, y = shape_mn, ymin = shape_mn-shape_sd, ymax = shape_mn+shape_sd), width=.1)+
  geom_path(data = spatial_metrics_plot, aes(x = date, y = shape_mn, color = "class1"), col = "purple2")

plot_shape


# enn
plot_enn = ggplot()+
  ggtitle("Mean ENN distance") +
  xlab("Day") + ylab("Value")+
  
  geom_point(data = spatial_metrics_plot, aes(x = date, y = enn_mn, color = "class1"), shape = 1, col = "cyan2")+
  geom_errorbar(data = spatial_metrics_plot, aes(x = date, y = enn_mn, ymin = enn_mn-enn_sd, ymax = enn_mn+enn_sd), width=.1)+
  geom_path(data = spatial_metrics_plot, aes(x = date, y = enn_mn, color = "class1"), col = "cyan2")

#plot_enn


plot_enn_np = ggplot()+
  ggtitle("Number of patches") +
  xlab("Day") + ylab("Value")+
  
  geom_point(data = spatial_metrics_plot, aes(x = date, y = enn_np, color = "class1"), shape = 1, col = "black")+
  geom_path(data = spatial_metrics_plot, aes(x = date, y = enn_np, color = "class1"), col = "black")



grid.arrange(plot_enn, plot_enn_np, nrow=2)





######## patch level plots #########

spatial_metrics_patch_plot_1718 = spatial_metrics_patch[c(1:500),] # select dates to plot
spatial_metrics_patch_plot_1819 = spatial_metrics_patch[c(501:944),] # select dates to plot
spatial_metrics_patch_plot_1920 = spatial_metrics_patch[c(945:1467),] # select dates to plot
spatial_metrics_patch_plot_2021 = spatial_metrics_patch[c(1468:2027),] # select dates to plot
spatial_metrics_patch_plot_2122 = spatial_metrics_patch[c(2028:2437),] # select dates to plot
spatial_metrics_patch_plot_22 = spatial_metrics_patch[c(2438:3070),] # select dates to plot
spatial_metrics_patch_plot_23 = spatial_metrics_patch[c(3071:3522),] # select dates to plot



par(mfrow = c(3,3))
hist(spatial_metrics_patch_plot_1718$enn, main = "2017-2018", xlab = "ENN", breaks = 15)
hist(spatial_metrics_patch_plot_1819$enn, main = "2018-2019", xlab = "ENN", breaks = 15)
hist(spatial_metrics_patch_plot_1920$enn, main = "2019-2020", xlab = "ENN", breaks = 15)
hist(spatial_metrics_patch_plot_2021$enn, main = "2020-2021", xlab = "ENN", breaks = 15)
hist(spatial_metrics_patch_plot_2122$enn, main = "2021-2022", xlab = "ENN", breaks = 15)
hist(spatial_metrics_patch_plot_22$enn, main = "2022", xlab = "ENN", breaks = 15)
hist(spatial_metrics_patch_plot_23$enn, main = "2023", xlab = "ENN", breaks = 15)

par(mfrow = c(1,1))


par(mfrow = c(2,3))
hist(log(spatial_metrics_patch_plot_1718$enn), main = "2017-2018", xlab = "log(ENN)", breaks = 10)
hist(log(spatial_metrics_patch_plot_1819$enn), main = "2018-2019", xlab = "log(ENN)", breaks = 10)
hist(log(spatial_metrics_patch_plot_1920$enn), main = "2019-2020", xlab = "log(ENN)", breaks = 10)
hist(log(spatial_metrics_patch_plot_2021$enn), main = "2020-2021", xlab = "log(ENN)", breaks = 10)
hist(log(spatial_metrics_patch_plot_2122$enn), main = "2021-2022", xlab = "log(ENN)", breaks = 10)
hist(log(spatial_metrics_patch_plot_22$enn), main = "2022-2023", xlab = "log(ENN)", breaks = 10)
hist(log(spatial_metrics_patch_plot_23$enn), main = "2023-2024", xlab = "log(ENN)", breaks = 10)



hist(log(spatial_metrics_patch_plot_1819$enn), main = "2018", xlab = "log(ENN)", breaks = seq(2.5,8,by = 0.5))
hist(log(spatial_metrics_patch_plot_1920$enn), main = "2019", xlab = "log(ENN)", breaks = seq(2.5,8,by = 0.5))
hist(log(spatial_metrics_patch_plot_2021$enn), main = "2020", xlab = "log(ENN)", breaks = seq(2.5,8,by = 0.5))
hist(log(spatial_metrics_patch_plot_2122$enn), main = "2021", xlab = "log(ENN)", breaks = seq(2.5,8,by = 0.5))
hist(log(spatial_metrics_patch_plot_22$enn), main = "2022", xlab = "log(ENN)", breaks = seq(2.5,8,by = 0.5))
hist(log(spatial_metrics_patch_plot_23$enn), main = "2023", xlab = "log(ENN)", breaks = seq(2.5,8,by = 0.5))

par(mfrow = c(1,1))


spatial_metrics_patch_plot_1718$logenn = log(spatial_metrics_patch_plot_1718$enn)
spatial_metrics_patch_plot_1718 = as.data.frame(spatial_metrics_patch_plot_1718)
spatial_metrics_patch_plot_1718$year = rep("2017-2018", nrow(spatial_metrics_patch_plot_1718))

spatial_metrics_patch_plot_1819$logenn = log(spatial_metrics_patch_plot_1819$enn)
spatial_metrics_patch_plot_1819 = as.data.frame(spatial_metrics_patch_plot_1819)
spatial_metrics_patch_plot_1819$year = rep("2018-2019", nrow(spatial_metrics_patch_plot_1819))

spatial_metrics_patch_plot_1920$logenn = log(spatial_metrics_patch_plot_1920$enn)
spatial_metrics_patch_plot_1920 = as.data.frame(spatial_metrics_patch_plot_1920)
spatial_metrics_patch_plot_1920$year = rep("2019-2020", nrow(spatial_metrics_patch_plot_1920))

spatial_metrics_patch_plot_2021$logenn = log(spatial_metrics_patch_plot_2021$enn)
spatial_metrics_patch_plot_2021 = as.data.frame(spatial_metrics_patch_plot_2021)
spatial_metrics_patch_plot_2021$year = rep("2020-2021", nrow(spatial_metrics_patch_plot_2021))

spatial_metrics_patch_plot_2122$logenn = log(spatial_metrics_patch_plot_2122$enn)
spatial_metrics_patch_plot_2122 = as.data.frame(spatial_metrics_patch_plot_2122)
spatial_metrics_patch_plot_2122$year = rep("2021-2022", nrow(spatial_metrics_patch_plot_2122))

spatial_metrics_patch_plot_22$logenn = log(spatial_metrics_patch_plot_22$enn)
spatial_metrics_patch_plot_22 = as.data.frame(spatial_metrics_patch_plot_22)
spatial_metrics_patch_plot_22$year = rep("2022", nrow(spatial_metrics_patch_plot_22))

spatial_metrics_patch_plot_23$logenn = log(spatial_metrics_patch_plot_23$enn)
spatial_metrics_patch_plot_23 = as.data.frame(spatial_metrics_patch_plot_23)
spatial_metrics_patch_plot_23$year = rep("2023", nrow(spatial_metrics_patch_plot_23))

all_data_hist <- do.call('rbind', list(spatial_metrics_patch_plot_1718,
                                       spatial_metrics_patch_plot_1819,
                                       spatial_metrics_patch_plot_1920,
                                       spatial_metrics_patch_plot_2021,
                                       spatial_metrics_patch_plot_2122,
                                       spatial_metrics_patch_plot_22,
                                       spatial_metrics_patch_plot_23))



par(mfrow = c(3,3))
h1 = gghistogram(data=spatial_metrics_patch_plot_1718, x="logenn", y = "density", bins = 15, title = "2017-2018")
h2 = gghistogram(data=spatial_metrics_patch_plot_1819, x="logenn", y = "density", bins = 15, title = "2018-2019")
h3 = gghistogram(data=spatial_metrics_patch_plot_1920, x="logenn", y = "density", bins = 15, title = "2019-2020")
h4 = gghistogram(data=spatial_metrics_patch_plot_2021, x="logenn", y = "density", bins = 15, title = "2020-2021")
h5 = gghistogram(data=spatial_metrics_patch_plot_2122, x="logenn", y = "density", bins = 15, title = "2021-2022")
h6 = gghistogram(data=spatial_metrics_patch_plot_22, x="logenn", y = "density", bins = 15, title = "2022-2023")
h7 = gghistogram(data=spatial_metrics_patch_plot_23, x="logenn", y = "density", bins = 15, title = "2023-2024")

ggarrange(h1,h2,h3,h4,h5,h6,h7)


par(mfrow = c(3,3))
h1 = gghistogram(data=spatial_metrics_patch_plot_1718, x="logenn", y = "count", bins = 7, title = "2017-2018")
h2 = gghistogram(data=spatial_metrics_patch_plot_1819, x="logenn", y = "count", bins = 7, title = "2018-2019")
h3 = gghistogram(data=spatial_metrics_patch_plot_1920, x="logenn", y = "count", bins = 7, title = "2019-2020")
h4 = gghistogram(data=spatial_metrics_patch_plot_2021, x="logenn", y = "count", bins = 7, title = "2020-2021")
h5 = gghistogram(data=spatial_metrics_patch_plot_2122, x="logenn", y = "count", bins = 7, title = "2021-2022")
h6 = gghistogram(data=spatial_metrics_patch_plot_22, x="logenn", y = "count", bins = 7, title = "2022-2023")
h7 = gghistogram(data=spatial_metrics_patch_plot_23, x="logenn", y = "count", bins = 7, title = "2023-2024")

ggarrange(h1,h2,h3,h4,h5,h6,h7)




###############################################

spatial_metrics_patch_plot_23$logenn = log(spatial_metrics_patch_plot_23$enn)
spatial_metrics_patch_plot_23 = as.data.frame(spatial_metrics_patch_plot_23)
res = hist(log10(spatial_metrics_patch_plot_23$enn))

spatial_metrics_patch_plot_1718$logenn = log(spatial_metrics_patch_plot_1718$enn)
spatial_metrics_patch_plot_1718 = as.data.frame(spatial_metrics_patch_plot_1718)
res2 = hist(log10(spatial_metrics_patch_plot_1718$enn))

spatial_metrics_patch_plot_22$logenn = log(spatial_metrics_patch_plot_22$enn)
spatial_metrics_patch_plot_22 = as.data.frame(spatial_metrics_patch_plot_22)
res3 = hist(log10(spatial_metrics_patch_plot_22$enn))


plot(res$mids,log10(res$density))
points(res2$mids,log10(res2$density), col = "red")
points(res3$mids,log10(res3$density), col = "green")

plot(res$mids,log10(res$counts))
points(res2$mids,log10(res2$counts), col = "red")
points(res3$mids,log10(res3$counts), col = "green")



######## khi2 tests ########

par(mfrow = c(3,3))
hist(log(spatial_metrics_patch_plot_1718$enn), main = "2017-2018", xlab = "log(ENN)", breaks = seq(2.5,8,by = 0.5))
hist(log(spatial_metrics_patch_plot_1819$enn), main = "2018-2019", xlab = "log(ENN)", breaks = seq(2.5,8,by = 0.5))
hist(log(spatial_metrics_patch_plot_1920$enn), main = "2019-2020", xlab = "log(ENN)", breaks = seq(2.5,8,by = 0.5))
hist(log(spatial_metrics_patch_plot_2021$enn), main = "2020-2021", xlab = "log(ENN)", breaks = seq(2.5,8,by = 0.5))
hist(log(spatial_metrics_patch_plot_2122$enn), main = "2021-2022", xlab = "log(ENN)", breaks = seq(2.5,8,by = 0.5))
hist(log(spatial_metrics_patch_plot_22$enn), main = "2022-2023", xlab = "log(ENN)", breaks = seq(2.5,8,by = 0.5))
hist(log(spatial_metrics_patch_plot_23$enn), main = "2023-2024", xlab = "log(ENN)", breaks = seq(2.5,8,by = 0.5))


par(mfrow = c(1,1))
hist_1718 = hist(log(spatial_metrics_patch_plot_1718$enn), main = "2017-2018", xlab = "log(ENN)", breaks = seq(2.5,8,by = 0.5))
hist_1819 = hist(log(spatial_metrics_patch_plot_1819$enn), main = "2018-2019", xlab = "log(ENN)", breaks = seq(2.5,8,by = 0.5))
hist_1920 = hist(log(spatial_metrics_patch_plot_1920$enn), main = "2019-2020", xlab = "log(ENN)", breaks = seq(2.5,8,by = 0.5))
hist_2021 = hist(log(spatial_metrics_patch_plot_2021$enn), main = "2020-2021", xlab = "log(ENN)", breaks = seq(2.5,8,by = 0.5))
hist_2122 = hist(log(spatial_metrics_patch_plot_2122$enn), main = "2021-2022", xlab = "log(ENN)", breaks = seq(2.5,8,by = 0.5))
hist_22 = hist(log(spatial_metrics_patch_plot_22$enn), main = "2022-2023", xlab = "log(ENN)", breaks = seq(2.5,8,by = 0.5))
hist_23 = hist(log(spatial_metrics_patch_plot_23$enn), main = "2023-2024", xlab = "log(ENN)", breaks = seq(2.5,8,by = 0.5))

tableau = rbind(hist_1718[["density"]],hist_1819[["density"]],hist_1920[["density"]],
                hist_2021[["density"]],hist_2122[["density"]],hist_22[["density"]],hist_23[["density"]])


tableau = tableau[,-c(8,9,10,11)]
khi_test = chisq.test(tableau)
khi_test # pas de difference particuliere entre les distributions (khi2 d'independance)





################################################
################## new analyses ################
################################################


#### patch area #########

spatial_metrics = NULL

#nb patches

patch_number = lsm_c_np(full_rasterstack_lambert93_masked_clean, directions = 4)
patch_number = subset(patch_number, patch_number$class == 1)
patch_number$date = all_dates[patch_number$layer]
patch_number$date = as.Date(patch_number$date)


patch_number$date2 = format(as.Date(patch_number$date), "%m-%d")
patch_number$date2 = as.Date(patch_number$date2, format = "%m-%d")



patch_number$saison = rep(0, length(patch_number$layer))


patch_number$saison[c(1:29)] = rep(1)
patch_number$saison[c(30:58)] = rep(2)
patch_number$saison[c(59:88)] = rep(3)
patch_number$saison[c(89:116)] = rep(4) 
patch_number$saison[c(117:147)] = rep(5) #postassec
patch_number$saison[c(148:168)] = rep(6)
patch_number$saison[c(169:193)] = rep(7) #postassec

patch_number$saison = as.factor(patch_number$saison)

#show_patches(full_rasterstack_lambert93_masked_clean[[5]], class = 1, directions = 4)

# Basic box plot
p <- ggplot(patch_number, aes(x=saison, y=value)) + 
  geom_boxplot()

p + geom_dotplot(binaxis='y', stackdir='center', dotsize=.5)

boxplot()



plot_np = ggplot(data = patch_number, aes(x = date2, y = value, color = saison),
                 group=factor(saison), colour=factor(saison), 
                 shape = 1)+
  ggtitle("Number of patches") +
  xlab("Day") + ylab("Value")+
  geom_point()+
  
  #scale_x_date(date_breaks = "month")
  #geom_path(data = patch_number, aes(x = date2, y = value, color = saison), col = "black")+
  scale_color_manual(values= c("black","black","black","black","red","black","red"))

plot_np


?merge
?match



########## patch area heterogeneity per year ###########

dates_np_sup10 = patch_number[,c(6,7,9)]
dates_np_sup10 = dates_np_sup10[dates_np_sup10$value>=10,]










all_patch_area = lsm_p_area(full_rasterstack_lambert93_masked_clean, directions = 4)
all_patch_area = subset(all_patch_area, all_patch_area$class == 1)
all_patch_area$date = rep(NA, length(all_patch_area$value))
all_patch_area$date = all_dates[all_patch_area$layer]


all_patch_area = all_patch_area[all_patch_area$date %in% dates_np_sup10$date,]
all_patch_area$saison = rep(0)


  

  
  
dataref = patch_number[,c(7,9)]




all_patch_area <- merge(dataref, all_patch_area, by = "date")
all_patch_area = all_patch_area[,-9]


colnames(all_patch_area) = c("date","saison","layer","level","class","id","metric","area")

all_patch_area = all_patch_area[all_patch_area$area>=0.5,]
years = c("2017","2018","2019","2020","2021","2022","2023")





model=lm(all_patch_area$area ~ all_patch_area$saison )

ANOVA=aov(model)

TUKEY <- TukeyHSD(x=ANOVA, 'all_patch_area$saison', conf.level=0.95)

plot(TUKEY , las=1 , col="brown")






# I need to group the treatments that are not different each other together.
generate_label_df <- function(TUKEY, variable){
  
  # Extract labels and factor levels from Tukey post-hoc 
  Tukey.levels <- TUKEY[[variable]][,4]
  Tukey.labels <- data.frame(multcompLetters(Tukey.levels)['Letters'])
  
  #I need to put the labels in the same order as in the boxplot :
  Tukey.labels$treatment=rownames(Tukey.labels)
  Tukey.labels=Tukey.labels[order(Tukey.labels$treatment) , ]
  return(Tukey.labels)
}

# Apply the function on my dataset
LABELS <- generate_label_df(TUKEY , "all_patch_area$saison")


names(LABELS)<-c('Letters','saison')
all_patch_area = merge(LABELS,all_patch_area)

p <- ggplot(all_patch_area, aes(x=saison, y=log(area), label = Letters)) + 
  geom_boxplot()+
  scale_x_discrete(labels = c("2017","2018","2019","2020","2021","2022","2023"))
  #geom_text(aes(x=saison, y=log(area), label = Letters))


p








#tri sur année normale à faire
#par(mfrow = c(2,3))






#all_patch_area_sub = all_patch_area[all_patch_area$saison == "6",]
#area_patchs = all_patch_area_sub$area[log(all_patch_area_sub$area)<4.5]


area_patchs = all_patch_area$area[log(all_patch_area$area)<4.5]

#area_patchs= area_patchs*10000 #passage en m2

ecdf_func = ecdf(area_patchs)



values_ecdf = ecdf_func(area_patchs)
values_inv_ecdf = 1-values_ecdf





### power law ####

m_bl = conpl$new(area_patchs)

est = estimate_xmin(m_bl)

m_bl$setXmin(est)


par(mfrow = c(1,1))

plot(m_bl, main = "2022")
lines(m_bl, col = 2, lwd = 2)


datalmx = log(area_patchs)
datalmx = datalmx[-14]
datalmy = log(values_inv_ecdf)
datalmy = datalmy[-14]
lm = lm(datalmy ~ datalmx)
summary(lm)


plot(log(area_patchs), log(values_inv_ecdf))

dfplot = data.frame(area_patchs, values_inv_ecdf)

p <- ggplot(dfplot, aes(x = area_patchs, y = values_inv_ecdf)) + 
  geom_point()+
  scale_x_continuous(trans = 'log10') + 
  scale_y_continuous(trans = 'log10')+
  annotation_logticks(sides="lb")+
  xlab("Patch area s (ha)") + ylab("P(S ≥ s)")+
  annotate("text", x = 35, y = 1, label = "Beta = 1.75, Rsq = 0.96")+
  geom_smooth(method = "lm", se = F)
p


########################################################
######################## ENN v2 ########################
########################################################








ENN_m = lsm_c_enn_mn(full_rasterstack_lambert93_masked_clean, directions = 4)
ENN_m = subset(ENN_m, ENN_m$class == 1)
ENN_m$date = rep(NA, length(ENN_m$value))
ENN_m$date = all_dates[ENN_m$layer]


ENN_m = ENN_m[ENN_m$date %in% dates_np_sup10$date,]
ENN_m$saison = rep(0)







ENN_m <- merge(dataref, ENN_m, by = "date")
ENN_m = ENN_m[,-9]


colnames(ENN_m) = c("date","saison","layer","level","class","id","metric","ENN_m")


plot_ENN_m = ggplot(data = ENN_m, aes(x = date, y = ENN_m, color = saison),
                 group=factor(saison), colour=factor(saison), 
                 shape = 1)+
  ggtitle("Mean of ENN") +
  xlab("Day") + ylab("Value")+
  geom_point()+
  
  #scale_x_date(date_breaks = "month")
  #geom_path(data = patch_number, aes(x = date2, y = value, color = saison), col = "black")+
  scale_color_manual(values= c("black","black","black","black","red","black","red"))

plot_ENN_m



p <- ggplot(ENN_m, aes(x=saison, y=ENN_m)) + 
  geom_boxplot()+
  xlab("Saison") + ylab("Moyenne des ENN (m)")+
  scale_x_discrete(labels = c("2017","2018","2019","2020","2021","2022","2023"))
#geom_text(aes(x=saison, y=log(area), label = Letters))


p



##### all patches enn #######

all_patch_enn = lsm_p_enn(full_rasterstack_lambert93_masked_clean, directions = 4)
all_patch_enn = subset(all_patch_enn, all_patch_enn$class == 1)

all_patch_enn$date = rep(NA, length(all_patch_enn$value))
all_patch_enn$date = all_dates[all_patch_enn$layer]


all_patch_enn = all_patch_enn[all_patch_enn$date %in% dates_np_sup10$date,]
all_patch_enn$saison = rep(0)


all_patch_enn <- merge(dataref, all_patch_enn, by = "date")
all_patch_enn = all_patch_enn[,-9]


colnames(all_patch_enn) = c("date","saison","layer","level","class","id","metric","ENN")



all_patch_enn1 = all_patch_enn[all_patch_enn$saison == "1",]
all_patch_enn2 = all_patch_enn[all_patch_enn$saison == "2",]
all_patch_enn3 = all_patch_enn[all_patch_enn$saison == "3",]
all_patch_enn4 = all_patch_enn[all_patch_enn$saison == "4",]
all_patch_enn5 = all_patch_enn[all_patch_enn$saison == "5",]
all_patch_enn6 = all_patch_enn[all_patch_enn$saison == "6",]
all_patch_enn7 = all_patch_enn[all_patch_enn$saison == "7",]




par(mfrow = c(2,4))
hist(log(all_patch_enn1$ENN), main = "2017", xlab = "log(ENN)", breaks = seq(2.5,8,by = 0.5))
hist(log(all_patch_enn2$ENN), main = "2018", xlab = "log(ENN)", breaks = seq(2.5,8,by = 0.5))
hist(log(all_patch_enn3$ENN), main = "2019", xlab = "log(ENN)", breaks = seq(2.5,8,by = 0.5))
hist(log(all_patch_enn4$ENN), main = "2020", xlab = "log(ENN)", breaks = seq(2.5,8,by = 0.5))
hist(log(all_patch_enn5$ENN), main = "2021", xlab = "log(ENN)", breaks = seq(2.5,8,by = 0.5))
hist(log(all_patch_enn6$ENN), main = "2022", xlab = "log(ENN)", breaks = seq(2.5,8,by = 0.5))
hist(log(all_patch_enn7$ENN), main = "2023", xlab = "log(ENN)", breaks = seq(2.5,8,by = 0.5))


par(mfrow = c(1,1))
hist_17 = hist(log(all_patch_enn1$ENN), main = "2017", xlab = "log(ENN)", breaks = seq(2.5,8,by = 0.5))
hist_18 = hist(log(all_patch_enn2$ENN), main = "2018", xlab = "log(ENN)", breaks = seq(2.5,8,by = 0.5))
hist_19 = hist(log(all_patch_enn3$ENN), main = "2019", xlab = "log(ENN)", breaks = seq(2.5,8,by = 0.5))
hist_20 = hist(log(all_patch_enn4$ENN), main = "2020", xlab = "log(ENN)", breaks = seq(2.5,8,by = 0.5))
hist_21 = hist(log(all_patch_enn5$ENN), main = "2021", xlab = "log(ENN)", breaks = seq(2.5,8,by = 0.5))
hist_22 = hist(log(all_patch_enn6$ENN), main = "2022", xlab = "log(ENN)", breaks = seq(2.5,8,by = 0.5))
hist_23 = hist(log(all_patch_enn7$ENN), main = "2023", xlab = "log(ENN)", breaks = seq(2.5,8,by = 0.5))

tableau = rbind(hist_17[["density"]],hist_18[["density"]],hist_19[["density"]],
                hist_20[["density"]],hist_21[["density"]],hist_22[["density"]],hist_23[["density"]])


tableau = tableau[,-c(8,9,10,11)]
khi_test = chisq.test(tableau)
khi_test # pas de difference particuliere entre les distributions (khi2 d'independance)




###################################
######### patch complexity ########
###################################


patch_pafrac_1 = lsm_c_pafrac(full_rasterstack_lambert93_masked, directions = 4)
patch_pafrac_1 = subset(patch_pafrac_1, patch_pafrac_1$class == 1)

patch_pafrac_1$date = rep(NA, length(patch_pafrac_1$value))
patch_pafrac_1$date = all_dates[patch_pafrac_1$layer]


#patch_pafrac_1 = patch_pafrac_1[patch_pafrac_1$date %in% dates_np_sup10$date,]
patch_pafrac_1$saison = rep(0)


patch_pafrac_1 <- merge(dataref, patch_pafrac_1, by = "date")
patch_pafrac_1 = patch_pafrac_1[,-9]



patch_pafrac_1$MoisJour <- format(patch_pafrac_1$date, "%m-%d")  # Extraire mois-jour
patch_pafrac_1$MoisJour = as.Date(patch_pafrac_1$MoisJour, format = "%m-%d")



ggplot(patch_pafrac_1 %>% filter(saison.x %in% c("2","4","7")), aes(x = MoisJour, y = value, color = factor(saison.x), group = saison.x)) +
  geom_line(na.rm = T) +                  # Tracer des lignes
  geom_point(na.rm = T) +                 # Ajouter des points
  labs(x = "Date", y = "Valeur PAFRAC", color = "Année")+
  scale_x_date(date_labels = "%b") +
  scale_color_manual(
    values = c("2" = "#1f78b4",  # Bleu doux
               "4" = "#ff7f00",
               "7" = "#6a3d9a"), # Orange clair, 
    labels = c("2" = "2018", "4" = "2020", "7" = "2023")  # Labels légende
  )



plot(full_rasterstack_lambert93_masked_clean[[31]])
show_patches(full_rasterstack_lambert93_masked_clean[[31]], class = 1, directions = 4, labels = F)

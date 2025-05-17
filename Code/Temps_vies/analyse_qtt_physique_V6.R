#analyse_qtt_physique_V6



library(raster)
library(lubridate)
library(ggplot2)
library(gridExtra)

################




mask = raster(paste(getwd(),"/Data/mask_analyse_spatiale/mask_v1.tiff", sep = ""))
premask <- is.na(mask)
mask = mask(premask, premask, inverse = F, maskvalue = 1)
maskinv = mask(premask, premask, inverse = T, maskvalue = 1)
plot(mask, colNA = "grey")
plot(maskinv, colNA = "grey")

masque_roseliere = mask

###################Functions######################
##################################################

get_change_count <- function(x) {     # reste à tester 
  nb_changes <- sum(diff(x) != 0)
  ifelse(length(nb_changes) == 0, NA, nb_changes)
}


get_physical_values <- function(x, method, vec_day) {
  rlex = rle(x)
  run_all <- rlex$lengths
  indna = which(rlex$values == 0)
  run_pre = run_all
  run_pre[indna] = NA
  
  lti = ifelse(!is.finite(max(na.omit(run_pre))), NA, ifelse(length(na.omit(run_pre)) == 0, 0, max(na.omit(run_pre))))
  
  pos2 = ifelse(!is.finite(max(na.omit(run_pre))), NA, min(which(run_pre == max(na.omit(run_pre))))) # position dans le vecteur run_pre
  bi = ifelse(!is.finite(max(na.omit(run_pre))), NA, 1+sum(run_all[1:pos2-1])) # somme nb d'images jusqu'a la fenetre ciblée, borne le premier jour de ma fenetre
  bs = ifelse(!is.finite(max(na.omit(run_pre))), NA, bi+max(na.omit(run_pre))-1) # dernier jour de ma fenetre
  #ltd = ifelse(!is.finite(max(na.omit(run_pre))), NA, vec_day[bs] - vec_day[bi])
  
  
  if (method == "lti") {
    return(lti)
  }
  
  #if (method == "ltd") {
   # return(ltd)
  #}
  
  if (method == "start") {
    return(bi)
  }
  
  if(method == "end"){
    return(bs)
  }


}


get_physical_values_abs <- function(x, method, vec_day) {
  rlex = rle(x)
  run_all <- rlex$lengths
  indna = which(rlex$values == 1)
  run_abs = run_all
  run_abs[indna] = NA
  
  lti = ifelse(!is.finite(max(na.omit(run_abs))), NA, ifelse(length(na.omit(run_abs)) == 0, 0, max(na.omit(run_abs))))
  
  pos2 = ifelse(!is.finite(max(na.omit(run_abs))), NA, min(which(run_abs == max(na.omit(run_abs)))))
  bi = ifelse(!is.finite(max(na.omit(run_abs))), NA, 1+sum(run_all[1:pos2-1]))
  bs = ifelse(!is.finite(max(na.omit(run_abs))), NA, bi+max(na.omit(run_abs))-1)
  #ltd = ifelse(!is.finite(max(na.omit(run_abs))), NA, vec_day[bs] - vec_day[bi])
  
  
  if (method == "ati") {
    return(lti)
  }
  
  if (method == "atd") {
    return(ltd)
  }
  if (method == "start") {
    return(bi)
  }
  
  if(method == "end"){
    return(bs)
  }
  
}

################### NEW DATA READ ######################
########################################################

#### periodes presence ####
## Saison biologique 2017 - 2018 ##

fs1718 <- c(list.files(path=paste(getwd(),"/Classified_maps/2017_RF_4bands_V2/rasters", sep = ""), pattern = "tif$", full.names = TRUE),
            list.files(path=paste(getwd(),"/Classified_maps/2018_RF_4bands_V2/rasters", sep = ""), pattern = "tif$", full.names = TRUE))
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

fs1819 <- c(list.files(path=paste(getwd(),"/Classified_maps/2018_RF_4bands_V2/rasters", sep = ""), pattern = "tif$", full.names = TRUE),
            list.files(path=paste(getwd(),"/Classified_maps/2019_RF_4bands_V2/rasters", sep = ""), pattern = "tif$", full.names = TRUE))
fs1819 = fs1819[-c(1:6,35:63)]
raster_stack_1819 = raster::stack(fs1819)
vec_names_1819 = names(raster_stack_1819)
date_values_1819 <- as.Date(names(raster_stack_1819), format="X%Y%m%d")
vec_day_1819 = yday(date_values_1819)
#vec_day_1819[29] = vec_day_1819[29] + 365 # 2019
crs(raster_stack_1819) <- "+proj=utm +zone=31 +datum=WGS84 +units=m +no_defs"
raster_stack_1819_lambert93 = projectRaster(raster_stack_1819, crs = "+init=epsg:2154", method = "ngb")

raster_stack_1819_lambert93_masked = mask(raster_stack_1819_lambert93, mask)
## Saison biologique 2019 - 2020 ##

fs1920 <- c(list.files(path=paste(getwd(),"/Classified_maps/2019_RF_4bands_V2/rasters", sep = ""), pattern = "tif$", full.names = TRUE),
            list.files(path=paste(getwd(),"/Classified_maps/2020_RF_4bands_V2/rasters", sep = ""), pattern = "tif$", full.names = TRUE))
fs1920 = fs1920[-c(34:60)]
raster_stack_1920 = raster::stack(fs1920)
vec_names_1920 = names(raster_stack_1920)
date_values_1920 <- as.Date(names(raster_stack_1920), format="X%Y%m%d")
vec_day_1920 = yday(date_values_1920)
vec_day_1920[30] = vec_day_1920[30] + 365 # 2020
vec_day_1920[31] = vec_day_1920[31] + 365
vec_day_1920[32] = vec_day_1920[32] + 365
vec_day_1920[33] = vec_day_1920[33] + 365
crs(raster_stack_1920) <- "+proj=utm +zone=31 +datum=WGS84 +units=m +no_defs"
raster_stack_1920_lambert93 = projectRaster(raster_stack_1920, crs = "+init=epsg:2154", method = "ngb")

raster_stack_1920_lambert93_masked = mask(raster_stack_1920_lambert93, mask)
## Saison biologique 2020 - 2021 ##

fs2021 <- c(list.files(path=paste(getwd(),"/Classified_maps/2020_RF_4bands_V2/rasters", sep = ""), pattern = "tif$", full.names = TRUE),
            list.files(path=paste(getwd(),"/Classified_maps/2021_RF_4bands_V2/rasters", sep = ""), pattern = "tif$", full.names = TRUE))
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

fs2122 <- c(list.files(path=paste(getwd(),"/Classified_maps/2021_RF_4bands_V2/rasters", sep = ""), pattern = "tif$", full.names = TRUE),
            list.files(path=paste(getwd(),"/Classified_maps/2022_RF_4bands_V2/rasters", sep = ""), pattern = "tif$", full.names = TRUE))
fs2122 = fs2122[-c(1:3, 41:64)]
raster_stack_2122 = raster::stack(fs2122)
vec_names_2122 = names(raster_stack_2122)
date_values_2122 <- as.Date(names(raster_stack_2122), format="X%Y%m%d")
vec_day_2122 = yday(date_values_2122)
vec_day_2122[29] = vec_day_2122[29] + 365
vec_day_2122[30] = vec_day_2122[30] + 365 # 2022
vec_day_2122[31] = vec_day_2122[31] + 365
vec_day_2122[32] = vec_day_2122[32] + 365
vec_day_2122[33] = vec_day_2122[33] + 365
vec_day_2122[34] = vec_day_2122[34] + 365
vec_day_2122[35] = vec_day_2122[35] + 365
vec_day_2122[36] = vec_day_2122[36] + 365
vec_day_2122[37] = vec_day_2122[37] + 365

crs(raster_stack_2122) <- "+proj=utm +zone=31 +datum=WGS84 +units=m +no_defs"
raster_stack_2122_lambert93 = projectRaster(raster_stack_2122, crs = "+init=epsg:2154", method = "ngb")

raster_stack_2122_lambert93_masked = mask(raster_stack_2122_lambert93, mask)
## Saison biologique 2022 ##

fs22 <- list.files(path=paste(getwd(),"/Classified_maps/2022_RF_4bands_V2/rasters", sep = ""), pattern = "tif$", full.names = TRUE)
fs22 = fs22[-c(1:9)]
raster_stack_22 = raster::stack(fs22)
vec_names_22 = names(raster_stack_22)
date_values_22 <- as.Date(names(raster_stack_22), format="X%Y%m%d")
vec_day_22 = yday(date_values_22)
crs(raster_stack_22) <- "+proj=utm +zone=31 +datum=WGS84 +units=m +no_defs"
raster_stack_22_lambert93 = projectRaster(raster_stack_22, crs = "+init=epsg:2154", method = "ngb")

raster_stack_22_lambert93_masked = mask(raster_stack_22_lambert93, mask)

## Saison biologique 2023 ##

fs23 <- list.files(path=paste(getwd(),"/Classified_maps/2023_RF_4bands_V2/rasters", sep = ""), pattern = "tif$", full.names = TRUE)
#fs23 = fs23[-c(1:8)]
raster_stack_23 = raster::stack(fs23)
vec_names_23 = names(raster_stack_23)
date_values_23 <- as.Date(names(raster_stack_23), format="X%Y%m%d")
vec_day_23 = yday(date_values_23)
crs(raster_stack_23) <- "+proj=utm +zone=31 +datum=WGS84 +units=m +no_defs"
raster_stack_23_lambert93 = projectRaster(raster_stack_23, crs = "+init=epsg:2154", method = "ngb")

raster_stack_23_lambert93_masked = mask(raster_stack_23_lambert93, mask)


########NA pixels
anymap = raster_stack_1718_lambert93_masked[[1]]
mapna = is.na(anymap)




#### periodes absences ####
## Saison biologique 2017 - 2018 ##

fs1718 <- c(list.files(path=paste(getwd(),"/Classified_maps/2017_RF_4bands_V2/rasters", sep = ""), pattern = "tif$", full.names = TRUE),
            list.files(path=paste(getwd(),"/Classified_maps/2018_RF_4bands_V2/rasters", sep = ""), pattern = "tif$", full.names = TRUE))
fs1718_abs = fs1718[-c(1:16,54:63)]
raster_stack_1718_abs = raster::stack(fs1718_abs)
vec_names_1718_abs = names(raster_stack_1718_abs)
date_values_1718_abs <- as.Date(names(raster_stack_1718_abs), format="X%Y%m%d")
vec_day_1718_abs = yday(date_values_1718_abs)
vec_day_1718_abs[c(14:37)] = vec_day_1718_abs[c(14:37)] + 365 # 2018

crs(raster_stack_1718_abs) <- "+proj=utm +zone=31 +datum=WGS84 +units=m +no_defs"
raster_stack_1718_abs_lambert93 = projectRaster(raster_stack_1718_abs, crs = "+init=epsg:2154", method = "ngb")


crs(raster_stack_1718_abs_lambert93)
crs(mask) = crs(raster_stack_1718_abs_lambert93)


mask <- projectRaster(mask,raster_stack_1718_abs_lambert93,method = 'ngb')

raster_stack_1718_abs_lambert93_masked = mask(raster_stack_1718_abs_lambert93, mask)


## Saison biologique 2018 - 2019 ##

fs1819 <- c(list.files(path=paste(getwd(),"/Classified_maps/2018_RF_4bands_V2/rasters", sep = ""), pattern = "tif$", full.names = TRUE),
            list.files(path=paste(getwd(),"/Classified_maps/2019_RF_4bands_V2/rasters", sep = ""), pattern = "tif$", full.names = TRUE))
fs1819_abs = fs1819[-c(1:24,54:63)]
raster_stack_1819_abs = raster::stack(fs1819_abs)
vec_names_1819_abs = names(raster_stack_1819_abs)
date_values_1819_abs <- as.Date(names(raster_stack_1819_abs), format="X%Y%m%d")
vec_day_1819_abs = yday(date_values_1819_abs)
vec_day_1819_abs[c(11:29)] = vec_day_1819_abs[c(11:29)] + 365 # 2019
crs(raster_stack_1819_abs) <- "+proj=utm +zone=31 +datum=WGS84 +units=m +no_defs"
raster_stack_1819_abs_lambert93 = projectRaster(raster_stack_1819_abs, crs = "+init=epsg:2154", method = "ngb")

raster_stack_1819_abs_lambert93_masked = mask(raster_stack_1819_abs_lambert93, mask)

## Saison biologique 2019 - 2020 ##

fs1920 <- c(list.files(path=paste(getwd(),"/Classified_maps/2019_RF_4bands_V2/rasters", sep = ""), pattern = "tif$", full.names = TRUE),
            list.files(path=paste(getwd(),"/Classified_maps/2020_RF_4bands_V2/rasters", sep = ""), pattern = "tif$", full.names = TRUE))
fs1920_abs = fs1920[-c(1:19,51:60)]
raster_stack_1920_abs = raster::stack(fs1920_abs)
vec_names_1920_abs = names(raster_stack_1920_abs)
date_values_1920_abs <- as.Date(names(raster_stack_1920_abs), format="X%Y%m%d")
vec_day_1920_abs = yday(date_values_1920_abs)
vec_day_1920_abs[c(11:31)] = vec_day_1920_abs[c(11:31)] + 365 # 2020

crs(raster_stack_1920_abs) <- "+proj=utm +zone=31 +datum=WGS84 +units=m +no_defs"
raster_stack_1920_abs_lambert93 = projectRaster(raster_stack_1920_abs, crs = "+init=epsg:2154", method = "ngb")

raster_stack_1920_abs_lambert93_masked = mask(raster_stack_1920_abs_lambert93, mask)
## Saison biologique 2020 - 2021 ##

fs2021 <- c(list.files(path=paste(getwd(),"/Classified_maps/2020_RF_4bands_V2/rasters", sep = ""), pattern = "tif$", full.names = TRUE),
            list.files(path=paste(getwd(),"/Classified_maps/2021_RF_4bands_V2/rasters", sep = ""), pattern = "tif$", full.names = TRUE))
fs2021_abs = fs2021[-c(1:21, 50:62)]
raster_stack_2021_abs = raster::stack(fs2021_abs)
vec_names_2021_abs = names(raster_stack_2021_abs)
date_values_2021_abs <- as.Date(names(raster_stack_2021_abs), format="X%Y%m%d")
vec_day_2021_abs = yday(date_values_2021_abs)
vec_day_2021_abs[c(11:28)] = vec_day_2021_abs[c(11:28)] + 366 # 2021 (année 2020 bisextile)

crs(raster_stack_2021_abs) <- "+proj=utm +zone=31 +datum=WGS84 +units=m +no_defs"
raster_stack_2021_abs_lambert93 = projectRaster(raster_stack_2021_abs, crs = "+init=epsg:2154", method = "ngb")

raster_stack_2021_abs_lambert93_masked = mask(raster_stack_2021_abs_lambert93, mask)
## Saison biologique 2021 - 2022 ##

fs2122 <- c(list.files(path=paste(getwd(),"/Classified_maps/2021_RF_4bands_V2/rasters", sep = ""), pattern = "tif$", full.names = TRUE),
            list.files(path=paste(getwd(),"/Classified_maps/2022_RF_4bands_V2/rasters", sep = ""), pattern = "tif$", full.names = TRUE))
fs2122_abs = fs2122[-c(1:18, 52:64)]
raster_stack_2122_abs = raster::stack(fs2122_abs)
vec_names_2122_abs = names(raster_stack_2122_abs)
date_values_2122_abs <- as.Date(names(raster_stack_2122_abs), format="X%Y%m%d")
vec_day_2122_abs = yday(date_values_2122_abs)
vec_day_2122_abs[c(14:33)] = vec_day_2122_abs[c(14:33)] + 365 # 2022

crs(raster_stack_2122_abs) <- "+proj=utm +zone=31 +datum=WGS84 +units=m +no_defs"
raster_stack_2122_abs_lambert93 = projectRaster(raster_stack_2122_abs, crs = "+init=epsg:2154", method = "ngb")

raster_stack_2122_abs_lambert93_masked = mask(raster_stack_2122_abs_lambert93, mask)
## Saison biologique 2022-2023 ##

fs2223 <- c(list.files(path=paste(getwd(),"/Classified_maps/2022_RF_4bands_V2/rasters", sep = ""), pattern = "tif$", full.names = TRUE),
            list.files(path=paste(getwd(),"/Classified_maps/2023_RF_4bands_V2/rasters", sep = ""), pattern = "tif$", full.names = TRUE))
fs2223_abs = fs2223[-c(1:20,54:67)]
raster_stack_2223_abs = raster::stack(fs2223_abs)
vec_names_2223_abs = names(raster_stack_2223_abs)
date_values_2223_abs <- as.Date(names(raster_stack_2223_abs), format="X%Y%m%d")
vec_day_2223_abs = yday(date_values_2223_abs)
vec_day_2223_abs[c(14:33)] = vec_day_2223_abs[c(14:33)] + 365 # 2023

crs(raster_stack_2223_abs) <- "+proj=utm +zone=31 +datum=WGS84 +units=m +no_defs"
raster_stack_2223_abs_lambert93 = projectRaster(raster_stack_2223_abs, crs = "+init=epsg:2154", method = "ngb")

raster_stack_2223_abs_lambert93_masked = mask(raster_stack_2223_abs_lambert93, mask)








############################################################
###################### first date ##########################
############################################################


###################### 2017-2018 ##########################

first_dates_1718 <- calc(raster_stack_1718_lambert93_masked, function(x){get_physical_values(x, method = "start", vec_day = vec_day_1718)})
first_dates_1718[mapna] = NA

color_palette <- colorRampPalette(c("green", "red"))


# Plot the first dates raster with the custom color palette and legend
plot(first_dates_1718, col=color_palette(100),main = "Starting date 2017-2018", 
     legend=TRUE, axis.args=list(at = c(1:33),
                                 labels = date_values_1718,
                                 cex.axis=0.6))
#raster::plot(masque_roseliere, add = T, legend = F, col = "darkgrey")

h = hist(first_dates_1718, labels = date_values_1718, main = "Starting date 2017-2018", breaks = c(0:33))

data_plot_1718 = data.frame(date_values_1718)
data_plot_1718$day_number = vec_day_1718
data_plot_1718$first_date = h[["counts"]]



###################### 2018-2019 ##########################

first_dates_1819 <- calc(raster_stack_1819_lambert93_masked, function(x){get_physical_values(x, method = "start", vec_day = vec_day_1819)})
first_dates_1819[mapna] = NA

color_palette <- colorRampPalette(c("green", "red"))


# Plot the first dates raster with the custom color palette and legend
plot(first_dates_1819, col=color_palette(100),main = "Starting date 2018-2019", 
     legend=TRUE, axis.args=list(at = c(1:28),
                                 labels = date_values_1819,
                                 cex.axis=0.6))
#raster::plot(masque_roseliere, add = T, legend = F, col = "darkgrey")

h = hist(first_dates_1819, labels = date_values_1819, main = "Starting date 2018-2019", breaks = c(0:28))

data_plot_1819 = data.frame(date_values_1819)
data_plot_1819$day_number = vec_day_1819
data_plot_1819$first_date = h[["counts"]]


###################### 2019-2020 ##########################

first_dates_1920 <- calc(raster_stack_1920_lambert93_masked, function(x){get_physical_values(x, method = "start", vec_day = vec_day_1920)})
first_dates_1920[mapna] = NA

color_palette <- colorRampPalette(c("green", "red"))


# Plot the first dates raster with the custom color palette and legend
plot(first_dates_1920, col=color_palette(100),main = "Starting date 2019-2020", 
     legend=TRUE, axis.args=list(at = c(1:33),
                                 labels = date_values_1920,
                                 cex.axis=0.6))
#raster::plot(masque_roseliere, add = T, legend = F, col = "darkgrey")

h = hist(first_dates_1920, labels = date_values_1920, main = "Starting date 2019-2020", breaks = c(0:33))

data_plot_1920 = data.frame(date_values_1920)
data_plot_1920$day_number = vec_day_1920
data_plot_1920$first_date = h[["counts"]]


###################### 2020-2021 ##########################

first_dates_2021 <- calc(raster_stack_2021_lambert93_masked, function(x){get_physical_values(x, method = "start", vec_day = vec_day_2021)})
first_dates_2021[mapna] = NA

color_palette <- colorRampPalette(c("green", "red"))


# Plot the first dates raster with the custom color palette and legend
plot(first_dates_2021, col=color_palette(100),main = "Starting date 2020-2021", 
     legend=TRUE, axis.args=list(at = c(1:30),
                                 labels = date_values_2021,
                                 cex.axis=0.6))
#raster::plot(masque_roseliere, add = T, legend = F, col = "darkgrey")

h = hist(first_dates_2021, labels = date_values_2021, main = "Starting date 2019-2020", breaks = c(0:30))

data_plot_2021 = data.frame(date_values_2021)
data_plot_2021$day_number = vec_day_2021
data_plot_2021$first_date = h[["counts"]]


###################### 2021-2022 ##########################

first_dates_2122 <- calc(raster_stack_2122_lambert93_masked, function(x){get_physical_values(x, method = "start", vec_day = vec_day_2122)})
first_dates_2122[mapna] = NA

color_palette <- colorRampPalette(c("green", "red"))


# Plot the first dates raster with the custom color palette and legend
plot(first_dates_2122, col=color_palette(100),main = "Starting date 2021-2022", 
     legend=TRUE, axis.args=list(at = c(1:37),
                                 labels = date_values_2122,
                                 cex.axis=0.6))
#raster::plot(masque_roseliere, add = T, legend = F, col = "darkgrey")

h = hist(first_dates_2122, labels = date_values_2122, main = "Starting date 2021-2022", breaks = c(0:37))

data_plot_2122 = data.frame(date_values_2122)
data_plot_2122$day_number = vec_day_2122
data_plot_2122$first_date = h[["counts"]]

###################### 2022 ##########################

first_dates_22 <- calc(raster_stack_22_lambert93_masked, function(x){get_physical_values(x, method = "start", vec_day = vec_day_22)})
first_dates_22[mapna] = NA

color_palette <- colorRampPalette(c("green", "red"))


# Plot the first dates raster with the custom color palette and legend
plot(first_dates_22, col=color_palette(100),main = "Starting date 2022", 
     legend=TRUE, axis.args=list(at = c(1:24),
                                 labels = date_values_22,
                                 cex.axis=0.6))
#raster::plot(masque_roseliere, add = T, legend = F, col = "darkgrey")

h = hist(first_dates_22, labels = date_values_22, main = "Starting date 2022", breaks = c(0:24))

data_plot_22 = data.frame(date_values_22)
data_plot_22$day_number = vec_day_22
data_plot_22$first_date = h[["counts"]]



###################### 2023 ##########################

first_dates_23 <- calc(raster_stack_23_lambert93_masked, function(x){get_physical_values(x, method = "start", vec_day = vec_day_23)})
first_dates_23[mapna] = NA

color_palette <- colorRampPalette(c("green", "red"))


# Plot the first dates raster with the custom color palette and legend
plot(first_dates_23, col=color_palette(100),main = "Starting date 2023", 
     legend=TRUE, axis.args=list(at = c(1:34),
                                 labels = date_values_23,
                                 cex.axis=0.6))
#raster::plot(masque_roseliere, add = T, legend = F, col = "darkgrey")

h = hist(first_dates_23, labels = date_values_23, main = "Starting date 2023", breaks = c(0:34))

data_plot_23 = data.frame(date_values_23)
data_plot_23$day_number = vec_day_23
data_plot_23$first_date = h[["counts"]]



######################## PLOTS STARTING DAY ##########################


p1 = ggplot()+
  ggtitle("Starting day") +
  xlab("Day") + ylab("Number of pixels")+
  
  geom_point(aes(x=data_plot_1718$day_number, y=data_plot_1718$first_date, color = "2017-2018"), shape = 2)+
  geom_path(aes(x=data_plot_1718$day_number, y=data_plot_1718$first_date, color = "2017-2018"))+
  
  geom_point(aes(x=data_plot_1819$day_number, y=data_plot_1819$first_date, color = "2018-2019"), shape = 2)+
  geom_path(aes(x=data_plot_1819$day_number, y=data_plot_1819$first_date, color = "2018-2019"))+
  
  geom_point(aes(x=data_plot_1920$day_number, y=data_plot_1920$first_date, color = "2019-2020"), shape = 2)+
  geom_path(aes(x=data_plot_1920$day_number, y=data_plot_1920$first_date, color = "2019-2020"))+
  
  geom_point(aes(x=data_plot_2021$day_number, y=data_plot_2021$first_date, color = "2020-2021"), shape = 2)+
  geom_path(aes(x=data_plot_2021$day_number, y=data_plot_2021$first_date, color = "2020-2021"))+
  
  geom_point(aes(x=data_plot_2122$day_number, y=data_plot_2122$first_date, color = "2021-2022"), shape = 2)+
  geom_path(aes(x=data_plot_2122$day_number, y=data_plot_2122$first_date, color = "2021-2022"))+
  
  geom_point(aes(x=data_plot_22$day_number, y=data_plot_22$first_date, color = "2022"), shape = 2)+
  geom_path(aes(x=data_plot_22$day_number, y=data_plot_22$first_date, color = "2022"))

plot(p1)


#########################################################
################### extinction dates ####################
#########################################################

color_palette <- colorRampPalette(c("red", "green"))

###################### 2017-2018 ########################

last_dates_1718 <- calc(raster_stack_1718_lambert93_masked, function(x){get_physical_values(x, method = "end", vec_day = vec_day_1718)})
last_dates_1718[mapna] = NA


plot(last_dates_1718, col=color_palette(100),main = "Extinction date 2017-2018", 
     legend=TRUE, axis.args=list(at = c(1:33),
                                 labels = date_values_1718,
                                 cex.axis=0.6))
#raster::plot(masque_roseliere, add = T, legend = F, col = "darkgrey")

h = hist(last_dates_1718, labels = date_values_1718, main = "Extinction date 2017-2018", breaks = c(0:33))

data_plot_1718 = data.frame(date_values_1718)
data_plot_1718$day_number = vec_day_1718
data_plot_1718$last_date = h[["counts"]]



###################### 2018-2019 ########################

last_dates_1819 <- calc(raster_stack_1819_lambert93_masked, function(x){get_physical_values(x, method = "end", vec_day = vec_day_1819)})
last_dates_1819[mapna] = NA


plot(last_dates_1819, col=color_palette(100),main = "Extinction date 2018-2019", 
     legend=TRUE, axis.args=list(at = c(1:28),
                                 labels = date_values_1819,
                                 cex.axis=0.6))
#raster::plot(masque_roseliere, add = T, legend = F, col = "darkgrey")

h = hist(last_dates_1819, labels = date_values_1819, main = "Extinction date 2018-2019", breaks = c(0:28))

data_plot_1819 = data.frame(date_values_1819)
data_plot_1819$day_number = vec_day_1819
data_plot_1819$last_date = h[["counts"]]

###################### 2019-2020 ########################

last_dates_1920 <- calc(raster_stack_1920_lambert93_masked, function(x){get_physical_values(x, method = "end", vec_day = vec_day_1920)})
last_dates_1920[mapna] = NA


plot(last_dates_1920, col=color_palette(100),main = "Extinction date 2019-2020", 
     legend=TRUE, axis.args=list(at = c(1:33),
                                 labels = date_values_1920,
                                 cex.axis=0.6))
#raster::plot(masque_roseliere, add = T, legend = F, col = "darkgrey")

h = hist(last_dates_1920, labels = date_values_1920, main = "Extinction date 2019-2020", breaks = c(0:33))

data_plot_1920 = data.frame(date_values_1920)
data_plot_1920$day_number = vec_day_1920
data_plot_1920$last_date = h[["counts"]]

###################### 2020-2021 ########################

last_dates_2021 <- calc(raster_stack_2021_lambert93_masked, function(x){get_physical_values(x, method = "end", vec_day = vec_day_2021)})
last_dates_2021[mapna] = NA


plot(last_dates_2021, col=color_palette(100),main = "Extinction date 2020-2021", 
     legend=TRUE, axis.args=list(at = c(1:30),
                                 labels = date_values_2021,
                                 cex.axis=0.6))
#raster::plot(masque_roseliere, add = T, legend = F, col = "darkgrey")

h = hist(last_dates_2021, labels = date_values_2021, main = "Extinction date 2020-2021", breaks = c(0:30))

data_plot_2021 = data.frame(date_values_2021)
data_plot_2021$day_number = vec_day_2021
data_plot_2021$last_date = h[["counts"]]

###################### 2021-2022 ########################

last_dates_2122 <- calc(raster_stack_2122_lambert93_masked, function(x){get_physical_values(x, method = "end", vec_day = vec_day_2122)})
last_dates_2122[mapna] = NA


plot(last_dates_2122, col=color_palette(100),main = "Extinction date 2021-2022", 
     legend=TRUE, axis.args=list(at = c(1:37),
                                 labels = date_values_2122,
                                 cex.axis=0.6))
#raster::plot(masque_roseliere, add = T, legend = F, col = "darkgrey")

h = hist(last_dates_2122, labels = date_values_2122, main = "Extinction date 2021-2022", breaks = c(0:37))

data_plot_2122 = data.frame(date_values_2122)
data_plot_2122$day_number = vec_day_2122
data_plot_2122$last_date = h[["counts"]]

######################### 2022 ##########################

last_dates_22 <- calc(raster_stack_22_lambert93_masked, function(x){get_physical_values(x, method = "end", vec_day = vec_day_22)})
last_dates_22[mapna] = NA

plot(last_dates_22, col=color_palette(100),main = "Extinction date 2022", 
     legend=TRUE, axis.args=list(at = c(1:24),
                                 labels = date_values_22,
                                 cex.axis=0.6))
#raster::plot(masque_roseliere, add = T, legend = F, col = "darkgrey")

h = hist(last_dates_22, labels = date_values_22, main = "Extinction date 2022", breaks = c(0:24))

data_plot_22 = data.frame(date_values_22)
data_plot_22$day_number = vec_day_22
data_plot_22$last_date = h[["counts"]]



######################## PLOTS EXTINCTION DAY ##########################


p2 = ggplot()+
  ggtitle("Extinction day") +
  xlab("Day") + ylab("Number of pixels")+
  
  geom_point(aes(x=data_plot_1718$day_number, y=data_plot_1718$last_date, color = "2017-2018"), shape = 2)+
  geom_path(aes(x=data_plot_1718$day_number, y=data_plot_1718$last_date, color = "2017-2018"))+
  
  geom_point(aes(x=data_plot_1819$day_number, y=data_plot_1819$last_date, color = "2018-2019"), shape = 2)+
  geom_path(aes(x=data_plot_1819$day_number, y=data_plot_1819$last_date, color = "2018-2019"))+
  
  geom_point(aes(x=data_plot_1920$day_number, y=data_plot_1920$last_date, color = "2019-2020"), shape = 2)+
  geom_path(aes(x=data_plot_1920$day_number, y=data_plot_1920$last_date, color = "2019-2020"))+
  
  geom_point(aes(x=data_plot_2021$day_number, y=data_plot_2021$last_date, color = "2020-2021"), shape = 2)+
  geom_path(aes(x=data_plot_2021$day_number, y=data_plot_2021$last_date, color = "2020-2021"))+
  
  geom_point(aes(x=data_plot_2122$day_number, y=data_plot_2122$last_date, color = "2021-2022"), shape = 2)+
  geom_path(aes(x=data_plot_2122$day_number, y=data_plot_2122$last_date, color = "2021-2022"))+
  
  geom_point(aes(x=data_plot_22$day_number, y=data_plot_22$last_date, color = "2022"), shape = 2)+
  geom_path(aes(x=data_plot_22$day_number, y=data_plot_22$last_date, color = "2022"))

plot(p2)



############################################################
################## Lifetime durations ######################
############################################################

color_palette <- colorRampPalette(c("darkorange3", "green"))

###################### 2017-2018 ##########################


diff_values_1718 = difftime(date_values_1718[last_dates_1718@data@values],
                            date_values_1718[first_dates_1718@data@values], 
                            units="days")
diff_values_1718 = as.numeric(diff_values_1718)

ltd_1718 = first_dates_1718
ltd_1718@data@values = diff_values_1718

ltd_1718[mapna] = NA

p_ltd_1718 = plot(ltd_1718, col = hcl.colors(20, "Greens", rev = TRUE), main = "Maximum lifetimes 2017/2018 (in days)")
#raster::plot(masque_roseliere, add = T, legend = F, col = "darkgrey")

h2 = hist(ltd_1718, main = "Lifetimes 2017/2018 (in days)", breaks = c(-.5:400.5))
h3 = hist(ltd_1718, main = "Lifetimes 2017/2018 (in days)")

summary(ltd_1718)


ltd_values_1718 = h2[["counts"]]
days_1718 = h2[["mids"]]

ltd_plot_df_1718 = data.frame(days_1718, ltd_values_1718)
ltd_plot_df_1718 = subset(ltd_plot_df_1718, ltd_plot_df_1718$ltd_values_1718 != 0)


p3 = ggplot()+
  ggtitle("Maximum lifetimes (in days)") +
  xlab("Days") + ylab("Number of pixels")+
  
  geom_point(aes(x=ltd_plot_df_1718$days_1718, y=ltd_plot_df_1718$ltd_values_1718, color = "2017-2018"), shape = 2)#+
#geom_path(aes(x=ltd_plot_df_1718$days_1718, y=ltd_plot_df_1718$ltd_values_1718, color = "2017-2018"))


plot(p3)


###################### 2018-2019 ##########################

diff_values_1819 = difftime(date_values_1819[last_dates_1819@data@values],
                            date_values_1819[first_dates_1819@data@values], 
                            units="days")
diff_values_1819 = as.numeric(diff_values_1819)

ltd_1819 = first_dates_1819
ltd_1819@data@values = diff_values_1819

ltd_1819[mapna] = NA

p_ltd_1819 = plot(ltd_1819, col = hcl.colors(20, "Greens", rev = TRUE), main = "Maximum lifetimes 2018/2019 (in days)")
#raster::plot(masque_roseliere, add = T, legend = F, col = "darkgrey")

h2 = hist(ltd_1819, main = "Lifetimes 2018/2019 (in days)", breaks = c(-0.5:400.5))
h3 = hist(ltd_1819, main = "Lifetimes 2018/2019 (in days)")


ltd_values_1819 = h2[["counts"]]
days_1819 = h2[["mids"]]

ltd_plot_df_1819 = data.frame(days_1819, ltd_values_1819)
ltd_plot_df_1819 = subset(ltd_plot_df_1819, ltd_plot_df_1819$ltd_values_1819 != 0)


p3 = ggplot()+
  ggtitle("Maximum lifetimes (in days)") +
  xlab("Days") + ylab("Number of pixels")+
  
  geom_point(aes(x=ltd_plot_df_1819$days_1819, y=ltd_plot_df_1819$ltd_values_1819, color = "2018-2019"), shape = 2)#+
#geom_path(aes(x=ltd_plot_df_1819$days_1819, y=ltd_plot_df_1819$ltd_values_1819, color = "2018-2019"))


plot(p3)


###################### 2019-2020 ##########################

diff_values_1920 = difftime(date_values_1920[last_dates_1920@data@values],
                            date_values_1920[first_dates_1920@data@values], 
                            units="days")
diff_values_1920 = as.numeric(diff_values_1920)

ltd_1920 = first_dates_1920
ltd_1920@data@values = diff_values_1920

ltd_1920[mapna] = NA

p_ltd_1920 = plot(ltd_1920, col = hcl.colors(20, "Greens", rev = TRUE), main = "Maximum lifetimes 2019/2020 (in days)")
#raster::plot(masque_roseliere, add = T, legend = F, col = "darkgrey")

h2 = hist(ltd_1920, main = "Lifetimes 2019/2020 (in days)", breaks = c(-0.5:400.5))
h3 = hist(ltd_1920, main = "Lifetimes 2019/2020 (in days)")


ltd_values_1920 = h2[["counts"]]
days_1920 = h2[["mids"]]

ltd_plot_df_1920 = data.frame(days_1920, ltd_values_1920)
ltd_plot_df_1920 = subset(ltd_plot_df_1920, ltd_plot_df_1920$ltd_values_1920 != 0)


p3 = ggplot()+
  ggtitle("Maximum lifetimes (in days)") +
  xlab("Days") + ylab("Number of pixels")+
  
  geom_point(aes(x=ltd_plot_df_1920$days_1920, y=ltd_plot_df_1920$ltd_values_1920, color = "2019-2020"), shape = 2)#+
#geom_path(aes(x=ltd_plot_df_1920$days_1920, y=ltd_plot_df_1920$ltd_values_1920, color = "2019-2020"))


plot(p3)


###################### 2020-2021 ##########################
diff_values_2021 = difftime(date_values_2021[last_dates_2021@data@values],
                            date_values_2021[first_dates_2021@data@values], 
                            units="days")
diff_values_2021 = as.numeric(diff_values_2021)

ltd_2021 = first_dates_2021
ltd_2021@data@values = diff_values_2021

ltd_2021[mapna] = NA

p_ltd_2021 = plot(ltd_2021, col = hcl.colors(20, "Greens", rev = TRUE), main = "Maximum lifetimes 2020/2021 (in days)")
#raster::plot(masque_roseliere, add = T, legend = F, col = "darkgrey")

h2 = hist(ltd_2021, main = "Lifetimes 2020/2021 (in days)", breaks = c(-0.5:400.5))
h3 = hist(ltd_2021, main = "Lifetimes 2020/2021 (in days)")


ltd_values_2021 = h2[["counts"]]
days_2021 = h2[["mids"]]

ltd_plot_df_2021 = data.frame(days_2021, ltd_values_2021)
ltd_plot_df_2021 = subset(ltd_plot_df_2021, ltd_plot_df_2021$ltd_values_2021 != 0)


p3 = ggplot()+
  ggtitle("Maximum lifetimes (in days)") +
  xlab("Days") + ylab("Number of pixels")+
  
  geom_point(aes(x=ltd_plot_df_2021$days_2021, y=ltd_plot_df_2021$ltd_values_2021, color = "2020-2021"), shape = 2)#+
#geom_path(aes(x=ltd_plot_df_2021$days_2021, y=ltd_plot_df_2021$ltd_values_2021, color = "2020-2021"))


plot(p3)

###################### 2021-2022 ##########################
diff_values_2122 = difftime(date_values_2122[last_dates_2122@data@values],
                            date_values_2122[first_dates_2122@data@values], 
                            units="days")
diff_values_2122 = as.numeric(diff_values_2122)

ltd_2122 = first_dates_2122
ltd_2122@data@values = diff_values_2122

ltd_2122[mapna] = NA

p_ltd_2122 = plot(ltd_2122, col = hcl.colors(20, "Greens", rev = TRUE), main = "Maximum lifetimes 2021/2022 (in days)")
#raster::plot(masque_roseliere, add = T, legend = F, col = "darkgrey")

h2 = hist(ltd_2122, main = "Lifetimes 2021/2022 (in days)", breaks = c(-0.5:400.5))
h3 = hist(ltd_2122, main = "Lifetimes 2021/2022 (in days)")


ltd_values_2122 = h2[["counts"]]
days_2122 = h2[["mids"]]

ltd_plot_df_2122 = data.frame(days_2122, ltd_values_2122)
ltd_plot_df_2122 = subset(ltd_plot_df_2122, ltd_plot_df_2122$ltd_values_2122 != 0)


p3 = ggplot()+
  ggtitle("Maximum lifetimes (in days)") +
  xlab("Days") + ylab("Number of pixels")+
  
  geom_point(aes(x=ltd_plot_df_2122$days_2122, y=ltd_plot_df_2122$ltd_values_2122, color = "2021-2022"), shape = 2)#+
#geom_path(aes(x=ltd_plot_df_2122$days_2122, y=ltd_plot_df_2122$ltd_values_2122, color = "2021-2022"))


plot(p3)


###################### 2022 ##########################
diff_values_22 = difftime(date_values_22[last_dates_22@data@values],
                          date_values_22[first_dates_22@data@values], 
                          units="days")
diff_values_22 = as.numeric(diff_values_22)

ltd_22 = first_dates_22
ltd_22@data@values = diff_values_22

ltd_22[mapna] = NA

p_ltd_22 = plot(ltd_22, col = hcl.colors(20, "Greens", rev = TRUE), main = "Maximum lifetimes 2022 (in days)")
#raster::plot(masque_roseliere, add = T, legend = F, col = "darkgrey")

h2 = hist(ltd_22, main = "Lifetimes 2022 (in days)", breaks = c(-0.5:400.5))
h3 = hist(ltd_22, main = "Lifetimes 2022 (in days)")

ltd_values_22 = h2[["counts"]]
days_22 = h2[["mids"]]

ltd_plot_df_22 = data.frame(days_22, ltd_values_22)
ltd_plot_df_22 = subset(ltd_plot_df_22, ltd_plot_df_22$ltd_values_22 != 0)


p3 = ggplot()+
  ggtitle("Maximum lifetimes (in days)") +
  xlab("Days") + ylab("Number of pixels")+
  
  geom_point(aes(x=ltd_plot_df_22$days_22, y=ltd_plot_df_22$ltd_values_22, color = "2022"), shape = 2)+
  geom_density()
#geom_path(aes(x=ltd_plot_df_22$days_22, y=ltd_plot_df_22$ltd_values_22, color = "2017-2018"))


plot(p3)




















############################################################
###################### first ABS date ##########################
############################################################


###################### 2017-2018 ##########################

first_dates_1718_abs <- calc(raster_stack_1718_abs_lambert93_masked, function(x){get_physical_values_abs(x, method = "start", vec_day = vec_day_1718_abs)})
first_dates_1718_abs[mapna] = NA

color_palette <- colorRampPalette(c("green", "red"))


# Plot the first dates raster with the custom color palette and legend
plot(first_dates_1718_abs, col=color_palette(100),main = "Starting date 2017-2018", 
     legend=TRUE, axis.args=list(at = c(1:37),
                                 labels = date_values_1718_abs,
                                 cex.axis=0.6))
#raster::plot(masque_roseliere, add = T, legend = F, col = "darkgrey")

h = hist(first_dates_1718_abs, labels = date_values_1718_abs, main = "Starting date 2017-2018", breaks = c(0:37))

data_plot_1718_abs = data.frame(date_values_1718_abs)
data_plot_1718_abs$day_number = vec_day_1718_abs
data_plot_1718_abs$first_date = h[["counts"]]



###################### 2018-2019 ##########################

first_dates_1819_abs <- calc(raster_stack_1819_abs_lambert93_masked, function(x){get_physical_values_abs(x, method = "start", vec_day = vec_day_1819_abs)})
first_dates_1819_abs[mapna] = NA

color_palette <- colorRampPalette(c("green", "red"))


# Plot the first dates raster with the custom color palette and legend
plot(first_dates_1819_abs, col=color_palette(100),main = "Starting date 2018-2019", 
     legend=TRUE, axis.args=list(at = c(1:29),
                                 labels = date_values_1819_abs,
                                 cex.axis=0.6))
#raster::plot(masque_roseliere, add = T, legend = F, col = "darkgrey")

h = hist(first_dates_1819_abs, labels = date_values_1819_abs, main = "Starting date 2018-2019", breaks = c(0:29))

data_plot_1819_abs = data.frame(date_values_1819_abs)
data_plot_1819_abs$day_number = vec_day_1819_abs
data_plot_1819_abs$first_date = h[["counts"]]


###################### 2019-2020 ##########################

first_dates_1920_abs <- calc(raster_stack_1920_abs_lambert93_masked, function(x){get_physical_values_abs(x, method = "start", vec_day = vec_day_1920_abs)})
first_dates_1920_abs[mapna] = NA

color_palette <- colorRampPalette(c("green", "red"))


# Plot the first dates raster with the custom color palette and legend
plot(first_dates_1920_abs, col=color_palette(100),main = "Starting date 2019-2020", 
     legend=TRUE, axis.args=list(at = c(1:31),
                                 labels = date_values_1920_abs,
                                 cex.axis=0.6))
#raster::plot(masque_roseliere, add = T, legend = F, col = "darkgrey")

h = hist(first_dates_1920_abs, labels = date_values_1920_abs, main = "Starting date 2019-2020", breaks = c(0:31))

data_plot_1920_abs = data.frame(date_values_1920_abs)
data_plot_1920_abs$day_number = vec_day_1920_abs
data_plot_1920_abs$first_date = h[["counts"]]


###################### 2020-2021_abs ##########################

first_dates_2021_abs <- calc(raster_stack_2021_abs_lambert93_masked, function(x){get_physical_values_abs(x, method = "start", vec_day = vec_day_2021_abs)})
first_dates_2021_abs[mapna] = NA

color_palette <- colorRampPalette(c("green", "red"))


# Plot the first dates raster with the custom color palette and legend
plot(first_dates_2021_abs, col=color_palette(100),main = "Starting date 2020-2021_abs", 
     legend=TRUE, axis.args=list(at = c(1:28),
                                 labels = date_values_2021_abs,
                                 cex.axis=0.6))
#raster::plot(masque_roseliere, add = T, legend = F, col = "darkgrey")

h = hist(first_dates_2021_abs, labels = date_values_2021_abs, main = "Starting date 2019-2020", breaks = c(0:28))

data_plot_2021_abs = data.frame(date_values_2021_abs)
data_plot_2021_abs$day_number = vec_day_2021_abs
data_plot_2021_abs$first_date = h[["counts"]]


###################### 2021_abs-2022 ##########################

first_dates_2122_abs <- calc(raster_stack_2122_abs_lambert93_masked, function(x){get_physical_values_abs(x, method = "start", vec_day = vec_day_2122_abs)})
first_dates_2122_abs[mapna] = NA

color_palette <- colorRampPalette(c("green", "red"))


# Plot the first dates raster with the custom color palette and legend
plot(first_dates_2122_abs, col=color_palette(100),main = "Starting date 2021_abs-2022", 
     legend=TRUE, axis.args=list(at = c(1:33),
                                 labels = date_values_2122_abs,
                                 cex.axis=0.6))
#raster::plot(masque_roseliere, add = T, legend = F, col = "darkgrey")

h = hist(first_dates_2122_abs, labels = date_values_2122_abs, main = "Starting date 2021_abs-2022", breaks = c(0:33))

data_plot_2122_abs = data.frame(date_values_2122_abs)
data_plot_2122_abs$day_number = vec_day_2122_abs
data_plot_2122_abs$first_date = h[["counts"]]

###################### 2022 ##########################

first_dates_2223_abs <- calc(raster_stack_2223_abs_lambert93_masked, function(x){get_physical_values_abs(x, method = "start", vec_day = vec_day_2223_abs)})
first_dates_2223_abs[mapna] = NA

color_palette <- colorRampPalette(c("green", "red"))


# Plot the first dates raster with the custom color palette and legend
plot(first_dates_2223_abs, col=color_palette(100),main = "Starting date 202223_abs", 
     legend=TRUE, axis.args=list(at = c(1:33),
                                 labels = date_values_2223_abs,
                                 cex.axis=0.6))
#raster::plot(masque_roseliere, add = T, legend = F, col = "darkgrey")

h = hist(first_dates_2223_abs, labels = date_values_2223_abs, main = "Starting date 202223_abs", breaks = c(0:33))

data_plot_2223_abs = data.frame(date_values_2223_abs)
data_plot_2223_abs$day_number = vec_day_2223_abs
data_plot_2223_abs$first_date = h[["counts"]]










#########################################################
################### extinction dates ####################
#########################################################

color_palette <- colorRampPalette(c("red", "green"))

###################### 2017-2018 ########################

last_dates_1718_abs <- calc(raster_stack_1718_abs_lambert93_masked, function(x){get_physical_values_abs(x, method = "end", vec_day = vec_day_1718_abs)})
last_dates_1718_abs[mapna] = NA


plot(last_dates_1718_abs, col=color_palette(100),main = "Extinction date 2017-2018", 
     legend=TRUE, axis.args=list(at = c(1:37),
                                 labels = date_values_1718_abs,
                                 cex.axis=0.6))
#raster::plot(masque_roseliere, add = T, legend = F, col = "darkgrey")

h = hist(last_dates_1718_abs, labels = date_values_1718_abs, main = "Extinction date 2017-2018", breaks = c(0:37))

data_plot_1718_abs = data.frame(date_values_1718_abs)
data_plot_1718_abs$day_number = vec_day_1718_abs
data_plot_1718_abs$last_date = h[["counts"]]



###################### 2018-2019 ########################

last_dates_1819_abs <- calc(raster_stack_1819_abs_lambert93_masked, function(x){get_physical_values_abs(x, method = "end", vec_day = vec_day_1819_abs)})
last_dates_1819_abs[mapna] = NA


plot(last_dates_1819_abs, col=color_palette(100),main = "Extinction date 2018-2019", 
     legend=TRUE, axis.args=list(at = c(1:29),
                                 labels = date_values_1819_abs,
                                 cex.axis=0.6))
#raster::plot(masque_roseliere, add = T, legend = F, col = "darkgrey")

h = hist(last_dates_1819_abs, labels = date_values_1819_abs, main = "Extinction date 2018-2019", breaks = c(0:29))

data_plot_1819_abs = data.frame(date_values_1819_abs)
data_plot_1819_abs$day_number = vec_day_1819_abs
data_plot_1819_abs$last_date = h[["counts"]]

###################### 2019-2020 ########################

last_dates_1920_abs <- calc(raster_stack_1920_abs_lambert93_masked, function(x){get_physical_values_abs(x, method = "end", vec_day = vec_day_1920_abs)})
last_dates_1920_abs[mapna] = NA


plot(last_dates_1920_abs, col=color_palette(100),main = "Extinction date 2019-2020", 
     legend=TRUE, axis.args=list(at = c(1:31),
                                 labels = date_values_1920_abs,
                                 cex.axis=0.6))
#raster::plot(masque_roseliere, add = T, legend = F, col = "darkgrey")

h = hist(last_dates_1920_abs, labels = date_values_1920_abs, main = "Extinction date 2019-2020", breaks = c(0:31))

data_plot_1920_abs = data.frame(date_values_1920_abs)
data_plot_1920_abs$day_number = vec_day_1920_abs
data_plot_1920_abs$last_date = h[["counts"]]

###################### 2020-2021_abs ########################

last_dates_2021_abs <- calc(raster_stack_2021_abs_lambert93_masked, function(x){get_physical_values_abs(x, method = "end", vec_day = vec_day_2021_abs)})
last_dates_2021_abs[mapna] = NA


plot(last_dates_2021_abs, col=color_palette(100),main = "Extinction date 2020-2021_abs", 
     legend=TRUE, axis.args=list(at = c(1:28),
                                 labels = date_values_2021_abs,
                                 cex.axis=0.6))
#raster::plot(masque_roseliere, add = T, legend = F, col = "darkgrey")

h = hist(last_dates_2021_abs, labels = date_values_2021_abs, main = "Extinction date 2020-2021_abs", breaks = c(0:28))

data_plot_2021_abs = data.frame(date_values_2021_abs)
data_plot_2021_abs$day_number = vec_day_2021_abs
data_plot_2021_abs$last_date = h[["counts"]]

###################### 2021_abs-2022 ########################

last_dates_2122_abs <- calc(raster_stack_2122_abs_lambert93_masked, function(x){get_physical_values_abs(x, method = "end", vec_day = vec_day_2122_abs)})
last_dates_2122_abs[mapna] = NA


plot(last_dates_2122_abs, col=color_palette(100),main = "Extinction date 2021_abs-2022", 
     legend=TRUE, axis.args=list(at = c(1:33),
                                 labels = date_values_2122_abs,
                                 cex.axis=0.6))
#raster::plot(masque_roseliere, add = T, legend = F, col = "darkgrey")

h = hist(last_dates_2122_abs, labels = date_values_2122_abs, main = "Extinction date 2021_abs-2022", breaks = c(0:33))

data_plot_2122_abs = data.frame(date_values_2122_abs)
data_plot_2122_abs$day_number = vec_day_2122_abs
data_plot_2122_abs$last_date = h[["counts"]]

######################### 2022 ##########################

last_dates_2223_abs <- calc(raster_stack_2223_abs_lambert93_masked, function(x){get_physical_values_abs(x, method = "end", vec_day = vec_day_2223_abs)})
last_dates_2223_abs[mapna] = NA

plot(last_dates_2223_abs, col=color_palette(100),main = "Extinction date 202223_abs", 
     legend=TRUE, axis.args=list(at = c(1:33),
                                 labels = date_values_2223_abs,
                                 cex.axis=0.6))
#raster::plot(masque_roseliere, add = T, legend = F, col = "darkgrey")

h = hist(last_dates_2223_abs, labels = date_values_2223_abs, main = "Extinction date 202223_abs", breaks = c(0:33))

data_plot_2223_abs = data.frame(date_values_2223_abs)
data_plot_2223_abs$day_number = vec_day_2223_abs
data_plot_2223_abs$last_date = h[["counts"]]





############################################################
################## abs time durations ######################
############################################################

color_palette <- colorRampPalette(c("darkorange3", "green"))

###################### 2017-2018 ##########################


diff_values_1718_abs = difftime(date_values_1718_abs[last_dates_1718_abs@data@values],
                            date_values_1718_abs[first_dates_1718_abs@data@values], 
                            units="days")
diff_values_1718_abs = as.numeric(diff_values_1718_abs)

atd_1718 = first_dates_1718_abs
atd_1718@data@values = diff_values_1718_abs

atd_1718[mapna] = NA

plot(atd_1718, col = hcl.colors(20, "blues", rev = TRUE), main = "Maximum absence times 2017/2018 (in days)")
#raster::plot(masque_roseliere, add = T, legend = F, col = "darkgrey")

h2 = hist(atd_1718, main = "absence times 2017/2018 (in days)", breaks = c(-0.5:500.5))
h3 = hist(atd_1718, main = "absence times 2017/2018 (in days)")


atd_values_1718 = h2[["counts"]]
days_1718 = h2[["mids"]]

atd_plot_df_1718 = data.frame(days_1718, atd_values_1718)
atd_plot_df_1718 = subset(atd_plot_df_1718, atd_plot_df_1718$atd_values_1718 != 0)


p3 = ggplot()+
  ggtitle("Maximum absence times (in days)") +
  xlab("Days") + ylab("Number of pixels")+
  
  geom_point(aes(x=atd_plot_df_1718$days_1718, y=atd_plot_df_1718$atd_values_1718, color = "2017-2018"), shape = 2)#+
#geom_path(aes(x=atd_plot_df_1718$days_1718, y=atd_plot_df_1718$atd_values_1718, color = "2017-2018"))


plot(p3)


###################### 2018-2019 ##########################

diff_values_1819_abs = difftime(date_values_1819_abs[last_dates_1819_abs@data@values],
                                date_values_1819_abs[first_dates_1819_abs@data@values], 
                                units="days")
diff_values_1819_abs = as.numeric(diff_values_1819_abs)

atd_1819 = first_dates_1819_abs
atd_1819@data@values = diff_values_1819_abs

atd_1819[mapna] = NA

plot(atd_1819, col = hcl.colors(20, "Blues", rev = TRUE), main = "Maximum absence times 2018/2019 (in days)")
#raster::plot(masque_roseliere, add = T, legend = F, col = "darkgrey")

h2 = hist(atd_1819, main = "absence times 2018/2019 (in days)", breaks = c(-0.5:500.5))
h3 = hist(atd_1819, main = "absence times 2018/2019 (in days)")


atd_values_1819 = h2[["counts"]]
days_1819 = h2[["mids"]]

atd_plot_df_1819 = data.frame(days_1819, atd_values_1819)
atd_plot_df_1819 = subset(atd_plot_df_1819, atd_plot_df_1819$atd_values_1819 != 0)


p3 = ggplot()+
  ggtitle("Maximum absence times (in days)") +
  xlab("Days") + ylab("Number of pixels")+
  
  geom_point(aes(x=atd_plot_df_1819$days_1819, y=atd_plot_df_1819$atd_values_1819, color = "2018-2019"), shape = 2)#+
#geom_path(aes(x=atd_plot_df_1819$days_1819, y=atd_plot_df_1819$atd_values_1819, color = "2018-2019"))


plot(p3)


###################### 2019-2020 ##########################

diff_values_1920_abs = difftime(date_values_1920_abs[last_dates_1920_abs@data@values],
                                date_values_1920_abs[first_dates_1920_abs@data@values], 
                                units="days")
diff_values_1920_abs = as.numeric(diff_values_1920_abs)

atd_1920 = first_dates_1920_abs
atd_1920@data@values = diff_values_1920_abs

atd_1920[mapna] = NA

plot(atd_1920, col = hcl.colors(20, "Blues", rev = TRUE), main = "Maximum absence times 2019/2020 (in days)")
#raster::plot(masque_roseliere, add = T, legend = F, col = "darkgrey")

h2 = hist(atd_1920, main = "absence times 2019/2020 (in days)", breaks = c(-0.5:500.5))
h3 = hist(atd_1920, main = "absence times 2019/2020 (in days)")


atd_values_1920 = h2[["counts"]]
days_1920 = h2[["mids"]]

atd_plot_df_1920 = data.frame(days_1920, atd_values_1920)
atd_plot_df_1920 = subset(atd_plot_df_1920, atd_plot_df_1920$atd_values_1920 != 0)


p3 = ggplot()+
  ggtitle("Maximum absence times (in days)") +
  xlab("Days") + ylab("Number of pixels")+
  
  geom_point(aes(x=atd_plot_df_1920$days_1920, y=atd_plot_df_1920$atd_values_1920, color = "2019-2020"), shape = 2)#+
#geom_path(aes(x=atd_plot_df_1920$days_1920, y=atd_plot_df_1920$atd_values_1920, color = "2019-2020"))


plot(p3)


###################### 2020-2021 ##########################

diff_values_2021_abs = difftime(date_values_2021_abs[last_dates_2021_abs@data@values],
                                date_values_2021_abs[first_dates_2021_abs@data@values], 
                                units="days")
diff_values_2021_abs = as.numeric(diff_values_2021_abs)

atd_2021 = first_dates_2021_abs
atd_2021@data@values = diff_values_2021_abs

atd_2021[mapna] = NA

plot(atd_2021, col = hcl.colors(20, "Blues", rev = TRUE), main = "Maximum absence times 2020/2021 (in days)")
#raster::plot(masque_roseliere, add = T, legend = F, col = "darkgrey")

h2 = hist(atd_2021, main = "absence times 2020/2021 (in days)", breaks = c(-0.5:500.5))
h3 = hist(atd_2021, main = "absence times 2020/2021 (in days)")


atd_values_2021 = h2[["counts"]]
days_2021 = h2[["mids"]]

atd_plot_df_2021 = data.frame(days_2021, atd_values_2021)
atd_plot_df_2021 = subset(atd_plot_df_2021, atd_plot_df_2021$atd_values_2021 != 0)


p3 = ggplot()+
  ggtitle("Maximum absence times (in days)") +
  xlab("Days") + ylab("Number of pixels")+
  
  geom_point(aes(x=atd_plot_df_2021$days_2021, y=atd_plot_df_2021$atd_values_2021, color = "2020-2021"), shape = 2)#+
#geom_path(aes(x=atd_plot_df_2021$days_2021, y=atd_plot_df_2021$atd_values_2021, color = "2020-2021"))


plot(p3)

###################### 2021-2022 ##########################


diff_values_2122_abs = difftime(date_values_2122_abs[last_dates_2122_abs@data@values],
                                date_values_2122_abs[first_dates_2122_abs@data@values], 
                                units="days")
diff_values_2122_abs = as.numeric(diff_values_2122_abs)

atd_2122 = first_dates_2122_abs
atd_2122@data@values = diff_values_2122_abs

atd_2122[mapna] = NA

plot(atd_2122, col = hcl.colors(20, "blues", rev = TRUE), main = "Maximum absence times 2021/2022 (in days)")
#raster::plot(masque_roseliere, add = T, legend = F, col = "darkgrey")

h2 = hist(atd_2122, main = "absence times 2021/2022 (in days)", breaks = c(-0.5:500.5))
h3 = hist(atd_2122, main = "absence times 2021/2022 (in days)")


atd_values_2122 = h2[["counts"]]
days_2122 = h2[["mids"]]

atd_plot_df_2122 = data.frame(days_2122, atd_values_2122)
atd_plot_df_2122 = subset(atd_plot_df_2122, atd_plot_df_2122$atd_values_2122 != 0)


p3 = ggplot()+
  ggtitle("Maximum absence times (in days)") +
  xlab("Days") + ylab("Number of pixels")+
  
  geom_point(aes(x=atd_plot_df_2122$days_2122, y=atd_plot_df_2122$atd_values_2122, color = "2021-2022"), shape = 2)#+
#geom_path(aes(x=atd_plot_df_2122$days_2122, y=atd_plot_df_2122$atd_values_2122, color = "2021-2022"))


plot(p3)


###################### 2022 - 2023 ##########################

diff_values_2223_abs = difftime(date_values_2223_abs[last_dates_2223_abs@data@values],
                                date_values_2223_abs[first_dates_2223_abs@data@values], 
                                units="days")
diff_values_2223_abs = as.numeric(diff_values_2223_abs)

atd_2223 = first_dates_2223_abs
atd_2223@data@values = diff_values_2223_abs

atd_2223[mapna] = NA

plot(atd_2223, col = hcl.colors(20, "blues", rev = TRUE), main = "Maximum absence times 2022-2023 (in days)")
#raster::plot(masque_roseliere, add = T, legend = F, col = "darkgrey")

h2 = hist(atd_2223, main = "absence times 2022-2023 (in days)", breaks = c(-0.5:500.5))
h3 = hist(atd_2223, main = "absence times 2022-2023 (in days)")

atd_values_2223 = h2[["counts"]]
days_2223 = h2[["mids"]]

atd_plot_df_2223 = data.frame(days_2223, atd_values_2223)
atd_plot_df_2223 = subset(atd_plot_df_2223, atd_plot_df_2223$atd_values_2223 != 0)


p3 = ggplot()+
  ggtitle("Maximum absence times (in days)") +
  xlab("Days") + ylab("Number of pixels")+
  
  geom_point(aes(x=atd_plot_df_2223$days_2223, y=atd_plot_df_2223$atd_values_2223, color = "2022-2023"), shape = 2)+
  geom_density()
#geom_path(aes(x=atd_plot_df_22$days_22, y=atd_plot_df_22$atd_values_22, color = "2017-2018"))


plot(p3)





###############################################
############# PLOT MULTI HISTOGRAM ############
###############################################
plot_histogram <- function(df, feature) {
  plt <- ggplot(df, aes(x=eval(parse(text=feature)))) +
    geom_histogram(aes(y = ..density..), bins = 20, alpha=0.7, fill="#33AADE", color="black") +
    geom_density(alpha=0.3, fill="red", adjust = 2) +
    geom_vline(aes(xintercept=mean(eval(parse(text=feature)))), color="black", linetype="dashed", size=1) +
    labs(x=feature, y = "Density")
  print(plt)
}

plot_multi_histogram <- function(df, feature, label_column, means) {
  plt <- ggplot(df, aes(x=eval(parse(text=feature)), fill=eval(parse(text=label_column)))) +
    geom_histogram(alpha=0.7, position="identity", aes(y = ..density..), bins = 30, color="black") +
    geom_density(alpha=0.7, adjust = 3) +
    #geom_vline(xintercept=means, color="black", linetype="dashed", size=1)
    labs(x="Lifetimes durations", y = "Density")
  plt + guides(fill=guide_legend(title=label_column))
}




data_hist_1718 = as.data.frame(ltd_1718)
data_hist_1718$year = rep("2017-2018", nrow(data_hist_1718))
data_hist_1819 = as.data.frame(ltd_1819)
data_hist_1819$year = rep("2018-2019", nrow(data_hist_1819))
data_hist_1920 = as.data.frame(ltd_1920)
data_hist_1920$year = rep("2019-2020", nrow(data_hist_1920))
data_hist_2021 = as.data.frame(ltd_2021)
data_hist_2021$year = rep("2020-2021", nrow(data_hist_2021))
data_hist_2122 = as.data.frame(ltd_2122)
data_hist_2122$year = rep("2021-2022", nrow(data_hist_2122))
data_hist_22 = as.data.frame(ltd_22)
data_hist_22$year = rep("2022", nrow(data_hist_22))
all_data_hist <- do.call('rbind', list(data_hist_1718,data_hist_1819,data_hist_1920,data_hist_2021,data_hist_2122,data_hist_22))


plot_histogram(data_hist_1718, "layer")
plot_histogram(data_hist_1819, "layer")
plot_histogram(data_hist_1920, "layer")
plot_histogram(data_hist_2021, "layer")
plot_histogram(data_hist_2122, "layer")
plot_histogram(data_hist_22, "layer")

options(repr.plot.width = 30, repr.plot.height = 8)
plot_multi_histogram(all_data_hist, "layer", "year")



##########

fd_hist_1718 = as.data.frame(first_dates_1718)
fd_hist_1718$date = date_values_1718[fd_hist_1718$layer+1]
fd_hist_1718$year = rep("2017-2018", nrow(fd_hist_1718))
fd_hist_1819 = as.data.frame(first_dates_1819)
fd_hist_1819$date = date_values_1819[fd_hist_1819$layer+1]
fd_hist_1819$year = rep("2018-2019", nrow(fd_hist_1819))
fd_hist_1920 = as.data.frame(first_dates_1920)
fd_hist_1920$date = date_values_1920[fd_hist_1920$layer+1]
fd_hist_1920$year = rep("2019-2020", nrow(fd_hist_1920))
fd_hist_2021 = as.data.frame(first_dates_2021)
fd_hist_2021$date = date_values_2021[fd_hist_2021$layer+1]
fd_hist_2021$year = rep("2020-2021", nrow(fd_hist_2021))
fd_hist_2122 = as.data.frame(first_dates_2122)
fd_hist_2122$date = date_values_2122[fd_hist_2122$layer+1]
fd_hist_2122$year = rep("2021-2022", nrow(fd_hist_2122))
fd_hist_22 = as.data.frame(first_dates_22)
fd_hist_22$date = date_values_22[fd_hist_22$layer+1]
fd_hist_22$year = rep("2022", nrow(fd_hist_22))
all_fd_data_hist <- do.call('rbind', list(fd_hist_1718,fd_hist_1819,fd_hist_1920,fd_hist_2021,fd_hist_2122,fd_hist_22))



ex_hist_1718 = as.data.frame(last_dates_1718)
ex_hist_1718$date = date_values_1718[ex_hist_1718$layer+1]
ex_hist_1718$year = rep("2017-2018", nrow(ex_hist_1718))
ex_hist_1819 = as.data.frame(last_dates_1819)
ex_hist_1819$date = date_values_1819[ex_hist_1819$layer+1]
ex_hist_1819$year = rep("2018-2019", nrow(ex_hist_1819))
ex_hist_1920 = as.data.frame(last_dates_1920)
ex_hist_1920$date = date_values_1920[ex_hist_1920$layer+1]
ex_hist_1920$year = rep("2019-2020", nrow(ex_hist_1920))
ex_hist_2021 = as.data.frame(last_dates_2021)
ex_hist_2021$date = date_values_2021[ex_hist_2021$layer+1]
ex_hist_2021$year = rep("2020-2021", nrow(ex_hist_2021))
ex_hist_2122 = as.data.frame(last_dates_2122)
ex_hist_2122$date = date_values_2122[ex_hist_2122$layer+1]
ex_hist_2122$year = rep("2021-2022", nrow(ex_hist_2122))
ex_hist_22 = as.data.frame(last_dates_22)
ex_hist_22$date = date_values_22[ex_hist_22$layer+1]
ex_hist_22$year = rep("2022", nrow(ex_hist_22))
all_ex_data_hist <- do.call('rbind', list(ex_hist_1718,ex_hist_1819,ex_hist_1920,ex_hist_2021,ex_hist_2122,ex_hist_22))







############ write rasters #################




raster::writeRaster(first_dates_1718, paste(getwd(),"/Graphical_results/Qtt_physiques/rasters/s1718_raster", sep = ""), format = "GTiff")
raster::writeRaster(first_dates_1819, paste(getwd(),"/Graphical_results/Qtt_physiques/rasters/s1819_raster", sep = ""), format = "GTiff")
raster::writeRaster(first_dates_1920, paste(getwd(),"/Graphical_results/Qtt_physiques/rasters/s1920_raster", sep = ""), format = "GTiff")
raster::writeRaster(first_dates_2021, paste(getwd(),"/Graphical_results/Qtt_physiques/rasters/s2021_raster", sep = ""), format = "GTiff")
raster::writeRaster(first_dates_2122, paste(getwd(),"/Graphical_results/Qtt_physiques/rasters/s2122_raster", sep = ""), format = "GTiff")
raster::writeRaster(first_dates_22, paste(getwd(),"/Graphical_results/Qtt_physiques/rasters/s22_raster", sep = ""), format = "GTiff")
raster::writeRaster(first_dates_23, paste(getwd(),"/Graphical_results/Qtt_physiques/rasters/s23_raster", sep = ""), format = "GTiff")

raster::writeRaster(last_dates_1718, paste(getwd(),"/Graphical_results/Qtt_physiques/rasters/e1718_raster", sep = ""), format = "GTiff")
raster::writeRaster(last_dates_1819, paste(getwd(),"/Graphical_results/Qtt_physiques/rasters/e1819_raster", sep = ""), format = "GTiff")
raster::writeRaster(last_dates_1920, paste(getwd(),"/Graphical_results/Qtt_physiques/rasters/e1920_raster", sep = ""), format = "GTiff")
raster::writeRaster(last_dates_2021, paste(getwd(),"/Graphical_results/Qtt_physiques/rasters/e2021_raster", sep = ""), format = "GTiff")
raster::writeRaster(last_dates_2122, paste(getwd(),"/Graphical_results/Qtt_physiques/rasters/e2122_raster", sep = ""), format = "GTiff")
raster::writeRaster(last_dates_22, paste(getwd(),"/Graphical_results/Qtt_physiques/rasters/e22_raster", sep = ""), format = "GTiff")
raster::writeRaster(last_dates_23, paste(getwd(),"/Graphical_results/Qtt_physiques/rasters/e23_raster", sep = ""), format = "GTiff")




raster::writeRaster(ltd_1718, paste(getwd(),"/Graphical_results/Qtt_physiques/rasters/ltd_1718_raster", sep = ""), format = "GTiff")
raster::writeRaster(ltd_1819, paste(getwd(),"/Graphical_results/Qtt_physiques/rasters/ltd_1819_raster", sep = ""), format = "GTiff")
raster::writeRaster(ltd_1920, paste(getwd(),"/Graphical_results/Qtt_physiques/rasters/ltd_1920_raster", sep = ""), format = "GTiff")
raster::writeRaster(ltd_2021, paste(getwd(),"/Graphical_results/Qtt_physiques/rasters/ltd_2021_raster", sep = ""), format = "GTiff")
raster::writeRaster(ltd_2122, paste(getwd(),"/Graphical_results/Qtt_physiques/rasters/ltd_2122_raster", sep = ""), format = "GTiff")
raster::writeRaster(ltd_22, paste(getwd(),"/Graphical_results/Qtt_physiques/rasters/ltd_22_raster", sep = ""), format = "GTiff")



raster::writeRaster(atd_1718, paste(getwd(),"/Graphical_results/Qtt_physiques/rasters/atd_1718_raster", sep = ""), format = "GTiff")
raster::writeRaster(atd_1819, paste(getwd(),"/Graphical_results/Qtt_physiques/rasters/atd_1819_raster", sep = ""), format = "GTiff")
raster::writeRaster(atd_1920, paste(getwd(),"/Graphical_results/Qtt_physiques/rasters/atd_1920_raster", sep = ""), format = "GTiff")
raster::writeRaster(atd_2021, paste(getwd(),"/Graphical_results/Qtt_physiques/rasters/atd_2021_raster", sep = ""), format = "GTiff")
raster::writeRaster(atd_2122, paste(getwd(),"/Graphical_results/Qtt_physiques/rasters/atd_2122_raster", sep = ""), format = "GTiff")
raster::writeRaster(atd_2223, paste(getwd(),"/Graphical_results/Qtt_physiques/rasters/atd_2223_raster", sep = ""), format = "GTiff")





dataset_lifetimes = all_data_hist
dataset_lifetimes = na.omit(all_data_hist)
dataset_lifetimes$year = as.factor(dataset_lifetimes$year)
colnames(dataset_lifetimes) = c("lifetime", "year")
summary(dataset_lifetimes)

write.csv(dataset_lifetimes, "~/Thèse/Resultats/Qtt_physiques/resultsV6/dataset_lifetimes_newmask.csv")


dataset_fd_ex = cbind(all_fd_data_hist, all_ex_data_hist)
dataset_fd_ex = dataset_fd_ex[,-c(1,3,4)]
colnames(dataset_fd_ex) = c("first_date", "last_date", "year")
dataset_fd_ex = na.omit(dataset_fd_ex)

write.csv(dataset_fd_ex, "~/Thèse/Resultats/Qtt_physiques/resultsV6/dataset_fd_ex_newmask.csv")



diffd = difftime(dataset_fd_ex$last_date,dataset_fd_ex$first_date, units="days")
dataset_fd_ex$diffd = diffd

hist(as.numeric(dataset_fd_ex$diffd), breaks = 40)
hist(dataset_lifetimes$lifetime, breaks = 40)




####### 6 plots ######

ltd_1718 = raster("~/Thèse/Resultats/Qtt_physiques/resultsV6/rasters/ltd_1718_raster.tif")

ltd_1819 = raster("~/Thèse/Resultats/Qtt_physiques/resultsV6/rasters/ltd_1819_raster.tif")

ltd_1920 = raster("~/Thèse/Resultats/Qtt_physiques/resultsV6/rasters/ltd_1920_raster.tif")

ltd_2021 = raster("~/Thèse/Resultats/Qtt_physiques/resultsV6/rasters/ltd_2021_raster.tif")

ltd_2122 = raster("~/Thèse/Resultats/Qtt_physiques/resultsV6/rasters/ltd_2122_raster.tif")


ltd_22 = raster("~/Thèse/Resultats/Qtt_physiques/resultsV6/rasters/ltd_22_raster.tif")


base::plot(p_ltd_1718)


grid.arrange(plot(ltd_1718),plot(ltd_1819),plot(ltd_1819),
             nrow = 3)
cont = rasterToPolygons(mask, dissolve = T)

par(mfrow = c(3,2))
p_ltd_1718 = plot(ltd_1718, col = hcl.colors(20, "Greens", rev = TRUE), main = "Maximum lifetimes 2017/2018 (in days)")
plot(cont, add = T)
p_ltd_1819 = plot(ltd_1819, col = hcl.colors(20, "Greens", rev = TRUE), main = "Maximum lifetimes 2018/2019 (in days)")
plot(cont, add = T)
p_ltd_1920 = plot(ltd_1920, col = hcl.colors(20, "Greens", rev = TRUE), main = "Maximum lifetimes 2019/2020 (in days)")
plot(cont, add = T)
p_ltd_2021 = plot(ltd_2021, col = hcl.colors(20, "Greens", rev = TRUE), main = "Maximum lifetimes 2020/2021 (in days)")
plot(cont, add = T)
p_ltd_2122 = plot(ltd_2122, col = hcl.colors(20, "Greens", rev = TRUE), main = "Maximum lifetimes 2021/2022 (in days)")
plot(cont, add = T)
p_ltd_22 = plot(ltd_22, col = hcl.colors(20, "Greens", rev = TRUE), main = "Maximum lifetimes 2022 (in days)")
plot(cont, add = T)

par(mfrow = c(1,1))

summary(ltd_1718)

ltd_1718



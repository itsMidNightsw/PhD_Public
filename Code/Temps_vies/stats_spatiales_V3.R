library(raster)
library(ggplot2)
library(lubridate)
library(gridExtra)

##data read####

ltd_1718 = raster(paste(getwd(),"/Graphical_results/Qtt_physiques/rasters/ltd_1718_raster.tif", sep = ""))
atd_1718 = raster(paste(getwd(),"/Graphical_results/Qtt_physiques/rasters/atd_1718_raster.tif", sep = ""))
stack_1718 <- stack(ltd_1718, atd_1718)
names(stack_1718) <- c("ltd", "atd")


ltd_1819 = raster(paste(getwd(),"/Graphical_results/Qtt_physiques/rasters/ltd_1819_raster.tif", sep = ""))
atd_1819 = raster(paste(getwd(),"/Graphical_results/Qtt_physiques/rasters/atd_1819_raster.tif", sep = ""))
stack_1819 <- stack(ltd_1819, atd_1819)
names(stack_1819) <- c("ltd", "atd")



ltd_1920 = raster(paste(getwd(),"/Graphical_results/Qtt_physiques/rasters/ltd_1920_raster.tif", sep = ""))
atd_1920 = raster(paste(getwd(),"/Graphical_results/Qtt_physiques/rasters/atd_1920_raster.tif", sep = ""))
stack_1920 <- stack(ltd_1920, atd_1920)
names(stack_1920) <- c("ltd", "atd")


ltd_2021 = raster(paste(getwd(),"/Graphical_results/Qtt_physiques/rasters/ltd_2021_raster.tif", sep = ""))
atd_2021 = raster(paste(getwd(),"/Graphical_results/Qtt_physiques/rasters/atd_2021_raster.tif", sep = ""))
stack_2021 <- stack(ltd_2021, atd_2021)
names(stack_2021) <- c("ltd", "atd")

ltd_2122 = raster(paste(getwd(),"/Graphical_results/Qtt_physiques/rasters/ltd_2122_raster.tif", sep = ""))
atd_2122 = raster(paste(getwd(),"/Graphical_results/Qtt_physiques/rasters/atd_2122_raster.tif", sep = ""))
stack_2122 <- stack(ltd_2122, atd_2122)
names(stack_2122) <- c("ltd", "atd")


ltd_22 = raster(paste(getwd(),"/Graphical_results/Qtt_physiques/rasters/ltd_22_raster.tif", sep = ""))
atd_2223 = raster(paste(getwd(),"/Graphical_results/Qtt_physiques/rasters/atd_2223_raster.tif", sep = ""))
stack_2223 <- stack(ltd_22, atd_2223)
names(stack_2223) <- c("ltd", "atd")



#########



data = read.csv(paste(getwd(),"/Data/Data_bagnas_herbier_hydro/herbier_4bands_V2_2017-2022.csv", sep = ""))
data$date = as.Date(data$date)
data$annee = data$date
data$annee = format(data$annee, format = "%Y")
data$annee = as.factor(data$annee)
data$surface = data$surface/10000
data$saison = rep(NA, length(data$date))
data$surface_p = round(data$surface/max(data$surface), digits = 3)
ind = which(data$surface_p == 1)
data$surface_p[ind] = 0.999

data$saison[1:35] = "2017-2018"
data$saison[36:68] = "2018-2019"
data$saison[69:95] = "2019-2020"
data$saison[96:127] = "2020-2021"
data$saison[128:164] = "2021-2022"
data$saison[165:187] = "2022"

data$saison = as.factor(data$saison)
summary(data)

season_ngf_min = aggregate(data$niveau_ngf~data$saison, FUN=min)
colnames(season_ngf_min) = c("saison","min_ngf")
season_ngf_min = as.data.frame(season_ngf_min)
season_ngf_min

#season_ngf_min$saison = as.character(season_ngf_min$saison)
#season_ngf_min[6,1] = "2022-2023"
#season_ngf_min
###### df's ######

df_1718 = as.data.frame(stack_1718)
df_1718 = na.omit(df_1718)
df_1718$saison = "2017-2018"
df_1718$min_ngf = season_ngf_min$min_ngf[1]
df_1718$assec = "normale"
df_1718$assec = as.factor(df_1718$assec)
summary(df_1718)

df_1819 = as.data.frame(stack_1819)
df_1819 = na.omit(df_1819)
df_1819$saison = "2018-2019"
df_1819$min_ngf = season_ngf_min$min_ngf[2]
df_1819$assec = "normale"
df_1819$assec = as.factor(df_1819$assec)
summary(df_1819)

df_1920 = as.data.frame(stack_1920)
df_1920 = na.omit(df_1920)
df_1920$saison = "2019-2020"
df_1920$min_ngf = season_ngf_min$min_ngf[3]
df_1920$assec = "normale"
df_1920$assec = as.factor(df_1920$assec)
summary(df_1920)

df_2021 = as.data.frame(stack_2021)
df_2021 = na.omit(df_2021)
df_2021$saison = "2020-2021"
df_2021$min_ngf = season_ngf_min$min_ngf[4]
df_2021$assec = "post-assec"
df_2021$assec = as.factor(df_2021$assec)
summary(df_2021)

df_2122 = as.data.frame(stack_2122)
df_2122 = na.omit(df_2122)
df_2122$saison = "2021-2022"
df_2122$min_ngf = season_ngf_min$min_ngf[5]
df_2122$assec = "normale"
df_2122$assec = as.factor(df_2122$assec)
summary(df_2122)

df_2223 = as.data.frame(stack_2223)
df_2223 = na.omit(df_2223)
df_2223$saison = "2022-2023"
df_2223$min_ngf = season_ngf_min$min_ngf[6]
df_2223$assec = "post-assec"
df_2223$assec = as.factor(df_2223$assec)
summary(df_2223)


#######




### all years 

df_all_years = rbind(df_1718,df_1819,df_1920,df_2021,df_2122,df_2223)
summary(df_all_years)

### all years no drought

df_all_years_nodrought = rbind(df_1718,df_1819,df_2021,df_2122)
summary(df_all_years_nodrought)


### all years with drought

df_all_years_drought = rbind(df_1920,df_2223)
summary(df_all_years_drought)



######### Stats #########

plot(df_all_years$atd, df_all_years$ltd, main = "all years")
cor.test(df_all_years$atd, df_all_years$ltd)
lm_all_years = lm(df_all_years$atd ~ df_all_years$ltd)
summary(lm_all_years)
abline(lm_all_years, col = "red")
legend("topright", paste("R squared = ",as.character(round(summary(lm_all_years)[["r.squared"]],digits = 3))))


plot(df_all_years_nodrought$atd, df_all_years_nodrought$ltd, main = "all years no drought")
cor.test(df_all_years_nodrought$atd, df_all_years_nodrought$ltd)
lm_all_years_nodrought = lm(df_all_years_nodrought$atd ~ df_all_years_nodrought$ltd)
summary(lm_all_years_nodrought)
abline(lm_all_years_nodrought, col = "red")
legend("topright", paste("R squared = ",as.character(round(summary(lm_all_years_nodrought)[["r.squared"]],digits = 3))))


plot(df_all_years_drought$atd, df_all_years_drought$ltd, main = "all years no drought")
cor.test(df_all_years_drought$atd, df_all_years_drought$ltd)
lm_all_years_drought = lm(df_all_years_drought$atd ~ df_all_years_drought$ltd)
summary(lm_all_years_drought)
abline(lm_all_years_drought, col = "red")
legend("topright", paste("R squared = ",as.character(round(summary(lm_all_years_drought)[["r.squared"]],digits = 3))))





plot(df_1718$ltd, df_1718$atd, main = "1718")
cor.test(df_1718$atd, df_1718$ltd)
lm1718 = lm(df_1718$atd ~ df_1718$ltd)
summary(lm1718)
abline(lm1718, col = "red")
legend("topright", paste("R squared = ",as.character(round(summary(lm1718)[["r.squared"]],digits = 3))))



plot(df_1819$ltd, df_1819$atd, main = "1819")
cor.test(df_1819$atd, df_1819$ltd)
lm1819 = lm(df_1819$atd ~ df_1819$ltd)
summary(lm1819)
abline(lm1819, col = "red")
legend("topright", paste("R squared = ",as.character(round(summary(lm1819)[["r.squared"]],digits = 3))))

plot(df_1920$ltd, df_1920$atd, main = "1920")
cor.test(df_1920$atd, df_1920$ltd)
lm1920 = lm(df_1920$atd ~ df_1920$ltd)
summary(lm1920)
abline(lm1920, col = "red")
legend("topright", paste("R squared = ",as.character(round(summary(lm1920)[["r.squared"]],digits = 3))))


plot(df_2021$ltd, df_2021$atd, main = "2021")
cor.test(df_2021$atd, df_2021$ltd)
lm2021 = lm(df_2021$atd ~ df_2021$ltd)
summary(lm2021)
abline(lm2021, col = "red")
legend("topright", paste("R squared = ",as.character(round(summary(lm2021)[["r.squared"]],digits = 3))))


plot(df_2122$ltd, df_2122$atd, main = "2122")
cor.test(df_2122$atd, df_2122$ltd)
lm2122 = lm(df_2122$atd ~ df_2122$ltd)
summary(lm2122)
abline(lm2122, col = "red")
legend("topright", paste("R squared = ",as.character(round(summary(lm2122)[["r.squared"]],digits = 3))))


plot(df_2223$ltd, df_2223$atd, main = "2223")
cor.test(df_2223$atd, df_2223$ltd)
lm2223 = lm(df_2223$atd ~ df_2223$ltd)
summary(lm2223)
abline(lm2223, col = "red")
legend("topright", paste("R squared = ",as.character(round(summary(lm2223)[["r.squared"]],digits = 3))))




#### density binned plot ###


coefs = coef(lm(df_all_years$atd ~ df_all_years$ltd))
ggplot(df_all_years, aes(x = ltd, y = atd)) +
  ggtitle("all years")+
  xlab("Tv")+
  ylab("Td")+
  stat_bin2d(bins = 50) +
  scale_fill_gradient(low = "lightgreen", high = "red3", limits = c(1, 1000), trans = "log10")+
  geom_abline(intercept = coefs[1], slope = coefs[2])

coefs = coef(lm(df_all_years_nodrought$atd ~ df_all_years_nodrought$ltd))
ggplot(df_all_years_nodrought, aes(x = ltd, y = atd)) +
  #ggtitle("all years no drought")+
  ggtitle("Années normales")+
  xlab("Tv")+
  ylab("Td")+
  stat_bin2d(bins = 50) +
  scale_fill_gradient(low = "lightgreen", high = "red3", limits = c(1, 1000), trans = "log10")+
  geom_abline(intercept = coefs[1], slope = coefs[2])


coefs = coef(lm(df_all_years_drought$atd ~ df_all_years_drought$ltd))
ggplot(df_all_years_drought, aes(x = ltd, y = atd)) +
  #ggtitle("all years with drought")+
  ggtitle("Années d'assec")+
  xlab("Tv")+
  ylab("Td")+
  stat_bin2d(bins = 50) +
  scale_fill_gradient(low = "lightgreen", high = "red3", limits = c(1, 1000), trans = "log10")+
  geom_abline(intercept = coefs[1], slope = coefs[2])







coefs = coef(lm(df_1718$atd ~ df_1718$ltd))
ggplot(df_1718, aes(x = ltd, y = atd)) +
  ggtitle("2017-2018")+
  xlab("Tv")+
  ylab("Td")+
  stat_bin2d(bins = 50) +
  scale_fill_gradient(low = "lightgreen", high = "red3", limits = c(1, 1000), trans = "log10")+
  geom_abline(intercept = coefs[1], slope = coefs[2])

coefs = coef(lm(df_1819$atd ~ df_1819$ltd))
ggplot(df_1819, aes(x = ltd, y = atd)) +
  ggtitle("2018-2019")+
  xlab("Tv")+
  ylab("Td")+
  stat_bin2d(bins = 50) +
  scale_fill_gradient(low = "lightgreen", high = "red3", limits = c(1, 1000), trans = "log10")+
  geom_abline(intercept = coefs[1], slope = coefs[2])

coefs = coef(lm(df_1920$atd ~ df_1920$ltd))
ggplot(df_1920, aes(x = ltd, y = atd)) +
  ggtitle("2019-2020")+
  xlab("Tv")+
  ylab("Td")+
  stat_bin2d(bins = 50) +
  scale_fill_gradient(low = "lightgreen", high = "red3", limits = c(1, 1000), trans = "log10")+
  geom_abline(intercept = coefs[1], slope = coefs[2])

coefs = coef(lm(df_2021$atd ~ df_2021$ltd))
ggplot(df_2021, aes(x = ltd, y = atd)) +
  ggtitle("2020-2021")+
  xlab("Tv")+
  ylab("Td")+
  stat_bin2d(bins = 50) +
  scale_fill_gradient(low = "lightgreen", high = "red3", limits = c(1, 1000), trans = "log10")+
  geom_abline(intercept = coefs[1], slope = coefs[2])

coefs = coef(lm(df_2122$atd ~ df_2122$ltd))
ggplot(df_2122, aes(x = ltd, y = atd)) +
  ggtitle("2021-2022")+
  xlab("Tv")+
  ylab("Td")+
  stat_bin2d(bins = 50) +
  scale_fill_gradient(low = "lightgreen", high = "red3", limits = c(1, 1000), trans = "log10")+
  geom_abline(intercept = coefs[1], slope = coefs[2])

coefs = coef(lm(df_2223$atd ~ df_2223$ltd))
ggplot(df_2223, aes(x = ltd, y = atd)) +
  ggtitle("2022-2023")+
  xlab("Tv")+
  ylab("Td")+
  stat_bin2d(bins = 50) +
  scale_fill_gradient(low = "lightgreen", high = "red3", limits = c(1, 1000), trans = "log10")+
  geom_abline(intercept = coefs[1], slope = coefs[2])




####### with the covariate

plot(df_all_years$atd ~ df_all_years$ltd, main = "all years with the covariable")
cor.test(df_all_years$atd, df_all_years$ltd)

lm_all_years_cova = lm(df_all_years$atd ~ df_all_years$ltd + df_all_years$min_ngf + df_all_years$ltd:df_all_years$min_ngf)
summary(lm_all_years_cova)
abline(lm_all_years_cova, col = "red")
legend("topright", paste("R squared = ",as.character(round(summary(lm_all_years_cova)[["r.squared"]],digits = 3))))

# temps de dormance courts lorsque temps de vies longs
# temps de dormance courts losqu'il après des années normales





##############################################################
########## TV en fonction de la date de redemarrage ##########
##############################################################


ltd_1718 = raster("~/Thèse/Resultats/Qtt_physiques/resultsV6/rasters/ltd_1718_raster.tif")
s_1819 = raster("~/Thèse/Resultats/Qtt_physiques/resultsV6/rasters/s1819_raster.tif")
stack_1 <- stack(ltd_1718, s_1819)
names(stack_1) <- c("ltd", "s")



ltd_1819 = raster("~/Thèse/Resultats/Qtt_physiques/resultsV6/rasters/ltd_1819_raster.tif")
s_1920 = raster("~/Thèse/Resultats/Qtt_physiques/resultsV6/rasters/s1920_raster.tif")
stack_2 <- stack(ltd_1819, s_1920)
names(stack_2) <- c("ltd", "s")



ltd_1920 = raster("~/Thèse/Resultats/Qtt_physiques/resultsV6/rasters/ltd_1920_raster.tif")
s_2021 = raster("~/Thèse/Resultats/Qtt_physiques/resultsV6/rasters/s2021_raster.tif")
stack_3 <- stack(ltd_1920, s_2021)
names(stack_3) <- c("ltd", "s")


ltd_2021 = raster("~/Thèse/Resultats/Qtt_physiques/resultsV6/rasters/ltd_2021_raster.tif")
s_2122 = raster("~/Thèse/Resultats/Qtt_physiques/resultsV6/rasters/s2122_raster.tif")
stack_4 <- stack(ltd_2021, s_2122)
names(stack_4) <- c("ltd", "s")

ltd_2122 = raster("~/Thèse/Resultats/Qtt_physiques/resultsV6/rasters/ltd_2122_raster.tif")
s_22 = raster("~/Thèse/Resultats/Qtt_physiques/resultsV6/rasters/s22_raster.tif")
stack_5 <- stack(ltd_2122, s_22)
names(stack_5) <- c("ltd", "s")


ltd_22 = raster("~/Thèse/Resultats/Qtt_physiques/resultsV6/rasters/ltd_22_raster.tif")
s_23 = raster("~/Thèse/Resultats/Qtt_physiques/resultsV6/rasters/s23_raster.tif")
stack_6 <- stack(ltd_22, s_23)
names(stack_6) <- c("ltd", "s")


#### vecday #####

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


fs1819 <- c(list.files(path="~/Thèse/Resultats/2018_RF_4bands_V2/rasters", pattern = "tif$", full.names = TRUE),
            list.files(path="~/Thèse/Resultats/2019_RF_4bands_V2/rasters", pattern = "tif$", full.names = TRUE))
fs1819 = fs1819[-c(1:6,35:63)]
raster_stack_1819 = raster::stack(fs1819)
vec_names_1819 = names(raster_stack_1819)
date_values_1819 <- as.Date(names(raster_stack_1819), format="X%Y%m%d")
vec_day_1819 = yday(date_values_1819)


fs1920 <- c(list.files(path="~/Thèse/Resultats/2019_RF_4bands_V2/rasters", pattern = "tif$", full.names = TRUE),
            list.files(path="~/Thèse/Resultats/2020_RF_4bands_V2/rasters", pattern = "tif$", full.names = TRUE))
fs1920 = fs1920[-c(34:60)]
raster_stack_1920 = raster::stack(fs1920)
vec_names_1920 = names(raster_stack_1920)
date_values_1920 <- as.Date(names(raster_stack_1920), format="X%Y%m%d")
vec_day_1920 = yday(date_values_1920)
vec_day_1920[30] = vec_day_1920[30] + 365 # 2020
vec_day_1920[31] = vec_day_1920[31] + 365
vec_day_1920[32] = vec_day_1920[32] + 365
vec_day_1920[33] = vec_day_1920[33] + 365

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


fs2122 <- c(list.files(path="~/Thèse/Resultats/2021_RF_4bands_V2/rasters", pattern = "tif$", full.names = TRUE),
            list.files(path="~/Thèse/Resultats/2022_RF_4bands_V2/rasters", pattern = "tif$", full.names = TRUE))
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


fs22 <- list.files(path="~/Thèse/Resultats/2022_RF_4bands_V2/rasters", pattern = "tif$", full.names = TRUE)
fs22 = fs22[-c(1:9)]
raster_stack_22 = raster::stack(fs22)
vec_names_22 = names(raster_stack_22)
date_values_22 <- as.Date(names(raster_stack_22), format="X%Y%m%d")
vec_day_22 = yday(date_values_22)

fs23 <- list.files(path="~/Thèse/Resultats/2023_RF_4bands_V2/rasters", pattern = "tif$", full.names = TRUE)
#fs23 = fs23[-c(1:8)]
raster_stack_23 = raster::stack(fs23)
vec_names_23 = names(raster_stack_23)
date_values_23 <- as.Date(names(raster_stack_23), format="X%Y%m%d")
vec_day_23 = yday(date_values_23)


########################

df_1 = as.data.frame(stack_1)
df_1 = na.omit(df_1)
df_1$saison = "2017-2018"
df_1$min_ngf = season_ngf_min$min_ngf[1]
df_1$assec = "normale"
df_1$assec = as.factor(df_1$assec)
df_1$s_day = vec_day_1819[df_1$s]
summary(df_1)

df_2 = as.data.frame(stack_2)
df_2 = na.omit(df_2)
df_2$saison = "2018-2019"
df_2$min_ngf = season_ngf_min$min_ngf[2]
df_2$assec = "normale"
df_2$assec = as.factor(df_2$assec)
df_2$s_day = vec_day_1920[df_2$s]
summary(df_2)

df_3 = as.data.frame(stack_3)
df_3 = na.omit(df_3)
df_3$saison = "2019-2020"
df_3$min_ngf = season_ngf_min$min_ngf[3]
df_3$assec = "assec"
df_3$assec = as.factor(df_3$assec)
df_3$s_day = vec_day_2021[df_3$s]
summary(df_3)

df_4 = as.data.frame(stack_4)
df_4 = na.omit(df_4)
df_4$saison = "2020-2021"
df_4$min_ngf = season_ngf_min$min_ngf[4]
df_4$assec = "normale"
df_4$assec = as.factor(df_4$assec)
df_4$s_day = vec_day_2122[df_4$s]
summary(df_4)

df_5 = as.data.frame(stack_5)
df_5 = na.omit(df_5)
df_5$saison = "2021-2022"
df_5$min_ngf = season_ngf_min$min_ngf[5]
df_5$assec = "normale"
df_5$assec = as.factor(df_5$assec)
df_5$s_day = vec_day_22[df_5$s]
summary(df_5)

df_6 = as.data.frame(stack_6)
df_6 = na.omit(df_6)
df_6$saison = "2022-2023"
df_6$min_ngf = season_ngf_min$min_ngf[6]
df_6$assec = "assec"
df_6$assec = as.factor(df_6$assec)
df_6$s_day = vec_day_23[df_6$s]
summary(df_6)


df_all_years = rbind(df_1,df_2,df_3,df_4,df_5,df_6)
summary(df_all_years)

df_4firsts = rbind(df_1,df_2,df_3,df_4)
summary(df_4firsts)

########### plots ###########

coefs = coef(lm(df_1$ltd ~ df_1$s_day))
plot1 = ggplot(df_1, aes(x = s_day, y = ltd)) +
  ggtitle("2017-2018 (normale)")+
  #xlab("Tv")+
  #ylab("Jours depuis le 1er Janvier")+
  xlab("Jours depuis le 1er Janvier 2018")+
  ylab("Temps de vie en 2017")+
  stat_bin2d(bins = 40) +
  scale_fill_gradient(low = "lightgreen", high = "red3", limits = c(1, 1000), trans = "log10")+
  geom_abline(intercept = coefs[1], slope = coefs[2])
plot1

coefs = coef(lm(df_2$ltd ~ df_2$s_day))
plot2 = ggplot(df_2, aes(x = s_day, y = ltd)) +
  ggtitle("2018-2019 (normale)")+
  #xlab("Tv")+
  #ylab("Jours depuis le 1er Janvier")+
  xlab("Jours depuis le 1er Janvier 2019")+
  ylab("Temps de vie en 2018")+
  stat_bin2d(bins = 40) +
  scale_fill_gradient(low = "lightgreen", high = "red3", limits = c(1, 1000), trans = "log10")+
  geom_abline(intercept = coefs[1], slope = coefs[2])
plot2

coefs = coef(lm(df_3$ltd ~ df_3$s_day))
plot3 = ggplot(df_3, aes(x = s_day, y = ltd)) +
  ggtitle("2019-2020 (assec)")+
  #xlab("Tv")+
  #ylab("Jours depuis le 1er Janvier")+
  xlab("Jours depuis le 1er Janvier 2020")+
  ylab("Temps de vie en 2019")+
  stat_bin2d(bins = 40) +
  scale_fill_gradient(low = "lightgreen", high = "red3", limits = c(1, 1000), trans = "log10")+
  geom_abline(intercept = coefs[1], slope = coefs[2])
plot3

coefs = coef(lm(df_4$ltd ~ df_4$s_day))
plot4 = ggplot(df_4, aes(x = s_day, y = ltd)) +
  ggtitle("2020-2021 (normale)")+
  #xlab("Tv")+
  #ylab("Jours depuis le 1er Janvier")+
  xlab("Jours depuis le 1er Janvier 2021")+
  ylab("Temps de vie en 2020")+
  stat_bin2d(bins = 40) +
  scale_fill_gradient(low = "lightgreen", high = "red3", limits = c(1, 1000), trans = "log10")+
  geom_abline(intercept = coefs[1], slope = coefs[2])
plot4

coefs = coef(lm(df_5$ltd ~ df_5$s_day))
plot5 = ggplot(df_5, aes(x = s_day, y = ltd)) +
  ggtitle("2021-2022 (normale)")+
  #xlab("Tv")+
  #ylab("Jours depuis le 1er Janvier")+
  xlab("Jours depuis le 1er Janvier 2022")+
  ylab("Temps de vie en 2021")+
  stat_bin2d(bins = 40) +
  scale_fill_gradient(low = "lightgreen", high = "red3", limits = c(1, 1000), trans = "log10")+
  geom_abline(intercept = coefs[1], slope = coefs[2])
plot5

coefs = coef(lm(df_6$ltd ~ df_6$s_day))
plot6 = ggplot(df_6, aes(x = s_day, y = ltd)) +
  ggtitle("2022-2023 (assec)")+
  #xlab("Tv")+
  #ylab("Jours depuis le 1er Janvier")+
  xlab("Jours depuis le 1er Janvier 2023")+
  ylab("Temps de vie en 2022")+
  stat_bin2d(bins = 40) +
  scale_fill_gradient(low = "lightgreen", high = "red3", limits = c(1, 1000), trans = "log10")+
  geom_abline(intercept = coefs[1], slope = coefs[2])
plot6


grid.arrange(plot1, plot2, plot3, plot4, plot5, plot6, nrow=3)






plot(df_all_years$s_day ~ df_all_years$ltd, main = "all years with the covariable")
cor.test(df_all_years$s_day, df_all_years$ltd)

lm_all_years_cova = lm(df_all_years$s_day ~ df_all_years$ltd + df_all_years$min_ngf + df_all_years$ltd:df_all_years$min_ngf)
summary(lm_all_years_cova)
abline(lm_all_years_cova, col = "red")
legend("topright", paste("R squared = ",as.character(round(summary(lm_all_years_cova)[["r.squared"]],digits = 3))))



plot(df_4firsts$s_day ~ df_4firsts$ltd, main = "all years with the covariable")
cor.test(df_4firsts$s_day, df_4firsts$ltd)

lm_4firsts_cova = lm(df_4firsts$s_day ~ df_4firsts$ltd + df_4firsts$min_ngf + df_4firsts$ltd:df_4firsts$min_ngf)
summary(lm_4firsts_cova)
abline(lm_4firsts_cova, col = "red")
legend("topright", paste("R squared = ",as.character(round(summary(lm_4firsts_cova)[["r.squared"]],digits = 3))))








########### plots dans le sens conventionnel ###########

coefs = coef(lm(df_1$s_day ~ df_1$ltd))
plot1 = ggplot(df_1, aes(x = ltd, y = s_day)) +
  ggtitle("2017-2018 (normale)")+
  #xlab("Tv")+
  #ylab("Jours depuis le 1er Janvier")+
  ylab("Jours depuis le 1er Janvier 2018")+
  xlab("Temps de vie en 2017")+
  stat_bin2d(bins = 40) +
  scale_fill_gradient(low = "lightgreen", high = "red3", limits = c(1, 1000), trans = "log10")+
  geom_abline(intercept = coefs[1], slope = coefs[2])
plot1

coefs = coef(lm(df_2$s_day ~ df_2$ltd))
plot2 = ggplot(df_2, aes(x = ltd, y = s_day)) +
  ggtitle("2018-2019 (normale)")+
  #xlab("Tv")+
  #ylab("Jours depuis le 1er Janvier")+
  ylab("Jours depuis le 1er Janvier 2019")+
  xlab("Temps de vie en 2018")+
  stat_bin2d(bins = 40) +
  scale_fill_gradient(low = "lightgreen", high = "red3", limits = c(1, 1000), trans = "log10")+
  geom_abline(intercept = coefs[1], slope = coefs[2])
plot2

coefs = coef(lm(df_3$s_day ~ df_3$ltd))
plot3 = ggplot(df_3, aes(x = ltd, y = s_day)) +
  ggtitle("2019-2020 (assec)")+
  #xlab("Tv")+
  #ylab("Jours depuis le 1er Janvier")+
  ylab("Jours depuis le 1er Janvier 2020")+
  xlab("Temps de vie en 2019")+
  stat_bin2d(bins = 40) +
  scale_fill_gradient(low = "lightgreen", high = "red3", limits = c(1, 1000), trans = "log10")+
  geom_abline(intercept = coefs[1], slope = coefs[2])
plot3

coefs = coef(lm(df_4$s_day ~ df_4$ltd))
plot4 = ggplot(df_4, aes(x = ltd, y = s_day)) +
  ggtitle("2020-2021 (normale)")+
  #xlab("Tv")+
  #ylab("Jours depuis le 1er Janvier")+
  ylab("Jours depuis le 1er Janvier 2021")+
  xlab("Temps de vie en 2020")+
  stat_bin2d(bins = 40) +
  scale_fill_gradient(low = "lightgreen", high = "red3", limits = c(1, 1000), trans = "log10")+
  geom_abline(intercept = coefs[1], slope = coefs[2])
plot4

coefs = coef(lm(df_5$s_day ~ df_5$ltd))
plot5 = ggplot(df_5, aes(x = ltd, y = s_day)) +
  ggtitle("2021-2022 (normale)")+
  #xlab("Tv")+
  #ylab("Jours depuis le 1er Janvier")+
  ylab("Jours depuis le 1er Janvier 2022")+
  xlab("Temps de vie en 2021")+
  stat_bin2d(bins = 40) +
  scale_fill_gradient(low = "lightgreen", high = "red3", limits = c(1, 1000), trans = "log10")+
  geom_abline(intercept = coefs[1], slope = coefs[2])
plot5

coefs = coef(lm(df_6$s_day ~ df_6$ltd))
plot6 = ggplot(df_6, aes(x = ltd, y = s_day)) +
  ggtitle("2022-2023 (assec)")+
  #xlab("Tv")+
  #ylab("Jours depuis le 1er Janvier")+
  ylab("Jours depuis le 1er Janvier 2023")+
  xlab("Temps de vie en 2022")+
  stat_bin2d(bins = 40) +
  scale_fill_gradient(low = "lightgreen", high = "red3", limits = c(1, 1000), trans = "log10")+
  geom_abline(intercept = coefs[1], slope = coefs[2])
plot6


grid.arrange(plot1, plot2, plot3, plot4, plot5, plot6, nrow=3)






######## resultats dans texte ########


plot(df_1$s_day, df_1$ltd, main = "1718")
cor.test(df_1$s_day, df_1$ltd)
lm1 = lm(df_1$ltd ~ df_1$s_day)
summary(lm1)
abline(lm1, col = "red")
legend("topright", paste("R squared = ",as.character(round(summary(lm1)[["r.squared"]],digits = 3))))



plot(df_2$s_day, df_2$ltd, main = "1819")
cor.test(df_2$s_day, df_2$ltd)
lm2 = lm(df_2$ltd ~ df_2$s_day)
summary(lm2)
abline(lm2, col = "red")
legend("topright", paste("R squared = ",as.character(round(summary(lm2)[["r.squared"]],digits = 3))))


plot(df_3$s_day, df_3$ltd, main = "1920")
cor.test(df_3$s_day, df_3$ltd)
lm3 = lm(df_3$ltd ~ df_3$s_day)
summary(lm3)
abline(lm3, col = "red")
legend("topright", paste("R squared = ",as.character(round(summary(lm3)[["r.squared"]],digits = 3))))


plot(df_4$s_day, df_4$ltd, main = "2021")
cor.test(df_4$s_day, df_4$ltd)
lm4 = lm(df_4$ltd ~ df_4$s_day)
summary(lm4)
abline(lm4, col = "red")
legend("topright", paste("R squared = ",as.character(round(summary(lm4)[["r.squared"]],digits = 3))))


plot(df_5$s_day, df_5$ltd, main = "2122")
cor.test(df_5$s_day, df_5$ltd)
lm5= lm(df_5$ltd ~ df_5$s_day)
summary(lm5)
abline(lm5, col = "red")
legend("topright", paste("R squared = ",as.character(round(summary(lm5)[["r.squared"]],digits = 3))))


plot(df_6$s_day, df_6$ltd, main = "2223")
cor.test(df_6$s_day, df_6$ltd)
lm6 = lm(df_6$ltd ~ df_6$s_day)
summary(lm6)
abline(lm6, col = "red")
legend("topright", paste("R squared = ",as.character(round(summary(lm6)[["r.squared"]],digits = 3))))


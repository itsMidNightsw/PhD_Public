# look at one image

library(sp)
library(rgdal)
library(RStoolbox)
library(hsdar)
library(maptools)
library(matrixStats)
library(Hmisc)
library(htmlwidgets)
library(ggplot2)
library(devtools)
library(raster)
library(terra)
library(RColorBrewer)

setwd("~/Thèse/General_use")
wd=getwd()
folder = "/media/adam/My Passport/DATA_THESE/DATA/2020"

imagery=dir(folder, pattern="V", full.names = F)  #nom des dossiers d'accés aux images brutes
imagery_fullnames=dir(folder, pattern="V", full.names = T)

date = "20200318"
dates=vector() #vecteur de dates de toutes les images de la série temporelle
for (i in 1:length(imagery))
{
  dates[i]=substr(imagery[i],12,19) #extraction de la date dans le nom de l'image
} #attention les dates ne sont pas par ordre chronologique à ce stade 


emprise=shapefile("/media/adam/My Passport/WD/_TEST/DATA/SHAPEFILE/emprise.shp") #shapefile emprise pour cropping
lagune_elargie=shapefile("/media/adam/My Passport/WD/_TEST/DATA/SHAPEFILE/lagune_elargie.shp") #shapefile lagune grand bagnas elargie avec bassins ouest
grand_bagnas=shapefile("/media/adam/My Passport/WD/_TEST/DATA/SHAPEFILE//grand_bagnas.shp") #shapefile lagune grand bagnas 
samp = shapefile("/media/adam/My Passport/WD/_TEST/DATA/SHAPEFILE/samp_eau_roseliere_herbier_2017.shp")#shapfile sampeling photointerpretation


###########


name=paste(date,"stack", sep="_")
DATA_path<-file.path(folder, imagery[which(dates==date)])

#' Concaténation des bandes et découpage de la zone
filenameB2=file.path(DATA_path,dir(DATA_path, pattern="FRE_B2"))
B2=raster(filenameB2[1])
filenameB3=file.path(DATA_path,dir(DATA_path, pattern="FRE_B3"))
B3=raster(filenameB3[1])
filenameB4=file.path(DATA_path,dir(DATA_path, pattern="FRE_B4"))
B4=raster(filenameB4[1])
filenameB8=file.path(DATA_path,dir(DATA_path, pattern="FRE_B8"))
B8=raster(filenameB8[1]) #sélection du premier B8 et pas B8A
stack<-addLayer(B2,B3,B4,B8)

data<-crop(stack,emprise)

names(data)<-c("bleu", "vert", "rouge", "pir")

#ndvi_mine = ((data$pir-data$rouge)+.00001)/((data$pir+data$rouge)+.00001)
#raster::plot(ndvi_mine)

summary(data)



#' Passer en réflectance
data[]<-getValues(data)/10000 #attention vérifier que réflectance jamais inférieure à 0 et pas de NA

summary(data)



ndvi=spectralIndices(data, blue = "bleu", green = "vert", red = "rouge", nir = "pir",
                       swir1 = NULL, swir2 = NULL, swir3 = NULL, scaleFactor = 1,
                       skipRefCheck = FALSE, indices = "NDVI",
                       index = NULL, coefs = list(L = 0.5, G = 2.5, L_evi = 1, C1 = 6, C2 = 7.5,
                                                   s = 1, swir2ccc = NULL, swir2coc = NULL))



ndvi_mine = (data@layers[[4]]-data@layers[[3]])/(data@layers[[4]]+data@layers[[3]])
raster::plot(data@layers[[3]])
raster::plot(data@layers[[4]])


ndvi_mine = (B8-B4)/(B8+B4)


raster::plot(ndvi_mine)

summary(ndvi)

data=addLayer(data,ndvi)



#writeRaster(data, filename=file.path("/media/adam/My Passport/DATA_THESE/DATA/2020/PRETRAIT",name), format="GTiff", overwrite=T)


################PLOTS

?raster::plot

raster::plotRGB(x = data, r = 3, g = 2, b = 1, stretch = "lin")

raster::plot(data@layers[[1]], colNA = "black")#blue
raster::plot(data@layers[[2]], colNA = "black")#green
raster::plot(data@layers[[3]], colNA = "black")#red
raster::plot(data@layers[[4]], colNA = "black")#NIR


raster::plot(ndvi,colNA = "black")



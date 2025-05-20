#differences_inter_annuelles
library(raster)
library(ggpubr)
library(ggplot2)
library(rstatix)
library(dunn.test)
library(FSA)
library(conover.test)
library(readr)
library(dplyr)
library(data.table)
############ LTD #############


#load data v6
first_dates_1718 = raster(paste(getwd(),"/Graphical_results/Qtt_physiques/rasters/s1718_raster.tif", sep = ""))
last_dates_1718 = raster(paste(getwd(),"/Graphical_results/Qtt_physiques/rasters/e1718_raster.tif", sep = ""))
ltd_1718 = raster(paste(getwd(),"/Graphical_results/Qtt_physiques/rasters/ltd_1718_raster.tif", sep = ""))

first_dates_1819 = raster(paste(getwd(),"/Graphical_results/Qtt_physiques/rasters/s1819_raster.tif", sep = ""))
last_dates_1819 = raster(paste(getwd(),"/Graphical_results/Qtt_physiques/rasters/e1819_raster.tif", sep = ""))
ltd_1819 = raster(paste(getwd(),"/Graphical_results/Qtt_physiques/rasters/ltd_1819_raster.tif", sep = ""))

first_dates_1920 = raster(paste(getwd(),"/Graphical_results/Qtt_physiques/rasters/s1920_raster.tif", sep = ""))
last_dates_1920 = raster(paste(getwd(),"/Graphical_results/Qtt_physiques/rasters/e1920_raster.tif", sep = ""))
ltd_1920 = raster(paste(getwd(),"/Graphical_results/Qtt_physiques/rasters/ltd_1920_raster.tif", sep = ""))

first_dates_2021 = raster(paste(getwd(),"/Graphical_results/Qtt_physiques/rasters/s2021_raster.tif", sep = ""))
last_dates_2021 = raster(paste(getwd(),"/Graphical_results/Qtt_physiques/rasters/e2021_raster.tif", sep = ""))
ltd_2021 = raster(paste(getwd(),"/Graphical_results/Qtt_physiques/rasters/ltd_2021_raster.tif", sep = ""))

first_dates_2122 = raster(paste(getwd(),"/Graphical_results/Qtt_physiques/rasters/s2122_raster.tif", sep = ""))
last_dates_2122 = raster(paste(getwd(),"/Graphical_results/Qtt_physiques/rasters/e2122_raster.tif", sep = ""))
ltd_2122 = raster(paste(getwd(),"/Graphical_results/Qtt_physiques/rasters/ltd_2122_raster.tif", sep = ""))

first_dates_22 = raster(paste(getwd(),"/Graphical_results/Qtt_physiques/rasters/s22_raster.tif", sep = ""))
last_dates_22 = raster(paste(getwd(),"/Graphical_results/Qtt_physiques/rasters/e22_raster.tif", sep = ""))
ltd_22 = raster(paste(getwd(),"/Graphical_results/Qtt_physiques/rasters/ltd_22_raster.tif", sep = ""))


############## tests comparaison moyenne ##########
ltd_stack = raster::stack(c(ltd_1718,ltd_1819,ltd_1920,ltd_2021,ltd_2122,ltd_22))

raster::boxplot(ltd_stack, names = c("2017-2018", "2018-2019","2019-2020","2020-2021","2021-2022","2022"), main = "Seagrass lifetimes in days from 2017 to 2022")


valuesstack = getValues(ltd_stack)

values1718 <- valuesstack[,1]
values1819 <- valuesstack[,2]
values1920 <- valuesstack[,3]
values2021 <- valuesstack[,4]
values2122 <- valuesstack[,5]
values22 <- valuesstack[,6]

###v4
x <- c(values1718, values1819, values1920, values2021, values2122, values22)
g <- factor(rep(1:6, c(25568,25568,25568,25568,25568,25568)),
            labels = c("2017-2018", "2018-2019","2019-2020","2020-2021","2021-2022","2022"))

kruskal.test(x, g)

dunn.test(x,g, kw = T, method = "bonferroni")
dunn.test(x,g, kw = T, method = "holm")

data1 <- data.frame(annee = rep(c("2017-2018", "2018-2019","2019-2020","2020-2021","2021-2022","2022"), each = 25568),
                   ltd = c(values1718, values1819, values1920, values2021, values2122, values22))

data1$annee = as.factor(data1$annee)


#v6
x <- c(values1718, values1819, values1920, values2021, values2122, values22)
g <- factor(rep(1:6, c(28908,28908,28908,28908,28908,28908)),
            labels = c("2017-2018", "2018-2019","2019-2020","2020-2021","2021-2022","2022"))

kruskal.test(x, g)

dunn.test(x,g, kw = T, method = "bonferroni")
dunn.test(x,g, kw = T, method = "holm")

data1 <- data.frame(annee = rep(c("2017-2018", "2018-2019","2019-2020","2020-2021","2021-2022","2022"), each = 28908),
                    ltd = c(values1718, values1819, values1920, values2021, values2122, values22))




data1$annee = as.factor(data1$annee)

summary(na.omit(data1))


p <- ggplot(data1, aes(x=annee, y=ltd)) + 
  geom_violin()
p = p + stat_summary(fun.y=mean, geom="point", shape=3, size=2, col = "red")
p

raster::boxplot(ltd_stack, names = c("2017-2018", "2018-2019","2019-2020","2020-2021","2021-2022","2022"), main = "SAV lifetimes in days from 2017 to 2022")




p = ggplot(data1, aes(x=annee, y=ltd)) + 
  xlab("Year")+
  ylab("SAV lifetimes")+
  geom_boxplot()
p





p2 = ggplot(data1, aes(x=annee, y=ltd)) +
  geom_violin(alpha = 0.5) +
  geom_dotplot(binaxis = "y",
               position = position_jitter(seed = 1, width = 0.2),
               stackdir = "center",
               dotsize = 0.1, method = "histodot",
               binwidth = 1/1000,
               stackratio=1.5)

p2
  
p2 = ggplot(data1, aes(x=annee, y=ltd)) +
  geom_violin(alpha = 0.5) +
  geom_dotplot(binaxis = "y",
               stackdir = "center",
               position = position_jitter(seed = 1, width = 0.2),
               dotsize = 0.1, method = "default",
               binwidth = 1/30)

p2




kruskal.test(ltd ~ annee, data = data1)

str(data1)
dunnTest(ltd ~ annee,
         data=data1,
         method="bonferroni")

conover.test(x,g, kw = T, method="bonferroni")   # DIFFERENCES SIGNIFICATIVES ENTRE TOUTES LES ANNEES


########### COR W/ clim vars ? ############
data_mf = read.csv2(file = "~/Thèse/Models_stats/data_meteofrance.csv", sep = ",")
data_mf$GDD = as.numeric(data_mf$GDD)
data_mf$TX = as.numeric(data_mf$TX)
data_mf$TN = as.numeric(data_mf$TN)

ltd_years = aggregate(ltd ~ annee, data = data1, mean)

gdd_years = aggregate(GDD ~ annee, data = data_mf, max)

max_years = aggregate(TX ~ annee, data = data_mf, mean)

min_years = aggregate(TN ~ annee, data = data_mf, mean)


data_table = data.frame(gdd_years$annee, ltd_years$ltd, gdd_years$GDD,max_years$TX, min_years$TN)
names(data_table) = c("annee","ltd","GDD", "TX", "TN")

res = cor.test(data_table$ltd, data_table$GDD)
res
plot(data_table$ltd~data_table$GDD)


res = cor.test(data_table$ltd, data_table$TX)
res
plot(data_table$ltd,data_table$TX)


res = cor.test(data_table$ltd, data_table$TN)
res
plot(data_table$ltd,data_table$TN)





#################################
########### BOOSTRAP ############
#################################


gdd_maxs = gdd_years$GDD

data1bs = na.omit(data1)
data1bs$annee = as.factor(data1bs$annee)



matrix_empty = matrix(nrow = 0, ncol = 2)
table_ltd_bs = data.frame(matrix_empty) 

colnames(table_ltd_bs) = c("annee", "ltd")

for (b in 1:1000) {
  


data1_sub = data1bs[sample(nrow(data1bs), 18000, replace = T), ]
table_ltd_bs_add = aggregate(ltd ~ annee, data = data1_sub, mean)

table_ltd_bs = rbind(table_ltd_bs, table_ltd_bs_add)

}



data_mf_bs = data_mf

table_mf_GDD_bs = data.frame(matrix_empty)
table_mf_TX_bs = data.frame(matrix_empty)
table_mf_TN_bs = data.frame(matrix_empty)


colnames(table_mf_GDD_bs) = c("annee", "GDDnorm")
colnames(table_mf_TX_bs) = c("annee", "TX")
colnames(table_mf_TN_bs) = c("annee", "TN")



for (b in 1:1000) {

data_mf_sub = data_mf_bs[sample(nrow(data_mf_bs), 210, replace = T), ]


table_mf_GDD_add = aggregate(GDD ~ annee, data = data_mf_sub, mean)
table_mf_GDD_add$GDDnorm = table_mf_GDD_add$GDD/gdd_maxs
table_mf_GDD_add = table_mf_GDD_add[,-2]

table_mf_GDD_bs = rbind(table_mf_GDD_bs, table_mf_GDD_add)


table_mf_TX_add = aggregate(TX ~ annee, data = data_mf_sub, mean)
table_mf_TX_bs = rbind(table_mf_TX_bs, table_mf_TX_add)


table_mf_TN_add = aggregate(TN ~ annee, data = data_mf_sub, mean)
table_mf_TN_bs = rbind(table_mf_TN_bs, table_mf_TN_add)

}


table_bs_full = data.frame(table_mf_GDD_bs$annee, table_ltd_bs$ltd, table_mf_GDD_bs$GDDnorm, table_mf_TX_bs$TX, table_mf_TN_bs$TN)
names(table_bs_full) = c("annee","ltd","GDDnorm", "TX", "TN")



########### COR ON BOOTSTRAP DATA ###############

res = cor.test(table_bs_full$ltd, table_bs_full$GDDnorm)
res
plot(table_bs_full$ltd,table_bs_full$GDDnorm, col = table_bs_full$annee, xlab = "Mean Lifetimes", ylab = "Normalized GDD")
abline(a = 0, b = res[["estimate"]])
legend("topright", inset=.02, title="Year",
       c("2017", "2018","2019","2020","2021","2022"), fill=table_bs_full$annee, horiz=TRUE, cex=0.8)


res = cor.test(table_bs_full$ltd, table_bs_full$TX)
res
plot(table_bs_full$ltd,table_bs_full$TX, col = table_bs_full$annee, xlab = "Mean Lifetimes", ylab = "Mean Max Temp")
legend("topright", inset=.02, title="Year",
       c("2017", "2018","2019","2020","2021","2022"), fill=table_bs_full$annee, horiz=TRUE, cex=0.8)


res = cor.test(table_bs_full$ltd, table_bs_full$TN)
res
plot(table_bs_full$ltd,table_bs_full$TN, col = table_bs_full$annee, xlab = "Mean Lifetimes", ylab = "Mean Min Temp")
legend("topright", inset=.02, title="Year",
       c("2017", "2018","2019","2020","2021","2022"), fill=table_bs_full$annee, horiz=TRUE, cex=0.8)





###########################################
########### BOOSTRAP BENCHMARK ############
###########################################


gdd_maxs = gdd_years$GDD

data1bs = na.omit(data1)
data1bs$annee = as.factor(data1bs$annee)

maxdata1bs = dim(data1bs)[1]
maxdatamf = dim(data_mf_bs)[1]


persdata1bs = round(seq(1, maxdata1bs, length.out = 100)) # pour avoir les 99 derniers pourcents
persdatamf = round(seq(1, maxdatamf, length.out = 100))

persdata1bs = persdata1bs[-c(1,2,3)]
persdatamf = persdatamf[-c(1,2,3)]

matrix_empty = matrix(nrow = 0, ncol = 2)
table_ltd_bs = data.frame(matrix_empty) 

colnames(table_ltd_bs) = c("annee", "ltd")


matrix_empty_bench = matrix(nrow = 0, ncol = 5)

benchm_t_gdd = data.frame(matrix_empty_bench) 
names(benchm_t_gdd) = c("i","cor","CI-low","CI-high", "p")

benchm_t_TX = data.frame(matrix_empty_bench) 
names(benchm_t_TX) = c("i","cor","CI-low","CI-high", "p")

benchm_t_TN = data.frame(matrix_empty_bench) 
names(benchm_t_TN) = c("i","cor","CI-low","CI-high", "p")

for (i in c(1:length(persdata1bs))) {
  

for (b in 1:2000) {
  
  
  
  data1_sub = data1bs[sample(nrow(data1bs), persdata1bs[i], replace = T), ]
  table_ltd_bs_add = aggregate(ltd ~ annee, data = data1_sub, mean)
  
  table_ltd_bs = rbind(table_ltd_bs, table_ltd_bs_add)
  
}


data_mf_bs = data_mf

table_mf_GDD_bs = data.frame(matrix_empty)
table_mf_TX_bs = data.frame(matrix_empty)
table_mf_TN_bs = data.frame(matrix_empty)


colnames(table_mf_GDD_bs) = c("annee", "GDDnorm")
colnames(table_mf_TX_bs) = c("annee", "TX")
colnames(table_mf_TN_bs) = c("annee", "TN")


for (b in 1:2000) {
  
  data_mf_sub = data_mf_bs[sample(nrow(data_mf_bs), persdatamf[i], replace = T), ]
  
  
  table_mf_GDD_add = aggregate(GDD ~ annee, data = data_mf_sub, mean)
  table_mf_GDD_add$GDDnorm = table_mf_GDD_add$GDD/gdd_maxs
  table_mf_GDD_add = table_mf_GDD_add[,-2]
  
  table_mf_GDD_bs = rbind(table_mf_GDD_bs, table_mf_GDD_add)
  
  
  table_mf_TX_add = aggregate(TX ~ annee, data = data_mf_sub, mean)
  table_mf_TX_bs = rbind(table_mf_TX_bs, table_mf_TX_add)
  
  
  table_mf_TN_add = aggregate(TN ~ annee, data = data_mf_sub, mean)
  table_mf_TN_bs = rbind(table_mf_TN_bs, table_mf_TN_add)
  
}

table_bs_full = data.frame(table_mf_GDD_bs$annee, table_ltd_bs$ltd, table_mf_GDD_bs$GDDnorm, table_mf_TX_bs$TX, table_mf_TN_bs$TN)
names(table_bs_full) = c("annee","ltd","GDDnorm", "TX", "TN")



res_gdd = cor.test(table_bs_full$ltd, table_bs_full$GDDnorm)
res_TX = cor.test(table_bs_full$ltd, table_bs_full$TX)
res_TN = cor.test(table_bs_full$ltd, table_bs_full$TN)



benchm_t_gdd_add = c(i+3,res_gdd[["estimate"]][[1]], res_gdd[["conf.int"]][[1]], res_gdd[["conf.int"]][[2]], res_gdd[["p.value"]]) # i+3 car les 3 premiers pourcents sont trop petits
benchm_t_TX_add = c(i+3,res_TX[["estimate"]][[1]], res_TX[["conf.int"]][[1]], res_TX[["conf.int"]][[2]], res_TX[["p.value"]])
benchm_t_TN_add = c(i+3,res_TN[["estimate"]][[1]], res_TN[["conf.int"]][[1]], res_TN[["conf.int"]][[2]], res_TN[["p.value"]])
  
  
benchm_t_gdd = rbind(benchm_t_gdd, benchm_t_gdd_add)
benchm_t_TX = rbind(benchm_t_TX, benchm_t_TX_add)
benchm_t_TN = rbind(benchm_t_TN, benchm_t_TN_add)



}

names(benchm_t_gdd) = c("perc","cor","CI-low","CI-high", "p")
names(benchm_t_TX) = c("perc","cor","CI-low","CI-high", "p")
names(benchm_t_TN) = c("perc","cor","CI-low","CI-high", "p")

benchm_t_gdd
benchm_t_TX
benchm_t_TN

######## write #######

#write.csv2(benchm_t_gdd, file = "~/Thèse/Resultats/Qtt_physiques/resultsV4/benchm_t_gdd_2000.csv")
#write.csv2(benchm_t_TX, file = "~/Thèse/Resultats/Qtt_physiques/resultsV4/benchm_t_TX_2000.csv")
#write.csv2(benchm_t_TN, file = "~/Thèse/Resultats/Qtt_physiques/resultsV4/benchm_t_TN_2000.csv")


############ PLOTS ###########

benchm_t_gdd_1000 = read.csv2("~/Thèse/Resultats/Qtt_physiques/resultsV4/benchm_t_gdd_1000.csv")
benchm_t_TX_1000 = read.csv2("~/Thèse/Resultats/Qtt_physiques/resultsV4/benchm_t_TX_1000.csv")
benchm_t_TN_1000 = read.csv2("~/Thèse/Resultats/Qtt_physiques/resultsV4/benchm_t_TN_1000.csv")


benchm_t_gdd_2000 = read.csv2("~/Thèse/Resultats/Qtt_physiques/resultsV4/benchm_t_gdd_2000.csv")
benchm_t_TX_2000 = read.csv2("~/Thèse/Resultats/Qtt_physiques/resultsV4/benchm_t_TX_2000.csv")
benchm_t_TN_2000 = read.csv2("~/Thèse/Resultats/Qtt_physiques/resultsV4/benchm_t_TN_2000.csv")


plot(benchm_t_gdd_1000$cor ~ benchm_t_gdd_1000$perc)
plot(benchm_t_gdd_2000$cor ~ benchm_t_gdd_2000$perc)

plot(benchm_t_TX_1000$cor ~ benchm_t_TX_1000$perc)
plot(benchm_t_TX_2000$cor ~ benchm_t_TX_2000$perc)

plot(benchm_t_TN_1000$cor ~ benchm_t_TN_1000$perc)
plot(benchm_t_TN_2000$cor ~ benchm_t_TN_2000$perc)





########## DIFFERENCES INTER ANNUELLES DONNEES TERRAIN + METEO FRANCE ###############

data_mf = read.csv2(file = "~/Thèse/Models_stats/data_meteofrance.csv", sep = ",")
data_mf$DATE = as.Date(data_mf$DATE)
data_mf$GDD = as.numeric(data_mf$GDD)
data_mf$TX = as.numeric(data_mf$TX)
data_mf$TN = as.numeric(data_mf$TN)
data_mf$TM = as.numeric(data_mf$TM)

plot(data_mf$TN)
plot(data_mf$TX)
plot(data_mf$TM)
plot(data_mf$GDD)


data_mf_summer = data_mf[data_mf$DATE >= "2017-06-01" & data_mf$DATE <= "2017-09-01" |
                        data_mf$DATE >= "2018-06-01" & data_mf$DATE <= "2018-09-01"  |
                        data_mf$DATE >= "2019-06-01" & data_mf$DATE <= "2019-09-01"  |
                        data_mf$DATE >= "2020-06-01" & data_mf$DATE <= "2020-09-01"  |
                        data_mf$DATE >= "2021-06-01" & data_mf$DATE <= "2021-09-01"  |
                        data_mf$DATE >= "2022-06-01" & data_mf$DATE <= "2022-09-01", ]



TM_years_summer = aggregate(TM ~ annee, data = data_mf_summer, mean)
TN_years_summer = aggregate(TN ~ annee, data = data_mf_summer, mean)
TX_years_summer = aggregate(TX ~ annee, data = data_mf_summer, mean)


#[c(1:5)]

res = cor.test(ltd_years$ltd, TM_years_summer$TM)
res
plot(ltd_years$ltd,TM_years_summer$TM, xlab = "Mean Lifetimes", ylab = "Mean Temp")


res = cor.test(ltd_years$ltd, TN_years_summer$TN)
res
plot(ltd_years$ltd,TN_years_summer$TN, xlab = "Mean Lifetimes", ylab = "Mean min Temp")

res = cor.test(ltd_years$ltd[c(1:5)], TN_years_summer$TN[c(1:5)])
res
plot(ltd_years$ltd[c(1:5)],TN_years_summer$TN[c(1:5)], xlab = "Mean Lifetimes", ylab = "Mean min Temp")
mod <- lm(TN_years_summer$TN[c(1:5)] ~ ltd_years$ltd[c(1:5)])
abline(mod, col = "red")
legend("topleft", legend = paste("pvalue =", round(res$p.value, digits = 4)))


res = cor.test(ltd_years$ltd, TX_years_summer$TX)
res
plot(ltd_years$ltd,TX_years_summer$TX, xlab = "Mean Lifetimes", ylab = "Mean max Temp")


##

data_mf_spring = data_mf[data_mf$DATE >= "2017-03-01" & data_mf$DATE <= "2017-06-01" |
                        data_mf$DATE >= "2018-03-01" & data_mf$DATE <= "2018-06-01"  |
                        data_mf$DATE >= "2019-03-01" & data_mf$DATE <= "2019-06-01"  |
                        data_mf$DATE >= "2020-03-01" & data_mf$DATE <= "2020-06-01"  |
                        data_mf$DATE >= "2021-03-01" & data_mf$DATE <= "2021-06-01"  |
                        data_mf$DATE >= "2022-03-01" & data_mf$DATE <= "2022-06-01", ]



TM_years_spring = aggregate(TM ~ annee, data = data_mf_spring, mean)
TN_years_spring = aggregate(TN ~ annee, data = data_mf_spring, mean)
TX_years_spring = aggregate(TX ~ annee, data = data_mf_spring, mean)




res = cor.test(ltd_years$ltd, TM_years_spring$TM)
res
plot(ltd_years$ltd,TM_years_spring$TM, xlab = "Mean Lifetimes", ylab = "Mean Temp")


res = cor.test(ltd_years$ltd, TN_years_spring$TN)
res
plot(ltd_years$ltd,TN_years_spring$TN, xlab = "Mean Lifetimes", ylab = "Mean min Temp")


res = cor.test(ltd_years$ltd, TX_years_spring$TX)
res
plot(ltd_years$ltd,TX_years_spring$TX, xlab = "Mean Lifetimes", ylab = "Mean max Temp")



############## Données Terrain #############


data_TB5 = read_csv2("~/Thèse/Terrain/data_TB5/HYDRO_2023-05-04_tb5.csv")
data_TB5 = data_TB5[,-c(1,4,7,9,10,14,15,16,17,18,19,20)]
data_TB5$`Date Releve` = as.Date(data_TB5$`Date Releve`, format = "%d/%m/%Y")
class(data_TB5$`Date Releve`)


data_TB5_2017_2022 = data_TB5[data_TB5$`Date Releve` > as.Date("2017-01-01"),]
data_TB5_2017_2022 = data_TB5_2017_2022[data_TB5_2017_2022$`Date Releve` < as.Date("2022-12-31"),]
colnames(data_TB5_2017_2022)
colnames(data_TB5_2017_2022) = c("Date", "Heure", "Conductivite", "Salinite", "Temperature", "Oxygene", "Saturation", "Ngf")
summary(data_TB5_2017_2022)


data_TB5_2017_2022$annee = data_TB5_2017_2022$Date
data_TB5_2017_2022$annee = format(data_TB5_2017_2022$annee, "%Y")


#subset dans les dates
data_TB5_summer = data_TB5_2017_2022[data_TB5_2017_2022$Date >= "2017-06-01" & data_TB5_2017_2022$Date <= "2017-09-01" |
                                       data_TB5_2017_2022$Date >= "2018-06-01" & data_TB5_2017_2022$Date <= "2018-09-01"  |
                                       data_TB5_2017_2022$Date >= "2019-06-01" & data_TB5_2017_2022$Date <= "2019-09-01"  |
                                       data_TB5_2017_2022$Date >= "2020-06-01" & data_TB5_2017_2022$Date <= "2020-09-01"  |
                                       data_TB5_2017_2022$Date >= "2021-06-01" & data_TB5_2017_2022$Date <= "2021-09-01"  |
                                       data_TB5_2017_2022$Date >= "2022-06-01" & data_TB5_2017_2022$Date <= "2022-09-01", ]


#moyennes par annee

#[c(1:5)]


temp_TB5_years = aggregate(Temperature ~ annee, data = data_TB5_summer, mean)
NGF_TB5_years = aggregate(Ngf ~ annee, data = data_TB5_summer, mean)
sal_TB5_years = aggregate(Salinite ~ annee, data = data_TB5_summer, mean)


res = cor.test(ltd_years$ltd, temp_TB5_years$Temperature)
res
plot(ltd_years$ltd,temp_TB5_years$Temperature, xlab = "Mean Lifetimes", ylab = "Mean Temp TB5")


res = cor.test(ltd_years$ltd, NGF_TB5_years$Ngf)
res
plot(ltd_years$ltd,NGF_TB5_years$Ngf, xlab = "Mean Lifetimes", ylab = "Mean ngf TB5")


res = cor.test(ltd_years$ltd, sal_TB5_years$Salinite)
res
plot(ltd_years$ltd,sal_TB5_years$Salinite, xlab = "Mean Lifetimes", ylab = "Mean salinity TB5")
mod <- lm(sal_TB5_years$Salinite ~ ltd_years$ltd)
abline(mod, col = "red")
legend("topright", legend = paste("pvalue =", round(res$p.value, digits = 4)))


### spring
#subset dans les dates
data_TB5_spring = data_TB5_2017_2022[data_TB5_2017_2022$Date >= "2017-03-01" & data_TB5_2017_2022$Date <= "2017-06-01" |
                                       data_TB5_2017_2022$Date >= "2018-03-01" & data_TB5_2017_2022$Date <= "2018-06-01"  |
                                       data_TB5_2017_2022$Date >= "2019-03-01" & data_TB5_2017_2022$Date <= "2019-06-01"  |
                                       data_TB5_2017_2022$Date >= "2020-03-01" & data_TB5_2017_2022$Date <= "2020-06-01"  |
                                       data_TB5_2017_2022$Date >= "2021-03-01" & data_TB5_2017_2022$Date <= "2021-06-01"  |
                                       data_TB5_2017_2022$Date >= "2022-03-01" & data_TB5_2017_2022$Date <= "2022-06-01", ]


#moyennes par annee

#[c(1:5)]


temp_TB5_years = aggregate(Temperature ~ annee, data = data_TB5_spring, mean)
NGF_TB5_years = aggregate(Ngf ~ annee, data = data_TB5_spring, mean)
sal_TB5_years = aggregate(Salinite ~ annee, data = data_TB5_spring, mean)


res = cor.test(ltd_years$ltd, temp_TB5_years$Temperature)
res
plot(ltd_years$ltd,temp_TB5_years$Temperature, xlab = "Mean Lifetimes", ylab = "Mean Temp TB5")


res = cor.test(ltd_years$ltd, NGF_TB5_years$Ngf)
res
plot(ltd_years$ltd,NGF_TB5_years$Ngf, xlab = "Mean Lifetimes", ylab = "Mean ngf TB5")
mod <- lm(NGF_TB5_years$Ngf ~ ltd_years$ltd)
abline(mod, col = "red")
legend("topleft", legend = paste("pvalue =", round(res$p.value, digits = 4)))


res = cor.test(ltd_years$ltd, sal_TB5_years$Salinite)
res
plot(ltd_years$ltd,sal_TB5_years$Salinite, xlab = "Mean Lifetimes", ylab = "Mean salinity TB5")
mod <- lm(sal_TB5_years$Salinite ~ ltd_years$ltd)
abline(mod, col = "red")
legend("topright", legend = paste("pvalue =", round(res$p.value, digits = 4)))


### saison reduite
#subset dans les dates
data_TB5_saisonr = data_TB5_2017_2022[data_TB5_2017_2022$Date >= "2017-03-01" & data_TB5_2017_2022$Date <= "2017-10-01" |
                                       data_TB5_2017_2022$Date >= "2018-03-01" & data_TB5_2017_2022$Date <= "2018-10-01"  |
                                       data_TB5_2017_2022$Date >= "2019-03-01" & data_TB5_2017_2022$Date <= "2019-10-01"  |
                                       data_TB5_2017_2022$Date >= "2020-03-01" & data_TB5_2017_2022$Date <= "2020-10-01"  |
                                       data_TB5_2017_2022$Date >= "2021-03-01" & data_TB5_2017_2022$Date <= "2021-10-01"  |
                                       data_TB5_2017_2022$Date >= "2022-03-01" & data_TB5_2017_2022$Date <= "2022-10-01", ]


#moyennes par annee

#[c(1:5)]


temp_TB5_years = aggregate(Temperature ~ annee, data = data_TB5_saisonr, mean)
NGF_TB5_years = aggregate(Ngf ~ annee, data = data_TB5_saisonr, mean)
sal_TB5_years = aggregate(Salinite ~ annee, data = data_TB5_saisonr, mean)


res = cor.test(ltd_years$ltd[c(1:5)], temp_TB5_years$Temperature[c(1:5)])
res
plot(ltd_years$ltd[c(1:5)],temp_TB5_years$Temperature[c(1:5)], xlab = "Mean Lifetimes", ylab = "Mean Temp TB5")


res = cor.test(ltd_years$ltd, NGF_TB5_years$Ngf)
res
plot(ltd_years$ltd,NGF_TB5_years$Ngf, xlab = "Mean Lifetimes", ylab = "Mean ngf TB5")


res = cor.test(ltd_years$ltd, sal_TB5_years$Salinite)
res
plot(ltd_years$ltd,sal_TB5_years$Salinite, xlab = "Mean Lifetimes", ylab = "Mean salinity TB5")



########### BIOVARS ##########
#[c(1:5)]

biovars = read.csv2(file = "~/Thèse/biovariables_annees/biovars_2017_2022.csv", sep = ";")
biovars = biovars[, -1]

#BIO5 : Max Temperature of Warmest Month 
res = cor.test(ltd_years$ltd, biovars$bio5)
res
plot(ltd_years$ltd, biovars$bio5, xlab = "Mean Lifetimes", ylab = "BIO5")


#BIO7 : Temperature Annual Range (BIO5-BIO6)
res = cor.test(ltd_years$ltd, biovars$bio7)
res
plot(ltd_years$ltd, biovars$bio7, xlab = "Mean Lifetimes", ylab = "BIO7")


#BIO10 : Mean Temperature of Warmest Quarter 
res = cor.test(ltd_years$ltd, biovars$bio10)
res
plot(ltd_years$ltd, biovars$bio10, xlab = "Mean Lifetimes", ylab = "BIO10")
mod <- lm(biovars$bio10 ~ ltd_years$ltd)
abline(mod, col = "red")
legend("topright", legend = paste("pvalue =", round(res$p.value, digits = 4)))


#BIO12 ? : Annual Precipitation 
res = cor.test(ltd_years$ltd, biovars$bio12)
res
plot(ltd_years$ltd, biovars$bio12, xlab = "Mean Lifetimes", ylab = "BIO12")


############### temps d'expo ngf ##########


data_TB5 = read_csv2("~/Thèse/Terrain/data_TB5/HYDRO_2023-05-04_tb5.csv")
data_TB5 = data_TB5[,-c(1,4,7,9,10,14,15,16,17,18,19,20)]
data_TB5$`Date Releve` = as.Date(data_TB5$`Date Releve`, format = "%d/%m/%Y")
class(data_TB5$`Date Releve`)


data_TB5_2017_2022 = data_TB5[data_TB5$`Date Releve` > as.Date("2017-01-01"),]
data_TB5_2017_2022 = data_TB5_2017_2022[data_TB5_2017_2022$`Date Releve` < as.Date("2022-12-31"),]
colnames(data_TB5_2017_2022)
colnames(data_TB5_2017_2022) = c("Date", "Heure", "Conductivite", "Salinite", "Temperature", "Oxygene", "Saturation", "Ngf")
summary(data_TB5_2017_2022)


data_TB5_2017_2022$annee = data_TB5_2017_2022$Date
data_TB5_2017_2022$annee = format(data_TB5_2017_2022$annee, "%Y")

data_TB5_2017_2022 = data_TB5_2017_2022[-180,] # outlayer


data = data_TB5_2017_2022[,c(1,8,9)]
data = na.omit(data)
rev_data_frame <- apply(data, 2, rev)

# converting the result to dataframe
data <- as.data.frame(rev_data_frame)
data$Date = as.Date(data$Date)
data$Ngf = as.numeric(data$Ngf)
data$annee = as.character(data$annee)


Thres = 0.30
#data$above_tresh = ifelse(data$Ngf>Thres, 1, 0)


data_summarised <-
  data %>% 
  filter(Ngf > Thres) %>%
  group_by(annee) %>%
  summarise(count = sum(Ngf>Thres),
            first_date = min(Date),
            last_date = max(Date))



data_summarised_20 <-
data %>% 
  # add id for different periods/events
  mutate(tmp_ngf = Ngf > 0.20, id = rleid(tmp_ngf)) %>% 
  # keep only periods with high temperature
  filter(tmp_ngf) %>%
  # for each period/event, get its duration
  group_by(id) %>%
  summarise(event_duration = difftime(last(Date), first(Date)))


data_summarised_20$event_duration = as.numeric(data_summarised_20$event_duration)

data_summarised_25 <-
  data %>% 
  # add id for different periods/events
  mutate(tmp_ngf = Ngf > 0.25, id = rleid(tmp_ngf)) %>% 
  # keep only periods with high temperature
  filter(tmp_ngf) %>%
  # for each period/event, get its duration
  group_by(id) %>%
  summarise(event_duration = difftime(last(Date), first(Date)))


data_summarised_25$event_duration = as.numeric(data_summarised_25$event_duration)

data_summarised_30 <-
  data %>% 
  # add id for different periods/events
  mutate(tmp_ngf = Ngf > 0.30, id = rleid(tmp_ngf)) %>% 
  # keep only periods with high temperature
  filter(tmp_ngf) %>%
  # for each period/event, get its duration
  group_by(id) %>%
  summarise(event_duration = difftime(last(Date), first(Date)))


data_summarised_30$event_duration = as.numeric(data_summarised_30$event_duration)

data_summarised_35 <-
  data %>% 
  # add id for different periods/events
  mutate(tmp_ngf = Ngf > 0.35, id = rleid(tmp_ngf)) %>% 
  # keep only periods with high temperature
  filter(tmp_ngf) %>%
  # for each period/event, get its duration
  group_by(id) %>%
  summarise(event_duration = difftime(last(Date), first(Date)))


data_summarised_35$event_duration = as.numeric(data_summarised_35$event_duration)

data_expo_ngf = data.frame(c("2017", "2018","2019","2020","2021","2022"), data_summarised_20$event_duration,
                           data_summarised_25$event_duration, data_summarised_30$event_duration, data_summarised_35$event_duration)




colnames(data_expo_ngf) = c("Annee", "ngf20", "ngf25", "ngf30", "ngf35")


p2 <- ggplot(na.omit(data_TB5_2017_2022[,c(1,8)]), aes(Date, Ngf))+
  xlab("Date") + ylab("Niveau NGF")+
  geom_point(color = "steelblue",shape = 2) + geom_line(color = "steelblue")#+
p2



##### Correlations ngf #####

res = cor.test(ltd_years$ltd, data_expo_ngf$ngf20)
res
plot(ltd_years$ltd, data_expo_ngf$ngf20, xlab = "Mean Lifetimes", ylab = "Days in ngf 20cm")


res = cor.test(ltd_years$ltd, data_expo_ngf$ngf25)
res
plot(ltd_years$ltd, data_expo_ngf$ngf25, xlab = "Mean Lifetimes", ylab = "Days in ngf 25cm")


res = cor.test(ltd_years$ltd, data_expo_ngf$ngf30)
res
plot(ltd_years$ltd, data_expo_ngf$ngf30, xlab = "Mean Lifetimes", ylab = "Days in ngf 30cm")
mod <- lm(data_expo_ngf$ngf30 ~ ltd_years$ltd)
abline(mod, col = "red")
legend("topleft", legend = paste("pvalue =", round(res$p.value, digits = 4)))

res = cor.test(ltd_years$ltd, data_expo_ngf$ngf35)
res
plot(ltd_years$ltd, data_expo_ngf$ngf35, xlab = "Mean Lifetimes", ylab = "Period duration at >35cm water depth")
mod <- lm(data_expo_ngf$ngf35 ~ ltd_years$ltd)
abline(mod, col = "red")
legend("topleft", legend = paste("pvalue =", round(res$p.value, digits = 4)))




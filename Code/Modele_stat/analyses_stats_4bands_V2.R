# analyses stats glmm etc on herbier_4bands_V2_2017-2022.csv


###### LIBRARIES ######

library(lme4)
library(lmerTest)
library(car)
library(stats)
library(ggplot2)
library(scatterplot3d)
library(rgl)
library(shiny)
library(plotly)
library(performance)
library(ggResidpanel)
library(see)
library(patchwork)
library(betareg)
library(glmmTMB)
library(sjPlot)

##### LOAD DATA #######

data = read.csv("~/Thèse/Models_stats/herbier_4bands_V2_2017-2022.csv")
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


data_no_2022 = subset(data,data$saison != "2022")

#write.csv(data, "~/Thèse/Models_stats/herbier_4bands_V2_2017-2022_2.csv")


######## Models #########

########## VIF ##########
mvif = glmer(surface ~ temperature + niveau_ngf + salinite + oxygene + (1|saison),
             data = data, family = "gaussian")
#ranova(mvif)
#Anova(mvif)
vif(mvif)

resid_panel(mvif, smoother = TRUE, qqbands = TRUE)
check_model(mvif)

par(mfrow = c(1,1))
check_model(mvif, check = "vif")
######### GLMER MODEL ###########

m8 = glmer(surface ~ temperature  + niveau_ngf + I(niveau_ngf^2) + salinite +I(salinite^2) + oxygene + (1|saison),
           data = data, family = "gaussian")

vif(m8)
ranova(m8)
Anova(m8)
summary(m8)


resid_panel(m8, smoother = TRUE, qqbands = TRUE)
check_model(m8)

hist(residuals(m8))
shapiro.test(residuals(m8))

########### BETA FIT #############

m9noalea = glmmTMB(surface_p ~ temperature  + niveau_ngf + salinite + oxygene,
             data, family=beta_family(link = "logit"))

m9 = glmmTMB(surface_p ~ temperature  + niveau_ngf + salinite + oxygene + (1|saison),
              data, family=beta_family(link = "logit"))

hist(residuals(m9), breaks = 10, main = "surface_p ~ temperature  + niveau_ngf + salinite + oxygene + (1|saison)")

check_model(m9)


summary(m9)
ranef(m9)
Anova(m9, type = 2)

anova(m9, m9noalea) # ok pour effet alea


fitv = fitted(m9)
resv = residuals(m9)
resv2 = residuals(m9, type = "working")


hist(resv, breaks = 10)
hist(resv2)

summary(m9)



plot(resv ~ fitv, ylim = c(-1,1), main = "surface_p ~ temperature + niveau_ngf + 
     salinite + oxygene + (1|saison)")



### comparaison de modeles ###

m9temp2 = glmmTMB(surface_p ~ temperature + I(temperature^2) + niveau_ngf + salinite + oxygene + (1|saison),
                  data, family=beta_family(link = "logit"), na.action = na.omit)

m9ngf2 = glmmTMB(surface_p ~ temperature + niveau_ngf + I(niveau_ngf^2) + salinite + oxygene + (1|saison),
                 data, family=beta_family(link = "logit"), na.action = na.omit)

m9sal2 = glmmTMB(surface_p ~ temperature + niveau_ngf + salinite + I(salinite^2) + oxygene + (1|saison),
                 data, family=beta_family(link = "logit"), na.action = na.omit)

m9temp2_ngf2 = glmmTMB(surface_p ~ temperature + I(temperature^2) + niveau_ngf + I(niveau_ngf^2) + salinite + oxygene + (1|saison),
                       data, family=beta_family(link = "logit"), na.action = na.omit)

m9temp2_sal2 = glmmTMB(surface_p ~ temperature + I(temperature^2) + niveau_ngf + salinite +I(salinite^2) + oxygene + (1|saison),
                       data, family=beta_family(link = "logit"), na.action = na.omit)

m9ngf2_sal2 = glmmTMB(surface_p ~ temperature + niveau_ngf + I(niveau_ngf^2) + salinite +I(salinite^2) + oxygene + (1|saison),
                      data, family=beta_family(link = "logit"), na.action = na.omit)


mfull = glmmTMB(surface_p ~ temperature + I(temperature^2) + niveau_ngf + I(niveau_ngf^2) + salinite +I(salinite^2) + oxygene + (1|saison),
                data, family=beta_family(link = "logit"), na.action = na.omit)



anova(mfull, m9ngf2_sal2, m9temp2_sal2, m9temp2_ngf2, m9sal2, m9ngf2, m9temp2 ,m9)
?anova()


anova(m9, m9sal2)
anova(m9sal2, m9ngf2_sal2)
anova(m9sal2, mfull)



check_model(m9ngf2_sal2)
hist(residuals(m9ngf2_sal2), breaks = 10, main = "m9ngf2_sal2")

Anova(m9ngf2_sal2)
summary(m9ngf2_sal2)
fitv = fitted(m9ngf2_sal2)
resv = residuals(m9ngf2_sal2)

plot(resv ~ fitv, ylim = c(-1,1), main = "m9ngf2_sal2")

shapiro.test(residuals(m9ngf2_sal2))
###########

check_model(m9sal2)
hist(residuals(m9sal2), breaks = 10, main = "m9sal2")
shapiro.test(residuals(m9sal2))


Anova(m9sal2)
summary(m9sal2)
fitv = fitted(m9sal2)
resv = residuals(m9sal2)

plot(resv ~ fitv, ylim = c(-1,1), main = "m9sal2")

anova(m9ngf2_sal2, m9sal2)


########### marginal effect of the salinity (and other variables)



plot_model(m9ngf2_sal2, type = "pred", terms="salinite [all]", title = "Predicted surface for salinity values in M1",
           axis.title = c("Salinity (g/l)","Seagrass area (%)"))


plot_model(m9sal2, type = "pred", terms="salinite [all]", title = "Predicted surface for salinity values in M2",
           axis.title = c("Salinity (g/l)","Seagrass area (%)"))




plot_model(m9ngf2_sal2, type = "pred", title = "")
plot_model(m9sal2, type = "pred", title = "")







########### effet independant des variables

m9sal2_a = glmmTMB(surface_p ~ temperature + (1|saison),
                   data, family=beta_family(link = "logit"), na.action = na.omit)

summary(m9sal2_a)

m9sal2_b = glmmTMB(surface_p ~ niveau_ngf + (1|saison),
                   data, family=beta_family(link = "logit"), na.action = na.omit)

summary(m9sal2_b)

m9sal2_b2 = glmmTMB(surface_p ~ niveau_ngf + I(niveau_ngf^2) + (1|saison),
                   data, family=beta_family(link = "logit"), na.action = na.omit)

summary(m9sal2_b2)

m9sal2_c = glmmTMB(surface_p ~ salinite + I(salinite^2) + (1|saison),
                   data, family=beta_family(link = "logit"), na.action = na.omit)

summary(m9sal2_c)

m9sal2_d = glmmTMB(surface_p ~ oxygene + (1|saison),
                   data, family=beta_family(link = "logit"), na.action = na.omit)

summary(m9sal2_d)


###########

check_model(mfull)

hist(residuals(mfull), breaks = 10, main = "mfull")


Anova(mfull)
fitv = fitted(mfull)
resv = residuals(mfull)

plot(resv ~ fitv, ylim = c(-1,1), main = "mfull")

# re check alea
m9ngf2_sal2_noalea = glmmTMB(surface_p ~ temperature + niveau_ngf + I(niveau_ngf^2) + salinite + I(salinite^2) + oxygene,
                                    data, family=beta_family(link = "logit"), na.action = na.omit)

anova(m9ngf2_sal2, m9ngf2_sal2_noalea)


m9sal2_noalea = glmmTMB(surface_p ~ temperature + niveau_ngf + salinite + I(salinite^2) + oxygene,
                             data, family=beta_family(link = "logit"), na.action = na.omit)

anova(m9sal2, m9sal2_noalea)




mfullnoalea = glmmTMB(surface_p ~ temperature + I(temperature^2) + niveau_ngf + I(niveau_ngf^2) + salinite +I(salinite^2) + oxygene,
                      data, family=beta_family(link = "logit"), na.action = na.omit)

anova(mfull, mfullnoalea)


m_test_plot = glmmTMB(surface_p ~ temperature + niveau_ngf + I(niveau_ngf^2) + salinite +I(salinite^2) + (1|saison),
                data, family=beta_family(link = "logit"), na.action = na.omit)





###### PLOTS ######
data$surface = log(data$surface)

hist(data$surface)
hist(log(data$surface))
hist(sqrt(data$surface))
hist(tdata)

hist(data$niveau_ngf)
hist(data$temperature)
shapiro.test(data$temperature)
hist(data$salinite)
hist(log10(data$salinite))
hist(data$oxygene)

plot(data$salinite)

plot(data[data$saison=="2017-2018",c(2,3,4,5,7)])
plot(data[data$saison=="2018-2019",c(2,3,4,5,7)])
plot(data[data$saison=="2019-2020",c(2,3,4,5,7)])
plot(data[data$saison=="2020-2021",c(2,3,4,5,7)])
plot(data[data$saison=="2021-2022",c(2,3,4,5,7)])
plot(data[data$saison=="2022",c(2,3,4,5,7)])



plot(surface_p ~ temperature, data = data)

plot(surface_p ~ salinite, data = data)

coefficients(m9ngf2_sal2)
summary(m9ngf2_sal2)
data$salinite

salValues <- seq(0, 35, 0.1)
#tempValues <- seq(0, 35, 0.1)


surfacepredict = -0.008617587*(salValues)^2 + 0.2744703*(salValues)
surfacepredictdata = -0.008617587*(data$salinite)^2 + 0.2744703*(data$salinite) - 4.774258

lines(cbind(data$salinite, surfacepredictdata))
plot(cbind(data$salinite, surfacepredictdata))
#surfacepredict2 = 0.2195793*(tempValues) + (-0.008617587)*(salValues)^2 + 0.2744703*(salValues)




testmod = lm(data$surface_p ~ data$salinite + I(data$salinite^2))
plot(surface_p ~ salinite, data = data)
pred = predict(testmod,list(sal=salValues, sal2=salValues^2))

points(cbind(data$salinite,pred), pch = 2)


plot(surfacepredict ~ salValues)
#plot(surfacepredict2 ~ salValues)

modelpredict <- predict(m9ngf2_sal2)
modelpredict <- predict(m9ngf2_sal2, list(sal=salValues, sal2=salValues^2))

#create scatterplot of original data values
plot(data$salinite, data$surface_p, pch=16)
#add predicted lines based on quadratic regression model
lines(salValues, modelpredict, col='blue')


p_temperature <- ggplot(data = data, aes(y = surface, x = temperature, colour = saison, group = saison))+
  xlab("Temperature") + ylab("Surface d'herbier (Ha)")+
  geom_point() #+
  #geom_line(aes(y = fitted(m8)))
#scale_linetype_manual(values=c("2021-06-14"="dashed", "2023-06-01"="dashed", "2023-06-26"="dashed", "Mean"="solid"))+
#scale_color_manual(values=c("2021-06-14"="purple", "2023-06-01"="green", "2023-06-26"="blue", "Mean"="black"))

plot(p_temperature)


p_niveauNGF <- ggplot(data = data, aes(y = surface, x = niveau_ngf, colour = saison, group = saison))+
  xlab("Niveau NGF") + ylab("Surface d'herbier (Ha)")+
  geom_point() #+
#scale_linetype_manual(values=c("2021-06-14"="dashed", "2023-06-01"="dashed", "2023-06-26"="dashed", "Mean"="solid"))+
#scale_color_manual(values=c("2021-06-14"="purple", "2023-06-01"="green", "2023-06-26"="blue", "Mean"="black"))

plot(p_niveauNGF)


p_salinite <- ggplot(data = data, aes(y = surface, x = salinite, colour = saison, group = saison))+
  xlab("Salinite") + ylab("Surface d'herbier (Ha)")+
  geom_point() #+
#scale_linetype_manual(values=c("2021-06-14"="dashed", "2023-06-01"="dashed", "2023-06-26"="dashed", "Mean"="solid"))+
#scale_color_manual(values=c("2021-06-14"="purple", "2023-06-01"="green", "2023-06-26"="blue", "Mean"="black"))

plot(p_salinite)


p_oxygene <- ggplot(data = data, aes(y = surface, x = oxygene, colour = saison, group = saison))+
  xlab("Oxygene") + ylab("Surface d'herbier (Ha)")+
  geom_point() #+
#scale_linetype_manual(values=c("2021-06-14"="dashed", "2023-06-01"="dashed", "2023-06-26"="dashed", "Mean"="solid"))+
#scale_color_manual(values=c("2021-06-14"="purple", "2023-06-01"="green", "2023-06-26"="blue", "Mean"="black"))

plot(p_oxygene)

annees = c(2017:2022)
saisons = c("2017-2018","2018-2019","2019-2020","2020-2021","2021-2022","2022")

########### TEMPERATURES ##########

for (a in annees) {
  p <- ggplot(data = data[data$annee == a,], aes(y = surface, x = temperature, colour = annee, group = annee))+
    ggtitle(print(as.character(a)))+
    xlab("Temperature") + ylab("Surface d'herbier (Ha)")+ xlim(c(0,31))+
    geom_point() #+
  #scale_linetype_manual(values=c("2021-06-14"="dashed", "2023-06-01"="dashed", "2023-06-26"="dashed", "Mean"="solid"))+
  #scale_color_manual(values=c("2021-06-14"="purple", "2023-06-01"="green", "2023-06-26"="blue", "Mean"="black"))
  
  plot(p)
}

for (s in saisons) {
  p <- ggplot(data = data[data$saison == s,], aes(y = surface, x = temperature, colour = saison, group = saison))+
    ggtitle(print(as.character(s)))+
    xlab("Temperature") + ylab("Surface d'herbier (Ha)")+ xlim(c(0,31))+
    geom_point() #+
  #scale_linetype_manual(values=c("2021-06-14"="dashed", "2023-06-01"="dashed", "2023-06-26"="dashed", "Mean"="solid"))+
  #scale_color_manual(values=c("2021-06-14"="purple", "2023-06-01"="green", "2023-06-26"="blue", "Mean"="black"))
  
  plot(p)
}

############ NIVEAU NGF #############

for (a in annees) {
  p <- ggplot(data = data[data$annee == a,], aes(y = surface, x = niveau_ngf, colour = annee, group = annee))+
    ggtitle(print(as.character(a)))+
    xlab("Niveau NGF") + ylab("Surface d'herbier (Ha)")+ xlim(c(-.3,.7))+
    geom_point() #+
  #scale_linetype_manual(values=c("2021-06-14"="dashed", "2023-06-01"="dashed", "2023-06-26"="dashed", "Mean"="solid"))+
  #scale_color_manual(values=c("2021-06-14"="purple", "2023-06-01"="green", "2023-06-26"="blue", "Mean"="black"))
  
  plot(p)
}

for (s in saisons) {
  p <- ggplot(data = data[data$saison == s,], aes(y = surface, x = niveau_ngf, colour = saison, group = saison))+
    ggtitle(print(as.character(s)))+
    xlab("Niveau NGF") + ylab("Surface d'herbier (Ha)")+ xlim(c(-.3,.7))+
    geom_point() #+
  #scale_linetype_manual(values=c("2021-06-14"="dashed", "2023-06-01"="dashed", "2023-06-26"="dashed", "Mean"="solid"))+
  #scale_color_manual(values=c("2021-06-14"="purple", "2023-06-01"="green", "2023-06-26"="blue", "Mean"="black"))
  
  plot(p)
}



############ SALINITE #############

for (a in annees) {
  p <- ggplot(data = data[data$annee == a,], aes(y = surface, x = salinite, colour = annee, group = annee))+
    ggtitle(print(as.character(a)))+
    xlab("Salinite") + ylab("Surface d'herbier (Ha)")+ xlim(c(0,35))+
    geom_point() #+
  #scale_linetype_manual(values=c("2021-06-14"="dashed", "2023-06-01"="dashed", "2023-06-26"="dashed", "Mean"="solid"))+
  #scale_color_manual(values=c("2021-06-14"="purple", "2023-06-01"="green", "2023-06-26"="blue", "Mean"="black"))
  
  plot(p)
}

for (s in saisons) {
  p <- ggplot(data = data[data$saison == s,], aes(y = surface, x = salinite, colour = saison, group = saison))+
    ggtitle(print(as.character(s)))+
    xlab("Salinite") + ylab("Surface d'herbier (Ha)")+ xlim(c(0,35))+
    geom_point() #+
  #scale_linetype_manual(values=c("2021-06-14"="dashed", "2023-06-01"="dashed", "2023-06-26"="dashed", "Mean"="solid"))+
  #scale_color_manual(values=c("2021-06-14"="purple", "2023-06-01"="green", "2023-06-26"="blue", "Mean"="black"))
  
  plot(p)
}

############ super plot en 3d #############

ui <-  fluidPage(
  actionButton('RUN', 'Run'),
  plotlyOutput("plot3D")
)
server = function(input, output) {
  output$plot3D <- renderPlotly({
    req(input$RUN)
    isolate({
      plot_ly(data, x = ~surface, y = ~niveau_ngf, z = ~temperature, color = ~temperature, type = "scatter3d", mode = "markers", size = 2)
    })
  })
}
runApp(shinyApp(ui, server))



############# FITS ##############

### BETA ###

nd = data[,c(3,4,5,7,10)]
nd$saison = as.character(nd$saison)
data$predbeta = predict(m9ngf2_sal2, newdata = nd, type = "response")

subdata = data.frame(data$date, data$predbeta)
colnames(subdata) = c("date","predbeta")
subdata = na.omit(subdata)

summary(subdata)

p <- ggplot(data, aes(date,predbeta)) +
  ylab("Seagrass area")+
  geom_point(data=data, mapping = aes(date,predbeta, colour = "prediction"), color = "cyan3")+
  geom_smooth(method="auto", se=F, fullrange=FALSE, span = .1, aes(colour = "prediction"), color = "cyan3") +
  geom_point(data = data, mapping = aes(date,surface_p), color = "darkgreen")+
  geom_path(aes(date,surface_p), color = "darkgreen")+
  ggtitle("Model vs true data")
plot(p)


p <- ggplot(data, aes(date,predbeta)) +
  geom_point(data=subdata, mapping = aes(date,predbeta, colour = "prediction"), color = "cyan3",na.rm=TRUE)+
  geom_path(aes(date,predbeta), color = "cyan3", na.rm=TRUE) +
  geom_point(data = data, mapping = aes(date,surface_p), color = "darkgreen")+
  geom_path(aes(date,surface_p), color = "darkgreen")+
  ggtitle("Model vs true data")
plot(p)



######### JUST TO CHECK STUFF ########
p <- ggplot(data = data, aes(y = salinite, x = niveau_ngf, colour = saison, group = saison))+
  xlab("Niveau ngf") + ylab("Salinite")+
  geom_point() #+
#scale_linetype_manual(values=c("2021-06-14"="dashed", "2023-06-01"="dashed", "2023-06-26"="dashed", "Mean"="solid"))+
#scale_color_manual(values=c("2021-06-14"="purple", "2023-06-01"="green", "2023-06-26"="blue", "Mean"="black"))

plot(p)

p <- ggplot(data = data, aes(y = salinite, x = temperature, colour = saison, group = saison))+
  xlab("temperature") + ylab("Salinite")+
  geom_point() #+
#scale_linetype_manual(values=c("2021-06-14"="dashed", "2023-06-01"="dashed", "2023-06-26"="dashed", "Mean"="solid"))+
#scale_color_manual(values=c("2021-06-14"="purple", "2023-06-01"="green", "2023-06-26"="blue", "Mean"="black"))

plot(p)

p <- ggplot(data = data, aes(y = niveau_ngf, x = temperature,color = saison, pch = saison, group = saison))+
  xlab("temperature") + ylab("ngf")+
  geom_point() #+
#scale_linetype_manual(values=c("2021-06-14"="dashed", "2023-06-01"="dashed", "2023-06-26"="dashed", "Mean"="solid"))+
#scale_color_manual(values=c("2021-06-14"="purple", "2023-06-01"="green", "2023-06-26"="blue", "Mean"="black"))

plot(p)

hist(data$surface)
hist(data$surface_binom)
plot(data$surface)
plot(data$surface_binom)


############ plot for paper ##############

p <- ggplot(data, aes(date,predbeta)) +
  ylab("SAV area (%)")+
  xlab("Date")+
  ggtitle("Model vs true data")+
  geom_point(data=data, mapping = aes(date,predbeta, colour = "prediction"), color = "cyan3")+
  geom_smooth(method="auto", se=F, fullrange=FALSE, span = .1, aes(colour = "prediction"), color = "cyan3") +
  
  geom_point(data = data, mapping = aes(date,surface_p), color = "darkgreen")+
  geom_path(aes(date,surface_p), color = "darkgreen")+
  
  scale_x_date(date_labels = "%m/%y", date_breaks = "4 months")+
  
  theme(legend.position = "topright")
plot(p)


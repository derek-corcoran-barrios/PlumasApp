library(latticeExtra)
library(dplyr)
library(knitr)
library(unmarked)
library(lubridate)
library(stargazer)
library(xtable)
library(MuMIn)
library(ggplot2)
library(RCurl)
library(foreign)
library(caret)

sampling.cov2 <- read.csv("~/Documents/PlumasBatsApp/sampling.cov2.csv")
sampling.cov2<- sampling.cov2[,-1]
sampling.cov2<- data.frame(sampling.cov2)
preprocov <-preProcess(sampling.cov2[,1:8], method = c("center","scale"))
sampling.cov2<-predict(preprocov, sampling.cov2[,1:8])
Dailycov3 <- read.csv("~/Documents/PlumasBatsApp/Dailycov3.csv")
Dailycov3 <- Dailycov3 [,-1]
preproDaily <-preProcess(Dailycov3[,], method = c("knnImpute"))
Dailycov3<-predict(preproDaily, Dailycov3)
Julian<-Dailycov3[,1:3]
Max.hum<-Dailycov3[,4:6]
Max.temp<-Dailycov3[,7:9]
Mean.hum<-Dailycov3[,10:12]
Mean.temp<-Dailycov3[,13:15]
Min.hum<-Dailycov3[,16:18]
Min.temp<-Dailycov3[,19:21]
Sd.hum<-Dailycov3[,22:24]
Sd.temp<-Dailycov3[,25:27]
Dailycov3<- list(Julian,Max.hum, Max.temp, Mean.hum,Mean.temp, Min.hum, Min.temp, Sd.hum, Sd.temp)
names(Dailycov3) <- c("Julian", "Maxhum","Maxtemp","Meanhum", "Meantemp","Minhum","Mintemp","sdhum","sdtemp")

BatOccu <- read.csv("~/Documents/PlumasBatsApp/BatOccu.csv")

library(unmarked)

BatOccuMyYu<-BatOccu[,2:4]
SimOccuMyYu2<-unmarkedFrameOccu(y = BatOccuMyYu, siteCovs =sampling.cov2, obsCovs=Dailycov3)
model.Occu1.My.Yu2 <- occu(~ I(Julian^2) + Meanhum + Meantemp + sdhum + sdtemp ~  Burn.intensity.Canopy + I(Burn.intensity.Canopy^2) + Distance.to.water + I(Distance.to.water^2) + Distance.to.road + I(Distance.to.road^2) + Existing.vegetation +I(Existing.vegetation^2) + Altitude + I(Altitude^2),SimOccuMyYu2)
select.My.Yu2 <- dredge(model.Occu1.My.Yu2, rank = "AICc")
best2.My.Yu2<-get.models(select.My.Yu2, 1)[[1]]

library(texreg)
screenreg(extract(subset(select.My.Yu2, delta <= 0.5, recalc.weights = FALSE)), file="myyu.txt")

##California bat (*Myotis californicus*)

#N=49
BatOccuMyCa<-BatOccu[,5:7]

SimOccuMyCa2<-unmarkedFrameOccu(y = BatOccuMyCa, siteCovs =sampling.cov2, obsCovs=Dailycov3)

model.Occu1.My.Ca2 <- occu(~ I(Julian^2) + Meanhum + Meantemp + sdhum + sdtemp ~  Burn.intensity.Canopy + I(Burn.intensity.Canopy^2) + Distance.to.water + I(Distance.to.water^2) + Distance.to.road + I(Distance.to.road^2) + Existing.vegetation +I(Existing.vegetation^2) + Altitude + I(Altitude^2), SimOccuMyCa2)

select.My.Ca2 <- dredge(model.Occu1.My.Ca2, rank = "AICc")

best2.My.Ca2<-get.models(select.My.Ca2, 1)[[1]]
library(texreg)
screenreg(extract(subset(select.My.Ca2, delta <= 1, recalc.weights = FALSE)), file="myca.txt")


##Western Small Footed Myotis (*Myotis ciliolabrum*)

BatOccuMyCi<-BatOccu[,8:10]
SimOccuMyCi2<-unmarkedFrameOccu(y = BatOccuMyCi, siteCovs =sampling.cov2, obsCovs=Dailycov3)
model.Occu1.My.Ci2 <- occu(~ I(Julian^2) + Meanhum + Meantemp + sdhum + sdtemp ~  Burn.intensity.Canopy + I(Burn.intensity.Canopy^2) + Distance.to.water + I(Distance.to.water^2) + Distance.to.road + I(Distance.to.road^2) + Existing.vegetation +I(Existing.vegetation^2) + Altitude + I(Altitude^2), SimOccuMyCi2)

select.My.Ci2 <- dredge(model.Occu1.My.Ci2, rank = "AICc")

best2.My.Ci2<-get.models(select.My.Ci2, 1)[[1]]

screenreg(extract(subset(select.My.Ci2, delta <= 1, recalc.weights = FALSE)), file= "myci.txt")


##Hairy-winged bat (*Myotis volans*)

library(unmarked)

BatOccuMyVo<-BatOccu[,11:13]

SimOccuMyVo2<-unmarkedFrameOccu(y = BatOccuMyVo, siteCovs =sampling.cov2, obsCovs=Dailycov3)
model.Occu1.My.Vo2 <- occu(~ I(Julian^2) + Meanhum + Meantemp + sdhum + sdtemp ~  Burn.intensity.Canopy + I(Burn.intensity.Canopy^2) + Distance.to.water + I(Distance.to.water^2) + Distance.to.road + I(Distance.to.road^2) + Existing.vegetation +I(Existing.vegetation^2) + Altitude + I(Altitude^2),SimOccuMyVo2)

select.My.Vo2 <- dredge(model.Occu1.My.Vo2, rank = "AICc")
best2.My.Vo2<-get.models(select.My.Vo2, 1)[[1]]

library(texreg)
screenreg(extract(subset(select.My.Vo2, delta <= 2, recalc.weights = FALSE)), file= "myvo.txt")

##Little Brown bat (*Myotis lucifugus*)

BatOccuMyLu<-BatOccu[,14:16]

SimOccuMyLu2<-unmarkedFrameOccu(y = BatOccuMyLu, siteCovs =sampling.cov2, obsCovs=Dailycov3)
model.Occu1.My.Lu2 <- occu(~ I(Julian^2) + Meanhum + Meantemp + sdhum + sdtemp ~  Burn.intensity.Canopy + I(Burn.intensity.Canopy^2) + Distance.to.water + I(Distance.to.water^2) + Distance.to.road + I(Distance.to.road^2) + Existing.vegetation +I(Existing.vegetation^2) + Altitude + I(Altitude^2), SimOccuMyLu2)
select.My.Lu2 <- dredge(model.Occu1.My.Lu2, rank = "AICc")
best2.My.Lu2<-get.models(select.My.Lu2, 1)[[1]]
library(texreg)
screenreg(extract(subset(select.My.Lu2, delta <= 2, recalc.weights = FALSE)), file= "mylu.txt")


##Western Red Bat (*Lasiurus blossevillii*)

#N=49

SimOccuLaBl2<-unmarkedFrameOccu(y = BatOccuLaBl, siteCovs =sampling.cov2, obsCovs=Dailycov3)
model.Occu1.La.Bl2 <- occu(~ I(Julian^2) + Meanhum + Meantemp + sdhum + sdtemp ~  Burn.intensity.Canopy + I(Burn.intensity.Canopy^2) + Distance.to.water + I(Distance.to.water^2) + Distance.to.road + I(Distance.to.road^2) + Existing.vegetation +I(Existing.vegetation^2) + Altitude + I(Altitude^2) ,SimOccuLaBl2)
select.La.Bl2 <- dredge(model.Occu1.La.Bl2, rank = "AICc")
best2.La.Bl2<-get.models(select.La.Bl2, 1)[[1]]
screenreg(extract(subset(select.La.Bl2, delta <= 2, recalc.weights = FALSE)), file= "labl.txt")

## Long-eared Bat (*Myotis evotis*)

BatOccuMyEv<-BatOccu[,23:25]
#N=49
SimOccuMyEv2<-unmarkedFrameOccu(y = BatOccuMyEv, siteCovs =sampling.cov2, obsCovs=Dailycov3)
model.Occu1.My.Ev2 <- occu(~ I(Julian^2) + Meanhum + Meantemp + sdhum + sdtemp ~  Burn.intensity.Canopy + I(Burn.intensity.Canopy^2) + Distance.to.water + I(Distance.to.water^2) + Distance.to.road + I(Distance.to.road^2) + Existing.vegetation +I(Existing.vegetation^2) + Altitude + I(Altitude^2), SimOccuMyEv2)
select.My.Ev2 <- dredge(model.Occu1.My.Ev2, rank = "AICc")
best2.My.Ev2<-get.models(select.My.Ev2, 1)[[1]]
screenreg(extract(subset(select.My.Ev2, delta <= 2, recalc.weights = FALSE)), file= "myev.txt")

## Pallid Bat (*Antrozous pallidus*)

#N=49
BatOccuAnPa<-BatOccu[,26:28]

SimOccuAnPa2<-unmarkedFrameOccu(y = BatOccuAnPa, siteCovs =sampling.cov2, obsCovs=Dailycov3)
model.Occu1.An.Pa2 <- occu(~ I(Julian^2) + Meanhum + Meantemp + sdhum + sdtemp ~  Burn.intensity.Canopy + I(Burn.intensity.Canopy^2) + Distance.to.water + I(Distance.to.water^2) + Distance.to.road + I(Distance.to.road^2) + Existing.vegetation +I(Existing.vegetation^2) + Altitude + I(Altitude^2), SimOccuAnPa2)
select.An.Pa2 <- dredge(model.Occu1.An.Pa2, rank = "AICc")
best2.An.Pa2<-get.models(select.An.Pa2, 1)[[1]]
library(texreg)
screenreg(extract(subset(select.An.Pa2, delta <= 2, recalc.weights = FALSE)), file= "anpa.txt")

```{r,echo=FALSE,cache=TRUE}
anpa.stack <- stack(bs,bc, bb, Occu1.layer.AnPa2$layer.1)

names(anpa.stack)<-c("Burn intensity Soil", "Burn intensity Canopy","Burn intensity Basal", "Occupancy")

pairs(anpa.stack)
```

###Fringed Bat (*Myotis thysanoides*)

```{r, echo=FALSE, message=FALSE, warning=FALSE, cache=TRUE, results='hide'}
#N=49
BatOccuMyTh<-BatOccu[,35:37]

```


```{r, echo=FALSE, message=FALSE, warning=FALSE, cache=TRUE}
#N=49
SimOccuMyTh2<-unmarkedFrameOccu(y = BatOccuMyTh, siteCovs =sampling.cov2, obsCovs=Dailycov3)

model.Occu1.My.Th2 <- occu(~ Julian +Meanhum + Meantemp + sdhum + sdtemp ~  Burn.intensity.soil + I(Burn.intensity.soil^2) + Burn.intensity.Canopy + I(Burn.intensity.Canopy^2) + Burn.intensity.basal +I(Burn.intensity.basal^2) ,SimOccuMyTh2)


select.My.Th2 <- dredge(model.Occu1.My.Th2, rank = "AICc")

best2.My.Th2<-get.models(select.My.Th2, 1)[[1]]
Occu1.layer.MyTh2 <- predict(best2.My.Th2, type = "state", newdata=Predictors)

Occu1.layer.MyTh2<-Occu1.layer.MyTh2*PNF
plot(Occu1.layer.MyTh2$layer.1, colNA="black", main="Occupancy estimation for Fringed Bat",breaks=brks, col=cols, lab.breaks=brks, zlim=c(0,1))
```

```{r,echo=FALSE,results='asis', warning=FALSE,message=FALSE}
library(texreg)
htmlreg(extract(subset(select.My.Th2, delta <= 2, recalc.weights = FALSE)))
```

```{r,echo=FALSE,cache=TRUE}
myth.stack <- stack(bs,bc, bb, Occu1.layer.MyTh2$layer.1)

names(myth.stack)<-c("Burn intensity Soil", "Burn intensity Canopy","Burn intensity Basal", "Occupancy")

pairs(myth.stack)
```

##Townsend’s Long-eared Bat (*Corynorhinus townsendii*)

```{r, echo=FALSE, message=FALSE, warning=FALSE, cache=TRUE, results='hide'}
#N=49
BatOccuCoTo<-BatOccu[,44:46]
```

```{r, echo=FALSE, message=FALSE, warning=FALSE, cache=TRUE}
#N=49
SimOccuCoTo2<-unmarkedFrameOccu(y = BatOccuCoTo, siteCovs =sampling.cov2, obsCovs=Dailycov3)

model.Occu1.Co.To2 <- occu(~ Julian + Meanhum + Meantemp  ~ Burn.intensity.soil + I(Burn.intensity.soil^2) + Burn.intensity.Canopy + I(Burn.intensity.Canopy^2) + Burn.intensity.basal + I(Burn.intensity.basal^2), SimOccuCoTo2)


select.Co.To2 <- dredge(model.Occu1.Co.To2, rank = "AICc")

best2.Co.To2<-get.models(select.Co.To2, 1)[[1]]
Occu1.layer.CoTo2 <- predict(best2.Co.To2, type = "state", newdata=Predictors)

Occu1.layer.CoTo2<-Occu1.layer.CoTo2*PNF
plot(Occu1.layer.CoTo2$layer.1, colNA="black", main="Occupancy estimation for Townsend big eared bat",breaks=brks, col=cols, lab.breaks=brks, zlim=c(0,1))
```

```{r,echo=FALSE,results='asis', warning=FALSE,message=FALSE}
library(texreg)
htmlreg(extract(subset(select.Co.To2, delta <= 2, recalc.weights = FALSE)))
```

```{r,echo=FALSE,cache=TRUE}
coto.stack <- stack(bs,bc, bb, Occu1.layer.CoTo2$layer.1)

names(coto.stack)<-c("Burn intensity Soil", "Burn intensity Canopy","Burn intensity Basal", "Occupancy")

pairs(coto.stack)
```

##The western pipistrelle (*Parastrellus hesperus*)

```{r, echo=FALSE, message=FALSE, warning=FALSE, cache=TRUE, results='hide'}
#N=49
BatOccuPaHe<-BatOccu[,17:19]
```

```{r, echo=FALSE, message=FALSE, warning=FALSE, cache=TRUE}
#N=49
SimOccuPaHe2<-unmarkedFrameOccu(y = BatOccuPaHe, siteCovs =sampling.cov2, obsCovs=Dailycov3)


#model.Occu1.Pa.He2 <- occu(~ Julian + Meanhum + Meantemp  ~ Burn.intensity.soil + I(Burn.intensity.soil^2) + Burn.intensity.Canopy + Burn.intensity.Canopy2 + Burn.intensity.basal +Burn.intensity.basal2, SimOccuPaHe2)


#select.Pa.He2 <- dredge(model.Occu1.Pa.He2, rank = "AICc")
```

```{r, echo=FALSE, cache= TRUE, results='hide', message=FALSE,warning=FALSE}
#best2.Pa.He2<-get.models(select.Pa.He2, 1)[[1]]
#Occu1.layer.PaHe2 <- predict(best2.Pa.He2, type = "state", newdata=Predictors)
```

```{r, echo=FALSE,warning=FALSE,message=FALSE}
#Occu1.layer.PaHe2<-Occu1.layer.PaHe2*PNF
#plot(Occu1.layer.PaHe2$layer.1, colNA="black", main="Occupancy estimation for Pipistrel",breaks=brks, col=cols, lab.breaks=brks, zlim=c(0,1))
```

```{r,echo=FALSE,results='asis', warning=FALSE,message=FALSE}
#library(texreg)
#htmlreg(extract(subset(select.Pa.He2, delta <= 2, recalc.weights = FALSE)))
```

```{r,echo=FALSE,cache=TRUE}
#pahe.stack <- stack(bs,bc, bb, Occu1.layer.PaHe2$layer.1)

#names(pahe.stack)<-c("Burn intensity Soil", "Burn intensity Canopy","Burn intensity Basal", "Occupancy")

#pairs(pahe.stack)
```

##big brown bat (*Eptesicus fuscus*)

```{r, echo=FALSE, message=FALSE, warning=FALSE, cache=TRUE, results='hide'}
#N=49
BatOccuEpFu<-BatOccu[,29:31]
```

```{r, echo=FALSE, message=FALSE, warning=FALSE, cache=TRUE}
#N=49
SimOccuEpFu2<-unmarkedFrameOccu(y = BatOccuEpFu, siteCovs =sampling.cov2, obsCovs=Dailycov3)

model.Occu1.Ep.Fu2 <- occu(~ Julian + Meanhum + Meantemp + sdhum + sdtemp ~ Burn.intensity.soil + I(Burn.intensity.soil^2) + Burn.intensity.Canopy + I(Burn.intensity.Canopy^2) + Burn.intensity.basal +I(Burn.intensity.basal^2), SimOccuEpFu2)


select.Ep.Fu2 <- dredge(model.Occu1.Ep.Fu2, rank = "AICc")

best2.Ep.Fu2<-get.models(select.Ep.Fu2, 1)[[1]]
Occu1.layer.EpFu2 <- predict(best2.Ep.Fu2, type = "state", newdata=Predictors)

Occu1.layer.EpFu2<-Occu1.layer.EpFu2*PNF
plot(Occu1.layer.EpFu2$layer.1, colNA="black", main="Occupancy estimation for Big Brown Bat",breaks=brks, col=cols, lab.breaks=brks, zlim=c(0,1))
```

```{r,echo=FALSE,results='asis', warning=FALSE,message=FALSE}
library(texreg)
htmlreg(extract(subset(select.Ep.Fu2, delta <= 2, recalc.weights = FALSE)))
```

```{r,echo=FALSE,cache=TRUE}
epfu.stack <- stack(bs,bc, bb, Occu1.layer.EpFu2$layer.1)

names(epfu.stack)<-c("Burn intensity Soil", "Burn intensity Canopy","Burn intensity Basal", "Occupancy")

pairs(epfu.stack)
```

##silver-haired bat (*Lasionycteris noctivagans*)

```{r, echo=FALSE, message=FALSE, warning=FALSE, cache=TRUE, results='hide'}
#N=49
BatOccuLaNo<-BatOccu[,32:34]
```

```{r, echo=FALSE, message=FALSE, warning=FALSE, cache=TRUE}
#N=49
SimOccuLaNo2<-unmarkedFrameOccu(y = BatOccuLaNo, siteCovs =sampling.cov2, obsCovs=Dailycov3)

model.Occu1.La.No2 <- occu(~ Julian + Meanhum + Meantemp + sdhum + sdtemp ~  Burn.intensity.soil + I(Burn.intensity.soil^2) + Burn.intensity.Canopy + I(Burn.intensity.Canopy^2) + Burn.intensity.basal + (Burn.intensity.basal^2), SimOccuLaNo2)


select.La.No2 <- dredge(model.Occu1.La.No2, rank = "AICc")

best2.La.No2<-get.models(select.La.No2, 1)[[1]]
Occu1.layer.LaNo2 <- predict(best2.La.No2, type = "state", newdata=Predictors)

Occu1.layer.LaNo2<-Occu1.layer.LaNo2*PNF
plot(Occu1.layer.LaNo2$layer.1, colNA="black", main="Occupancy estimation for Silver Haired Bat",breaks=brks, col=cols, lab.breaks=brks, zlim=c(0,1))
```

```{r,echo=FALSE,results='asis', warning=FALSE,message=FALSE}
library(texreg)
htmlreg(extract(subset(select.La.No2, delta <= 2, recalc.weights = FALSE)))
```

```{r,echo=FALSE,cache=TRUE}
lano.stack <- stack(bs,bc, bb, Occu1.layer.LaNo2$layer.1)

names(lano.stack)<-c("Burn intensity Soil", "Burn intensity Canopy","Burn intensity Basal", "Occupancy")

pairs(lano.stack)
```

##Brazilian free-tailed bat (*Tadarida brasiliensis*)

```{r,echo=FALSE, warning=FALSE,message=FALSE}
BatOccuTaBr<-BatOccu[,38:40]
```

```{r, echo=FALSE, message=FALSE, warning=FALSE, cache=TRUE}
#N=49
SimOccuTaBr2<-unmarkedFrameOccu(y = BatOccuTaBr, siteCovs =sampling.cov2, obsCovs=Dailycov3)


model.Occu1.Ta.Br2 <- occu(~ Julian + Meanhum + Meantemp  ~ Burn.intensity.soil + I(Burn.intensity.soil^2) + Burn.intensity.Canopy +  I(Burn.intensity.Canopy^2) + Burn.intensity.basal + I(Burn.intensity.basal^2), SimOccuTaBr2)


select.Ta.Br2 <- dredge(model.Occu1.Ta.Br2, rank = "AICc")

best2.Ta.Br2<-get.models(select.Ta.Br2, 1)[[1]]
Occu1.layer.TaBr2 <- predict(best2.Ta.Br2, type = "state", newdata=Predictors)

Occu1.layer.TaBr2<-Occu1.layer.TaBr2*PNF
plot(Occu1.layer.TaBr2$layer.1, colNA="black", main="Occupancy estimation for Brazilian free-tailed bat",breaks=brks, col=cols, lab.breaks=brks, zlim=c(0,1))
```

```{r,echo=FALSE,results='asis', warning=FALSE,message=FALSE}
library(texreg)
htmlreg(extract(subset(select.Ta.Br2, delta <= 2, recalc.weights = FALSE)))
```

```{r,echo=FALSE,cache=TRUE}
tabr.stack <- stack(bs,bc, bb, Occu1.layer.TaBr2$layer.1)

names(tabr.stack)<-c("Burn intensity Soil", "Burn intensity Canopy","Burn intensity Basal", "Occupancy")

pairs(tabr.stack)
```

##hoary bat (*Lasiurus cinereus*)


```{r,echo=FALSE, warning=FALSE,message=FALSE}
BatOccuLaCi<-BatOccu[,41:43]
```

```{r, echo=FALSE, message=FALSE, warning=FALSE, cache=TRUE}
#N=49
SimOccuLaCi2<-unmarkedFrameOccu(y = BatOccuLaCi, siteCovs =sampling.cov2, obsCovs=Dailycov3)

model.Occu1.La.Ci2 <- occu(~ Julian + Meanhum + Meantemp + sdhum + sdtemp ~ Burn.intensity.soil + I(Burn.intensity.soil^2) + Burn.intensity.Canopy + (Burn.intensity.Canopy^2) + Burn.intensity.basal + I(Burn.intensity.basal^2), SimOccuLaCi2)


select.La.Ci2 <- dredge(model.Occu1.La.Ci2, rank = "AICc")

best2.La.Ci2<-get.models(select.La.Ci2, 1)[[1]]
Occu1.layer.LaCi2 <- predict(best2.La.Ci2, type = "state", newdata=Predictors)

Occu1.layer.LaCi2<-Occu1.layer.LaCi2*PNF
plot(Occu1.layer.LaCi2$layer.1, colNA="black", main="Occupancy estimation for Hoary Bat",breaks=brks, col=cols, lab.breaks=brks, zlim=c(0,1))
```

```{r,echo=FALSE,results='asis', warning=FALSE,message=FALSE}
library(texreg)
htmlreg(extract(subset(select.La.Ci2, delta <= 2, recalc.weights = FALSE)))
```

```{r,echo=FALSE,cache=TRUE}
laci.stack <- stack(bs,bc, bb, Occu1.layer.LaCi2$layer.1)

names(laci.stack)<-c("Burn intensity Soil", "Burn intensity Canopy","Burn intensity Basal", "Occupancy")

pairs(laci.stack)
```

##Spotted bat (*Euderma maculatum*)

```{r,echo=FALSE, warning=FALSE,message=FALSE}
BatOccuEuMa<-BatOccu[,47:49]
```

```{r, echo=FALSE, message=FALSE, warning=FALSE, cache=TRUE}
#N=49
SimOccuEuMa2<-unmarkedFrameOccu(y = BatOccuEuMa, siteCovs =sampling.cov2, obsCovs=Dailycov3)

model.Occu1.Eu.Ma2 <- occu(~ Julian + Meanhum + Meantemp + sdhum + sdtemp ~ Burn.intensity.soil + I(Burn.intensity.soil^2) + Burn.intensity.Canopy + I(Burn.intensity.Canopy^2) + Burn.intensity.basal +I(Burn.intensity.basal^2), SimOccuEuMa2)


select.Eu.Ma2 <- dredge(model.Occu1.Eu.Ma2, rank = "AICc")

best2.Eu.Ma2<-get.models(select.Eu.Ma2, 1)[[1]]
Occu1.layer.EuMa2 <- predict(best2.Eu.Ma2, type = "state", newdata=Predictors)

Occu1.layer.EuMa2<-Occu1.layer.EuMa2*PNF
plot(Occu1.layer.EuMa2$layer.1, colNA="black", main="Occupancy estimation for Spotted Bat",breaks=brks, col=cols, lab.breaks=brks, zlim=c(0,1))
```

```{r,echo=FALSE,results='asis', warning=FALSE,message=FALSE}
library(texreg)
htmlreg(extract(subset(select.Eu.Ma2, delta <= 2, recalc.weights = FALSE)))
```

```{r,echo=FALSE,cache=TRUE}
euma.stack <- stack(bs,bc, bb, Occu1.layer.EuMa2$layer.1)

names(euma.stack)<-c("Burn intensity Soil", "Burn intensity Canopy","Burn intensity Basal", "Occupancy")

pairs(euma.stack)
```

## western mastiff bat (*Eumops perotis*)

```{r,echo=FALSE, warning=FALSE,message=FALSE}
BatOccuEuPe<-BatOccu[,50:52]
```

```{r, echo=FALSE, message=FALSE, warning=FALSE, cache=TRUE}
#N=49


sampling.cov3<-data.frame(scale(sampling.cov2))
SimOccuEuPe2<-unmarkedFrameOccu(y = BatOccuEuPe, siteCovs =sampling.cov2, obsCovs=Dailycov3)

model.Occu1.Eu.Pe2 <- occu(~ Julian + Meanhum + Meantemp + sdhum + sdtemp ~ Burn.intensity.soil + I(Burn.intensity.soil^2) + Burn.intensity.Canopy + I(Burn.intensity.Canopy^2) + Burn.intensity.basal + I(Burn.intensity.basal^2), SimOccuEuPe2)


select.Eu.Pe2 <- dredge(model.Occu1.Eu.Pe2, rank = "AICc")

best2.Eu.Pe2<-get.models(select.Eu.Pe2, 1)[[1]]
Occu1.layer.EuPe2 <- predict(best2.Eu.Pe2, type = "state", newdata=Predictors)

Occu1.layer.EuPe2<-Occu1.layer.EuPe2*PNF
plot(Occu1.layer.EuPe2$layer.1, colNA="black", main="Occupancy estimation for western mastiff bat",breaks=brks, col=cols, lab.breaks=brks, zlim=c(0,1))
```

```{r,echo=FALSE,results='asis', warning=FALSE,message=FALSE}
library(texreg)
htmlreg(extract(subset(select.Eu.Pe2, delta <= 2, recalc.weights = FALSE)))
write.csv(sampling.cov2, "yahoo.csv")
```

```{r,echo=FALSE,cache=TRUE}
eupe.stack <- stack(bs,bc, bb, Occu1.layer.EuPe2$layer.1)

names(eupe.stack)<-c("Burn intensity Soil", "Burn intensity Canopy","Burn intensity Basal", "Occupancy")

pairs(eupe.stack)
```

#Relationships between different species of Bats


```{r,echo=FALSE, cache=TRUE}
#N=49a
#AllBats<-stack(Occu1.layer.MyYu$layer.1,Occu1.layer.MyCa$layer.1,Occu1.layer.MyCi$layer.1,Occu1.layer.MyVo$layer.1,Occu1.layer.MyLu$layer.1, Occu1.layer.LaBl$layer.1, Occu1.layer.MyEv$layer.1, Occu1.layer.AnPa$layer.1, Occu1.layer.MyTh$layer.1, Occu1.layer.CoTo$layer.1)`
#names(AllBats)<-c("Yuma Myotis", "California Bat","Small Footed Myotis", "Hairy-winged bat" ,"Little Brown Bat", "Western Red Bat", "Long-eared Bat", "Pallid Bat", "Fringed Bat", "Townsend's big-eared Bat")
#plot(AllBats, breaks=brks, col=cols, lab.breaks=brks, zlim=c(0,1), colNA="black") 
#pairs(AllBats, xlim=c(0,1), ylim=c(0,1))
```

```{r,echo=FALSE, cache=TRUE}
#N=49a
#AllBats<-stack(Occu1.layer.MyYu2$layer.1, Occu1.layer.MyCa2$layer.1, Occu1.layer.MyCi2$layer.1,Occu1.layer.MyVo2$layer.1,Occu1.layer.MyLu2$layer.1, Occu1.layer.LaBl2$layer.1, Occu1.layer.MyEv2$layer.1,Occu1.layer.AnPa2$layer.1, Occu1.layer.MyTh2$layer.1, Occu1.layer.CoTo2$layer.1, Occu1.layer.EpFu2$layer.1, Occu1.layer.LaNo2$layer.1, Occu1.layer.TaBr2$layer.1, Occu1.layer.LaCi2$layer.1, Occu1.layer.EuMa2$layer.1,Occu1.layer.EuPe2$layer.1)
#names(AllBats)<-c("Yuma Myotis", "California bat","Small Footed Myotis", "Hairy-winged bat" ,"Little Brown Bat", "Western Red Bat","Long-eared Bat" ,"Pallid Bat", "Fringed Bat", "Townsend's big-eared Bat", "Big Brow Bat","Silver-Haired Bat", "Brazilian free-tailed bat", "Hoary Bat", "Spotted bat", " western mastiff bat")
#plot(AllBats, breaks=brks, col=cols, lab.breaks=brks, zlim=c(0,1), colNA="black") 
#pairs(AllBats, xlim=c(0,1), ylim=c(0,1))
```

#Fire bats


```{r,echo=FALSE, cache=TRUE}
#N=49a
FireBats<-stack(Occu1.layer.MyYu2$layer.1, Occu1.layer.MyCi2$layer.1, Occu1.layer.MyLu2$layer.1, Occu1.layer.LaBl2$layer.1, Occu1.layer.MyEv2$layer.1,Occu1.layer.AnPa2$layer.1, Occu1.layer.CoTo2$layer.1, Occu1.layer.EpFu2$layer.1, Occu1.layer.LaNo2$layer.1, Occu1.layer.TaBr2$layer.1, Occu1.layer.EuPe2$layer.1)
names(FireBats)<-c("Yuma Myotis","Small Footed Myotis","Little Brown Bat", "Western Red Bat","Long-eared Bat" ,"Pallid Bat", "Townsend's big-eared Bat", "Big Brow Bat","Silver-Haired Bat", "Brazilian free-tailed bat", " western mastiff bat")
plot(FireBats, breaks=brks, col=cols, lab.breaks=brks, zlim=c(0,1), colNA="black") 
pairs(FireBats, xlim=c(0,1), ylim=c(0,1))
```


```{r,echo=FALSE, cache=TRUE}
levelplot(FireBats, col.regions = rev(terrain.colors(99)), colorkey = list(space = "bottom"))
```


```{r, echo=FALSE, cache=TRUE}
fire <- readGDAL("~/Documents/PlumasBatsApp/fire.asc")
fire<-raster (fire)

not.fire <- readGDAL("~/Documents/PlumasBatsApp/not_fire.asc")
not.fire<-raster (not.fire)

fire<- reclassify(fire, cbind(0, NA))

plot(fire)
plot(not.fire)

bats.with.fire1<-FireBats*fire
bats.without.fire1<-FireBats*not.fire

names(bats.with.fire1)<-c("Yuma Myotis","Small Footed Myotis","Little Brown Bat", "Western Red Bat","Long-eared Bat" ,"Pallid Bat", "Townsend's big-eared Bat", "Big Brow Bat","Silver-Haired Bat", "Brazilian free-tailed bat", " western mastiff bat")


names(bats.without.fire1)<-c("Yuma Myotis","Small Footed Myotis","Little Brown Bat", "Western Red Bat","Long-eared Bat" ,"Pallid Bat", "Townsend's big-eared Bat", "Big Brow Bat","Silver-Haired Bat", "Brazilian free-tailed bat", " western mastiff bat")

plot(bats.with.fire1, breaks=brks, col=cols, lab.breaks=brks, zlim=c(0,1), colNA="black", main= "Fire")
plot(bats.without.fire1, breaks=brks, col=cols, lab.breaks=brks, zlim=c(0,1), colNA="black", main= "Not Fire")
```

```{r,echo=FALSE, cache=TRUE}
levelplot(bats.with.fire1, col.regions = rev(terrain.colors(99)), colorkey = list(space = "bottom"))

levelplot(bats.without.fire1, col.regions = rev(terrain.colors(99)), colorkey = list(space = "bottom"))
```

```{r, echo=FALSE, cache=TRUE}
with.fire1<-cellStats(bats.with.fire1, 'mean', na.rm=TRUE)
without.fire1<-cellStats(bats.without.fire1, 'mean', na.rm=TRUE)
with.fire.sd1<-cellStats(bats.with.fire1, 'sd', na.rm=TRUE)

with.fire1<-data.frame(with.fire1)
with.fire1<-data.frame(with.fire1)
without.fire1<-data.frame(without.fire1)

fire.response1<-cbind(with.fire1,without.fire1, with.fire.sd1)
kable(fire.response1, digits=2)
write.csv(fire.response1, file="Fire_response.csv")
```

```{r,echo=FALSE, cache=TRUE}
myyu<-as.vector(subset(bats.with.fire1,1))
myyu<-myyu[!is.na(myyu)]
t.test(myyu, alternative = "two.sided", mu = fire.response1$without.fire[1])
myyu <-(myyu-fire.response1$without.fire[1])

myci<-as.vector(subset(bats.with.fire1,2))
myci<-myci[!is.na(myci)]
t.test(myci, alternative = "two.sided", mu = fire.response1$without.fire[2])
myci <-(myci-fire.response1$without.fire[2])

mylu<-as.vector(subset(bats.with.fire1,3))
mylu<-mylu[!is.na(mylu)]
t.test(mylu, alternative = "two.sided", mu = fire.response1$without.fire[3])
mylu <-(mylu-fire.response1$without.fire[3])

labl<-as.vector(subset(bats.with.fire1,4))
labl<-labl[!is.na(labl)]
t.test(labl, alternative = "two.sided", mu = fire.response1$without.fire[4])
labl <-(labl-fire.response1$without.fire1[4])

myev<-as.vector(subset(bats.with.fire1,5))
myev<-myev[!is.na(myev)]
t.test(myev, alternative = "two.sided", mu = fire.response1$without.fire[5])
myev <-(myev-fire.response1$without.fire[5])

anpa<-as.vector(subset(bats.with.fire1,6))
anpa<-anpa[!is.na(anpa)]
t.test(anpa, alternative = "two.sided", mu = fire.response1$without.fire[6])
anpa <-(anpa-fire.response1$without.fire[6])

coto<-as.vector(subset(bats.with.fire1,7))
coto<-coto[!is.na(coto)]
t.test(coto, alternative = "two.sided", mu = fire.response1$without.fire[7])
coto <-(coto-fire.response1$without.fire[7])

epfu<-as.vector(subset(bats.with.fire1,8))
epfu<-epfu[!is.na(epfu)]
t.test(epfu, alternative = "two.sided", mu = fire.response1$without.fire[8])
epfu <-(epfu-fire.response1$without.fire[8])

lano<-as.vector(subset(bats.with.fire1,9))
lano<-lano[!is.na(lano)]
t.test(lano, alternative = "two.sided", mu = fire.response1$without.fire[9])
lano <-(lano-fire.response1$without.fire[9])

tabr<-as.vector(subset(bats.with.fire1,10))
tabr<-tabr[!is.na(tabr)]
t.test(tabr, alternative = "two.sided", mu = fire.response1$without.fire[10])
tabr <-(tabr-fire.response1$without.fire[10])


eupe<-as.vector(subset(bats.with.fire1,11))
eupe<-eupe[!is.na(eupe)]
t.test(eupe, alternative = "two.sided", mu = fire.response1$without.fire[11])
eupe <-(eupe-fire.response1$without.fire[11])

br<-cbind.data.frame(myyu,myci, mylu, labl, myev, anpa, coto, epfu, lano, tabr, eupe)
par(las = 2)
boxplot(br, outline=FALSE, cex= 0.8, ylab="Difference from unburned occupancy")
abline(h=0)

```

```{r,echo=FALSE, cache=TRUE}
myyu3<- cbind.data.frame(myyu, rep("myyu", times=(length(myyu))))
colnames(myyu3)<-c("Occu", "Species")

myci3<- cbind.data.frame(myci, rep("myci", times=(length(myci))))
colnames(myci3)<-c("Occu", "Species")

mylu3<- cbind.data.frame(mylu, rep("mylu", times=(length(mylu))))
colnames(mylu3)<-c("Occu", "Species")

labl3<- cbind.data.frame(labl, rep("labl", times=(length(labl))))
colnames(labl3)<-c("Occu", "Species")

myev3<- cbind.data.frame(myev, rep("myev", times=(length(myev))))
colnames(myev3)<-c("Occu", "Species")

anpa3<- cbind.data.frame(anpa, rep("anpa", times=(length(anpa))))
colnames(anpa3)<-c("Occu", "Species")

coto3<- cbind.data.frame(coto, rep("coto", times=(length(coto))))
colnames(coto3)<-c("Occu", "Species")

epfu3<- cbind.data.frame(epfu, rep("epfu", times=(length(epfu))))
colnames(epfu3)<-c("Occu", "Species")

lano3<- cbind.data.frame(lano, rep("lano", times=(length(lano))))
colnames(lano3)<-c("Occu", "Species")

tabr3<- cbind.data.frame(tabr, rep("tabr", times=(length(tabr))))
colnames(tabr3)<-c("Occu", "Species")

eupe3<- cbind.data.frame(eupe, rep("eupe", times=(length(eupe))))
colnames(eupe3)<-c("Occu", "Species")

br3<- rbind(myyu3, myci3, mylu3, labl3, myev3, anpa3, coto3, epfu3, lano3, tabr3, eupe3)
br3$Species<-as.factor(br3$Species)

g<-ggplot(br3, aes(x=Species, y=Occu))
g+geom_jitter(alpha=0.2, aes(color=Species), position = position_jitter(width = 0.1))+theme_bw()+geom_violin(alpha=0.7, color="gray",scale = "width")+ stat_summary(fun.y=mean, geom="point", shape=18, size=3, color="black")+ geom_hline(yintercept = 0)+theme(legend.position="none")


g+geom_violin(alpha=0.5, color="gray",scale = "width")+theme_bw()+ stat_summary(fun.y=mean, geom="point", shape=18, size=3, color="black")+ geom_hline(yintercept = 0)


g<-ggplot(br3, aes(x=Species, y=Occu))
g+geom_jitter(alpha=0.2, aes(color="grey"), position = position_jitter(width = 0.1))+theme_bw()+geom_violin(alpha=0.7, color="gray",scale = "width")+ stat_summary(fun.y=mean, geom="point", shape=18, size=3, color="black")+ geom_hline(yintercept = 0)+theme(legend.position="none")

```

```{r, cache=TRUE}
library(vioplot)
vioplot(myyu,myci, mylu, labl, myev, anpa, coto, epfu, lano, tabr, eupe, col="grey")
```


#**The End**



```{r,echo=FALSE, warning=FALSE,message=FALSE, cache=TRUE}

brickses=c(0,1.01,4)
colores=c("chartreuse4", "darkorange2")
plot(PNF, legend=FALSE)
plot((PNF+fire), legend=FALSE, breaks=brickses, col=colores, add=TRUE)
points(RESULTS$LONG, RESULTS$LAT, cex=0.5)
map.scale(-120.7, 39.5, relwidth = 0.15, metric = TRUE, ratio = FALSE,cex=0.7)
```

```{r, echo=TRUE}
#valuetable <- getValues(AllLayers2)
#km1 <- kmeans(na.omit(valuetable), centers = 5, iter.max = 100, nstart = 10)
# create a blank raster with default values of 0
#rNA <- setValues(raster(AllLayers2), 0)
#for(i in 1:nlayers(AllLayers2)){
#rNA[is.na(AllLayers2[[i]])] <- 1
#}
# convert rNA to an integer vector
#rNA <- getValues(rNA)
# convert valuetable to a data.frame
#valuetable <- as.data.frame(valuetable)
# if rNA is a 0, assign the cluster value at that position
#valuetable$class[rNA==0] <- km1$cluster
# if rNA is a 1, assign an NA at that position
#valuetable$class[rNA==1] <- NA
# create a blank raster
#classes1 <- raster(AllLayers2)
# assign values from the 'class' column of valuetable
#classes1 <- setValues(classes1, valuetable$class)
#plot(classes1, legend=TRUE, colNA="black")
#More info on how to do this clasification in *https://geoscripting-wur.github.io/AdvancedRasterAnalysis/*
```

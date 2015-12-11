
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(unmarked)
library(ggplot2)
library(caret)
library(RANN)

sampling.cov2 <- read.csv("sampling.cov2.csv")
sampling.cov2<- sampling.cov2[,-1]
sampling.cov2<- data.frame(sampling.cov2)
preprocov <-preProcess(sampling.cov2[,1:8], method = c("center","scale"))
sampling.cov2<-predict(preprocov, sampling.cov2[,1:8])
Dailycov3 <- read.csv("Dailycov3.csv")
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

BatOccu <- read.csv("BatOccu.csv")

library(unmarked)

BatOccuMyYu<-BatOccu[,2:4]
SimOccuMyYu2<-unmarkedFrameOccu(y = BatOccuMyYu, siteCovs =sampling.cov2, obsCovs=Dailycov3)
best2.My.Yu2<-occu(formula = ~1 ~ Altitude + I(Altitude^2) + I(Burn.intensity.Canopy^2) + I(Distance.to.road^2) + Existing.vegetation + I(Existing.vegetation^2) + 1, data = SimOccuMyYu2)

BatOccuMyCa<-BatOccu[,5:7]
SimOccuMyCa2<-unmarkedFrameOccu(y = BatOccuMyCa, siteCovs =sampling.cov2, obsCovs=Dailycov3)
best2.My.Ca2<-occu(formula = ~Meanhum + 1 ~ Altitude + I(Altitude^2) + Distance.to.road + 1, data = SimOccuMyCa2)

BatOccuMyCi<-BatOccu[,8:10]
SimOccuMyCi2<-unmarkedFrameOccu(y = BatOccuMyCi, siteCovs =sampling.cov2, obsCovs=Dailycov3)
best2.My.Ci2<-occu(formula = ~1 ~ Altitude + Burn.intensity.Canopy + Distance.to.road + I(Distance.to.road^2) + 1, data = SimOccuMyCi2)

BatOccuMyVo<-BatOccu[,11:13]
SimOccuMyVo2<-unmarkedFrameOccu(y = BatOccuMyVo, siteCovs =sampling.cov2, obsCovs=Dailycov3)
best2.My.Vo2<-occu(formula = ~1 ~ Altitude + Burn.intensity.Canopy + I(Distance.to.road^2) +Distance.to.water + Existing.vegetation + 1, data = SimOccuMyVo2)

BatOccuMyLu<-BatOccu[,14:16]
SimOccuMyLu2<-unmarkedFrameOccu(y = BatOccuMyLu, siteCovs =sampling.cov2, obsCovs=Dailycov3)
best2.My.Lu2<-occu(formula = ~I(Julian^2) + Meanhum + Meantemp + 1 ~ Altitude + Burn.intensity.Canopy + I(Burn.intensity.Canopy^2) + Distance.to.road + I(Distance.to.water^2) + 1, data = SimOccuMyLu2)

BatOccuLaBl<-BatOccu[,20:22]
SimOccuLaBl2<-unmarkedFrameOccu(y = BatOccuLaBl, siteCovs =sampling.cov2, obsCovs=Dailycov3)
best2.La.Bl2<-occu(formula = ~1 ~ Altitude + Burn.intensity.Canopy + I(Distance.to.road^2) + I(Distance.to.water^2) + Existing.vegetation + 1, data = SimOccuLaBl2)

BatOccuMyEv<-BatOccu[,23:25]
SimOccuMyEv2<-unmarkedFrameOccu(y = BatOccuMyEv, siteCovs =sampling.cov2, obsCovs=Dailycov3)
best2.My.Ev2<- occu(formula = ~I(Julian^2) + 1 ~ Altitude + I(Altitude^2) + Burn.intensity.Canopy + Distance.to.road + I(Distance.to.road^2) + Distance.to.water + 1, data = SimOccuMyEv2)

BatOccuAnPa<-BatOccu[,26:28]
SimOccuAnPa2<-unmarkedFrameOccu(y = BatOccuAnPa, siteCovs =sampling.cov2, obsCovs=Dailycov3)
best2.An.Pa2<-occu(formula = ~Meantemp + 1 ~ I(Altitude^2) + Burn.intensity.Canopy + Distance.to.water + Existing.vegetation + I(Existing.vegetation^2) + 1, data = SimOccuAnPa2)

shinyServer(function(input, output) {

  output$distPlot <- renderPlot({

    # generate the data
    new.data<-c(input$Distance.to.water, input$Distance.to.road, 8.970, 14.97, input$Altitude, 1.4178, input$BC, 2.0149)
    new.data<-data.frame(new.data)
    new.data<-t(new.data)
    new.data<-data.frame(new.data)
    colnames(new.data)<- c("Distance.to.water", "Distance.to.road", "Existing.vegetation", "Fire.Interval", "Altitude","Burn.intensity.soil", "Burn.intensity.Canopy", "Burn.intensity.basal")
    new.data<-predict(preprocov, new.data)
    
    pred.My.Ca <-predict(best2.My.Ca2, type = "state", new.data)$Predicted
    SE.My.Ca <-predict(best2.My.Ca2, type = "state", new.data)$SE
    pred.My.Yu <-predict(best2.My.Yu2, type = "state", new.data)$Predicted
    SE.My.Yu <-predict(best2.My.Yu2, type = "state", new.data)$SE
    pred.My.Ci <-predict(best2.My.Ci2, type = "state", new.data)$Predicted
    SE.My.Ci <-predict(best2.My.Ci2, type = "state", new.data)$SE
    pred.My.Vo <-predict(best2.My.Vo2, type = "state", new.data)$Predicted
    SE.My.Vo <-predict(best2.My.Vo2, type = "state", new.data)$SE
    pred.My.Lu <-predict(best2.My.Lu2, type = "state", new.data)$Predicted
    SE.My.Lu <-predict(best2.My.Lu2, type = "state", new.data)$SE
    pred.La.Bl <-predict(best2.La.Bl2, type = "state", new.data)$Predicted
    SE.La.Bl <-predict(best2.La.Bl2, type = "state", new.data)$SE
    pred.My.Ev <-predict(best2.My.Ev2, type = "state", new.data)$Predicted
    SE.My.Ev <-predict(best2.My.Ev2, type = "state", new.data)$SE
    pred.An.Pa <-predict(best2.An.Pa2, type = "state", new.data)$Predicted
    SE.An.Pa <-predict(best2.An.Pa2, type = "state", new.data)$SE
  
    Species <- c("Myca", "Myyu", "Myci", "Myvo", "Mylu", "Labl", "Myev", "Anpa")
    Prediction <- c(pred.My.Ca, pred.My.Yu, pred.My.Ci, pred.My.Vo, pred.My.Lu, pred.La.Bl, pred.My.Ev, pred.An.Pa)
    SE<-c(SE.My.Ca, SE.My.Yu, SE.My.Ci, SE.My.Vo, SE.My.Lu, SE.La.Bl, SE.My.Ev, SE.An.Pa)
    DF <-data.frame(cbind(Species, Prediction))

    # plot occupancy
    Bats.plot <- barplot(Prediction, ylim=c(0, 1.2), xlab=NULL, ylab="Occupancy", main = "Bat occupancy in Plumas")
    abline(h=1, lty=3)
    axis(1, labels= c("Myca", "Myyu", "Myci", "Myvo", "Mylu", "Labl", "Myev", "Anpa"), at = Bats.plot)
    stDevs.t <- matrix(SE,length(SE))
    segments(Bats.plot, Prediction - stDevs.t, Bats.plot, Prediction + stDevs.t, lwd=2)
    segments(Bats.plot - 0.1, Prediction - stDevs.t, Bats.plot + 0.1, Prediction - stDevs.t, lwd=2)
    segments(Bats.plot - 0.1, Prediction + stDevs.t, Bats.plot + 0.1, Prediction + stDevs.t, lwd=2)
  })
  output$downloadPlot <- downloadHandler(
    filename = function() { paste("occuplot", '.png', sep='') },
    content = function(file) {
      png(file)
      new.data<-c(input$Distance.to.water, input$Distance.to.road, 8.970, 14.97, input$Altitude, 1.4178, input$BC, 2.0149)
      new.data<-data.frame(new.data)
      new.data<-t(new.data)
      new.data<-data.frame(new.data)
      colnames(new.data)<- c("Distance.to.water", "Distance.to.road", "Existing.vegetation", "Fire.Interval", "Altitude","Burn.intensity.soil", "Burn.intensity.Canopy", "Burn.intensity.basal")
      new.data<-predict(preprocov, new.data)
      
      pred.My.Ca <-predict(best2.My.Ca2, type = "state", new.data)$Predicted
      SE.My.Ca <-predict(best2.My.Ca2, type = "state", new.data)$SE
      pred.My.Yu <-predict(best2.My.Yu2, type = "state", new.data)$Predicted
      SE.My.Yu <-predict(best2.My.Yu2, type = "state", new.data)$SE
      pred.My.Ci <-predict(best2.My.Ci2, type = "state", new.data)$Predicted
      SE.My.Ci <-predict(best2.My.Ci2, type = "state", new.data)$SE
      pred.My.Vo <-predict(best2.My.Vo2, type = "state", new.data)$Predicted
      SE.My.Vo <-predict(best2.My.Vo2, type = "state", new.data)$SE
      pred.My.Lu <-predict(best2.My.Lu2, type = "state", new.data)$Predicted
      SE.My.Lu <-predict(best2.My.Lu2, type = "state", new.data)$SE
      pred.La.Bl <-predict(best2.La.Bl2, type = "state", new.data)$Predicted
      SE.La.Bl <-predict(best2.La.Bl2, type = "state", new.data)$SE
      pred.My.Ev <-predict(best2.My.Ev2, type = "state", new.data)$Predicted
      SE.My.Ev <-predict(best2.My.Ev2, type = "state", new.data)$SE
      pred.An.Pa <-predict(best2.An.Pa2, type = "state", new.data)$Predicted
      SE.An.Pa <-predict(best2.An.Pa2, type = "state", new.data)$SE
      
      Species <- c("Myca", "Myyu", "Myci", "Myvo", "Mylu", "Labl", "Myev", "Anpa")
      Prediction <- c(pred.My.Ca, pred.My.Yu, pred.My.Ci, pred.My.Vo, pred.My.Lu, pred.La.Bl, pred.My.Ev, pred.An.Pa)
      SE<-c(SE.My.Ca, SE.My.Yu, SE.My.Ci, SE.My.Vo, SE.My.Lu, SE.La.Bl, SE.My.Ev, SE.An.Pa)
      DF <-data.frame(cbind(Species, Prediction))
      
      # plot occupancy
      Bats.plot <- barplot(Prediction, ylim=c(0, 1.2), xlab=NULL, ylab="Occupancy", main = "Bat occupancy in Plumas")
      abline(h=1, lty=3)
      axis(1, labels= c("Myca", "Myyu", "Myci", "Myvo", "Mylu", "Labl", "Myev", "Anpa"), at = Bats.plot)
      stDevs.t <- matrix(SE,length(SE))
      segments(Bats.plot, Prediction - stDevs.t, Bats.plot, Prediction + stDevs.t, lwd=2)
      segments(Bats.plot - 0.1, Prediction - stDevs.t, Bats.plot + 0.1, Prediction - stDevs.t, lwd=2)
      segments(Bats.plot - 0.1, Prediction + stDevs.t, Bats.plot + 0.1, Prediction + stDevs.t, lwd=2)
      dev.off()
    })
  output$visFun <- renderDataTable({
    new.data<-c(input$Distance.to.water, input$Distance.to.road, 8.970, 14.97, input$Altitude, 1.4178, input$BC, 2.0149)
    new.data<-data.frame(new.data)
    new.data<-t(new.data)
    new.data<-data.frame(new.data)
    colnames(new.data)<- c("Distance.to.water", "Distance.to.road", "Existing.vegetation", "Fire.Interval", "Altitude","Burn.intensity.soil", "Burn.intensity.Canopy", "Burn.intensity.basal")
    new.data<-predict(preprocov, new.data)
    
    pred.My.Ca <-predict(best2.My.Ca2, type = "state", new.data)$Predicted
    pred.My.Yu <-predict(best2.My.Yu2, type = "state", new.data)$Predicted
    pred.My.Ci <-predict(best2.My.Ci2, type = "state", new.data)$Predicted
    pred.My.Vo <-predict(best2.My.Vo2, type = "state", new.data)$Predicted
    pred.My.Lu <-predict(best2.My.Lu2, type = "state", new.data)$Predicted
    pred.La.Bl <-predict(best2.La.Bl2, type = "state", new.data)$Predicted
    pred.My.Ev <-predict(best2.My.Ev2, type = "state", new.data)$Predicted
    pred.An.Pa <-predict(best2.An.Pa2, type = "state", new.data)$Predicted

    Species <- c("Myca", "Myyu", "Myci", "Myvo", "Mylu", "Labl", "Myev", "Anpa")
    Prediction <- c(pred.My.Ca, pred.My.Yu, pred.My.Ci, pred.My.Vo, pred.My.Lu, pred.La.Bl, pred.My.Ev, pred.An.Pa)
    Prediction<-round(Prediction,digits = 2)
    DF <-data.frame(t(Prediction))
    colnames(DF) <- Species
    DF
    },options=list(paging = FALSE,
      searching = FALSE))
  output$downloadData <- downloadHandler(
    filename = function() { paste("DF", '.csv', sep='') },
    content = function(file) {
      new.data<-c(input$Distance.to.water, input$Distance.to.road, 8.970, 14.97, input$Altitude, 1.4178, input$BC, 2.0149)
      new.data<-data.frame(new.data)
      new.data<-t(new.data)
      new.data<-data.frame(new.data)
      colnames(new.data)<- c("Distance.to.water", "Distance.to.road", "Existing.vegetation", "Fire.Interval", "Altitude","Burn.intensity.soil", "Burn.intensity.Canopy", "Burn.intensity.basal")
      new.data1<-predict(preprocov, new.data)
      
      pred.My.Ca <-predict(best2.My.Ca2, type = "state", new.data1)$Predicted
      pred.My.Yu <-predict(best2.My.Yu2, type = "state", new.data1)$Predicted
      pred.My.Ci <-predict(best2.My.Ci2, type = "state", new.data1)$Predicted
      pred.My.Vo <-predict(best2.My.Vo2, type = "state", new.data1)$Predicted
      pred.My.Lu <-predict(best2.My.Lu2, type = "state", new.data1)$Predicted
      pred.La.Bl <-predict(best2.La.Bl2, type = "state", new.data1)$Predicted
      pred.My.Ev <-predict(best2.My.Ev2, type = "state", new.data1)$Predicted
      pred.An.Pa <-predict(best2.An.Pa2, type = "state", new.data1)$Predicted
      
      Species <- c("Myca", "Myyu", "Myci", "Myvo", "Mylu", "Labl", "Myev", "Anpa")
      Prediction <- c(pred.My.Ca, pred.My.Yu, pred.My.Ci, pred.My.Vo, pred.My.Lu, pred.La.Bl, pred.My.Ev, pred.An.Pa)
      Prediction<-round(Prediction,digits = 2)
      DF <-data.frame(t(Prediction))
      colnames(DF) <- Species
      DF<- cbind(DF,new.data)
      write.csv(DF, file)
    }
  )
})


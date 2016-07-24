
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

preprocov <- readRDS("preprocov.rds")
  
library(unmarked)

best2.My.Yu2 <- readRDS("best2.My.Yu2.rds")

best2.My.Ca2 <- readRDS("best2.My.Ca2.rds")

best2.My.Ci2 <- readRDS("best2.My.Ci2.rds")

best2.My.Vo2 <- readRDS("best2.My.Vo2.rds")

best2.My.Lu2 <- readRDS("best2.My.Lu2.rds")

best2.La.Bl2 <- readRDS("best2.La.Bl2.rds")

best2.My.Ev2 <- readRDS("best2.My.Ev2.rds")

best2.An.Pa2 <- readRDS("best2.An.Pa2.rds")

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


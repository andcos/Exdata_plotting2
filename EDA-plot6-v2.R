##=====================================
## Coursera EDA 
## Date 19.02.2015
## Project 2 programming assigment
## PART 6
##=====================================

library(dplyr)
library(plyr)
library(data.table)
library(lattice)
library(ggplot2)
library(qdap)

## This first line will likely take a few seconds. Be patient!
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")


##6
##Compare emissions from motor vehicle sources in Baltimore City
##with emissions from motor vehicle sources in Los Angeles 
##County, California (fips == "06037"). 
##Which city has seen greater changes over time in motor vehicle
##emissions?

explore_q6_plot<-function(){
  
  png("EDA_plot6.png", width=480,height=480)
  ## create plot
  ## BAltimore city
  f_maryland<-filter(NEI,fips == "24510")
  baltimore_c<- grep(".*Onroad.*", SCC[,2])
  baltimore_vec<-SCC[baltimore_c,]
  ## perform the match
  match_ba<-match(baltimore_vec$SCC, f_maryland$SCC)
  Baltimore_motor<-f_maryland[na.omit(match_ba),]
  
  
  ##LA California (fips == "06037")
  f_la<-filter(NEI,fips == "06037")
  ## subset SCC
  la_c<- grep(".*Onroad.*", SCC[,2])
  la_vec<-SCC[la_c,]
  ## NEI and SCC have comon column SCC
  match_la<-match(la_vec$SCC, f_la$SCC)
  LA_motor<-f_la[na.omit(match_la),]

  
  ## baltimore/LA extra column
  extra<-c(rep("BA",dim(Baltimore_motor)[1]))
  Baltimore_motor<-cbind(Baltimore_motor,extra)
  extra<-c(rep("LA",dim(LA_motor)[1]))
  LA_motor<-cbind(LA_motor,extra)
  data6<-rbind(Baltimore_motor, LA_motor)
  ## final results
  data6$extra<-as.factor(data6$extra)
  

  ## this is one way
  ggplot(data6,aes(x=extra,y=Emissions))+
    geom_bar(stat="identity",aes(fill=extra), colour="black", position="dodge") +
    facet_grid(. ~ year) +
    ylim(0,400) +
    theme(strip.text = element_text(face="bold", size=rel(1.5)),
          strip.background = element_rect(fill="lightblue", colour="black",
                                          size=1))
  

  
  dev.off()
  
  
}
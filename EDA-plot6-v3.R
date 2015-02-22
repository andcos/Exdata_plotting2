##=====================================
## Coursera EDA 
## Date 22.02.2015
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
  
  
  png("EDA_plot6-new.png", width=600,height=480)
  
  
  ## agregate data /filter
  ##=============================
  
  data6<- filter(NEI,(NEI$fips=="24510"|NEI$fips=="06037") & NEI$type=="ON-ROAD" )
  
  
  ##or
 p<- ggplot(data6, aes(factor(year), Emissions))+
  facet_grid(. ~ fips) +
   geom_bar(stat="identity",aes(fill=fips))  +
    xlab("year") +
    ylab(expression('Total PM'[2.5]*" Emissions")) +
     labs(title="Emissions from vehicle type=ON-ROAD \n in Baltimore City,  MD (fips = 24510) and \n Los Angeles, CA (fips = 06037)") +
    theme(strip.text = element_text(face="bold"),
          strip.background = element_rect(fill="lightblue", colour="black",
                                          size=1)) 
  print(p)
  
  
  dev.off()
  
  
}
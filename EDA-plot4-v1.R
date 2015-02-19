##=====================================
## Coursera EDA 
## Date 18.02.2015
## Project 2 programming assigment
## PART 4
##=====================================

library(dplyr)
library(plyr)
library(data.table)
library(lattice)
library(ggplot2)

## This first line will likely take a few seconds. Be patient!
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

##====
##4
##====
##Across the United States, how have emissions from coal
##combustion-related sources changed from 1999-2008?

explore_q4_plot<-function(){
  ## find coal combustion related sources
  png("EDA_plot4.png", width=480,height=480)
  ## create plot
  
  coal_c<- grep(".*Coal.*", SCC[,3])
  coal_vec<-SCC[coal_c,]
  NEI_short<-filter(NEI,NEI$SCC==coal_vec$SCC)
  p <- ggplot(NEI_short, aes(year, Emissions)) + 
  geom_point() + 
  ## eliminating outliers
  ylim(0,60)  +
  geom_smooth() +
  labs(title="Coal combustion related sources between 1999 and 2008")
    
  print(p + facet_grid(type ~ .))
  dev.off()
}


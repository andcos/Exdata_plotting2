##=====================================
## Coursera EDA 
## Date 18.02.2015
## Project 2 programming assigment
## PART 5
##=====================================

library(dplyr)
library(plyr)
library(data.table)
library(lattice)
library(ggplot2)


## This first line will likely take a few seconds. Be patient!
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")
##===
##5
##===
##How have emissions from motor vehicle sources changed from 
##1999-2008 in Baltimore City?

explore_q5_plot<-function(){
  
  png("EDA_plot5-new.png", width=480,height=480)
  
  # subset
  f_maryland<-filter(NEI,fips == "24510" & NEI$type=="ON-ROAD")
  
  ## create plot
  p <- ggplot(f_maryland, aes(year, Emissions)) +
  geom_bar(stat="identity") +   
  labs(title="Emissions of motor sources in Baltimore between 1999 and 2008")  
  ## With one variable
  print(p )   
  
  
 dev.off()
  
  
}


 
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
  
  png("EDA_plot5.png", width=480,height=480)
  
  # subset
  f_maryland<-filter(NEI,fips == "24510")
  motor_c<- grep(".*Onroad.*", SCC[,2])
  motor_vec<-SCC[motor_c,]
  NEI_motor<-filter(NEI,NEI$SCC==motor_vec$SCC)
  
  ## create plot
  p <- ggplot(NEI_motor, aes(year, Emissions)) +
  geom_point() +
  ##geam_line(data=NEI_motor$Emissions) + 
  ## take outliers out
  ylim(0,30)  +
  geom_smooth(col="steelblue") +
  labs(title="Emissions of motor sources in Baltimore between 1999 and 2008")  
  ## With one variable
  print(p + facet_grid(. ~ type))   
  
  dev.off()
  
  
}

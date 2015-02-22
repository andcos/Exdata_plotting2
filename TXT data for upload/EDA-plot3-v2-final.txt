##=====================================
## Coursera EDA 
## Date 18.02.2015
## Project 2 programming assigment
## PART 3
##=====================================

library(dplyr)
library(plyr)
library(data.table)
library(lattice)
library(ggplot2)


## This first line will likely take a few seconds. Be patient!
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

##==
##3
##==
##Of the four types of sources indicated by the type (point,
##nonpoint, onroad, nonroad) variable, which of these four 
##sources have seen decreases in emissions from 1999-2008 
##for Baltimore City? Which have seen increases in emissions 
##from 1999-2008? Use the ggplot2 plotting system to make a plot
##answer this question.


## Baltimore data
## fips == "24510"
f_maryland<-filter(NEI,fips == "24510")

  explore_q3_plot<-function(){
   
  png("EDA_plot3.png", width=350,height=480)
  ## create plot
  ## ggplot with panel  
  
  p <- ggplot(f_maryland, aes(year, Emissions)) +
    geom_point() + 
    geom_smooth() +
    labs(title="Emissions in Baltimore city between 1999 and 2008")
  ## With one variable
  print(p + facet_grid(type ~. ) )
 
  dev.off()
  
}

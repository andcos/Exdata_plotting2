
##=====================================
## Coursera EDA 
## Date 16.02.2015
## Project 2 programming assigment
## PART 1
##=====================================


##load libraryes
library(dplyr)
library(plyr)
library(data.table)

## This first line will likely take a few seconds. Be patient!
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

##====
##q1
##====
##Have total emissions from PM2.5 decreased in the United States
##from 1999 to 2008? Using the base plotting system, make a plot
##showing the total PM2.5 emission from all sources for each of 
##the years 1999, 2002, 2005, and 2008.

## Filter the data for plot 1
f1999<-filter(NEI,year==1999)
f2002<-filter(NEI,year==2002)
f2005<-filter(NEI,year==2005)
f2008<-filter(NEI,year==2008)


require(stats)


explore_q1_stats <- function(){
  
  png("EDA_plot1.png", width=480,height=480)
  ## create plot
  
  ## ==========================
  ## 1  Mean values Emissions
  ##===========================  
  
  ## requires(stats)
  trending_mean<-c(mean(f1999$Emissions),mean(f2002$Emissions),
                   mean(f2005$Emissions),mean(f2008$Emissions))
  plot(trending_mean,
       main=" Mean values of the NSI Emissions for the given years",
       ylab=" mean Emmissions",
       xlab="years",
       axes=FALSE)
  axis(1,at=c(1,2,3,4),lab=c("1999","2002","2005","2008"))
  axis(2, at=c(0,2,4,6))
  abline(lsfit(1:4,trending_mean))
  box()
  
  dev.off()
  
}






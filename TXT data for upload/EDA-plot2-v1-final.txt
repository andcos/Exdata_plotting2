##=====================================
## Coursera EDA 
## Date 16.02.2015
## Project 2 programming assigment
## PART 2
##=====================================


##load libraryes
library(dplyr)
library(plyr)
library(data.table)

## This first line will likely take a few seconds. Be patient!
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

require(stats)

##===
##q2
##===
##Have total emissions from PM2.5 decreased in the Baltimore 
##City, Maryland (fips == "24510") from 1999 to 2008? Use the
##base plotting system to make a plot answering this question.

explore_q2_plot<-function(){
  
  f_maryland<-filter(NEI,fips == "24510")
  f_mary_1999<-filter(NEI,fips == "24510" & year == "1999")
  f_mary_2002<-filter(NEI,fips == "24510" & year == "2002")
  f_mary_2005<-filter(NEI,fips == "24510" & year == "2005")
  f_mary_2008<-filter(NEI,fips == "24510" & year == "2008")
  
  png("EDA_plot2.png", width=480,height=480)
  ## create plot
  
  ## ==========================
  ## 1  Mean values Emissions
  ##===========================  
  
  ## requires(stats)
  trending_mean<-c(mean(f_mary_1999$Emissions),
                   mean(f_mary_2002$Emissions),
                   mean(f_mary_2005$Emissions),
                   mean(f_mary_2008$Emissions))
  plot(trending_mean,
       main=" Mean values of the NSI Emissions in Baltimore City",
       ylab=" mean Emmissions",
       xlab="years",
       axes=FALSE)
  axis(1,at=c(1,2,3,4),lab=c("1999","2002","2005","2008"))
  axis(2, at=c(0,2,4,6))
  abline(lsfit(1:4,trending_mean))
  box()
  
  dev.off()
  
}
  

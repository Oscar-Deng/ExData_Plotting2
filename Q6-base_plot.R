# Compare emissions from motor vehicle sources in Baltimore City with emissions from motor vehicle sources in Los Angeles County, California (fips == "06037"). Which city has seen greater changes over time in motor vehicle emissions?
# Load packages
library(data.table)
library(dplyr)
library(ggplot2)

# Setting function
plotme <- function(){
    # Read rds files
    classA <- readRDS("Source_Classification_Code.rds")
    summA <- readRDS("summarySCC_PM25.rds")
    EmisBal <- filter(summA,fips == "24510")
    EmisLos <- filter(summA,fips == "06037")
    # filt motor vehicle sources
    Carclass <- filter(classA,grepl('Vehicles',EI.Sector))
    CarEmisBal <- merge(Carclass[,c("SCC","EI.Sector")],EmisBal, by="SCC") %>% group_by(year) %>% summarize(average=mean(Emissions,na.rm=TRUE))
    CarEmisLos <- merge(Carclass[,c("SCC","EI.Sector")],EmisLos, by="SCC") %>% group_by(year) %>% summarize(average=mean(Emissions,na.rm=TRUE))
    # create plot (using base plotting system)
par(mar=c(5, 4, 4, 4) + 0.1)
plot(CarEmisBal$year,CarEmisBal$average,pch=16, axes = FALSE, type="b", xlab="",ylab="",col="black", main="Motor Vehicles PM2.5 emissions\nBaltimore vs Los Angeles")
axis(2, ylim=c(0,2),col="black",las=1)
mtext("average PM2.5 emission",side=2,line=2.5)
box()
par(new=TRUE)
plot(CarEmisLos$year,CarEmisLos$average,pch=15, axes = FALSE, xlab="",ylab="",col="blue", type="b",ylim=c(0,100))
axis(4,ylim=c(0,100),col="blue",col.axis="blue",las=1)
axis(1,c("1999","2002","2005","2008"))
mtext("Year",side=1,col="black",line=2.5)
legend("top",legend=c("Baltimore","Los Angeles"),text.col = c("black","blue") , pch=c(16,15) , col=c("black","blue"))
    # Save
    dev.copy(png, file = "Q6-base_plot.png")
    dev.off()
}
# Run function
plotme()
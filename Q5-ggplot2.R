# How have emissions from motor vehicle sources changed from 1999–2008 in Baltimore City?
# Load packages
library(data.table)
library(dplyr)
library(ggplot2)

# package "data.table" "dplyr" "ggplot2" is in need
# Setting function
plotme <- function(){
    # Read rds files
    classA <- readRDS("Source_Classification_Code.rds")
    summA <- readRDS("summarySCC_PM25.rds")
    sumEmisA <- filter(summA,fips == "24510")
    # filt motor vehicle sources
    Carclass <- filter(classA,grepl('Vehicles',EI.Sector))
    CarEmis <- merge(Carclass[,c("SCC","EI.Sector")],sumEmisA, by="SCC") %>% group_by(year) %>% summarize(total=sum(Emissions))
    # create plot
    ggplot(data=CarEmis,aes(x=year,y=total))+geom_line(colour = "blue") + geom_point()+labs(x="year",y="total emissions (PM2.5)") + ggtitle("Motor Vehicles PM2.5 emissions")-> myplot
    # Save
    ggsave(filename = "Q5-ggplot2.png", plot=myplot)
}
# Run function
plotme()
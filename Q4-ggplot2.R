# Across the United States, how have emissions from coal combustion-related sources changed from 1999–2008?

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
    # filt coal combustion-related sources
    Coalclass <- filter(classA,grepl('Coal',EI.Sector))
    CoalEmis <- merge(Coalclass[,c("SCC","EI.Sector")],summA, by="SCC") %>% group_by(year) %>% summarize(total=sum(Emissions))
    # create plot
    ggplot(data=CoalEmis,aes(x=year,y=total))+geom_line(colour = "blue") + geom_point()+labs(x="year",y="total emissions (PM2.5)") + ggtitle("Coal combustion-related sources PM2.5 emissions") -> myplot
    # Save
    ggsave(filename = "Q4-ggplot2.png", plot=myplot)
}
# Run function
plotme()
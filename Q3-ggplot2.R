# Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) variable, which of these four sources have seen decreases in emissions from 1999–2008 for Baltimore City? Which have seen increases in emissions from 1999–2008? Use the ggplot2 plotting system to make a plot answer this question.

# Load packages
library(data.table)
library(dplyr)
library(ggplot2)

# package "data.table" "dplyr" "ggplot2" is in need
# Setting function
plotme <- function(){
    # Read rds files
    # classA <- readRDS("Source_Classification_Code.rds")
    summA <- readRDS("summarySCC_PM25.rds")
    # Set filter that fips is 24510
    sumEmisA <- filter(summA,fips == "24510")
    # add up "Emissions" by "year and type" and submit it to "plotting"
    plotting <- as.data.table(sumEmisA)[,sum(Emissions), by=.(year,type)]
    # copy to png file

    # create plot
    ggplot(plotting, aes(x=year, y=V1)) + geom_line(colour = 4) + facet_grid(type~.) + geom_point(colour = 2) + ylab("total") -> myplot
    # Save
    ggsave(filename = "Q3-ggplot2.png", plot=myplot)
}
# Run function
plotme()
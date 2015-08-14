# Have total emissions from PM2.5 decreased in the United States from 1999 to 2008? Using the base plotting system, make a plot showing the total PM2.5 emission from all sources for each of the years 1999, 2002, 2005, and 2008.

library(data.table)
library(dplyr)
# package "data.table"&"dplyr" is in need
# Setting function
plotme <- function(){
# classA <- readRDS("Source_Classification_Code.rds") # won't use now
    summA <- readRDS("summarySCC_PM25.rds")
    plotting <- summarize(group_by(summA,year), total=sum(Emissions))
    png(filename="Q1-base_plot.png",bg="transparent")
    barplot(plotting$total, names.arg = plotting$year, col="red", main = "Total Emissions(PM2.5)", xlab = "year", ylab = "total")
    dev.off()
}

# Run function
plotme()
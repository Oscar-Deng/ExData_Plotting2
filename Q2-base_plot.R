# Have total emissions from PM2.5 decreased in the Baltimore City, Maryland (fips == "24510") from 1999 to 2008? Use the base plotting system to make a plot answering this question.

library(data.table)
# package "data.table" is in need
# Setting function
plotme <- function(){
# classA <- readRDS("Source_Classification_Code.rds") # won't use now
    summA <- readRDS("summarySCC_PM25.rds")
    sumEmisA <- filter(summA,fips == "24510")
    years <- group_by(sumEmisA, year)
    plotting <- summarize(years, total=sum(Emissions))
    png(filename="Q2-base_plot.png",bg="transparent")
    barplot(plotting$total, names.arg = plotting$year, col="red", main = "Total Emissions(PM2.5) in Baltimore City, Maryland", xlab = "year", ylab = "total")
    dev.off()
}

# Run function
plotme()
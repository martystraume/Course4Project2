#
#-------------------------------------------------------------------------------
#
# plot1.R
#
#-------------------------------------------------------------------------------
#
rdsFile <- "./data/summarySCC_PM25.rds"
NEI <- readRDS(rdsFile)
rdsFile <- "./data/Source_Classification_Code.rds"
SCC <- readRDS(rdsFile)
# 
#-------------------------------------------------------------------------------
# 
# Question #1
# 
# Have total emissions from PM2.5 decreased in the United States from 1999 to
# 2008? Using the base plotting system, make a plot showing the total PM2.5
# emission from all sources for each of the years 1999, 2002, 2005, and 2008.
# 
# Upload a PNG file containing your plot addressing this question.
# 
#-------------------------------------------------------------------------------
# 
png("plot1.png", width = 960, height = 480, units = "px", res = 72)
L1999 <- NEI$year == 1999L
L2002 <- NEI$year == 2002L
L2005 <- NEI$year == 2005L
L2008 <- NEI$year == 2008L
Year <- vector()
Total.PM25.Emissions <- vector()
Total.PM25.Emissions[1] <- sum(NEI$Emissions[L1999]/1000000)
Year[1] <- 1999L
Total.PM25.Emissions[2] <- sum(NEI$Emissions[L2002]/1000000)
Year[2] <- 2002L
Total.PM25.Emissions[3] <- sum(NEI$Emissions[L2005]/1000000)
Year[3] <- 2005L
Total.PM25.Emissions[4] <- sum(NEI$Emissions[L2008]/1000000)
Year[4] <- 2008L
plot(Year, Total.PM25.Emissions,
     xlab = "Year (1999, 2002, 2005, 2008)",
     ylab = "Total U.S. PM2.5 Emissions (millions of tons)",
     xaxp = c(1999L, 2008L, 3),
     pch = 19,
     col = "red")
LRline <- lm(Total.PM25.Emissions ~ Year)
abline(LRline, lwd = 2)
title(main = "Total U.S. PM2.5 emissions from all sources by year")
mtext("Line represents the 4-point linear regression estimate", side = 3)
dev.off()
# 
#-------------------------------------------------------------------------------
# 

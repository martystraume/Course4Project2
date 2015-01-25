#
#-------------------------------------------------------------------------------
#
# plot2.R
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
# Question #2
# 
# Have total emissions from PM2.5 decreased in the Baltimore City, Maryland
# (fips == 24510) from 1999 to 2008? Use the base plotting system to make a plot
# answering this question.
# 
# Upload a PNG file containing your plot addressing this question.
# 
#-------------------------------------------------------------------------------
# 
png("plot2.png", width = 960, height = 480, units = "px", res = 72)
L1999 <- NEI$year == 1999L & NEI$fips == "24510"
L2002 <- NEI$year == 2002L & NEI$fips == "24510"
L2005 <- NEI$year == 2005L & NEI$fips == "24510"
L2008 <- NEI$year == 2008L & NEI$fips == "24510"
Year <- vector()
Total.PM25.Emissions.BaltoCity <- vector()
Total.PM25.Emissions.BaltoCity[1] <- sum(NEI$Emissions[L1999]/1000)
Year[1] <- 1999L
Total.PM25.Emissions.BaltoCity[2] <- sum(NEI$Emissions[L2002]/1000)
Year[2] <- 2002L
Total.PM25.Emissions.BaltoCity[3] <- sum(NEI$Emissions[L2005]/1000)
Year[3] <- 2005L
Total.PM25.Emissions.BaltoCity[4] <- sum(NEI$Emissions[L2008]/1000)
Year[4] <- 2008L
plot(Year, Total.PM25.Emissions.BaltoCity,
     xlab = "Year (1999, 2002, 2005, 2008)",
     ylab = "Total Baltimore City PM2.5 Emissions (thousands of tons)",
     xaxp = c(1999L, 2008L, 3),
     pch = 19,
     col = "red")
LRline <- lm(Total.PM25.Emissions.BaltoCity ~ Year)
abline(LRline, lwd = 2)
title(main = "Total Baltimore City PM2.5 emissions by year")
mtext("Line represents the 4-point linear regression estimate", side = 3)
dev.off()
# 
#-------------------------------------------------------------------------------
# 

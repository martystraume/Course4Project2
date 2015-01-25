#
#-------------------------------------------------------------------------------
#
# plot3.R
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
# Question #3
# 
# Of the four types of sources indicated by the type (point, nonpoint, onroad,
# nonroad) variable, which of these four sources have seen decreases in
# emissions from 1999-2008 for Baltimore City? Which have seen increases in
# emissions from 1999-2008? Use the ggplot2 plotting system to make a plot to
# answer this question.
# 
# Upload a PNG file containing your plot addressing this question.
# 
#-------------------------------------------------------------------------------
# 
# "POINT" [type] factor
#
L1999 <- NEI$year == 1999L & NEI$fips == "24510" & NEI$type == "POINT"
L2002 <- NEI$year == 2002L & NEI$fips == "24510" & NEI$type == "POINT"
L2005 <- NEI$year == 2005L & NEI$fips == "24510" & NEI$type == "POINT"
L2008 <- NEI$year == 2008L & NEI$fips == "24510" & NEI$type == "POINT"
#
DFPlot <- data.frame()
#
DFPlot[1,1] <- 1999L
DFPlot[2,1] <- 2002L
DFPlot[3,1] <- 2005L
DFPlot[4,1] <- 2008L
#
DFPlot[1,2] <- sum(NEI$Emissions[L1999])
DFPlot[2,2] <- sum(NEI$Emissions[L2002])
DFPlot[3,2] <- sum(NEI$Emissions[L2005])
DFPlot[4,2] <- sum(NEI$Emissions[L2008])
#
DFPlot[1:4,3] <- "POINT"
#
#---------------------------------------
# 
# "NONPOINT" [type] factor
#
L1999 <- NEI$year == 1999L & NEI$fips == "24510" & NEI$type == "NONPOINT"
L2002 <- NEI$year == 2002L & NEI$fips == "24510" & NEI$type == "NONPOINT"
L2005 <- NEI$year == 2005L & NEI$fips == "24510" & NEI$type == "NONPOINT"
L2008 <- NEI$year == 2008L & NEI$fips == "24510" & NEI$type == "NONPOINT"
#
DFPlot[5,1] <- 1999L
DFPlot[6,1] <- 2002L
DFPlot[7,1] <- 2005L
DFPlot[8,1] <- 2008L
#
DFPlot[5,2] <- sum(NEI$Emissions[L1999])
DFPlot[6,2] <- sum(NEI$Emissions[L2002])
DFPlot[7,2] <- sum(NEI$Emissions[L2005])
DFPlot[8,2] <- sum(NEI$Emissions[L2008])
#
DFPlot[5:8,3] <- "NONPOINT"
#
#---------------------------------------
# 
# "ON-ROAD" [type] factor
#
L1999 <- NEI$year == 1999L & NEI$fips == "24510" & NEI$type == "ON-ROAD"
L2002 <- NEI$year == 2002L & NEI$fips == "24510" & NEI$type == "ON-ROAD"
L2005 <- NEI$year == 2005L & NEI$fips == "24510" & NEI$type == "ON-ROAD"
L2008 <- NEI$year == 2008L & NEI$fips == "24510" & NEI$type == "ON-ROAD"
#
DFPlot[9,1] <- 1999L
DFPlot[10,1] <- 2002L
DFPlot[11,1] <- 2005L
DFPlot[12,1] <- 2008L
#
DFPlot[9,2] <- sum(NEI$Emissions[L1999])
DFPlot[10,2] <- sum(NEI$Emissions[L2002])
DFPlot[11,2] <- sum(NEI$Emissions[L2005])
DFPlot[12,2] <- sum(NEI$Emissions[L2008])
#
DFPlot[9:12,3] <- "ON-ROAD"
#
#---------------------------------------
# 
# "NON-ROAD" [type] factor
#
L1999 <- NEI$year == 1999L & NEI$fips == "24510" & NEI$type == "NON-ROAD"
L2002 <- NEI$year == 2002L & NEI$fips == "24510" & NEI$type == "NON-ROAD"
L2005 <- NEI$year == 2005L & NEI$fips == "24510" & NEI$type == "NON-ROAD"
L2008 <- NEI$year == 2008L & NEI$fips == "24510" & NEI$type == "NON-ROAD"
#
DFPlot[13,1] <- 1999L
DFPlot[14,1] <- 2002L
DFPlot[15,1] <- 2005L
DFPlot[16,1] <- 2008L
#
DFPlot[13,2] <- sum(NEI$Emissions[L1999])
DFPlot[14,2] <- sum(NEI$Emissions[L2002])
DFPlot[15,2] <- sum(NEI$Emissions[L2005])
DFPlot[16,2] <- sum(NEI$Emissions[L2008])
#
DFPlot[13:16,3] <- "NON-ROAD"
#
#---------------------------------------
#
names(DFPlot)[1:3] <- c("Year", "Emissions", "Type")
par(mfrow = c(4, 1))
png("plot3.png", width = 960, height = 480, units = "px", res = 72)
library(ggplot2)
DFPlot[,4] <- log10(DFPlot$Emissions)
names(DFPlot)[4] <- "log10.Emissions"
plot3 <- ggplot(DFPlot, aes(Year, log10.Emissions))
plot3 <- plot3 + scale_x_continuous(breaks=c(1999, 2002, 2005, 2008))
plot3 <- plot3 + geom_point()
plot3 <- plot3 + geom_line()
plot3 <- plot3 + geom_smooth(method = "lm")
plot3 <- plot3 + facet_grid(. ~ Type) + geom_smooth(method = "lm", col = "red")
plot3 <- plot3 + labs(title = "Baltimore City -- Time-Dependent Changes in log10(PM2.5) Emissions by Type")
plot3 <- plot3 + labs(x = "Year (1999, 2002, 2005, 2008)")
plot3 <- plot3 + labs(y = "log10[PM2.5 (tons)]  (red line = linear regression)")
print(plot3)
dev.off()
# 
#-------------------------------------------------------------------------------
# 

#
#-------------------------------------------------------------------------------
#
# plot4.R
#
#-------------------------------------------------------------------------------
#
rdsFile <- "./data/summarySCC_PM25.rds"
NEI <- readRDS(rdsFile)
rdsFile <- "./data/Source_Classification_Code.rds"
SCC <- readRDS(rdsFile)
# 
DF <- merge(NEI, SCC, by = "SCC")
#
#-------------------------------------------------------------------------------
#
# Question #4
# 
# Across the United States, how have emissions from coal combustion-related
# sources changed from 1999-2008?
# 
# Upload a PNG file containing your plot addressing this question.
# 
#-------------------------------------------------------------------------------
#
index <- vector()
for(icol in 1:dim(DF)[2]){
    temp <- as.character(DF[,icol])
    index <- append(index, grep("coal", temp),
                       after = length(index))
    index <- append(index, grep("Coal", temp),
                        after = length(index))
}
iCoal <- unique(index)
#
iCoal <- sort(iCoal)
lCoal <- vector(mode = "logical", length = length(iCoal))
lCoal <- rep(FALSE, dim(DF)[1])
for(irow in 1:length(iCoal)){
    lCoal[iCoal[irow]] <- TRUE
}
DFCoal <- DF[lCoal,]
#
PlotCoal <- data.frame()
#
L1999 <- DFCoal$year == 1999L
L2002 <- DFCoal$year == 2002L
L2005 <- DFCoal$year == 2005L
L2008 <- DFCoal$year == 2008L
#
PlotCoal[1,1] <- 1999L
PlotCoal[2,1] <- 2002L
PlotCoal[3,1] <- 2005L
PlotCoal[4,1] <- 2008L
#
PlotCoal[1,2] <- sum(DFCoal$Emissions[L1999])
PlotCoal[2,2] <- sum(DFCoal$Emissions[L2002])
PlotCoal[3,2] <- sum(DFCoal$Emissions[L2005])
PlotCoal[4,2] <- sum(DFCoal$Emissions[L2008])
#
names(PlotCoal)[1:2] <- c("Year", "Coal.Emissions")
PlotCoal[,3] <- log10(PlotCoal$Coal.Emissions)
names(PlotCoal)[3] <- "log10.Coal.Emissions"
#
png("plot4.png", width = 960, height = 480, units = "px", res = 72)
library(ggplot2)
plot4 <- ggplot(PlotCoal, aes(Year, log10.Coal.Emissions))
plot4 <- plot4 + scale_x_continuous(breaks=c(1999, 2002, 2005, 2008))
plot4 <- plot4 + geom_point()
plot4 <- plot4 + geom_line()
plot4 <- plot4 + geom_smooth(method = "lm")
plot4 <- plot4 + geom_smooth(method = "lm", col = "red")
plot4 <- plot4 + labs(title = "Time-Dependent Changes in U.S. Coal-Related log10(PM2.5) Emissions")
plot4 <- plot4 + labs(x = "Year (1999, 2002, 2005, 2008)")
plot4 <- plot4 + labs(y = "log10[PM2.5 (tons)]  (red line = linear regression)")
print(plot4)
dev.off()
# 
#-------------------------------------------------------------------------------
# 

#
#-------------------------------------------------------------------------------
#
# plot5.R
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
# Question #5
# 
# How have emissions from motor vehicle sources changed from 1999-2008 in
# Baltimore City?
# 
# Upload a PNG file containing your plot addressing this question.
#
#-------------------------------------------------------------------------------
# 
# The five relevant factor categories of [SCC.Level.Two] are:
#
#     "Highway Vehicles - Gasoline"
#     "Highway Vehicles - Diesel"
#     "Off-highway Vehicle Gasoline, 2-Stroke"
#     "Off-highway Vehicle Gasoline, 4-Stroke"
#     "Off-highway Vehicle Diesel"
#
MotorVehicleSources <- vector()
MotorVehicleSources[1] <- "Highway Vehicles - Gasoline"
MotorVehicleSources[2] <- "Highway Vehicles - Diesel"
MotorVehicleSources[3] <- "Off-highway Vehicle Gasoline, 2-Stroke"
MotorVehicleSources[4] <- "Off-highway Vehicle Gasoline, 4-Stroke"
MotorVehicleSources[5] <- "Off-highway Vehicle Diesel"
#
#-------------------------------------------------------------------------------
# 
lBaltoCity <- DF$fips == "24510"
DFBaltoCity <- DF[lBaltoCity,]
#
index <- vector()
temp <- as.character(DFBaltoCity$SCC.Level.Two)
for(irow in 1:length(MotorVehicleSources)){
    index <- append(index, grep(MotorVehicleSources[irow], temp),
                    after = length(index))
}
i_BC_MVS <- unique(index)
#
i_BC_MVS <- sort(i_BC_MVS)
l_BC_MVS <- vector(mode = "logical", length = dim(DFBaltoCity)[1])
l_BC_MVS <- rep(FALSE, dim(DFBaltoCity)[1])
for(irow in 1:length(i_BC_MVS)){
    l_BC_MVS[i_BC_MVS[irow]] <- TRUE
}
DF_BC_MVS <- DFBaltoCity[l_BC_MVS,]
#
Plot_BC_MVS <- data.frame()
#
L1999 <- DF_BC_MVS$year == 1999L
L2002 <- DF_BC_MVS$year == 2002L
L2005 <- DF_BC_MVS$year == 2005L
L2008 <- DF_BC_MVS$year == 2008L
#
Plot_BC_MVS[1,1] <- 1999L
Plot_BC_MVS[2,1] <- 2002L
Plot_BC_MVS[3,1] <- 2005L
Plot_BC_MVS[4,1] <- 2008L
#
Plot_BC_MVS[1,2] <- sum(DF_BC_MVS$Emissions[L1999], na.rm = TRUE)
Plot_BC_MVS[2,2] <- sum(DF_BC_MVS$Emissions[L2002], na.rm = TRUE)
Plot_BC_MVS[3,2] <- sum(DF_BC_MVS$Emissions[L2005], na.rm = TRUE)
Plot_BC_MVS[4,2] <- sum(DF_BC_MVS$Emissions[L2008], na.rm = TRUE)
#
names(Plot_BC_MVS)[1:2] <- c("Year", "BC_MVS.Emissions")
Plot_BC_MVS[,3] <- log10(Plot_BC_MVS$BC_MVS.Emissions)
names(Plot_BC_MVS)[3] <- "log10.BC_MVS.Emissions"
#
png("plot5.png", width = 960, height = 480, units = "px", res = 72)
library(ggplot2)
plot5 <- ggplot(Plot_BC_MVS, aes(Year, log10.BC_MVS.Emissions))
plot5 <- plot5 + scale_x_continuous(breaks=c(1999, 2002, 2005, 2008))
plot5 <- plot5 + geom_point()
plot5 <- plot5 + geom_line()
plot5 <- plot5 + geom_smooth(method = "lm")
plot5 <- plot5 + geom_smooth(method = "lm", col = "red")
plot5 <- plot5 + labs(title = "Baltimore City Motor Vehicle Sources log10(PM2.5) Emissions")
plot5 <- plot5 + labs(x = "Year (1999, 2002, 2005, 2008)")
plot5 <- plot5 + labs(y = "log10[PM2.5 (tons)]  (red line = linear regression)")
print(plot5)
dev.off()
# 
#-------------------------------------------------------------------------------
#

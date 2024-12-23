#codes for Paddock Soil Data Processing
#updoad soil data from .dat file

#Read the file without headers
tempData <- read.table(file = "Ross T1_SoilData.dat", header = FALSE, sep = "\t")

# Check that the file has been read correctly by looking at the top
head(tempData)
str(tempData)

# Set the column names to the values of the second row
colnames(tempData) <- as.character(tempData[2, ])

# Remove unnecessary rows 
SoilData <- tempData[-c(1, 2, 3, 4), ]

# Confirm the changes by looking at the top of the modified dataframe
head(SoilData)
str(SoilData)


# Create new data frame 'EC' using the column indices
# 
EC <- SoilData[, c(1, 4, 7, 10, 13, 16, 19)]

# Alternatively, if the columns are consecutive, you can use
# EC <- SoilData[, 1:6]

# Check the structure and head of the 'EC' dataset
str(EC)
head(EC)

# Assuming EC is already loaded and we want to exclude rows with any zeros
ECp <- EC[!apply(EC == 0, 1, any), ]

# Check the structure and head of the 'ECp' dataset
str(ECp)
head(ECp)
 
#Write the 'ECp' dataframe to a .dat file with a tab delimiter and save it
write.table(ECp, file = "ECp.dat", sep = "\t", row.names = FALSE, col.names = TRUE)


#Create the plot using ggplot2

#Assuming ECp has been loaded and column 1 is in the expected date format

# Convert the date strings in column 1 to POSIXct datetime objects
ECp[, 1] <- as.POSIXct(ECp[, 1], format = "%d/%m/%Y %H:%M")

#Choose your desired y-axis limits (and change it manually in line below).
y_min <- min(c(ECp[, 2], ECp[, 3], ECp[, 4], ECp[, 5], ECp[, 6], ECp[, 7]), na.rm = TRUE)
y_max <- max(c(ECp[, 2], ECp[, 3], ECp[, 4], ECp[, 5], ECp[, 6], ECp[, 7]), na.rm = TRUE)

#Create a plot with timestamp on the x-axis and adjusted y-axis limits (adjust the y_max and y_min as you wish)
plot(ECp[, 1], ECp[, 2], type = 'l', col = 'red', xlab = 'Timestamp', ylab = 'Values', 
     main = 'Values Over Time', ylim = c(0.1, 0.4))

# Add the rest of the series to the same plot
lines(ECp[, 1], ECp[, 3], col = 'blue')
lines(ECp[, 1], ECp[, 4], col = 'green')
lines(ECp[, 1], ECp[, 5], col = 'purple')
lines(ECp[, 1], ECp[, 6], col = 'orange')
lines(ECp[, 1], ECp[, 7], col = 'yellow')
# Add a legend to distinguish the different lines
legend("topleft", legend = c("Series 2", "Series 3", "Series 4", "Series 5", "Series 6"), 
       col = c('red', 'blue', 'green', 'purple', 'orange', 'yellow'), lty = 1, cex = 0.8)


# make a graph to compare EC across lysimeters










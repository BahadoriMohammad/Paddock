#Author Mohammad Bahadori
#codes for Paddock Soil Data Processing

#uploading soil data from .dat file

#Read the file without headers
tempData <- read.table(file = "Ross T1_SoilData.dat", header = FALSE, sep = "\t")

# Check that the file has been read correctly by looking at the top
head(tempData)
str(tempData)

# Set the column names to the values of the second row
colnames(tempData) <- as.character(tempData[2, ])

# Remove unnecessary rows 
SoilData <- tempData[-c(1, 2, 3, 4), ]

# Reset row names to start at 1 and continue sequentially
rownames(SoilData) <- NULL

# Confirm the changes by looking at the top of the modified dataframe
head(SoilData)
str(SoilData)


# Create new data frame 'EC' using the column indices

EC <- SoilData[, c(1, 4, 7, 10, 13, 16, 19)]

# Alternatively, if the columns are consecutive, you can use
# EC <- SoilData[, 1:6]

# Check the structure and head of the 'EC' dataset
str(EC)
head(EC)


# Check to see whether data are "as characer" or "numeric"
sapply(EC[, 2:7], class)

# Convert all columns 2 to 7 to numeric if needed
EC[, 2:7] <- sapply(EC[, 2:7], function(x) as.numeric(as.character(x)))

# Remove rows that contain any NaN values
# First, loop through all elements and convert any "nan" or "NAN" strings to NA
EC[EC == "nan" | EC == "NAN"] <- NA

# Then, use na.omit() to remove any rows containing NA values in the dataset
EC <- na.omit(EC)

# to exclude rows with any zeros
EC <- EC[!apply(EC == 0, 1, any), ]


# Check the structure and head of the 'ECp' dataset
str(EC)
head(EC)

 
#Write the 'EC' dataframe to a .dat file with a tab delimiter and save it
write.table(EC, file = "EC.dat", sep = "\t", row.names = FALSE, col.names = TRUE)


#Create the plot using ggplot2

# Convert the date strings in column 1 to POSIXct datetime objects
EC[, 1] <- as.POSIXct(EC[, 1], format = "%d/%m/%Y %H:%M")

#Choose your desired y-axis limits (and change it manually in line below).
#y_min <- min(c(EC[, 2], EC[, 3], EC[, 4], EC[, 5], EC[, 6], EC[, 7]), na.rm = TRUE)
#y_max <- max(c(EC[, 2], EC[, 3], EC[, 4], EC[, 5], EC[, 6], EC[, 7]), na.rm = TRUE)

#Create a plot with timestamp on the x-axis and adjusted y-axis limits (adjust the y_max and y_min as you wish)
plot(EC[, 1], EC[, 2], type = 'l', col = 'red', xlab = 'Timestamp', ylab = 'Values', 
     main = 'Values Over Time', ylim = c(0.1978, 0.3887))

# Add the rest of the series to the same plot
lines(EC[, 1], EC[, 3], col = 'blue')
lines(EC[, 1], EC[, 4], col = 'green')
lines(EC[, 1], EC[, 5], col = 'purple')
lines(EC[, 1], EC[, 6], col = 'orange')
lines(EC[, 1], EC[, 7], col = 'yellow')
# Add a legend to distinguish the different lines
legend("bottomright", legend = c("Series 2", "Series 3", "Series 4", "Series 5", "Series 6"), 
       col = c('red', 'blue', 'green', 'purple', 'orange', 'yellow'), lty = 1, cex = 0.8)


# Do some basic statistics
# Assuming EC is your dataset with named columns

# List of column names from the 2nd to the 7th
columnNames <- names(EC)[2:7]

# Calculate mean and SD for each of the specified columns
means <- sapply(EC[, columnNames], mean, na.rm = TRUE)
sds <- sapply(EC[, columnNames], sd, na.rm = TRUE)

# Create a new dataframe for the results
results <- data.frame(
  Mean = means, 
  SD = sds
)

# Set the row names of the results dataframe to the column names
rownames(results) <- columnNames

# Display the results
print(results)


# make a graph to compare EC across lysimeters

# Assuming 'results' dataframe contains the mean and SD information
# for each of the columns

# Create a bar plot for the mean values
barplot(results$Mean, names.arg = rownames(results), beside = TRUE,
        col = 'lightblue', ylim = c(0, max(results$Mean + results$SD)), 
        main = "Mean values with SD error bars", ylab = "Mean values")

# Add error bars for SD
# Calculate the center position of each bar on the x-axis
bar_centers <- barplot(results$Mean, plot = FALSE)

# Add the error bars using the arrow function
arrows(x0 = bar_centers, y0 = results$Mean - results$SD, 
       x1 = bar_centers, y1 = results$Mean + results$SD, 
       angle = 90, code = 3, length = 0.1, col = 'darkred')


# Do ANOVA

# Ensure that you have the reshape2 package installed
install.packages("reshape2")
library(reshape2)

#Melt the data frame from wide format to long format for ANOVA
longEC <- melt(EC, id.vars = "TIMESTAMP", measure.vars = colnames(EC)[2:7])

# Perform ANOVA
aov_results <- aov(value ~ variable, data = longEC)

# Display the summary of the ANOVA
summary(aov_results)

# Post-hoc test if ANOVA significant
if (summary(aov_results)[[1]][["Pr(>F)"]][1] < 0.05) {
  TukeyHSD(aov_results)
} else {
  print("ANOVA is not significant; post-hoc analysis is not applicable.")
}


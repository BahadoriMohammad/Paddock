#Author Mohammad Bahadori
#codes for Lysimeter Data Processing

#Psckages to install/call
#install.packages("ggplot2")
#install.packages("reshape2")
#library(ggplot2)
#library(reshape2)


#uploading soil data from .dat file

#Read the file without headers
tempData <- read.table(file = "Ross T1_LysimeterData.dat", header = FALSE, sep = ",", fill = TRUE)


# Check that the file has been read correctly by looking at the top
head(tempData)
str(tempData)

# Set the column names to the values of the second row
colnames(tempData) <- as.character(tempData[2, ])

# Remove unnecessary rows 
LysData <- tempData[-c(1, 2, 3, 4), ]

# Reset row names to start at 1 and continue sequentially
rownames(LysData) <- NULL

# Confirm the changes by looking at the top of the modified dataframe
head(LysData)
str(LysData)


# Create new data frame 'EC' using the column indices
Lys_mod_depth <- LysData[, c(1, 4, 10, 16, 22, 28, 34)]
Lys_Temp <- LysData[, c(1, 5, 11, 17, 23, 29, 35)]
Lys_EC <- LysData[, c(1, 6, 12, 18, 24, 30, 36)]



# Alternatively, if the columns are consecutive, you can use
#Lys_mod_depth <- LysData[, 1:6]
#Lys_Temp <- LysData[, 1:6]
#Lys_EC <- LysData[, 1:6]


# Lets do Lys_mod_depth first

# Check the structure and head of the 'Lys_mod_depth' dataset
str(Lys_mod_depth)
head(Lys_mod_depth)

# Check to see whether data are "as characer" or "numeric"
sapply(Lys_mod_depth[, 2:7], class)

# Convert all columns 2 to 7 to numeric if needed
Lys_mod_depth[, 2:7] <- sapply(Lys_mod_depth[, 2:7], function(x) as.numeric(as.character(x)))

# Remove rows that contain any NaN values

# First, loop through all elements and convert any "nan" or "NAN" strings to NA
Lys_mod_depth[Lys_mod_depth == "nan" | Lys_mod_depth == "NAN"] <- NA
# Then, use na.omit() to remove any rows containing NA values in the dataset
Lys_mod_depth <- na.omit(Lys_mod_depth)

# to exclude rows with any zeros
Lys_mod_depth <- Lys_mod_depth[!apply(Lys_mod_depth == 0, 1, any), ]

# Check the structure and head of the 'Lys_mod_depth' dataset
str(Lys_mod_depth)
head(Lys_mod_depth)

#Write the 'Lys_mod_depth' dataframe to a .dat file with a tab delimiter and save it
write.table(Lys_mod_depth, file = "Lys_mod_depth1.dat", sep = "\t", row.names = FALSE, col.names = TRUE)



# Create the plot using ggplot2

# Convert the date strings in column 1 to POSIXct datetime objects
Lys_mod_depth[, 1] <- as.POSIXct(Lys_mod_depth[, 1], format = "%Y-%m-%d %H:%M")

#Choose your desired y-axis limits (and change it manually in line below).
#y_min <- min(c(Lys_mod_depth[, 2], Lys_mod_depth[, 3], Lys_mod_depth[, 4], Lys_mod_depth[, 5], Lys_mod_depth[, 6], Lys_mod_depth[, 7]), na.rm = TRUE)
#y_max <- max(c(Lys_mod_depth[, 2], Lys_mod_depth[, 3], Lys_mod_depth[, 4], Lys_mod_depth[, 5], Lys_mod_depth[, 6], Lys_mod_depth[, 7]), na.rm = TRUE)

#Create a plot with timestamp on the x-axis and adjusted y-axis limits (adjust the y_max and y_min as you wish)
plot(Lys_mod_depth[, 1], Lys_mod_depth[, 2], type = 'l', col = 'red', xlab = 'Timestamp', ylab = 'Values',
     main = 'Lys_mod_depth Over Time', ylim = c(0, 2200))

# Add the rest of the series to the same plot
lines(Lys_mod_depth[, 1], Lys_mod_depth[, 3], col = 'blue')
lines(Lys_mod_depth[, 1], Lys_mod_depth[, 4], col = 'green')
lines(Lys_mod_depth[, 1], Lys_mod_depth[, 5], col = 'purple')
lines(Lys_mod_depth[, 1], Lys_mod_depth[, 6], col = 'orange')
lines(Lys_mod_depth[, 1], Lys_mod_depth[, 7], col = 'yellow')
# Add a legend to distinguish the different lines
legend("bottomright", legend = c("Lys_mod_depth 1", "Lys_mod_depth 2", "Lys_mod_depth 3", "Lys_mod_depth 4", "Lys_mod_depth 5", "Lys_mod_depth 6"),
       col = c('red', 'blue', 'green', 'purple', 'orange', 'yellow'), lty = 1, cex = 0.8)



# Do some basic statistics
# Assuming Lys_mod_depth is your dataset with named columns

# List of column names from the 2nd to the 7th
columnNames_Lys_mod_depth <- names(Lys_mod_depth)[2:7]

# Calculate mean and SD for each of the specified columns
means_Lys_mod_depth <- sapply(Lys_mod_depth[, columnNames_Lys_mod_depth], mean, na.rm = TRUE)
sds_Lys_mod_depth <- sapply(Lys_mod_depth[, columnNames_Lys_mod_depth], sd, na.rm = TRUE)

# Create a new dataframe for the results
results_Lys_mod_depth <- data.frame(
  Mean_Lys_mod_depth = means_Lys_mod_depth,
  SD_Lys_mod_depth = sds_Lys_mod_depth
)

# Set the row names of the results dataframe to the column names
rownames(results_Lys_mod_depth) <- columnNames_Lys_mod_depth

# Display the results
print(results_Lys_mod_depth)

# make a graph to compare Lys_mod_depth across lysimeters

# Assuming 'results' dataframe contains the mean and SD information
# for each of the columns

# Create a bar plot for the mean values
barplot(results_Lys_mod_depth$Mean, names.arg = rownames(results_Lys_mod_depth), beside = TRUE,
        col = 'lightblue', ylim = c(0, max(results_Lys_mod_depth$Mean + results_Lys_mod_depth$SD)),
        main = "Mean values with SD error bars", ylab = "Mean values")

# Add error bars for SD
# Calculate the center position of each bar on the x-axis
bar_centers_Lys_mod_depth <- barplot(results_Lys_mod_depth$Mean, plot = FALSE)

# Add the error bars using the arrow function
arrows(x0 = bar_centers_Lys_mod_depth, y0 = results_Lys_mod_depth$Mean - results_Lys_mod_depth$SD,
       x1 = bar_centers_Lys_mod_depth, y1 = results_Lys_mod_depth$Mean + results_Lys_mod_depth$SD,
       angle = 90, code = 3, length = 0.1, col = 'darkred')



# Do ANOVA
# Ensure that you have the reshape2 package installed

#Melt the data frame from wide format to long format for ANOVA
longLys_mod_depth <- melt(Lys_mod_depth, id.vars = "TIMESTAMP", measure.vars = colnames(Lys_mod_depth)[2:7])

# Perform ANOVA
aov_results_Lys_mod_depth <- aov(value ~ variable, data = longLys_mod_depth)

# Display the summary of the ANOVA
summary(aov_results_Lys_mod_depth)

# Post-hoc test if ANOVA significant
if (summary(aov_results_Lys_mod_depth)[[1]][["Pr(>F)"]][1] < 0.05) {
  TukeyHSD(aov_results_Lys_mod_depth)
} else {
  print("ANOVA is not significant; post-hoc analysis is not applicable.")
}



# Now let's do Lys_Temp

# Check the structure and head of the 'Lys_Temp' dataset
str(Lys_Temp)
head(Lys_Temp)

# Check to see whether data are "as characer" or "numeric"
sapply(Lys_Temp[, 2:7], class)

# Convert all columns 2 to 7 to numeric if needed
Lys_Temp[, 2:7] <- sapply(Lys_Temp[, 2:7], function(x) as.numeric(as.character(x)))

# Remove rows that contain any NaN values
# First, loop through all elements and convert any "nan" or "NAN" strings to NA
Lys_Temp[Lys_Temp == "nan" | Lys_Temp == "NAN"] <- NA

# Then, use na.omit() to remove any rows containing NA values in the dataset
Lys_Temp <- na.omit(Lys_Temp)

# to exclude rows with any zeros
Lys_Temp <- Lys_Temp[!apply(Lys_Temp == 0, 1, any), ]


# Check the structure and head of the 'Lys_Temp' dataset
str(Lys_Temp)
head(Lys_Temp)

# Write the 'Lys_Temp' dataframe to a .dat file with a tab delimiter and save it
write.table(Lys_Temp, file = "Lys_Temp1.dat", sep = "\t", row.names = FALSE, col.names = TRUE)

# Create the plot using ggplot2

# Convert the date strings in column 1 to POSIXct datetime objects
Lys_Temp[, 1] <- as.POSIXct(Lys_Temp[, 1], format = "%Y-%m-%d %H:%M")

# Choose your desired y-axis limits (and change it manually in line below).
#y_min <- min(c(Lys_Temp[, 2], Lys_Temp[, 3], Lys_Temp[, 4], Lys_Temp[, 5], Lys_Temp[, 6], Lys_Temp[, 7]), na.rm = TRUE)
#y_max <- max(c(Lys_Temp[, 2], Lys_Temp[, 3], Lys_Temp[, 4], Lys_Temp[, 5], Lys_Temp[, 6], Lys_Temp[, 7]), na.rm = TRUE)

# Create a plot with timestamp on the x-axis and adjusted y-axis limits (adjust the y_max and y_min as you wish)
plot(Lys_Temp[, 1], Lys_Temp[, 2], type = 'l', col = 'red', xlab = 'Timestamp', ylab = 'Values', 
     main = 'Lys_Temp Over Time', ylim = c(23, 29))

# Add the rest of the series to the same plot
lines(Lys_Temp[, 1], Lys_Temp[, 3], col = 'blue')
lines(Lys_Temp[, 1], Lys_Temp[, 4], col = 'green')
lines(Lys_Temp[, 1], Lys_Temp[, 5], col = 'purple')
lines(Lys_Temp[, 1], Lys_Temp[, 6], col = 'orange')
lines(Lys_Temp[, 1], Lys_Temp[, 7], col = 'yellow')
# Add a legend to distinguish the different lines
legend("bottomright", legend = c("Lys_Temp 1", "Lys_Temp 2", "Lys_Temp 3", "Lys_Temp 4", "Lys_Temp 5", "Lys_Temp 6"), 
       col = c('red', 'blue', 'green', 'purple', 'orange', 'yellow'), lty = 1, cex = 0.8)

# Do some basic statistics
# Assuming Lys_Temp is your dataset with named columns

# List of column names from the 2nd to the 7th
columnNames_Lys_Temp <- names(Lys_Temp)[2:7]

# Calculate mean and SD for each of the specified columns
means_Lys_Temp <- sapply(Lys_Temp[, columnNames_Lys_Temp], mean, na.rm = TRUE)
sds_Lys_Temp <- sapply(Lys_Temp[, columnNames_Lys_Temp], sd, na.rm = TRUE)

# Create a new dataframe for the results
results_Lys_Temp <- data.frame(
  Mean_Lys_Temp = means_Lys_Temp,
  SD_Lys_Temp = sds_Lys_Temp
)

# Set the row names of the results dataframe to the column names
rownames(results_Lys_Temp) <- columnNames_Lys_Temp

# Display the results
print(results_Lys_Temp)

# Make a graph to compare Lys_Temp across lysimeters

# Assuming 'results' dataframe contains the mean and SD information
# for each of the columns

# Create a bar plot for the mean values
barplot(results_Lys_Temp$Mean_Lys_Temp, names.arg = rownames(results_Lys_Temp), beside = TRUE,
        col = 'lightblue', ylim = c(0, max(results_Lys_Temp$Mean_Lys_Temp + results_Lys_Temp$SD_Lys_Temp)),
        main = "Mean values with SD error bars", ylab = "Mean values")

# Add error bars for SD
# Calculate the center position of each bar on the x-axis
bar_centers_Lys_Temp <- barplot(results_Lys_Temp$Mean_Lys_Temp, plot = FALSE)

# Add the error bars using the arrow function
arrows(x0 = bar_centers_Lys_Temp, y0 = results_Lys_Temp$Mean_Lys_Temp - results_Lys_Temp$SD_Lys_Temp,
       x1 = bar_centers_Lys_Temp, y1 = results_Lys_Temp$Mean_Lys_Temp + results_Lys_Temp$SD_Lys_Temp,
       angle = 90, code = 3, length = 0.1, col = 'darkred')

# Do ANOVA

# Ensure that you have the reshape2 package installed
# Commented out because it should be run manually only if needed

# Melt the data frame from wide format to long format for ANOVA
longLys_Temp <- melt(Lys_Temp, id.vars = "TIMESTAMP", measure.vars = colnames(Lys_Temp)[2:7])

# Perform ANOVA
aov_results_Lys_Temp <- aov(value ~ variable, data = longLys_Temp)

# Display the summary of the ANOVA
summary(aov_results_Lys_Temp)

# Post-hoc test if ANOVA significant
if (summary(aov_results_Lys_Temp)[[1]][["Pr(>F)"]][1] < 0.05) {
  TukeyHSD(aov_results_Lys_Temp)
} else {
  print("ANOVA is not significant; post-hoc analysis is not applicable.")
}




# Finally let's do Lys_EC

# Check the structure and head of the 'Lys_EC' dataset
str(Lys_EC)
head(Lys_EC)

# Check to see whether data are "as characer" or "numeric"
sapply(Lys_EC[, 2:7], class)

# Convert all columns 2 to 7 to numeric if needed
Lys_EC[, 2:7] <- sapply(Lys_EC[, 2:7], function(x) as.numeric(as.character(x)))

# Remove rows that contain any NaN values
# First, loop through all elements and convert any "nan" or "NAN" strings to NA
Lys_EC[Lys_EC == "nan" | Lys_EC == "NAN"] <- NA

# Then, use na.omit() to remove any rows containing NA values in the dataset
Lys_EC <- na.omit(Lys_EC)

# to exclude rows with any zeros
Lys_EC <- Lys_EC[!apply(Lys_EC == 0, 1, any), ]

# Check the structure and head of the 'Lys_EC' dataset
str(Lys_EC)
head(Lys_EC)

#Write the 'Lys_EC' dataframe to a .dat file with a tab delimiter and save it
write.table(Lys_EC, file = "Lys_EC1.dat", sep = "\t", row.names = FALSE, col.names = TRUE)

#Create the plot using ggplot2

# Convert the date strings in column 1 to POSIXct datetime objects
Lys_EC[, 1] <- as.POSIXct(Lys_EC[, 1], format = "%Y-%m-%d %H:%M")

#Choose your desired y-axis limits (and change it manually in line below).
#y_min <- min(c(Lys_EC[, 2], Lys_EC[, 3], Lys_EC[, 4], Lys_EC[, 5], Lys_EC[, 6], Lys_EC[, 7]), na.rm = TRUE)
#y_max <- max(c(Lys_EC[, 2], Lys_EC[, 3], Lys_EC[, 4], Lys_EC[, 5], Lys_EC[, 6], Lys_EC[, 7]), na.rm = TRUE)

#Create a plot with timestamp on the x-axis and adjusted y-axis limits (adjust the y_max and y_min as you wish)
plot(Lys_EC[, 1], Lys_EC[, 2], type = 'l', col = 'red', xlab = 'Timestamp', ylab = 'Values',
     main = 'Lys_EC Over Time', ylim = c(0, 1200))

# Add the rest of the series to the same plot
lines(Lys_EC[, 1], Lys_EC[, 3], col = 'blue')
lines(Lys_EC[, 1], Lys_EC[, 4], col = 'green')
lines(Lys_EC[, 1], Lys_EC[, 5], col = 'purple')
lines(Lys_EC[, 1], Lys_EC[, 6], col = 'orange')
lines(Lys_EC[, 1], Lys_EC[, 7], col = 'yellow')
# Add a legend to distinguish the different lines
legend("bottomright", legend = c("Lys_EC 1", "Lys_EC 2", "Lys_EC 3", "Lys_EC 4", "Lys_EC 5", "Lys_EC 6"),
       col = c('red', 'blue', 'green', 'purple', 'orange', 'yellow'), lty = 1, cex = 0.8)

# Do some basic statistics
# Assuming Lys_EC is your dataset with named columns

# List of column names from the 2nd to the 7th
columnNames_Lys_EC <- names(Lys_EC)[2:7]

# Calculate mean and SD for each of the specified columns
means_Lys_EC <- sapply(Lys_EC[, columnNames_Lys_EC], mean, na.rm = TRUE)
sds_Lys_EC <- sapply(Lys_EC[, columnNames_Lys_EC], sd, na.rm = TRUE)

# Create a new dataframe for the results
results_Lys_EC <- data.frame(
  Mean_Lys_EC = means_Lys_EC,
  SD_Lys_EC = sds_Lys_EC
)

# Set the row names of the results dataframe to the column names
rownames(results_Lys_EC) <- columnNames_Lys_EC

# Display the results
print(results_Lys_EC)

# make a graph to compare Lys_EC across lysimeters

# Assuming 'results' dataframe contains the mean and SD information
# for each of the columns

# Create a bar plot for the mean values
barplot(results_Lys_EC$Mean_Lys_EC, names.arg = rownames(results_Lys_EC), beside = TRUE,
        col = 'lightblue', ylim = c(0, max(results_Lys_EC$Mean_Lys_EC + results_Lys_EC$SD_Lys_EC)),
        main = "Mean values with SD error bars", ylab = "Mean values")

# Add error bars for SD
# Calculate the center position of each bar on the x-axis
bar_centers_Lys_EC <- barplot(results_Lys_EC$Mean_Lys_EC, plot = FALSE)

# Add the error bars using the arrow function
arrows(x0 = bar_centers_Lys_EC, y0 = results_Lys_EC$Mean_Lys_EC - results_Lys_EC$SD_Lys_EC,
       x1 = bar_centers_Lys_EC, y1 = results_Lys_EC$Mean_Lys_EC + results_Lys_EC$SD_Lys_EC,
       angle = 90, code = 3, length = 0.1, col = 'darkred')

# Do ANOVA

# Ensure that you have the reshape2 package installed

#Melt the data frame from wide format to long format for ANOVA
longLys_EC <- melt(Lys_EC, id.vars = "TIMESTAMP", measure.vars = colnames(Lys_EC)[2:7])

# Perform ANOVA
aov_results_Lys_EC <- aov(value ~ variable, data = longLys_EC)

# Display the summary of the ANOVA
summary(aov_results_Lys_EC)

# Post-hoc test if ANOVA significant
if (summary(aov_results_Lys_EC)[[1]][["Pr(>F)"]][1] < 0.05) {
  TukeyHSD(aov_results_Lys_EC)
} else {
  print("ANOVA is not significant; post-hoc analysis is not applicable.")
}


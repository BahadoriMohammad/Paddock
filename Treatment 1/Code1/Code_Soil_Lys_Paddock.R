#Author Mohammad Bahadori
#codes for Paddock Soil and Lysimeter Data Processing

####Packages to install####
install.packages("ggplot2")
install.packages("reshape2")
###Packages to call####
library(ggplot2)
library(reshape2)

#################################################### Soil Data Processing########################################


###################################  Treatment 1#####################################

#uploading soil data from .dat file

#Read the "Ross T1_SoilData.dat" file without headers
tempData <- read.table(file = "Ross T1_SoilData.dat", header = FALSE, sep = ",", fill = TRUE)


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
VWC <- SoilData[, c(1, 3, 6, 9, 12, 15, 18)]
EC <- SoilData[, c(1, 4, 7, 10, 13, 16, 19)]
Temp <- SoilData[, c(1, 5, 8, 11, 14, 17, 20)]

SV_VWC <- SoilData[, c(1, 21, 25, 29, 33, 37, 41, 45, 49, 53)]
SV_EC <- SoilData[, c(1, 24, 28, 32, 36, 40, 44, 48, 52, 56)]
SV_Temp <- SoilData[, c(1, 23, 27, 31, 35, 39, 43, 47, 51, 55)]


# Alternatively, if the columns are consecutive, you can use
#VWC <- SoilData[, 1:6]
# EC <- SoilData[, 1:6]
#Temp <- SoilData[, 1:6]



###########VWC  ###########

# Check the structure and head of the 'VWC' dataset
str(VWC)
head(VWC)

# Check to see whether data are "as characer" or "numeric"
sapply(VWC[, 2:7], class)

# Convert all columns 2 to 7 to numeric if needed
VWC[, 2:7] <- sapply(VWC[, 2:7], function(x) as.numeric(as.character(x)))

# Remove rows that contain any NaN values

# First, loop through all elements and convert any "nan" or "NAN" strings to NA
VWC[VWC == "nan" | VWC == "NAN"] <- NA
# Then, use na.omit() to remove any rows containing NA values in the dataset
VWC <- na.omit(VWC)

# to exclude rows with any zeros
VWC <- VWC[!apply(VWC == 0, 1, any), ]

# Check the structure and head of the 'VWCp' dataset
str(VWC)
head(VWC)

#Write the 'VWC' dataframe to a .dat file with a tab delimiter and save it
write.table(VWC, file = "VWC1.dat", sep = "\t", row.names = FALSE, col.names = TRUE)



#Create the plot using ggplot2

# Convert the date strings in column 1 to POSIXct datetime objects
VWC[, 1] <- as.POSIXct(VWC[, 1], format = "%Y-%m-%d %H:%M")

#Choose your desired y-axis limits (and change it manually in line below).
#y_min <- min(c(VWC[, 2], VWC[, 3], VWC[, 4], VWC[, 5], VWC[, 6], VWC[, 7]), na.rm = TRUE)
#y_max <- max(c(VWC[, 2], VWC[, 3], VWC[, 4], VWC[, 5], VWC[, 6], VWC[, 7]), na.rm = TRUE)

#Create a plot with timestamp on the x-axis and adjusted y-axis limits (adjust the y_max and y_min as you wish)
plot(VWC[, 1], VWC[, 2], type = 'l', col = 'red', xlab = 'Timestamp', ylab = 'Values',
     main = 'VWC Over Time', ylim = c(0.30, 0.45))

# Add the rest of the series to the same plot
lines(VWC[, 1], VWC[, 3], col = 'blue')
lines(VWC[, 1], VWC[, 4], col = 'green')
lines(VWC[, 1], VWC[, 5], col = 'purple')
lines(VWC[, 1], VWC[, 6], col = 'orange')
lines(VWC[, 1], VWC[, 7], col = 'yellow')
# Add a legend to distinguish the different lines
legend("bottomright", legend = c("VWC 1", "VWC 2", "VWC 3", "VWC 4", "VWC 5", "VWC 6"),
       col = c('red', 'blue', 'green', 'purple', 'orange', 'yellow'), lty = 1, cex = 0.8)



# Do some basic statistics
# Assuming VWC is your dataset with named columns

# List of column names from the 2nd to the 7th
columnNames_VWC <- names(VWC)[2:7]

# Calculate mean and SD for each of the specified columns
means_VWC <- sapply(VWC[, columnNames_VWC], mean, na.rm = TRUE)
sds_VWC <- sapply(VWC[, columnNames_VWC], sd, na.rm = TRUE)

# Create a new dataframe for the results
results_VWC <- data.frame(
  Mean_VWC = means_VWC,
  SD_VWC = sds_VWC
)

# Set the row names of the results dataframe to the column names
rownames(results_VWC) <- columnNames_VWC

# Display the results
print(results_VWC)

# make a graph to compare VWC across lysimeters

# Assuming 'results' dataframe contains the mean and SD information
# for each of the columns

# Create a bar plot for the mean values
barplot(results_VWC$Mean, names.arg = rownames(results_VWC), beside = TRUE,
        col = 'lightblue', ylim = c(0, max(results_VWC$Mean + results_VWC$SD)),
        main = "Mean values with SD error bars", ylab = "Mean values")

# Add error bars for SD
# Calculate the center position of each bar on the x-axis
bar_centers_VWC <- barplot(results_VWC$Mean, plot = FALSE)

# Add the error bars using the arrow function
arrows(x0 = bar_centers_VWC, y0 = results_VWC$Mean - results_VWC$SD,
       x1 = bar_centers_VWC, y1 = results_VWC$Mean + results_VWC$SD,
       angle = 90, code = 3, length = 0.1, col = 'darkred')



# Do ANOVA
# Ensure that you have the reshape2 package installed

#Melt the data frame from wide format to long format for ANOVA
longVWC <- melt(VWC, id.vars = "TIMESTAMP", measure.vars = colnames(VWC)[2:7])

# Perform ANOVA
aov_results_VWC <- aov(value ~ variable, data = longVWC)

# Display the summary of the ANOVA
summary(aov_results_VWC)

# Post-hoc test if ANOVA significant
if (summary(aov_results_VWC)[[1]][["Pr(>F)"]][1] < 0.05) {
  TukeyHSD(aov_results_VWC)
} else {
  print("ANOVA is not significant; post-hoc analysis is not applicable.")
}


###########EC #####

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
write.table(EC, file = "EC1.dat", sep = "\t", row.names = FALSE, col.names = TRUE)


#Create the plot using ggplot2

# Convert the date strings in column 1 to POSIXct datetime objects
EC[, 1] <- as.POSIXct(EC[, 1], format = "%Y-%m-%d %H:%M")

#Choose your desired y-axis limits (and change it manually in line below).
#y_min <- min(c(EC[, 2], EC[, 3], EC[, 4], EC[, 5], EC[, 6], EC[, 7]), na.rm = TRUE)
#y_max <- max(c(EC[, 2], EC[, 3], EC[, 4], EC[, 5], EC[, 6], EC[, 7]), na.rm = TRUE)

#Create a plot with timestamp on the x-axis and adjusted y-axis limits (adjust the y_max and y_min as you wish)
plot(EC[, 1], EC[, 2], type = 'l', col = 'red', xlab = 'Timestamp', ylab = 'Values', 
     main = 'EC Over Time', ylim = c(0.1978, 0.3887))

# Add the rest of the series to the same plot
lines(EC[, 1], EC[, 3], col = 'blue')
lines(EC[, 1], EC[, 4], col = 'green')
lines(EC[, 1], EC[, 5], col = 'purple')
lines(EC[, 1], EC[, 6], col = 'orange')
lines(EC[, 1], EC[, 7], col = 'yellow')
# Add a legend to distinguish the different lines
legend("bottomright", legend = c("EC 1", "EC 2", "EC 3", "EC 4", "EC 5", "EC 6"), 
       col = c('red', 'blue', 'green', 'purple', 'orange', 'yellow'), lty = 1, cex = 0.8)


# Do some basic statistics
# Assuming EC is your dataset with named columns

# List of column names from the 2nd to the 7th
columnNames_EC <- names(EC)[2:7]

# Calculate mean and SD for each of the specified columns
means_EC <- sapply(EC[, columnNames_EC], mean, na.rm = TRUE)
sds_EC <- sapply(EC[, columnNames_EC], sd, na.rm = TRUE)

# Create a new dataframe for the results
results_EC <- data.frame(
  Mean_EC = means_EC,
  SD_EC = sds_EC
)

# Set the row names of the results dataframe to the column names
rownames(results_EC) <- columnNames_EC

# Display the results
print(results_EC)

# make a graph to compare EC across lysimeters

# Assuming 'results' dataframe contains the mean and SD information
# for each of the columns

# Create a bar plot for the mean values
barplot(results_EC$Mean_EC, names.arg = rownames(results_EC), beside = TRUE,
        col = 'lightblue', ylim = c(0, max(results_EC$Mean_EC + results_EC$SD_EC)),
        main = "Mean values with SD error bars", ylab = "Mean values")

# Add error bars for SD
# Calculate the center position of each bar on the x-axis
bar_centers_EC <- barplot(results_EC$Mean_EC, plot = FALSE)

# Add the error bars using the arrow function
arrows(x0 = bar_centers_EC, y0 = results_EC$Mean_EC - results_EC$SD_EC,
       x1 = bar_centers_EC, y1 = results_EC$Mean_EC + results_EC$SD_EC,
       angle = 90, code = 3, length = 0.1, col = 'darkred')


# Do ANOVA

# Ensure that you have the reshape2 package installed
# Commented out because it should be run manually only if needed

#Melt the data frame from wide format to long format for ANOVA
longEC <- melt(EC, id.vars = "TIMESTAMP", measure.vars = colnames(EC)[2:7])

# Perform ANOVA
aov_results_EC <- aov(value ~ variable, data = longEC)

# Display the summary of the ANOVA
summary(aov_results_EC)

# Post-hoc test if ANOVA significant
if (summary(aov_results_EC)[[1]][["Pr(>F)"]][1] < 0.05) {
  TukeyHSD(aov_results_EC)
} else {
  print("ANOVA is not significant; post-hoc analysis is not applicable.")
}


###########Tempe #####

# Check the structure and head of the 'Temp' dataset
str(Temp)
head(Temp)

# Check to see whether data are "as characer" or "numeric"
sapply(Temp[, 2:7], class)

# Convert all columns 2 to 7 to numeric if needed
Temp[, 2:7] <- sapply(Temp[, 2:7], function(x) as.numeric(as.character(x)))

# Remove rows that contain any NaN values
# First, loop through all elements and convert any "nan" or "NAN" strings to NA
Temp[Temp == "nan" | Temp == "NAN"] <- NA

# Then, use na.omit() to remove any rows containing NA values in the dataset
Temp <- na.omit(Temp)

# to exclude rows with any zeros
Temp <- Temp[!apply(Temp == 0, 1, any), ]

# Check the structure and head of the 'Tempp' dataset
str(Temp)
head(Temp)

#Write the 'Temp' dataframe to a .dat file with a tab delimiter and save it
write.table(Temp, file = "Temp1.dat", sep = "\t", row.names = FALSE, col.names = TRUE)

#Create the plot using ggplot2

# Convert the date strings in column 1 to POSIXct datetime objects
Temp[, 1] <- as.POSIXct(Temp[, 1], format = "%Y-%m-%d %H:%M")

#Choose your desired y-axis limits (and change it manually in line below).
#y_min <- min(c(Temp[, 2], Temp[, 3], Temp[, 4], Temp[, 5], Temp[, 6], Temp[, 7]), na.rm = TRUE)
#y_max <- max(c(Temp[, 2], Temp[, 3], Temp[, 4], Temp[, 5], Temp[, 6], Temp[, 7]), na.rm = TRUE)

#Create a plot with timestamp on the x-axis and adjusted y-axis limits (adjust the y_max and y_min as you wish)
plot(Temp[, 1], Temp[, 2], type = 'l', col = 'red', xlab = 'Timestamp', ylab = 'Values',
     main = 'Temperature Over Time', ylim = c(23, 30))

# Add the rest of the series to the same plot
lines(Temp[, 1], Temp[, 3], col = 'blue')
lines(Temp[, 1], Temp[, 4], col = 'green')
lines(Temp[, 1], Temp[, 5], col = 'purple')
lines(Temp[, 1], Temp[, 6], col = 'orange')
lines(Temp[, 1], Temp[, 7], col = 'yellow')
# Add a legend to distinguish the different lines
legend("bottomright", legend = c("Temp 1", "Temp 2", "Temp 3", "Temp 4", "Temp 5", "Temp 6"),
       col = c('red', 'blue', 'green', 'purple', 'orange', 'yellow'), lty = 1, cex = 0.8)

# Do some basic statistics
# Assuming Temp is your dataset with named columns

# List of column names from the 2nd to the 7th
columnNames_Temp <- names(Temp)[2:7]

# Calculate mean and SD for each of the specified columns
means_Temp <- sapply(Temp[, columnNames_Temp], mean, na.rm = TRUE)
sds_Temp <- sapply(Temp[, columnNames_Temp], sd, na.rm = TRUE)

# Create a new dataframe for the results
results_Temp <- data.frame(
  Mean_Temp = means_Temp,
  SD_Temp = sds_Temp
)

# Set the row names of the results dataframe to the column names
rownames(results_Temp) <- columnNames_Temp

# Display the results
print(results_Temp)

# make a graph to compare Temp across lysimeters

# Assuming 'results' dataframe contains the mean and SD information
# for each of the columns

# Create a bar plot for the mean values
barplot(results_Temp$Mean_Temp, names.arg = rownames(results_Temp), beside = TRUE,
        col = 'lightblue', ylim = c(0, max(results_Temp$Mean_Temp + results_Temp$SD_Temp)),
        main = "Mean values with SD error bars", ylab = "Mean values")

# Add error bars for SD
# Calculate the center position of each bar on the x-axis
bar_centers_Temp <- barplot(results_Temp$Mean_Temp, plot = FALSE)

# Add the error bars using the arrow function
arrows(x0 = bar_centers_Temp, y0 = results_Temp$Mean_Temp - results_Temp$SD_Temp,
       x1 = bar_centers_Temp, y1 = results_Temp$Mean_Temp + results_Temp$SD_Temp,
       angle = 90, code = 3, length = 0.1, col = 'darkred')

# Do ANOVA

# Ensure that you have the reshape2 package installed

#Melt the data frame from wide format to long format for ANOVA
longTemp <- melt(Temp, id.vars = "TIMESTAMP", measure.vars = colnames(Temp)[2:7])

# Perform ANOVA
aov_results_Temp <- aov(value ~ variable, data = longTemp)

# Display the summary of the ANOVA
summary(aov_results_Temp)

# Post-hoc test if ANOVA significant
if (summary(aov_results_Temp)[[1]][["Pr(>F)"]][1] < 0.05) {
  TukeyHSD(aov_results_Temp)
} else {
  print("ANOVA is not significant; post-hoc analysis is not applicable.")
}


#################SV_VWC##########
# Check the structure and head of the 'SV_VWC' dataset
str(SV_VWC)
head(SV_VWC)

# Check to see whether data are "as characer" or "numeric"
sapply(SV_VWC[, 2:10], class)

# Convert all columns 2 to 10 to numeric if needed
SV_VWC[, 2:10] <- sapply(SV_VWC[, 2:10], function(x) as.numeric(as.character(x)))

# Remove rows that contain any NaN values

# First, loop through all elements and convert any "nan" or "NAN" strings to NaN
SV_VWC[SV_VWC == "nan" | SV_VWC == "NAN"] <- NaN
# Then, use na.omit() to remove any rows containing NaN values in the dataset
#SV_VWC <- na.omit(SV_VWC)

# to exclude rows with any zeros
#SV_VWC <- SV_VWC[!apply(SV_VWC == 0, 1, any), ]

# Check the structure and head of the 'SV_VWCp' dataset
str(SV_VWC)
head(SV_VWC)

#Write the 'SV_VWC' dataframe to a .dat file with a tab delimiter and save it
write.table(SV_VWC, file = "SV_VWC1.dat", sep = "\t", row.names = FALSE, col.names = TRUE)

#Create the plot using ggplot2

# Convert the date strings in column 1 to POSIXct datetime objects
SV_VWC[, 1] <- as.POSIXct(SV_VWC[, 1], format = "%Y-%m-%d %H:%M")

#Choose your desired y-axis limits (and change it manually in line below).
#y_min <- min(c(SV_VWC[, 2], SV_VWC[, 3], SV_VWC[, 4], SV_VWC[, 5], SV_VWC[, 6], SV_VWC[, 7], SV_VWC[, 8], SV_VWC[, 9], SV_VWC[, 10])), na.rm = TRUE)
#y_max <- max(c(SV_VWC[, 2], SV_VWC[, 3], SV_VWC[, 4], SV_VWC[, 5], SV_VWC[, 6], SV_VWC[, 7], SV_VWC[, 8], SV_VWC[, 9], SV_VWC[, 10]), na.rm = TRUE)

#Create a plot with timestamp on the x-axis and adjusted y-axis limits (adjust the y_max and y_min as you wish)
plot(SV_VWC[, 1], SV_VWC[, 2], type = 'l', col = 'red', xlab = 'Timestamp', ylab = 'Values',
     main = 'SV_VWC Over Time', ylim = c(0.30, 0.45))

# Add the rest of the series to the same plot
lines(SV_VWC[, 1], SV_VWC[, 3], col = 'blue')
lines(SV_VWC[, 1], SV_VWC[, 4], col = 'green')
lines(SV_VWC[, 1], SV_VWC[, 5], col = 'purple')
lines(SV_VWC[, 1], SV_VWC[, 6], col = 'orange')
lines(SV_VWC[, 1], SV_VWC[, 7], col = 'yellow')
lines(SV_VWC[, 1], SV_VWC[, 8], col = 'brown')
lines(SV_VWC[, 1], SV_VWC[, 9], col = 'lightblue')
lines(SV_VWC[, 1], SV_VWC[, 10], col = 'lightgreen')
# Add a legend to distinguish the different lines
legend("bottomright", legend = c("SV_VWC 5cm", "SV_VWC 10cm", "SV_VWC 20cm", "SV_VWC 30cm", "SV_VWC 40cm", "SV_VWC 50cm", "SV_VWC 60cm", "SV_VWC 75cm", "SV_VWC 100cm"),
       col = c('red', 'blue', 'green', 'purple', 'orange', 'yellow', 'brown', 'lightblue', 'lightgreen'), lty = 1, cex = 0.8)

# Do some basic statistics
# Assuming SV_VWC is your dataset with named columns

# List of column names from the 2nd to the 7th
columnNames_SV_VWC <- names(SV_VWC)[2:10]

# Calculate mean and SD for each of the specified columns
means_SV_VWC <- sapply(SV_VWC[, columnNames_SV_VWC], mean, na.rm = TRUE)
sds_SV_VWC <- sapply(SV_VWC[, columnNames_SV_VWC], sd, na.rm = TRUE)

# Create a new dataframe for the results
results_SV_VWC <- data.frame(
  Mean_SV_VWC = means_SV_VWC,
  SD_SV_VWC = sds_SV_VWC
)

# Set the row names of the results dataframe to the column names
rownames(results_SV_VWC) <- columnNames_SV_VWC

# Display the results
print(results_SV_VWC)

# make a graph to compare SV_VWC in soil profive

# Assuming 'results' dataframe contains the mean and SD information
# for each of the columns

# Create a bar plot for the mean values
barplot(results_SV_VWC$Mean_SV_VWC, names.arg = rownames(results_SV_VWC), beside = TRUE,
        col = 'lightblue', ylim = c(0, max(results_SV_VWC$Mean_SV_VWC + results_SV_VWC$SD_SV_VWC)),
        main = "Mean values with SD error bars", ylab = "Mean values")

# Add error bars for SD
# Calculate the center position of each bar on the x-axis
bar_centers_SV_VWC <- barplot(results_SV_VWC$Mean_SV_VWC, plot = FALSE)

# Add the error bars using the arrows function
arrows(x0 = bar_centers_SV_VWC, y0 = results_SV_VWC$Mean_SV_VWC - results_SV_VWC$SD_SV_VWC,
       x1 = bar_centers_SV_VWC, y1 = results_SV_VWC$Mean_SV_VWC + results_SV_VWC$SD_SV_VWC,
       angle = 90, code = 3, length = 0.1, col = 'darkred')

# Do ANOVA
# Ensure that you have the reshape2 package installed

##Melt the data frame from wide format to long format for ANOVA
longSV_VWC <- melt(SV_VWC, id.vars = "TIMESTAMP", measure.vars = colnames(SV_VWC)[2:10])

##Perform ANOVA
aov_results_SV_VWC <- aov(value ~ variable, data = longSV_VWC)

##Display the summary of the ANOVA
summary(aov_results_SV_VWC)

##Post-hoc test if ANOVA significant
#if (summary(aov_results_SV_VWC)[[1]][["Pr(>F)"]][1] < 0.05) {
 # TukeyHSD(aov_results_SV_VWC)
#} else {
  #print("ANOVA is not significant; post-hoc analysis is not applicable.")
#}



#################SV_EC#######
# Check the structure and head of the 'SV_EC' dataset
str(SV_EC)
head(SV_EC)

# Check to see whether data are "as character" or "numeric"
sapply(SV_EC[, 2:10], class)

# Convert all columns 2 to 10 to numeric if needed
SV_EC[, 2:10] <- sapply(SV_EC[, 2:10], function(x) as.numeric(as.character(x)))

# Remove rows that contain any NaN values

## First, loop through all elements and convert any "nan" or "NAN" strings to NaN
#SV_EC[SV_EC == "nan" | SV_EC == "NAN"] <- NaN
##Then, use na.omit() to remove any rows containing NaN values in the dataset
#SV_EC <- na.omit(SV_EC)

##to exclude rows with any zeros
#SV_EC <- SV_EC[!apply(SV_EC == 0, 1, any), ]

# Check the structure and head of the 'SV_ECp' dataset
str(SV_EC)
head(SV_EC)

#Write the 'SV_EC' dataframe to a .dat file with a tab delimiter and save it
write.table(SV_EC, file = "SV_EC1.dat", sep = "\t", row.names = FALSE, col.names = TRUE)

# Create the plot using ggplot2

# Convert the date strings in column 1 to POSIXct datetime objects
SV_EC[, 1] <- as.POSIXct(SV_EC[, 1], format = "%Y-%m-%d %H:%M")

#Choose your desired y-axis limits (and change it manually in line below).
#y_min <- min(c(SV_EC[, 2], SV_EC[, 3], SV_EC[, 4], SV_EC[, 5], SV_EC[, 6], SV_EC[, 7], SV_EC[, 8], SV_EC[, 9], SV_EC[, 10]), na.rm = TRUE)
#y_max <- max(c(SV_EC[, 2], SV_EC[, 3], SV_EC[, 4], SV_EC[, 5], SV_EC[, 6], SV_EC[, 7], SV_EC[, 8], SV_EC[, 9], SV_EC[, 10]), na.rm = TRUE)

#Create a plot with timestamp on the x-axis and adjusted y-axis limits (adjust the y_max and y_min as you wish)
plot(SV_EC[, 1], SV_EC[, 2], type = 'l', col = 'red', xlab = 'Timestamp', ylab = 'Values',
     main = 'SV_EC Over Time', ylim = c(0.30, 0.45))

# Add the rest of the series to the same plot
lines(SV_EC[, 1], SV_EC[, 3], col = 'blue')
lines(SV_EC[, 1], SV_EC[, 4], col = 'green')
lines(SV_EC[, 1], SV_EC[, 5], col = 'purple')
lines(SV_EC[, 1], SV_EC[, 6], col = 'orange')
lines(SV_EC[, 1], SV_EC[, 7], col = 'yellow')
lines(SV_EC[, 1], SV_EC[, 8], col = 'brown')
lines(SV_EC[, 1], SV_EC[, 9], col = 'lightblue')
lines(SV_EC[, 1], SV_EC[, 10], col = 'lightgreen')
# Add a legend to distinguish the different lines
legend("bottomright", legend = c("SV_EC 5cm", "SV_EC 10cm", "SV_EC 20cm", "SV_EC 30cm", "SV_EC 40cm", "SV_EC 50cm", "SV_EC 60cm", "SV_EC 75cm", "SV_EC 100cm"),
       col = c('red', 'blue', 'green', 'purple', 'orange', 'yellow', 'brown', 'lightblue', 'lightgreen'), lty = 1, cex = 0.8)

# Do some basic statistics
# Assuming SV_EC is your dataset with named columns

# List of column names from the 2nd to the 7th
columnNames_SV_EC <- names(SV_EC)[2:10]

# Calculate mean and SD for each of the specified columns
means_SV_EC <- sapply(SV_EC[, columnNames_SV_EC], mean, na.rm = TRUE)
sds_SV_EC <- sapply(SV_EC[, columnNames_SV_EC], sd, na.rm = TRUE)

# Create a new dataframe for the results
results_SV_EC <- data.frame(
  Mean_SV_EC = means_SV_EC,
  SD_SV_EC = sds_SV_EC
)

# Set the row names of the results dataframe to the column names
rownames(results_SV_EC) <- columnNames_SV_EC

# Display the results
print(results_SV_EC)

# make a graph to compare SV_EC across paddock

# Assuming 'results' dataframe contains the mean and SD information
# for each of the columns

# Create a bar plot for the mean values
barplot(results_SV_EC$Mean_SV_EC, names.arg = rownames(results_SV_EC), beside = TRUE,
        col = 'lightblue', ylim = c(0, max(results_SV_EC$Mean_SV_EC + results_SV_EC$SD_SV_EC)),
        main = "Mean values with SD error bars", ylab = "Mean values")

# Add error bars for SD
# Calculate the center position of each bar on the x-axis
bar_centers_SV_EC <- barplot(results_SV_EC$Mean_SV_EC, plot = FALSE)

# Add the error bars using the arrows function
arrows(x0 = bar_centers_SV_EC, y0 = results_SV_EC$Mean_SV_EC - results_SV_EC$SD_SV_EC,
       x1 = bar_centers_SV_EC, y1 = results_SV_EC$Mean_SV_EC + results_SV_EC$SD_SV_EC,
       angle = 90, code = 3, length = 0.1, col = 'darkred')

# Do ANOVA
##Ensure that you have the reshape2 package installed

#Melt the data frame from wide format to long format for ANOVA
longSV_EC <- melt(SV_EC, id.vars = "TIMESTAMP", measure.vars = colnames(SV_EC)[2:10])

##Perform ANOVA
aov_results_SV_EC <- aov(value ~ variable, data = longSV_EC)

# Display the summary of the ANOVA
summary(aov_results_SV_EC)

##Post-hoc test if ANOVA significant
#if (summary(aov_results_SV_EC)[[1]][["Pr(>F)"]][1] < 0.05) {
  #TukeyHSD(aov_results_SV_EC)
#} else {
  #print("ANOVA is not significant; post-hoc analysis is not applicable.")
#}



#################SV_Temp##########
# Check the structure and head of the 'SV_Temp' dataset
str(SV_Temp)
head(SV_Temp)

# Check to see whether data are "as character" or "numeric"
sapply(SV_Temp[, 2:10], class)

# Convert all columns 2 to 10 to numeric if needed
SV_Temp[, 2:10] <- sapply(SV_Temp[, 2:10], function(x) as.numeric(as.character(x)))

# Remove rows that contain any NaN values

## First, loop through all elements and convert any "nan" or "NAN" strings to NaN
#SV_Temp[SV_Temp == "nan" | SV_Temp == "NAN"] <- NaN
##Then, use na.omit() to remove any rows containing NaN values in the dataset
#SV_Temp <- na.omit(SV_Temp)

##to exclude rows with any zeros
#SV_Temp <- SV_Temp[!apply(SV_Temp == 0, 1, any), ]

# Check the structure and head of the 'SV_Temp' dataset
str(SV_Temp)
head(SV_Temp)

#Write the 'SV_Temp' dataframe to a .dat file with a tab delimiter and save it
write.table(SV_Temp, file = "SV_Temp1.dat", sep = "\t", row.names = FALSE, col.names = TRUE)

# Create the plot using ggplot2

# Convert the date strings in column 1 to POSIXct datetime objects
SV_Temp[, 1] <- as.POSIXct(SV_Temp[, 1], format = "%Y-%m-%d %H:%M")

#Choose your desired y-axis limits (and change it manually in line below).
#y_min <- min(c(SV_Temp[, 2], SV_Temp[, 3], SV_Temp[, 4], SV_Temp[, 5], SV_Temp[, 6], SV_Temp[, 7], SV_Temp[, 8], SV_Temp[, 9], SV_Temp[, 10]), na.rm = TRUE)
#y_max <- max(c(SV_Temp[, 2], SV_Temp[, 3], SV_Temp[, 4], SV_Temp[, 5], SV_Temp[, 6], SV_Temp[, 7], SV_Temp[, 8], SV_Temp[, 9], SV_Temp[, 10]), na.rm = TRUE)

#Create a plot with timestamp on the x-axis and adjusted y-axis limits (adjust the y_max and y_min as you wish)
plot(SV_Temp[, 1], SV_Temp[, 2], type = 'l', col = 'red', xlab = 'Timestamp', ylab = 'Values',
     main = 'SV_Temp Over Time', ylim = c(0.30, 0.45))

# Add the rest of the series to the same plot
lines(SV_Temp[, 1], SV_Temp[, 3], col = 'blue')
lines(SV_Temp[, 1], SV_Temp[, 4], col = 'green')
lines(SV_Temp[, 1], SV_Temp[, 5], col = 'purple')
lines(SV_Temp[, 1], SV_Temp[, 6], col = 'orange')
lines(SV_Temp[, 1], SV_Temp[, 7], col = 'yellow')
lines(SV_Temp[, 1], SV_Temp[, 8], col = 'brown')
lines(SV_Temp[, 1], SV_Temp[, 9], col = 'lightblue')
lines(SV_Temp[, 1], SV_Temp[, 10], col = 'lightgreen')
# Add a legend to distinguish the different lines
legend("bottomright", legend = c("SV_Temp 5cm", "SV_Temp 10cm", "SV_Temp 20cm", "SV_Temp 30cm", "SV_Temp 40cm", "SV_Temp 50cm", "SV_Temp 60cm", "SV_Temp 75cm", "SV_Temp 100cm"),
       col = c('red', 'blue', 'green', 'purple', 'orange', 'yellow', 'brown', 'lightblue', 'lightgreen'), lty = 1, cex = 0.8)

# Do some basic statistics
# Assuming SV_Temp is your dataset with named columns

# List of column names from the 2nd to the 7th
columnNames_SV_Temp <- names(SV_Temp)[2:10]

# Calculate mean and SD for each of the specified columns
means_SV_Temp <- sapply(SV_Temp[, columnNames_SV_Temp], mean, na.rm = TRUE)
sds_SV_Temp <- sapply(SV_Temp[, columnNames_SV_Temp], sd, na.rm = TRUE)

# Create a new dataframe for the results
results_SV_Temp <- data.frame(
  Mean_SV_Temp = means_SV_Temp,
  SD_SV_Temp = sds_SV_Temp
)

# Set the row names of the results dataframe to the column names
rownames(results_SV_Temp) <- columnNames_SV_Temp

# Display the results
print(results_SV_Temp)

# make a graph to compare SV_Temp across paddock

# Assuming 'results' dataframe contains the mean and SD information
# for each of the columns

# Create a bar plot for the mean values
barplot(results_SV_Temp$Mean_SV_Temp, names.arg = rownames(results_SV_Temp), beside = TRUE,
        col = 'lightblue', ylim = c(0, max(results_SV_Temp$Mean_SV_Temp + results_SV_Temp$SD_SV_Temp)),
        main = "Mean values with SD error bars", ylab = "Mean values")

# Add error bars for SD
# Calculate the center position of each bar on the x-axis
bar_centers_SV_Temp <- barplot(results_SV_Temp$Mean_SV_Temp, plot = FALSE)

# Add the error bars using the arrows function
arrows(x0 = bar_centers_SV_Temp, y0 = results_SV_Temp$Mean_SV_Temp - results_SV_Temp$SD_SV_Temp,
       x1 = bar_centers_SV_Temp, y1 = results_SV_Temp$Mean_SV_Temp + results_SV_Temp$SD_SV_Temp,
       angle = 90, code = 3, length = 0.1, col = 'darkred')

# Do ANOVA
# Ensure that you have the reshape2 package installed

#Melt the data frame from wide format to long format for ANOVA
longSV_Temp <- melt(SV_Temp, id.vars = "TIMESTAMP", measure.vars = colnames(SV_Temp)[2:10])

# Perform ANOVA
aov_results_SV_Temp <- aov(value ~ variable, data = longSV_Temp)

# Display the summary of the ANOVA
summary(aov_results_SV_Temp)

##Post-hoc test if ANOVA significant
#if (summary(aov_results_SV_Temp)[[1]][["Pr(>F)"]][1] < 0.05) {
  #TukeyHSD(aov_results_SV_Temp)
#} else {
  #print("ANOVA is not significant; post-hoc analysis is not applicable.")
#}




###################################  Treatment 2############################

#uploading soil data from .dat file

#Read the "Ross T2_SoilData.dat" file without headers
tempData2 <- read.table(file = "Ross T2_SoilData.dat", header = FALSE, sep = ",", fill = TRUE)

# Check that the file has been read correctly by looking at the top
head(tempData2)
str(tempData2)

# Set the column names to the values of the second row
colnames(tempData2) <- as.character(tempData2[2, ])

# Remove unnecessary rows 
SoilData2 <- tempData2[-c(1, 2, 3, 4), ]

# Reset row names to start at 1 and continue sequentially
rownames(SoilData2) <- NULL

# Confirm the changes by looking at the top of the modified dataframe
head(SoilData2)
str(SoilData2)

# Create new data frame 'EC2' using the column indices
VWC2 <- SoilData2[, c(1, 3, 6, 9, 12, 15, 18)]
EC2 <- SoilData2[, c(1, 4, 7, 10, 13, 16, 19)]
Temp2 <- SoilData2[, c(1, 5, 8, 11, 14, 17, 20)]

SV_VWC2 <- SoilData2[, c(1, 21, 25, 29, 33, 37, 41, 45, 49, 53)]
SV_EC2 <- SoilData2[, c(1, 24, 28, 32, 36, 40, 44, 48, 52, 56)]
SV_Temp2 <- SoilData2[, c(1, 23, 27, 31, 35, 39, 43, 47, 51, 55)]

# Alternatively, if the columns are consecutive, you can use
#VWC2 <- SoilData2[, 1:6]
# EC2 <- SoilData2[, 1:6]
#Temp2 <- SoilData2[, 1:6]

###########VWC2 ####

# Check the structure and head of the 'VWC2' dataset
str(VWC2)
head(VWC2)

# Check to see whether data are "as character" or "numeric"
sapply(VWC2[, 2:7], class)

# Convert all columns 2 to 7 to numeric if needed
VWC2[, 2:7] <- sapply(VWC2[, 2:7], function(x) as.numeric(as.character(x)))

# Remove rows that contain any NaN values

# First, loop through all elements and convert any "nan" or "NAN" strings to NA
VWC2[VWC2 == "nan" | VWC2 == "NAN"] <- NA
# Then, use na.omit() to remove any rows containing NA values in the dataset
VWC2 <- na.omit(VWC2)

# to exclude rows with any zeros
VWC2 <- VWC2[!apply(VWC2 == 0, 1, any), ]

# Check the structure and head of the 'VWC2' dataset
str(VWC2)
head(VWC2)

#Write the 'VWC2' dataframe to a .dat file with a tab delimiter and save it
write.table(VWC2, file = "VWC2.dat", sep = "\t", row.names = FALSE, col.names = TRUE)

#Create the plot

# Convert the date strings in column 1 to POSIXct datetime objects
VWC2[, 1] <- as.POSIXct(VWC2[, 1], format = "%Y-%m-%d %H:%M")

# Choose your desired y-axis limits (and change it manually in line below).
#y_min <- min(c(VWC2[, 2], VWC2[, 3], VWC2[, 4], VWC2[, 5], VWC2[, 6], VWC2[, 7]), na.rm = TRUE)
#y_max <- max(c(VWC2[, 2], VWC2[, 3], VWC2[, 4], VWC2[, 5], VWC2[, 6], VWC2[, 7]), na.rm = TRUE)

#Create a plot with timestamp on the x-axis and adjusted y-axis limits (adjust the y_max and y_min as you wish)
plot(VWC2[, 1], VWC2[, 2], type = 'l', col = 'red', xlab = 'Timestamp', ylab = 'Values',
     main = 'VWC2 Over Time', ylim = c(0.30, 0.45))

# Add the rest of the series to the same plot
lines(VWC2[, 1], VWC2[, 3], col = 'blue')
lines(VWC2[, 1], VWC2[, 4], col = 'green')
lines(VWC2[, 1], VWC2[, 5], col = 'purple')
lines(VWC2[, 1], VWC2[, 6], col = 'orange')
lines(VWC2[, 1], VWC2[, 7], col = 'yellow')

# Add a legend to distinguish the different lines
legend("bottomright", legend = c("VWC2 1", "VWC2 2", "VWC2 3", "VWC2 4", "VWC2 5", "VWC2 6"),
       col = c('red', 'blue', 'green', 'purple', 'orange', 'yellow'), lty = 1, cex = 0.8)

# Do some basic statistics
# Assuming VWC2 is your dataset with named columns

# List of column names from the 2nd to the 7th
columnNames_VWC2 <- names(VWC2)[2:7]

# Calculate mean and SD for each of the specified columns
means_VWC2 <- sapply(VWC2[, columnNames_VWC2], mean, na.rm = TRUE)
sds_VWC2 <- sapply(VWC2[, columnNames_VWC2], sd, na.rm = TRUE)

# Create a new dataframe for the results
results_VWC2 <- data.frame(
  Mean_VWC2 = means_VWC2,
  SD_VWC2 = sds_VWC2
)

# Set the row names of the results dataframe to the column names
rownames(results_VWC2) <- columnNames_VWC2

# Display the results
print(results_VWC2)

# Create a bar plot for the mean values
barplot(results_VWC2$Mean_VWC2, names.arg = rownames(results_VWC2), beside = TRUE,
        col = 'lightblue', ylim = c(0, max(results_VWC2$Mean_VWC2 + results_VWC2$SD_VWC2)),
        main = "Mean values with SD error bars", ylab = "Mean values")

# Add error bars for SD
# Calculate the center position of each bar on the x-axis
bar_centers_VWC2 <- barplot(results_VWC2$Mean_VWC2, plot = FALSE)

# Add the error bars using the arrow function
arrows(x0 = bar_centers_VWC2, y0 = results_VWC2$Mean_VWC2 - results_VWC2$SD_VWC2,
       x1 = bar_centers_VWC2, y1 = results_VWC2$Mean_VWC2 + results_VWC2$SD_VWC2,
       angle = 90, code = 3, length = 0.1, col = 'darkred')

# Do ANOVA
# Ensure that you have the reshape2 package installed

# Melt the data frame from wide format to long format for ANOVA
longVWC2 <- melt(VWC2, id.vars = "TIMESTAMP", measure.vars = colnames(VWC2)[2:7])

# Perform ANOVA
aov_results_VWC2 <- aov(value ~ variable, data = longVWC2)

# Display the summary of the ANOVA
summary(aov_results_VWC2)

# Post-hoc test if ANOVA significant
if (summary(aov_results_VWC2)[[1]][["Pr(>F)"]][1] < 0.05) {
  TukeyHSD(aov_results_VWC2)
} else {
  print("ANOVA is not significant; post-hoc analysis is not applicable.")
}





###########EC2#######

# Check the structure and head of the 'EC2' dataset
str(EC2)
head(EC2)

# Check to see whether data are "as character" or "numeric"
sapply(EC2[, 2:7], class)

# Convert all columns 2 to 7 to numeric if needed
EC2[, 2:7] <- sapply(EC2[, 2:7], function(x) as.numeric(as.character(x)))

# Remove rows that contain any NaN values
# First, loop through all elements and convert any "nan" or "NAN" strings to NA
EC2[EC2 == "nan" | EC2 == "NAN"] <- NA

# Then, use na.omit() to remove any rows containing NA values in the dataset
EC2 <- na.omit(EC2)

# to exclude rows with any zeros
EC2 <- EC2[!apply(EC2 == 0, 1, any), ]


# Check the structure and head of the 'EC2p' dataset
str(EC2)
head(EC2)


#Write the 'EC2' dataframe to a .dat file with a tab delimiter and save it
write.table(EC2, file = "EC2.dat", sep = "\t", row.names = FALSE, col.names = TRUE)


#Create the plot using ggplot2

# Convert the date strings in column 1 to POSIXct datetime objects
EC2[, 1] <- as.POSIXct(EC2[, 1], format = "%Y-%m-%d %H:%M")

#Choose your desired y-axis limits (and change it manually in line below).
#y_min <- min(c(EC2[, 2], EC2[, 3], EC2[, 4], EC2[, 5], EC2[, 6], EC2[, 7]), na.rm = TRUE)
#y_max <- max(c(EC2[, 2], EC2[, 3], EC2[, 4], EC2[, 5], EC2[, 6], EC2[, 7]), na.rm = TRUE)

#Create a plot with timestamp on the x-axis and adjusted y-axis limits (adjust the y_max and y_min as you wish)
plot(EC2[, 1], EC2[, 2], type = 'l', col = 'red', xlab = 'Timestamp', ylab = 'Values', 
     main = 'EC2 Over Time', ylim = c(0.1978, 0.3887))

# Add the rest of the series to the same plot
lines(EC2[, 1], EC2[, 3], col = 'blue')
lines(EC2[, 1], EC2[, 4], col = 'green')
lines(EC2[, 1], EC2[, 5], col = 'purple')
lines(EC2[, 1], EC2[, 6], col = 'orange')
lines(EC2[, 1], EC2[, 7], col = 'yellow')

# Add a legend to distinguish the different lines
legend("bottomright", legend = c("EC2 1", "EC2 2", "EC2 3", "EC2 4", "EC2 5", "EC2 6"), 
       col = c('red', 'blue', 'green', 'purple', 'orange', 'yellow'), lty = 1, cex = 0.8)


# Do some basic statistics
# Assuming EC2 is your dataset with named columns

# List of column names from the 2nd to the 7th
columnNames_EC2 <- names(EC2)[2:7]

# Calculate mean and SD for each of the specified columns
means_EC2 <- sapply(EC2[, columnNames_EC2], mean, na.rm = TRUE)
sds_EC2 <- sapply(EC2[, columnNames_EC2], sd, na.rm = TRUE)

# Create a new dataframe for the results
results_EC2 <- data.frame(
  Mean_EC2 = means_EC2,
  SD_EC2 = sds_EC2
)

# Set the row names of the results dataframe to the column names
rownames(results_EC2) <- columnNames_EC2

# Display the results
print(results_EC2)

# make a graph to compare EC2 across lysimeters

# Assuming 'results' dataframe contains the mean and SD information
# for each of the columns

# Create a bar plot for the mean values
barplot(results_EC2$Mean_EC2, names.arg = rownames(results_EC2), beside = TRUE,
        col = 'lightblue', ylim = c(0, max(results_EC2$Mean_EC2 + results_EC2$SD_EC2)),
        main = "Mean values with SD error bars", ylab = "Mean values")

# Add error bars for SD
# Calculate the center position of each bar on the x-axis
bar_centers_EC2 <- barplot(results_EC2$Mean_EC2, plot = FALSE)

# Add the error bars using the arrow function
arrows(x0 = bar_centers_EC2, y0 = results_EC2$Mean_EC2 - results_EC2$SD_EC2,
       x1 = bar_centers_EC2, y1 = results_EC2$Mean_EC2 + results_EC2$SD_EC2,
       angle = 90, code = 3, length = 0.1, col = 'darkred')


# Do ANOVA

# Ensure that you have the reshape2 package installed
# Commented out because it should be run manually only if needed

#Melt the data frame from wide format to long format for ANOVA
longEC2 <- melt(EC2, id.vars = "TIMESTAMP", measure.vars = colnames(EC2)[2:7])

# Perform ANOVA
aov_results_EC2 <- aov(value ~ variable, data = longEC2)

# Display the summary of the ANOVA
summary(aov_results_EC2)

# Post-hoc test if ANOVA significant
if (summary(aov_results_EC2)[[1]][["Pr(>F)"]][1] < 0.05) {
  TukeyHSD(aov_results_EC2)
} else {
  print("ANOVA is not significant; post-hoc analysis is not applicable.")
}

###########Temp2#########

# Check the structure and head of the 'Temp2' dataset
str(Temp2)
head(Temp2)

# Check to see whether data are "as character" or "numeric"
sapply(Temp2[, 2:7], class)

# Convert all columns 2 to 7 to numeric if needed
Temp2[, 2:7] <- sapply(Temp2[, 2:7], function(x) as.numeric(as.character(x)))

# Remove rows that contain any NaN values
# First, loop through all elements and convert any "nan" or "NAN" strings to NA
Temp2[Temp2 == "nan" | Temp2 == "NAN"] <- NA

# Then, use na.omit() to remove any rows containing NA values in the dataset
Temp2 <- na.omit(Temp2)

# to exclude rows with any zeros
Temp2 <- Temp2[!apply(Temp2 == 0, 1, any), ]

# Check the structure and head of the 'Temp2p' dataset
str(Temp2)
head(Temp2)

#Write the 'Temp2' dataframe to a .dat file with a tab delimiter and save it
write.table(Temp2, file = "Temp2.dat", sep = "\t", row.names = FALSE, col.names = TRUE)

#Create the plot using ggplot2

# Convert the date strings in column 1 to POSIXct datetime objects
Temp2[, 1] <- as.POSIXct(Temp2[, 1], format = "%Y-%m-%d %H:%M")

#Choose your desired y-axis limits (and change it manually in line below).
#y_min <- min(c(Temp2[, 2], Temp2[, 3], Temp2[, 4], Temp2[, 5], Temp2[, 6], Temp2[, 7]), na.rm = TRUE)
#y_max <- max(c(Temp2[, 2], Temp2[, 3], Temp2[, 4], Temp2[, 5], Temp2[, 6], Temp2[, 7]), na.rm = TRUE)

#Create a plot with timestamp on the x-axis and adjusted y-axis limits (adjust the y_max and y_min as you wish)
plot(Temp2[, 1], Temp2[, 2], type = 'l', col = 'red', xlab = 'Timestamp', ylab = 'Values',
     main = 'Temperature2 Over Time', ylim = c(23, 30))

# Add the rest of the series to the same plot
lines(Temp2[, 1], Temp2[, 3], col = 'blue')
lines(Temp2[, 1], Temp2[, 4], col = 'green')
lines(Temp2[, 1], Temp2[, 5], col = 'purple')
lines(Temp2[, 1], Temp2[, 6], col = 'orange')
lines(Temp2[, 1], Temp2[, 7], col = 'yellow')

# Add a legend to distinguish the different lines
legend("bottomright", legend = c("Temp2 1", "Temp2 2", "Temp2 3", "Temp2 4", "Temp2 5", "Temp2 6"),
       col = c('red', 'blue', 'green', 'purple', 'orange', 'yellow'), lty = 1, cex = 0.8)

# Do some basic statistics
# Assuming Temp2 is your dataset with named columns

# List of column names from the 2nd to the 7th
columnNames_Temp2 <- names(Temp2)[2:7]

# Calculate mean and SD for each of the specified columns
means_Temp2 <- sapply(Temp2[, columnNames_Temp2], mean, na.rm = TRUE)
sds_Temp2 <- sapply(Temp2[, columnNames_Temp2], sd, na.rm = TRUE)

# Create a new dataframe for the results
results_Temp2 <- data.frame(
  Mean_Temp2 = means_Temp2,
  SD_Temp2 = sds_Temp2
)

# Set the row names of the results dataframe to the column names
rownames(results_Temp2) <- columnNames_Temp2

# Display the results
print(results_Temp2)

# make a graph to compare Temp2 across lysimeters

# Assuming 'results' dataframe contains the mean and SD information
# for each of the columns

# Create a bar plot for the mean values
barplot(results_Temp2$Mean_Temp2, names.arg = rownames(results_Temp2), beside = TRUE,
        col = 'lightblue', ylim = c(0, max(results_Temp2$Mean_Temp2 + results_Temp2$SD_Temp2)),
        main = "Mean values with SD error bars", ylab = "Mean values")

# Add error bars for SD
# Calculate the center position of each bar on the x-axis
bar_centers_Temp2 <- barplot(results_Temp2$Mean_Temp2, plot = FALSE)

# Add the error bars using the arrow function
arrows(x0 = bar_centers_Temp2, y0 = results_Temp2$Mean_Temp2 - results_Temp2$SD_Temp2,
       x1 = bar_centers_Temp2, y1 = results_Temp2$Mean_Temp2 + results_Temp2$SD_Temp2,
       angle = 90, code = 3, length = 0.1, col = 'darkred')

# Do ANOVA

# Ensure that you have the reshape2 package installed

#Melt the data frame from wide format to long format for ANOVA
longTemp2 <- melt(Temp2, id.vars = "TIMESTAMP", measure.vars = colnames(Temp2)[2:7])

# Perform ANOVA
aov_results_Temp2 <- aov(value ~ variable, data = longTemp2)

# Display the summary of the ANOVA
summary(aov_results_Temp2)

# Post-hoc test if ANOVA significant
if (summary(aov_results_Temp2)[[1]][["Pr(>F)"]][1] < 0.05) {
  TukeyHSD(aov_results_Temp2)
} else {
  print("ANOVA is not significant; post-hoc analysis is not applicable.")
}


#################SV_VWC2##########
# Check the structure and head of the 'SV_VWC2' dataset
str(SV_VWC2)
head(SV_VWC2)

# Check to see whether data are "as character" or "numeric"
sapply(SV_VWC2[, 2:10], class)

# Convert all columns 2 to 10 to numeric if needed
SV_VWC2[, 2:10] <- sapply(SV_VWC2[, 2:10], function(x) as.numeric(as.character(x)))

# Remove rows that contain any NaN values

##First, loop through all elements and convert any "nan" or "NAN" strings to NaN
#SV_VWC2[SV_VWC2 == "nan" | SV_VWC2 == "NAN"] <- NaN
##Then, use na.omit() to remove any rows containing NaN values in the dataset
#SV_VWC2 <- na.omit(SV_VWC2)

# to exclude rows with any zeros
#SV_VWC2 <- SV_VWC2[!apply(SV_VWC2 == 0, 1, any), ]

# Check the structure and head of the 'SV_VWCp' dataset
str(SV_VWC2)
head(SV_VWC2)

#Write the 'SV_VWC2' dataframe to a .dat file with a tab delimiter and save it
write.table(SV_VWC2, file = "SV_VWC2.dat", sep = "\t", row.names = FALSE, col.names = TRUE)

#Create the plot using ggplot2

# Convert the date strings in column 1 to POSIXct datetime objects
SV_VWC2[, 1] <- as.POSIXct(SV_VWC2[, 1], format = "%Y-%m-%d %H:%M")

#Choose your desired y-axis limits (and change it manually in line below).
#y_min <- min(c(SV_VWC2[, 2], SV_VWC2[, 3], SV_VWC2[, 4], SV_VWC2[, 5], SV_VWC2[, 6], SV_VWC2[, 7], SV_VWC2[, 8], SV_VWC2[, 9], SV_VWC2[, 10])), na.rm = TRUE)
#y_max <- max(c(SV_VWC2[, 2], SV_VWC2[, 3], SV_VWC2[, 4], SV_VWC2[, 5], SV_VWC2[, 6], SV_VWC2[, 7], SV_VWC2[, 8], SV_VWC2[, 9], SV_VWC2[, 10]), na.rm = TRUE)

#Create a plot with timestamp on the x-axis and adjusted y-axis limits (adjust the y_max and y_min as you wish)
plot(SV_VWC2[, 1], SV_VWC2[, 2], type = 'l', col = 'red', xlab = 'Timestamp', ylab = 'Values',
     main = 'SV_VWC2 Over Time', ylim = c(0.30, 0.45))

# Add the rest of the series to the same plot
lines(SV_VWC2[, 1], SV_VWC2[, 3], col = 'blue')
lines(SV_VWC2[, 1], SV_VWC2[, 4], col = 'green')
lines(SV_VWC2[, 1], SV_VWC2[, 5], col = 'purple')
lines(SV_VWC2[, 1], SV_VWC2[, 6], col = 'orange')
lines(SV_VWC2[, 1], SV_VWC2[, 7], col = 'yellow')
lines(SV_VWC2[, 1], SV_VWC2[, 8], col = 'brown')
lines(SV_VWC2[, 1], SV_VWC2[, 9], col = 'lightblue')
lines(SV_VWC2[, 1], SV_VWC2[, 10], col = 'lightgreen')
# Add a legend to distinguish the different lines
legend("bottomright", legend = c("SV_VWC2 5cm", "SV_VWC2 10cm", "SV_VWC2 20cm", "SV_VWC2 30cm", "SV_VWC2 40cm", "SV_VWC2 50cm", "SV_VWC2 60cm", "SV_VWC2 75cm", "SV_VWC2 100cm"),
       col = c('red', 'blue', 'green', 'purple', 'orange', 'yellow', 'brown', 'lightblue', 'lightgreen'), lty = 1, cex = 0.8)

# Do some basic statistics
# Assuming SV_VWC2 is your dataset with named columns

# List of column names from the 2nd to the 7th
columnNames_SV_VWC2 <- names(SV_VWC2)[2:10]

# Calculate mean and SD for each of the specified columns
means_SV_VWC2 <- sapply(SV_VWC2[, columnNames_SV_VWC2], mean, na.rm = TRUE)
sds_SV_VWC2 <- sapply(SV_VWC2[, columnNames_SV_VWC2], sd, na.rm = TRUE)

# Create a new dataframe for the results
results_SV_VWC2 <- data.frame(
  Mean_SV_VWC2 = means_SV_VWC2,
  SD_SV_VWC2 = sds_SV_VWC2
)

# Set the row names of the results dataframe to the column names
rownames(results_SV_VWC2) <- columnNames_SV_VWC2

# Display the results
print(results_SV_VWC2)

# make a graph to compare SV_VWC2 in soil profile

# Assuming 'results' dataframe contains the mean and SD information
# for each of the columns

# Create a bar plot for the mean values
barplot(results_SV_VWC2$Mean_SV_VWC2, names.arg = rownames(results_SV_VWC2), beside = TRUE,
        col = 'lightblue', ylim = c(0, max(results_SV_VWC2$Mean_SV_VWC2 + results_SV_VWC2$SD_SV_VWC2)),
        main = "Mean values with SD error bars", ylab = "Mean values")

# Add error bars for SD
# Calculate the center position of each bar on the x-axis
bar_centers_SV_VWC2 <- barplot(results_SV_VWC2$Mean_SV_VWC2, plot = FALSE)

# Add the error bars using the arrows function
arrows(x0 = bar_centers_SV_VWC2, y0 = results_SV_VWC2$Mean_SV_VWC2 - results_SV_VWC2$SD_SV_VWC2,
       x1 = bar_centers_SV_VWC2, y1 = results_SV_VWC2$Mean_SV_VWC2 + results_SV_VWC2$SD_SV_VWC2,
       angle = 90, code = 3, length = 0.1, col = 'darkred')

##Do ANOVA
##Ensure that you have the reshape2 package installed

##Melt the data frame from wide format to long format for ANOVA
longSV_VWC2 <- melt(SV_VWC2, id.vars = "TIMESTAMP", measure.vars = colnames(SV_VWC2)[2:10])

##Perform ANOVA
aov_results_SV_VWC2 <- aov(value ~ variable, data = longSV_VWC2)

##Display the summary of the ANOVA
summary(aov_results_SV_VWC2)

## Post-hoc test if ANOVA significant
#if (summary(aov_results_SV_VWC2)[[1]][["Pr(>F)"]][1] < 0.05) {
  #TukeyHSD(aov_results_SV_VWC2)
#} else {
 # print("ANOVA is not significant; post-hoc analysis is not applicable.")
#}


#################SV_EC2#############

# Check the structure and head of the 'SV_EC2' dataset
str(SV_EC2)
head(SV_EC2)

# Check to see whether data are "as character" or "numeric"
sapply(SV_EC2[, 2:10], class)

# Convert all columns 2 to 10 to numeric if needed
SV_EC2[, 2:10] <- sapply(SV_EC2[, 2:10], function(x) as.numeric(as.character(x)))

# Remove rows that contain any NaN values

## First, loop through all elements and convert any "nan" or "NAN" strings to NaN
#SV_EC2[SV_EC2 == "nan" | SV_EC2 == "NAN"] <- NaN
## Then, use na.omit() to remove any rows containing NaN values in the dataset
#SV_EC2 <- na.omit(SV_EC2)

## to exclude rows with any zeros
#SV_EC2 <- SV_EC2[!apply(SV_EC2 == 0, 1, any), ]

# Check the structure and head of the 'SV_EC2' dataset
str(SV_EC2)
head(SV_EC2)

# Write the 'SV_EC2' dataframe to a .dat file with a tab delimiter and save it
write.table(SV_EC2, file = "SV_EC2.dat", sep = "\t", row.names = FALSE, col.names = TRUE)

# Create the plot using ggplot2

# Convert the date strings in column 1 to POSIXct datetime objects
SV_EC2[, 1] <- as.POSIXct(SV_EC2[, 1], format = "%Y-%m-%d %H:%M")

# Choose your desired y-axis limits (and change it manually in line below).
#y_min <- min(c(SV_EC2[, 2], SV_EC2[, 3], SV_EC2[, 4], SV_EC2[, 5], SV_EC2[, 6], SV_EC2[, 7], SV_EC2[, 8], SV_EC2[, 9], SV_EC2[, 10]), na.rm = TRUE)
#y_max <- max(c(SV_EC2[, 2], SV_EC2[, 3], SV_EC2[, 4], SV_EC2[, 5], SV_EC2[, 6], SV_EC2[, 7], SV_EC2[, 8], SV_EC2[, 9], SV_EC2[, 10]), na.rm = TRUE)

# Create a plot with timestamp on the x-axis and adjusted y-axis limits (adjust the y_max and y_min as you wish)
plot(SV_EC2[, 1], SV_EC2[, 2], type = 'l', col = 'red', xlab = 'Timestamp', ylab = 'Values',
     main = 'SV_EC2 Over Time', ylim = c(0.30, 0.45))

# Add the rest of the series to the same plot
lines(SV_EC2[, 1], SV_EC2[, 3], col = 'blue')
lines(SV_EC2[, 1], SV_EC2[, 4], col = 'green')
lines(SV_EC2[, 1], SV_EC2[, 5], col = 'purple')
lines(SV_EC2[, 1], SV_EC2[, 6], col = 'orange')
lines(SV_EC2[, 1], SV_EC2[, 7], col = 'yellow')
lines(SV_EC2[, 1], SV_EC2[, 8], col = 'brown')
lines(SV_EC2[, 1], SV_EC2[, 9], col = 'lightblue')
lines(SV_EC2[, 1], SV_EC2[, 10], col = 'lightgreen')
# Add a legend to distinguish the different lines
legend("bottomright", legend = c("SV_EC2 5cm", "SV_EC2 10cm", "SV_EC2 20cm", "SV_EC2 30cm", "SV_EC2 40cm", "SV_EC2 50cm", "SV_EC2 60cm", "SV_EC2 75cm", "SV_EC2 100cm"),
       col = c('red', 'blue', 'green', 'purple', 'orange', 'yellow', 'brown', 'lightblue', 'lightgreen'), lty = 1, cex = 0.8)

# Do some basic statistics
# Assuming SV_EC2 is your dataset with named columns

# List of column names from the 2nd to the 7th
columnNames_SV_EC2 <- names(SV_EC2)[2:10]

# Calculate mean and SD for each of the specified columns
means_SV_EC2 <- sapply(SV_EC2[, columnNames_SV_EC2], mean, na.rm = TRUE)
sds_SV_EC2 <- sapply(SV_EC2[, columnNames_SV_EC2], sd, na.rm = TRUE)

# Create a new dataframe for the results
results_SV_EC2 <- data.frame(
  Mean_SV_EC2 = means_SV_EC2,
  SD_SV_EC2 = sds_SV_EC2
)

# Set the row names of the results dataframe to the column names
rownames(results_SV_EC2) <- columnNames_SV_EC2

# Display the results
print(results_SV_EC2)

# Make a graph to compare SV_EC2 across paddock

# Assuming 'results' dataframe contains the mean and SD information
# for each of the columns

# Create a bar plot for the mean values
barplot(results_SV_EC2$Mean_SV_EC2, names.arg = rownames(results_SV_EC2), beside = TRUE,
        col = 'lightblue', ylim = c(0, max(results_SV_EC2$Mean_SV_EC2 + results_SV_EC2$SD_SV_EC2)),
        main = "Mean values with SD error bars", ylab = "Mean values")

# Add error bars for SD
# Calculate the center position of each bar on the x-axis
bar_centers_SV_EC2 <- barplot(results_SV_EC2$Mean_SV_EC2, plot = FALSE)

# Add the error bars using the arrows function
arrows(x0 = bar_centers_SV_EC2, y0 = results_SV_EC2$Mean_SV_EC2 - results_SV_EC2$SD_SV_EC2,
       x1 = bar_centers_SV_EC2, y1 = results_SV_EC2$Mean_SV_EC2 + results_SV_EC2$SD_SV_EC2,
       angle = 90, code = 3, length = 0.1, col = 'darkred')

##Do ANOVA
##Ensure that you have the reshape2 package installed

##Melt the data frame from wide format to long format for ANOVA
longSV_EC2 <- melt(SV_EC2, id.vars = "TIMESTAMP", measure.vars = colnames(SV_EC2)[2:10])

##Perform ANOVA
aov_results_SV_EC2 <- aov(value ~ variable, data = longSV_EC2)

##Display the summary of the ANOVA
summary(aov_results_SV_EC2)

##Post-hoc test if ANOVA significant
#if (summary(aov_results_SV_EC2)[[1]][["Pr(>F)"]][1] < 0.05) {
  #TukeyHSD(aov_results_SV_EC2)
#} else {
  #print("ANOVA is not significant; post-hoc analysis is not applicable.")
#}


#################SV_Temp2########
# Check the structure and head of the 'SV_Temp2' dataset
str(SV_Temp2)
head(SV_Temp2)

# Check to see whether data are "as character" or "numeric"
sapply(SV_Temp2[, 2:10], class)

# Convert all columns 2 to 10 to numeric if needed
SV_Temp2[, 2:10] <- sapply(SV_Temp2[, 2:10], function(x) as.numeric(as.character(x)))

# Remove rows that contain any NaN values

## First, loop through all elements and convert any "nan" or "NAN" strings to NaN
#SV_Temp2[SV_Temp2 == "nan" | SV_Temp2 == "NAN"] <- NaN
## Then, use na.omit() to remove any rows containing NaN values in the dataset
#SV_Temp2 <- na.omit(SV_Temp2)

## to exclude rows with any zeros
#SV_Temp2 <- SV_Temp2[!apply(SV_Temp2 == 0, 1, any), ]

# Check the structure and head of the 'SV_Temp2' dataset
str(SV_Temp2)
head(SV_Temp2)

# Write the 'SV_Temp2' dataframe to a .dat file with a tab delimiter and save it
write.table(SV_Temp2, file = "SV_Temp2.dat", sep = "\t", row.names = FALSE, col.names = TRUE)

# Create the plot using ggplot2

# Convert the date strings in column 1 to POSIXct datetime objects
SV_Temp2[, 1] <- as.POSIXct(SV_Temp2[, 1], format = "%Y-%m-%d %H:%M")

# Create a plot with timestamp on the x-axis
plot(SV_Temp2[, 1], SV_Temp2[, 2], type = 'l', col = 'red', xlab = 'Timestamp', ylab = 'Values',
     main = 'SV_Temp2 Over Time', ylim = c(0.30, 0.45))

# Add the rest of the series to the same plot
lines(SV_Temp2[, 1], SV_Temp2[, 3], col = 'blue')
lines(SV_Temp2[, 1], SV_Temp2[, 4], col = 'green')
lines(SV_Temp2[, 1], SV_Temp2[, 5], col = 'purple')
lines(SV_Temp2[, 1], SV_Temp2[, 6], col = 'orange')
lines(SV_Temp2[, 1], SV_Temp2[, 7], col = 'yellow')
lines(SV_Temp2[, 1], SV_Temp2[, 8], col = 'brown')
lines(SV_Temp2[, 1], SV_Temp2[, 9], col = 'lightblue')
lines(SV_Temp2[, 1], SV_Temp2[, 10], col = 'lightgreen')
# Add a legend to distinguish the different lines
legend("bottomright", legend = c("SV_Temp2 5cm", "SV_Temp2 10cm", "SV_Temp2 20cm", "SV_Temp2 30cm", "SV_Temp2 40cm", "SV_Temp2 50cm", "SV_Temp2 60cm", "SV_Temp2 75cm", "SV_Temp2 100cm"),
       col = c('red', 'blue', 'green', 'purple', 'orange', 'yellow', 'brown', 'lightblue', 'lightgreen'), lty = 1, cex = 0.8)

# Do some basic statistics
# Assuming SV_Temp2 is your dataset with named columns

# List of column names from the 2nd to the 7th
columnNames_SV_Temp2 <- names(SV_Temp2)[2:10]

# Calculate mean and SD for each of the specified columns
means_SV_Temp2 <- sapply(SV_Temp2[, columnNames_SV_Temp2], mean, na.rm = TRUE)
sds_SV_Temp2 <- sapply(SV_Temp2[, columnNames_SV_Temp2], sd, na.rm = TRUE)

# Create a new dataframe for the results
results_SV_Temp2 <- data.frame(
  Mean_SV_Temp2 = means_SV_Temp2,
  SD_SV_Temp2 = sds_SV_Temp2
)

# Set the row names of the results dataframe to the column names
rownames(results_SV_Temp2) <- columnNames_SV_Temp2

# Display the results
print(results_SV_Temp2)

# Make a graph to compare SV_Temp2 across paddock

# Assuming 'results' dataframe contains the mean and SD information
# for each of the columns

# Create a bar plot for the mean values
barplot(results_SV_Temp2$Mean_SV_Temp2, names.arg = rownames(results_SV_Temp2), beside = TRUE,
        col = 'lightblue', ylim = c(0, max(results_SV_Temp2$Mean_SV_Temp2 + results_SV_Temp2$SD_SV_Temp2)),
        main = "Mean values with SD error bars", ylab = "Mean values")

# Add error bars for SD
# Calculate the center position of each bar on the x-axis
bar_centers_SV_Temp2 <- barplot(results_SV_Temp2$Mean_SV_Temp2, plot = FALSE)

# Add the error bars using the arrows function
arrows(x0 = bar_centers_SV_Temp2, y0 = results_SV_Temp2$Mean_SV_Temp2 - results_SV_Temp2$SD_SV_Temp2,
       x1 = bar_centers_SV_Temp2, y1 = results_SV_Temp2$Mean_SV_Temp2 + results_SV_Temp2$SD_SV_Temp2,
       angle = 90, code = 3, length = 0.1, col = 'darkred')

##Do ANOVA
##Ensure that you have the reshape2 package installed

##Melt the data frame from wide format to long format for ANOVA
longSV_Temp2 <- melt(SV_Temp2, id.vars = "TIMESTAMP", measure.vars = colnames(SV_Temp2)[2:10])

##Perform ANOVA
aov_results_SV_Temp2 <- aov(value ~ variable, data = longSV_Temp2)

##Display the summary of the ANOVA
summary(aov_results_SV_Temp2)

##Post-hoc test if ANOVA significant
#if (summary(aov_results_SV_Temp2)[[1]][["Pr(>F)"]][1] < 0.05) {
  #TukeyHSD(aov_results_SV_Temp2)
#} else {
  #print("ANOVA is not significant; post-hoc analysis is not applicable.")
#}




#################################### Treatment 3###############################



#uploading soil data from .dat file

#Read the "Ross T3_SoilData.dat" file without headers
tempData3 <- read.table(file = "Ross T3_SoilData.dat", header = FALSE, sep = ",", fill = TRUE)

# Check that the file has been read correctly by looking at the top
head(tempData3)
str(tempData3)

# Set the column names to the values of the second row
colnames(tempData3) <- as.character(tempData3[2, ])

# Remove unnecessary rows 
SoilData3 <- tempData3[-c(1, 2, 3, 4), ]

# Reset row names to start at 1 and continue sequentially
rownames(SoilData3) <- NULL

# Confirm the changes by looking at the top of the modified dataframe
head(SoilData3)
str(SoilData3)

# Create new data frame 'EC2' using the column indices
VWC3 <- SoilData3[, c(1, 3, 6, 9, 12, 15, 18)]
EC3 <- SoilData3[, c(1, 4, 7, 10, 13, 16, 19)]
Temp3 <- SoilData3[, c(1, 5, 8, 11, 14, 17, 20)]

SV_VWC3 <- SoilData3[, c(1, 21, 25, 29, 33, 37, 41, 45, 49, 53)]
SV_EC3 <- SoilData3[, c(1, 24, 28, 32, 36, 40, 44, 48, 52, 56)]
SV_Temp3 <- SoilData3[, c(1, 23, 27, 31, 35, 39, 43, 47, 51, 55)]


# Alternatively, if the columns are consecutive, you can use
#VWC2 <- SoilData2[, 1:6]
# EC2 <- SoilData2[, 1:6]
#Temp2 <- SoilData2[, 1:6]



###########VWC3 ######

# Check the structure and head of the 'VWC3' dataset
str(VWC3)
head(VWC3)

# Check to see whether data are "as character" or "numeric"
sapply(VWC3[, 2:7], class)

# Convert all columns 2 to 7 to numeric if needed
VWC3[, 2:7] <- sapply(VWC3[, 2:7], function(x) as.numeric(as.character(x)))

# Remove rows that contain any NaN values

# First, loop through all elements and convert any "nan" or "NAN" strings to NA
VWC3[VWC3 == "nan" | VWC3 == "NAN"] <- NA
# Then, use na.omit() to remove any rows containing NA values in the dataset
VWC3 <- na.omit(VWC3)

# to exclude rows with any zeros
VWC3 <- VWC3[!apply(VWC3 == 0, 1, any), ]

# Check the structure and head of the 'VWC3' dataset
str(VWC3)
head(VWC3)

#Write the 'VWC3' dataframe to a .dat file with a tab delimiter and save it
write.table(VWC3, file = "VWC3.dat", sep = "\t", row.names = FALSE, col.names = TRUE)

#Create the plot

# Convert the date strings in column 1 to POSIXct datetime objects
VWC3[, 1] <- as.POSIXct(VWC3[, 1], format = "%Y-%m-%d %H:%M")

# Choose your desired y-axis limits (and change it manually in line below).
#y_min <- min(c(VWC3[, 2], VWC3[, 3], VWC3[, 4], VWC3[, 5], VWC3[, 6], VWC3[, 7]), na.rm = TRUE)
#y_max <- max(c(VWC3[, 2], VWC3[, 3], VWC3[, 4], VWC3[, 5], VWC3[, 6], VWC3[, 7]), na.rm = TRUE)

#Create a plot with timestamp on the x-axis and adjusted y-axis limits (adjust the y_max and y_min as you wish)
plot(VWC3[, 1], VWC3[, 2], type = 'l', col = 'red', xlab = 'Timestamp', ylab = 'Values',
     main = 'VWC3 Over Time', ylim = c(0.30, 0.45))

# Add the rest of the series to the same plot
lines(VWC3[, 1], VWC3[, 3], col = 'blue')
lines(VWC3[, 1], VWC3[, 4], col = 'green')
lines(VWC3[, 1], VWC3[, 5], col = 'purple')
lines(VWC3[, 1], VWC3[, 6], col = 'orange')
lines(VWC3[, 1], VWC3[, 7], col = 'yellow')

# Add a legend to distinguish the different lines
legend("bottomright", legend = c("VWC3 1", "VWC3 2", "VWC3 3", "VWC3 4", "VWC3 5", "VWC3 6"),
       col = c('red', 'blue', 'green', 'purple', 'orange', 'yellow'), lty = 1, cex = 0.8)

# Do some basic statistics
# Assuming VWC3 is your dataset with named columns

# List of column names from the 2nd to the 7th
columnNames_VWC3 <- names(VWC3)[2:7]

# Calculate mean and SD for each of the specified columns
means_VWC3 <- sapply(VWC3[, columnNames_VWC3], mean, na.rm = TRUE)
sds_VWC3 <- sapply(VWC3[, columnNames_VWC3], sd, na.rm = TRUE)

# Create a new dataframe for the results
results_VWC3 <- data.frame(
  Mean_VWC3 = means_VWC3,
  SD_VWC3 = sds_VWC3
)

# Set the row names of the results dataframe to the column names
rownames(results_VWC3) <- columnNames_VWC3

# Display the results
print(results_VWC3)

# Create a bar plot for the mean values
barplot(results_VWC3$Mean_VWC3, names.arg = rownames(results_VWC3), beside = TRUE,
        col = 'lightblue', ylim = c(0, max(results_VWC3$Mean_VWC3 + results_VWC3$SD_VWC3)),
        main = "Mean values with SD error bars", ylab = "Mean values")

# Add error bars for SD
# Calculate the center position of each bar on the x-axis
bar_centers_VWC3 <- barplot(results_VWC3$Mean_VWC3, plot = FALSE)

# Add the error bars using the arrow function
arrows(x0 = bar_centers_VWC3, y0 = results_VWC3$Mean_VWC3 - results_VWC3$SD_VWC3,
       x1 = bar_centers_VWC3, y1 = results_VWC3$Mean_VWC3 + results_VWC3$SD_VWC3,
       angle = 90, code = 3, length = 0.1, col = 'darkred')

# Do ANOVA
# Ensure that you have the reshape2 package installed

# Melt the data frame from wide format to long format for ANOVA
longVWC3 <- melt(VWC3, id.vars = "TIMESTAMP", measure.vars = colnames(VWC3)[2:7])

# Perform ANOVA
aov_results_VWC3 <- aov(value ~ variable, data = longVWC3)

# Display the summary of the ANOVA
summary(aov_results_VWC3)

# Post-hoc test if ANOVA significant
if (summary(aov_results_VWC3)[[1]][["Pr(>F)"]][1] < 0.05) {
  TukeyHSD(aov_results_VWC3)
} else {
  print("ANOVA is not significant; post-hoc analysis is not applicable.")
}



###########EC3######


# Check the structure and head of the 'EC3' dataset
str(EC3)
head(EC3)

# Check to see whether data are "as character" or "numeric"
sapply(EC3[, 2:7], class)

# Convert all columns 2 to 7 to numeric if needed
EC3[, 2:7] <- sapply(EC3[, 2:7], function(x) as.numeric(as.character(x)))

# Remove rows that contain any NaN values
# First, loop through all elements and convert any "nan" or "NAN" strings to NA
EC3[EC3 == "nan" | EC3 == "NAN"] <- NA

# Then, use na.omit() to remove any rows containing NA values in the dataset
EC3 <- na.omit(EC3)

# to exclude rows with any zeros
EC3 <- EC3[!apply(EC3 == 0, 1, any), ]


# Check the structure and head of the 'EC3' dataset
str(EC3)
head(EC3)


#Write the 'EC3' dataframe to a .dat file with a tab delimiter and save it
write.table(EC3, file = "EC3.dat", sep = "\t", row.names = FALSE, col.names = TRUE)


#Create the plot using ggplot2

# Convert the date strings in column 1 to POSIXct datetime objects
EC3[, 1] <- as.POSIXct(EC3[, 1], format = "%Y-%m-%d %H:%M")

#Choose your desired y-axis limits (and change it manually in line below).
#y_min <- min(c(EC3[, 2], EC3[, 3], EC3[, 4], EC3[, 5], EC3[, 6], EC3[, 7]), na.rm = TRUE)
#y_max <- max(c(EC3[, 2], EC3[, 3], EC3[, 4], EC3[, 5], EC3[, 6], EC3[, 7]), na.rm = TRUE)

#Create a plot with timestamp on the x-axis and adjusted y-axis limits (adjust the y_max and y_min as you wish)
plot(EC3[, 1], EC3[, 2], type = 'l', col = 'red', xlab = 'Timestamp', ylab = 'Values', 
     main = 'EC3 Over Time', ylim = c(0.0, 0.35))

# Add the rest of the series to the same plot
lines(EC3[, 1], EC3[, 3], col = 'blue')
lines(EC3[, 1], EC3[, 4], col = 'green')
lines(EC3[, 1], EC3[, 5], col = 'purple')
lines(EC3[, 1], EC3[, 6], col = 'orange')
lines(EC3[, 1], EC3[, 7], col = 'yellow')

# Add a legend to distinguish the different lines
legend("bottomright", legend = c("EC3 1", "EC3 2", "EC3 3", "EC3 4", "EC3 5", "EC3 6"), 
       col = c('red', 'blue', 'green', 'purple', 'orange', 'yellow'), lty = 1, cex = 0.8)


# Do some basic statistics
# Assuming EC3 is your dataset with named columns

# List of column names from the 2nd to the 7th
columnNames_EC3 <- names(EC3)[2:7]

# Calculate mean and SD for each of the specified columns
means_EC3 <- sapply(EC3[, columnNames_EC3], mean, na.rm = TRUE)
sds_EC3 <- sapply(EC3[, columnNames_EC3], sd, na.rm = TRUE)

# Create a new dataframe for the results
results_EC3 <- data.frame(
  Mean_EC3 = means_EC3,
  SD_EC3 = sds_EC3
)

# Set the row names of the results dataframe to the column names
rownames(results_EC3) <- columnNames_EC3

# Display the results
print(results_EC3)

# make a graph to compare EC3 across lysimeters

# Assuming 'results' dataframe contains the mean and SD information
# for each of the columns

# Create a bar plot for the mean values
barplot(results_EC3$Mean_EC3, names.arg = rownames(results_EC3), beside = TRUE,
        col = 'lightblue', ylim = c(0, max(results_EC3$Mean_EC3 + results_EC3$SD_EC3)),
        main = "Mean values with SD error bars", ylab = "Mean values")

# Add error bars for SD
# Calculate the center position of each bar on the x-axis
bar_centers_EC3 <- barplot(results_EC3$Mean_EC3, plot = FALSE)

# Add the error bars using the arrow function
arrows(x0 = bar_centers_EC3, y0 = results_EC3$Mean_EC3 - results_EC3$SD_EC3,
       x1 = bar_centers_EC3, y1 = results_EC3$Mean_EC3 + results_EC3$SD_EC3,
       angle = 90, code = 3, length = 0.1, col = 'darkred')


# Do ANOVA

# Ensure that you have the reshape2 package installed
# Commented out because it should be run manually only if needed

#Melt the data frame from wide format to long format for ANOVA
longEC3 <- melt(EC3, id.vars = "TIMESTAMP", measure.vars = colnames(EC3)[2:7])

# Perform ANOVA
aov_results_EC3 <- aov(value ~ variable, data = longEC3)

# Display the summary of the ANOVA
summary(aov_results_EC3)

# Post-hoc test if ANOVA significant
if (summary(aov_results_EC3)[[1]][["Pr(>F)"]][1] < 0.05) {
  TukeyHSD(aov_results_EC3)
} else {
  print("ANOVA is not significant; post-hoc analysis is not applicable.")
}




###########Temp3######

# Check the structure and head of the 'Temp3' dataset
str(Temp3)
head(Temp3)

# Check to see whether data are "as character" or "numeric"
sapply(Temp3[, 2:7], class)

# Convert all columns 2 to 7 to numeric if needed
Temp3[, 2:7] <- sapply(Temp3[, 2:7], function(x) as.numeric(as.character(x)))

# Remove rows that contain any NaN values
# First, loop through all elements and convert any "nan" or "NAN" strings to NA
Temp3[Temp3 == "nan" | Temp3 == "NAN"] <- NA

# Then, use na.omit() to remove any rows containing NA values in the dataset
Temp3 <- na.omit(Temp3)

# to exclude rows with any zeros
Temp3 <- Temp3[!apply(Temp3 == 0, 1, any), ]

# Check the structure and head of the 'Temp3' dataset
str(Temp3)
head(Temp3)

#Write the 'Temp3' dataframe to a .dat file with a tab delimiter and save it
write.table(Temp3, file = "Temp3.dat", sep = "\t", row.names = FALSE, col.names = TRUE)

#Create the plot using ggplot2

# Convert the date strings in column 1 to POSIXct datetime objects
Temp3[, 1] <- as.POSIXct(Temp3[, 1], format = "%Y-%m-%d %H:%M")

#Choose your desired y-axis limits (and change it manually in line below).
#y_min <- min(c(Temp3[, 2], Temp3[, 3], Temp3[, 4], Temp3[, 5], Temp3[, 6], Temp3[, 7]), na.rm = TRUE)
#y_max <- max(c(Temp3[, 2], Temp3[, 3], Temp3[, 4], Temp3[, 5], Temp3[, 6], Temp3[, 7]), na.rm = TRUE)

#Create a plot with timestamp on the x-axis and adjusted y-axis limits (adjust the y_max and y_min as you wish)
plot(Temp3[, 1], Temp3[, 2], type = 'l', col = 'red', xlab = 'Timestamp', ylab = 'Values',
     main = 'Temperature3 Over Time', ylim = c(23, 30))

# Add the rest of the series to the same plot
lines(Temp3[, 1], Temp3[, 3], col = 'blue')
lines(Temp3[, 1], Temp3[, 4], col = 'green')
lines(Temp3[, 1], Temp3[, 5], col = 'purple')
lines(Temp3[, 1], Temp3[, 6], col = 'orange')
lines(Temp3[, 1], Temp3[, 7], col = 'yellow')

# Add a legend to distinguish the different lines
legend("bottomright", legend = c("Temp3 1", "Temp3 2", "Temp3 3", "Temp3 4", "Temp3 5", "Temp3 6"),
       col = c('red', 'blue', 'green', 'purple', 'orange', 'yellow'), lty = 1, cex = 0.8)

# Do some basic statistics
# Assuming Temp3 is your dataset with named columns

# List of column names from the 2nd to the 7th
columnNames_Temp3 <- names(Temp3)[2:7]

# Calculate mean and SD for each of the specified columns
means_Temp3 <- sapply(Temp3[, columnNames_Temp3], mean, na.rm = TRUE)
sds_Temp3 <- sapply(Temp3[, columnNames_Temp3], sd, na.rm = TRUE)

# Create a new dataframe for the results
results_Temp3 <- data.frame(
  Mean_Temp3 = means_Temp3,
  SD_Temp3 = sds_Temp3
)

# Set the row names of the results dataframe to the column names
rownames(results_Temp3) <- columnNames_Temp3

# Display the results
print(results_Temp3)

# make a graph to compare Temp3 across lysimeters

# Assuming 'results' dataframe contains the mean and SD information
# for each of the columns

# Create a bar plot for the mean values
barplot(results_Temp3$Mean_Temp3, names.arg = rownames(results_Temp3), beside = TRUE,
        col = 'lightblue', ylim = c(0, max(results_Temp3$Mean_Temp3 + results_Temp3$SD_Temp3)),
        main = "Mean values with SD error bars", ylab = "Mean values")

# Add error bars for SD
# Calculate the center position of each bar on the x-axis
bar_centers_Temp3 <- barplot(results_Temp3$Mean_Temp3, plot = FALSE)

# Add the error bars using the arrow function
arrows(x0 = bar_centers_Temp3, y0 = results_Temp3$Mean_Temp3 - results_Temp3$SD_Temp3,
       x1 = bar_centers_Temp3, y1 = results_Temp3$Mean_Temp3 + results_Temp3$SD_Temp3,
       angle = 90, code = 3, length = 0.1, col = 'darkred')

# Do ANOVA

# Ensure that you have the reshape2 package installed

#Melt the data frame from wide format to long format for ANOVA
longTemp3 <- melt(Temp3, id.vars = "TIMESTAMP", measure.vars = colnames(Temp3)[2:7])

# Perform ANOVA
aov_results_Temp3 <- aov(value ~ variable, data = longTemp3)

# Display the summary of the ANOVA
summary(aov_results_Temp3)

# Post-hoc test if ANOVA significant
if (summary(aov_results_Temp3)[[1]][["Pr(>F)"]][1] < 0.05) {
  TukeyHSD(aov_results_Temp3)
} else {
  print("ANOVA is not significant; post-hoc analysis is not applicable.")
}

##################SV_VWC3##########
# Check the structure and head of the 'SV_VWC3' dataset
str(SV_VWC3)
head(SV_VWC3)

# Check to see whether data are "as character" or "numeric"
sapply(SV_VWC3[, 2:10], class)

# Convert all columns 2 to 10 to numeric if needed
SV_VWC3[, 2:10] <- sapply(SV_VWC3[, 2:10], function(x) as.numeric(as.character(x)))

# Remove rows that contain any NaN values

##First, loop through all elements and convert any "nan" or "NAN" strings to NaN
#SV_VWC3[SV_VWC3 == "nan" | SV_VWC3 == "NAN"] <- NaN
##Then, use na.omit() to remove any rows containing NaN values in the dataset
#SV_VWC3 <- na.omit(SV_VWC3)

# to exclude rows with any zeros
#SV_VWC3 <- SV_VWC3[!apply(SV_VWC3 == 0, 1, any), ]

# Check the structure and head of the 'SV_VWC3' dataset
str(SV_VWC3)
head(SV_VWC3)

#Write the 'SV_VWC3' dataframe to a .dat file with a tab delimiter and save it
write.table(SV_VWC3, file = "SV_VWC3.dat", sep = "\t", row.names = FALSE, col.names = TRUE)

#Create the plot using ggplot2

# Convert the date strings in column 1 to POSIXct datetime objects
SV_VWC3[, 1] <- as.POSIXct(SV_VWC3[, 1], format = "%Y-%m-%d %H:%M")

#Choose your desired y-axis limits (and change it manually in line below).
#y_min <- min(c(SV_VWC3[, 2], SV_VWC3[, 3], SV_VWC3[, 4], SV_VWC3[, 5], SV_VWC3[, 6], SV_VWC3[, 7], SV_VWC3[, 8], SV_VWC3[, 9], SV_VWC3[, 10])), na.rm = TRUE)
#y_max <- max(c(SV_VWC3[, 2], SV_VWC3[, 3], SV_VWC3[, 4], SV_VWC3[, 5], SV_VWC3[, 6], SV_VWC3[, 7], SV_VWC3[, 8], SV_VWC3[, 9], SV_VWC3[, 10]), na.rm = TRUE)

#Create a plot with timestamp on the x-axis and adjusted y-axis limits (adjust the y_max and y_min as you wish)
plot(SV_VWC3[, 1], SV_VWC3[, 2], type = 'l', col = 'red', xlab = 'Timestamp', ylab = 'Values',
     main = 'SV_VWC3 Over Time', ylim = c(0.30, 0.45))

# Add the rest of the series to the same plot
lines(SV_VWC3[, 1], SV_VWC3[, 3], col = 'blue')
lines(SV_VWC3[, 1], SV_VWC3[, 4], col = 'green')
lines(SV_VWC3[, 1], SV_VWC3[, 5], col = 'purple')
lines(SV_VWC3[, 1], SV_VWC3[, 6], col = 'orange')
lines(SV_VWC3[, 1], SV_VWC3[, 7], col = 'yellow')
lines(SV_VWC3[, 1], SV_VWC3[, 8], col = 'brown')
lines(SV_VWC3[, 1], SV_VWC3[, 9], col = 'lightblue')
lines(SV_VWC3[, 1], SV_VWC3[, 10], col = 'lightgreen')

# Add a legend to distinguish the different lines
legend("bottomright", legend = c("SV_VWC3 5cm", "SV_VWC3 10cm", "SV_VWC3 20cm", "SV_VWC3 30cm", "SV_VWC3 40cm", "SV_VWC3 50cm", "SV_VWC3 60cm", "SV_VWC3 75cm", "SV_VWC3 100cm"),
       col = c('red', 'blue', 'green', 'purple', 'orange', 'yellow', 'brown', 'lightblue', 'lightgreen'), lty = 1, cex = 0.8)

# Do some basic statistics
# Assuming SV_VWC3 is your dataset with named columns

# List of column names from the 2nd to the 7th
columnNames_SV_VWC3 <- names(SV_VWC3)[2:10]

# Calculate mean and SD for each of the specified columns
means_SV_VWC3 <- sapply(SV_VWC3[, columnNames_SV_VWC3], mean, na.rm = TRUE)
sds_SV_VWC3 <- sapply(SV_VWC3[, columnNames_SV_VWC3], sd, na.rm = TRUE)

# Create a new dataframe for the results
results_SV_VWC3 <- data.frame(
  Mean_SV_VWC3 = means_SV_VWC3,
  SD_SV_VWC3 = sds_SV_VWC3
)

# Set the row names of the results dataframe to the column names
rownames(results_SV_VWC3) <- columnNames_SV_VWC3

# Display the results
print(results_SV_VWC3)

# make a graph to compare SV_VWC3 in soil profile

# Assuming 'results' dataframe contains the mean and SD information
# for each of the columns

# Create a bar plot for the mean values
barplot(results_SV_VWC3$Mean_SV_VWC3, names.arg = rownames(results_SV_VWC3), beside = TRUE,
        col = 'lightblue', ylim = c(0, max(results_SV_VWC3$Mean_SV_VWC3 + results_SV_VWC3$SD_SV_VWC3)),
        main = "Mean values with SD error bars", ylab = "Mean values")

# Add error bars for SD
# Calculate the center position of each bar on the x-axis
bar_centers_SV_VWC3 <- barplot(results_SV_VWC3$Mean_SV_VWC3, plot = FALSE)

# Add the error bars using the arrows function
arrows(x0 = bar_centers_SV_VWC3, y0 = results_SV_VWC3$Mean_SV_VWC3 - results_SV_VWC3$SD_SV_VWC3,
       x1 = bar_centers_SV_VWC3, y1 = results_SV_VWC3$Mean_SV_VWC3 + results_SV_VWC3$SD_SV_VWC3,
       angle = 90, code = 3, length = 0.1, col = 'darkred')

# Do ANOVA
# Ensure that you have the reshape2 package installed

#Melt the data frame from wide format to long format for ANOVA
longSV_VWC3 <- melt(SV_VWC3, id.vars = "TIMESTAMP", measure.vars = colnames(SV_VWC3)[2:10])

# Perform ANOVA
aov_results_SV_VWC3 <- aov(value ~ variable, data = longSV_VWC3)

# Display the summary of the ANOVA
summary(aov_results_SV_VWC3)

##Post-hoc test if ANOVA significant
#if (summary(aov_results_SV_VWC3)[[1]][["Pr(>F)"]][1] < 0.05) {
  #TukeyHSD(aov_results_SV_VWC3)
#} else {
 # print("ANOVA is not significant; post-hoc analysis is not applicable.")
#}



##################SV_EC3##########
# Check the structure and head of the 'SV_EC3' dataset
str(SV_EC3)
head(SV_EC3)

# Check to see whether data are "as character" or "numeric"
sapply(SV_EC3[, 2:10], class)

# Convert all columns 2 to 10 to numeric if needed
SV_EC3[, 2:10] <- sapply(SV_EC3[, 2:10], function(x) as.numeric(as.character(x)))

# Remove rows that contain any NaN values

##First, loop through all elements and convert any "nan" or "NAN" strings to NaN
#SV_EC3[SV_EC3 == "nan" | SV_EC3 == "NAN"] <- NaN
##Then, use na.omit() to remove any rows containing NaN values in the dataset
#SV_EC3 <- na.omit(SV_EC3)

# to exclude rows with any zeros
#SV_EC3 <- SV_EC3[!apply(SV_EC3 == 0, 1, any), ]

# Check the structure and head of the 'SV_EC3' dataset
str(SV_EC3)
head(SV_EC3)

#Write the 'SV_EC3' dataframe to a .dat file with a tab delimiter and save it
write.table(SV_EC3, file = "SV_EC3.dat", sep = "\t", row.names = FALSE, col.names = TRUE)

#Create the plot using ggplot2

# Convert the date strings in column 1 to POSIXct datetime objects
SV_EC3[, 1] <- as.POSIXct(SV_EC3[, 1], format = "%Y-%m-%d %H:%M")

#Choose your desired y-axis limits (and change it manually in line below).
#y_min <- min(c(SV_EC3[, 2], SV_EC3[, 3], SV_EC3[, 4], SV_EC3[, 5], SV_EC3[, 6], SV_EC3[, 7], SV_EC3[, 8], SV_EC3[, 9], SV_EC3[, 10])), na.rm = TRUE)
#y_max <- max(c(SV_EC3[, 2], SV_EC3[, 3], SV_EC3[, 4], SV_EC3[, 5], SV_EC3[, 6], SV_EC3[, 7], SV_EC3[, 8], SV_EC3[, 9], SV_EC3[, 10]), na.rm = TRUE)

#Create a plot with timestamp on the x-axis and adjusted y-axis limits (adjust the y_max and y_min as you wish)
plot(SV_EC3[, 1], SV_EC3[, 2], type = 'l', col = 'red', xlab = 'Timestamp', ylab = 'Values',
     main = 'SV_EC3 Over Time', ylim = c(0.30, 0.45))

# Add the rest of the series to the same plot
lines(SV_EC3[, 1], SV_EC3[, 3], col = 'blue')
lines(SV_EC3[, 1], SV_EC3[, 4], col = 'green')
lines(SV_EC3[, 1], SV_EC3[, 5], col = 'purple')
lines(SV_EC3[, 1], SV_EC3[, 6], col = 'orange')
lines(SV_EC3[, 1], SV_EC3[, 7], col = 'yellow')
lines(SV_EC3[, 1], SV_EC3[, 8], col = 'brown')
lines(SV_EC3[, 1], SV_EC3[, 9], col = 'lightblue')
lines(SV_EC3[, 1], SV_EC3[, 10], col = 'lightgreen')

# Add a legend to distinguish the different lines
legend("bottomright", legend = c("SV_EC3 5cm", "SV_EC3 10cm", "SV_EC3 20cm", "SV_EC3 30cm", "SV_EC3 40cm", "SV_EC3 50cm", "SV_EC3 60cm", "SV_EC3 75cm", "SV_EC3 100cm"),
       col = c('red', 'blue', 'green', 'purple', 'orange', 'yellow', 'brown', 'lightblue', 'lightgreen'), lty = 1, cex = 0.8)

# Do some basic statistics
# Assuming SV_EC3 is your dataset with named columns

# List of column names from the 2nd to the 7th
columnNames_SV_EC3 <- names(SV_EC3)[2:10]

# Calculate mean and SD for each of the specified columns
means_SV_EC3 <- sapply(SV_EC3[, columnNames_SV_EC3], mean, na.rm = TRUE)
sds_SV_EC3 <- sapply(SV_EC3[, columnNames_SV_EC3], sd, na.rm = TRUE)

# Create a new dataframe for the results
results_SV_EC3 <- data.frame(
  Mean_SV_EC3 = means_SV_EC3,
  SD_SV_EC3 = sds_SV_EC3
)

# Set the row names of the results dataframe to the column names
rownames(results_SV_EC3) <- columnNames_SV_EC3

# Display the results
print(results_SV_EC3)

# make a graph to compare SV_EC3 in soil profile

# Assuming 'results' dataframe contains the mean and SD information
# for each of the columns

# Create a bar plot for the mean values
barplot(results_SV_EC3$Mean_SV_EC3, names.arg = rownames(results_SV_EC3), beside = TRUE,
        col = 'lightblue', ylim = c(0, max(results_SV_EC3$Mean_SV_EC3 + results_SV_EC3$SD_SV_EC3)),
        main = "Mean values with SD error bars", ylab = "Mean values")

# Add error bars for SD
# Calculate the center position of each bar on the x-axis
bar_centers_SV_EC3 <- barplot(results_SV_EC3$Mean_SV_EC3, plot = FALSE)

# Add the error bars using the arrows function
arrows(x0 = bar_centers_SV_EC3, y0 = results_SV_EC3$Mean_SV_EC3 - results_SV_EC3$SD_SV_EC3,
       x1 = bar_centers_SV_EC3, y1 = results_SV_EC3$Mean_SV_EC3 + results_SV_EC3$SD_SV_EC3,
       angle = 90, code = 3, length = 0.1, col = 'darkred')

# Do ANOVA
# Ensure that you have the reshape2 package installed

#Melt the data frame from wide format to long format for ANOVA
longSV_EC3 <- melt(SV_EC3, id.vars = "TIMESTAMP", measure.vars = colnames(SV_EC3)[2:10])

# Perform ANOVA
aov_results_SV_EC3 <- aov(value ~ variable, data = longSV_EC3)

# Display the summary of the ANOVA
summary(aov_results_SV_EC3)

##Post-hoc test if ANOVA significant
#if (summary(aov_results_SV_EC3)[[1]][["Pr(>F)"]][1] < 0.05) {
  #TukeyHSD(aov_results_SV_EC3)
#} else {
  #print("ANOVA is not significant; post-hoc analysis is not applicable.")
#}



##################SV_Temp3##########
str(SV_Temp3)
head(SV_Temp3)

# Check to see whether data are "as character" or "numeric"
sapply(SV_Temp3[, 2:10], class)

# Convert all columns 2 to 10 to numeric if needed
SV_Temp3[, 2:10] <- sapply(SV_Temp3[, 2:10], function(x) as.numeric(as.character(x)))

# Remove rows that contain any NaN values

##First, loop through all elements and convert any "nan" or "NAN" strings to NaN
#SV_Temp3[SV_Temp3 == "nan" | SV_Temp3 == "NAN"] <- NaN
##Then, use na.omit() to remove any rows containing NaN values in the dataset
#SV_Temp3 <- na.omit(SV_Temp3)

# to exclude rows with any zeros
#SV_Temp3 <- SV_Temp3[!apply(SV_Temp3 == 0, 1, any), ]

# Check the structure and head of the 'SV_Temp3' dataset
str(SV_Temp3)
head(SV_Temp3)

#Write the 'SV_Temp3' dataframe to a .dat file with a tab delimiter and save it
write.table(SV_Temp3, file = "SV_Temp3.dat", sep = "\t", row.names = FALSE, col.names = TRUE)

#Create the plot using ggplot2

# Convert the date strings in column 1 to POSIXct datetime objects
SV_Temp3[, 1] <- as.POSIXct(SV_Temp3[, 1], format = "%Y-%m-%d %H:%M")

#Choose your desired y-axis limits (and change it manually in line below).
#y_min <- min(c(SV_Temp3[, 2], SV_Temp3[, 3], SV_Temp3[, 4], SV_Temp3[, 5], SV_Temp3[, 6], SV_Temp3[, 7], SV_Temp3[, 8], SV_Temp3[, 9], SV_Temp3[, 10])), na.rm = TRUE)
#y_max <- max(c(SV_Temp3[, 2], SV_Temp3[, 3], SV_Temp3[, 4], SV_Temp3[, 5], SV_Temp3[, 6], SV_Temp3[, 7], SV_Temp3[, 8], SV_Temp3[, 9], SV_Temp3[, 10]), na.rm = TRUE)

#Create a plot with timestamp on the x-axis and adjusted y-axis limits (adjust the y_max and y_min as you wish)
plot(SV_Temp3[, 1], SV_Temp3[, 2], type = 'l', col = 'red', xlab = 'Timestamp', ylab = 'Values',
     main = 'SV_Temp3 Over Time', ylim = c(0.30, 0.45))

# Add the rest of the series to the same plot
lines(SV_Temp3[, 1], SV_Temp3[, 3], col = 'blue')
lines(SV_Temp3[, 1], SV_Temp3[, 4], col = 'green')
lines(SV_Temp3[, 1], SV_Temp3[, 5], col = 'purple')
lines(SV_Temp3[, 1], SV_Temp3[, 6], col = 'orange')
lines(SV_Temp3[, 1], SV_Temp3[, 7], col = 'yellow')
lines(SV_Temp3[, 1], SV_Temp3[, 8], col = 'brown')
lines(SV_Temp3[, 1], SV_Temp3[, 9], col = 'lightblue')
lines(SV_Temp3[, 1], SV_Temp3[, 10], col = 'lightgreen')

# Add a legend to distinguish the different lines
legend("bottomright", legend = c("SV_Temp3 5cm", "SV_Temp3 10cm", "SV_Temp3 20cm", "SV_Temp3 30cm", "SV_Temp3 40cm", "SV_Temp3 50cm", "SV_Temp3 60cm", "SV_Temp3 75cm", "SV_Temp3 100cm"),
       col = c('red', 'blue', 'green', 'purple', 'orange', 'yellow', 'brown', 'lightblue', 'lightgreen'), lty = 1, cex = 0.8)

# Do some basic statistics
# Assuming SV_Temp3 is your dataset with named columns

# List of column names from the 2nd to the 7th
columnNames_SV_Temp3 <- names(SV_Temp3)[2:10]

# Calculate mean and SD for each of the specified columns
means_SV_Temp3 <- sapply(SV_Temp3[, columnNames_SV_Temp3], mean, na.rm = TRUE)
sds_SV_Temp3 <- sapply(SV_Temp3[, columnNames_SV_Temp3], sd, na.rm = TRUE)

# Create a new dataframe for the results
results_SV_Temp3 <- data.frame(
  Mean_SV_Temp3 = means_SV_Temp3,
  SD_SV_Temp3 = sds_SV_Temp3
)

# Set the row names of the results dataframe to the column names
rownames(results_SV_Temp3) <- columnNames_SV_Temp3

# Display the results
print(results_SV_Temp3)

# make a graph to compare SV_Temp3 in soil profile

# Assuming 'results' dataframe contains the mean and SD information
# for each of the columns

# Create a bar plot for the mean values
barplot(results_SV_Temp3$Mean_SV_Temp3, names.arg = rownames(results_SV_Temp3), beside = TRUE,
        col = 'lightblue', ylim = c(0, max(results_SV_Temp3$Mean_SV_Temp3 + results_SV_Temp3$SD_SV_Temp3)),
        main = "Mean values with SD error bars", ylab = "Mean values")

# Add error bars for SD
# Calculate the center position of each bar on the x-axis
bar_centers_SV_Temp3 <- barplot(results_SV_Temp3$Mean_SV_Temp3, plot = FALSE)

# Add the error bars using the arrows function
arrows(x0 = bar_centers_SV_Temp3, y0 = results_SV_Temp3$Mean_SV_Temp3 - results_SV_Temp3$SD_SV_Temp3,
       x1 = bar_centers_SV_Temp3, y1 = results_SV_Temp3$Mean_SV_Temp3 + results_SV_Temp3$SD_SV_Temp3,
       angle = 90, code = 3, length = 0.1, col = 'darkred')

# Do ANOVA
# Ensure that you have the reshape2 package installed

#Melt the data frame from wide format to long format for ANOVA
longSV_Temp3 <- melt(SV_Temp3, id.vars = "TIMESTAMP", measure.vars = colnames(SV_Temp3)[2:10])

# Perform ANOVA
aov_results_SV_Temp3 <- aov(value ~ variable, data = longSV_Temp3)

# Display the summary of the ANOVA
summary(aov_results_SV_Temp3)

# Post-hoc test if ANOVA significant
#if (summary(aov_results_SV_Temp3)[[1]][["Pr(>F)"]][1] < 0.05) {
  #TukeyHSD(aov_results_SV_Temp3)
#} else {
 # print("ANOVA is not significant; post-hoc analysis is not applicable.")
#}




################################################### Lysimeter Data Processing##########################



###################################  Treatment 1################

#uploading Lysimeter data from .dat file
#Read the "Ross T1_LysimeterData.dat" file without headers
tempData_L1 <- read.table(file = "Ross T1_LysimeterData.dat", header = FALSE, sep = ",", fill = TRUE)


# Check that the file has been read correctly by looking at the top
head(tempData_L1)
str(tempData_L1)

# Set the column names to the values of the second row
colnames(tempData_L1) <- as.character(tempData_L1[2, ])

# Remove unnecessary rows 
LysData <- tempData_L1[-c(1, 2, 3, 4), ]

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


#############Lys1_mod_depth ###########

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
write.table(Lys_mod_depth, file = "Lys1_mod_depth.dat", sep = "\t", row.names = FALSE, col.names = TRUE)



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



############ Lys1_Temp ######

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
write.table(Lys_Temp, file = "Lys1_Temp.dat", sep = "\t", row.names = FALSE, col.names = TRUE)

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




#############Lys1_EC#########

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
write.table(Lys_EC, file = "Lys1_EC.dat", sep = "\t", row.names = FALSE, col.names = TRUE)

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





##################################  Treatment 2################

#uploading Lysimeter data from .dat file
#Read the "Ross T2_LysimeterData.dat" file without headers
tempData_L2 <- read.table(file = "Ross T2_LysimeterData.dat", header = FALSE, sep = ",", fill = TRUE)


# Check that the file has been read correctly by looking at the top
head(tempData_L2)
str(tempData_L2)

# Set the column names to the values of the second row
colnames(tempData_L2) <- as.character(tempData_L2[2, ])

# Remove unnecessary rows 
LysData2 <- tempData_L2[-c(1, 2, 3, 4), ]

# Reset row names to start at 1 and continue sequentially
rownames(LysData2) <- NULL

# Confirm the changes by looking at the top of the modified dataframe
head(LysData2)
str(LysData2)


# Create new data frame 'EC' using the column indices
Lys2_mod_depth <- LysData2[, c(1, 4, 10, 16, 22, 28, 34)]
Lys2_Temp <- LysData2[, c(1, 5, 11, 17, 23, 29, 35)]
Lys2_EC <- LysData2[, c(1, 6, 12, 18, 24, 30, 36)]



# Alternatively, if the columns are consecutive, you can use
#Lys_mod_depth <- LysData[, 1:6]
#Lys_Temp <- LysData[, 1:6]
#Lys_EC <- LysData[, 1:6]


#############Lys2_mod_depth#########

# Check the structure and head of the 'Lys2_mod_depth' dataset
str(Lys2_mod_depth)
head(Lys2_mod_depth)

# Check to see whether data are "as character" or "numeric"
sapply(Lys2_mod_depth[, 2:7], class)

# Convert all columns 2 to 7 to numeric if needed
Lys2_mod_depth[, 2:7] <- sapply(Lys2_mod_depth[, 2:7], function(x) as.numeric(as.character(x)))

# Remove rows that contain any NaN values

# First, loop through all elements and convert any "nan" or "NAN" strings to NA
Lys2_mod_depth[Lys2_mod_depth == "nan" | Lys2_mod_depth == "NAN"] <- NA
# Then, use na.omit() to remove any rows containing NA values in the dataset
Lys2_mod_depth <- na.omit(Lys2_mod_depth)

# to exclude rows with any zeros
Lys2_mod_depth <- Lys2_mod_depth[!apply(Lys2_mod_depth == 0, 1, any), ]

# Check the structure and head of the 'Lys2_mod_depth' dataset
str(Lys2_mod_depth)
head(Lys2_mod_depth)

#Write the 'Lys2_mod_depth' dataframe to a .dat file with a tab delimiter and save it
write.table(Lys2_mod_depth, file = "Lys2_mod_depth.dat", sep = "\t", row.names = FALSE, col.names = TRUE)

# Create the plot using ggplot2

# Convert the date strings in column 1 to POSIXct datetime objects
Lys2_mod_depth[, 1] <- as.POSIXct(Lys2_mod_depth[, 1], format = "%Y-%m-%d %H:%M")

# Choose your desired y-axis limits (and change it manually in line below).
# y_min <- min(c(Lys2_mod_depth[, 2], Lys2_mod_depth[, 3], Lys2_mod_depth[, 4], Lys2_mod_depth[, 5], Lys2_mod_depth[, 6], Lys2_mod_depth[, 7]), na.rm = TRUE)
# y_max <- max(c(Lys2_mod_depth[, 2], Lys2_mod_depth[, 3], Lys2_mod_depth[, 4], Lys2_mod_depth[, 5], Lys2_mod_depth[, 6], Lys2_mod_depth[, 7]), na.rm = TRUE)

# Create a plot with timestamp on the x-axis and adjusted y-axis limits (adjust the y_max and y_min as you wish)
plot(Lys2_mod_depth[, 1], Lys2_mod_depth[, 2], type = 'l', col = 'red', xlab = 'Timestamp', ylab = 'Values',
     main = 'Lys2_mod_depth Over Time', ylim = c(0, 2200))

# Add the rest of the series to the same plot
lines(Lys2_mod_depth[, 1], Lys2_mod_depth[, 3], col = 'blue')
lines(Lys2_mod_depth[, 1], Lys2_mod_depth[, 4], col = 'green')
lines(Lys2_mod_depth[, 1], Lys2_mod_depth[, 5], col = 'purple')
lines(Lys2_mod_depth[, 1], Lys2_mod_depth[, 6], col = 'orange')
lines(Lys2_mod_depth[, 1], Lys2_mod_depth[, 7], col = 'yellow')

# Add a legend to distinguish the different lines
legend("bottomright", legend = c("Lys2_mod_depth 1", "Lys2_mod_depth 2", "Lys2_mod_depth 3", "Lys2_mod_depth 4", "Lys2_mod_depth 5", "Lys2_mod_depth 6"),
       col = c('red', 'blue', 'green', 'purple', 'orange', 'yellow'), lty = 1, cex = 0.8)

# Do some basic statistics
# Assuming Lys2_mod_depth is your dataset with named columns

# List of column names from the 2nd to the 7th
columnNames_Lys2_mod_depth <- names(Lys2_mod_depth)[2:7]

# Calculate mean and SD for each of the specified columns
means_Lys2_mod_depth <- sapply(Lys2_mod_depth[, columnNames_Lys2_mod_depth], mean, na.rm = TRUE)
sds_Lys2_mod_depth <- sapply(Lys2_mod_depth[, columnNames_Lys2_mod_depth], sd, na.rm = TRUE)

# Create a new dataframe for the results
results_Lys2_mod_depth <- data.frame(
  Mean_Lys2_mod_depth = means_Lys2_mod_depth,
  SD_Lys2_mod_depth = sds_Lys2_mod_depth
)

# Set the row names of the results dataframe to the column names
rownames(results_Lys2_mod_depth) <- columnNames_Lys2_mod_depth

# Display the results
print(results_Lys2_mod_depth)

# make a graph to compare Lys2_mod_depth across lysimeters

# Assuming 'results' dataframe contains the mean and SD information
# for each of the columns

# Create a bar plot for the mean values
barplot(results_Lys2_mod_depth$Mean, names.arg = rownames(results_Lys2_mod_depth), beside = TRUE,
        col = 'lightblue', ylim = c(0, max(results_Lys2_mod_depth$Mean + results_Lys2_mod_depth$SD)),
        main = "Mean values with SD error bars", ylab = "Mean values")

# Add error bars for SD
# Calculate the centre position of each bar on the x-axis
bar_centers_Lys2_mod_depth <- barplot(results_Lys2_mod_depth$Mean, plot = FALSE)

# Add the error bars using the arrow function
arrows(x0 = bar_centers_Lys2_mod_depth, y0 = results_Lys2_mod_depth$Mean - results_Lys2_mod_depth$SD,
       x1 = bar_centers_Lys2_mod_depth, y1 = results_Lys2_mod_depth$Mean + results_Lys2_mod_depth$SD,
       angle = 90, code = 3, length = 0.1, col = 'darkred')

# Do ANOVA
# Ensure that you have the reshape2 package installed

#Melt the data frame from wide format to long format for ANOVA
longLys2_mod_depth <- melt(Lys2_mod_depth, id.vars = "TIMESTAMP", measure.vars = colnames(Lys2_mod_depth)[2:7])

# Perform ANOVA
aov_results_Lys2_mod_depth <- aov(value ~ variable, data = longLys2_mod_depth)

# Display the summary of the ANOVA
summary(aov_results_Lys2_mod_depth)

# Post-hoc test if ANOVA significant
if (summary(aov_results_Lys2_mod_depth)[[1]][["Pr(>F)"]][1] < 0.05) {
  TukeyHSD(aov_results_Lys2_mod_depth)
} else {
  print("ANOVA is not significant; post-hoc analysis is not applicable.")
}

#############Lys2_Temp########

# Check the structure and head of the 'Lys2_Temp' dataset
str(Lys2_Temp)
head(Lys2_Temp)

# Check to see whether data are "as character" or "numeric"
sapply(Lys2_Temp[, 2:7], class)

# Convert all columns 2 to 7 to numeric if needed
Lys2_Temp[, 2:7] <- sapply(Lys2_Temp[, 2:7], function(x) as.numeric(as.character(x)))

# Remove rows that contain any NaN values
# First, loop through all elements and convert any "nan" or "NAN" strings to NA
Lys2_Temp[Lys2_Temp == "nan" | Lys2_Temp == "NAN"] <- NA

# Then, use na.omit() to remove any rows containing NA values in the dataset
Lys2_Temp <- na.omit(Lys2_Temp)

# to exclude rows with any zeros
Lys2_Temp <- Lys2_Temp[!apply(Lys2_Temp == 0, 1, any), ]

# Check the structure and head of the 'Lys2_Temp' dataset
str(Lys2_Temp)
head(Lys2_Temp)

# Write the 'Lys2_Temp' dataframe to a .dat file with a tab delimiter and save it
write.table(Lys2_Temp, file = "Lys2_Temp.dat", sep = "\t", row.names = FALSE, col.names = TRUE)

# Create the plot using ggplot2

# Convert the date strings in column 1 to POSIXct datetime objects
Lys2_Temp[, 1] <- as.POSIXct(Lys2_Temp[, 1], format = "%Y-%m-%d %H:%M")

# Choose your desired y-axis limits (and change it manually in line below).
#y_min <- min(c(Lys2_Temp[, 2], Lys2_Temp[, 3], Lys2_Temp[, 4], Lys2_Temp[, 5], Lys2_Temp[, 6], Lys2_Temp[, 7]), na.rm = TRUE)
#y_max <- max(c(Lys2_Temp[, 2], Lys2_Temp[, 3], Lys2_Temp[, 4], Lys2_Temp[, 5], Lys2_Temp[, 6], Lys2_Temp[, 7]), na.rm = TRUE)

# Create a plot with timestamp on the x-axis and adjusted y-axis limits (adjust the y_max and y_min as you wish)
plot(Lys2_Temp[, 1], Lys2_Temp[, 2], type = 'l', col = 'red', xlab = 'Timestamp', ylab = 'Values', 
     main = 'Lys2_Temp Over Time', ylim = c(23, 29))

# Add the rest of the series to the same plot
lines(Lys2_Temp[, 1], Lys2_Temp[, 3], col = 'blue')
lines(Lys2_Temp[, 1], Lys2_Temp[, 4], col = 'green')
lines(Lys2_Temp[, 1], Lys2_Temp[, 5], col = 'purple')
lines(Lys2_Temp[, 1], Lys2_Temp[, 6], col = 'orange')
lines(Lys2_Temp[, 1], Lys2_Temp[, 7], col = 'yellow')
# Add a legend to distinguish the different lines
legend("bottomright", legend = c("Lys2_Temp 1", "Lys2_Temp 2", "Lys2_Temp 3", "Lys2_Temp 4", "Lys2_Temp 5", "Lys2_Temp 6"), 
       col = c('red', 'blue', 'green', 'purple', 'orange', 'yellow'), lty = 1, cex = 0.8)

# Do some basic statistics
# Assuming Lys2_Temp is your dataset with named columns

# List of column names from the 2nd to the 7th
columnNames_Lys2_Temp <- names(Lys2_Temp)[2:7]

# Calculate mean and SD for each of the specified columns
means_Lys2_Temp <- sapply(Lys2_Temp[, columnNames_Lys2_Temp], mean, na.rm = TRUE)
sds_Lys2_Temp <- sapply(Lys2_Temp[, columnNames_Lys2_Temp], sd, na.rm = TRUE)

# Create a new dataframe for the results
results_Lys2_Temp <- data.frame(
  Mean_Lys2_Temp = means_Lys2_Temp,
  SD_Lys2_Temp = sds_Lys2_Temp
)

# Set the row names of the results dataframe to the column names
rownames(results_Lys2_Temp) <- columnNames_Lys2_Temp

# Display the results
print(results_Lys2_Temp)

# Make a graph to compare Lys2_Temp across lysimeters

# Assuming 'results' dataframe contains the mean and SD information
# for each of the columns

# Create a bar plot for the mean values
barplot(results_Lys2_Temp$Mean_Lys2_Temp, names.arg = rownames(results_Lys2_Temp), beside = TRUE,
        col = 'lightblue', ylim = c(0, max(results_Lys2_Temp$Mean_Lys2_Temp + results_Lys2_Temp$SD_Lys2_Temp)),
        main = "Mean values with SD error bars", ylab = "Mean values")

# Add error bars for SD
# Calculate the centre position of each bar on the x-axis
bar_centers_Lys2_Temp <- barplot(results_Lys2_Temp$Mean_Lys2_Temp, plot = FALSE)

# Add the error bars using the arrow function
arrows(x0 = bar_centers_Lys2_Temp, y0 = results_Lys2_Temp$Mean_Lys2_Temp - results_Lys2_Temp$SD_Lys2_Temp,
       x1 = bar_centers_Lys2_Temp, y1 = results_Lys2_Temp$Mean_Lys2_Temp + results_Lys2_Temp$SD_Lys2_Temp,
       angle = 90, code = 3, length = 0.1, col = 'darkred')

# Do ANOVA

# Ensure that you have the reshape2 package installed

#Melt the data frame from wide format to long format for ANOVA
longLys2_Temp <- melt(Lys2_Temp, id.vars = "TIMESTAMP", measure.vars = colnames(Lys2_Temp)[2:7])

# Perform ANOVA
aov_results_Lys2_Temp <- aov(value ~ variable, data = longLys2_Temp)

# Display the summary of the ANOVA
summary(aov_results_Lys2_Temp)

# Post-hoc test if ANOVA significant
if (summary(aov_results_Lys2_Temp)[[1]][["Pr(>F)"]][1] < 0.05) {
  TukeyHSD(aov_results_Lys2_Temp)
} else {
  print("ANOVA is not significant; post-hoc analysis is not applicable.")
}

#############Lys2_EC#########


# Check the structure and head of the 'Lys2_EC' dataset
str(Lys2_EC)
head(Lys2_EC)

# Check to see whether data are "as character" or "numeric"
sapply(Lys2_EC[, 2:7], class)

# Convert all columns 2 to 7 to numeric if needed
Lys2_EC[, 2:7] <- sapply(Lys2_EC[, 2:7], function(x) as.numeric(as.character(x)))

# Remove rows that contain any NaN values
# First, loop through all elements and convert any "nan" or "NAN" strings to NA
Lys2_EC[Lys2_EC == "nan" | Lys2_EC == "NAN"] <- NA

# Then, use na.omit() to remove any rows containing NA values in the dataset
Lys2_EC <- na.omit(Lys2_EC)

# to exclude rows with any zeros
Lys2_EC <- Lys2_EC[!apply(Lys2_EC == 0, 1, any), ]

# Check the structure and head of the 'Lys2_EC' dataset
str(Lys2_EC)
head(Lys2_EC)

#Write the 'Lys2_EC' dataframe to a .dat file with a tab delimiter and save it
write.table(Lys2_EC, file = "Lys2_EC.dat", sep = "\t", row.names = FALSE, col.names = TRUE)

#Create the plot using ggplot2

# Convert the date strings in column 1 to POSIXct datetime objects
Lys2_EC[, 1] <- as.POSIXct(Lys2_EC[, 1], format = "%Y-%m-%d %H:%M")

#Choose your desired y-axis limits (and change it manually in line below).
#y_min <- min(c(Lys2_EC[, 2], Lys2_EC[, 3], Lys2_EC[, 4], Lys2_EC[, 5], Lys2_EC[, 6], Lys2_EC[, 7]), na.rm = TRUE)
#y_max <- max(c(Lys2_EC[, 2], Lys2_EC[, 3], Lys2_EC[, 4], Lys2_EC[, 5], Lys2_EC[, 6], Lys2_EC[, 7]), na.rm = TRUE)

#Create a plot with timestamp on the x-axis and adjusted y-axis limits (adjust the y_max and y_min as you wish)
plot(Lys2_EC[, 1], Lys2_EC[, 2], type = 'l', col = 'red', xlab = 'Timestamp', ylab = 'Values',
     main = 'Lys2_EC Over Time', ylim = c(0, 1200))

# Add the rest of the series to the same plot
lines(Lys2_EC[, 1], Lys2_EC[, 3], col = 'blue')
lines(Lys2_EC[, 1], Lys2_EC[, 4], col = 'green')
lines(Lys2_EC[, 1], Lys2_EC[, 5], col = 'purple')
lines(Lys2_EC[, 1], Lys2_EC[, 6], col = 'orange')
lines(Lys2_EC[, 1], Lys2_EC[, 7], col = 'yellow')
# Add a legend to distinguish the different lines
legend("bottomright", legend = c("Lys2_EC 1", "Lys2_EC 2", "Lys2_EC 3", "Lys2_EC 4", "Lys2_EC 5", "Lys2_EC 6"),
       col = c('red', 'blue', 'green', 'purple', 'orange', 'yellow'), lty = 1, cex = 0.8)


# Do some basic statistics
# Assuming Lys2_EC is your dataset with named columns

# List of column names from the 2nd to the 7th
columnNames_Lys2_EC <- names(Lys2_EC)[2:7]

# Calculate mean and SD for each of the specified columns
means_Lys2_EC <- sapply(Lys2_EC[, columnNames_Lys2_EC], mean, na.rm = TRUE)
sds_Lys2_EC <- sapply(Lys2_EC[, columnNames_Lys2_EC], sd, na.rm = TRUE)

# Create a new dataframe for the results
results_Lys2_EC <- data.frame(
  Mean_Lys2_EC = means_Lys2_EC,
  SD_Lys2_EC = sds_Lys2_EC
)

# Set the row names of the results dataframe to the column names
rownames(results_Lys2_EC) <- columnNames_Lys2_EC

# Display the results
print(results_Lys2_EC)

# make a graph to compare Lys2_EC across lysimeters

# Assuming 'results' dataframe contains the mean and SD information
# for each of the columns

# Create a bar plot for the mean values
barplot(results_Lys2_EC$Mean_Lys2_EC, names.arg = rownames(results_Lys2_EC), beside = TRUE,
        col = 'lightblue', ylim = c(0, max(results_Lys2_EC$Mean_Lys2_EC + results_Lys2_EC$SD_Lys2_EC)),
        main = "Mean values with SD error bars", ylab = "Mean values")

# Add error bars for SD
# Calculate the center position of each bar on the x-axis
bar_centers_Lys2_EC <- barplot(results_Lys2_EC$Mean_Lys2_EC, plot = FALSE)

# Add the error bars using the arrow function
arrows(x0 = bar_centers_Lys2_EC, y0 = results_Lys2_EC$Mean_Lys2_EC - results_Lys2_EC$SD_Lys2_EC,
       x1 = bar_centers_Lys2_EC, y1 = results_Lys2_EC$Mean_Lys2_EC + results_Lys2_EC$SD_Lys2_EC,
       angle = 90, code = 3, length = 0.1, col = 'darkred')

# Do ANOVA

# Ensure that you have the reshape2 package installed

#Melt the data frame from wide format to long format for ANOVA
longLys2_EC <- melt(Lys2_EC, id.vars = "TIMESTAMP", measure.vars = colnames(Lys2_EC)[2:7])

# Perform ANOVA
aov_results_Lys2_EC <- aov(value ~ variable, data = longLys2_EC)

# Display the summary of the ANOVA
summary(aov_results_Lys2_EC)

# Post-hoc test if ANOVA significant
if (summary(aov_results_Lys2_EC)[[1]][["Pr(>F)"]][1] < 0.05) {
  TukeyHSD(aov_results_Lys2_EC)
} else {
  print("ANOVA is not significant; post-hoc analysis is not applicable.")
}




#################################   Treatment  3  ################


#Read the "Ross T3_LysimeterData.dat" file without headers
tempData_L3 <- read.table(file = "Ross T3_LysimeterData.dat", header = FALSE, sep = ",", fill = TRUE)


# Check that the file has been read correctly by looking at the top
head(tempData_L3)
str(tempData_L3)

# Set the column names to the values of the second row
colnames(tempData_L3) <- as.character(tempData_L3[2, ])

# Remove unnecessary rows 
LysData3 <- tempData_L3[-c(1, 2, 3, 4), ]

# Reset row names to start at 1 and continue sequentially
rownames(LysData3) <- NULL

# Confirm the changes by looking at the top of the modified dataframe
head(LysData3)
str(LysData3)


# Create new data frame 'EC' using the column indices
Lys3_mod_depth <- LysData3[, c(1, 4, 10, 16, 22, 28, 34)]
Lys3_Temp <- LysData3[, c(1, 5, 11, 17, 23, 29, 35)]
Lys3_EC <- LysData3[, c(1, 6, 12, 18, 24, 30, 36)]



# Alternatively, if the columns are consecutive, you can use
#Lys_mod_depth <- LysData[, 1:6]
#Lys_Temp <- LysData[, 1:6]
#Lys_EC <- LysData[, 1:6]


#############Lys3_mod_depth#####

# Check the structure and head of the 'Lys3_mod_depth' dataset
str(Lys3_mod_depth)
head(Lys3_mod_depth)

# Check to see whether data are "as character" or "numeric"
sapply(Lys3_mod_depth[, 2:7], class)

# Convert all columns 2 to 7 to numeric if needed
Lys3_mod_depth[, 2:7] <- sapply(Lys3_mod_depth[, 2:7], function(x) as.numeric(as.character(x)))

# Remove rows that contain any NaN values

# First, loop through all elements and convert any "nan" or "NAN" strings to NA
Lys3_mod_depth[Lys3_mod_depth == "nan" | Lys3_mod_depth == "NAN"] <- NA
# Then, use na.omit() to remove any rows containing NA values in the dataset
Lys3_mod_depth <- na.omit(Lys3_mod_depth)

# to exclude rows with any zeros
Lys3_mod_depth <- Lys3_mod_depth[!apply(Lys3_mod_depth == 0, 1, any), ]

# Check the structure and head of the 'Lys3_mod_depth' dataset
str(Lys3_mod_depth)
head(Lys3_mod_depth)

#Write the 'Lys3_mod_depth' dataframe to a .dat file with a tab delimiter and save it
write.table(Lys3_mod_depth, file = "Lys3_mod_depth.dat", sep = "\t", row.names = FALSE, col.names = TRUE)

# Create the plot using ggplot2

# Convert the date strings in column 1 to POSIXct datetime objects
Lys3_mod_depth[, 1] <- as.POSIXct(Lys3_mod_depth[, 1], format = "%Y-%m-%d %H:%M")

# Choose your desired y-axis limits (and change it manually in line below).
# y_min <- min(c(Lys3_mod_depth[, 2], Lys3_mod_depth[, 3], Lys3_mod_depth[, 4], Lys3_mod_depth[, 5], Lys3_mod_depth[, 6], Lys3_mod_depth[, 7]), na.rm = TRUE)
# y_max <- max(c(Lys3_mod_depth[, 2], Lys3_mod_depth[, 3], Lys3_mod_depth[, 4], Lys3_mod_depth[, 5], Lys3_mod_depth[, 6], Lys3_mod_depth[, 7]), na.rm = TRUE)

# Create a plot with timestamp on the x-axis and adjusted y-axis limits (adjust the y_max and y_min as you wish)
plot(Lys3_mod_depth[, 1], Lys3_mod_depth[, 2], type = 'l', col = 'red', xlab = 'Timestamp', ylab = 'Values',
     main = 'Lys3_mod_depth Over Time', ylim = c(0, 2200))

# Add the rest of the series to the same plot
lines(Lys3_mod_depth[, 1], Lys3_mod_depth[, 3], col = 'blue')
lines(Lys3_mod_depth[, 1], Lys3_mod_depth[, 4], col = 'green')
lines(Lys3_mod_depth[, 1], Lys3_mod_depth[, 5], col = 'purple')
lines(Lys3_mod_depth[, 1], Lys3_mod_depth[, 6], col = 'orange')
lines(Lys3_mod_depth[, 1], Lys3_mod_depth[, 7], col = 'yellow')

# Add a legend to distinguish the different lines
legend("bottomright", legend = c("Lys3_mod_depth 1", "Lys3_mod_depth 2", "Lys3_mod_depth 3", "Lys3_mod_depth 4", "Lys3_mod_depth 5", "Lys3_mod_depth 6"),
       col = c('red', 'blue', 'green', 'purple', 'orange', 'yellow'), lty = 1, cex = 0.8)

# Do some basic statistics
# Assuming Lys3_mod_depth is your dataset with named columns

# List of column names from the 2nd to the 7th
columnNames_Lys3_mod_depth <- names(Lys3_mod_depth)[2:7]

# Calculate mean and SD for each of the specified columns
means_Lys3_mod_depth <- sapply(Lys3_mod_depth[, columnNames_Lys3_mod_depth], mean, na.rm = TRUE)
sds_Lys3_mod_depth <- sapply(Lys3_mod_depth[, columnNames_Lys3_mod_depth], sd, na.rm = TRUE)

# Create a new dataframe for the results
results_Lys3_mod_depth <- data.frame(
  Mean_Lys3_mod_depth = means_Lys3_mod_depth,
  SD_Lys3_mod_depth = sds_Lys3_mod_depth
)

# Set the row names of the results dataframe to the column names
rownames(results_Lys3_mod_depth) <- columnNames_Lys3_mod_depth

# Display the results
print(results_Lys3_mod_depth)

# make a graph to compare Lys3_mod_depth across lysimeters

# Assuming 'results' dataframe contains the mean and SD information
# for each of the columns

# Create a bar plot for the mean values
barplot(results_Lys3_mod_depth$Mean, names.arg = rownames(results_Lys3_mod_depth), beside = TRUE,
        col = 'lightblue', ylim = c(0, max(results_Lys3_mod_depth$Mean + results_Lys3_mod_depth$SD)),
        main = "Mean values with SD error bars", ylab = "Mean values")

# Add error bars for SD
# Calculate the centre position of each bar on the x-axis
bar_centers_Lys3_mod_depth <- barplot(results_Lys3_mod_depth$Mean, plot = FALSE)

# Add the error bars using the arrow function
arrows(x0 = bar_centers_Lys3_mod_depth, y0 = results_Lys3_mod_depth$Mean - results_Lys3_mod_depth$SD,
       x1 = bar_centers_Lys3_mod_depth, y1 = results_Lys3_mod_depth$Mean + results_Lys3_mod_depth$SD,
       angle = 90, code = 3, length = 0.1, col = 'darkred')

# Do ANOVA
# Ensure that you have the reshape2 package installed

#Melt the data frame from wide format to long format for ANOVA
longLys3_mod_depth <- melt(Lys3_mod_depth, id.vars = "TIMESTAMP", measure.vars = colnames(Lys3_mod_depth)[2:7])

# Perform ANOVA
aov_results_Lys3_mod_depth <- aov(value ~ variable, data = longLys3_mod_depth)

# Display the summary of the ANOVA
summary(aov_results_Lys3_mod_depth)

# Post-hoc test if ANOVA significant
if (summary(aov_results_Lys3_mod_depth)[[1]][["Pr(>F)"]][1] < 0.05) {
  TukeyHSD(aov_results_Lys3_mod_depth)
} else {
  print("ANOVA is not significant; post-hoc analysis is not applicable.")
}

#############Lys3_Temp########

# Check the structure and head of the 'Lys3_Temp' dataset
str(Lys3_Temp)
head(Lys3_Temp)

# Check to see whether data are "as character" or "numeric"
sapply(Lys3_Temp[, 2:7], class)

# Convert all columns 2 to 7 to numeric if needed
Lys3_Temp[, 2:7] <- sapply(Lys3_Temp[, 2:7], function(x) as.numeric(as.character(x)))

# Remove rows that contain any NaN values
# First, loop through all elements and convert any "nan" or "NAN" strings to NA
Lys3_Temp[Lys3_Temp == "nan" | Lys3_Temp == "NAN"] <- NA

# Then, use na.omit() to remove any rows containing NA values in the dataset
Lys3_Temp <- na.omit(Lys3_Temp)

# to exclude rows with any zeros
Lys3_Temp <- Lys3_Temp[!apply(Lys3_Temp == 0, 1, any), ]

# Check the structure and head of the 'Lys3_Temp' dataset
str(Lys3_Temp)
head(Lys3_Temp)

# Write the 'Lys3_Temp' dataframe to a .dat file with a tab delimiter and save it
write.table(Lys3_Temp, file = "Lys3_Temp.dat", sep = "\t", row.names = FALSE, col.names = TRUE)

# Create the plot using ggplot2

# Convert the date strings in column 1 to POSIXct datetime objects
Lys3_Temp[, 1] <- as.POSIXct(Lys3_Temp[, 1], format = "%Y-%m-%d %H:%M")

# Choose your desired y-axis limits (and change it manually in line below).
#y_min <- min(c(Lys3_Temp[, 2], Lys3_Temp[, 3], Lys3_Temp[, 4], Lys3_Temp[, 5], Lys3_Temp[, 6], Lys3_Temp[, 7]), na.rm = TRUE)
#y_max <- max(c(Lys3_Temp[, 2], Lys3_Temp[, 3], Lys3_Temp[, 4], Lys3_Temp[, 5], Lys3_Temp[, 6], Lys3_Temp[, 7]), na.rm = TRUE)

# Create a plot with timestamp on the x-axis and adjusted y-axis limits (adjust the y_max and y_min as you wish)
plot(Lys3_Temp[, 1], Lys3_Temp[, 2], type = 'l', col = 'red', xlab = 'Timestamp', ylab = 'Values', 
     main = 'Lys3_Temp Over Time', ylim = c(23, 29))

# Add the rest of the series to the same plot
lines(Lys3_Temp[, 1], Lys3_Temp[, 3], col = 'blue')
lines(Lys3_Temp[, 1], Lys3_Temp[, 4], col = 'green')
lines(Lys3_Temp[, 1], Lys3_Temp[, 5], col = 'purple')
lines(Lys3_Temp[, 1], Lys3_Temp[, 6], col = 'orange')
lines(Lys3_Temp[, 1], Lys3_Temp[, 7], col = 'yellow')
# Add a legend to distinguish the different lines
legend("bottomright", legend = c("Lys3_Temp 1", "Lys3_Temp 2", "Lys3_Temp 3", "Lys3_Temp 4", "Lys3_Temp 5", "Lys3_Temp 6"), 
       col = c('red', 'blue', 'green', 'purple', 'orange', 'yellow'), lty = 1, cex = 0.8)

# Do some basic statistics
# Assuming Lys3_Temp is your dataset with named columns

# List of column names from the 2nd to the 7th
columnNames_Lys3_Temp <- names(Lys3_Temp)[2:7]

# Calculate mean and SD for each of the specified columns
means_Lys3_Temp <- sapply(Lys3_Temp[, columnNames_Lys3_Temp], mean, na.rm = TRUE)
sds_Lys3_Temp <- sapply(Lys3_Temp[, columnNames_Lys3_Temp], sd, na.rm = TRUE)

# Create a new dataframe for the results
results_Lys3_Temp <- data.frame(
  Mean_Lys3_Temp = means_Lys3_Temp,
  SD_Lys3_Temp = sds_Lys3_Temp
)

# Set the row names of the results dataframe to the column names
rownames(results_Lys3_Temp) <- columnNames_Lys3_Temp

# Display the results
print(results_Lys3_Temp)

# Make a graph to compare Lys3_Temp across lysimeters

# Assuming 'results' dataframe contains the mean and SD information
# for each of the columns

# Create a bar plot for the mean values
barplot(results_Lys3_Temp$Mean_Lys3_Temp, names.arg = rownames(results_Lys3_Temp), beside = TRUE,
        col = 'lightblue', ylim = c(0, max(results_Lys3_Temp$Mean_Lys3_Temp + results_Lys3_Temp$SD_Lys3_Temp)),
        main = "Mean values with SD error bars", ylab = "Mean values")

# Add error bars for SD
# Calculate the centre position of each bar on the x-axis
bar_centers_Lys3_Temp <- barplot(results_Lys3_Temp$Mean_Lys3_Temp, plot = FALSE)

# Add the error bars using the arrow function
arrows(x0 = bar_centers_Lys3_Temp, y0 = results_Lys3_Temp$Mean_Lys3_Temp - results_Lys3_Temp$SD_Lys3_Temp,
       x1 = bar_centers_Lys3_Temp, y1 = results_Lys3_Temp$Mean_Lys3_Temp + results_Lys3_Temp$SD_Lys3_Temp,
       angle = 90, code = 3, length = 0.1, col = 'darkred')

# Do ANOVA

# Ensure that you have the reshape2 package installed

#Melt the data frame from wide format to long format for ANOVA
longLys3_Temp <- melt(Lys3_Temp, id.vars = "TIMESTAMP", measure.vars = colnames(Lys3_Temp)[2:7])

# Perform ANOVA
aov_results_Lys3_Temp <- aov(value ~ variable, data = longLys3_Temp)

# Display the summary of the ANOVA
summary(aov_results_Lys3_Temp)

# Post-hoc test if ANOVA significant
if (summary(aov_results_Lys3_Temp)[[1]][["Pr(>F)"]][1] < 0.05) {
  TukeyHSD(aov_results_Lys3_Temp)
} else {
  print("ANOVA is not significant; post-hoc analysis is not applicable.")
}

#############Lys3_EC########

# Check the structure and head of the 'Lys3_EC' dataset
str(Lys3_EC)
head(Lys3_EC)

# Check to see whether data are "as character" or "numeric"
sapply(Lys3_EC[, 2:7], class)

# Convert all columns 2 to 7 to numeric if needed
Lys3_EC[, 2:7] <- sapply(Lys3_EC[, 2:7], function(x) as.numeric(as.character(x)))

# Remove rows that contain any NaN values
# First, loop through all elements and convert any "nan" or "NAN" strings to NA
Lys3_EC[Lys3_EC == "nan" | Lys3_EC == "NAN"] <- NA

# Then, use na.omit() to remove any rows containing NA values in the dataset
Lys3_EC <- na.omit(Lys3_EC)

# to exclude rows with any zeros
Lys3_EC <- Lys3_EC[!apply(Lys3_EC == 0, 1, any), ]

# Check the structure and head of the 'Lys3_EC' dataset
str(Lys3_EC)
head(Lys3_EC)

# Write the 'Lys3_EC' dataframe to a .dat file with a tab delimiter and save it
write.table(Lys3_EC, file = "Lys3_EC.dat", sep = "\t", row.names = FALSE, col.names = TRUE)

# Create the plot using ggplot2

# Convert the date strings in column 1 to POSIXct datetime objects
Lys3_EC[, 1] <- as.POSIXct(Lys3_EC[, 1], format = "%Y-%m-%d %H:%M")

# Choose your desired y-axis limits (and change it manually in line below).
# y_min <- min(c(Lys3_EC[, 2], Lys3_EC[, 3], Lys3_EC[, 4], Lys3_EC[, 5], Lys3_EC[, 6], Lys3_EC[, 7]), na.rm = TRUE)
# y_max <- max(c(Lys3_EC[, 2], Lys3_EC[, 3], Lys3_EC[, 4], Lys3_EC[, 5], Lys3_EC[, 6], Lys3_EC[, 7]), na.rm = TRUE)

# Create a plot with timestamp on the x-axis and adjusted y-axis limits (adjust the y_max and y_min as you wish)
plot(Lys3_EC[, 1], Lys3_EC[, 2], type = 'l', col = 'red', xlab = 'Timestamp', ylab = 'Values',
     main = 'Lys3_EC Over Time', ylim = c(0, 1200))

# Add the rest of the series to the same plot
lines(Lys3_EC[, 1], Lys3_EC[, 3], col = 'blue')
lines(Lys3_EC[, 1], Lys3_EC[, 4], col = 'green')
lines(Lys3_EC[, 1], Lys3_EC[, 5], col = 'purple')
lines(Lys3_EC[, 1], Lys3_EC[, 6], col = 'orange')
lines(Lys3_EC[, 1], Lys3_EC[, 7], col = 'yellow')
# Add a legend to distinguish the different lines
legend("bottomright", legend = c("Lys3_EC 1", "Lys3_EC 2", "Lys3_EC 3", "Lys3_EC 4", "Lys3_EC 5", "Lys3_EC 6"),
       col = c('red', 'blue','green', 'purple', 'orange', 'yellow'), lty=1, cex=0.8)

# Do some basic statistics
# Assuming Lys3_EC is your dataset with named columns

# List of column names from the 2nd to the 7th
columnNames_Lys3_EC <- names(Lys3_EC)[2:7]

# Calculate mean and SD for each of the specified columns
means_Lys3_EC <- sapply(Lys3_EC[, columnNames_Lys3_EC], mean, na.rm = TRUE)
sds_Lys3_EC <- sapply(Lys3_EC[, columnNames_Lys3_EC], sd, na.rm = TRUE)

# Create a new dataframe for the results
results_Lys3_EC <- data.frame(
  Mean_Lys3_EC = means_Lys3_EC,
  SD_Lys3_EC = sds_Lys3_EC
)

# Set the row names of the results dataframe to the column names
rownames(results_Lys3_EC) <- columnNames_Lys3_EC

# Display the results
print(results_Lys3_EC)

# make a graph to compare Lys3_EC across lysimeters

# Assuming 'results' dataframe contains the mean and SD information
# for each of the columns

# Create a bar plot for the mean values
barplot(results_Lys3_EC$Mean_Lys3_EC, names.arg = rownames(results_Lys3_EC), beside = TRUE,
        col = 'lightblue', ylim = c(0, max(results_Lys3_EC$Mean_Lys3_EC + results_Lys3_EC$SD_Lys3_EC)),
        main = "Mean values with SD error bars", ylab = "Mean values")

# Add error bars for SD
# Calculate the center position of each bar on the x-axis
bar_centers_Lys3_EC <- barplot(results_Lys3_EC$Mean_Lys3_EC, plot = FALSE)

# Add the error bars using the arrow function
arrows(x0 = bar_centers_Lys3_EC, y0 = results_Lys3_EC$Mean_Lys3_EC - results_Lys3_EC$SD_Lys3_EC,
       x1 = bar_centers_Lys3_EC, y1 = results_Lys3_EC$Mean_Lys3_EC + results_Lys3_EC$SD_Lys3_EC,
       angle = 90, code = 3, length = 0.1, col = 'darkred')

# Do ANOVA

# Ensure that you have the reshape2 package installed

#Melt the data frame from wide format to long format for ANOVA
longLys3_EC <- melt(Lys3_EC, id.vars = "TIMESTAMP", measure.vars = colnames(Lys3_EC)[2:7])

# Perform ANOVA
aov_results_Lys3_EC <- aov(value ~ variable, data = longLys3_EC)

# Display the summary of the ANOVA
summary(aov_results_Lys3_EC)

# Post-hoc test if ANOVA significant
if (summary(aov_results_Lys3_EC)[[1]][["Pr(>F)"]][1] < 0.05) {
  TukeyHSD(aov_results_Lys3_EC)
} else {
  print("ANOVA is not significant; post-hoc analysis is not applicable.")
}




#            Cheers :) Mohammad






















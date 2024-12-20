#codes for Paddock Soil Data Processing
# updoad soil data from .dat file
soilData <- read.table(file = "Ross T1_SoilData.dat", header = TRUE, sep = "\t")

# After loading you can check the first few rows to make sure it's loaded correctly
head(soilData)

# You can check the structure of the data
str(soilData)

# Assuming soilData is already loaded


# Create new data frame 'EC' using the column indices
# 
EC <- soilData[, c(1, 4, 7, 10, 13, 16, 19)]

# Alternatively, if the columns are consecutive, you can use
# EC <- soilData[, 1:6]

# Check the structure and head of the 'EC' dataset
str(EC)
head(EC)

# Assuming EC is already loaded and we want to exclude rows with any zeros
ECp <- EC[!apply(EC == 0, 1, any), ]

# Check the structure and head of the 'ECp' dataset
str(ECp)
head(ECp)
 
# Write the 'ECp' dataframe to a .dat file with a tab delimiter
write.table(ECp, file = "ECp.dat", sep = "\t", row.names = FALSE, col.names = TRUE)

# Assuming ECp is your dataframe
# Remove the 1st, 2nd and 3rd rows from ECp
ECp <- ECp[-c(1, 2, 3), ]

# Check the top of the dataframe to ensure the rows are removed
head(ECp)


#to create time stamp for 1 column

# Convert the first column from string to POSIXct datetime object
ECp[, 1] <- as.POSIXct(ECp[, 1], format = "%d/%m/%Y %H:%M")

# Plotting the converted datetime against the second column
# Replace 'type' with "l" if you want a line plot instead
plot(ECp[, 1], ECp[, 2], type = 'p',
     xlab = 'Timestamp', ylab = 'EC1 Value', main = 'EC1 Values Over Time')


#to create timestamp for all columns


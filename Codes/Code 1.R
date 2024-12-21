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
 
# Write the 'ECp' dataframe to a .dat file with a tab delimiter and save it
write.table(ECp, file = "ECp.dat", sep = "\t", row.names = FALSE, col.names = TRUE)








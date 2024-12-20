#codes for Paddock Data Processing
# updoad soil data from .dat file
soilData <- read.table(file = "Ross T1_SoilData.dat", header = TRUE, sep = "\t")

# After loading you can check the first few rows to make sure it's loaded correctly
head(soilData)

# You can check the structure of the data
str(soilData)

# Assuming soilData is already loaded
# Example: soilData <- read.table(...)

# Create new data frame 'EC' using the column indices
# 
EC <- soilData[, c(1, 4, 7, 10, 13, 16, 19)]

# Alternatively, if the columns are consecutive, you can use
# EC <- soilData[, 1:6]

# Check the structure and head of the 'EC' dataset
str(EC)
head(EC)
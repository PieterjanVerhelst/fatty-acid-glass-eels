# Read in dataset for glass eel analysis
# By Pieterjan Verhelst
# Pieterjan.Verhelst@ugent.be


library(tidyverse)
library(lubridate)
library(FSA)


rm(list = ls())


# Read in data
data <- read.csv("./data/raw/glass_eel_metadata.csv",sep=";",stringsAsFactors = FALSE)

# Change column name
colnames(data)[1] <- "sample_code"

# Set columns in correct format
summary(data)

data$Date <- dmy(data$Date)
data$Location <- factor(data$Location)
data$Location_type <- factor(data$Location_type)
data$Pigmentation <- factor(data$Pigmentation)
data$Pigmentation_group <- factor(data$Pigmentation_group)
data$Condition_factor<-gsub(",", ".", data$Condition_factor)
data$Condition_factor <- as.numeric(data$Condition_factor)
data$Length <- as.numeric(data$Length)
data$Weight <- as.numeric(data$Weight)

# Add month and year to dataset
data$Month <- month(data$Date)
data$Year <- year(data$Date)
data <- unite(data, MY, Year, Month, sep = "_", remove = FALSE)
data$MY <- factor(data$MY)
data$Year <- factor(data$Year)
data$Month <- factor(data$Month)

# Remove "potloodjes"
unique(data$Pigmentation)
data <- subset(data, Pigmentation != "potlood")


# Remove substrates in Ganzepoot and IJzer en sleepnetdata
data$Location <- factor(data$Location)
data <- subset(data, Location != "IJZ_SU")
data <- subset(data, Location != "GA_SU")
data <- subset(data, Location_type != "SN")

# Remove NA
data <- na.omit(data)


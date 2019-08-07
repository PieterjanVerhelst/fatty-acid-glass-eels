# Read in dataset for glass eel analysis
# By Pieterjan Verhelst
# Pieterjan.Verhelst@ugent.be


library(tidyverse)
library(lubridate)
library(FSA)


rm(list = ls())


# Read in data
data <- read.csv("./data/raw/glass_eel_metadata.csv",sep=",",stringsAsFactors = FALSE)


# Set columns in correct format
summary(data)

data$Date <- dmy(data$Date)
data$Location <- factor(data$Location)
data$Fishing_method <- factor(data$Fishing_method)
data$Pigmentation <- factor(data$Pigmentation)
data$Pigmentation_group <- factor(data$Pigmentation_group)
#data$Condition_factor<-gsub(",", ".", data$Condition_factor)
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

# Create numerical time column
data$cdate <- as.numeric(data$Date)

# Remove elvers
unique(data$Pigmentation)
data <- subset(data, Pigmentation != "elver")


# Remove substrates in Ganzepoot and IJzer en sleepnetdata
data$Location <- factor(data$Location)
data <- subset(data, Location != "IJZ_SU")
data <- subset(data, Location != "GA_SU")
data <- subset(data, Fishing_method != "SN")

# Remove NA
data <- na.omit(data)


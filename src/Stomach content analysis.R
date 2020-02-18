# Read in dataset for glass eel stomach content analysis
# By Michiel Perneel

library(ggplot2)
library(dplyr)
library(lubridate)

maag_AS <- read.csv("data/raw/maaginhoud_AS_time.csv",sep=",",stringsAsFactors = FALSE)
maag_EL <- read.csv("data/raw/maaginhoud_el_occurrence_time.csv", sep = ",", stringsAsFactors = FALSE, header = TRUE)
maaginhoud <- read.csv("data/raw/Maaginhoud3.csv", sep = ";")
maaginhoud$datum <- dmy(maaginhoud$datum)

# only AS & EL

maaginhoud_no_kn <- maaginhoud %>%
  mutate(total_food = rowSums(maaginhoud[8:25])) %>%
  filter(Methode %in% c("Artificial Substrates", "Eel Ladder")) %>%
  group_by(Methode, week(datum)) %>%
  summarize(Week_totaal = sum(total_food, na.rm = TRUE))

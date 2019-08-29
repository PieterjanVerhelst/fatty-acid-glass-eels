# Upload fatty acid dataset and merge with glass eel metadata
# By Pieterjan Verhelst
# Pieterjan.Verhelst@ugent.be

library(ggfortify)
library(data.table)
library(ggbiplot)

# Upload FA data
fa_data <- read.csv("C:/Users/Admin/Documents/Biologie/Master/Thesis/Thesis/fatty-acid-glass-eels/data/raw/FA_rel.csv",stringsAsFactors = FALSE)


# Join glass eel meta data with FA data
fa_data <- left_join(fa_data, data, by = "sample_code")

# Remove NA
# These are glass eels not from the Veurne-Ambacht canal
fa_data <- na.omit(fa_data)

# Add fatty acid ratio's to dataset


fa_data <- fa_data %>%
  mutate(A = X22.6n.3 /X20.5n.3 ) %>%
  mutate(B = (X18.2n.6 + X18.3n.3 + X18.4n.3 + X20.3n.6 + X20.4n.3 + X20.4n.6 + X20.5n.3 + X22.5n.3 + X22.5n.6 + X22.6n.3 + X18.2n.6) /
           (X14.0 + X15.0 + X16.0 + X17.0 + X18.0 + X20.0 + X22.0 + cis.9.16.1 + X16.1.iso.17.0 + X17.1.16.2 + cis.9.18.1 + cis.11.18.1 + X20.1 + X24.1)) %>%
  mutate(C = X20.5n.3 / X20.4n.6) %>%
  mutate(D = (X18.3n.3 + X18.4n.3 + X20.4n.3 + X20.5n.3 + X22.5n.3 +  X22.6n.3 ) / (X18.2n.6 +  X20.3n.6 + X20.4n.6  + X22.5n.6 + X18.2n.6)) %>%
  mutate(E = (X20.5n.3 + X22.6n.3) / (X18.2n.6 + X18.3n.3 + X18.4n.3 + X20.3n.6 + X20.4n.3 + X20.4n.6 + X20.5n.3 + X22.5n.3 + X22.5n.6 + X22.6n.3 + X18.2n.6 + X14.0 + X15.0 + X16.0 + X17.0 + X18.0 + X20.0 + X22.0 + cis.9.16.1 + X16.1.iso.17.0 + X17.1.16.2 + cis.9.18.1 + cis.11.18.1 + X20.1 + X24.1))

fa_data <- setnames(fa_data, old=colnames(fa_data), new=c("sample_code","14:0", "15:0", "16:0", "16:1 iso 17:0", "16:1 (cis-9)", "17:0", "17:1 + 16:2", "18:0", "18:1 (cis-9)", "18:1 (cis-11)", "18:2 (n-6)", "20:0", "18:3 (n-3)", "20:1", "18:4 (n-3)", "20:3 (n-6)", "22:0", "20:4 (n-6)", "20:4 (n-3)", "20:5 (n-3)", "24:1", "22:5 (n-6)", "22:5 (n-3)", "22:6 (n-3)", "Date", "Location","Fishing_method", "Length", "Weight", "Pigmentation", "Pigmentation_group", "Condition_factor", "MY", "Month", "Year", "cdate", "DHA/EPA", "PUFA/(SFA + MUFA)", "EPA/AA", "n-3/n-6", "FLQ"))

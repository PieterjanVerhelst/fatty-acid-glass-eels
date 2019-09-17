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

# Remove the samples that are not in the glass eel metadata
fa_data <- fa_data[fa_data$sample_code %in% data$sample_code,]


# Add fatty acid ratio's to dataset


fa_data <- fa_data %>%
  mutate(A = X22.6n.3 /X20.5n.3 ) %>%
  mutate(B = (X18.2n.6 + X18.3n.3 + X18.4n.3 + X20.3n.6 + X20.4n.3 + X20.4n.6 + X20.5n.3 + X22.5n.3 + X22.5n.6 + X22.6n.3 + X18.2n.6) /
           (X14.0 + X15.0 + X16.0 + X17.0 + X18.0 + X20.0 + X22.0 + cis.9.16.1 + X16.1.iso.17.0 + X17.1.16.2 + cis.9.18.1 + cis.11.18.1 + X20.1 + X24.1)) %>%
  mutate(C = X20.5n.3 / X20.4n.6) %>%
  mutate(D = (X18.3n.3 + X18.4n.3 + X20.4n.3 + X20.5n.3 + X22.5n.3 +  X22.6n.3 ) / (X18.2n.6 +  X20.3n.6 + X20.4n.6  + X22.5n.6 + X18.2n.6)) %>%
  mutate(E = (X20.5n.3 + X22.6n.3) / (X18.2n.6 + X18.3n.3 + X18.4n.3 + X20.3n.6 + X20.4n.3 + X20.4n.6 + X20.5n.3 + X22.5n.3 + X22.5n.6 + X22.6n.3 + X18.2n.6 + X14.0 + X15.0 + X16.0 + X17.0 + X18.0 + X20.0 + X22.0 + cis.9.16.1 + X16.1.iso.17.0 + X17.1.16.2 + cis.9.18.1 + cis.11.18.1 + X20.1 + X24.1))

n <- colnames(fa_data)
fa_data <- setnames(fa_data, old=colnames(fa_data), new=c(n[1:37], "DHA_EPA", "PUFA_SFAMUFA", "EPA_AA", "n3_n6", "FLQ"))

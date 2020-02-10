# Upload fatty acid dataset and merge with glass eel metadata
# By Pieterjan Verhelst
# Pieterjan.Verhelst@ugent.be

library(ggfortify)
library(data.table)
library(ggbiplot)
library(dplyr)

# Upload FA data

fa_rel <- read.csv("data/raw/FA_rel.csv",stringsAsFactors = FALSE)
fa_abs <- read.csv("data/raw/FA_abs.csv",stringsAsFactors = FALSE, dec=",", sep=";")

# Join glass eel meta data with FA data
fa_rel <- left_join(fa_rel, data, by = "sample_code")
fa_abs <- left_join(fa_abs, data, by = "sample_code")

# Remove NA
# These are glass eels not from the Veurne-Ambacht canal
fa_rel <- na.omit(fa_rel)
fa_abs <- na.omit(fa_abs)

# Remove the samples that are not in the glass eel metadata
fa_rel <- fa_rel[fa_rel$sample_code %in% data$sample_code,]
fa_abs <- fa_abs[fa_abs$sample_code %in% data$sample_code,]
  

# Add fatty acid ratio's to dataset


fa_rel <- fa_rel %>%
  mutate(A = X22.6n.3 /X20.5n.3 ) %>%
  mutate(B = (X18.2n.6 + X18.3n.3 + X18.4n.3 + X20.3n.6 + X20.4n.3 + X20.4n.6 + X20.5n.3 + X22.5n.3 + X22.5n.6 + X22.6n.3 + X18.2n.6) /
           (X14.0 + X15.0 + X16.0 + X17.0 + X18.0 + X20.0 + X22.0 + cis.9.16.1 + X16.1.iso.17.0 + X17.1.16.2 + cis.9.18.1 + cis.11.18.1 + X20.1 + X24.1)) %>%
  mutate(C = X20.5n.3 / X20.4n.6) %>%
  mutate(D = (X18.3n.3 + X18.4n.3 + X20.4n.3 + X20.5n.3 + X22.5n.3 +  X22.6n.3 ) / (X18.2n.6 +  X20.3n.6 + X20.4n.6  + X22.5n.6 + X18.2n.6)) %>%
  mutate(E = (X20.5n.3 + X22.6n.3) / (X18.2n.6 + X18.3n.3 + X18.4n.3 + X20.3n.6 + X20.4n.3 + X20.4n.6 + X20.5n.3 + X22.5n.3 + X22.5n.6 + X22.6n.3 + X18.2n.6 + X14.0 + X15.0 + X16.0 + X17.0 + X18.0 + X20.0 + X22.0 + cis.9.16.1 + X16.1.iso.17.0 + X17.1.16.2 + cis.9.18.1 + cis.11.18.1 + X20.1 + X24.1))

n <- colnames(fa_rel)
fa_rel <- setnames(fa_rel, old=colnames(fa_rel), new=c(n[1:37], "DHA_EPA", "PUFA_SFAMUFA", "EPA_AA", "n3_n6", "FLQ"))

fa_abs <- fa_abs %>%
  mutate(A = X22.6n.3 /X20.5n.3 ) %>%
  mutate(B = (X18.2n.6 + X18.3n.3 + X18.4n.3 + X20.3n.6 + X20.4n.3 + X20.4n.6 + X20.5n.3 + X22.5n.3 + X22.5n.6 + X22.6n.3 + X18.2n.6) /
           (X14.00 + X15.00 + X16.00 + X17.00 + X18.00 + X20.00 + X22.00 + cis.9.16.1 + X16.1.iso.17.0 + X17.1.16.2 + cis.9.18.1 + cis.11.18.1 + X20.01 + X24.01.00)) %>%
  mutate(C = X20.5n.3 / X20.4n.6) %>%
  mutate(D = (X18.3n.3 + X18.4n.3 + X20.4n.3 + X20.5n.3 + X22.5n.3 +  X22.6n.3 ) / (X18.2n.6 +  X20.3n.6 + X20.4n.6  + X22.5n.6 + X18.2n.6)) %>%
  mutate(E = (X20.5n.3 + X22.6n.3) / (X18.2n.6 + X18.3n.3 + X18.4n.3 + X20.3n.6 + X20.4n.3 + X20.4n.6 + X20.5n.3 + X22.5n.3 + X22.5n.6 + X22.6n.3 + X18.2n.6 + X14.00 + X15.00 + X16.00 + X17.00 + X18.00 + X20.00 + X22.00 + cis.9.16.1 + X16.1.iso.17.0 + X17.1.16.2 + cis.9.18.1 + cis.11.18.1 + X20.01 + X24.01.00))

n <- colnames(fa_abs)
fa_abs <- setnames(fa_abs, old=colnames(fa_abs), new=c(n[1:38], "DHA_EPA", "PUFA_SFAMUFA", "EPA_AA", "n3_n6", "FLQ"))


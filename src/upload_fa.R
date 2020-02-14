# Upload fatty acid dataset and merge with glass eel metadata
# By Pieterjan Verhelst
# Pieterjan.Verhelst@ugent.be

library(ggfortify)
library(data.table)
library(ggbiplot)
library(dplyr)
library(xlsx)

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
  mutate(E = (X20.5n.3 + X22.6n.3) / (X18.2n.6 + X18.3n.3 + X18.4n.3 + X20.3n.6 + X20.4n.3 + X20.4n.6 + X20.5n.3 + X22.5n.3 + X22.5n.6 + X22.6n.3 + X18.2n.6 + X14.0 + X15.0 + X16.0 + X17.0 + X18.0 + X20.0 + X22.0 + cis.9.16.1 + X16.1.iso.17.0 + X17.1.16.2 + cis.9.18.1 + cis.11.18.1 + X20.1 + X24.1)) %>%
  mutate(F = (X22.6n.3/X20.5n.3)) %>%
  mutate(G = (X20.5n.3/X18.4n.3))

n <- colnames(fa_rel)
fa_rel <- setnames(fa_rel, old=colnames(fa_rel), new=c(n[1:37], "DHA_EPA", "PUFA_SFAMUFA", "EPA_AA", "n3_n6", "FLQ", "Dinoflagellate_biomarker", "Diatom_production"))

# Make tables that summarise the relative fatty acid concentration per month & for PG & SU
fa_rel_summary <- fa_rel %>%
  filter(Fishing_method == c("Palinggoot", "Substraat")) %>%
  group_by(Month, Fishing_method) %>%
  summarise_at(vars(X14.0,X15.0,X14.0,X15.0,X16.0,X16.1.iso.17.0,cis.9.16.1,X17.0,X17.1.16.2,X18.0,cis.9.18.1,cis.11.18.1,X18.2n.6,X20.0,X18.3n.3,X20.1,X18.4n.3,X20.3n.6,X22.0,X20.4n.6,X20.4n.3,X20.5n.3,X24.1,X22.5n.6,X22.5n.3,X22.6n.3), funs(mean, sd))

write.csv(fa_rel_summary, file = "data/raw/FA_rel_summary.csv", row.names = TRUE)

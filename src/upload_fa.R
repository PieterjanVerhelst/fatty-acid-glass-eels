# Upload fatty acid dataset and merge with glass eel metadata
# By Pieterjan Verhelst
# Pieterjan.Verhelst@ugent.be


# Upload FA data
fa_data <- read.csv("./data/raw/FA_rel.csv",stringsAsFactors = FALSE)


# Join glass eel meta data with FA data
fa_data <- left_join(fa_data, data, by = "sample_code")

# Remove NA
# These are glass eels not from the Veurne-Ambacht canal
fa_data <- na.omit(fa_data)

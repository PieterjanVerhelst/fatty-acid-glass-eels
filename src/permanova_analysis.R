# PERMANOVA analysis
# By Pieterjan Verhelst
# Pieterjan.Verhelst@ugent.be

# http://www.sthda.com/english/wiki/manova-test-in-r-multivariate-analysis-of-variance


library(mvnormtest)



# 1. PERMANOVA ####

# Create dataset - do not take into account "kruisnetten"
subset <- filter(fa_data, Fishing_method == "Palinggoot" |
                   Fishing_method == "Substraat")

# FA data
df <- subset[,c(2:25)] 
# Meta data
meta <- subset[,c(26:33, 37)]

# Perform PERMANOVA
# R2 gives the explained variance
adonis(df~meta$cdate + meta$Fishing_method + meta$Condition_factor, permutations = 999, method = 'bray')


# 2. PERMDISP ####
#An important assumption for PERMANOVA is to have similar "multivariate spread" among groups, which is similar to variance homogeneity in univariate ANOVA. To test if one or more groups is more variable than the others, a PERMDISP can be conducted in R. 

# Create distance matrix
d_df <- vegdist(df, method="bray") 

bd <- betadisper(d_df,meta$cdate, type = "centroid")
boxplot(bd)
permutest(bd)   # Differences between the dispersions exist (p < 0.05)




# 2. SIMPER

sim <- simper(df, group = meta$Fishing_method)
sim	
summary(sim)




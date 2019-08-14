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
adonis(df~meta$cdate, permutations = 999, method = 'euclidean')


# Apply nested design (nestedness within Fishing_method)
# Correct method should be method according traditional anova notation (Y ~ Z/X)
adonis(df~meta$cdate, permutations = 999, strata = meta$Fishing_method, method = 'euclidean')
adonis(df~meta$Fishing_method/meta$cdate, permutations = 999, method = 'euclidean')




# Marginal tests: the separate effect of each environmental variable on the response matrix
adonis2(vegdist(df, "euclidean") ~ meta$cdate + meta$Fishing_method, by = "margin")

# Conditional test: the interaction between environmental variables
adonis2(vegdist(df, "euclidean") ~ meta$cdate + meta$Fishing_method, by = "terms")


# Visualistion of the results
dbRDA <- capscale(vegdist(df, "euclidean") ~ meta$cdate + meta$Fishing_method + meta$Condition_factor)
plot(dbRDA)
summary(dbRDA)





# 2. Pairwise comparison ####
pairwise.adonis <- function(x,factors, sim.function = 'vegdist', sim.method = 'euclidean', p.adjust.m ='bonferroni')
{
  
  
  co = combn(unique(as.character(factors)),2)
  pairs = c()
  F.Model =c()
  R2 = c()
  p.value = c()
  
  
  for(elem in 1:ncol(co)){
    if(sim.function == 'daisy'){
      library(cluster); x1 = daisy(x[factors %in% c(co[1,elem],co[2,elem]),],metric=sim.method)
    } else{x1 = vegdist(x[factors %in% c(co[1,elem],co[2,elem]),],method=sim.method)}
    
    ad = adonis(x1 ~ factors[factors %in% c(co[1,elem],co[2,elem])], permutations = 9999 );
    pairs = c(pairs,paste(co[1,elem],'vs',co[2,elem]));
    F.Model =c(F.Model,ad$aov.tab[1,4]);
    R2 = c(R2,ad$aov.tab[1,5]);
    p.value = c(p.value,ad$aov.tab[1,6])
  }
  p.adjusted = p.adjust(p.value,method=p.adjust.m)
  sig = c(rep('',length(p.adjusted)))
  sig[p.adjusted <= 0.05] <-'.'
  sig[p.adjusted <= 0.01] <-'*'
  sig[p.adjusted <= 0.001] <-'**'
  sig[p.adjusted <= 0.0001] <-'***'
  
  pairw.res = data.frame(pairs,F.Model,R2,p.value,p.adjusted,sig)
   return(pairw.res)
  
} 


pairwise.adonis(df, meta$cdate) 



# 3. PERMDISP ####
#An important assumption for PERMANOVA is to have similar "multivariate spread" among groups, which is similar to variance homogeneity in univariate ANOVA. To test if one or more groups is more variable than the others, a PERMDISP can be conducted in R. 

# Create distance matrix
d_df <- vegdist(df, method="euclidean") 

bd <- betadisper(d_df,meta$cdate, type = "centroid")
boxplot(bd)
permutest(bd)   # Differences between the dispersions exist (p < 0.05)




# 2. SIMPER

sim <- simper(df, group = meta$Fishing_method)
sim	
summary(sim)




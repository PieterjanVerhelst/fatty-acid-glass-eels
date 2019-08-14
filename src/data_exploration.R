# Data exploration
# By Pieterjan Verhelst
# Pieterjan.Verhelst@ugent.be



library(DataExplorer)
library(ggplot2)
library(ggpubr)
library(ggloop)
library(vegan)





# 1. Create scatter plots with smoother ####

# 1.1 All ==============
#ggplot(fa_data, aes(x=Date, y=X14.0)) + 
#  geom_point() +
#  geom_smooth(method="auto", se=TRUE, fullrange=FALSE, level=0.95) # geom_abline for regression line

g <- ggloop(fa_data, aes_loop(x = Date, y = X14.0:X22.6n.3)) %L+%
  geom_point() %L+%
  geom_smooth(method="auto", se=TRUE, fullrange=FALSE, level=0.95) # geom_abline for regression line

g$x.Date_y.X14 


# Save plot
pdf("./Figures/Fa_date.pdf")
g 
dev.off()


# 1.2 Catch method ==============

# Per catch method
fa_data_pg <- filter(fa_data, Fishing_method == "Palinggoot")
fa_data_su <- filter(fa_data, Fishing_method == "Substraat")
fa_data_kn <- filter(fa_data, Fishing_method == "Kruisnetten")

g <- ggloop(fa_data_kn, aes_loop(x = Date, y = X14.0:X22.6n.3)) %L+%
  geom_point() %L+%
  geom_smooth(method="auto", se=TRUE, fullrange=FALSE, level=0.95) # geom_abline for regression line

# Save plot
pdf("./Figures/Fa_date_kn.pdf")
g 
dev.off()



# Different catch methods in 1 plot
fa_data_no_kn <- filter(fa_data, Fishing_method == "Palinggoot" |
                        Fishing_method == "Substraat")

g <- ggloop(fa_data_no_kn, aes_loop(x = Date, y = X14.0:X22.6n.3)) %L+%
  geom_point() %L+%
  geom_point(aes(colour = factor(Fishing_method)), size = 2) %L+%
  geom_smooth(aes(colour = factor(Fishing_method)), method="auto", se=TRUE, fullrange=FALSE, level=0.95) # geom_abline for regression line


# Save plot
pdf("./Figures/Fa_date_Fishingmethods.pdf")
g 
dev.off()



# 1.3. Weight and catch method ==============
g <- ggloop(fa_data_no_kn, aes_loop(x = Weight, y = X14.0:X22.6n.3)) %L+%
  geom_point() %L+%
  geom_point(aes(colour = factor(Fishing_method)), size = 2) %L+%
  geom_smooth(aes(colour = factor(Fishing_method)), method="auto", se=TRUE, fullrange=FALSE, level=0.95) # geom_abline for regression line


# Save plot
pdf("./Figures/Fa_weight_Fishingmethods.pdf")
g 
dev.off()



# 1.4. Different locations of substrates ==============

unique(fa_data_su$Location)

fa_data_su$Location_type <- NA

for (i in 1:dim(fa_data_su)[1]){
  if (fa_data_su$Location[i] == "VA_pg_LO_SU" | fa_data_su$Location[i] == "VA_pg_RO_SU"){
    fa_data_su$Location_type[i] = "pg"
  } else if (fa_data_su$Location[i] == "VA_MI_RO_SU"){
    fa_data_su$Location_type[i] = "MI"
  } else{
    fa_data_su$Location_type[i] = "SC"
  }}

fa_data_su$Location_type <- factor(fa_data_su$Location_type)


g <- ggloop(fa_data_su, aes_loop(x = Date, y = X14.0:X22.6n.3)) %L+%
  geom_point() %L+%
  geom_point(aes(colour = factor(Location_type)), size = 2) %L+%
  geom_smooth(aes(colour = factor(Location_type)), method="auto", se=TRUE, fullrange=FALSE, level=0.95) # geom_abline for regression line


# Save plot
pdf("./Figures/Fa_date_locationtype.pdf")
g 
dev.off()


# 1.5 Substrates near pumping station and eel ladders ==============

su_pump <- filter(fa_data_su, Location_type == "pg")
su_pump$Location_type <- NULL
su_pg <- rbind(su_pump, fa_data_pg)


g <- ggloop(su_pg, aes_loop(x = Date, y = X14.0:X22.6n.3)) %L+%
  geom_point() %L+%
  geom_point(aes(colour = factor(Fishing_method)), size = 2) %L+%
  geom_smooth(aes(colour = factor(Fishing_method)), method="auto", se=TRUE, fullrange=FALSE, level=0.95) # geom_abline for regression line


# Save plot
pdf("./Figures/pumpingstation.pdf")
g 
dev.off()




# 2. NMDS ####

# 2.1. Total ####
NMDS <- metaMDS(fa_data[,c(2:25)],k=3,trymax=100)

stressplot(NMDS)
plot(NMDS, type = "text")
ordiplot(NMDS,type="n")
orditorp(NMDS,display="species",col="red",air=0.01)
orditorp(NMDS,display="sites",cex=0.75,air=0.01)


fa_pca_method <- c(rep("pg", 2),
                   rep("kn",2), 
                   "su",
                   rep("kn",3), 
                   "pg", 
                   rep("su", 2),
                   "pg",
                   "su",
                   "pg",
                   rep("su", 2),
                   rep("pg", 2),
                   "kn",
                   rep("pg", 2),
                   "su",
                   "pg",
                   "su",
                   "pg",
                   "su",
                   "pg",
                   "su",
                   "pg",
                   rep("su", 9),
                   "pg",
                   rep("su", 5),
                   rep("kn",2),
                   "pg",
                   rep("su", 3),
                   "kn",
                   rep("su", 5),
                   rep("pg", 2),
                   "su",
                   rep("pg", 2),
                   rep("su", 12),
                   rep("pg", 2),
                   rep("su", 3),
                   rep("pg", 2),
                   rep("su", 10),
                   rep("pg", 2),
                   rep("su", 2),
                   "kn",
                   rep("pg", 2),
                   rep("su", 16),
                   "pg",
                   rep("su", 2),
                   "pg",
                   rep("su", 3),
                   rep("pg", 4),
                   "su",
                   rep("pg", 2),
                   rep("su", 5),
                   rep("pg", 5),
                   rep("kn", 5),
                   rep("pg", 11),
                   "su",
                   rep("kn", 5),
                   rep("pg", 10),
                   rep("kn", 5),
                   rep("pg", 6),
                   rep("su", 2),
                   rep("kn", 4),
                   rep("pg", 3),
                   rep("su", 2),
                   "kn",
                   rep("pg", 7),
                   rep("su", 6),
                   rep("kn", 3),
                   rep("pg", 8),
                   "su",
                   "kn",
                   rep("pg", 7),
                   rep("su", 3),
                   rep("pg", 5),
                   "su",
                   rep("pg", 3),
                   rep("su", 2),
                   rep("pg", 17))


ordiplot(NMDS,type="n")
ordihull(NMDS, groups=fa_pca_method,draw="polygon",col="grey90",label=F)
orditorp(NMDS,display="species",col="black",air=0.01)
orditorp(NMDS,display="sites",col=c(rep("red", 2),     # pg = red, su = blue, kn = green
           rep("green",2), 
           "blue",
           rep("green",3), 
           "red", 
           rep("blue", 2),
           "red",
           "blue",
           "red",
           rep("blue", 2),
           rep("red", 2),
           "green",
           rep("red", 2),
           "blue",
           "red",
           "blue",
           "red",
           "blue",
           "red",
           "blue",
           "red",
           rep("blue", 9),
           "red",
           rep("blue", 5),
           rep("green",2),
           "red",
           rep("blue", 3),
           "green",
           rep("blue", 5),
           rep("red", 2),
           "blue",
           rep("red", 2),
           rep("blue", 12),
           rep("red", 2),
           rep("blue", 3),
           rep("red", 2),
           rep("blue", 10),
           rep("red", 2),
           rep("blue", 2),
           "green",
           rep("red", 2),
           rep("blue", 16),
           "red",
           rep("blue", 2),
           "red",
           rep("blue", 3),
           rep("red", 4),
           "blue",
           rep("red", 2),
           rep("blue", 5),
           rep("red", 5),
           rep("green", 5),
           rep("red", 11),
           "blue",
           rep("green", 5),
           rep("red", 10),
           rep("green", 5),
           rep("red", 6),
           rep("blue", 2),
           rep("green", 4),
           rep("red", 3),
           rep("blue", 2),
           "green",
           rep("red", 7),
           rep("blue", 6),
           rep("green", 3),
           rep("red", 8),
           "blue",
           "green",
           rep("red", 7),
           rep("blue", 3),
           rep("red", 5),
           "blue",
           rep("red", 3),
           rep("blue", 2),
           rep("red", 17)),
         air=0.01,cex=0.75)



# 2.1. NMDS after 15th of May only for substrates and eel ladder ####

subset <- filter(fa_data, Fishing_method == "Palinggoot" |
                          Fishing_method == "Substraat")
subset <- filter(subset, cdate > 17296)


NMDS2 <- metaMDS(subset[,c(2:25)],k=2,trymax=50)

stressplot(NMDS2)
plot(NMDS2, type = "text")
ordiplot(NMDS2,type="n")
orditorp(NMDS2,display="species",col="red",air=0.01)
orditorp(NMDS2,display="sites",cex=0.75,air=0.01)


fa_pca_method2 <- c("pg",
                   rep("su", 2),
                   rep("pg", 3),
                   "su",
                   "pg",
                   "su",
                   rep("pg", 2),
                   rep("su", 7),
                   rep("pg", 2),
                   rep("su", 18),
                   rep("pg", 2),
                   rep("su", 2),
                   rep("pg", 3),
                   rep("su", 2),
                   rep("pg", 17))


ordiplot(NMDS2,type="n")
ordihull(NMDS2, groups=fa_pca_method2,draw="polygon",col="grey90",label=F)
orditorp(NMDS2,display="species",col="black",air=0.01)
orditorp(NMDS2,display="sites",col=c("red",              # pg = red, su = blue
                                     rep("blue", 2),
                                     rep("red", 3),
                                     "blue",
                                     "red",
                                     "blue",
                                     rep("red", 2),
                                     rep("blue", 7),
                                     rep("red", 2),
                                     rep("blue", 18),
                                     rep("red", 2),
                                     rep("blue", 2),
                                     rep("red", 3),
                                     rep("blue", 2),
                                     rep("red", 17)),
         air=0.01,cex=0.75)


# 2.2. NMDS per month ####

subset <- filter(fa_data, Fishing_method == "Palinggoot" |
                   Fishing_method == "Substraat")

# 2.2.1 March ####
march <- filter(subset, Month == "3")


NMDS_march <- metaMDS(march[,c(2:25)],k=2,trymax=50)

stressplot(NMDS_march)
plot(NMDS_march, type = "text")
ordiplot(NMDS_march,type="n")
orditorp(NMDS_march,display="species",col="red",air=0.01)
orditorp(NMDS_march,display="sites",cex=0.75,air=0.01)


fa_pca_method_march <- c(rep("su", 2),
                    "pg",
                    rep("su", 3),
                    rep("pg", 2),
                    rep("su", 10),
                    "pg",
                    rep("su", 6),
                    rep("pg", 16),
                    "su",
                    rep("pg", 16),
                    rep("su",2))


ordiplot(NMDS_march,type="n")
ordihull(NMDS_march, groups=fa_pca_method_march,draw="polygon",col="grey90",label=F)
orditorp(NMDS_march,display="species",col="black",air=0.01)
orditorp(NMDS_march,display="sites",col=c(rep("blue", 2),    # pg = red, su = blue
                                            "red",
                                            rep("blue", 3),
                                            rep("red", 2),
                                            rep("blue", 10),
                                            "red",
                                            rep("blue", 6),
                                            rep("red", 16),
                                            "blue",
                                            rep("red", 16),
                                            rep("blue",2)),
         air=0.01,cex=0.75)



# 2.2.2 April ####
april <- filter(subset, Month == "4")


NMDS_april <- metaMDS(april[,c(2:25)],k=2,trymax=50)

stressplot(NMDS_april)
plot(NMDS_april, type = "text")
ordiplot(NMDS_april,type="n")
orditorp(NMDS_april,display="species",col="red",air=0.01)
orditorp(NMDS_april,display="sites",cex=0.75,air=0.01)


fa_pca_method_april <- c(rep("pg", 2),
                         "su",
                         "pg",
                         rep("su", 2),
                         rep("pg", 2),
                         rep("su", 1),
                         "pg",
                         rep("su", 7),
                         "pg",
                         rep("su", 5),
                         "pg",
                         rep("su", 10),
                         rep("pg",10),
                         rep("su", 2),
                         rep("pg", 7),
                         rep("su", 6),
                         rep("pg", 8),
                         "su",
                         rep("pg", 7),
                         rep("su", 3))


ordiplot(NMDS_april,type="n")
ordihull(NMDS_april, groups=fa_pca_method_april,draw="polygon",col="grey90",label=F)
orditorp(NMDS_april,display="species",col="black",air=0.01)
orditorp(NMDS_april,display="sites",col=c(rep("red", 2),      # pg = red, su = blue
                                            "blue",
                                            "red",
                                            rep("blue", 2),
                                            rep("red", 2),
                                            rep("blue", 1),
                                            "red",
                                            rep("blue", 7),
                                            "red",
                                            rep("blue", 5),
                                            "red",
                                            rep("blue", 10),
                                            rep("red",10),
                                            rep("blue", 2),
                                            rep("red", 7),
                                            rep("blue", 6),
                                            rep("red", 8),
                                            "blue",
                                            rep("red", 7),
                                            rep("blue", 3)),
         air=0.01,cex=0.75)


# 2.2.3 May ####
may <- filter(subset, Month == "5")


NMDS_may <- metaMDS(may[,c(2:25)],k=2,trymax=50)

stressplot(NMDS_may)
plot(NMDS_may, type = "text")
ordiplot(NMDS_may,type="n")
orditorp(NMDS_may,display="species",col="red",air=0.01)
orditorp(NMDS_may,display="sites",cex=0.75,air=0.01)


fa_pca_method_may <- c("pg",
                       "su",
                      "pg",
                      rep("su", 2),
                      rep("pg", 3),
                      "su",
                      "pg",
                      "su",
                      "pg",
                      rep("su", 3),
                      rep("pg", 4),
                      rep("su", 9),
                      "pg",
                      rep("su", 3),
                      rep("pg", 2),
                      rep("su", 18),
                      rep("pg", 2),
                      rep("su", 2),
                      rep("pg", 5),
                      "su",
                      rep("pg", 3),
                      rep("su", 2),
                      rep("pg", 17))


ordiplot(NMDS_may,type="n")
ordihull(NMDS_may, groups=fa_pca_method_may,draw="polygon",col="grey90",label=F)
orditorp(NMDS_may,display="species",col="black",air=0.01)
orditorp(NMDS_may,display="sites",col=c("red",         # pg = red, su = blue
                                          "blue",
                                          "red",
                                          rep("blue", 2),
                                          rep("red", 3),
                                          "blue",
                                          "red",
                                          "blue",
                                          "red",
                                          rep("blue", 3),
                                          rep("red", 4),
                                          rep("blue", 9),
                                          "red",
                                          rep("blue", 3),
                                          rep("red", 2),
                                          rep("blue", 18),
                                          rep("red", 2),
                                          rep("blue", 2),
                                          rep("red", 5),
                                          "blue",
                                          rep("red", 3),
                                          rep("blue", 2),
                                          rep("red", 17)),
         air=0.01,cex=0.75)


## Plot all NMDS in single figure
grid <- matrix(c(1,1,2,3,4,5), nrow = 3,
               ncol = 2, byrow = TRUE)


layout(grid)




# 3. PCA ####
library(devtools)
install.packages("remotes")
remotes::install_github("vqv/ggbiplot")
library(ggbiplot)


# Create PCA
fa_pca <- prcomp(fa_data[,c(2:25)], center = TRUE,scale. = TRUE)
summary(fa_pca)
str(fa_pca)

# Plot PcA
ggbiplot(fa_pca)

# Create grouping variable
fa_pca_method <- c(rep("pg", 2), 
                   rep("kn",2), 
                   "su",
                   rep("kn",3), 
                   "pg", 
                   rep("su", 2),
                   "pg",
                   "su",
                   "pg",
                   rep("su", 2),
                   rep("pg", 2),
                   "kn",
                   rep("pg", 2),
                   "su",
                   "pg",
                   "su",
                   "pg",
                   "su",
                   "pg",
                   "su",
                   "pg",
                   rep("su", 9),
                   "pg",
                   rep("su", 5),
                   rep("kn",2),
                   "pg",
                   rep("su", 3),
                   "kn",
                   rep("su", 5),
                   rep("pg", 2),
                   "su",
                   rep("pg", 2),
                   rep("su", 12),
                   rep("pg", 2),
                   rep("su", 3),
                   rep("pg", 2),
                   rep("su", 10),
                   rep("pg", 2),
                   rep("su", 2),
                   "kn",
                   rep("pg", 2),
                   rep("su", 16),
                   "pg",
                   rep("su", 2),
                   "pg",
                   rep("su", 3),
                   rep("pg", 4),
                   "su",
                   rep("pg", 2),
                   rep("su", 5),
                   rep("pg", 5),
                   rep("kn", 5),
                   rep("pg", 11),
                   "su",
                   rep("kn", 5),
                   rep("pg", 10),
                   rep("kn", 5),
                   rep("pg", 6),
                   rep("su", 2),
                   rep("kn", 4),
                   rep("pg", 3),
                   rep("su", 2),
                   "kn",
                   rep("pg", 7),
                   rep("su", 6),
                   rep("kn", 3),
                   rep("pg", 8),
                   "su",
                   "kn",
                   rep("pg", 7),
                   rep("su", 3),
                   rep("pg", 5),
                   "su",
                   rep("pg", 3),
                   rep("su", 2),
                   rep("pg", 17))

# Add ellipses according to group
ggbiplot(fa_pca, ellipse=TRUE, groups=fa_pca_method)

# Look at different axes
ggbiplot(fa_pca, ellipse=TRUE, choices=c(3,4) , groups=fa_pca_method)





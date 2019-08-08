# Data exploration
# By Pieterjan Verhelst
# Pieterjan.Verhelst@ugent.be



library(DataExplorer)
library(ggplot2)
library(ggpubr)
library(ggloop)
library(vegan)



list1 <- colnames(fa_data)
list1 <- list1[-c(1, 26:37)]


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
  if (fa_data_su$Location[i] == "VA_PG_LO_SU" | fa_data_su$Location[i] == "VA_PG_RO_SU"){
    fa_data_su$Location_type[i] = "PG"
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

su_pump <- filter(fa_data_su, Location_type == "PG")
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




# 2. MDS ####

fa_data2 <- read.csv("./data/raw/FA_rel.csv",stringsAsFactors = FALSE)
fa_data2 <- fa_data2[,-1]



NMDS <- metaMDS(fa_data2,k=2,trymax=100)

stressplot(NMDS)
plot(NMDS, type = "text")
ordiplot(NMDS,type="n")
orditorp(NMDS,display="species",col="red",air=0.01)
orditorp(NMDS,display="sites",cex=0.75,air=0.01)




#treat <- c(rep("Treatment1",12),rep("Treatment2",12))

pg <- c(1:6,12:22, 33:42, 64:72, 88:97,117:126,147:156,176:185,196:205,216:220,232:245,256:265)
su <- c(23:27, 48:55,57,73:82,102:111,132:141,157,158,160,161,167:171,186:192,194,195,206:215,221:227,229,230,246:255, 267:269,271:276)
kn <- c(7:11,28:32,58:62,83:87,112:116,142:146,172:175)

#ordihull(groups=treat,draw="polygon",col="grey90",label=F)
ordiplot(NMDS,type="n")
orditorp(NMDS,display="species",col="red",air=0.01)
orditorp(NMDS,display="sites",col=c(
  rep("green",pg),
  rep("blue",su),
  rep("yellow", kn)), 
  air=0.01,cex=0.75)








# 3. Cluster analyse ####

soren <- vegdist(fa_data2, "bray")
tree.soren <- hclust(soren, "average") 
plot(tree.soren, hang = -1)





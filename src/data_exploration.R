# Data exploration
# By Pieterjan Verhelst
# Pieterjan.Verhelst@ugent.be



library(DataExplorer)
library(ggplot2)
library(ggpubr)
library(ggloop)




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





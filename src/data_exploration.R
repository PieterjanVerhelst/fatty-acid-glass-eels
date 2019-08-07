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

# 1.1 All
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


# 1.2 Catch method

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


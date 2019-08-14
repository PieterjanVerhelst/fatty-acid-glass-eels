# Analysis of polynomial distributions
# By Pieterjan Verhelst
# Pieterjan.Verhelst@ugent.be


library(userfriendlyscience)
library(lsmeans)


# Create dataset without "kruisnetten"
subset <- filter(fa_data, Fishing_method == "Palinggoot" |
                   Fishing_method == "Substraat")



# 1. METHOD 1 ####

# https://stats.stackexchange.com/questions/33013/what-test-can-i-use-to-compare-slopes-from-two-or-more-regression-models

# Create model
m.interaction <- lm(cdate ~ poly(X22.5n.3, 2) * Fishing_method, data = subset)
anova(m.interaction)

# Obtain slopes
m.interaction$coefficients
m.lst <- lstrends(m.interaction, "Fishing_method", var="X22.5n.3")

# Compare slopes
pairs(m.lst)



# 2. METHOD 2 ####    PREFERRED METHOD

# https://stats.stackexchange.com/questions/231059/compare-the-statistical-significance-of-the-difference-between-two-polynomial-re

#model without grouping variable
fit1 <- lm(cdate ~ poly(X22.6n.3, 3), data = subset)

#model with grouping variable
fit2 <- lm(cdate ~ poly(X22.6n.3, 3) * Fishing_method, data = subset)

#compare models 
# Model with the smaller RSS indicates the better fitting model
anova(fit1, fit2)   



# Test if model of another order fits better
fit3 <- lm(cdate ~ poly(X22.6n.3, 1) * Fishing_method, data = subset)
fit4 <- lm(cdate ~ poly(X22.6n.3, 3) * Fishing_method, data = subset)
#compare models 
anova(fit3, fit2)   
anova(fit2, fit4)




# Plot for distribution check
ggplot(subset, aes(x = Date, y = X22.6n.3)) +
  geom_point(aes(colour = factor(Fishing_method)), size = 2) +
  geom_smooth(aes(colour = factor(Fishing_method)), method ="lm", formula = y ~ poly(x,3)) 


# Save plot
pdf("./Figures/Fa_polynomial.pdf")



dev.off()










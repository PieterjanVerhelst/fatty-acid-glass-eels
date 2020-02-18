# Analysis of polynomial distributions
# By Pieterjan Verhelst
# Pieterjan.Verhelst@ugent.be


library(userfriendlyscience)
library(lsmeans)
library(AppliedPredictiveModeling)
library(caret)
library(patchwork)


# Create dataset without "kruisnetten"
subset <- filter(fa_rel,
                 Fishing_method == "Palinggoot" |
                   Fishing_method == "Substraat")

# 2. POLYNOMIAL REGRESSION ANALYSIS OF LIPID BIOSYNTHESIS PATHWAY FA

# https://stats.stackexchange.com/questions/231059/compare-the-statistical-significance-of-the-difference-between-two-polynomial-re

## 18:1 (n-9)
fit1 <- lm(subset$cis.9.18.1 ~ poly(cdate, 2), data = subset)
fit2 <- lm(subset$cis.9.18.1 ~ poly(cdate, 2) * Fishing_method, data = subset)

anova(fit1,fit2) # significant difference between fishing methods

# Test if model of another order fits better
fit3 <- lm(subset$cis.9.18.1 ~ poly(cdate, 1) * Fishing_method, data = subset)
fit4 <- lm(subset$cis.9.18.1 ~ poly(cdate, 3) * Fishing_method, data = subset)
#compare models 
anova(fit3, fit2)    
anova(fit2, fit4) # 2nd degree is a significant better fit

# Plot to check distribution
FA1 <- ggplot(subset, aes(x = Date, y = cis.9.18.1, colour = factor(Fishing_method))) +
  geom_point() +
  geom_smooth(method ="lm", formula = y ~ poly(x,2))  +
  ylab("18:1 (n-9)") + 
  scale_colour_manual(name = "Fishing method", labels = c("Eel ladder", "Artificial substrates"), values = c("#6CA6CD", "#E64B35B2")) +
  theme(legend.position = "none")

## 18:3 (n-3)
fit1 <- lm(subset$X18.3n.3 ~ poly(cdate, 2), data = subset)
fit2 <- lm(subset$X18.3n.3 ~ poly(cdate, 2) * Fishing_method, data = subset)

anova(fit1,fit2) # significant difference between fishing methods

# Test if model of another order fits better
fit3 <- lm(subset$X18.3n.3 ~ poly(cdate, 1) * Fishing_method, data = subset)
fit4 <- lm(subset$X18.3n.3 ~ poly(cdate, 3) * Fishing_method, data = subset)
#compare models 
anova(fit3, fit2)    
anova(fit2, fit4) # 3d degree is a significant better fit

# Plot to check distribution

FA2 <- ggplot(subset, aes(x = Date, y = X18.3n.3, colour = factor(Fishing_method))) +
  geom_point() +
  geom_smooth(method ="lm", formula = y ~ poly(x,3))  +
  ylab("18:3 (n-3)") + 
  scale_colour_manual(name = "Fishing method", labels = c("Eel ladder", "Artificial substrates"), values = c("#6CA6CD", "#E64B35B2")) +
  theme(legend.position = "none")

## 18:4 (n-3)
fit1 <- lm(subset$X18.4n.3 ~ poly(cdate, 2), data = subset)
fit2 <- lm(subset$X18.4n.3 ~ poly(cdate, 2) * Fishing_method, data = subset)

anova(fit1,fit2) # significant difference between fishing method

# Test if model of another order fits better
fit3 <- lm(subset$X18.4n.3 ~ poly(cdate, 1) * Fishing_method, data = subset)
fit4 <- lm(subset$X18.4n.3 ~ poly(cdate, 3) * Fishing_method, data = subset)
#compare models 
anova(fit3, fit2)    
anova(fit2, fit4) # 2nd degree is a significant better fit

# Plot to check distribution
FA3 <- ggplot(subset, aes(x = Date, y = X18.4n.3, colour = factor(Fishing_method))) +
  geom_point() +
  geom_smooth(method ="lm", formula = y ~ poly(x,2))  +
  ylab("18:4 (n-3)") + 
  scale_colour_manual(name = "Fishing method", labels = c("Eel ladder", "Artificial substrates"), values = c("#6CA6CD", "#E64B35B2")) +
  theme(legend.position = "none")

## 20:4 (n-3)
fit1 <- lm(subset$X20.4n.3 ~ poly(cdate, 2), data = subset)
fit2 <- lm(subset$X20.4n.3 ~ poly(cdate, 2) * Fishing_method, data = subset)

anova(fit1,fit2) # significant difference between fishing method

# Test if model of another order fits better
fit3 <- lm(subset$X20.4n.3 ~ poly(cdate, 1) * Fishing_method, data = subset)
fit4 <- lm(subset$X20.4n.3 ~ poly(cdate, 3) * Fishing_method, data = subset)
#compare models 
anova(fit3, fit2)    
anova(fit2, fit4) # 2nd degree is a significant better fit

# Plot to check distribution
FA4 <- ggplot(subset, aes(x = Date, y = X20.4n.3, colour = factor(Fishing_method))) +
  geom_point() +
  geom_smooth(method ="lm", formula = y ~ poly(x,2))  +
  ylab("20:4 (n-3)") + 
  scale_colour_manual(name = "Fishing method", labels = c("Eel ladder", "Artificial substrates"), values = c("#6CA6CD", "#E64B35B2")) +
  theme(legend.position = "none")

## 20:5 (n-3)
fit1 <- lm(subset$X20.5n.3 ~ poly(cdate, 2), data = subset)
fit2 <- lm(subset$X20.5n.3 ~ poly(cdate, 2) * Fishing_method, data = subset)

anova(fit1,fit2) # significant difference between fishing methods

# Test if model of another order fits better
fit3 <- lm(subset$X20.5n.3 ~ poly(cdate, 1) * Fishing_method, data = subset)
fit4 <- lm(subset$X20.5n.3 ~ poly(cdate, 3) * Fishing_method, data = subset)
#compare models 
anova(fit3, fit2)    
anova(fit2, fit4) # 2nd degree is a significant better fit

# Plot to check distribution
FA5 <- ggplot(subset, aes(x = Date, y = X20.5n.3, colour = factor(Fishing_method))) +
  geom_point() +
  geom_smooth(method ="lm", formula = y ~ poly(x,2))  +
  ylab("20:5 (n-3)") + 
  scale_colour_manual(name = "Fishing method", labels = c("Eel ladder", "Artificial substrates"), values = c("#6CA6CD", "#E64B35B2")) +
  theme(legend.position = "none")

## 22:5 (n-3)
fit1 <- lm(subset$X22.5n.3 ~ poly(cdate, 2), data = subset)
fit2 <- lm(subset$X22.5n.3 ~ poly(cdate, 2) * Fishing_method, data = subset)

anova(fit1,fit2) # significant difference between fishing methods

# Test if model of another order fits better
fit3 <- lm(subset$X22.5n.3 ~ poly(cdate, 1) * Fishing_method, data = subset)
fit4 <- lm(subset$X22.5n.3 ~ poly(cdate, 3) * Fishing_method, data = subset)
#compare models 
anova(fit3, fit2)   
anova(fit2, fit4) # 3d degree is a significant better fit

# Plot to check distribution
FA6 <- ggplot(subset, aes(x = Date, y = X22.5n.3, colour = factor(Fishing_method))) +
  geom_point() +
  geom_smooth(method ="lm", formula = y ~ poly(x,3))  +
  ylab("22:5 (n-3)") + 
  scale_colour_manual(name = "Fishing method", labels = c("Eel ladder", "Artificial substrates"), values = c("#6CA6CD", "#E64B35B2")) +
  theme(legend.position = "none")

## 22:6 (n-3)
fit1 <- lm(subset$X22.6n.3 ~ poly(cdate, 2), data = subset)
fit2 <- lm(subset$X22.6n.3 ~ poly(cdate, 2) * Fishing_method, data = subset)

anova(fit1,fit2) # no significant difference between fishing methods

# Test if model of another order fits better
fit3 <- lm(subset$X22.6n.3 ~ poly(cdate, 1), data = subset)
fit4 <- lm(subset$X22.6n.3 ~ poly(cdate, 3), data = subset)
#compare models 
anova(fit3, fit2)   
anova(fit2, fit4) # 1st degree is a significant better fit

# Plot to check distribution
FA7 <- ggplot(subset, aes(x = Date, y = X22.6n.3, colour = factor(Fishing_method))) +
  geom_point() +
  geom_smooth(method ="lm", formula = y ~ poly(x,1))  +
  ylab("22:6 (n-3)") + 
  scale_colour_manual(name = "Fishing method", labels = c("Eel ladder", "Artificial substrates"), values = c("#6CA6CD", "#E64B35B2")) +
  theme(legend.position = "none")

## 18:2 (n-6)
fit1 <- lm(subset$X18.2n.6 ~ poly(cdate, 2), data = subset)
fit2 <- lm(subset$X18.2n.6 ~ poly(cdate, 2) * Fishing_method, data = subset)

anova(fit1,fit2) # significant difference between fishing methods

# Test if model of another order fits better
fit3 <- lm(subset$X18.2n.6 ~ poly(cdate, 1) * Fishing_method, data = subset)
fit4 <- lm(subset$X18.2n.6 ~ poly(cdate, 3) * Fishing_method, data = subset)
#compare models 
anova(fit3, fit2) 
anova(fit2, fit4) # 3d degree is a significant better fit

# Plot to check distribution
FA8 <- ggplot(subset, aes(x = Date, y = X18.2n.6, colour = factor(Fishing_method))) +
  geom_point() +
  geom_smooth(method ="lm", formula = y ~ poly(x,3))  +
  ylab("18:2 (n-6)") + 
  scale_colour_manual(name = "Fishing method", labels = c("Eel ladder", "Artificial substrates"), values = c("#6CA6CD", "#E64B35B2")) +
  theme(legend.position = "none")

## 20:3 (n-6)
fit1 <- lm(subset$X20.3n.6 ~ poly(cdate, 2), data = subset)
fit2 <- lm(subset$X20.3n.6 ~ poly(cdate, 2) * Fishing_method, data = subset)

anova(fit1,fit2) # significant difference between fishing methods

# Test if model of another order fits better
fit3 <- lm(subset$X20.3n.6 ~ poly(cdate, 1) * Fishing_method, data = subset)
fit4 <- lm(subset$X20.3n.6 ~ poly(cdate, 3) * Fishing_method, data = subset)
#compare models 
anova(fit3, fit2) # 1ST degree is a significant better fit   
anova(fit2, fit4) # 

# Plot to check distribution
FA9 <- ggplot(subset, aes(x = Date, y = X20.3n.6, colour = factor(Fishing_method))) +
  geom_point() +
  geom_smooth(method ="lm", formula = y ~ poly(x,1))  +
  ylab("20:3 (n-6)") + 
  scale_colour_manual(name = "Fishing method", labels = c("Eel ladder", "Artificial substrates"), values = c("#6CA6CD", "#E64B35B2")) +
  theme(legend.position = "none")

## 20:4 (n-6)
fit1 <- lm(subset$X20.4n.6 ~ poly(cdate, 2), data = subset)
fit2 <- lm(subset$X20.4n.6 ~ poly(cdate, 2) * Fishing_method, data = subset)

anova(fit1,fit2) # no significant difference between fishing method

# Test if model of another order fits better
fit3 <- lm(subset$X20.4n.6 ~ poly(cdate, 1), data = subset)
fit4 <- lm(subset$X20.4n.6 ~ poly(cdate, 3), data = subset)
#compare models 
anova(fit3, fit1) # 1ST degree is a significant better fit   
anova(fit1, fit4) # 

# Plot to check distribution
FA10 <- ggplot(subset, aes(x = Date, y = X20.4n.6, colour = factor(Fishing_method))) +
  geom_point() +
  geom_smooth(method ="lm", formula = y ~ poly(x,1))  +
  ylab("20:4 (n-6)") + 
  scale_colour_manual(name = "Fishing method", labels = c("Eel ladder", "Artificial substrates"), values = c("#6CA6CD", "#E64B35B2")) +
  theme(legend.position = "none")

## 22:5 (n-6)
fit1 <- lm(subset$X22.5n.6 ~ poly(cdate, 2), data = subset)
fit2 <- lm(subset$X22.5n.6 ~ poly(cdate, 2) * Fishing_method, data = subset)

anova(fit1,fit2) # no significant difference between fishing method

# Test if model of another order fits better
fit3 <- lm(subset$X22.5n.6 ~ poly(cdate, 1), data = subset)
fit4 <- lm(subset$X22.5n.6 ~ poly(cdate, 3), data = subset)
#compare models 
anova(fit3, fit1) # 1ST degree is a significant better fit   
anova(fit1, fit4) # 

# Plot to check distribution
FA11 <- ggplot(subset, aes(x = Date, y = X22.5n.6, colour = factor(Fishing_method))) +
  geom_point() +
  geom_smooth(method ="lm", formula = y ~ poly(x,1))  +
  ylab("22:5 (n-6)") + 
  scale_colour_manual(name = "Fishing method", labels = c("Eel ladder", "Artificial substrates"), values = c("#6CA6CD", "#E64B35B2")) +
  theme(legend.position = "none")

FA_polynomials <- (FA2 + FA3 + FA4 + FA5 + FA6 + FA7 + FA8 + FA9 + FA10 + FA11)
FA_polynomials
# Save plot
pdf(FA_polynomials, "./Figures/Fa_polynomials.pdf")

dev.off()






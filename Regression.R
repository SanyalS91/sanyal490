#Regression
#==================================================================================

library(MASS)
Advertising <- read.csv("D:/Analytics/R/Advertising.csv",header = TRUE)
View(Advertising)

dim(Advertising)
head(Advertising)
str(Advertising)

#Is there a relationship between advertising budget and sales?
#How strong is the relationship between advertising budget and sales?
#Which media contribute to sales?
#How accurately can we estimate the effect of each medium on sales?
#How accurately can we predict future sales?
#Is the relationship linear?
#Is there synergy among the advertising media?

# Linear Regression Sales on TV 
#===============================
slr.model.TV <- lm(Sales ~ TV,data = Advertising)
slr.model.TV
summary(slr.model.TV)
plot(x = Advertising$TV,y=Advertising$Sales,col="red",pch=20,xlab="TV budget(in units of 1000$)",ylab = "Sales")
abline(slr.model.TV,col="black",lwd = 2.5)
title("Linear Regression of Sales vs TV budget")

# Linear Regression Sales on Radio
#===============================
slr.model.Radio <- lm(Sales ~ Radio,data = Advertising)
slr.model.Radio
summary(slr.model.Radio)
plot(x = Advertising$Radio,y=Advertising$Sales,col="blue",pch=20,xlab="Radio budget(in units of 1000$)",ylab = "Sales")
abline(slr.model.Radio,col="red",lwd = 2.5)
title("Linear Regression of Sales vs Radio budget")

# Linear Regression Sales on Newspaper 
#===============================
slr.model.Newspaper <- lm(Sales ~ Newspaper,data = Advertising)
slr.model.Newspaper
summary(slr.model.Newspaper)

plot(x = Advertising$Newspaper,y=Advertising$Sales,col="blue",pch=20,xlab="Newspaper budget(in units of 1000$)",ylab = "Sales")
abline(slr.model.Newspaper,col="red",lwd = 2.5)
title("Linear Regression of Sales vs Newspaper budget")

# Multiple Linear Regression
#=========================================
mlr.model <- lm(Sales ~ . , data = Advertising)
summary(mlr.model)
confint(mlr.model)

mlr.model <- lm(Sales ~ TV + Radio, data = Advertising)
summary(mlr.model)

# Interaction 
mlr.interaction <- lm(Sales ~ TV +Radio + TV:Radio, data = Advertising)
summary(mlr.interaction)

# Polynomial
mlr.poly <- lm(Sales ~ TV +Radio +I(Radio^2),data = Advertising)
summary(mlr.poly)

#============================================================================
par(mfrow=c(2,2))
plot(mlr.model)
plot(mlr.interaction)

par(mfrow=c(1,1))
library(car)
vif(mlr.model)

# Prediction
#===================================
predict(mlr.model,newdata = data.frame(TV = 150, Radio = 50, Newspaper = 75 ),interval =  "confidence")

# F-test
# R^2,RSE
# p-value
# se/CI of betas
# Predict
# Residual plots
# Interaction

# 1. Non-linearity of the response-predictor relationships.
# 2. Correlation of error terms.
# 3. Non-constant variance of error terms.
# 4. Outliers.
# 5. High-leverage points.
# 6. Collinearity.
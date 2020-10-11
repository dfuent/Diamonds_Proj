# STAT 6021
# Proj 1: Diamonds

library(MASS) # for Box-Cox
library(dplyr)

diamonds_data<-read.csv("diamonds4.csv", header=TRUE)
#diamonds_data # display data when uncommented

attach(diamonds_data) # attach

head(diamonds_data)

# initial data checks:

# 1) Factor
is.factor(clarity)
is.factor(color)
is.factor(cut)

clarity <- factor(clarity)
color <- factor(color)
cut <- factor(cut)

# 2) View Levels:

levels(clarity) 
levels(color)
levels(cut)

# 3) Check to make sure the quant variables are actually quantitative

is.numeric(carat)
is.numeric(price)

# 4) run a full regression, summary, and ANOVA. Notice that cut doesn't seem significant
# based on its t-vals; same with some colors

r_full <- lm(price~., data = diamonds_data)
summary(r_full)
anova(r_full)

# Begin to set up scatters:

# subset by cut, color, and clarity

p_cut <- c('price', 'cut')
p_color <- diamonds_data[c('price', 'color')]
p_clarity <- diamonds_data[c('price', 'clarity')]


## consider each cut as a subset
AI<-subset(diamonds_data,cut=="Astor Ideal") 
I<-subset(diamonds_data,cut=="Ideal") 
G<-subset(diamonds_data,cut=="Good") 
VG<-subset(diamonds_data,cut=="Very Good") 

# fit separate regressions
price_AI <- lm(price~carat,data=AI)
price_I <- lm(price~carat,data=I)
price_G <- lm(price~carat,data=G)
price_VG <- lm(price~carat,data=VG)

# summary(price_AI)

# Astor Ideal is the reference

# Create scatters:

plot(carat, price, main="Bodyweight of the Baby and Mother's Age by Race")
points(I$carat, I$price, pch=2, col="blue")
points(G$carat, G$price, pch=3, col="orange")
points(VG$carat, VG$price, pch=4, col="red")

abline(price_AI,lty=1)
abline(price_I,lty=2, col="blue") 
abline(price_G,lty=3, col="orange")
abline(price_VG,lty=4, col="red")
legend("topleft", c("Astor Ideal", "Ideal", "Good", "Very Good"), lty=c(1, 2, 3, 4), pch=c(1,2,12), col=c("black", "blue", "orange", "red")) 


# Clearly a non-linear pattern in the data and lack of residual variance
# Focus on transforming the data:

price_carat <- lm(price~carat)
summary(price_carat)

plot(price~carat, main = "Price by Carat")
abline(price_carat, col = 'red')
grid()

plot(price_carat$fitted.values, price_carat$residuals, main = 'Residual Plot') # residual plot
abline(h=0,col="red")
grid()

## impossible to tell if the variance is constant; clearly a non-linear pattern issue. Run Box-Cox

boxcox(price_carat, lambda = seq(.27, .33, by = 0.01)) # Box-Cox plot
grid()

# to fix the variance issue, we can transform price by lambda = 3/10

diamonds_data <- mutate(diamonds_data, pt3_price = (price)^(3/10))

head(diamonds_data)

attach(diamonds_data)

t_price_mod <- lm(pt3_price~carat)

summary(t_price_mod)

plot(pt3_price~carat, main = "Price by Carat")
abline(t_price_mod, col = 'red')
grid()

plot(t_price_mod$fitted.values, t_price_mod$residuals, main = 'Residual Plot') # residual plot
abline(h=0,col="red")
grid()

boxcox(t_price_mod, lambda = seq(.8, 1.2, by = 0.1)) # Box-Cox plot
grid()

# We've corrected for the variance issue on y

# need to transform carat (looks like a non-linear relationship)
# From the scatter plot, we see a potentially log or sqrt function

diamonds_data <- mutate(diamonds_data, log_carat = log(carat), log_t_price = log(pt3_price))

head(diamonds_data)

attach(diamonds_data)

t_both_mod <- lm(log_t_price~log_carat)

summary(t_both_mod)

plot(log_t_price~log_carat, main = "Price by Carat")
abline(t_both_mod, col = 'red')
grid()

plot(t_both_mod$fitted.values, t_both_mod$residuals, main = 'Residual Plot') # residual plot
abline(h=0,col="red")
grid()

boxcox(t_both_mod, lambda = seq(0, .8, by = 0.1)) # Box-Cox plot

# This second transformation didn't help, though the residuals LOOKED better and the model seemed linear, the Box-Cox
# did not show constant residual variance. Looking back at the scatter, lower carat values didn't have constant residual
# variance.

# We will stick with the 3/10ths transform on y as it passed the tests on regression variance 
# model is t_price_mod

summary(t_price_mod)
anova(t_price_mod)

# it's clear that carat and price have a linear relationship -- F-val is 1.5e+04 w/ p-val 0.05 >> 2.2e-16

data_t <- diamonds_data[c('carat', 'clarity', 'color', 'cut', 'pt3_price')]

# redo scatters

p_cut <- data_t[c('price', 'cut')]
p_color <- data_t[c('price', 'color')]
p_clarity <- data_t[c('price', 'clarity')]


## consider each cut as a subset
AI<-subset(data_t,cut=="Astor Ideal") 
I<-subset(data_t,cut=="Ideal") 
G<-subset(data_t,cut=="Good") 
VG<-subset(data_t,cut=="Very Good") 

# fit separate regressions
price_AI <- lm(pt3_price~carat,data=AI)
price_I <- lm(pt3_price~carat,data=I)
price_G <- lm(pt3_price~carat,data=G)
price_VG <- lm(pt3_price~carat,data=VG)

# summary(price_AI)

# Astor Ideal is the reference

# Create scatters:

plot(carat, pt3_price, main="Bodyweight of the Baby and Mother's Age by Race")
points(I$carat, I$pt3_price, pch=2, col="blue")
points(G$carat, G$pt3_price, pch=3, col="orange")
points(VG$carat, VG$pt3_price, pch=4, col="red")

abline(price_AI,lty=1)
abline(price_I,lty=2, col="blue") 
abline(price_G,lty=3, col="orange")
abline(price_VG,lty=4, col="red")
legend("topleft", c("Astor Ideal", "Ideal", "Good", "Very Good"), lty=c(1, 2, 3, 4), pch=c(1,2,12), col=c("black", "blue", "orange", "red")) 


# STAT 6021
# Proj 1: Diamonds

library(MASS) # for Box-Cox
library(dplyr)

diamonds_data<-read.csv("diamonds4.csv", header=TRUE)
#diamonds_data # display data when uncommented

summary(diamonds_data)
attach(diamonds_data) # attach

diamonds_data
head(diamonds_data)

# initial data checks:

# 1) Factor
is.factor(clarity)
is.factor(color)
is.factor(cut)

clarity <- factor(clarity, levels = c('SI2', 'SI1', 'VS2', 'VS1', 'VVS2', 'VVS1', 'IF', 'FL'))
color <- factor(color, levels = c('J', 'I', 'H', 'G', 'F', 'E', 'D'))
cut <- factor(cut, levels = c('Good', 'Very Good', 'Ideal', 'Astor Ideal'))

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

# 5) check n per each categorical variable

count(diamonds_data, c(cut))
count(diamonds_data, c(clarity))
count(diamonds_data, c(color))

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

plot(carat, price, main="Price by Carat and Cut")
points(I$carat, I$price, pch=2, col="blue")
points(G$carat, G$price, pch=3, col="orange")
points(VG$carat, VG$price, pch=4, col="red")

abline(price_AI,lty=1)
abline(price_I,lty=2, col="blue") 
abline(price_G,lty=3, col="orange")
abline(price_VG,lty=4, col="red")
legend("topleft", c("Astor Ideal", "Ideal", "Good", "Very Good"), lty=c(1, 2, 3, 4), 
       pch=c(1,2,12), col=c("black", "blue", "orange", "red")) 


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

# to fix the variance issue, we can transform price by lambda = 1/3

diamonds_data <- mutate(diamonds_data, cbrt_price = (price)^(1/3))

head(diamonds_data)

attach(diamonds_data)

t_price_mod <- lm(cbrt_price~carat)

summary(t_price_mod)

plot(cbrt_price~carat, main = "Cuberoot Price by Carat")
abline(t_price_mod, col = 'red')
grid()

plot(t_price_mod$fitted.values, t_price_mod$residuals, main = 'Residual Plot') # residual plot
abline(h=0,col="red")
grid()

boxcox(t_price_mod, lambda = seq(.5, 1.2, by = 0.1)) # Box-Cox plot
grid()

# We've corrected for the variance issue on y more or less using cuberoot

# need to transform carat (looks like a non-linear relationship)
# From the scatter plot, we see a potentially log or sqrt function. We'll try log first

diamonds_data <- mutate(diamonds_data, log_carat = log(carat), 
                        log_cbrt_price = log(price^(1/3)), log_price = log(price))

head(diamonds_data)

attach(diamonds_data)

t_1_mod <- lm(cbrt_price~log_carat)

plot(cbrt_price~log_carat, main = "Log Price by Log Carat")
abline(t_1_mod, col = 'red')
grid()

plot(t_1_mod$fitted.values, t_1_mod$residuals, main = 'Residual Plot') # residual plot
abline(h=0,col="red")
grid()

boxcox(t_1_mod, lambda = seq(-.5, .5, by = 0.1)) # Box-Cox plot
grid()

# variance looks better, but a huge non-linear pattern now
# we can run another transform on y to try to flatten out the pattern


t_both_mod <- lm(log_cbrt_price~log_carat)

summary(t_both_mod)

plot(log_cbrt_price~log_carat, main = "Log of Cuberoot Price by Log Carat")
abline(t_both_mod, col = 'red')
grid()

plot(t_both_mod$fitted.values, t_both_mod$residuals, main = 'Residual Plot') # residual plot
abline(h=0,col="red")
grid()

acf(t_both_mod$residuals)

boxcox(t_both_mod, lambda = seq(0, .8, by = 0.1)) # Box-Cox plot

# Though this is complicated, we can visually see that the graph is linear. We may want to explore the lag issues on the ACF, though
# We notice that the Box-Cox plot does not show lambda = 1 in our confidence interval, but this could be due to the size of the dataset;
# as n increases, small changes in residual variance become more pronounced and harder to 'pindown' in our Box-Cox plot.

# Note that our first model (transform on price only) had a funneling out on the residual variance even though it had a an ideal Box-Cox

# BUT, we know that log(x^a) = a*log(x), so by taking the log of the cube root, we are simply multiplying each x by 1/3, which
# doesn't matter for our transformation here

# => to simplify, we can use log(price), log(carat)


# quick model ANOVA  and summarr
t_both_mod <- lm(log_price~log_carat)

summary(t_both_mod)
anova(t_both_mod)

# it's clear that carat and price have a linear relationship -- F-val is huge w/ p-val 0.05 >> 2.2e-16
# Initial R^2 is 0.9547


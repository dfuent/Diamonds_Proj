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

diamonds_data <- mutate(diamonds_data, log_carat = log(carat), log_t_price = log(price^(1/3)))

head(diamonds_data)

attach(diamonds_data)

t_both_mod <- lm(log_t_price~log_carat)

summary(t_both_mod)

plot(log_t_price~log_carat, main = "Log of Cuberoot Price by Log Carat")
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

# Note that our first model had a funneling out on the residual variance even though it had a an ideal Box-Cox

# We can use this new model

summary(t_both_mod)
anova(t_both_mod)

# it's clear that carat and price have a linear relationship -- F-val is huge w/ p-val 0.05 >> 2.2e-16

data_t <- diamonds_data[c('log_carat', 'clarity', 'color', 'cut', 'log_t_price')]

head(data_t)

t_full <- lm(log_t_price~., data = data_t)
summary(t_full)
anova(t_full)

unique(cut)
unique(color)
unique(clarity)

# redo scatters

p_cut <- data_t[c('log_t_price', 'cut')]
p_color <- data_t[c('log_t_price', 'color')]
p_clarity <- data_t[c('log_t_price', 'clarity')]


## consider each cut as a subset
AI<-subset(data_t,cut=="Astor Ideal") 
I<-subset(data_t,cut=="Ideal") 
G<-subset(data_t,cut=="Good") 
VG<-subset(data_t,cut=="Very Good") 

# fit separate regressions
price_AI <- lm(log_t_price~log_carat,data=AI)
price_I <- lm(log_t_price~log_carat,data=I)
price_G <- lm(log_t_price~log_carat,data=G)
price_VG <- lm(log_t_price~log_carat,data=VG)

# Astor Ideal is the reference

# Create scatters:

plot(log_carat, log_t_price, main="Price by Carat and Cut")
points(I$log_carat, I$log_t_price, pch=2, col="blue")
points(G$log_carat, G$log_t_price, pch=3, col="orange")
points(VG$log_carat, VG$log_t_price, pch=4, col="red")

abline(price_AI,lty=1)
abline(price_I,lty=2, col="blue") 
abline(price_G,lty=3, col="orange")
abline(price_VG,lty=4, col="red")
legend("topleft", c("Astor Ideal", "Ideal", "Good", "Very Good"), lty=c(1, 2, 3, 4), 
       pch=c(1,2,12), col=c("black", "blue", "orange", "red")) 

#colors()[1:50] # R colors for graphs

# color:
D <-subset(data_t,color=="E") 
E <-subset(data_t,color=="E") 
eF<-subset(data_t,color=="F") # f is a reserved variable
G<-subset(data_t,color=="G") 
H<-subset(data_t,color=="H") 
I<-subset(data_t,color=="I") 
J<-subset(data_t,color=="J") 

# fit separate regressions
price_D <- lm(log_t_price~log_carat,data=D)
price_E <- lm(log_t_price~log_carat,data=E)
price_eF <- lm(log_t_price~log_carat,data=eF)
price_G <- lm(log_t_price~log_carat,data=G)
price_H <- lm(log_t_price~log_carat,data=H)
price_I <- lm(log_t_price~log_carat,data=I)
price_J <- lm(log_t_price~log_carat,data=J)

plot(log_carat, log_t_price, main="Price by Carat and Color")
points(E$log_carat, E$log_t_price, pch=2, col="chartreuse")
points(eF$log_carat, eF$log_t_price, pch=3, col="blue")
points(G$log_carat, G$log_t_price, pch=4, col="orange")
points(H$log_carat, H$log_t_price, pch=5, col="red")
points(I$log_carat, I$log_t_price, pch=6, col="bisque1")
points(J$log_carat, J$log_t_price, pch=7, col="aquamarine")

abline(price_D,lty=1, col = "black")
abline(price_E,lty=2, col = "chartreuse")
abline(price_eF,lty=3, col="blue") 
abline(price_G,lty=4, col="orange")
abline(price_H,lty=5, col="red")
abline(price_I,lty=6, col="bisque1")
abline(price_J,lty=7, col="aquamarine")


legend("topleft", c("D", "E", "F", "G", "H", "I", "J"), lty=c(1, 2, 3, 4, 5, 6, 7), 
       pch=c(1,2,3,4,5,6,7), col=c("black", "chartreuse", "blue", "orange", "red", "bisque1", "aquamarine")) 

# clarity:
FL <-subset(data_t,clarity=="FL") 
IF <-subset(data_t,clarity=="IF") 
SI1 <-subset(data_t,clarity=="SI1") 
SI2 <-subset(data_t,clarity=="SI2") 
VS1 <-subset(data_t,clarity=="VS1") 
VS2 <-subset(data_t,clarity=="VS2") 
VVS1 <-subset(data_t,clarity=="VVS1") 
VVS2 <-subset(data_t,clarity=="VVS2") 

# fit separate regressions
price_FL <- lm(log_t_price~log_carat,data=FL)
price_IF <- lm(log_t_price~log_carat,data=IF)
price_SI1 <- lm(log_t_price~log_carat,data=SI1)
price_SI2 <- lm(log_t_price~log_carat,data=SI2)
price_VS1 <- lm(log_t_price~log_carat,data=VS1)
price_VS2 <- lm(log_t_price~log_carat,data=VS2)
price_VVS1 <- lm(log_t_price~log_carat,data=VVS1)
price_VVS2 <- lm(log_t_price~log_carat,data=VVS2)

plot(log_carat, log_t_price, main="Price by Carat and Clarity")
points(IF$log_carat, IF$log_t_price, pch=2, col="chartreuse")
points(SI1$log_carat, SI1$log_t_price, pch=3, col="blue")
points(SI2$log_carat, SI2$log_t_price, pch=4, col="orange")
points(VS1$log_carat, VS1$log_t_price, pch=5, col="red")
points(VS2$log_carat, VS2$log_t_price, pch=6, col="bisque1")
points(VVS1$log_carat, VVS1$log_t_price, pch=7, col="aquamarine")
points(VVS2$log_carat, VVS2$log_t_price, pch=8, col="burlywood")

abline(price_FL,lty=1, col = "black")
abline(price_IF,lty=2, col = "chartreuse")
abline(price_SI1,lty=3, col="blue") 
abline(price_SI2,lty=4, col="orange")
abline(price_VS1,lty=5, col="red")
abline(price_VS2,lty=6, col="bisque1")
abline(price_VVS1,lty=7, col="aquamarine")
abline(price_VVS2,lty=8, col="burlywood")


legend("topleft", c("FL", "IF", "SI1", "SI2", "VS1", "VS2", "VVS1", "VVS2"), lty=c(1, 2, 3, 4, 5, 6, 7, 8), 
       pch=c(1,2,3,4,5,6,7), col=c("black", "chartreuse", "blue", "orange", "red", "bisque1", "aquamarine", "burlywood")) 


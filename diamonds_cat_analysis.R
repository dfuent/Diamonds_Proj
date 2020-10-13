# STAT 6021
# Proj 1: Diamonds

library(MASS) # for Box-Cox
library(dplyr)

diamonds_data<-read.csv("diamonds4.csv", header=TRUE)
#diamonds_data # display data when uncommented

clarity <- factor(clarity, levels = c('SI2', 'SI1', 'VS2', 'VS1', 'VVS2', 'VVS1', 'IF', 'FL'))
color <- factor(color, levels = c('J', 'I', 'H', 'G', 'F', 'E', 'D'))
cut <- factor(cut, levels = c('Good', 'Very Good', 'Ideal', 'Astor Ideal'))

summary(diamonds_data)
attach(diamonds_data) # attach

diamonds_data
head(diamonds_data)

diamonds_data <- mutate(diamonds_data, log_carat = log(carat), log_t_price = log(price^(1/3)))

diamonds_data <- diamonds_data[c('log_carat', 'clarity', 'color', 'cut', 'log_t_price')]

head(diamonds_data)
attach(diamonds_data)

boxplot(log_carat~cut, main="Boxplot of Carat and Cut")
boxplot(log_carat~color, main="Boxplot of Carat and Color")
boxplot(log_carat~clarity, main="Boxplot of Carat and Clarity")

boxplot(log_t_price~cut, main="Boxplot of Price and Cut")
boxplot(log_t_price~color, main="Boxplot of Price and Cut")
boxplot(log_t_price~clarity, main="Boxplot of Price and Cut")

t_full <- lm(log_t_price~., data = diamonds_data)
summary(t_full)
anova(t_full)

unique(cut)
unique(color)
unique(clarity)

# redo scatters

#p_cut <- data_t[c('log_t_price', 'cut')]
#p_color <- data_t[c('log_t_price', 'color')]
#p_clarity <- data_t[c('log_t_price', 'clarity')]


## consider each cut as a subset
AI<-subset(diamonds_data,cut=="Astor Ideal") 
I<-subset(diamonds_data,cut=="Ideal") 
VG<-subset(diamonds_data,cut=="Very Good") 
G<-subset(diamonds_data,cut=="Good") 


# fit separate regressions
price_AI <- lm(log_t_price~log_carat,data=AI)
price_I <- lm(log_t_price~log_carat,data=I)
price_VG <- lm(log_t_price~log_carat,data=VG)
price_G <- lm(log_t_price~log_carat,data=G)


# Astor Ideal is the reference

# Create scatters:

plot(log_carat, log_t_price, main="Price by Carat and Cut, transformed model")
points(I$log_carat, I$log_t_price, pch=2, col="blue")
points(VG$log_carat, VG$log_t_price, pch=3, col="red")
points(G$log_carat, G$log_t_price, pch=4, col="orange")


abline(price_AI,lty=1)
abline(price_I,lty=2, col="blue") 
abline(price_VG,lty=3, col="red")
abline(price_G,lty=4, col="orange")
legend("topleft", c("Astor Ideal", "Ideal", "Very Good", "Good"), lty=c(1, 2, 3, 4), 
       pch=c(1,2,3, 4), col=c("black", "blue", "red", "orange")) 

cut <- factor(cut)

m_cut <- lm(log_t_price~log_carat + cut, data = diamonds_data)

summary(m_cut)


# Tukey tests
library(lawstat)
library(multcomp)


pairwise<-glht(m_cut, linfct = mcp(cut = "Tukey")) # can group based on cut -> Good and VG, Ideal and Astor Ideal (plus not many of the top kinds in the data)
summary(pairwise)

# group the data based on cut

cut

top <- c('Astor Ideal', 'Ideal')

diamonds_data$cut_group <- ifelse(diamonds_data$cut %in% top, 'Top Cut', 'Bottom Cut')

head(diamonds_data)

## consider each cut as a subset
tc<-subset(diamonds_data,cut_group=="Top Cut") 
sc<-subset(diamonds_data,cut_group=="Bottom Cut") 

head(sc)

# fit separate regressions
price_TopCut <- lm(log_t_price~log_carat,data=tc)
price_SecondCut <- lm(log_t_price~log_carat,data=sc)


# Create scatters:

plot(tc$log_carat, tc$log_t_price, col = 'blue', main="Price by Carat and Cut, transformed model")
points(sc$log_carat, sc$log_t_price, pch=2, col="red")


abline(price_TopCut,lty=1, col = 'blue')
abline(price_SecondCut,lty=2, col="red") 
legend("topleft", c("Top Cut", "Bottom Cut"), lty=c(1, 2), 
       pch=c(1,2), col=c("blue", "red")) 
grid()

is.factor(cut_group)
cut_group <- factor(cut_group, levels = c('Top Cut', 'Bottom Cut'))
levels(cut_group)
is.factor(cut_group)

attach(diamonds_data)

count(diamonds_data, c(cut_group))

m_cut <- lm(log_t_price~log_carat + cut_group)

summary(m_cut)

pairwise<-glht(m_cut, linfct = mcp(cut_group = "Tukey")) 
summary(pairwise)

#colors()[1:50] # R colors for graphs

# color:
D <-subset(diamonds_data,color=="E") 
E <-subset(diamonds_data, color=="E") 
eF<-subset(diamonds_data,color=="F") # f is a reserved variable
G<-subset(diamonds_data,color=="G") 
H<-subset(diamonds_data,color=="H") 
I<-subset(diamonds_data,color=="I") 
J<-subset(diamonds_data,color=="J") 

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

count(diamonds_data, c(color))

best_col <- c('D', 'E', 'F', 'G', 'H')

diamonds_data$col_group <- ifelse(diamonds_data$color %in% best_col, 'Top Col', 'Bottom Col')

# color:
top_col <-subset(diamonds_data,col_group=="Top Col") 
bottom_col <-subset(diamonds_data, col_group=="Bottom Col") 


# fit separate regressions
price_topc <- lm(log_t_price~log_carat,data=top_col)
price_bottomc <- lm(log_t_price~log_carat,data=bottom_col)


plot(log_carat, log_t_price, main="Price by Carat and Color, top and bottom")
points(bottom_col$log_carat, bottom_col$log_t_price, pch=2, col="red")


abline(price_topc,lty=1, col = "black")
abline(price_bottomc,lty=2, col = "red")


legend("topleft", c("Top Colors", "Bottom Colors"), lty=c(1, 2), 
       pch=c(1,2), col=c("black", "red")) 



is.factor(col_group)
col_group <- factor(col_group, levels = c('Top Col', 'Bottom Col'))
levels(col_group)
is.factor(col_group)

attach(diamonds_data)

count(diamonds_data, c(col_group))

m_col <- lm(log_t_price~log_carat + col_group)

summary(m_col)

pairwise<-glht(m_col, linfct = mcp(col_group = "Tukey")) 
summary(pairwise)


# clarity:
FL <-subset(diamonds_data,clarity=="FL") 
IF <-subset(diamonds_data,clarity=="IF") 
VVS1 <-subset(diamonds_data,clarity=="VVS1") 
VVS2 <-subset(diamonds_data,clarity=="VVS2") 
VS1 <-subset(diamonds_data,clarity=="VS1") 
VS2 <-subset(diamonds_data,clarity=="VS2") 
SI1 <-subset(diamonds_data,clarity=="SI1") 
SI2 <-subset(diamonds_data,clarity=="SI2") 

# fit separate regressions
price_FL <- lm(log_t_price~log_carat,data=FL)
price_IF <- lm(log_t_price~log_carat,data=IF)
price_VVS1 <- lm(log_t_price~log_carat,data=VVS1)
price_VVS2 <- lm(log_t_price~log_carat,data=VVS2)
price_VS1 <- lm(log_t_price~log_carat,data=VS1)
price_VS2 <- lm(log_t_price~log_carat,data=VS2)
price_SI1 <- lm(log_t_price~log_carat,data=SI1)
price_SI2 <- lm(log_t_price~log_carat,data=SI2)


plot(log_carat, log_t_price, main="Price by Carat and Clarity")
points(IF$log_carat, IF$log_t_price, pch=2, col="chartreuse")
points(VVS1$log_carat, VVS1$log_t_price, pch=3, col="aquamarine")
points(VVS2$log_carat, VVS2$log_t_price, pch=4, col="burlywood")
points(VS1$log_carat, VS1$log_t_price, pch=5, col="red")
points(VS2$log_carat, VS2$log_t_price, pch=6, col="bisque1")
points(SI1$log_carat, SI1$log_t_price, pch=7, col="blue")
points(SI2$log_carat, SI2$log_t_price, pch=8, col="orange")



abline(price_FL,lty=1, col = "black")
abline(price_IF,lty=2, col = "chartreuse")
abline(price_VVS1,lty=3, col="aquamarine")
abline(price_VVS2,lty=4, col="burlywood")
abline(price_VS1,lty=5, col="red")
abline(price_VS2,lty=6, col="bisque1")
abline(price_SI1,lty=7, col="blue") 
abline(price_SI2,lty=8, col="orange")


legend("topleft", c("FL", "IF", "VVS1", "VVS2", "VS1", "VS2", "SI1", "SI2"), lty=c(1, 2, 3, 4, 5, 6, 7, 8), 
       pch=c(1,2,3,4,5,6,7), col=c("black", "chartreuse", "aquamarine", "burlywood", "red", "bisque1", "blue", "orange")) 


fl_cl <- c('FL', 'IF')
vs_cl <- c('VVS1', 'VVS2', 'VS1', 'VS2')


diamonds_data$cl_group <- ifelse(diamonds_data$clarity %in% fl_cl, 
                                 'Flawless', ifelse(diamonds_data$clarity %in% vs_cl,
                                                    'Very Slightly', 'Slightly'))

count(diamonds_data, c(cl_group))

attach(diamonds_data)
head(diamonds_data)

cl_group <- factor(cl_group, levels = c('Flawless', 'Very Slightly', 'Slightly'))

levels(cl_group)

# clarity:
FL <-subset(diamonds_data,cl_group=="Flawless") 
VS <-subset(diamonds_data,cl_group=="Very Slightly") 
S <-subset(diamonds_data,cl_group=="Slightly")

head(S)


# fit separate regressions
price_fl <- lm(log_t_price~log_carat,data=FL)
price_vs <- lm(log_t_price~log_carat,data=VS)
price_s <- lm(log_t_price~log_carat,data=S)



plot(log_carat, log_t_price, main="Price by Carat and Clarity")
points(VS$log_carat, VS$log_t_price, pch=2, col="red")
points(S$log_carat, S$log_t_price, pch=3, col="blue")


abline(price_fl,lty=1, col = "black")
abline(price_vs,lty=2, col = "red")
abline(price_s,lty=3, col="blue")



legend("topleft", c("Flawless", 'Very Slightly Included', 'Included'), lty=c(1, 2, 3), 
       pch=c(1,2,3), col=c("black", "red", "blue")) 
grid()


m_clarity <- lm(log_t_price~log_carat + cl_group)


pairwise<-glht(m_clarity, linfct = mcp(cl_group= "Tukey"))
summary(pairwise)


boxplot(log_carat~cut_group, main="Boxplot of Carat and Cut")
boxplot(log_carat~col_group, main="Boxplot of Carat and Color")
boxplot(log_carat~cl_group, main="Boxplot of Carat and Clarity")

boxplot(log_t_price~cut_group, main="Boxplot of Price and Cut")
boxplot(log_t_price~col_group, main="Boxplot of Price and Color")
boxplot(log_t_price~cl_group, main="Boxplot of Price and Clarity")
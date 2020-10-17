# STAT 6021
# Proj 1: Diamonds - Categorical Variable Analyses


##### From this line until 21 is redundant when combined with the prior file

library(MASS) # for Box-Cox
library(dplyr)
library(faraway)

diamonds_data<-read.csv("diamonds4.csv", header=TRUE)
#diamonds_data # display data when uncommented

clarity <- factor(clarity, levels = c('SI2', 'SI1', 'VS2', 'VS1', 'VVS2', 'VVS1', 'IF', 'FL'))
color <- factor(color, levels = c('J', 'I', 'H', 'G', 'F', 'E', 'D'))
cut <- factor(cut, levels = c('Good', 'Very Good', 'Ideal', 'Astor Ideal'))

summary(diamonds_data)
attach(diamonds_data) # attach_

##### These lines are redundant when combined with the prior file ^^^

diamonds_data <- mutate(diamonds_data, log_carat = log(carat), log_price = log(price))  # transform the data

diamonds_data <- diamonds_data[c('log_carat', 'clarity', 'color', 'cut', 'log_price')] # remove the untransformed data

attach(diamonds_data)

# Some variance checks
boxplot(log_carat~cut, main="Boxplot of log Carat and Cut")
boxplot(log_carat~color, main="Boxplot of log Carat and Color")
boxplot(log_carat~clarity, main="Boxplot of log Carat and Clarity")

boxplot(log_price~cut, main="Boxplot of log Price and Cut")
boxplot(log_price~color, main="Boxplot of log Price and Color")
boxplot(log_price~clarity, main="Boxplot of log Price and Clarity")

# run a full model to see how it looks (it looks very complicated)
t_full <- lm(log_price~., data = diamonds_data)
summary(t_full) 
anova(t_full)

cut <- factor(cut)

# view the qualitative data to see how the data look
table(clarity, cut)/nrow(diamonds_data) # rel frequency table
table(clarity, color) # frequency table
table(color, cut)

plot(clarity~cut) # frequency plot to check variance; nothing weird
plot(color~cut) 
plot(clarity~color) 

# consider each cut as a subset
AI<-subset(diamonds_data,cut=="Astor Ideal") 
I<-subset(diamonds_data,cut=="Ideal") 
VG<-subset(diamonds_data,cut=="Very Good") 
G<-subset(diamonds_data,cut=="Good") 

# fit separate regressions
price_AI <- lm(log_price~log_carat,data=AI)
price_I <- lm(log_price~log_carat,data=I)
price_VG <- lm(log_price~log_carat,data=VG)
price_G <- lm(log_price~log_carat,data=G)

# Astor Ideal is the reference

# Create scatters:

plot(log_carat, log_price, main="log Price by log Carat and Cut")
points(I$log_carat, I$log_price, pch=2, col="blue")
points(VG$log_carat, VG$log_price, pch=3, col="red")
points(G$log_carat, G$log_price, pch=4, col="orange")

abline(price_AI,lty=1)
abline(price_I,lty=2, col="blue") 
abline(price_VG,lty=3, col="red")
abline(price_G,lty=4, col="orange")
legend("topleft", c("Astor Ideal", "Ideal", "Very Good", "Good"), lty=c(1, 2, 3, 4), 
       pch=c(1,2,3, 4), col=c("black", "blue", "red", "orange")) 

m_cut <- lm(log_price~log_carat + cut, data = diamonds_data)

summary(m_cut)

# Tukey tests
library(lawstat)
library(multcomp)

pairwise<-glht(m_cut, linfct = mcp(cut = "Tukey")) # can group based on cut -> Good and VG, Ideal and Astor Ideal (plus not many of the top kinds in the data)
summary(pairwise)

# group the data based on cut

top <- c('Astor Ideal', 'Ideal') # top cut

diamonds_data$cut_group <- ifelse(diamonds_data$cut %in% top, 'Top Cut', 'Bottom Cut') # bottom cut is very good and good

head(diamonds_data)

## consider each cut as a subset
tc<-subset(diamonds_data,cut_group=="Top Cut") 
sc<-subset(diamonds_data,cut_group=="Bottom Cut") 

# fit separate regressions
price_TopCut <- lm(log_price~log_carat,data=tc)
price_SecondCut <- lm(log_price~log_carat,data=sc)

# Create scatters:
plot(tc$log_carat, tc$log_price, col = 'blue', main="Log Price by Log Carat and Grouped Cut")
points(sc$log_carat, sc$log_price, pch=2, col="red")

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

m_cut <- lm(log_price~log_carat + cut_group)

summary(m_cut)

pairwise<-glht(m_cut, linfct = mcp(cut_group = "Tukey")) 
summary(pairwise)

# color:
D <-subset(diamonds_data,color=="D") 
E <-subset(diamonds_data, color=="E") 
eF<-subset(diamonds_data,color=="F") # f is a reserved variable
G<-subset(diamonds_data,color=="G") 
H<-subset(diamonds_data,color=="H") 
I<-subset(diamonds_data,color=="I") 
J<-subset(diamonds_data,color=="J") 

# fit separate regressions
price_D <- lm(log_price~log_carat,data=D)
price_E <- lm(log_price~log_carat,data=E)
price_eF <- lm(log_price~log_carat,data=eF)
price_G <- lm(log_price~log_carat,data=G)
price_H <- lm(log_price~log_carat,data=H)
price_I <- lm(log_price~log_carat,data=I)
price_J <- lm(log_price~log_carat,data=J)

plot(log_carat, log_price, main="Price by Carat and Color")
points(E$log_carat, E$log_price, pch=2, col="chartreuse")
points(eF$log_carat, eF$log_price, pch=3, col="blue")
points(G$log_carat, G$log_price, pch=4, col="orange")
points(H$log_carat, H$log_price, pch=5, col="red")
points(I$log_carat, I$log_price, pch=6, col="bisque1")
points(J$log_carat, J$log_price, pch=7, col="aquamarine")

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

count(diamonds_data, c(clarity))

best_col <- c('D', 'E', 'F', 'G', 'H') # all but bottom 2

diamonds_data$col_group <- ifelse(diamonds_data$color %in% best_col, 'Top Col', 'Bottom Col')

# color:
top_col <-subset(diamonds_data,col_group=="Top Col") 
bottom_col <-subset(diamonds_data, col_group=="Bottom Col") 

# fit separate regressions
price_topc <- lm(log_price~log_carat,data=top_col)
price_bottomc <- lm(log_price~log_carat,data=bottom_col)

plot(log_carat, log_price, main="log Price by log Carat and Grouped Color")
points(bottom_col$log_carat, bottom_col$log_price, pch=2, col="red")

abline(price_topc,lty=1, col = "black")
abline(price_bottomc,lty=2, col = "red")

legend("topleft", c("Top Colors", "Bottom Colors"), lty=c(1, 2), 
       pch=c(1,2), col=c("black", "red")) 

plot(price_topc$fitted.values, price_topc$residuals, main = 'Residual Plot')
abline(h=0,col="red")
grid()

plot(price_bottomc$fitted.values, price_bottomc$residuals, main = 'Residual Plot')
abline(h=0,col="red")
grid()

is.factor(col_group)
col_group <- factor(col_group, levels = c('Top Col', 'Bottom Col'))
levels(col_group)
is.factor(col_group)

attach(diamonds_data)

count(diamonds_data, c(col_group))

m_col <- lm(log_price~log_carat + col_group)

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
price_FL <- lm(log_price~log_carat,data=FL)
price_IF <- lm(log_price~log_carat,data=IF)
price_VVS1 <- lm(log_price~log_carat,data=VVS1)
price_VVS2 <- lm(log_price~log_carat,data=VVS2)
price_VS1 <- lm(log_price~log_carat,data=VS1)
price_VS2 <- lm(log_price~log_carat,data=VS2)
price_SI1 <- lm(log_price~log_carat,data=SI1)
price_SI2 <- lm(log_price~log_carat,data=SI2)

plot(log_carat, log_price, main="log Price by log Carat and Clarity")
points(IF$log_carat, IF$log_price, pch=2, col="chartreuse")
points(VVS1$log_carat, VVS1$log_price, pch=3, col="aquamarine")
points(VVS2$log_carat, VVS2$log_price, pch=4, col="burlywood")
points(VS1$log_carat, VS1$log_price, pch=5, col="red")
points(VS2$log_carat, VS2$log_price, pch=6, col="bisque1")
points(SI1$log_carat, SI1$log_price, pch=7, col="blue")
points(SI2$log_carat, SI2$log_price, pch=8, col="orange")

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

# Group the clarity variables

#############################

# Three clarity classes
# 
# cl_group <- factor(cl_group, levels = c('Flawless_VVSI', 'Very Slightly', 'Slightly'))
# 
# levels(cl_group)
# 
# # clarity:
# FL <-subset(diamonds_data,cl_group=="Flawless_VVSI")
# VS <-subset(diamonds_data,cl_group=="Very Slightly")
# S <-subset(diamonds_data,cl_group=="Slightly")
# 
# head(FL)
# 
# 
# # fit separate regressions
# price_fl <- lm(log_price~log_carat,data=FL)
# price_vs <- lm(log_price~log_carat,data=VS)
# price_s <- lm(log_price~log_carat,data=S)
# 
# 
# 
# plot(log_carat, log_price, main="log Price by log Carat and Grouped Clarity")
# points(VS$log_carat, VS$log_price, pch=2, col="red")
# points(S$log_carat, S$log_price, pch=3, col="blue")
# 
# 
# abline(price_fl,lty=1, col = "black")
# abline(price_vs,lty=2, col = "red")
# abline(price_s,lty=3, col="blue")
# 
# 
# 
# legend("topleft", c("Flawless_VVSI", 'Very Slightly Included', 'Included'), lty=c(1, 2, 3),
#        pch=c(1,2,3), col=c("black", "red", "blue"))
# grid()
# 
# 
# m_cl <- lm(log_price~log_carat + cl_group + col_group + cut_group)
# pairwise<-glht(m_cl, linfct = mcp(cl_group= "Tukey"))
# summary(pairwise)

########################################

## Redid clarity with two groups to see it significance stayed (two groups were significant in interaction, but not the third w/ three groups)

fl_cl <- c('FL', 'IF', 'VVS1', 'VVS2')
vs_cl <- c('VS1', 'VS2')

diamonds_data$cl_group <- ifelse(diamonds_data$clarity %in% fl_cl,
                                 'Flawless_VVSI', 'Very Slightly')

cl_group <- factor(cl_group, levels = c('Flawless_VVSI', 'Very Slightly'))

levels(cl_group)

table(cl_group)

# clarity:
FL <-subset(diamonds_data,cl_group=="Flawless_VVSI")
VS <-subset(diamonds_data,cl_group=="Very Slightly")
#S <-subset(diamonds_data,cl_group=="Slightly")

head(FL)

# fit separate regressions
price_fl <- lm(log_price~log_carat,data=FL)
price_vs <- lm(log_price~log_carat,data=VS)
#price_s <- lm(log_price~log_carat,data=S)

plot(log_carat, log_price, main="log Price by log Carat and Grouped Clarity")
points(VS$log_carat, VS$log_price, pch=2, col="red")
#points(S$log_carat, S$log_price, pch=3, col="blue")

abline(price_fl,lty=1, col = "black")
abline(price_vs,lty=2, col = "red")
#abline(price_s,lty=3, col="blue")

#write.csv(diamonds_data,"C:\\Users\\fuent\\OneDrive\\Desktop\\Master's Program\\STAT 6021\\Project\\df.csv", row.names = FALSE)
##############################################

plot(cut_group~price) # frequency plot to check variance; nothing weird
plot(col_group~cut_group)
plot(cl_group~col_group)

m_clarity <- lm(log_price~log_carat + cl_group)

pairwise<-glht(m_clarity, linfct = mcp(cl_group= "Tukey"))
summary(pairwise)

boxplot(log_carat~cut_group, main="Boxplot of log Carat and Grouped Cut")
boxplot(log_carat~col_group, main="Boxplot of log Carat and Grouped Color")
boxplot(log_carat~cl_group, main="Boxplot of log Carat and Grouped Clarity")

boxplot(log_price~cut_group, main="Boxplot of log Price and Grouped Cut")
boxplot(log_price~col_group, main="Boxplot of log Price and Grouped Color")
boxplot(log_price~cl_group, main="Boxplot of log Price and Grouped Clarity")

m_clarity <- lm(log_price~log_carat + cl_group)

summary(m_clarity)
summary(m_col)
summary(m_cut)

m_group <- lm(log_price~log_carat + col_group + cl_group + cut_group)
m_group2 <- lm(log_price~log_carat + col_group + cl_group)

m_col_int <- lm(log_price~ log_carat*col_group)
m_simple <- lm(log_price~ log_carat)

m_cl_int <- lm(log_price~ log_carat*cl_group)
m_cut_int <- lm(log_price~ log_carat*cut_group)

summary(m_group)
summary(m_col_int)
summary(m_col)

summary(m_col_int)
summary(m_cl_int)
summary(m_cut_int)

anova(m_col_int, m_simple)
anova(m_col, m_col_int)
anova(m_cut, m_group)
anova(m_col, m_group)
anova(m_col_int, m_group)
anova(m_clarity, m_group)
anova(m_simple, m_group)

anova(m_group2, m_group)

m_group_ <- lm(log_price~log_carat*col_group)
summary(m_group_)

m_group2 <- lm(log_price~log_carat*col_group + log_carat*cl_group + col_group*cl_group + log_carat*cut_group)
summary(m_group2)

group_full <- lm(log_price~log_carat*col_group + log_carat*cl_group + col_group*cl_group + log_carat*cut_group + col_group*cut_group + cl_group*cut_group)
summary(group_full)

reduced <- lm(log_price~log_carat*cl_group + log_carat*cut_group + log_carat*col_group)
summary(reduced)
anova(reduced)

####FINAL MODEL CREATION####
diamonds_data$cl_group <- relevel(cl_group, ref = 'Very Slightly')
levels(cl_group)

full_int <- lm(log_price~log_carat*cut_group + log_carat*cl_group + log_carat*col_group, data = diamonds_data)
summary(full_int)
anova(full_int)

levels(cl_group)

reduced <- lm(log_price~log_carat + cut_group + cl_group + log_carat*col_group, data = diamonds_data)
summary(reduced)
anova(reduced)

no_int <- lm(log_price~log_carat + cut_group + cl_group, data = diamonds_data)
summary(no_int)
anova(no_int)

anova(no_int, reduced)

vif(reduced)
vif(no_int)
vif(full_int)
#write.csv(diamonds_data,"C:\\Users\\fuent\\OneDrive\\Desktop\\Master's Program\\STAT 6021\\Project\\df.csv", row.names = FALSE)

######

anova(reduced, group_full)

boxplot(log_price~cl_group*col_group)
boxplot(log_price~cut_group*col_group)
boxplot(log_price~cl_group*cut_group)
boxplot(log_price~cl_group*col_group*cut)

m_group2_red <- lm(log_price~log_carat*col_group + log_carat*cl_group + col_group*cl_group)
summary(m_group2_red)

anova(m_group2_red, m_group2)

m_group2_red <- lm(log_price~col_group*log_carat + cl_group*log_carat) #+ col_group*cl_group
summary(m_group2_red)

anova(m_group2_red, m_group2)

m_group_full_int <- lm(log_price~log_carat*col_group + log_carat*cl_group + log_carat*cut_group)
summary(m_group_full_int)
anova(m_group_full_int)
m_group_red <- lm(log_price~log_carat + col_group + cl_group)
summary(m_group_red)
anova(m_group_red, m_group_full)

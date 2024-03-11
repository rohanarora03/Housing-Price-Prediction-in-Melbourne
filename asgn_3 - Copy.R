# Use linear regression when outcome variable is a continuous variable
# Logistic regression is used when outcome variable is a bianry variable

library(tidyverse)
library(ggplot2)
library(gridExtra)

melb_data_orig <- 
  read_csv("C:/Users/rohan/OneDrive/Desktop/Winter 2023/MSCI 718 (Statistics for Data Analytics) - MC 4020/Assignments/Assignment 3/melb_data.csv")

str(melb_data_orig)
view(melb_data_orig)
glimpse(melb_data_orig)
summary(melb_data_orig)
cor(melb_data)

#sapply can also be used to find out NA values in all columns
sum(is.na(melb_data))

nrow(melb_data)

####
# outcome variable -> Price
# predictor variable -> 
# as part of cleaning the dataset, we can select the below required columns only and drop the non-required ones from our dataset
# dropping variables :- SellerG ; Suburb ; Address ; Date ; Postcode ; Bedroom2 ; BuildingArea ; YearBuilt ; CouncilArea ; Lattitude ; Longtitude ; Car
# selected variables :- Method ; Rooms ; Type ; Price ; Distance ; Bathroom ; Landsize ; Regionname ; Propertycount
# Dummy encoding/ One-hot encoding for categorical variables
#
# DATA CLEANING :-
# select columns
# Remove NA's in cars column
# factor() to input the categorical variables
# plot and check normality, etc.. and select your predictor variables accordingly
#
# Data Transformation :-
# Price -> did log transformation then Q-Q line showed normality
# Distance -> Removed outliers then sqrt() 
# Propertycount -> sqrt()
# Landsize -> log()
#
# Check assumptions of MLR before and after creating the model
# find correlation b/w each predictor variable with external variables
# plot residuals and check Q-Q plot of residuals should display normality
# Stepwise regression for choosing the variables?

summary(melb_data)

# Q-Q plot on log(Price)
melb_data %>% 
  mutate(Price.log = log(Price)) %>% 
  ggplot(aes(sample=Price.log)) + stat_qq() + geom_qq_line(aes(color="red")) + theme(legend.position = "none")

# adding new variable in melb_data
melb_data <- melb_data %>% 
  mutate(Price.log = log(Price))

# Log transformation made our 'Price' data normal
melb_data_log

melb_data$Price.log

ggplot(melb_data, aes(y=melb_data$Price, x=melb_data$Distance)) +
  geom_point()

melb_data %>%
  group_by(Suburb) %>%
  ggplot(aes(y=Price, x=Suburb)) +
  geom_point() +
  labs(y="Price (in dollars)", x="Suburb")

melb_data$Propertycount
str(melb_data)
summary(melb_data)

####

ggpairs(melb_data)


####
# Price
####
summary(melb_data$Price)
melb_data %>% 
  ggplot(aes(Price)) + geom_boxplot()
melb_data %>% 
  ggplot(aes(sample=Price))+stat_qq()+geom_qq_line(aes(color="red"))+theme(legend.position = "none")+labs(x="X-axis", y="Y-axis")

quantile(melb_data$Price,probs = c(0,0.25,0.5,0.75,1))
Q1<-quantile(melb_data$Price,0.25)
Q1

Q3<-quantile(melb_data$Price,0.75)
Q3
IQR<-IQR(melb_data$Price)
IQR
left <- (Q1-(1.5*IQR))
right <- (Q3+(1.5*IQR))

Range <-c(left,right)
Range
#out<-boxplot.stats(melb_data$Price)$out
print(right)
print(left)

melb_data_price <- filter(melb_data, Price<=right & Price>=left)
nrow(melb_data_price)

melb_data_price %>% 
  ggplot(aes(Price)) + geom_boxplot()

# Price

melb_data_price %>%
  ggplot(aes(x=seq(Price), y=Price))+geom_point()

melb_data_price %>% 
  ggplot(aes(Price))+geom_histogram()

melb_data_price %>% 
  ggplot(aes(Price))+geom_boxplot()

#plotting curve on the histogram
melb_data_price %>% 
  ggplot(aes(x=Price)) + theme(legend.position = "none") +
  geom_histogram(aes(y=..density..)) + 
  stat_function(fun=dnorm, args=list(mean=mean(melb_data_price$Price, na.rm=TRUE), sd=sd(melb_data_price$Price, na.rm=TRUE)))

# Q-Q plot
melb_data_price %>% 
  ggplot(aes(sample=Price))+stat_qq()

# Price -> We see positive skewness so we'll have to do transformation of data
melb_data_price %>% 
  ggplot(aes(sample=Price))+stat_qq()+geom_qq_line(aes(color="red"))+theme(legend.position = "none")+labs(x="X-axis", y="Y-axis")

# Q-Q plot on log(Price)
melb_data_price %>% 
  mutate(Price.log = log(Price)) %>% 
  ggplot(aes(sample=Price.log)) + stat_qq() + geom_qq_line(aes(color="red")) + theme(legend.position = "none")


####
# Propertycount
####
summary(melb_data$Propertycount)
melb_data %>% 
  ggplot(aes(Propertycount)) + geom_boxplot()
melb_data %>% 
  ggplot(aes(sample=Propertycount))+stat_qq()+geom_qq_line(aes(color="red"))+theme(legend.position = "none")+labs(x="X-axis", y="Y-axis")

quantile(melb_data$Propertycount,probs = c(0,0.25,0.5,0.75,1))
Q1<-quantile(melb_data$Propertycount,0.25)
Q1

Q3<-quantile(melb_data$Propertycount,0.75)
Q3
IQR<-IQR(melb_data$Propertycount)
IQR
left <- (Q1-(1.5*IQR))
right <- (Q3+(1.5*IQR))

Range <-c(left,right)
Range
#out<-boxplot.stats(melb_data$Propertycount)$out
print(right)
print(left)

melb_data_prop <- filter(melb_data, Propertycount<=right & Propertycount>=left)
nrow(melb_data_prop)

melb_data_prop %>% 
  ggplot(aes(Propertycount)) + geom_boxplot()

# Propertycount

melb_data_prop %>%
  ggplot(aes(x=seq(Propertycount), y=Propertycount))+geom_point()

melb_data_prop %>% 
  ggplot(aes(Propertycount))+geom_histogram()

melb_data_prop %>% 
  ggplot(aes(Propertycount))+geom_boxplot()

#plotting curve on the histogram
melb_data_prop %>% 
  ggplot(aes(x=Propertycount)) + theme(legend.position = "none") +
  geom_histogram(aes(y=..density..)) + 
  stat_function(fun=dnorm, args=list(mean=mean(melb_data_prop$Propertycount, na.rm=TRUE), sd=sd(melb_data_prop$Propertycount, na.rm=TRUE)))

# Q-Q plot
melb_data_prop %>% 
  ggplot(aes(sample=Propertycount))+stat_qq()

# Price -> We see positive skewness so we'll have to do transformation of data
melb_data_prop %>% 
  ggplot(aes(sample=Propertycount))+stat_qq()+geom_qq_line(aes(color="red"))+theme(legend.position = "none")+labs(x="X-axis", y="Y-axis")

# Q-Q plot on sqrt(Propertycount)
melb_data_prop %>% 
  mutate(Propertycount.sqrt = sqrt(Propertycount)) %>% 
  ggplot(aes(sample=Propertycount.sqrt)) + stat_qq() + geom_qq_line(aes(color="red")) + theme(legend.position = "none")


####
# Distance
####
summary(melb_data$Distance)
melb_data %>% 
  ggplot(aes(Distance)) + geom_boxplot()
melb_data %>% 
  ggplot(aes(sample=Distance))+stat_qq()+geom_qq_line(aes(color="red"))+theme(legend.position = "none")+labs(x="X-axis", y="Y-axis")

quantile(melb_data$Distance,probs = c(0,0.25,0.5,0.75,1))
Q1<-quantile(melb_data$Distance,0.25)
Q1

Q3<-quantile(melb_data$Distance,0.75)
Q3
IQR<-IQR(melb_data$Distance)
IQR
left <- (Q1-(1.5*IQR))
right <- (Q3+(1.5*IQR))

Range <-c(left,right)
Range
#out<-boxplot.stats(melb_data$Distance)$out
print(right)
print(left)

melb_data_dist <- filter(melb_data, Distance<=right & Distance>=left)
nrow(melb_data_dist)

melb_data_dist %>% 
  ggplot(aes(Distance)) + geom_boxplot()

# Distance

melb_data_dist %>%
  ggplot(aes(x=seq(Distance), y=Distance))+geom_point()

melb_data_dist %>% 
  ggplot(aes(Distance))+geom_histogram()

melb_data_dist %>% 
  ggplot(aes(Distance))+geom_boxplot()

#plotting curve on the histogram
melb_data_dist %>% 
  ggplot(aes(x=Distance)) + theme(legend.position = "none") +
  geom_histogram(aes(y=..density..)) + 
  stat_function(fun=dnorm, args=list(mean=mean(melb_data_dist$Distance, na.rm=TRUE), sd=sd(melb_data_dist$Distance, na.rm=TRUE)))

# Q-Q plot
melb_data_dist %>% 
  ggplot(aes(sample=Distance))+stat_qq()

# Price -> We see positive skewness so we'll have to do transformation of data
melb_data_dist %>% 
  ggplot(aes(sample=Distance))+stat_qq()+geom_qq_line(aes(color="red"))+theme(legend.position = "none")+labs(x="X-axis", y="Y-axis")

# Q-Q plot on sqrt(Distance)
melb_data_dist %>% 
  mutate(Distance.sqrt = sqrt(Distance)) %>% 
  ggplot(aes(sample=Distance.sqrt)) + stat_qq() + geom_qq_line(aes(color="red")) + theme(legend.position = "none")


#####
# Landsize
#####

summary(melb_data$Landsize)
melb_data %>% 
  ggplot(aes(Landsize)) + geom_boxplot()
melb_data %>% 
  ggplot(aes(sample=Landsize))+stat_qq()+geom_qq_line(aes(color="red"))+theme(legend.position = "none")+labs(x="X-axis", y="Y-axis")

quantile(melb_data$Landsize,probs = c(0,0.25,0.5,0.75,1))
Q1<-quantile(melb_data$Landsize,0.25)
Q1

Q3<-quantile(melb_data$Landsize,0.75)
Q3
IQR<-IQR(melb_data$Landsize)
IQR
left <- (Q1-(1.5*IQR))
right <- (Q3+(1.5*IQR))

Range <-c(left,right)
Range
#out<-boxplot.stats(melb_data$Landsize)$out
print(right)
print(left)

melb_data_land <- filter(melb_data, Landsize<=right & Landsize>=left)
nrow(melb_data_land)

melb_data_land %>% 
  ggplot(aes(Landsize)) + geom_boxplot()

arrange(melb_data_land, Landsize) %>%
  select(Landsize)

# Landsize

melb_data %>%
  select("Landsize")
sort(melb_data, desc(melb_data$Landsize))
arrange(melb_data, desc(Landsize))

melb_data_land <- filter(melb_data, Landsize<200000)

melb_data_land %>%
  ggplot(aes(x=seq(Landsize), y=Landsize))+geom_point()

melb_data_land %>% 
  ggplot(aes(Landsize))+geom_histogram()

melb_data_land %>% 
  ggplot(aes(Landsize))+geom_boxplot()

#plotting curve on the histogram
melb_data_land %>% 
  ggplot(aes(x=Landsize)) + theme(legend.position = "none") +
  geom_histogram(aes(y=..density..)) + 
  stat_function(fun=dnorm, args=list(mean=mean(melb_data_land$Landsize, na.rm=TRUE), sd=sd(melb_data_land$Landsize, na.rm=TRUE)))

# Q-Q plot
melb_data_land %>% 
  ggplot(aes(sample=Landsize))+stat_qq()

# Price -> We see positive skewness so we'll have to do transformation of data
melb_data_land %>% 
  ggplot(aes(sample=Landsize))+stat_qq()+geom_qq_line(aes(color="red"))+theme(legend.position = "none")+labs(x="X-axis", y="Y-axis")

# Q-Q plot on log(Landsize)
melb_data_land %>% 
  mutate(Landsize.log = log(Landsize)) %>% 
  ggplot(aes(sample=Landsize.log)) + stat_qq() + geom_qq_line(aes(color="red")) + theme(legend.position = "none")


########
# MLR
########
melb_data.model <- lm(Price ~ Distance+Propertycount+Landsize+Rooms+factor(Method)+factor(Regionname), data = melb_data)
summary(melb_data.model)
summary(melb_data)
str(melb_data)
melb_data.model1 <- lm(Price ~ Distance+Propertycount, data = melb_data)
melb_data.model2 <- lm(Price ~ Distance+Propertycount+Landsize, data = melb_data)
summary(melb_data.model1)
summary(melb_data.model2)

ggPredict(melb_data.model, se=TRUE)

melb_data.model$residuals

# Test of independence - Autocorrelation
durbinWatsonTest(melb_data.model)

#The Durbin-Watson test for independent errors was significant at the 5% level of significance (d=1.67, p=0). 
#As d is a little away from 2, it shows some degree of positive autocorrelation; it means the model could
#be fine tuned, but since D-W statistic is between 1 and 3, we fail to reject the null hypothesis 
#that the errors are independent, and continue with the assumption of independence met.

plot(melb_data.model)

#Residuals look homoscedastic and linear
#QQ-plot looks normal - great!

#####
# Variable selection using all-subsets method 
# All-subsets methods simply construct all possible combinations of predictors and compare their 
# explanatory power (e.g., adj R2 or AIC)
#####

install.packages("leaps")
library(leaps)
melb_data.leaps <- regsubsets(Price ~ Distance + Propertycount + Landsize, data=melb_data)
plot(melb_data.leaps, scale = "adjr2")
summary(melb_data.leaps)

#####
# Multicollinearity
#####
ggplot(melb_data, aes(y=Landsize, x=Rooms)) + geom_point();
ggplot(melb_data, aes(y=Landsize, x=Propertycount)) + geom_point();
ggplot(melb_data, aes(y=Distance, x=Propertycount)) + geom_point();
ggplot(melb_data, aes(y=Landsize, x=Distance)) + geom_point();

# No perfect multicollinearity (predictor variables should not correlate highly)-visual inspection looked good

#VIF
install.packages("car")
library(car)

vif(melb_data.model)
mean(vif(melb_data.model))
1/vif(melb_data.model)

# We inspected the VIF (Variance Inflation Factor) to investigate multicollinearity. 
# The largest VIF was 2.066829, less than 10; the average vif was 1.658504, close to 1. 
# The lowest tolerance (1/VIF) was 0.4838330, much greater than 0.1 (which would indicate a serious problem) and 0.2 (which indicates a potential problem).
# We thus conclude that there is no collinearity in our data.

# Outliers in residuals

melb_data$fitted <- melb_data.model$fitted
melb_data$residuals <- melb_data.model$residuals
melb_data$standardized.residuals <- rstandard(melb_data.model)

possible.outliers <- subset(melb_data, standardized.residuals < -1.96 | standardized.residuals > 1.96)
possible.outliers # 562
nrow(melb_data) # 13580
# 562/13580 = 4.13%
# We found 562 residuals are above or below 1.96 standard deviations. 
# As this represents 6.5% of the observations, expected if the residuals 
# are normal (5% of data is expected to be outside of 2 standard deviations),
# we do not consider any of these observations as outliers and continued 
# with all 200 observations included in the model.

# Influential cases
# Cook’s distances is greater than 1 are a cause for concern, and you may want to consider removing them

melb_data$cooks <- cooks.distance(melb_data.model)
plot(sort(melb_data$cooks, decreasing=TRUE))
max(melb_data$cooks)
# 5.154375

#To investigate influential cases, we calculated Cook’s Distance on the developed model. 
#Cook’s distance was a maximum of 0.0707659, far below the chosen cutoff value of 1. 
#We thus conclude that there are no influential cases.


####
# Comparison among 2 models - ANOVA
# We'd want to compare two models to see if they are significantly different
# Model 1 -> Distance + Propertycount
# Model 2 -> Distance + Propertycount + Landsize
anova(melb_data.model1, melb_data.model2)

# Result: p-value < 0.05(1.029e-06) means we reject H0 thus imply that there is significant difference
# among the 2 models


# AIC
# The model with the lowest AIC value is always listed first. From the output we can see that the 
#following model has the lowest AIC value and is thus the best fitting model:
#mpg = β0 + β1(disp) + β2(hp) + β3(wt) + β4(qsec)

install.packages("AICcmodavg")
library(AICcmodavg)
models <- list(melb_data.model1, melb_data.model2)
mod.names <- c('model1', 'model2')
aictab(cand.set = models, modnames = mod.names)


# confidence intervals of the model
confint(melb_data.model)

# Interpret the model
summary(melb_data.model)

#All three predictor variables have an influence on albums sales at the 5% level of significance: 
#adverts (t(196)=12.261, p < 2e-16), airplay (t(196)=12.123, p < 2e-16), and 
#attract (t(196)=4.548, p < 9.49e-06). The intercept is not significantly different from 0
#(t(196) = -1.534, p=0.127). R^2 is 0.6647, and adjusted R^2 is 0.6595. 
#Coefficients and 95% confidence intervals are listed below.

#######
#Conclusion
########
#We built a linear model predicting albums sales from money spent on advertisement, songs played before 
#album release, and rated attractiveness of the band. 
#Assumptions of the linear model were all met.

#From this model, we conclude that advertising, song airplay, and attractiveness of the band explain 66% of 
#the variance in album sales. In addition, we can predict the influence of these variables on future
#album sales - for example, if nothing else is changed, playing a additional song from an album increases
#albums sold by 2.82-3.92 (19 times out of 20).
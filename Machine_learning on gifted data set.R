### Some packages
install.packages("corrplot")
install.packages("pastecs")

#### Loading the libraries
library(ctv)
library(pastecs)
library(ggplot2)
library(corrplot)

###### Load and describe the data set
gift=read.csv('gifted.csv')
head(gift)
tail(gift)
colnames(gift)

###### Upper triangular scatter plot matrix to find the correlation between score and other variables.
pairs(gift,lower.panel=NULL)

##### Correlation matrix and corrplot that shows the contribution of different variables to the scores of a child.
cor=cor(gift)
cor
corrplot(cor(gift))# mother,count and read are high correlated to score

#### Histogram of the response (score) with frequency
hist(gift$score,col='green',xlab = 'score',main="Histogram of score as Y")
shapiro.test(gift$score)

########## Simple linear regression Model between score and any other one variable
model=lm(score~motheriq,data=gift)
summary(model)

####### Residuals of the results from simple linear regression
par(mfrow=c(2,2))
plot(model,which = 1,main = "linearity",col="blue")
plot(model,which=2,main = "normality",col="blue")
plot(model,which=3,main = "homoskedasticity",col="blue")
plot(model,which=4,main="outliers",col="blue")

##### Multiple Linear regression Model
multi.mod=lm(score~., data=gift)
summary(multi.mod)

####### Residuals of the results from Multiple linear regression
plot(multi.mod,which = 1,main = "linearity",col="blue")
plot(multi.mod,which=2,main = "normality",col="blue")
plot(multi.mod,which=3,main = "homoskedasticity",col="blue")
plot(multi.mod,which=4,main="outliers",col="blue")
     

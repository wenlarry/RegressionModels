#Quiz 1

#Q1
x<-c(0.18,-1.54,0.42,.095)
w<-c(2,1,3,2)
sum(x*w)/sum(w) 
#The value of the mean minimizes the function

#Q2
x<-c(0.8,0.47,0.51,0.73,0.36,0.58,0.57,0.85,0.44,0.42)
y<-c(1.39,0.72,1.55,0.48,1.19,-1.59,1.23,-0.65,1.49,0.05) 
lm(y~x-1) 
#fit the regression thru the origin and get the slope 
#treating y as the outcome and x as the regressor
#add -1 to the regression formula to fit thru the origin

#Q3
data(mtcars)
cars<-lm(mpg~wt,mtcars) 
cars$coefficients[2] 
#fit the regression model with mpg as outcome and wt the
#predictor.Get the slope coefficient   

#Q4
#cov(x,y)=cor(x,y)*s(x)*s(y)
#cor(x,y)=0.05
#s(x)=0.5s(y)
#slope:cov(x,y)/(s(x)^2)=2*cor(x,y)*(s(x)^2)/s(x)^2)=0.25
# the sd of the predictor is 0.5 of outcome. 
#The correlation btw the 2 variables is 0.5. Get the
#slope coefficient

#Q5
1.5*0.4
#2 tests and scores nomarlized with mean =0 and var =1.
#Correlation btw the scores on 2 tests is 0.4.Get the 
#expected score on Quiz 2 for a score of 1.5 on Quiz 1.

#Q6
x<-c(8.58,10.46,9.01,9.64,8.86)
mean(x)/sd(x)  
#value of the first measurement if x normalized

#Q7
x<-c(0.8,0.47,0.51,0.73,0.36,0.58,0.57,0.85,0.44,0.42)
y<-c(1.39,0.72,1.55,0.48,1.19,-1.59,1.23,-0.65,1.49,0.05) 
lm(y~x)$coefficients
# the intercept for fitting the model with x as the
#predictor and y the outcome

#Q9
x<-c(0.8,0.47,0.51,0.73,0.36,0.58,0.57,0.85,0.44,0.42)
mean(x) 
# the value that minimizes the squared distances should be
#the mean

#Quiz 2

#Q1 & 2
x<-c(0.61,0.93,0.83,0.35,0.54,0.16,0.91,0.62,0.62)
y<-c(0.67,0.84,0.6,0.18,0.85,0.47,1.1,0.65,0.36) 
p<-lm(y~x) 
summary(p) 
#Get a p value for a 2 sided hypothesis test on whether
#beta 1 from a linear regression model is ) or not.
# Get the estimate of the residual standard deviation

#Q3
data(mtcars)
x<-mtcars$wt
y<-mtcars$mpg
fit<-lm(y~x)
predict(fit,data.frame(x=mean(x)),interval="confidence")
# fit a linear regression model of wt and mpg (outcome). 
#Get a 95% confidence interval for the expected mog at the
#average wt and the lower endpoint.

#Q4
?mtcars
# wt is 1000 lbs

#Q5
predict (fit,data.frame(x=mean(3)),interval="prediction") 
# mtcars dataset.a linear regression model with mpg as 
#predicted by wt(1000 lbs) . A new car(3000 lbs).Get a 95%
# prediction interval for its mpg and the upper endpoint

#Q6
fit2<-lm(y~I(x/2)) 
summary(fit2)$coef[2,1]-2*summary(fit2)$coef[2,2]

# mtcars dataset. a linear regression model with mpg as 
#predicted by wt (1000lbs). A short ton is 2000 lbs. Get a 
#95% confidence interval for the expected change in mpg per 
#1 short ton increase in wt and the lower endpoint

#Q7
summary(fit)$coefficients[2,1]

fit3<-lm(y~I(x/100))
summary(fit3)$coefficients[2,1] 

#Q8
#A outcome Y and predictor X and fit a linear regression
#model (Y=beta) + beta1X + c to obtain beta0 and beta1.
#What is the consequence to the subsequent slope and 
#intercept if the model is refitted with a new regressor
#X + c for some constant c ? the new intercept is 
#beta hat 0-c*beta hat1
#beta 0 = intercept; beta 1= slope; changing X value by
#+ c changes the intercept but not the slope

#Q9

fit0<-lm(data=mtcars,mpg~1)
fit1<-lm(data=mtcars,mpg~wt)
sqe0<-sum((mtcars$mpg-predict(fit0))^2) 
sqe1<-sum((mtcars$mpg-predict(fit1))^2)  
sqe1/sqe0

#mtcars dataset. mpg(outcome) and wt (predictor). What is
#the ratio of the sum of square^d errors (Yi -Yihat)^2
#when comparing a model with just an intercept 
#(denominator) to the model with the intercept and 
#slope (numerator) 
#Ask for residual variability/total variability 

#Quiz 3

#Q1
data(mtcars)
fit<-lm(mpg~as.factor(cyl)+wt,mtcars) 
summary(fit) 
#A model with mpg (outcome) that includes # of cyl as a
#factor variable and wt (cofounder).Give the adjusted
#estimate for the expected change in mpg comparing 8 cyl
#to 4 cyl

#Q2
fitAdj<-lm(mpg~as.factor(cyl) + wt,mtcars) 
fitNadj<-lm(mpg~as.factor(cyl),mtcars) 
summary(fitAdj)
summary(fitNadj) 
#Including wt attenuates the effect of cyl on mpg

#Q3
fitAdj<-lm(mpg~as.factor(cyl) + wt,mtcars) 
fitInt<-lm(mpg~as.factor(cyl)*wt,mtcars) 
anova(fitAdj,fitInt) 
# A model with mpg (outcome) that includes # of cyl as a
#factor variable and wt (cofounder). Then fit a second 
#model that considers interaction bte cyl as a factor 
#variable and wt. Give the p_value for a ratio test 
#comparing the 2 models and suggest a model using
#005 as a type 1 error rate significance.
# The p_value is larger than 0.05.Therefor fail to reject
#that means that the interaction term may not be necessary.

#Q4
lm(mpg~I(wt*0.5)+factor(cyl),mtcars) 
# A model with mpg (outcome) that includes cyl as a 
#variable and wt included.
# Wt coefficient is interpreted as tht estimated change in 
#mpg per one ton increase in wt for a specificed cy (4,6,8)

#Q5& Q6
x<-c(0.586,0.166,-0.042,-0.614,11.72) 
y<-c(0.549,-0.026,-0.127,-0.751,1.344) 
fit<-lm(y~x) 
plot(x,y) 
points(x,predict(fit)) 
hatvalues(fit) 
# 0.9946 - the most influential hat diagonal.
dfbetas(fit) 
# -134 - the slope dfbeta for the point with the 
#highest hat value
useX~factor()
#7
# Consider a regression btw X and Y with/without
#adjustment for a third variable Z. Compare the regression
#coefficient btw Y and X with/without adj for Z. It is 
#possible for the coefficent to reverse sign after adj

#Quiz 4

#Q 1
library(MASS)
summary(shuttle) 
useX<-as.numeric(shuttle$use=='auto') 
windX<-as.numeric(shuttle$wind=='head') 
fit<-glm(useX~factor(wind)-1,binomial,shuttle) 
summary(fit)$coef
#Data from shuttle in MASS. Model the use (autolander) as
# the outcome. fit a logistic regression model with var 
#(auto). Not use labeled as auto 1 vs auto 0.Predicted
#by wind variable. Get the est odd ratio for autolander
#use comparing head winds (head var). Numerator (headwind) and
#deominator (tailwind).

#Q2
fit<-glm(useX~factor(wind) + factor(magn)-1,binomial,
         shuttle) 
summary(fit)
exp(coef(fit)) 
exp(cbind(odds=coef(fit),confint(fit)))
1.4384/1.4852
#1.286/1.327-exact answer
# give the estimated odds for autolander use comparing 
#head winds (numerator) to tail wind (denominator) 
#adjusting for wind strength from the var magn.

#Q3
#Fit a logistic regression model to a binary var, eg use 
#autolander, then fit a logistic regression model for one
#minus the outcome (not using autolander), what's the 
#coefficient ? The coef reverse their signs.

#Q4
fit<-glm(count~factor(spray)-1,poisson,InsectSprays) 
summary(fit)$coef       
exp(coef(fit))       
14.5/15.3333        
# fit a poission model to InsectSprays (data).Compare spray A
#(numerator) to spray B (denominator) .This is the est 
#relative rate

#Q5
fit<-glm(count~factor(spray),poisson,InsectSprays,
offset=log(count+1) )
summary (fit) $coef

fit2<-glm(count~factor(spray),poisson,InsectSprays,
offset=log(10)+log(count+1)) 
summary(fit2) $coef

#A poisson glm with an offset,t .eg a model (glm(count~x+offset(t)
#poisson), where x is a factor varible comparing a 
#treatment (1) to a control (0) and t is the natural log 
#of a monitoring time. What is the impact of the coefficient
#for x if we fit the model glm(count~x +offset(t2)
#,poission) where t2<-log(10)+t?
#In other words, what happens to the coefficient if we 
#change the units of the offset variable (adding log(10)
#on the log scale is multiplting by 10 on the original
#sacle)

#Q6
x<--5:5
y<-c(5.12,3.93,2.67,1.87,0.52,0.08,0.93,2.05,2.54,3.87,4.97)
knots<-c(0) 
spline<-sapply(knots, function(knot)(x>knot)*(x-knot)) 
xmat<-cbind(1,x,spline) 
fit3<-lm(y~xmat-1) 
yhat<-predict(fit3) 
summary(fit3)$coef
(yhat[10]-yhat[6])/4
# Use a knot point (0), fit a linear model that looks 
#a hockey stick with 2 lines meeting at x=0. Include an
#intercept x,and the knot point. Get the estimated slope of
#the line after 0



#Calculation of PK parameters.
#Pharmacokinetic parameters give an overall indication of the behavior of the drug in the body.

#Single intravenous bolus dose model
#c(t)=C0e^-K1t
#Ct=C0*exp(1)^(-K1*t)
#K1 elimination constant rate=CL/Vd
#C0=DOSE/ Vd

rm(list=ls()) 


#Assign Parameters
K1=0.119
C0=149

#Create a function
f=function(t){C0*exp(1)^(-K1*t)}

#Plot the function 
plot(f,0,100)

#Dependent variable, plasma concentration
plasma_c=f(0:100)
plasma_c

#Time
time_h=c(0:100)

ivda<-data.frame(cbind(time_h,plasma_c))
ivda

plot(ivda$plasma_c~ivda$time_h)

#Add variability
factor<-rnorm(n=101, mean=15, sd=9)
plot(factor)
ivda$plasma_c=ivda$plasma_c+factor
plot(ivda$plasma_c~ivda$time_h)

#Local Regression using lowess
#LOWESS(Locally Weighted scatterplot smoothing)
lines(lowess(ivda$plasma_c~ivda$time_h, f=0.7),col=5, lwd=3)
lines(lowess(ivda$plasma_c~ivda$time_h, f=0.5),col=4, lwd=3)
lines(lowess(ivda$plasma_c~ivda$time_h, f=0.3),col=3, lwd=3)
lines(lowess(ivda$plasma_c~ivda$time_h, f=0.2),col=2, lwd=3)


#Plor the data.
plot(ivda$plasma_c~ivda$time_h)

#Apply Linear Regression
M1<-lm(ivda$plasma_c~ivda$time_h)
M1
plot(M1)
plot(ivda$plasma_c~ivda$time_h)
lines(ivda$time_h,fitted(M1),lwd=3,col=4)

#No-Linear Regression
M2<-nls(formula=plasma_c~C0*exp(1)^(-K1*time_h),data=ivda,start = list(C0=1,K1=0.1),trace = T)
M2
lines(ivda$time_h,fitted(M2),lwd=3,col=4)

#Which Models fits best?
#Akaike's An Information Criterion.
#The Akaike information criterion (AIC) is an estimator 
#of the relative quality of statistical models for a given set of data.
BIC(M1)
BIC(M2)

summary(M1)
summary(M2)

#Final model plasma concentration.
#*********** Plasma_C = 142.4*e^(-0.07417*t) ****************


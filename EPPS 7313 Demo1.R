library(foreign)
library(tidyr)
library(dplyr)
install.packages("tidyverse")
install.packages("ggplot2")
library(ggplot2)
library(tidyverse)
setwd("C:/Users/19728/Desktop/EPPS 7313")

gss<-read.dta("GSS2012.dta")
gss<-as.data.frame(gss)
sex<-gss$sex
tvhours<-gss$tvhours
rhhwork<-gss$rhhwork
sphhwork<-gss$sphhwork
class<-gss$class
marital<-gss$marital


#t-test assuming equal variance
boxplot(tvhours ~ sex)
t.test(tvhours~sex, var.equal=TRUE)
t.test(tvhours[sex == "female"] and tvhours[sex == "male"])
#unequal variances
t.test(tvhours~sex, var.equal=FALSE)

#paired t-test
t.test(rhhwork, sphhwork, paired=TRUE)


#f test to checking equality of variances
tvhours[is.na(tvhours)]<- 0
head(gss$tvhours)
var(gss$tvhours[sex=="female"])
var(gss$tvhours[sex=="male"])
length(tvhours)
length(sex)
var.test(tvhours~sex)
var.test(rhhwork,sphhwork)
str(tvhours)

#chi-squared analysis
install.packages("MASS")
library(MASS)
tbl <- table(class, marital) #contingency table created
chisq.test(tbl)
tbl

#linear model
modelex <- lm(tvhours~rhhwork,data=gss)
ggplot(gss,aes(x=rhhwork,y=tvhours))+geom_point()
summary(modelex)
plot(rhhwork,tvhours,main="Scatterplot")
abline(modelex,col=2,lwd=3)
coef(modelex)
confint(modelex)
anova(modelex)
plot(modelex)

#correlation test
plot(sphhwork,tvhours)
cor.test(tvhours,sphhwork,method="pearson")

#ANOVA
anova<-aov(age~class, data=gss)
summary(anova)
anova2<-aov(educ~race,data=gss)
summary(anova2)
testing_set <- rnorm(14,31,3)
t.test(testing_set, mu=30)

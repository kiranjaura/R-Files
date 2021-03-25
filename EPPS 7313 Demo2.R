install.packages("matlib")
library(matlib)
setwd("C:/Users/19728/Desktop/EPPS 7313")
study<-read.csv("Hours Studied Data Set.csv")

lm(HrsStudied,TestScore, data=study)
reg<-lm()
install.packages(c("car","effect","knitr",dependencies=TRUE))
update.packages()
(x<- 1)
column_1 <- matrix(1,15,1,byrow=FALSE)
column_2 <- matrix(c(93,99,83,97,96,91,78,90,78,76,84,68,76,74,69),15,1,byrow=FALSE)
column_2
x <- cbind(column_1,column_2)
x

det(x)
inv(x)*
y<- matrix(c(7,8,8,11,14,16,17,19,21,22,24,31,33,33,34),15,1,byrow=FALSE)
t(x)%*%x
inv(t(x)%*%x)%*%t(x)%*%y
A <- matrix(c(2,-3,5,5),2,2,byrow = TRUE)
A
A
inv(A)
X <- matrix(c(1,1,1,1,0,0,1,1),nrow=4)
rownames(X) <- c("a","a","b","b")
X
beta <- c(5,2)
new <- X%*%beta
new[ 1:2, ]
fitted = X%*%beta
fitted[ 1:2,]
X
beta
g <- 9.8 ## meters per second
h0 <- 56.67
v0 <- 0
n <- 25
tt <- seq(0,3.4,len=n) ##time in secs, t is a base function
y <- h0 + v0 *tt  - 0.5* g*tt^2 + rnorm(n,sd=1)
X = cbind(1,tt,tt^2)
A = solve(crossprod(X))%*%t(X)
X
A
-2 * (A %*% y) [3]
(A %*% y)
library(UsingR)
x = father.son$fheight
y = father.son$sheight
n = length(y)
N = 50
set.seed(1)
index = sample(n,N)
sampledat = father.son[index,]
x = sampledat$fheight
y = sampledat$sheight
betahat = lm(y~x)$coef
fit = lm(y ~ x)
fit$fitted.values
SSR <- sum((predict(betahat) - mean(y))^2)
(sigma2 = SSR / 48)
var(y)
X = cbind(rep(1,N), x)
X
(t(X)) %*% X
solve(crossprod(X))
try <- solve(t(X)%*%X)
diag1 <- diag(try)
product <- try%*%diag1
sqrt(product)
fit = lm(y ~ x)
(SSR_check <- sum((y - fit$fitted.values)^2))
sigma2 = sum((y - fit$fitted.values)^2) / (N - 2)
sqrt(sigma2 * diag(solve(t(X) %*% X)))
library(tidyr)
library(dplyr)
library(ggplot2)
library(tidyverse)
setwd("C:/Users/19728/Desktop/EPPS 7313")
house_data <- read.csv("kc_house_data.csv")
head(house_data)
model.matrix(~sqft_living, data=house_data)
nx=5
ny=7
X = cbind(rep(1,nx + ny),
          rep(c(0,1),
              c(nx, ny)))
X
t(X)%*%X
url <- "https://raw.githubusercontent.com/genomicsclass/datdata/master/inst/extdata/spider_wolff_gorb_2013.csv"
file_name <- "spier_wolff_gorb_2013.csv"
install.packages(downloader)
install_github("downloader")
install.packages("rafalib")
library(rafalib)
install.packages("swirl")
library(swirl)
swirl()
Kiran

binomial_function<-function(n,x,theta) {
  answer <- factorial(n)/(factorial(x)*factorial(n-x))*(theta^x)*(1-theta)^(n-x)
  return(answer)
}

binomial_function(6,15,.4)

n_choose_r <- function(n,r) {
  answer <- factorial(n)/(factorial(r)*factorial(n-r))
  return(answer)
}
n_choose_r(6,4)*(4/5)^4*(1/5)^2
binomial_function(5,12,0.5)
binomial_function(3,10,.4)
binomial_function(5,400,.02)

poisson_dist<-function(lambda,x){
  answer<-(exp(1)^(-lambda)*lambda^x)/factorial(x)
  return(answer)
}
poisson_dist(1.8,2)
poisson_dist(1.2,1)
poisson_dist(1.2,2)
poisson_dist(1.2,1)+poisson_dist(1.2,2)
poisson_dist(1.2,0)
poisson_dist(1.2,0)+poisson_dist(1.2,1)+poisson_dist(1.2,2)
poisson_dist(3.3,2)
poisson_dist(5.2,3)
poisson_dist_cum<-function(lambda,x) {
  new_answer=0
  if (x>=0) {
    answer<-new_answer+poisson_dist(lambda,x)
    x=x-1
    new_answer=answer
    }
  else {
    return answer
    }
}
poisson_dist(5.2,4)+poisson_dist(5.2,5)+poisson_dist(5.2,6)
multinomial_dist<-function(a,b,c,a1,b1,c1) {
  answer <- factorial(a+b+c)/(factorial(a)*factorial(b)*factorial(c))*((a^a1)*(b^b1)*(c^c1))
  return answer
}
multinomial_dist(.4,.5,.1,3,6,1)
(n_choose_r(10,3)*n_choose_r(5,1)*n_choose_r(3,2))/(n_choose_r(18,6))
binomial_function(3,5,.1)+ binomial_function(4,5,.1)+binomial_function(5,5,.1)
binomial_function(5,6,.7)
binomial_function(6,15,.4)
binomial_function(10,18,.5)
n_choose_r(10,9)
binomial_function(6,15,.42)
x<-rbinom(100,1,.5)
x
phat=numeric()
for (i in 1:20)
{
  phat[i]=(x[5*i]+x[(5*i)-1]+x[(5*i)-2]+x[(5*i)-3]+x[(5*i)-4])/5
}
phat
n<-1:20
plot(n,phat)
abline(h=0.5,v=0,col=3)
}
binomial_function(1,6,.5)
neg_binomial_function<-function(x,k,theta) {
  answer <- factorial(x-1)/(factorial(k-1)*factorial(x-k))*(theta^k)*(1-theta)^(x-k)
  return(answer)
}
neg_binomial_function(7,2,.5)
neg_binomial_function(10,5,.5)+neg_binomial_function(10,4,.5)
neg_binomial_function(8,5,.75)
neg_binomial_function(15,10,.75)
neg_binomial_function(6,1,.3)
neg_binomial_function(15,2,.05)
neg_binomial_function(800,1,.001)
binomial_function(1,6,.5)
geometric_function<-function(x, theta) {
  answer <- (theta)*(1-theta)^(x-1)
  return(answer)
}
geometric_function(800,.001)

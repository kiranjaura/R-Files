binomial_function<-function(x,n,theta) {
  answer <- factorial(n)/(factorial(x)*factorial(n-x))*(theta^x)*(1-theta)^(n-x)
  return(answer)
}
neg_binomial_function<-function(x,k,theta) {
  answer <- factorial(x-1)/(factorial(k-1)*factorial(x-k))*(theta^k)*(1-theta)^(x-k)
  return(answer)
}
n_choose_r <- function(n,r) {
  answer <- factorial(n)/(factorial(r)*factorial(n-r))
  return(answer)
}
poisson_dist<-function(x,lambda){
  answer<-(exp(1)^(-lambda)*lambda^x)/factorial(x)
  return(answer)
}
multinomial_dist<-function(a,b,c,a1,b1,c1) {
  answer <- factorial(a+b+c)/(factorial(a)*factorial(b)*factorial(c))*(a^a1)*(b^b1)*(c^c1)
  return answer
}
poisson_dist(2,2.1)
poisson_dist(5,6)
poisson_dist(0,6)+poisson_dist(1,6)+poisson_dist(2,6)+poisson_dist(3,6)
poisson_dist(0,1.2)+poisson_dist(1,1.2)+poisson_dist(2,1.2)
poisson_dist(2,3.3)
poisson_dist(0,1.8)
poisson_dist(1,1.8)
poisson_dist(1,.5)+poisson_dist(1,.5)
.0015+.0015
.003/sqrt(12)
1-2*pt(2,5,lower.tail=FALSE)
2*pt(2,5,lower.tail=FALSE)
pchisq(3.57,24,lower.tail=FALSE)

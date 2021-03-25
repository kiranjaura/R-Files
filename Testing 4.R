
my_function<- function(theta,n) {
  e<-exp(1)
  Poisson_probability<-((((theta*n)^x)*(e^(-1*theta*n)))/(factorial(x))) 
  return(Poisson_probability)
}

my_function(0.00005,10000)


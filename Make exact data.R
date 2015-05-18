#Make exact data command
## Slightly more precise than rnorm

z.score<-function(x){(x-mean(x))/sd(x)}

rescale.numbers<- function(x,mean,sd){
  z <- z.score(x)
  return(z*sd + mean)}

make.exact.data <- function(n, mean, sd){
  x <- rnorm(n)
  return(rescale.numbers(x,mean,sd))}
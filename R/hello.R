result <- function(a,b,c){
  if(delta(a,b,c) > 0){ # first case D>0
    x_1 = (-b+sqrt(delta(a,b,c)))/(2*a)
    x_2 = (-b-sqrt(delta(a,b,c)))/(2*a)
    result = c(x_1,x_2)
  }
  else if(delta(a,b,c) == 0){ # second case D=0
    x = -b/(2*a)
  }
  else {i=(sqrt(as.complex(-1)))
  x_3 = (-b+sqrt(delta(a,b,c)/i))/(2*a)
  x_4 = (-b-sqrt(delta(a,b,c)/i))/(2*a)
  result=c(x_3,x_4)
  print(result)} # third case D<0
}

# Constructing delta
delta<-function(a,b,c){
  b^2-4*a*c
}


#https://rpubs.com/kikihatzistavrou/80124 21/10/2016 14:00

#the coding has been modified to allow for complex answers.

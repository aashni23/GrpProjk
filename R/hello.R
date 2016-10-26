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



#Ivan-Peri Version:

quad_roots <- function(a,b,c) # a simple function that calculates the roots of a quadratic equation

{ dis <- b^2 - 4*a*c #calculting the discriminant 

	if (a != 0) # check that we actually have a quadratic equation (not linear)
  
		{if (dis >= 0) # case1: non-negative discriminant
			{roots <- c((-b+sqrt(dis))/(2*a),(-b-sqrt(dis))/(2*a))} # result as vector of length 2 
	
		else # otherwise give a complex solution
			{i <- sqrt(as.complex(-1)) # imaginary unit (i)
			roots <- c((-b+sqrt(dis*(-1)))*i/(2*a),(-b-sqrt(dis*(-1)))*i/(2*a))} # result as vector of length 2 

		print(paste('X1 = ', roots[1], ' : X2 = ', roots[2])) # print the result in a user-friendly format

		return(roots)} # return the result

	else # if the equation is linear
	{print('please provide valid quadratic eqution coefficients!')}
}

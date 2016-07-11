
###############################################################
# quadratic approximation of the absolute value function
abs_approx <- function( x , eps = 1E-5){
	res <- base::sqrt( x^2 + eps )
	base::return(res)
}

abs_approx_D1 <- function( x , eps = 1E-5){
	res <-  x / base::sqrt( x^2 + eps )
	base::return(res)
}
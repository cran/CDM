################################################################################
# utility method for computing intermediate information                        #
################################################################################
rowMaxs <-
function(mat){
# Call: from din()
# Input: numeric matrix
# Output: row maxima of input matrix
    n <- nrow(mat)
    p <- ncol(mat)
    x <- as.vector(mat)
    x <- matrix(x[order(rep(1:n, p), x)], p, n)
    x[p , ]
}
#########################################################
rowMaxs2 <-
function(mat){
    n <- nrow(mat)
    p <- ncol(mat)
	maxval <- mat[,1]
	maxind <- 1
	for ( cc in 2:p){
		maxval0 <- maxval
		ind <- ( mat[,cc] > maxval )
		maxval <- ifelse( ind , mat[,cc] , maxval )
		maxind <- ifelse( ind , cc , maxind)
				   }
	res <- list( "maxval" = maxval , "maxind" = maxind )
	return(res)
}

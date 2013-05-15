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



solve_add_ridge <- function(A , eps = 1E-7)
{
	A0 <- A
	base::diag(A) <- base::diag(A0) * ( 1 + eps )
	A2 <- base::solve(A)
	base::return(A2)
}
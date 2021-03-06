## File Name: solve_add_ridge.R
## File Version: 0.05

solve_add_ridge <- function(A, eps=1E-7)
{
    A0 <- A
    diag(A) <- diag(A0) * ( 1 + eps )
    A2 <- solve(A)
    return(A2)
}

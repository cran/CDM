#########################################
# Log likelihood functions
#########################################
# din class
logLik.din <- function (object, ...) {
	# extract log-likelihood
	out <- object$loglike
    # number of parameters
    attr(out, "df") <- sum( object$Npars )
	# extract number of observations
    attr(out, "nobs") <- nrow(object$I)
    class(out) <- "logLik"
    out
}
#########################################
# gdina class
logLik.gdina <- function (object, ...) {
	# extract log-likelihood
	out <- object$loglike
    # number of parameters
    attr(out, "df") <- sum( object$Npars )
	# extract number of observations
    attr(out, "nobs") <- nrow(object$I)
    class(out) <- "logLik"
    out
}
###########################################
# gdm class 
logLik.gdm <- function (object, ...) {
	# extract log-likelihood
	out <- object$loglike
    # number of parameters
    attr(out, "df") <- sum( object$Npars )
	# extract number of observations
    attr(out, "nobs") <- nrow(object$N)
    class(out) <- "logLik"
    out
}
#############################################
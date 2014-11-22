################################################################################
# print method class gdm
################################################################################
print.gdm <-
function(x, ... ){
	cat("Estimation of general diagnostic model\n")
	cat("\nCall:\n", paste(deparse(x$call), sep = "\n", collapse = "\n"), 
				"\n\n", sep = "")
	#*** parameters
	cat(paste0("Number of cases = " , x$N , "\n") )
	cat(paste0("Number of groups = " , x$G , "\n") )
	cat(paste0("Number of items = " , ncol(x$data) , "\n") )
	cat(paste0("Number of skill dimensions = " , dim(x$Qmatrix)[3] , "\n") )	
	cat(paste0("Number of skill classes = " , nrow(x$pi.k) , "\n") )		
	cat(paste0("Number of parameters = " , x$Npars , "\n") )
	cat(paste0("  Number of item parameters = " , x$ic$itempars , "\n") )
	cat(paste0("  Number of skill distribution parameters = " , 
				x$ic$traitpars , "\n") )
	#*** likelihood
	cat( paste0( "\nLog-Likelihood = " , round( x$loglike ,2 ) , "\n") )
	#*** information criteria
	cat( paste0( "AIC = " , round( x$AIC ,0 ) , "\n") )
	cat( paste0( "BIC = " , round( x$BIC ,0 ) , "\n") )
	invisible(x)	
}


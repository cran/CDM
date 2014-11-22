################################################################################
# print method class mcdina
################################################################################
print.mcdina <-
function(x, ... ){
	cat("Estimation of multiple choice DINA model\n")
	cat("\nCall:\n", paste(deparse(x$call), sep = "\n", collapse = "\n"), 
				"\n\n", sep = "")
	#*** parameters
	cat(paste0("Number of cases = " , x$I , "\n") )
	cat(paste0("Number of groups = " , x$G , "\n") )
	cat(paste0("Number of items = " , ncol(x$dat) , "\n") )
	cat(paste0("Number of skill dimensions = " , ncol(x$q.matrix) - 2, "\n") )	
	cat(paste0("Number of skill classes = " , nrow(x$attribute.patt) , "\n") )		
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


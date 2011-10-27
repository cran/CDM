################################################################################
# summary method for objects of class "din"                                    #
################################################################################
summary.din <-
function(object, ...){

# Call: generic
# Input: object of class din
# Output: a named list, of the class summary.din (to be passed to print.summary.din), consisting of the following five components
# 	CALL: a character specifying the model rule, the number of items and the number of attributes underlying the items.
#	IDI: a matrix giving the diagnostic accuracy for each item. (see help file)
#	SKILL.CLASS.PROB: a table which returns the minimum, maximum, quantile and mean information of the skill pattern distribution.
#	AIC: a numeric giving the AIC of the specified model object.
#	BIC: a numeric giving the BIC of the specified model object.

################################################################################
# extract output from din object                                               #
################################################################################
	
	CALL <- paste(object$display,"on", ncol(object$data), "items for", nrow(object$skill.patt),"attributes")
	
	AIC <- round(object$AIC, 3)
	BIC <- round(object$BIC, 3)
	
	IDI <- matrix(round(
	  apply(rbind(object$guess[,1],object$slip[,1]), 2, function(x) 1-x[1]/(1-x[2]))
	    ,3), nrow=1, dimnames=list("",colnames(object$data)))
	    
	SKILL.CLASS.PROB <- round(summary(object$attribute.patt[,1]), 3)

################################################################################
# return list                                                                  #
################################################################################
	
	out <- list("CALL"=CALL,"IDI"=IDI,"SKILL.CLASS.PROB"=SKILL.CLASS.PROB,"AIC"=AIC,"BIC"=BIC)
	class(out) <- "summary.din"
	return(out)
}



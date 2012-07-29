################################################################################
# print method for objects of class "din"                                      #
################################################################################
print.din <-
function(x, ... ){

# Call: generic
# Input: 
#	x: object of class din
# Output: x
# Print: prints the itemwise guessing and slipping values with standard errors, as well as the skill probabilities and invisibly returns x.

################################################################################
# extract output from din object                                               #
################################################################################

	skill.patts <- x$skill.patt
	colnames(skill.patts)<-""
	rownames(skill.patts)<- colnames(x$q.matrix)

	item.pars <- cbind(rownames(t(x$coef)),t(x$coef))
	rownames(item.pars) <- rep("",5)

################################################################################
# print                                                                        #
################################################################################

	cat("Slipping and guessing parameters:", "\n" )
	print(item.pars, quote=F)
    cat("\nSkill probabilities:", "\n" )
    print(t(round(skill.patts, 3)))
    invisible(x)
    
}


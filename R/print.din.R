################################################################################
# print method for objects of class "din"                                      #
################################################################################
print.din <-
function(x, highest=0.05, ... ){

# Call: generic
# Input: 
#	x: object of class din
#	highest: a numeric between 0 and 1 giving the percentage of skill patterns with highest occurrence frequency to be printed.
# Output: x
# Print: prints the itemwise guessing and slipping values with standard errors, the skill patterns with highest occurrence frequnecy and the skill probabilities and invisibly returns x.

################################################################################
# extract output from din object                                               #
################################################################################

	patt.fq <- x$attribute.patt[,1]
	main.effects <- which(patt.fq >= ifelse(ceiling((highest)*length(patt.fq))>0,
	  sort(patt.fq,decreasing=TRUE)[ceiling((highest)*length(patt.fq))],1))

	class.prob <- matrix(round(x$attribute.patt[main.effects,1] , 3),
  		dimnames=list("",rownames(x$attribute.patt[main.effects,])),nrow=1)
	class.prob <- t(as.matrix(class.prob[,order(class.prob[1,],decreasing = TRUE)]))
	rownames(class.prob)<-""

	skill.patts <- x$skill.patt
	colnames(skill.patts)<-""
	rownames(skill.patts)<- colnames(x$q.matrix)

	skill.prob <- cbind(rownames(t(x$coef)),t(x$coef))
	rownames(skill.prob) <- rep("",5)

################################################################################
# print                                                                        #
################################################################################

	cat("Slipping and guessing parameters:", "\n" )
	print(skill.prob, quote=F)
    cat(paste("\n",highest*100,"%",sep=""),"highest skill pattern probabilities:\n")
    print(class.prob)
    cat("\nSkill probabilities:", "\n" )
    print(t(round(skill.patts, 3)))
    return(x)
    
}


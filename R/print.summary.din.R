################################################################################
# print method for objects of class "summary.din"                              #
################################################################################
print.summary.din <-
function(x, ...){

# Call: generic
# Input: object of class summary.din
# Print: prints the named list, of an object of class summary.din

################################################################################
# console output                                                               #
################################################################################
if(is.null(x$log.file)){
  cat("Call:\n",
      x$CALL, "\n",
      "\nItem discrimination index:\n")
      print(x$IDI)    
      cat(paste("\n",length(x$SKILL.CLASSES), " most frequent skill classes:\n", sep = ""))
      print(x$SKILL.CLASSES)
  cat("\nInformation criteria:",
      "\n  AIC = " , x$AIC,
      "\n  BIC = " , x$BIC, "\n")
}else{

################################################################################
# logfile output                                                               #
################################################################################
    tr <- try({sink(file = x$log.file)})
    if(is.null(tr)){
      gowidth <- getOption("width")
      options(width = 10000)
	  cat("#.......................................................\n")
	  d <-  packageDescription("CDM")
#	  c1 <- citation("CDM")
	  cat(paste( "This is CDM package version " , d$Version , " (" , d$Date , ")\n",sep=""))
#	  print(c1)
	  cat("\n")	
      cat("#-------------------------\n")
      cat("# SUMMARY OF ANALYSIS\n")
	  cat( "Start:" , paste(x$start.analysis ))
	  cat( "\nEnd  :" , paste(x$end.analysis ))
      cat("\n#-------------------------\n\n")
      cat("Model rule:",x$display, "\n")
      cat("Number of observations:",nrow(x$data), "\n")
      cat("Number of items:",nrow(x$q.matrix), "\n")
      cat("Labels of items:", paste(rownames(x$q.matrix), collapse=", "), "\n")
      cat("Number of skills:",ncol(x$q.matrix), "\n")
      cat("Labels of skills:", paste(attributes(x$q.matrix)$skill.labels, collapse=", "), "\n")
      cat("Q-Matrix:\n\n")
      print(data.frame(x$q.matrix))
      
      cat("\n#-------------------------\n")
      cat("# SUMMARY OF MODEL FIT\n")
      cat("#-------------------------\n\n")
      cat("Loglikelihood:", x$loglike, "\n")
      cat("AIC:", x$AIC, "\n")
      cat("BIC:", x$BIC, "\n\n")
      cat("Item discrimination index:\n\n")
      print(x$IDI)
      cat(paste("\n",length(x$SKILL.CLASSES), " most frequent skill classes:\n", sep = ""))
      print(x$SKILL.CLASSES)
       
      cat("\n#-------------------------\n")
      cat("SUMMARY OF MODEL RESULTS\n")
      cat("#-------------------------\n\n")
      cat("Item parameter estimates:\n\n")
      print(x$coef)
      cat("\nSkill probability:\n\n")
      print(x$skill.patt)
      cat("\nSkill pattern occurrence probability:\n\n")
      print(x$attribute.patt)
      cat("\nSkill class assignment and skill assignment probabilities for respective response pattern:\n\n")
      print(cbind("freq" = as.vector(table(x$pattern[,1])), unique(x$pattern)))
    
      sink()
      options(width = gowidth)
      cat("\nExtensive summary written to log file:\n", x$log.file,"\n")
    }else cat("\nError while trying to write summary to log file:\n", tr[1])          
  }
}

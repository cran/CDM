################################################################################
# print method for objects of class "summary.din"                              #
################################################################################
print.summary.din <-
function(x, ...){

# Call: generic
# Input: object of class summary.din
# Print: prints the named list, of an object of class summary.din

  cat("Call:\n",
      x$CALL, "\n",
      "\nDiagnostic accuracy:\n")
      print(as.table(x$IDI))    
      cat("\nSummary of skill pattern distribution:\n")
      print(x$SKILL.CLASS.PROB)
  cat("\nInformation criteria:",
      "\n  AIC = " , x$AIC,
      "\n  BIC = " , x$BIC, "\n")
}

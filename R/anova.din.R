##############################################################
# Likelihood ratio test for din objects
anova.din <- function( object , object1 , ...){ 
	model1 <- object
	model2 <- object1	
    dfr1 <- data.frame( "Model" = "Model 1" , 
		"loglike" = model1$loglike , 
		"Deviance" = -2*model1$loglike )
    dfr1$Npars <- sum(model1$Npars)
    dfr1$AIC <- model1$AIC
    dfr1$BIC <- model1$BIC
    dfr2 <- data.frame( "Model" = "Model 2" , 
		"loglike" = model2$loglike , 	
		"Deviance" = -2*model2$loglike )
    dfr2$Npars <- sum(model2$Npars)
    dfr2$AIC <- model2$AIC
    dfr2$BIC <- model2$BIC
    dfr <- rbind( dfr1 , dfr2 )
    dfr <- dfr[ order( dfr$Npars ), ]
    dfr$Chisq <- NA
    dfr$df <- NA
    dfr$p <- NA
    dfr[1,"Chisq"] <- dfr[1,"Deviance"] - dfr[2,"Deviance"]
    dfr[1,"df"] <- abs( dfr[1,"Npars"] - dfr[2,"Npars"] )
    dfr[ 1, "p" ] <- round( 1 - pchisq( dfr[1,"Chisq"] , df= dfr[1,"df"] ) , 5 )
    for ( vv in 2:( ncol(dfr))){ dfr[,vv] <- round( dfr[,vv] , 5 ) }
    print( dfr )
    invisible(dfr)
            }
##############################################################

anova.gdina <- anova.din
anova.gdm <- anova.din
anova.mcdina <- anova.din
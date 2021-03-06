## File Name: predict.CDM.R
## File Version: 0.07


################################################
# predict method in CDM package
predict.din <- function( object, group=1, ... ){
    dat <- as.matrix( object$dat )
    res <- IRT.predict( object, dat=dat, group=group )
    return(res)
            }
#################################################
predict.gdm <- predict.din
predict.gdina <- predict.din
predict.mcdina <- predict.din
predict.slca <- predict.din
##################################################


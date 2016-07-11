
########################
# coef for din object
coef.din <-
function (object, ...) {
	cof <- object$coef
    base::return(cof)
}

########################
# coef for gdina object
coef.gdina <-
function (object, ...) {
	cof <- object$coef
    base::return(cof)
}

########################
# coef for gdm object
coef.gdm <-
function (object, ...) {
	cof <- object$item
    base::return(cof)
}


########################
# coef for mcdina object
coef.mcdina <-
function (object, ...) {
	cof <- object$item
    base::return(cof)
}

########################
# coef for slca object
coef.slca <-
function (object, ...) {
	cof <- object$Xlambda
    base::return(cof)
}
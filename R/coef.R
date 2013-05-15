
########################
# coef for din object
coef.din <-
function (object, ...) {
    if (!inherits(object, "din"))
        stop("Use only with 'din' objects.\n")
	cof <- object$coef
    cof
}

########################
# coef for gdina object
coef.gdina <-
function (object, ...) {
    if (!inherits(object, "gdina"))
        stop("Use only with 'gdina' objects.\n")
	cof <- object$coef
    cof
}

########################
# coef for gdm object
coef.gdm <-
function (object, ...) {
    if (!inherits(object, "gdm"))
        stop("Use only with 'gdm' objects.\n")
	cof <- object$item
    cof
}

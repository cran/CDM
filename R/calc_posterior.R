
###########################################################
# compute posterior distribution V2 BY ARb
#...TK 06.08.2012 -- speed up by using c Code
#..........................................................
calc_posterior.v2 <- function(rprobs , gwt , resp , nitems , 
	resp.ind.list , normalization = TRUE , 
	thetasamp.density = NULL , snodes = 0 ){

	if ( snodes == 0 ){ 
		fx <- gwt  
	} else {
		# calculate individual 'sampling weight'
		swt <- fx <- gwt / base::outer( base::rep(1, base::nrow(gwt)) , thetasamp.density )
	} 
	nstud <- base::nrow(fx)
	 
	# using c Code here
	base::storage.mode(resp) <- "integer"
	fx <- base::.Call("calcfx", fx, rprobs, resp.ind.list, resp)
	# numerical integration
	if ( snodes == 0 ){ 
		rfx <- base::rowSums(fx)  
		if (normalization ){
			hwt <- fx / rfx 
		} else {   
			hwt <- fx 
		}
	}
	# Monte Carlo integration
	if ( snodes > 0 ){ 
		rfx <- base::rowMeans(fx)
		if (normalization ){
			hwt <- fx / rfx 
		} else { 
			hwt <- fx 
		}
	}
	res <-  base::list("hwt" = hwt , "rfx" = rfx )
	if ( snodes > 0 ){ 
		res[["swt" ]] <- swt 
	}
	base::return(res)
}
#..........................................................
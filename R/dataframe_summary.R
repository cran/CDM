
dataframe_summary <- function( dfr , exclude_index , labels , na.rm=TRUE )
{
	if ( ! base::is.null(exclude_index) ){
		dfr1 <- dfr[ , - exclude_index, drop=FALSE]
	} else {
		dfr1 <- dfr
	}	
	dfr_summary <- base::data.frame( "Parm" = labels ,
			"M"= base::apply( dfr1 , 2, base::mean , na.rm=na.rm) , 
			"SD"= base::apply( dfr1 , 2 , stats::sd , na.rm=na.rm) ,
			"Min"= base::apply( dfr1 , 2 , base::min , na.rm=na.rm),
			"Max"= base::apply( dfr1 , 2 , base::max , na.rm=na.rm )
					)
	base::rownames(dfr_summary) <- NULL
	base::return(dfr_summary)
}
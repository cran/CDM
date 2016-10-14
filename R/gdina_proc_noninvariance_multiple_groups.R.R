#####################################################################
# handle non-invariance of multiple group parameters
gdina_proc_noninvariance_multiple_groups <- function( data , q.matrix , invariance ,
	group ){	
	
	#*** treatment of argument invariance
	invariant_all <- base::mean( invariance == TRUE ) == 1
	invariant_none <- base::mean( invariance == FALSE ) == 1
	invariant_some <- ! ( invariant_all | invariant_none )
	#***
	if ( ( ! base::is.null(group) ) & ( ! invariant_all ) ){
		I <- base::ncol(data)		
		#--- extract invariant items
		invariant_items <- NULL
		if (invariant_some){
			invariant_items <- invariance
		}		
		#--- create extended dataset
		data <- item_by_group(dat = data, group=group, invariant = invariant_items)
		#--- extract Q-matrix
		q.matrix <- q.matrix[ base::attr(data,"all_index") , ]
	}
	#*** output
	res <- base::list( data = data, q.matrix = q.matrix )
	base::return(res)
}
#####################################################################
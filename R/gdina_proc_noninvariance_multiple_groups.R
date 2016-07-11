#####################################################################
# handle non-invariance of multiple group parameters
gdina_proc_noninvariance_multiple_groups <- function( data , q.matrix , invariance ,
	group ){	
	if ( ( ! base::is.null(group) ) & ( ! invariance ) ){
		I <- base::ncol(data)
		data <- item_by_group(dat = data, group=group)
		G <- base::ncol(data) / I
		q.matrix <- q.matrix[ base::rep(1:I , each = G ) , ]
	}
	res <- base::list( data = data, q.matrix = q.matrix )
	base::return(res)
}
#####################################################################
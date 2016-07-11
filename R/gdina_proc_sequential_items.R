
gdina_proc_sequential_items <- function( data , q.matrix ){
	maxK <- base::max( data , na.rm=TRUE )
	sequential <- FALSE
	if ( maxK > 1){
		res0 <- sequential.items( data = data )
		data <- res0$dat.expand
		sequential <- TRUE
		q.matrix <- q.matrix[,-c(1:2)]
	}
	
	res <- base::list( data = data , sequential = sequential ,
				q.matrix = q.matrix )
	base::return(res)
}
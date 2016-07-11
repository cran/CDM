
##########################################################
# creates an extended dataset with item responses in which
# items are defined as combinations of original items and
# group
item_by_group <- function( dat , group , rm.empty = TRUE ){
	vars <- base::colnames(dat)
	I <- base::length(vars)
	group_unique <- base::sort( base::unique(group) )
	G <- base::length(group_unique)
	#*** create extended dataset
	dat2 <- base::matrix( NA , nrow = base::nrow(dat) , ncol= I*G )
	cn <- base::sapply( vars , FUN = function(vv){
			  base::paste0( vv , "_group" , group_unique ) } , simplify=FALSE)
	base::colnames(dat2) <- base::unlist(cn)
	for (gg in 1:G){
		# gg <- 1
		ind_gg <- base::which( group == group_unique[gg] )	
		for (ii in 1:I){
			# ii <- 1
			dat2[ ind_gg , G*(ii-1) + gg ] <- dat[ ind_gg , ii ]
		}	
	}
	if (rm.empty){
		ind <- base::which( base::colMeans( base::is.na(dat2) ) == 1 )
		if ( base::length(ind) > 0 ){
			dat2 <- dat2[ , - ind ]
		}
	}
	base::return(dat2)
}
############################################################
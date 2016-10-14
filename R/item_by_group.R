
##########################################################
# creates an extended dataset with item responses in which
# items are defined as combinations of original items and
# group
item_by_group <- function( dat , group , invariant = NULL ,
		rm.empty = TRUE )
{
	vars <- base::colnames(dat)
	some_invariant_items <- ( ! base::is.null(invariant) )	
	if ( some_invariant_items ){
		vars <- setdiff(vars,invariant)
	}
	
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
			dat2[ ind_gg , G*(ii-1) + gg ] <- dat[ ind_gg , vars[ii] ]
		}	
	}
	#--- include invariant items
	if ( some_invariant_items ){
		dat2a <- dat[ , invariant]
		dat2 <- base::cbind( dat2a , dat2 )
	}	
	
	#--- remove empty columns
	if (rm.empty){
		ind <- base::which( base::colMeans( base::is.na(dat2) ) == 1 )
		if ( base::length(ind) > 0 ){
			dat2 <- dat2[ , - ind ]
		}
	}
	#--- include some attributes: variables and variable indices
	base::attr(dat2,"noninvariant") <- vars
	base::attr(dat2,"invariant") <- invariant
	base::attr(dat2,"noninvariant_index") <- base::match( vars , base::colnames(dat))
	base::attr(dat2,"noninvariant_index_extended") <- 
			base::rep( base::attr(dat2,"noninvariant_index") , each = G )
	base::attr(dat2,"invariant_index") <- base::match( invariant , base::colnames(dat))	
	base::attr(dat2,"all_index") <- base::c( base::attr(dat2,"invariant_index") , 
					base::attr(dat2,"noninvariant_index_extended") )
	cn <- base::colnames(dat2)
	base::names(cn) <- NULL
	base::colnames(dat2) <- cn					
	#--- output
	base::return(dat2)
}
############################################################
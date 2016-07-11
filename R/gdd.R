#################################################################
# generalized distance discriminating method
gdd <- function( data , q.matrix , theta , b , a  , skillclasses=NULL){
	data <- base::as.matrix(data)
	dataresp <- base::as.matrix( 1 - base::is.na( data ) )
	data[ base::is.na(data) ] <- 0
	q.matrix <- base::as.matrix(q.matrix)
	skillspace <- skillclasses
	# compute ideal response pattern
	res <- ideal.response.pattern( q.matrix , skillspace )
	idealresp <- res$idealresp
	skillspace <- res$skillspace
	# apply generalized distance discriminating method
	res <- generalized_distance_method__Cpp( data , dataresp , 
	            idealresp, theta , a , b )
	# extract results
	distmatrix <- res$dist
	skillclass.est <- skillspace[ res$est_skill , ]
	res <- base::list( "skillclass.est" = skillspace , "distmatrix" = distmatrix ,
				  "skillspace" = skillspace , "theta" = theta )   
	base::return(res)
}
###############################################################################
# auxiliary function for generalized distance discriminating method
#  SEXP generalized_distance_method__C( SEXP data_, SEXP dataresp_, SEXP idealresp_, 
#   SEXP theta_, SEXP a_, SEXP b_) ;
generalized_distance_method__Cpp <- 
function( data_ , dataresp_ , idealresp_ , theta_ , a_ , b_ ){
	res <- base::.Call("generalized_distance_method__C", 
					data_ , dataresp_ , idealresp_ , theta_ , a_ , b_ , 
					PACKAGE = "CDM")
    base::return(res)			
}
###############################################################################
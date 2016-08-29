#################################################################
# generalized distance discriminating method
gdd <- function( data , q.matrix , theta , b , a  , skillclasses=NULL){
	data <- base::as.matrix(data)
	data_isna <- base::is.na( data )
	dataresp <- base::as.matrix( 1 - data_isna )
	data[ data_isna ] <- 0
	q.matrix <- base::as.matrix(q.matrix)
	skillspace <- skillclasses
	# compute ideal response pattern
	res <- ideal.response.pattern( q.matrix , skillspace )
	idealresp <- res$idealresp
	skillspace <- res$skillspace
	# apply generalized distance discriminating method written in Rcpp
	res <- base::.Call("generalized_distance_method__C", 
					data , dataresp , idealresp , theta , a , b , 
					PACKAGE = "CDM")				
	# extract results
	distmatrix <- res$dist
	skillclass.est <- skillspace[ res$est_skill , ]
	res <- base::list( "skillclass.est" = skillspace , "distmatrix" = distmatrix ,
				  "skillspace" = skillspace , "theta" = theta )   
	base::return(res)
}
###############################################################################

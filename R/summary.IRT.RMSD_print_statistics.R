
summary.IRT.RMSD_print_statistics <- function( stat_summary, stat, digits){
	obji <- stat_summary
	NC <- base::ncol(obji)
	for (gg in 2:NC ){
		obji[,gg] <- base::round( obji[,gg] , digits=digits)
	}
	print( obji )   	
	cat("\n")
	obji <- stat
	NC <- base::ncol(obji)
	for (gg in 2:NC ){
		obji[,gg] <- base::round( obji[,gg] , digits=digits)
	}
	print( obji )   
}
#  zzz.R
#
# This function is simply copied from mice package.

#------------------------------.onLoad-------------------------------
#.onLoad <- function(...){
#  d <- packageDescription("CDM")
#  cat("\n#############################\n")
#  packageStartupMessage(paste(d$Package," " , d$Version," (",d$Date,")",sep=""))
#  cat("#############################\n")
#  return()
#}
version <- function(pkg="CDM"){
  lib <- dirname(system.file(package = pkg))
  d <- packageDescription(pkg)
  return(paste(d$Package,d$Version,d$Date,lib))
}
# on attach CDM
.onAttach <- function(libname,pkgname){
  d <- packageDescription("CDM")
  packageStartupMessage("**********************************\n",
		paste("** ", d$Package," " , d$Version," (",d$Date,")      **\n",sep="") ,
		paste("** Cognitive Diagnostic Models  **",sep="") ,		
		"\n**********************************\n" )
}
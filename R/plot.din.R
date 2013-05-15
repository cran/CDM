################################################################################
# summary method for objects of class "din"                                    #
################################################################################
plot.din <- 
function(x, items=c(1:ncol(x$data)), pattern="", 
  uncertainty=0.1, top.n.skill.classes=6, pdf.file="", 
  hide.idi = FALSE, hide.obs = FALSE,
  display.nr=1:5, ...){

# Call: generic
# Input: 
#	x: object of class din
#	items:	an index vector giving the items to be visualized in the first plot
#	pattern: an optional character specifying a response pattern of an examinee, whose attributes are then analyzed in a seperate grafic.
#	uncertainty: a numeric between 0 and 0.5 giving the uncertanity bounds for deriving the observed skill occurrence probabilities in plot 2 
#	and the simplified deterministic attribute profiles in plot 5.
# top.n.skill.classes: a numeric, specifying the number of skill classes, starting with the most frequent, to be plotted in plot 3. Default value is 6.
#	pdf.file: an optional character string. If specified the graphics obtained 
#           from the function plot.din are provided in a pdf file.
# hide.idi: an optional logical value. If set to \code{TRUE}, the IDI curve in 
#           first graphic is not displayed.
# hide.obs: an optional logical value. If set to \code{TRUE}, the polygonal 
#           chain for observed frequencies of skill class probabilities in the 
#           second graphic is not displayed.
# display.nr: an optional numeric or numeric vector. If specified, only the 
#             plots in display.nr are shown. 
# Output: none

################################################################################
# check consistency of input                                                   #
################################################################################

	suc <- which(unique(x$pattern[,"pattern"]) == pattern) #subject under control
	if(pdf.file!="") try(pdf(file=pdf.file, ...))
	  
	try(if(uncertainty<0||uncertainty>.5|| #uncertainty >= 0, <= .5
	  top.n.skill.classes<0||top.n.skill.classes>2^length(x$skill.patt) )  #top.n.skill.classes >= 0, <=2^K
		warning("check your plot parameter specifications. See Help-files."))   
	
	old.par <- par(no.readonly = TRUE)
	on.exit(par(old.par))
################################################################################
# Plot 1                                                                       #
################################################################################

if(1 %in% display.nr){
	# extract information
	errors <- rbind(x$guess[,1],x$slip[,1])[,items]
	if(!is.null(colnames(x$data)[items])){ 
		colnames(errors) <- colnames(x$data)[items]
	}else{
		colnames(errors) <- paste("Item", items, sep="")
	}

	# generate plot
	barplot(errors, ylim=c(0,1.19), beside=TRUE, col=c("gray","darkred"), 
		ylab="Parameter estimate", cex.lab=1.3)
	if(!hide.idi){
    if(any(apply(errors, 2, function(x) 1-x[1] - x[2] < 0 ))){
		  warning(paste("Item discrimination index < 0 for item",
				which(apply(errors, 2,  function(x) 1-x[1]-x[2] < 0 )),"\n"))
    }else{
	    lines(seq(2,2+3*(ncol(errors)-1),3),apply(errors, 2, function(x) 1-x[1]-x[2] ), lty=1)
		  points(seq(2,2+3*(ncol(errors)-1),3),apply(errors, 2, function(x) 1-x[1]-x[2] ), pch=19, cex=1.5)
		  legend("topright",c("guessing parameter","slipping parameter", "item discrimination index"), 
	      lty=c(1,1,1), pch=c(NA,NA,19), lwd=c(2,2,2), col=c("gray","darkred", "black"), bg = "gray97")
		}
  }else{
	  legend("topright",c("guessing parameter","slipping parameter"), 
	    lty=c(1,1), lwd=c(2,2), col=c("gray","darkred"), bg = "gray97")
	}
	  
	      
	if(pdf.file=="") par(ask=T)
	if(1 == display.nr[length(display.nr)]) par(ask=FALSE)
}
	   
################################################################################
# Plot 2                                                                       #
################################################################################

if(2 %in% display.nr){
	# extract information
	skill.patterns <- x$skill.patt[length(x$skill.patt):1,]
  
  ind <- match( apply(x$item.patt.split, 1, paste, collapse ="") , 
                unique(x$pattern)[,"pattern"] )
  EAP <- ifelse( unique(x$pattern)[ ind, grep("post.attr", colnames(x$pattern) ) ] > 
    0.5 + uncertainty, 1, NA )
  
	master <- colSums( apply( EAP , 2, function(y) y*x$item.patt.freq), na.rm=TRUE )
  master <- ( master/ nrow(x$data) )[length(x$skill.patt):1]

	# generate plot
	par(yaxt="n")
	barplot(skill.patterns, horiz=TRUE, ylim=c(0,length(skill.patterns)*1.2+0.9),
		xlim=c(0,1), xlab="Skill probability", axes=F, cex.lab=1.3, col="gray")
	axis(1,at=seq(0,1,0.2))
	
	text(attributes(x$q.matrix)$skill.labels[length(x$skill.patt):1], 
       x=c(rep(0.01,length(skill.patterns))), y=seq(0.7,
        0.7+1.2*(length(skill.patterns)-1),1.2), col="black", pos=4, cex=1.5)
	
  if(!hide.obs){
    points(x=master, y=seq(0.7,0.7+1.2*(length(skill.patterns)-1),1.2),pch=19, cex=1.5)
    lines(x=master, y=seq(0.7,0.7+1.2*(length(skill.patterns)-1),1.2),lty=1)
    legend("topright",c("Marginal skill probability", "Percentage of masters (EAP)"),
           lty=c(1,1), pch=c(NA,19), lwd=c(2,2), col=c("gray", "black"), bg = "gray97")
  }
	
	par(yaxt="s")
	if(pdf.file=="") par(ask=TRUE)
	if(2 == display.nr[length(display.nr)]) par(ask=FALSE)
}
	  
################################################################################
# Plot 3                                                                       #
################################################################################

if(3 %in% display.nr){
	# extract information
	patt.fq <- x$attribute.patt[,1]
	main.effects <- which(rownames(x$attribute.patt)%in%
	  rownames(x$attribute.patt[order(x$attribute.patt[,1], decreasing = TRUE),][
	  1:min(top.n.skill.classes, 2^length(x$skill.patt)), ])
	    )
	
	# generate plot
	par(xaxt="n"); par(omi=c(.3,0,0,0))
	plot(c(0:(length(patt.fq)+1)),c(0,t(patt.fq),0),type="h", ylab="Skill class occurrence probability",
		xlab="", ylim=c(0,max(patt.fq)+.02),cex.lab=1.5, col=c(NA ,rep("black",length(patt.fq)), NA))
	par(xaxt="s")
	axis(1, at=main.effects, las=2, labels=rownames(x$attribute.patt)[main.effects], cex.axis=.8)
	par(omi=c(0,0,0,0))
	
	if(pdf.file=="") par(ask=TRUE)
	if(3 == display.nr[length(display.nr)]) par(ask=FALSE)
}

################################################################################
# Plot 4                                                                       #
################################################################################

if(4 %in% display.nr){
	# extract information
	post.skill <- rbind(unique(x$pattern)[,grep("post.attr", colnames(x$pattern))],rep(0,nrow(x$skill.patt)),rep(1,nrow(x$skill.patt)))
	colnames(post.skill) <- colnames(x$q.matrix)
	
	rx <- apply(post.skill, 2L, range, na.rm = TRUE)
	a <- apply(post.skill, 2L, function(y) (y - min(y, na.rm = TRUE))/(max(y,
		na.rm = TRUE) - min(y, na.rm = TRUE)))

	# generate plot; cf. parcoord()
	cols <- c(rep("lightgray",nrow(post.skill)),0,0)
	lwid <- c(rep(.5,nrow(post.skill)),0,0)
	matplot(1L:ncol(a), t(a), type = "l", col = cols, lty = 1, ylab = "Skill probabilities conditional on response patterns", 
		xlab = "", axes = FALSE, lwd=lwid, cex.lab=1.5)
	
	axis(1, at = 1L:ncol(a), labels = attributes(x$q.matrix)$skill.labels)
	axis(2, at = seq(0,1,0.2), labels = seq(0,1,0.2))
	for (i in 1L:ncol(a)) lines(c(i, i), c(0, 1), col = "grey50")
	
	if(pdf.file=="") par(ask=T)
	if(4 == display.nr[length(display.nr)]) par(ask=FALSE)  
}

################################################################################
# Plot 5                                                                       #
################################################################################

if(5 %in% display.nr){   
	if(pattern!=""){
    if(length(suc) == 0)
      warning("The specified pattern was not achieved.")
     	
		# if a pattern is specified extract information
		post.skill <- as.matrix(unique(x$pattern)[suc ,grep("post.attr", colnames(x$pattern))])[nrow(x$skill.patt):1]
		names(post.skill) <- colnames(x$q.matrix)[nrow(x$skill.patt):1]
		
		# generate plot
		par(yaxt="n")
		barplot(post.skill, horiz=TRUE, xlab=paste("Skill probabilities conditional on response pattern\n",pattern ,sep=""), 
			xlim=c(0,1), axes=F, cex.lab=1.3, col="gray")
		
		axis(1,at=seq(0,1,0.2))
		abline(v=c(.5-uncertainty,.5+uncertainty),lty=1,col="red")
		#abline(v=c(.5-uncertainty,.5+uncertainty),lty=2,col="black")
		axis(3, at=c((.5-uncertainty)/2,.5,.5+uncertainty+(1-(.5+uncertainty))/2), tick=F, 
			labels=c("not mastered", "unclassified", "mastered"),cex.axis=1.5)
		text(attributes(x$q.matrix)$skill.labels, x=c(rep(0.01,length(row.names(x$skill.patt)))),
			y=seq(0.7,0.7+1.2*(length(row.names(x$skill.patt))-1),1.2),col="black", pos=4, cex=1.5)
		par(yaxt="s")
	}
	
	if(5 == display.nr[length(display.nr)]) par(ask=FALSE)  
}
	# reset open plot parameter
	if(pdf.file!="") try(dev.off())     
	invisible()
}



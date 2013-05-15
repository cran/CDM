
#**********************************************************
# calculates a latent response under the din function

din.latent.response <- function( q.matrix , S , rule="DINA"){
  Q <- as.matrix(q.matrix)
  S <- as.matrix(S)
  L <- matrix(nrow = nrow(Q), ncol = nrow(S))
#  for (i in 1:nrow(S)){
#    for (j in 1:nrow(Q)){
#      L[j,i] <- prod(S[i,]^Q[j,])
#    }
#  }
#  for (i in 1:nrow(S)){
#    for (j in 1:nrow(Q)){
#	  L[j,i] <- sum( S[i,] * Q[j,] )
#	  if ( rule[j] == "DINO"){ L[j,i] <- 1 * ( L[j,i] >= 1 ) }
#	  if ( rule[j] == "DINA"){ L[j,i] <- 1 * ( L[j,i] >= sum(Q[j,]) ) }	  
#    }
#  }
#  colnames(L) <- rownames(S)
#  print(L)
  SQ <- S %*% t(Q)
  nums <- rowSums(Q)
  nums <- ifelse( rule=="DINO" , 1 , nums )
  nums <- matrix( nums , nrow=nrow(SQ) , ncol=ncol(SQ) , byrow=T )
  SQ <- 1 * ( SQ >= nums  )
  L <- t(SQ)
  colnames(L) <- rownames(S)
# print(nums)
#  print(L)
  return(L)
}

###################################
# calculation of equivalent skill classes

din.equivalent.class <-function( q.matrix , rule="DINA"){
  Q <- q.matrix
  # Matrix with all skill classes
  S<-expand.grid( as.data.frame( t( matrix( rep( c(0,1), each = ncol(Q) ), ncol=2 ) ) ) )
  J <- nrow(Q)
  if ( length(rule) == 1){ rule <- rep( rule , J ) }
  rownames(S) <- paste0("Skills_" , apply( S , 1 , 
			FUN = function(ll){ paste(ll , collapse="" ) } )  )
    
  # Calculation of latent response of every skill class  
  A<-din.latent.response(Q,S,rule=rule)
  A<-t(A)
  I<-nrow(A)
  # calculate latent responses
  latent.response <- paste0("LatResp_" , 
		sapply( 1:I, FUN = function(ii){ paste( A[ ii, ], collapse="" )  } ) )
  
  skillclasses <- data.frame( "skillclass" = rownames(S) )
  skillclasses$latent.response <- latent.response
  
  # define distinguishable skill classes
  skillclasses$distinguish.class <- match( latent.response , unique( latent.response ) )
  # calculates how many skill classes correspond to the same latent response
  # berechnet wieviele skillklassen zu einem latenten muster führen
  latent.response <- table( latent.response )
  six <- sort( latent.response, index.return=FALSE, decreasing=TRUE)  
	gini.CDM <- .gini( as.numeric(six) )
    res <- list( "latent.responseM" = A , "latent.response" = latent.response ,
				"S" = S , "gini" = gini.CDM , "skillclasses" = skillclasses )
  cat( nrow(S) , "Skill classes |" , max( skillclasses$distinguish.class ) , 
		" distinguishable skill classes |" ,
		"Gini coefficient =" , round( gini.CDM ,3 ) , "\n")
  return(res)
#  invisible(res)
		}

#********************
# Gini coefficient , simply copied from the R ineq package
.gini <- function (x) 
{
    n <- length(x)
    x <- sort(x)
    G <- sum(x * 1:n)
    G <- 2 * G/(n * sum(x))
    G - 1 - (1/n)
}






#############################################################
# Original code A. C. George 
#############################################################

##   
##   
##   latent <- function(Q , S){
##     L <- matrix(nrow = nrow(Q), ncol = nrow(S))
##     for (i in 1:nrow(S)){
##       for (j in 1:nrow(Q)){
##         L[j,i] <- prod(S[i,]^Q[j,])
##       }
##     }
##     L
##   }
##   
##   # Q ist die im Modell genutzte Q-matrix
##   skill.class.distri <-function(Q){
##     
##     library(ineq)
##     
##     # matrix aller skillsklassen
##     S<-expand.grid( as.data.frame( t( matrix( rep( c(0,1), each = ncol(Q) ), ncol=2 ) ) ) )
##     
##     # funktion zur berechnung der latenten antwort unter jeder skillklasse
##     
##     A<-latent(Q,S)
##     A<-t(A)
##     I<-nrow(A)
##     
##     # bestimmt alle verschiedenen latenten antwortmuster
##     latent.response <- sapply( 1:I, FUN = function(ii){ paste( A[ ii, ], collapse="" )  } )
##     # berechnet wieviele skillklassen zu einem latenten muster führen
##     latent.response <- table( latent.response )
##     six <- sort( latent.response, index.return=F, decreasing=T)
##     
##     # plot der verteilung der skillklassen auf latente antwortmuster
##     # 2^K antwortmuster können höchstens zu 2^K latenten Antwortmuster führen (dann wären alle Klassen unterscheidbar) und der Plot wäre die Winkelhalbierende
##     # je mehr die Kurve "durchhängt" desto mehr skillklassen führen zu gleichen latenten antworten. diese skillklassen sind dann nicht mehr unterscheidbar
##     Calc<-Lc(as.numeric(six))
##     par(xaxt="n")
##     plot(Calc, xlab="number latent response pattern", ylab="L(latent response)", main=paste("Distribution of", 2^ncol(Q1), "skill classes to", dim(latent.response)[1], "lantent responses"))
##     # ein Koeffizient von 0 signalisiert dass alle skillklassen identifizierbar sind
##     # ein Koeffizient um 1 hieße dass alle skillklassen nur zu EINER latenten antwort führen 
##     mtext(paste("G =",round(Gini(as.numeric(six)),2)))
##     par(xaxt="s")
##     a <- seq(1,dim(latent.response)[1],10)
##     if(as.numeric(dim(latent.response)[1] %in% a) == 0){
##       a <- c(a,dim(latent.response)[1] ) }
##     print(a)
##     axis(1, at=a/dim(latent.response)[1], labels=a)
##     
##   }

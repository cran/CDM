//  Code created: 2014-05-30 18:26:39


// includes from the plugin

#include <Rcpp.h>


#ifndef BEGIN_RCPP
#define BEGIN_RCPP
#endif

#ifndef END_RCPP
#define END_RCPP
#endif

using namespace Rcpp;


// user includes


// declarations
extern "C" {
SEXP calc_scoredistribution_cdm( SEXP P1_, SEXP Q1_) ;
}

// definition

SEXP calc_scoredistribution_cdm( SEXP P1_, SEXP Q1_ ){
BEGIN_RCPP
  
       
     Rcpp::NumericMatrix P1(P1_);          
     Rcpp::NumericMatrix Q1(Q1_);  
       
     int TP= P1.nrow();  
     int I= P1.ncol() ;  
       
     // scoredistribution <- matrix(NA , TP , I+1 )  
       
     Rcpp::NumericMatrix scoredist(TP,I+1) ;  
     Rcpp::NumericMatrix scoredist0(TP,I+1);  
       
     scoredist(_,0) = Q1(_,0) ;  
     scoredist(_,1) = P1(_,0) ;  
       
     for (int ii=1 ; ii < I ; ii++ ){  
     scoredist0 = scoredist ;  
     //        scoredistribution[,ii+1] <- P1[,ii] * scoredistribution0[,ii]  
     for (int hh=0;hh<TP;hh++){  
        scoredist(hh,ii+1) = P1(hh,ii)*scoredist0(hh,ii) ;  
        			}	  
     //        for (kk in seq( 0 , ii - 2 , 1 ) ){  
     //            scoredistribution[,ii-kk] <- Q1[,ii] * scoredistribution0[,ii-kk] +   
     //					P1[,ii] * scoredistribution0[,ii-kk-1]  
     for (int kk=0; kk < ii ; kk++){  
     	for (int hh=0;hh<TP;hh++){  
     	   scoredist(hh,ii-kk) = Q1(hh,ii) * scoredist0(hh,ii-kk) +   
     				P1(hh,ii) * scoredist0(hh,ii-kk-1) ;  
     			}  
     		}		  
     //                        }  
     //        scoredistribution[,1] <- Q1[,ii] * scoredistribution0[,1]  
     for (int hh=0;hh<TP;hh++){  
        scoredist(hh,0) = Q1(hh,ii)*scoredist0(hh,0) ;  
        			}  
        		}  
       
     return( wrap( scoredist) );   		  
        		  
     //*************************************************      
     // OUTPUT              
                   
     //  return Rcpp::List::create(    
     // _["scoredist"] = scoredist ,  
     //    ) ;  
END_RCPP
}






// includes from the plugin
#include <RcppArmadillo.h>
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
SEXP IRT_predict( SEXP resp_, SEXP irf1_, SEXP K_, SEXP TP_) ;
}

// definition

SEXP IRT_predict( SEXP resp_, SEXP irf1_, SEXP K_, SEXP TP_ ){
BEGIN_RCPP
   
       
     Rcpp::NumericMatrix resp(resp_);          
     Rcpp::NumericVector irf1(irf1_);  
     int K = as<int>(K_);  
     int TP = as<int>(TP_);  
       
       
     int N=resp.nrow();  
     int I=resp.ncol();  
       
     Rcpp::NumericVector probs_categ(N*K*TP*I);   
     // Rcpp::NumericVector pred(N*TP*I);  
     arma::cube pred(N,TP,I);  
     arma::cube var1(N,TP,I);  
     arma::cube resid1(N,TP,I);  
     arma::cube sresid1(N,TP,I);  
       
     double p1=0;  
     double v1=0;  
       
     for (int ii=0;ii<I;ii++){  
     for (int nn=0;nn<N;nn++){  
       if ( ! R_IsNA( resp(nn,ii) ) ){  
     	for (int tt=0;tt<TP;tt++){ // begin tt  
     	       v1 = 0 ;  
     		for (int kk=0;kk<K;kk++){ // begin kk  
     		    p1 = irf1[ ii + kk*I + tt*I*K ] ;  
     		    probs_categ[ nn + kk*N + tt*N*K + ii*N*K*TP ] = p1 ;  
     		    v1 += kk * p1 ;  
     				} // end kk  
     	       pred(nn,tt,ii) = v1 ;   			  
     	       v1 = 0 ;  
     		for (int kk=0;kk<K;kk++){ // begin kk  
     		    p1 = irf1[ ii + kk*I + tt*I*K ] ;  
     		    v1 += pow( kk - pred(nn,tt,ii) , 2.0 ) * p1 ;  
     				} // end kk         
     	  
     	       var1(nn,tt,ii) = v1 ;    
     	       // residuals  
     	       resid1(nn,tt,ii) = ( resp( nn , ii ) - pred(nn,tt,ii) ) ;  
     	       sresid1(nn,tt,ii) = resid1(nn,tt,ii) / sqrt( var1(nn,tt,ii) ) ;   
     	       	         
     		 } // end tt 		 		 		   
     		   
     	    }  
       if ( R_IsNA( resp(nn,ii) ) ){  
         for (int tt=0;tt<TP;tt++){  
       	pred(nn,tt,ii) = NA_REAL ;  
       	resid1(nn,tt,ii) = NA_REAL ;  
       	sresid1(nn,tt,ii) = NA_REAL ;  	  
       			     }  
       			}  
     	       } // end nn  
     	     } // end ii  
       
                      
     //	Rcpp::Rcout << "Ntot=" <<  Ntot <<  std::flush << std::endl ;  
         		  
         		  
     //*************************************************      
     // OUTPUT              
               
     return Rcpp::List::create(   
         _["pred"] = pred , _["probs_categ"] = probs_categ ,    
         _["var1"] = var1 , _["resid1"] = resid1 , _["sresid1"] = sresid1  
         ) ;    
       
     // maximal list length is 20!  
       
       
     // Rcpp::Rcout << "tmp1 " <<  tmp1 <<  std::flush << std::endl ;  
       
       
     
END_RCPP
}




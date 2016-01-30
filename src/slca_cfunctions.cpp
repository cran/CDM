


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
SEXP calc_slca_probs( SEXP XdesM_, SEXP dimXdes_, SEXP Xlambda_) ;
}

// definition

SEXP calc_slca_probs( SEXP XdesM_, SEXP dimXdes_, SEXP Xlambda_ ){
BEGIN_RCPP
  
       
     Rcpp::NumericMatrix XdesM(XdesM_);          
     Rcpp::NumericVector dimXdes(dimXdes_);        
     Rcpp::NumericVector Xlambda(Xlambda_);  
       
     // $dimXdes  
     // [1]  6  4 21 19  
       
     int I= dimXdes[0] ;  
     int maxK= dimXdes[1] ;  
     int TP= dimXdes[2] ;  
//     int Nlam = dimXdes[3] ;  
     int RR = XdesM.nrow() ;  
       
     //	p1 <- probs <- array( 0 , dim=c(I,K+1,TP) )  
     //	for (tt in 1:TP){  
     //		tmp0 <- 0  
     //		for (hh in 1:(K+1) ){  
     //			tmp1 <- exp( Xdes[ , hh , tt , ] %*% Xlambda )  
     //			tmp0 <- tmp0 + tmp1  
     //			p1[ , hh , tt  ] <- tmp1   
     //								}  
     //		for (hh in 1:(K+1) ){  
     //			probs[,hh,tt] <- p1[ , hh , tt  ] / tmp0  
     //								}  
     //				}  
       
     int PP = I * maxK * TP ;  
     Rcpp::NumericVector probs(PP);  
     Rcpp::NumericVector p1(PP);  
       
     int pp = 0 ;  
     int ll=0 ;  
     for (int rr = 0 ; rr <RR ; rr++){  
        pp = XdesM(rr,0) +I*XdesM(rr,1)+I*maxK*XdesM(rr,2);  
        ll = XdesM(rr,3) ;  
     // Rcpp::Rcout << "pp = " << pp << " XdesM(rr,3) = " << XdesM(rr,3) << std::endl;  
        p1[pp] += XdesM(rr,4) * Xlambda[ll] ;  
        			}  
       
     //****  
     // exponentiation and normalization  
       
     double tmp=0;  
       
     for (int ii = 0 ; ii<I;ii++){  
     for (int tt = 0 ;tt<TP;tt++){  
       
     tmp=0 ;  
     for (int kk = 0 ; kk < maxK ; kk++ ){  
        pp = ii +I*kk+I*maxK*tt;  
        probs[pp] = exp( p1[pp] ) ;  
        tmp += probs[pp] ;  
        	}  
     for (int kk = 0 ; kk < maxK ; kk++ ){  
        pp = ii +I*kk+I*maxK*tt;  
        probs[pp] = probs[pp] / tmp ;  
        	}  
     }  
     }  
          
     return wrap(probs) ;  
       
     //*************************************************      
     // OUTPUT              
                   
     // return Rcpp::List::create(    
     //    _["probs" ] = probs   
     //    ) ;  
END_RCPP
}




// declarations
extern "C" {
SEXP calc_slca_deriv( SEXP XdesM_, SEXP dimXdes_, SEXP Xlambda_, SEXP probs_, SEXP nik_, SEXP Nik_) ;
}

// definition

SEXP calc_slca_deriv( SEXP XdesM_, SEXP dimXdes_, SEXP Xlambda_, SEXP probs_, SEXP nik_, SEXP Nik_ ){
BEGIN_RCPP
  
       
     Rcpp::NumericMatrix XdesM(XdesM_);          
     Rcpp::NumericVector dimXdes(dimXdes_);        
     Rcpp::NumericVector Xlambda(Xlambda_);  
     Rcpp::NumericVector probs(probs_) ;  
     Rcpp::NumericVector nik(nik_) ;  
     Rcpp::NumericVector Nik(Nik_) ;  
       
     // $dimXdes  
     // [1]  6  4 21 19  
       
     int I= dimXdes[0] ;  
     int maxK= dimXdes[1] ;  
     int TP= dimXdes[2] ;  
     int Nlam = dimXdes[3] ;  
       
     int RR = XdesM.nrow() ;  
       
     // int PP = I * maxK * TP ;  
       
     Rcpp::NumericVector d1b(Nlam);  
     Rcpp::NumericVector d2b(Nlam);  
       
     int ii=0;  
     int hh=0;  
     int tt=0;  
     int ll=0;  
       
     //  # XdesM     [ii,kk,tt,ll, value ]   
     //        # probs  num [1:I, 1:maxK, 1:TP]  
     //        # n.ik  num [1:I, 1:maxK, 1:TP]  
     //        # N.ik  num [1:I,1:TP]  
     //        # Xdes  num [1:I, 1:maxK, 1:TP, 1:Nlam]           
       
       
     ///*********************  
     // First derivative  
       
     //  for (hh in 1:maxK){  
     //      t1 <- sum( Xdes[ , hh , , ll] * ( n.ik[ , hh , ] - probs[,hh,] * N.ik ) )  
     //       d1.b[ll] <- d1.b[ll] + t1  
     //			}  
     for (int rr = 0 ; rr <RR ; rr++){  
     	// # XdesM     [ii,kk,tt,ll, value ]   
     	ii = XdesM(rr,0);  
     	hh = XdesM(rr,1);  
     	tt = XdesM(rr,2);  
     	ll = XdesM(rr,3);  
     	// sum( Xdes[ , hh , , ll] * ( n.ik[ , hh , ] - probs[,hh,] * N.ik ) )  
     	d1b[ll] += XdesM(rr,4) * ( nik[ii+I*hh+I*maxK*tt] -  
     	      probs[ii+I*hh+I*maxK*tt] * Nik[ ii+I*tt ] ) ;  
     		}  
       
       
     ///*********************  
     // Second derivative  
       
     int NS = I*TP*Nlam ;  
     Rcpp::NumericVector tmp1(NS) ;  
     // tmp1 <- 0  
     // for (hh in 1:maxK ){  
     //   tmp1 <- tmp1 + probs[,hh,] * Xdes[,hh,,ll]  
     //		}			  
       
     // parameter ll; item i ; class tt  
     int vv=0;  
     for (int rr=0;rr<RR;rr++){  
        ii = XdesM(rr,0);  
        hh = XdesM(rr,1);  
        tt = XdesM(rr,2);  
        ll = XdesM(rr,3);  
        vv = ii + I*tt + I*TP*ll ;  
        tmp1[vv] += XdesM(rr,4) * probs[ii+I*hh+I*maxK*tt] ;  
        			}  
       
     // for (hh in 1:maxK){  
     //  t2 <- sum( Xdes[ , hh , , ll] * N.ik * probs[,hh,] *  
     //   ( Xdes[, hh , , ll ] - tmp1 ) )  
     // d2.b[ll] <- d2.b[ll] + t2  
     //		}  
       
     // int rr = 0 ;  
     for (int rr=0;rr<RR;rr++){  
        ii = XdesM(rr,0);  
        hh = XdesM(rr,1);  
        tt = XdesM(rr,2);  
        ll = XdesM(rr,3);  
        vv = ii + I*tt + I*TP*ll ;  
     //  t2 <- sum( Xdes[ , hh , , ll] * N.ik * probs[,hh,] *  
     //   ( Xdes[, hh , , ll ] - tmp1 ) )     
       d2b[ll] += XdesM(rr,4) * Nik[ii + I*tt ] * probs[ ii + I*hh + I*maxK*tt ] *   
       	              ( XdesM(rr,4) - tmp1[vv] ) ;  
     //   tmp1[vv] += XdesM(rr,4) * probs[ii+I*hh+I*maxK*tt] ;  
        			}  
       
        			  
     //*************************************************      
     // OUTPUT              
                   
      return Rcpp::List::create(    
         Rcpp::_["d1b" ] = d1b ,  
         Rcpp::_["d2b" ] = d2b 
                       ) ;  
END_RCPP
}



// declarations
extern "C" {
SEXP calc_Xdes( SEXP Xdes_, SEXP dimXdes_) ;
}

// definition

SEXP calc_Xdes( SEXP Xdes_, SEXP dimXdes_ ){
BEGIN_RCPP
  
       
     Rcpp::NumericVector XDES(Xdes_);          
     Rcpp::NumericVector dimXdes(dimXdes_);        
       
     // $dimXdes  
     // [1]  6  4 21 19  
       
     int I= dimXdes[0] ;  
     int maxK= dimXdes[1] ;  
     int TP= dimXdes[2] ;  
     int Nlam = dimXdes[3] ;  
     int RR = XDES.size() ;  
       
     Rcpp::NumericMatrix XdesM(RR,5) ;  
       
     int rr = 0 ;  
       
     int ind = 0 ;  
       
     for (int ii=0; ii<I;ii++){  
     for (int kk=0; kk <maxK ; kk++){  
     for ( int tt=0; tt<TP; tt++ ){   
     for ( int ll=0; ll<Nlam ; ll++ ){  
       
        rr=ii+I*kk+I*maxK*tt+I*maxK*TP*ll ;  
       
     // Rcpp::Rcout << "rr = " << rr << " XDES[rr] = " << XDES[rr] << std::endl;  
       
     if ( XDES[rr] != 0 ){  
          XdesM(ind,0) = ii ;	  
          XdesM(ind,1) = kk ;  
          XdesM(ind,2) = tt ;  
          XdesM(ind,3) = ll ;  
          XdesM(ind,4) = XDES[rr] ;  
          ind = ind + 1 ;  
          		}  
          	}  
     }  
     }  
     }  
       
       
     //*************************************************      
     // OUTPUT              
                   
     return Rcpp::List::create(    
         Rcpp::_["NXdesM"] = ind  ,  
         Rcpp::_["XdesM" ] = XdesM  
         ) ;  
END_RCPP
}






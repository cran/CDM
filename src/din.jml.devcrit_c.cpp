//  Code created: 2013-08-14 16:57:29


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
SEXP din_jml_devcrit_C( SEXP dat, SEXP datresp, SEXP latresp, SEXP guess, SEXP slip) ;
}

// definition

SEXP din_jml_devcrit_C( SEXP dat, SEXP datresp, SEXP latresp, SEXP guess, SEXP slip ){
BEGIN_RCPP
  
     /////////////////////////////////////  
     // INPUT  
     Rcpp::NumericMatrix DAT(dat);  
     Rcpp::NumericMatrix DATRESP(datresp) ;   
     Rcpp::NumericMatrix LATRESP(latresp) ;  
     Rcpp::NumericVector GUESS(guess) ;  
     Rcpp::NumericVector SLIP(slip) ;  
       
     // define row and column numbers  
     int N=DAT.nrow();  
     int I=DAT.ncol();  
     int AP=LATRESP.nrow();  
       
     // create output ghat matrix  
     NumericMatrix devcrit (N,AP) ;  
     NumericVector mincrit (N) ;  
     NumericVector indexcrit (N) ;  
     NumericVector rn (1) ;  
     mincrit.fill(-1);  
     devcrit.fill(1);  
       
     RNGScope scope;		// ensure RNG gets set/reset  
       
     for (int aa=0;aa<AP;aa++){ // begin attributes  
     for (int nn=0;nn<N;nn++){ // begin cases   
     for (int ii=0;ii<I;ii++){ // begin item loop  
     	if ( DATRESP(nn,ii)==1 ){  // begin if datresp == 1  
     		if ( (LATRESP(aa,ii) == 1 ) && ( DAT(nn,ii)==0 ) ){  
     		    devcrit(nn,aa) = devcrit(nn,aa) * SLIP[ii];  
     						}  
     		if ( (LATRESP(aa,ii) == 1 ) && ( DAT(nn,ii)==1 ) ){  
     		    devcrit(nn,aa) = devcrit(nn,aa) * (1-SLIP[ii]);  
     						}						  
     		if ( (LATRESP(aa,ii) == 0 ) && ( DAT(nn,ii)==1 ) ){  
     		    devcrit(nn,aa) = devcrit(nn,aa) * GUESS[ii] ;  
     						}	  
     		if ( (LATRESP(aa,ii) == 0 ) && ( DAT(nn,ii)==0 ) ){  
     		    devcrit(nn,aa) = devcrit(nn,aa) * (1-GUESS[ii]) ;  
     						}											  
     				} // end if datresp == 1				  
     			} // end loop over items ii	  
     if (mincrit[nn]<devcrit(nn,aa)){  
     	mincrit[nn]=devcrit(nn,aa) ;  
     	indexcrit[nn]=aa+1 ;  
     		}  
     	// handle ties		  
     	if (mincrit[nn]==devcrit(nn,aa)){  
     		rn(0)=R::runif(0,1);  
     		if (rn(0)>0.5){  
     			mincrit[nn]=devcrit(nn,aa) ;  
     			indexcrit[nn]=aa+1 ;  
     				}  
     			}		  
     	} // end cases	  
     	} // end attributes  
     	  
     //#### original R code  
     //        lat.aa <- matrix( latresp[aa,] , nrow=N , ncol=I , byrow=TRUE)  
     //        dev.crit[,aa] <- rowSums( slip * ( lat.aa - dat ) * ( lat.aa == 1 ) * dat.resp +   
     //                    guess * ( dat - lat.aa ) * ( lat.aa == 0 ) * dat.resp )  
       
       
     ///////////////////////////////////////  
     /// OUTPUT                  
       
       
     return Rcpp::List::create(
               Rcpp::_["devcrit"]=devcrit , 
               Rcpp::_["mincrit"]= mincrit ,  
     	       Rcpp::_["indexcrit"]=indexcrit 
                        ) ;  
     //	   _["matrk"]=MATRK  , _["indexmatr"]=INDEXMATR ) ;     
     // return List::create(_["yM"]=YM , _["wM"]=WM ) ;     
     
END_RCPP
}




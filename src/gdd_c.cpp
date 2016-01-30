

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
SEXP generalized_distance_method__C( SEXP data_, SEXP dataresp_, SEXP idealresp_, SEXP theta_, SEXP a_, SEXP b_) ;
}

// definition

SEXP generalized_distance_method__C( SEXP data_, SEXP dataresp_, SEXP idealresp_, SEXP theta_, SEXP a_, SEXP b_ ){
BEGIN_RCPP
  
     Rcpp::NumericMatrix data(data_);          
     Rcpp::NumericMatrix dataresp(dataresp_);          
     Rcpp::NumericMatrix idealresp(idealresp_);    
     Rcpp::NumericVector theta(theta_);    
     Rcpp::NumericVector a(a_);    
     Rcpp::NumericVector b(b_);    
       
     int I = idealresp.nrow() ;  
     int L = idealresp.ncol() ;  
     int N = data.nrow() ;  
       
     double ind = 0 ;  
     double dmin = 100 * I ;  
       
     Rcpp::NumericMatrix dist(N,L) ;  
     Rcpp::NumericVector est_skill(N) ;  
       
       
       
     for (int nn=0;nn<N;nn++){ // begin person nn  
         // int nn = 0 ;  
         dmin=100*I;  
         ind=0;  
       
         for (int ll=0; ll <L;ll++){  // begin skill class ll  
         for (int ii=0;ii<I;ii++){   //  begin item ii  
         if ( dataresp(nn,ii)==1 ){      
             //**************          
             // data=1 and idealresp=0  
             if ( ( data(nn,ii) == 1 ) & ( idealresp(ii,ll) == 0 ) ){  
                 //     1 - P0  = P1  
                  dist(nn,ll) += pow( 1 + exp( - ( b[ii] + a[ii] * theta[nn] ) ) , -1 ) ;  
                     }  
             //**************          
             // data=0 and idealresp=1          
             if ( ( data(nn,ii) == 0 ) & ( idealresp(ii,ll) == 1 ) ){  
                 //     1 - P1  = P0  
                  dist(nn,ll) += pow( 1 + exp( ( b[ii] + a[ii] * theta[nn] ) ) , -1 ) ;  
                     }  
                 }              
                 }       // end ii  
         if ( dmin > dist(nn,ll) ){  
             dmin = dist(nn,ll) ;  
             ind = ll +1 ;      
                     }      
             }   // end ll         
         est_skill[nn] = ind ;      
     }  // end nn      
           
           
     /////////////////////////////////////////////  
     // OUTPUT:  
      return Rcpp::List::create(  
         Rcpp::_["dist"] = dist ,  
         Rcpp::_["est_skill"] = est_skill  
                 ) ;  
                   
END_RCPP
}




// declarations
extern "C" {
SEXP ideal_resp_pattern__C( SEXP qmatrix_, SEXP skillspace_) ;
}

// definition

SEXP ideal_resp_pattern__C( SEXP qmatrix_, SEXP skillspace_ ){
BEGIN_RCPP
  
     /// model fit statistics  
       
     Rcpp::NumericMatrix qmatrix(qmatrix_);          
     Rcpp::NumericMatrix skillspace(skillspace_);    
       
     int I = qmatrix.nrow() ;  
     int K = skillspace.ncol() ;  
     int L = skillspace.nrow() ;  
       
     Rcpp::NumericMatrix idealresp(I,L) ;  
       
       
     for (int ii=0;ii<I;ii++){  
       
     for (int ll=0;ll<L;ll++){  
         idealresp(ii,ll) = 1 ;  
         for (int kk=0;kk<K;kk++){  
             // int kk = 0 ;  
             if ( ( qmatrix(ii,kk) == 1 ) & ( skillspace(ll,kk) == 0 ) ){  
                 idealresp(ii, ll) = 0 ;  
             }  
         }  
     }  
     }  
       
     return wrap( idealresp ) ;  

END_RCPP
}







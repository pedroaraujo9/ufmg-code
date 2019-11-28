#include <Rcpp.h>
#include <math.h>

using namespace Rcpp;

// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp 
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/
//

// [[Rcpp::export]]
NumericVector cppff(NumericVector y, double p) {
  
  NumericVector prob(y.length());
  
  for(int i = 0; i < y.length(); i++) {
    if(y[i] >= 10000) {
      prob[i] = 1;
    }else if(y[i] <= -10000) {
      prob[i] = 0;
    }else{
      if(y[i] < 0) {
        prob[i] = p*exp(y[i]*(1-p));
      }else{
        prob[i] =  1 - (1-p)*exp(-y[i]*p);
      }
    }
  }
  
  return prob;
}

// [[Rcpp::export]]
NumericVector cppgammas(int y, NumericVector gammas) {
  NumericVector res(2);
  
  res(0) = gammas(y-1);
  res(1) = gammas(y);
  
  return res;
}


// [[Rcpp::export]]
double logvero(NumericVector deltas, NumericMatrix x, NumericVector y, double p) {
  
  NumericVector gammasin = cumsum(exp(deltas));
  NumericVector gammas = NumericVector::create(-10000, 0);
  
  for(int i = 0; i < gammasin.length();i++) {
    gammas.push_back(gammasin[i]);
  }
  
  gammas.push_back(10000);
  
  
  NumericVector probs(y.length());
  for(int i = 0; i < y.length(); i++) {
    probs[i] = cppff(cppgammas(y[i], gammas)(1) - sum(x(i,_)*y[i]), p)(0) - cppff(cppgammas(y[i],gammas)(1) - sum(x(i,_)*y[i]),p)(0); 
  }
  
  
  return -sum(log(probs));
}



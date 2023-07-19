// [[Rcpp::depends(BH)]]

#include <Rcpp.h>
#include <boost/math/distributions/non_central_t.hpp> 
#include <unordered_map>

// [[Rcpp::export]]
double dnct(const double x, const double df, const double ncp) {
  boost::math::non_central_t dist(df, ncp);
  return pdf(dist, x);
}

// [[Rcpp::export]]
double pnct(const double q, const double df, const double ncp) {
  boost::math::non_central_t dist(df, ncp);
  return cdf(dist, q);
}


// [[Rcpp::export]]
double qnct(const double p, const double df, const double ncp) {
  boost::math::non_central_t dist(df, ncp);
  return quantile(dist, p);
}


// [[Rcpp::export]]
Rcpp::NumericVector dnct_vec(const Rcpp::NumericVector& x, 
                             const Rcpp::NumericVector& df, 
                             const Rcpp::NumericVector& ncp) {
  
  // Check that input vectors have the same dimensions
  if (x.size() != df.size() || x.size() != ncp.size()) {
    throw std::invalid_argument("Input vectors must have the same length");
  }
  
  int n = x.size();
  Rcpp::NumericVector result(n);
  
  for (int i = 0; i < n; ++i) {
    boost::math::non_central_t dist(df[i], ncp[i]);
    result[i] = pdf(dist, x[i]);
  }
  
  return result;
}




// [[Rcpp::export]]
Rcpp::List createDistributionList(const Rcpp::NumericVector& df, const Rcpp::NumericVector& ncp) {
  int n = df.size();
  Rcpp::List distList(n);
  
  for (int i = 0; i < n; ++i) {
    boost::math::non_central_t dist(df[i], ncp[i]);
    distList[i] = Rcpp::XPtr<boost::math::non_central_t>(&dist, true);
  }
  
  return distList;
}


// [[Rcpp::export]]
Rcpp::NumericMatrix evaluateDistributions(const Rcpp::List& distList, const Rcpp::NumericVector& x) {
  int n_dist = distList.size();
  int n_x = x.size();
  
  Rcpp::NumericMatrix result(n_x, n_dist);
  
  for (int i = 0; i < n_dist; ++i) {
    boost::math::non_central_t* distPtr = Rcpp::as<Rcpp::XPtr<boost::math::non_central_t>>(distList[i]);
    boost::math::non_central_t& dist = *distPtr;
    
    for (int j = 0; j < n_x; ++j) {
      result(j, i) = pdf(dist, x[j]);
    }
  }
  
  return result;
}

// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp 
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/
//


// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically 
// run after the compilation.
//

// /*** R
// # R code here
// */

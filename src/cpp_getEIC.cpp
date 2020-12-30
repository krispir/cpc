// [[Rcpp::plugins(cpp11)]]

#include <RcppArmadillo.h>
#include <Rcpp.h>
#include <vector>
#include <algorithm>
#include <math.h>

using namespace Rcpp;
using namespace std;

using vec_d = vector<double>;
using vec_i = vector<int>;


int lowerBound(vec_d &x, double val, int first, int length) {
  int half, mid;
  
  while (length > 0) {
    half = length >> 1;
    mid = first;
    mid += half;
    if (x[mid] < val){
      first = mid;
      first ++;
      length = length - half -1;
    }
    else length = half;
  }
  return first;
}


int upperBound(vec_d &x, double val, int first, int length) {
  int half, mid;
  
  while (length > 0) {
    half = length >> 1;
    mid = first;
    mid += half;
    if (val < x[mid]){
      length = half;
    }
    else {
      first = mid;
      first ++;
      length = length - half -1;
    }
  }
  return first;
}


int lowerBound(NumericVector &x, double val, int first, int length) {
  int half, mid;
  
  while (length > 0) {
    half = length >> 1;
    mid = first;
    mid += half;
    if (x[mid] < val){
      first = mid;
      first ++;
      length = length - half -1;
    }
    else length = half;
  }
  return first;
}


int upperBound(NumericVector &x, double val, int first, int length) {
  int half, mid;
  
  while (length > 0) {
    half = length >> 1;
    mid = first;
    mid += half;
    if (val < x[mid]){
      length = half;
    }
    else {
      first = mid;
      first ++;
      length = length - half -1;
    }
  }
  return first;
}


// [[Rcpp::export]]
vec_d getEIC_min(vec_d &mz, 
                 vec_d &intensity, 
                 vec_i &scan_idx, 
                 vec_d &mz_range, 
                 vec_i &scan_range)
{
  int nscantot = scan_idx.size();
  int nout;
  int i_first, i_last;
  int i, j;

  double total;
  
  // data checks
  
  if (mz_range.size() != 2) return vec_d(0);
  if (scan_range.size() != 2) return vec_d(0);
  
  if (scan_range.at(0) < 0) scan_range.at(0) = 0;
  if (scan_range.at(1) > nscantot) scan_range.at(1) = scan_idx.size()-1;
  
  nout = scan_range.at(1)-scan_range.at(0)+1;
  
  // output container
  vec_d eic(nout);
  
  // loop over scans
  for (i = scan_range.at(0); i <= scan_range.at(1); i++)
  {
    // reset sum
    total = 0;

    // loop over current scan
    // if (i == 0) i_first = 0; else i_first = scan_idx.at(i-1)+1;
    
    if (i == 0)
    {
      i_first = lowerBound(mz, mz_range.at(0), 0, scan_idx.at(i));
      i_last = upperBound(mz, mz_range.at(1), 0, scan_idx.at(i));
    } else
    {
      i_first = lowerBound(mz, mz_range.at(0), scan_idx.at(i-1)+1, 
                           scan_idx.at(i)-scan_idx.at(i-1));
      i_last = upperBound(mz, mz_range.at(1), scan_idx.at(i-1)+1, 
                          scan_idx.at(i)-scan_idx.at(i-1));
    }

    for (j = i_first; j <= i_last; j++)
    {
      // check if within mz range and add to total if it is
      if (mz.at(j) >= mz_range.at(0) && 
          mz.at(j) <= mz_range.at(1)) 
        total += intensity.at(j);
    }

    eic.at(i-scan_range.at(0)) = total;
  }
  
  return eic;
}

// [[Rcpp::export]]
NumericVector getEIC_Rcpp(NumericVector &mz, 
                          NumericVector &intensity, 
                          IntegerVector &scan_idx, 
                          NumericVector &mz_range, 
                          IntegerVector &scan_range)
{
  int nscantot = scan_idx.size();
  int nout;
  int i_first, i_last;
  int i, j;
  
  double total;
  
  // data checks
  
  if (mz_range.size() != 2) return NumericVector(0);
  if (scan_range.size() != 2) return NumericVector(0);
  
  if (scan_range.at(0) < 0) scan_range.at(0) = 0;
  if (scan_range.at(1) > nscantot) scan_range.at(1) = scan_idx.size()-1;
  
  nout = scan_range.at(1)-scan_range.at(0)+1;
  
  // output container
  NumericVector eic(nout);
  
  // loop over scans
  for (i = scan_range.at(0); i <= scan_range.at(1); i++)
  {
    // reset sum
    total = 0;
    
    // loop over current scan
    // if (i == 0) i_first = 0; else i_first = scan_idx.at(i-1)+1;
    
    if (i == 0)
    {
      i_first = lowerBound(mz, mz_range.at(0), 0, scan_idx.at(i));
      i_last = upperBound(mz, mz_range.at(1), 0, scan_idx.at(i));
    } else
    {
      i_first = lowerBound(mz, mz_range.at(0), scan_idx.at(i-1)+1, 
                           scan_idx.at(i)-scan_idx.at(i-1));
      i_last = upperBound(mz, mz_range.at(1), scan_idx.at(i-1)+1, 
                          scan_idx.at(i)-scan_idx.at(i-1));
    }
    
    for (j = i_first; j <= i_last; j++)
    {
      // check if within mz range and add to total if it is
      if (mz.at(j) >= mz_range.at(0) && 
          mz.at(j) <= mz_range.at(1)) 
        total += intensity.at(j);
    }
    
    eic.at(i-scan_range.at(0)) = total;
  }
  
  return eic;
}

//#include <vector>
//#include <map>
//#include <algorithm>
#include <RcppArmadillo.h>
//#include <armadillo>
#include <cmath>
//#include <string>
//[[Rcpp::depends(RcppArmadillo)]]

//#include <Rcpp.h>
using namespace Rcpp;
using namespace std;
using namespace arma;

#define Max(a, b) a>=b?a:b;

void update(arma::vec& bit, int n, int i, double val){
  i += 1;
  while(i<=n){
    bit[i]+=val;
    i += i&-i;
  }
}

double psum(arma::vec& bit, int i){
  i += 1;
  double sum = 0;
  while(i>0){
    sum += bit[i];
    i -= (i&-i);
  }
  return sum;
}

double dxysum(arma::vec& x, arma::vec& y, arma::uvec& idy){
  // x is already sorted
  int n = x.size();
  double sumdxy = 0.0, sumy = arma::sum(y);
  arma::vec ranky(n);
  for(int i=0;i<n;++i) ranky[idy[i]] = i;
  arma::vec bit1 = arma::zeros(n+1), bit2 = arma::zeros(n+1);
  arma::vec rsm=arma::zeros(n), rsym=arma::zeros(n);
  for(int i=n-1;i>-1;--i){
    rsm[i] = psum(bit1,n-1-ranky[i]);
    rsym[i] = psum(bit2,n-1-ranky[i]);
    update(bit1,n,n-1-ranky[i],1);
    update(bit2,n,n-1-ranky[i],y[i]);
  }
  bit1.zeros(); bit2.zeros();
  double sm, sym;
  for(int i=0;i<n;++i){
    sm = 2*(psum(bit1,ranky[i]) + rsm[i]) - n;
    sym = 2*(psum(bit2,ranky[i]) + rsym[i]) - sumy;
    sumdxy += (2*x[i]*y[i]*sm - 2*x[i]*sym);
    update(bit1,n,ranky[i],1);
    update(bit2,n,ranky[i],y[i]);
  }
  return(sumdxy);
}


//[[Rcpp::export]]
double fastdcor(arma::vec x,arma::vec y){
  //x=arma::abs(x-arma::mean(x));

  int n = x.size();
  double d1,d2,d3;
  d1 = n*(n+0.0); d2 = n; d3 = 1;

  arma::uvec idx = sort_index(x);
  arma::vec temp = x; for(int i=0;i<n;++i) x[i] = temp[idx[i]];
  temp = y; for(int i=0;i<n;++i) y[i] = temp[idx[i]];
  arma::uvec idy = sort_index(y); //x[idx[i]] = i smallest value in x

  double sumx = arma::sum(x), sumy = arma::sum(y);
  double csumx = 0.0, csumy = 0.0;
  double amm, bmm;
  arma::vec am(n), bm(n);
  double sumdxy = dxysum(x,y,idy);
  for(int i=0;i<n;++i){
    am[i] = x[i]*(i*2-n)/n - (2*csumx-sumx)/n;
    bm[idy[i]] = y[idy[i]]*(i*2-n)/n - (2*csumy-sumy)/n;
    csumx += x[i]; csumy += y[idy[i]];
  }
  amm = arma::mean(am); bmm = arma::mean(bm);
  double dcov = sumdxy/d1 - 2*arma::sum(am%bm)/d2 + amm*bmm/d3;

  double sumdxx = 2*arma::sum(arma::square(x))*n - 2*sumx*sumx;
  double sumdyy = 2*arma::sum(arma::square(y))*n - 2*sumy*sumy;
  double dx2 = sumdxx/d1 - 2*arma::sum(am%am)/d2 + amm*amm/d3;
  double dy2 = sumdyy/d1 - 2*arma::sum(bm%bm)/d2 + bmm*bmm/d3;

  double dcor = dcov/std::sqrt(dx2*dy2);
  if(isnan(dcor)||dx2<0||dy2<0){
    return 0;
  }else return dcor;//dcor;
}

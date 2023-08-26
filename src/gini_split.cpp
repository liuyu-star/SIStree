//#include "quickie.h"
#include <RcppArmadillo.h>
//#include <armadillo>
#include <cmath>
//#include <string>
//[[Rcpp::depends(RcppArmadillo)]]

//#include <Rcpp.h>
using namespace Rcpp;
using namespace std;
using namespace arma;

//[[Rcpp::export]]
List gini_split(arma::mat X,arma::vec y,int minleaf, int numLabels){
  int M=y.size(), N=X.n_cols;
  double *sorted_X, minGini, maxGini, bcval=1, bcvalj=1;
  double cGini, cGini0, gr, gl;
  int i=0, j=0, cl, nl, mi;
  int *diff_y_l, *diff_y_r, *diff_y, *sorted_y, bcvar=1;
  arma::uvec idx;
  arma::vec varGini(N);
  //varGini.fill(1e+10);


  diff_y_l = new int[numLabels];
  diff_y_r = new int[numLabels];
  diff_y = new int[numLabels];
  sorted_y = new int[M];
  sorted_X =  new double[M];

  for(nl=0;nl<numLabels;nl++){
    diff_y[nl]=0;
  }

  for(i = 0;i<M;i++) {
    cl = y[i];
    diff_y[cl-1]++;
  }


cGini0=1e+10;
 for(j = 0;j<N;j++){

minGini=1e+10;
maxGini=1e-10;
 for(nl=0;nl<numLabels;nl++){
   diff_y_l[nl] = 0;
   diff_y_r[nl] = diff_y[nl];
 }

 idx = arma::sort_index(X.col(j));
 for(i = 0;i<M;i++) {
   sorted_X[i] = X(idx[i],j);
   sorted_y[i] = y[idx[i]];
 }

 for(mi = 0; mi<minleaf; mi++){
   //cl=sorted_y[j];
   //diff_y_l[--cl]++;
   //diff_y_r[cl]--;
   cl=sorted_y[mi]-1;
   diff_y_l[cl]++;
   diff_y_r[cl]--;
 }


 for(i = minleaf;i<M-minleaf;i++){
   //cl=sorted_y[j];
   //diff_y_l[--cl]++;
   //diff_y_r[cl]--;
   cl=sorted_y[i]-1;
   diff_y_l[cl]++;
   diff_y_r[cl]--;

   gr = 0;
   gl = 0;
   for(nl=0;nl<numLabels;nl++) {
     gl+=diff_y_l[nl]*diff_y_l[nl];
     gr+=diff_y_r[nl]*diff_y_r[nl];
   }
   gl = 1 - gl/((i+1)*(i+1));
   gr = 1 - gr/((M-i-1)*(M-i-1));
   cGini = (i+1)*gl/M + (M-i-1)*gr/M;
   //ch = gl*pow((j+1)/((j+1)-1),2) + gr*pow((M-j-1)/((M-j-1)-1),2);
   //ch = gl*pow(j+1,3)/pow(j,2) + gr*pow(M-j-1,3)/pow(M-j-2,2);

   if (cGini<minGini){
     if (fabs(sorted_X[i+1]-sorted_X[i])>1e-15){
       minGini=cGini;
     }
   }

   if (cGini>maxGini){
     if (fabs(sorted_X[i+1]-sorted_X[i])>1e-15){
       maxGini=cGini;
       bcvalj = 0.5*(sorted_X[i+1]+sorted_X[i]);
     }
   }
 }

     varGini[j]=minGini;
     if (minGini<cGini0){
       bcvar = j+1;
       bcval = bcvalj;
       cGini0=minGini;
   }

  }


  delete[] diff_y_l;
  delete[] diff_y_r;
  delete[] diff_y;

  delete[] sorted_y;
  delete[] sorted_X;

  return List::create(_["BestCutVar"]= bcvar,
                      _["BestCutVal"]= bcval,
                      _["varGini"]= varGini
  );
}

#ifndef _ITEMFUNCTIONS_H
#define _ITEMFUNCTIONS_H

#include <RcppArmadillo.h>
using namespace Rcpp;
using std::vector;

double antilogit(const double *);

double vecsum(const vector<double> &);

SEXP vec2mat(vector<double> &, const int &, const int &);

const double ABS_MAX_Z = 35;

vector<double> ProbTrace(const S4 &, const vector<double> &);

arma::mat Info(const S4 &, const vector<double> &);

#endif

#ifndef _ITEMFUNCTIONS_H
#define _ITEMFUNCTIONS_H

#include <RcppArmadillo.h>
using namespace Rcpp;
using std::vector;

double antilogit(const double *);

double vecsum(const vector<double> &);

SEXP vec2mat(vector<double> &, const int &, const int &);

const double ABS_MAX_Z = 35;

void itemTrace(vector<double> &, vector<double> &, const vector<double> &, const double *,
        const NumericMatrix &, const double *, const double *, const NumericVector &);

void P_dich(vector<double> &, const vector<double> &, const NumericMatrix &,
    const NumericVector &, const int &, const int &);

void P_graded(vector<double> &, const vector<double> &,
    const NumericMatrix &, const NumericVector &, const int &,
    const int &, const int &, const int &, const int &);

void P_nominal(vector<double> &, const vector<double> &,
    const NumericMatrix &, const NumericVector &, const int &,
    const int &, const int &, const int &, const int &);

void P_nested(vector<double> &, const vector<double> &,
    const NumericMatrix &, const int &, const int &, const int &,
    const int &);

void P_comp(vector<double> &, const vector<double> &,
    const NumericMatrix &, const int &, const int &);

#endif

#include "item_functions.h"

RcppExport SEXP FI_all(SEXP Rpars, SEXP RTheta, SEXP Rwhich_not_answered, SEXP Rtotal_info)
{
    BEGIN_RCPP

    const List pars(Rpars);
    const NumericMatrix Theta(RTheta);
    const vector<int> which_not_answered = as< vector<int> >(Rwhich_not_answered);
    const int total_info = as<int>(Rtotal_info);
    const int nfact = Theta.ncol();

    for(int pick = 0; pick < which_not_answered.size(); ++pick){
    	int whc = which_not_answered[pick];


    }



    END_RCPP
}
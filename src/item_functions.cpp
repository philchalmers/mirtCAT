#include "item_functions.h"
// The majority of these functions are modification of code from the 
// mirt package, version 1.7.1. December 22, 2014

// hack fix
void R_init_mirtCAT(DllInfo* info) {
    R_registerRoutines(info, NULL, NULL, NULL, NULL);
    R_useDynamicSymbols(info, TRUE);
}

double antilogit(const double *x)
{
    double ret;
    if(*x > 998.0) ret = 1.0;
    else if(*x < -998.0) ret = 0.0;
    else ret = 1.0 / (1.0 + exp(-1.0 * (*x)));
    return(ret);
}

double vecsum(const vector<double> &x)
{
    double sum = 0.0;
    const int size = x.size();
    for(int i = 0; i < size; ++i)
        sum += x[i];
    return(sum);
}

SEXP vec2mat(vector<double> &x, const int &nrow, const int &ncol) 
{
  NumericVector output = wrap(x);
  output.attr("dim") = Dimension(nrow, ncol);
  return(output);
}

void itemTrace(double &P, double &Pstar, const vector<double> &a, const double *d,
        const vector<double> &Theta, const int &nfact, const double *g, const double *u)
{
    if((*u - *g) > 0){
        double z = *d;
        for (int j = 0; j < nfact; ++j)
            z += a[j] * Theta[j];
        if(z > ABS_MAX_Z) z = ABS_MAX_Z;
        else if(z < -ABS_MAX_Z) z = -ABS_MAX_Z;
        Pstar = 1.0 / (1.0 + exp(-z));
        P = *g + (*u - *g) * Pstar;
    }
}

void P_dich(vector<double> &P, const vector<double> &par, const vector<double> &Theta,
    const int &nfact)
{
    const int len = par.size();
    const double utmp = par[len-1];
    const double gtmp = par[len-2];
    const double g = antilogit(&gtmp);
    const double u = antilogit(&utmp);
    const double d = par[len-3];

    if((u - g) > 0){
        double z = d;
        for (int j = 0; j < nfact; ++j)
            z += par[j] * Theta[j];
        if(z > ABS_MAX_Z) z = ABS_MAX_Z;
        else if(z < -ABS_MAX_Z) z = -ABS_MAX_Z;
        P[1] = g + (u - g) /(1.0 + exp(-z));
        P[0] = 1.0 - P[1];
    }
}

void P_graded(vector<double> &P, const vector<double> &par,
    const vector<double> &Theta, 
    const int &nfact, const int &nint, const int &israting)
{
    const int parsize = par.size();
    vector<double> a(nfact);
    for(int i = 0; i < nfact; ++i) a[i] = par[i];
    vector<double> d(nint, 0.0);
    if(israting){
        const double t = par[parsize-1];
        for(int i = nfact; i < parsize - 1; ++i)
            d[i - nfact] = par[i] + t;
    } else {
        for(int i = nfact; i < parsize; ++i)
            d[i - nfact] = par[i];
    }
    const double nullzero = 0.0, nullone = 1.0;
    vector<double> Pk(nint + 2);
    const int Pk_size = Pk.size();
    Pk[0] = 1.0;
    for(int i = 0; i < nint; ++i){
        double tmp1, tmp2;
        itemTrace(tmp1, tmp2, a, &d[i], Theta, nfact, &nullzero, &nullone);
        Pk[i+1] = tmp2;
    }
    int which = nint;
    for(int i = (Pk_size-2); i >= 0; --i){
        P[which] = Pk[i] - Pk[i+1];
        if(P[which] < 1e-50) P[which] = 1e-50;
        else if((1.0 - P[which]) < 1e-50) P[which] = 1.0 - 1e-50;
        --which;
    }
}

void P_nominal(vector<double> &P, const vector<double> &par,
    const vector<double> &Theta, const int &nfact, const int &ncat,
    const int &israting, const int &returnNum)
{
    vector<double> a(nfact), ak(ncat), d(ncat);
    for(int i = 0; i < nfact; ++i)
        a[i] = par[i];
    for(int i = 0; i < ncat; ++i){
        ak[i] = par[i + nfact];
        if(israting){
            if(i)
                d[i] = par[i + nfact + ncat] + par[par.size()-1];
        } else {
            d[i] = par[i + nfact + ncat];
        }
    }
    vector<double> Num(ncat);
    vector<double> z(ncat);
    double Den = 0.0;
    double innerprod = 0.0;
    
    for(int j = 0; j < nfact; ++j)
        innerprod += Theta[j] * a[j];
    for(int j = 0; j < ncat; ++j)
        z[j] = ak[j] * innerprod + d[j];
    double maxz = *std::max_element(z.begin(), z.end());
    for(int j = 0; j < ncat; ++j){
        z[j] = z[j] - maxz;
        if(z[j] < -ABS_MAX_Z) z[j] = -ABS_MAX_Z;
        Num[j] = exp(z[j]);
        Den += Num[j];
    }
    if(returnNum){
    	for(int j = 0; j < ncat; ++j)
	        P[j] = Num[j];
    } else {
	    for(int j = 0; j < ncat; ++j)
	        P[j] = Num[j] / Den;
	}
}

void P_nested(vector<double> &P, const vector<double> &par,
    const vector<double> &Theta, const int &nfact, const int &ncat,
    const int &correct)
{
	const int par_size = par.size();
    vector<double> dpar(nfact+3), npar(par_size - nfact - 3, 1.0);
    for(int i = 0; i < nfact+3; ++i)
        dpar[i] = par[i];
    for(int i = nfact+3; i < par_size; ++i)
        npar[i - (nfact+3) + nfact] = par[i];
    vector<double> Pd(2), Pn(ncat-1);
    P_dich(Pd, dpar, Theta, nfact);
    P_nominal(Pn, npar, Theta, nfact, ncat-1, 0, 0);
    int k = 0;
    for(int i = 0; i < ncat; ++i){
        if((i+1) == correct){
            P[k] = Pd[1];
            --k;
        } else {
            P[k] = Pd[0] * Pn[k];
        }
        ++k;
    }
}

void P_comp(vector<double> &P, const vector<double> &par,
    const vector<double> &Theta, const int &nfact)
{
    vector<double> a(nfact), d(nfact);
    for(int j = 0; j < nfact; ++j){
        a[j] = par[j];
        d[j] = par[j+nfact];
    }
    const double gtmp = par[nfact*2];
    const double g = antilogit(&gtmp);
    P[1] = 1.0;
    for(int j = 0; j < nfact; ++j)
        P[1] = P[1] * (1.0 / (1.0 + exp(-(a[j] * Theta[j] + d[j]))));
    P[1] = g + (1.0 - g) * P[1];
    if(P[1] < 1e-20) P[1] = 1e-50;
    else if (P[1] > 1.0 - 1e-50) P[1] = 1.0 - 1e-50;
    P[0] = 1.0 - P[1];
}

vector<double> ProbTrace(const S4 &item, const vector<double> &Theta)
{
    const int nfact = Theta.size();
    int itemclass = as<int>(item.slot("itemclass"));
    int correct = 0;
    if(itemclass == 8) correct = as<int>(item.slot("correctcat"));
    int ncat = as<int>(item.slot("ncat"));
    vector<double> par = as< vector<double> >(item.slot("par"));
    vector<double> P(ncat);

    /*
        1 = dich
        2 = graded
        3 = gpcm
        4 = nominal
        5 = grsm
        6 = rsm
        7 = partcomp
        8 = nestlogit
    */
    switch(itemclass){
        case 1 :
            P_dich(P, par, Theta, nfact);
            break;
        case 2 :
            P_graded(P, par, Theta, nfact, ncat-1, 0);
            break;
        case 3 :
            P_nominal(P, par, Theta, nfact, ncat, 0, 0);
            break;
        case 4 :
            P_nominal(P, par, Theta, nfact, ncat, 0, 0);
            break;
        case 5 :
            P_graded(P, par, Theta, nfact, ncat-1, 1);
            break;
        case 6 :
            P_nominal(P, par, Theta, nfact, ncat, 1, 0);
            break;
        case 7 :
            P_comp(P, par, Theta, nfact);
            break;
        case 8 :
            P_nested(P, par, Theta, nfact, ncat, correct);
            break;
        case 9 :
            break;
        default :
            Rprintf("Traceline function not supported.\n");
            break;
    }

    return(P);
}

void I_dich(arma::mat &info_mat, const S4 &item, 
	const vector<double> &par, const vector<double> &Theta, const int &nfact)
{
	vector<double> a(nfact);
    for(int i = 0; i < nfact; ++i) a[i] = par[i];
    const int len = par.size();
    const double utmp = par[len-1];
    const double gtmp = par[len-2];
    const double d = par[len-3];
    const double g = antilogit(&gtmp);
    const double u = antilogit(&utmp);
    double P = 0, Ps = 0;
    itemTrace(P, Ps, a, &d, Theta, nfact, &g, &u);
    double Q = 1.0 - P;
    double PQ = (1.0 - Ps) * Ps;
    for(int i = 0; i < nfact; ++i){
        double dP1 = (u-g) * a[i] * PQ;
        for(int j = 0; j < nfact; ++j){
            if(i < j){
                double dP2 = (u-g) * a[j] * PQ;
                info_mat(i,j) = dP1 * dP2 / Q + dP1 * dP2 / P;
                info_mat(j,i) = info_mat(i,j);
            } else {
                info_mat(i,i) = dP1 * dP1 / Q + dP1 * dP1 / P;
            }
        }
    }
    
}

void I_graded(arma::mat &info_mat, const S4 &item, 
	const vector<double> &par, const vector<double> &Theta, const int &nfact)
{
    vector<double> P = ProbTrace(item, Theta);
    const int P_size = P.size();
    vector<double> Pstar(P_size-1);
    double accum = P[P_size-1];
    Pstar[P_size-2] = accum;
    if(P_size > 2){
        for(int i = P_size - 2; i > 0; --i){
            accum += P[i];
            Pstar[i-1] = accum;
        }
    }
    double PQ = 0.0;
    for(int i = 0; i < P_size - 1; ++i)
        PQ += Pstar[i] * (1.0 - Pstar[i]);
    for(int i = 0; i < nfact; ++i){
        for(int j = 0; j < nfact; ++j){
            if(i <= j)
                info_mat(i,j) = par[i] * par[j] * PQ;
            if(i != j)
                info_mat(j,i) = info_mat(i,j);
        }
    } 
}

void I_nominal(arma::mat &info_mat, const S4 &item, 
	const vector<double> &par, const vector<double> &Theta, const int &nfact,
	const int &israting)
{
	const int ncat = as<int>(item.slot("ncat"));
	vector<double> a(nfact), ak(ncat), d(ncat);
    for(int i = 0; i < nfact; ++i)
        a[i] = par[i];
    for(int i = 0; i < ncat; ++i)
        ak[i] = par[i + nfact];
    vector<double> Num(ncat), P(ncat);
    double Den = 0.0;
    P_nominal(Num, par, Theta, nfact, ncat, israting, 1);
    for(int i = 0; i < ncat; ++i) Den += Num[i];
    for(int i = 0; i < ncat; ++i) P[i] = Num[i]/Den;
    for(int k = 0; k < nfact; ++k){
	    for(int j = 0; j < nfact; ++j){
	    	if(k <= j){
		    	double I2 = 0.0;
		    	vector<double> dP1(ncat);
		    	double inner = 0.0;
		    	for(int i = 0; i < ncat; ++i)
		    		inner += Num[i] * ak[i] * a[j];
		    	for(int i = 0; i < ncat; ++i)
		    		dP1[i] = ak[i] * a[j] * P[i] - P[i] * inner / Den;
		    	if(k != j){
		    		vector<double> dP2(ncat);
		    		for(int i = 0; i < ncat; ++i)
		    			inner += Num[i] * ak[i] * a[k];
			    	for(int i = 0; i < ncat; ++i)
			    		dP2[i] = ak[i] * a[k] * P[i] - P[i] * inner / Den;
			    	for(int i = 0; i < ncat; ++i)
			    		I2 += dP1[i] * dP2[i] / P[i];
			    	info_mat(j, k) = I2;
			    	info_mat(k, j) = I2;
		    	} else {
		    		for(int i = 0; i < ncat; ++i)
			    		I2 += dP1[i] * dP1[i] / P[i];
			    	info_mat(j, j) = I2;
		    	}
		    }
	    }
	}
}

void I_comp(arma::mat &info_mat, const S4 &item, 
	const vector<double> &par, const vector<double> &Theta, const int &nfact)
{
	Rprintf("Information for partcomp models not implemented yet\n");
}

void I_nested(arma::mat &info_mat, const S4 &item, 
	const vector<double> &par, const vector<double> &Theta, const int &nfact)
{
	Rprintf("Information for nested-logit models not implemented yet.\nUse nominal model instead.\n");
}

arma::mat Info(const S4 &item, const vector<double> &Theta){
    const int nfact = Theta.size();
    arma::mat info_mat = arma::zeros<arma::mat>(nfact, nfact); 
    int itemclass = as<int>(item.slot("itemclass")); 
    vector<double> par = as< vector<double> >(item.slot("par"));

    switch(itemclass){
    	case 1 :
            I_dich(info_mat, item, par, Theta, nfact);
            break;
        case 2 :
            I_graded(info_mat, item, par, Theta, nfact);
            break;
        case 3 :
            I_nominal(info_mat, item, par, Theta, nfact, 0);
            break;
        case 4 :
            I_nominal(info_mat, item, par, Theta, nfact, 0);
            break;
        case 5 :
            I_graded(info_mat, item, par, Theta, nfact);
            break;
        case 6 :
            I_nominal(info_mat, item, par, Theta, nfact, 1);
            break;
        case 7 :
            I_comp(info_mat, item, par, Theta, nfact);
            break;
        case 8 :
            I_nested(info_mat, item, par, Theta, nfact);
            break;
        case 9 :
            break;
        default :
            Rprintf("Infomation function not supported.\n");
            break;
    }
    return(info_mat);
}

RcppExport SEXP ItemInfo(SEXP Ritem, SEXP RTheta){
    const S4 item(Ritem);
    const vector<double> Theta = as< vector<double> >(RTheta);
    arma::mat ret = Info(item, Theta);
    return(wrap(ret));
}

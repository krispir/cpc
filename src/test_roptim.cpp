// [[Rcpp::plugins(cpp11)]]

#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]

#include <Rcpp.h>

#include <roptim.h>
// [[Rcpp::depends(roptim)]]

#include <cmath> // std::pow

using namespace Rcpp;
using namespace std;
using namespace roptim;

using vec_d = std::vector<double>;
using vec_i = std::vector<int>;

class EMGFit : public Functor {
private:
  arma::vec si;
  arma::vec st;
  arma::vec wt;
  arma::vec seed;
  arma::vec pars_plus_h;
  arma::vec pars_minus_h;

  unsigned int npeaks;
  unsigned int npars;
  unsigned int nvals;
  unsigned int nx;
  double h = 1e-5;

public:
  EMGFit(arma::vec &_si,
         arma::vec &_st,
         arma::vec &_wt,
         arma::vec &_seed,
         unsigned int _npeaks,
         double _h = 1e-5)
  {
    si = _si;
    st = _st;
    wt = _wt;
    seed = _seed;
    npeaks = _npeaks;
    npars = _seed.size()/_npeaks;
    nx = _si.size();
    nvals = _seed.size();
    h = _h;

    pars_plus_h = arma::vec(this->nvals);
    pars_minus_h = arma::vec(this->nvals);
  }

  double operator()(const arma::vec &pars) override {
    /* calculate sum of squares */
    double SS = 0.0;
    double sum = 0.0;

    for (unsigned int i = 0; i < this->nx; i++)
    {
      sum = 0.0;

      // A * emg[x_i]
      for (unsigned int j = 0; j < this->npeaks; j++)
      {
        sum += exp(pars.at(3+j*this->npars)) * // A
          this->emg(this->st.at(i),
                    pars.at(j*this->npars),
                    exp(pars.at(1+j*this->npars)),
                    exp(pars.at(2+j*this->npars))); // emg[x_i]
      }

      SS += this->wt.at(i) * std::pow(this->si.at(i) - sum, 2);
    }

    return SS;
  }

  void Gradient(const arma::vec &pars, arma::vec &gr) override {
    if (gr.size() != this->nvals)
    {
      gr = arma::zeros<arma::vec>(this->nvals);
    }

    for (unsigned int i = 0; i < this->nvals; i++)
    {
      this->pars_plus_h = pars;
      this->pars_minus_h = pars;

      // pars_plus_h.at(i) = pars.at(i) + this->h;
      // pars_minus_h.at(i) = pars.at(i) - this->h;
      this->pars_plus_h.at(i) = pars.at(i) + this->h*pars.at(i);
      this->pars_minus_h.at(i) = pars.at(i) - this->h*pars.at(i);

      gr.at(i) = ((*this)(this->pars_plus_h)-(*this)(this->pars_minus_h))/(2*this->h*pars.at(i));
    }
  }

  double emg(double x, double u, double s, double l)
  {
    return exp(log(l)+l*(u+((l*s*s)/2)-x) +
               R::pnorm((u+l*s*s-x)/s, 0.0, 1.0, false, true));
  }
};



double emg(double x, double u, double s, double l)
{
  return exp(log(l)+l*(u+((l*s*s)/2)-x) +
             R::pnorm((u+l*s*s-x)/s, 0.0, 1.0, false, true));
}

// [[Rcpp::export]]
vec_d emgfun(vec_d &x, vec_d &pars, unsigned int npeaks)
{
  unsigned int nx = x.size();
  unsigned int npars = pars.size()/npeaks;
  
  vec_d y(nx, 0.0);
  
  for (unsigned int i = 0; i < nx; i++)
  {
    for (unsigned int j = 0; j < npeaks; j++)
    {
      y.at(i) += pars.at(3+j*npars) * 
        emg(x.at(i), pars.at(j*npars), pars.at(1+j*npars), pars.at(2+j*npars));
    }
  }
  
  return y;
}


// [[Rcpp::export]]
arma::vec c_emgfit_bfgs(arma::vec &si, arma::vec &st, arma::vec &wt,
                        arma::vec seed,
                        int np, bool hess = false, int trace = 0, 
                        double h = 1e-5)
{
  EMGFit fit(si, st, wt, seed, np);
  
  Roptim<EMGFit> opt("BFGS");
  
  opt.control.trace = trace;
  
  opt.set_hessian(hess);
  
  opt.minimize(fit, seed);
  
  if (trace)
  {
    Rcpp::Rcout << "-------------------------" << std::endl;
    opt.print();
  }
  
  return opt.par();
}

// [[Rcpp::export]]
arma::vec c_emgfit_lbfgsb(arma::vec &si, arma::vec &st, arma::vec &wt,
                               arma::vec seed, arma::vec lower, arma::vec upper, 
                               int np, bool hess = false, int trace = 0, 
                               double h = 1e-5)
{
  EMGFit fit(si, st, wt, seed, np, h);
  
  Roptim<EMGFit> opt("L-BFGS-B");
  
  opt.set_lower(lower);
  opt.set_upper(upper);
  opt.control.trace = trace;
  opt.set_hessian(hess);
  
  opt.minimize(fit, seed);
  
  if (trace)
  {
    Rcpp::Rcout << "-------------------------" << std::endl;
    opt.print();
  }
  
  return opt.par();
}

// [[Rcpp::export]]
arma::vec c_emgfit_nmead(arma::vec &si, arma::vec &st, arma::vec &wt,
                          arma::vec seed, 
                          int np, bool hess = false, int trace = 0, 
                          double h = 1e-5)
{
  EMGFit fit(si, st, wt, seed, np, h);
  
  Roptim<EMGFit> opt("Nelder-Mead");
  opt.control.trace = trace;
  opt.set_hessian(hess);
  
  opt.minimize(fit, seed);
  
  if (trace)
  {
    Rcpp::Rcout << "-------------------------" << std::endl;
    opt.print();
  }
  
  return opt.par();
}

// [[Rcpp::export]]
arma::vec c_emgfit_sann(arma::vec &si, arma::vec &st, arma::vec &wt,
                         arma::vec seed, 
                         int np, bool hess = false, int trace = 0, 
                         double h = 1e-5)
{
  EMGFit fit(si, st, wt, seed, np, h);
  
  Roptim<EMGFit> opt("SANN");
  opt.control.trace = trace;
  opt.set_hessian(hess);
  
  opt.minimize(fit, seed);
  
  if (trace)
  {
    Rcpp::Rcout << "-------------------------" << std::endl;
    opt.print();
  }
  
  return opt.par();
}

// [[Rcpp::export]]
arma::vec c_emgfit_cg(arma::vec &si, arma::vec &st, arma::vec &wt,
                        arma::vec seed, 
                        int np, bool hess = false, int trace = 0, 
                        double h = 1e-5)
{
  EMGFit fit(si, st, wt, seed, np, h);
  
  Roptim<EMGFit> opt("CG");
  opt.control.trace = trace;
  opt.set_hessian(hess);
  
  opt.minimize(fit, seed);
  
  if (trace)
  {
    Rcpp::Rcout << "-------------------------" << std::endl;
    opt.print();
  }
  
  return opt.par();
}

// [[Rcpp::export]]
arma::vec c_emgfit(arma::vec &si, arma::vec &st, arma::vec &wt, arma::vec seed, 
                   arma::vec lower, arma::vec upper, 
                   int np, bool hess = false, int trace = 0, 
                   double h = 1e-5, const std::string method = "Nelder-Mead")
{
  EMGFit fit(si, st, wt, seed, np, h);
  Roptim<EMGFit> opt(method);
  
  if (method == "L-BFGS-B" && 
      (lower.size() != seed.size() || upper.size() != seed.size()))
  {
    Rcpp::stop("L-BFGS-B require bounds");
  } else if (method == "L-BFGS-B")
  {
    opt.set_lower(lower);
    opt.set_upper(upper);
  }
  
  opt.control.trace = trace;
  
  opt.set_hessian(hess);
  
  opt.minimize(fit, seed);
  
  if (trace)
  {
    Rcpp::Rcout << "-------------------------" << std::endl;
    opt.print();
  }
  
  return opt.par();
}
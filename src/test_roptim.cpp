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

/*******************************************************************************
 * Original EMGFit class                                               
 * 
 * This is the one that is implemented with Chromatogram at the moment 
 * 
 ******************************************************************************/

// class EMGFit : public Functor {
// private:
//   arma::vec si;
//   arma::vec st;
//   arma::vec wt;
//   arma::vec seed;
//   arma::vec pars_plus_h;
//   arma::vec pars_minus_h;
// 
//   unsigned int npeaks;
//   unsigned int npars;
//   unsigned int nvals;
//   unsigned int nx;
//   double h = 1e-5;
// 
// public:
//   EMGFit(arma::vec &_si,
//          arma::vec &_st,
//          arma::vec &_wt,
//          arma::vec &_seed,
//          unsigned int _npeaks,
//          double _h = 1e-5)
//   {
//     si = _si;
//     st = _st;
//     wt = _wt;
//     seed = _seed;
//     npeaks = _npeaks;
//     npars = _seed.size()/_npeaks;
//     nx = _si.size();
//     nvals = _seed.size();
//     h = _h;
// 
//     pars_plus_h = arma::vec(this->nvals);
//     pars_minus_h = arma::vec(this->nvals);
//   }
// 
//   double operator()(const arma::vec &pars) override {
//     /* calculate sum of squares */
//     double SS = 0.0;
//     double sum = 0.0;
// 
//     for (unsigned int i = 0; i < this->nx; i++)
//     {
//       sum = 0.0;
// 
//       // A * emg[x_i]
//       for (unsigned int j = 0; j < this->npeaks; j++)
//       {
//         sum += exp(pars.at(3+j*this->npars)) * // A
//           this->emg(this->st.at(i),
//                     pars.at(j*this->npars),
//                     exp(pars.at(1+j*this->npars)),
//                     exp(pars.at(2+j*this->npars))); // emg[x_i]
//       }
// 
//       SS += this->wt.at(i) * std::pow(this->si.at(i) - sum, 2);
//     }
// 
//     return SS;
//   }
// 
//   void Gradient(const arma::vec &pars, arma::vec &gr) override {
//     if (gr.size() != this->nvals)
//     {
//       gr = arma::zeros<arma::vec>(this->nvals);
//     }
// 
//     for (unsigned int i = 0; i < this->nvals; i++)
//     {
//       this->pars_plus_h = pars;
//       this->pars_minus_h = pars;
// 
//       // pars_plus_h.at(i) = pars.at(i) + this->h;
//       // pars_minus_h.at(i) = pars.at(i) - this->h;
//       this->pars_plus_h.at(i) = pars.at(i) + this->h*pars.at(i);
//       this->pars_minus_h.at(i) = pars.at(i) - this->h*pars.at(i);
// 
//       gr.at(i) = ((*this)(this->pars_plus_h)-(*this)(this->pars_minus_h))/(2*this->h*pars.at(i));
//     }
//   }
// 
//   double emg(double x, double u, double s, double l)
//   {
//     return exp(log(l)+l*(u+((l*s*s)/2)-x) +
//                R::pnorm((u+l*s*s-x)/s, 0.0, 1.0, false, true));
//   }
// };

/*******************************************************************************
 * Development EMGFit class
 * 
 * I am here trying to apply a scaling factor to the optimization parameters
 * in order to make the optimization more homogenous.
 * 
 ******************************************************************************/

class EMGFit2 : public Functor {
private:
  arma::vec si;
  arma::vec st;
  arma::vec wt;
  arma::vec seed;
  arma::vec lower;
  arma::vec upper;
  arma::vec pars_plus_h;
  arma::vec pars_minus_h;
  
  std::string method;
  
  unsigned int npeaks;
  unsigned int npars;
  unsigned int nvals;
  unsigned int nx;
  double h = 1e-5;
  
  bool range_scaled = false;
  
  // double min_mu, max_mu;
  // double min_sigma, max_sigma;
  // double min_lambda, max_lambda;
  // double min_area, max_area;
  // vec_d scale_mu;
  // vec_d scale_sigma;
  // vec_d scale_lambda;
  // vec_d scale_area;
  
public:
  EMGFit2(arma::vec &_si,
          arma::vec &_st,
          arma::vec &_wt,
          arma::vec &_seed,
          arma::vec &_lower,
          arma::vec &_upper,
          unsigned int _npeaks,
          double _h = 1e-5,
          const std::string &_method = "Not-NM",
          bool _range_scaled = false)
  {
    si = _si;
    st = _st;
    wt = _wt;
    seed = _seed;
    lower = _lower;
    upper = _upper;
    
    npeaks = _npeaks;
    npars = _seed.size()/_npeaks;
    nx = _si.size();
    nvals = _seed.size();
    h = _h;
    method = _method;
    range_scaled = _range_scaled;
    
    pars_plus_h = arma::vec(this->nvals);
    pars_minus_h = arma::vec(this->nvals);
  }
  
  double emg(const unsigned int i, const double u, const double s, const double l)
  {
    return std::exp(std::log(l)+l*(u+((l*s*s)/2)-this->st.at(i)) +
                    R::pnorm((u+l*s*s-this->st.at(i))/(s), 0.0, 1.0, false, true));
  }
  
  double objFun(const arma::vec &pars) {
    /* calculate sum of squares */
    double SS = 0.0;
    double sum = 0.0;
    
    for (unsigned int i = 0; i < this->nx; i++)
    {
      sum = 0.0;
      
      // A * emg[x_i]
      for (unsigned int j = 0; j < this->npeaks; j++)
      {
        sum += std::exp(pars.at(3+j*this->npars)) * // A
          this->emg(i,
                    pars.at(j*this->npars),
                    pars.at(1+j*this->npars),
                    std::exp(pars.at(2+j*this->npars))); // emg[x_i]
      }
      
      SS += this->wt.at(i) * std::pow(this->si.at(i) - sum, 2);
    }
    
    return SS;
  }
  
  double objFun_rs(const arma::vec &pars)
  {
    /* calculate sum of squares */
    double SS = 0.0;
    double sum = 0.0;
    
    for (unsigned int i = 0; i < this->nx; i++)
    {
      sum = 0.0;
      
      // A * emg[x_i]
      for (unsigned int j = 0; j < this->npeaks; j++)
      {
        sum += ((pars.at(3+j*this->npars) * (this->upper(3+j*this->npars) - this->lower(3+j*this->npars))) + this->lower(3+j*this->npars)) * // A
          this->emg(i,
                    (pars.at(j*this->npars) * (this->upper(j*this->npars) - this->lower(j*this->npars))) + this->lower(j*this->npars),
                    (pars.at(1+j*this->npars) * (this->upper(1+j*this->npars) - this->lower(1+j*this->npars))) + this->lower(1+j*this->npars),
                    (pars.at(2+j*this->npars) * (this->upper(2+j*this->npars) - this->lower(2+j*this->npars))) + this->lower(2+j*this->npars)); // emg[x_i]
      }
      
      SS += this->wt.at(i) * std::pow(this->si.at(i) - sum, 2);
    }
    
    return SS;
  }
  
  double penFun(const arma::vec &pars) {
    double pen = 0.0;
    
    for (unsigned int i = 0; i < this->nvals; i++)
    {
      if (this->range_scaled &&
          ((pars.at(i) * (this->upper(i) - this->lower(i))) + this->lower.at(i) > this->upper.at(i) ||
          (pars.at(i) * (this->upper(i) - this->lower(i))) + this->lower.at(i) < this->lower.at(i)))
      {
        pen += std::numeric_limits<double>::infinity();
      } else if (pars.at(i) > upper.at(i) || pars.at(i) < lower.at(i))
      {
        pen += std::numeric_limits<double>::infinity();
      }
    }
    
    return pen;
  }
  
  double operator()(const arma::vec &pars) override {
    if (this->method.compare("Nelder-Mead") == 0)
    {
      if (this->range_scaled)
      {
        return this->objFun_rs(pars) + this->penFun(pars);
      } else
      {
        return this->objFun(pars) + this->penFun(pars);
      }
    } else
    {
      if (this->range_scaled)
      {
        return this->objFun_rs(pars);
      } else
      {
        return this->objFun(pars);
      }
    }
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
      
      gr.at(i) = (this->objFun(this->pars_plus_h)-this->objFun(this->pars_minus_h))/(2*this->h*pars.at(i));
    }
  }
};


/*******************************************************************************
 * External EMG fit function for development
 ******************************************************************************/

// [[Rcpp::export]]
arma::vec c_emgfit(arma::vec &si, arma::vec &st, arma::vec &wt, arma::vec seed, 
                   arma::vec &lower, arma::vec &upper,
                   int np, bool hess = false, int trace = 0, 
                   double h = 1e-5, const std::string method = "Nelder-Mead",
                   const bool range_scaled = false)
{
  // EMGFit2(arma::vec &_si,
  //         arma::vec &_st,
  //         arma::vec &_wt,
  //         arma::vec &_seed,
  //         arma::vec &_lseed,
  //         arma::vec &_useed,
  //         unsigned int _npeaks,
  //         double _h = 1e-5,
  //         const std::string &_method = "Not-NM")
  EMGFit2 fit(si, st, wt, seed, lower, upper, np, h, method, range_scaled);
  
  Roptim<EMGFit2> opt(method);
  
  if (method == "L-BFGS-B" &&
      (lower.size() != seed.size() || upper.size() != seed.size()))
  {
    Rcpp::stop("L-BFGS-B require bounds");
  } else if (method == "L-BFGS-B")
  {
    opt.set_lower(lower);
    opt.set_upper(upper);
  }

  // if (method.compare("Nelder-Mead") == 0)
  // {
  //   opt.control.maxit = 2000;
  // }
  
  opt.control.trace = trace;

  opt.set_hessian(hess);

  opt.minimize(fit, seed);

  if (trace)
  {
    Rcpp::Rcout << std::endl;
    opt.print();
    Rcpp::Rcout << std::endl;
  }
  
  return opt.par();
}

/*******************************************************************************
 * External EMG function for plotting
 ******************************************************************************/

double emg(double x, double u, double s, double l)
{
  return exp(log(l)+l*(u+((l*s*s)/2)-x) +
             R::pnorm((u+l*s*s-x)/s, 0.0, 1.0, false, true));
}

/*******************************************************************************
 * External EMG trace generator for development and plotting
 ******************************************************************************/

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

/*******************************************************************************
 * Old functions that are incorporated in c_emgfit()
 ******************************************************************************/

// arma::vec c_emgfit_bfgs(arma::vec &si, arma::vec &st, arma::vec &wt,
//                         arma::vec seed,
//                         int np, bool hess = false, int trace = 0, 
//                         double h = 1e-5)
// {
//   EMGFit2 fit(si, st, wt, seed, np);
//   
//   Roptim<EMGFit2> opt("BFGS");
//   
//   opt.control.trace = trace;
//   
//   opt.set_hessian(hess);
//   
//   opt.minimize(fit, seed);
//   
//   if (trace)
//   {
//     Rcpp::Rcout << "-------------------------" << std::endl;
//     opt.print();
//   }
//   
//   return opt.par();
// }
// 
// arma::vec c_emgfit_lbfgsb(arma::vec &si, arma::vec &st, arma::vec &wt,
//                                arma::vec seed, arma::vec lower, arma::vec upper, 
//                                int np, bool hess = false, int trace = 0, 
//                                double h = 1e-5)
// {
//   EMGFit2 fit(si, st, wt, seed, np, h);
//   
//   Roptim<EMGFit2> opt("L-BFGS-B");
//   
//   opt.set_lower(lower);
//   opt.set_upper(upper);
//   opt.control.trace = trace;
//   opt.set_hessian(hess);
//   
//   opt.minimize(fit, seed);
//   
//   if (trace)
//   {
//     Rcpp::Rcout << "-------------------------" << std::endl;
//     opt.print();
//   }
//   
//   return opt.par();
// }
// 
// arma::vec c_emgfit_nmead(arma::vec &si, arma::vec &st, arma::vec &wt,
//                           arma::vec seed, 
//                           int np, bool hess = false, int trace = 0, 
//                           double h = 1e-5)
// {
//   EMGFit2 fit(si, st, wt, seed, np, h);
//   
//   Roptim<EMGFit2> opt("Nelder-Mead");
//   opt.control.trace = trace;
//   opt.set_hessian(hess);
//   
//   opt.minimize(fit, seed);
//   
//   if (trace)
//   {
//     Rcpp::Rcout << "-------------------------" << std::endl;
//     opt.print();
//   }
//   
//   return opt.par();
// }
// 
// arma::vec c_emgfit_sann(arma::vec &si, arma::vec &st, arma::vec &wt,
//                          arma::vec seed, 
//                          int np, bool hess = false, int trace = 0, 
//                          double h = 1e-5)
// {
//   EMGFit2 fit(si, st, wt, seed, np, h);
//   
//   Roptim<EMGFit2> opt("SANN");
//   opt.control.trace = trace;
//   opt.set_hessian(hess);
//   
//   opt.minimize(fit, seed);
//   
//   if (trace)
//   {
//     Rcpp::Rcout << "-------------------------" << std::endl;
//     opt.print();
//   }
//   
//   return opt.par();
// }
// 
// arma::vec c_emgfit_cg(arma::vec &si, arma::vec &st, arma::vec &wt,
//                         arma::vec seed, 
//                         int np, bool hess = false, int trace = 0, 
//                         double h = 1e-5)
// {
//   EMGFit2 fit(si, st, wt, seed, np, h);
//   
//   Roptim<EMGFit2> opt("CG");
//   opt.control.trace = trace;
//   opt.set_hessian(hess);
//   
//   opt.minimize(fit, seed);
//   
//   if (trace)
//   {
//     Rcpp::Rcout << "-------------------------" << std::endl;
//     opt.print();
//   }
//   
//   return opt.par();
// }
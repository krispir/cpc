// [[Rcpp::plugins(cpp11)]]

// #include <RcppArmadillo.h>
// ***[[Rcpp::depends(RcppArmadillo)]]

#include <Rcpp.h>
// #include <bits/stdc++.h>
#include <deque>

// #include <roptim.h>
// ***[[Rcpp::depends(roptim)]]

#include <cmath> // std::pow
#include <numeric> // std::iota
#include <iomanip> // for outputting tables

using namespace Rcpp;
using namespace std;
// using namespace roptim;

using vec_d = std::vector<double>;
using vec_i = std::vector<int>;

/********************
 * utility function *
 ********************/

// [[Rcpp::export]]
vec_i LinearSpacedArray(int a, int b, int N)
{
    int h;
    int Nout;
    
    if (N <= 1)
    {
        return vector<int>(1,a);
    }
    
    if (b-a+1 <= N)
    {
        Nout = b-a+1;
        h = 1;
    } else
    {
        Nout = N;
        h = (b - a) / (N-1);
    }
    
    vec_i xs(Nout);
    vec_i::iterator x;
    
    int val;
    
    for (x = xs.begin(), val = a; x != xs.end(); ++x, val += h) {
        *x = val;
    }
    
    if (xs.at(0) != a) xs.at(0) = a; // ensure first point is equal to a
    if (xs.at(Nout-1) != b) xs.at(Nout-1) = b; // ensure last point is equal to b
    
    return xs;
}


double interpolate_x(int x1, int x2, double y1, double y2, double yval)
{
    return (((yval - y1) * (x2-x1) / (y2 - y1))) + x1;
}

double interpolate_x(vec_i &x, vec_d &y, double yval)
{
    return (((yval - y[0]) * (x[1]-x[0]) / (y[1] - y[0]))) + x[0];
}

double interpolate_y(int x1, int x2, double y1, double y2, double xval)
{
    return (((y2 - y1) / (x2 - x1)) * (xval - x1)) + y1;
}

double interpolate_y(vec_i &x, vec_d &y, double xval)
{
    return (((y[1] - y[0]) / (x[1] - x[0])) * (xval - x[0])) + y[0];
}

int which_min_double(vec_d &v, int a, int b)
{
    int nv = v.size();
    int min = a;
    int c;
    
    if (a < 0) a = 0;
    if (b > nv-1) b = nv-1;
    
    if (a > b)
    {
        c = a;
        a = b;
        b = c;
    }
    
    for (int i = a; i <= b; i++) 
        if (v[i] < v[min]) min = i;
    
    return min;
}

int which_max_double(vec_d &v, int a, int b)
{
    int nv = v.size();
    int max = a;
    int c;
    
    if (a < 0) a = 0;
    if (b > nv-1) b = nv-1;
    
    if (a > b)
    {
        c = a;
        a = b;
        b = c;
    }
    
    for (int i = a; i <= b; i++)
    {
        if (v[i] > v[max]) max = i;
    }
    
    return max;
}

int which_min(vec_d &v, int a, int b)
{
    int nv = v.size();
    int min = a;
    int c;
    
    if (a < 0) a = 0;
    if (b > nv-1) b = nv-1;
    
    if (a > b)
    {
        c = a;
        a = b;
        b = c;
    }
    
    for (int i = a; i <= b; i++) 
    {
        if (v[i] < v[min]) min = i;
    }
    
    return min;
}

int which_min(vec_i &v, int a, int b)
{
    int nv = v.size();
    int min = a;
    int c;
    
    if (a < 0) a = 0;
    if (b > nv-1) b = nv-1;
    
    if (a > b)
    {
        c = a;
        a = b;
        b = c;
    }
    
    for (int i = a; i <= b; i++)
    {
        if (v[i] < v[min])
            min = i;
    }
    
    return min;
}

int which_max(vec_d &v, int a, int b)
{
    int nv = v.size();
    int max = a;
    int c;
    
    if (a < 0) a = 0;
    if (b > nv-1) b = nv-1;
    
    if (a > b)
    {
        c = a;
        a = b;
        b = c;
    }
    
    for (int i = a; i <= b; i++)
    {
        if (v[i] > v[max]) max = i;
    }
    
    return max;
}

int which_max(vec_i &v, int a, int b)
{
    int nv = v.size();
    int max = a;
    int c;
    
    if (a < 0) a = 0;
    if (b > nv-1) b = nv-1;
    
    if (a > b)
    {
        c = a;
        a = b;
        b = c;
    }
    
    for (int i = a; i <= b; i++)
    {
        if (v[i] > v[max]) max = i;
    }
    
    return max;
}

int next_max(vec_d &v, int a, int b, int d)
{
    int nv = v.size();
    int max;
    int i;
    int c;
    
    if (d < 0)
    {
        d = -1;
    } else
    {
        d = 1;
    }
    
    if (a < 0) a = 0;
    if (b > nv-1) b = nv-1;
    
    if (a > b)
    {
        c = a;
        a = b;
        b = c;
    }
    
    if (d < 0)
    {
        i = b;
    } else
    {
        i = a;
    }
    
    max = i;
    while(i >= a && i <= b)
    {
        if (v.at(i) > v.at(max))
        {
            max = i;
        } else if (v.at(i) < v.at(max))
        {
            break;
        }
        
        i += d;
    }
    
    return max;
}

int next_max(vec_i &v, int a, int b, int d)
{
    int nv = v.size();
    int max;
    int i;
    int c;
    
    if (d < 0)
    {
        d = -1;
    } else
    {
        d = 1;
    }
    
    if (a < 0) a = 0;
    if (b > nv-1) b = nv-1;
    
    if (a > b)
    {
        c = a;
        a = b;
        b = c;
    }
    
    if (d < 0)
    {
        i = b;
    } else
    {
        i = a;
    }
    
    max = i;
    while(i >= a && i <= b)
    {
        if (v.at(i) > v.at(max))
        {
            max = i;
        } else if (v.at(i) < v.at(max))
        {
            break;
        }
        
        i += d;
    }
    
    return max;
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
vec_d c_emgfun(vec_d &x, vec_d &pars, unsigned int npeaks)
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

/*******************
 * data structures *
 *******************/
struct ChromatogramOptions
{
    double apex_thresh = 0.0;
    double liftoff = 0.0;
    double touchdown = 0.5;
    int min_inf_pts = 2;
    int w = 5;
    int p = -1;
    int output = 0;
    int fit_emg = 1;
    int fit_only_vip = 1;
    int fit_hess = 0;
    int pts_per_peak = 20;
    int min_pts_per_peak = 20;
    int max_pts_per_peak = 40;
    int min_shoulder_pts = 3;
    int min_rounded_pts = 3;
    double fit_rel_lim = 0.05;
    double reltol;
    double abstol;
    double alpha;
    double gamma;
    double rho;
    double sigma;
    int maxit;
    int maxeval;
    
    std::string fit_method = "Nelder-Mead";
    
    ChromatogramOptions(const double apex_thresh_ = 0.0,
                        const double liftoff_ = 0.0,
                        const double touchdown_ = 0.5,
                        const int w_ = 5,
                        const int p_ = -1,
                        const int output_ = 0,
                        const int fit_emg_ = 1,
                        const int fit_only_vip_ = 1,
                        const int fit_hess_ = 0,
                        const double fit_rel_lim_ = 0.05,
                        const int pts_per_peak_ = 30,
                        const int min_shoulder_pts_ = 3,
                        const int min_rounded_pts_ = 3,
                        const double reltol_ = 1e-8,
                        const double abstol_ = 1.0e35,
                        const double alpha_ = 1.0,
                        const double gamma_ = 2.1,
                        const double rho_ = 0.75,
                        const double sigma_ = 0.5,
                        const int maxit_ = 2000,
                        const int maxeval_ = 2000)
    {
        this->apex_thresh = apex_thresh_;
        this->liftoff = liftoff_;
        this->touchdown = touchdown_;
        this->w = w_;
        this->p = p_;
        this->output = output_;
        this->fit_emg = fit_emg_;
        this->fit_only_vip = fit_only_vip_;
        this->fit_hess = fit_hess_;
        this->fit_rel_lim = fit_rel_lim_;
        this->pts_per_peak = pts_per_peak_;
        this->min_shoulder_pts = min_shoulder_pts_;
        this->min_rounded_pts = min_rounded_pts_;
        this->reltol = reltol_;
        this->abstol = abstol_;
        this->alpha = alpha_;
        this->gamma = gamma_;
        this->rho = rho_;
        this->sigma = sigma_;
        this->maxit = maxit_;
        this->maxeval = maxeval_;
    }
};

struct Peak
{
    // TODO
    
    int id = -1;
    
    // baseline expansion results
    int apex = -1, adj_apex = -1, atyp = -1;
    double finf = -1, tinf = -1;
    int fexp = -1, texp = -1;
    double fthr = -1, tthr = -1;
    int fblb = -1, tblb = -1;
    int fpkb = -1, tpkb = -1;
    int ftyp = -1, ttyp = -1;
    
    // emg fit results
    double emg_mu = -1.0;
    double emg_sigma = -1.0;
    double emg_lambda = -1.0;
    double emg_area = -1.0;
    int emg_conv = -1;
    
    // Peak(_id, 
    //      _apex, _adj_apex, 
    //      _atyp,
    //      _finf, _tinf,
    //      _fexp, _texp,
    //      _fthr, _tthr,
    //      _fblb, _tblb,
    //      _fpkb, _tpkb,
    //      _ftyp, _ttyp)
    // {
    //     id = _id;
    //     apex = _apex;
    //     adj_apex = _adj_apex;
    //     atyp = _atyp;
    //     finf = _finf;
    //     tinf = _tinf;
    //     fexp = _fexp;
    //     texp = _texp;
    //     fthr = _fthr;
    //     tthr = _tthr;
    //     fblb = _fblb;
    //     tblb = _tblb;
    //     fpkb = _fpkb;
    //     tpkb = _tpkb;
    //     ftyp = _ftyp;
    //     ttyp = _ttyp;
    // }
};

struct PeakClusters
{
    // TODO
};

struct PeakTable
{
    // peak characteristics determined by the baseline expansion algorithm
    vec_i apex;         // apex point
    vec_i atyp;         // apex type (1 = normal
                        //            2 = front shoulder apex 
                        //            3 = tail shoulder peak
                        //            4 = lhs rounded apex
                        //            5 = rhs rounded peak)
    vec_i adj_apex;     // adjusted apex points
    vec_d finf, tinf;   // front and tail inflection points
    vec_i fexp, texp;   // front and tail expansion points
    vec_d fthr, tthr;   // front and tail expansion thresholds
    vec_i fblb, tblb;   // front and tail baseline bounds
    vec_i fpkb, tpkb;   // front and tail peak bounds
    vec_i ftyp, ttyp;   // front and tail peak bound types (0 = baseline
                        //                                  1 = valley
                        //                                  2 = shoulder
                        //                                  3 = rounded)
    vec_d area;         // peak area by trapezoid method between peak bounds
    vec_d inf_area;     // area between inflection points
    
    // peak characteristics determined by emg deconvolution
    vec_d emg_mu;       // EMG fitted mu
    vec_d emg_sigma;    // EMG fitted sigma
    vec_d emg_lambda;   // EMG fitted lambda
    vec_d emg_area;     // EMG fitted area
    vec_i emg_conv;     // EMG fitting convergence
    
    vec_i remove_these_later;
    vec_i remove_these_now;
    
    int npeaks;
    int vip = -1;
    int output = 0;
    
    void init_peaks(vec_i &_apex);
    void remove_peak(int _peak);
    void remove_tagged_peaks();
    
    void print_remove_these_now();
    void print_remove_these_later();
    void summary();
    void print_clusters();
    
    bool check_boundary_overlap(int first, int next);
    
    deque< vec_i > clust;
    deque< vec_i > final_clust;
};


/*******************************************************************************
 * 
 * Data types used in Nelder-Mead minimizer
 * 2021-01-18 Kristian Pirttilä
 * 
 ******************************************************************************/

/* Point data struct */
struct Vertice {
public:
    vec_d par;
    double fval = 0.0;
    
    Vertice(){}
    
    Vertice(unsigned int npar_)
    {
        this->par = vec_d(npar_);
    }
    
    Vertice(vec_d &par_)
    {
        this->par = par_;
    }
    
    void R_print()
    {
        Rcpp::Rcout << "pars : {";
        for (std::size_t i = 0; i < this->par.size(); i++)
        {
            Rcpp::Rcout << this->par.at(i);
            if (i < this->par.size()-1) Rcpp::Rcout << ", ";
        }
        Rcpp::Rcout << "} -> fval = " << this->fval << std::endl; 
    }
    
    // operator overloads
    bool operator<(const Vertice &v2) const
    {
        return this->fval < v2.fval;
    }
    
    bool operator<=(const Vertice &v2) const
    {
        return this->fval <= v2.fval;
    }
    
    bool operator>(const Vertice &v2) const
    {
        return this->fval > v2.fval;
    }
    
    bool operator>=(const Vertice &v2) const
    {
        return this->fval >= v2.fval;
    }
    
    bool operator==(const Vertice &v2) const
    {
        return this->fval == v2.fval;
    }
};

/* Simplex data struct */
struct Simplex {
public:
    
    /*****************
     * Simplex states 
     *****************/
    
    vector<Vertice> verts;
    
    Vertice vcen;
    Vertice vr;
    Vertice ve;
    Vertice vc;
    Vertice vs;
    Vertice vopt;
    
    /*********************
     * Nelder-Mead params
     *********************/
    
    unsigned int npar;
    unsigned int maxit;
    unsigned int maxeval;
    
    unsigned int citer;
    unsigned int ceval;
    
    double alpha = 1.0;
    double gamma = 2.0;
    double rho = 0.5;
    double sigma = 0.5;
    
    double reltol;
    double abstol;
    
    int titer;
    int teval;
    int tfval;
    int tpval;
    
    
    /**********************
     * Nelder-Mead methods
     **********************/
    
    // interfaces
    
    
    
    // proc methods
    void sort();
    // void find_extreme_vertices();
    void calculate_centroid();
    void reflect(const unsigned int iv_, const double alpha_);
    void expand(const Vertice &v_, const double gamma_);
    void contract(const unsigned int iv_, const double rho_);
    void shrink(const double sigma_);
    
    
    /***************
     * constructors
     ***************/
    
    
    Simplex(){}
    
    Simplex(unsigned int npar_)
    {
        this->verts = vector<Vertice>(npar_ + 1, Vertice(npar_));
        this->npar = npar_;
    }
    
    Simplex(vec_d &pars_, vec_d &stepsize)
    {
        // initialize the simplex
        this->npar = pars_.size();
        Vertice initv(this->npar);
        initv.par = pars_;
        
        this->verts = vector<Vertice>(this->npar + 1, initv);
        
        // calculate initial simplex vertices based on the given stepsize
        for (unsigned int i = 1; i < this->npar+1; i++)
        {
            this->verts.at(i).par.at(i-1) += stepsize.at(i-1);
        }
        
        this->vcen = initv;
        this->vr = initv;
        this->ve = initv;
        this->vc = initv;
        this->vs = initv;
        this->vopt = initv;
    }
    
    
    /*****************
     * output methods
     *****************/
    
    
    void R_print()
    {
        Rcpp::Rcout << "Simplex with " << this->npar << " variables and "
                    << this->verts.size() << " vertices." << std::endl
                    << "Vertices:" << std::endl;
        for (unsigned int i = 0; i < this->verts.size(); i++)
            this->verts.at(i).R_print();
        Rcpp::Rcout << "Centroid:" << std::endl;
        this->vcen.R_print();
    }
    
    void R_trace()
    {
        Rcpp::Rcout << setprecision(10) << setw(20) << right << this->verts.at(this->npar).fval;
        Rcpp::Rcout << " : ";
        Rcpp::Rcout << setprecision(10) << setw(20) << left << this->verts.at(0).fval;
    }
    
};

// Sort simplex such that p[0] has the lowest p->fval and p[npar-1] has the 
// highest p->fval
void Simplex::sort()
{
    std::sort(this->verts.begin(), this->verts.end());
}

// Calculate the centroid of the simplex
void Simplex::calculate_centroid()
{
    vec_d parsum(this->npar, 0.0);
    
    // determine max and min params
    for (unsigned int i = 0; i < this->npar; i++) // loop over vertices
    {
        for (unsigned int j = 0; j < this->npar; j++) // loop over pars
        {
            parsum.at(j) += this->verts.at(i).par.at(j);
        }
    }
    
    // set centroid pars
    for (unsigned int i = 0; i < this->npar; i++)
    {
        this->vcen.par.at(i) = parsum.at(i)/this->npar;
    }
}

void Simplex::reflect(const unsigned int iv_, const double alpha_)
{
    // REFLECTION: x_r = x_0 + alpha(x_0 - x_(n+1)) with alpha > 0; DEF 1.0
    
    for (unsigned int i = 0; i < this->npar; i++)
    {
        this->vr.par.at(i) = 
            this->vcen.par.at(i) + alpha_ * (this->vcen.par.at(i) - this->verts.at(iv_).par.at(i));
    }
}

void Simplex::expand(const Vertice &v_, const double gamma_)
{
    // 4 EXPANSION: x_e = x_0 + gamma(x_r - x_0) with gamma > 1; DEF 2.0
    
    for (unsigned int i = 0; i < this->npar; i++)
    {
        this->ve.par.at(i) = 
            this->vcen.par.at(i) + gamma_ * (v_.par.at(i) - this->vcen.par.at(i));
    }
}

void Simplex::contract(const unsigned int iv_, const double rho_)
{
    // 5 CONTRACTION: x_c = x_0 + rho(x_ihi - x_0) with 0 < rho <= 0.5; DEF 0.5
    
    for (unsigned int i = 0; i < this->npar; i++)
    {
        this->vc.par.at(i) = 
            this->vcen.par.at(i) + rho_ * (this->verts.at(iv_).par.at(i) - this->vcen.par.at(i));
    }
}

void Simplex::shrink(const double sigma_)
{
    // 6 SHRINK: (x_2:x_n) = x_1 + sigma(x_i - x_1) sigma DEF 0.5
    
    for (unsigned int i = 1; i < this->npar+1; i++) // loop over vertices
    {
        for (unsigned int j = 0; j < this->npar; j++) // loop over pars
        {
            this->verts.at(i).par.at(j) = 
                this->verts.at(0).par.at(j) + sigma_ * (this->verts.at(i).par.at(j) - this->verts.at(0).par.at(j));
        }
    }
}

/*******************************************************************************
 * 
 * Minimizer class implementation
 * 2021-01-18 Kristian Pirttilä
 * 
 * Implementation of a generic minimizer
 * 
 * Input data:
 * 
 * - Parameter seed -> vec_d(npar)
 * - Objective function (Functor)
 * 
 ******************************************************************************/

/* Nelder-Mead param struct */
struct MinimizerControl {
public:
    int npar; // dimentionality of minimization problem
    int maxit = 500; // max iterations
    int maxeval = 500;
    
    int trace = 1; // output
    int method; // minimization method (0 = Nelder-Mead, ...)
    
    // Nelder-Mead params
    double alpha = 1.0; // reflection coeff (>0)
    double gamma = 2.0; // expansion coeff (>1)
    double rho = 0.5; // contraction coeff (0<rho<=0.5)
    double sigma = 0.5; // shrink coeff ()
    
    double ptol = std::sqrt(std::numeric_limits<double>::epsilon()); // tolerance for parameter shift
    double ftol;
    double eps = std::numeric_limits<double>::epsilon();
    double reltol = 1e-8;
    double abstol = R_NegInf;
    
    int keep = 0;
    
    // constructors
    MinimizerControl() {}
    
    MinimizerControl(const int method_)
    {
        this->method = method_;
        
        if (this->method == 0) // Nelder-Mead
        {
            this->maxit = 10;
        } else
        {
            this->maxit = 10;
        }
    }
    
    // methods
    void R_print()
    {
        Rcpp::Rcout << "Method ";
        if (this->method == 0)
        {
            Rcpp::Rcout << "Nelder-Mead minimizer";
        } else
        {
            Rcpp::Rcout << "Unknown";
        }
        Rcpp::Rcout << "." << std::endl;
    }
};

/* Nelder-Mead minimizer class implementation */
template <typename Derived>
class Minimizer {
public:
    // minimizer functions
    
    // minimizer states
    int citer = 0; // iteration counds
    int ceval = 0; // fn call counts
    
    // termination params
    int tfval = 0;
    int tvert = 0;
    int titer = 0;
    int teval = 0;
    
    double fmin = -1.0;
    double rfdiff = -1.0;
    double rpdiff = -1.0;
    
    /* Nelder-Mead vars */
    Simplex sim; // Current simplex during minimization
    Vertice vopt; // Optimal params after running the minimizer
    vector<Simplex> hist; // Simplex history (if hist = 1)
    std::string action; // Action output
    
    /* process params */
    MinimizerControl control;
    
    
    /* constructors */
    Minimizer(int method_)
    {
        this->control = MinimizerControl(method_);
    }
    
    Minimizer(MinimizerControl control_) : control(control_) {}
    
    /******************************
     * minimizer interface methods
     ******************************/
    
    /* Nelder-Mead interface methods */
    
    void nmmin(Derived &fn, vec_d &pars);
    void nmmin(Derived &fn, vec_d &pars, double stepsize);
    void nmmin(Derived &fn, vec_d &pars, vec_d &stepsize);
    void nmmin(Derived &fn);
    
    // void bfgsmin(); // NYI
    // void cgmin(); // NYI
    // void lbfgsbmin(); // NYI
    
    void R_print()
    {
        this->sim.R_print();
    }
    
};

/* interface for Nelder-Mead minimizer */
template<typename Derived>
void Minimizer<Derived>::nmmin(Derived &fn_, vec_d &pars_)
{
    // calculate stepsize
    double stepsize = 0.1;
    
    // call next interface
    this->nmmin(fn_, pars_, stepsize);
    
}

template<typename Derived>
void Minimizer<Derived>::nmmin(Derived &fn_, vec_d &pars_, double stepsize_)
{
    // construct stepsize vector
    vec_d stepsize(pars_.size(), stepsize_);
    
    // call next interface
    this->nmmin(fn_, pars_, stepsize);
    
}

template<typename Derived>
void Minimizer<Derived>::nmmin(Derived &fn_, vec_d &pars_, vec_d &stepsize_)
{
    // calculate initial simplex based on stepsize
    // Simplex sim(pars_.size());
    // Simplex sim(pars_, stepsize_);
    
    this->sim = Simplex(pars_, stepsize_);
    
    // call next interface
    this->nmmin(fn_);
    
}

template<typename Derived>
void Minimizer<Derived>::nmmin(Derived &fn_)
{
    /***************
     * check params
     ***************/
    
    
    // check that the NM params are within required ranges
    // alpha
    // 
    
    
    
    /****************************
     * Calculate initial simplex
     ****************************/
    
    
    // calculate function vals for initial simplex
    for (auto &v : this->sim.verts) v.fval = fn_(v.par);
    
    this->ceval += this->sim.npar + 1;
    
    // calculate ftol based on seed val
    // Note: This value becomes very large in some cases and may not be the
    //       best way terminate due to this.
    this->control.ftol = this->control.reltol * 
        (this->sim.verts.at(0).fval + this->control.reltol);
    
    // sort the simplex in ascending based on fval
    this->sim.sort();
    
    
    /***********************************
     * Calculate termination tolerances
     ***********************************/
    
    // calculate ptol based on initial simplex
    this->control.ptol = 1.0e-6; // made up small value for now...
    
    // TODO: ENSURE ALL FVAL ARE DEFINED THROUGHOUT THE ITERATIONS
    // I need to find out a good way to do this! I should probably check the 
    // value provided by fn_ at all locations where fn_ is called to ensure it is
    // defined everywhere. If left unchecked, it could lead to strange behaviors 
    // and/or crashes.
    
    
    /****************
     * Output header
     ****************/
    
    
    if (this->control.trace)
    {
        // Header
        Rcpp::Rcout << "Nelder-Mead minimizer" << std::endl;
        
        // initial parameters
        Rcpp::Rcout << setw(8) << setfill(' ') << left << "maxit";
        Rcpp::Rcout << setw(8) << setfill(' ') << left << "maxeval";
        Rcpp::Rcout << setw(8) << setfill(' ') << left << "alpha";
        Rcpp::Rcout << setw(8) << setfill(' ') << left << "gamma";
        Rcpp::Rcout << setw(8) << setfill(' ') << left << "rho";
        Rcpp::Rcout << setw(8) << setfill(' ') << left << "sigma";
        Rcpp::Rcout << setw(8) << setfill(' ') << left << "reltol";
        Rcpp::Rcout << setw(8) << setfill(' ') << left << "abstol" << std::endl;
        
        Rcpp::Rcout << setw(8) << setprecision(2) << setfill(' ') << left << this->control.maxit;
        Rcpp::Rcout << setw(8) << setprecision(2) << setfill(' ') << left << this->control.maxeval;
        Rcpp::Rcout << setw(8) << setprecision(2) << setfill(' ') << left << this->control.alpha;
        Rcpp::Rcout << setw(8) << setprecision(2) << setfill(' ') << left << this->control.gamma;
        Rcpp::Rcout << setw(8) << setprecision(2) << setfill(' ') << left << this->control.rho;
        Rcpp::Rcout << setw(8) << setprecision(2) << setfill(' ') << left << this->control.sigma;
        Rcpp::Rcout << setw(8) << setprecision(2) << setfill(' ') << left << this->control.reltol;
        Rcpp::Rcout << setw(8) << setprecision(2) << setfill(' ') << left << this->control.abstol << std::endl;
        
        // convergence tolerance
        Rcpp::Rcout << "Scaled termination tolerance " << this->control.ftol << std::endl;
        
        // seed vertice
        
    }
    
    
    /**********************
     * Output init simplex
     **********************/
    
    
    if (this->control.trace)
    {
        Rcpp::Rcout << setw(10) << setfill(' ') << left << "INIT";
        Rcpp::Rcout << setw(6) << setfill(' ') << right << this->ceval;
        this->sim.R_trace();
        Rcpp::Rcout << std::endl;
    }
    
    
    /************
     * Main loop
     ************/
    
    
    // main outer loop of minimizer
    for ( ; ; )
    {
        /*****************************
         * Record the current simplex
         *****************************/
        
        
        if (this->control.keep)
        {
            this->hist.push_back(this->sim);
        }
        
        
        /*********************
         * Termination checks
         *********************/
        
        
        // check for fval termination
        // calculate relative difference in the current simplex as implemented in 
        // Numerical Recipes (add edition)
        this->rfdiff = 
        2.0 * abs(this->sim.verts.at(this->sim.npar).fval - 
        this->sim.verts.at(0).fval) / 
        (abs(this->sim.verts.at(this->sim.npar).fval) + 
        abs(this->sim.verts.at(0).fval) + this->control.eps);
        
        // if (this->sim.verts.at(0).fval < this->control.abstol)
        if (this->rfdiff < this->control.ftol ||
            this->sim.verts.at(0).fval < this->control.abstol)
        {
            // set fval convergence flag to true
            this->tfval = 1;
            
            // set this->vopt to the lowest fval vertice
            this->vopt = this->sim.verts.at(0);
            
            break;
        }
        
        // check for maximum iteration termination
        if (this->citer >= this->control.maxit)
        {
            this->titer = 1;
            
            this->vopt = this->sim.verts.at(0);
            
            break;
        }
        
        // check for maximum function evaluations
        if (this->ceval >= this->control.maxeval)
        {
            this->teval = 1;
            
            this->vopt = this->sim.verts.at(0);
            
            break;
        }
        
        
        /******************
         * Start iteration
         ******************/
        
        
        // 2 calculate x_0 = centroid point
        this->sim.calculate_centroid(); // update centroid vertex
        // this->sim.vcen.fval = fn_(this->sim.vcen.par); // calculate centroid fval
        // this->ceval++;
        
        // 3 REFLECTION: x_r = x_0 + alpha(x_0 - x_(n+1)) with alpha > 0; DEF 1.0
        this->sim.reflect(this->sim.npar, this->control.alpha); // reflect last point (worst)
        this->sim.vr.fval = fn_(this->sim.vr.par); // calculate fval for reflected point
        this->ceval++;
        
        // output
        if (this->control.trace) this->action = "REFLECT";
        
        // if x_r < x_1
        // reflected vertice is new best vertice
        if (this->sim.vr < this->sim.verts.at(0))
        {
            // 4 EXPANSION: x_e = x_0 + gamma(x_r - x_0) with gamma > 1; DEF 2.0
            this->sim.expand(this->sim.vr, this->control.gamma);
            this->sim.ve.fval = fn_(this->sim.ve.par);
            this->ceval++;
            
            // if x_e < x_r
            // expanded vertice is better than reflected vertice
            if (this->sim.ve.fval < this->sim.vr.fval)
            {
                // x_(n+1) = x_e
                // replace worst vertice with expanded vertice
                this->sim.verts.at(this->sim.npar) = this->sim.ve;
                
                // output
                if (this->control.trace) this->action = "EXPAND";
                
                // else -> x_e >= x_r
                // expanded vertice is same or worse as reflected vertice
            } else 
            {
                // x_(n+1) = x_r
                // replace worst vertice with reflected vertice
                this->sim.verts.at(this->sim.npar) = this->sim.vr;
                
            }
            
            // else -> x_r >= x_1
            // reflected vertice is same or worse than best vertice
        } else
        {
            // output
            if (this->control.trace) this->action = "HI-CONTRACT";
            
            // if x_r < x_(n+1)
            // reflected vertice is better than worst vertice
            if (this->sim.vr.fval < this->sim.verts.at(this->sim.npar).fval)
            {
                // output
                if (this->control.trace) this->action = "LO-CONTRACT";
                
                // x_(n+1) = x_r
                // replace worst vertice with reflected vertice
                this->sim.verts.at(this->sim.npar) = this->sim.vr;
                
            }
            
            // 5 CONTRACTION: x_c = x_0 + rho(x_(n+1) - x_0) with 0 < rho <= 0.5; DEF 0.5
            this->sim.contract(this->sim.npar, this->control.rho);
            this->sim.vc.fval = fn_(this->sim.vc.par);
            this->ceval++;
            
            // if x_c < x_(n)
            // contracted vertice is better than next worst vertice
            if (this->sim.vc.fval < this->sim.verts.at(this->sim.npar-1).fval)
            {
                // x_(n+1) = x_c
                this->sim.verts.at(this->sim.npar) = this->sim.vc;
                
                // else -> x_c >= x_(n+1)
                // contracted vertice is same or worse than the worst vertice
            } else
            {
                // if x_r >= x_(n+1)
                if (this->sim.vr.fval >= this->sim.verts.at(this->sim.npar).fval)
                {
                    // 6 SHRINK: (x_2:x_n) = x_1 + sigma(x_i - x_1) sigma DEF 0.5
                    // TODO: Add size check as is done in R implementation to make sure
                    // that the size of the polygon is changed in the shrink
                    this->sim.shrink(this->control.sigma);
                    
                    // output
                    if (this->control.trace) this->action = "SHRINK";
                    
                }
                
            }
        }
        
        
        /***********************************************************
         * Sort the simplex based on fval so that index 0 is lowest
         ***********************************************************/
        
        
        this->sim.sort();
        
        
        /***************
         * Output trace
         ***************/
        
        
        if (this->control.trace)
        {
            Rcpp::Rcout << setw(10) << setfill(' ') << left << this->action;
            Rcpp::Rcout << setw(6) << setfill(' ') << right << this->ceval;
            this->sim.R_trace();
            Rcpp::Rcout << std::endl;
        }
        
        
        // add to the iteration count
        this->citer++;
    }
    
    // wrap up
    if (this->control.trace)
    {
        // int tfval = 0;
        // int tvert = 0;
        // int titer = 0;
        // int teval = 0;
        if (this->tfval)
        {
            Rcpp::Rcout << "Terminated by function value tolerance." << std::endl;
        } else if (this->tvert)
        {
            Rcpp::Rcout << "Terminated by simplex size tolerance." << std::endl;
        } else if (this->titer)
        {
            Rcpp::Rcout << "Maximum iterations reached." << std::endl;
        } else if (this->tfval)
        {
            Rcpp::Rcout << "Maximum function evaluations reached." << std::endl;
        }
    }
}


/***************************
 * EMGFit class definition *
 ***************************/

/*******************************************************************************
 * 
 * EMGFit functor class
 * 2021-01-18 Kristian Pirttilä
 * 
 * Implementation of an EMG fitting objective function for minimization of 
 * parameters.
 * 
 ******************************************************************************/
class EMGFit {
private:
    // private vars
    vec_d x, y, wt;
    vec_d seed, lower, upper;
    vec_d parscale;
    
    unsigned int npeaks;
    unsigned int npars;
    unsigned int nvals;
    unsigned int nx;
    unsigned int scalemethod;
    double invsq_nx;
    
    double lambdascale = 1.0;
    
    double SS = 0.0;
    double sd = 0.0;
    double scansum = 0.0;
    
    double emgval = 0.0; // objective function val
    double penval = 0.0; // penalty function val
    
public:
    // public vars
    
    // constructor
    EMGFit(const vec_d &x_, const vec_d &y_, const vec_d &wt_,
           const vec_d &seed_, const vec_d &lower_, const vec_d &upper_,
           unsigned int npeaks_, unsigned int scalemethod_, double lambdascale_ = 1.0) : 
    x(x_), y(y_), wt(wt_), seed(seed_), lower(lower_), upper(upper_), 
    npeaks(npeaks_), scalemethod(scalemethod_), lambdascale(lambdascale_)
    {
        // this->x = x_;
        // this->y = y_;
        // this->wt = wt_;
        // this->seed = seed_;
        // this->lower = lower_;
        // this->upper = upper_;
        // this->npeaks = npeaks_;
        
        this->nx = x_.size();
        this->invsq_nx = std::sqrt(1/(this->nx-1)); // for calculating SD
        this->nvals = seed_.size();
        this->npars = this->nvals / npeaks_;
        
        if (this->npars * this->npeaks != nvals)
        {
            throw("Size mismatch in vectors.");
        }
    }
    
    // internal functions
    // the emg function is a very slow function due to the exponentials
    // I would like to fix this at some point...
    void emg(const unsigned int &i, const double &u, 
             const double &s, const double &l)
    {
        this->emgval = std::exp(std::log(l)+l*(u+((l*s*s)/2)-this->x.at(i)) +
            R::pnorm((u+l*s*s-this->x.at(i))/(s), 0.0, 1.0, false, true));
        
        // this->emgval = (l/2)*std::exp((l/2)*(2*u+l*s*s-2*this->x.at(i))) * 
        //     2 * R::pnorm((u+l*s*s-this->x.at(i))/s, 0.0, 1.0, false, true);
    }
    
    // objective function
    void objFun(const vec_d &par_) {
        /* calculate sum of squares */
        this->SS = 0.0;
        
        for (unsigned int i = 0; i < this->nx; i++)
        {
            this->scansum = 0.0;
            
            // A * emg[x_i]
            for (unsigned int j = 0; j < this->npeaks; j++)
            {
                if (this->scalemethod == 1)
                    // log scaled lambda
                {
                    this->emgval = 0.0;
                    this->emg(i,
                              par_.at(j*this->npars),
                              par_.at(1+j*this->npars),
                              std::exp(par_.at(2+j*this->npars)));
                    this->scansum += par_.at(3+j*this->npars) * this->emgval;
                } else if (this->scalemethod == 2)
                    // multiplication scaled lambda
                {
                    this->emgval = 0.0;
                    this->emg(i,
                              par_.at(j*this->npars),
                              par_.at(1+j*this->npars),
                              par_.at(2+j*this->npars)/this->lambdascale);
                    this->scansum += par_.at(3+j*this->npars) * this->emgval;
                } else if (this->scalemethod == 3)
                    // log scaled lambda and area
                {
                    this->emgval = 0.0;
                    this->emg(i,
                              par_.at(j*this->npars),
                              par_.at(1+j*this->npars),
                              std::exp(par_.at(2+j*this->npars)));
                    this->scansum += std::exp(par_.at(3+j*this->npars)) * this->emgval;
                } else
                {
                    this->emgval = 0.0;
                    // this->emg(i,
                    //           par_.at(j*this->npars),
                    //           par_.at(1+j*this->npars),
                    //           std::exp(par_.at(2+j*this->npars)));
                    // this->sum += std::exp(par_.at(3+j*this->npars)) * this->val;
                    this->emg(i,
                              par_.at(j*this->npars),
                              par_.at(1+j*this->npars),
                              par_.at(2+j*this->npars));
                    this->scansum += par_.at(3+j*this->npars) * this->emgval;
                }
                
            }
            
            // this->SS += this->wt.at(i) * std::pow(this->si.at(i) - this->sum, 2);
            this->SS += this->wt.at(i) * (this->y.at(i) - this->scansum) * (this->y.at(i) - this->scansum);
        }
        
        // here I should be able to use the quake fast inverse square root 
        // function down the line. we have sd = sqrt(ss/(nx-1)) which can be 
        // rewritten sd = sqrt(ss)*sqrt(1/(nx-1))
        // or I can calculate sqrt(1/(nx-1)) when I instatiate the class which
        // will mean I only calculate it once.
        // It is, for now, calculated upon instatiating the object and stored in
        // this->invsq_nx.
        this->sd = std::sqrt(this->SS/(this->nx-1));
        // this->sd = std::sqrt(this->SS)*this->invsq_nx;
        
    }
    
    // penalty function
    void penFun(const vec_d &par_) {
        this->penval = 0.0;
        
        for (unsigned int i = 0; i < this->nvals; i++)
        {
            if (par_.at(i) > this->upper.at(i) || 
                par_.at(i) < this->lower.at(i))
            {
                // this->pen += std::numeric_limits<double>::infinity();
                this->penval += 1.0e35;
                
                break;
            }
        }
    }
    
    double operator()(vec_d &par_)
    {
        this->objFun(par_);
        this->penFun(par_);
        
        return this->sd + this->penval;
    }
    
    
    
    // other methods
    
};

// class EMGFit : public Functor {
// private:
//     arma::vec si;
//     arma::vec st;
//     arma::vec wt;
//     arma::vec seed;
//     arma::vec lower;
//     arma::vec upper;
//     arma::vec pars_plus_h;
//     arma::vec pars_minus_h;
//     
//     // double st_mid = -1.0;
//     
//     std::string method;
//     
//     unsigned int npeaks;
//     unsigned int npars;
//     unsigned int nvals;
//     unsigned int nx;
//     double h = 1e-5;
//     double SS = 0.0;
//     double sum = 0.0;
//     double val = 0.0;
//     double pen = 0.0;
//     
//     bool seed_scaled = false;
//     
// public:
//     EMGFit(arma::vec &_si,
//            arma::vec &_st,
//            arma::vec &_wt,
//            arma::vec _seed,
//            arma::vec &_lower,
//            arma::vec &_upper,
//            unsigned int _npeaks,
//            double _h = 1e-5,
//            const std::string &_method = "Not-NM")
//     {
//         si = _si;
//         st = _st;
//         wt = _wt;
//         seed = _seed;
//         lower = _lower;
//         upper = _upper;
//         
//         npeaks = _npeaks;
//         npars = _seed.size()/_npeaks;
//         nx = _si.size();
//         nvals = _seed.size();
//         h = _h;
//         method = _method;
//         
//         pars_plus_h = arma::vec(this->nvals);
//         pars_minus_h = arma::vec(this->nvals);
//     }
//     
//     void emg(const double &val, const unsigned int &i, const double &u, const double &s, const double &l)
//     {
//         this->val = std::exp(std::log(l)+l*(u+((l*s*s)/2)-this->st.at(i)) +
//             R::pnorm((u+l*s*s-this->st.at(i))/(s), 0.0, 1.0, false, true));
//     }
//     
//     void objFun(const arma::vec &pars) {
//         /* calculate sum of squares */
//         this->SS = 0.0;
//         this->sum = 0.0;
//         
//         for (unsigned int i = 0; i < this->nx; i++)
//         {
//             this->sum = 0.0;
//             
//             // A * emg[x_i]
//             for (unsigned int j = 0; j < this->npeaks; j++)
//             {
//                 this->val = 0.0;
//                 this->emg(this->val,
//                           i,
//                           pars.at(j*this->npars),
//                           pars.at(1+j*this->npars),
//                           std::exp(pars.at(2+j*this->npars)));
//                 this->sum += std::exp(pars.at(3+j*this->npars)) * this->val;
//                 
//             }
//             
//             this->SS += this->wt.at(i) * std::pow(this->si.at(i) - this->sum, 2);
//         }
//         
//     }
//     
//     void penFun(const arma::vec &pars) {
//         this->pen = 0.0;
//         
//         for (unsigned int i = 0; i < this->nvals; i++)
//         {
//             if (pars.at(i) > upper.at(i) || pars.at(i) < lower.at(i))
//             {
//                 this->pen += std::numeric_limits<double>::infinity();
//                 break;
//             }
//         }
//     }
//     
//     double operator()(const arma::vec &pars) override {
//         this->objFun(pars);
//         
//         if (this->method.compare("Nelder-Mead") == 0)
//         {
//             this->penFun(pars);
//         }
//         
//         return this->SS + this->pen;
//     }
//     
//     void Gradient(const arma::vec &pars, arma::vec &gr) override {
//         if (gr.size() != this->nvals)
//         {
//             gr = arma::zeros<arma::vec>(this->nvals);
//         }
//         
//         for (unsigned int i = 0; i < this->nvals; i++)
//         {
//             this->pars_plus_h = pars;
//             this->pars_minus_h = pars;
//             
//             // pars_plus_h.at(i) = pars.at(i) + this->h;
//             // pars_minus_h.at(i) = pars.at(i) - this->h;
//             this->pars_plus_h.at(i) = pars.at(i) + this->h*pars.at(i);
//             this->pars_minus_h.at(i) = pars.at(i) - this->h*pars.at(i);
//             
//             this->objFun(this->pars_plus_h);
//             this->sum = this->val;
//             this->objFun(this->pars_minus_h);
//             this->sum -= this->val;
//             
//             gr.at(i) = this->sum/(2*this->h*pars.at(i));
//             
//         }
//     }
// };

// class EMGFit2 : public Functor {
// private:
//     arma::vec si;
//     arma::vec st;
//     arma::vec wt;
//     arma::vec seed;
//     arma::vec lower;
//     arma::vec upper;
//     arma::vec pars_plus_h;
//     arma::vec pars_minus_h;
//     
//     // double st_mid = -1.0;
//     
//     std::string method;
//     
//     unsigned int npeaks;
//     unsigned int npars;
//     unsigned int nvals;
//     unsigned int nx;
//     double h = 1e-5;
//     double SS = 0.0;
//     double sum = 0.0;
//     double val = 0.0;
//     double pen = 0.0;
//     int objout = 0;
//     
//     bool seed_scaled = false;
//     
// public:
//     EMGFit2(arma::vec &_si,
//            arma::vec &_st,
//            arma::vec &_wt,
//            arma::vec _seed,
//            arma::vec &_lower,
//            arma::vec &_upper,
//            unsigned int _npeaks,
//            double _h = 1e-5,
//            const std::string &_method = "Not-NM",
//            const int _objout = 0)
//     {
//         si = _si;
//         st = _st;
//         wt = _wt;
//         seed = _seed;
//         lower = _lower;
//         upper = _upper;
//         
//         objout = _objout;
//         
//         npeaks = _npeaks;
//         npars = _seed.size()/_npeaks;
//         nx = _si.size();
//         nvals = _seed.size();
//         h = _h;
//         method = _method;
//         
//         pars_plus_h = arma::vec(this->nvals);
//         pars_minus_h = arma::vec(this->nvals);
//     }
//     
//     void emg(const double &val, const unsigned int &i, const double &u, const double &s, const double &l)
//     {
//         this->val = std::exp(std::log(l)+l*(u+((l*s*s)/2)-this->st.at(i)) +
//             R::pnorm((u+l*s*s-this->st.at(i))/(s), 0.0, 1.0, false, true));
//     }
//     
//     void objFun(const arma::vec &pars) {
//         /* calculate sum of squares */
//         this->SS = 0.0;
//         this->sum = 0.0;
//         
//         if (this->objout)
//         {
//             Rcout << "pars: ";
//             
//             for (auto & i : pars) Rcout << i << " ";
//             
//             Rcout << std::endl;
//         }
//         
//         for (unsigned int i = 0; i < this->nx; i++)
//         {
//             this->sum = 0.0;
//             
//             // A * emg[x_i]
//             for (unsigned int j = 0; j < this->npeaks; j++)
//             {
//                 this->val = 0.0;
//                 this->emg(this->val,
//                           i,
//                           pars.at(j*this->npars),
//                           pars.at(1+j*this->npars),
//                           std::exp(pars.at(2+j*this->npars)));
//                 this->sum += std::exp(pars.at(3+j*this->npars)) * this->val;
//                 
//             }
//             
//             this->SS += this->wt.at(i) * std::pow(this->si.at(i) - this->sum, 2);
//         }
//         
//     }
//     
//     void penFun(const arma::vec &pars) {
//         this->pen = 0.0;
//         
//         for (unsigned int i = 0; i < this->nvals; i++)
//         {
//             if (pars.at(i) > upper.at(i) || pars.at(i) < lower.at(i))
//             {
//                 this->pen += std::numeric_limits<double>::infinity();
//                 break;
//             }
//         }
//     }
//     
//     double operator()(const arma::vec &pars) override {
//         this->objFun(pars);
//         
//         if (this->method.compare("Nelder-Mead") == 0)
//         {
//             this->penFun(pars);
//         }
//         
//         return this->SS + this->pen;
//     }
//     
//     void Gradient(const arma::vec &pars, arma::vec &gr) override {
//         if (gr.size() != this->nvals)
//         {
//             gr = arma::zeros<arma::vec>(this->nvals);
//         }
//         
//         for (unsigned int i = 0; i < this->nvals; i++)
//         {
//             this->pars_plus_h = pars;
//             this->pars_minus_h = pars;
//             
//             // pars_plus_h.at(i) = pars.at(i) + this->h;
//             // pars_minus_h.at(i) = pars.at(i) - this->h;
//             this->pars_plus_h.at(i) = pars.at(i) + this->h*pars.at(i);
//             this->pars_minus_h.at(i) = pars.at(i) - this->h*pars.at(i);
//             
//             this->objFun(this->pars_plus_h);
//             this->sum = this->val;
//             this->objFun(this->pars_minus_h);
//             this->sum -= this->val;
//             
//             gr.at(i) = this->sum/(2*this->h*pars.at(i));
//             
//         }
//     }
// };


// CLASS ApexFinder
class cpcApexFinder
{
private:
    
    vec_d v;
    vec_i loc_min;
    vec_i apices;
    
    int j;
    int start = -1;
    int end = -1;
    int last_min = -1;
    int last_max = -1;
    int npeaks = 0;
    int nscans;
    
    double w;
    
    bool is_loc_min = false;
    bool is_loc_max = false;
    
public:
    cpcApexFinder(const vec_d &_v, const double &_w)
    {
        this->v = _v;
        this->w = _w;
        
        this->nscans = this->v.size();
        this->loc_min = vec_i(this->nscans, 0);
        
        /***********************
         * detect local minima *
         ***********************/
        for (int i = 0; i < this->nscans; i++)
        {
            is_loc_min = true;
            
            if (i < this->w)
            {
                start = 0;
                end = (2 * this->w) + 1;
            } else if (i > this->nscans - this->w - 1)
            {
                start = this->nscans - 2 * this->w - 1;
                end = this->nscans - 1;
            } else
            {
                start = i - this->w;
                end = i + this->w;
            }
            
            // check if i is an extreme point (minima or maxima)
            for (int j = start; j <= end; j++)
            {
                // check if i is a local minima
                if (this->v.at(j) < this->v.at(i)) is_loc_min = false;
                
            }
            
            // if at a local negative minima (possible apex)
            if (is_loc_min && this->v.at(i) < 0.0)
            {
                // check that the current minima is not closer to last minima by w
                if (last_min > 0 && i - last_min < this->w)
                {
                    if (this->v.at(i) < this->v.at(last_min))
                    {
                        loc_min.at(last_min) = 0;
                        loc_min.at(i) = 1;
                    }
                    
                    // if no previous minima has been found or the current minima is
                    // further away than w
                } else
                {
                    loc_min.at(i) = 1;
                    this->npeaks++;
                }
                
                last_min = i;
                
            }
        }
        
        /**********************
         * create apex vector *
         **********************/
        this->apices = vec_i(this->npeaks, -1);
        j = 0;
        
        for (int i = 0; i < this->nscans; i++)
        {
            if (loc_min.at(i) > 0)
            {
                this->apices.at(j) = i;
                j++;
            }
        }
    }
    
    vec_i getApices()   { return this->apices; }
    
};

// [[Rcpp::export]]
vec_i testApexFinder(vec_d v, int w)
{
    cpcApexFinder af(v, w);
    
    return af.getApices();
}



/*********************************
 * Chromatogram class definition *
 *********************************/
class Chromatogram
{
private:
    int nscans;
    int npeaks;
    
    ChromatogramOptions options;
    
    // general vars
    PeakTable pt;
    
    vec_d d0;
    vec_d d1;
    vec_d d2;
    vec_d d0_res;
    vec_d st;
    
    // emg fitting vars
    // Roptim<EMGFit> fit;
    vec_i clusters_to_fit;
    
    double slope_at_idx(int _idx);
    double slope_at_point(double _point);
    double integrate_d0(int a = 0, int b = 0);
    
    void find_apices();
    void detect_vip();
    void calculate_peak_expansion_thresholds(int peak);
    void determine_peak_expansion_start_bounds(int peak);
    // void expand_to_baseline(int _peak);
    void peak_baseline_expansion(int peak);
    void calculate_baseline_residual(int fblb, int tblb);
    void adjust_cluster_baseline(vec_i &clust);
    void check_peaks();
    void detect_peaks();
    void determine_apex_type(int peak_);
    void detect_clusters();
    void expand_all_peaks();
    void expand_all_clusters();
    void adjust_apices();
    void calculate_peak_characteristics();
    
    // emg fitting member functions
    void determine_clusters_to_fit();
    void fit_emg(); // peak deconvolution with emg
    
    vec_i expand_to_baseline(int fexp, int texp, double fthr, double tthr);
    
public:
    Chromatogram(const vec_d &_d0, 
                 const vec_d &_d2, 
                 const vec_d &_st,
                 const double _apex_thresh, 
                 const int _w, 
                 const int _p,
                 const double _liftoff = 0.0, 
                 const double _touchdown = 0.5, 
                 const int _output = 0,
                 const int _fit_emg = 1, 
                 const int _fit_only_vip = 1, 
                 const int _fit_hess = 0,
                 const double _fit_rel_lim = 0.05,
                 const int _pts_per_peak = 10,
                 const int _min_shoulder_pts = 3,
                 const int _min_rounded_pts = 3,
                 const double _reltol = 1.0e-8,
                 const double _abstol = -1.0e35,
                 const double _alpha = 1.0,
                 const double _gamma = 2.1,
                 const double _rho = 0.75,
                 const double _sigma = 0.5,
                 const int _maxit = 2000,
                 const int _maxeval = 2000)
    {
        nscans = (int) _d0.size();
        
        this->d0 = _d0;
        this->d2 = _d2;
        this->st = _st;
        this->d0_res = vec_d(nscans, 0.0);
        
        // process options
        // ChromatogramOptions(const double apex_thresh_ = 0.0,
        //                     const double liftoff_ = 0.0,
        //                     const double touchdown_ = 0.5,
        //                     const int w_ = 5,
        //                     const int p_ = -1,
        //                     const int output_ = 0,
        //                     const int fit_emg_ = 1,
        //                     const int fit_only_vip_ = 1,
        //                     const int fit_hess_ = 0,
        //                     const double fit_rel_lim_ = 0.05,
        //                     const int pts_per_peak_ = 20,
        //                     const double reltol_ = 1e-8,
        //                     const double abstol_ = 1.0e35,
        //                     const double alpha_ = 1.0,
        //                     const double gamma_ = 2.1,
        //                     const double rho_ = 0.75,
        //                     const double sigma_ = 0.5,
        //                     const int maxit_ = 2000,
        //                     const int maxeval_ = 2000)
        this->options = ChromatogramOptions(_apex_thresh, 
                                            _liftoff,
                                            _touchdown, 
                                            _w, 
                                            _p, 
                                            _output, 
                                            _fit_emg, 
                                            _fit_only_vip, 
                                            _fit_hess, 
                                            _fit_rel_lim,
                                            _pts_per_peak,
                                            _min_shoulder_pts,
                                            _min_rounded_pts,
                                            _reltol,
                                            _abstol,
                                            _alpha,
                                            _gamma,
                                            _rho,
                                            _sigma,
                                            _maxit,
                                            _maxeval);
        
        this->pt.output = _output; // carry the output option into the peak table
    }
    
    // process methods
    void process_chromatogram(); // performs all chromatogram
    
    // getters
    int get_vip() { return this->pt.vip; }
    
    vec_i get_apex()        { return this->pt.apex; }
    vec_i get_adj_apex()    { return this->pt.adj_apex; }
    vec_d get_finf()        { return this->pt.finf; }
    vec_d get_tinf()        { return this->pt.tinf; }
    vec_i get_fblb()        { return this->pt.fblb; }
    vec_i get_tblb()        { return this->pt.tblb; }
    vec_i get_fpkb()        { return this->pt.fpkb; }
    vec_i get_tpkb()        { return this->pt.tpkb; }
    vec_i get_ftyp()        { return this->pt.ftyp; }
    vec_i get_ttyp()        { return this->pt.ttyp; }
    vec_d get_area()        { return this->pt.area; }
    vec_d get_inf_area()    { return this->pt.inf_area; }
    vec_d get_emg_mu()      { return this->pt.emg_mu; }
    vec_d get_emg_sigma()   { return this->pt.emg_sigma; }
    vec_d get_emg_lambda()  { return this->pt.emg_lambda; }
    vec_d get_emg_area()    { return this->pt.emg_area; }
    vec_i get_emg_conv()    { return this->pt.emg_conv; }
    
    // summary output
    void summary();
    void print_all();
    void print_clusters();
};

/**********************
 * Method definitions *
 **********************/

void PeakTable::print_remove_these_now()
{
    Rcout << "remove_these_now:";
    
    for (int i = 0; i < (int) this->remove_these_now.size(); i++)
        Rcout << " " << this->remove_these_now.at(i);
    
    Rcout << std::endl;
}

void PeakTable::print_remove_these_later()
{
    Rcout << "remove_these_now:";
    
    for (int i = 0; i < (int) this->remove_these_later.size(); i++)
        Rcout << " " << this->remove_these_later.at(i);
    
    Rcout << std::endl;
}

// PeakTable methods
void PeakTable::summary()
{
    Rcout << "Peak table: " << std::endl
          << npeaks << " peaks detected." << std::endl;
    
    // Rcout << "#\tapex\tadja\tatyp\tcode\tfinf\ttinf\tfblb\ttblb\tfpkb\t"
    //       << "tpkb\tmu\tsigm\tlamb\tarea\tconv\tvip"
    //       << std::endl;
    
    const char filler = ' ';
    const int outwidth = 10;
    
    // header
    Rcout << left << setw(3) << setfill(filler) << "#";
    Rcout << left << setw(5) << setfill(filler) << "apex";
    Rcout << left << setw(5) << setfill(filler) << "adj";
    Rcout << left << setw(5) << setfill(filler) << "atyp";
    Rcout << left << setw(5) << setfill(filler) << "code";
    Rcout << left << setw(outwidth) << setfill(filler) << "finf";
    Rcout << left << setw(outwidth) << setfill(filler) << "tinf";
    Rcout << left << setw(5) << setfill(filler) << "fblb";
    Rcout << left << setw(5) << setfill(filler) << "tblb";
    Rcout << left << setw(5) << setfill(filler) << "fpkb";
    Rcout << left << setw(5) << setfill(filler) << "tpkb";
    Rcout << left << setw(outwidth) << setfill(filler) << "area";
    Rcout << left << setw(outwidth) << setfill(filler) << "emu";
    Rcout << left << setw(outwidth) << setfill(filler) << "esigma";
    Rcout << left << setw(outwidth) << setfill(filler) << "elambda";
    Rcout << left << setw(outwidth) << setfill(filler) << "earea";
    Rcout << left << setw(5) << setfill(filler) << "econv";
    Rcout << left << setw(5) << setfill(filler) << "vip";
    Rcout << std::endl;
    
    for (int i = 0; i < static_cast<int>(apex.size()); i++)
    {
        std::string code;
        Rcout << left << setw(3) << setfill(filler) << i;
        Rcout << left << setw(5) << setfill(filler) << this->apex.at(i);
        Rcout << left << setw(5) << setfill(filler) << this->adj_apex.at(i);
        Rcout << left << setw(5) << setfill(filler) << this->atyp.at(i);
        
        switch(this->ftyp.at(i)) {
        case 0: code.append("B"); break;
        case 1: code.append("V"); break;
        case 2: code.append("S"); break;
        case 3: code.append("R"); break;
        default: code.append("U");
        }
        
        switch(this->ttyp.at(i)) {
        case 0: code.append("B"); break;
        case 1: code.append("V"); break;
        case 2: code.append("S"); break;
        case 3: code.append("R"); break;
        default: code.append("U");
        }
        
        Rcpp::Rcout << left << setw(5) << setfill(filler) << code;
        Rcpp::Rcout << left << setw(outwidth) << setfill(filler) 
                    << std::setprecision(6) << this->finf.at(i);
        Rcpp::Rcout << left << setw(outwidth) << setfill(filler) 
                    << std::setprecision(6) << this->tinf.at(i);
        Rcpp::Rcout << left << setw(5) << setfill(filler) << this->fblb.at(i);
        Rcpp::Rcout << left << setw(5) << setfill(filler) << this->tblb.at(i);
        Rcpp::Rcout << left << setw(5) << setfill(filler) << this->fpkb.at(i);
        Rcpp::Rcout << left << setw(5) << setfill(filler) << this->tpkb.at(i);
        Rcpp::Rcout << left << setw(outwidth) << setfill(filler) 
                    << this->area.at(i);
        Rcpp::Rcout << left << setw(outwidth) << setfill(filler) 
                    << std::setprecision(6) << this->emg_mu.at(i);
        Rcpp::Rcout << left << setw(outwidth) << setfill(filler) 
                    << std::setprecision(6) << this->emg_sigma.at(i);
        Rcpp::Rcout << left << setw(outwidth) << setfill(filler) 
                    << std::setprecision(6) << this->emg_lambda.at(i);
        Rcpp::Rcout << left << setw(outwidth) << setfill(filler) 
                    << std::setprecision(6) << this->emg_area.at(i);
        Rcpp::Rcout << left << setw(5) << setfill(filler) 
                    << this->emg_conv.at(i);
        
        if (this->vip == i)
        {
            Rcout << left << setw(5) << setfill(filler) << "*";
        }
        Rcout << std::endl;
    }
    
    this->print_remove_these_now();
    this->print_remove_these_later();
    
    Rcout << this->final_clust.size() << " clusters detected." << std::endl;
}

void PeakTable::init_peaks(vec_i &_apex)
{
    // number of detected apices
    this->npeaks = (int) _apex.size();
    
    // baseline expansion results
    this->apex = _apex;
    this->atyp = vec_i(this->npeaks, -1);
    this->adj_apex = _apex;
    this->finf = vec_d(this->npeaks, -1.0);
    this->tinf = vec_d(this->npeaks, -1.0);
    this->fexp = vec_i(this->npeaks, -1);
    this->texp = vec_i(this->npeaks, -1);
    this->fthr = vec_d(this->npeaks, -1.0);
    this->tthr = vec_d(this->npeaks, -1.0);
    this->fblb = vec_i(this->npeaks, -1);
    this->tblb = vec_i(this->npeaks, -1);
    this->fpkb = vec_i(this->npeaks, -1);
    this->tpkb = vec_i(this->npeaks, -1);
    this->ftyp = vec_i(this->npeaks, -1);
    this->ttyp = vec_i(this->npeaks, -1);
    this->area = vec_d(this->npeaks, -1);
    this->inf_area = vec_d(this->npeaks, -1);
    
    // emg fitted values
    this->emg_mu        = vec_d(this->npeaks, -1.0);
    this->emg_sigma     = vec_d(this->npeaks, -1.0);
    this->emg_lambda    = vec_d(this->npeaks, -1.0);
    this->emg_area      = vec_d(this->npeaks, -1.0);
    this->emg_conv      = vec_i(this->npeaks, -1.0);
    
    // removal tags
    this->remove_these_later = vec_i(this->npeaks, 0);
    this->remove_these_now = vec_i(this->npeaks, 0);
}

void PeakTable::remove_peak(int _peak)
{
    unsigned int i;
    
    if (this->output) Rcout << "Remove peak " << _peak << "; ";
    
    /************************
     * remove vector values *
     ************************/
    this->apex.erase(this->apex.begin() + _peak);
    this->atyp.erase(this->atyp.begin() + _peak);
    this->adj_apex.erase(this->adj_apex.begin() + _peak);
    this->finf.erase(this->finf.begin() + _peak);
    this->tinf.erase(this->tinf.begin() + _peak);
    this->fexp.erase(this->fexp.begin() + _peak);
    this->texp.erase(this->texp.begin() + _peak);
    this->fthr.erase(this->fthr.begin() + _peak);
    this->tthr.erase(this->tthr.begin() + _peak);
    this->fblb.erase(this->fblb.begin() + _peak);
    this->tblb.erase(this->tblb.begin() + _peak);
    this->fpkb.erase(this->fpkb.begin() + _peak);
    this->tpkb.erase(this->tpkb.begin() + _peak);
    this->ftyp.erase(this->ftyp.begin() + _peak);
    this->ttyp.erase(this->ttyp.begin() + _peak);
    this->area.erase(this->area.begin() + _peak);
    this->inf_area.erase(this->inf_area.begin() + _peak);
    this->emg_mu.erase(this->emg_mu.begin() + _peak);
    this->emg_sigma.erase(this->emg_sigma.begin() + _peak);
    this->emg_lambda.erase(this->emg_lambda.begin() + _peak);
    this->emg_area.erase(this->emg_area.begin() + _peak);
    this->emg_conv.erase(this->emg_conv.begin() + _peak);
    
    this->remove_these_later.erase(this->remove_these_later.begin() + _peak);
    this->remove_these_now.erase(this->remove_these_now.begin() + _peak);
    
    /*********************************
     * adjust vip value if necessary *
     *********************************/
    if (this->vip == _peak)
    {
        if (this->output) Rcout << "vip removed; ";
        this->vip = -1;
    } else if (this->vip >= 0 && this->vip > _peak)
    {
        if (this->output) Rcout << "vip--; ";
        this->vip--;
    }
    
    /*******************
     * update clusters *
     *******************/
    if (this->final_clust.size() > 0) // if there are clusters
    {
        // determine which cluster the current peak is in
        i = 0;
        
        while (i < this->final_clust.size())
        {
            if (this->final_clust.at(i).at(0) <= _peak &&
                this->final_clust.at(i).at(1) >= _peak)
            {
                break;
            } else
            {
                i++;
            }
        } // i is now the cluster idx holding the _peak
        
        // update clusters
        if (i < this->final_clust.size()) // check that a cluster was found
        {
            if (this->output) Rcout << "cluster " << i << "; ";
            
            if (this->final_clust.at(i).at(0) == _peak &&
                this->final_clust.at(i).at(1) == _peak) // only peak in cluster
            {
                // remove entire selected cluster
                if (this->output) Rcout << "cluster removed; ";
                
                this->final_clust.erase(this->final_clust.begin() + i);
                
            } else
            {
                // update current cluster
                this->final_clust.at(i).at(1)--;
                
                if (this->output) Rcout << "cluster set "
                                                << this->final_clust.at(i).at(0)
                                                << "->"
                                                << this->final_clust.at(i).at(1)
                                                << "; ";
                
                // i++; // move to next cluster
                
            }
            
            // update all clusters
            i = 0;
            
            while(i < this->final_clust.size())
            {
                if (this->final_clust.at(i).at(0) > _peak)
                {
                    this->final_clust.at(i).at(0)--;
                }
                
                if (this->final_clust.at(i).at(1) > _peak)
                {
                    this->final_clust.at(i).at(1)--;
                }
                i++;
            }
            
        } // else: peak is not in a cluster - might happen if VIP is set
        else
        {
            if (this->output) Rcout << "peak not in cluster";
            
            // update all clusters after removed peak
            i = 0;
            while(i < this->final_clust.size())
            {
                if (this->final_clust.at(i).at(0) > _peak)
                {
                    this->final_clust.at(i).at(0)--;
                }
                
                if (this->final_clust.at(i).at(1) > _peak)
                {
                    this->final_clust.at(i).at(1)--;
                }
                
                i++;
            }
            
        }
        
    }
    
    if (this->output) Rcout << std::endl;
    
    this->npeaks--;
}

bool PeakTable::check_boundary_overlap(int first, int cur)
{
    bool check = false;
    
    for (int j = first; j < cur; j++)
    {
        if (this->fblb.at(cur) <= this->tblb.at(j))
        {
            check = true;
            break;
        }
    }
    
    return check;
}

void PeakTable::remove_tagged_peaks()
{
    int i = 0;
    int N_removed_peaks = 0;
    
    while (i < (int) this->remove_these_later.size())
    {
        if (this->remove_these_later.at(i) == 1)
        {
            this->remove_peak(i);
            N_removed_peaks++;
        } else
        {
            i++;
        }
    }
    
    if (this->output) Rcout << N_removed_peaks << " peaks removed." << std::endl;
}

void PeakTable::print_clusters()
{
    Rcpp::Rcout << "Detected clusters: " << std::endl;
    
    vec_i cur_clust(2);
    
    for (unsigned int i = 0; i < this->final_clust.size(); i++)
    {
        cur_clust = this->final_clust.at(i);
        
        Rcout << i << ":[" << cur_clust[0] << "->" << cur_clust[1] << "] ";
    }
    
    Rcout << std::endl;
}

// Chromatogram methods
void Chromatogram::summary()
{
    Rcout << "Chromatogram class." << std::endl
          << "nscans = " << this->nscans << std::endl;
    
    this->pt.summary();
}

void Chromatogram::print_clusters()
{
    this->pt.print_clusters();
}

// TODO: Add last maxima as well to keep track of apex magnitude
void Chromatogram::find_apices()
{
    int j;
    int start, end;
    
    int last_min = -1;
    // int last_max = -1;
    
    this->npeaks = 0;
    
    vec_i local_min(this->nscans, 0);
    
    bool is_local_min = false;
    // bool is_local_max = false;
    
    /***********************
     * detect local minima *
     ***********************/
    for (int i = 0; i < this->nscans; i++)
    {
        is_local_min = true;
        
        if (i < this->options.w)
        {
            start = 0;
            end = (2 * this->options.w) + 1;
        } else if (i > this->nscans - this->options.w - 1)
        {
            start = this->nscans - 2 * this->options.w - 1;
            end = this->nscans - 1;
        } else
        {
            start = i - this->options.w;
            end = i + this->options.w;
        }
        
        // check if i is an extreme point (minima or maxima)
        for (int j = start; j <= end; j++)
        {
            // check if i is a local minima
            if (this->d2.at(j) < this->d2.at(i)) is_local_min = false;
            
            // check if i is a local maxima
            // if (this->d2.at(j) > this->d2.at(i)) is_local_max = false;
            
        }
        
        // if a local maxima
        // this logic could probably be better formulated...
        // if (is_local_max)
        // {
        //     // check that the current maxima is not closer to last maxima by w
        //     if (last_max > 0 && i - last_max < this->options.w)
        //     {
        //         // check if the current maxima is larger than the last maxima
        //         if (this->d2.at(i) > this->d2.at(last_max))
        //         {
        //             last_max = i;
        //         }
        //     
        //     // if no previous maxima is found or the current maxima is further
        //     // away than w from the last maxima
        //     } else
        //     {
        //         last_max = i;
        //         
        //     }
        //     
        // }
        
        // if at a local minima -> check if the local minima deviates from the
        // last maxima more than the noise level
        // this was added to avoid false detection of shoulders and rounded
        // peak shapes in noisy data
        // if (is_local_min && last_max > 0 &&
        //     std::abs(this->d2.at(i) - this->d2.at(last_max)) < 
        //         this->options.apex_thresh)
        // {
        //     is_local_min = false;
        //     
        // }
        
        // if at a local negative minima (possible apex)
        if (is_local_min && this->d2.at(i) < 0.0)
        {
            // check that the current minima is not closer to last minima by w
            if (last_min > 0 && i - last_min < this->options.w)
            {
                if (this->d2.at(i) < this->d2.at(last_min))
                {
                    local_min.at(last_min) = 0;
                    local_min.at(i) = 1;
                }
                
            // if no previous minima has been found or the current minima is
            // further away than w
            } else
            {
                local_min.at(i) = 1;
                this->npeaks++;
            }
            
            last_min = i;
            
        }
    }
    
    /**********************
     * create apex vector *
     **********************/
    vec_i apices(this->npeaks, -1);
    j = 0;
    
    for (int i = 0; i < this->nscans; i++)
    {
        if (local_min.at(i) > 0)
        {
            apices.at(j) = i;
            j++;
        }
    }
    
    /*****************************
     * initialize the peak table *
     *****************************/
    this->pt.init_peaks(apices);
    
}

void Chromatogram::detect_vip()
{
    if (this->options.p > 0)
    {
        if (this->options.output) Rcout << "Detecting VIP: ";
        
        for (int i = 0; i < this->pt.npeaks; i++)
        {
            if (this->pt.fpkb.at(i) <= this->options.p &&
                this->pt.tpkb.at(i) >= this->options.p)
            {
                if (this->get_vip() > 0 && 
                    abs(this->pt.apex.at(i) - this->options.p) < abs(this->pt.apex.at(this->get_vip()) - this->options.p))
                {
                    this->pt.vip = i;
                } else if (this->get_vip() < 0)
                {
                    this->pt.vip = i;
                }
            }
        }
        
        if (this->options.output) Rcout << this->pt.vip << std::endl;
    }
}

double Chromatogram::slope_at_idx(int _idx)
{
    if (_idx < 1)
    {
        _idx = 0;
        return this->d0.at(_idx + 1) - this->d0.at(_idx);
    } else if (_idx > this->nscans - 2)
    {
        _idx = this->nscans - 2;
        return this->d0.at(_idx) - this->d0.at(_idx - 1);
    } else
    {
        return (this->d0.at(_idx + 1) - this->d0.at(_idx - 1)) / 2;
    }
}

double Chromatogram::slope_at_point(double _point)
{
    int lhs_idx = (int) floor(_point);
    int rhs_idx = (int) floor(_point) + 1;
    
    double lhs_d1 = slope_at_idx(lhs_idx);
    double rhs_d1 = slope_at_idx(rhs_idx);
    
    return interpolate_y(lhs_idx, rhs_idx, lhs_d1, rhs_d1, _point);
}

/*
 * check_apices():
 * in this method checks of the detected apices can be performed with solutions
 * implemented
 * 
 */
void Chromatogram::check_peaks()
{
    int i;
    int maxima;
    // int smaller;
    
    // loop over the detected peaks and runs checks on them
    if (this->options.output) Rcout << "Check peaks: ";
    
    i = 1;
    while (i < this->pt.npeaks)
    {
        if (this->options.output) Rcout << i << ":";
        
        // ensure that peak detection has been performed
        if (this->pt.finf.at(i) > 0 && this->pt.tinf.at(i) > 0)
        {
            // check for shoulder and rounded events that are lower than the noise
            // in magnitude
            
            // check if the current peak share inflection points with the 
            // previous peak (rounded peak shape)
            if (this->pt.finf.at(i) == this->pt.finf.at(i-1) &&
                this->pt.tinf.at(i) == this->pt.tinf.at(i-1))
            {
                // ensure that the absolute difference in value of the d2 minima
                // and the maxima between the two d2 minimas is larger than the
                // noise value
                maxima = which_max(this->d2, 
                                   (int) this->pt.apex.at(i-1),
                                   (int) this->pt.apex.at(i));
                
                // if (this->d2.at(this->pt.apex.at(i)) <
                //     this->d2.at(this->pt.apex.at(i-1)))
                // {
                //     smaller = i-1;
                // } else
                // {
                //     smaller = i;
                // }

                if (std::abs(this->d2.at(this->pt.apex.at(i)) -
                    this->d2.at(maxima)) < this->options.apex_thresh ||
                    std::abs(this->d2.at(this->pt.apex.at(i-1)) -
                    this->d2.at(maxima)) < this->options.apex_thresh)
                {
                    if (this->options.output) Rcout << "R";
                    
                    // mark peak for removal now
                    this->pt.remove_these_now.at(i-1) = 1;
                
                // ensure that there is more than min_rounded_pts points between
                // the maxima and minima for at least one of the peaks
                } else if (std::abs(this->pt.apex.at(i) - maxima + 1 < this->options.min_rounded_pts) || 
                           std::abs(this->pt.apex.at(i-1) - maxima + 1 < this->options.min_rounded_pts))
                {
                    if (this->options.output) Rcout << "R";
                    
                    // mark the prior peak for removal, in case there is another rounded event following
                    this->pt.remove_these_now.at(i-1) = 1;
                }
                
                
                
                
            }
        }
        
        if (this->options.output) Rcout << " ";
        
        i++;
    }
    
    if (this->options.output) Rcout << std::endl;
}

void Chromatogram::detect_peaks()
{
    // first find apices using d2
    // this->find_apices();
    
    cpcApexFinder af(this->d2, this->options.w);
    vec_i apices = af.getApices();
    
    if (this->options.output)
    {
        Rcout << "Detected apices: ";
        for (int i = 0; i < static_cast<int>(apices.size()); i++)
        {
            Rcout << apices.at(i);
            if (i < static_cast<int>(apices.size())-1)
            {
                Rcout << ", ";
            }
        }
        Rcout << std::endl;
    }
    
    this->pt.init_peaks(apices);
    
    if (this->options.output) this->pt.summary();
    
    int i,j;
    
    // stack<int> remove_these_now;
    
    // vec_i remove_these_now(this->pt.npeaks, 0);
    
    bool remove_later = false;
    
    // loop over apices and find their inflection points
    for (i = 0; i < this->pt.npeaks; i++)
    {
        if (this->options.output) Rcout << "Peak " << i << ": ";
        
        remove_later = false;
        
        // locate lhs inflection point
        j = this->pt.apex.at(i);
        
        if (this->options.output) Rcout << "apex = " << this->pt.apex.at(i) << "; ";
        
        // if too small -> mark for removal later
        if (this->d2.at(this->pt.apex.at(i)) > -this->options.apex_thresh)
        {
            remove_later = true;
        }
        
        while(j > 0 && this->d2.at(j) < 0.0) j--;
        
        if (this->options.output) Rcout << "finf_idx = " << j << "; ";
        
        // check inflection point
        if (j == 0 && this->d2.at(j) < 0.0)
        {
            // mark for removal now
            // remove_these_now.push(i);
            this->pt.remove_these_now.at(i) = 1;
            
            if (this->options.output) Rcout << "remove" << std::endl;
            
            continue;
            
        } else
        {
            // interpolate lhs inflection point
            this->pt.finf.at(i) = interpolate_x(j, j + 1, this->d2.at(j), 
                             this->d2.at(j + 1), 0.0);
            
            if (this->options.output) Rcout << "finf = " << pt.finf.at(i) << "; ";
        }
        
        // locate rhs inflection point
        j = this->pt.apex.at(i);
        while(j < this->nscans - 1 && this->d2.at(j) < 0.0) j++;
        
        if (this->options.output) Rcout << "tinf_idx = " << j << "; ";
        
        // check inflection point
        if (j == nscans - 1 && d2.at(j) < 0.0)
        {
            // mark for removal now
            // remove_these_now.push(i);
            this->pt.remove_these_now.at(i) = 1;
            
            if (this->options.output) Rcout << "remove" << std::endl;
            
            continue;
            
        } else
        {
            // interpolate rhs inflection point
            this->pt.tinf.at(i) = interpolate_x(j - 1, j, this->d2.at(j - 1), this->d2.at(j), 0.0);
            
            if (this->options.output) Rcout << "tinf = " << this->pt.tinf.at(i) << "; ";
        }
        
        // remove peaks now if it is just a spike (< 2 points < 0.0)
        if (this->pt.tinf.at(i) - this->pt.finf.at(i) < this->options.min_inf_pts)
        {
            // mark for removal now
            // remove_these_now.push(i);
            this->pt.remove_these_now.at(i) = 1;
            
            if (this->options.output) Rcout << "remove" << std::endl;
            
            continue;
        }
        
        // remove peak if it doesnt have sufficient apex points
        if (remove_later)
        {
            // mark for removal later
            this->pt.remove_these_later.at(i) = 1;
            
            if (this->options.output) Rcout << "remove later.";
        }
        
        if (this->options.output) Rcout << std::endl;
    }
    
    // check the detected peaks
    this->check_peaks();
    
    // remove peaks marked for removal now
    // while (!remove_these_now.empty())
    i = 0;
    while (i < this->pt.npeaks)
    {
        // j = remove_these_now.top();
        // remove_these_now.pop();
        // this->pt.remove_peak(j);
        
        if (this->pt.remove_these_now.at(i) > 0)
        {
            this->pt.remove_peak(i);
        } else
        {
            i++;
        }
    }
}

void Chromatogram::calculate_peak_expansion_thresholds(int peak)
{
    if (peak > this->pt.npeaks - 1 || peak < 0) throw("OUT_OF_BOUNDS");
    
    double cur_front_d1, cur_tail_d1, cur_front_d0, cur_tail_d0, baseline_slope;
    
    vec_d thresholds(2, 0.0);
    
    // calculate interpolated d1 value at front inflection point
    cur_front_d1 = this->slope_at_point((double) this->pt.finf.at(peak));
    
    // calculate interpolated d1 value at tail inflection point
    cur_tail_d1 = this->slope_at_point((double) this->pt.tinf.at(peak));
    
    // calculate interpolated d0 value at front inflection point
    cur_front_d0 = interpolate_y(floor(this->pt.finf.at(peak)), floor(this->pt.finf.at(peak))+1, 
                                 d0.at(floor(this->pt.finf.at(peak))), 
                                 d0.at(floor(this->pt.finf.at(peak))+1), 
                                 (double) this->pt.finf.at(peak));
    
    // calculate interpolated d0 value at tail inflection point
    cur_tail_d0 = interpolate_y(floor(this->pt.tinf.at(peak)), floor(this->pt.tinf.at(peak))+1, 
                                d0.at(floor(this->pt.tinf.at(peak))), 
                                d0.at(floor(this->pt.tinf.at(peak))+1), 
                                (double) this->pt.tinf.at(peak));
    
    if (cur_front_d1 > 0 && cur_tail_d1 < 0) // both inflection points are expandable
    {
        // calculate slope between inflection points
        baseline_slope = (cur_tail_d0 - cur_front_d0) / 
            ((double) this->pt.tinf.at(peak) - (double) this->pt.finf.at(peak));
        
        this->pt.fthr.at(peak) = (cur_front_d1 - baseline_slope) * (this->options.liftoff / 100);
        this->pt.tthr.at(peak) = (baseline_slope - cur_tail_d1) * (this->options.touchdown / 100);
        
    } else if (cur_front_d1 > 0) // front is expandable
    {
        baseline_slope = 0;
        
        this->pt.fthr.at(peak) = (cur_front_d1 - baseline_slope) * (this->options.liftoff / 100);
        this->pt.tthr.at(peak) = (cur_front_d1 - baseline_slope) * (this->options.touchdown / 100);
        
    } else if (cur_tail_d1 < 0) // tail is expandable
    {
        baseline_slope = 0;
        
        this->pt.fthr.at(peak) = (baseline_slope - cur_tail_d1) * (this->options.liftoff / 100);
        this->pt.tthr.at(peak) = (baseline_slope - cur_tail_d1) * (this->options.touchdown / 100);
        
    } else
    {
        // calculate slope between inflection points normally (this will give bad results)
        baseline_slope = (cur_tail_d0 - cur_front_d0) / 
            ((double) this->pt.tinf.at(peak) - (double) this->pt.finf.at(peak));
        
        this->pt.fthr.at(peak) = (cur_front_d1 - baseline_slope) * (this->options.liftoff / 100);
        this->pt.tthr.at(peak) = (baseline_slope - cur_tail_d1) * (this->options.touchdown / 100);
    }
    
    if (this->options.output) Rcout << "fthr = " << this->pt.fthr.at(peak) << "; "
                                    << "tthr = " << this->pt.tthr.at(peak) << "; ";
}

vec_i Chromatogram::expand_to_baseline(int fexp, int texp, double fthr, double tthr)
{
    /************************
     * vars and data checks *
     ************************/
    // proc vars

    double cur_baseline_slope;
    double cur_front_diff, cur_tail_diff;

    vec_i cur_bounds(2, -1);

    // data checks

    // check start indices
    if (fexp > texp)
    {
        int tmp = fexp; // swap them
        fexp = texp;
        texp = tmp;
    } else if (fexp == texp) return cur_bounds; // return -1

    /*****************************
     * determine starting points *
     *****************************/
    // front starting point
    cur_bounds[0] = fexp;

    // tail starting point
    cur_bounds[1] = texp;

    /******************************************
     * calculate initial values for expansion *
     ******************************************/

    // calculate slope between bounds
    cur_baseline_slope = (d0[cur_bounds[1]] - d0[cur_bounds[0]]) /
                                                (cur_bounds[1] - cur_bounds[0]);

    // calculate front slope difference from d1 val
    cur_front_diff = this->slope_at_idx(cur_bounds[0]) - cur_baseline_slope;

    // calculate tail slope difference from d1 val
    cur_tail_diff = cur_baseline_slope - this->slope_at_idx(cur_bounds[1]);

    /*************
     * expansion *
     *************/
    
    if (this->options.output)
    {
        Rcout << std::endl;
        Rcout << left << setw(5) << setfill(' ') << "fpt";
        Rcout << left << setw(5) << setfill(' ') << "tpt";
        Rcout << left << setw(10) << setfill(' ') << "bslp";
        Rcout << left << setw(10) << setfill(' ') << "fslp";
        Rcout << left << setw(10) << setfill(' ') << "tslp";
        Rcout << left << setw(10) << setfill(' ') << "fdif";
        Rcout << left << setw(10) << setfill(' ') << "tdif";
        Rcout << std::endl;
    }
    
    while((cur_front_diff > fthr &&
          cur_bounds[0] > 0 &&
          this->slope_at_idx(cur_bounds[0]) > 0) ||
          (cur_tail_diff > tthr &&
          cur_bounds[1] < this->nscans - 1 &&
          this->slope_at_idx(cur_bounds[1]) < 0))
    {
        if (this->options.output)
        {
            Rcout << left << setw(5) << setfill(' ') << cur_bounds[0];
            Rcout << left << setw(5) << setfill(' ') << cur_bounds[1];
            Rcout << left << setw(10) << setfill(' ') << cur_baseline_slope;
            Rcout << left << setw(10) << setfill(' ') << this->slope_at_idx(cur_bounds[0]);
            Rcout << left << setw(10) << setfill(' ') << this->slope_at_idx(cur_bounds[1]);
            Rcout << left << setw(10) << setfill(' ') << cur_front_diff;
            Rcout << left << setw(10) << setfill(' ') << cur_tail_diff;
            Rcout << std::endl;
        }
        
        // expand front
        if (cur_front_diff > fthr && cur_bounds[0] > 0 && 
            this->slope_at_idx(cur_bounds[0]) > 0)
        {
            cur_bounds[0]--;
        }

        // expand tail
        if (cur_tail_diff > tthr && cur_bounds[1] < this->nscans - 1 &&
            this->slope_at_idx(cur_bounds[1]) < 0)
        {
            cur_bounds[1]++;
        }

        // calculate baseline slope
        cur_baseline_slope = (d0[cur_bounds[1]] - d0[cur_bounds[0]]) /
                                                (cur_bounds[1] - cur_bounds[0]);

        // calculate front slope difference from d1 val
        // cur_front_diff = d1[cur_bounds[0]] - cur_baseline_slope;
        cur_front_diff = this->slope_at_idx(cur_bounds[0]) - cur_baseline_slope;

        // calculate tail slope difference from d1 val
        // cur_tail_diff = cur_baseline_slope - d1[cur_bounds[1]];
        cur_tail_diff = cur_baseline_slope - this->slope_at_idx(cur_bounds[1]);
    }
    
    if (this->options.output)
    {
        Rcout << left << setw(5) << setfill(' ') << cur_bounds[0];
        Rcout << left << setw(5) << setfill(' ') << cur_bounds[1];
        Rcout << left << setw(10) << setfill(' ') << cur_baseline_slope;
        Rcout << left << setw(10) << setfill(' ') << this->slope_at_idx(cur_bounds[0]);
        Rcout << left << setw(10) << setfill(' ') << this->slope_at_idx(cur_bounds[1]);
        Rcout << left << setw(10) << setfill(' ') << cur_front_diff;
        Rcout << left << setw(10) << setfill(' ') << cur_tail_diff;
        Rcout << std::endl;
    }

    // return
    return cur_bounds;
}

// void Chromatogram::expand_to_baseline(int _peak)
// {
//     /************************
//      * vars and data checks *
//      ************************/
//     // proc vars
//     
//     double cur_baseline_slope;
//     double cur_front_diff, cur_tail_diff;
//     
//     vec_i cur_bounds(2, -1);
//     
//     // data checks
//     
//     // check start indices
//     if (this->pt.fexp.at(_peak) > this->pt.texp.at(_peak))
//     {
//         int tmp = this->pt.fexp.at(_peak); // swap them
//         this->pt.fexp.at(_peak) = this->pt.texp.at(_peak);
//         this->pt.texp.at(_peak) = tmp;
//     } else if (this->pt.fexp.at(_peak) != this->pt.texp.at(_peak))
//     {
//         /*****************************
//          * determine starting points *
//          *****************************/
//         // front starting point
//         cur_bounds[0] = this->pt.fexp.at(_peak);
//         
//         // tail starting point
//         cur_bounds[1] = this->pt.texp.at(_peak);
//         
//         /******************************************
//          * calculate initial values for expansion *
//          ******************************************/
//         
//         // calculate slope between bounds
//         cur_baseline_slope = (d0[cur_bounds[1]] - d0[cur_bounds[0]]) / 
//         (cur_bounds[1] - cur_bounds[0]);
//         
//         // calculate front slope difference from d1 val
//         // cur_front_diff = d1[cur_bounds[0]] - cur_baseline_slope;
//         cur_front_diff = this->slope_at_idx(cur_bounds[0]) - cur_baseline_slope;
//         
//         // calculate tail slope difference from d1 val
//         // cur_tail_diff = cur_baseline_slope - d1[cur_bounds[1]];
//         cur_tail_diff = cur_baseline_slope - this->slope_at_idx(cur_bounds[1]);
//         
//         /*************
//          * expansion *
//          *************/
//         
//         // while((cur_front_diff > fthr && cur_bounds[0] > 0 && d1[cur_bounds[0]] > 0) ||
//         //       (cur_tail_diff > tthr && cur_bounds[1] < nscans-1 && d1[cur_bounds[1]] < 0))
//         while((cur_front_diff > this->pt.fthr.at(_peak) && 
//         cur_bounds[0] > 0 && 
//         this->slope_at_idx(cur_bounds[0]) > 0) || 
//         (cur_tail_diff > this->pt.tthr.at(_peak) && 
//         cur_bounds[1] < this->nscans - 1 && 
//         this->slope_at_idx(cur_bounds[1]) < 0))
//         {
//             // expand front
//             // if (cur_front_diff > fthr && cur_bounds[0] > 0 && d1[cur_bounds[0]] > 0)
//             if (cur_front_diff > this->pt.fthr.at(_peak) && 
//                 cur_bounds[0] > 0 && 
//                 this->slope_at_idx(cur_bounds[0]) > 0)
//             {
//                 cur_bounds[0]--;
//             }
//             
//             // expand tail
//             // if (cur_tail_diff > tthr && cur_bounds[1] < nscans-1 && d1[cur_bounds[1]] < 0)
//             if (cur_tail_diff > this->pt.tthr.at(_peak) && 
//                 cur_bounds[1] < this->nscans - 1 && 
//                 this->slope_at_idx(cur_bounds[1]) < 0)
//             {
//                 cur_bounds[1]++;
//             }
//             
//             // calculate baseline slope
//             cur_baseline_slope = (d0[cur_bounds[1]] - d0[cur_bounds[0]]) / 
//                 (cur_bounds[1] - cur_bounds[0]);
//             
//             // calculate front slope difference from d1 val
//             // cur_front_diff = d1[cur_bounds[0]] - cur_baseline_slope;
//             cur_front_diff = this->slope_at_idx(cur_bounds[0]) - cur_baseline_slope;
//             
//             // calculate tail slope difference from d1 val
//             // cur_tail_diff = cur_baseline_slope - d1[cur_bounds[1]];
//             cur_tail_diff = cur_baseline_slope - this->slope_at_idx(cur_bounds[1]);
//         }
//         
//         // set new bounds
//         this->pt.fblb.at(_peak) = cur_bounds[0];
//         this->pt.tblb.at(_peak) = cur_bounds[1];
//         this->pt.fpkb.at(_peak) = cur_bounds[0];
//         this->pt.tpkb.at(_peak) = cur_bounds[1];
//     }
//     
// }

void Chromatogram::determine_peak_expansion_start_bounds(int peak)
{
    this->pt.fexp.at(peak) = floor(this->pt.finf.at(peak));
    this->pt.texp.at(peak) = floor(this->pt.tinf.at(peak))+1;
    
    if (this->options.output) Rcout << "fexp = " << this->pt.fexp.at(peak) << "; "
                                    << "texp = " << this->pt.texp.at(peak) << "; ";
}

void Chromatogram::peak_baseline_expansion(int peak)
{
    // calculate thresholds
    this->calculate_peak_expansion_thresholds(peak);
    
    // determine peak expansion starting bounds
    this->determine_peak_expansion_start_bounds(peak);
    
    // expand peak bounds to baseline
    // this->expand_to_baseline(peak);
    vec_i cur_exp_res = this->expand_to_baseline(this->pt.fexp.at(peak),
                                                 this->pt.texp.at(peak),
                                                 this->pt.fthr.at(peak),
                                                 this->pt.tthr.at(peak));
    
    // set values
    this->pt.fblb.at(peak) = cur_exp_res[0];
    this->pt.tblb.at(peak) = cur_exp_res[1];
    this->pt.fpkb.at(peak) = cur_exp_res[0];
    this->pt.tpkb.at(peak) = cur_exp_res[1];
    
    // output
    if (this->options.output) Rcout << "fblb = " << this->pt.fblb.at(peak) << "; "
                                    << "tblb = " << this->pt.tblb.at(peak) << "; "
                                    << "fpkb = " << this->pt.fpkb.at(peak) << "; "
                                    << "tpkb = " << this->pt.tpkb.at(peak) << "; ";
}

void Chromatogram::expand_all_peaks()
{
    for (int i = 0; i < this->pt.npeaks; i++)
    {
        if (this->options.output) Rcout << "Peak: " << i << "; ";
        this->peak_baseline_expansion(i);
        if (this->options.output) Rcout << std::endl;
    }
    
    // if an apex of interest has been supplied (p): determine which detected peak contain
    // the vip
    if (this->options.p > 0) this->detect_vip();
}

// void Chromatogram::detect_clusters()
// {
//     double cur_front_d1, cur_tail_d1, next_front_d1;
//     
//     int new_bound = -1;
//     int i;
//     
//     vec_i cur_clust(2, 0);
//     
//     /*******************************************
//      * loop over all peaks and check apex type *
//      *******************************************/
//     i = 0;
//     if (this->options.output) Rcout << "Checking apex types...\n";
//     while(i < this->pt.npeaks)
//     {
//         // Rcout << "Peak: " << i << "; ";
//         
//         /*********************************************************************
//          * calculate slope at inflection points of current peak
//          *********************************************************************/
//         // cur_front_d1 = interpolate_y(floor(pt.finf.at(i)),
//         //                              floor(pt.finf.at(i))+1,
//         //                              d1[floor(pt.finf.at(i))],
//         //                                d1[floor(pt.finf.at(i))+1],
//         //                                  (double) pt.finf.at(i));
//         cur_front_d1 = this->slope_at_point((double) this->pt.finf.at(i));
//         if (this->options.output) Rcout << "fd1 = " << cur_front_d1 << "; ";
//         
//         // cur_tail_d1 = interpolate_y(floor(pt.tinf.at(i)),
//         //                             floor(pt.tinf.at(i))+1,
//         //                             d1[floor(pt.tinf.at(i))],
//         //                               d1[floor(pt.tinf.at(i))+1],
//         //                                 (double) pt.tinf.at(i));
//         cur_tail_d1 = this->slope_at_point((double) this->pt.tinf.at(i));
//         if (this->options.output) Rcout << "td1 = " << cur_tail_d1 << "; ";
//         
//         
//         /********************************************
//          * determine apex type for the current peak *
//          ********************************************/
//         if (cur_front_d1 > 0 && cur_tail_d1 < 0) // normal apex (type = 1)
//         {
//             if (i > 0 &&
//                 (floor(this->pt.finf.at(i)) == floor(this->pt.finf.at(i-1)) && // rhs rounded peak
//                 floor(this->pt.tinf.at(i)) == floor(this->pt.tinf.at(i-1))))
//             {
//                 if (this->options.output) Rcout << "apex type = 5 (rhs rounded)";
//                 this->pt.atyp.at(i) = 5; // apex type = 5 (rhs rounded)
//                 
//             } else if (i < this->pt.npeaks - 1 &&
//                 (floor(this->pt.finf.at(i)) == floor(this->pt.finf.at(i+1)) && // lhs rounded peak
//                 floor(this->pt.tinf.at(i)) == floor(this->pt.tinf.at(i+1))))
//             {
//                 if (this->options.output) Rcout << "apex type = 4 (lhs rounded)";
//                 this->pt.atyp.at(i) = 4; // apex type = 4 (lhs rounded)
//                 
//             } else
//             {
//                 if (this->options.output) Rcout << "apex type = 1 (normal)";
//                 this->pt.atyp.at(i) = 1; // 
//             }
//             
//         } else if (cur_front_d1 > 0 && cur_tail_d1 >= 0) // front shoulder peak (type = 2)
//         {
//             if (this->options.output) Rcout << "apex type = 2 (lhs shoulder peak)";
//             this->pt.atyp.at(i) = 2; // apex type = 2 (lhs shoulder peak)
//             
//         } else if (cur_front_d1 <= 0 && cur_tail_d1 < 0) // tail shoulder peak (type = 3)
//         {
//             if (this->options.output) Rcout << "apex type = 3 (rhs shoulder peak)";
//             this->pt.atyp.at(i) = 3; // apex type = 3 (rhs shoulder peak)
//             
//         } else
//         {
//             if (this->options.output) Rcout << "REMOVED!\n";
//             // remove peak - its either too noisy or negative it should be removed here so
//             // that it will not affect the cluster processing further down
//             this->pt.remove_peak(i);
//             
//             // restart at same value for i
//             continue;
//         }
//         if (this->options.output) Rcout << std::endl;
//         
//         i++;
//     }
//     
//     // pt.summary();
//     
//     /*******************************************
//      * loop over all peaks and create clusters *
//      *******************************************/
//     i = 0;
//     cur_clust.at(0) = 0;
// 
//     // set first front peak bound type to 0
//     this->pt.ftyp.at(0) = 0;
// 
//     // set first front peak bound to the same as baseline bound
//     this->pt.fpkb.at(0) = this->pt.fblb.at(0);
//     
//     if (this->options.output) Rcout << "Identifying clusters...\n";
//     while(i < this->pt.npeaks)
//     {
//         // add current peak to current cluster
//         cur_clust.at(1) = i;
// 
//         // if (this->options.output) Rcout << "Peak: " << i << "; ";
// 
//         /*********************************************************************
//          * calculate interpolated d1 for current peak front inflection point *
//          *********************************************************************/
//         // cur_front_d1 = interpolate_y(floor(pt.finf.at(i)),
//         //                              floor(pt.finf.at(i))+1,
//         //                              d1[floor(pt.finf.at(i))],
//         //                                d1[floor(pt.finf.at(i))+1],
//         //                                  (double) pt.finf.at(i));
//         cur_front_d1 = this->slope_at_point((double) this->pt.finf.at(i));
// 
//         // calculate interpolated d1 for current peak tail inflection point
//         // cur_tail_d1 = interpolate_y(floor(pt.tinf.at(i)),
//         //                             floor(pt.tinf.at(i))+1,
//         //                             d1[floor(pt.tinf.at(i))],
//         //                               d1[floor(pt.tinf.at(i))+1],
//         //                                 (double) pt.tinf.at(i));
//         cur_tail_d1 = this->slope_at_point((double) this->pt.tinf.at(i));
//         
//         
//         
//         /*******************************************************************
//          * check if there are more peaks and if the next peak belong in
//          * the current cluster.
//          *
//          * if there are no more peaks:
//          *      1 update peak bound type and location for the current peak
//          *      2 add the current cluster to the stack
//          *
//          * if the next peak doesnt belong in the cluster:
//          *      1 update peak bound type and location for the current peak
//          *      2 add the current cluster to the stack
//          *      3 start a new cluster from the next peak
//          *      4 set front bound type and location for the next peak
//          *
//          * if the next peak belongs in the cluster:
//          *      1 determine tail bound type of current peak
//          *      2 determine tail bound location for the current peak
//          *      3 update tail peak bound of current peak and fron peak bound
//          *        of next peak
//          ********************************************************************/
//         // if (this->options.output) Rcout << "cur_front_d1 = " << cur_front_d1 << "; ";
//         // if (this->options.output) Rcout << "cur_tail_d1 = " << cur_tail_d1 << "; ";
//         if (i == this->pt.npeaks - 1) // if last peak
//         {
//             // if (this->options.output) Rcout << "last peak in chromatogram.; ";
//             
//             // set first peak in cluster front code to 0 (baseline)
//             this->pt.ftyp.at(cur_clust.at(0)) = 0;
//             
//             // set last peak in cluster tail code to 1 (baseline)
//             this->pt.ttyp.at(i) = 0;
// 
//             // set last peak in cluster tail bound to baseline bound
//             // already set?
//             this->pt.tpkb.at(i) = this->pt.tblb.at(i);
//             
//             // if (this->options.output) Rcout << "ttyp[i] = " << pt.ttyp.at(i) << "; ";
//             
//             // push current cluster onto stack
//             this->pt.clust.push_back(cur_clust);
// 
//         } else if (!this->pt.check_boundary_overlap(cur_clust.at(0), i + 1)) // next peak doesnt
//                                                                              // belong in cluster
//         {
//             // if (this->options.output) Rcout << "last peak in cluster.; ";
//             
//             // set first peak in cluster front code to 0 (baseline)
//             this->pt.ftyp.at(cur_clust.at(0)) = 0;
//             
//             // set last peak in cluster tail code to 0 (baseline)
//             this->pt.ttyp.at(i) = 0;
// 
//             // set last peak in cluster tail peak bound to tail baseline bound
//             this->pt.tpkb.at(i) = this->pt.tblb.at(i);
// 
//             // push current cluster onto stack
//             this->pt.clust.push_back(cur_clust);
// 
//             // start a new cluster from next peak
//             cur_clust.at(0) = i + 1;
//             
//             // if (this->options.output) Rcout << "ttyp[i] = " << pt.ttyp.at(i) << "; ";
//             // if (this->options.output) Rcout << "ftyp[i] = " << pt.ftyp.at(i) << "; ";
//             // if (this->options.output) Rcout << "ftyp[i+1] = " << pt.ftyp.at(i+1) << "; ";
//             
//             // set first front peak bound to the same as baseline bound
//             this->pt.fpkb.at(i + 1) = this->pt.fblb.at(i + 1);
// 
//         } else // next peak belong in the cluster
//         {
//             // calculate interpolated d1 for next peak front inflection point
//             // next_front_d1 = interpolate_y(floor(pt.finf.at(i+1)),
//             //                               floor(pt.finf.at(i+1))+1,
//             //                               d1[floor(pt.finf.at(i+1))],
//             //                                 d1[floor(pt.finf.at(i+1))+1],
//             //                                   (double) pt.finf.at(i+1));
//             next_front_d1 = this->slope_at_point((double) this->pt.finf.at(i + 1));
//             // if (this->options.output) Rcout << "next_front_d1 = " << next_front_d1 << "; ";
// 
//             // determine tail bound type for the current peak
//             if (cur_tail_d1 < 0 && next_front_d1 > 0) // valley tail
//             {
//                 // if (this->options.output) Rcout << "valley tail.; ";
//                 this->pt.ttyp.at(i) = 1; // 1 = valley
//                 this->pt.ftyp.at(i + 1) = 1; // 1 = valley
//             }
// 
//             if ((cur_tail_d1 < 0 && next_front_d1 < 0) ||
//                 (cur_tail_d1 > 0 && next_front_d1 > 0)) //
//             {
//                 // if (this->options.output) Rcout << "shoulder tail.; ";
//                 pt.ttyp.at(i) = 2; // 2 = shoulder
//                 pt.ftyp.at(i + 1) = 2; // 2 = shoulder
//             }
// 
//             if (this->pt.tinf.at(i) == this->pt.tinf.at(i + 1) &&
//                 this->pt.finf.at(i) == this->pt.finf.at(i + 1)) // rounded tail
//             {
//                 // if (this->options.output) Rcout << "rounded tail.; ";
//                 this->pt.ttyp.at(i) = 3; // 3 = rounded peak
//                 this->pt.ftyp.at(i + 1) = 3; // 3 = rounded peak
//             }
//             
//             // Rcout << "ttyp[i] = " << pt.ttyp.at(i) << "; ";
//             // Rcout << "ftyp[i] = " << pt.ftyp.at(i) << "; ";
//             // Rcout << "ftyp[i+1] = " << pt.ftyp.at(i+1) << "; ";
// 
//             new_bound = -1;
//             
//             // determine tail peak bound location for the current peak and front peak
//             // bound location for the next peak
//             if (this->pt.ttyp.at(i) == 1) // valley
//             {
//                 // set peak bound to minima in d0
//                 new_bound = which_min(this->d0, (int) floor(this->pt.tinf.at(i)),
//                                                 (int) floor(this->pt.finf.at(i + 1)));
// 
//             } else if (this->pt.ttyp.at(i) == 2) // shoulder
//             {
//                 // set peak bound to d2 maxima between inflection points
//                 new_bound = which_max(this->d2, (int) floor(this->pt.tinf.at(i)),
//                                                 (int) floor(this->pt.finf.at(i + 1)));
// 
//             } else if (this->pt.ttyp.at(i) == 3) // rounded
//             {
//                 // set peak bounds to d2 maxima between apices
//                 new_bound = which_max(this->d2, (int) this->pt.apex.at(i),
//                                                 (int) this->pt.apex.at(i + 1));
//             }
//             
//             // if (this->options.output) Rcout << "new_bound = " << new_bound;
//             
//             // update peak bounds
//             this->pt.tpkb.at(i) = new_bound;
//             this->pt.fpkb.at(i + 1) = new_bound;
// 
//         }
//         
//         // if (this->options.output) Rcout << "\n";
// 
//         i++;
//     }
//     
// }

void Chromatogram::detect_clusters()
{
    double cur_fslp, cur_tslp;
    
    int i;
    
    int new_bound = -1;
    
    vec_i cur_cluster(2, 0);
    
    /*******************************************
     * loop over all peaks and check apex type *
     *******************************************/
    i = 0;
    
    while(i < this->pt.npeaks)
    {
        if (this->options.output) Rcout << "# " << i << ": ";
        
        /******************************************************
         * calculate slope at front and tail inflection points
         ******************************************************/
        
        cur_fslp = this->slope_at_point((double) this->pt.finf.at(i));
        cur_tslp = this->slope_at_point((double) this->pt.tinf.at(i));
        
        if (this->options.output) Rcout << setprecision(6) 
                                        << "fpkb " << this->pt.fpkb.at(i) << "; "
                                        << "tpkb " << this->pt.tpkb.at(i) << "; "
                                        << "fslp "
                                        << cur_fslp << "; "
                                        << "tslp " << cur_tslp << "; ";
        
        /**********************************
         * determine preliminary apex type
         **********************************/
        
        // normal apex type
        if (cur_fslp > 0 && cur_tslp < 0)
        {
            this->pt.atyp.at(i) = 1;
            
        // left shoulder
        } else if (cur_fslp > 0 && cur_tslp >= 0)
        {
            this->pt.atyp.at(i) = 2;
            
        // right shoulder
        } else if (cur_fslp <= 0 && cur_tslp < 0)
        {
            this->pt.atyp.at(i) = 3;
            
        // non peak shape -> remove peak
        } else
        {
            this->pt.remove_peak(i);
            
            if (this->options.output) Rcout << "removed" << std::endl;
            
            // restart at same value for i
            continue;
            
        }
        
        if (this->options.output) Rcout << "atyp " << this->pt.atyp.at(i) 
                                        << "; ";
        
        
        /*******************************************************
         * cluster detection and determination of boundary type
         *******************************************************/
        
        new_bound = -1;
        
        // current peak is not first peak in current cluster and is a part of 
        // the current cluster
        if (i > cur_cluster.at(0) && 
            this->pt.check_boundary_overlap(cur_cluster.at(0), i))
        {
            // check for rounded peak shape
            // if ((this->pt.atyp.at(i) == 3 && this->pt.atyp.at(i-1) == 2) &&
            //     (this->pt.finf.at(i) == this->pt.finf.at(i-1) && 
            //      this->pt.tinf.at(i) == this->pt.tinf.at(i-1)))
            if ((this->pt.finf.at(i) == this->pt.finf.at(i-1) && 
                 this->pt.tinf.at(i) == this->pt.tinf.at(i-1)))
            {
                this->pt.atyp.at(i-1) = 4;
                this->pt.atyp.at(i) = 5;
                this->pt.finf.at(i) = this->pt.finf.at(i-1);
                this->pt.tinf.at(i-1) = this->pt.tinf.at(i);
                
                if (this->options.output) Rcout << "rounded; ";
                
            }
            
            // // valley peak bound
            // if ((this->pt.atyp.at(i-1) == 1 || this->pt.atyp.at(i-1) == 3 || this->pt.atyp.at(i-1) == 5) && 
            //     this->pt.atyp.at(i) == 1)
            // {
            // 
            //     
            // shoulder peak bound
            if (this->pt.atyp.at(i-1) == 2 || this->pt.atyp.at(i) == 3)
            {
                if (this->options.output) Rcout << "shoulder; ";
                
                this->pt.ttyp.at(i-1) = 2;
                this->pt.ftyp.at(i) = 2;
                
                // new_bound = which_max(this->d2, (int) floor(this->pt.tinf.at(i - 1)),
                //                                 (int) floor(this->pt.finf.at(i)));
                new_bound = next_max(this->d2, (int) floor(this->pt.tinf.at(i - 1)),
                                               (int) floor(this->pt.finf.at(i)),
                                               -1);
                
            // rounded peak bound
            } else if (this->pt.atyp.at(i-1) == 4 && this->pt.atyp.at(i) == 5)
            {
                this->pt.ttyp.at(i-1) = 3;
                this->pt.ftyp.at(i) = 3;
                
                new_bound = next_max(this->d2, (int) this->pt.apex.at(i - 1),
                                               (int) this->pt.apex.at(i),
                                               -1);
                
            // any other case is a valley
            } else
            {
                this->pt.ttyp.at(i-1) = 1;
                this->pt.ftyp.at(i) = 1;
                
                new_bound = which_min(this->d0, (int) floor(this->pt.tinf.at(i - 1)),
                                      (int) floor(this->pt.finf.at(i)));
                
            }
            
            if (this->options.output) Rcout << "new_bound " << new_bound << "; ";
            
            // set new peak bounds
            this->pt.tpkb.at(i - 1) = new_bound;
            this->pt.fpkb.at(i) = new_bound;
            
        // current peak is not the first in current cluster and is not part of
        // the current cluster
        } else if (i > cur_cluster.at(0))
        {
            // save current cluster
            this->pt.ttyp.at(i-1) = 0;
            cur_cluster.at(1) = i-1;
            this->pt.clust.push_back(cur_cluster);
            
            // initialize new cluster
            cur_cluster.at(0) = i;
            
        }
        
        // check if first peak in cluster
        if (i == cur_cluster.at(0)) this->pt.ftyp.at(i) = 0;
        
        // check if last peak
        if (i == this->pt.npeaks - 1)
        {
            // finalize and save current cluster
            this->pt.ttyp.at(i) = 0;
            cur_cluster.at(1) = i;
            this->pt.clust.push_back(cur_cluster);
            
        }
        
        i++;
        
        if (this->options.output) Rcout << std::endl;
        
    }
    
    // 
    // /* OLD CODE */
    // 
    // while(i < this->pt.npeaks)
    // {
    //     // Rcout << "Peak: " << i << "; ";
    //     
    //     /*********************************************************************
    //      * calculate slope at inflection points of current peak
    //      *********************************************************************/
    //     // cur_front_d1 = interpolate_y(floor(pt.finf.at(i)),
    //     //                              floor(pt.finf.at(i))+1,
    //     //                              d1[floor(pt.finf.at(i))],
    //     //                                d1[floor(pt.finf.at(i))+1],
    //     //                                  (double) pt.finf.at(i));
    //     cur_front_d1 = this->slope_at_point((double) this->pt.finf.at(i));
    //     if (this->options.output) Rcout << "fd1 = " << cur_front_d1 << "; ";
    //     
    //     // cur_tail_d1 = interpolate_y(floor(pt.tinf.at(i)),
    //     //                             floor(pt.tinf.at(i))+1,
    //     //                             d1[floor(pt.tinf.at(i))],
    //     //                               d1[floor(pt.tinf.at(i))+1],
    //     //                                 (double) pt.tinf.at(i));
    //     cur_tail_d1 = this->slope_at_point((double) this->pt.tinf.at(i));
    //     if (this->options.output) Rcout << "td1 = " << cur_tail_d1 << "; ";
    //     
    //     
    //     /********************************************
    //      * determine apex type for the current peak *
    //      ********************************************/
    //     if (cur_front_d1 > 0 && cur_tail_d1 < 0) // normal apex (type = 1)
    //     {
    //         if (i > 0 &&
    //             (floor(this->pt.finf.at(i)) == floor(this->pt.finf.at(i-1)) && // rhs rounded peak
    //             floor(this->pt.tinf.at(i)) == floor(this->pt.tinf.at(i-1))))
    //         {
    //             if (this->options.output) Rcout << "apex type = 5 (rhs rounded)";
    //             this->pt.atyp.at(i) = 5; // apex type = 5 (rhs rounded)
    //             
    //         } else if (i < this->pt.npeaks - 1 &&
    //             (floor(this->pt.finf.at(i)) == floor(this->pt.finf.at(i+1)) && // lhs rounded peak
    //             floor(this->pt.tinf.at(i)) == floor(this->pt.tinf.at(i+1))))
    //         {
    //             if (this->options.output) Rcout << "apex type = 4 (lhs rounded)";
    //             this->pt.atyp.at(i) = 4; // apex type = 4 (lhs rounded)
    //             
    //         } else
    //         {
    //             if (this->options.output) Rcout << "apex type = 1 (normal)";
    //             this->pt.atyp.at(i) = 1; // 
    //         }
    //         
    //     } else if (cur_front_d1 > 0 && cur_tail_d1 >= 0) // front shoulder peak (type = 2)
    //     {
    //         if (this->options.output) Rcout << "apex type = 2 (lhs shoulder peak)";
    //         this->pt.atyp.at(i) = 2; // apex type = 2 (lhs shoulder peak)
    //         
    //     } else if (cur_front_d1 <= 0 && cur_tail_d1 < 0) // tail shoulder peak (type = 3)
    //     {
    //         if (this->options.output) Rcout << "apex type = 3 (rhs shoulder peak)";
    //         this->pt.atyp.at(i) = 3; // apex type = 3 (rhs shoulder peak)
    //         
    //     } else
    //     {
    //         if (this->options.output) Rcout << "REMOVED!\n";
    //         // remove peak - its either too noisy or negative it should be removed here so
    //         // that it will not affect the cluster processing further down
    //         this->pt.remove_peak(i);
    //         
    //         // restart at same value for i
    //         continue;
    //     }
    //     if (this->options.output) Rcout << std::endl;
    //     
    //     i++;
    // }
    // 
    // // pt.summary();
    // 
    // /*******************************************
    //  * loop over all peaks and create clusters *
    //  *******************************************/
    // i = 0;
    // cur_clust.at(0) = 0;
    // 
    // // set first front peak bound type to 0
    // this->pt.ftyp.at(0) = 0;
    // 
    // // set first front peak bound to the same as baseline bound
    // this->pt.fpkb.at(0) = this->pt.fblb.at(0);
    // 
    // if (this->options.output) Rcout << "Identifying clusters...\n";
    // while(i < this->pt.npeaks)
    // {
    //     // add current peak to current cluster
    //     cur_clust.at(1) = i;
    //     
    //     // if (this->options.output) Rcout << "Peak: " << i << "; ";
    //     
    //     /*********************************************************************
    //      * calculate interpolated d1 for current peak front inflection point *
    //      *********************************************************************/
    //     // cur_front_d1 = interpolate_y(floor(pt.finf.at(i)),
    //     //                              floor(pt.finf.at(i))+1,
    //     //                              d1[floor(pt.finf.at(i))],
    //     //                                d1[floor(pt.finf.at(i))+1],
    //     //                                  (double) pt.finf.at(i));
    //     cur_front_d1 = this->slope_at_point((double) this->pt.finf.at(i));
    //     
    //     // calculate interpolated d1 for current peak tail inflection point
    //     // cur_tail_d1 = interpolate_y(floor(pt.tinf.at(i)),
    //     //                             floor(pt.tinf.at(i))+1,
    //     //                             d1[floor(pt.tinf.at(i))],
    //     //                               d1[floor(pt.tinf.at(i))+1],
    //     //                                 (double) pt.tinf.at(i));
    //     cur_tail_d1 = this->slope_at_point((double) this->pt.tinf.at(i));
    //     
    //     
    //     
    //     /*******************************************************************
    //      * check if there are more peaks and if the next peak belong in
    //      * the current cluster.
    //      *
    //      * if there are no more peaks:
    //      *      1 update peak bound type and location for the current peak
    //      *      2 add the current cluster to the stack
    //      *
    //      * if the next peak doesnt belong in the cluster:
    //      *      1 update peak bound type and location for the current peak
    //      *      2 add the current cluster to the stack
    //      *      3 start a new cluster from the next peak
    //      *      4 set front bound type and location for the next peak
    //      *
    //      * if the next peak belongs in the cluster:
    //      *      1 determine tail bound type of current peak
    //      *      2 determine tail bound location for the current peak
    //      *      3 update tail peak bound of current peak and fron peak bound
    //      *        of next peak
    //      ********************************************************************/
    //     // if (this->options.output) Rcout << "cur_front_d1 = " << cur_front_d1 << "; ";
    //     // if (this->options.output) Rcout << "cur_tail_d1 = " << cur_tail_d1 << "; ";
    //     if (i == this->pt.npeaks - 1) // if last peak
    //     {
    //         // if (this->options.output) Rcout << "last peak in chromatogram.; ";
    //         
    //         // set first peak in cluster front code to 0 (baseline)
    //         this->pt.ftyp.at(cur_clust.at(0)) = 0;
    //         
    //         // set last peak in cluster tail code to 1 (baseline)
    //         this->pt.ttyp.at(i) = 0;
    //         
    //         // set last peak in cluster tail bound to baseline bound
    //         // already set?
    //         this->pt.tpkb.at(i) = this->pt.tblb.at(i);
    //         
    //         // if (this->options.output) Rcout << "ttyp[i] = " << pt.ttyp.at(i) << "; ";
    //         
    //         // push current cluster onto stack
    //         this->pt.clust.push_back(cur_clust);
    //         
    //     } else if (!this->pt.check_boundary_overlap(cur_clust.at(0), i + 1)) // next peak doesnt
    //         // belong in cluster
    //     {
    //         // if (this->options.output) Rcout << "last peak in cluster.; ";
    //         
    //         // set first peak in cluster front code to 0 (baseline)
    //         this->pt.ftyp.at(cur_clust.at(0)) = 0;
    //         
    //         // set last peak in cluster tail code to 0 (baseline)
    //         this->pt.ttyp.at(i) = 0;
    //         
    //         // set last peak in cluster tail peak bound to tail baseline bound
    //         this->pt.tpkb.at(i) = this->pt.tblb.at(i);
    //         
    //         // push current cluster onto stack
    //         this->pt.clust.push_back(cur_clust);
    //         
    //         // start a new cluster from next peak
    //         cur_clust.at(0) = i + 1;
    //         
    //         // if (this->options.output) Rcout << "ttyp[i] = " << pt.ttyp.at(i) << "; ";
    //         // if (this->options.output) Rcout << "ftyp[i] = " << pt.ftyp.at(i) << "; ";
    //         // if (this->options.output) Rcout << "ftyp[i+1] = " << pt.ftyp.at(i+1) << "; ";
    //         
    //         // set first front peak bound to the same as baseline bound
    //         this->pt.fpkb.at(i + 1) = this->pt.fblb.at(i + 1);
    //         
    //     } else // next peak belong in the cluster
    //     {
    //         // calculate interpolated d1 for next peak front inflection point
    //         // next_front_d1 = interpolate_y(floor(pt.finf.at(i+1)),
    //         //                               floor(pt.finf.at(i+1))+1,
    //         //                               d1[floor(pt.finf.at(i+1))],
    //         //                                 d1[floor(pt.finf.at(i+1))+1],
    //         //                                   (double) pt.finf.at(i+1));
    //         next_front_d1 = this->slope_at_point((double) this->pt.finf.at(i + 1));
    //         // if (this->options.output) Rcout << "next_front_d1 = " << next_front_d1 << "; ";
    //         
    //         // determine tail bound type for the current peak
    //         if (cur_tail_d1 < 0 && next_front_d1 > 0) // valley tail
    //         {
    //             // if (this->options.output) Rcout << "valley tail.; ";
    //             this->pt.ttyp.at(i) = 1; // 1 = valley
    //             this->pt.ftyp.at(i + 1) = 1; // 1 = valley
    //         }
    //         
    //         if ((cur_tail_d1 < 0 && next_front_d1 < 0) ||
    //             (cur_tail_d1 > 0 && next_front_d1 > 0)) //
    //         {
    //             // if (this->options.output) Rcout << "shoulder tail.; ";
    //             pt.ttyp.at(i) = 2; // 2 = shoulder
    //             pt.ftyp.at(i + 1) = 2; // 2 = shoulder
    //         }
    //         
    //         if (this->pt.tinf.at(i) == this->pt.tinf.at(i + 1) &&
    //             this->pt.finf.at(i) == this->pt.finf.at(i + 1)) // rounded tail
    //         {
    //             // if (this->options.output) Rcout << "rounded tail.; ";
    //             this->pt.ttyp.at(i) = 3; // 3 = rounded peak
    //             this->pt.ftyp.at(i + 1) = 3; // 3 = rounded peak
    //         }
    //         
    //         // Rcout << "ttyp[i] = " << pt.ttyp.at(i) << "; ";
    //         // Rcout << "ftyp[i] = " << pt.ftyp.at(i) << "; ";
    //         // Rcout << "ftyp[i+1] = " << pt.ftyp.at(i+1) << "; ";
    //         
    //         new_bound = -1;
    //         
    //         // determine tail peak bound location for the current peak and front peak
    //         // bound location for the next peak
    //         if (this->pt.ttyp.at(i) == 1) // valley
    //         {
    //             // set peak bound to minima in d0
    //             new_bound = which_min(this->d0, (int) floor(this->pt.tinf.at(i)),
    //                                   (int) floor(this->pt.finf.at(i + 1)));
    //             
    //         } else if (this->pt.ttyp.at(i) == 2) // shoulder
    //         {
    //             // set peak bound to d2 maxima between inflection points
    //             new_bound = which_max(this->d2, (int) floor(this->pt.tinf.at(i)),
    //                                   (int) floor(this->pt.finf.at(i + 1)));
    //             
    //         } else if (this->pt.ttyp.at(i) == 3) // rounded
    //         {
    //             // set peak bounds to d2 maxima between apices
    //             new_bound = which_max(this->d2, (int) this->pt.apex.at(i),
    //                                   (int) this->pt.apex.at(i + 1));
    //         }
    //         
    //         // if (this->options.output) Rcout << "new_bound = " << new_bound;
    //         
    //         // update peak bounds
    //         this->pt.tpkb.at(i) = new_bound;
    //         this->pt.fpkb.at(i + 1) = new_bound;
    //         
    //     }
    //     
    //     // if (this->options.output) Rcout << "\n";
    //     
    //     i++;
    // }
}

void Chromatogram::calculate_baseline_residual(int fblb, int tblb)
{
    double baseline_slope;
    
    if (fblb > tblb) // sanity check...
    {
        int tmp = fblb;
        fblb = tblb;
        tblb = tmp;
    }
    
    if (fblb < 0) fblb = 0;
    if (tblb > (int) this->d0.size() - 1) tblb = (int) this->d0.size() - 1;
    
    baseline_slope = (this->d0.at(tblb) - this->d0.at(fblb)) / (tblb - fblb);
    
    for (int i = fblb; i <= tblb; i++)
    {
        this->d0_res.at(i) = this->d0.at(i) - (this->d0.at(fblb) + ((i - fblb) * baseline_slope));
    }
    
}

// TODO: later adjust this to work against the cluster object when this is implemented
void Chromatogram::adjust_cluster_baseline(vec_i &clust)
{
    // proc vars
    int lowest_point;
    int cur_peak;
    int new_bound;
    
    vec_i lhs_clust(2);
    vec_i rhs_clust(2);
    
    // create a cluster queue and add the supplied cluster to it
    deque< vec_i > new_clust;
    new_clust.push_back(clust);
    
    while(!new_clust.empty())
    {
        clust = new_clust.back();
        new_clust.pop_back();
        
        if (this->options.output) Rcout << "Adjust baseline " << clust[0] << "->" 
                                        << clust[1] << "; ";
        
        if (this->get_vip() > 0 && (clust[1] < this->get_vip() || clust[0] > this->get_vip()))
        {
            if (this->options.output) Rcout << std::endl;
            
            continue; // skip the current cluster
        }
        
        // calculate d0 baseline residual between baseline bounds
        this->calculate_baseline_residual(this->pt.fblb.at(clust[0]), 
                                          this->pt.tblb.at(clust[1]));
        
        // loop over the entire baseline range
        lowest_point = this->pt.fblb.at(clust[0]);
        
        for (int i = this->pt.fblb.at(clust[0]) + 1; i < this->pt.tblb.at(clust[1]); i++)
        {
            if (this->d0_res.at(i) < this->d0_res.at(lowest_point)) lowest_point = i;
        }
        
        if (this->options.output) Rcout << "lowest_point " << lowest_point << "; ";
        
        // if lowest_point is negative -> break the cluster at this point!
        if (lowest_point > this->pt.fblb.at(clust[0]) && 
            this->d0_res.at(lowest_point) < 0.0)
        {
            // determine which peak the break point is in
            cur_peak = -1;
            for (int i = clust[0]; i <= clust[1]; i++)
            {
                if (lowest_point >= this->pt.fpkb.at(i) && lowest_point <= this->pt.tpkb.at(i))
                {
                    cur_peak = i;
                    break;
                }
            }
            
            if (this->options.output) Rcout << "cur_peak " << cur_peak << "; ";
            
            // if the break point is on the front of the peak
            if (lowest_point <= this->pt.apex.at(cur_peak))
            {
                if (this->options.output) Rcout << "front; ";
                
                if (cur_peak == clust[0]) // if cur_peak is first peak in cluster
                {
                    if (this->options.output) Rcout << "first; ";
                    
                    // set new_bound to be the lowest point
                    new_bound = lowest_point;
                    
                    if (this->options.output) Rcout << "new_bound " << new_bound << "; ";
                    
                    // update front baseline bound to new_bound for entire cluster
                    for (int i = clust[0]; i <= clust[1]; i++)
                        this->pt.fblb.at(i) = new_bound;
                    
                    // update front peak bound for clust[0] to new bound
                    this->pt.fpkb.at(clust[0]) = new_bound;
                    
                    // push the cluster back onto the stack for a second check
                    new_clust.push_back(clust);
                    
                } else // if cur_peak is NOT first peak in cluster
                {
                    if (this->options.output) Rcout << "mid; ";
                    
                    // break the cluster at the front bound of the peak
                    lhs_clust[0] = clust[0];
                    lhs_clust[1] = cur_peak-1;
                    
                    rhs_clust[0] = cur_peak;
                    rhs_clust[1] = clust[1];
                    
                    if (this->options.output) Rcout << "lhs_clust " << lhs_clust[0]
                                      << "->" << lhs_clust[1] << "; ";
                    if (this->options.output) Rcout << "rhs_clust " << rhs_clust[0]
                                      << "->" << rhs_clust[1] << "; ";
                    
                    // determine new bound for clusters
                    // new_bound = which_min(d0_res, 
                    //                       pt.apex.at(cur_peak-1)+1,
                    //                       pt.apex.at(cur_peak)-1);
                    new_bound = lowest_point;
                    
                    if (this->options.output) Rcout << "new_bound " << new_bound << "; ";
                    
                    // set new tail baseline bounds for left_cluster
                    for (int i = lhs_clust[0]; i <= lhs_clust[1]; i++)
                        this->pt.tblb.at(i) = new_bound;
                    
                    // set new front baseline bounds for right_cluster
                    for (int i = rhs_clust[0]; i <= rhs_clust[1]; i++)
                        this->pt.fblb.at(i) = new_bound;
                    
                    // set new tail peak bound for last peak in lhs clust
                    this->pt.tpkb.at(lhs_clust[1]) = new_bound;
                    
                    // set new front peak bound for first peak in rhs clust
                    this->pt.fpkb.at(rhs_clust[0]) = new_bound;
                    
                    // update peak bound codes for last peak in lhs clust and first peak
                    // in rhs clust to 0 (baseline)
                    // pt.ftyp.at(lhs_clust[1]) = 0;
                    // pt.ttyp.at(rhs_clust[0]) = 0;
                    
                    // push clusters back onto stack
                    new_clust.push_back(lhs_clust);
                    new_clust.push_back(rhs_clust);
                    
                } // end of if first peak or not
                
            } else // if the break point is on the tail of the peak
            {
                if (this->options.output) Rcout << "tail; ";
                
                if (cur_peak == clust[1]) // if cur_peak is last peak in cluster
                {
                    if (this->options.output) Rcout << "last; ";
                    
                    // set new_bound to the lowest_point
                    new_bound = lowest_point;
                    
                    if (this->options.output) Rcout << "new_bound " << new_bound << "; ";
                    
                    // update tail baseline bound for entire cluster to new_bound
                    for (int i = clust[0]; i <= clust[1]; i++)
                        this->pt.tblb.at(i) = new_bound;
                    
                    // update tail peak bound of clust[1] to new_bound
                    this->pt.tpkb.at(clust[1]) = new_bound;
                    
                    // push the cluster back onto the stack for a second check
                    new_clust.push_back(clust);
                    
                } else
                {
                    if (this->options.output) Rcout << "mid; ";
                    
                    // determine new bound for cluster
                    if (this->pt.apex.at(cur_peak+1) - this->pt.apex.at(cur_peak) > 0)
                    {
                        // break the cluster at the tail bound of the peak
                        lhs_clust[0] = clust[0];
                        lhs_clust[1] = cur_peak;
                        
                        rhs_clust[0] = cur_peak+1;
                        rhs_clust[1] = clust[1];
                        
                        if (this->options.output) Rcout << "lhs_clust " << lhs_clust[0]
                                          << "->" << lhs_clust[1] << "; ";
                        if (this->options.output) Rcout << "rhs_clust " << rhs_clust[0]
                                          << "->" << rhs_clust[1] << "; ";
                        
                        // new_bound = which_min_double(d0,
                        //                              pt.apex.at(cur_peak)+1,
                        //                              pt.apex.at(cur_peak+1)-1);
                        new_bound = lowest_point;
                        
                        if (this->options.output) Rcout << "new_bound " << new_bound << "; ";
                        
                        // set new tail baseline bounds for lhs_clust
                        for (int i = lhs_clust[0]; i <= lhs_clust[1]; i++)
                            this->pt.tblb.at(i) = new_bound;
                        
                        // set new front baseline bounds for rhs_clust
                        for (int i = rhs_clust[0]; i <= rhs_clust[1]; i++)
                            this->pt.fblb.at(i) = new_bound;
                        
                        // set new tail peak bound for lhs_clust[1]
                        this->pt.tpkb.at(lhs_clust[1]) = new_bound;
                        
                        // set new front peak bound for rhs_clust[0]
                        this->pt.fpkb.at(rhs_clust[0]) = new_bound;
                        
                        // update peak bound codes for lhs_clust[1] and rhs_clust[0]
                        // to 0 (baseline)
                        // pt.ftyp.at(lhs_clust[1]) = 0;
                        // pt.ttyp.at(rhs_clust[0]) = 0;
                        
                        // push clusters back onto stack
                        new_clust.push_back(lhs_clust);
                        new_clust.push_back(rhs_clust);
                    }
                } // end of if last peak or not
                
            } // end of if front or tail
        
        } else // no intersection
        {
            // add cluster to final clusters
            this->pt.final_clust.push_back(clust);
            
        } // end of if there is an intersection
        
        if (this->options.output) Rcout << std::endl;
        
    } // end of loop
}

void Chromatogram::expand_all_clusters()
{
    vec_i cur_clust(2);
    vec_i cur_exp_res(2);
    
    int exp_first_peak, exp_last_peak;
    
    while(!this->pt.clust.empty())
    {
        // get next cluster from stack
        cur_clust = this->pt.clust.back();
        this->pt.clust.pop_back();
        
        if (this->options.output) Rcout << "Cluster " << cur_clust[0] << "->" << cur_clust[1] << ": ";
        
        // if the current cluster does not contain the selected peak
        if (this->get_vip() > 0 && (cur_clust[1] < this->get_vip() || cur_clust[0] > this->get_vip()))
        {
            if (this->options.output) Rcout << "skip (vip not in cluster)" << std::endl;
            continue; // skip the current cluster
        }
        
        // if cluster has more than one peak - if only 1 peak in cluster there is no point
        // in expanding it again as it would only produce the same result.
        if (cur_clust[1] - cur_clust[0] > 0)
        {
            exp_first_peak = cur_clust[0];
            exp_last_peak = cur_clust[1];
            
            if (this->options.output) Rcout << "mult; ";
            
            if (this->options.output) Rcout << "fexp " << this->pt.fexp.at(exp_first_peak) << "; "
                                            << "texp " << this->pt.texp.at(exp_last_peak) << "; "
                                            << "fthr " << this->pt.fthr.at(exp_first_peak) << "; "
                                            << "tthr " << this->pt.tthr.at(exp_last_peak) << "; ";
            
            // expand to cluster baseline
            cur_exp_res = this->expand_to_baseline(this->pt.fexp.at(exp_first_peak),
                                                   this->pt.texp.at(exp_last_peak),
                                                   this->pt.fthr.at(exp_first_peak),
                                                   this->pt.tthr.at(exp_last_peak));
            
            if (this->options.output) Rcout << "cur_exp_rep = " << cur_exp_res[0] << ", " 
                                            << cur_exp_res[1] << "; ";
            
            // set baseline values for cluster
            for (int i = cur_clust[0]; i <= cur_clust[1]; i++)
            {
                this->pt.fblb.at(i) = cur_exp_res[0];
                this->pt.tblb.at(i) = cur_exp_res[1];
            }
            
            // set peak bounds for first and last peak in cluster
            this->pt.fpkb.at(cur_clust[0]) = cur_exp_res[0];
            this->pt.tpkb.at(cur_clust[1]) = cur_exp_res[1];
        } else if (this->options.output)
        {
            Rcout << "skip (only 1 peak in cluster)";
        }
        
        if (this->options.output) Rcout << std::endl;
        
        // adjust baseline bounds for current cluster
        this->adjust_cluster_baseline(cur_clust);
    }
}

void Chromatogram::adjust_apices()
{
    // currently apices reported are the d2 apices for the cases where they are not
    // adjusted due to being shoulder peaks
    
    // TODO: The way this is done now might lead to issues if the peak apex is detected as
    // a shoulder when the peak next to it is removed or not detected. Neither of those
    // should reasonable be an issue since if a shoulder peak is detected, the one next to
    // it should also be detected but I should probably manage this differently for best
    // results.
    // 
    // One way to do it could be to keep the vector of localmaxima generated in the
    // find_apices function and use the apices there to do these determinations. I would
    // then also need to keep track of which apex each peaks apex correspond to in this
    // vector.
    
    // loop over all peaks in pt and adjust the apex depending on the peak boundary type
    for (int i = 0; i < this->pt.npeaks; i++)
    {
        switch(this->pt.atyp.at(i))
        {
            case 1:
                // normal apex
                // adjusted apex should be d0 maxima between inflection points
                this->pt.adj_apex.at(i) = which_max(this->d0, (int) floor(this->pt.finf.at(i)),
                                                              (int) floor(this->pt.tinf.at(i))+1);
                
                break;
                
            case 2:
                // front shoulder peak
                // adjusted apex should be the d2 minima (same as apex)
                this->pt.adj_apex.at(i) = this->pt.apex.at(i);
                
                break;
                
            case 3:
                // tail shoulder peak
                // adjusted apex should be the d2 minima (same as apex)
                this->pt.adj_apex.at(i) = this->pt.apex.at(i);
                
                break;
                
            case 4:
                // lhs rounded peak
                // adjusted apex should be the d2 minima (same as apex)
                this->pt.adj_apex.at(i) = this->pt.apex.at(i);
                
                break;
                
            case 5:
                // rhs rounded peak
                // adjusted apex should be the d2 minima (same as apex)
                this->pt.adj_apex.at(i) = this->pt.apex.at(i);
                
                break;
                
            default:
                // to prevent a crash
                // adjusted apex should be d0 maxima between inflection points
                this->pt.adj_apex.at(i) = which_max(this->d0, (int) floor(this->pt.finf.at(i)),
                                                              (int) floor(this->pt.tinf.at(i))+1);
            
                break;
        
        }
    }
}

// TODO: adjust to use st vector as well 
double Chromatogram::integrate_d0(int a, int b)
{
    // Check that the bounds a and b are within scope
    if (a < 0)
    {
        a = 0;
    }
    
    if (b > static_cast<int>(this->d0.size()-1))
    {
        b = static_cast<int>(this->d0.size()-1);
    }
    
    // Check that a is lower than b
    if (a > b)
    {
        // swap them
        int c = b;
        b = a;
        a = c;
    }
    
    double sum = 0.0;
    
    for (int i = a; i < b; i++)
    {
        // sum += ((y[i] + y[i+1]) / 2) * (x[i+1] - x[i]);
        sum += ((this->d0.at(i) + this->d0.at(i+1)) / 2);
    }
    
    return sum;
}

void Chromatogram::calculate_peak_characteristics()
{
    // loop over all peaks
    for (int i = 0; i < this->pt.npeaks; i++)
    {
        // integrate the peak
        this->pt.area.at(i) = this->integrate_d0(this->pt.fpkb.at(i),
                                                 this->pt.tpkb.at(i));
        
        // integrate between inflection points (for emg fitting estimation)
        this->pt.inf_area.at(i) = this->integrate_d0((int) floor(this->pt.finf.at(i)+0.5),
                                                     (int) floor(this->pt.tinf.at(i)+0.5));
    }
}

void Chromatogram::determine_clusters_to_fit()
{
    unsigned int i;
    
    if (this->options.fit_only_vip && this->get_vip() >= 0)
    {
        // determine which cluster the vip is contained in
        i = 0;
        while(i < this->pt.final_clust.size() &&
              !(this->pt.final_clust.at(i).at(0) <= this->get_vip() &&
              this->pt.final_clust.at(i).at(1) >= this->get_vip()))
        {
            i++;
        }
        
        // ensure we arrived at a clusters containing the vip
        if (this->pt.final_clust.at(i).at(0) <= this->get_vip() &&
            this->pt.final_clust.at(i).at(1) >= this->get_vip())
        {
            // output
            if (this->options.output)
            {
                Rcout << "Cluster to fit: " << i << std::endl;
            }
            
            // set clusters to fit to the cluster of the vip
            this->clusters_to_fit = vec_i(1, i);
        } else
        {
            // init an empty vector
            // the emg fit values are initialized with -1
            this->clusters_to_fit = vec_i(0);
            
            // output
            if (this->options.output)
            {
                Rcout << "No cluster fitted (vip missing)" << std::endl;
            }
        }
        
    } else
    {
        // set clusters to fit to be all clusters
        this->clusters_to_fit = vec_i(this->pt.final_clust.size());
        
        i = 0;
        std::fill(this->clusters_to_fit.begin(), this->clusters_to_fit.end(), i++);
    }
}

void Chromatogram::fit_emg()
{
    // output formatting
    const char filler = ' ';
    const int colwidth = 10;
    
    // process vars
    vec_i cur_clust(2);
    vec_i cur_scans(this->options.pts_per_peak);
    vec_i scans_to_fit(0);
    vec_i clusters_to_fit;
    
    // int cur_clust_idx = -1;
    // int cur_n_scans = 0; // n scans to fit for current processed peak
    // unsigned int i, j, k; // iterators
    int n_peaks_to_fit = 0; // n peaks that will be fit for current cluster
    int n_scans_to_fit = 0; // n scans that will be fit for current cluster
    
    double bl_slope;
    double bl_m;
    int bl_x0;
    
    /***************************************************************************
     * DETERMINE WHICH CLUSTERS TO PERFORM EMG FITTING ON
     *
     * if fit_only_vip is set to 1 and a vip has been detected it will only
     * fit the cluster that contain the vip
     *
     * if fit_only_vip is set to 0 and/or no vip is found then it will perform
     * emg fitting on all clusters
     * 
     * TODO: Change logic such that if fit_only_vip == 1 and no vip is found
     *       then no fitting is performed.
     *
     **************************************************************************/
    if (this->options.fit_only_vip && this->get_vip() >= 0)
    {
        if (this->options.output) Rcout << "Only VIP" << std::endl;
        
        // determine which cluster the vip is contained in
        int i = 0;
        while(i < static_cast<int>(this->pt.final_clust.size()) &&
              !(this->pt.final_clust.at(i).at(0) <= this->get_vip() &&
              this->pt.final_clust.at(i).at(1) >= this->get_vip()))
        {
            i++;
        }
        
        if (this->options.output) Rcout << "VIP in cluster: " << i << std::endl;

        // ensure we arrived at a clusters containing the vip
        if (this->pt.final_clust.at(i).at(0) <= this->get_vip() &&
            this->pt.final_clust.at(i).at(1) >= this->get_vip())
        {
            // set clusters to fit to the cluster of the vip
            clusters_to_fit = vec_i(1,i);
            
            // output
            if (this->options.output)
            {
                Rcout << "Cluster to fit: " << i << std::endl;
            }
        } else
        {
            // output
            if (this->options.output)
            {
                Rcout << "No cluster fitted (vip missing)" << std::endl;
            }
        }

    } else
    {
        // set clusters to fit to be all clusters
        clusters_to_fit = vec_i(this->pt.final_clust.size());

        // std::fill(clusters_to_fit.begin(), clusters_to_fit.end(), i++);
        std::iota(clusters_to_fit.begin(), clusters_to_fit.end(), 0);
        
        if (this->options.output)
        {
            Rcout << "Clusters to fit (" << clusters_to_fit.size() << "): ";
            for (int i = 0; i < static_cast<int>(clusters_to_fit.size()); i++)
            {
                Rcout << clusters_to_fit.at(i) << ":[" 
                      << this->pt.final_clust.at(clusters_to_fit.at(i)).at(0)
                      << "->" 
                      << this->pt.final_clust.at(clusters_to_fit.at(i)).at(1) << "] ";
            }
            Rcout << std::endl;
        }
    }
    
    
    
    /***************************************************************************
     * loop over the clusters determined above and perform emg fitting
     **************************************************************************/
    for (int i = 0; i < static_cast<int>(clusters_to_fit.size()); i++)
    {
        if (this->options.output)
        {
            Rcout << std::endl << "Fitting cluster: " << clusters_to_fit.at(i) << std::endl;
        }
        
        // clear the scan vector
        scans_to_fit.clear();
        
        // get the current cluster
        cur_clust = this->pt.final_clust.at(clusters_to_fit.at(i));
        
        
        /**************************************************
         * determine which peaks within the cluster to fit
         **************************************************/
        
        vec_i peaks_to_fit;
        
        // if fit only vip is set
        if (this->options.fit_only_vip && this->get_vip() >= 0)
        {
            double cur_rs = 0.0;
            double wb_j = 0.0;
            double wb_vip = 0.0;
            
            wb_vip = 2*(this->pt.tinf.at(this->get_vip()) - this->pt.finf.at(this->get_vip()));
            
            for (int j = cur_clust.at(0); j <= cur_clust.at(1); j++)
            {
                wb_j = 2*(this->pt.tinf.at(j) - this->pt.finf.at(j));
                
                cur_rs = 2*abs(this->pt.adj_apex.at(j) - this->pt.adj_apex.at(this->get_vip())) / 
                                (1.70*(wb_j + wb_vip));
                
                if (this->options.output)
                {
                    Rcout << "# " << j << ": "
                          << "Rs " << cur_rs << "; "
                          << "wb_vip " << wb_vip << "; wb_j " << wb_j << "; ";
                }
                
                // check if resolution to vip is > 1.2
                // experiments have shown that this is a suitable limit to 
                // ensure no loss of information
                // if (cur_rs <= 1.5 && 
                //     this->d0.at(this->pt.adj_apex.at(j)) >= 
                //     this->options.fit_rel_lim * this->d0.at(this->pt.adj_apex.at(this->get_vip())))
                if (cur_rs <= 1.5)
                {
                    if (this->options.output) Rcout << "added; ";
                    
                    peaks_to_fit.insert(peaks_to_fit.end(), { j });
                }
                
                if (this->options.output) Rcout << std::endl;
            }
            
            if (this->options.output) Rcout << std::endl;
            
        // if all peaks should be fit
        // TODO: implement the same resolution limitation to this case as well
        // to increase speed (low priority as this will only be used when the
        // tool is used to process full chromatograms)
        } else
        {
            // TODO: replace with iota this is slow as hell
            for (int j = cur_clust.at(0); j <= cur_clust.at(1); j++)
            {
                peaks_to_fit.insert(peaks_to_fit.end(), { j });
            }
        }
        
        // 
        // // determine which peaks in the cluster to fit using relative size limit
        // // any peak with an apex value < this->options.rel_fit_lim * largest peak apex val
        // // will not be fitted
        // vec_i peaks_to_fit;
        // int largest_peak = cur_clust.at(0);
        // for (j = cur_clust.at(0); j <= cur_clust.at(1); j++)
        // {
        //     if (this->d0.at(this->pt.adj_apex.at(j)) > 
        //             this->d0.at(this->pt.adj_apex.at(largest_peak)))
        //     {
        //         largest_peak = j;
        //     }
        // }
        // 
        // for (j = cur_clust.at(0); j <= cur_clust.at(1); j++)
        // {
        //     if (this->d0.at(this->pt.adj_apex.at(j)) >= 
        //         this->options.fit_rel_lim * this->d0.at(this->pt.adj_apex.at(largest_peak)))
        //     {
        //         peaks_to_fit.insert(peaks_to_fit.end(), { j });
        //     }
        // }
        
        // determine the number of peaks to fit
        // n_peaks_to_fit = cur_clust.at(1) - cur_clust.at(0) + 1;
        n_peaks_to_fit = (int) peaks_to_fit.size();

        
        /****************************************************************
         * determine which scans to fit by peak
         * 
         * for each peak:
         *   get a set of 20 to 40 (min->max) points spanning the range
         *   between peak bounds including the front and tail peak bound
         * 
         ****************************************************************/
        
        
        // create a vector with the scans to fit for each peak
        n_scans_to_fit = 0;
        // int cur_nscan = 0;
        int j_first_scan = 0;
        int j_last_scan = 0;
        
        for (int j = 0; j < n_peaks_to_fit; j++)
        {
            vec_i cur_scans;
            
            j_last_scan = this->pt.tpkb.at(peaks_to_fit.at(j));
            
            if (j == 0)
            {
                j_first_scan = this->pt.fpkb.at(peaks_to_fit.at(j));
            } else
            {
                j_first_scan = this->pt.fpkb.at(peaks_to_fit.at(j))+1;
            }
            
            if (j_last_scan - j_first_scan + 1 <= this->options.pts_per_peak)
            {
                // all scans between peak bounds are used
                cur_scans = LinearSpacedArray(j_first_scan, j_last_scan,
                                              j_last_scan - j_first_scan + 1);
                
            } else
            {
                // get N=pts_per_peak evenly spaced points between peak bounds
                cur_scans = LinearSpacedArray(j_first_scan, j_last_scan,
                                              this->options.pts_per_peak);
                
            }
            
            // add the scans to the vector (this is rather slow I think...)
            // an alternative could be to loop over the data first and 
            // determine how many scans should be fitted and then preallocate
            // the vector after that.
            scans_to_fit.insert(scans_to_fit.end(), cur_scans.begin(), cur_scans.end());
            
        }
        
        if (this->options.output)
        {
            Rcout << "Scans to fit: ";
            for (int j = 0; j < static_cast<int>(scans_to_fit.size()); j++)
            {
                Rcout << scans_to_fit.at(j);
                if (j < static_cast<int>(scans_to_fit.size()-1)) Rcout << ", ";
            }
            Rcout << std::endl;
        }
        
        n_scans_to_fit = scans_to_fit.size();
        
        
        /***************************************************************
         * create data vectors and setup seeds and bounds for minimizer
         ***************************************************************/
        
        // create data vectors for minimizer
        vec_d x(n_scans_to_fit); // scan index (time)
        vec_d y(n_scans_to_fit); // scan intensity
        vec_d wt(n_scans_to_fit); // weights
        vec_d seed(n_peaks_to_fit*4); // seed params
        vec_d lower(n_peaks_to_fit*4); // lower bounds for params
        vec_d upper(n_peaks_to_fit*4); // upper bounds for params
        vec_d stepsize(n_peaks_to_fit*4); // stepsize for params
        
        // calculate baseline data for the current cluster
        bl_slope = (this->d0.at(this->pt.tblb.at(cur_clust.at(0))) - 
                        this->d0.at(this->pt.fblb.at(cur_clust.at(0)))) / 
                    (this->pt.tblb.at(cur_clust.at(0)) - 
                        this->pt.fblb.at(cur_clust.at(0)));
        bl_m = this->d0.at(this->pt.fblb.at(cur_clust.at(0)));
        bl_x0 = this->pt.fblb.at(cur_clust.at(0));
        
        // populate the vectors
        for (int j = 0; j < n_scans_to_fit; j++)
        {
            // fill x with scan idx
            x.at(j) = scans_to_fit.at(j);
            
            // fill y with d0 value
            // y.at(j) = this->d0.at(scans_to_fit.at(j));
            
            // fill y with baseline corrected d0 vals
            y.at(j) = this->d0.at(scans_to_fit.at(j)) - 
                        (bl_slope * (x.at(j)-bl_x0) + bl_m);
            wt.at(j) = 1.0;
        }
        
        for (int j = 0; j < n_peaks_to_fit; j++)
        {
            // seed
            // seed[0] mu seed value determined as the adj_apex
            // seed[1] sigma seed value determined as half the distance between 
            //         inflection points
            // seed[2] lambda seed value set to log(0.5) (experimentally 
            //         determined)
            // seed[3] area seed value determined by numerical intregration 
            //         between peak bounds
            seed.at(j*4) = (double) this->pt.adj_apex.at(peaks_to_fit.at(j)); // mu
            seed.at(1+(j*4)) = (double) (this->pt.tinf.at(peaks_to_fit.at(j)) -
                this->pt.finf.at(peaks_to_fit.at(j))) / 2; // sigma
            seed.at(2+(j*4)) = std::log(0.1); // lambda
            seed.at(3+(j*4)) = this->integrate_d0((int) this->pt.fpkb.at(peaks_to_fit.at(j)),
                    (int) this->pt.tpkb.at(peaks_to_fit.at(j)));
            
            // lower bounds
            // lower[0] mu lower bound set to seed - 20 scans
            // lower[1] sigma lower bound set to seed * 0.5
            // lower[2] lambda lower bound set to log(1e-4) (heavy tailing)
            // lower[3] area lower bound set to log(seed * 0.5)
            lower.at(j*4) = seed.at(j*4) - 20; // lower mu
            lower.at(1+(j*4)) = seed.at(1+(j*4)) * 0.5; // lower sigma
            lower.at(2+(j*4)) = std::log(1e-4); // lower lambda
            lower.at(3+(j*4)) = std::log(seed.at(3+(j*4)) * 0.5); // lower area
            
            // upper bounds
            // upper[0] mu upper bound set to seed - 20 scans
            // upper[1] sigma upper bound set to seed * 1.5
            // upper[2] lambda upper bound set to log(20) (no tailing)
            // upper[3] area upper bound set to log(seed * 1.5)
            upper.at(j*4) = seed.at(j*4) + 20; // upper mu
            upper.at(1+(j*4)) = seed.at(1+(j*4)) * 1.5; // upper sigma
            upper.at(2+(j*4)) = std::log(20); // upper lambda
            upper.at(3+(j*4)) = std::log(seed.at(3+(j*4)) * 1.5); // upper area (infinite)
            
            // log-scale area of seed
            seed.at(3+(j*4)) = std::log(seed.at(3+(j*4)));
            
            // stepsize
            // initial simplex will have a stepsize of 0.1 times the range of 
            // the feasible region
            stepsize.at(j*4) = (upper.at(j*4) - lower.at(j*4)) * 0.1;
            stepsize.at(1+j*4) = (upper.at(1+j*4) - lower.at(1+j*4)) * 0.1;
            stepsize.at(2+j*4) = (upper.at(2+j*4) - lower.at(2+j*4)) * 0.1;
            stepsize.at(3+j*4) = (upper.at(3+j*4) - lower.at(3+j*4)) * 0.1;
            
        }
        
        if (this->options.output)
        {
            // print seed values
            Rcout << "Seed parameters:" << std::endl;
            Rcout << left << setw(5) << setfill(filler) << "peak";
            Rcout << left << setw(colwidth) << setfill(filler) << "mu";
            Rcout << left << setw(colwidth) << setfill(filler) << "sigma";
            Rcout << left << setw(colwidth) << setfill(filler) << "lambda";
            Rcout << left << setw(colwidth) << setfill(filler) << "area";
            Rcout << left << setw(colwidth) << setfill(filler) << "vip" << std::endl;
            
            for (int j = 0; j < n_peaks_to_fit; j++)
            {
                Rcout << left << setw(5) << setfill(filler) << cur_clust.at(0)+j;
                Rcout << left << setw(colwidth) << setfill(filler) << seed.at((j*4)+0);
                Rcout << left << setw(colwidth) << setfill(filler) << seed.at((j*4)+1);
                Rcout << left << setw(colwidth) << setfill(filler) << seed.at((j*4)+2);
                Rcout << left << setw(colwidth) << setfill(filler) << seed.at((j*4)+3);
                
                if (this->get_vip() == cur_clust.at(0)+j)
                {
                    Rcout << left << setw(colwidth) << setfill(filler) << "*";
                }
                
                Rcout << std::endl;
            }
            Rcout << std::endl;
            
            // print lower bounds
            Rcout << std::endl << "Lower bounds:" << std::endl;
            Rcout << left << setw(5) << setfill(filler) << "peak";
            Rcout << left << setw(colwidth) << setfill(filler) << "mu";
            Rcout << left << setw(colwidth) << setfill(filler) << "sigma";
            Rcout << left << setw(colwidth) << setfill(filler) << "lambda";
            Rcout << left << setw(colwidth) << setfill(filler) << "area";
            Rcout << left << setw(colwidth) << setfill(filler) << "vip" << std::endl;
            
            for (int j = 0; j < n_peaks_to_fit; j++)
            {
                Rcout << left << setw(5) << setfill(filler) << cur_clust.at(0)+j;
                Rcout << left << setw(colwidth) << setfill(filler) << lower.at((j*4)+0);
                Rcout << left << setw(colwidth) << setfill(filler) << lower.at((j*4)+1);
                Rcout << left << setw(colwidth) << setfill(filler) << lower.at((j*4)+2);
                Rcout << left << setw(colwidth) << setfill(filler) << lower.at((j*4)+3);
                
                if (this->get_vip() == cur_clust.at(0)+j)
                {
                    Rcout << left << setw(colwidth) << setfill(filler) << "*";
                }
                
                Rcout << std::endl;
            }
            Rcout << std::endl;
            
            // print upper bounds
            Rcout << std::endl << "Upper bounds:" << std::endl;
            Rcout << left << setw(5) << setfill(filler) << "peak";
            Rcout << left << setw(colwidth) << setfill(filler) << "mu";
            Rcout << left << setw(colwidth) << setfill(filler) << "sigma";
            Rcout << left << setw(colwidth) << setfill(filler) << "lambda";
            Rcout << left << setw(colwidth) << setfill(filler) << "area";
            Rcout << left << setw(colwidth) << setfill(filler) << "vip" << std::endl;
            
            for (int j = 0; j < n_peaks_to_fit; j++)
            {
                Rcout << left << setw(5) << setfill(filler) << cur_clust.at(0)+j;
                Rcout << left << setw(colwidth) << setfill(filler) << upper.at((j*4)+0);
                Rcout << left << setw(colwidth) << setfill(filler) << upper.at((j*4)+1);
                Rcout << left << setw(colwidth) << setfill(filler) << upper.at((j*4)+2);
                Rcout << left << setw(colwidth) << setfill(filler) << upper.at((j*4)+3);
                
                if (this->get_vip() == cur_clust.at(0)+j)
                {
                    Rcout << left << setw(colwidth) << setfill(filler) << "*";
                }
                
                Rcout << std::endl;
            }
            Rcout << std::endl;
            
        }
        
        // instantiate an objective function functor
        // EMGFit(const vec_d &x_, const vec_d &y_, const vec_d &wt_,
        //        const vec_d &seed_, const vec_d &lower_, const vec_d &upper_,
        //        unsigned int npeaks_, unsigned int scalemethod_, double lambdascale_ = 1.0)
        EMGFit objfun(x, y, wt, 
                      seed, lower, upper, 
                      n_peaks_to_fit, 3);
        
        // instantiate a MinimizerControl object -> minctrl
        MinimizerControl control(0); // Nelder-Mead
        control.maxit = this->options.maxit;
        control.maxeval = this->options.maxeval;
        if (this->options.output)
        {
            control.trace = 1;
        } else
        {
            control.trace = 0;
        }
        control.reltol = this->options.reltol;
        control.abstol = this->options.abstol;
        control.alpha = this->options.alpha;
        control.gamma = this->options.gamma;
        control.rho = this->options.rho;
        control.sigma = this->options.sigma;
        
        control.keep = 0;
        
        // instantiate a Minimize object -> opt
        Minimizer<EMGFit> opt(control);
        
        // run minimizer
        opt.nmmin(objfun, seed, stepsize);

        vec_d opt_param = opt.vopt.par;

        // record values
        for (int j = 0; j < n_peaks_to_fit; j++)
        {
            // this->pt.emg_mu.at(cur_clust.at(0)+j) = opt_param.at(j*4);
            // this->pt.emg_sigma.at(cur_clust.at(0)+j) = opt_param.at(1+(j*4));
            // this->pt.emg_lambda.at(cur_clust.at(0)+j) = exp(opt_param.at(2+(j*4)));
            // this->pt.emg_area.at(cur_clust.at(0)+j) = exp(opt_param.at(3+(j*4)));
            // this->pt.emg_conv.at(cur_clust.at(0)+j) = this->fit.convergence();
            this->pt.emg_mu.at(peaks_to_fit.at(j)) = opt_param.at(j*4);
            this->pt.emg_sigma.at(peaks_to_fit.at(j)) = opt_param.at(1+(j*4));
            this->pt.emg_lambda.at(peaks_to_fit.at(j)) = exp(opt_param.at(2+(j*4)));
            this->pt.emg_area.at(peaks_to_fit.at(j)) = exp(opt_param.at(3+(j*4)));
            // this->pt.emg_conv.at(peaks_to_fit.at(j)) = this->fit.convergence();
            this->pt.emg_conv.at(peaks_to_fit.at(j)) = opt.tfval;
        }
        
    }
    
}

void Chromatogram::process_chromatogram()
{
    if (this->options.output) Rcout << "Detecting peaks..." << std::endl;
    
    // run peak detection
    this->detect_peaks();
    
    if (this->options.output) Rcout << "Found " << this->pt.npeaks << " peaks."
                                    << std::endl << std::endl;
    
    // expand all peaks
    if (this->options.output) Rcout << "Expanding peaks..." << std::endl;
    this->expand_all_peaks();
    
    if (this->options.output) pt.summary();
    
    // only run the processing if the VIP is detecter or no p is set
    if ((this->options.p >= 0 && this->get_vip() >= 0) || this->options.p < 0)
    {
        if (this->options.output) Rcout << "Detecting clusters..." << std::endl;
        
        // detect clusters
        this->detect_clusters();
        
        if (this->options.output) Rcout << "Found " << this->pt.clust.size()
                                        << " clusters." << std::endl << std::endl;
        
        // expand all clusters
        if (this->options.output) Rcout << "Expanding all clusters..." << std::endl;
        this->expand_all_clusters();
        
        // adjust apex location to the point maxima
        if (this->options.output) Rcout << "Adjusting apices..." << std::endl;
        this->adjust_apices();

    }

    if (this->options.output) this->print_clusters();

    // remove peaks that have been tagged for removal
    if (this->options.output) Rcout << "Removing tagged peaks... " << std::endl;
    this->pt.remove_tagged_peaks();

    if (this->options.output) this->pt.summary();
    if (this->options.output) this->print_clusters();

    // calculate peak characteristics
    this->calculate_peak_characteristics();

    if (this->options.output) this->pt.summary();
    if (this->options.output) this->print_clusters();

    // fit emg to peak cluster
    if (this->options.fit_emg) this->fit_emg();

    if (this->options.output) this->pt.summary();
    
}


/************************
 * R interface function *
 ************************/

//' @encoding UTF-8
//' @title Rcpp wrapper that calls the C++ chromatogram processing framework
//' 
//' @description
//' 
//' The function takes smoothed d0, d1, and d2 vectors and processes the 
//' chromatogram by baseline expansion of peaks detected in the second 
//' derivative. In short, the algorithm detects all peak apices in the 
//' chromatogram as negative minima craddled by inflection points in the 
//' second derivative of the chromatogram. Each peak apex detected is then
//' subjected to a baseline expansion algorithm in order to determine the
//' peak boundaries and baseline boundaries of the peaks.
//' 
//' It is not recommended to use this function directly unless you know what
//' you are doing. Instead, use the supplied API functions defined.
//'
//' @param d0 Numeric vector containing the smoothed XIC trace
//' @param d2 Numeric vector containing the second derivative of smoothed XIC
//' @param st Numeric vector containing scan times
//' @param apex_thresh Second derivative threshold value.
//' @param w Apex detection window. Do not change unless you know what you are doing.
//' @param p Apex location of a specific peak. Leave blank if you wish to process the entire chromatogram.
//' @param liftoff Slope difference threshold (in percent) for the peak front.
//' @param touchdown Slope difference threshold (in percent) for the peak tail.
//' @param output \code{integer} value (0 = no output, 1 = verbose output).
//' @param fit_emg Indicates if EMG deconvolution should be performed (0 = no EMG deconvolution, 1 = EMG deconvolution will be performed)
//' @param fit_only_vip Indicates if EMG deconvolution should be performed on only the selected peak indicated by p (0 = no, 1 = yes)
//' @param fit_hess Deprecated.
//' @param fit_rel_lim Minimum relative peak height of neighboring peaks to the selected peak that will be deconvoluted.
//' @param pts_per_peak Minimum number of points per peak
//' @param min_shoulder_pts Minimum number of points between minima and maxima for at least one of the peaks in a shoulder pair
//' @param min_rounded_pts Minimum number of points between minima and maxima for at least one of the peaks in a rounded peak pair
//' @param reltol Relative tolerance of the Nelder-Mead minimizer for EMG deconvolution
//' @param abstol Absolute tolerance of the Nelder-Mead minimizer for EMG deconvolution
//' @param alpha Reflection coefficient of the Nelder-Mead minimizer for EMG deconvolution
//' @param gamma Expansion coefficient of the Nelder-Mead minimizer for EMG deconvolution
//' @param rho Contraction coefficient of the Nelder-Mead minimizer for EMG deconvolution
//' @param sigma Shrink coefficient of the Nelder-Mead minimizer for EMG deconvolution
//' @param maxit Maximum iterations for the Nelder-Mead minimizer for EMG deconvolution
//' @param maxeval Maximum objective function calls for the Nelder-Mead minimizer for EMG deconvolution
//' 
//' @return A list of peak characteristics.
//'
//' @export
//'
//' @examples
//' require("signal")
//' x <- seq(1, 200, 1)
//' st <- seq_len(length(x))*0.05
//' vec <- 1e5*dnorm(x, 100, 5) # create a vector with a gaussian peak
//' noise <- rnorm(length(x), 0, 5) # generate a noise vector
//' nvec <- vec + noise # create a noisy `chromatogram`.
//' smvec <- signal::sgolayfilt(nvec, n = 5) # smooth the vector using Savitzky-Golay
//' ddsmvec <- signal::sgolayfilt(nvec, n = 5, m = 2) # get the second derivative of the smoothed vector
//' cpc::process_chromatogram(d0 = smvec, d2 = ddsmvec, st = st, apex_thresh = 10)
// [[Rcpp::export]]
Rcpp::List process_chromatogram(vec_d &d0, 
                                vec_d &d2,
                                vec_d &st,
                                double apex_thresh = 0.0, 
                                int w = 5, 
                                int p = -1,
                                double liftoff = 0.0, 
                                double touchdown = 0.5, 
                                int output = 0,
                                int fit_emg = 1,
                                int fit_only_vip = 1,
                                int fit_hess = 0,
                                double fit_rel_lim = 0.05,
                                int pts_per_peak = 30,
                                int min_shoulder_pts = 3,
                                int min_rounded_pts = 3,
                                const double reltol = 1.0e-8,
                                const double abstol = -1.0e35,
                                const double alpha = 1.0,
                                const double gamma = 2.1,
                                const double rho = 0.75,
                                const double sigma = 0.75,
                                const int maxit = 2000,
                                const int maxeval = 2000)
{
    Chromatogram chrom(d0,              // Smoothed XIC
                       d2,              // Smoothed second derivative
                       st,              // Scantimes
                       apex_thresh,     // Apex intensity threshold
                       w,               // Apex search window width
                       p,               // VIP apex location (for directed search)
                       liftoff,         // Liftoff limit (for baseline expansion)
                       touchdown,       // Touchdown limit (for baseline expansion)
                       output,          // 0 = no output, 1 = output
                       fit_emg,         // 0 = no emg fitting, 1 = emg fitting
                       fit_only_vip,    // 0 = fit all peaks, 1 = fit only vip cluster
                       fit_hess,        // 0 = no hess, 1 = use hess
                       fit_rel_lim,     // relative size limit for fitting emg
                       pts_per_peak,    // max number of points to fit per peak
                       min_shoulder_pts,// Minimum points between maxima and minima for shoulder peaks
                       min_rounded_pts, // Minimum points between maxima and minima for rounded peaks
                       reltol,          // Algorithm convergence tolerance
                       abstol,          // Absolute convergence tolerance
                       alpha,           // Alpha value for the Nelder-Mead algo
                       gamma,           // Gamma value for the Nelder-Mead algo
                       rho,             // Rho value for the Nelder-Mead algo
                       sigma,           // Sigma value for the Nelder-Mead algo
                       maxit,           // max iterations for optimizer
                       maxeval);        // max function evaluations for optimizer
    
    // if (output) Rcout << "Start processing..." << std::endl;
    
    // process chromatogram
    chrom.process_chromatogram();
    
    // chrom.summary();
    
    return Rcpp::List::create(_["current_peak"] = chrom.get_vip(),
                              _["d2_apex"] = chrom.get_apex(),
                              _["adj_apex"] = chrom.get_adj_apex(),
                              _["front_inf"] = chrom.get_finf(),
                              _["tail_inf"] = chrom.get_tinf(),
                              _["front_baseline_bound"] = chrom.get_fblb(),
                              _["tail_baseline_bound"] = chrom.get_tblb(),
                              _["front_peak_bound"] = chrom.get_fpkb(),
                              _["tail_peak_bound"] = chrom.get_tpkb(),
                              _["front_code"] = chrom.get_ftyp(),
                              _["tail_code"] = chrom.get_ttyp(),
                              _["area"] = chrom.get_area(),
                              _["inf_area"] = chrom.get_inf_area(),
                              _["emg_mu"] = chrom.get_emg_mu(),
                              _["emg_sigma"] = chrom.get_emg_sigma(),
                              _["emg_lambda"] = chrom.get_emg_lambda(),
                              _["emg_area"] = chrom.get_emg_area(),
                              _["emg_conv"] = chrom.get_emg_conv());
}

// [[Rcpp::export]]
List test_emgfit(vec_d &x_, vec_d &y_, vec_d &wt_, 
                 vec_d &seed_, vec_d &lower_, vec_d &upper_, vec_d &stepsize_,
                 const int npeaks_,
                 const double alpha_ = 1.0, const double gamma_ = 2.0,
                 const double rho_ = 0.5, const double sigma_ = 0.5,
                 const int trace_ = 0, const int maxit_ = 500, const int maxeval_ = 500,
                 const double reltol_ = 1.0e-8, const double abstol_ = -1.0e-35, 
                 const int trace2_ = 0, const int nmin_ = 1, const int restart_ = 0,
                 const int keep_ = 0, const int scalemethod_ = 1, const double lambdascale_ = 1.0)
{
    // create emg functor object
    // EMGFit(const vec_d &x_, const vec_d &y_, const vec_d &wt_,
    //        const vec_d &seed_, const vec_d &lower_, const vec_d &upper_,
    //        unsigned int npeaks_)
    EMGFit fn(x_, y_, wt_, seed_, lower_, upper_, npeaks_, scalemethod_, lambdascale_);
    
    
    /*****************************************************************************
     * randomly select npar vertices within the feasible region in combination
     * with the given seed and select the vertice with the lowest fval to be the
     * new seed
     *****************************************************************************/
    
    
    // Vertice seedv(seed_);
    // vector<Vertice> screenv(50 + 1, seedv);
    // int lowestv = 0;
    // 
    // srand(time(NULL));
    // 
    // if (trace_)
    // {
    //   Rcout << "Searching random vertices: " << std::endl;
    // }
    // 
    // // calculate the seed values
    // screenv.at(0).fval = fn(screenv.at(0).par);
    // 
    // screenv.at(0).R_print();
    // 
    // // select random 
    // for (unsigned int i = 1; i < screenv.size(); i++)
    // {
    //   // randomly select par vals
    //   for (unsigned int j = 0; j < seed_.size(); j++)
    //   {
    //     // screenv.at(i).par.at(j) = (double) std::rand() % upper_.at(j) + lower_.at(j);
    //     screenv.at(i).par.at(j) = random<double>(lower_.at(j), upper_.at(j));
    //   }
    //   
    //   // run the function on the current vertice
    //   screenv.at(i).fval = fn(screenv.at(i).par);
    //   
    //   // check if this is the new lowest
    //   if (screenv.at(i).fval < screenv.at(lowestv).fval) lowestv = i;
    //   
    //   if (trace_) screenv.at(i).R_print();
    // }
    // 
    // // output
    // Rcpp::Rcout << "New seed selected (" << lowestv << "): " << std::endl;
    // screenv.at(lowestv).R_print();
    // 
    // seed_ = screenv.at(lowestv).par;
    
    /***********************************************
     * run the minimizer with the new selected seed
     ***********************************************/
    
    // Rcout << fn(seed_) << std::endl;
    
    // create minimizer object
    MinimizerControl control(0); // Nelder-Mead
    control.maxit = maxit_;
    control.maxeval = maxeval_;
    control.trace = trace_;
    control.reltol = reltol_;
    control.abstol = abstol_;
    
    control.alpha = alpha_;
    control.gamma = gamma_;
    control.rho = rho_;
    control.sigma = sigma_;
    
    control.keep = keep_;
    
    Minimizer<EMGFit> opt(control);
    
    for (int i = 0; i < nmin_; i++)
    {
        opt.nmmin(fn, seed_, stepsize_);
    }
    
    if (restart_)
    {
        opt.ceval = 0;
        opt.citer = 0;
        opt.nmmin(fn, opt.vopt.par, stepsize_);
        
        // opt.nmmin(fn);
    }
    
    if (trace_) opt.vopt.R_print();
    if (trace_) Rcout << "Function evaluations: " << opt.ceval << std::endl;
    
    Rcpp::List outlist;
    
    outlist.push_back(opt.vopt.par);
    
    if (keep_)
    {
        vec_d par;
        vec_d fval;
        Rcpp::List parlist;
        Rcpp::List histlist;
        
        vector<Simplex> hist = opt.hist;
        
        for (unsigned int i = 0; i < hist.size(); i++)
        {
            fval.clear();
            
            parlist.erase(parlist.begin(),parlist.end());
            
            for (auto &v : hist.at(i).verts)
            {
                fval.push_back(v.fval);
                
                par.clear();
                
                for (auto &p : v.par)
                {
                    par.push_back(p);
                }
                
                parlist.push_back(par);
            }
            
            Rcpp::List simlist = Rcpp::List::create(fval, parlist);
            
            histlist.push_back(simlist);
        }
        
        outlist.push_back(histlist);
    }
    
    return outlist;
    // return opt.vopt.par;
    
}







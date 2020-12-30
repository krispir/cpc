// [[Rcpp::plugins(cpp11)]]

#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]

#include <Rcpp.h>
#include <bits/stdc++.h>
#include <deque>

#include <roptim.h>
// [[Rcpp::depends(roptim)]]

using namespace Rcpp;
using namespace std;

using vec_d = vector<double>;
using vec_i = vector<int>;

#define __APEX  0
#define __FINF  1
#define __TINF  2
#define __FEXP  3
#define __TEXP  4
#define __FTHR  5
#define __TTHR  6
#define __FBLB  7
#define __TBLB  8
#define __FPKB  9
#define __TPKB  10
#define __FTYP  11
#define __TTYP  12

/********************
 * utility function *
 ********************/

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
        if (v[i] > v[max]) max = i;
    
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
        if (v[i] < v[min]) min = i;
        
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
        if (v[i] < v[min]) min = i;
        
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
        if (v[i] > v[max]) max = i;
        
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
        if (v[i] > v[max]) max = i;
        
        return max;
}

// [[Rcpp::export]]
Roptim<EMGFit> c_emgfit(arma::vec &si, arma::vec &st, arma::vec &wt, arma::vec seed, 
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
    
    return opt;
}

/*******************
 * data structures *
 *******************/
struct PeakTable
{
    vec_i apex;         // apex point
    vec_i atyp;         // apex type (1 = normal, 2 = front shoulder apex, 
                        //            3 = tail shoulder peak, 
                        //            4 = lhs rounded apex, 5 = rhs rounded peak)
    vec_i adj_apex;     // adjusted apex points
    vec_d finf, tinf;   // inflection points
    vec_i fexp, texp;   // expansion points
    vec_d fthr, tthr;   // expansion thresholds
    vec_i fblb, tblb;   // baseline bounds
    vec_i fpkb, tpkb;   // peak bounds
    vec_i ftyp, ttyp;   // peak bound types
    
    vec_d emg_mu;       // EMG fitted mu
    vec_d emg_sigma;    // EMG fitted sigma
    vec_d emg_lambda;   // EMG fitted lambda
    vec_d emg_area;     // EMG fitted area
    
    vec_i remove_these_later;
    
    int npeaks;
    int vip = -1;
    
    void init_peaks(vec_i &_apex);
    void remove_peak(int _peak);
    void remove_tagged_peaks();
    
    void summary();
    
    bool check_boundary_overlap(int first, int next);
    
    deque< vec_i > clust;
    deque< vec_i > final_clust;
};

struct ChromatogramOptions
{
    double apex_tresh = 0.0;
    double liftoff = 0.0;
    double touchdown = 0.5;
    int w = 5;
    int p = -1;
    int output = 0;
    int fit_emg = 1;
    int fit_only_vip = 1;
};

/***************************
 * EMGFit class definition *
 ***************************/
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

/*********************************
 * Chromatogram class definition *
 *********************************/
class Chromatogram
{
private:
    int nscans;
    int npeaks;
    
    double apex_thresh;
    double liftoff, touchdown;
    int w;
    int p;
    int vip = -1;
    int output = 0;
    
    PeakTable pt;
    
    vec_d d0;
    vec_d d1;
    vec_d d2;
    vec_d d0_res;
    
    double slope_at_idx(int _idx);
    double slope_at_point(double _point);
    void find_apices();
    void detect_vip();
    
    void calculate_peak_expansion_thresholds(int peak);
    void determine_peak_expansion_start_bounds(int peak);
    void peak_baseline_expansion(int peak);
    void calculate_baseline_residual(int fblb, int tblb);
    void adjust_cluster_baseline(vec_i &clust);
    void detect_peaks(int min_inf_pts);
    void detect_clusters();
    void expand_all_peaks();
    void expand_all_clusters();
    void adjust_apices();
    void fit_emg(int only_vip); // deconvolution with emg
    
    vec_i expand_to_baseline(int fexp, int texp, double fthr, double tthr);
    
public:
    Chromatogram(vec_d &_d0, vec_d &_d1, vec_d &_d2, 
                 double _apex_thresh, int _w, int _p,
                 double _liftoff = 0.0, double _touchdown = 0.5, int _output = 0)
    {
        nscans = (int) _d0.size();
        
        d0 = _d0;
        d1 = _d1;
        d2 = _d2;
        d0_res = vec_d(nscans, 0.0);
        
        apex_thresh = _apex_thresh;
        liftoff = _liftoff;
        touchdown = _touchdown;
        
        w = _w;
        p = _p;
        
        output = _output;
    }
    
    // process methods
    void process_chromatogram(int min_inf_pts); // performs all chromatogram
    
    // getters
    int get_vip() { return pt.vip; }
    
    vec_i get_apex()        { return pt.apex; }
    vec_i get_adj_apex()    { return pt.adj_apex; }
    vec_d get_finf()        { return pt.finf; }
    vec_d get_tinf()        { return pt.tinf; }
    vec_i get_fblb()        { return pt.fblb; }
    vec_i get_tblb()        { return pt.tblb; }
    vec_i get_fpkb()        { return pt.fpkb; }
    vec_i get_tpkb()        { return pt.tpkb; }
    vec_i get_ftyp()        { return pt.ftyp; }
    vec_i get_ttyp()        { return pt.ttyp; }
    
    // summary output
    void summary();
    void print_all();
};

/**********************
 * Method definitions *
 **********************/

// PeakTable methods
void PeakTable::summary()
{
    Rcout << "PeakTable class.\n"
          << npeaks << " peaks detected.\n";
    
    Rcout << "apex:";
    for (int i = 0; i < (int) apex.size(); i++) Rcout << " " << apex.at(i);
    Rcout << "\n\n";
    
    Rcout << "atyp:";
    for (int i = 0; i < (int) atyp.size(); i++) Rcout << " " << atyp.at(i);
    Rcout << "\n\n";
    
    Rcout << "ftyp:";
    for (int i = 0; i < (int) ftyp.size(); i++) Rcout << " " << ftyp.at(i);
    Rcout << "\n\n";
    
    Rcout << "ttyp:";
    for (int i = 0; i < (int) ttyp.size(); i++) Rcout << " " << ttyp.at(i);
    Rcout << "\n\n";
    
    Rcout << "adj_apex:";
    for (int i = 0; i < (int) adj_apex.size(); i++) Rcout << " " << adj_apex.at(i);
    Rcout << "\n\n";
    
    Rcout << "finf:";
    for (int i = 0; i < (int) finf.size(); i++) Rcout << " " << finf.at(i);
    Rcout << "\n\n";
    
    Rcout << "tinf:";
    for (int i = 0; i < (int) tinf.size(); i++) Rcout << " " << tinf.at(i);
    Rcout << "\n\n";
    
    Rcout << "fblb:";
    for (int i = 0; i < (int) fblb.size(); i++) Rcout << " " << fblb.at(i);
    Rcout << "\n\n";
    
    Rcout << "tblb:";
    for (int i = 0; i < (int) tblb.size(); i++) Rcout << " " << tblb.at(i);
    Rcout << "\n\n";
    
    Rcout << "fpkb:";
    for (int i = 0; i < (int) fpkb.size(); i++) Rcout << " " << fpkb.at(i);
    Rcout << "\n\n";
    
    Rcout << "tpkb:";
    for (int i = 0; i < (int) tpkb.size(); i++) Rcout << " " << tpkb.at(i);
    Rcout << "\n\n";
    
    Rcout << "remove_these_later:";
    for (int i = 0; i < (int) remove_these_later.size(); i++) Rcout << " " 
                                << remove_these_later.at(i);
    Rcout << "\n";
    
    Rcout << clust.size() << " clusters detected.\n";
}

void PeakTable::init_peaks(vec_i &_apex)
{
    apex = _apex;
    npeaks = (int) apex.size();
    
    atyp = vec_i(npeaks, -1);
    adj_apex = vec_i(npeaks, -1);
    finf = vec_d(npeaks, -1);
    tinf = vec_d(npeaks, -1);
    fexp = vec_i(npeaks, -1);
    texp = vec_i(npeaks, -1);
    fthr = vec_d(npeaks, -1);
    tthr = vec_d(npeaks, -1);
    fblb = vec_i(npeaks, -1);
    tblb = vec_i(npeaks, -1);
    fpkb = vec_i(npeaks, -1);
    tpkb = vec_i(npeaks, -1);
    ftyp = vec_i(npeaks, -1);
    ttyp = vec_i(npeaks, -1);
    
    remove_these_later = vec_i(npeaks, 0);
}

void PeakTable::remove_peak(int _peak)
{
    /************************
     * remove vector values *
     ************************/
    apex.erase(apex.begin() + _peak);
    atyp.erase(atyp.begin() + _peak);
    adj_apex.erase(adj_apex.begin() + _peak);
    finf.erase(finf.begin() + _peak);
    tinf.erase(tinf.begin() + _peak);
    fexp.erase(fexp.begin() + _peak);
    texp.erase(texp.begin() + _peak);
    fthr.erase(fthr.begin() + _peak);
    tthr.erase(tthr.begin() + _peak);
    fblb.erase(fblb.begin() + _peak);
    tblb.erase(tblb.begin() + _peak);
    fpkb.erase(fpkb.begin() + _peak);
    tpkb.erase(tpkb.begin() + _peak);
    ftyp.erase(ftyp.begin() + _peak);
    ttyp.erase(ttyp.begin() + _peak);
    
    remove_these_later.erase(remove_these_later.begin() + _peak);
    
    /*********************************
     * adjust vip value if necessary *
     *********************************/
    if (vip == _peak)
    {
        vip = -1;
    } else if (vip > _peak)
    {
        vip--;
    }
    
    npeaks--;
}

bool PeakTable::check_boundary_overlap(int first, int next)
{
    bool check = false;
    
    for (int j = first; j < next; j++)
    {
        if (fblb.at(next) <= tblb.at(j))
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
    
    while (i < (int) remove_these_later.size())
    {
        if (remove_these_later.at(i) == 1)
        {
            remove_peak(i);
        } else
        {
            i++;
        }
    }
}

// Chromatogram methods
void Chromatogram::summary()
{
    Rcout << "Chromatogram class.\n"
          << "nscans = " << nscans << "\n";
    
    pt.summary();
}

void Chromatogram::find_apices()
{
    int j;
    int start, end;
    
    int last_min = -1;
    npeaks = 0;
    
    vec_i local_min(nscans, 0);
    
    bool is_local_min = false;
    
    /***********************
     * detect local minima *
     ***********************/
    for (int i = 0; i < nscans; i++)
    {
        is_local_min = true;
        
        if (i < w)
        {
            start = 0;
            end = (2*w)+1;
        } else if (i > nscans-w-1)
        {
            start = nscans - 2*w - 1;
            end = nscans - 1;
        } else
        {
            start = i-w;
            end = i+w;
        }
        
        for (int j = start; j <= end; j++)
        {
            if (d2.at(j) < d2.at(i)) is_local_min = false;
        }
        
        if (is_local_min && d2.at(i) < 0.0)// && d2[i] <= -apex_thresh)
        {
            // if (last_min > 0 && i - last_min < 2*w)
            if (last_min > 0 && i - last_min < w)
            {
                if (d2.at(i) < d2.at(last_min))
                {
                    local_min.at(last_min) = 0;
                    local_min.at(i) = 1;
                }
            } else
            {
                local_min.at(i) = 1;
                npeaks++;
            }
            
            last_min = i;
        }
    }
    
    /**********************
     * create apex vector *
     **********************/
    vec_i apices(npeaks,-1);
    j = 0;
    
    for (int i = 0; i < nscans; i++)
    {
        if (local_min[i] > 0)
        {
            apices[j] = i;
            j++;
        }
    }
    
    /*****************************
     * initialize the peak table *
     *****************************/
    pt.init_peaks(apices);
}

void Chromatogram::detect_vip()
{
    if (p > 0)
    {
        for (int i = 0; i < pt.npeaks; i++)
        {
            // if (pt.finf.at(i) <= p &&
            //     pt.tinf.at(i) >= p)
            // {
            //     if (get_vip() > 0 && 
            //         abs(pt.apex.at(i) - p) < abs(pt.apex.at(get_vip()) - p))
            //     {
            //         pt.vip = i;
            //     } else if (get_vip() < 0)
            //     {
            //         pt.vip = i;
            //     }
            // }
            if (pt.fpkb.at(i) <= p &&
                pt.tpkb.at(i) >= p)
            {
                if (get_vip() > 0 && 
                    abs(pt.apex.at(i) - p) < abs(pt.apex.at(get_vip()) - p))
                {
                    pt.vip = i;
                } else if (get_vip() < 0)
                {
                    pt.vip = i;
                }
            }
        }
    }
}

double Chromatogram::slope_at_idx(int _idx)
{
    if (_idx < 1)
    {
        _idx = 0;
        return d0.at(_idx+1) - d0.at(_idx);
    } else if (_idx > nscans-2)
    {
        _idx = nscans-2;
        return d0.at(_idx) - d0.at(_idx-1);
    } else
    {
        return (d0.at(_idx+1) - d0.at(_idx-1)) / 2;
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

void Chromatogram::detect_peaks(int min_inf_pts = 2)
{
    // first find apices using d2
    this->find_apices();
    
    int j;
    
    stack<int> remove_these_now;
    
    bool remove_later = false;
    
    // loop over apices and find their inflection points
    for (int i = 0; i < pt.npeaks; i++)
    {
        if (output) Rcout << "Peak " << i << ": ";
        remove_later = false;
        
        // locate lhs inflection point
        j = pt.apex.at(i);
        
        if (output) Rcout << "apex = " << pt.apex.at(i) << "; ";
        
        // if too small -> mark for removal later
        if (d2.at(pt.apex.at(i)) > -apex_thresh) remove_later = true;
        
        while(j > 0 && d2.at(j) < 0.0) j--;
        
        if (output) Rcout << "finf_idx = " << j << "; ";
        
        // check inflection point
        if (j == 0 && d2.at(j) < 0.0)
        {
            // mark for removal now
            remove_these_now.push(i);
            
            if (output) Rcout << "remove\n";
            
            continue;
            
        } else
        {
            // interpolate lhs inflection point
            pt.finf.at(i) = interpolate_x(j, j+1, d2.at(j), d2.at(j+1), 0.0);
            
            if (output) Rcout << "finf = " << pt.finf.at(i) << "; ";
        }
        
        // locate rhs inflection point
        j = pt.apex.at(i);
        while(j < nscans - 1 && d2.at(j) < 0.0) j++;
        
        if (output) Rcout << "tinf_idx = " << j << "; ";
        
        // check inflection point
        if (j == nscans - 1 && d2.at(j) < 0.0)
        {
            // mark for removal now
            remove_these_now.push(i);
            
            if (output) Rcout << "remove\n";
            
            continue;
            
        } else
        {
            // interpolate rhs inflection point
            pt.tinf.at(i) = interpolate_x(j-1, j, d2.at(j-1), d2.at(j), 0.0);
            
            if (output) Rcout << "tinf = " << pt.tinf.at(i) << "; ";
        }
        
        // remove peaks now if it is just a spike (< 2 points < 0.0)
        if (pt.tinf.at(i) - pt.finf.at(i) < min_inf_pts)
        {
            // mark for removal now
            remove_these_now.push(i);
            
            if (output) Rcout << "remove\n";
            
            continue;
        }
        
        // remove peak if it doesnt have sufficient apex points
        if (remove_later)
        {
            // mark for removal later
            pt.remove_these_later.at(i) = 1;
            
            if (output) Rcout << "remove later.";
        }
        
        if (output) Rcout << "\n";
    }
    
    // remove peaks marked for removal now
    if (output) Rcout << "Found " << remove_these_now.size() 
                      << " peaks to remove.\nRemoving: ";
    while (!remove_these_now.empty())
    {
        j = remove_these_now.top();
        remove_these_now.pop();
        
        if (output) Rcout << j << " ";

        pt.remove_peak(j);
    }
    if (output) Rcout << "\n";
    
    // if an apex of interest has been supplied (p): determine which detected peak contain
    // the vip
    // if (p > 0) detect_vip();
}

vec_i Chromatogram::expand_to_baseline(int fexp, int texp, 
                                       double fthr, double tthr)
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
    // cur_front_diff = d1[cur_bounds[0]] - cur_baseline_slope;
    cur_front_diff = slope_at_idx(cur_bounds[0]) - cur_baseline_slope;
    
    // calculate tail slope difference from d1 val
    // cur_tail_diff = cur_baseline_slope - d1[cur_bounds[1]];
    cur_tail_diff = cur_baseline_slope - slope_at_idx(cur_bounds[1]);
    
    /*************
     * expansion *
     *************/
    
    // while((cur_front_diff > fthr && cur_bounds[0] > 0 && d1[cur_bounds[0]] > 0) ||
    //       (cur_tail_diff > tthr && cur_bounds[1] < nscans-1 && d1[cur_bounds[1]] < 0))
    while((cur_front_diff > fthr && 
            cur_bounds[0] > 0 && 
            slope_at_idx(cur_bounds[0]) > 0) || 
            (cur_tail_diff > tthr && 
             cur_bounds[1] < nscans-1 && 
             slope_at_idx(cur_bounds[1]) < 0))
    {
        // expand front
        // if (cur_front_diff > fthr && cur_bounds[0] > 0 && d1[cur_bounds[0]] > 0)
        if (cur_front_diff > fthr && cur_bounds[0] > 0 && slope_at_idx(cur_bounds[0]) > 0)
        {
            cur_bounds[0]--;
        }
        
        // expand tail
        // if (cur_tail_diff > tthr && cur_bounds[1] < nscans-1 && d1[cur_bounds[1]] < 0)
        if (cur_tail_diff > tthr && cur_bounds[1] < nscans-1 && 
            slope_at_idx(cur_bounds[1]) < 0)
        {
            cur_bounds[1]++;
        }
        
        // calculate baseline slope
        cur_baseline_slope = (d0[cur_bounds[1]] - d0[cur_bounds[0]]) / 
                                                          (cur_bounds[1] - cur_bounds[0]);
        
        // calculate front slope difference from d1 val
        // cur_front_diff = d1[cur_bounds[0]] - cur_baseline_slope;
        cur_front_diff = slope_at_idx(cur_bounds[0]) - cur_baseline_slope;
        
        // calculate tail slope difference from d1 val
        // cur_tail_diff = cur_baseline_slope - d1[cur_bounds[1]];
        cur_tail_diff = cur_baseline_slope - slope_at_idx(cur_bounds[1]);
    }
    
    // return
    return cur_bounds;
}

void Chromatogram::calculate_peak_expansion_thresholds(int peak)
{
    if (peak > pt.npeaks - 1 || peak < 0) throw("OUT_OF_BOUNDS");
    
    double cur_front_d1, cur_tail_d1, cur_front_d0, cur_tail_d0, baseline_slope;
    
    vec_d thresholds(2, 0.0);
    
    // calculate interpolated d1 value at front inflection point
    // cur_front_d1 = interpolate_y(floor(pt.finf[peak]), floor(pt.finf[peak])+1, 
    //                              d1[floor(pt.finf[peak])], d1[floor(pt.finf[peak])+1],
    //                              (double) pt.finf[peak]);
    cur_front_d1 = slope_at_point((double) pt.finf.at(peak));
    
    if (output) Rcout << "1";
    
    // calculate interpolated d1 value at tail inflection point
    // cur_tail_d1 = interpolate_y(floor(pt.tinf[peak]), floor(pt.tinf[peak])+1, 
    //                             d1[floor(pt.tinf[peak]8)], d1[floor(pt.tinf[peak])+1],
    //                             (double) pt.tinf[peak]);
    cur_tail_d1 = slope_at_point((double) pt.tinf.at(peak));
    
    if (output) Rcout << "2";
    
    // calculate interpolated d0 value at front inflection point
    cur_front_d0 = interpolate_y(floor(pt.finf.at(peak)), floor(pt.finf.at(peak))+1, 
                                 d0.at(floor(pt.finf.at(peak))), 
                                 d0.at(floor(pt.finf.at(peak))+1), 
                                 (double) pt.finf.at(peak));
    
    if (output) Rcout << "3";
    
    // calculate interpolated d0 value at tail inflection point
    cur_tail_d0 = interpolate_y(floor(pt.tinf.at(peak)), floor(pt.tinf.at(peak))+1, 
                                d0.at(floor(pt.tinf.at(peak))), 
                                d0.at(floor(pt.tinf.at(peak))+1), 
                                (double) pt.tinf.at(peak));
    
    if (output) Rcout << "4";
    
    if (cur_front_d1 > 0 && cur_tail_d1 < 0) // both inflection points are expandable
    {
        // calculate slope between inflection points
        baseline_slope = (cur_tail_d0 - cur_front_d0) / 
            ((double) pt.tinf.at(peak) - (double) pt.finf.at(peak));
        
        pt.fthr.at(peak) = (cur_front_d1 - baseline_slope) * (liftoff / 100);
        pt.tthr.at(peak) = (baseline_slope - cur_tail_d1) * (touchdown / 100);
        
    } else if (cur_front_d1 > 0) // front is expandable
    {
        baseline_slope = 0;
        
        pt.fthr.at(peak) = (cur_front_d1 - baseline_slope) * (liftoff / 100);
        pt.tthr.at(peak) = (cur_front_d1 - baseline_slope) * (touchdown / 100);
        
    } else if (cur_tail_d1 < 0) // tail is expandable
    {
        baseline_slope = 0;
        
        pt.fthr.at(peak) = (baseline_slope - cur_tail_d1) * (liftoff / 100);
        pt.tthr.at(peak) = (baseline_slope - cur_tail_d1) * (touchdown / 100);
        
    } else
    {
        // calculate slope between inflection points normally (this will give bad results)
        baseline_slope = (cur_tail_d0 - cur_front_d0) / 
            ((double) pt.tinf.at(peak) - (double) pt.finf.at(peak));
        
        pt.fthr.at(peak) = (cur_front_d1 - baseline_slope) * (liftoff / 100);
        pt.tthr.at(peak) = (baseline_slope - cur_tail_d1) * (touchdown / 100);
    }
    
    if (output) Rcout << "5";
    
    if (output) Rcout << "fthr = " << pt.fthr.at(peak) << "; "
                      << "tthr = " << pt.tthr.at(peak) << "; ";
}

void Chromatogram::determine_peak_expansion_start_bounds(int peak)
{
    pt.fexp.at(peak) = floor(pt.finf.at(peak));
    pt.texp.at(peak) = floor(pt.tinf.at(peak))+1;
    
    if (output) Rcout << "fexp = " << pt.fexp.at(peak) << "; "
                      << "texp = " << pt.texp.at(peak) << "; ";
}

void Chromatogram::peak_baseline_expansion(int peak)
{
    // calculate thresholds
    calculate_peak_expansion_thresholds(peak);
    
    // determine peak expansion starting bounds
    determine_peak_expansion_start_bounds(peak);
    
    // expand peak bounds to baseline
    vec_i baseline_bounds = expand_to_baseline(pt.fexp.at(peak), pt.texp.at(peak), 
                                               pt.fthr.at(peak), pt.tthr.at(peak));
    
    // set new bounds for peak
    pt.fblb.at(peak) = baseline_bounds[0];
    pt.tblb.at(peak) = baseline_bounds[1];
    pt.fpkb.at(peak) = baseline_bounds[0];
    pt.tpkb.at(peak) = baseline_bounds[1];
    
    if (output) Rcout << "fblb = " << pt.fblb.at(peak) << "; "
                      << "tblb = " << pt.tblb.at(peak) << "; "
                      << "fpkb = " << pt.fpkb.at(peak) << "; "
                      << "tpkb = " << pt.tpkb.at(peak) << "; ";
}

void Chromatogram::expand_all_peaks()
{
    for (int i = 0; i < pt.npeaks; i++)
    {
        if (output) Rcout << "Peak: " << i << "; ";
        peak_baseline_expansion(i);
        if (output) Rcout << "\n";
    }
    
    // if an apex of interest has been supplied (p): determine which detected peak contain
    // the vip
    if (p > 0) detect_vip();
}

void Chromatogram::detect_clusters()
{
    double cur_front_d1, cur_tail_d1, next_front_d1;
    
    int new_bound = -1;
    int i;
    
    vec_i cur_clust(2, 0);
    
    /*******************************************
     * loop over all peaks and check apex type *
     *******************************************/
    i = 0;
    // Rcout << "Checking apex types...\n";
    while(i < pt.npeaks)
    {
        // Rcout << "Peak: " << i << "; ";
        
        /*********************************************************************
         * calculate interpolated d1 for current peak front inflection point *
         *********************************************************************/
        // cur_front_d1 = interpolate_y(floor(pt.finf.at(i)),
        //                              floor(pt.finf.at(i))+1,
        //                              d1[floor(pt.finf.at(i))],
        //                                d1[floor(pt.finf.at(i))+1],
        //                                  (double) pt.finf.at(i));
        cur_front_d1 = slope_at_point((double) pt.finf.at(i));
        
        // calculate interpolated d1 for current peak tail inflection point
        // cur_tail_d1 = interpolate_y(floor(pt.tinf.at(i)),
        //                             floor(pt.tinf.at(i))+1,
        //                             d1[floor(pt.tinf.at(i))],
        //                               d1[floor(pt.tinf.at(i))+1],
        //                                 (double) pt.tinf.at(i));
        cur_tail_d1 = slope_at_point((double) pt.tinf.at(i));
        
        
        /********************************************
         * determine apex type for the current peak *
         ********************************************/
        if (cur_front_d1 > 0 && cur_tail_d1 < 0) // normal apex (type = 1)
        {
            if (i > 0 &&
                (floor(pt.finf.at(i)) == floor(pt.finf.at(i-1)) && // rhs rounded peak
                floor(pt.tinf.at(i)) == floor(pt.tinf.at(i-1))))
            {
                // Rcout << "apex type = 5 (rhs rounded)";
                pt.atyp.at(i) = 5; // apex type = 5 (rhs rounded)
                
            } else if (i < pt.npeaks - 1 &&
                (floor(pt.finf.at(i)) == floor(pt.finf.at(i+1)) && // lhs rounded peak
                floor(pt.tinf.at(i)) == floor(pt.tinf.at(i+1))))
            {
                // Rcout << "apex type = 4 (lhs rounded)";
                pt.atyp.at(i) = 4; // apex type = 4 (lhs rounded)
                
            } else
            {
                // Rcout << "apex type = apex type = 1 (normal)";
                pt.atyp.at(i) = 1; // 
            }
            
        } else if (cur_front_d1 > 0 && cur_tail_d1 > 0) // front shoulder peak (type = 2)
        {
            // Rcout << "apex type = 2 (lhs shoulder peak)";
            pt.atyp.at(i) = 2; // apex type = 2 (lhs shoulder peak)
            
        } else if (cur_front_d1 < 0 && cur_tail_d1 < 0) // tail shoulder peak (type = 3)
        {
            // Rcout << "apex type = 3 (rhs shoulder peak)";
            pt.atyp.at(i) = 3; // apex type = 3 (rhs shoulder peak)
            
        } else
        {
            // Rcout << "REMOVED!\n";
            // remove peak - its either too noisy or negative it should be removed here so
            // that it will not affect the cluster processing further down
            pt.remove_peak(i);
            
            // restart at same value for i
            continue;
        }
        // Rcout << "\n";
        
        i++;
    }
    
    // pt.summary();
    
    /*******************************************
     * loop over all peaks and create clusters *
     *******************************************/
    i = 0;
    cur_clust.at(0) = 0;

    // set first front peak bound type to 0
    pt.ftyp.at(0) = 0;

    // set first front peak bound to the same as baseline bound
    pt.fpkb.at(0) = pt.fblb.at(0);
    
    // Rcout << "Identifying clusters...\n";
    while(i < pt.npeaks)
    {
        // add current peak to current cluster
        cur_clust.at(1) = i;

        // Rcout << "Peak: " << i << "; ";

        /*********************************************************************
         * calculate interpolated d1 for current peak front inflection point *
         *********************************************************************/
        // cur_front_d1 = interpolate_y(floor(pt.finf.at(i)),
        //                              floor(pt.finf.at(i))+1,
        //                              d1[floor(pt.finf.at(i))],
        //                                d1[floor(pt.finf.at(i))+1],
        //                                  (double) pt.finf.at(i));
        cur_front_d1 = slope_at_point((double) pt.finf.at(i));

        // calculate interpolated d1 for current peak tail inflection point
        // cur_tail_d1 = interpolate_y(floor(pt.tinf.at(i)),
        //                             floor(pt.tinf.at(i))+1,
        //                             d1[floor(pt.tinf.at(i))],
        //                               d1[floor(pt.tinf.at(i))+1],
        //                                 (double) pt.tinf.at(i));
        cur_tail_d1 = slope_at_point((double) pt.tinf.at(i));
        
        
        
        /*******************************************************************
         * check if there are more peaks and if the next peak belong in
         * the current cluster.
         *
         * if there are no more peaks:
         *      1 update peak bound type and location for the current peak
         *      2 add the current cluster to the stack
         *
         * if the next peak doesnt belong in the cluster:
         *      1 update peak bound type and location for the current peak
         *      2 add the current cluster to the stack
         *      3 start a new cluster from the next peak
         *      4 set front bound type and location for the next peak
         *
         * if the next peak belongs in the cluster:
         *      1 determine tail bound type of current peak
         *      2 determine tail bound location for the current peak
         *      3 update tail peak bound of current peak and fron peak bound
         *        of next peak
         ********************************************************************/
        // Rcout << "cur_front_d1 = " << cur_front_d1 << "; ";
        // Rcout << "cur_tail_d1 = " << cur_tail_d1 << "; ";
        if (i == pt.npeaks - 1) // if last peak
        {
            // Rcout << "last peak in chromatogram.; ";
            
            // set first peak in cluster front code to 0 (baseline)
            pt.ftyp.at(cur_clust.at(0)) = 0;
            
            // set last peak in cluster tail code to 1 (baseline)
            pt.ttyp.at(i) = 0;

            // set last peak in cluster tail bound to baseline bound
            // already set?
            pt.tpkb.at(i) = pt.tblb.at(i);
            
            // Rcout << "ttyp[i] = " << pt.ttyp.at(i) << "; ";
            
            // push current cluster onto stack
            pt.clust.push_back(cur_clust);

        } else if (!pt.check_boundary_overlap(cur_clust.at(0), i+1)) // next peak doesnt
                                                                  // belong in cluster
        {
            // Rcout << "last peak in cluster.; ";
            
            // set first peak in cluster front code to 0 (baseline)
            pt.ftyp.at(cur_clust.at(0)) = 0;
            
            // set last peak in cluster tail code to 0 (baseline)
            pt.ttyp.at(i) = 0;

            // set last peak in cluster tail peak bound to tail baseline bound
            pt.tpkb.at(i) = pt.tblb.at(i);

            // push current cluster onto stack
            pt.clust.push_back(cur_clust);

            // start a new cluster from next peak
            cur_clust.at(0) = i + 1;
            
            // Rcout << "ttyp[i] = " << pt.ttyp.at(i) << "; ";
            // Rcout << "ftyp[i] = " << pt.ftyp.at(i) << "; ";
            // Rcout << "ftyp[i+1] = " << pt.ftyp.at(i+1) << "; ";
            
            // set first front peak bound to the same as baseline bound
            pt.fpkb.at(i + 1) = pt.fblb.at(i + 1);

        } else // next peak belong in the cluster
        {
            // calculate interpolated d1 for next peak front inflection point
            // next_front_d1 = interpolate_y(floor(pt.finf.at(i+1)),
            //                               floor(pt.finf.at(i+1))+1,
            //                               d1[floor(pt.finf.at(i+1))],
            //                                 d1[floor(pt.finf.at(i+1))+1],
            //                                   (double) pt.finf.at(i+1));
            next_front_d1 = slope_at_point((double) pt.finf.at(i+1));
            // Rcout << "next_front_d1 = " << next_front_d1 << "; ";

            // determine tail bound type for the current peak
            if (cur_tail_d1 < 0 && next_front_d1 > 0) // valley tail
            {
                // Rcout << "valley tail.; ";
                pt.ttyp.at(i) = 1; // 1 = valley
                pt.ftyp.at(i+1) = 1; // 1 = valley
            }

            if ((cur_tail_d1 < 0 && next_front_d1 < 0) ||
                (cur_tail_d1 > 0 && next_front_d1 > 0)) //
            {
                // Rcout << "shoulder tail.; ";
                pt.ttyp.at(i) = 2; // 2 = shoulder
                pt.ftyp.at(i+1) = 2; // 2 = shoulder
            }

            if (pt.tinf.at(i) == pt.tinf.at(i+1) &&
                pt.finf.at(i) == pt.finf.at(i+1)) // rounded tail
            {
                // Rcout << "rounded tail.; ";
                pt.ttyp.at(i) = 3; // 3 = rounded peak
                pt.ftyp.at(i+1) = 3; // 3 = rounded peak
            }
            
            // Rcout << "ttyp[i] = " << pt.ttyp.at(i) << "; ";
            // Rcout << "ftyp[i] = " << pt.ftyp.at(i) << "; ";
            // Rcout << "ftyp[i+1] = " << pt.ftyp.at(i+1) << "; ";

            new_bound = -1;
            
            // determine tail peak bound location for the current peak and front peak
            // bound location for the next peak
            if (pt.ttyp.at(i) == 1) // valley
            {
                // set peak bound to minima in d0
                new_bound = which_min(d0, (int) floor(pt.tinf.at(i)),
                                          (int) floor(pt.finf.at(i+1)));

            } else if (pt.ttyp.at(i) == 2) // shoulder
            {
                // set peak bound to d2 maxima between inflection points
                new_bound = which_max(d2, (int) floor(pt.tinf.at(i)),
                                          (int) floor(pt.finf.at(i+1)));

            } else if (pt.ttyp.at(i) == 3) // rounded
            {
                // set peak bounds to d2 maxima between apices
                new_bound = which_max(d2, pt.apex.at(i),
                                          pt.apex.at(i+1));
            }
            
            // Rcout << "new_bound = " << new_bound;
            
            // update peak bounds
            pt.tpkb.at(i) = new_bound;
            pt.fpkb.at(i+1) = new_bound;

        }
        
        // Rcout << "\n";

        i++;
    }
    
    // loop over peaks
    // for (int i = 0; i < pt.npeaks; i++)
    // {
    //     cur_clust[0] = i;
    //     cur_clust[1] = i;
    // 
    //     // set first peak in cluster front code to 1 (baseline)
    //     pt.ftyp.at(i) = 0;
    // 
    //     // set first peak in cluster front bound to its baseline bound
    //     // this should already be done...?
    //     pt.fpkb.at(i) = pt.fblb.at(i);
    // 
    //     // if i is not last point
    //     if (i < pt.npeaks - 1)
    //     {
    //         // locate cluster points
    //         while(i < pt.npeaks - 1 &&
    //               pt.check_boundary_overlap(cur_clust[0], i+1))
    //         {
    //             // calculate interpolated d1 for current peak front inflection point
    //             cur_front_d1 = interpolate_y(floor(pt.finf.at(i)),
    //                                          floor(pt.finf.at(i))+1,
    //                                          d1[floor(pt.finf.at(i))],
    //                                            d1[floor(pt.finf.at(i))+1],
    //                                              (double) pt.finf.at(i));
    // 
    //             // calculate interpolated d1 for current peak tail inflection point
    //             cur_tail_d1 = interpolate_y(floor(pt.tinf.at(i)),
    //                                         floor(pt.tinf.at(i))+1,
    //                                         d1[floor(pt.tinf.at(i))],
    //                                           d1[floor(pt.tinf.at(i))+1],
    //                                             (double) pt.tinf.at(i));
    // 
    //             // calculate interpolated d1 for next peak front inflection point
    //             next_front_d1 = interpolate_y(floor(pt.finf.at(i+1)),
    //                                           floor(pt.finf.at(i+1))+1,
    //                                           d1[floor(pt.finf.at(i+1))],
    //                                             d1[floor(pt.finf.at(i+1))+1],
    //                                               (double) pt.finf.at(i+1));
    // 
    // 
    // 
    //             // determine apex type for the current peak
    //             if (cur_front_d1 > 0 && cur_tail_d1 < 0) // normal apex (type = 1)
    //             {
    //                 if (i > 0 &&
    //                     (floor(pt.finf.at(i)) == floor(pt.finf.at(i-1)) && // rhs rounded peak
    //                      floor(pt.tinf.at(i)) == floor(pt.tinf.at(i-1))))
    //                 {
    //                     pt.atyp.at(i) = 5; // apex type = 5 (rhs rounded)
    //                     pt.atyp.at(i-1) = 4; // apex type = 4 (lhs rounded)
    //                 } else if (i < pt.npeaks-1 &&
    //                            (floor(pt.finf.at(i)) == floor(pt.finf.at(i+1)) && // lhs rounded peak
    //                             floor(pt.tinf.at(i)) == floor(pt.tinf.at(i+1))))
    //                 {
    //                     pt.atyp.at(i) = 4; // apex type = 4 (lhs rounded)
    //                     pt.atyp.at(i+1) = 5; // apex type = 5 (rhs rounded)
    //                 } else
    //                 {
    //                     pt.atyp.at(i) = 1; // apex type = 1 (normal)
    //                 }
    //             } else if (cur_front_d1 > 0 && cur_tail_d1 > 0) // front shoulder peak (type = 2)
    //             {
    //                 pt.atyp.at(i) = 2; // apex type = 2 (lhs shoulder peak)
    //             } else if (cur_front_d1 < 0 && cur_tail_d1 < 0) // tail shoulder peak (type = 3)
    //             {
    //                 pt.atyp.at(i) = 3; // apex type = 3 (rhs shoulder peak)
    //             }
    // 
    // 
    // 
    // 
    //             // check current peak tail bound type
    //             if (cur_tail_d1 < 0 && next_front_d1 > 0) // valley tail
    //             {
    //                 pt.ttyp.at(i) = 1; // 1 = valley
    //                 pt.ftyp.at(i+1) = 1; // 1 = valley
    //             }
    // 
    //             if ((cur_tail_d1 < 0 && next_front_d1 < 0) ||
    //                 (cur_tail_d1 > 0 && next_front_d1 > 0)) //
    //             {
    //                 pt.ttyp.at(i) = 2; // 2 = shoulder
    //                 pt.ftyp.at(i+1) = 2; // 2 = shoulder
    //             }
    // 
    //             if (pt.tinf.at(i) == pt.tinf.at(i+1) &&
    //                 pt.finf.at(i) == pt.finf.at(i+1)) // rounded tail
    //             {
    //                 pt.ttyp.at(i) = 3; // 3 = rounded peak
    //                 pt.ftyp.at(i+1) = 3; // 3 = rounded peak
    //             }
    // 
    // 
    // 
    // 
    //             // adjust peak bounds
    //             if (pt.ttyp.at(i) == 1) // valley
    //             {
    //                 // set peak bound to minima in d0
    //                 new_bound = which_min(d0, (int) floor(pt.tinf.at(i)),
    //                                           (int) floor(pt.finf.at(i+1)));
    //             } else if (pt.ttyp.at(i) == 2) // shoulder
    //             {
    //                 // set peak bound to d2 maxima between inflection points
    //                 new_bound = which_max(d2, (int) floor(pt.tinf.at(i)),
    //                                           (int) floor(pt.finf.at(i+1)));
    //             } else if (pt.ttyp.at(i) == 3) // rounded
    //             {
    //                 // set peak bounds to d2 maxima between apices
    //                 new_bound = which_max(d2, pt.apex.at(i),
    //                                           pt.apex.at(i+1));
    //             }
    // 
    //             pt.tpkb.at(i) = new_bound;
    //             pt.fpkb.at(i+1) = new_bound;
    // 
    //             // move to next peak
    //             i++;
    //         }
    //     }
    // 
    //     cur_clust[1] = i;
    // 
    //     // set last peak in cluster tail code to 1 (baseline)
    //     pt.ttyp.at(i) = 0;
    // 
    //     // set last peak in cluster tail bound to baseline bound
    //     // already set?
    //     pt.tpkb.at(i) = pt.tblb.at(i);
    // 
    //     pt.clust.push_back(cur_clust);
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
    if (tblb > (int) d0.size() - 1) tblb = (int) d0.size() - 1;
    
    baseline_slope = (d0.at(tblb) - d0.at(fblb)) / (tblb - fblb);
    
    for (int i = fblb; i <= tblb; i++)
    {
        d0_res.at(i) = d0.at(i) - (d0.at(fblb) + ((i - fblb) * baseline_slope));
    }
}

void Chromatogram::adjust_cluster_baseline(vec_i &clust)
{
    // proc vars
    int lowest_point;
    int cur_peak;
    int new_bound;
    
    vec_i lhs_clust(2);
    vec_i rhs_clust(2);
    
    deque< vec_i > new_clust;
    new_clust.push_back(clust);
    
    while(!new_clust.empty())
    {
        clust = new_clust.back();
        new_clust.pop_back();
        
        if (output) Rcout << "Adjust baseline " << clust[0] << "->" 
                          << clust[1] << "; ";
        
        if (get_vip() > 0 && (clust[1] < get_vip() || clust[0] > get_vip()))
        {
            if (output) Rcout << "\n";
            
            continue; // skip the current cluster
        }
        
        
        // calculate d0 baseline residual between baseline bounds
        calculate_baseline_residual(pt.fblb.at(clust[0]), 
                                    pt.tblb.at(clust[1]));
        
        // loop over the entire baseline range
        lowest_point = pt.fblb.at(clust[0]);
        
        for (int i = pt.fblb.at(clust[0]) + 1; i < pt.tblb.at(clust[1]); i++)
        {
            if (d0_res.at(i) < d0_res.at(lowest_point)) lowest_point = i;
        }
        
        if (output) Rcout << "lowest_point " << lowest_point << "; ";
        
        // if lowest_point is negative -> break the cluster at this point!
        if (lowest_point > pt.fblb.at(clust[0]) && 
            d0_res.at(lowest_point) < 0.0)
        {
            // determine which peak the break point is in
            cur_peak = -1;
            for (int i = clust[0]; i <= clust[1]; i++)
            {
                if (lowest_point >= pt.fpkb.at(i) && lowest_point <= pt.tpkb.at(i))
                {
                    cur_peak = i;
                    break;
                }
            }
            
            if (output) Rcout << "cur_peak " << cur_peak << "; ";
            
            // if the break point is on the front of the peak
            if (lowest_point <= pt.apex.at(cur_peak))
            {
                if (output) Rcout << "front; ";
                
                if (cur_peak == clust[0]) // if cur_peak is first peak in cluster
                {
                    if (output) Rcout << "first; ";
                    
                    // set new_bound to be the lowest point
                    new_bound = lowest_point;
                    
                    if (output) Rcout << "new_bound " << new_bound << "; ";
                    
                    // update front baseline bound to new_bound for entire cluster
                    for (int i = clust[0]; i <= clust[1]; i++)
                        pt.fblb.at(i) = new_bound;
                    
                    // update front peak bound for clust[0] to new bound
                    pt.fpkb.at(clust[0]) = new_bound;
                    
                    // push the cluster back onto the stack for a second check
                    new_clust.push_back(clust);
                    
                } else // if cur_peak is NOT first peak in cluster
                {
                    if (output) Rcout << "mid; ";
                    
                    // break the cluster at the front bound of the peak
                    lhs_clust[0] = clust[0];
                    lhs_clust[1] = cur_peak-1;
                    
                    rhs_clust[0] = cur_peak;
                    rhs_clust[1] = clust[1];
                    
                    if (output) Rcout << "lhs_clust " << lhs_clust[0]
                                      << "->" << lhs_clust[1] << "; ";
                    if (output) Rcout << "rhs_clust " << rhs_clust[0]
                                      << "->" << rhs_clust[1] << "; ";
                    
                    // determine new bound for clusters
                    // new_bound = which_min(d0_res, 
                    //                       pt.apex.at(cur_peak-1)+1,
                    //                       pt.apex.at(cur_peak)-1);
                    new_bound = lowest_point;
                    
                    if (output) Rcout << "new_bound " << new_bound << "; ";
                    
                    // set new tail baseline bounds for left_cluster
                    for (int i = lhs_clust[0]; i <= lhs_clust[1]; i++)
                        pt.tblb.at(i) = new_bound;
                    
                    // set new front baseline bounds for right_cluster
                    for (int i = rhs_clust[0]; i <= rhs_clust[1]; i++)
                        pt.fblb.at(i) = new_bound;
                    
                    // set new tail peak bound for last peak in lhs clust
                    pt.tpkb.at(lhs_clust[1]) = new_bound;
                    
                    // set new front peak bound for first peak in rhs clust
                    pt.fpkb.at(rhs_clust[0]) = new_bound;
                    
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
                if (output) Rcout << "tail; ";
                
                if (cur_peak == clust[1]) // if cur_peak is last peak in cluster
                {
                    if (output) Rcout << "last; ";
                    
                    // set new_bound to the lowest_point
                    new_bound = lowest_point;
                    
                    if (output) Rcout << "new_bound " << new_bound << "; ";
                    
                    // update tail baseline bound for entire cluster to new_bound
                    for (int i = clust[0]; i <= clust[1]; i++)
                        pt.tblb.at(i) = new_bound;
                    
                    // update tail peak bound of clust[1] to new_bound
                    pt.tpkb.at(clust[1]) = new_bound;
                    
                    // push the cluster back onto the stack for a second check
                    new_clust.push_back(clust);
                    
                } else
                {
                    if (output) Rcout << "mid; ";
                    
                    // determine new bound for cluster
                    if (pt.apex.at(cur_peak+1) - pt.apex.at(cur_peak) > 0)
                    {
                        // break the cluster at the tail bound of the peak
                        lhs_clust[0] = clust[0];
                        lhs_clust[1] = cur_peak;
                        
                        rhs_clust[0] = cur_peak+1;
                        rhs_clust[1] = clust[1];
                        
                        if (output) Rcout << "lhs_clust " << lhs_clust[0]
                                          << "->" << lhs_clust[1] << "; ";
                        if (output) Rcout << "rhs_clust " << rhs_clust[0]
                                          << "->" << rhs_clust[1] << "; ";
                        
                        // new_bound = which_min_double(d0,
                        //                              pt.apex.at(cur_peak)+1,
                        //                              pt.apex.at(cur_peak+1)-1);
                        new_bound = lowest_point;
                        
                        if (output) Rcout << "new_bound " << new_bound << "; ";
                        
                        // set new tail baseline bounds for lhs_clust
                        for (int i = lhs_clust[0]; i <= lhs_clust[1]; i++)
                            pt.tblb.at(i) = new_bound;
                        
                        // set new front baseline bounds for rhs_clust
                        for (int i = rhs_clust[0]; i <= rhs_clust[1]; i++)
                            pt.fblb.at(i) = new_bound;
                        
                        // set new tail peak bound for lhs_clust[1]
                        pt.tpkb.at(lhs_clust[1]) = new_bound;
                        
                        // set new front peak bound for rhs_clust[0]
                        pt.fpkb.at(rhs_clust[0]) = new_bound;
                        
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
            
        } // end of if there is an intersection
        
        if (output) Rcout << "\n";
        
    } // end of loop
}

void Chromatogram::expand_all_clusters()
{
    vec_i cur_clust(2);
    vec_i cur_exp_res(2);
    
    int exp_first_peak, exp_last_peak;
    
    while(!pt.clust.empty())
    {
        // get next cluster from stack
        cur_clust = pt.clust.back();
        pt.clust.pop_back();
        
        if (output) Rcout << "Cluster " << cur_clust[0] << "->" << cur_clust[1] << ": ";
        
        // if the current cluster does not contain the selected peak
        if (get_vip() > 0 && (cur_clust[1] < get_vip() || cur_clust[0] > get_vip()))
        {
            if (output) Rcout << "\n";
            continue; // skip the current cluster
        }
        
        // if cluster has more than one peak - if only 1 peak in cluster there is no point
        // in expanding it again as it would only produce the same result.
        if (cur_clust[1] - cur_clust[0] > 0)
        {
            exp_first_peak = cur_clust[0];
            exp_last_peak = cur_clust[1];
            
            if (output) Rcout << "mult; ";
            
            if (output) Rcout << "fexp " << pt.fexp.at(exp_first_peak) << "; "
                              << "texp " << pt.texp.at(exp_last_peak) << "; "
                              << "fthr " << pt.fthr.at(exp_first_peak) << "; "
                              << "tthr " << pt.tthr.at(exp_last_peak) << "; ";
            
            // expand to cluster baseline
            cur_exp_res = expand_to_baseline(pt.fexp.at(exp_first_peak),
                                             pt.texp.at(exp_last_peak),
                                             pt.fthr.at(exp_first_peak),
                                             pt.tthr.at(exp_last_peak));
            
            if (output) Rcout << "cur_exp_rep = " << cur_exp_res[0] << ", " 
                              << cur_exp_res[1] << "; ";
            
            // set baseline values for cluster
            for (int i = cur_clust[0]; i <= cur_clust[1]; i++)
            {
                pt.fblb.at(i) = cur_exp_res[0];
                pt.tblb.at(i) = cur_exp_res[1];
            }
            
            // set peak bounds for first and last peak in cluster
            pt.fpkb.at(cur_clust[0]) = cur_exp_res[0];
            pt.tpkb.at(cur_clust[1]) = cur_exp_res[1];
        }
        
        if (output) Rcout << "\n";
        
        // adjust baseline bounds for current cluster
        adjust_cluster_baseline(cur_clust);
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
    for (int i = 0; i < pt.npeaks; i++)
    {
        switch(pt.atyp[i])
        {
            case 1:
                // normal apex
                // adjusted apex should be d0 maxima between inflection points
                pt.adj_apex[i] = which_max(d0, (int) floor(pt.finf[i]),
                                               (int) floor(pt.tinf[i])+1);
                
                break;
                
            case 2:
                // front shoulder peak
                // adjusted apex should be the d2 minima (same as apex)
                pt.adj_apex[i] = pt.apex[i];
                
                break;
                
            case 3:
                // tail shoulder peak
                // adjusted apex should be the d2 minima (same as apex)
                pt.adj_apex[i] = pt.apex[i];
                
                break;
                
            case 4:
                // lhs rounded peak
                // adjusted apex should be the d2 minima (same as apex)
                pt.adj_apex[i] = pt.apex[i];
                
                break;
                
            case 5:
                // rhs rounded peak
                // adjusted apex should be the d2 minima (same as apex)
                pt.adj_apex[i] = pt.apex[i];
                
                break;
                
            default:
                // to prevent a crash
                // adjusted apex should be d0 maxima between inflection points
                pt.adj_apex[i] = which_max(d0, (int) floor(pt.finf[i]),
                                               (int) floor(pt.tinf[i])+1);
            
                break;
        
        }
    }
}

void Chromatogram::fit_emg(int only_vip = 1)
{
    
}

void Chromatogram::process_chromatogram(int min_inf_pts = 2)
{
    if (output) Rcout << "Detecting peaks...\n";
    detect_peaks();
    if (output) Rcout << "Found " << pt.npeaks << " peaks.\n\n";
    
    if (output) Rcout << "Expanding peaks...\n";
    // expand all peaks
    expand_all_peaks();
    
    if ((p > 0 && get_vip() > 0) || p < 0)
    {
        if (output) pt.summary();
        
        if (output) Rcout << "Detecting clusters...\n";
        // detect clusters
        detect_clusters();
        
        // if (output) pt.summary();
        
        if (output) Rcout << "Found " << pt.clust.size() << " clusters.\n\n";
        
        if (output) Rcout << "Expanding all clusters...\n";
        // expand all clusters
        expand_all_clusters();
        
        if (output) pt.summary();
        
        if (output) Rcout << "Adjusting apices...\n";
        // determine adjusted apices
        adjust_apices();
    }
    
    // if (output) pt.summary();
    
    if (output) Rcout << "Removing tagged peaks...\n";
    
    // remove marked peaks before returning
    // TODO: This might need to be adjusted. The removal of peaks might have implications
    // for the output if the peak is a member of a cluster. Although the type of peak
    // bound doesnt change either way so maybe it doesnt matter...
    pt.remove_tagged_peaks();

    if (output) pt.summary();
}


/************************
 * R interface function *
 ************************/

//' @encoding UTF-8
//' @title Rcpp wrapper that calls the C++ chromatogram processing framework
//' 
//' @description
//' 
//' The function takes smoothed d0, d1, and d2 vectors and processes the chromatogram by baseline expansion of peaks detected in the second derivative.
//' 
//' Do not use this function, this function is called via the R interface functions.
//'
//' @param d0 Smoothed XIC trace
//' @param d1 First derivative of smoothed XIC. Currently not used.
//' @param d2 Second derivative of smoothed XIC
//' @param apex_thresh Second derivative threshold value.
//' @param w Apex detection window. Do not change unless you know what you are doing.
//' @param p Apex location of a specific peak. Leave blank if you wish to process the entire chromatogram.
//' @param liftoff Slope difference threshold (in percent) for the peak front.
//' @param touchdown Slope difference threshold (in percent) for the peak tail.
//' @param output \code{integer} value (0 = no output, 1 = verbose output).
//' 
//' @return A list of peak characteristics.
//'
//' @export
//'
//' @examples
//' require("signal")
//' x <- seq(1, 200, 1)
//' vec <- 1e5*dnorm(x, 100, 5) # create a vector with a gaussian peak
//' noise <- rnorm(length(x), 0, 5) # generate a noise vector
//' nvec <- vec + noise # create a noisy `chromatogram`.
//' smvec <- signal::sgolayfilt(nvec, n = 5) # smooth the vector using Savitzky-Golay
//' ddsmvec <- signal::sgolayfilt(nvec, n = 5, m = 2) # get the second derivative of the smoothed vector
//' cpc::process_chromatogram(d0 = smvec, d1 = 1.0, d2 = ddsmvec, apex_thresh = 10)
// [[Rcpp::export]]
Rcpp::List process_chromatogram(vec_d &d0, vec_d &d1, vec_d &d2,
                          double apex_thresh = 0.0, int w = 5, int p = -1,
                          double liftoff = 0.0, double touchdown = 0.5, int output = 0)
{
    // initialize Chromatogram class
    Chromatogram chrom(d0, d1, d2, apex_thresh, w, p, liftoff, touchdown, output);
    
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
                              _["tail_code"] = chrom.get_ttyp());
}





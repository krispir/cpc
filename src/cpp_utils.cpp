// [[Rcpp::plugins(cpp11)]]

#include <RcppArmadillo.h>
#include <Rcpp.h>
#include <vector>
#include <algorithm>
#include <math.h>
#include <cmath> // for erfc, fabs

using namespace Rcpp;
using namespace std;

using vec_d = std::vector<double>;
using vec_i = std::vector<int>;


// double emg(int x, double u, double s, double l)
// {
//     return (l/2)*exp((l/2)*((2*u)+(l*s*s)-(2*x)))*erfc((u+(l*s*s)-x)/(pow(2,0.5)*s));
// }


// Davids R-code
// demg <- function(x, mu = 0, sigma = 1, lambda = 1) {
//     exp(
//         log(lambda)+
//             lambda*(mu+lambda*sigma^2/2-x)+
//             pnorm((mu+lambda*sigma^2-x)/sigma, lower = FALSE, log.p = T)
//     )
// }
// double emg_2(double x, double u, double s, double l)
// {
//     return exp(log(l)+l*(u+((l*s*s)/2)-x)+R::pnorm((u+l*s*s-x)/s, 0.0, 1.0, false, true));
// }
// 
// vec_d c_demg(int xf, int xl, double u, double s, double l)
// {
//     // data checks
//     
//     if (xf > xl) // if xf > xl -> swap
//     {
//         double xtmp = xf;
//         xf = xl;
//         xl = xtmp;
//     }
//     
//     // check for large lambda
//     
//     // initialize output vector
//     vec_d out(xl-xf+1);
//     
//     // fill the output vector
//     for (int i = 0; i < xl-xf+1; i++)
//     {
//         // calculate emg val for x[i]
//         out.at(i) = emg((int) xf+i, u, s, l);
//     }
//     
//     return out;
// }
// 
// vec_d c_demg_2(vec_d x, double u, double s, double l)
// {
//     // data checks
//     int nx = x.size();
//     
//     // if (xf > xl) // if xf > xl -> swap
//     // {
//     //     double xtmp = xf;
//     //     xf = xl;
//     //     xl = xtmp;
//     // }
//     
//     // check for large lambda
//     
//     // initialize output vector
//     vec_d out(nx);
//     
//     // fill the output vector
//     for (int i = 0; i < nx; i++)
//     {
//         // calculate emg val for x[i]
//         out.at(i) = emg_2(x.at(i), u, s, l);
//     }
//     
//     return out;
// }

// Davids R-code
// cemgsmat <- function(pars.m, .x)
// {
//     apply(pars.m, 1, FUN = function(p, .x) {
//         p[4] * demg(x = .x, mu = p[1], sigma = p[2], lambda = p[3])
//     }, .x = .x)
// }
// 
// NumericMatrix c_cemgsmat(NumericMatrix parsm, vec_d x)
// {
//     int npar = parsm.nrow();
//     int nx = x.size();
//     
//     vec_d emg(nx);
//     NumericMatrix emgmat(x.size(), parsm.nrow());
//     
//     for (int i = 0; i < npar; i++)
//     {
//         emg = c_demg_2(x, parsm(i, 0), parsm(i, 1), parsm(i, 2));
// 
//         for (int j = 0; j < nx; j++)
//         {
//             emgmat(j, i) = parsm(i, 3) * emg[j];
//         }
//     }
//     
//     return emgmat;
// }

// Davids R-code
// minfunc <- function(pars, .x, .y, .w, .n)
// {
//     pars.m <- matrix(pars, nrow=.n)
//     pars.m[,2:4] <- exp(pars.m[,2:4])
//     pred <- cemgsmat(pars.m, .x)
//     sum(.w*(.y-rowSums(pred))^2)
// }
// 
// double c_minfunc(vec_d pars, vec_d x, vec_d y, vec_d w, int n)
// {
//     if ((int) pars.size() % n != 0)
//     {
//         return -1.0;
//     }
//     
//     int nx = x.size();
//     int npars = pars.size()/n;
//     
//     /* create pars matrix */
//     NumericMatrix parsm(n, npars);
//     
//     for (int i = 0; i < n; i++)
//     {
//         for (int j = 0; j < npars; j++)
//         {
//             parsm(i, j) = pars.at((j*n)+i);
//         }
//     }
//     
//     /* calculate exponential of pars 1:3 */
//     for (int i = 1; i < npars; i++)
//     {
//         for (int j = 0; j < n; j++)
//         {
//             parsm(j, i) = exp(parsm(j, i));
//         }
//     }
//     
//     /* calculate emgmat */
//     NumericMatrix emgmat = c_cemgsmat(parsm, x);
//     // mat_d emgmat = c_cemgsmat(parsm, x);
//     
//     vec_d emg(nx);
//     
//     /* calculate sum of squares */ 
//     double SS = 0.0;
//     double rowsum = 0.0;
//     
//     for (int i = 0; i < nx; i++)
//     {
//         rowsum = 0.0;
// 
//         for (int j = 0; j < n; j++)
//         {
//             rowsum = rowsum + emgmat(i, j);
//         }
//         
//         Rcout << rowsum << " ";
// 
//         SS = SS + w.at(i) * pow(y.at(i) - rowsum, 2);
//     }
//     
//     return SS;
// }
// 
// double c_minfunc_2(vec_d pars, vec_d x, vec_d y, vec_d w, int n)
// {
//     if ((int) pars.size() % n != 0)
//     {
//         return -1.0;
//     }
//     
//     int nx = x.size();
//     
//     vec_d emg(nx);
//     
//     /* calculate sum of squares */ 
//     double SS = 0.0;
//     double rowsum = 0.0;
//     
//     for (int i = 0; i < nx; i++)
//     {
//         rowsum = 0.0;
//         
//         for (int j = 0; j < n; j++)
//         {
//             rowsum += exp(pars.at(j+(3*n))) * emg_2(x[i], pars.at(j), exp(pars.at(j+n)), exp(pars.at(j+(2*n))));
//         }
//         
//         SS = SS + w.at(i) * pow(y.at(i) - rowsum, 2);
//     }
//     
//     return SS;
// }

// Gradient function R-code
// mingrad <- function(pars, x, y, w, n, h = 10e-6) {
//     
//     len <- length(pars)
//     gradout <- numeric(len)
//     
//     for (i in 1:len) {
//         
//         plus_h <- replace(pars,i,pars[i] + h) # replace element and return vector
//         minus_h <- replace(pars,i,pars[i] - h)
//         
//         gradout[i] <- (c_minfunc(plus_h, x, y, w, n) - 
//             c_minfunc(minus_h, x, y, w, n))/(2 * h)
//         
//     }
//     
//     gradout
// }
// 
// vec_d c_mingrad(vec_d pars, vec_d x, vec_d y, vec_d w, int n, double h = 10e-6)
// {
//     unsigned int nvals = pars.size();
//     
//     vec_d gradout(nvals);
//     vec_d pars_plus_h(nvals);
//     vec_d pars_minus_h(nvals);
//     
//     for (unsigned int i = 0; i < nvals; i++)
//     {
//         pars_plus_h = pars;
//         pars_minus_h = pars;
//         
//         pars_plus_h[i] = pars[i] + h;
//         pars_minus_h[i] = pars[i] - h;
//         
//         gradout[i] = (c_minfunc_2(pars_plus_h, x, y, w, n) - c_minfunc_2(pars_minus_h, x, y, w, n))/(2*h);
//     }
//     
//     return gradout;
// }

// [[Rcpp::export]]
vec_i fast_match(vec_i &v1, vec_i &v2)
{
    vec_i out((int) v1.size(), -1);
    
    for (int i = 0; i < (int) v1.size(); i++)
    {
        for (int j = 0; j < (int) v2.size(); j++)
        {
            if (v1[i] == v2[j])
            {
                out[i] = j;
                break;
            }
        }
    }
    
    return out;
}

// [[Rcpp::export]]
vec_i match_to_range(vec_d &v, vec_d &vmin, vec_d &vmax)
{
    vec_i out((int) v.size(), 0);
    
    for (int i = 0; i < (int) v.size(); i++)
    {
        for (int j = 0; j < (int) vmin.size(); j++)
        {
            if (vmin[j] <= v[i] && vmax[j] >= v[i])
            {
                out[i] = 1;
            }
        }
    }
    
    return out;
}

// [[Rcpp::export]]
int skim_to_val(vec_d& v, double val, 
                int start_at, int break_at, 
                int direction = 1)
{
    int nv = v.size();
    int i = start_at;
    
    int adj;
    
    if (v[start_at] >= val)
    {
        adj = 1;
    } else
    {
        adj = -1;
    }
    
    if (direction > 0) // forward
    {
        if (break_at > nv-1) break_at = nv-1;
        
        if (break_at <= start_at) return i;
    } else // backwards
    {
        if (break_at < 0) break_at = 0;
        
        if (break_at >= start_at) return i;
    }
    
    while (i > 0 && i < nv && 
           i != break_at && 
           adj * (v[i] - val) > 0) i += direction;
    
    return i;
}

// [[Rcpp::export]]
double c_integrate_vector(vec_d& x, vec_d& y, int a, int b)
{
    int ny = y.size();
    int nx = x.size();
    
    if (nx != ny)
    {
        x.resize(ny);
        
        for (int i = 0; i < ny; i++) x[i] = i;
    }
    
    // Check that the bounds a and b are within scope
    if (a < 0 || b > ny-1)
        return -1.0;
    
    // Check that a is lower than b
    if (a >= b)
        return -1.0;
    
    double sum = 0.0;
    
    // for (int i = a+1; i <= b; i++)
    // {
    //   sum += ((x[i]-x[i-1])*max(y[i], y[i-1]))-((x[i]-x[i-1])*abs(y[i]-y[i-1])*0.5);
    // }
    
    for (int i = a; i < b; i++)
    {
        sum += ((y[i] + y[i+1]) / 2) * (x[i+1] - x[i]);
    }
    
    return sum;
}

// TODO: This is slow because each window is iterated repeatedly so it becomes something
//       like O(bN) when in fact I can just subtract the first element in the last window
//       and add the last element in the new window to the sum. That would make it almost O(N).
// [[Rcpp::export]]
vec_d c_run_mean(vec_d& x, int b)
{
    int nx = x.size();
    
    int i;
    double sum;
    
    vec_d out(nx, -1.0);
    
    if (nx < b) return out;
    
    // sum first window
    sum = 0.0;
    
    for (i = 0; i < (2*b)+1; i++)
        sum += x[i];
    
    // loop over entire x
    for (i = 0; i < nx; i++)
    {
        if (i > b && i < nx-b)
        {
            sum -= x[i-b-1];
            sum += x[i+b];
        }
        
        out[i] = sum/((2*b)+1);
    }
    
    return out;
}


//----------------------------
// Linear regression function
// Returns a vector of length 5
// [0] slope
// [1] intercept
// [2] correlation coefficient
// [3] slope sd
// [4] intercept sd
//----------------------------

// [[Rcpp::export]]
vec_d c_lm_fit(vec_d &x, vec_d &y, int a = -1, int b = -1)
{
    vec_d out(5, 0.0);
    
    if (x.size() != y.size()) throw("SIZE_MISMATCH");
    
    if (a > b)
    {
        int c = a;
        a = b;
        b = c;
    }
    if (a > 0 && b > 0 && a == b) return out;
    if (a < 0) a = 0;
    if (b > (int) x.size()-1 || b < 0) b = (int) x.size()-1;
    
    int n = b-a+1;
    
    double sumx = 0.0;
    double sumy = 0.0;
    double sumxx = 0.0;
    
    double sumdxdy = 0.0;
    double SSdx = 0.0;
    double SSdy = 0.0;
    
    double SSyyh = 0.0; // y residual sum of squares
    double Syx = 0.0; // random error in y direction
    
    for (int i = a; i <= b; i++)
    {
        sumx += x[i];
        sumy += y[i];
        sumxx += x[i]*x[i];
    }
    
    double meanx = sumx / n;
    double meany = sumy / n;
    
    for (int i = a; i <= b; i++)
    {
        SSdx += pow((x[i] - meanx), 2);
        SSdy += pow((y[i] - meany), 2);
        sumdxdy += (x[i] - meanx) * (y[i] - meany);
    }
    
    out[0] = sumdxdy / SSdx; // slope
    out[1] = meany - out[0] * meanx; // intercept
    out[2] = sumdxdy / sqrt(SSdx * SSdy); // correlation coefficient
    
    // y residual sum of squares
    for (int i = a; i <= b; i++) SSyyh += pow(y[i] - (out[1] + out[0] * x[i]), 2);
    
    Syx = sqrt(SSyyh / (n - 1));
    
    out[3] = Syx / sqrt(SSdx); // slope sd
    out[4] = Syx * sqrt(sumxx / (n * SSdx));; // intercept sd
    
    return out;
}


// [[Rcpp::export]]
double c_lm_slope(vec_d &x, vec_d &y, int a = -1, int b = -1)
{
    double out = 0.0;
    
    if (x.size() != y.size()) throw("SIZE_MISMATCH");
    
    if (a > b)
    {
        int c = a;
        a = b;
        b = c;
    }
    if (a > 0 && b > 0 && a == b) return out;
    if (a < 0) a = 0;
    if (b > (int) x.size()-1 || b < 0) b = (int) x.size()-1;
    
    int n = b-a+1;
    
    double sumx = 0.0;
    double sumy = 0.0;
    
    double sumdxdy = 0.0;
    double SSdx = 0.0;
    
    for (int i = a; i <= b; i++)
    {
        sumx += x[i];
        sumy += y[i];
    }
    
    double meanx = sumx / n;
    double meany = sumy / n;
    
    for (int i = a; i <= b; i++)
    {
        SSdx += pow((x[i] - meanx), 2);
        sumdxdy += (x[i] - meanx) * (y[i] - meany);
    }
    
    out = sumdxdy / SSdx; // slope
    
    return out;
}

// [[Rcpp::export]]
vec_d vector_residual(NumericVector& x, NumericVector& y)
{
    int nx = x.size();
    int ny = y.size();
    
    if (nx != ny) throw("SIZE_MISMATCH");
    
    vec_d out(nx);
    
    for (int i = 0; i < nx; i++)
        out[i] = y[i] - x[i];
    
    return out;
}

// [[Rcpp::export]]
vec_i c_local_extremes2(vec_d& v, int w = 2)
{
    int nv = v.size();
    
    if (2*w+1 > nv) throw("SIZE_MISMATCH");
    
    vec_i local_extremes(nv, 0);
    
    int last_min = -1;
    int last_max = -1;
    
    bool local_min = false;
    bool local_max = false;
    
    // loop over v
    for (int i = w; i < nv-w; i++)
    {
        // loop over current window and determine max
        local_min = true;
        local_max = true;
        
        for (int j = i-w; j < i+w; j++)
        {
            if (v[j] > v[i]) local_max = false;
            if (v[j] < v[i]) local_min = false;
        }
        
        if (local_max) local_extremes[i] = 1;
        if (local_min) local_extremes[i] = -1;
    }
    
    for (int i = 0; i < nv; i++)
    {
        if (local_extremes[i] == 1)
        {
            if (last_max > 0 && i - last_max < w)
            {
                if (v[i] > v[last_max])
                {
                    local_extremes[last_max] = 0;
                    last_max = i;
                } else
                {
                    local_extremes[i] = 0;
                    last_max = i;
                }
            } else
            {
                last_max = i;
            }
        } else if (local_extremes[i] == -1)
        {
            if (last_min > 0 && i - last_min < w)
            {
                if (v[i] < v[last_min])
                {
                    local_extremes[last_min] = 0;
                    last_min = i;
                } else
                {
                    local_extremes[i] = 0;
                    last_min = i;
                }
            } else
            {
                last_min = i;
            }
        }
    }
    
    return local_extremes;
}

// New extremes function based on (and improving) the localMax/localMin functions used in
// the MassSpecWavelet package
// [[Rcpp::export]]
vec_i c_local_extremes(vec_d& v, int w = 3)
{
    int nv = v.size();
    int nwin = (int) floor(nv/w) + 1;
    
    int cur_min = 0;
    int cur_max = 0;
    int last_max;
    int last_min;
    
    int first, last;
    
    int cj = 0;
    
    int shift = (int) floor(w/2);;
    
    vec_i local_extremes(nv, 0);
    
    vec_i sel_idx;
    
    for (int i = 0; i < nwin; i++)
    {
        cur_max = i*w;
        cur_min = i*w;
        
        for (int j = (i*w); j < ((i*w)+w); j++)
        {
            if (j > nv-1)
            {
                cj = nv-1;
            } else 
            {
                cj = j;
            }
            
            if (v[cj] > v[i*w] &&
                v[cj] > v[(i*w)+w-1] &&
                v[cj] > v[cur_max]) cur_max = cj;
            
            if (v[cj] < v[i*w] &&
                v[cj] < v[(i*w)+w-1] &&
                v[cj] < v[cur_min]) cur_min = cj;
        }
        
        if (cur_max > i*w) local_extremes[cur_max] = 1;
        if (cur_min > i*w) local_extremes[cur_min] = -1;
    }
    
    nwin = (int) floor((nv+shift)/w) + 1;
    
    for (int i = 0; i < nwin; i++)
    {
        if ((i*w)-shift < 0)
        {
            first = 0;
            cur_max = 0;
            cur_min = 0;
        } else
        {
            first = (i*w)-shift;
            cur_max = (i*w)-shift;
            cur_min = (i*w)-shift;
        }
        
        if  ((i*w)+w-shift > nv)
        {
            last = nv-1;
        } else
        {
            last = (i*w)+w-shift-1;
        }
        
        for (int j = (i*w)-shift; j < ((i*w)+w)-shift; j++)
        {
            if (j < 0)
            {
                cj = 0;
            } else if (j > nv-1)
            {
                cj = nv-1;
            } else 
            {
                cj = j;
            }
            
            if (v[cj] > v[first] &&
                v[cj] > v[last] &&
                v[cj] > v[cur_max]) cur_max = cj;
            
            if (v[cj] < v[first] &&
                v[cj] < v[last] &&
                v[cj] < v[cur_min]) cur_min = cj;
        }
        
        if (cur_max > first) local_extremes[cur_max] = 1;
        if (cur_min > first) local_extremes[cur_min] = -1;
    }
    
    // return local_extremes;
    
    last_max = -1;
    last_min = -1;
    
    for (int i = 0; i < nv; i++)
    {
        if (local_extremes[i] > 0)
        {
            if (last_max > 0 &&
                i - last_max < w)
            {
                if (v[i] > v[last_max])
                {
                    local_extremes[last_max] = 0;
                    last_max = i;
                } else
                {
                    local_extremes[i] = 0;
                    last_max = i;
                }
            } else
            {
                last_max = i;
            }
        } else if (local_extremes[i] < 0)
        {
            if (last_min > 0 &&
                i - last_min < w)
            {
                if (v[i] < v[last_min])
                {
                    local_extremes[last_min] = 0;
                    last_min = i;
                } else
                {
                    local_extremes[i] = 0;
                    last_min = i;
                }
            } else
            {
                last_min = i;
            }
        }
    }
    
    return local_extremes;
}

// [[Rcpp::export]]
double c_peak_to_peak_noise(vec_i &x, vec_d &y, int w)
{
    int nx = x.size();
    
    if (nx > (int) y.size()) throw("SIZE_MISMATCH");
    
    int nvals = 0;
    
    double Sdx = 0.0;
    
    int last_min = 0;
    int last_max = 0;
    
    vec_i local_extremes = c_local_extremes2(y, w);
    
    /*******************************************
     * mean
     *******************************************/
    for (int i = 0; i < nx; i++)
    {
        if (local_extremes[x[i]] < 0) // minima
        {
            Sdx += abs(y[x[i]] - y[last_max]);
            nvals++;
            
            last_min = x[i];
            
        } else if (local_extremes[x[i]] > 0)
        {
            Sdx += abs(y[x[i]] - y[last_min]);
            nvals++;
            
            last_max = x[i];
            
        }
    }
    
    if (nvals < 1)
    {
        return 0.0;
    } else
    {
        return Sdx/nvals;
    }
}


// [[Rcpp::export]]
vec_d c_running_slope(vec_d &x, vec_d &y, int w)
{
    // data checks
    if (x.size() != y.size()) throw("SIZE_MISMATCH");
    
    if (2*w > (int) x.size()) throw("SIZE_MISMATCH");
    
    // proc vars
    int n = (int) x.size(); // vector size
    
    int start, end; // window boundaries
    
    // output vector
    vec_d out(n, 0.0);
    
    // loop over entire vector
    for (int i = 0; i < n; i++)
    {
        // set start and end boundaries for the current window
        start = max(0, i-w);
        end = min(n-1, i+w);
        
        // loop over window and calculate local model
        for (int j = start; j <= end; j++)
        {
            // calculate linear model for current window
            // vec_d c_lm_fit(vec_d &x, vec_d &y, int a = 0, int b = (int) x.size()-1)
            out[i] = c_lm_slope(x, y, start, end);
        }
    }
    
    return out;
}



// [[Rcpp::export]]
vec_i c_local_max(vec_d& v, int w = 2)
{
    int nv = v.size();
    
    if ((2*w)+1 > nv) throw("SIZE_MISMATCH");
    
    vec_i out(nv, 0);
    
    int cur_max = 0;
    
    // loop over v
    for (int i = 0; i < nv-(2*w); i++)
    {
        cur_max = i;
        
        // loop over current window and determine max
        for (int j = i+1; j < i + (2*w); j++)
        {
            out[j] = 0;
            
            if (v[j] > v[cur_max])
            {
                cur_max = j;
            }
        }
        
        if (v[cur_max] > v[i] && v[cur_max] > v[i + (2*w)])
        {
            out[cur_max] = 1;
        }
    }
    
    return out;
}

// [[Rcpp::export]]
vec_i c_local_min(vec_d& v, int w = 2)
{
    int nv = v.size();
    
    if ((2*w)+1 > nv) throw("SIZE_MISMATCH");
    
    vec_i out(nv, 0);
    
    int cur_min = 0;
    
    // loop over v
    for (int i = 0; i < nv-(2*w); i++)
    {
        cur_min = i;
        
        // loop over current window and determine max
        for (int j = i+1; j < i + (2*w); j++)
        {
            out[j] = 0;
            
            if (v[j] < v[cur_min])
            {
                cur_min = j;
            }
        }
        
        if (v[cur_min] < v[i] && v[cur_min] < v[i + (2*w)])
        {
            out[cur_min] = 1;
        }
    }
    
    return out;
}

// [[Rcpp::export]]
vec_i c_get_inflection_points(vec_d &x, int b = 0)
{
    int nx = x.size();
    
    vec_i p;
    
    for (int i = 0; i < nx-1; i++)
        if ((x[i] >= b && x[i+1] <= b) || (x[i] <= b && x[i+1] >= b)) p.push_back(i);
        
        return p;
}

// [[Rcpp::export]]
vec_i c_get_directional_inflection_points(std::vector<double> &x, int b = 0)
{
    int nx = x.size();
    
    vec_i p(nx, 0);
    
    for (int i = 0; i < nx-1; i++)
    {
        if ((x[i] >= b && x[i+1] < b))
        {
            p[i] = -1;
        } else if ((x[i] <= b && x[i+1] > b))
        {
            p[i] = 1;
        }
    }
    
    return p;
}
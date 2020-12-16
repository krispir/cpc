
#' @title Wrapper function for characterization of the peaks detected by XCMS
#'
#' @description
#' 
#' This function is called by \code{filter_xcms_peaklist} but can be called 
#' independently to only characterize the peaks in the \code{XCMSnExp}.
#' 
#' TODO: Add a description about the algorithms used.
#'
#' @param xd \code{XCMSnExp} object with peak information
#' @param ppm \code{ppm} value for generating XICs
#' @param min_pts Minimum points across the peak
#' @param min_inf_width Minimum points between inflection points (~60\% peak height)
#' @param min_sn Minimum signal-to-noise ratio.
#' @param min_frac 
#' @param min_intensity Minimum value for second derivative apex points
#' @param smooth_method Method used for smoothing (Default: "savgol").
#' @param smooth_times Number of smoothing iterations (Default: 2).
#' @param smooth_win Width of the smoothing function window. Keep as \code{NULL} if you want the algorithm to determine it automatically.
#' @param fit_emg \code{boolean} indicating if EMG deconvolution should be performed.
#' @param sel_peaks \code{integer} vector of peak indices in the XCMS peak table. Leave as \code{NULL} if you want to process all peaks.
#' @param sel_files \code{integer} vector of file indices in the \code{XCMSnExp} object. Leave as \code{NULL} if you want to process all files.
#' @param verbose_output \code{boolean} indicating if you want verbose output.
#' @param plot \code{boolean} indicating if you want each peak to be plotted after characterization.
#'
#' @return \code{cpc} object
#' 
#' @seealso \link{filter_xcms_peaklist}, \link{cpc-class}
#' 
#' @export
#'
#' @examples
characterize_xcms_peaklist <- function(xd, ppm = 50, 
                                       min_pts = 7, # not used here 
                                       min_inf_width = 3, # not used here
                                       min_sn = 10, # not used here
                                       min_frac = NULL, # not used here (or at all)
                                       min_intensity = NULL,
                                       smooth_method = c("savgol", "mean"),
                                       smooth_times = 2,
                                       smooth_win = NULL,
                                       fit_emg = TRUE,
                                       sel_peaks = NULL,
                                       sel_files = NULL,
                                       verbose_output = FALSE,
                                       plot = FALSE)
{
    # requirements
    if(!require(mzR)) stop("Package: mzR required...")
    if(!require(signal)) stop("Package: signal required...")
    if(!require(foreach)) stop("Package: foreach required...")
    
    # check params
    
    ## check if any params that need to be set are implicitly set to NULL
    if (is.null(min_pts)) min_pts <- match.arg(min_pts)
    if (is.null(min_inf_width)) min_inf_width <- match.arg(min_inf_width)
    if (is.null(min_sn)) min_sn <- match.arg(min_sn)
    if (is.null(smooth_times)) smooth_times <- match.arg(smooth_times)
    smooth_method <- match.arg(smooth_method)
    
    if(!is.logical(verbose_output))
    {
        message(paste("Argument 'verbose_output' has to be TRUE/FALSE.",
                      "Using verbose_output = FALSE."))
        verbose_output = FALSE
    }
    
    ## check values of params
    if (!is.numeric(min_pts)) stop("'min_pts' has to be an integer value.")
    if (!is.numeric(min_inf_width)) stop("'min_inf_width' has to be an integer value.")
    if (!is.numeric(min_sn)) stop("'min_sn' has to be an integer value.")
    if (!is.null(min_intensity) && !is.numeric(min_intensity))
        stop("'min_intensity' has to be a numeric value.")
    if (!is.numeric(smooth_times)) stop("'smooth_times' has to be an integer value.")
    if (!is.null(smooth_win) && !is.numeric(smooth_win))
        stop("'smooth_win' has to be an integer value.")
    
    ## check that xd object is XCMSnExp
    if (class(xd) != "XCMSnExp") stop("'xd' must be an XCMS object of type 'XCMSnExp'.")
    
    # create cpc object
    cpc <- new("cpc", xd = xd)
    
    # set params in object
    setProcParams(cpc) <- list(ppm = ppm, 
                               min_pts = min_pts,
                               min_inf_width = min_inf_width,
                               min_sn = min_sn,
                               min_frac = min_frac,
                               min_intensity = min_intensity,
                               smooth_method = smooth_method,
                               smooth_times = smooth_times,
                               smooth_win = smooth_win,
                               fit_emg = fit_emg, 
                               sel_peaks = sel_peaks,
                               sel_files = sel_files,
                               verbose_output = verbose_output,
                               plot = plot)
    
    # parse peaklist from XCMS object
    cpc <- parsePeaklist(cpc)
    
    # process peaklist
    cpc <- processPeaks(cpc)
    
    # return object
    return(cpc)
}

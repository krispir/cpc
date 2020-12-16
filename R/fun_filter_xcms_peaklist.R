# Top level wrapper for filtering peaks

#' @title Wrapper function for filter the peaks detected by XCMS
#' 
#' @description 
#' 
#' Use this function to filter the peaks detected by XCMS based on width, size, 
#' and signal-to-noise. The supplied \code{XCMSnExp} object should have peak
#' information contained in it. Notice that any feature definitions and retention
#' time alignment information is removed in the process so these processing
#' steps will have to be performed again after running this function.
#'
#' @param xd \code{XCMSnExp} object
#' @param verbose_output \code{boolean} indicating if verbose output is to be given.
#' @param return_type Default: xcms.
#' @param plot \code{boolean} indicating if you want each peak to be plotted after characterization.
#' @param ... Other parameters passed to \code{characterize_peaklist}.
#'
#' @return Either an \code{XCMSnExp} object or a \code{cpc} object.
#' 
#' @seealso \link{characterize_xcms_peaklist}
#' 
#' @export
#'
#' @examples
filter_xcms_peaklist <- function(xd, verbose_output = FALSE,
                                 return_type = c("xcms", "cpc"),
                                 plot = FALSE, ...)
{
    # requirements
    if(!require(mzR)) stop("Package: mzR required...")
    if(!require(signal)) stop("Package: signal required...")
    if(!require(foreach)) stop("Package: foreach required...")
    
    # check params
    return_type <- match.arg(return_type)
    
    if(!is.logical(verbose_output))
    {
        message(paste("Argument 'verbose_output' has to be TRUE/FALSE.",
                      "Using verbose_output = FALSE."))
        verbose_output = FALSE
    }
    
    # data checks
    
    ## check that xd object is XCMSnExp
    if (class(xd) != "XCMSnExp") stop("'xd' must be an XCMS object of type 'XCMSnExp'.")
    
    # run peak processing on peaklist
    cpc <- characterize_xcms_peaklist(xd = xd, 
                                      verbose_output = verbose_output, 
                                      plot = plot, ...)
    
    # filter peaklist in XCMS object
    cpc <- filterPeaks(cpc)
    
    # check output param and return
    if (return_type == "xcms")
    {
        # return filtered XCMS object
        return(cpc@xdFilt)
    } else
    {
        # return full cpc object
        return(cpc)
    }
}

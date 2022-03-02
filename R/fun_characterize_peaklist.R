
#' @title Wrapper function for characterization of the peaks detected by XCMS
#'
#' @description
#' 
#' This wrapper function for the characterization of all peaks detected by
#' XCMS takes an *XCMSnExp* object and a *cpcProcParam* object as arguments
#' and will run over all peaks detected in all files or the subset defined in 
#' the *cpcProcParam* object. Before running this function, XCMS must be used
#' to detect chromatographic peaks in the data. Currently the available algorithm for peak
#' characterization is an adaption of the ApexTrack algorithm implemented in 
#' many instrumental software. In short, the algorithm proceeds by first 
#' detecting all peak apices present as negative minima craddled by inflection
#' points in the second derivative of the chromatogram. All detected peak apices
#' is then subjected to a baseline expansion algorithm in order to determine the
#' peak boundaries and baseline boundaries.
#' 
#' This function is called by \code{filter_xcms_peaklist} but can be called 
#' independently to only characterize the peaks in the \code{XCMSnExp}.
#'
#' @param xd \code{XCMSnExp} object with peak information
#' @param param \code{cpcProcParam} object with process parameters
#'
#' @return \code{cpc} object
#' 
#' @seealso \link{filter_xcms_peaklist}, \link{cpc-class}, \link{cpcProcParam}
#' 
#' @export
characterize_xcms_peaklist <- function(xd, param = cpcProcParam())
{
    # requirements
    # if(!require(mzR)) stop("Package: mzR required...")
    # if(!require(signal)) stop("Package: signal required...")
    # if(!require(foreach)) stop("Package: foreach required...")
    
    ## check that xd object is XCMSnExp
    if (class(xd) != "XCMSnExp") stop("'xd' must be an XCMS object of type 'XCMSnExp'.")
    
    # create cpc object
    cpc <- new("cpc", xd = xd, param = param)
    
    # parse peaklist from XCMS object
    cpc <- parseXCMSPeaklist(cpc)
    
    # process peaklist
    cpc <- processPeaks(cpc)
    
    # return object
    return(cpc)
}



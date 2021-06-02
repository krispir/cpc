
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
    if(!require(mzR)) stop("Package: mzR required...")
    if(!require(signal)) stop("Package: signal required...")
    if(!require(foreach)) stop("Package: foreach required...")
    
    ## check that xd object is XCMSnExp
    if (class(xd) != "XCMSnExp") stop("'xd' must be an XCMS object of type 'XCMSnExp'.")
    
    # create cpc object
    cpc <- new("cpc", xd = xd, param = param)
    
    # parse peaklist from XCMS object
    cpc <- parsePeaklist(cpc)
    
    # process peaklist
    cpc <- processPeaks(cpc)
    
    # return object
    return(cpc)
}



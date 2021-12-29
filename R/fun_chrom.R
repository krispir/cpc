
#' @title Create a cpc_chrom object from a chromatogram trace
#' 
#' @description 
#' 
#' This function creates a *cpc_chrom* object from a chromatogram trace for 
#' external processing of chromatograms without XCMS.
#'
#' @param st Scantimes for the chromatogram. If not supplied will default to 1:length(trace).
#' @param trace Numeric vector containing the chromatogram trace to be processed.
#' @param param *cpcChromParam* object contaning the processing parameters.
#'
#' @return
#' @export
#'
#' @examples
chromFromTrace <- function(st,
                           trace, 
                           param = cpcChromParam())
{
  # check if st is supplied
  if (missing(st)) 
  {
    # if st is not supplied, set it to 1:length(trace)
    st <- seq_len(length(trace))
   
    # if st is supplied, ensure that it is of the same length as trace 
  } else if (length(st) != length(trace))
  {
    stop("'st' must be a numeric vector of the same length as 'trace'.")
    
  }
  
  # set missing important parameters
  
  chrom <- new("cpc_chrom", 
               st = st, 
               xic = trace, 
               param = param)
  
  # set plotrange (for now set to entire chromatogram and let users change it
  # outside if they need to)
  setProcData(chrom) <- list(plotrange = c(1, length(trace)))
  
  return(chrom)
  
}
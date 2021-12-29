
#' @title Create a cpc_chrom object from a chromatogram trace
#' 
#' @description 
#' 
#' This function creates a *cpc_chrom* object from a chromatogram trace for 
#' external processing of chromatograms without XCMS.
#'
#' @param st Scantimes for the chromatogram. If not supplied will default to 1:length(xic).
#' @param xic Numeric vector containing the chromatogram trace to be processed.
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
  chrom <- new("cpc_chrom", 
               st = st, 
               xic = trace, 
               param = param)
  
  return(chrom)
  
}
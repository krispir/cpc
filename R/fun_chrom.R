
#' @title Constructor for the cpc_chrom object
#' 
#' @description 
#' 
#' This function creates a *cpc_chrom* object from a chromatogram trace.
#'
#' @param id Integer of length 1 indicating the peak id
#' @param st Scantimes for the chromatogram. If not supplied will default to 1:length(trace)
#' @param trace Numeric vector containing the chromatogram trace to be processed
#' @param param *cpcChromParam* object contaning the processing parameters
#'
#' @return A *cpc_chrom* object that can be used for further data processing
#' @export
#'
#' @examples
cpc_chrom <- function(id,
                           st,
                           trace, 
                           param = cpcChromParam())
{
  # check if id is supplied
  if (missing(id))
  {
    # if missing, set to 1
    id <- 1
  } else if (!is.integer(id))
  {
    stop("Invalid 'id'. 'id' should be a single integer value.")
    
  }
  
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
               id = id,
               st = st, 
               xic = trace, 
               param = param)
  
  return(chrom)
  
}


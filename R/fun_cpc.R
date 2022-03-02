cpcFromXCMS <- function(xd, param = cpcProcParam())
{
  # check XCMSnExp object
  if (missing(xd) || class(xd) != "XCMSnExp" || !xcms::hasChromPeaks(xd))
  {
    stop(paste0("Missing or invalid XCMSnExp object or does not have peak ",
                "information. An XCMSnExp object with peak information must ",
                "be supplied. Please run peak detection using XCMS and supply ",
                "the returned object."))
    
  }
  
  # create a ptable from peak information in the XCMSnExp object
  ptable <- list()
  
  
  # create a cpc object
  
  
}

ptable <- data.frame(mz = c(188.0714, 120.0814, 144.1025),
                     rt = c(539.1683, 532.4985, 512.0002),
                     dummy = c(1,2,3))

files <- list.files(system.file("extdata", package = "cpc"), full.names = T)

cpcFromPeaktable <- function(ptable, files, param = cpcProcParam())
{
  # convert ptable to list
  if (!all(class(ptable) %in% c("list", "data.frame")))
  {
    stop(paste0("'ptable' should be a data.frame or list with the elements ", 
                "'rt' and 'mz' defining the retention time and m/z of the ", 
                "peaks."))
    
  } else if (all(class(ptable) != "list"))
  {
    ptable <- as.list(ptable)
    
  }
  
  # check ptable
  if (!all(c("mz","rt") %in% names(ptable)))
  {
    stop(paste0("Invalid peak table supplied. 'ptable' should be a data.frame ",
                "with the columns 'mz' and 'rt' included."))
    
  }
  
  # check that all files exist
  if (!all(file.exists(files)))
  {
    stop(paste0("One or more raw files are missing. Ensure that all files in ", 
                "'files' exist and is readable."))
    
  }
  
  # ensure only the elements 'rt' and 'mz' are present in the peaktable
  # drop any other information
  ptable <- ptable[c("rt", "mz")]
  
  # create cpc object from peaktable
  cpc <- new("cpc",
             cpt = ptable,
             files = files)
  
  # return object
  return(cpc)
  
}
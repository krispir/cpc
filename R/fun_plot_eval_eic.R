#' @title Plotting of characterized peaks
#' 
#' @description 
#' 
#' Allows visualization of the characterization results. Plots the chromatogram and the second derivative with peak boundaries illustrated.
#' 
#' @param cpc A \code{cpc} object
#' @param peakIdx \code{numeric} vector of peak indices to be plotted
#' @param outPath (optional) Path to save plots to
#' @param prefix (optional) Filename prefix, if NULL the filename of the raw data is used
#' @param device Graphics device to be used for saving plots (default: "png")
#' @param plotEMG \code{logical} indicating if the EMG fitting results should be plotted (default: FALSE)
#' @param plotXCMS \code{logical} indicating if the XCMS data should be included in the plot (default: TRUE)
#' @param noPrompt \code{logical} indicating if the prompt when many peaks are being plotted will be suppressed (default: FALSE)
#' @param annotation (Optional) A single string or a vector of strings for each plotted peak with additional annotations for the plot
#' @param ... Additional parameters to be sent to the graphics device
#'
#' @return \code{NULL}
#' @export
plotPeaks <- function(cpc, peakIdx, 
                      outPath = NULL, prefix = NULL, device = "png", 
                      plotEMG = FALSE, plotXCMS = TRUE, noPrompt = FALSE, 
                      annotation = character(1), ...)
{
    # check that there are selected peaks, otherwise plot all with query for 
    # the user (if noPrompt is FALSE)
    if (missing(peakIdx))
    {
        response <- 
            readline(prompt = 
                         paste0("No peaks were selected. Do you want to plot ",
                                "all peaks in the XCMS object (NUMBER) or all ", 
                                "processed peaks? (a = all peaks, p = all ",
                                "processed peaks) "))
        
        if (response %in% c("a", "all"))
        {
            # set peakIdx to contain all peaks in the XCMS object
            peakIdx <- 1:nrow(cpc@pt)
            
        } else if (response %in% c("p", "processed"))
        {
            # set peakIdx to contain all processed peaks in the XCMS object
            peakIdx <- match(row.names(cpc@cpt), row.names(cpc@pt))
            
        } else
        {
            stop("No peaks were selected...")
        }
        
        if (!noPrompt)
        {
            response <- ""
            
            while(!(response %in% c("y", "n", "yes", "no")))
            {
                response <- 
                    readline(prompt = 
                                 paste0("A total of ", length(peakIdx), 
                                        " peak(s) will be plotted. Do ",
                                        "you want to continue? (y/n) \n"))
            }
            
            if (response %in% c("n", "no"))
            {
                stop("No peaks were selected...")
            }
            
        }
    } else if (!is.numeric(peakIdx))
    {
        # check if peakIdx is a character vector, in which case the supplied
        # ids may be peak ids
        if (is.character(peakIdx))
        {
            # check that all the ids exist in the full peak list
            if (all(peakIdx %in% row.names(xcms::chromPeaks(cpc@xd))))
            {
                # replace peakIdx with the numeric indices for now
                peakIdx <- match(peakIdx, row.names(xcms::chromPeaks(cpc@xd)))
            } else
            {
                stop(paste0("Invalid peaks selected. peakIdx should contain a ",
                            "numeric vector of peak indices (matching the ",
                            "XCMS peak table) or a character vector of peak ",
                            "IDs matching the rownames of the peak list."))
            }
        }
    }
    
    # quick check to make sure there are now selected peaks
    if (is.null(peakIdx) || is.na(peakIdx) || length(peakIdx) < 1)
    {
        stop("No peaks were selected...")
    }
    
    # check if supplied annotation vector length match the number of peaks
    # or is 1
    if (length(annotation) > 1 && length(annotation) != length(peakIdx))
    {
        warning(paste0("annotation should be a single string or a vector of ",
                       "strings with length equal to the length of peakIdx. ",
                       "using annotation[1]...\n"))
        annotation <- annotation[1]
    }
    
    # check that all peakIdx selected are within range
    if (any(peakIdx < 1 || peakIdx > nrow(cpc@pt)))
    {
        stop("One or more peaks selected are out of bounds.")
    }
    
    # get the unique names of the selected peaks
    peakNames <- row.names(cpc@pt)[peakIdx]
    
    # determine which files the peaks are from
    selectedFiles <- sort(unique(cpc@pt[peakNames, "sample"]))
    
    # loop over files
    # (curFile <- selectedFiles[1])
    for (curFile in selectedFiles)
    {
        # determine which of the selected peaks are from curFile
        peaksFromCurFile <- peakIdx[which(cpc@pt[peakNames, "sample"] == curFile)]
        
        # output
        # TODO: Do I really need to open the raw file here? I dont think so...
        cat(paste("Opening file:", cpc@procData$file_paths[curFile], "\n"))
        
        # fetch raw data
        raw <- new("cpc_raw", file_path = MSnbase::fileNames(cpc@xd)[curFile])
        raw <- parseMz(raw)
        
        # loop over peaks
        # curPeak <- 1
        for (curPeak in 1:length(peaksFromCurFile))
        {
            curAnnotation <- ifelse(length(annotation) > 1,
                                    annotation[match(peaksFromCurFile[curPeak], 
                                                     peakIdx)],
                                    annotation[1])
            
            if (is.na(curAnnotation))
            {
                warning("Something went wrong with annotation...")
            }
            
            # open device (optional)
            if (!is.null(outPath) || !is.null(prefix))
            {
                if (is.null(outPath))
                {
                    outPath <- getwd()
                } else
                {
                    # check path
                    if (!dir.exists(outPath))
                    {
                        dir.create(path = outPath, recursive = T)
                    }
                }
                
                outFileName <- 
                    paste0(outPath, "/", 
                           ifelse(is.null(prefix),
                                  basename(sub(".CDF", "", 
                                               MSnbase::fileNames(cpc@xd)[curFile])),
                                  prefix),
                           "_", peakNames[match(peaksFromCurFile[curPeak], peakIdx)], 
                           ".", device)
                
                do.call(device, list(filename = outFileName, ...))
                # do.call(device, list(filename = outFileName,
                #                      width = 8, height = 6,
                #                      units = "in", res = 330))
            }
            
            # create a chromatogram object
            # x <- cpc
            chrom <- getChromatogram(cpc, id = peaksFromCurFile[curPeak])
            
            # chrom@rawProcResults <- as.list(cpc@rawResults[[peaksFromCurFile[curPeak]]])
            
            # plot peak
            # x <- chrom
            plotPeak(chrom, plotEMG = plotEMG, plotXCMS = plotXCMS, 
                     annotation = curAnnotation)
            
            # close device(optional)
            if (!is.null(outPath) || !is.null(prefix))
            {
                grDevices::dev.off()
            }
        }
    }
}


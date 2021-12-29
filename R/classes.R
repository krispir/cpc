
#### type unions ####

setClassUnion("numericOrNULL", members=c("numeric", "NULL"))
setClassUnion("integerOrNULL", members=c("integer", "NULL"))

#### Class: cpcProcParam ####

#' @title Class cpcProcParam
#' 
#' @description 
#' 
#' The class *cpcProcParam* holds all necessary parameters used in the characterization 
#' and filtering of peaks contained in an XCMS object. Instances of this class 
#' should be created using the constructor *cpcProcParam*.
#'
#' @slot ppm Numeric holding the ppm value for calculating the m/z range when extracting the ion trace.
#' @slot min_pts Integer holding the minimum required scan points between the peak bounds. Used for filtering based on peak width.
#' @slot min_inf_width Integer holding the minimum required number of scan points between the inflection points. Used internally when detecting peak apices in the chromatogram.
#' @slot min_sn Numeric holding the minimum required signal-to-noise ratio. Used to filter peaks based on signal-to-noise ratio.
#' @slot min_frac Numeric holding the minimum fraction of samples a peak should be contained in. Currently not used.
#' @slot min_intensity Numeric holding the minimum required intensity of a peak. Used to filter peaks based on peak intensity.
#' @slot min_shoulder_pts Integer holding the minimum number of points between peak apices when detecting should peaks. Used internally when detecting peak apices in the chromatogram.
#' @slot min_rounded_pts Integer holding the minimum number of points between peak apices when detecting rounded peaks. Used internally when detecting peak apices in the chromatogram.
#' @slot interval_tf Numeric of length 2 defining the required tailing factor interval. Used when filtering peaks based on symmetry using the tailing factor.
#' @slot min_fwhm Numeric holding the minimum required full width at half maxima. Used in peak filtering based on full width half maxima.
#' @slot min_w Numeric defining the minimum smoothing window width.
#' @slot max_w Numeric defining the maximum smoothing window width.
#' @slot smooth_method String defining the smoothing method to be used. 'savgol' is default and uses a Savitzky-Golay smoother. 'mean' uses a moving window mean smoother.
#' @slot smooth_times Integer defining the number of times the smoother will be applied to the data.
#' @slot smooth_win Numeric defining the smoothing window width.
#' @slot max_sigma Numeric defining the maximum signal value to be used in determining the smoothing window width based on XCMS data. Used internally.
#' @slot fit_emg Logical defining if EMG deconvolution should be applied.
#' @slot sel_peaks Integer vector defining which peaks in the XCMS peak list that should be processed. Default: NULL means all peaks will be processed.
#' @slot sel_files Integer vector defining which files in the XCMS object that should be processed. Default: NULL means all files will be processed.
#' @slot verbose_output Logical defining if verbose output should be given during processing. Default: FALSE.
#' @slot save_all Logical defining if all processing data should be saved in the object. Default: FALSE. Warning: This may result in very large objects and should be used with care.
#' @slot plot Logical defining if the results should be plotted after each peak is processed. Default: FALSE. Warning: Will slow down processing very much if a large number of peaks are processed. Should only be used for a small number of peaks.
#'
#' @return
#' @export
setClass("cpcProcParam",
         representation(ppm = "numericOrNULL", 
                        min_pts = "numericOrNULL",
                        min_inf_width = "numericOrNULL",
                        min_sn = "numericOrNULL",
                        min_frac = "numericOrNULL",
                        min_intensity = "numericOrNULL",
                        min_shoulder_pts = "numericOrNULL",
                        min_rounded_pts = "numericOrNULL",
                        interval_tf = "numericOrNULL",
                        min_fwhm = "numericOrNULL",
                        min_w = "numericOrNULL",
                        max_w = "numericOrNULL",
                        smooth_method = "character",
                        smooth_times = "numericOrNULL",
                        smooth_win = "numericOrNULL",
                        max_sigma = "numericOrNULL",
                        fit_emg = "logical", 
                        sel_peaks = "numericOrNULL",
                        sel_files = "numericOrNULL",
                        verbose_output = "logical",
                        save_all = "logical",
                        plot = "logical"),
         prototype(ppm = 50.0, 
                   min_pts = 7L,
                   min_inf_width = 3.0,
                   min_sn = 10.0,
                   min_frac = 0.5,
                   min_intensity = NULL,
                   min_shoulder_pts = 3L,
                   min_rounded_pts = 3L,
                   interval_tf = NULL,
                   min_fwhm = NULL,
                   min_w = 5L,
                   max_w = 21L,
                   smooth_method = "savgol",
                   smooth_times = 2L,
                   smooth_win = NULL,
                   max_sigma = NULL,
                   fit_emg = FALSE, 
                   sel_peaks = NULL,
                   sel_files = NULL,
                   verbose_output = FALSE,
                   save_all = FALSE,
                   plot = FALSE))

#' @title Class cpcChromParam
#' 
#' @description 
#' 
#' The class *cpcChromParam* holds all necessary parameters to characterize the 
#' peaks in a chromatogram. Instances of this class should be created using the
#' constructor *cpcChromParam*. The class *cpcChromParam* extends the class 
#' *cpcProcParam*.
#'
#' @slot mz Numeric holding the m/z value of the selected peak.
#' @slot p Retention time of the selected peak. 
#' @slot s Peak standard deviation to be used for determining the smoothing windows size.
#' @slot mz_range Numeric vector of length 2 holding the m/z range for extracting the ion trace.
#' @slot nscan Integer holding the length in number of scans of the ion trace.
#'
#' @return
#' @export
setClass("cpcChromParam",
         representation(mz = "numericOrNULL",
                        p = "numericOrNULL",
                        s = "numericOrNULL",
                        mz_range = "numericOrNULL",
                        nscan = "numericOrNULL"),
         prototype(mz = NULL,
                   p = NULL,
                   s = NULL,
                   mz_range = NULL,
                   nscan = NULL),
         contains = "cpcProcParam")

setClassUnion("cpcParam",
              members = c("cpcProcParam", "cpcChromParam"))

#### Method: setParam<- ####

#' @title Setter method for the parameter slot
#' 
#' @description 
#' 
#' Takes a named \code{list} with parameter values as argument. Each existing
#' parameter given in the list will be updated with the value in the list. The
#' parameter names should be the same as the slot names in the *cpcProcParam* 
#' and *cpcChromParam* classes.
#' 
#' @param x A \code{cpc_chrom} object.
#' @param value A named \code{list} with parameter values.
#' 
#' @return A \code{cpc_chrom} object
#' 
#' @export
#' @docType methods
setMethod("setParam<-", signature("cpcParam"), function(x, value) 
{
    if (class(value) == "list")
    {
        namesInArgument <- names(value)
        valueType <- "list"
        
        # check that the list is named
        if (length(namesInArgument) < 1) 
            stop("Process params must be a named list.")
        
    } else if (extends(class(value), "cpcParam"))
    {
        namesInArgument <- slotNames(value)
        valueType <- "cpcParam"
    } else
    {
        stop(paste0("Process params must be a named list 
                    or a cpcParam object."))
    }
    
    # get the slot names from the object
    namesToMatch <- slotNames(x)
    
    # determine matching names and missing names
    matchedNames <- which(!is.na(match(namesInArgument, namesToMatch)))
    missingNames <- which(is.na(match(namesInArgument, namesToMatch)))
    
    # to avoid errors, output a message with the missing names
    if (length(missingNames) > 0)
    {
        message(paste0("Unknown slot(s): ", paste0(namesInArgument[missingNames], 
                                                   collapse=", ")))
    }
    
    # update the slots with matching names
    if (length(matchedNames) > 0)
    {
        for (i in 1:length(matchedNames))
        {
            if (valueType == "list")
            {
                slot(x, namesInArgument[i]) <- value[[i]]
            } else
            {
                slot(x, namesInArgument[i]) <- slot(value, namesInArgument[i])
            }
        }
    }
    
    x
})

#### Method: getParam ####

#' @title Getter method for the parameter slot
#' 
#' @param x A \code{cpc_chrom} object.
#' @param param A string defining which parameter should be returned.
#' 
#' @return A parameter value.
#' 
#' @export
#' @docType methods
setMethod("getParam", signature("cpcParam"), function(x, param)
{
    # check the param argument
    if (!is.character(param) | length(param) != 1)
    {
        message("Argument `param` should be a single length character specifying
                the name of the parameter...")
        
        return(NULL)
    }
    
    # check that the param is a slot in the object
    if (param[1] %in% slotNames(x))
    {
        return(slot(x, param[1]))
    } else
    {
        message(paste0("Missing slots in object: ", 
                       paste0(param, collapse = ", ")))
        
        return(NULL)
    }
})

#### Method: show ####
#' @title Show method for the cpcParam object
#' 
#' @description 
#' 
#' Outputs a summary of the cpcParam object.
#' 
#' @param object A \code{cpc} object
#' 
#' @export
setMethod("show", signature("cpcParam"), function(object)
{
    paste0("'", class(object), "' object:\n")
    sNames <- slotNames(object)
    for (i in 1:length(sNames))
    {
        paste0(sNames[i], ": ", slot(object, sNames[i]), "\n")
    }
})

#### Class: cpc_chrom class ####

#' @title Class used to process an XIC and calculate peak characteristics
#' 
#' @description
#' 
#' Class used to process the peaks found by XCMS. All smoothing and processing 
#' of a chromatogram is performed using this class.
#' 
#' @slot id \code{integer} value indicating which peak in the peak table is being processed.
#' @slot xic Raw XIC trace
#' @slot d0 Smoothed XIC trace
#' @slot d2 Smoothed second derivative of the XIC trace
#' @slot st Scantimes
#' @slot param cpcParam object holding the processing parameters
#' @slot mzMeta Metadata from the raw MS files
#' @slot procParams \code{list} containing processing parameters. (deprecated)
#' @slot procData \code{list} containing processing data.
#' @slot procResults \code{list} containing the processing results for peak \code{id}.
#' @slot rawProcResults \code{list} containing the raw processing results returned by \code{process_chromatogram}.
#' @slot results \code{list} containing calculated peak characteristics for peak \code{id}.
#' 
#' @name cpc_chrom-class
#' 
#' @rdname cpc_chrom-class
#' 
#' @export
setClass("cpc_chrom",
         representation(
             id = "integer",
             xic = "numeric",
             d0 = "numeric",
             d2 = "numeric",
             st = "numeric",
             param = "cpcParam",
             mzMeta = "list",
             procParams = "list",
             procData = "list",
             procResults = "list",
             rawProcResults = "list",
             results = "list"
         ),
         
         prototype(
             id = NA_integer_,
             xic = NA_real_,
             d0 = NA_real_,
             d2 = NA_real_,
             st = NA_real_,
             param = cpcChromParam(),
             mzMeta = list(),
             procParams = list(mz = -1,
                               p = -1,
                               s = -1,
                               mz_range = c(-1,-1),
                               minpts = 3,
                               scanrange = NULL),
             procData = list(mz = -1,
                             p = -1,
                             s = -1,
                             mz_range = c(-1,-1)),
             rawProcResults = list(),
             procResults = list(),
             results = list(rt = -1.0,
                            rtmin = -1.0,
                            rtmax = -1.0,
                            rtf1b = -1.0, 
                            rtt1b = -1.0, 
                            rtf5b = -1.0, 
                            rtt5b = -1.0, 
                            rtf10b = -1.0, 
                            rtt10b = -1.0, 
                            rtf50b = -1.0, 
                            rtt50b = -1.0, 
                            wb = -1.0,
                            fwhm = -1.0,
                            area = -1.0,
                            height = -1.0,
                            a = -1.0,
                            b = -1.0,
                            tf = -1.0,
                            sn = -1.0,
                            apex = -1L,
                            finf = -1.0,
                            tinf = -1.0,
                            fblb = -1L,
                            tblb = -1L,
                            fpkb = -1L,
                            tpkb = -1L,
                            fcode = -1L,
                            tcode = -1L,
                            blslp = -1.0,
                            emu = -1.0,
                            esigma = -1.0,
                            elambda = -1.0,
                            earea = -1.0,
                            econv = -1L,
                            fh1b = -1L,
                            th1b = -1L,
                            fh5b = -1L,
                            th5b = -1L,
                            fh10b = -1L,
                            th10b = -1L,
                            fh50b = -1L,
                            th50b = -1L,
                            file = -1L,
                            note = "",
                            exectime = -1.0)
         ))

#### Method: getProcParams ####

#' @title Getter method for the procParams slot
#' 
#' @param x A \code{cpc_chrom} object.
#' @param value A named \code{list} of parameters. If \code{NULL}, returns a named \code{list} of all process parameters.
#' 
#' @return A named \code{list} of process parameters.
#' 
#' @docType methods
setMethod("getProcParams", signature("cpc_chrom"), function(x, value = NULL)
{
    if (!is.null(value) & is.character(value))
    {
        unlist(x@procParams[value])
    } else
    {
        x@procParams
    }
})

#### Method: setProcParams<- ####

#' @title Setter method for the procParams slot
#' 
#' @description 
#' 
#' Takes a named \code{list} of parameters as argument. If a parameter name 
#' already exists, it is updated with the new value and if it does not already
#' exist, it is added.
#' 
#' @param x A \code{cpc_chrom} object.
#' @param value A named \code{list} of parameters.
#' 
#' @return A \code{cpc_chrom} object
#' 
#' @docType methods
setMethod("setProcParams<-", signature("cpc_chrom"), function(x, value)
{
    if (class(value) != "list") stop("Process params must be a named list.")
    
    parNames <- names(x@procParams)
    valNames <- names(value)
    
    if (length(valNames) < 1) stop("Process params must be a named list.")
    
    matchedNames <- which(!is.na(match(valNames, parNames)))
    newNames <- which(is.na(match(valNames, parNames)))
    
    if (length(matchedNames) > 0)
    {
        x@procParams[valNames[matchedNames]] <- value[matchedNames]
    }
    
    if (length(newNames) > 0)
    {
        x@procParams[valNames[newNames]] <- value[newNames]
    }
    
    x
})

#### Method: getProcData ####

#' @title Getter method for the procData slot
#' 
#' @param x A \code{cpc_chrom} object.
#' @param value A named \code{list} with data. If \code{NULL}, returns a named \code{list} with all process data.
#' 
#' @return A named \code{list} with process data.
#' 
#' @export
#' @docType methods
setMethod("getProcData", signature("cpc_chrom"), function(x, value = NULL)
{
    if (!is.null(value) && is.character(value))
    {
        if (length(value) > 1)
        {
            message("Can only return a single post or all process data. Returning the first value.")
            value <- value[1]
        }
        
        unlist(x@procData[value])
    } else
    {
        x@procData
    }
})

### Method: setProcData<- ####

#' @title Setter method for the procData slot
#' 
#' @description 
#' 
#' Takes a named \code{list} with data as argument. If a data name 
#' already exists, it is updated with the new value and if it does not already
#' exist, it is added.
#' 
#' @param x A \code{cpc_chrom} object.
#' @param value A named \code{list} with data.
#' 
#' @return A \code{cpc_chrom} object
#' 
#' @export
#' @docType methods
setMethod("setProcData<-", signature("cpc_chrom"), function(x, value)
{
    if (class(value) != "list") stop("Process data must be a named list.")
    
    datNames <- names(x@procData)
    valNames <- names(value)
    
    if (length(valNames) < 1) stop("Process data must be a named list.")
    
    matchedNames <- which(!is.na(match(valNames, datNames)))
    newNames <- which(is.na(match(valNames, datNames)))
    
    if (length(matchedNames) > 0)
    {
        x@procData[valNames[matchedNames]] <- value[matchedNames]
    }
    
    if (length(newNames) > 0)
    {
        x@procData[valNames[newNames]] <- value[newNames]
    }
    
    x
})


#### Method: getMzRange ####

#' @title Getter for mz range parameter
#' 
#' @param x A \code{cpc_chrom} object.
#' 
#' @return A \code{numeric} vector with mz range.
#' 
#' @docType methods
setMethod("getMzRange", signature("cpc_chrom"), 
          function(x) getParam(x@param, "mz_range"))


#### Method: getResults ####

#' @title Getter method for the results slot
#' 
#' @param x A \code{cpc_chrom} object.
#' 
#' @return A \code{data.frame} with the results from the peak processing.
#' 
#' @docType methods
setMethod("getResults", signature("cpc_chrom"), 
          function(x) x@results)


#### Method: setResults<- ####

#' @title Setter method for the results slot
#' 
#' @param x A \code{cpc_chrom} object.
#' 
#' @return A \code{cpc_chrom} object.
#' 
#' @docType methods
setMethod("setResults<-", signature("cpc_chrom"), function(x, value)
{
    if (class(value) != "list") stop("Result vals must be a named list.")
    if (any(unlist(lapply(value, length)) > 1))
    {
        stop("Results need to be a named list with all elements of length 1.")
    }
    
    resNames <- names(x@results)
    valNames <- names(value)
    
    if (length(valNames) < 1) stop("Result vals must be a named list.")
    
    matchedNames <- which(!is.na(match(valNames, resNames)))
    newNames <- which(is.na(match(valNames, resNames)))
    
    x@results[valNames[matchedNames]] <- value[matchedNames]
    
    if (length(matchedNames) > 0)
    {
        x@results[valNames[matchedNames]] <- value[matchedNames]
    }
    
    if (length(newNames) > 0)
    {
        x@results[valNames[newNames]] <- value[newNames]
    }
    
    x
})


#### Method: setXIC<- ####

#' @title Setter method for the XIC slot
#' 
#' @param x A \code{cpc_chrom} object.
#' @param value A \code{numeric} vector.
#' 
#' @return A \code{cpc_chrom} object
#' 
#' @docType methods
setMethod("setXIC<-", signature("cpc_chrom"), function(x, value)
{
    if (!is.numeric(value)) stop("XIC must be a numeric vector.")
    
    x@xic <- value
    x
})


#### Method: plotPeak ####

#' @title Plot method that plots a panel with results of the peak characterization of the VIP.
#' 
#' @description 
#' 
#' This method will plot the processing result as a panel for review.
#' 
#' @param x A \code{cpc_chrom} object.
#' @param plotEMG \code{logical} indicating if results from EMG deconvolution should be plotted (default: TRUE)
#' @param plotXCMS \code{logical} indicating if XCMS reported peak bounds should be plotted (default: TRUE)
#' @param annotation (Optional) A single string or a vector of strings for each plotted peak with additional annotations for the plot
#' 
#' @return NULL
#' 
#' @export
#' @docType methods
#' @rdname cpc_chrom-methods
setMethod("plotPeak", signature("cpc_chrom"), function(x, plotEMG = TRUE, plotXCMS = TRUE,
                                                       annotation = character(1))
{
    if (!is.na(x@results$apex) && 
        !is.null(x@results$apex) && 
        x@results$apex > 0)
    {
        results = TRUE
    } else
    {
        results = FALSE
    }
    
    if (results)
    {
        ylim = c(0, max(x@xic[x@results$fblb:x@results$tblb]))
    } else
    {
        ylim = c(0, max(x@xic[x@procData$plotrange[1]:x@procData$plotrange[2]]))
    }
    
    graphics::layout(mat = matrix(c(1,2), nrow = 2, ncol = 1, byrow = T))
    par_bk <- graphics::par()
    
    # d0 main plot 
    graphics::par(mar = c(1,4.1,1,1))
    base::plot(x = x@st,
         y = x@d0, type = "l", 
         xlim = x@st[x@procData$plotrange],
         ylim = ylim,
         ylab = "",
         xlab = "",
         xaxt = "n")
    graphics::title(ylab = "XIC", line = 2.5)
    
    ## d0 points
    graphics::points(x = x@st, y = x@d0, pch = 20, cex = 0.9)
    
    ## unsmoothed XIC
    graphics::lines(x = x@st, y = x@xic, col = "#00000075", lty = "dashed")
    
    ## d0 apex point
    if (results)
    {
        graphics::points(x = x@st[x@results$apex], y = x@d0[x@results$apex], 
                         col = "red", pch = 20)
    }
    
    ## d0 current vars
    char_ann <- paste0("id ", x@id, "\n",
                       "note ", x@results$note, "\n")
    
    if (nchar(annotation)>0)
    {
        char_ann <- paste0(char_ann, annotation, "\n")
    }
    
    ## results textbox
    if (results)
    {
        char_ann <- paste0(char_ann,
                           "bl_bound ", paste(c(x@results$fblb,
                                                x@results$tblb), 
                                              collapse = "->"), "\n",
                           "peak_bound ", paste(c(x@results$fpkb,
                                                  x@results$tpkb), 
                                                collapse = "->"), "\n",
                           "code ", paste0(x@results$fcode, x@results$tcode), "\n",
                           "SN ", round(x@results$sn, 3), "\n",
                           "fwhm ", round(x@results$fwhm, 3), "\n",
                           "wb ", round(x@results$wb, 3), "\n",
                           "TF ", round(x@results$tf, 3), "\n")
    }
    
    ## metadata textbox
    graphics::text(x = x@st[x@procData$plotrange[2]], y = 0.95*ylim[2],
                   labels = paste0("m/z ", round(getParam(x@param, "mz_range")[1], 3), 
                                   " : ", 
                                   round(getParam(x@param, "mz_range")[2], 3)), 
                   adj = c(1,1), cex = 0.75)
    
    ## d0 emg fit
    # if (!is.null(cpc_xic$emg))
    # {
    #     lines(x = cpc_xic$res$bl_front_bound:cpc_xic$res$bl_tail_bound,
    #           y = cpc_xic$emg$area *
    #               c_demg2(x = cpc_xic$res$bl_front_bound:cpc_xic$res$bl_tail_bound,
    #                       mu = cpc_xic$emg$mu,
    #                       sigma = cpc_xic$emg$sigma,
    #                       lambda = cpc_xic$emg$lambda),
    #           col = "red", pch = 20, type = "o")
    # }
    
    # d0 emg fit
    if (plotEMG && !is.null(x@results$emu) && x@results$emu > 0 &&
        length(x@rawProcResults) > 0)
    {
        fittedPeaks <- which(x@rawProcResults$emg_mu > 0)
        
        if(any(fittedPeaks == x@rawProcResults$current_peak[1]+1))
        {
            fittedVIP <- which(fittedPeaks == x@rawProcResults$current_peak[1]+1)
        }
        
        fittedPars <- vector(mode = "numeric", length = 4*length(fittedPeaks))
        
        # populate fittedPars
        for (i in 1:length(fittedPeaks))
        {
            fittedPars[(i-1)*4+1] <- x@rawProcResults$emg_mu[fittedPeaks[i]]+1
            fittedPars[(i-1)*4+2] <- x@rawProcResults$emg_sigma[fittedPeaks[i]]
            fittedPars[(i-1)*4+3] <- x@rawProcResults$emg_lambda[fittedPeaks[i]]
            fittedPars[(i-1)*4+4] <- x@rawProcResults$emg_area[fittedPeaks[i]]
        }
        
        # plot fitted combined trace
        graphics::points(x = x@st[x@results$fblb:x@results$tblb],
               y = c_emgfun(x = x@results$fblb:x@results$tblb, 
                            pars = fittedPars, 
                            npeaks = length(fittedPeaks)),
               col = "red")
        
        # plot each peak separately
        for (i in 1:length(fittedPeaks))
        {
            graphics::lines(x = x@st[x@results$fblb:x@results$tblb],
                  y = c_emgfun(x = x@results$fblb:x@results$tblb, 
                               pars = fittedPars[(i-1)*4+(1:4)], 
                               npeaks = 1), 
                  col = base::ifelse(i == fittedVIP, "red", "blue"), 
                  type = "o", pch = 20)
        }
        
        # lines(x = x@results$fblb:x@results$tblb,
        #       y = c_emgfun(x = x@results$fblb:x@results$tblb, 
        #                    pars = c(x@results$emu,
        #                             x@results$esigma,
        #                             x@results$elambda,
        #                             x@results$earea), 
        #                    npeaks = 1), col = "red", type = "o", pch = 20)
        
        char_ann <- paste0(char_ann,
                           "fit_emg: yes")
    }
    
    
    ## d0 peak box
    if (results)
    {
        # d0 baseline
        cur_bl <- x@d0[x@results$fblb] + 
            (1:length(x@d0)-x@results$fblb) * x@results$blslp
        graphics::lines(x = x@st[x@results$fblb:x@results$tblb],
                        y = cur_bl[x@results$fblb:x@results$tblb], 
                        col = "red", lty = "dashed")
        # points(x = c(x@results$fblb,
        #              x@results$tblb),
        #        y = x@d0[c(x@results$fblb, x@results$tblb)],
        #        col = "red", pch = 20, cex = 1.1)
        # lines(x = c(x@results$fblb, x@results$tblb),
        #       y = x@d0[c(x@results$fblb, x@results$tblb)],
        #       col = "red", lty = "dashed")
        
        # polygon
        cpc_pkbounds <- c(x@results$fpkb,
                          x@results$tpkb)
        graphics::polygon(x = c(x@st[cpc_pkbounds[1]:cpc_pkbounds[2]],
                                x@st[rev(cpc_pkbounds[1]:cpc_pkbounds[2])]),
                          y = c(x@d0[cpc_pkbounds[1]:cpc_pkbounds[2]],
                                cur_bl[rev(cpc_pkbounds[1]:cpc_pkbounds[2])]), 
                          col = "#FF000050", border = NA)
        
        # peak bound lines to baseline
        graphics::lines(x = rep(x@st[cpc_pkbounds[1]], 2),
                        y = c(x@d0[cpc_pkbounds[1]],
                              cur_bl[cpc_pkbounds[1]]), col = "red")
        graphics::lines(x = rep(x@st[cpc_pkbounds[2]], 2),
                        y = c(x@d0[cpc_pkbounds[2]],
                              cur_bl[cpc_pkbounds[2]]), col = "red")
        
        # peak bound points
        graphics::points(x = x@st[cpc_pkbounds],
                         y = x@d0[cpc_pkbounds],
                         col = "red", pch = 20, cex = 0.9)
        
        
        # ## bottom
        # tmp_x <- unlist(x@results[c("fpkb", "tpkb")])
        # tmp_y <- c(interpolate_y(x = unlist(x@results[c("fblb", "tblb")]),
        #                          y = x@d0[unlist(x@results[c("fblb", "tblb")])],
        #                          xval = unlist(x@results$fpkb)),
        #            interpolate_y(x = unlist(x@results[c("fblb", "tblb")]),
        #                          y = x@d0[unlist(x@results[c("fblb", "tblb")])],
        #                          xval = unlist(x@results$tpkb)))
        # 
        # lines(x = tmp_x, y = tmp_y, col = "red")
        # 
        # ## left
        # tmp_x <- rep(x@results$fpkb, 2)
        # tmp_y <- c(interpolate_y(x = unlist(x@results[c("fblb", "tblb")]),
        #                          y = x@d0[unlist(x@results[c("fblb", "tblb")])],
        #                          xval = unlist(x@results$fpkb)),
        #            max(x@d0[floor(x@results$fpkb):floor(x@results$tpkb)]))
        # 
        # lines(x = tmp_x, y = tmp_y, col = "red")
        # 
        # ## top
        # tmp_x <- unlist(x@results[c("fpkb", "tpkb")])
        # tmp_y <- rep(max(x@d0[floor(x@results$fpkb):floor(x@results$tpkb)]), 2)
        # 
        # lines(x = tmp_x, y = tmp_y, col = "red")
        # 
        # ## right
        # tmp_x <- rep(x@results$tpkb, 2)
        # tmp_y <- c(interpolate_y(x = unlist(x@results[c("fblb", "tblb")]),
        #                          y = x@d0[unlist(x@results[c("fblb", "tblb")])],
        #                          xval = unlist(x@results$tpkb)),
        #            max(x@d0[floor(x@results$fpkb):floor(x@results$tpkb)]))
        # 
        # lines(x = tmp_x, y = tmp_y, col = "red")
    }
    
    # plot XCMS bounds if plotXCMS == TRUE and XCMS data exist for the peak
    if (plotXCMS && 
        all(is.numeric(unlist(x@procData$pd[c("mz", 
                                              "rt", 
                                              "rtmin", 
                                              "rtmax")]))))
    {
        xcms_fb <- binsearch_closest(x = x@mzMeta$header$retentionTime,
                                     val = x@procData$pd$rtmin)
        xcms_tb <- binsearch_closest(x = x@mzMeta$header$retentionTime,
                                     val = x@procData$pd$rtmax)
        xcms_apex <- binsearch_closest(x = x@mzMeta$header$retentionTime,
                                       val = x@procData$pd$rt)
        graphics::points(x = x@st[c(xcms_fb, xcms_apex, xcms_tb)],
                         y = x@d0[c(xcms_fb, xcms_apex, xcms_tb)],
                         col = "#0000FF", cex = 1.5, lwd = 2)
    }
    
    # data box in main plot
    graphics::text(x = x@st[x@procData$plotrange[1]], y = 0.95*ylim[2], 
                   labels = char_ann, adj = c(0,1), cex = 0.6)
    
    
    # d2 main plot
    graphics::par(mar = c(4.1,4.1,0,1))
    graphics::plot(x = x@st,
                   y = x@d2, type = "l", col = "#000000", 
                   xlim = x@st[x@procData$plotrange],
                   ylim = c(min(x@d2[x@procData$plotrange[1]:x@procData$plotrange[2]]),
                            max(x@d2[x@procData$plotrange[1]:x@procData$plotrange[2]])),
                   ylab = "",
                   xlab = "")
    graphics::title(xlab = "Time (sec)",
                    ylab = "2nd derivative", line = 2.5)
    graphics::points(x = x@st, y = x@d2, pch = 20, cex = 0.9)
    # d2 peak bounds
    if (results)
    {
        graphics::points(x = x@st[unlist(x@results[c("fpkb", "tpkb")])], 
                         y = x@d2[unlist(x@results[c("fpkb", "tpkb")])],
                         col = "red", pch = 20)
    }
    
    # d2 0 line
    graphics::abline(h = 0, col = "red")
    
    graphics::layout(mat = matrix(c(1), nrow = 1, ncol = 1))
    graphics::par(mar = par_bk$mar)
    
})


#### Method: calculatePeakCharacteristics ####

#' @title Internal method for calculation of the peak characteristics
#' 
#' @description 
#' 
#' This is an internal function for calculating the peak characteristics based 
#' on the processing results.
#' 
#' @param x A \code{cpc_chrom} object.
#' 
#' @return cpc_chrom object
#' 
#' @docType methods
setMethod("calculatePeakCharacteristics", signature("cpc_chrom"), function(x)
{
    # peak height
    setResults(x) <- list(height = x@d0[x@results$apex] - 
                              interpolate_y(x = c(x@results$fblb, 
                                                  x@results$tblb), 
                                            y = x@d0[c(x@results$fblb, 
                                                       x@results$tblb)], 
                                            xval = x@results$apex))
    
    # retention time in @st units
    setResults(x) <- list(rt = x@st[x@results$apex])
    
    # peak bounds in @st units
    setResults(x) <- list(rtmin = x@st[x@results$fpkb])
    setResults(x) <- list(rtmax = x@st[x@results$tpkb])
    
    # height bounds
    ## 1% peak height
    fh1bidx <- find_height_bounds(y = x@d0, apex = x@results$apex,
                                  bl_bounds = c(x@results$fblb, 
                                                x@results$tblb),
                                  peak_bounds = c(x@results$fpkb, 
                                                  x@results$tpkb),
                                  frac = 0.01, id = x@id,
                                  debug = getParam(x@param, "verbose_output"), 
                                  plot = getParam(x@param, "plot"))
    setResults(x) <- list(fh1b = fh1bidx[1],
                          th1b = fh1bidx[2])
    setResults(x) <- list(rtf1b = interpolate_y(x = c(floor(fh1bidx[1]),
                                                      floor(fh1bidx[1])+1),
                                                y = x@st[c(floor(fh1bidx[1]),
                                                           floor(fh1bidx[1])+1)],
                                                xval = fh1bidx[1]),
                          rtt1b = interpolate_y(x = c(floor(fh1bidx[2]),
                                                      floor(fh1bidx[2])+1),
                                                y = x@st[c(floor(fh1bidx[2]),
                                                           floor(fh1bidx[2])+1)],
                                                xval = fh1bidx[2]))
    
    ## 5% peak height
    fh5bidx <- find_height_bounds(y = x@d0, apex = x@results$apex,
                                  bl_bounds = c(x@results$fblb, 
                                                x@results$tblb),
                                  peak_bounds = c(x@results$fpkb, 
                                                  x@results$tpkb),
                                  frac = 0.05, id = x@id,
                                  debug = getParam(x@param, "verbose_output"), 
                                  plot = getParam(x@param, "plot"))
    setResults(x) <- list(fh5b = fh5bidx[1],
                          th5b = fh5bidx[2])
    setResults(x) <- list(rtf5b = interpolate_y(x = c(floor(fh5bidx[1]),
                                                      floor(fh5bidx[1])+1),
                                                y = x@st[c(floor(fh5bidx[1]),
                                                           floor(fh5bidx[1])+1)],
                                                xval = fh5bidx[1]),
                          rtt5b = interpolate_y(x = c(floor(fh5bidx[2]),
                                                      floor(fh5bidx[2])+1),
                                                y = x@st[c(floor(fh5bidx[2]),
                                                           floor(fh5bidx[2])+1)],
                                                xval = fh5bidx[2]))
    
    ## 10% peak height
    fh10bidx <- find_height_bounds(y = x@d0, apex = x@results$apex,
                                   bl_bounds = c(x@results$fblb, 
                                                 x@results$tblb),
                                   peak_bounds = c(x@results$fpkb, 
                                                   x@results$tpkb),
                                   frac = 0.10, id = x@id,
                                   debug = getParam(x@param, "verbose_output"), 
                                   plot = getParam(x@param, "plot"))
    setResults(x) <- list(fh10b = fh10bidx[1],
                          th10b = fh10bidx[2])
    setResults(x) <- list(rtf10b = interpolate_y(x = c(floor(fh10bidx[1]),
                                                      floor(fh10bidx[1])+1),
                                                y = x@st[c(floor(fh10bidx[1]),
                                                           floor(fh10bidx[1])+1)],
                                                xval = fh10bidx[1]),
                          rtt10b = interpolate_y(x = c(floor(fh10bidx[2]),
                                                      floor(fh10bidx[2])+1),
                                                y = x@st[c(floor(fh10bidx[2]),
                                                           floor(fh10bidx[2])+1)],
                                                xval = fh10bidx[2]))
    
    ## 50% peak height
    fh50bidx <- find_height_bounds(y = x@d0, apex = x@results$apex,
                                   bl_bounds = c(x@results$fblb, 
                                                 x@results$tblb),
                                   peak_bounds = c(x@results$fpkb, 
                                                   x@results$tpkb),
                                   frac = 0.5, id = x@id,
                                   debug = getParam(x@param, "verbose_output"), 
                                   plot = getParam(x@param, "plot"))
    setResults(x) <- list(fh50b = fh50bidx[1],
                          th50b = fh50bidx[2])
    setResults(x) <- list(rtf50b = interpolate_y(x = c(floor(fh50bidx[1]),
                                                       floor(fh50bidx[1])+1),
                                                 y = x@st[c(floor(fh50bidx[1]),
                                                            floor(fh50bidx[1])+1)],
                                                 xval = fh50bidx[1]),
                          rtt50b = interpolate_y(x = c(floor(fh50bidx[2]),
                                                       floor(fh50bidx[2])+1),
                                                 y = x@st[c(floor(fh50bidx[2]),
                                                            floor(fh50bidx[2])+1)],
                                                 xval = fh50bidx[2]))
    
    # base width in @st units
    setResults(x) <- list(wb = x@results$rtt1b - x@results$rtf1b)
    
    # fwhm in @st units
    setResults(x) <- list(fwhm = x@results$rtt50b - x@results$rtf50b)
    
    # peak integral in @st units
    ## Build an idx vector of all points between the height bounds
    integral_idx <- (max(1, floor(fh1bidx[1])+1)):min(length(x@d0), 
                                                      floor(fh1bidx[2]))
    
    ## Build a y vector of all points between the height bounds
    integral_y <- x@d0[integral_idx]
    
    ## Build an st vector of all points between the height bounds
    integral_x <- x@st[integral_idx]
    
    ## Add the interpolated idx, st and y values of the height bounds to the st 
    ## and y vectors, respectively
    integral_idx <- c(fh1bidx[1], integral_idx)
    
    integral_x <- c(interpolate_y(x = c(floor(fh1bidx[1]),
                                        floor(fh1bidx[1])+1),
                                  y = x@st[c(floor(fh1bidx[1]),
                                             floor(fh1bidx[1])+1)],
                                  xval = fh1bidx[1]), 
                    integral_x)
    
    integral_y <- c(interpolate_y(x = c(floor(fh1bidx[1]),
                                        floor(fh1bidx[1])+1),
                                  y = x@d0[c(floor(fh1bidx[1]),
                                             floor(fh1bidx[1])+1)],
                                  xval = fh1bidx[1]), 
                    integral_y)
    
    if (floor(fh1bidx[2]) < fh1bidx[2])
    {
        integral_idx <- c(integral_idx, fh1bidx[2])
        
        intergral_x <- c(integral_x,
                         interpolate_y(x = c(floor(fh1bidx[2]),
                                             floor(fh1bidx[2])+1),
                                       y = x@st[c(floor(fh1bidx[2]),
                                                  floor(fh1bidx[2])+1)],
                                       xval = fh1bidx[2]))
        
        integral_y <- c(integral_y,
                        interpolate_y(x = c(floor(fh1bidx[2]),
                                            floor(fh1bidx[2])+1),
                                      y = x@d0[c(floor(fh1bidx[2]),
                                                 floor(fh1bidx[2])+1)],
                                      xval = fh1bidx[2]))
    }
    
    ## TODO: Ensure I am properly handling few points
    ## TODO: Ensure I am not moving out of bounds
    integral_bl <- ((integral_idx - x@results$fblb) * x@results$blslp) + 
        x@d0[x@results$fblb]
    
    setResults(x) <- 
        list(area = peak_integral(x = integral_x, 
                                  y = integral_y - integral_bl)) 
    # ~5 µsecs for 25509
    
    # front and tail width as well as asymmetry in @st units
    setResults(x) <- list(a = x@results$rt - x@results$rtf10b,
                          b = x@results$rtt10b - x@results$rt)
    
    setResults(x) <- list(tf = x@results$b / x@results$a)
    
    # signal-to-noise
    setResults(x) <- list(sn = ifelse(x@procData$xic_noise > 0,
                                      2*x@results$height / x@procData$xic_noise,
                                      Inf))
    
    # note
    setResults(x) <- list(note = "detected")
    
    # return object
    return(x)
    
})


#### Method: smoothChromatogram ####

#' @title Method for calculating smoothed XICs in a cpc_chrom object.
#' 
#' @param x A \code{cpc_chrom} object.
#' 
#' @return A \code{cpc_chrom} object.
#' 
#' @docType methods
setMethod("smoothChromatogram", signature("cpc_chrom"), function(x)
{
    if (length(x@xic) < 1)
    {
        stop("No XIC data in object.")
    }
    
    # smooth times
    if (getParam(x@param, "smooth_method") == "savgol")
    {
        if (!is.null(getParam(x@param, "smooth_times")))
        {
            if (getParam(x@param, "smooth_times") < 0L)
            {
                warning(paste("smooth_times cannot be negative. ",
                              "Using default: 2", 
                              sep = ""))
                
                setParam(x@param) <- list(smooth_times = 2L)
            }
        }
        
        if (getParam(x@param, "smooth_times") > 3) 
            warning(paste("Excessive smoothing (", getParam(x@param, "smooth_times"),
                          "). Consider using fewer smoothing steps.", 
                          sep = ""))
    } else
    {
        setParam(x@param) <- list(smooth_times = 2L)
    }
    
    # smooth window
    if (is.null(getParam(x@param, "smooth_win")) || 
        getParam(x@param, "smooth_win") == 0L)
    {
        setParam(x@param) <- 
            list(smooth_win = 
                     ifelse(floor(2.354*getParam(x@param, "s")) %% 2 == 0, # If w is even
                            floor(2.354*getParam(x@param, "s")) + 1, # w + 1
                            floor(2.354*getParam(x@param, "s")))) # else w
    }
    
    if (getParam(x@param, "smooth_win") < getParam(x@param, "min_w"))
    {
        setParam(x@param) <- list(smooth_win = getParam(x@param, "min_w"))
    } else if (getParam(x@param, "smooth_win") > getParam(x@param, "max_w"))
    {
        setParam(x@param) <- list(smooth_win = getParam(x@param, "max_w"))
    }
    
    # calculate smoothed vectors
    if (getParam(x@param, "smooth_method") == "savgol") # use savitzky-golay filter
    {
        x@d0 <- x@xic
        
        if (getParam(x@param, "smooth_times") > 1)
        {
            for (i in 1:(getParam(x@param, "smooth_times")-1))
            {
                x@d0 <- pmax(0, signal::sgolayfilt(x = x@d0, p = 2, 
                                                   n = getParam(x@param, "smooth_win"), 
                                                   m = 0)) # ~780 µsecs for 25509
            }
        }
        
        x@d2 <- signal::sgolayfilt(x = x@d0, p = 2, 
                                   n = getParam(x@param, "smooth_win"), 
                                   m = 2) # ~789 µsecs for 25509
        
        x@d0 <- pmax(0, signal::sgolayfilt(x = x@d0, p = 2, 
                                           n = getParam(x@param, "smooth_win"), 
                                           m = 0)) # ~780 µsecs for 25509
        
    } else # use mean smoothing
    {
        x@d0 <- x@xic
        
        # calculate d1 using running linear regression on d0
        d1 <- c_running_slope(1:getParam(x@param, "nscan"), x@d0,
                              floor(getParam(x@param, "smooth_win")/2))
        
        # calculate d2 using running linear regression on d1
        x@d2 <- c_running_slope(1:getParam(x@param, "nscan"), d1, 
                                floor(getParam(x@param, "smooth_win")/2))
    }
    
    return(x)
})

#### Method: processChromatogram ####

#' @title Main method for processing a chromatogram
#' 
#' @description 
#' 
#' This method will perform the processing of a chromatogram on a \code{cpc_chrom}
#' object. This method is called repeatedly by \code{characterize_xcms_peaklist}.
#' 
#' @param x A \code{cpc_chrom} object.
#' 
#' @return A \code{cpc_chrom} object.
#' 
#' @export
#' @docType methods
#' @rdname cpc_chrom-methods
setMethod("processChromatogram", signature("cpc_chrom"), function(x)
{
    if (is.null(x@xic) || length(x@xic) < 1)
    {
        stop("No XIC supplied to processChromatogram().")
    } else
    {
        setParam(x@param) <- list(nscan = length(x@xic))
    }
    
    ## check that XCMS data is not corrupt
    # if (is.null(getParam(x@param, "p")))
    # {
    #     if (getParam(x@param, "verbose_output"))
    #         message(paste0("[debug] idx =", x@id, "missing xcms data.\n"))
    #     
    #     setResults(x) <- list(id = x@id, note = "xcms_missing")
    #     
    #     return(x)
    #     
    # } else 
    if (getParam(x@param, "p") < 1)
    {
        if (getParam(x@param, "verbose_output"))
            message(paste0("[debug] idx =", x@id, "missing xcms data.\n"))
        
        setResults(x) <- list(id = x@id, note = "xcms_missing")
        
        return(x)
        
    }
    
    # if a vip is selected, set plotrange to be 20 standard deviations around
    # that peak, else set it to the entire chromatogram
    if (!is.null(getParam(x@param, "p")))
    {
        setProcData(x) <- list(plotrange = c(max(1, floor(getParam(x@param, "p") - 
                                                              20*getParam(x@param, "s"))),
                                             min(x@procData$nscan, 
                                                 floor(getParam(x@param, "p") + 
                                                           20*getParam(x@param, "s")))))
        vip_selected <- 1L
        
    } else
    {
        setProcData(x) <- list(plotrange = c(1, x@procData$nscan))
        vip_selected <- 0L
        
    }
    
    # setup and check procParams
    ## smooth times
    if (getParam(x@param, "smooth_method") == "savgol")
    {
        if (!is.null(getParam(x@param, "smooth_times")) && 
            getParam(x@param, "smooth_times") < 0)
        {
            warning(paste0("smooth_times cannot be negative. ",
                           "Using default: 2"))
            
            setParam(x@param) <- list(smooth_times = 2L)
        }
        
        if (getParam(x@param, "smooth_times") > 3) 
            warning(paste0("Excessive smoothing (", 
                           getParam(x@param, "smooth_times"),
                           "). Consider using fewer smoothing steps."))
    } else
    {
        setParam(x@param) <- list(smooth_times = 2L)
    }
    
    ## smooth window
    if (is.null(getParam(x@param, "smooth_win")) || 
        getParam(x@param, "smooth_win") == 0L)
    {
        setParam(x@param) <- 
            list(smooth_win = 
                     ifelse(floor(2.354*getParam(x@param, "s")) %% 2 == 0, # If w is even
                            floor(2.354*getParam(x@param, "s")) + 1, # w + 1
                            floor(2.354*getParam(x@param, "s")))) # else w
        
    }
    
    if (getParam(x@param, "smooth_win") < getParam(x@param, "min_w"))
    {
        setParam(x@param) <- list(smooth_win = getParam(x@param, "min_w"))
        
    } else if (getParam(x@param, "smooth_win") > getParam(x@param, "max_w"))
    {
        setParam(x@param) <- list(smooth_win = getParam(x@param, "max_w"))
        
    }
    
    # calculate smoothed vectors
    if (getParam(x@param, "smooth_method") == "savgol") # use savitzky-golay filter
    {
        x@d0 <- x@xic
        
        if (getParam(x@param, "smooth_times") > 1L)
        {
            for (i in 1:(getParam(x@param, "smooth_times")-1))
            {
                x@d0 <- pmax(0, signal::sgolayfilt(x = x@d0, p = 2, 
                                                   n = getParam(x@param, "smooth_win"), 
                                                   m = 0)) # ~780 µsecs for 25509
                
            }
            
        }
        
        x@d2 <- signal::sgolayfilt(x = x@d0, p = 2, 
                                   n = getParam(x@param, "smooth_win"), 
                                   m = 2) # ~789 µsecs for 25509
        
        x@d0 <- pmax(0, signal::sgolayfilt(x = x@d0, p = 2, 
                                           n = getParam(x@param, "smooth_win"), 
                                           m = 0)) # ~780 µsecs for 25509
        
    } else # use mean smoothing
    {
        x@d0 <- x@xic
        
        # calculate d1 using running linear regression on d0
        d1 <- c_running_slope(1:getParam(x@param, "nscan"), x@d0, 
                              floor(getParam(x@param, "smooth_win")/2))
        
        # calculate d2 using running linear regression on d1
        x@d2 <- c_running_slope(1:getParam(x@param, "nscan"), d1, 
                                floor(getParam(x@param, "smooth_win")/2))
        
    }
    
    # x <- smoothChromatogram(x)
    
    # calculate noise
    setProcData(x) <- 
        list(noise_sel = which(abs(x@d2) <= 
                                   stats::quantile(abs(x@d2), .95)))
    
    if (length(x@procData$noise_sel) < floor(getParam(x@param, "nscan")/2))
    {
        x@procData$noise_sel <- 1:getParam(x@param, "nscan")
        
    }
    
    setProcData(x) <- list(xic_noise = c_peak_to_peak_noise(x = x@procData$noise_sel-1,
                                                            y = x@xic, w = 1),
                           d0_noise = c_peak_to_peak_noise(x = x@procData$noise_sel-1,
                                                           y = x@d0, w = 1),
                           d2_noise = c_peak_to_peak_noise(x = x@procData$noise_sel-1,
                                                           y = x@d2, w = 1))
    
    # calculate detection threshold (if set)
    if (!is.null(getParam(x@param, "min_intensity")))
    {
        # TODO: recalculate d0 minimum intensity to d2 minimum intensity using
        #       second derivative of gaussian
        setParam(x@param) <- list(min_intensity = x@procData$d2_noise)
        # x@procParams$min_intensity <- x@procData$d2_noise
        
    } else
    {
        setParam(x@param) <- list(min_intensity = x@procData$d2_noise)
        # x@procParams$min_intensity <- x@procData$d2_noise
        
    }
    
    # process chromatogram (C++ function)
    x@rawProcResults <- process_chromatogram(d0 = x@d0, 
                                             d2 = x@d2, 
                                             st = x@st,
                                             apex_thresh = 0L,
                                             w = floor(getParam(x@param, "smooth_win")/2L), 
                                             p = getParam(x@param, "p")-1L,
                                             output = as.integer(x@param@verbose_output), 
                                             fit_emg = as.integer(x@param@fit_emg), 
                                             fit_only_vip = as.integer(vip_selected),
                                             min_shoulder_pts = getParam(x@param, "min_shoulder_pts"),
                                             min_rounded_pts = getParam(x@param, "min_rounded_pts"))
    
    # if a vip was selected
    if (vip_selected > 0L)
    {
        # check that the current peak was detected
        if (x@rawProcResults$current_peak < 0)
        {
            # set not_detected flag in results
            setResults(x) <- list(note = "not_detected")
            
            # plot result if plot
            if (getParam(x@param, "plot")) plotPeak(x)
            
            return(x)
            
        }
        
        # record results from processing
        setResults(x) <- list(
            apex = x@rawProcResults$adj_apex[x@rawProcResults$current_peak+1]+1,
            finf = x@rawProcResults$front_inf[x@rawProcResults$current_peak+1]+1,
            tinf = x@rawProcResults$tail_inf[x@rawProcResults$current_peak+1]+1,
            fblb = x@rawProcResults$front_baseline_bound[x@rawProcResults$current_peak+1]+1,
            tblb = x@rawProcResults$tail_baseline_bound[x@rawProcResults$current_peak+1]+1,
            fpkb = x@rawProcResults$front_peak_bound[x@rawProcResults$current_peak+1]+1,
            tpkb = x@rawProcResults$tail_peak_bound[x@rawProcResults$current_peak+1]+1,
            fcode = switch(x@rawProcResults$front_code[x@rawProcResults$current_peak+1]+1, 
                           "B", "V", "S", "R"),
            tcode = switch(x@rawProcResults$tail_code[x@rawProcResults$current_peak+1]+1, 
                           "B", "V", "S", "R"),
            blslp = (x@d0[(x@rawProcResults$tail_baseline_bound[x@rawProcResults$current_peak+1]+1)] - 
                         x@d0[(x@rawProcResults$front_baseline_bound[x@rawProcResults$current_peak+1]+1)]) / 
                ((x@rawProcResults$tail_baseline_bound[x@rawProcResults$current_peak+1]+1) - 
                     (x@rawProcResults$front_baseline_bound[x@rawProcResults$current_peak+1]+1)),
            emu = x@rawProcResults$emg_mu[x@rawProcResults$current_peak+1]+1,
            esigma = x@rawProcResults$emg_sigma[x@rawProcResults$current_peak+1],
            elambda = x@rawProcResults$emg_lambda[x@rawProcResults$current_peak+1],
            earea = x@rawProcResults$emg_area[x@rawProcResults$current_peak+1],
            econv = x@rawProcResults$emg_conv[x@rawProcResults$current_peak+1],
            note = "detected"
        )
        
        # check peak bounds and apex
        if (x@results$fblb < 1 || x@results$tblb < 1 || 
            x@results$fpkb < 1 || x@results$tpkb < 1 || 
            x@results$apex < 1)
        {
            setResults(x) <- list(note = "not_detected")
            
            if (getParam(x@param, "plot")) plotPeak(x)
            
            return(x)
            
        }
        
        # check peak width
        # if (x@results$tpkb - x@results$fpkb < getParam(x@param, "min_pts"))
        # {
        #     setResults(x) <- list(note = "too_narrow")
        # 
        #     if (getParam(x@param, "plot")) plotPeak(x)
        # 
        #     return(x)
        # }
        
        # calculate peak characteristics
        x <- calculatePeakCharacteristics(x)
        
        # if plot true -> plot result
        if (getParam(x@param, "plot")) plotPeak(x)
        
    }
    
    # return object
    return(x)
})


#### Class: cpc_raw ####

#' @title Class for handling the MS raw data backend
#' 
#' @description 
#' 
#' Class to manage the MS raw data backend using the mzR R-package to parse the
#' raw data.
#' 
#' @slot file_path File path to the raw MS file
#' @slot header Header of the raw MS file
#' @slot runInfo Experiment information from the raw MS file
#' @slot intensity \code{numeric} vector of the individual MS peak intensities
#' @slot mz \code{numeric} vector of the individual MS peak m/z values
#' @slot scanidx \code{integer} vector indicating last idx in \code{intensity} and \code{mz} for each scan
#' @slot scanrate Scans per second
#' @slot scantime Seconds per scan
#' @slot nscan Number of scans
#' @slot backend String defining the backend used (currently not used).
#' 
#' @name cpc_raw-class
#' @rdname cpc_raw-class
#' @export
setClass("cpc_raw",
         representation(
             file_path = "character",
             header = "data.frame",
             runInfo = "list",
             intensity = "numeric",
             mz = "numeric",
             scanidx = "integer",
             scanrate = "numeric",
             scantime = "numeric",
             nscan = "integer",
             backend = "character"
         ),
         
         prototype(
             file_path = NA_character_,
             header = data.frame(),
             runInfo = list(),
             intensity = NA_real_,
             mz = NA_real_,
             scanidx = NA_integer_,
             scanrate = NA_real_,
             scantime = NA_real_,
             nscan = NA_integer_,
             backend = "pwiz"
         ))

#### Method: getXIC ####

#' @title Method to generate an XIC from a \code{cpc_raw} object
#' 
#' @param x A \code{cpc_raw} object with parsed MS data
#' @param mzrange A \code{numeric} vector of length 2.
#' @param scanrange An \code{integer} vector of length 2.
#' @param method A single \code{integer} indicating which method to use.
#' 
#' @return A \code{numeric} vector.
#' 
#' @docType methods
setMethod("getXIC", signature("cpc_raw"), function(x, mzrange, scanrange = NULL, 
                                                   method = 1)
{
    if (is.null(scanrange)) scanrange <- c(0, x@runInfo$scanCount-1)
    
    if (method == 1)
    {
        return(getEIC_Rcpp(mz = x@mz, 
                           intensity = x@intensity,
                           scan_idx = x@scanidx-1,
                           mz_range = mzrange,
                           scan_range = scanrange))
    } else
    {
        # for some reason this is much slower, using std vectors as arguments instead
        # of Rcpp-types... I'll just leave it like this until I can look into this.
        return(getEIC_min(mz = x@mz, 
                          intensity = x@intensity,
                          scan_idx = x@scanidx-1,
                          mz_range = mzrange,
                          scan_range = scanrange))
    }
})

#### Method: parseMz ####

#' @title Parsing method for the MS raw data contained in the specified file
#' 
#' @param x A \code{cpc_raw} object
#' 
#' @return A \code{cpc_raw} object
#' 
#' @docType methods
setMethod("parseMz", signature("cpc_raw"), function(x) 
{
    conn <- mzR::openMSfile(filename = x@file_path)
    peaks <- mzR::peaks(conn)
    
    x@scanidx <- cumsum(unlist(lapply(peaks, nrow)))
    x@nscan <- length(x@scanidx)
    
    x@mz <- unlist(lapply(peaks, function(x) return(x[, 1])))
    x@intensity <- unlist(lapply(peaks, function(x) return(x[, 2])))
    
    x@runInfo <- mzR::runInfo(conn)
    
    x@header <- as.data.frame(mzR::header(conn))
    
    # get scantimes
    x@scantime <- x@header$retentionTime
    
    # determine scanrate
    x@scanrate <- (x@nscan - 1) / 
        (x@runInfo$dEndTime - x@runInfo$dStartTime)
    
    return(x)
})


#### Method: scantime ####

#' @title Getter method for the scantime vector in a \code{cpc_raw} object
#' 
#' @param x A \code{cpc_raw} object
#' 
#' @return A \code{numeric} vector with scantimes
#' 
#' @docType methods
setMethod("scantime", signature("cpc_raw"), function(x)
{
    return(x@header$retentionTime)
})



#### Class: cpc ####

#' cpc class
#' 
#' @slot xd Original \code{XCMSnExp} object.
#' @slot xdFilt Filtered \code{XCMSnExp} object.
#' @slot rawResults A list of \code{data.frame}s with back-end results from the processing.
#' @slot pt \code{data.frame} containing the original peak table from \code{xd}.
#' @slot cpt \code{data.frame} containing the determined peak characteristics.
#' @slot outcomes \code{data.frame} containing the results from the peak filters.
#' @slot fdef Feature definitions (not used currently).
#' @slot fpeaks Feature defining peaks (not used currently).
#' @slot param \code{cpcParam} object holding the processing parameters
#' @slot procData \code{list} containing processing data.
#' @slot procParams \code{list} containing processing parameters. (deprecated)
#' 
#' @name cpc-class
#' @rdname cpc-class
#' @export
setClass("cpc",
         representation(
             xd = "XCMSnExp",
             xdFilt = "XCMSnExp",
             rawResults = "list",
             pt = "data.frame",
             cpt = "data.frame",
             outcomes = "data.frame",
             fdef = "data.frame",
             fpeaks = "list",
             param = "cpcParam",
             procData = "list",
             procParams = "list"
         ),
         
         prototype(
             xd = new("XCMSnExp"),
             xdFilt = new("XCMSnExp"),
             rawResults = list(),
             pt = data.frame(),
             cpt = data.frame(),
             outcomes = data.frame(),
             fdef = data.frame(),
             fpeaks = list(),
             param = cpcProcParam(),
             procData = list(),
             procParams = list(ppm = 50, 
                               min_pts = 7,
                               min_inf_width = 3,
                               min_sn = 10,
                               min_frac = NULL,
                               min_intensity = NULL,
                               smooth_method = "savgol",
                               smooth_times = 2,
                               smooth_win = NULL,
                               fit_emg = TRUE, 
                               sel_peaks = NULL,
                               sel_files = NULL,
                               verbose_output = FALSE)
         ))



#### Method: hasPeakTable ####

#' @title Method to check if a \code{cpc} object has a parsed peaktable
#' 
#' @param x A \code{cpc} object
#' 
#' @return A \code{logical} indicating if there is a peak table in the object
#' 
#' @docType methods
setMethod("hasPeakTable", signature("cpc"), function(x)
{
    return(nrow(x@pt) > 0)
})


#### Method: hasCharacterizedPeakTable ####

#' @title Method to check if a \code{cpc} object has a characterized peaktable
#' 
#' @param x A \code{cpc} object
#' 
#' @return A \code{logical} indicating if there is a cpc characterized peak table in the object
#' 
#' @docType methods
setMethod("hasCharacterizedPeakTable", signature("cpc"), function(x) 
{
    return(nrow(x@cpt) > 0 & nrow(x@cpt) == length(getParam(x@param, "sel_peaks")))
})


#### Method: xdObj ####

#' @title Getter method for the \code{XCMSnExp} object contained in a \code{cpc} object
#' 
#' @param x A \code{cpc} object
#' 
#' @return A \code{XCMSnExp} object
#' 
#' @docType methods
setMethod("xdObj", signature("cpc"), function(x) x@xdObj)


#### Method: xdObj<- ####

#' @title Setter method for the \code{XCMSnExp} object slot in a \code{cpc} object
#' 
#' @param x A \code{cpc} object
#' 
#' @return A \code{cpc} object
#' 
#' @docType methods
setMethod("xdObj<-", signature("cpc"), function(x, value) 
{
    # data checks
    if (class(value) != "XCMSnExp")
        stop("xd had to be a class of type 'XCMSnExp'")
    
    x@xdObj <- value
    return(x)
})

#### Method: filteredObject ####

#' @title Getter method that returns the filtered \code{XCMSnExp} object.
#' 
#' @param x A \code{cpc} object
#' 
#' @return A filtered \code{XCMSnExp} object
#' 
#' @docType methods
setMethod("filteredObject", signature("cpc"), function(x) return(x@xdFilt))


#### Method: peaksToKeep ####

#' @title Method to generate an idx vector of peaks that pass the filter criteria after peak characterization.
#' 
#' @param x A \code{cpc} object
#' @param returnBoolean \code{logical} indicating if a vector of logicals should be returned instead of the row numbers (default: FALSE)
#' 
#' @return An \code{integer} vector of peak idx that pass the criteria or if \code{returnBoolean} is TRUE, a vector of logicals indicating, for each peak, if they pass the criteria
#' 
#' @docType methods
setMethod("peaksToKeep", signature("cpc"), function(x, returnBoolean = FALSE)
{
    if(!hasCharacterizedPeakTable(x))
    {
        stop("Please run processPeaks() before filtering.")
    }
    
    if (is.null(getParam(x@param, "min_intensity")))
    {
        min_intensity <- 0L
    } else
    {
        min_intensity <- getParam(x@param, "min_intensity")
    }
    
    keep <- which(x@cpt$note == "detected")
    
    keep <- keep[which(x@cpt$sn[keep] >= getParam(x@param, "min_sn"))]
    keep <- keep[which(x@cpt$wb[keep] >= getParam(x@param, "min_pts"))]
    keep <- keep[which(x@cpt$area[keep] >= getParam(x@param, "min_intensity"))]
    
    if (returnBoolean)
    {
        return(1:nrow(x@cpt) %in% keep)
    } else
    {
        return(keep)
    }
})


#### Method: getPeaklist ####

#' @title Getter method for the parsed peak table in a \code{cpc} object
#' 
#' @param x A \code{cpc} object
#' 
#' @return A \code{data.frame} of the peak table
#' 
#' @export
#' @docType methods
#' @rdname cpc-methods
setMethod("getPeaklist", signature("cpc"), function(x) return(x@pt))



#### Method: setPeaklist<- ####

#' @title Setter method for the parsed peak table in a \code{cpc} object
#' 
#' @param x A \code{cpc} object
#' @param value A \code{data.frame} with peak information
#' 
#' @return A \code{cpc} object
#' 
#' @docType methods
setMethod("setPeaklist<-", signature("cpc"), function(x, value) 
{
    # data checks
    
    x@pt <- value
    return(x)
})


#### Method: cpt ####

#' @title Getter method for the \code{cpt} slot in a \code{cpc} object
#' 
#' @description 
#' 
#' Method that returns the characterized peak list with all calculated peak characteristics.
#' 
#' @param x A \code{cpc} object
#' 
#' @return A \code{data.frame} of the characterized peak table
#' 
#' @export
#' @docType methods
#' @rdname cpc-methods
setMethod("cpt", signature("cpc"), function(x) x@cpt)


#### Method: cpt<- ####

#' Setter method for the \code{cpt} slot in a \code{cpc} object
#' 
#' @param x A \code{cpc} object
#' @param value A \code{data.frame} of the characterized peak table
#' 
#' @return A \code{cpc} object
#' 
#' @docType methods
setMethod("cpt<-", signature("cpc"), function(x, value) 
{
    # data checks
    
    x@cpt <- value
    return(x)
})


#### Method: getProcData ####

#' @title Getter for the \code{procData} slot in a \code{cpc} object
#' 
#' @param x A \code{cpc} object
#' @param value Parameter name, if NULL it returns the entire slot (default = NULL)
#' 
#' @return Named \code{list} holding the parameter values
#' 
#' @docType methods
setMethod("getProcData", signature("cpc"), function(x, value = NULL)
{
    if (!is.null(value) & is.character(value))
    {
        unlist(x@procData[value])
    } else
    {
        x@procData
    }
})


#### Method: setProcData<- ####

#' @title Setter for the \code{procData} slot in a \code{cpc} object
#' 
#' Takes a named \code{list} of parameters as argument. If a parameter name 
#' already exists, it is updated with the new value and if it does not already
#' exist, it is added.
#' 
#' @param x A cpc object
#' @param value Named list of parameter values.
#' 
#' @return A \code{cpc} object
#' 
#' @docType methods
setMethod("setProcData<-", signature("cpc"), function(x, value)
{
    x@procData <- value
    return(x)
    
    if (class(value) != "list") stop("Process params must be a named list.")
    
    datNames <- names(x@procData)
    valNames <- names(value)
    
    if (length(valNames) < 1) stop("Process params must be a named list.")
    
    matchedNames <- which(!is.na(match(valNames, datNames)))
    
    # parse sel_peaks and sel_files
    
    # parse ppm
    # parse min_intensity
    # parse min_sn
    # parse smooth method, smooth_times, and smooth_win
    
    x@procData[valNames[matchedNames]] <- value[matchedNames]
    return(x)
})


#### Method: setProcData<- ####

#' @title Setter for the \code{procData} slot in a \code{cpc} object
#' 
#' @param x A \code{cpc} object
#' @param value Named \code{list} of parameter values
#' 
#' @return A \code{cpc} object
#' 
#' @docType methods
setMethod("setProcData<-", signature("cpc"), function(x, value)
{
    if (class(value) != "list") stop("Process params must be a named list.")
    
    datNames <- names(x@procData)
    valNames <- names(value)
    
    if (length(valNames) < 1) stop("Process params must be a named list.")
    
    matchedNames <- which(!is.na(match(valNames, datNames)))
    newNames <- which(is.na(match(valNames, datNames)))
    
    if (length(matchedNames) > 0)
    {
        x@procData[valNames[matchedNames]] <- value[matchedNames]
    }
    
    if (length(newNames) > 0)
    {
        x@procData[valNames[newNames]] <- value[newNames]
    }
    
    return(x)
})


#### Method: getProcParams ####

#' @title Getter method for the procParams slot
#' 
#' @param x A \code{cpc} object.
#' @param value A named \code{list} of parameters. If \code{NULL}, returns a named \code{list} of all process parameters.
#' 
#' @return A named \code{list} of process parameters.
#' 
#' @docType methods
setMethod("getProcParams", signature("cpc"), function(x, value = NULL) 
{
    if (!is.null(value) & is.character(value))
    {
        unlist(x@procParams[value])
    } else
    {
        x@procParams
    }
})


#### Method: setProcParams<- ####

#' @title Setter method for the procParams slot
#' 
#' @description 
#' 
#' Takes a named \code{list} of parameters as argument. If a parameter name 
#' already exists, it is updated with the new value and if it does not already
#' exist, it is added.
#' 
#' @param x A \code{cpc} object.
#' @param value A named \code{list} of parameters.
#' 
#' @return A \code{cpc} object.
#' 
#' @docType methods
setMethod("setProcParams<-", signature("cpc"), function(x, value)
{
    if (class(value) != "list") stop("Process params must be a named list.")
    
    parNames <- names(x@procParams)
    valNames <- names(value)
    
    if (length(valNames) < 1) stop("Process params must be a named list.")
    
    matchedNames <- which(!is.na(match(valNames, parNames)))
    newNames <- which(is.na(match(valNames, parNames)))
    
    if (length(matchedNames) > 0)
    {
        x@procParams[valNames[matchedNames]] <- value[matchedNames]
    }
    
    if (length(newNames) > 0)
    {
        x@procParams[valNames[newNames]] <- value[newNames]
    }
    
    return(x)
})

#### Method: getParam ####
#' @title Getter method for the process parameter object contained in the cpc
#' object.
#' 
#' @description 
#' 
#' Takes a \code{cpc} object and a single character vector indicating a slot in 
#' the \code{cpcParam} object.
#' 
#' @param x A \code{cpc} object.
#' @param value A \code{character} vector indicating a parameter slot in the
#' \code{cpcParam} object.
#' 
#' @return The parameter value.
#' 
#' @docType methods
setMethod("getParam", signature("cpc"), function(x, param)
{
    return(getParam(x@param, param))
    
})

#### Method: setParam<- ####
#' @title Setter method for the process parameter object contained in the cpc
#' object.
#' 
#' @description 
#' 
#' Takes a named \code{list} of parameters or another \code{cpcParam} object as 
#' argument. The element names in the \code{list} must correspond to a slot name in the 
#' cpcProcParam object.
#' 
#' @param x A \code{cpc} object.
#' @param value A named \code{list} of parameters.
#' 
#' @return A \code{cpc} object.
#' 
#' @docType methods
setMethod("setParam<-", signature("cpc"), function(x, value) 
{
    # x@param <- setParam(x@param, value)
    setParam(x@param) <- value
    
    return(x)
    
})


#### Method: parsePeaklist ####

#' @title Method for parsing the peak table from an \code{XCMSnExp} object contained in the \code{cpc} object
#' 
#' @description 
#' 
#' Parses the peak table contained in the \code{XCMSnExp} object and stores it 
#' in the \code{pt} slot.
#' 
#' @param x A \code{cpc} object
#' 
#' @return A \code{cpc} object
#' 
#' @export
#' @docType methods
#' @rdname cpc-methods
setMethod("parsePeaklist", signature("cpc"), function(x)
{
    # check that xd contain peak information
    if (nrow(xcms::chromPeaks(x@xd)) < 1) 
        stop("'xd' does not contain any peak information.")
    
    # check if there is feature data
    if (xcms::hasFeatures(x@xd)) {
        message(paste0("Removing existing feature definitions from XCMS object. ",
                      "Run retention alignment and peak filling again after ",
                      "processing."))
        
        x@xd <- xcms::dropFeatureDefinitions(x@xd)
        
    }
    
    # check if there are filled peaks and remove them
    if (xcms::hasFilledChromPeaks(x@xd))
    {
        message(paste0("Removing filled peak data from the XCMS object. "))
        
        
    }
    
    # get the peaklist from the XCMS object
    x@pt <- as.data.frame(xcms::chromPeaks(x@xd))
    
    # add id column to the peaklist
    # I think it would be better to instead convert the rownames into numbers
    # by removing "CP" and then always match to rownames when referencing peaks
    # in the peak table - this way I will always reference the right peak, even
    # if the ordering is off - might be slower tho...
    x@pt <- data.frame(id = as.numeric(sub("CP", "", rownames(x@pt))), x@pt)
    # x@pt
    
    # parse sel_peaks and sel_files
    if (!is.null(getParam(x@param, "sel_peaks")) &&
        is.numeric(getParam(x@param, "sel_peaks")))
    {
        if (!is.null(getParam(x@param, "sel_files")))
            message("Note: parameter sel_peaks will override sel_files.")
        
        if (any(getParam(x@param, "sel_peaks") < 1L &
                getParam(x@param, "sel_peaks") > nrow(x@pt))) 
            stop("Selected peaks out of bounds.")
        
        setParam(x@param) <- 
            list(sel_files = 
                     sort(unique(x@pt$sample[getParam(x@param, "sel_peaks")])))
        
    } else
    {
        if (is.null(getParam(x@param, "sel_files")))
        {
            setParam(x@param) <- list(sel_peaks = 1:nrow(x@pt),
                                      sel_files = sort(unique(x@pt$sample)))
        } else
        {
            # check that the selected files are not out of bounds of the files
            # present in the XCMS object
            if (any(!(getParam(x@param, "sel_files") %in% 
                      sort(unique(x@pt$sample)))))
                stop("Invalid files selected")
            
            # set the sel_peaks param to be all peaks originating from the
            # selected files
            setParam(x@param) <- 
                list(sel_peaks = which(x@pt$sample %in% getParam(x@param, "sel_files")))
        }
    }
    
    # ensure that file_paths are added to procData
    if (is.null(x@procData$file_paths))
    {
        # get file_paths from the XCMS object
        x@procData$file_paths <- MSnbase::fileNames(x@xd)#[getParam(x@param, "sel_files")]
        
        # check that all files can be opened
        if (!all(file.exists(x@procData$file_paths[getParam(x@param, "sel_files")]))) 
            stop("Raw data files missing or cannot be opened.")
    }
    
    # return object
    return(x)
})





#### Method: filePaths ####

#' @title Getter for the \code{procData$file_paths} slot in a \code{cpc} object
#' 
#' @param x A \code{cpc} object
#' 
#' @return A \code{character} vector with file paths
#' 
#' @docType methods
setMethod("filePaths", signature("cpc"), function(x) 
{
    x@procData$file_paths
})


#### Method: determineMaxSigma ####
setMethod("determineMaxSigma", signature("cpc"), function(x, scantime, scanrate)
{
    # ensure that the peaklist has been parsed
    
    # determine nearest scans for rtmin and rtmax
    scmin <- sapply(x@pt$rtmin, binsearch_closest, 
                    x = scantime, simplify = "array")
    scpos <- sapply(x@pt$rt, binsearch_closest, 
                    x = scantime, simplify = "array")
    # scmax <- sapply(x@pt$rtmax, binsearch_closest,
    #                 x = scantime, simplify = "array")
    
    # set max sigma value
    setParam(x@param) <- 
        list(max_sigma = 
                 as.numeric(stats::quantile(scpos - scmin,
                                            probs = 0.75, na.rm = T)) * 
                 scanrate / 2)
    
    return(x)
    
})




#### Method: getChromatogram ####

#' @title Method that generates a \code{cpc_chrom} object from a peak table entry
#' 
#' @description 
#' 
#' Prior to running this method the peak table must be parsed using the 
#' \code{parsePeaklist} or \code{parseFeatures} methods.
#' 
#' @param x A \code{cpc} object
#' @param id The peak ID
#' 
#' @return A \code{cpc_chrom} object
#' 
#' @export
#' @docType methods
#' @rdname cpc-methods
setMethod("getChromatogram", signature("cpc"), function(x, id) 
{
    if (nrow(cpt(x)) > 0 && !is.na(match(id, cpt(x)$id)))
    {
        results <- x@cpt[match(id, cpt(x)$id), ]
        if (length(x@rawResults) > 0)
        {
            rawResults <- as.list(x@rawResults[[match(id, cpt(x)$id)]])
        } else
        {
            rawResults <- list()
        }
        
    } else
    {
        results <- data.frame()
        rawResults <- list()
    }
    
    # load raw data
    matchedPeakIdx <- match(id, x@pt$id)
    
    if (is.na(matchedPeakIdx))
    {
        # try cpt instead
        matchedPeakIdx <- match(id, x@cpt$id)
        
        if (is.na(matchedPeakIdx))
        {
            stop(paste0("Could not find 'id' = ", id))
        }
    }
    
    matchedFileName <- MSnbase::fileNames(x@xd)[x@pt$sample[matchedPeakIdx]]
    
    if (!is.character(matchedFileName))
    {
        stop("Could not determine file path")
    }
    
    # get raw data
    raw <- new("cpc_raw", file_path = matchedFileName)
    raw <- parseMz(raw)
    
    # determine parameters for processing the chromatogram
    ## peak location
    cur_p <- binsearch_closest(x = raw@scantime,
                               val = x@pt$rt[id])
    
    ## filter width for smoothing
    # if (is.null(getParam(x@param, "max_sigma")))
    
    # check that max_sigma is calculated - otherwise, calculate it
    if (is.null(getParam(x@param, "max_sigma")))
    {
        x <- determineMaxSigma(x, raw@scantime, raw@scanrate)
        
        # setParam(x@param) <- 
        #     list(max_sigma = 
        #              as.numeric(quantile(x@pt$rtmax - x@pt$rtmin,
        #                                  probs = 0.75, na.rm = T)) * 
        #              raw@scanrate / 4)
    }
    
    # cur_rtmin <- binsearch_closest(x = raw@scantime,
    #                                val = x@pt$rtmin[id])
    # cur_rtmax <- binsearch_closest(x = raw@scantime,
    #                                val = x@pt$rtmax[id])
    # 
    # cur_s <- ifelse(as.numeric(cur_rtmax - cur_rtmin)*0.25 >
    #                     getParam(x@param, "max_sigma"),
    #                 as.numeric(getParam(x@param, "max_sigma")),
    #                 as.numeric(cur_rtmax - cur_rtmin)*0.25)
    
    ## filter width for smoothing
    cur_rtmin <- binsearch_closest(x = raw@scantime, 
                                   val = x@pt$rtmin[id])
    # cur_rtmax <- binsearch_closest(x = raw@scantime, 
    #                                val = x@pt$rtmax[id])
    
    cur_s <- (cur_p - cur_rtmin)/2
    
    if (is.na(cur_s) || 
        is.null(cur_s) || 
        cur_s > getParam(x@param, "max_sigma"))
    {
        cur_s <- floor(getParam(x@param, "max_sigma")+0.5)
    } else if (cur_s < 3)
    {
        cur_s <- 3
    }
    
    ## mz range for extracting ion traces
    
    # create a cpc_chrom object for processing
    chrom <- 
        new("cpc_chrom",
            id = as.integer(id),
            st = raw@scantime,
            param = cpcChromParam(mz = as.numeric(x@pt$mz[id]),
                                  p = cur_p,
                                  s = cur_s,
                                  mz_range = c(as.numeric(x@pt$mz[id]) -
                                                   as.numeric(x@pt$mz[id])/1e6*getParam(x@param, "ppm"),
                                               as.numeric(x@pt$mz[id]) +
                                                   as.numeric(x@pt$mz[id])/1e6*getParam(x@param, "ppm"))),
            mzMeta = list(runInfo = raw@runInfo,
                          header = raw@header),
            results = results,
            rawProcResults = rawResults)
    
    # new param methodology with a cpcParam object
    setParam(chrom@param) <- x@param
    
    setProcData(chrom) <- list(pd = x@pt[id, ])
    
    # save MS info
    setParam(chrom@param) <- list(nscan = raw@runInfo$scanCount)
    
    # set XIC in chrom
    setXIC(chrom) <- getXIC(raw, mzrange = getParam(chrom@param, "mz_range"))
    
    # smooth trace
    chrom <- smoothChromatogram(chrom)
    
    # set plotrange
    if (is.na(match("plotrange", names(x@procData))))
    {
        setProcData(chrom) <- 
            list(plotrange = 
                     c(max(1, floor(getParam(chrom@param, "p") - 
                                        20*getParam(chrom@param, "s"))),
                       min(getParam(chrom@param, "nscan"), 
                           floor(getParam(chrom@param, "p") + 
                                     20*getParam(chrom@param, "s")))))
    }
    
    return(chrom)
})


#### Method: processPeaks ####

#' @title Main method for processing the peak table in a cpc object.
#' 
#' @description 
#' 
#' This is the main method run on a \code{cpc} object to characterize the peaks detected by XCMS.
#' Prior to running this method the peak table must be parsed using the \code{parsePeaklist} method.
#' 
#' @param x A \code{cpc} object
#' 
#' @return A \code{cpc} object
#' 
#' @export
#' @docType methods
#' @rdname cpc-methods
setMethod("processPeaks", signature("cpc"), function(x)
{
    # initialize empty cpt data.frame
    cpt(x) <- data.frame()
    
    # check to ensure that all data needed is present
    if (nrow(x@pt) < 1)
    {
        x <- parsePeaklist(x)
        
        if (nrow(x@pt) < 1) stop("No peak information in object")
    }
    
    # loop over files
    # (i <- getParam(x@param, "sel_files")[1])
    for (i in getParam(x@param, "sel_files"))
    {
        # start timer
        full_timer <- Sys.time()
        
        i_idx <- getParam(x@param, "sel_peaks")[
            which(x@pt$sample[getParam(x@param, "sel_peaks")] == i)
        ]
        i_npeaks <- length(i_idx)
        
        # output
        message(paste0("Processing file: ", x@procData$file_paths[i], "\n"))
        
        # load raw data
        raw <- new("cpc_raw", file_path = x@procData$file_paths[i])
        raw <- parseMz(raw)
        
        # output
        message(paste0("Found ", i_npeaks, " peak(s).\n"))
        
        i_counter = 1
        
        if (getParam(x@param, "verbose_output"))
        {
            message(paste("[debug] starting peak processing...\n"))
        } else
        {
            message("% complete: ")
            i_progress <- floor(floor(i_counter/i_npeaks*100)/10)*10
            message(paste0(i_progress, " ", sep=""))
            i_lastprogress <- i_progress
        }
        
        # calculate filter window max threshold for smoother
        # first checks so that it hasnt been calculated already - assuming that
        # the files have the same scanrate it shouldnt change but if it does
        # it is good to recalculate it for each file
        if (is.null(getParam(x@param, "max_sigma")) ||
            !is.numeric(getParam(x@param, "max_sigma")))
        {
            # setParam(x@param) <- 
            #     list(max_sigma = 
            #              as.numeric(quantile(x@pt$rtmax - x@pt$rtmin,
            #                                  probs = 0.75, na.rm = T)) * 
            #              raw@scanrate / 4)
            
            x <- determineMaxSigma(x, raw@scantime, raw@scanrate)
            
            if (is.na(getParam(x@param, "max_sigma")) ||
                is.null(getParam(x@param, "max_sigma")) ||
                !is.numeric(getParam(x@param, "max_sigma")))
            {
                stop(paste0("Could not determine filter width threshold ", 
                            "the smoother..."))
            }
        }
        
        # (pd <- x@pt[i_idx[25509], ]) # - tryptophan 210518
        # (pd <- x@pt[i_idx[1], ])
        res <- do.call("list", apply(x@pt[i_idx, ], 1, FUN = function(pd)
        {
            # start timer
            proc_timer <- Sys.time()
            
            # output
            if (getParam(x@param, "verbose_output")) 
            {
                message(paste0("[debug] peak: ", pd["id"], "; ", sep = ""))
            } else
            {
                # <<- is dangerous... should probably find another
                # way to deal with the progress output
                i_progress <<- floor(floor(i_counter/i_npeaks*100)/10)*10
                i_counter <<- i_counter + 1
                
                if (i_progress > i_lastprogress)
                {
                    message(paste0(i_progress, " ", sep=""))
                    i_lastprogress <<- i_progress
                }
            }
            
            # check if xcms data is missing
            if (!is.numeric(unlist(pd[c("mz", "rt", "rtmin", "rtmax")])))
            {
                if (getParam(x@param, "verbose_output"))
                {
                    message("missing xcms data")
                }
                
                setResults(chrom) <- list(note = "xcms_missing", file = i)
            } else # if xcms data exist
            {
                # determine params for processing
                ## peak location
                cur_p <- binsearch_closest(x = raw@scantime, val = pd["rt"])
                
                ## filter width for smoothing
                # cur_rtmax <- binsearch_closest(x = raw@scantime, 
                #                                val = pd["rtmax"])
                cur_rtmin <- binsearch_closest(x = raw@scantime, 
                                               val = pd["rtmin"])
                
                cur_s <- (cur_p - cur_rtmin)/2
                
                if (is.na(cur_s) || 
                    is.null(cur_s) || 
                    cur_s > getParam(x@param, "max_sigma"))
                {
                    cur_s <- floor(getParam(x@param, "max_sigma")+0.5)
                } else if (cur_s < 3)
                {
                    cur_s <- 3
                }
                
                ## mz range for extracting ion traces
                cur_mzrange <- 
                    c(as.numeric(pd["mz"]) -
                          as.numeric(pd["mz"])/1e6*getParam(x@param, "ppm"),
                      as.numeric(pd["mz"]) +
                          as.numeric(pd["mz"])/1e6*getParam(x@param, "ppm"))
                
                # create chrom object
                chrom <- 
                    new("cpc_chrom",
                        id = as.integer(pd["id"]),
                        st = raw@scantime,
                        param = cpcChromParam(mz = as.numeric(pd["mz"]),
                                              p = cur_p,
                                              s = cur_s,
                                              mz_range = cur_mzrange))
                
                # chrom <- 
                #     new("cpc_chrom",
                #         id = as.integer(pd["id"]),
                #         param = cpcChromParam(mz = as.numeric(pd["mz"]),
                #                               p = cur_p,
                #                               s = cur_s,
                #                               mz_range = cur_mzrange),
                #         procParams = list(mz = as.numeric(pd["mz"]),
                #                           p = binsearch_closest(x = raw$scantime,
                #                                                 val = pd["rt"]),
                #                           s = ifelse((is.na(pd["sigma"]) || as.numeric(pd["sigma"]) >
                #                                           as.numeric(getProcData(x, "max_sigma")[i])),
                #                                      as.numeric(getProcData(x, "max_sigma")[i]),
                #                                      as.numeric(pd["sigma"])),
                #                           mz_range = c(as.numeric(pd["mz"]) -
                #                                            as.numeric(pd["mz"])/1e6*getParam(x@param, "ppm"),
                #                                        as.numeric(pd["mz"]) +
                #                                            as.numeric(pd["mz"])/1e6*getParam(x@param, "ppm"))))
                
                # old param methodlogy with a list() holding the params
                # setProcParams(chrom) <- getProcParams(x)
                
                # new param methodology with a cpcParam object holding the params
                # for better control
                # add params from cpc object
                setParam(chrom@param) <- x@param
                
                setProcData(chrom) <- list(pd = pd)
                
                # get XIC
                setXIC(chrom) <- getXIC(raw, mzrange = getParam(chrom@param, 
                                                                "mz_range"))
                
                # process chromatogram
                chrom <- processChromatogram(chrom)
                
                setResults(chrom) <- list(file = i)
                
            }
            
            setResults(chrom) <- list(exectime = 
                                          as.numeric(difftime(Sys.time(), 
                                                              proc_timer,
                                                              units = "secs")))
            
            # end output line
            if (getParam(x@param, "verbose_output")) message("\n")
            
            # return the data.frame row for the current peak
            # row.names are added from the original peak table for matching
            # purposes later on
            # return(list(res = chrom@rawProcResults,
            #             df = data.frame(id = chrom@id, getResults(chrom),
            #                        row.names = row.names(pd)[1])))
            
            return(list(data.frame(id = chrom@id, getResults(chrom),
                                   row.names = row.names(pd)[1]),
                        data.frame(chrom@rawProcResults)))
        }))
        
        df <- do.call("rbind", lapply(res, function(x) return(x[[1]])))
        
        if (getParam(x@param, "save_all"))
        {
            rawResults <- do.call("list", 
                                  lapply(res, function(x) return(x[[2]])))
        }
        
        # end output
        if (getParam(x@param, "verbose_output"))
        {
            message("[debug] Done!\n\n")
        } else
        {
            message(paste("Done!\n", sep  = ""))
        }
        
        if (getParam(x@param, "save_all"))
        {
            x@rawResults <- c(x@rawResults, rawResults)
        }
        
        cpt(x) <- rbind(cpt(x), df)
        
        # output timer
        output_formatted_time(full_timer, Sys.time())
    }
    
    # return object
    return(x)
})

#### Method: checkPeaksAgainstCriteria ####
#' @title Method that check each peak in the XCMS object against the criteria 
#' specified by the user in the *cpcProcParam* object
#' 
#' @description 
#' 
#' This method i called by the *filterPeaks* method and checks the peak
#' characteristics of each detected peak against the criteria specified by the
#' user in the *cpcProcParam* object.
#' 
#' @param x A \code{cpc} object
#' 
#' @return A \code{cpc} object
#' 
#' @docType methods
setMethod("checkPeaksAgainstCriteria", signature("cpc"), function(x) {
    # check that the peak processing has been performed
    if (!hasCharacterizedPeakTable(x))
    {
        stop("Please run peak characterization first!")
    }
    
    for (i in 1:nrow(x@cpt))
    {
        # TODO: Parallell testing here instead and save result of each test.
        # TODO: Add tailing factor filter.
        if (x@cpt$note[i] == "detected")
        {
            if (x@cpt$sn[i] < getParam(x@param, "min_sn"))
            {
                x@cpt$note[i] <- "low_sn"
            } else if (x@cpt$tpkb[i] - x@cpt$fpkb[i] + 1 < 
                       getParam(x@param, "min_pts"))
            {
                x@cpt$note[i] <- "too_narrow"
            } else if (x@cpt$area[i] < getParam(x@param, "min_intensity"))
            {
                x@cpt$note[i] <- "too_small"
            }
        }
    }
    
    # # mark peaks with too low intensity
    # x@cpt$note[which(x@cpt$area < 
    #                        getParam(x@param, "min_intensity"))] <- "too_small"
    # 
    # # mark peaks that are too narrow
    # x@cpt$note[which(x@cpt$tpkb - 
    #                        x@cpt$fpkb + 1 < 
    #                        getParam(x@param, "min_pts"))] <- "too_narrow"
    # 
    # # mark peaks with too low signal-to-noise
    # x@cpt$note[which(x@cpt$sn < getParam(x@param, "min_sn"))] <- "low_sn"
    
    # return object
    return(x)
})


#### Method: setFilterOutcomes ####
#' @docType methods
setMethod("setFilterOutcomes<-", signature("cpc"), function(x, value) {
    # ensure that value is a data frame with the same number of rows as cpt
    if (class(value) != "data.frame")
    {
        stop("Incorrect format on filter outcomes. It should be a data.frame.")
    } else if (nrow(value) != nrow(x@cpt))
    {
        stop(paste0("Incorrect number of rows in outcomes. It should be a a ",
                    "data.frame with the same number of rows as the cpt ",
                    "data.frame..."))
    }
    
    x@outcomes <- value
    
    return(x)
})

#### Method: determineFilterOutcomes ####
#' @docType methods
setMethod("determineFilterOutcomes", signature("cpc"), function(x) {
    # check that the peak processing has been performed
    if (!hasCharacterizedPeakTable(x))
    {
        stop("Please run peak characterization first!")
    }
    
    res <- data.frame(
        # checks if a peak is detected by the algorithm
        detected = x@cpt$note != "not_detected",
        # checks if a peak has a higher than specified signal-to-noise ratio
        sn = if(!(is.null(getParam(x@param, "min_sn")))) {
            ((x@cpt$sn >= getParam(x@param, "min_sn") & 
                  x@cpt$note != "not_detected"))
        } else {
            rep(TRUE, nrow(x@cpt))
        },
        # checks if a peak is as wide or wider than the specified minimum width
        width = if(!(is.null(getParam(x@param, "min_pts")))) {
            (x@cpt$tpkb - x@cpt$fpkb + 1 >= 
                 getParam(x@param, "min_pts") & 
                 x@cpt$note != "not_detected")
        } else {
            rep(TRUE, nrow(x@cpt))
        },
        fwhm = if(!(is.null(getParam(x@param, "min_fwhm")))) {
            ((x@cpt$fwhm >= getParam(x@param, "min_fwhm") & 
                  x@cpt$note != "not_detected"))
        } else {
            rep(TRUE, nrow(x@cpt))
        }, 
        tf = if(!(is.null(getParam(x@param, "interval_tf")))) {
            (x@cpt$tf >= 
                 getParam(x@param, "interval_tf")[1] & 
                 x@cpt$tf <= 
                 getParam(x@param, "interval_tf")[2] & 
                 x@cpt$note != "not_detected")
        } else {
            rep(TRUE, nrow(x@cpt))
        },
        # checks if a peak is as intense or more intense than specified minimum
        # intensity
        intensity = if(!(is.null(getParam(x@param, "min_intensity")))) {
            (x@cpt$area >= getParam(x@param, "min_intensity") & 
                 x@cpt$note != "not_detected")
        } else {
            rep(TRUE, nrow(x@cpt))
        }
    )
    
    # set all filter outcomes to TRUE for those not detected
    res <- as.data.frame(t(apply(res, 1, FUN = function(x){
        if (!x[1])
        {
            x[2:ncol(res)] <- rep(TRUE, ncol(res)-1)
        }
        
        return(x)
    })))
    
    # TODO: Add custom filters
    
    row.names(res) <- row.names(x@cpt)
    
    setFilterOutcomes(x) <- res
    
    return(x)
})

#### Method: getFilterOutcomes ####
#' @export
#' @docType methods
#' @rdname cpc-methods
setMethod("getFilterOutcomes", signature("cpc"), function(x) {
    if (nrow(x@outcomes) <= 0)
    {
        warning(paste0("No filter outcomes appears to be present. ",
                       "Run determineFilterOutcomes()."))
    }
    
    return(x@outcomes)
})


#### Method: filterPeaks ####

#' @title Method that creates a filtered \code{XCMSnExp} object based on filter criteria
#' 
#' @description 
#' 
#' This method is run after \code{processPeaks} in order to remove the peaks 
#' that do not fulfill the criteria and creates a filtered \code{XCMSnExp} 
#' object and stores it in the \code{xdFilt} slot.
#' 
#' @param x A \code{cpc} object
#' 
#' @return A \code{cpc} object
#' 
#' @export
#' @docType methods
#' @rdname cpc-methods
setMethod("filterPeaks", signature("cpc"), function(x)
{
    # check that peak characterization has been performed on the object
    # and that peak characteristics data exist in the object
    if (!hasCharacterizedPeakTable(x))
        stop(paste("Please run processPeaks() before trying to filter peaks."))
    
    # get the original chromPeaks data frame
    orgcp <- xcms::chromPeaks(x@xd)
    
    # check that the cpt contains the same amount of peaks as original 
    # chromPeaks data frame
    if (nrow(x@cpt) != 
        length(which(orgcp[, "sample"] %in% 
                     getParam(x@param, "sel_files"))))
    {
        stop(paste0("The number of peaks inte the characterized peaklist does ",
                    "not match the number of peaks in the XCMSnExp object."))
    }
    
    # TODO: Invoke a refineChromPeaks() method that is available in the github but does
    # not seem to be available in my current package version. For compatibility reasons
    # I will implement my own version of this.
    
    # check that there are chromatographic peaks in the xcms object
    if (!xcms::hasChromPeaks(x@xd)) {
        stop("No chromatographic peaks present in XCMS object.")
        
    }
    
    # copy the xcms object
    # x@xdFilt <- x@xd
    
    # drop feature definitions if present
    if (xcms::hasFeatures(x@xdFilt)) {
        message(paste0("Removing existing feature definitions from XCMS object. ",
                       "Run retention alignment and peak filling again after ",
                       "filtering."))
        
        x@xdFilt <- xcms::dropFeatureDefinitions(x@xdFilt)
        
    }
    
    # get chromPeaks from the filtered object
    # ncp <- xcms::chromPeaks(x@xdFilt)
    
    # check all peaks against the specified criteria in the params object
    # x <- checkPeaksAgainstCriteria(x)
    x <- determineFilterOutcomes(x)
    
    # create a vector of peaks to remove based on params
    # keep will be which peaks in the cpt slot that will be kept (NOT which
    # peaks in the XCMS object that will be kept!)
    # keep <- peaksToKeep(x)
    cpt <- cpt(x)
    keep <- which(apply(getFilterOutcomes(x), 1, FUN=function(z) all(z)))
    
    # if the XCMSnExp has filled peaks they will be removed when running
    # xcms::dropFeatureDefinitions() and so if this is the case, then I will 
    # need to match the idx in keep with those remaining.
    if (xcms::hasFilledChromPeaks(x@xd))
    {
        keep_names <- rownames(xcms::chromPeaks(x@xd))[x@cpt$id[keep]]
        
        keep_names <- keep_names[which(!is.na(match(keep_names, rownames(orgcp))))]
        
        keep <- match(keep_names, rownames(orgcp))
        
    }
    
    # remove peaks from xcms object
    # xcms::chromPeaks(x@xdFilt) <- ncp[keep, ]
    x@xdFilt <- xcms::filterChromPeaks(x@xd, keep = keep)
    
    # output
    message(paste0("Keeping ", length(keep), " of ", nrow(cpt), " (",
                   round(length(keep)/nrow(cpt)*100, 2),
                   "%) peaks.\n"))
    
    # return cpc object
    return(x)
})

#### Method: getOriginalXCMS ####
#' @title Getter for the original unfiltered XCMS object
#' 
#' @description 
#' 
#' This method returns the unfiltered original \code{XCMSnExp} object.
#' 
#' @param x A \code{cpc} object
#' 
#' @return A \code{XCMSnExp} object
#' 
#' @export
#' @docType methods
#' @rdname cpc-methods
setMethod("getOriginalXCMS", signature("cpc"), function(x) {
    return(x@xd)
})


#### Method: getFilteredXCMS ####
#' @title Getter for the filtered XCMS object
#' 
#' @description 
#' 
#' This method returns the filtered XCMS object after running the CPC
#' algorithm.
#' 
#' @param x A \code{cpc} object
#' 
#' @return A \code{XCMSnExp} object
#' 
#' @export
#' @docType methods
#' @rdname cpc-methods
setMethod("getFilteredXCMS", signature("cpc"), function(x) {
    return(x@xdFilt)
})


#### Method: getRemovedPeaks ####
#' @title Getter for the removed peaks after CPC processing
#' 
#' @description 
#' 
#' This method will return a vector containing the peak IDs of the peaks that
#' were removed during the CPC filtering.
#' 
#' @param x A \code{cpc} object
#' 
#' @return A vector of peak IDs
#' 
#' @export
#' @docType methods
#' @rdname cpc-methods
setMethod("getRemovedPeaks", signature("cpc"), function(x) {
    allPeaks <- row.names(xcms::chromPeaks(x@xd))
    retainedPeaks <- row.names(xcms::chromPeaks(x@xdFilt))
    
    return(allPeaks[which(!(allPeaks %in% retainedPeaks))])
    
})

#### Method: getRetainedPeaks ####
#' @title Getter for the retained peaks after CPC processing
#' 
#' @description 
#' 
#' This method will return a vector containing the peak IDs of the peaks that
#' were *not* removed during the CPC filtering.
#' 
#' @param x A \code{cpc} object
#' 
#' @return A vector of peak IDs
#' 
#' @export
#' @docType methods
#' @rdname cpc-methods
setMethod("getRetainedPeaks", signature("cpc"), function(x) {
    return(row.names(xcms::chromPeaks(x@xdFilt)))
    
})


#### Method: show ####
#' @title Show method for the cpc object
#' 
#' @description 
#' 
#' Outputs a summary of the cpc object.
#' 
#' @param object A \code{cpc} object
#' 
#' @export
#' @docType methods
#' @rdname cpc-methods
setMethod("show", signature("cpc"), function(object)
{
    cat("S4 object of type 'cpc'.\n")
    attributes(object)
    
})

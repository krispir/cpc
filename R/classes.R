
#### type unions ####

setClassUnion("numericOrNULL", members=c("numeric", "NULL"))
setClassUnion("integerOrNULL", members=c("integer", "NULL"))

#### Class: cpcProcParam ####

setClass("cpcProcParam",
         representation(ppm = "numericOrNULL", 
                        min_pts = "numericOrNULL",
                        min_inf_width = "numericOrNULL",
                        min_sn = "numericOrNULL",
                        min_frac = "numericOrNULL",
                        min_intensity = "numericOrNULL",
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
                        plot = "logical"),
         prototype(ppm = 50.0, 
                   min_pts = 7L,
                   min_inf_width = 3.0,
                   min_sn = 10.0,
                   min_frac = 0.5,
                   min_intensity = 1000L,
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
                   plot = FALSE))

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

setMethod("setParam<-", signature("cpcParam"), function(x, value) 
{
    if (class(value) == "list")
    {
        namesInArgument <- names(value)
        valueType <- "list"
        
        # check that the list is named
        if (length(namesInArgument) < 1) stop("Process params must be a named list.")
        
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

# setMethod("getProcParam", signature("cpcParam"), function(x, param)
# {
#     # find which params exist
#     matchedNames <- which(!is.na(match(param, slotNames(x))))
#     
#     # check if there are unmatched params and give a message
#     if (length(matchedNames) < length(param))
#     {
#         message(paste0("Missing slots in object: ", 
#                        paste0(param[-matchedNames], collapse = ", ")))
#     }
#     
#     # if length > 1 -> return matched params as a list
#     paramList <- vector(mode = "list", length = length(matchedNames))
#     names(paramList) <- param[matchedNames]
#     
#     for (i in 1:length(matchedNames))
#     {
#         paramList[param[matchedNames[i]]] <- slot(x, param[matchedNames[i]])
#     }
#     
#     
# })

#### Method: getParam ####
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
#' @export
setMethod("show", signature("cpcParam"), function(x)
{
    paste0("'", class(x), "' object:\n")
    sNames <- slotNames(x)
    for (i in 1:length(sNames))
    {
        paste0(sNames[i], ": ", slot(x, sNames[i]), "\n")
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
#' @slot procParams \code{list} containing processing parameters.
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
             param = "cpcParam",
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
             param = cpcChromParam(),
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
             results = list(apex = -1,
                            bl_front_bound = -1,
                            bl_tail_bound = -1,
                            front_bound = -1,
                            tail_bound = -1,
                            h1_front_bound = -1,
                            h1_tail_bound = -1,
                            h5_front_bound = -1,
                            h5_tail_bound = -1,
                            h50_front_bound = -1,
                            h50_tail_bound = -1,
                            fwhm = -1,
                            wb = -1,
                            a = -1,
                            b = -1,
                            tf = -1,
                            height = -1,
                            area = -1,
                            sn = -1,
                            code = "",
                            note = "",
                            exectime = -1,
                            file = -1)
         ))

#### Method: getProcParams ####

#' @title Getter method for the procParams slot
#' 
#' @param x A \code{cpc_chrom} object.
#' @param value A named \code{list} of parameters. If \code{NULL}, returns a named \code{list} of all process parameters.
#' 
#' @return A named \code{list} of process parameters.
#' 
#' @export
#' @docType methods
#' @rdname cpc_chrom-methods
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
#' @export
#' @docType methods
#' @rdname cpc_chrom-methods
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
#' @rdname cpc_chrom-methods
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
#' @rdname cpc_chrom-methods
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
#' @export
#' @docType methods
#' @rdname cpc_chrom-methods
setMethod("getMzRange", signature("cpc_chrom"), function(x) getParam(x@param, "mz_range"))


#### Method: getResults ####

#' @title Getter method for the results slot
#' 
#' @param x A \code{cpc_chrom} object.
#' 
#' @return A \code{data.frame} with the results from the peak processing.
#' 
#' @export
#' @docType methods
#' @rdname cpc_chrom-methods
setMethod("getResults", signature("cpc_chrom"), function(x) x@results)


#### Method: setResults<- ####

#' @title Setter method for the results slot
#' 
#' @param x A \code{cpc_chrom} object.
#' 
#' @return A \code{cpc_chrom} object.
#' 
#' @export
#' @docType methods
#' @rdname cpc_chrom-methods
setMethod("setResults<-", signature("cpc_chrom"), function(x, value)
{
    if (class(value) != "list") stop("Result vals must be a named list.")
    
    resNames <- names(x@results)
    valNames <- names(value)
    
    if (length(valNames) < 1) stop("Result vals must be a named list.")
    
    matchedNames <- which(!is.na(match(valNames, resNames)))
    
    x@results[valNames[matchedNames]] <- value[matchedNames]
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
#' @export
#' @docType methods
#' @rdname cpc_chrom-methods
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
#' 
#' @return NULL
#' 
#' @export
#' @docType methods
#' @rdname cpc_chrom-methods
setMethod("plotPeak", signature("cpc_chrom"), function(x)
{
    if (x@results$apex > 0)
    {
        results = TRUE
    } else
    {
        results = FALSE
    }
    
    if (results)
    {
        ylim = c(0, max(x@xic[x@results$bl_front_bound:x@results$bl_tail_bound]))
    } else
    {
        ylim = c(0, max(x@xic[x@procData$plotrange[1]:x@procData$plotrange[2]]))
    }
    
    layout(mat = matrix(c(1,2), nrow = 2, ncol = 1, byrow = T))
    par_bk <- par()
    
    
    # d0 main plot 
    par(mar = c(1,4.1,1,1))
    plot(x@d0, type = "l", 
         xlim = x@procData$plotrange,
         ylim = ylim,
         ylab = "XIC",
         xlab = "",
         xaxt = "n")
    abline(v = getParam(x@param, "p"), col = "red", lty = "dashed")
    
    ## d0 points
    points(x@d0, pch = 20, cex = 0.9)
    lines(x@xic, col = "#00000075", lty = "dashed")
    
    ## d0 apex point
    if (results)
        points(x = x@results$apex, y = x@d0[x@results$apex], col = "red", pch = 20)
    
    ## d0 current vars
    char_ann <- paste0("id ", x@id, "\n",
                      "note ", x@results$note, "\n")
    
    ## results textbox
    if (results)
        char_ann <- paste0(char_ann,
                          "bl_bound ", paste(c(x@results$bl_front_bound,
                                               x@results$bl_tail_bound), collapse = "->"), "\n",
                          "peak_bound ", paste(c(x@results$front_bound,
                                                 x@results$tail_bound), collapse = "->"), "\n",
                          "code ", x@results$code, "\n",
                          "SN ", round(x@results$sn, 3), "\n",
                          "fwhm ", round(x@results$fwhm, 3), "\n",
                          "wb ", round(x@results$wb, 3), "\n",
                          "TF ", round(x@results$tf, 3), "\n")
    
    text(x = x@procData$plotrange[1], y = 0.95*ylim[2], 
         labels = char_ann, adj = c(0,1), cex = 0.75)
    
    ## metadata textbox
    text(x = x@procData$plotrange[2], y = 0.95*ylim[2],
         labels = paste0("m/z ", round(getParam(x@param, "mz_range")[1], 3), " : ", 
                         round(getParam(x@param, "mz_range")[2], 3)), adj = c(1,1), cex = 0.75)
    
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
    
    
    ## d0 peak box
    
    if (results)
    {
        # d0 baseline
        points(x = c(x@results$bl_front_bound,
                     x@results$bl_tail_bound),
               y = x@d0[c(x@results$bl_front_bound,
                          x@results$bl_tail_bound)],
               col = "red", pch = 20, cex = 1.1)
        lines(x = c(x@results$bl_front_bound,
                    x@results$bl_tail_bound),
              y = x@d0[c(x@results$bl_front_bound,
                         x@results$bl_tail_bound)],
              col = "red", lty = "dashed")
        
        ## bottom
        tmp_x <- unlist(x@results[c("front_bound", "tail_bound")])
        tmp_y <- c(interpolate_y(x = unlist(x@results[c("bl_front_bound", "bl_tail_bound")]),
                                 y = x@d0[unlist(x@results[c("bl_front_bound", "bl_tail_bound")])],
                                 xval = unlist(x@results$front_bound)),
                   interpolate_y(x = unlist(x@results[c("bl_front_bound", "bl_tail_bound")]),
                                 y = x@d0[unlist(x@results[c("bl_front_bound", "bl_tail_bound")])],
                                 xval = unlist(x@results$tail_bound)))
        
        lines(x = tmp_x, y = tmp_y, col = "red")
        
        ## left
        tmp_x <- rep(x@results$front_bound, 2)
        tmp_y <- c(interpolate_y(x = unlist(x@results[c("bl_front_bound", "bl_tail_bound")]),
                                 y = x@d0[unlist(x@results[c("bl_front_bound", "bl_tail_bound")])],
                                 xval = unlist(x@results$front_bound)),
                   max(x@d0[floor(x@results$front_bound):floor(x@results$tail_bound)]))
        
        lines(x = tmp_x, y = tmp_y, col = "red")
        
        ## top
        tmp_x <- unlist(x@results[c("front_bound", "tail_bound")])
        tmp_y <- rep(max(x@d0[floor(x@results$front_bound):floor(x@results$tail_bound)]), 2)
        
        lines(x = tmp_x, y = tmp_y, col = "red")
        
        ## right
        tmp_x <- rep(x@results$tail_bound, 2)
        tmp_y <- c(interpolate_y(x = unlist(x@results[c("bl_front_bound", "bl_tail_bound")]),
                                 y = x@d0[unlist(x@results[c("bl_front_bound", "bl_tail_bound")])],
                                 xval = unlist(x@results$tail_bound)),
                   max(x@d0[floor(x@results$front_bound):floor(x@results$tail_bound)]))
        
        lines(x = tmp_x, y = tmp_y, col = "red")
    }
    
    # d2 main plot
    par(mar = c(5.1,4.1,0,1))
    plot(x@d2, type = "l", col = "#000000", 
         xlim = x@procData$plotrange,
         ylim = c(min(x@d2[x@procData$plotrange[1]:x@procData$plotrange[2]]),
                  max(x@d2[x@procData$plotrange[1]:x@procData$plotrange[2]])),
         ylab = "2nd derivative",
         xlab = "scan")
    points(x@d2, pch = 20, cex = 0.9)
    # d2 peak bounds
    if (results)
        points(x = unlist(x@results[c("front_bound", "tail_bound")]), 
               y = x@d2[unlist(x@results[c("front_bound", "tail_bound")])],
           col = "red", pch = 20)
    # d2 0 line
    abline(h = 0, col = "red")
    
    layout(mat = matrix(c(1), nrow = 1, ncol = 1))
    par(mar = par_bk$mar)
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
#' @export
#' @docType methods
#' @rdname cpc_chrom-methods
setMethod("calculatePeakCharacteristics", signature("cpc_chrom"), function(x)
{
    # peak apex
    setResults(x) <- list(apex = x@procResults$adj_apex)
    
    # baseline bounds
    setResults(x) <- list(bl_front_bound = x@procResults$bl_bounds[1],
                          bl_tail_bound = x@procResults$bl_bounds[2])
    
    # peak bounds
    setResults(x) <- list(front_bound = x@procResults$peak_bounds[1],
                          tail_bound = x@procResults$peak_bounds[2])
    
    # peak height
    setResults(x) <- list(height = x@d0[x@results$apex] - 
                              interpolate_y(x = x@procResults$bl_bounds, 
                                            y = x@d0[x@procResults$bl_bounds], 
                                            xval = x@results$apex))
    
    # height bounds
    ## 1% peak height
    tmp <- find_height_bounds(y = x@d0, apex = x@results$apex,
                              bl_bounds = x@procResults$bl_bounds,
                              peak_bounds = x@procResults$peak_bounds,
                              frac = 0.01, id = x@id,
                              debug = getParam(x@param, "verbose_output"), 
                              plot = getParam(x@param, "plot"))
    setResults(x) <- list(h1_front_bound = tmp[1],
                          h1_tail_bound = tmp[2])
    
    ## 5% peak height
    tmp <- find_height_bounds(y = x@d0, apex = x@results$apex,
                              bl_bounds = x@procResults$bl_bounds,
                              peak_bounds = x@procResults$peak_bounds,
                              frac = 0.05, id = x@id,
                              debug = getParam(x@param, "verbose_output"), 
                              plot = getParam(x@param, "plot"))
    setResults(x) <- list(h5_front_bound = tmp[1],
                          h5_tail_bound = tmp[2])
    
    ## 50% peak height
    tmp <- find_height_bounds(y = x@d0, apex = x@results$apex,
                              bl_bounds = x@procResults$bl_bounds,
                              peak_bounds = x@procResults$peak_bounds,
                              frac = 0.5, id = x@id,
                              debug = getParam(x@param, "verbose_output"), 
                              plot = getParam(x@param, "plot"))
    setResults(x) <- list(h50_front_bound = tmp[1],
                          h50_tail_bound = tmp[2])
    
    # base width
    setResults(x) <- list(wb = x@results$h5_tail_bound - x@results$h5_front_bound)
    
    # fwhm
    setResults(x) <- list(fwhm = x@results$h50_tail_bound - x@results$h50_front_bound)
    
    # peak integral
    integral_x <- floor(x@results$h1_front_bound):floor(x@results$h1_tail_bound)
    integral_y <- x@d0[floor(x@results$h1_front_bound):floor(x@results$h1_tail_bound)]
    
    if (floor(x@results$h1_front_bound) < x@results$h1_front_bound)
    {
        integral_x <- c(x@results$h1_front_bound, integral_x)
        integral_y <- c(interpolate_y(x = c(floor(x@results$h1_front_bound),
                                            floor(x@results$h1_front_bound)+1),
                                      y = x@d0[c(floor(x@results$h1_front_bound),
                                                 floor(x@results$h1_front_bound)+1)],
                                      xval = x@results$h1_front_bound),
                        integral_y)
    }
    
    if (floor(x@results$h1_tail_bound) < x@results$h1_tail_bound)
    {
        integral_x <- c(integral_x, x@results$h1_tail_bound)
        integral_y <- c(integral_y,
                        interpolate_y(x = c(floor(x@results$h1_tail_bound),
                                            floor(x@results$h1_tail_bound)+1),
                                      y = x@d0[c(floor(x@results$h1_tail_bound),
                                                 floor(x@results$h1_tail_bound)+1)],
                                      xval = x@results$h1_tail_bound))
    }
    
    integral_bl <- ((integral_x - x@results$bl_front_bound) * x@procResults$bl_slope) + 
        x@d0[x@results$bl_front_bound]
    
    setResults(x) <- list(area = peak_integral(x = integral_x, y = integral_y - integral_bl)) # ~5 µsecs for 25509
    
    # asymmetry
    setResults(x) <- list(a = x@results$apex - x@results$h50_front_bound,
                          b = x@results$h50_tail_bound - x@results$apex)
    
    setResults(x) <- list(tf = x@results$b / x@results$a)
    
    # signal-to-noise
    setResults(x) <- list(sn = ifelse(x@procData$xic_noise > 0,
                                      2*x@results$height / x@procData$xic_noise,
                                      Inf))
    
    # boundary codes
    setResults(x) <- list(code = paste(x@procResults$code, collapse = ""))
    
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
#' @export
#' @docType methods
#' @rdname cpc_chrom-methods
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


#### Method: fitEMG ####

#' @title Method EMG deconvolution of a chromatogram
#' 
#' @description 
#' 
#' Internal method for fitting a series of EMG functions to the chromatogram for
#' deconvolution of the peak shapes.
#' 
#' @param x A \code{cpc_chrom} object
#' 
#' @return A \code{cpc_chrom} object
#' 
#' @export
#' @docType methods
#' @rdname cpc_chrom-methods
setMethod("fitEMG", signature("cpc_chrom"), function(x)
{
    # select peaks to use in the fitting
    # to test: just use the VIP and the closest peaks before and after
    sel <- x@rawProcResults$current_peak+1
    
    if (x@rawProcResults$front_code[x@rawProcResults$current_peak+1] != 0)
    {
        sel <- c(x@rawProcResults$current_peak, sel)
    }
    
    if (x@rawProcResults$tail_code[x@rawProcResults$current_peak+1] != 0)
    {
        sel <- c(sel, x@rawProcResults$current_peak+2)
    }
    
    # calculate sigma for selected peaks
    sel_sigma <- (x@rawProcResults$tail_inf[sel] - x@rawProcResults$front_inf[sel])/2
    
    # determine points to fit: front bound of first peak -> tail bound of last peak
    sel_bound_full <- c(x@rawProcResults$front_peak_bound[sel[1]]+1,
                        x@rawProcResults$tail_peak_bound[sel[length(sel)]]+1)
    
    sel_bound_vip <- c(x@rawProcResults$front_peak_bound[x@rawProcResults$current_peak+1]+1,
                       x@rawProcResults$tail_peak_bound[x@rawProcResults$current_peak+1]+1)
    
    # use only front inf, 3p around apex and tail inf of each selected peak
    sel1_idx <- sort(unique(c(x@rawProcResults$front_inf[sel]+1,
                              x@rawProcResults$adj_apex[sel],
                              x@rawProcResults$adj_apex[sel]+1,
                              x@rawProcResults$adj_apex[sel]+2,
                              x@rawProcResults$tail_inf[sel]+1)))
    
    # fit peaks
    (bfgs_full_xic <- fitemgs_bfgs(signal = x@xic[sel_bound_full[1]:sel_bound_full[2]], 
                                  scantime = sel_bound_full[1]:sel_bound_full[2], 
                                  seed = list(mu = x@rawProcResults$adj_apex[sel],
                                              sigma = sel_sigma,
                                              lambda = rep(10, length(sel))),
                                  upper = list(mu = x@rawProcResults$adj_apex[sel]-5,
                                               sigma = sel_sigma*.75,
                                               lambda = rep(1, length(sel))*.9),
                                  lower = list(mu = x@rawProcResults$adj_apex[sel]+5,
                                               sigma = sel_sigma*1.25,
                                               lambda = rep(30, length(sel))*1.1),
                                  plot.fit = T))
    (bfgs_vip_xic <- fitemgs_bfgs(signal = x@xic[sel_bound_vip[1]:sel_bound_vip[2]], 
                                 scantime = sel_bound_vip[1]:sel_bound_vip[2], 
                                 seed = list(mu = x@rawProcResults$adj_apex[sel],
                                             sigma = sel_sigma,
                                             lambda = rep(10, length(sel))),
                                 lower = list(mu = x@rawProcResults$adj_apex[sel]-5,
                                              sigma = sel_sigma*.75,
                                              lambda = rep(1, length(sel))*.9),
                                 upper = list(mu = x@rawProcResults$adj_apex[sel]+5,
                                              sigma = sel_sigma*1.25,
                                              lambda = rep(30, length(sel))*1.1),
                                 plot.fit = T))
    (bfgs_sel1_xic <- fitemgs_bfgs(signal = x@xic[sel1_idx], 
                                  scantime = sel1_idx, 
                                  seed = list(mu = x@rawProcResults$adj_apex[sel],
                                              sigma = sel_sigma,
                                              lambda = rep(10, length(sel))),
                                  upper = list(mu = x@rawProcResults$adj_apex[sel]-5,
                                               sigma = sel_sigma*.75,
                                               lambda = rep(1, length(sel))*.9),
                                  lower = list(mu = x@rawProcResults$adj_apex[sel]+5,
                                               sigma = sel_sigma*1.25,
                                               lambda = rep(30, length(sel))*1.1),
                                  plot.fit = T))
    
    (bfgs2_full_xic <- fitemgs_bfgs_2(signal = x@xic[sel_bound_full[1]:sel_bound_full[2]], 
                                     scantime = sel_bound_full[1]:sel_bound_full[2], 
                                     seed = list(mu = x@rawProcResults$adj_apex[sel],
                                                 sigma = sel_sigma,
                                                 lambda = rep(10, length(sel))),
                                     plot.fit = T))
    (bfgs2_vip_xic <- fitemgs_bfgs_2(signal = x@xic[sel_bound_vip[1]:sel_bound_vip[2]], 
                                    scantime = sel_bound_vip[1]:sel_bound_vip[2], 
                                    seed = list(mu = x@rawProcResults$adj_apex[sel],
                                                sigma = sel_sigma,
                                                lambda = rep(10, length(sel))),
                                    plot.fit = T))
    (bfgs2_sel1_xic <- fitemgs_bfgs_2(signal = x@xic[sel1_idx], 
                                    scantime = sel1_idx, 
                                    seed = list(mu = x@rawProcResults$adj_apex[sel],
                                                sigma = sel_sigma,
                                                lambda = rep(10, length(sel))),
                                    plot.fit = T))
    
    (bfgs_full_d0 <- fitemgs_bfgs(signal = x@d0[sel_bound_full[1]:sel_bound_full[2]], 
                                   scantime = sel_bound_full[1]:sel_bound_full[2], 
                                   seed = list(mu = x@rawProcResults$adj_apex[sel],
                                               sigma = sel_sigma,
                                               lambda = rep(10, length(sel))),
                                   upper = list(mu = x@rawProcResults$adj_apex[sel]-5,
                                                sigma = sel_sigma*.75,
                                                lambda = rep(1, length(sel))*.9),
                                   lower = list(mu = x@rawProcResults$adj_apex[sel]+5,
                                                sigma = sel_sigma*1.25,
                                                lambda = rep(30, length(sel))*1.1),
                                   plot.fit = T))
    (bfgs_vip_d0 <- fitemgs_bfgs(signal = x@d0[sel_bound_vip[1]:sel_bound_vip[2]], 
                                  scantime = sel_bound_vip[1]:sel_bound_vip[2], 
                                  seed = list(mu = x@rawProcResults$adj_apex[sel],
                                              sigma = sel_sigma,
                                              lambda = rep(10, length(sel))),
                                  upper = list(mu = x@rawProcResults$adj_apex[sel]-5,
                                               sigma = sel_sigma*.75,
                                               lambda = rep(1, length(sel))*.9),
                                  lower = list(mu = x@rawProcResults$adj_apex[sel]+5,
                                               sigma = sel_sigma*1.25,
                                               lambda = rep(30, length(sel))*1.1),
                                  plot.fit = T))
    (bfgs_sel1_d0 <- fitemgs_bfgs(signal = x@d0[sel1_idx], 
                                   scantime = sel1_idx, 
                                   seed = list(mu = x@rawProcResults$adj_apex[sel],
                                               sigma = sel_sigma,
                                               lambda = rep(10, length(sel))),
                                   upper = list(mu = x@rawProcResults$adj_apex[sel]-5,
                                                sigma = sel_sigma*.75,
                                                lambda = rep(1, length(sel))*.9),
                                   lower = list(mu = x@rawProcResults$adj_apex[sel]+5,
                                                sigma = sel_sigma*1.25,
                                                lambda = rep(30, length(sel))*1.1),
                                   plot.fit = T))
    
    (bfgs2_full_d0 <- fitemgs_bfgs_2(signal = x@d0[sel_bound_full[1]:sel_bound_full[2]], 
                                      scantime = sel_bound_full[1]:sel_bound_full[2], 
                                      seed = list(mu = x@rawProcResults$adj_apex[sel],
                                                  sigma = sel_sigma,
                                                  lambda = rep(10, length(sel))),
                                      plot.fit = T))
    (bfgs2_vip_d0 <- fitemgs_bfgs_2(signal = x@d0[sel_bound_vip[1]:sel_bound_vip[2]], 
                                     scantime = sel_bound_vip[1]:sel_bound_vip[2], 
                                     seed = list(mu = x@rawProcResults$adj_apex[sel],
                                                 sigma = sel_sigma,
                                                 lambda = rep(10, length(sel))),
                                     plot.fit = T))
    (bfgs2_sel1_d0 <- fitemgs_bfgs_2(signal = x@d0[sel1_idx], 
                                     scantime = sel1_idx, 
                                     seed = list(mu = x@rawProcResults$adj_apex[sel],
                                                 sigma = sel_sigma,
                                                 lambda = rep(10, length(sel))),
                                     plot.fit = T))
    
    fitemgs(signal = x@xic[sel_bound_full[1]:sel_bound_full[2]], 
            scantime = sel_bound_full[1]:sel_bound_full[2], 
            seed = list(mu = x@rawProcResults$adj_apex[sel]+1,
                        sigma = sel_sigma,
                        lambda = rep(10, length(sel))),
            upper = list(mu = x@rawProcResults$adj_apex[sel]+10,
                         sigma = sel_sigma*2,
                         lambda = rep(1, length(sel))),
            lower = list(mu = x@rawProcResults$adj_apex[sel]-10,
                         sigma = sel_sigma*0.25,
                         lambda = rep(100, length(sel))),
            plot.fit = T)
    
    fitemgs(signal = x@d0[sel_bound_full[1]:sel_bound_full[2]], 
            scantime = sel_bound_full[1]:sel_bound_full[2], 
            seed = list(mu = x@rawProcResults$adj_apex[sel]+1,
                        sigma = sel_sigma,
                        lambda = rep(10, length(sel))),
            upper = list(mu = c(70, 90, 110),
                         sigma = c(5, 7, 6),
                         lambda = rep(1, length(sel))),
            lower = list(mu = c(50, 70, 90),
                         sigma = c(1.5, 4, 3),
                         lambda = rep(100, length(sel))),
            plot.fit = T)
    
    
    
    # return result
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
    # x <- chrom
    if(!require(signal, quietly = T)) stop("Package: signal required...")
    
    if (is.null(x@xic) || length(x@xic) < 1)
    {
        stop("No XIC supplied to processChromatogram().")
    } else
    {
        setParam(x@param) <- list(nscan = length(x@xic))
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
    
    ## check that XCMS data exist
    if (is.null(getParam(x@param, "p")))
    {
        if (getParam(x@param, "vebose_output"))
            cat(paste("[debug] idx =", x@id, "missing xcms data.\n"))
        
        setResults(x) <- list(id = x@id, note = "xcms_missing")
        
        return(x)
        
    } else if (getParam(x@param, "p") < 1)
    {
        if (getParam(x@param, "vebose_output"))
            cat(paste("[debug] idx =", x@id, "missing xcms data.\n"))
        
        setResults(x) <- list(id = x@id, note = "xcms_missing")
        
        return(x)
    }
    
    # determine plotrange
    setProcData(x) <- list(plotrange = c(max(1, floor(getParam(x@param, "p") - 
                                                          20*getParam(x@param, "s"))),
                                         min(x@procData$nscan, 
                                             floor(getParam(x@param, "p") + 
                                                       20*getParam(x@param, "s")))))
    
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
    
    # calculate noise
    setProcData(x) <- list(noise_sel = which(abs(x@d2) <= quantile(abs(x@d2), .95)))
    
    if (length(x@procData$noise_sel) < floor(getParam(x@param, "nscan")/2)) 
        x@procData$noise_sel <- 1:getParam(x@param, "nscan")
    
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
    # Rcpp::List process_chromatogram(vec_d &d0, 
    #                                 vec_d &d2,
    #                                 double apex_thresh = 0.0, 
    #                                 int w = 5, 
    #                                 int p = -1,
    #                                 double liftoff = 0.0, 
    #                                 double touchdown = 0.5, 
    #                                 int output = 0,
    #                                 int fit_emg = 1,
    #                                 int fit_only_vip = 1,
    #                                 int fit_hess = 0,
    #                                 double fit_rel_lim = 0.05,
    #                                 int pts_per_peak = 10,
    #                                 const double reltol = 1.0e-8,
    #                                 const double abstol = -1.0e35,
    #                                 const double alpha = 1.0,
    #                                 const double gamma = 2.1,
    #                                 const double rho = 0.75,
    #                                 const double sigma = 0.75,
    #                                 const int maxit = 2000,
    #                                 const int maxeval = 2000)
    x@rawProcResults <- process_chromatogram(d0 = x@d0, 
                                             d2 = x@d2, 
                                             apex_thresh = 0L,
                                             w = floor(getParam(x@param, "smooth_win")/2L), 
                                             p = getParam(x@param, "p")-1L,
                                             output = as.integer(x@param@verbose_output), 
                                             fit_emg = 1L, fit_only_vip = 1L)
    
    # check that the current peak was detected
    if (x@rawProcResults$current_peak < 0)
    {
        setResults(x) <- list(note = "not_detected")
        
        if (getParam(x@param, "plot")) plotPeak(x)
        
        return(x)
    }
    
    # record results from processing
    x@procResults <- list(
        adj_apex = x@rawProcResults$adj_apex[x@rawProcResults$current_peak+1]+1,
        bl_bounds = c(x@rawProcResults$front_baseline_bound[x@rawProcResults$current_peak+1]+1,
                      x@rawProcResults$tail_baseline_bound[x@rawProcResults$current_peak+1]+1),
        bl_slope = (x@d0[(x@rawProcResults$tail_baseline_bound[x@rawProcResults$current_peak+1]+1)] - 
                        x@d0[(x@rawProcResults$front_baseline_bound[x@rawProcResults$current_peak+1]+1)]) / 
            ((x@rawProcResults$tail_baseline_bound[x@rawProcResults$current_peak+1]+1) - 
                 (x@rawProcResults$front_baseline_bound[x@rawProcResults$current_peak+1]+1)),
        peak_bounds = c(x@rawProcResults$front_peak_bound[x@rawProcResults$current_peak+1]+1,
                        x@rawProcResults$tail_peak_bound[x@rawProcResults$current_peak+1]+1),
        code = c(switch(x@rawProcResults$front_code[x@rawProcResults$current_peak+1]+1, 
                        "B", "V", "S", "R"),
                 switch(x@rawProcResults$tail_code[x@rawProcResults$current_peak+1]+1, 
                        "B", "V", "S", "R")),
        inf = c(x@rawProcResults$front_inf[x@rawProcResults$current_peak+1]+1,
                x@rawProcResults$tail_inf[x@rawProcResults$current_peak+1]+1),
        emg_mu = x@rawProcResults$emg_mu[x@rawProcResults$current_peak+1]+1,
        emg_sigma = x@rawProcResults$emg_sigma[x@rawProcResults$current_peak+1],
        emg_lambda = x@rawProcResults$emg_lambda[x@rawProcResults$current_peak+1],
        emg_area = x@rawProcResults$emg_area[x@rawProcResults$current_peak+1],
        emg_conv = x@rawProcResults$emg_conv[x@rawProcResults$current_peak+1]
    )
    
    # check peak bounds and apex
    if (any(x@procResults$bl_bounds < 1) ||
        any(x@procResults$peak_bounds < 1) ||
        x@procResults$adj_apex < 1)
    {
        setResults(x) <- list(note = "not_detected")
        
        if (getParam(x@param, "plot")) plotPeak(x)
        
        return(x)
    }
    
    # check peak width
    if (x@procResults$peak_bounds[2] - 
        x@procResults$peak_bounds[1] < getParam(x@param, "min_pts"))
    {
        setResults(x) <- list(note = "too_narrow")
        
        if (getParam(x@param, "plot")) plotPeak(x)
        
        return(x)
    }
    
    # calculate peak characteristics
    x <- calculatePeakCharacteristics(x)
    
    # if plot true -> plot result
    if (getParam(x@param, "plot")) plotPeak(x)
    
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
#' @param scanrage An \code{integer} vector of length 2.
#' @param method A single \code{integer} indicating which method to use.
#' 
#' @return A \code{numeric} vector.
#' 
#' @export
#' @docType methods
#' @rdname cpc_raw-methods
setMethod("getXIC", signature("cpc_raw"), function(x, mzrange, scanrange = NULL, method = 1)
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
#' @export
#' @docType methods
#' @rdname cpc_raw-methods
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
    
    return(x)
})


#### Method: scantime ####

#' @title Getter method for the scantime vector in a \code{cpc_raw} object
#' 
#' @param x A \code{cpc_raw} object
#' 
#' @return A \code{numeric} vector with scantimes
#' 
#' @export
#' @docType methods
#' @rdname cpc_raw-methods
setMethod("scantime", signature("cpc_raw"), function(x)
{
    return(x@header$retentionTime)
})



#### Class: cpc ####

#' cpc class
#' 
#' @slot xd Original \code{XCMSnExp} object.
#' @slot xdFilt Filtered \code{XCMSnExp} object.
#' @slot pt \code{data.frame} containing the original peak table from \code{xd}.
#' @slot cpt \code{data.frame} containing the determined peak characteristics.
#' @slot fdef Feature definitions (not used currently).
#' @slot fpeaks Feature defining peaks (not used currently).
#' @slot procData \code{list} containing processing data.
#' @slot procParams \code{list} containing processing parameters.
#' 
#' @name cpc-class
#' @rdname cpc-class
#' @export
setClass("cpc",
         representation(
             xd = "XCMSnExp",
             xdFilt = "XCMSnExp",
             pt = "data.frame",
             cpt = "data.frame",
             fdef = "data.frame",
             fpeaks = "list",
             param = "cpcParam",
             procData = "list",
             procParams = "list"
         ),
         
         prototype(
             xd = new("XCMSnExp"),
             xdFilt = new("XCMSnExp"),
             pt = data.frame(),
             cpt = data.frame(),
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
#' @return A \link{boolean}
#' 
#' @export
#' @docType methods
#' @rdname cpc-methods
setMethod("hasPeakTable", signature("cpc"), function(x)
{
    return(nrow(x@pt) > 0)
})


#### Method: hasCharacterizedPeakTable ####

#' @title Method to check if a \code{cpc} object has a characterized peaktable
#' 
#' @param x A \code{cpc} object
#' 
#' @return A \link{boolean}
#' 
#' @export
#' @docType methods
#' @rdname cpc-methods
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
#' @export
#' @docType methods
#' @rdname cpc-methods
setMethod("xdObj", signature("cpc"), function(x) x@xdObj)


#### Method: xdObj<- ####

#' @title Setter method for the \code{XCMSnExp} object slot in a \code{cpc} object
#' 
#' @param x A \code{cpc} object
#' 
#' @return A \code{cpc} object
#' 
#' @export
#' @docType methods
#' @rdname cpc-methods
setMethod("xdObj<-", signature("cpc"), function(x, value) 
{
    # data checks
    if (class(value) != "XCMSnExp")
        stop("xd had to be a class of type 'XCMSnExp'")
    
    x@xdObj <- value
    return(x)
})


#### Method: peaksToKeep ####

#' @title Method to generate an idx vector of peaks that pass the filter criteria after peak characterization.
#' 
#' @param x A \code{cpc} object
#' @param returnBoolean \code{boolean} indicating if ...
#' 
#' @return An \code{integer} vector of peak idx that pass the criteria
#' 
#' @export
#' @docType methods
#' @rdname cpc-methods
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
setMethod("getPeaklist", signature("cpc"), function(x) x@pt)


#### Method: setPeaklist<- ####

#' @title Setter method for the parsed peak table in a \code{cpc} object
#' 
#' @param x A \code{cpc} object
#' @param value A \code{data.frame} with peak information
#' 
#' @return A \code{cpc} object
#' 
#' @export
#' @docType methods
#' @rdname cpc-methods
setMethod("setPeaklist<-", signature("cpc"), function(x, value) 
{
    # data checks
    
    x@pt <- value
    return(x)
})


#### Method: cpt ####

#' @title Getter method for the \code{cpt} slot in a \code{cpc} object
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
#' @export
#' @docType methods
#' @rdname cpc-methods
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
#' @export
#' @docType methods
#' @rdname cpc-methods
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
#' @export
#' @docType methods
#' @rdname cpc-methods
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
#' @export
#' @docType methods
#' @rdname cpc-methods
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
#' @export
#' @docType methods
#' @rdname cpc-methods
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
#' @export
#' @docType methods
#' @rdname cpc-methods
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
    if (nrow(chromPeaks(x@xd)) < 1) stop("'xd' does not contain any peak information.")
    
    # get the peaklist from the XCMS object
    x@pt <- as.data.frame(xcms::chromPeaks(x@xd))
    
    # add id column to the peaklist
    x@pt <- data.frame(id = 1:nrow(x@pt), x@pt)
    
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
            # sel_peaks <- 1:nrow(ptab)
            if (any(getParam(x@param, "sel_files") < 1 ||
                    getParam(x@param, "sel_files") > length(x@procData$file_paths)))
                stop("Invalid files selected")
            
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
    
    # set max_sigma for all files
    setProcData(x) <- 
        list(max_sigma = foreach(i = 1:length(x@procData$file_paths), .combine = "c") %do% {
            as.numeric(quantile(x@pt$sigma[which(x@pt$sample == i &
                                                     !is.na(x@pt$sigma) &
                                                     x@pt$sigma > 0)],
                                probs = 0.75))
        })
    
    # return object
    return(x)
})


#### Method: parseFeatures ####

#' @title Method for parsing the feature table from an \code{XCMSnExp} object contained in the \code{cpc} object
#' 
#' @description
#' 
#' This method will set the \code{sel_peaks} parameter in \code{procParams} to 
#' include only peaks that make up a part of a feature. It will call 
#' \code{parsePeaklist} with the new peak subset.
#' 
#' @param x A \code{cpc} object
#' 
#' @return A \code{cpc} object
#' 
#' @seealso \link{parsePeaklist}
#' 
#' @export
#' @docType methods
#' @rdname cpc-methods
setMethod("parseFeatures", signature("cpc"), function(x)
{
    # ensure that use_features is set in params (in case the method is run externally.)
    setProcParams(x) <- list(use_features = TRUE)
    
    # check for feature information in xd (in case the method is run externally.)
    ## check that correspondence has been run on the xcms object
    if (!any(unlist(lapply(x@xd@.processHistory, 
                           function(o) o@type)) == "Peak grouping"))
    {
        stop(paste("It does not appear that correspondence has been run on 'xd'. ",
                   "Use XCMS to perform correspondence on 'xd' before using cpc to ",
                   "process feature data.", sep = ""))
    } else
    {
        corr_proc_idx <- which(unlist(lapply(x@xd@.processHistory, 
                                             function(o) o@type)) == "Peak grouping")
    }
    
    ## check that feature information is present in the xcms object
    if (nrow(featureDefinitions(xd)) < 1)
        stop(paste("'xd' does not contain any feature information. ",
                   "Run peak picking and correspondence on the data before running ",
                   "cpc.", sep = ""))
    
    # set min_frac argument
    if (getParam(x@param, "use_features") && is.null(getParam(x@param, "min_frac")))
    {
        # get min_frac from the xcms object
        setParam(x@param) <- 
            list(min_frac = x@xd@.processHistory[[corr_proc_idx]]@param@minFraction)
    }
    
    # get feature definitions from xcms object
    fdef <- xcms::featureDefinitions(x@xd)
    
    x@fdef <- data.frame(fdef[, 1:(ncol(fdef)-1)])
    x@fpeaks <- fdef$peakidx
    
    setParam(x@param) <- list(sel_peaks = sort(as.integer(unique(unlist(lapply(x@fpeaks, 
                                                                               function(k) return(k)))))))
    
    x <- parsePeaklist(x)
    
    return(x)
})


#### Method: filePaths ####

#' @title Getter for the \code{procData$file_paths} slot in a \code{cpc} object
#' 
#' @param x A \code{cpc} object
#' 
#' @return A \code{character} vector with file paths
#' 
#' @export
#' @docType methods
#' @rdname cpc-methods
setMethod("filePaths", signature("cpc"), function(x) 
{
    x@procData$file_paths
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
#' @seealso \link{parsePeaklist}, \link{parseFeatures}
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

        # calculate threshold for sigma as 75% quantile of pt$sigma in current file
        setParam(x@param) <- list(max_sigma =
                                as.numeric(quantile(x@pt$sigma[which(x@pt$sample == i &
                                                                         !is.na(x@pt$sigma) &
                                                                         x@pt$sigma > 0)],
                                                    probs = 0.75)))

        i_idx <- getParam(x@param, "sel_peaks")[which(x@pt$sample[getParam(x@param, "sel_peaks")] == i)]
        i_npeaks <- length(i_idx)

        # output
        cat(paste("Processing file:", x@procData$file_paths[i], "\n"))
        
        # load raw data
        raw <- new("cpc_raw", file_path = x@procData$file_paths[2])
        raw <- parseMz(raw)
        
        # output
        cat(paste("Found ", i_npeaks, " peak(s).\n", sep = ""))
        
        # output <- list(i_progress = 0,
        #                i_lastprogress = 0,
        #                i_counter = 1,
        #                i_npeaks = i_npeaks)
        i_counter = 1
        
        if (getParam(x@param, "verbose_output"))
        {
            cat(paste("[debug] starting peak processing...\n"))
        } else
        {
            cat("% complete: ")
            i_progress <- floor(floor(i_counter/i_npeaks*100)/10)*10
            cat(paste(i_progress, " ", sep=""))
            i_lastprogress <- i_progress
        }
        
        # (pd <- x@pt[i_idx[1], ])
        # (pd <- x@pt[i_idx[2], ])
        # (pd <- x@pt[i_idx[9], ])
        # (pd <- x@pt[i_idx[19640], ]) # tryptophan
        # (pd <- x@pt[i_idx[10808], ]) # this peak causes out of bounds in getEIC
        # (pd <- x@pt[i_idx[which(i_idx == 30295)], ]) # this peak causes std::bad_alloc
        df <- do.call("rbind", apply(x@pt[i_idx, ], 1, FUN = function(pd)
        {
            # start timer
            proc_timer <- Sys.time()
            
            # output
            if (getParam(x@param, "verbose_output")) 
            {
                cat(paste("[debug] peak: ", pd["id"], "; ", sep = ""))
            } else
            {
                i_progress <<- floor(floor(i_counter/i_npeaks*100)/10)*10
                i_counter <<- i_counter + 1
                
                if (i_progress > i_lastprogress)
                {
                    cat(paste(i_progress, " ", sep=""))
                    i_lastprogress <<- i_progress
                }
            }
            
            # create chrom object
            chrom <- new("cpc_chrom",
                         id = as.integer(pd["id"]),
                         param = cpcChromParam(mz = as.numeric(pd["mz"]),
                                               p = as.integer(pd["scpos"]),
                                               s = ifelse((is.na(pd["sigma"]) || as.numeric(pd["sigma"]) >
                                                               as.numeric(getProcData(x, "max_sigma")[i])),
                                                          as.numeric(getProcData(x, "max_sigma")[i]),
                                                          as.numeric(pd["sigma"])),
                                               mz_range = c(as.numeric(pd["mz"]) -
                                                                as.numeric(pd["mz"])/1e6*getParam(x@param, "ppm"),
                                                            as.numeric(pd["mz"]) +
                                                                as.numeric(pd["mz"])/1e6*getParam(x@param, "ppm"))),
                         procParams = list(mz = as.numeric(pd["mz"]),
                                           p = as.integer(pd["scpos"]),
                                           s = ifelse((is.na(pd["sigma"]) || as.numeric(pd["sigma"]) >
                                                           as.numeric(getProcData(x, "max_sigma")[i])),
                                                      as.numeric(getProcData(x, "max_sigma")[i]),
                                                      as.numeric(pd["sigma"])),
                                           mz_range = c(as.numeric(pd["mz"]) -
                                                            as.numeric(pd["mz"])/1e6*getParam(x@param, "ppm"),
                                                        as.numeric(pd["mz"]) +
                                                            as.numeric(pd["mz"])/1e6*getParam(x@param, "ppm"))))
            
            # old param methodlogy with a list() holding the params
            # setProcParams(chrom) <- getProcParams(x)
            
            # new param methodology with a cpcParam object holding the params
            # for better control
            # add params from cpc object
            setParam(chrom@param) <- x@param
            
            # check if xcms data is missing
            if (any(c(pd["scpos"], pd["sigma"]) == -1) || is.na(unlist(pd["sigma"])))
            {
                if (getParam(x@param, "verbose_output")) cat("missing xcms data")
                
                setResults(chrom) <- list(note = "xcms_missing", file = i)
            } else # if xcms data exist
            {
                setResults(chrom) <- list(file = i)
                
                # get XIC
                setXIC(chrom) <- getXIC(raw, mzrange = getParam(chrom@param, "mz_range"))
                
                # process chromatogram
                chrom <- processChromatogram(chrom)
                
            }
            
            setResults(chrom) <- list(exectime = as.numeric(difftime(Sys.time(), proc_timer,
                                                                     units = "secs")))
            
            # end output line
            if (getParam(x@param, "verbose_output")) cat("\n")
            
            return(data.frame(id = chrom@id, getResults(chrom)))
        }))
        
        # end output
        if (getParam(x@param, "verbose_output"))
        {
            cat("[debug] Done!\n\n")
        } else
        {
            cat(paste("Done!\n", sep  = ""))
        }
        
        cpt(x) <- rbind(cpt(x), df)
        
        # output timer
        output_formatted_time(full_timer, Sys.time())
    }
    
    # return object
    return(x)
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
#' @seealso \link{processPeaks}
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
    
    # TODO: Invoke a refineChromPeaks() method that is available in the github but does
    # not seem to be available in my current package version. For compatibility reasons
    # I will implement my own version of this.
    
    # check that there are chromatographic peaks in the xcms object
    if (!hasChromPeaks(x@xd)) {
        stop("No chromatographic peaks present in XCMS object.")
    }
    
    # copy the xcms object
    x@xdFilt <- x@xd
    
    # drop feature definitions if present
    if (hasFeatures(x@xdFilt)) {
        message(paste("Removing existing feature definitions from XCMS object.",
                      "Please run correspondence again..."))
        x@xdFilt <- dropFeatureDefinitions(x@xdFilt)
    }
    
    # create a vector of peaks to remove based on params
    keep <- peaksToKeep(x)
    
    # remove peaks from xcms object
    ncp <- chromPeaks(x@xdFilt)
    chromPeaks(x@xdFilt) <- ncp[keep, ]
    
    # return cpc object
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
#' 
#' @return A \code{cpc_chrom} object
#' 
#' @seealso \link{parsePeaklist}, \link{parseFeatures}, \link{cpc_chrom-class}
#' 
#' @export
#' @docType methods
#' @rdname cpc-methods
setMethod("getChromatogram", signature("cpc"), function(x, id) 
{
    if (nrow(cpt(x)) > 0 && !is.na(match(id, cpc::cpt(x)$id)))
    {
        results <- x@cpt[match(id, cpc::cpt(x)$id), ]
    }
    
    # create a cpc_chrom object for processing
    chrom <- new("cpc_chrom",
                 id = as.integer(id),
                 param = cpc::cpcChromParam(mz = as.numeric(x@pt$mz[id]),
                                            p = as.integer(x@pt$scpos[id]),
                                            s = ifelse(as.numeric(x@pt$sigma[id]) >
                                                           as.numeric(x@procData$max_sigma[x@pt$sample[id]]),
                                                       as.numeric(x@procData$max_sigma[x@pt$sample[id]]),
                                                       as.numeric(x@pt$sigma[id])),
                                            mz_range = c(as.numeric(x@pt$mz[id]) -
                                                             as.numeric(x@pt$mz[id])/1e6*getParam(x@param, "ppm"),
                                                         as.numeric(x@pt$mz[id]) +
                                                             as.numeric(x@pt$mz[id])/1e6*getParam(x@param, "ppm"))),
                 results = results)
    
    # new param methodology with a cpcParam object
    setParam(chrom@param) <- x@param
    
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
    
    if (!is.na(matchedPeakIdx))
    {
        matchedFileName <- MSnbase::fileNames(x@xd)[x@pt$sample[matchedPeakIdx]]
        
        if (!is.character(matchedFileName))
        {
            stop("Could not determine file path")
        }
    } else
    {
        stop("Could not determine file path")
    }
    
    # get raw data
    raw <- new("cpc_raw", file_path = matchedFileName)
    raw <- parseMz(raw)
    
    # save MS info
    setParam(chrom@param) <- list(nscan = raw@runInfo$scanCount)
    
    # set XIC in chrom
    setXIC(chrom) <- getXIC(raw, mzrange = getParam(chrom@param, "mz_range"))
    
    # smooth trace
    chrom <- smoothChromatogram(chrom)
    
    # set plotrange
    if (is.na(match("plotrange", names(x@procData))))
    {
        setProcData(chrom) <- list(plotrange = c(max(1, floor(getParam(chrom@param, "p") - 
                                                                  20*getParam(chrom@param, "s"))),
                                                 min(getParam(chrom@param, "nscan"), 
                                                     floor(getParam(chrom@param, "p") + 
                                                               20*getParam(chrom@param, "s")))))
    }
    
    return(chrom)
})


#### Method: show ####

#' @export
#' @docType methods
#' @rdname cpc-methods
setMethod("show", signature("cpc"), function(x)
{
    cat(paste("S4 object of type 'cpc'.\n"))
    attributes(x)
})


#### Method: summary ####

#' @export
#' @docType methods
#' @rdname cpc-methods
setMethod("summary", signature("cpc"), function(x)
{
    cat(paste("S4 object of type 'cpc'.\n"))
})



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
#' @param ... Additional parameters to be sent to the graphics device
#'
#' @return \code{NULL}
#' @export
plotPeaks <- function(cpc, peakIdx, outPath = NULL, prefix = NULL, device = "png", ...)
{
    # determine which files the peaks are from
    selectedFiles <- sort(unique(cpc@pt$sample[peakIdx]))
    
    # loop over files
    # (curFile <- selectedFiles[1])
    for (curFile in selectedFiles)
    {
        # determine which of the selected peaks are from curFile
        peaksFromCurFile <- peakIdx[which(cpc@pt$sample[peakIdx] == curFile)]
        
        # fetch raw data
        raw <- new("cpc_raw", file_path = MSnbase::fileNames(cpc@xd)[curFile])
        raw <- parseMz(raw)
        
        # loop over peaks
        for (curPeak in peaksFromCurFile)
        {
            # open device (optional)
            if (!is.null(outPath) || !is.null(prefix))
            {
                if (is.null(outPath))
                {
                    outPath <- getwd()
                } else
                {
                    # check path
                    if (!file.exists(outPath))
                    {
                        dir.create(path = outPath, recursive = T)
                    }
                }
                
                outFileName <- paste0(outPath, "/", 
                                      ifelse(is.null(prefix),
                                             basename(sub(".CDF", "", 
                                                          MSnbase::fileNames(cpc@xd)[curFile])),
                                             prefix),
                                      "_", curPeak, ".", device)
                
                do.call(device, list(filename = outFileName, ...))
            }
            
            # create a chromatogram object
            chrom <- cpc::getChromatogram(cpc, id = curPeak)
            
            # plot peak
            cpc::plotPeak(chrom)
            
            # close device(optional)
            if (!is.null(outPath) || !is.null(prefix))
            {
                dev.off()
            }
        }
    }
}

plot_eval_eic <- function(xd = NULL, pks = NULL, cpt = NULL,
                          out_path = NULL, prefix = NULL, ...)
{
    require(mzR)
    
    # get file paths
    cdf_path <- xd@processingData@files
    
    # get peaklist
    pt <- as.data.frame(xcms::chromPeaks(xd))
    
    # get files for selected peaks
    pks <- pks[order(pt$sample[pks])]
    
    j_cpt_idx <- NULL
    
    # get unique file ids
    files <- sort(unique(pt$sample[pks]))
    
    # make sure that output path exist
    if (!is.null(out_path))
    {
        if (!dir.exists(out_path))
        {
            dir.create(out_path, recursive = T)
        }
    }
    
    if (is.null(prefix)) prefix <- "plot"
    
    # LOOP OVER FILES
    #i <- 1
    for (i in files)
    {
        ## get idx of peaks in current file
        i_idx <- pks[which(pt$sample[pks] == i)]
        
        ## calculate threshold for sigma as 75% quantile of pt$sigma
        max_sigma <- 
            as.numeric(quantile(pt$sigma[which(pt$sample == i & !is.na(pt$sigma))], 
                                probs = 0.75))
        
        i_n_idx <- length(i_idx)
        
        ## load mz data file
        cat(paste("File: ", cdf_path[i], "\n", sep = ""))
        i_raw <- mzR::openMSfile(filename = cdf_path[i], backend = "netCDF")
        i_peaks <- mzR::peaks(i_raw)
        
        i_scanidx <- cumsum(unlist(lapply(i_peaks, nrow)))
        i_nscan <- length(i_scanidx)
        
        i_peaks_mz <- unlist(lapply(i_peaks, function(x) return(x[, 1])))
        i_peaks_intensity <- unlist(lapply(i_peaks, function(x) return(x[, 2])))
        
        ## LOOP OVER PEAKS
        # j <- 25509
        for (j in i_idx)
        {
            p <- pt$scpos[j]
            
            if (!is.null(cpt))
            {
                j_cpt_idx <- which(cpt$id == j)
            }
            
            ### calculate mz range
            j_mz_range <- c(pt$mz[j] - pt$mz[j]/1e6*50,
                            pt$mz[j] + pt$mz[j]/1e6*50)
            
            ### load XIC
            j_xic <- getEIC(mz = i_peaks_mz, intensity = i_peaks_intensity,
                            scan_idx = i_scanidx,
                            mz_range = j_mz_range,
                            scan_range = c(0,length(i_scanidx)-1)) # ~790 µsecs
            
            nx <- length(j_xic)
            
            # calculate sg filter derivatives
            if (!is.na(pt$sigma[j]) & pt$sigma[j] > 0)
            {
                w <- ifelse(floor(2.354*ifelse(pt$sigma[j] > max_sigma,
                                               max_sigma,
                                               pt$sigma[j])) %% 2 == 0, # If w is even
                            floor(2.354*ifelse(pt$sigma[j] > max_sigma,
                                               max_sigma,
                                               pt$sigma[j])) + 1, # w + 1
                            floor(2.354*ifelse(pt$sigma[j] > max_sigma,
                                               max_sigma,
                                               pt$sigma[j]))) # else w
                
                # set x plotrange
                plotrange <- c(max(1, floor(p - 20*ifelse(pt$sigma[j] > max_sigma,
                                                          max_sigma,
                                                          pt$sigma[j]))),
                               min(nx, floor(p + 20*ifelse(pt$sigma[j] > max_sigma,
                                                           max_sigma,
                                                           pt$sigma[j]))))
                
                if (w < 3) w <- 3
            } else {
                w <- 3
                
                # set x plotrange
                plotrange <- c(max(1, floor(p) - 60),
                               min(nx, floor(p) + 60))
            }
            
            j_xic_sm <- signal::sgolayfilt(x = j_xic, p = 2, n = w, m = 0) # ~612 µsecs for 25509
            j_xic_d0 <- pmax(0, signal::sgolayfilt(x = j_xic_sm, p = 2, n = w, m = 0)) # ~812 µsecs for 25509
            j_xic_d1 <- signal::sgolayfilt(x = j_xic_sm, p = 2, n = w, m = 1) # ~592 µsecs for 25509
            j_xic_d2 <- signal::sgolayfilt(x = j_xic_sm, p = 2, n = w, m = 2) # ~789 µsecs for 25509
            
            # if out_path is defined -> open device
            if (!is.null(out_path))
            {
                if (!is.null(j_cpt_idx))
                {
                    png(filename = paste(out_path, 
                                         paste(prefix, 
                                               "_",
                                               strsplit(basename(cdf_path[i]), "[.]")[[1]][1],
                                               "_", 
                                               ifelse(cpt$note[j_cpt_idx] == "good", 1, 0), 
                                               "_", 
                                               cpt$id[j_cpt_idx], 
                                               ".png",
                                               sep = ""), 
                                         sep = "/"), ...)
                } else
                {
                    png(filename = paste(out_path, 
                                         paste(prefix, 
                                               "_", 
                                               strsplit(basename(cdf_path[i]), "[.]")[[1]][1],
                                               "_",
                                               i, 
                                               ".png",
                                               sep = ""), 
                                         sep = "/"), ...)
                }
            }
            
            layout(mat = matrix(c(1,1,2,3), nrow = 2, ncol = 2, byrow = T))
            par_bk <- par()
            
            # d0
            ylim <- c(0, 1.05*max(j_xic[cpc_xic$res$bl_front_bound:cpc_xic$res$bl_tail_bound]))
            par(mar = c(4.1,4.1,2.1,1))
            plot(j_xic_d0, type = "l", col = "#000000", 
                 xlim = plotrange,
                 ylim = ylim,
                 xlab = "scan",
                 ylab = "XIC")
            lines(j_xic, col = "#44444475", lty = "dashed")
            points(j_xic_d0, pch = 20, cex = 0.8)
            abline(v = p, col = "red", lty = "dashed")
            title(main = paste("ID: ", j, sep = ""))
            label <- paste(basename(cdf_path[i]), "\n", 
                           "m/z ", round(pt$mz[j],3), " (+/- ", 
                           round(j_mz_range[2]-j_mz_range[1], 3), ")", sep = "")
            
            if (!is.null(j_cpt_idx))
            {
                if (!any(cpt[j_cpt_idx, ] == -1))
                {
                    label <- paste(label, 
                                   paste("area", round(as.numeric(cpt$area[j_cpt_idx]),2)), 
                                   paste("height", round(as.numeric(cpt$height[j_cpt_idx]),2)),
                                   paste("tf", round(as.numeric(cpt$tf[j_cpt_idx]), 3)), 
                                   paste("sn", round(as.numeric(cpt$sn[j_cpt_idx]), 3)), 
                                   paste("code", cpt$code[j_cpt_idx]),
                                   sep = "\n")
                }
                label <- paste(label,
                               cpt$note[j_cpt_idx],
                               sep = "\n")
            }
            
            text(x = plotrange[1], y = 0.95*ylim[2],
                 labels = label, cex = 0.8, adj = c(0,1))
            
            if (!is.null(j_cpt_idx))
            {
                if (!any(cpt[j_cpt_idx, ] == -1))
                {
                    front_bound <- as.numeric(cpt$h1_front_bound[j_cpt_idx])
                    tail_bound <- as.numeric(cpt$h1_tail_bound[j_cpt_idx])
                    apex <- as.numeric(cpt$apex[j_cpt_idx])
                    
                    # d0 peak box
                    ## bottom
                    tmp_x <- c(as.numeric(cpt$h1_front_bound[j_cpt_idx]),
                               as.numeric(cpt$h1_tail_bound[j_cpt_idx]))
                    tmp_y <- c(interpolate_y(x = as.numeric(c(cpt$bl_front_bound[j_cpt_idx],
                                                              cpt$bl_tail_bound[j_cpt_idx])),
                                             y = j_xic_d0[as.numeric(c(cpt$bl_front_bound[j_cpt_idx],
                                                                       cpt$bl_tail_bound[j_cpt_idx]))],
                                             xval = as.numeric(cpt$h1_front_bound[j_cpt_idx])),
                               interpolate_y(x = as.numeric(c(cpt$bl_front_bound[j_cpt_idx],
                                                              cpt$bl_tail_bound[j_cpt_idx])),
                                             y = j_xic_d0[as.numeric(c(cpt$bl_front_bound[j_cpt_idx],
                                                                       cpt$bl_tail_bound[j_cpt_idx]))],
                                             xval = as.numeric(cpt$h1_tail_bound[j_cpt_idx])))
                    
                    lines(x = tmp_x, y = tmp_y, col = "red")
                    ## left
                    tmp_x <- rep(as.numeric(cpt$h1_front_bound[j_cpt_idx]), 2)
                    tmp_y <- c(interpolate_y(x = as.numeric(c(cpt$bl_front_bound[j_cpt_idx],
                                                              cpt$bl_tail_bound[j_cpt_idx])),
                                             y = j_xic_d0[as.numeric(c(cpt$bl_front_bound[j_cpt_idx],
                                                                       cpt$bl_tail_bound[j_cpt_idx]))],
                                             xval = as.numeric(cpt$h1_front_bound[j_cpt_idx])),
                               max(j_xic_d0[floor(as.numeric(cpt$h1_front_bound[j_cpt_idx])):floor(as.numeric(cpt$h1_tail_bound[j_cpt_idx]))]))
                    
                    lines(x = tmp_x, y = tmp_y, col = "red")
                    ## top
                    tmp_x <- c(as.numeric(cpt$h1_front_bound[j_cpt_idx]),
                               as.numeric(cpt$h1_tail_bound[j_cpt_idx]))
                    tmp_y <- rep(max(j_xic_d0[floor(as.numeric(cpt$h1_front_bound[j_cpt_idx])):floor(as.numeric(cpt$h1_tail_bound[j_cpt_idx]))]), 2)
                    
                    lines(x = tmp_x, y = tmp_y, col = "red")
                    ## right
                    tmp_x <- rep(as.numeric(cpt$h1_tail_bound[j_cpt_idx]), 2)
                    tmp_y <- c(interpolate_y(x = as.numeric(c(cpt$bl_front_bound[j_cpt_idx],
                                                              cpt$bl_tail_bound[j_cpt_idx])),
                                             y = j_xic_d0[as.numeric(c(cpt$bl_front_bound[j_cpt_idx],
                                                                       cpt$bl_tail_bound[j_cpt_idx]))],
                                             xval = as.numeric(cpt$h1_tail_bound[j_cpt_idx])),
                               max(j_xic_d0[floor(as.numeric(cpt$h1_front_bound[j_cpt_idx])):floor(as.numeric(cpt$h1_tail_bound[j_cpt_idx]))]))
                    
                    lines(x = tmp_x, y = tmp_y, col = "red")
                    
                    ## baseline
                    lines(x = c(as.numeric(cpt$bl_front_bound[j_cpt_idx]),
                                as.numeric(cpt$bl_tail_bound[j_cpt_idx])),
                          y = j_xic_d0[c(as.numeric(cpt$bl_front_bound[j_cpt_idx]),
                                         as.numeric(cpt$bl_tail_bound[j_cpt_idx]))],
                          col = "red", lty = "dashed")
                }
            }
            
            # d1
            par(mar = c(5.1,4.1,1,1))
            plot(j_xic_d1, type = "l", col = "#000000", xlim = plotrange,
                 ylim = c(min(j_xic_d1[plotrange[1]:plotrange[2]]),
                          max(j_xic_d1[plotrange[1]:plotrange[2]])),
                 xlab = "scan",
                 ylab = "1st derivative")
            points(j_xic_d1, pch = 20, cex = 0.8)
            abline(v = p, col = "red", lty = "dashed")
            
            # d2
            par(mar = c(5.1,4.1,1,1))
            plot(j_xic_d2, type = "l", col = "#000000", xlim = plotrange,
                 ylim = c(min(j_xic_d2[plotrange[1]:plotrange[2]]),
                          max(j_xic_d2[plotrange[1]:plotrange[2]])),
                 xlab = "scan",
                 ylab = "2nd derivative")
            points(j_xic_d2, pch = 20, cex = 0.8)
            abline(v = p, col = "red", lty = "dashed")
            
            par(mar = par_bk$mar)
            layout(mat = matrix(c(1), nrow = 1, ncol = 1))
            
            # if out_path is defined -> close device
            if (!is.null(out_path))
            {
                dev.off()
            }
        }
    }
}


# plotting function
# arguments
# * cpc
# * peak selection (if NULL -> all)
# * output folder
# * file prefix
# * dppm
# * dmz (ignored in dmz set)
# * drt


#' @title Plot chromatogram(s) with process results
#' 
#' @description 
#' 
#' This function is used to plot an evaluation panel with results from peak 
#' processing using the cpc-package.
#'
#' @param cpc A \code{cpc} object
#' @param sel_peaks An \code{integer} vector with peaks indices to be plotted
#' @param out_path Path to save plots. (Default: NULL)
#' @param out_prefix Prefix for filenames. (Default: NULL)
#' @param dppm ppm-value for generating XICs.
#' @param dmz m/z range for generating XICs (ignored in dppm is set)
#' @param drt Retention time range for generated XICs (if NULL entire chromatogram is presented)
#' @param device Graphics device to be used (Default: \code{png})
#' @param ... Further arguments passed to graphics device
#'
#' @return
#' @export
plot_chromatograms <- function(cpc, sel_peaks = NULL, 
                               out_path = NULL, out_prefix = NULL,
                               dppm = 50, dmz = NULL, drt = NULL, device = "png",
                               ...)
{
    require(mzR)
    require(MSnbase)
    require(xcms)
    
    # set arguments for dev
    # sel_peaks <- 25509
    # out_path <- NULL
    # out_prefix <- NULL
    # dppm <- 50
    # drt <- 60
    
    # get file paths
    file_paths <- MSnbase::fileNames(cpc@xd)
    backend <- unlist(lapply(strsplit(file_paths, "[.]"), function(x) {
        if (x[length(x)] == "CDF")
        {
            return("netCDF")
        } else if (x[length(x)] == "mzML")
        {
            return("pwiz")
        } else if (x[length(x)] == "mzXML")
        {
            return("pwiz")
        } else
        {
            stop(paste0("Unsupported file types: ", x[length(x)]))
        }
    }), use.names = F)
    
    # get peaklist
    pt <- as.data.frame(xcms::chromPeaks(cpc@xd))
    
    # get characterized peaklist
    cpt <- cpt(cpc)
    
    # subset pt by cpt peaks
    pt <- pt[rownames(cpt), ]
    
    # make sure there is a peak selection
    if (is.null(sel_peaks)) sel_peaks <- 1:nrow(pt)
    
    # check that all peaks are within bounds of the peaklist
    if (any(sel_peaks < 1) | any(sel_peaks > nrow(pt)))
    {
        oob_peaks <- which(sel_peaks < 1 | sel_peaks > nrow(pt))
        message(paste0(length(oob_peaks), " out of bounds peaks were removed."))
        sel_peaks <- sel_peaks[-oob_peaks]
    }
    
    # make sure that output path exist
    if (!is.null(out_path))
    {
        if (!dir.exists(out_path))
        {
            dir.create(out_path, recursive = T)
        }
    }
    
    # get a list of files with selected peaks
    files <- sort(unique(pt$sample[sel_peaks]))
    
    # loop over files
    for (f in files)
    {
        # get peak idx for current file
        f_idx <- sel_peaks[which(pt[sel_peaks, "sample"] == f)]
        
        # load raw mz data
        cat(paste0("File: ", file_paths[f], "\n"))
        raw <- new("cpc_raw", file_path = file_paths[f])
        raw <- parseMz(raw)
        
        # loop over peaks in file
        # p <- 14358
        for (p in f_idx)
        {
            # determine mz range
            if (!is.null(dppm))
            {
                p_mzr <- c(pt$mz[p] - pt$mz[p]/1e6*dppm,
                           pt$mz[p] + pt$mz[p]/1e6*dppm)
            } else if (!is.null(dmz))
            {
                p_mzr <- c(pt$mz[p] - dmz,
                           pt$mz[p] + dmz)
            } else
            {
                stop("No ppm or dmz value set...")
            }
            
            # determine scanrange
            if (!is.null(drt))
            {
                # determine retention time range
                p_rtr <- c(max(pt$rt[p] - drt, 0),
                           min(pt$rt[p] + drt, raw@runInfo$dEndTime))
                
                # determine scanrange
                p_scidx <- which(raw@header$retentionTime >= p_rtr[1] &
                                     raw@header$retentionTime <= p_rtr[2])
                p_scr <- c(p_scidx[1],
                           p_scidx[length(p_scidx)])
            } else
            {
                # full retention time range
                p_rtr <- c(raw@runInfo$dStartTime, raw@runInfo$dEndTime)
                
                # determine scanrange
                p_scidx <- 1:raw@runInfo$scanCount
                p_scr <- c(1, raw@runInfo$scanCount)
            }
            
            # get cpc_chrom instance
            chrom <- getChromatogram(cpc, p)
            
            # get XIC from raw data
            chrom@xic <- getXIC(raw, mzrange = p_mzr)
            
            # calculate smooth chromatogram
            chrom <- smoothChromatogram(chrom)
            
            # set plotrange
            chrom@procData$plotrange <- p_scr
            
            # if out_path is defined -> open device
            if (!is.null(out_path))
            {
                if (device == "png")
                {
                    png(filename = paste(out_path, 
                                         paste(ifelse(!is.null(out_prefix), 
                                                      paste0(out_prefix, "_"), paste0("")), 
                                               strsplit(basename(file_paths[f]), "[.]")[[1]][1],
                                               "_",
                                               p, 
                                               ".png",
                                               sep = ""), 
                                         sep = "/"), ...)
                } else
                {
                    png(filename = paste(out_path, 
                                         paste(ifelse(!is.null(out_prefix), 
                                                      paste0(out_prefix, "_"), paste0("")), 
                                               strsplit(basename(file_paths[f]), "[.]")[[1]][1],
                                               "_",
                                               p, 
                                               ".png",
                                               sep = ""), 
                                         sep = "/"), ...)
                }
            }
            
            # create plot
            plotPeak(chrom)
            
            # if out_path is defined -> close device
            if (!is.null(out_path))
            {
                dev.off()
            }
            
            rm(chrom)
        }
        
        rm(raw)
    }
}

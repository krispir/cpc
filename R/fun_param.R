
#' @title Constructor for a cpcProcParam object
#' 
#' @description 
#' 
#' These objects are used to set the parameters for the peak processing. Filter 
#' criteria are used to filter out peaks that do not meet the criteria.
#'
#' @param ppm The \code{ppm} range for generating XICs
#' @param min_pts Filter criteria for minimum points between peak boundaries
#' @param min_inf_width Filter criteria for minimum points between peak inflection points. Should be set >=3 to avoid issues.
#' @param min_sn Filter criteria for minimum signal-to-noise ratio.
#' @param min_frac Filter criteria for minimum samples a peak is found in. This is currently not used as it is only applicable to feature filtering.
#' @param min_intensity Filter criteria for minimum peak area.
#' @param smooth_method Smoothing method used during processing. Available: "savgol" for Savitzky-Golay smoothing or "mean" for moving mean smoothing.
#' @param smooth_times Number of smooth iterations,
#' @param smooth_win Width of the smoothing function. If set to NULL it will be determined from the peak sigma values in the XCMSnExp object.
#' @param max_sigma Largest allowable sigmal value used to determine the peak widths from the XCMSnExp object.
#' @param fit_emg \code{logical} indicating if EMG deconvolution should be performed. Note that this adds a significant amount of time to the processing but gives better estimates of peak characteristics.
#' @param sel_peaks \code{integer} vector indicating a subset of peak indices to be processed from the XCMSnExp object. If NULL all peaks are processed.
#' @param sel_files \code{integer} vector indicating a subset of file indices to be processed from the XCMSnExp object. If NULL all files are processed.
#' @param verbose_output \code{logical} value indicating if output is to be given during processing.
#' @param save_all \code{logical} indicating if all processing data should be saved. Caution: This will result in a very large object!
#' @param plot \code{logical} indicating if the results should be plotted for each peak
#'
#' @return A \code{cpcProcParam} object
#' @export
cpcProcParam <- function(ppm = 50.0, 
                         min_pts = 7L,
                         min_inf_width = 3.0,
                         min_sn = 10.0,
                         min_frac = 0.5,
                         min_shoulder_pts = 3L,
                         min_rounded_pts = 3L,
                         min_intensity = NULL,
                         interval_tf = NULL,
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
                         plot = FALSE)
{
  # check params
  
  ## check if any params that need to be set are implicitly set to NULL
  if (is.null(min_pts)) min_pts <- match.arg(min_pts)
  if (is.null(min_inf_width)) min_inf_width <- match.arg(min_inf_width)
  if (is.null(min_sn)) min_sn <- match.arg(min_sn)
  if (is.null(smooth_times)) smooth_times <- match.arg(smooth_times)
  smooth_method <- match.arg(smooth_method)
  
  if(!is.logical(verbose_output))
  {
    message(paste("Argument 'verbose_output' has to be TRUE/FALSE.",
                  "Using verbose_output = FALSE."))
    verbose_output = FALSE
  }
  
  ## check values of params
  if (!is.numeric(min_pts)) stop("'min_pts' has to be an integer value.")
  if (!is.numeric(min_inf_width)) stop("'min_inf_width' has to be an integer value.")
  if (!is.numeric(min_sn)) stop("'min_sn' has to be an integer value.")
  if (!is.null(min_intensity) && !is.numeric(min_intensity))
    stop("'min_intensity' has to be a numeric value.")
  if (!is.null(interval_tf) && !is.numeric(interval_tf))
    stop("'interval_tf' has to be a numeric vector of length 2.")
  if (!is.numeric(smooth_times)) stop("'smooth_times' has to be an integer value.")
  if (!is.null(smooth_win) && !is.numeric(smooth_win))
    stop("'smooth_win' has to be an integer value.")
  
  new("cpcProcParam", ppm = ppm, min_pts = min_pts, 
      min_inf_width = min_inf_width, min_sn = min_sn,
      min_frac = min_frac,
      min_intensity = min_intensity,
      min_shoulder_pts = min_shoulder_pts,
      min_rounded_pts = min_rounded_pts,
      min_w = min_w, max_w = max_w,
      interval_tf = interval_tf,
      smooth_method = smooth_method,
      smooth_times = smooth_times,
      smooth_win = smooth_win,
      max_sigma = max_sigma,
      fit_emg = fit_emg, 
      sel_peaks = sel_peaks,
      sel_files = sel_files,
      verbose_output = verbose_output,
      save_all = save_all,
      plot = plot)
}

#' @title Constructor for a cpcChromParam object
#' 
#' @description 
#' 
#' These objects are used to set the parameters for the peak processing. Filter 
#' criteria are used to filter out peaks that do not meet the criteria.
#' 
#' @param mz m/z value for the peak
#' @param p Max scan of the peak (calculated from \code{rt}) in the \code{XCMSnExp} peak table
#' @param s Sigma value of the peak in the \code{XCMSnExp} peak table
#' @param mz_range m/z range for generating the XIC (determined from the ppm value)
#' @param ppm The ppm range for generating XICs
#' @param min_pts Filter criteria for minimum points between peak boundaries
#' @param min_inf_width Filter criteria for minimum points between peak inflection points. Should be set >=3 to avoid issues.
#' @param min_sn Filter criteria for minimum signal-to-noise ratio.
#' @param min_frac Filter criteria for minimum samples a peak is found in. This is currently not used as it is only applicable to feature filtering.
#' @param min_intensity Filter criteria for minimum peak area.
#' @param smooth_method Smoothing method used during processing. Available: "savgol" for Savitzky-Golay smoothing or "mean" for moving mean smoothing.
#' @param smooth_times Number of smooth iterations,
#' @param smooth_win Width of the smoothing function. If set to \code{NULL} it will be determined from the peak sigma values in the XCMSnExp object.
#' @param max_sigma Largest allowable sigmal value used to determine the peak widths from the XCMSnExp object.
#' @param fit_emg \code{logical} indicating if EMG deconvolution should be performed. Note that this adds a significant amount of time to the processing but gives better estimates of peak characteristics.
#' @param sel_peaks \code{integer} vector indicating a subset of peak indices to be processed from the XCMSnExp object. If NULL all peaks are processed.
#' @param sel_files \code{integer} vector indicating a subset of file indices to be processed from the XCMSnExp object. If NULL all files are processed.
#' @param verbose_output \code{logical} value indicating if output is to be given during processing.
#' @param plot \code{logical} indicating if the results should be plotted for each peak
#'
#' @return A \code{cpcProcParam} object
#' @export
cpcChromParam <- function(mz = NULL,
                          p = NULL,
                          s = NULL,
                          mz_range = NULL,
                          ppm = 50.0, 
                          min_pts = 7L,
                          min_inf_width = 3.0,
                          min_sn = 10.0,
                          min_frac = 0.5,
                          min_intensity = NULL,
                          interval_tf = NULL,
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
                          plot = FALSE)
{
  ## check values of params
  if (!is.numeric(min_pts)) stop("'min_pts' has to be an integer value.")
  if (!is.numeric(min_inf_width)) stop("'min_inf_width' has to be an integer value.")
  if (!is.numeric(min_sn)) stop("'min_sn' has to be an integer value.")
  if (!is.null(min_intensity) && !is.numeric(min_intensity))
    stop("'min_intensity' has to be a numeric value.")
  if (!is.null(interval_tf) && !is.numeric(interval_tf))
    stop("'interval_tf' has to be a numeric vector of length 2.")
  if (!is.numeric(smooth_times)) stop("'smooth_times' has to be an integer value.")
  if (!is.null(smooth_win) && !is.numeric(smooth_win))
    stop("'smooth_win' has to be an integer value.")
  
  new("cpcChromParam", 
      mz = mz, p = p, s = s, mz_range = mz_range,
      ppm = ppm, min_pts = min_pts, 
      min_inf_width = min_inf_width, min_sn = min_sn,
      min_frac = min_frac,
      min_intensity = min_intensity,
      interval_tf = interval_tf,
      min_w = min_w, max_w = max_w,
      smooth_method = smooth_method,
      smooth_times = smooth_times,
      smooth_win = smooth_win,
      max_sigma = max_sigma,
      fit_emg = fit_emg, 
      sel_peaks = sel_peaks,
      sel_files = sel_files,
      verbose_output = verbose_output,
      save_all = save_all,
      plot = plot)
}
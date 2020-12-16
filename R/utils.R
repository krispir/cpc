#' Title
#'
#' @param m 
#' @param block.size 
#' @param nb 
#'
#' @return
#' @export
#'
#' @examples
cut_by_size <- function (m, block.size, nb = ceiling(m/block.size))
{
    if (nb > m)
        nb <- m
    int <- m/nb
    upper <- round(1:nb * int)
    lower <- c(1, upper[-nb] + 1)
    size <- c(upper[1], diff(upper))
    cbind(lower, upper, size)
}

#' Title
#'
#' @param start 
#' @param end 
#'
#' @return
#' @export
#'
#' @examples
output_formatted_time <- function(start, end)
{
    elapsed_time <- as.numeric(difftime(end, start, units = "secs"))
    
    if (elapsed_time >= 3600)
    {
        divisor <- 3600
    } else if (elapsed_time >= 60)
    {
        divisor <- 60
    } else
    {
        divisor <- 1
    }
    
    cat("Finished in ", 
        round(elapsed_time/divisor,3), " ",
        switch(as.character(divisor), 
               "3600" = "hours", 
               "60" = "mins", 
               "1" = "secs"), 
        ".\n", sep = "")
}

#' Title
#'
#' @param y 
#' @param apex 
#' @param bl_bounds 
#' @param peak_bounds 
#' @param frac 
#' @param exact 
#' @param id 
#' @param debug 
#' @param plot 
#'
#' @return
#' @export
#'
#' @examples
find_height_bounds <- function(y = NULL, apex = NULL, 
                               bl_bounds = NULL, peak_bounds = NULL, 
                               frac = NULL, exact = TRUE, id = NULL,
                               debug = FALSE, plot = FALSE)
{
    # exact = TRUE
    # data checks
    if(any(is.null(y), is.null(frac)))
        stop("missing arguments.")
    
    ny <- length(y)
    
    if (ny < 2)
        stop("y has to be at least 2")
    
    if (any(bl_bounds < 1) | any(bl_bounds > ny) | 
        any(peak_bounds < 1) | any(peak_bounds > ny)) # if a or b is outside of bounds
        stop("bad bounds.")
    
    if (frac < 0 | frac > 1) # if invalid frac is supplied
        stop("invalid p. p needs to be between 0 and 1.")
    
    # calculate the baseline
    ## slope
    baseline_slope <- (y[bl_bounds[2]] - y[bl_bounds[1]]) / (bl_bounds[2] - bl_bounds[1])
    
    ## baseline
    baseline <- (1:ny - bl_bounds[1]) * baseline_slope + y[bl_bounds[1]]
    
    # calculate peak height at apex
    if (is.null(apex))
    {
        apex <- which.max(y[round(peak_bounds[1]):round(peak_bounds[2])]) + 
            round(peak_bounds[1]) - 1
    } else if (apex < peak_bounds[1] | apex > peak_bounds[2])
    {
        warning(paste("Apex out of bounds in id = ", id, 
                      ". Locating maxima within bounds...", "\n", sep = ""))
        apex <- which.max(y[round(peak_bounds[1]):round(peak_bounds[2])]) + 
            round(peak_bounds[1]) - 1
    }
    
    y_adj <- y - baseline
    
    height <- y_adj[apex]
    
    threshold <- frac * height
    
    front_bound <- skim_to_val(y_adj,
                               threshold, 
                               apex-1,
                               floor(peak_bounds[1])-1, 
                               -1) + 1 # returns first point below thresh
    # OR the break point
    
    tail_bound <- skim_to_val(y_adj,
                              threshold, 
                              apex-1,
                              floor(peak_bounds[2]), 
                              1) + 1 # returns first point below thresh
    # OR the break point
    
    output <- rep(-1, 2)
    
    if (exact) # if exact values
    {
        # front
        if (front_bound == floor(peak_bounds[1])) # the skimmer stopped at peak_bound
        {
            output[1] <- peak_bounds[1]
        } else
        {
            output[1] <- interpolate_x(x = c(front_bound,
                                             front_bound + 1),
                                       y = y_adj[c(front_bound,
                                                   front_bound + 1)],
                                       yval = threshold)
            
            if (output[1] < peak_bounds[1]) output[1] <- peak_bounds[1]
        }
        
        # tail
        if (tail_bound == floor(peak_bounds[2])+1) # the skimmer stopped at peak_bound
        {
            output[2] <- peak_bounds[2]
        } else
        {
            output[2] <- interpolate_x(x = c(tail_bound - 1,
                                             tail_bound),
                                       y = y_adj[c(tail_bound - 1,
                                                   tail_bound)],
                                       yval = threshold)
            
            if (output[2] > peak_bounds[2]) output[2] <- peak_bounds[2]
        }
    } else
    {
        output <- c(front_bound+1, tail_bound-1)
        
        if (output[1] < round(peak_bounds[1])) output[1] <- round(peak_bounds[1])
        if (output[2] > round(peak_bounds[2])) output[2] <- round(peak_bounds[2])
    }
    
    return(output)
}

#' Title
#'
#' @param x 
#' @param y 
#' @param a 
#' @param b 
#'
#' @return
#' @export
#'
#' @examples
peak_integral <- function(x = NULL, y = NULL, a = NULL, b = NULL)
{
    if (is.null(x)) x <- 1:ny
    
    if (is.null(y)) stop("no y...")
    
    nx = length(x)
    ny = length(y)
    
    if (nx != ny)
    {
        x = 1:ny
    }
    
    if (is.null(a))
    {
        a <- 1
    } else if (a < 1)
    {
        a <- 1
    }
    
    if (is.null(b))
    {
        b <- ny
    } else if (b > ny)
    {
        b <- ny
    }
    
    if (a > b)
    {
        c <- a
        a <- b
        b <- c
    }
    
    c_integrate_vector(x = x, y = y, a = a-1, b = b-1)
}

#' Intepolate x value between two points given a y value
#'
#' @param x Vector of length 2 giving the x values
#' @param y Vector of length 2 giving the y values
#' @param yval y value
#'
#' @return
#' @export
#'
#' @examples
interpolate_x <- function(x = NULL, y = NULL, yval = NULL)
{
    ((yval - y[1]) / (y[2]-y[1])) + x[1]
}

#' Intepolate y value between two points given an x value
#'
#' @param x Vector of length 2 giving the x values
#' @param y Vector of length 2 giving the y values
#' @param xval x value
#'
#' @return
#' @export
#'
#' @examples
interpolate_y <- function(x = NULL, y = NULL, xval = NULL)
{
    ((y[2]-y[1])/(x[2]-x[1])) * (xval-x[1]) + y[1]
}


#' Title
#'
#' @param x 
#' @param y 
#' @param k 
#' @param yval 
#'
#' @return
#' @export
#'
#' @examples
interpolate_2p_x <- function(x = NULL, y = NULL, k = NULL, yval = NULL)
{
    ((yval - y[1]) / k) + x[1]
}
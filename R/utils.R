#' Set x-axis ticks
#'
#' Create ticks points.
#'
#' @param at Numerical vector, create ticks at given values.
#' @inheritParams forest
#'
#' @return A vector
#'
#' @keywords internal

make_ticks <- function(at = NULL,
                       xlim,
                       refline = 1,
                       x_trans = "none"){

  if(is.null(at)){

    if(x_trans %in% c("none", "scientific")){
      ticks_at <- pretty(xlim)
    }else {
      pt_cut <- pretty(range(c(xlim, log(refline))))

      if(all(exp(pt_cut) >= 1)){
        pt_cut <- round(exp(pt_cut))
      }else{
        pt_cut <- round(exp(pt_cut), 1) # Keep 1 digits
      }

      ticks_at <- log(unique(pt_cut)) # avoid any duplicate
      # Limit values inside xlim
      ticks_at <- ticks_at[exp(ticks_at) <= max(exp(xlim))]
    }

  }else {
    ticks_at <- xscale(at, scale = x_trans)
  }

  ticks_at <- ticks_at[is.finite(ticks_at)]
  ticks_at[ticks_at >= min(xlim) & ticks_at <= max(xlim)]

}


#' Create xlim
#'
#' Create xlim based on value ranges.
#'
#' @inheritParams forest
#'
#' @return A list
#'
#' @keywords internal
#'
make_xlim <- function(xlim = NULL,
                      lower,
                      upper,
                      ref_line = ifelse(x_trans %in% c("log", "log2", "log10"), 1, 0),
                      ticks_at = NULL,
                      x_trans = "none"){

  # Use range if missing
  if(is.null(xlim)){
    xlim <- range(c(min(unlist(lower), na.rm = TRUE),
                    ref_line,
                    max(unlist(upper), na.rm = TRUE)),
                  na.rm = TRUE)
  }

  if(x_trans %in% c("log", "log2", "log10")){
    if(min(xlim) == 0)
      xlim[which.min(xlim)] <- min(c(unlist(lower),
                                     ref_line,
                                     ticks_at),
                                   na.rm = TRUE)
    xlim <- xscale(xlim, scale = x_trans)

  }

  return(xlim)

}

# Stop if not a gpar() object
# internal
stop_ifnot_gpar <- function(x) {
  nm <-deparse(substitute(x))
  if(!inherits(x, "gpar"))
    stop(sprintf("Error: %s must be a gpar() object", nm))
}


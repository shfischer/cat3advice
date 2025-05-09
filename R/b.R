#' @include generics.R
#' @importFrom icesAdvice icesRound
NULL

### ------------------------------------------------------------------------ ###
### b class ####
### ------------------------------------------------------------------------ ###
#' @title b-class
#' 
#' @description  An S4 class to represent component b of the rfb/rb/chr rules.
#' 
#' This class (\code{b}) stores the input for component b (the biomass
#' safeguard) as well as the resulting b value. 
#' 
#' The classes \code{rfb_b}, \code{rb_b}, and \code{chr_b} inherit from 
#' \code{b} and their only difference is that the slot \code{hcr}
#' is set to the corresponding catch rule name ('rfb', 'rb', or 'chr').
#' 
#' @slot value The value of component b
#' @slot idx_value Index value that is compared to Itrigger.
#' @slot Itrigger The index trigger value below which the advice is reduced. Usually calculated as \code{Itrigger=Iloss*w}.
#' @slot Iloss The lowest observed index value. Can be used as the basis for \code{Itrigger}.
#' @slot w Index trigger buffer. Connects \code{Itrigger} to \code{Iloss}.
#' @slot yr_ref Reference year on which Itrigger is based.
#' @slot yr_last Last data year of the biomass index. The index value in this year is compared to \code{Itrigger}.
#' @slot lag \code{numeric}. Time lag between the last index year and the last year to be used.
#' @slot n_years \code{numeric}. The number of years used for the index value.
#' @slot idx \code{data.frame}. A \code{data.frame} with the index values.
#' @slot units \code{character}. The units of the biomass index, e.g. 'kg/hr'.
#' @slot hcr \code{character}. The harvest control rule (hcr) for which the biomass safeguard is used. One of 'rfb', 'rb', or 'chr'.
#' 
#' @name b-class
#' @export
setClass(Class = "b", 
         slots = c(value = "numeric",
                   idx_value = "numeric",
                   Itrigger = "numeric", 
                   Iloss = "numeric", 
                   w = "numeric",
                   yr_ref = "numeric",
                   yr_last = "numeric",
                   lag = "numeric",
                   n_yrs = "numeric",
                   idx = "data.frame",
                   units = "character",
                   hcr = "character"),
         prototype = list(value = NA_real_,
                          idx_value = NA_real_,
                          Itrigger = NA_real_,
                          Iloss = NA_real_,
                          w = 1.4,
                          yr_ref = NA_real_,
                          yr_last = NA_real_,
                          lag = 0,
                          n_yrs = 1,
                          idx = data.frame(year = NULL, index = NULL),
                          units = NA_character_,
                          hcr = NA_character_))

#' @rdname b-class
setClass(Class = "rfb_b", 
         contains = "b",
         prototype = list(hcr = "rfb"))
#' @rdname b-class
setClass(Class = "rb_b", 
         contains = "b",
         prototype = list(hcr = "rb"))
#' @rdname b-class
setClass(Class = "chr_b", 
         contains = "b",
         prototype = list(hcr = "chr"))

### ------------------------------------------------------------------------ ###
### b methods ####
### ------------------------------------------------------------------------ ###
#' rb/rfb/chr rule - component b (biomass safeguard)
#'
#' This function calculates component b (the biomass safeguard) of the rb, rfb,
#' and chr rule. The index needs to be a biomass index without age structure.
#' 
#' The biomass safeguard compares the last index value (\out{<i>I</i>}) to an index trigger value (\out{<i>I</i><sub>trigger</sub>}). If the current index value is below the trigger, the biomass safeguard reduces the catch advice:
#' 
#' \out{<i>b</i> = min{1, <i>I</i> / <i>I</i><sub>trigger</sub>}}
#' 
#' 
#' , where \out{<i>I</i><sub>trigger</sub>} is usually derived from the lowest observed biomass index value (\out{<i>I</i><sub>loss</sub>}) as:
#' 
#' \out{<i>I</i><sub>trigger</sub> = w * <i>I</i><sub>loss</sub>}
#' 
#' with 
#' 
#' \out{<i>w</i> = 1.4}
#' 
#' See ICES (2022) for the full definition definition.
#'
#' Usually, this method is used by providing only a biomass index, e.g. 
#' as a \code{data.frame}. The method uses this index, searches for the lowest
#' index value (\code{Iloss}), multiplies this value by the index trigger
#' buffer (\code{w}) to get the index trigger value (\code{Itrigger}). 
#' The last index value in the time series is then compared to \code{Itrigger}
#' and if the index value is below, the biomass safeguard reduces the catch
#' advice.
#'
#' The biomass safeguard is identical in the rfb, rb, and chr rules. 
#' \code{rfb_b()}, \code{rb_b()} and \code{chr_b()} are aliases for 
#' \code{b()} with identical arguments and functionality.
#' 
#'
#' @param object The biomass index. Can be a \code{data.frame} with columns 'data' and 'index'.
#' @param idx_value Optional. The current index value. Only used if no index time series is supplied.
#' @param Itrigger Optional. The index trigger value below which the biomass safeguard reduces the catch advice.
#' @param Iloss Optional. The lowest index value, can be used to calculate \code{Itrigger}.
#' @param w Optional. The index trigger buffer (multiplier) to link \code{Itrigger} to \code{Iloss}. Defaults to \code{w=1.4}.
#' @param yr_ref Optional. If supplied, this specifies the year in the biomass index which is used as \code{Iloss} and \code{Itrigger} is calculated from this value.
#' @param lag Optional. Time lag between the last index year and the last year to be used. By default, the last index year is used (\code{lag=0}).
#' @param n_yrs Optional. The number of years used in the index. By default, only the last index value is used (\code{n_yrs=1}).
#' @param units Optional. The units of the biomass index, e.g. 'kg/hr'. Only used for plotting.
#' @param hcr Optional. One of 'rfb', 'rb', or 'chr'.
#' @param ... Additional arguments. Not used.
#'  
#' @section Warning:
#' Please note that \out{<i>I</i><sub>trigger</sub>} should only be defined once the first time the empirical harvest control rule is applied. In the following years, the same value should be used for \out{<i>I</i><sub>trigger</sub>}.
#' For application in ICES, do not change the defaults unless the change is supported by stock-specific simulations.
#'
#' @references 
#' ICES. 2025. ICES Guidelines - Advice rules for stocks in category 2 and 3. Version 3. ICES Guidelines and Policies - Advice Technical Guidelines. 31 pp. \url{https://doi.org/10.17895/ices.pub.28506179}.
#' 
#' Fischer, S. H., De Oliveira, J. A. A., Mumford, J. D., and Kell, L. T. 2023. Risk equivalence in data‐limited and data‐rich fisheries management: An example based on the ICES advice framework. Fish and Fisheries, 24: 231--247. \url{https://doi.org/10.1111/faf.12722}.
#' 
#' Fischer, S. H., De Oliveira, J. A. A., Mumford, J. D., and Kell, L. T. 2022. Exploring a relative harvest rate strategy for moderately data-limited fisheries management. ICES Journal of Marine Science, 79: 1730--1741. \url{https://doi.org/10.1093/icesjms/fsac103}.
#' 
#' Fischer, S. H., De Oliveira, J. A. A., Mumford, J. D., and Kell, L. T. 2021. Application of explicit precautionary principles in data-limited fisheries management. ICES Journal of Marine Science, 78: 2931--2942. \url{https://doi.org/10.1093/icesjms/fsab169}.
#' 
#' Fischer, S. H., De Oliveira, J. A. A., Mumford, J. D., and Kell, L. T. 2021. Using a genetic algorithm to optimize a data-limited catch rule. ICES Journal of Marine Science, 78: 1311--1323. \url{https://doi.org/10.1093/icesjms/fsab018}.
#' 
#' Fischer, S. H., De Oliveira, J. A. A., and Kell, L. T. 2020. Linking the performance of a data-limited empirical catch rule to life-history traits. ICES Journal of Marine Science, 77: 1914--1926. \url{https://doi.org/10.1093/icesjms/fsaa054}.
#'
#'
#' @return An object of class \code{b} with the value of the biomass 
#' safeguard
#'
#' @examples
#' # If the value of the biomass safeguard is known
#' b(1)
#' 
#' # First application of the biomass safeguard
#' # Use a data.frame with index values
#' df_idx <- data.frame(year = 2017:2021,
#'                      index = c(1.33, 1.13, 0.84, 0.60, 1.03))
#' b <- b(df_idx)
#' b
#' advice(b)
#' 
#' # plot
#' plot(b(df_idx, units = "kg/hr"))
#' 
#' # Use of the biomass safeguard in a following year without updating Itrigger
#' df_idx <- data.frame(year = 2017:2022,
#'                      index = c(1.33, 1.13, 0.84, 0.60, 1.03, 0.5))
#' b(df_idx, yr_ref = 2020)
#' 
#' @export
setGeneric(name = "b", 
           def = function(object, idx_value, Itrigger, Iloss, w,
                          yr_ref, lag, n_yrs, units, hcr, ...) 
           standardGeneric("b"),
           signature = c("object"))

### data.frame -> use as index
#' @rdname b
#' @usage NULL
#' @export
setMethod(b, 
          signature = c(object = "data.frame"),
          function(object, idx_value, Itrigger, Iloss, w,
                   yr_ref, lag, n_yrs, units, hcr, ...) {
  idx <- object
  names(idx) <- tolower(names(idx))
  ### check if "year" column exists
  if (isFALSE("year" %in% names(idx)))
    stop("Column \"year\" missing in idx")
  ### check if "index" column exists
  if (isFALSE("index" %in% names(idx))) {
    if (identical(ncol(idx), 2L)) {
      message(paste0("Column \"index\" missing in idx. Using column ",
                     "\"", names(idx)[2], "\" instead"))
    } else {
      stop("Column \"index\" missing in idx")
    }
  }
  b_calc(idx = idx, idx_value = idx_value, Itrigger = Itrigger, 
         Iloss = Iloss, w = w, yr_ref = yr_ref, lag = lag, n_yrs = n_yrs,
         units = units, 
         ...)
})

### I -> use as index
#' @rdname b
#' @usage NULL
#' @export
setMethod(b, 
          signature = c(object = "I"),
          function(object, idx_value, Itrigger, Iloss, w,
                   yr_ref, lag, n_yrs, units, hcr, ...) {
  b_calc(idx = object@idx, idx_value = object, Itrigger = Itrigger, 
         Iloss = Iloss, w = w, yr_ref = yr_ref, lag = lag, n_yrs = n_yrs,
         units = units,
         ...)
})

### numeric -> use as b
#' @rdname b
#' @usage NULL
#' @export
setMethod(b, 
          signature = c(object = "numeric"), 
          function(object, idx_value, Itrigger, Iloss, w,
                   yr_ref, lag, n_yrs, units, hcr, ...) {
            
  ### create empty b object
  res <- new("b")
  if (!missing(hcr)) res@hcr <- hcr
  
  ### remove parameters
  res@w <- NA_real_
  
  ### insert value
  res@value <- object
  
  ### check value
  if (isTRUE(res@value > 1)) {
    warning("The biomass safeguard value exceeds 1, replacing value with 1")
    res@value <- 1
  }
  
  return(res)
})
### b -> check validity and update values if necessary
#' @rdname b
#' @usage NULL
#' @export
setMethod(b, 
          signature = c(object = "b"), 
          function(object, idx_value, Itrigger, Iloss, w,
                   yr_ref, lag, n_yrs, units, hcr, ...) {
  ### check validity
  validObject(object)
  ### run b() to update slots and recalculate if needed
  b_calc(object, ...)
})

### ------------------------------------------------------------------------ ###
### alias ####
### ------------------------------------------------------------------------ ###
### define aliases rfb_b, rb_b, and chr_b for b
### set object signature to ANY and let b deal with method dispatch

### rfb
#' @rdname b
#' @export
setGeneric(name = "rfb_b", 
           def = function(object, idx_value, Itrigger, Iloss, w,
                          yr_ref, lag, n_yrs, units, hcr = "rfb", ...) 
             standardGeneric("rfb_b"),
           signature = c("object"))
#' @rdname b
#' @usage NULL
#' @export
setMethod(rfb_b, 
          signature = c(object = "ANY"),
          function(object, idx_value, Itrigger, Iloss, w,
                   yr_ref, lag, n_yrs, units, hcr = "rfb", ...) {
  hcr <- match.arg(hcr)
  object <- b(object = object, idx_value = idx_value, Itrigger = Itrigger,
              Iloss = Iloss, w = w, yr_ref = yr_ref, lag = lag, n_yrs = n_yrs, 
              units = units, hcr = hcr, ... = ...)
  class(object) <- "rfb_b"
  object@hcr <- "rfb"
  return(object)
})
### rb
#' @rdname b
#' @export
setGeneric(name = "rb_b", 
           def = function(object, idx_value, Itrigger, Iloss, w,
                          yr_ref, lag, n_yrs, units, hcr = "rb", ...) 
             standardGeneric("rb_b"),
           signature = c("object"))
#' @rdname b
#' @usage NULL
#' @export
setMethod(rb_b, 
          signature = c(object = "ANY"),
          function(object, idx_value, Itrigger, Iloss, w,
                   yr_ref, lag, n_yrs, units, hcr = "rb", ...) {
  hcr <- match.arg(hcr)
  object <- b(object = object, idx_value = idx_value, Itrigger = Itrigger,
              Iloss = Iloss, w = w, yr_ref = yr_ref, lag = lag, n_yrs = n_yrs,
              units = units, hcr = hcr, ... = ...)
  class(object) <- "rb_b"
  object@hcr <- "rb"
  return(object)
})
### chr
#' @rdname b
#' @export
setGeneric(name = "chr_b", 
           def = function(object, idx_value, Itrigger, Iloss, w,
                          yr_ref, lag, n_yrs, units, hcr = "chr", ...) 
             standardGeneric("chr_b"),
           signature = c("object"))
#' @rdname b
#' @usage NULL
#' @export
setMethod(chr_b, 
          signature = c(object = "ANY"),
          function(object, idx_value, Itrigger, Iloss, w,
                   yr_ref, lag, n_yrs, units, hcr = "chr", ...) {
  hcr <- match.arg(hcr)
  object <- b(object = object, idx_value = idx_value, Itrigger = Itrigger,
              Iloss = Iloss, w = w, yr_ref = yr_ref, lag = lag, n_yrs = n_yrs,
              units = units, hcr = hcr, ... = ...)
  class(object) <- "chr_b"
  object@hcr <- "chr"
  return(object)
})

### ------------------------------------------------------------------------ ###
### b validity ####
### ------------------------------------------------------------------------ ###
### validity checks
setValidity("b", function(object) {
  if (!identical(length(object@value), 1L)) {
    "slot value must be of length 1"
  } else if (isTRUE(object@value > 1)) {
    "The biomass safeguard (component b) value cannot exceed 1"
  } else if (!identical(length(object@idx_value), 1L)) {
    "slot idx_value must be of length 1"
  } else if (!identical(length(object@Itrigger), 1L)) {
    "slot Itrigger must be of length 1"
  } else if (!identical(length(object@Iloss), 1L)) {
    "slot Iloss must be of length 1"
  } else if (!identical(length(object@w), 1L)) {
    "slot w must be of length 1"
  } else if (!identical(length(object@yr_ref), 1L)) {
    "slot yr_ref must be of length 1"
  } else if (!identical(length(object@lag), 1L)) {
    "slot lag must be of length 1"
  } else if (!identical(length(object@n_yrs), 1L)) {
    "slot lag must be of length 1"
  } else if (!is(object@idx, "data.frame")) {
    "slot idx must be a data.frame"
  } else if (!identical(length(object@units), 1L)) {
    "slot units must be of length 1"
  } else {
    TRUE
  }
})

### ------------------------------------------------------------------------ ###
### b calculation ####
### ------------------------------------------------------------------------ ###
### function for creating/calculating biomass safeguard
b_calc <- function(object, idx, idx_value, Itrigger, Iloss, w, lag, n_yrs,
                   yr_ref, yr_last, units, hcr) {
  ### create empty b object, if missing
  if (missing(object)) object <- new("b")
  if (!missing(hcr)) object@hcr <- hcr
  
  ### add/update index, if provided
  if (!missing(idx)) object@idx <- idx
  
  ### add/update parameters, if provided
  #if (!missing(idx_value)) ### later
  if (!missing(Itrigger)) object@Itrigger <- Itrigger
  if (!missing(Iloss)) object@Iloss <- Iloss
  if (!missing(w)) object@w <- w
  if (!missing(lag)) object@lag <- lag
  if (!missing(n_yrs)) object@n_yrs <- n_yrs
  if (!missing(yr_ref)) object@yr_ref <- yr_ref
  if (!missing(units)) object@units <- units
  
  ### find last data year
  if (isTRUE(length(object@idx) > 0)) 
    object@yr_last <- tail(object@idx$year, 1)
  
  ### use Iloss if provided
  if (!is.na(object@Iloss)) {
    
    object@Itrigger <- object@Iloss * object@w
    
  ### if reference year given, use this year to get Iloss
  } else if  (!is.na(object@yr_ref)) {
    
    object@Iloss <- object@idx$index[which(object@idx$year == object@yr_ref)]
    object@Itrigger <- object@Iloss * object@w
    
  ### determine Iloss from index time series and estimate Itrigger from Iloss
  } else if (isTRUE(length(object@idx) > 0) & 
             missing(Itrigger) & missing(Iloss)) {
    
    pos_loss <- which.min(object@idx$index)
    object@yr_ref <- object@idx$year[pos_loss]
    object@Iloss <- object@idx$index[pos_loss]
    object@Itrigger <- object@Iloss * object@w
    
  }
  
  ### if index value supplied as argument, use this value
  if (!missing(idx_value)) {
    
    if (is(idx_value, "I")) {
      object@idx_value <- idx_value@value
      object@lag <- idx_value@lag
      object@n_yrs <- idx_value@n_yrs
      object@yr_last <- idx_value@yr_last
    } else {
      object@idx_value <- idx_value
    }
    
  ### get index value from object@idx
  } else if (isTRUE(length(object@idx) > 0)) {
  
    ### determine years to use
    yrs_use <- seq(from = object@yr_last - object@lag - object@n_yrs + 1, 
                   to = object@yr_last - object@lag)
    ### estimate mean index over these years
    object@idx_value <- mean(object@idx$index[object@idx$year %in% yrs_use],
                         na.rm = TRUE)
    
  }
  
  ### compare index value to Itrigger
  if (isTRUE(!is.na(object@idx_value))) {
    
    object@value <- ifelse(object@idx_value >= object@Itrigger, 
                           1, object@idx_value/object@Itrigger)
    
  }
  
  return(object)
}

### ------------------------------------------------------------------------ ###
### convenience methods ####
### ------------------------------------------------------------------------ ###
#' @rdname summary
#' @export
setMethod(f = "summary", signature = "b", 
          definition = function(object, ...) {
  txt <- (paste0(paste(rep("-", 50), collapse = ""), "\n",
                 "component b (biomass safeguard):\n",
                 "Itrigger (", object@Itrigger, ") is based on:\n",
                 ifelse(!is.na(object@yr_ref),
                   paste0("  The index value in year ", object@yr_ref, 
                          " (", object@Iloss, ") is used as Iloss.\n",
                          "  Itrigger is calculated as Itrigger=Iloss*w:\n",
                          "   Itrigger = ", object@Iloss, " * ", object@w,
                          " = ", object@Itrigger),
                   paste("  Itrigger = ", object@Itrigger, "\n")),
                 "\nThe last index value (", object@idx_value,") is from ",
                 object@yr_last, "\n",
                 ifelse(object@idx_value >= object@Itrigger,
                   paste0("The last index value is AT OR ABOVE Itrigger;\n",
                          " therefore, ",
                          "the biomass safeguard does not reduce the catch ",
                          "advice:\n b = ", object@value),
                   paste0("The last index value is BELOW Itrigger;\n",
                          " therefore, ",
                          "the biomass safeguard reduce the catch ",
                          "advice:\n b = ", object@value)),
                 "\n",
                 paste0(paste(rep("-", 50), collapse = ""))))
  cat(txt)
})


### weird bug - method definition moved to r.R, 
### other method is not found ...
# #' @rdname value
# #' @export
# setMethod(f = "value", signature = "b", 
#           definition = function(object) {
#             return(object@value)
#           })

### print
setMethod(f = "print", signature = "b", 
          definition = function(x) {
            cat(paste0("An object of class \"", class(x), "\".\n",
                       "Value: ", x@value, "\n"))
})

### show
setMethod(f = "show", signature = "b", 
          definition = function(object) {
            cat(paste0("An object of class \"", class(object), "\".\n",
                       "Value: ", object@value, "\n"))
})

### ------------------------------------------------------------------------ ###
### ICES advice style table ####
### ------------------------------------------------------------------------ ###
#' @rdname advice
#' @usage NULL
#' @export
setMethod(
  f = "advice", signature = "b",
  definition = function(object) {
    txt <- paste0(paste(rep("-", 80), collapse = ""), "\n",
                  "Biomass safeguard\n",
                  paste(rep("-", 80), collapse = ""), "\n")
    
    if (!is.na(object@yr_last)) {
      I_last_year <- object@yr_last
      if (isTRUE(object@n_yrs > 1)) {
        I_last_year <- paste(c(I_last_year - object@n_yrs + 1, I_last_year),
                             collapse = "-")
      }
    } else {
      I_last_year <- "last"
    }
    txt_I <- paste0("Last index value (I", I_last_year, ")")
    txt_I_trigger <- paste0("Index trigger value (Itrigger = Iloss x ", 
                            object@w, ")")
    txt_b1 <- paste0("b: multiplier for index relative to trigger,")
    txt_b2 <- paste0("min{I", I_last_year, "/Itrigger, 1}")
    
    val_I <- paste0(ifelse(object@idx_value > 100, 
                           round(object@idx_value),
                           icesAdvice::icesRound(object@idx_value)),
                    ifelse(!is.na(object@units), paste0(" ", object@units), ""))
    val_I_trigger <- paste0(ifelse(object@Itrigger > 100, 
                                   round(object@Itrigger),
                                   icesAdvice::icesRound(object@Itrigger)),
                            ifelse(!is.na(object@units), 
                                   paste0(" ", object@units), ""))
    val_b <- icesAdvice::icesRound(object@value)
    
    txt_I_full <- paste0(format(txt_I, width = 48), " | ",
                         format(val_I, width = 29, justify = "right"),
                         "\n")
    ### don't show index value for chr rule 
    ### (already shown above in same table)
    if (identical(object@hcr, "chr")) txt_I_full <- NULL
    
    txt <- paste0(txt,
                  txt_I_full,
                  paste0(format(txt_I_trigger, width = 48), " | ",
                         format(val_I_trigger, width = 29, justify = "right"),
                         "\n"),
                  paste0(format(txt_b1, width = 48), " | ",
                         format(val_b, width = 29, justify = "right"),
                         "\n",
                         format(paste0("   ", txt_b2), width = 48), " | ",
                         format("", width = 29, justify = "right"),
                         "\n")
    )
    
    cat(txt)
  }
)

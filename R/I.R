#' @include generics.R
#' @importFrom icesAdvice icesRound
NULL

### ------------------------------------------------------------------------ ###
### I class ####
### ------------------------------------------------------------------------ ###

#' @title I-class
#' 
#' @description  An S4 class to represent component I (the current biomass 
#' index value) of the chr rule.
#' 
#' This class (\code{I}) stores the input for component I
#' as well as the resulting I value. 
#' 
#' @slot value The value of component I
#' @slot lag The time lag (in years) between the last available index value and the value to be used.
#' @slot n_years \code{numeric}. The number of years used for the index value.
#' @slot idx \code{data.frame}. A \code{data.frame} with the index values.
#' @slot yr_last \code{numeric}. The last year with index data.
#' @slot units \code{character}. The units of the biomass index, e.g. 'kg/hr'.
#' @slot hcr \code{character}. The harvest control rule (hcr) for which the index is used. Only applicable to 'chr'.
#' 
#' @name I-class
#' @title I
#' @export
setClass(
  Class = "I",
  slots = c(
    value = "numeric",
    lag = "numeric",
    n_yrs = "numeric",
    idx = "data.frame",
    yr_last = "numeric",
    units = "character",
    hcr = "character"
  ),
  prototype = list(
    value = NA_real_,
    lag = 0,
    n_yrs = 1,
    idx = data.frame(matrix(
      ncol = 2,
      nrow = 0,
      dimnames = list(NULL, c("year", "index"))
    )),
    yr_last = NA_real_,
    units = NA_character_,
    hcr = "chr"
  )
)


#' @rdname I-class
setClass(Class = "chr_I", 
         contains = "I",
         prototype = list(hcr = "chr"))

### ------------------------------------------------------------------------ ###
### I methods ####
### ------------------------------------------------------------------------ ###
#' chr rule - component I (biomass index value)
#'
#' This function calculates component I (the last biomass index value) of the chr rule. The index needs to be a biomass index without age structure.
#' 
#' See ICES (2022) for the full definition definition.
#'
#' Usually, this method is used by providing only a biomass index, e.g. as a \code{data.frame}. The method uses this index, and takes the last index value.
#'
#' \code{chr_I()} is an alias for \code{I()} with identical arguments and functionality.
#'
#' @param object The biomass index. Can be a \code{data.frame} with columns 'data' and 'index', a vector, or a single value.
#' @param lag Optional. The time lag (in years) between the last available index value and the value to be used. Defaults to 0 (the last value is used).
#' @param n_yrs Optional. The number of years if an average index value is used. Defaults to 1 (use last year's value only).
#' @param units Optional. The units of the biomass index, e.g. 'kg/hr'. Only used for plotting.
#' @param hcr Optional. Should be 'chr'.
#' @param ... Additional arguments. Not used.
#'  
#' @section Warning:
#' For application in ICES, do not change the defaults unless the change is supported by stock-specific simulations.
#'
#' @references 
#' ICES. 2022. ICES technical guidance for harvest control rules and stock assessments for stocks in categories 2 and 3. In Report of ICES Advisory Committee, 2022. ICES Advice 2022, Section 16.4.11, 20 pp. \url{https://doi.org/10.17895/ices.advice.19801564}.
#' 
#' Fischer, S. H., De Oliveira, J. A. A., Mumford, J. D., and Kell, L. T. 2023. Risk equivalence in data‐limited and data‐rich fisheries management: An example based on the ICES advice framework. Fish and Fisheries, 24: 231--247. \url{https://doi.org/10.1111/faf.12722}.
#' 
#' Fischer, S. H., De Oliveira, J. A. A., Mumford, J. D., and Kell, L. T. 2022. Exploring a relative harvest rate strategy for moderately data-limited fisheries management. ICES Journal of Marine Science, 79: 1730--1741. \url{https://doi.org/10.1093/icesjms/fsac103}.
#'
#' @return An object of class \code{I} with the value of the biomass 
#' index
#'
#' @examples
#' # Use a data.frame with index values
#' df_idx <- data.frame(year = 2017:2021,
#'                      index = c(1.33, 1.13, 0.84, 0.60, 1.03))
#' I <- I(df_idx)
#' I
#' advice(I)
#' 
#' 
#' # If only the value of the last biomass index is known
#' I(1)
#' 
#' # ple7e example data
#' data(ple7e_idx)
#' I <- I(ple7e_idx)
#' advice(I)
#' plot(I)
#' 
#' @export
setGeneric(name = "I", 
           def = function(object, lag = 0, n_yrs = 1, units, 
                          hcr = "chr", ...) 
             standardGeneric("I"),
           signature = c("object"))

### data.frame -> use as index
#' @rdname I
#' @usage NULL
#' @export
setMethod(I, 
          signature = c(object = "data.frame"),
          function(object, lag = 0, n_yrs = 1, units, hcr = "chr", ...) {
  idx <- object
  names(idx) <- tolower(names(idx))
  ### check if "index" column exists
  if (isFALSE("index" %in% names(idx))) {
    if (identical(ncol(idx), 2L)) {
      message(paste0("Column \"index\" missing in idx. Using column ",
                     "\"", names(idx)[2], "\" instead"))
    } else {
      stop("Column \"index\" missing in idx")
    }
  }
  ### check if "year" column exists
  if (isFALSE("year" %in% names(idx)))
    idx$years <- seq_along(idx$index)
  I_calc(idx = idx, lag = lag, n_yrs = n_yrs, units = units, 
              hcr = hcr, ...)
})

### vector -> use as index
#' @rdname I
#' @usage NULL
#' @export
setMethod(I, 
          signature = c(object = "vector"),
          function(object, lag = 0, n_yrs = 1, units, hcr = "chr", ...) {
  idx <- data.frame(index = object, years = NA)
  ### use names, if provided
  if (!is.null(names(object)))
    idx$years <- names(object)
  ### pass to data.frame method
  I(object = idx, lag = lag, n_yrs = n_yrs, units = units, 
         hcr = hcr, ... = ...)
})

### I -> validate & update
#' @rdname I
#' @usage NULL
#' @export
setMethod(I, 
          signature = c(object = "I"),
          function(object, lag = object@lag, n_yrs = object@n_yrs, 
                   units = object@units, hcr = object@hcr, ...) {#browser()
  validObject(object)
  ### update slots, if provided
  if (!missing(lag)) object@lag <- lag
  if (!missing(n_yrs)) object@n_yrs <- n_yrs
  if (!missing(units)) object@units <- units
  if (!missing(hcr)) object@hcr <- hcr
  ### pass to function
  I_calc(object = object, idx = object@idx, lag = object@lag, 
         n_yrs = object@n_yrs, units = object@units, 
         hcr = object@hcr, ... = ...)
})

### ------------------------------------------------------------------------ ###
### I calculation ####
### ------------------------------------------------------------------------ ###
I_calc <- function(object, idx, lag = 0, n_yrs = 1, units, 
                        hcr = "chr", ...) {
  ### create empty I object, if missing
  if (missing(object)) object <- new("I")
  if (!missing(hcr)) {
    hcr <- match.arg(hcr, choices = c("chr"))
    object@hcr <- hcr
  }
  
  ### add/update index, if provided
  if (!missing(idx)) {
    object@idx <- idx
  }
  
  ### add/update parameters, if provided
  if (!missing(lag)) object@lag <- lag
  if (!missing(n_yrs)) object@n_yrs <- n_yrs
  
  if (!missing(units)) object@units <- units
  
  if (isTRUE(length(object@idx) < 1)) {
    
    warning("Empty index provided, cannot get index value!")
    
  } else {
    
    ### find last data year
    object@yr_last <- tail(object@idx$year, 1)
    
    ### determine years to use
    yrs_use <- seq(from = object@yr_last - object@lag - object@n_yrs + 1, 
                   to = object@yr_last - object@lag)
    ### estimate mean index over these years
    object@value <- mean(object@idx$index[object@idx$year %in% yrs_use],
                         na.rm = TRUE)

  }
  
  return(object)
}

### ------------------------------------------------------------------------ ###
### I aliases ####
### ------------------------------------------------------------------------ ###

### chr
#' @rdname I
#' @export
setGeneric(name = "chr_I", 
           def = function(object, lag = 0, n_yrs = 1, units, 
                          hcr = "chr", ...) 
             standardGeneric("chr_I"),
           signature = c("object"))
#' @rdname I
#' @usage NULL
#' @export
setMethod(chr_I, 
          signature = c(object = "ANY"),
          function(object, lag = 0, n_yrs = 1, units, 
                   hcr = "chr", ...) {
  hcr <- match.arg(hcr)
  object <- I(object = object, 
                   lag = lag, n_yrs = n_yrs, 
                   units = units, hcr = hcr, ... = ...)
  class(object) <- "chr_I"
  return(object)
})

### ------------------------------------------------------------------------ ###
### I validity ####
### ------------------------------------------------------------------------ ###
### validity checks
setValidity("I", function(object) {
  if (!identical(length(object@value), 1L)) {
    "slot value must be of length 1"
  } else if (!identical(length(object@lag), 1L)) {
    "slot lag must be of length 1"
  } else if (!identical(length(object@n_yrs), 1L)) {
    "slot n_yrs must be of length 1"
  } else if (!all(c("year", "index") %in% names(object@idx))) {
    "data.frame in slot idx must contain columns 'year' and 'index'"
  } else if (!identical(length(object@yr_last), 1L)) {
    "slot yr_last must be of length 1"
  } else if (!identical(length(object@units), 1L)) {
    "slot units must be of length 1"
  } else if (!identical(length(object@hcr), 1L)) {
    "slot hcr must be of length 1"
  } else {
    TRUE
  }
})

### ------------------------------------------------------------------------ ###
### convenience methods ####
### ------------------------------------------------------------------------ ###

#' @rdname value
#' @export
setMethod(f = "value", signature = "I",
          definition = function(object) {
            return(object@value)
          })

### print
setMethod(f = "print", signature = "I", 
          definition = function(x) {
            cat(paste0("An object of class \"", class(x), "\".\n",
                       "Value: ", x@value, "\n"))
          })

### show
setMethod(f = "show", signature = "I", 
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
  f = "advice", signature = "I",
  definition = function(object) {
    txt <- paste0(paste(rep("-", 80), collapse = ""), "\n",
                  "Biomass index\n",
                  paste(rep("-", 80), collapse = ""), "\n")
    
    I_last_year <- ifelse(!is.na(object@yr_last), object@yr_last, "last")
    if (isTRUE(object@n_yrs > 1))
      I_last_year <- paste0(I_last_year - object@n_yrs + 1, "-", I_last_year)
    txt_I <- paste0("I: most recent biomass index (I", I_last_year, ")")
    
    val_I <- paste0(ifelse(object@value > 100, 
                           round(object@value),
                           icesAdvice::icesRound(object@value)),
                    ifelse(!is.na(object@units), paste0(" ", object@units), ""))
    
    txt <- paste0(txt,
                  paste0(format(txt_I, width = 48), " | ",
                         format(val_I, width = 29, justify = "right"),
                         "\n"))
    
    cat(txt)
  }
)

#' @include generics.R
#' @importFrom icesAdvice icesRound
NULL

### ------------------------------------------------------------------------ ###
### r class ####
### ------------------------------------------------------------------------ ###

#' @title r-class
#' 
#' @description  An S4 class to represent component r of the rfb and rb rules.
#' 
#' This class (\code{r}) stores the input for component r (the index ratio
#' ) as well as the resulting r value. 
#' 
#' @slot value The value of component r
#' @slot n0,n1,n2 Parameters for the calculation of the r component. See \code{?rfb_r} for details.
#' @slot yr_last \code{numeric}. The last year with index data.
#' @slot n1_yrs,n2_yrs \code{numeric}. The years used for \code{n1} and \code{n2}.
#' @slot n1_mean,n2_mean \code{numeric}. The mean index values over \code{n1_yrs} and \code{n2_yrs}.
#' @slot idx \code{data.frame}. A \code{data.frame} with the index values.
#' @slot units \code{character}. The units of the biomass index, e.g. 'kg/hr'.
#' @slot hcr \code{character}. The harvest control rule (hcr) for which the biomass safeguard is used. One of 'rfb' or 'rb'.
#' 
#' @name r-class
#' @title r
#' @export
setClass(Class = "r", 
         slots = c(value = "numeric",
                   n0 = "numeric", n1 = "numeric", n2 = "numeric",
                   yr_last = "numeric",
                   n1_yrs = "numeric",
                   n2_yrs = "numeric",
                   n1_mean = "numeric",
                   n2_mean = "numeric",
                   idx = "data.frame",
                   units = "character",
                   hcr = "character"),
         prototype = list(value = NA_real_, 
                          n0 = 0, n1 = 2, n2 = 3,
                          n1_yrs = NA_real_, n2_yrs = NA_real_,
                          n1_mean = NA_real_, n2_mean = NA_real_,
                          units = NA_character_,
                          hcr = NA_character_))
#' @rdname r-class
setClass(Class = "rfb_r", 
         contains = "r",
         prototype = list(hcr = "rfb"))
#' @rdname r-class
setClass(Class = "rb_r", 
         contains = "r",
         prototype = list(hcr = "rb"))
### ------------------------------------------------------------------------ ###
### r methods ####
### ------------------------------------------------------------------------ ###

#' rfb/rb rule - component r (index ratio)
#'
#' This function calculates component r (the index ratio) of the rfb and rb
#'  rule. The index needs to be a biomass index without age structure.
#' 
#' Usually, this method is used by providing only a biomass index, e.g. 
#' as a \code{data.frame}. The default index ratio is the average of the last
#' two index values, divided by the average of the three preceeding index 
#' values.
#' 
#' The index ratio is identical in the rfb and rb rules. 
#' \code{rfb_r()} and \code{rb_b()} are aliases for 
#' \code{r()} with identical arguments and functionality.
#'
#' @param object The biomass index. Can be a \code{data.frame} with columns 'data' and 'index' or an \code{FLQuant} object defined by \code{FLCore}.
#' @param n0 Optional. Time lag between the last index year and the last year to be used. By default, the last index year is used (\code{n0=0})
#' @param n1 Optional. Number of years used in the numerator of the r component. Defaults to 2 (i.e. \code{n1} and \code{n2} use a 2 over 3 ratio).
#' @param n2 Optional. Number of years used in the denominator of the r component. Defaults to 3.
#' @param units Optional. The units of the biomass index, e.g. 'kg/hr'. Only used for plotting.
#' @param hcr Optional. One of 'rfb' or 'rb'.
#' @param ... Additional arguments. Not used.
#'  
#' @section Warning:
#' For application in ICES, do not change the defaults (\code{n0}, \code{n1}, \code{n2}) unless the change is supported by stock-specific simulations.
#'
#' @references 
#' ICES. 2022. ICES technical guidance for harvest control rules and stock assessments for stocks in categories 2 and 3. In Report of ICES Advisory Committee, 2022. ICES Advice 2022, Section 16.4.11, 20 pp. \url{https://doi.org/10.17895/ices.advice.19801564}.
#' 
#' Fischer, S. H., De Oliveira, J. A. A., Mumford, J. D., and Kell, L. T. 2023. Risk equivalence in data‐limited and data‐rich fisheries management: An example based on the ICES advice framework. Fish and Fisheries, 24: 231--247. \url{https://doi.org/10.1111/faf.12722}.
#' 
#' Fischer, S. H., De Oliveira, J. A. A., Mumford, J. D., and Kell, L. T. 2021. Application of explicit precautionary principles in data-limited fisheries management. ICES Journal of Marine Science, 78: 2931--2942. \url{https://doi.org/10.1093/icesjms/fsab169}.
#' 
#' Fischer, S. H., De Oliveira, J. A. A., Mumford, J. D., and Kell, L. T. 2021. Using a genetic algorithm to optimize a data-limited catch rule. ICES Journal of Marine Science, 78: 1311--1323. \url{https://doi.org/10.1093/icesjms/fsab018}.
#' 
#' Fischer, S. H., De Oliveira, J. A. A., and Kell, L. T. 2020. Linking the performance of a data-limited empirical catch rule to life-history traits. ICES Journal of Marine Science, 77: 1914--1926. \url{https://doi.org/10.1093/icesjms/fsaa054}.
#'
#'
#' @return An object of class \code{r}
#'
#' @examples
#' # If the value of r is known
#' rfb_r(0.9)
#' r(0.9)
#' 
#' # Use a data.frame with index values
#' df_idx <- data.frame(year = 2017:2021,
#'                      index = c(1.33, 1.13, 0.84, 0.60, 1.03))
#' r <- r(df_idx)
#' r
#' advice(r)
#' 
#' # plot
#' plot(r(df_idx, units = "kg/hr"))
#' 
#' @export
setGeneric(name = "r", 
           def = function(object, n0, n1, n2, units, hcr, ...) 
             standardGeneric("r"),
           signature = c("object"))

### FLQuant -> convert to data.frame
# #' @rdname r
# setMethod(r, 
#           signature = c(object = "FLQuant"), 
#           function(object, n0, n1, n2, units, hcr, ...) {
#   ### convert FLQuant into data.frame
#   idx <- as.data.frame(object)[, c("year", "data")]
#   names(idx)[2] <- "index"
#   r(idx, n0 = n0, n1 = n1, n2 = n2, units = units, hcr = hcr,
#          ...)
# })
### data.frame -> use as index
#' @rdname r
#' @usage NULL
#' @export
setMethod(r, 
          signature = c(object = "data.frame"),
          function(object, n0, n1, n2, units, hcr, ...) {
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
  r_calc(idx = idx, n0 = n0, n1 = n1, n2 = n2, units = units, 
              hcr = hcr, ...)
})
### numeric -> use as ratio
#' @rdname r
#' @usage NULL
#' @export
setMethod(r, 
          signature = c(object = "numeric"), 
          function(object, ...) {
  
  ### create empty r object
  res <- new("r")
  
  ### remove parameters
  res@n0 <- NA_real_
  res@n1 <- NA_real_
  res@n2 <- NA_real_
  
  ### insert value
  res@value <- object
  
  return(res)
})
### r -> check validity and update values if necessary
#' @rdname r
#' @usage NULL
#' @export
setMethod(r, 
          signature = c(object = "r"), 
          function(object, n0, n1, n2, units, hcr, ...) {
  ### check validity
  validObject(object)
  ### run r() to update slots and recalculate if needed
  r_calc(object, n0 = n0, n1 = n1, n2 = n2, units = units, hcr = hcr, ...)
})

### ------------------------------------------------------------------------ ###
### aliases ####
### ------------------------------------------------------------------------ ###
### define aliases rfb_r and rb_r for r
### set object signature to ANY and let r deal with method dispatch

### rfb_r
#' @rdname r
#' @export
setGeneric(name = "rfb_r", 
           def = function(object, n0, n1, n2, units, hcr = "rfb", ...) 
             standardGeneric("rfb_r"),
           signature = c("object"))
#' @rdname r
#' @usage NULL
#' @export
setMethod(rfb_r, 
          signature = c(object = "ANY"),
          function(object, n0, n1, n2, units, hcr = "rfb", ...) {
  hcr <- match.arg(hcr)
  object <- r(object = object, n0 = n0, n1 = n1, n2 = n2, units = units,
                   hcr = hcr, ... = ...)
  class(object) <- "rfb_r"
  return(object)
})
### rb_r
#' @rdname r
#' @export
setGeneric(name = "rb_r", 
           def = function(object, n0, n1, n2, units, hcr = "rb", ...) 
             standardGeneric("rb_r"),
           signature = c("object"))
#' @rdname r
#' @usage NULL
#' @export
setMethod(rb_r, 
          signature = c(object = "ANY"),
          function(object, n0, n1, n2, units, hcr = "rb", ...) {
  hcr <- match.arg(hcr)
  object <- r(object = object, n0 = n0, n1 = n1, n2 = n2, units = units,
                   hcr = hcr, ... = ...)
  class(object) <- "rb_r"
  return(object)
})

### ------------------------------------------------------------------------ ###
### validity checks ####
### ------------------------------------------------------------------------ ###
setValidity("r", function(object) {
  if (any(c(length(object@n0), length(object@n0), length(object@n0)) != 1)) {
    "n0, n1, and n2 must each be of length 1"
  } else if (!identical(length(object@value), 1L)) {
    "value must be of length 1"
  } else if (!identical(length(object@units), 1L)) {
    "units must be of length 1"
  } else {
    TRUE
  }
})


### ------------------------------------------------------------------------ ###
### r calculation ####
### ------------------------------------------------------------------------ ###
r_calc <- function(object, idx, n0, n1, n2, units, hcr) {
  ### create empty rfb_r object, if missing
  if (missing(object)) object <- new("rfb_r")
  if (!missing(hcr)) {
    hcr <- match.arg(hcr, choices = c("rfb", "rb"))
    object@hcr <- hcr
  }
  
  ### add/update index, if provided
  if (!missing(idx)) object@idx <- idx
  
  ### add/update parameters, if provided
  if (!missing(n0)) object@n0 <- n0
  if (!missing(n1)) object@n1 <- n1
  if (!missing(n2)) object@n2 <- n2
  
  if (!missing(units)) object@units <- units
  
  if (isTRUE(length(object@idx) < 1)) {
    
    warning("Empty index provided, cannot calculate/update ratio!")
    
  } else {
    
    ### find last data year
    object@yr_last <- tail(object@idx$year, 1)
    
    ### determine years to use
    object@n1_yrs <- 
      seq(from = object@yr_last - object@n0 - object@n1 + 1, 
          to = object@yr_last - object@n0)
    object@n2_yrs <- 
      seq(from = object@yr_last - object@n0 - object@n1 - object@n2 + 1, 
          to = object@yr_last - object@n0 - object@n1)
    ### estimate mean index over these years
    object@n1_mean <- 
      mean(object@idx$index[object@idx$year %in% object@n1_yrs], 
           na.rm = TRUE)
    object@n2_mean <- 
      mean(object@idx$index[object@idx$year %in% object@n2_yrs], 
           na.rm = TRUE)
    ### calculate index ratio
    object@value <- object@n1_mean/object@n2_mean
  }
  
  return(object)
}

### ------------------------------------------------------------------------ ###
### convenience methods ####
### ------------------------------------------------------------------------ ###

#' @rdname summary
#' @export
setMethod(f = "summary", signature = "r", 
  definition = function(object, ...) {
    txt <- (paste0(paste(rep("-", 50), collapse = ""), "\n",
               "component r:\n",
               "last index year: ", object@yr_last, "\n",
               "using a ", object@n1, " over ", object@n2, " ratio ",
               "with a time lag of n0=", object@n0, "\n",
               "index average (", paste(object@n1_yrs, collapse = ","), 
               ") = ",
               object@n1_mean, "\n",
               "index average (", paste(object@n2_yrs, collapse = ","), 
               ") = ",
               object@n2_mean, "\n",
               "ratio r = ", object@value, "\n",
               paste0(paste(rep("-", 50), collapse = ""))))
    cat(txt)
})

#' @rdname value
#' @export
setMethod(f = "value", signature = "r", 
          definition = function(object) {
  return(object@value)
})

#' @rdname value
#' @export
setMethod(f = "value", signature = "b", 
          definition = function(object) {
            return(object@value)
          })

### print
setMethod(f = "print", signature = "r", 
          definition = function(x) {
            cat(paste0("An object of class \"", class(x), "\".\n",
                       "Value: ", x@value, "\n"))
})

### show
setMethod(f = "show", signature = "r", 
          definition = function(object) {
            cat(paste0("An object of class \"", class(object), "\".\n",
                       "Value: ", object@value, "\n"))
})

# 
# ### shows which methods are used (sequentially if neccessary)
# sloop::s3_dispatch()
# 
# library(FLCore)
# sloop::s3_dispatch(print(FLQuant(1)))
# 
# 

### ------------------------------------------------------------------------ ###
### ICES advice style table ####
### ------------------------------------------------------------------------ ###
#' @rdname advice
#' @usage NULL
#' @export
setMethod(
  f = "advice", signature = "r",
  definition = function(object) {
    txt <- paste0(paste(rep("-", 80), collapse = ""), "\n",
                  "Stock biomass trend\n",
                  paste(rep("-", 80), collapse = ""), "\n")

    txt_A <- paste0("Index A (", paste0(object@n1_yrs, collapse = ","), ")")
    txt_B <- paste0("Index B (", paste0(object@n2_yrs, collapse = ","), ")")
    txt_r <- paste0("r: stock biomass trend (index ratio A/B)")
    
    val_A <- paste0(icesAdvice::icesRound(object@n1_mean), 
                    ifelse(!is.na(object@units), paste0(" ", object@units), ""))
    val_B <- paste0(icesAdvice::icesRound(object@n2_mean), 
                    ifelse(!is.na(object@units), paste0(" ", object@units), ""))
    val_r <- icesAdvice::icesRound(object@value)
    
    txt <- paste0(txt,
                  paste0(format(txt_A, width = 48), " | ",
                         format(val_A, width = 29, justify = "right"),
                         "\n"),
                  paste0(format(txt_B, width = 48), " | ",
                         format(val_B, width = 29, justify = "right"),
                         "\n"),
                  paste0(format(txt_r, width = 48), " | ",
                         format(val_r, width = 29, justify = "right"),
                         "\n")
                  )
    #txt <- paste0(txt, paste(rep("-", 80), collapse = ""), "\n")
    
    cat(txt)
  }
)

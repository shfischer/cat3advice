#' @include generics.R
#' @include length_data.R
#' @importFrom icesAdvice icesRound
NULL

### ------------------------------------------------------------------------ ###
### f class ####
### ------------------------------------------------------------------------ ###
#' @title f-class
#' 
#' @description  An S4 class to represent component f of the rfb rule.
#'
#' This class (\code{f}) stores the input for component f (the length 
#' indicator as well as the resulting f value.
#'
#' @slot value The value of component f
#' @slot indicator Length indicator time series
#' @slot yr_last \code{numeric}. The last year with data.
#' @slot years Years with data.
#' @slot Lmean Mean catch length.
#' @slot Lref Reference catch length.
#' @slot units \code{character}. The units of the biomass index, e.g. 'kg/hr'.
#' @slot hcr \code{factor}. The harvest control rule (hcr) for which component f is used (rfb).
#'
#' @name f-class
#' @export
setClass(
  Class = "f",
  slots = c(
    value = "numeric",
    indicator = "data.frame",
    yr_last = "numeric",
    years = "numeric",
    Lmean = "Lmean",
    Lref = "Lref",
    units = "character",
    hcr = "character"
  ),
  prototype = list(
    value = NA_real_,
    indicator = data.frame(matrix(
      ncol = 4, nrow = 0,
      dimnames = list(NULL, c(
        "year", "indicator", "Lmean", "Lref"
      ))
    )),
    yr_last = NA_integer_,
    years = NA_integer_,
    Lmean = new("Lmean"),
    Lref = new("Lref"),
    units = NA_character_,
    hcr = NA_character_
  )
)
#' @rdname f-class
setClass(
  Class = "rfb_f",
  contains = "f",
  prototype = list(hcr = "rfb"
  )
)

### ------------------------------------------------------------------------ ###
### f methods ####
### ------------------------------------------------------------------------ ###
#' rfb rule - component f (fishing pressure proxy, length indicator)
#'
#' This function calculates component f (the fishing pressure proxy, derived from a length indicator ) of the rfb rule. 
#' 
#' The value is calculated by comparing the mean catch length (above length of first capture Lc) to a reference length.
#'
#' \code{rfb_f()} is an alias for
#' \code{f()} with identical arguments and functionality.
#'
#' @param object Optional. An object of class \code{f}.
#' @param Lmean The mean catch length. Either a \code{data.frame} with columns
#'              'year' and 'Lmean' or an object of class \code{Lmean}.
#' @param Lref The reference length. Either a \code{numeric} with the value or 
#'             an object of class \code{Lref}.
#'  
#' ...
#'
#' @param units Optional. The units of the length dat, e.g. 'cm'. Only used for plotting.
#' @param hcr Optional. Defaults to 'rfb'.
#' @param ... Additional arguments. Not currently used.
#'
#' @section Note:
#' The reference length Lref should be kept constant for all years unless there 
#' a substantial changes in the fishery or fishery selectivity.
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
#' @return An object of class \code{f} with the length indicator value(s).
#' 
#' @export
setGeneric(
  name = "f",
  def = function(object, Lmean, Lref, units, hcr, ...) {
    standardGeneric("f")
  },
  signature = c("object", "Lmean", "Lref")
)

### Lmean = data.frame; Lref = numeric
### Lmean = data.frame; Lref = data.frame
### Lmean = Lmean; Lref = numeric
### Lmean = Lmean; Lref = data.frame
### Lmean = Lmean; Lref = Lref

### object = missing; Lmean = Lmean; Lref = Lref
#' @rdname f
#' @usage NULL
#' @export
setMethod(f,
  signature = c(object = "missing", Lmean = "Lmean", Lref = "Lref"),
  function(object, Lmean, Lref, units, hcr, ...) {
    
    object <- new("f")
    object@Lmean <- Lmean
    object@Lref <- Lref
    if (!missing(units)) object@units <- units
    if (!missing(hcr)) object@hcr <- hcr
    
    ### get mean length data
    object@indicator <- Lmean@summary
    ### add column with reference length
    if (is.na(Lref@years)) {
      object@indicator$Lref <- Lref@value
    } else {
      # do something with annual values...
    }
    ### calculate Lmean/Lref ratio
    object@indicator$indicator <- object@indicator$Lmean/object@indicator$Lref
    
    ### years
    object@years <- object@indicator$year
    object@yr_last <- tail(object@years, 1)
    
    ### indicator value
    object@value <- tail(object@indicator$indicator, 1)
    
    return(object)
  }
)

### f -> check validity
#' @rdname f
#' @usage NULL
#' @export
setMethod(f,
  signature = c(object = "f", Lmean = "missing", Lref = "missing"),
  function(object, Lmean = object@Lmean, Lref = object@Lmean, 
           units, hcr, ...) {
    ### check validity
    validObject(object)
    
    return(object)
  }
)

### numeric -> use as value
#' @rdname f
#' @usage NULL
#' @export
setMethod(f,
          signature = c(object = "numeric", Lmean = "missing", Lref = "missing"),
          function(object, Lmean = object@Lmean, Lref = object@Lmean, 
                   units, hcr, ...) {
  ### empty object with value
  value <- object
  object <- new("f")
  object@value <- value
  if (!missing(units)) object@units <- units
  if (!missing(hcr)) object@hcr <- hcr
  return(object)
})

### ------------------------------------------------------------------------ ###
### aliases ####
### ------------------------------------------------------------------------ ###
### define aliases rfb_f
### set object signature to ANY and let f deal with method dispatch

### rfb_f
#' @rdname f
#' @usage NULL
#' @export
setGeneric(
  name = "rfb_f",
  def = function(object, Lmean, Lref, units, hcr = "rfb", ...) {
    standardGeneric("rfb_f")
  },
  signature = c("object", "Lmean", "Lref")
)
#' @rdname f
#' @usage NULL
#' @export
setMethod(rfb_f,
          signature = c(object = "missing", Lmean = "ANY", Lref = "ANY"),
          function(object, Lmean, Lref, units, hcr = "rfb", ...) {
  hcr <- match.arg(hcr)
  ### ignore object because it is missing
  object <- f(Lmean = Lmean, Lref = Lref, units = units, 
                   hcr = hcr, ... = ...)
  class(object) <- "rfb_f"
  return(object)
})
### f -> check validity
#' @rdname f
#' @usage NULL
#' @export
setMethod(rfb_f,
          signature = c(object = "ANY", Lmean = "missing", Lref = "missing"),
          function(object, Lmean, Lref, units, hcr = "rfb", ...) {
  hcr <- match.arg(hcr)
  ### ignore Lmean & Lref becuase they are missing
  object <- f(object = object, units = units,
                   hcr = hcr, ... = ...)
  class(object) <- "rfb_f"
  return(object)
})

### ------------------------------------------------------------------------ ###
### f validity ####
### ------------------------------------------------------------------------ ###
### validity checks

### TO DO

# setValidity("r", function(object) {
#   if (any(c(length(object@n0), length(object@n0), length(object@n0)) != 1)) {
#     "n0, n1, and n2 must each be of length 1"
#   } else if (!identical(length(object@value), 1L)) {
#     "value must be of length 1"
#   } else if (!identical(length(object@units), 1L)) {
#     "units must be of length 1"
#   } else {
#     TRUE
#   }
# })

### ------------------------------------------------------------------------ ###
### f convenience methods ####
### ------------------------------------------------------------------------ ###

#' @rdname summary
#' @export
setMethod(
  f = "summary", signature = "f",
  definition = function(object) {
    txt <- (paste0(
      paste(rep("-", 50), collapse = ""), "\n",
      "component f:\n",
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
      paste0(paste(rep("-", 50), collapse = ""))
    ))
    cat(txt)
  }
)

#' @rdname value
#' @export
setMethod(
  f = "value", signature = "f",
  definition = function(object) {
    return(object@value)
  }
)

### print
setMethod(f = "print", signature = "f", 
          definition = function(x) {
            cat(paste0("An object of class \"", class(x), "\".\n",
                       "Value: ", x@value, "\n"))
})

### show
setMethod(f = "show", signature = "f", 
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
  f = "advice", signature = "f",
  definition = function(object) {
    txt <- paste0(
      paste(rep("-", 80), collapse = ""), "\n",
      "Fishing pressure proxy\n",
      paste(rep("-", 80), collapse = ""), "\n"
    )

    txt_Lmean <- paste0("Mean catch length (Lmean = L", object@yr_last, ")")
    txt_Lref <- paste0("MSY proxy length (", object@Lref@basis, ")")
    txt_f1 <- paste0("f: Fishing pressure proxy relative to MSY proxy")
    txt_f2 <- paste0("   (L", object@yr_last, "/", object@Lref@basis, ")")

    val_Lmean <- paste0(
      icesAdvice::icesRound(object@Lmean@value[as.character(object@yr_last)]),
      ifelse(!is.na(object@units), paste0(" ", object@units), "")
    )
    val_Lref <- paste0(
      icesAdvice::icesRound(object@Lref@value),
      ifelse(!is.na(object@units), paste0(" ", object@units), "")
    )
    val_f <- icesAdvice::icesRound(object@value)

    txt <- paste0(
      txt,
      paste0(
        format(txt_Lmean, width = 48), " | ",
        format(val_Lmean, width = 29, justify = "right"),
        "\n"
      ),
      paste0(
        format(txt_Lref, width = 48), " | ",
        format(val_Lref, width = 29, justify = "right"),
        "\n"
      ),
      paste0(format(txt_f1, width = 48), " | \n"),
      paste0(
        format(txt_f2, width = 48), " | ",
        format(val_f, width = 29, justify = "right"),
        "\n"
      )
    )
    #txt <- paste0(txt, paste(rep("-", 80), collapse = ""), "\n")

    cat(txt)
  }
)
### advice - rfb_f
#' @rdname advice
#' @usage NULL
#' @export
setMethod(
  f = "advice", signature = "rfb_f",
  definition = function(object) {
    txt <- callNextMethod()
    cat(txt)
  })













#' @include generics.R
#' @include length_data.R

### ------------------------------------------------------------------------ ###
### comp_f class ####
### ------------------------------------------------------------------------ ###
#' An S4 class to represent component f of the rfb rule.
#'
#' This class (\code{comp_f}) stores the input for component f (the length 
#' indicator as well as the resulting f value.
#'
#' @slot value The value of component f
#' @slot indicator Length indicator time series
#' @slot yr_last \code{numeric}. The last year with data.
#' @slot years Years with data.
#' @slot Lmean Mean catch length.
#' @slot Lref Reference catch length.
#' @slot units \code{character}. The units of the biomass index, e.g. 'kg/hr'.
#' @slot catch_rule \code{factor}. The catch rule for which component f is used (rfb).
#'
#' @rdname comp_f-class
#' @export
setClass(
  Class = "comp_f",
  slots = c(
    value = "numeric",
    indicator = "data.frame",
    yr_last = "numeric",
    years = "numeric",
    Lmean = "Lmean",
    Lref = "Lref",
    units = "character",
    catch_rule = "factor"
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
    catch_rule = factor(NA_character_,
      levels = c("rfb")
    )
  )
)
#' @rdname comp_f-class
setClass(
  Class = "rfb_f",
  contains = "comp_f",
  prototype = list(catch_rule = factor("rfb",
    levels = c("rfb")
  ))
)

### ------------------------------------------------------------------------ ###
### comp_f methods ####
### ------------------------------------------------------------------------ ###
#' rfb rule - component f (fishing pressure proxy, length indicator)
#'
#' This function calculates component f (the fishing pressure proxy, derived from a length indicator ) of the rfb rule. 
#' 
#' The value is calculated by comparing the mean catch length (above length of first capture Lc) to a reference length.
#'
#' \code{rfb_f()} is an alias for
#' \code{comp_f()} with identical arguments and functionality.
#'
#' @param Lmean The mean catch length. Either a \code{data.frame} with columns
#'              'year' and 'Lmean' or an object of class \code{Lmean}.
#' @param Lref The reference length. Either a \code{numeric} with the value or 
#'             an object of class \code{Lref}.
#'  
#' ...
#'
#' @param units Optional. The units of the length dat, e.g. 'cm'. Only used for plotting.
#' @param catch_rule Optional. Defaults to 'rfb'.
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
#' @return An object of class \code{comp_f} with the length indicator value(s).
#'
#' @examples
#' 
#' @export
setGeneric(
  name = "comp_f",
  def = function(data, Lmean, Lref, units, catch_rule, ...) {
    standardGeneric("comp_f")
  },
  signature = c("data", "Lmean", "Lref")
)

### Lmean = data.frame; Lref = numeric
### Lmean = data.frame; Lref = data.frame
### Lmean = Lmean; Lref = numeric
### Lmean = Lmean; Lref = data.frame
### Lmean = Lmean; Lref = Lref

### data = missing; Lmean = Lmean; Lref = Lref
#' @rdname comp_f
setMethod(comp_f,
  signature = c(data = "missing", Lmean = "Lmean", Lref = "Lref"),
  function(data, Lmean, Lref, units, catch_rule, ...) {
    
    Lmean <- lmean
    Lref <- lref
    
    object <- new("comp_f")
    object@Lmean <- Lmean
    object@Lref <- Lref
    if (!missing(units)) object@units <- units
    if (!missing(catch_rule)) object@catch_rule <- catch_rule
    
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


### data.frame -> use as index
#' @rdname comp_r
# setMethod(comp_r,
#   signature = c(object = "data.frame"),
#   function(object, n0, n1, n2, units, catch_rule, ...) {
#     idx <- object
#     names(idx) <- tolower(names(idx))
#     ### check if "year" column exists
#     if (isFALSE("year" %in% names(idx))) {
#       stop("Column \"year\" missing in idx")
#     }
#     ### check if "index" column exists
#     if (isFALSE("index" %in% names(idx))) {
#       if (identical(ncol(idx), 2L)) {
#         message(paste0(
#           "Column \"index\" missing in idx. Using column ",
#           "\"", names(idx)[2], "\" instead"
#         ))
#       } else {
#         stop("Column \"index\" missing in idx")
#       }
#     }
#     comp_r_calc(
#       idx = idx, n0 = n0, n1 = n1, n2 = n2, units = units,
#       catch_rule = catch_rule, ...
#     )
#   }
# )
### numeric -> use as ratio
#' @rdname comp_r
# setMethod(comp_r,
#   signature = c(object = "numeric"),
#   function(object, ...) {
# 
#     ### create empty comp_r object
#     res <- new("comp_r")
# 
#     ### remove parameters
#     res@n0 <- NA_real_
#     res@n1 <- NA_real_
#     res@n2 <- NA_real_
# 
#     ### insert value
#     res@value <- object
# 
#     return(res)
#   }
# )
### comp_r -> check validity and update values if necessary
#' @rdname comp_r
# setMethod(comp_r,
#   signature = c(object = "comp_r"),
#   function(object, n0, n1, n2, units, catch_rule, ...) {
#     ### check validity
#     validObject(object)
#     ### run comp_r() to update slots and recalculate if needed
#     comp_r_calc(object, n0, n1, n2, units, catch_rule, ...)
#   }
# )

### alias for rfb and rb rule
#' @rdname comp_r
#' @export
# rfb_r <- comp_r
#' @rdname comp_r
#' @export
# rb_r <- comp_r

### ------------------------------------------------------------------------ ###
### comp_f validity ####
### ------------------------------------------------------------------------ ###
### validity checks
# setValidity("comp_r", function(object) {
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
### comp_f convenience methods ####
### ------------------------------------------------------------------------ ###

### print to screen
setMethod(
  f = "show", signature = "comp_f",
  definition = function(object) {
    cat(paste0(object@value, "\n"))
  }
)
setMethod(
  f = "summary", signature = "comp_f",
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
setGeneric(
  name = "value",
  def = function(object) standardGeneric("value")
)
setMethod(
  f = "value", signature = "comp_f",
  definition = function(object) {
    return(object@value)
  }
)

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
### advice - comp_f
#' @rdname advice
#' @usage NULL
#' @export
setMethod(
  f = "advice", signature = "comp_f",
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













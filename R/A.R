#' @include generics.R
NULL

### ------------------------------------------------------------------------ ###
### A class ####
### ------------------------------------------------------------------------ ###
#' @title A-class
#' 
#' @description An S4 class to represent component Ay (the last advice or reference catch) 
#' of the rfb, rb, and chr rules.
#' 
#' The classes \code{rfb_A}, \code{rb_A}, and \code{chr_A} inherit from 
#' \code{A} and their only difference is that the slot \code{hcr}
#' is set to the corresponding catch rule name ('rfb', 'rb', or 'chr').
#' 
#' @slot value The value of component Ay (reference catch)
#' @slot hcr The harvest control rule (hcr) for which Ay is used. One of 'rfb', 'rb', or 'chr'.
#' @slot data Time series of historical catches and/or advice
#' @slot avg_years Number of years for calculating average catch
#' @slot basis Basis of Ay. Either "advice" for using previous advice or "average catch" when based on average of historical catch
#' @slot advice_metric Advice metric, 'catch' or 'landings'.
#' 
#' @name A-class
#' @export
setClass(
  Class = "A",
  slots = c(
    value = "numeric",
    units = "character",
    hcr = "character",
    data = "vector",
    avg_years = "numeric",
    basis = "character",
    advice_metric = "character"
  ),
  prototype = list(
    value = NA_real_,
    units = NA_character_,
    hcr = NA_character_,
    data = data.frame(matrix(
      ncol = 3, nrow = 0,
      dimnames = list(NULL, c("year", "catch", "advice"))
    )),
    avg_years = NA_real_,
    basis = NA_character_,
    advice_metric = NA_character_
  )
)

#' @rdname A-class
setClass(Class = "rfb_A", 
         contains = "A",
         prototype = list(hcr = "rfb"))
#' @rdname A-class
setClass(Class = "rb_A", 
         contains = "A",
         prototype = list(hcr = "rb"))
#' @rdname A-class
setClass(Class = "chr_A", 
         contains = "A",
         prototype = list(hcr = "chr"))

### validity checks
setValidity("A", function(object) {
  if (!identical(length(object@value), 1L)) {
    "slot value must be of length 1"
  } else if (!identical(length(object@units), 1L)) {
    "slot units must be of length 1"
  } else if (isFALSE(object@hcr %in% c(NA, "rfb", "rb", "chr"))) {
    paste0("Unknown catch rule ", object@hcr, ". Must be ",
           "rfb, rb, or chr!")
  } else if (!identical(length(object@hcr), 1L)) {
    "slot hcr must be of length 1"
  } else if (isFALSE(object@basis %in% c(NA, "advice", "average catch"))) {
    paste0("Unknown catch rule ", object@basis, ". Must be ",
           "'average' or 'average catch'!")
  } else if (!identical(length(object@basis), 1L)) {
    "slot basis must be of length 1"
  } else if (!identical(length(object@advice_metric), 1L)) {
    "slot advice_metric must be of length 1"
  } else {
    TRUE
  }
})

### ------------------------------------------------------------------------ ###
### A methods ####
### ------------------------------------------------------------------------ ###
#' rfb/rb/chr rule - component Ay (reference catch or advice)
#'
#' This function defines the reference catch (last advice or average of 
#' historical catches) for the rfb, rb, and chr rules.
#' 
#' The function accepts as its first argument (`object`):
#' - a single value representing a reference catch, e.g. the previous catch advice
#' - a vector of historical values which are used to calculate the average catch
#' - a data.frame with columns 'year' and either of 'advice', 'catch', 'landings'
#' - an object of class `A`
#' 
#' \code{rfb_A()}, \code{rb_A()}, and \code{chr_A()} are aliases for
#' \code{A()} in which the \code{hcr} argument is already set to 
#' 'rfb', 'rb', or 'chr'.
#' 
#' The reference catch is set following ICES (2022).
#'
#' Usually, the reference catch is the previous advised catch. Alternatively,
#' if the rfb/rb/chr rule is applied the first time, it can be based on an
#' average of historical catches.
#' 
#' @param object The reference catch. See details
#' @param units [Optional] The units of the reference catch, e.g. "tonnes".
#' @param hcr [Optional] The harvest control rule (hcr) for which the multiplier is used. One of 'rfb', 'rb', or 'chr'.
#' @param data [Internal] Data used for calculating reference catch.
#' @param avg_years [Optional] Number of years for calculating average catch or vector years to use
#' @param basis [Optional] Basis of Ay. Either "advice" for using the previous advice or "average catch" when based on an average of historical catch
#' @param advice_metric Advice metric, e.g. catch or landings.
#' @param ... Additional arguments (Not used) 
#'
#' @references 
#' ICES. 2022. ICES technical guidance for harvest control rules and stock assessments for stocks in categories 2 and 3. In Report of ICES Advisory Committee, 2022. ICES Advice 2022, Section 16.4.11, 20 pp. \url{https://doi.org/10.17895/ices.advice.19801564}.
#' 
#' @examples
#' # data.frame with advice/catch history
#' df <- data.frame(year = 2018:2022,
#'                  advice = c(3257, 3648, 2721, 2177, 1742),
#'                  catch = c(2513, 2091, 1887, 1614, NA),
#'                  landings = c(1880, 1725, 1373, 1403, NA))
#' # get reference catch (advice)
#' A <- A(object = df, basis = "advice", units = "tonnes", advice_metric = "catch")
#' advice(A)
#' # use average catch
#' A(object = df, basis = "average catch", avg_years = 3, units = "tonnes", advice_metric = "catch")
#'
#' @return An object of class \code{A}
#' 
#' @name A
#' @export
NULL

#' @rdname A
setGeneric(
  name = "A",
  def = function(object, value, units, hcr, data, avg_years, 
                 basis = "advice", advice_metric = "catch", ...) {
    standardGeneric("A")
  },
  signature = c("object")
)

### numeric -> use as Ay
#' @rdname A
#' @usage NULL
#' @keywords internal
setMethod(A,
  signature = c(object = "numeric"),
  function(object, value, units, hcr, data, avg_years, basis,
           advice_metric, ...) {
    value <- object
    object <- new(Class = "A")
    A_calc(
      object = object, value = value, units = units, hcr = hcr, 
      data = data, avg_years = avg_years, basis = basis,
      advice_metric = advice_metric, ...
    )
  }
)

### numeric -> use as Ay
#' @rdname A
#' @usage NULL
#' @keywords internal
setMethod(A,
  signature = c(object = "data.frame"),
  function(object, value, units, hcr, data, avg_years, basis,
           advice_metric, ...) {
    data <- object
    object <- new(Class = "A")
    A_calc(
      object = object, value = value, units = units, hcr = hcr,
      data = data, avg_years = avg_years, basis = basis,
      advice_metric = advice_metric, ...
    )
  }
)

### A -> validate and update if needed
#' @rdname A
#' @usage NULL
#' @keywords internal
setMethod(A,
  signature = c(object = "A"),
  function(object, value, units = object@units, hcr = object@hcr, data, 
           avg_years = object@avg_years, basis = object@basis, 
           advice_metric = object@advice_metric, ...) {
    validObject(object)
    A_calc(
      object = object, value = value, units = units, hcr = hcr,
      data = data, avg_years = avg_years, basis = basis,
      advice_metric = advice_metric, ...
    )
  }
)

### ------------------------------------------------------------------------ ###
### A calculation ####
### ------------------------------------------------------------------------ ###
A_calc <- function(object, value, units, hcr, data, avg_years, 
                         basis, advice_metric, ...) {
  
  ### create empty object, if missing
  if (missing(object)) object <- new(Class = "A")
  
  ### format/check/insert values, if provided
  if (!missing(units))
    if (!identical(units, ""))
      object@units <- as.character(units)
  if (!missing(hcr))
    object@hcr <- match.arg(hcr, choices = c("rfb", "rb", "chr"))
  if (!missing(basis))
    object@basis <- match.arg(basis, choices = c("advice", "average catch"))
  if (!missing(advice_metric))
    object@advice_metric <- match.arg(advice_metric, 
                                      choices = c("catch", "landings"))
  
  ### argument "data" contains data to use for calculating Ay
  if (!missing(data)) {
    if (is(data, "data.frame")) {
      ### make sure that columns "catch"/"landings"/"advice" and "year" exist
      names(data) <- tolower(names(data))
      names(data) <- trimws(names(data), which = "both")
      if (identical(ncol(data), 1L)) {
        ### if single column provided, assume this contains the catch
        names(data) <- object@advice_metric
        data$year <- seq_along(data$catch)
      } else {
        if (all(!c("catch", "landings", "advice") %in% names(data))) 
          stop("column 'catch'/'landings'/'advice' missing in data.frame provided as data")
        if (isFALSE("year" %in% names(data))) 
          stop("column 'year' missing in data.frame provided as data")
      }
      ### check if request metric (catch/landings) is available
      if (identical(object@basis, "average catch")) {
        if (identical(object@advice_metric, "catch")) {
          if (!"catch" %in% names(data)) 
            stop("Average catch requested but catch not provided")
        } else if (identical(object@advice_metric, "landings")) {
          if (!"landings" %in% names(data)) 
            stop("Average landings requested but landings not provided")
        }
      }
    }
    object@data <- data
    ### if avg_years not specified, use all years
    if (missing(avg_years) & identical(object@basis, "average catch")) 
      avg_years <- length(data$year)
  }
  ### years for average catch
  if (!missing(avg_years)) {
    ### if a single number is provided, this is the number of years to use
    if (identical(length(avg_years), 1L) & all(!is.na(avg_years)) &
        isTRUE(avg_years < 100)) {
      avg_years <- tail(object@data[!is.na(object@data[, advice_metric]), "year"], avg_years)
      ### otherwise assume years to be considered are provided
    }
    object@avg_years <- avg_years
  }
  
  ### use value, if provided
  if (!missing(value)) {
    object@value <- value
    
  ### calculate Ay
  } else if (!missing(data)) {
    if (is.na(object@basis) | identical(object@basis, "average catch")) {
      value <- mean(object@data$catch[object@data$year %in% object@avg_years],
                  na.rm = TRUE)
      object@basis <- "average catch"
    } else if (identical(object@basis, "advice")) {
      ### use advice 
      if (all(!is.na(object@avg_years))) {
        value <- mean(object@data$advice[object@data$year == object@avg_years],
                      na.rm = TRUE)
      } else {
        ### find last advice value 
        pos <- tail(which(!is.na(object@data$advice)), 1)
        value <- object@data$advice[pos]
        if (isTRUE(nrow(object@data) > 0))
          object@avg_years <- object@data$year[pos]
      }
    }
    object@value <- value
    
  }
  
  return(object)
  
}

### ------------------------------------------------------------------------ ###
### alias ####
### ------------------------------------------------------------------------ ###
### define aliases rfb_A, rb_A, and chr_A for A
### set object signature to ANY and let A deal with method dispatch

### alias for rfb rule
#' @rdname A
#' @export
setGeneric(
  name = "rfb_A",
  def = function(object, value, units, hcr = "rfb", data, avg_years,
                 basis = "advice", advice_metric = "advice", ...) {
    standardGeneric("rfb_A")
  },
  signature = c("object")
)
#' @rdname A
#' @usage NULL
#' @export
setMethod(rfb_A,
  signature = c(object = "ANY"),
  function(object, value, units, hcr = "rfb", data, avg_years,
           basis, advice_metric, ...) {#browser()
    hcr <- match.arg(hcr)
    if (missing(basis)) basis <- object@basis
    if (missing(units)) units <- object@units
    if (missing(avg_years)) avg_years <- object@avg_years
    if (missing(advice_metric)) advice_metric <- object@advice_metric
    object <- A(
      object = object, value = value, units = units, hcr = hcr,
      data = data, avg_years = avg_years, basis = basis,
      ...
    )
    class(object) <- "rfb_A"
    return(object)
  }
)

### alias for rb rule
#' @rdname A
#' @export
setGeneric(
  name = "rb_A",
  def = function(object, value, units, hcr = "rb", data, avg_years, 
                 basis = "advice", advice_metric = "catch", ...) {
    standardGeneric("rb_A")
  },
  signature = c("object")
)
#' @rdname A
#' @usage NULL
#' @export
setMethod(rb_A,
  signature = c(object = "ANY"),
  function(object, value, units, hcr = "rb", data, avg_years,
           basis, ...) {
    hcr <- match.arg(hcr)
    object <- A(
      object = object, value = value, units = units, hcr = hcr,
      data = data, avg_years = avg_years, basis = basis,
      ...
    )
    class(object) <- "rb_A"
    return(object)
  }
)

### alias for chr rule
#' @rdname A
#' @export
setGeneric(
  name = "chr_A",
  def = function(object, value, units, hcr = "chr", data, avg_years,
                 basis = "advice", advice_metric = "catch", ...) {
    standardGeneric("chr_A")
  },
  signature = c("object")
)
#' @rdname A
#' @usage NULL
#' @export
setMethod(chr_A,
  signature = c(object = "ANY"),
  function(object, value, hcr = "chr", data, avg_years,
           basis, ...) {
    hcr <- match.arg(hcr)
    object <- A(
      object = object, value = value, units = units, hcr = hcr,
      data = data, avg_years = avg_years, basis = basis,
      ...
    )
    class(object) <- "chr_A"
    return(object)
  }
)

### ------------------------------------------------------------------------ ###
### convenience methods ####
### ------------------------------------------------------------------------ ###
### value
#' @rdname value
setMethod(
  f = "value", signature = "A",
  definition = function(object) {
    return(object@value)
  }
)

### print
setMethod(f = "print", signature = "A", 
          definition = function(x) {
            cat(paste0("An object of class \"", class(x), "\".\n",
                       "Value: ", x@value, "\n"))
})

### show
setMethod(f = "show", signature = "A", 
          definition = function(object) {
            cat(paste0("An object of class \"", class(object), "\".\n",
                       "Value: ", object@value, "\n"))
})

### detailed summary
#' @rdname summary
#' @export
setMethod(
  f = "summary", signature = "A",
  definition = function(object, ...) {
    txt <- paste0(
      paste(rep("-", 50), collapse = ""), "\n",
      "component Ay:\n"
    )
    txt <- paste0(txt, paste0("Reference catch Ay = ", object@value, "\n"))
    if (identical(object@basis, "average catch")) {
      txt <- paste0(
        txt, "based on average catches (years ",
        paste0(object@avg_years, collapse = ", "), ")\n"
      )
    }
    txt <- paste0(txt, paste0(paste(rep("-", 50), collapse = "")))
    cat(txt)
  }
)

### ------------------------------------------------------------------------ ###
### ICES advice style table ####
### ------------------------------------------------------------------------ ###
#' @rdname advice
#' @usage NULL
#' @export
setMethod(
  f = "advice", signature = "A",
  definition = function(object) {
    txt <- paste0(paste(rep("-", 80), collapse = ""), "\n")
    if (identical(object@basis, "advice")) {
      txt_A <- paste0("Previous ", object@advice_metric, 
                       " advice Ay (advised ", object@advice_metric, " for ", 
                       object@avg_years, ")")
    } else if (identical(object@basis, "average catch")) {
      txt_A <- paste0("Mean ", object@advice_metric, " Cy (", 
                       paste0(object@avg_years, collapse = ", "), ")")
    } else {
      txt_A <- paste0("Reference ", object@advice_metric)
    }
    Ay_value <- round(object@value)
    txt_A_value <- paste0(Ay_value, " ", object@units)
    txt_add <- paste0(format(txt_A, width = 48), " | ",
                      format(txt_A_value, width = 29, justify = "right"),
                      "\n")
    txt <- paste0(txt, txt_add)
    cat(txt)
  }
)

#' @include generics.R
#' @importFrom icesAdvice icesRound
#' @importFrom methods callNextMethod is new validObject
NULL

### ------------------------------------------------------------------------ ###
### m class ####
### ------------------------------------------------------------------------ ###
#' @title m-class

#' @description An S4 class to represent component m (the multiplier) of the rfb, rb, and 
#' chr rules.
#' 
#' The classes \code{rfb_m}, \code{rb_m}, and \code{chr_m} inherit from 
#' \code{m} and their only difference is that the slot \code{hcr}
#' is set to the corresponding catch rule name ('rfb', 'rb', or 'chr').
#' 
#' @slot value The value of component m
#' @slot hcr The harvest control rule (hcr) for which the multiplier is used. One of 'rfb', 'rb', or 'chr'.
#' @slot k Optional. The von Bertalanffy k parameter (individual growth rate, unit: 1/year).
#' @slot MSE \code{logical}. Generic multiplier or multiplier based on stock-specific simulations.
#' 
#' @name m-class
#' @export
setClass(Class = "m", 
         slots = c(value = "numeric",
                   hcr = "character",
                   k = "numeric",
                   MSE = "logical"
                   ),
         prototype = list(value = NA_real_, 
                          hcr = NA_character_,
                          k = NA_real_,
                          MSE = FALSE))

#' @rdname m-class
setClass(Class = "rfb_m", 
         contains = "m",
         prototype = list(hcr = "rfb"))
#' @rdname m-class
setClass(Class = "rb_m", 
         contains = "m",
         prototype = list(hcr = "rb"))
#' @rdname m-class
setClass(Class = "chr_m", 
         contains = "m",
         prototype = list(hcr = "chr"))

### ------------------------------------------------------------------------ ###
### m methods ####
### ------------------------------------------------------------------------ ###
#' rfb/rb/chr rule - component m (multiplier)
#'
#' This function returns the default multiplier for the rfb, rb, and chr rules.
#' 
#' \code{rfb_m()}, \code{rb_m()}, and \code{chr_m()} are aliases for
#' \code{m()} in which the \code{hcr} argument is already set to 
#' 'rfb', 'rb', or 'chr'.
#' 
#' The multiplier is set following ICES (2025).
#'
#' For the rfb rule, the multiplier is set depending on the von Bertalanffy 
#' parameter k (individual growth rate, units: 1/year; ICES, 2022). 
#' For species where 
#' k is below 0.2/year, the multiplier is set to m=0.95. For species where 
#' k is at or above 0.2/year, but below 0.32/year, the multiplier is set to
#' m=0.90. If the rfb rule is applied and k is unknown, the more precautionary
#' multiplier of m=0.90 is used.
#'
#' For the rb rule, the multiplier is set to m=0.50 (ICES, 2022).
#' 
#' For the chr rule, the multiplier is set to m=0.50 (ICES, 2022).
#' 
#' @param object Optional. A multiplier m value, if known, or an existing \code{m} object.
#' @param hcr The harvest control rule (hcr) for which the multiplier is used. One of 'rfb', 'rb', or 'chr'.
#' @param k Optional. The von Bertalanffy k parameter (individual growth rate, unit: 1/year).
#' @param MSE Optional. \code{logical}. Default multiplier or multiplier derived from stock specific MSE?
#' @param ... Additional arguments. Not used.
#'  
#' @section Warning:
#' For application in ICES, do not change the default multiplier unless the
#'  change is supported by stock-specific simulations.
#'
#' @references 
#' ICES. 2025. ICES Guidelines - Advice rules for stocks in category 2 and 3. Version 3. ICES Guidelines and Policies - Advice Technical Guidelines. 31 pp. \url{https://doi.org/10.17895/ices.pub.28506179}.
#'
#'
#' @return An object of class \code{m}
#'
#' @examples
#' # rfb rule with known k
#' rfb_m(k = 0.1) # 0.95
#' m(hcr = "rfb", k = 0.1) # 0.95
#' rfb_m(k = 0.25) # 0.90
#' m(hcr = "rfb", k = 0.25) # 0.90
#' # rfb rule with unknown k
#' rfb_m() # 0.90
#' m(hcr = "rfb") # 0.90
#' 
#' # rb rule
#' rb_m() # 0.5
#' m(hcr = "rb") # 0.5
#' 
#' # chr rule
#' chr_m() # 0.5
#' m(hcr = "chr") # 0.5
#' 
#' @name m
#' @export
NULL

#' @rdname m
#' @export
setGeneric(name = "m", 
           def = function(object, hcr, k, MSE, ...) 
             standardGeneric("m"),
           signature = c("object"))

### numeric -> use as m value
#' @rdname m
#' @usage NULL
#' @export
setMethod(m, 
          signature = c(object = "numeric"), 
          function(object, hcr, k, MSE, ...) {
            
  value <- object
  object <- new(Class = "m")
  m_calc(object = object, value = value, hcr = hcr, k = k, MSE = MSE,
              ...)
    
})
### m -> validate and update if needed
#' @rdname m
#' @usage NULL
#' @export
setMethod(m, 
          signature = c(object = "m"), 
          function(object, hcr, k, MSE, ...) {
            
  validObject(object)
  m_calc(object = object, hcr = hcr, k = k, MSE = MSE,
              ...)

})

### missing -> derive m
#' @rdname m
#' @usage NULL
#' @export
setMethod(m, 
          signature = c(object = "missing"), 
          function(object, hcr, k, MSE, ...) {
            
  m_calc(object = object, hcr = hcr, k = k, MSE = MSE,
              ...)
  
})

### ------------------------------------------------------------------------ ###
### m calculation ####
### ------------------------------------------------------------------------ ###
m_calc <- function(object, value, hcr, k, MSE, ...) {
  
  ### create empty object, if missing
  if (missing(object)) object <- new(Class = "m")
  
  if (!missing(MSE)) object@MSE <- MSE
  
  if (!missing(hcr))
    object@hcr <- match.arg(hcr, choices = c("rfb", "rb", "chr"))
  if (!missing(k))
    object@k <- k
  
  ### use multiplier, if provided
  if (!missing(value)) {
    object@value <- value
  ### derive multiplier from input data
  } else {
    
    ### if multiplier already exists in object, keep value
    if (!is.na(object@value)) {
      m_exists <- TRUE
      object_value <- object@value
    } else {
      m_exists <- FALSE
    }
    
    ### chr rule
    if (identical(object@hcr, "chr")) {
      if (!is.na(object@k)) {
        if (object@k < 0.32 | object@k >= 0.45)
          warning(paste0("k=", object@k, "/year is outside the recommended ",
                         "range of 0.32<=k<0.45/year for the chr rule. ",
                         "Proceed with caution!"))
      }
      if (isTRUE(is.na(object@value)))
        object@value <- 0.5
    ### rb rule
    } else if (identical(object@hcr, "rb")) {
      object@value <- 0.5
    ### rfb rule
    } else if (identical(object@hcr, "rfb")) {
      if (!is.na(object@k)) {
        if (object@k < 0.2) {
          object@value <- 0.95
          if (isFALSE(m_exists)) {
            message(paste0("Multiplier (m) for the rfb rule: ",
                           "selecting value based on k: m=", object@value))
          }
        } else if (object@k < 0.32) {
          object@value <- 0.9
          if (isFALSE(m_exists)) {
            message(paste0("Multiplier (m) for the rfb rule: ",
                           "selecting value based on k: m=", object@value))
          }
        } else if (object@k >= 0.32)
          stop(paste0("k=", object@k, "/year is outside the recommended",
                      "range of k<0.32/year for the rfb rule!"))
      } else {
        object@value <- 0.90
        if (isFALSE(m_exists)) {
          message(paste0("Multiplier (m) for the rfb rule: ",
                         "no value for k given, selecting the more",
                         "precautionary m=", object@value))
        }
      }
      
      ### if m provided, compare with new calculation
      if (isTRUE(m_exists)) {
        if (!identical(object_value, object@value)) {
          message(paste0("The value provided for the multiplier ",
                         "m=", object_value, " does not follow the ICES ",
                         "technical guidelines (m=", object@value, "). ",
                         "Proceed with caution!"))
          object@value <- object_value
        }
      }
      
    } else {
      
      ### stop here if no catch rule defined and no value provided
      if (is.na(object@value)) stop("unknown catch rule")
    
    }
    
  }
  
  return(object)
  
}

### ------------------------------------------------------------------------ ###
### m aliases ####
### ------------------------------------------------------------------------ ###
### define aliases rfb_m, rb_m, and chr_m for m
### set object signature to ANY and let m deal with method dispatch
### also include signature "missing" because "ANY" does not include it

### rfb
#' @rdname m
#' @export
setGeneric(name = "rfb_m", 
           def = function(object, hcr = "rfb", k, MSE, ...) 
             standardGeneric("rfb_m"),
           signature = c("object"))
#' @rdname m
#' @export
#' @usage NULL
setMethod(rfb_m, 
          signature = c(object = "ANY"), 
          function(object, hcr = "rfb", k, MSE, ...) {
  hcr <- match.arg(hcr)
  #if (is.numeric(object)) value <- object
  object <- m(object = object, hcr = hcr, 
                   k = k, MSE = MSE, ...)
  class(object) <- "rfb_m"
  return(object)
})
#' @rdname m
#' @export
#' @usage NULL
setMethod(rfb_m, 
          signature = c(object = "missing"), 
          function(object, hcr = "rfb", k, MSE, ...) {
  hcr <- match.arg(hcr)
  object <- m(hcr = hcr, k = k, MSE = MSE, ...)
  class(object) <- "rfb_m"
  return(object)
})

### rb
#' @rdname m
#' @export
setGeneric(name = "rb_m", 
           def = function(object, hcr = "rb", k, MSE, ...) 
             standardGeneric("rb_m"),
           signature = c("object"))
#' @rdname m
#' @export
#' @usage NULL
setMethod(rb_m, 
          signature = c(object = "ANY"), 
          function(object, hcr = "rb", k, MSE, ...) {
  hcr <- match.arg(hcr)
  object <- m(object = object, hcr = hcr, MSE = MSE,
                   k = k, ...)
  class(object) <- "rb_m"
  return(object)
})
#' @rdname m
#' @export
#' @usage NULL
setMethod(rb_m, 
          signature = c(object = "missing"), 
          function(object, hcr = "rb", k, MSE, ...) {
  hcr <- match.arg(hcr)
  object <- m(hcr = hcr, k = k, MSE = MSE, ...)
  class(object) <- "rb_m"
  return(object)
})

### chr
#' @rdname m
#' @export
setGeneric(name = "chr_m", 
           def = function(object, hcr = "chr", k, MSE, ...) 
             standardGeneric("chr_m"),
           signature = c("object"))
#' @rdname m
#' @export
#' @usage NULL
setMethod(chr_m, 
          signature = c(object = "ANY"), 
          function(object, hcr = "chr", k, MSE, ...) {
  hcr <- match.arg(hcr)
  object <- m(object = object, hcr = hcr, 
                   k = k, MSE = MSE, ...)
  class(object) <- "chr_m"
  return(object)
})
#' @rdname m
#' @export
#' @usage NULL
setMethod(chr_m, 
          signature = c(object = "missing"), 
          function(object, hcr = "chr", k, MSE, ...) {
  hcr <- match.arg(hcr)
  object <- m(hcr = hcr, k = k, MSE = MSE, ...)
  class(object) <- "chr_m"
  return(object)
})

### ------------------------------------------------------------------------ ###
### m validity ####
### ------------------------------------------------------------------------ ###
### validity checks
setValidity("m", function(object) {
  if (!identical(length(object@value), 1L)) {
    "slot value must be of length 1"
  } else if (isFALSE(object@hcr %in% c(NA, "rfb", "rb", "chr"))) {
    paste0("Unknown harvest control rule ", object@hcr, ". Must be ",
           "rfb, rb, or chr!")
  } else if (!identical(length(object@hcr), 1L)) {
    "slot hcr must be of length 1"
  } else if (!identical(length(object@k), 1L)) {
    "slot k must be of length 1"
  } else if (!is.na(object@k)) {
    if (object@k < 0 | object@k > 10) "slot k value infeasible"
  } else {
    TRUE
  }
})

### ------------------------------------------------------------------------ ###
### m convience methods ####
### ------------------------------------------------------------------------ ###
#' @rdname value
#' @export
setMethod(f = "value", signature = "m", 
          definition = function(object) {
            return(object@value)
})

### print
setMethod(f = "print", signature = "m", 
          definition = function(x) {
            cat(paste0("An object of class \"", class(x), "\".\n",
                       "Value: ", x@value, "\n"))
})

### show
setMethod(f = "show", signature = "m", 
          definition = function(object) {
            cat(paste0("An object of class \"", class(object), "\".\n",
                       "Value: ", object@value, "\n"))
})

### summary
#' @rdname summary
#' @export
setMethod(
  f = "summary", signature = "m",
  definition = function(object, ...) {
    txt <- paste0(paste(rep("-", 50), collapse = ""), "\n", 
                  "component m:\n")
    if (isTRUE(!is.na(object@hcr))) {
      txt <- paste0(txt, paste0("generic multiplier for ", object@hcr,
                                " rule"))
      if (identical(object@hcr, "rfb") & isTRUE(!is.na(object@k))) {
        txt <- paste0(txt, paste0(" for k=", object@k, "/yr"))
      }
      txt <- paste0(txt, "\n")
    }
    txt <- paste0(txt, paste0("multiplier m = ", object@value, "\n"))
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
  f = "advice", signature = "m",
  definition = function(object) {
    txt <- paste0(paste(rep("-", 80), collapse = ""), "\n",
                  "Precautionary multiplier to maintain biomass above Blim ",
                  "with 95% probability\n",
                  paste(rep("-", 80), collapse = ""), "\n")
    if ((is(object, "rfb_m") | identical(object@hcr, "rfb")) &
        isTRUE(object@value %in% c(0.95, 0.9))) {
      generic <- TRUE
    } else if ((is(object, "rb_m") | identical(object@hcr, "rb")) &
               isTRUE(object@value %in% c(0.5))) {
      generic <- TRUE
    } else if ((is(object, "chr_m") | identical(object@hcr, "chr")) &
               isTRUE(object@value %in% c(0.5))) {
      generic <- TRUE
    } else {
      generic <- FALSE
    }
    
    txt_m1 <- paste0("m: multiplier") 
    txt_m2 <- paste0(ifelse(generic, 
                            "(generic multiplier based on life history)",
                            "(stock-specific multiplier)"))
    if (isTRUE(object@MSE)) 
      txt_m2 <- "(derived from stock-specific simulations)"
    val_m <- icesAdvice::icesRound(object@value)
    
    txt <- paste0(txt,
                  paste0(format(txt_m1, width = 48), " | ",
                         format(val_m, width = 29, justify = "right"),
                         "\n",
                         format(paste0("   ", txt_m2), width = 48), " | ",
                         format("", width = 29, justify = "right"),
                         "\n")
    )
    
    cat(txt)
  }
)
### advice - rfb_m
#' @rdname advice
#' @usage NULL
#' @export
setMethod(
  f = "advice", signature = "rfb_m",
  definition = function(object) {
    txt <- callNextMethod()
    cat(txt)
  })
### advice - rb_m
#' @rdname advice
#' @usage NULL
#' @export
setMethod(
  f = "advice", signature = "rb_m",
  definition = function(object) {
    txt <- callNextMethod()
    cat(txt)
  })
### advice - chr_m
#' @rdname advice
#' @usage NULL
#' @export
setMethod(
  f = "advice", signature = "chr_m",
  definition = function(object) {
    txt <- callNextMethod()
    cat(txt)
  })


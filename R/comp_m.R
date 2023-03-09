#' @include generics.R

### ------------------------------------------------------------------------ ###
### comp_m class ####
### ------------------------------------------------------------------------ ###
#' An S4 class to represent component m (the multiplier) of the rfb, rb, and 
#' chr rules.
#' 
#' The classes \code{rfb_m}, \code{rb_m}, and \code{chr_m} inherit from 
#' \code{comp_m} and their only difference is that the slot \code{catch_rule}
#' is set to the corresponding catch rule name ('rfb', 'rb', or 'chr').
#' 
#' @slot value The value of component m
#' @slot catch_rule The catch rule for which the multiplier is used. One of 'rfb', 'rb', or 'chr'.
#' @slot k Optional. The von Bertalanffy k parameter (individual growth rate, unit: 1/year).
#' 
#' @rdname comp_m-class
#' @export
setClass(Class = "comp_m", 
         slots = c(value = "numeric",
                   catch_rule = "character",
                   k = "numeric"
                   ),
         prototype = list(value = NA_real_, 
                          catch_rule = NA_character_,
                          k = NA_real_))

#' @rdname comp_m-class
setClass(Class = "rfb_m", 
         contains = "comp_m",
         prototype = list(catch_rule = "rfb"))
#' @rdname comp_m-class
setClass(Class = "rb_m", 
         contains = "comp_m",
         prototype = list(catch_rule = "rb"))
#' @rdname comp_m-class
setClass(Class = "chr_m", 
         contains = "comp_m",
         prototype = list(catch_rule = "chr"))

### ------------------------------------------------------------------------ ###
### comp_m methods ####
### ------------------------------------------------------------------------ ###
#' rfb/rb/chr rule - component m (multiplier)
#'
#' This function returns the default multiplier for the rfb, rb, and chr rules.
#' 
#' \code{rfb_m()}, \code{rb_m()}, and \code{chr_m()} are aliases for
#' \code{comp_m()} in which the \code{catch_rule} argument is already set to 
#' 'rfb', 'rb', or 'chr'.
#' 
#' The multiplier is set following ICES (2022).
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
#' @param object Optional. A multiplier m value, if known, or an existing \code{comp_m} object.
#' @param catch_rule The catch rule for which the multiplier is used. One of 'rfb', 'rb', or 'chr'.
#' @param k Optional. The von Bertalanffy k parameter (individual growth rate, unit: 1/year).
#'  
#' @section Warning:
#' For application in ICES, do not change the default multiplier unless the
#'  change is supported by stock-specific simulations.
#'
#' @references 
#' ICES. 2022. ICES technical guidance for harvest control rules and stock assessments for stocks in categories 2 and 3. In Report of ICES Advisory Committee, 2022. ICES Advice 2022, Section 16.4.11, 20 pp. \url{https://doi.org/10.17895/ices.advice.19801564}.
#'
#'
#' @return An object of class \code{comp_m}
#'
#' @examples
#' # rfb rule with known k
#' rfb_m(k = 0.1) # 0.95
#' comp_m(catch_rule = "rfb", k = 0.1) # 0.95
#' rfb_m(k = 0.25) # 0.90
#' comp_m(catch_rule = "rfb", k = 0.25) # 0.90
#' # rfb rule with unknown k
#' rfb_m() # 0.90
#' comp_m(catch_rule = "rfb") # 0.90
#' 
#' # rb rule
#' rb_m() # 0.5
#' comp_m(catch_rule = "rb") # 0.5
#' 
#' # chr rule
#' chr_m() # 0.5
#' comp_m(catch_rule = "chr") # 0.5
#' 
#' @name comp_m
#' @export
NULL

#' @rdname comp_m
setGeneric(name = "comp_m", 
           def = function(object, catch_rule, k, ...) 
             standardGeneric("comp_m"),
           signature = c("object"))

### numeric -> use as m value
#' @rdname comp_m
#' @keywords internal
#' @usage NULL

setMethod(comp_m, 
          signature = c(object = "numeric"), 
          function(object, catch_rule, k, ...) {
            
  value <- object
  object <- new(Class = "comp_m")
  comp_m_calc(object = object, value = value, catch_rule = catch_rule, k = k,
              ...)
    
})
### comp_m -> validate and update if needed
#' @rdname comp_m
#' @keywords internal
#' @usage NULL

setMethod(comp_m, 
          signature = c(object = "comp_m"), 
          function(object, catch_rule, k, ...) {
            
  validObject(object)
  comp_m_calc(object = object, catch_rule = catch_rule, k = k,
              ...)

})

### missing -> derive m
#' @rdname comp_m
#' @keywords internal
#' @usage NULL

setMethod(comp_m, 
          signature = c(object = "missing"), 
          function(object, catch_rule, k, ...) {
            
  comp_m_calc(object = object, catch_rule = catch_rule, k = k,
              ...)
  
})

### ------------------------------------------------------------------------ ###
### comp_m calculation ####
### ------------------------------------------------------------------------ ###
comp_m_calc <- function(object, value, catch_rule, k, ...) {
  
  ### create empty object, if missing
  if (missing(object)) object <- new(Class = "comp_m")
  
  if (!missing(catch_rule))
    object@catch_rule <- match.arg(catch_rule, choices = c("rfb", "rb", "chr"))
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
    if (identical(object@catch_rule, "chr")) {
      if (!is.na(object@k)) {
        if (object@k < 0.32 | object@k >= 0.45)
          warning(paste0("k=", object@k, "/year is outside the recommended ",
                         "range of 0.32<=k<0.45/year for the chr rule. ",
                         "Proceed with caution!"))
      }
      object@value <- 0.5
    ### rb rule
    } else if (identical(object@catch_rule, "rb")) {
      object@value <- 0.5
    ### rfb rule
    } else if (identical(object@catch_rule, "rfb")) {
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
### comp_m aliases ####
### ------------------------------------------------------------------------ ###

### alias for rfb rule
#' @rdname comp_m
setGeneric(name = "rfb_m", 
           def = function(object, value, catch_rule = "rfb", k, ...) 
             standardGeneric("rfb_m"),
           signature = c("object"))
#' @rdname comp_m
#' @keywords internal
#' @usage NULL

setMethod(rfb_m, 
          signature = c(object = "comp_m"), 
          function(object, value, catch_rule = "rfb", k, ...) {
  catch_rule <- match.arg(catch_rule)
  object <- comp_m(object = object, value = value, catch_rule = catch_rule, 
                   k = k, ...)
  class(object) <- "rfb_m"
})
#' @rdname comp_m
#' @keywords internal
#' @usage NULL

setMethod(rfb_m, 
          signature = c(object = "numeric"), 
          function(object, value, catch_rule = "rfb", k, ...) {
  catch_rule <- match.arg(catch_rule)
  value <- object
  object <- new(Class = "rfb_m")
  comp_m(object = object, value = value, catch_rule = catch_rule, k = k, ...)
})
#' @rdname comp_m
#' @keywords internal
#' @usage NULL

setMethod(rfb_m, 
          signature = c(object = "missing"), 
          function(object, value, catch_rule = "rfb", k, ...) {
  catch_rule <- match.arg(catch_rule)
  object <- new(Class = "rfb_m")
  comp_m(object = object, value = value, catch_rule = catch_rule, k = k, ...)
})
### alias for rb rule
#' @rdname comp_m
setGeneric(name = "rb_m", 
           def = function(object, value, catch_rule = "rb", k, ...) 
             standardGeneric("rb_m"),
           signature = c("object"))
#' @rdname comp_m
#' @keywords internal
#' @usage NULL

setMethod(rb_m, 
          signature = c(object = "comp_m"), 
          function(object, value, catch_rule = "rb", k, ...) {
  catch_rule <- match.arg(catch_rule)
  object <- comp_m(object = object, value = value, catch_rule = catch_rule, 
                   k = k, ...)
  class(object) <- "rb_m"
})
#' @rdname comp_m
#' @keywords internal
#' @usage NULL

setMethod(rb_m, 
          signature = c(object = "numeric"), 
          function(object, value, catch_rule = "rb", k, ...) {
  catch_rule <- match.arg(catch_rule)
  value <- object
  object <- new(Class = "rb_m")
  comp_m(object = object, value = value, catch_rule = catch_rule, k = k, ...)
})
#' @rdname comp_m
#' @keywords internal
#' @usage NULL

setMethod(rb_m, 
          signature = c(object = "missing"), 
          function(object, value, catch_rule = "rb", k, ...) {
  catch_rule <- match.arg(catch_rule)
  object <- new(Class = "rb_m")
  comp_m(object = object, value = value, catch_rule = catch_rule, k = k, ...)
})
### alias for chr rule
#' @rdname comp_m
setGeneric(name = "chr_m", 
           def = function(object, value, catch_rule = "chr", k, ...) 
             standardGeneric("chr_m"),
           signature = c("object"))
#' @rdname comp_m
#' @keywords internal
#' @usage NULL

setMethod(chr_m, 
          signature = c(object = "comp_m"), 
          function(object, value, catch_rule = "chr", k, ...) {
  catch_rule <- match.arg(catch_rule)
  object <- comp_m(object = object, value = value, catch_rule = catch_rule, 
                   k = k, ...)
  class(object) <- "chr_m"
})
#' @rdname comp_m
#' @keywords internal
#' @usage NULL

setMethod(chr_m, 
          signature = c(object = "numeric"), 
          function(object, value, catch_rule = "chr", k, ...) {
  catch_rule <- match.arg(catch_rule)
  value <- object
  object <- new(Class = "chr_m")
  comp_m(object = object, value = value, catch_rule = catch_rule, k = k, ...)
})
#' @rdname comp_m
#' @keywords internal
#' @usage NULL

setMethod(chr_m, 
          signature = c(object = "missing"), 
          function(object, value, catch_rule = "chr", k, ...) {
  catch_rule <- match.arg(catch_rule)
  object <- new(Class = "chr_m")
  comp_m(object = object, value = value, catch_rule = catch_rule, k = k, ...)
})

### ------------------------------------------------------------------------ ###
### comp_m validity ####
### ------------------------------------------------------------------------ ###
### validity checks
setValidity("comp_m", function(object) {
  if (!identical(length(object@value), 1L)) {
    "slot value must be of length 1"
  } else if (isFALSE(object@catch_rule %in% c(NA, "rfb", "rb", "chr"))) {
    paste0("Unknown catch rule ", object@catch_rule, ". Must be ",
           "rfb, rb, or chr!")
  } else if (!identical(length(object@catch_rule), 1L)) {
    "slot catch_rule must be of length 1"
  } else if (!identical(length(object@k), 1L)) {
    "slot k must be of length 1"
  } else if (!is.na(object@k)) {
    if (object@k < 0 | object@k > 10) "slot k value infeasible"
  } else {
    TRUE
  }
})

### ------------------------------------------------------------------------ ###
### comp_m convience methods ####
### ------------------------------------------------------------------------ ###
### value of object
setGeneric(name = "value", 
           def = function(object)  standardGeneric("value"))
setMethod(f = "value", signature = "comp_m", 
          definition = function(object) {
            return(object@value)
})

### print to screen
setMethod(f = "show", signature = "comp_m", 
          definition = function(object) {
            cat(paste0(object@value, "\n"))
})

### summary
setMethod(
  f = "summary", signature = "comp_m",
  definition = function(object) {
    txt <- paste0(paste(rep("-", 50), collapse = ""), "\n", 
                  "component m:\n")
    if (isTRUE(!is.na(object@catch_rule))) {
      txt <- paste0(txt, paste0("generic multiplier for ", object@catch_rule,
                                " rule"))
      if (identical(object@catch_rule, "rfb") & isTRUE(!is.na(object@k))) {
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
  f = "advice", signature = "comp_m",
  definition = function(object) {
    txt <- paste0(paste(rep("-", 80), collapse = ""), "\n",
                  "Precautionary multiplier to maintain biomass above Blim ",
                  "with 95% probability\n",
                  paste(rep("-", 80), collapse = ""), "\n")
    if (is(object, "rfb_m") | identical(object@catch_rule, "rfb") &
        isTRUE(object@value %in% c(0.95, 0.9))) {
      generic <- TRUE
    } else if (is(object, "rb_m") | identical(object@catch_rule, "rb") &
               isTRUE(object@value %in% c(0.5))) {
      generic <- TRUE
    } else if (is(object, "chr_m") | identical(object@catch_rule, "chr") &
               isTRUE(object@value %in% c(0.5))) {
      generic <- TRUE
    } else {
      generic <- FALSE
    }
    
    txt_m1 <- paste0("m: multiplier") 
    txt_m2 <- paste0(ifelse(generic, 
                            "(generic multiplier based on life history)",
                            "(stock-specific multiplier)"))
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


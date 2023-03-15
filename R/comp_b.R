#' @include generics.R
#' @importFrom icesAdvice icesRound
NULL

### ------------------------------------------------------------------------ ###
### comp_b class ####
### ------------------------------------------------------------------------ ###
#' @title comp_b-class
#' 
#' @description  An S4 class to represent component b of the rfb/rb/chr rules.
#' 
#' This class (\code{comp_b}) stores the input for component b (the biomass
#' safeguard) as well as the resulting b value. 
#' 
#' The classes \code{rfb_b}, \code{rb_b}, and \code{chr_b} inherit from 
#' \code{comp_b} and their only difference is that the slot \code{catch_rule}
#' is set to the corresponding catch rule name ('rfb', 'rb', or 'chr').
#' 
#' @slot value The value of component b
#' @slot idx_value Index value that is compared to Itrigger.
#' @slot Itrigger The index trigger value below which the advice is reduced. Usually calculated as \code{Itrigger=Iloss*w}.
#' @slot Iloss The lowest observed index value. Can be used as the basis for \code{Itrigger}.
#' @slot w Index trigger buffer. Connects \code{Itrigger} to \code{Iloss}.
#' @slot yr_ref Reference year on which Itrigger is based.
#' @slot yr_last Last data year of the biomass index. The index value in this year is compared to \code{Itrigger}.
#' @slot n0 Time lag between the last index year and the last year to be used.
#' @slot idx \code{data.frame}. A \code{data.frame} with the index values.
#' @slot units \code{character}. The units of the biomass index, e.g. 'kg/hr'.
#' @slot catch_rule \code{character}. The catch rule for which the biomass safeguard is used. One of 'rfb', 'rb', or 'chr'.
#' 
#' @name comp_b-class
#' @export
setClass(Class = "comp_b", 
         slots = c(value = "numeric",
                   idx_value = "numeric",
                   Itrigger = "numeric", 
                   Iloss = "numeric", 
                   w = "numeric",
                   yr_ref = "numeric",
                   yr_last = "numeric",
                   n0 = "numeric",
                   idx = "data.frame",
                   units = "character",
                   catch_rule = "character"),
         prototype = list(value = NA_real_,
                          idx_value = NA_real_,
                          Itrigger = NA_real_,
                          Iloss = NA_real_,
                          w = 1.4,
                          yr_ref = NA_real_,
                          yr_last = NA_real_,
                          n0 = 0,
                          idx = data.frame(year = NULL, index = NULL),
                          units = NA_character_,
                          catch_rule = NA_character_))

#' @rdname comp_b-class
setClass(Class = "rfb_b", 
         contains = "comp_b",
         prototype = list(catch_rule = "rfb"))
#' @rdname comp_b-class
setClass(Class = "rb_b", 
         contains = "comp_b",
         prototype = list(catch_rule = "rb"))
#' @rdname comp_b-class
setClass(Class = "chr_b", 
         contains = "comp_b",
         prototype = list(catch_rule = "chr"))

### ------------------------------------------------------------------------ ###
### comp_b methods ####
### ------------------------------------------------------------------------ ###
#' rb/rfb/chr rule - component b (biomass safeguard)
#'
#' This function calculates component b (the biomass safeguard) of the rb, rfb,
#' and chr rule. The index needs to be a biomass index without age structure.
#' 
#' The biomass safeguard compares the last index value to an index trigger 
#' value. If the current index value is below the trigger, the biomass safeguard
#' reduces the catch advice. See ICES (2022) for the definition.
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
#' \code{comp_b()} with identical arguments and functionality.
#' 
#'
#' @param object The biomass index. Can be a \code{data.frame} with columns 'data' and 'index' or an \code{FLQuant} object defined by \code{FLCore}.
#' @param idx_value Optional. The current index value. Only used if no index time series is supplied.
#' @param Itrigger Optional. The index trigger value below which the biomass safeguard reduces the catch advice.
#' @param Iloss Optional. The lowest index value, can be used to calculate \code{Itrigger}.
#' @param w Optional. The index trigger buffer (multiplier) to link \code{Itrigger} to \code{Iloss}. Defaults to \code{w=1.4}.
#' @param yr_ref Optional. If supplied, this specifies the year in the biomass index which is used as \code{Iloss} and \code{Itrigger} is calculated from this value.
#' @param n0 Optional. Time lag between the last index year and the last year to be used. By default, the last index year is used (\code{n0=0})
#' @param units Optional. The units of the biomass index, e.g. 'kg/hr'. Only used for plotting.
#' @param catch_rule Optional. One of 'rfb', 'rb', or 'chr'.
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
#' Fischer, S. H., De Oliveira, J. A. A., Mumford, J. D., and Kell, L. T. 2021. Application of explicit precautionary principles in data-limited fisheries management. ICES Journal of Marine Science, 78: 2931--2942. \url{https://doi.org/10.1093/icesjms/fsab169}.
#' 
#' Fischer, S. H., De Oliveira, J. A. A., Mumford, J. D., and Kell, L. T. 2021. Using a genetic algorithm to optimize a data-limited catch rule. ICES Journal of Marine Science, 78: 1311--1323. \url{https://doi.org/10.1093/icesjms/fsab018}.
#' 
#' Fischer, S. H., De Oliveira, J. A. A., and Kell, L. T. 2020. Linking the performance of a data-limited empirical catch rule to life-history traits. ICES Journal of Marine Science, 77: 1914--1926. \url{https://doi.org/10.1093/icesjms/fsaa054}.
#'
#'
#' @return An object of class \code{comp_b} with the value of the biomass 
#' safeguard
#'
#' @examples
#' # If the value of the biomass safeguard is known
#' comp_b(1)
#' 
#' # Use a data.frame with index values
#' df_idx <- data.frame(year = 2017:2021,
#'                      index = c(1.33, 1.13, 0.84, 0.60, 1.03))
#' comp_b(df_idx)
#' 
#' # plot
#' plot(comp_b(df_idx, units = "kg/hr"))
#' 
#' @export
setGeneric(name = "comp_b", 
           def = function(object, idx_value, Itrigger, Iloss, w,
                          yr_ref, n0, units, catch_rule, ...) 
           standardGeneric("comp_b"),
           signature = c("object"))

### FLQuant -> convert to data.frame
# #' @rdname comp_b
# #' @usage NULL
# #' @export
# setMethod(comp_b, 
#           signature = c(object = "FLQuant"), 
#           function(object, idx_value, Itrigger, Iloss, w,
#                    yr_ref, n0, units, ...) {
#   ### convert FLQuant into data.frame
#   idx <- as.data.frame(object)[, c("year", "data")]
#   names(idx)[2] <- "index"
#   comp_b(object = idx, idx_value = idx_value, Itrigger = Itrigger, Iloss = Iloss, 
#          w = w, yr_ref = yr_ref, n0 = n0, units = units, ...)
# })
### data.frame -> use as index
#' @rdname comp_b
#' @usage NULL
#' @export
setMethod(comp_b, 
          signature = c(object = "data.frame"),
          function(object, idx_value, Itrigger, Iloss, w,
                   yr_ref, n0, units, catch_rule, ...) {
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
  comp_b_calc(idx = idx, idx_value = idx_value, Itrigger = Itrigger, 
              Iloss = Iloss, w = w, yr_ref = yr_ref, n0 = n0, units = units, 
              ...)
})
### numeric -> use as b
#' @rdname comp_b
#' @usage NULL
#' @export
setMethod(comp_b, 
          signature = c(object = "numeric"), 
          function(object, idx_value, Itrigger, Iloss, w,
                   yr_ref, n0, units, catch_rule, ...) {
            
  ### create empty comp_b object
  res <- new("comp_b")
  if (!missing(catch_rule)) res@catch_rule <- catch_rule
  
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
### comp_b -> check validity and update values if necessary
#' @rdname comp_b
#' @usage NULL
#' @export
setMethod(comp_b, 
          signature = c(object = "comp_b"), 
          function(object, idx_value, Itrigger, Iloss, w,
                   yr_ref, n0, units, catch_rule, ...) {
  ### check validity
  validObject(object)
  ### run comp_b() to update slots and recalculate if needed
  comp_b_calc(object, ...)
})

### ------------------------------------------------------------------------ ###
### alias ####
### ------------------------------------------------------------------------ ###
### define aliases rfb_b, rb_b, and chr_b for comp_b
### set object signature to ANY and let comp_b deal with method dispatch

### rfb
#' @rdname comp_b
#' @export
setGeneric(name = "rfb_b", 
           def = function(object, idx_value, Itrigger, Iloss, w,
                          yr_ref, n0, units, catch_rule = "rfb", ...) 
             standardGeneric("rfb_b"),
           signature = c("object"))
#' @rdname comp_b
#' @usage NULL
#' @export
setMethod(rfb_b, 
          signature = c(object = "ANY"),
          function(object, idx_value, Itrigger, Iloss, w,
                   yr_ref, n0, units, catch_rule = "rfb", ...) {
  catch_rule <- match.arg(catch_rule)
  object <- comp_b(object = object, idx_value = idx_value, Itrigger = Itrigger,
                   Iloss = Iloss, w = w, yr_ref = yr_ref, n0 = n0, 
                   units = units, catch_rule = catch_rule, ... = ...)
  class(object) <- "rfb_b"
  return(object)
})
### rb
#' @rdname comp_b
#' @export
setGeneric(name = "rb_b", 
           def = function(object, idx_value, Itrigger, Iloss, w,
                          yr_ref, n0, units, catch_rule = "rb", ...) 
             standardGeneric("rb_b"),
           signature = c("object"))
#' @rdname comp_b
#' @usage NULL
#' @export
setMethod(rb_b, 
          signature = c(object = "ANY"),
          function(object, idx_value, Itrigger, Iloss, w,
                   yr_ref, n0, units, catch_rule = "rb", ...) {
  catch_rule <- match.arg(catch_rule)
  object <- comp_b(object = object, idx_value = idx_value, Itrigger = Itrigger,
                   Iloss = Iloss, w = w, yr_ref = yr_ref, n0 = n0, 
                   units = units, catch_rule = catch_rule, ... = ...)
  class(object) <- "rb_b"
  return(object)
})
### chr
#' @rdname comp_b
#' @export
setGeneric(name = "chr_b", 
           def = function(object, idx_value, Itrigger, Iloss, w,
                          yr_ref, n0, units, catch_rule = "chr", ...) 
             standardGeneric("chr_b"),
           signature = c("object"))
#' @rdname comp_b
#' @usage NULL
#' @export
setMethod(chr_b, 
          signature = c(object = "ANY"),
          function(object, idx_value, Itrigger, Iloss, w,
                   yr_ref, n0, units, catch_rule = "chr", ...) {
  catch_rule <- match.arg(catch_rule)
  object <- comp_b(object = object, idx_value = idx_value, Itrigger = Itrigger,
                   Iloss = Iloss, w = w, yr_ref = yr_ref, n0 = n0, 
                   units = units, catch_rule = catch_rule, ... = ...)
  class(object) <- "chr_b"
  return(object)
})

### ------------------------------------------------------------------------ ###
### comp_b validity ####
### ------------------------------------------------------------------------ ###
### validity checks
setValidity("comp_b", function(object) {
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
  } else if (!identical(length(object@n0), 1L)) {
    "slot n0 must be of length 1"
  } else if (!is(object@idx, "data.frame")) {
    "slot idx must be a data.frame"
  } else if (!identical(length(object@units), 1L)) {
    "slot units must be of length 1"
  } else {
    TRUE
  }
})

### ------------------------------------------------------------------------ ###
### comp_b calculation ####
### ------------------------------------------------------------------------ ###
### function for creating/calculating biomass safeguard
comp_b_calc <- function(object, idx, idx_value, Itrigger, Iloss, w, n0, yr_ref,
                       yr_last, units, catch_rule) {
  ### create empty comp_b object, if missing
  if (missing(object)) object <- new("comp_b")
  if (!missing(catch_rule)) object@catch_rule <- catch_rule
  
  ### add/update index, if provided
  if (!missing(idx)) object@idx <- idx
  
  ### add/update parameters, if provided
  if (!missing(idx_value)) object@idx_value <- idx_value
  if (!missing(Itrigger)) object@Itrigger <- Itrigger
  if (!missing(Iloss)) object@Iloss <- Iloss
  if (!missing(w)) object@w <- w
  if (!missing(n0)) object@n0 <- n0
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
  } else if (isTRUE(length(object@idx) > 0)) {
    
    pos_loss <- which.min(object@idx$index)
    object@yr_ref <- object@idx$year[pos_loss]
    object@Iloss <- object@idx$index[pos_loss]
    object@Itrigger <- object@Iloss * object@w
    
  }
  
  ### if index value supplied as argument, use this value
  if (!missing(idx_value)) {
    
    object@idx_value <- idx_value
    
  ### get index value from object@idx
  } else if (isTRUE(length(object@idx) > 0)) {
  
    ### find last index value to use
    object@idx_value <- object@idx$index[which(object@idx$year == 
                                                 object@yr_last) -
                                           object@n0]
    
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
setMethod(f = "summary", signature = "comp_b", 
          definition = function(object) {
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
setGeneric(name = "value", 
           def = function(object)  standardGeneric("value"))
setMethod(f = "value", signature = "comp_b", 
          definition = function(object) {
  return(object@value)
})

### ------------------------------------------------------------------------ ###
### ICES advice style table ####
### ------------------------------------------------------------------------ ###
#' @rdname advice
#' @usage NULL
#' @export
setMethod(
  f = "advice", signature = "comp_b",
  definition = function(object) {
    txt <- paste0(paste(rep("-", 80), collapse = ""), "\n",
                  "Biomass safeguard\n",
                  paste(rep("-", 80), collapse = ""), "\n")
    
    I_last_year <- ifelse(!is.na(object@yr_last), object@yr_last, "last")
    txt_I <- paste0("Last index value (I", I_last_year, ")")
    txt_I_trigger <- paste0("Index trigger value (Itrigger = Iloss x ", 
                            object@w, ")")
    txt_b1 <- paste0("b: index relative to trigger value,")
    txt_b2 <- paste0("min{I", I_last_year, "/Itrigger, 1}")
    
    val_I <- paste0(icesAdvice::icesRound(object@idx_value), 
                    ifelse(!is.na(object@units), paste0(" ", object@units), ""))
    val_I_trigger <- paste0(icesAdvice::icesRound(object@Itrigger), 
                    ifelse(!is.na(object@units), paste0(" ", object@units), ""))
    val_b <- icesAdvice::icesRound(object@value)
    
    txt <- paste0(txt,
                  paste0(format(txt_I, width = 48), " | ",
                         format(val_I, width = 29, justify = "right"),
                         "\n"),
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

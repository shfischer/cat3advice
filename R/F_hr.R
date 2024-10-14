#' @include generics.R
#' @include f.R
#' @importFrom icesAdvice icesRound
NULL

### ------------------------------------------------------------------------ ###
### HR class ####
### ------------------------------------------------------------------------ ###

#' @title HR-class
#' 
#' @description  An S4 class to represent the harvest rate (hr) of the chr rule.
#' 
#' This class (\code{HR}) stores the input for the harvest rate 
#' (catch, landings) as well as the resulting harvest rates. 
#' 
#' @slot value The values of the harvest rate time series.
#' @slot metric The metric for the harvest rate (e.g. catch, landings, or
#'  dead catch)
#' @slot data \code{data.frame}. The input data (catch and index values)
#' @slot units \code{character}. The units of the harvest rate.
#' @slot units_catch \code{character}. The units of the catch.
#' @slot units_index \code{character}. The units of the index.
#' @slot hcr \code{character}. The harvest control rule (hcr) for which the index is used. Only applicable to 'chr'.
#' 
#' @name HR-class
#' @title HR
#' @export
setClass(
  Class = "HR",
  slots = c(
    value = "numeric",
    metric = "character",
    data = "data.frame",
    units = "character",
    units_catch = "character",
    units_index = "character",
    hcr = "character"
  ),
  prototype = list(
    value = NA_real_,
    metric = "catch",
    data = data.frame(matrix(
      ncol = 3,
      nrow = 0,
      dimnames = list(NULL, c("year", "catch", "index"))
    )),
    units = NA_character_,
    units_catch = NA_character_,
    units_index = NA_character_,
    hcr = "chr"
  )
)

### ------------------------------------------------------------------------ ###
### HR methods ####
### ------------------------------------------------------------------------ ###

### object = missing, data = data.frame
#' Calculation of the (relative) harvest rate
#'
#' The (relative) harvest rate is calculated by dividing the catch values by biomass index values.
#' 
#' Usually, this functions is used by providing a \code{data.frame} with columns 'year', 'catch' and 'index'. 
#' The catch can be split into landings and discards by providing 'landings' and 'discards' columns. 
#' The harvest rate can be calculated on the dead catch by specifying 
#' \code{split_catch=TRUE} and defining \code{discard_survival}. In this case,
#' the harvest rate will be calculated on the dead catch (landings plus
#' proportion of discards that die).
#' 
#' If an object of class \code{HR} is provided, its validity is checked.
#'
#' @param object The data to use. Usually a \code{data.frame} with columns 'year', 'catch' and 'index'.
#' @param split_discards Shall the catch be split into landings and discards? Defaults to \code{FALSE}.
#' @param discard_survival Discard survival (0-1). If \code{split_discards=TRUE}, this will be used to calculate the dead discards and these will be used in the harvest rate calculation.
#' @param units Optional. The units of the harvest rate. Can be derived automatically from \code{units_catch} and \code{units_index}.
#' @param units_catch Optional. The units of the catch, e.g. 'tonnes'.
#' @param units_index Optional. The units of the biomass index, e.g. 'kg/hr'.
#' @param ... Additional arguments. Not currently used.
#'
#' @references
#' ICES. 2022. ICES technical guidance for harvest control rules and stock assessments for stocks in categories 2 and 3. In Report of ICES Advisory Committee, 2022. ICES Advice 2022, Section 16.4.11, 20 pp. \url{https://doi.org/10.17895/ices.advice.19801564}.
#' 
#' Fischer, S. H., De Oliveira, J. A. A., Mumford, J. D., and Kell, L. T. 2023. Risk equivalence in data‐limited and data‐rich fisheries management: An example based on the ICES advice framework. Fish and Fisheries, 24: 231--247. \url{https://doi.org/10.1111/faf.12722}.
#'
#' Fischer, S. H., De Oliveira, J. A. A., Mumford, J. D., and Kell, L. T. 2022. Exploring a relative harvest rate strategy for moderately data-limited fisheries management. ICES Journal of Marine Science, 79: 1730--1741. \url{https://doi.org/10.1093/icesjms/fsac103}.
#'
#' @return An object of class \code{HR} with the harvest rate value(s).
#' 
#' @examples 
#' 
#' # create data.frame with data
#' df <- data.frame(year = 2017:2021, 
#'                  index = c(1.33, 1.13, 0.84, 0.60, 1.03),
#'                  catch = c(2949, 2513, 2091, 1888, 1615))
#' # calculate harvest rate
#' HR(df)
#' 
#' # check objects validity
#' HR(HR(df))
#' 
#' # plot harvest rate
#' # plot(HR(df, units_catch = "tonnes", units_index = "kg/hr"))
#' 
#' @export
setGeneric(
  name = "HR",
  def = function(object, split_discards = FALSE, discard_survival, units_catch,
                 units_index, units, ...) {
    standardGeneric("HR")
  },
  signature = c("object")
)

### object = data.frame -> calculate HR
#' @rdname HR
#' @usage NULL
#' @export
setMethod(HR,
          signature = c(object = "data.frame"),
          function(object, split_discards = FALSE, discard_survival = 0,
                   units_catch, units_index, units, ...) {
  object <- calc_HR(data = object, split_discards = split_discards,
                    discard_survival = discard_survival,
                    units_catch = units_catch, 
                    units_index = units_index, units = units)
  validObject(object)
  return(object)
})

### HR -> check validity
#' @rdname HR
#' @usage NULL
#' @export
setMethod(HR,
          signature = c(object = "HR"),
          function(object, split_discards = FALSE, discard_survival = 0,
                   units_catch, units_index, units, ...) {
  validObject(object)
  return(object)
})

### ------------------------------------------------------------------------ ###
### calculate HR ####
### ------------------------------------------------------------------------ ###
calc_HR <- function(object = new("HR"), 
                    data, 
                    split_discards = FALSE,
                    discard_survival,
                    units_catch, units_index, units) {
  #browser()
  
  names(data) <- tolower(names(data))
  if (!"index" %in% names(data)) 
    stop("column \"index\" missing")
  if (!"catch" %in% names(data)) 
    stop("column \"catch\" missing")
  object@data <- data
  
  ### get catch
  ### default: use (total) catch
  if (isFALSE(split_discards)) {
    catch <- object@data$catch
  } else  {
  ### alternative: use dead catch (landings + dead discards)
    if (!"landings" %in% names(data)) 
      stop("split of catch into landings/discards requested ", 
           "but column \"landings\" missing")
    if (!"discards" %in% names(data)) 
      stop("split of catch into landings/discards requested ", 
           "but column \"discards\" missing")
    if (!missing(discard_survival))
      object@data$discard_survival <- discard_survival
    catch <- object@data$landings + 
      object@data$discards * (1 - object@data$discard_survival)
  }
  
  ### calculate harvest rate
  object@data$harvest_rate <- catch/object@data$index
  
  object@value <- object@data$harvest_rate
  names(object@value) <- object@data$year
  object@value <- object@value[!is.na(object@value)]
  
  object@metric <- "catch"
  if (isTRUE(split_discards)) {
    object@metric <- ifelse(isTRUE(discard_survival < 1),
                            "dead catch", "landings")
  }
  
  if (!missing(units)) object@units <- units
  if (!missing(units_catch)) object@units_catch <- units_catch
  if (!missing(units_index)) object@units_index <- units_index
  
  if (missing(units) & !missing(units_catch) & !missing(units_index))
    object@units <- paste(units_catch, "/", units_index)
  
  return(object)
  
}

### ------------------------------------------------------------------------ ###
### HR validity ####
### ------------------------------------------------------------------------ ###
### validity checks
setValidity("HR", function(object) {
  if (isFALSE(object@hcr %in% c(NA, "rfb", "rb", "chr"))) {
    paste0("Unknown harvest control rule ", object@hcr, ". Must be ",
           "rfb, rb, or chr!")
  } else if (!identical(length(object@hcr), 1L)) {
    "slot hcr must be of length 1"
  } else if (!identical(length(object@units), 1L)) {
    "slot units must be of length 1"
  } else {
    TRUE
  }
})

### ------------------------------------------------------------------------ ###
### HR convience methods ####
### ------------------------------------------------------------------------ ###
#' @rdname value
#' @export
setMethod(f = "value", signature = "HR", 
          definition = function(object) {
            return(object@value)
          })

### print
setMethod(f = "print", signature = "HR", 
          definition = function(x) {
            cat(paste0("An object of class \"", class(x), "\".\n",
                       "Value(s) (based on ", x@metric, "): \n"))
            print(x@value)
          })

### show
setMethod(f = "show", signature = "HR", 
          definition = function(object) {
            cat(paste0("An object of class \"", class(object), "\".\n",
                       "Value(s) (based on ", object@metric, "): \n"))
            print(object@value)
          })

### summary
#' @rdname summary
#' @export
setMethod(
  f = "summary", signature = "HR",
  definition = function(object) {
    txt <- paste0(
      "An object of class \"", class(object), "\".\n",
      "Harvest rate values: ", sum(!is.na(object@data$harvest_rate)), "\n",
      "Catch observations: ", sum(!is.na(object@data$catch)), "\n",
      "Index observations: ", sum(!is.na(object@data$index)), "\n",
      "Harvest rate based on ", object@metric, "\n")
    cat(txt)
  }
)

### ------------------------------------------------------------------------ ###
### F class ####
### ------------------------------------------------------------------------ ###
#' @title F
#' 
#' @description  An S4 class to represent component F (the target harvest rate) of the chr rule.
#' 
#' This class (\code{F}) stores the input for the target harvest rate (if any) as well as the resulting target harvest rate.
#' 
#' @slot value The target harvest rate value.
#' @slot metric The metric for the harvest rate (e.g. catch, landings, or
#'  dead catch)
#' @slot data \code{data.frame}. The data (harvest rates) used for calculating the target harvest rate.
#' @slot yr_ref \code{numeric}. The years from which data are used.
#' @slot units \code{character}. The units of the harvest rate.
#' @slot HR \code{HR}. The harvest rate input data.
#' @slot indicator \code{F}. The indicator used to select years of the harvest rate.
#' @slot hcr \code{character}. The harvest control rule (hcr) for which the index is used. Only applicable to 'chr'.
#' @slot MSE \code{logical}. Is the harvest rate a generic value or was it calculated with stock-specific simulations (MSE)? Defaults to \code{FALSE}.
#' @slot multiplier \code{numeric}. Optional. Multiplier to adjust the target harvest rate. Only applicable if \code{MSE=TRUE}.
#' 
#' @name Ftarget-class
#' @title F
#' @export
setClass(
  Class = "F",
  slots = c(
    value = "numeric",
    metric = "character",
    data = "data.frame",
    yr_ref = "numeric",
    units = "character",
    HR = "HR",
    indicator = "f",
    hcr = "character",
    MSE = "logical",
    multiplier = "numeric"
  ),
  prototype = list(
    value = NA_real_,
    metric = "catch",
    data = data.frame(matrix(
      ncol = 2,
      nrow = 0,
      dimnames = list(NULL, c("year", "harvest_rate"))
    )),
    yr_ref = NA_real_,
    units = NA_character_,
    HR = new("HR"),
    indicator = new("f"),
    hcr = "chr",
    MSE = FALSE,
    multiplier = NA_real_
  )
)

### ------------------------------------------------------------------------ ###
### F methods ####
### ------------------------------------------------------------------------ ###
#' Calculation of the (relative) harvest rate target
#'
#' This function calculates the target harvest rate for chr rule.
#' 
#' Usually, this functions is used by providing a time series of (relative) harvest rate values (see \code{\link{HR}}) and a length-based indicator based on the mean catch length (see \code{\link{f}}). The functions then finds those years where the indicator values are above 1, indicating that the fishing pressure is likely below Fmsy, extracts the corresponding (relative) harvest rate values for these years, and returns the average of these values as the target harvest rate.
#' 
#' Alternatively, years can directly be specified with the argument \code{yr_ref} and the target harvest rate is then calculated as the average of the (relative) harvest rates for these years. See the ICES technical guidelines (ICES, 2022) for details.
#' 
#' If stock-specific simulations were conducted to derive the target harvest rate, the calculation may differ. Nevertheless, it is good practice to express the target harvest rate relative to the harvest of one or more years. This is useful when historical harvest rates are revised (e.g. because of a revision of historical biomass index values) because the target harvest rate will then be scaled accordingly. If the argument \code{MSE=TRUE}, it is possible to include a multiplier directly in the calculation of the target harvest with the argument \code{multiplier}.
#' 
#' If an object of class \code{F} is provided, its validity is checked.
#'
#' @param object The time series with (relative) harvest rate values. See \code{\link{HR}}.
#' @param indicator The length based indicator. See \code{\link{f}}.
#' @param units Optional. The units of the harvest rate. Can be derived automatically from argument \code{HR}.
#' @param yr_ref Optional. Allows direct specification of years to include in the calculation instead of using \code{indicator}.
#' @param MSE Optional. \code{TRUE/FALSE}. Is the harvest rate a generic value or was it calculated with stock-specific simulations (MSE)?
#' @param multiplier Optional. \code{numeric}. Multiplier to adjust the target harvest rate. Only used if \code{MSE=TRUE}.
#' @param ... Additional arguments. Not currently used.
#'
#' @references
#' ICES. 2022. ICES technical guidance for harvest control rules and stock assessments for stocks in categories 2 and 3. In Report of ICES Advisory Committee, 2022. ICES Advice 2022, Section 16.4.11, 20 pp. \url{https://doi.org/10.17895/ices.advice.19801564}.
#' 
#' Fischer, S. H., De Oliveira, J. A. A., Mumford, J. D., and Kell, L. T. 2023. Risk equivalence in data‐limited and data‐rich fisheries management: An example based on the ICES advice framework. Fish and Fisheries, 24: 231--247. \url{https://doi.org/10.1111/faf.12722}.
#'
#' Fischer, S. H., De Oliveira, J. A. A., Mumford, J. D., and Kell, L. T. 2022. Exploring a relative harvest rate strategy for moderately data-limited fisheries management. ICES Journal of Marine Science, 79: 1730--1741. \url{https://doi.org/10.1093/icesjms/fsac103}.
#'
#' @return An object of class \code{F} with the target harvest rate and the input data.
#' 
#' @section Warning:
#' For application in ICES, the target harvest rate should only be calculated in the first year the chr rule is used and the same value used in subsequent years.
#' 
#' @examples 
#' 
#' # load harvest rate time series and length-based indicator
#' data(ple7e_hr)
#' data(ple7e_f2)
#' # calculate target harvest rate
#' F <- F(ple7e_hr, ple7e_f2)
#' F
#' advice(F)
#' plot(F)
#'
#' # use reference years when using in following years
#' F(ple7e_hr, yr_ref = c(2016, 2019))
#' 
#' # full example with ple7e data
#' data(ple7e_length)
#' # calculate (pooled) length at first capture first
#' lc <- Lc(data = ple7e_length, pool = 2017:2021)
#' # calculate mean catch length
#' lmean <- Lmean(data = ple7e_length, Lc = lc, units = "mm")
#' # reference length
#' lref <- Lref(Lc = 264, Linf = 528)
#' # calculate component f
#' f <- f(Lmean = lmean, Lref = lref, units = "mm")
#' # harvest rate
#' data(ple7e_idx)
#' data(ple7e_catch)
#' df <- merge(ple7e_catch, ple7e_idx, all = TRUE) # combine catch & index data
#' hr <- HR(df, units_catch = "tonnes", units_index = "kg/hr")
#' # calculate (relative) target harvest rate
#' F <- F(hr, f)
#' F
#' advice(F)
#' plot(F)
#' 
#' # application in following years without updating target harvest rate
#' F <- F(hr, yr_ref = c(2016, 2019))
#' 
#' @rdname Ftarget
#' @export
setGeneric(
  name = "F",
  def = function(object, indicator, yr_ref, units, MSE, ...) {
    standardGeneric("F")
  },
  signature = c("object", "indicator")
)

### object = HR, indicator = F
#' @rdname Ftarget
#' @usage NULL
#' @export
setMethod(F,
          signature = c(object = "HR", indicator = "f"),
          function(object, indicator, yr_ref, units, MSE, multiplier, ...) {
  object <- calc_F(HR = object, indicator = indicator, units = units,
                   yr_ref = yr_ref, MSE = MSE, multiplier = multiplier)
  validObject(object)
  return(object)
})

### object = HR, indicator = missing + yr_ref
#' @rdname Ftarget
#' @usage NULL
#' @export
setMethod(F,
          signature = c(object = "HR", indicator = "missing"),
          function(object, indicator, yr_ref, units, MSE, multiplier, ...) {
  object <- calc_F(HR = object, indicator = indicator, units = units,
                   yr_ref = yr_ref, MSE = MSE, multiplier = multiplier)
  validObject(object)
  return(object)
})

### object = numeric, indicator = missing -> use as value
#' @rdname Ftarget
#' @usage NULL
#' @export
setMethod(F,
          signature = c(object = "numeric",
                        indicator = "missing"),
          function(object = new("F"), indicator, yr_ref, units, MSE, multiplier,
                   ...) {
  value <- object
  object <- new("F")
  object@value <- value
  if (!missing(units)) object@units <- units
  if (!missing(yr_ref)) object@yr_ref <- yr_ref
  validObject(object)
  return(object)
})

### object = F -> check validity
#' @rdname Ftarget
#' @usage NULL
#' @export
setMethod(F,
          signature = c(object = "F", indicator = "missing"),
          function(object, indicator, yr_ref, units, MSE, multiplier, ...) {
  validObject(object)
  return(object)
})


### ------------------------------------------------------------------------ ###
### F calculation ####
### ------------------------------------------------------------------------ ###
### calculate target harvest rate
calc_F <- function(object = new("F"), HR, indicator, yr_ref, units, MSE,
                   multiplier) {
  #browser()
  
  ### use indicator to select years, if supplied
  if (!missing(indicator)) {
    ### check if indicator values >=1 exist
    if (isTRUE(all(indicator@indicator$indicator < 1)))
      stop("All indicator values are <1. Impossible to select reference years!")
    object@indicator <- indicator
    ### find years where indicator >= 1
    yr_ref <- indicator@indicator$year[indicator@indicator$indicator >= 1]
  }
  object@yr_ref <- yr_ref
  
  ### extract harvest rates
  object@HR <- HR
  object@data <- object@HR@data[object@HR@data$year %in% 
                                  object@yr_ref, c("year", "harvest_rate")]
  if (isTRUE(nrow(object@data) < 1))
    stop(paste0("No harvest rate values available for reference years (",
                paste(object@yr_ref, collapse = ", "), "). ",
                "Impossible to calculate target harvest rate."))
  ### calculate target harvest rate
  object@value <- mean(object@data$harvest_rate, na.rm = TRUE)
  
  ### units
  if (!missing(units)) {
    object@units <- units
  } else if (!is.na(object@HR@units)) {
    object@units <- object@HR@units
  }
  ### catch metric
  object@metric <- object@HR@metric
  
  ### harvest rate origin: generic approach or MSE?
  if (!missing(MSE)) object@MSE <- MSE
  
  ### apply multiplier to harvest rate
  ### (only applicable after stock-specific MSE)
  if (isTRUE(object@MSE) & !missing(multiplier)) {
    object@multiplier <- multiplier
    object@value <- object@value * multiplier
  }
  
  return(object)
  
}

### ------------------------------------------------------------------------ ###
### F validity ####
### ------------------------------------------------------------------------ ###
### validity checks
setValidity("F", function(object) {
  if (!identical(length(object@value), 1L)) {
    "slot value must be of length 1"
  } else if (!identical(length(object@units), 1L)) {
    "slot units must be of length 1"
  } else if (!identical(length(object@hcr), 1L)) {
    "slot hcr must be of length 1"
  } else if (!identical(length(object@metric), 1L)) {
    "slot metric must be of length 1"
  } else if (!identical(length(object@MSE), 1L)) {
    "slot MSE must be of length 1"
  } else if (!identical(length(object@multiplier), 1L)) {
    "slot multiplier must be of length 1"
  } else {
    TRUE
  }
})

### ------------------------------------------------------------------------ ###
### F convenience methods ####
### ------------------------------------------------------------------------ ###

# #' @rdname summary
# #' @export
# setMethod(
#   f = "summary", signature = "F",
#   definition = function(object) {
#     txt <- (paste0(
#       paste(rep("-", 50), collapse = ""), "\n",
#       "component f:\n",
#       "last index year: ", object@yr_last, "\n",
#       "using a ", object@n1, " over ", object@n2, " ratio ",
#       "with a time lag of n0=", object@n0, "\n",
#       "index average (", paste(object@n1_yrs, collapse = ","),
#       ") = ",
#       object@n1_mean, "\n",
#       "index average (", paste(object@n2_yrs, collapse = ","),
#       ") = ",
#       object@n2_mean, "\n",
#       "ratio r = ", object@value, "\n",
#       paste0(paste(rep("-", 50), collapse = ""))
#     ))
#     cat(txt)
#   }
# )

#' @rdname value
#' @export
setMethod(
  f = "value", signature = "F",
  definition = function(object) {
    return(object@value)
  }
)

### print
setMethod(f = "print", signature = "F", 
          definition = function(x) {
  cat(paste0("An object of class \"", class(x), "\".\n",
             "Value: ", x@value, "\n"))
})

### show
setMethod(f = "show", signature = "F", 
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
  f = "advice", signature = "F",
  definition = function(object) {
    txt <- paste0(
      paste(rep("-", 80), collapse = ""), "\n",
      "MSY proxy harvest rate\n",
      paste(rep("-", 80), collapse = ""), "\n"
    )
    
    val_F <- paste0(ifelse(object@value > 100, 
                           round(object@value),
                           icesAdvice::icesRound(object@value)),
                    ifelse(!is.na(object@units), paste0(" ", object@units), ""))
    
    if (!isTRUE(object@MSE)) {
      txt_F1 <- paste0("HRMSYproxy: MSY proxy harvest rate (average of")
      txt_F2 <- paste0("  the ratio of catch to biomass index for the")
      txt_F3 <- paste0("  years for which f>1, where f=Lmean/LF=M)")
      
      txt2 <- paste0(
        paste0(format(txt_F1, width = 48), " | \n"),
        paste0(format(txt_F2, width = 48), " | \n"),
        paste0(
          format(txt_F3, width = 48), " | ",
          format(val_F, width = 29, justify = "right"),
          "\n"
        )
      )
    } else {
      txt_F1 <- "HRMSYproxy: MSY proxy harvest rate"
      txt_F2 <- "  (derived from stock-specific simulations)"
      
      txt2 <- paste0(
        paste0(format(txt_F1, width = 48), " | \n"),
        paste0(
          format(txt_F2, width = 48), " | ",
          format(val_F, width = 29, justify = "right"),
          "\n"
        )
      )
      
    }
    

    
    txt <- paste0(
      txt,
      txt2
    )
    
    cat(txt)
  }
)

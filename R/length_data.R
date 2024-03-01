#' @importFrom dplyr filter group_by ungroup summarise
#' @importFrom stats weighted.mean
NULL

### ------------------------------------------------------------------------ ###
### Length at first capture Lc ####
### ------------------------------------------------------------------------ ###


#' An S4 class to represent length at first capture
#'
#' This class (\code{Lc}) stores the input data (length frequencies) for the
#' length at first capture and the resulting length at first capture value(s).
#'
#' @slot value The length at first capture value(s)
#' @slot summary A summary of the length at first capture value(s)
#' @slot years The years used in the calculation of length at first capture
#' @slot pooled Are length data from several years combined (pooled)?
#' @slot averaged Are annual values from several years average?
#' @slot units The units for length data (e.g. cm)
#' @slot data The data (length frequencies) used in the calculation
#'
#' @rdname Lc-class
#' @export
setClass(
  Class = "Lc",
  slots = c(
    value = "numeric",
    summary = "data.frame",
    years = "numeric",
    pooled = "vector",
    averaged = "vector",
    units = "character",
    data = "data.frame"
  ),
  prototype = list(
    value = NA_real_,
    summary = data.frame(matrix(
      ncol = 5, nrow = 0,
      dimnames = list(NULL, c(
        "year", "Lmode", "Nmode",
        "Lc", "Nc"
      ))
    )),
    years = numeric(),
    pooled = FALSE,
    averaged = FALSE,
    units = character(),
    data = data.frame()
  )
)

#' Length at first capture Lc
#'
#' This function calculates length at first capture Lc from a length frequency
#' distribution for use with the rfb/chr rules.
#'
#' Length at first capture Lc is defined as the first length class, in which the
#' numbers of fish is at or above the mode of the distribution (the length class
#' with the highest number of fish).
#'
#' The length distribution is passed to \code{Lc} with the argument \code{data}.
#' \code{data} is ideally a \code{data.frame} with columns "year", "length", and
#' "numbers".
#'
#' The argument \code{pool} allows the pooling of length data from several years
#' in the estimation of Lc. If set to \code{FALSE} (default), Lc will be
#' calculated for each year, if set to \code{TRUE}, all years will be combined.
#' Alternatively, a vector of years can be provided and only these years will be
#' used.
#'
#' The argument \code{average} allows the averaging of annual Lc values over
#' several years. If set to \code{FALSE} (default), Lc will be
#' calculated for each year, if set to \code{TRUE}, Lc values from all years
#' will be averaged. Alternatively, a vector of years can be provided and only
#' these years will be used for the average.
#'
#' The optional arguments \code{lmin} and \code{lmax} allow removing of length
#' classes outside this range. \code{lstep} can be used to combine the length
#' into broader length classes. This can be useful if data are noisy with
#' several local minima/maxima and to smooth the length distribution.
#'
#' @section Note:
#' For application with the rfb or chr rule, Lc should be set once in the first
#' year of the implementation. At every subsequent application of the method,
#' the previous Lc should then be kept unless a substantial change happened
#' (e.g. because of changed in the fishery or fishery selectivity).
#'
#' @param data The input data with the length distribution. (see details below)
#' @param pool Pool data from several years in the calculation?
#'             \code{TRUE}/\code{FALSE} or a vector specifying years to use.
#' @param average Calculate Lc as the average of several annual values?
#'                \code{TRUE}/\code{FALSE} or a vector specifying years to use.
#' @param lmin Optional. Smallest length class to use. If defined, length
#'             classes below \code{lmin} are ignored.
#' @param lmax Optional. Largest length class to use. If defined, length
#'             classes above \code{lmax} are ignored.
#' @param lstep Optional. Size of length classes. Allows combining length
#'              classes into larger length classes. (see details below)
#' @param rounding Optional. The method used to round length classes when using
#'                 \code{lstep}. Defaults to \code{floor}, can also be
#'                 \code{ceiling} or \code{round}.
#' @param units Units of length data, e.g. "cm".
#' @param ... Additional arguments. Not currently used.
#'
#' @references
#' ICES. 2022. ICES technical guidance for harvest control rules and stock assessments for stocks in categories 2 and 3. In Report of ICES Advisory Committee, 2022. ICES Advice 2022, Section 16.4.11, 20 pp. \url{https://doi.org/10.17895/ices.advice.19801564}.
#'
#' @return An object of class \code{Lc}
#' 
#' @examples 
#' # example data for plaice
#' data(ple7e_length)
#' # annual length at first capture
#' lc <- Lc(data = ple7e_length)
#' lc
#' plot(lc)
#' 
#' # pool data over several years (recommended)
#' lc <- Lc(data = ple7e_length, pool = 2017:2021)
#' plot(lc)
#'
#' 
#' @export
setGeneric(
  name = "Lc",
  def = function(data, pool = FALSE, average = FALSE, lmin, lmax,
                 lstep, rounding = floor, units, ...) {
    standardGeneric("Lc")
  },
  signature = c("data")
)

### numeric -> use as Lc
#' @rdname Lc
setMethod(Lc,
  signature = c(data = "numeric"),
  function(data, pool = FALSE, average = FALSE, lmin, lmax, lstep,
           rounding = floor, units, ...) {
    ### create empty Lc
    object <- new("Lc")
    object@value <- data
    object@summary <- data.frame(year = NA, Lmode = NA, Nmode = NA, 
                                 Lc = data, Nc = NA)
    ### check validity
    object <- Lc(object)
    return(object)
  }
)

### Lc -> check validity
#' @rdname Lc
setMethod(Lc,
  signature = c(data = "Lc"),
  function(data, pool = FALSE, average = FALSE, lmin, lmax, lstep,
           rounding = floor, units, ...) {
    if (validObject(data)) {
      return(data)
    }
  }
)

### data.frame -> use as input data
#' @rdname Lc
setMethod(Lc,
  signature = c(data = "data.frame"),
  function(data, pool = FALSE, average = FALSE, lmin, lmax, lstep,
           rounding = floor, units, ...) {
    object <- new("Lc")

    ### check data.frame columns
    names(data) <- tolower(names(data))
    if (!"year" %in% names(data)) 
      stop("column 'year' missing in data.frame")
    if (!"length" %in% names(data)) 
      stop("column 'length' missing in data.frame")
    if (!"numbers" %in% names(data)) 
      stop("column 'numbers' missing in data.frame")
    
    ### set minimum length
    if (missing(lmin)) {
      lmin <- min(data$length, na.rm = TRUE)
    }
    data <- data[data$length >= lmin, ]
    ### set maximum length
    if (missing(lmax)) lmax <- max(data$length, na.rm = TRUE)
    data <- data[data$length <= lmax, ]
    ### set step size, steps defined by rounding
    if (!missing(lstep)) {
      data$length <- rounding(data$length / lstep) * lstep
    }
    ### pool data from several years, if requested
    if (!isFALSE(pool)) {
      if (isTRUE(pool)) {
        data$year <- NA
      } else if (is.numeric(pool)) {
        data <- data[data$year %in% pool, ]
        data$year <- NA
      }
      object@pooled <- TRUE
    }
    ### use average Lc? -> remove redundant years
    if (!isFALSE(average)) {
      if (is.numeric(average)) {
        data <- data[data$year %in% average, ]
      }
      object@averaged <- TRUE
    }
    ### length units
    if (!missing(units)) object@units <- units

    ### aggregate data - keep only year, length, numbers
    data <- data %>%
      dplyr::group_by(year, length) %>%
      dplyr::summarise(numbers = sum(numbers), .groups = "drop")
    object@data <- data
    ### find Lc per year (first length class where numbers >= half of mode)
    smry <- data %>%
      dplyr::group_by(year) %>%
      dplyr::summarise(
        Lmode = length[numbers == max(numbers)],
        Nmode = max(numbers),
        Lc = min(length[numbers >= max(numbers) / 2], na.rm = TRUE),
        Nc = min(numbers[length == Lc], na.rm = TRUE),
        .groups = "keep"
      )
    smry <- as.data.frame(smry)
    object@summary <- smry
    object@value <- smry$Lc

    ### average annual Lc values?
    if (!isFALSE(average)) {
      object@value <- mean(object@summary$Lc, na.rm = TRUE)
    }

    if (!all(is.na(smry$year))) {
      if (identical(length(object@value), length(object@summary$Lc))) {
        names(object@value) <- smry$year
      } else {
        names(object@value) <- "pooled"
      }
    }

    return(object)
  }
)

### print to screen
setMethod(
  f = "show", signature = "Lc",
  definition = function(object) {
    print(object@value)
  }
)
#' @rdname summary
#' @usage NULL
#' @export
setMethod(
  f = "summary", signature = "Lc",
  definition = function(object, ...) {
    print(object@summary)
  }
)
#' @rdname value
#' @usage NULL
#' @export
setMethod(
  f = "value", signature = "Lc",
  definition = function(object) {
    print(object@value)
  }
)

### ------------------------------------------------------------------------ ###
### mean catch length Lmean ####
### ------------------------------------------------------------------------ ###

#' An S4 class to represent mean catch length
#'
#' This class (\code{Lmean}) stores the input data (length frequencies) for the
#' mean catch length and the resulting mean catch length(s), above the length
#' at first capture (Lc).
#'
#' @slot value The mean catch length value(s)
#' @slot summary A summary of the mean catch length value(s)
#' @slot years The years for which mean catch length is calculated
#' @slot Lc The length at first capture
#' @slot include_Lc Include Lc in the calculation of the mean length? (default: \code{TRUE})
#' @slot units The units for length data (e.g. cm)
#' @slot data The data (length frequencies) used in the calculation
#'
#' @rdname Lmean-class
#' @export
setClass(
  Class = "Lmean",
  slots = c(
    value = "numeric",
    summary = "data.frame",
    years = "numeric",
    Lc = "Lc",
    include_Lc = "logical",
    units = "character",
    data = "data.frame"
  ),
  prototype = list(
    value = NA_real_,
    summary = data.frame(matrix(
      ncol = 3, nrow = 0,
      dimnames = list(NULL, c(
        "year", "Lc", "Lmean"
      ))
    )),
    years = numeric(),
    Lc = new("Lc"),
    include_Lc = TRUE,
    units = character(),
    summary = data.frame(matrix(
      ncol = 4, nrow = 0,
      dimnames = list(NULL, c("year", "catch_category", "length", "numbers"))
    ))
  )
)

#' Mean catch length Lmean
#'
#' This function calculates the mean catch length above the length of first 
#' capture. 
#'
#' The mean catch length is calculated as the mean length of fish in the catch
#' which are above the length of first capture (Lc). The mean catch length is
#' essentially the mean of lengths, weighted by the number of fish in the length
#' classes.
#'
#' The length distribution is passed to \code{Lmean} with the argument 
#' \code{data}. \code{data} is ideally a \code{data.frame} with columns "year", 
#' "length", and "numbers". An optional column, "catch_category", can be 
#' included to distinguish between categories such as "landings" and 
#' "discards".
#'
#' The mean catch length calculation only considers length classes above the 
#' length of first capture \code{Lc}. \code{Lc} can be provided as a single 
#' value (recommended) or annual values can be provided with a \code{data.frame}
#' with columns "year" and "Lc". Generally, it is recommended to use a single 
#' value for Lc and only change it if there are substantial changes between 
#' years.
#' 
#' If \code{Lc} is not provided, the input \code{data} is passed to \code{Lc()}
#' to estimate Lc.
#'
#' The optional arguments \code{lmin} and \code{lmax} allow removing of length
#' classes outside this range. \code{lstep} can be used to combine the length
#' into broader length classes. This can be useful if data are noisy with
#' several local minima/maxima and to smooth the length distribution.
#' 
#' By default, the calculation of the mean length includes individuals at the 
#' length of first capture (Lc). This can be manually turned off by 
#' providing the argument \code{include_Lc = FALSE}. If this is turned off,
#' only fish above Lc are considered.
#'
#'
#' @param data The input data with the length distribution. (see details below)
#' @param Lc Length of first capture. Either a single value used for all years
#'           or a data.frame with columns "year" and "Lc".
#' @param lmin Optional. Smallest length class to use. If defined, length
#'             classes below \code{lmin} are ignored.
#' @param lmax Optional. Largest length class to use. If defined, length
#'             classes above \code{lmax} are ignored.
#' @param lstep Optional. Size of length classes. Allows combining length
#'              classes into larger length classes. (see details below)
#' @param rounding Optional. The method used to round length classes when using
#'                 \code{lstep}. Defaults to \code{floor}, can also be
#'                 \code{ceiling} or \code{round}.
#' @param include_Lc Optional. Include individuals at the length of first
#'                    capture (Lc)? Defaults to \code{TRUE}. If set to \code{FALSE},
#'                    only individuals above Lc are considered.
#' @param units Units of length data, e.g. "cm".
#' @param ... Additional arguments. Not currently used.
#'
#'
#' @references
#' ICES. 2022. ICES technical guidance for harvest control rules and stock assessments for stocks in categories 2 and 3. In Report of ICES Advisory Committee, 2022. ICES Advice 2022, Section 16.4.11, 20 pp. \url{https://doi.org/10.17895/ices.advice.19801564}.
#'
#' @return An object of class \code{Lc}
#' 
#' @examples 
#' # example data for plaice
#' data(ple7e_length)
#' # calculate (pooled) length at first capture first
#' lc <- Lc(data = ple7e_length, pool = 2017:2021)
#' # mean catch length
#' lmean <- Lmean(data = ple7e_length, Lc = lc, units = "mm")
#' lmean
#' plot(lmean)
#'
#' 
#' @export
setGeneric(
  name = "Lmean",
  def = function(data, Lc, lmin, lmax, lstep, rounding = floor, 
                 include_Lc = TRUE, units, ...) {
    standardGeneric("Lmean")
  },
  signature = c("data", "Lc")
)

### numeric -> use as Lmean
#' @rdname Lmean
setMethod(Lmean,
  signature = c(data = "numeric", Lc = "missing"),
  function(data, Lc, lmin, lmax, lstep, rounding = floor, 
           include_Lc = TRUE, units, ...) {
    ### create empty Lc
    out <- new("Lmean")
    out@summary <- data.frame(year = seq(length(data)), Lc = NA, Lmean = data)
    out@value <- data
    return(out)
  }
)

### Lmean -> check validity
#' @rdname Lmean
setMethod(Lmean,
  signature = c(data = "Lmean", Lc = "missing"),
  function(data, Lc, lmin, lmax, lstep, rounding = floor, 
           include_Lc = TRUE, units, ...) {
    if (validObject(data)) {
      return(data)
    }
  }
)

### data = data.frame -> use as input data
### Lc = missing -> calculate with data
#' @rdname Lmean
setMethod(Lmean,
  signature = c(data = "data.frame", Lc = "missing"),
  function(data, Lc, lmin, lmax, lstep, rounding = floor, 
           include_Lc = TRUE, units, ...) {
    ### calculate Lc from data
    Lc <- Lc(
      data = data, lmin = lmin, lmax = lmax, lstep = lstep,
      rounding = rounding, units = units, ...
    )
    ### call Lc
    object <- Lmean(
      data = data, Lc = Lc, lmin = lmin, lmax = lmax, lstep = lstep,
      rounding = rounding, include_Lc = include_Lc, units = units, ...
    )
    return(Lc)
  }
)
### data = data.frame -> use as input data
### Lc = data.frame -> use as Lc
#' @rdname Lmean
setMethod(Lmean,
  signature = c(data = "data.frame", Lc = "data.frame"),
  function(data, Lc, lmin, lmax, lstep, rounding = floor, 
           include_Lc = include_Lc, units, ...) {
    ### extract Lc from data.frame
    if (!"year" %in% names(Lc)) stop("column 'year' missing in Lc data.frame")
    if (!"Lc" %in% names(Lc)) stop("column 'Lc' missing in Lc data.frame")
    if (!"Lmode" %in% names(Lc)) Lc$Lmode <- NA
    if (!"Nmode" %in% names(Lc)) Lc$Nmode <- NA
    if (!"Nc" %in% names(Lc)) Lc$Nc <- NA
    Lc <- new("Lc")
    Lc@summary <- data.frame(year = Lc$year, Lmode = Lc$Lmode, Nmode = Lc$Lmode,
                             Lc = Lc$Lc, Nc = Lc$Nmode)
    
    ### call Lc
    object <- Lmean(
      data = data, Lc = Lc, lmin = lmin, lmax = lmax, lstep = lstep,
      rounding = rounding, include_Lc = include_Lc, units = units, ...
    )
    return(object)
  }
)
### data = data.frame -> use as input data
### Lc = Lc -> use as Lc
#' @rdname Lmean
setMethod(Lmean,
  signature = c(data = "data.frame", Lc = "Lc"),
  function(data, Lc, lmin, lmax, lstep, rounding = floor, 
           include_Lc = include_Lc, units, ...) {
    object <- new("Lmean")
    if (!missing(units)) object@units <- units
    if (!missing(include_Lc)) object@include_Lc <- include_Lc
    
    ### check if required columns exist in data
    if (!"year" %in% names(data))
      stop("column 'year' missing in data")
    if (!"length" %in% names(data))
      stop("column 'length' missing in data")
    if (!"numbers" %in% names(data))
      stop("column 'numbers' missing in data")
    
    ### process length data
    ### set minimum length
    if (missing(lmin)) {
      lmin <- min(data$length, na.rm = TRUE)
    }
    data <- data[data$length >= lmin, ]
    ### set maximum length
    if (missing(lmax)) lmax <- max(data$length, na.rm = TRUE)
    data <- data[data$length <= lmax, ]
    ### set step size, steps defined by rounding
    if (!missing(lstep)) {
      data$length <- rounding(data$length / lstep) * lstep
    }
    object@data <- data
    
    ### get years with data
    years <- sort(unique(data$year))
    object@years <- years
    
    ### add Lc
    ### by year
    if (isTRUE(nrow(Lc@summary) > 1) & 
        isFALSE(Lc@pooled) & isFALSE(Lc@averaged)) {
      years_Lc <- Lc@summary$year
      
      ### keep only data years for which Lc values exist
      if (any(years %in% years_Lc)) {
        warning(paste0("annual Lc values provided but not for all years in",
                       "data,\n  removing years without Lc from data!"))
        data <- data %>% filter(year %in% years_Lc)
      }
      
      ### add annual Lc values to data
      data <- full_join(x = data, 
                        y = Lc@summary %>% select(year, Lc),
                        by = "year")
      
    ### same value for all years
    } else {
      data$Lc <- Lc@value
    }
    object@Lc <- Lc
    
    ### check suitability of Lc value
    if (all(data$Lc < data$length) | all(data$Lc > data$length)) {
      warning("Lc is outside the range of the length data")
    }
    
    ### calculate mean length above Lc
    tmp_smry <- data %>%
      dplyr::group_by(year, length, Lc) %>%
      dplyr::summarise(numbers = sum(numbers), .groups = "keep") %>%
      dplyr::ungroup(length)
    ### include Lc?
    if (isTRUE(object@include_Lc)) {
      tmp_smry <- tmp_smry %>% dplyr::filter(length >= Lc)
    } else {
      tmp_smry <- tmp_smry %>% dplyr::filter(length > Lc)
    }
    ### mean 
    object@summary <-  tmp_smry %>%
      ### mean of length classes, weighted by catch numbers at length
      dplyr::summarise(Lmean = stats::weighted.mean(x = length, w = numbers),
                       .groups = "keep")

    ### add NAs for year with missing data 
    ### -> useful for plotting later so that line does not pass through missing
    ###    years
    yrs_full <- min(object@years):max(object@years)
    yrs_missing <- setdiff(yrs_full, object@years)
    if (isTRUE(length(yrs_missing) > 0)) {
      object@summary <- dplyr::bind_rows(object@summary, data.frame(year = yrs_missing)) %>%
        dplyr::arrange(year)
    }
    
    ### fill value slot
    object@value <- object@summary$Lmean
    names(object@value) <- object@summary$year
    
    return(object)
  }
)

### print to screen
setMethod(
  f = "show", signature = "Lmean",
  definition = function(object) {
    print(object@value)
  }
)
#' @rdname summary
#' @usage NULL
#' @export
setMethod(
  f = "summary", signature = "Lmean",
  definition = function(object, ...) {
    print(object@summary)
  }
)
#' @rdname value
#' @usage NULL
#' @export
setMethod(
  f = "value", signature = "Lmean",
  definition = function(object) {
    print(object@value)
  }
)

### ------------------------------------------------------------------------ ###
### LF=M reference length calculation ####
### ------------------------------------------------------------------------ ###

#' An S4 class to represent the reference catch length
#'
#' This class (\code{Lref}) stores the value of the reference catch length, 
#' e.g. LF=M.
#'
#' @param value The reference catch length.
#' @param basis The basis for the calculation, defaults to "LF=M".
#' @param Lc The length at first capture.
#' @param Linf The asymptotic length from a von Bertalanffy growth model.
#' @param Mk The ratio of natural mortality M to von Bertalanffy k.
#' @param gamma Links fishing mortality F to natural mortality M
#'              to set the proxy for MSY (see details).
#' @param theta Alternative option to link M and k.
#' @param years Years, if annual values provided.
#' @param units The units for length data (e.g. cm)
#'
#' @rdname Lref-class
#' @export
setClass(
  Class = "Lref",
  slots = c(
    value = "numeric",
    basis = "character",
    Lc = "numeric",
    Linf = "numeric",
    Mk = "numeric",
    gamma = "numeric",
    theta = "numeric",
    years = "numeric",
    units = "character"
  ),
  prototype = list(
    value = NA_real_,
    basis = "LF=M",
    Lc = numeric(),
    Linf = numeric(),
    Mk = 1.5, 
    gamma = 1, 
    theta = 2/3,
    years = NA_integer_,
    units = character()
  )
)


#' Reference catch length
#'
#' This function calculates the reference catch length.
#'
#' The default is to calculate the MSY proxy reference length following 
#' Beverton & Holt (1957) and as derived by Jardim et al. (2015):
#'
#' \code{(theta * Linf + Lc * (gamma + 1)) / (theta + gamma + 1)}
#' 
#' where \code{Linf} is the asymptotic length of a von Bertalanffy growth model,
#' \code{Lc} the length of first capture, 
#' theta links von Bertalanffy individual growth parameter k and natural
#' mortality (M) through \code{k = theta * M} and gamma links fishing mortality 
#' F to M through \code{F = gamma * M}. The default reference length calculation
#' assumes \code{theta = 2/3}, i.e. that \code{M/k = 1.5} and that 
#' \code{gamma = 1}, i.e. that \code{F = M} can be used as a proxy for MSY.
#' The ratio M/k can be set directly with the argument \code{Mk} or indirectly
#' with \code{theta} which defaults to \code{1/Mk}.
#' 
#' @section Warning: Changing the default parameters is discouraged. 
#' Any change to the default parameters should be well justified.
#' 
#' The reference length is usually set once the first time the rfb rule is 
#' applied and should then be kept constant unless there a substantial changes
#' in the fishery or fishery selectivity.
#'
#' @param value Optional. The reference length value, if already known.
#' @param basis The basis for the calculation, defaults to "LF=M".
#' @param Lc The length at first capture.
#' @param Linf The asymptotic length from a von Bertalanffy growth model.
#' @param Mk The ratio of natural mortality M to von Bertalanffy k.
#' @param gamma Links fishing mortality F to natural mortality M
#'              to set the proxy for MSY (see details).
#' @param theta Alternative option to link M and k.
#' @param units The units for length data (e.g. cm)
#' @param ... Additional arguments. Not currently used.
#'
#' @references
#' Beverton, R. J. H., and Holt, S. J. 1957. On the Dynamics of Exploited Fish Populations. Fishery Investigation Series 2. HMSO for Ministry of Agriculture, Fisheries and Food, London. 533 pp.
#' ICES. 2022. ICES technical guidance for harvest control rules and stock assessments for stocks in categories 2 and 3. In Report of ICES Advisory Committee, 2022. ICES Advice 2022, Section 16.4.11, 20 pp. \url{https://doi.org/10.17895/ices.advice.19801564}.
#' Jardim, E., Azevedo, M., and Brites, N. M. 2015. Harvest control rules for data limited stocks using length-based reference points and survey biomass indices. Fisheries Research, 171: 12–19. \url{https://doi.org/10.1016/j.fishres.2014.11.013}.
#'
#' @return An object of class \code{Lref}
#'
#' @examples
#' # calculate MSY proxy LF=M
#' Lref(Lc = 26.4, Linf = 58.5)
#'
#' @export
Lref <- function(value,
                 basis = "LF=M",
                 Lc, Linf,
                 Mk = 1.5, gamma = 1, theta = 1 / Mk, units, ...) {
  
  object <- new("Lref")
  object@basis <- basis
  if (!missing(units)) object@units <- units
  if (!missing(value)) {
    object@value <- value
    return(object)
  }
  
  ### get Lc
  if (is(Lc, "Lc")) Lc <- Lc@value
  
  ### check parameters for LF=M
  if (identical(basis, "LF=M")) {
    if (missing(Lc)) {
      stop("Lc missing")
    } else {
      object@Lc <- Lc
    }
    if (missing(Linf)) {
      stop("Linf missing")
    } else {
      object@Linf <- Linf
    }
    object@theta <- theta
    object@gamma <- gamma
    
    object@value <- (theta * Linf + Lc * (gamma + 1)) / (theta + gamma + 1)
    
  } else {
    stop("unknown basis")
  }
  
  return(object)
}

setMethod(
  f = "show", signature = "Lref",
  definition = function(object) {
    print(object@value)
  }
)
#' @rdname value
#' @usage NULL
#' @export
setMethod(
  f = "value", signature = "Lref",
  definition = function(object) {
    print(object@value)
  }
)



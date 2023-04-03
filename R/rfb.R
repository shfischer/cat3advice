#' @include r.R
#' @include f.R
#' @include b.R
#' @include m.R
#' @include A.R
#' @importFrom icesAdvice icesRound
#' @importFrom utils capture.output tail
NULL

### ------------------------------------------------------------------------ ###
### rfb class ####
### ------------------------------------------------------------------------ ###

#' @title An S4 class to represent the rfb rule.
#'
#' @description This class contains the components of the rfb rule (\code{rfb_A},
#' \code{rfb_r}, \code{rfb_f}, \code{rfb_b}, \code{rfb_m}).
#'
#' @slot advice The value of the catch advice.
#' @slot advice_landings Landings corresponding to the catch advice.
#' @slot advice_uncapped The value of the catch advice without the uncertainty cap.
#' @slot units The unit (e.g. tonnes) of the catch advice.
#' @slot advice_metric The advice metric, 'catch' or 'landings'.
#' @slot frequency The advice frequence (annual/biennial).
#' @slot years The years for which the advice is valid.
#' @slot A Component A (the reference catch).
#' @slot r Component r (the biomass index ratio).
#' @slot f Component f (the fishing pressure proxy).
#' @slot b Component b (the biomass safeguard).
#' @slot m Component m (the multiplier).
#' @slot cap Uncertainty cap (stability clause, restricts changes in advice).
#' @slot cap_lower Maximum allowed reduction in advice in \%, e.g. -30.
#' @slot cap_upper Maximum allowed increase in advice in \%, e.g. 20.
#' @slot change Change in advice compared to previous advice.
#' @slot change_uncapped Change in advice compared to previous advice before application of the uncertainty cap.
#' @slot discard_rate Discard rate (\%).
#'
#' @rdname rfb-class
#' @export
setClass(
  Class = "rfb",
  slots = c(
    advice = "numeric",
    advice_landings = "numeric",
    advice_uncapped = "numeric",
    units = "character",
    advice_metric = "character",
    frequency = "character",
    years = "numeric",
    A = "A",
    r = "r",
    f = "f",
    b = "b",
    m = "m",
    cap = "logical",
    cap_lower = "numeric",
    cap_upper = "numeric",
    change = "numeric",
    change_uncapped = "numeric",
    discard_rate = "numeric"
  ),
  prototype = list(
    advice = NA_real_,
    advice_landings = NA_real_,
    advice_uncapped = NA_real_,
    units = NA_character_,
    advice_metric = NA_character_,
    frequency = "biennial",
    years = NA_real_,
    A = new("rfb_A"),
    r = new("rfb_r"),
    f = new("rfb_f"),
    b = new("rfb_b"),
    m = new("rfb_m"),
    cap = NA,
    cap_lower = -30,
    cap_upper = 20,
    change = NA_real_,
    change_uncapped = NA_real_,
    discard_rate = NA_real_
  )
)


### ------------------------------------------------------------------------ ###
### rfb calculation ####
### ------------------------------------------------------------------------ ###
#' rfb rule
#'
#' This function applies the rfb rule.
#' 
#' The function requires the elements of the rfb rule: A (the reference)
#' catch, r (the biomass index ratio), f (the fising pressure proxy), 
#' b (the biomass safeguard) and m (the multiplier). See the [A()], 
#' [r()], [f()], [b()], and [m()] help files for details.
#' 
#' @param object Optional. An object of class \code{rfb}.
#' @param A The reference catch. Should be an object of class \code{A}, see [A()].
#' @param r The biomass index ratio. Should be an object of class \code{r}, see [r()].
#' @param f The fishing pressure proxy. Should be an object of class \code{f}, see [f()].
#' @param b The biomass safeguard. Should be an object of class \code{b}, see [b()].
#' @param m The multiplier. Should be an object of class \code{m}, see [m()].
#' @param cap \code{logical}. The uncertainty cap (stability clause). Defaults to \code{TRUE}
#' @param cap_upper Optional. \code{numeric}. The maximum allowed increase in the catch advice in \%. Default to +20.
#' @param cap_lower Optional. \code{numeric}. The maximum allowed decrease in the catch advice in \%. Default to -20.
#' @param years Optional. \code{numeric}. The years for which the advice should be given.
#' @param frequency Optional. The frequency of the advice ('annual'/'biennial'/'triennial'). Defaults to 'biennial'.
#' @param  discard_rate Optional. The discard rate for the advice (\code{numeric}). If provided, advice values for catch and landings are given.
#' @param ... Additional parameters. Not used.
#'  
#' @section Warning:
#' For application in ICES, do not change the default parameters (frequency, 
#' stability clause, etc) unless the changes are supported by case-specific
#' simulations.
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
#' @return An object of class \code{rfb}.
#'
#' @name rfb
#' @export
NULL

#' @rdname rfb
#' @export
setGeneric(name = "rfb", 
           def = function(object, A, r, f, b, m, cap = "conditional", 
                          cap_upper = 20, cap_lower = -30, years, 
                          frequency = "biennial",
                          discard_rate = NA,
                          ...) 
             standardGeneric("rfb"),
           signature = c("object", "A", "r", "f", "b", "m"))
### object = missing, A/r/f/b/m = A/r/f/b/m
#' @rdname rfb
#' @usage NULL
#' @export
setMethod(rfb,
          signature = c(object = "missing", 
                        A = "A", r = "r", f = "f", b = "b",
                        m = "m"),
          function(A, r, f, b, m,
                   cap,
                   cap_upper, cap_lower,
                   years, frequency,
                   discard_rate = NA,
                   ...) {#browser()
  object <- rfb_calc(A = A, r = r, f = f, b = b, m = m,
                     cap = cap, cap_upper = cap_upper, cap_lower = cap_lower,
                     year = years, frequency = frequency, 
                     discard_rate = discard_rate, ... = ...)
  return(object)
})
### object = missing, A/r/f/b/m = numeric
#' @rdname rfb
#' @usage NULL
#' @export
setMethod(rfb,
          signature = c(object = "missing", 
                        A = "numeric", r = "numeric", f = "numeric", 
                        b = "numeric", m = "numeric"),
          function(A, r, f, b, m,
                   cap,
                   cap_upper, cap_lower,
                   years, frequency,
                   discard_rate = NA,
                   ...) {#browser()
  object <- rfb_calc(A = A, r = r, f = f, b = b, m = m,
                     cap = cap, cap_upper = cap_upper, cap_lower = cap_lower,
                     year = years, frequency = frequency, 
                     discard_rate = discard_rate, ... = ...)
  return(object)
})
### object = rfb, A/r/f/b/m = missing -> check validity
#' @rdname rfb
#' @usage NULL
#' @export
setMethod(rfb,
          signature = c(object = "rfb", 
                        A = "missing", r = "missing", f = "missing", 
                        b = "missing", m = "missing"),
          function(object, 
                   A = object@A, r = object@r, f = object@f, 
                   b = object@b, m = object@m,
                   cap = "conditional",
                   cap_upper = 20, cap_lower = -30,
                   years, frequency = "biennial",
                   discard_rate = NA,
                   ...) {
  ### check validity
  validObject(object)
  ### update object if arguments provided
  object <- rfb_calc(object = object,
                     cap = cap, cap_upper = cap_upper, cap_lower = cap_lower,
                     year = years, frequency = frequency, 
                     discard_rate = discard_rate, ... = ...)
  return(object)
  
})
### object = rfb, A/r/f/b/m = A/r/f/b/m -> check validity & update
#' @rdname rfb
#' @usage NULL
#' @export
setMethod(rfb,
          signature = c(object = "rfb", 
                        A = "A", r = "r", f = "f", 
                        b = "b", m = "m"),
          function(object, 
                   A = object@A, r = object@r, f = object@f, 
                   b = object@b, m = object@m,
                   cap = "conditional",
                   cap_upper = 20, cap_lower = -30,
                   years, frequency = "biennial",
                   discard_rate = NA,
                   ...) {
  ### check validity
  validObject(object)
  ### update object
  object <- rfb_calc(object = object,
                     A = A, r = r, f = f, b = b, m = m,
                     cap = cap, cap_upper = cap_upper, cap_lower = cap_lower,
                     year = years, frequency = frequency, 
                     discard_rate = discard_rate, ... = ...)
  return(object)
})

### ------------------------------------------------------------------------ ###
### rfb calculation ####
### ------------------------------------------------------------------------ ###
rfb_calc <- function(object = new("rfb"), 
                     A = object@A, r = object@r, f = object@f, 
                     b = object@b, m = object@m,
                     cap = "conditional",
                     cap_upper = 20,
                     cap_lower = -30,
                     years,
                     frequency = "biennial",
                     discard_rate = NA,
                     ...) {
  #browser()

  ### convert all components into corresponding classes and check validity
  if (!missing(A)) object@A <- rfb_A(A, hcr = "rfb")
  if (!missing(r)) object@r <- rfb_r(r)
  if (!missing(f)) object@f <- rfb_f(f)
  if (!missing(b)) object@b <- rfb_b(b)
  if (!missing(m)) {
    object@m <- m(m)
  } else if (is.na(object@m@value)) {
    ### use default multiplier if missing
    object@m <- rfb_m()
  }
  if (!missing(cap_upper)) object@cap_upper <- cap_upper
  if (!missing(cap_lower)) object@cap_lower <- cap_lower
  ### check arguments
  if (!missing(frequency)) 
    frequency <- match.arg(arg = frequency, 
                           choices = c("biennial", "annual", "triennial"))
  if (!missing(cap)) 
    cap <- match.arg(arg = cap, choices = c("conditional", TRUE, FALSE))

  ### calculate r*f*b*m
  factor <- object@r@value * object@f@value * object@b@value * object@m@value

  ### calculate new catch advice
  object@advice_uncapped <- object@advice <- object@A@value * factor
  
  ### advice change
  object@change <- (object@advice_uncapped/object@A@value - 1)*100
  object@change_uncapped <- object@change

  ### uncertainty cap / catch constraint
  if (!isFALSE(cap)) {

    ### check if cap needs to be applied
    cap_consider <- cap
    if (identical(cap, "conditional"))
      cap_consider <- ifelse(object@b@value < 1, FALSE, TRUE)
    if (isFALSE(cap_consider %in% c(TRUE, FALSE, "conditional")))
      stop("unknown uncertainty cap condition")

    ### apply cap if required
    object@cap <- FALSE
    if (isTRUE(cap_consider)) {
      ### cap_upper/cap_lower are values in percent
      object@change_uncapped <- object@change
      if (isTRUE(object@change_uncapped > object@cap_upper)) {
        object@advice <- object@A@value * (100 + object@cap_upper)/100
        object@cap <- TRUE
      } else if (isTRUE(object@change_uncapped < object@cap_lower)) {
        ### use "+" because cap_lower is negative value
        object@advice <- object@A@value * (100 + object@cap_lower)/100
        object@cap <- TRUE
      }
    }
    
  }
  object@change <- (object@advice/object@A@value - 1)*100
  
  ### discards
  if (!is.na(discard_rate)) {
    object@discard_rate <- discard_rate
    object@advice_landings <- object@advice * (1 - object@discard_rate/100)
  } else {
    object@advice_landings <- object@advice
  }
  
  ### advice years
  object@frequency <- match.arg(frequency)
  ### if advice years missing, try to guess from previous advice
  if (missing(years)) {
    if (!is.na(object@A@avg_years)) {
      object@years <- seq(from = tail(sort(object@A@avg_years), 1) + 1, 
                          length.out = switch(object@frequency, 
                                              "annual" = 1, 
                                              "biennial" = 2, 
                                              "triennial" = 3))
    } else {
      object@years <- 1:2
    }
  }

  return(object)
  
}

### ------------------------------------------------------------------------ ###
### rfb convenience methods ####
### ------------------------------------------------------------------------ ###

### value
#' @rdname value
#' @export
setMethod(f = "value", signature = "rfb", 
          definition = function(object) {
            return(object@advice)
})

### print
setMethod(f = "print", signature = "rfb", 
          definition = function(x) {
            cat(paste0("An object of class \"", class(x), "\".\n",
                       "Value: ", x@advice, "\n"))
})

### show
setMethod(f = "show", signature = "rfb", 
          definition = function(object) {
            cat(paste0("An object of class \"", class(object), "\".\n",
                       "Value: ", object@advice, "\n"))
})

### ------------------------------------------------------------------------ ###
### ICES advice style table ####
### ------------------------------------------------------------------------ ###
#' @rdname advice
#' @usage NULL
#' @export
setMethod(
  f = "advice", signature = "rfb",
  definition = function(object) {
    
    ### rfb rule components
    txt_A <- paste0(capture.output(advice(object@A)), collapse = "\n")
    txt_r <- paste0(capture.output(advice(object@r)), collapse = "\n")
    txt_f <- paste0(capture.output(advice(object@f)), collapse = "\n")
    txt_b <- paste0(capture.output(advice(object@b)), collapse = "\n")
    txt_m <- paste0(capture.output(advice(object@m)), collapse = "\n")

    object@units <- ifelse(!is.na(object@A@units), 
                           paste0(" ", object@A@units), "")
    ### rfb calculation (uncapped advice)
    rfb_txt <- "RFB calculation (r*f*b*m)"
    rfb_val <- paste0(icesAdvice::icesRound(object@advice_uncapped), 
                      object@units)
    txt_rfb <- paste0(
      format(rfb_txt, width = 48), " | ",
      format(rfb_val, width = 29, justify = "right"), "\n")
    ### stability clause (uncertainty cap)
    cap_txt1 <- paste0("Stability clause (+", object@cap_upper, "%/",
                       object@cap_lower, "% compared to Ay,")
    cap_txt2 <- paste0("   only applied if b=1)")
    cap_val1 <- ifelse(object@cap, "Applied", "Not applied")
    cap_val2 <- ifelse(object@cap, object@advice/object@A@value, "")
    txt_cap <- paste0(format(cap_txt1, width = 48), " | \n",
                      format(cap_txt2, width = 48), " | ",
                      format(cap_val1, width = 13, justify = "right"), " | ",
                      format(cap_val2, width = 13, justfify = "right"), "\n")
    ### catch advice
    catch_adv_txt1 <- paste0("Catch advice for ", 
                             paste0(object@years, collapse = " and "))
    catch_adv_txt2 <- ifelse(isTRUE(object@cap),
                             "   (Ay * stability clause)",
                             "   (Ay * r * f * b * m)")
    catch_adv_val <- paste0(icesAdvice::icesRound(object@advice), object@units)
    txt_catch_adv <- paste0(format(catch_adv_txt1, width = 48), " | \n",
                            format(catch_adv_txt2, width = 48), " | ",
                            format(catch_adv_val, width = 29, 
                                   justify = "right"), "\n")
    ### discards
    if (!is.na(object@discard_rate)) {
      disc_rate_txt <- "Discard rate"
      disc_rate_val <- paste0(icesAdvice::icesRound(object@discard_rate), 
                              "%")
      land_txt <- "Projected landings corresponding to advice"
      land_val <- paste0(icesAdvice::icesRound(object@advice_landings),
                         object@units)
      txt_disc_land <- paste0(format(disc_rate_txt, width = 48), " | ",
                              format(disc_rate_val, width = 29, 
                                     justify = "right"), "\n",
                              format(land_txt, width = 48), " | ",
                              format(land_val, width = 29, 
                                     justify = "right"), "\n")
    } else {
      txt_disc_land <- ""
    }
    ### advice change
    change_txt <- "% advice change"
    change_val <- paste0(icesAdvice::icesRound(object@change), "%")
    txt_change <- paste0(format(change_txt, width = 48), " | ",
                         format(change_val, width = 29, 
                                justify = "right"), "\n")
    
    txt <- paste0(
      txt_A, "\n", txt_r, "\n", txt_f, "\n", txt_b, "\n", txt_m, "\n",
      txt_rfb,
      txt_cap,
      txt_catch_adv,
      txt_disc_land,
      txt_change
    )
    cat(txt)
  }
  
)


#' @include r.R
#' @include b.R
#' @include m.R
#' @include A.R
#' @importFrom icesAdvice icesRound
#' @importFrom utils capture.output tail
NULL

### ------------------------------------------------------------------------ ###
### rb class ####
### ------------------------------------------------------------------------ ###

#' @title An S4 class to represent the rb rule.
#'
#' @description This class contains the components of the rb rule (\code{rb_A},
#' \code{rb_r}, \code{rb_b}, \code{rb_m}).
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
#' @slot b Component b (the biomass safeguard).
#' @slot m Component m (the multiplier).
#' @slot cap Uncertainty cap (stability clause, restricts changes in advice).
#' @slot cap_lower Maximum allowed reduction in advice in \%, e.g. -30.
#' @slot cap_upper Maximum allowed increase in advice in \%, e.g. 20.
#' @slot change Change in advice compared to previous advice.
#' @slot change_uncapped Change in advice compared to previous advice before application of the uncertainty cap.
#' @slot discard_rate Discard rate (\%).
#'
#' @rdname rb-class
#' @export
setClass(
  Class = "rb",
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
    A = new("rb_A"),
    r = new("rb_r"),
    b = new("rb_b"),
    m = new("rb_m"),
    cap = NA,
    cap_lower = -30,
    cap_upper = 20,
    change = NA_real_,
    change_uncapped = NA_real_,
    discard_rate = NA_real_
  )
)


### ------------------------------------------------------------------------ ###
### rb calculation ####
### ------------------------------------------------------------------------ ###
#' rb rule
#'
#' This function applies the rb rule.
#' 
#' The function requires the elements of the rb rule: A (the reference)
#' catch, r (the biomass index ratio), f (the fising pressure proxy), 
#' b (the biomass safeguard) and m (the multiplier). See the help files for details \code{\link{A}}, v\code{\link{r}}, \code{\link{b}}, and \code{\link{m}}.
#' 
#' @param object Optional. An object of class \code{rfb}.
#' @param A The reference catch. Should be an object of class \code{A}, see \code{\link{A}}.
#' @param r The biomass index ratio. Should be an object of class \code{r}, see \code{\link{r}}.
#' @param b The biomass safeguard. Should be an object of class \code{b}, see \code{\link{b}}.
#' @param m The multiplier. Should be an object of class \code{m}, see \code{\link{m}}.
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
#' ICES. 2025. ICES Guidelines - Advice rules for stocks in category 2 and 3. Version 3. ICES Guidelines and Policies - Advice Technical Guidelines. 31 pp. \url{https://doi.org/10.17895/ices.pub.28506179}.
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
#' @return An object of class \code{rb}.
#' 
#' @examples 
#' #' # calculate elements of rb rule for plaice
#' # reference catch
#' data(ple7e_catch)
#' A <- A(object = ple7e_catch, basis = "advice", units = "tonnes", advice_metric = "catch")
#' # biomass index trend
#' data(ple7e_idx)
#' r <- r(ple7e_idx)
#' # biomass safeguard
#' b <- b(ple7e_idx)
#' # multiplier
#' m <- m(hcr = "rb", k = 0.1)
#' # apply rb rule
#' advice <- rb(A = A, r = r, b = b, m = m, discard_rate = 27)
#' advice
#' advice(advice)
#' 
#' ### application in subsequent years (without updating reference levels)
#' A <- A(object = ple7e_catch, basis = "advice", units = "tonnes", advice_metric = "catch")
#' r <- r(ple7e_idx)
#' b <- b(ple7e_idx, yr_ref = 2007) # use reference year for Itrigger
#' m <- m(0.95) # keep multiplier
#' advice <- rb(A = A, r = r, b = b, m = m, discard_rate = 27)
#' advice
#' advice(advice)
#'
#' @name rb
#' @export
NULL

#' @rdname rb
#' @export
setGeneric(name = "rb", 
           def = function(object, A, r, b, m, cap = "conditional", 
                          cap_upper = 20, cap_lower = -30, years, 
                          frequency = "biennial",
                          discard_rate = NA,
                          ...) 
             standardGeneric("rb"),
           signature = c("object", "A", "r", "b", "m"))
### object = missing, A/r/b/m = A/r/b/m
#' @rdname rb
#' @usage NULL
#' @export
setMethod(rb,
          signature = c(object = "missing", 
                        A = "A", r = "r", b = "b",
                        m = "m"),
          function(A, r, b, m,
                   cap,
                   cap_upper, cap_lower,
                   years, frequency,
                   discard_rate = NA,
                   ...) {#browser()
  object <- rb_calc(A = A, r = r, b = b, m = m,
                    cap = cap, cap_upper = cap_upper, cap_lower = cap_lower,
                    years = years, frequency = frequency, 
                    discard_rate = discard_rate, ... = ...)
  return(object)
})
### object = rb, A/r/b/m = missing -> check validity
#' @rdname rb
#' @usage NULL
#' @export
setMethod(rb,
          signature = c(object = "rb", 
                        A = "missing", r = "missing", 
                        b = "missing", m = "missing"),
          function(object, 
                   A = object@A, r = object@r, 
                   b = object@b, m = object@m,
                   cap = "conditional",
                   cap_upper = 20, cap_lower = -30,
                   years, frequency = "biennial",
                   discard_rate = NA,
                   ...) {
  ### check validity
  validObject(object)
  ### update object if arguments provided
  object <- rb_calc(object = object,
                     cap = cap, cap_upper = cap_upper, cap_lower = cap_lower,
                     years = years, frequency = frequency, 
                     discard_rate = discard_rate, ... = ...)
  return(object)
  
})
### object = rb, A/r/b/m = A/r/b/m -> check validity & update
#' @rdname rb
#' @usage NULL
#' @export
setMethod(rb,
          signature = c(object = "rb", 
                        A = "A", r = "r", 
                        b = "b", m = "m"),
          function(object, 
                   A = object@A, r = object@r, 
                   b = object@b, m = object@m,
                   cap = "conditional",
                   cap_upper = 20, cap_lower = -30,
                   years, frequency = "biennial",
                   discard_rate = NA,
                   ...) {
  ### check validity
  validObject(object)
  ### update object
  object <- rb_calc(object = object,
                     A = A, r = r, b = b, m = m,
                     cap = cap, cap_upper = cap_upper, cap_lower = cap_lower,
                     years = years, frequency = frequency, 
                     discard_rate = discard_rate, ... = ...)
  return(object)
})

### ------------------------------------------------------------------------ ###
### rb calculation ####
### ------------------------------------------------------------------------ ###
rb_calc <- function(object = new("rb"), 
                    A = object@A, r = object@r, 
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
  if (!missing(A)) object@A <- rb_A(A, hcr = "rb")
  if (!missing(r)) object@r <- rb_r(r)
  if (!missing(b)) object@b <- rb_b(b)
  if (!missing(m)) {
    object@m <- m(m)
  } else if (is.na(object@m@value)) {
    ### use default multiplier if missing
    object@m <- rb_m()
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
  factor <- object@r@value * object@b@value * object@m@value

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
      cap_consider <- ifelse(b@value < 1, FALSE, TRUE)
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
### rb convenience methods ####
### ------------------------------------------------------------------------ ###

### value
#' @rdname value
#' @export
setMethod(f = "value", signature = "rb", 
          definition = function(object) {
            return(object@advice)
})

### print
setMethod(f = "print", signature = "rb", 
          definition = function(x) {
            cat(paste0("An object of class \"", class(x), "\".\n",
                       "Value: ", x@advice, "\n"))
})

### show
setMethod(f = "show", signature = "rb", 
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
  f = "advice", signature = "rb",
  definition = function(object) {
    
    ### rb rule components
    txt_A <- paste0(capture.output(advice(object@A)), collapse = "\n")
    txt_r <- paste0(capture.output(advice(object@r)), collapse = "\n")
    txt_b <- paste0(capture.output(advice(object@b)), collapse = "\n")
    txt_m <- paste0(capture.output(advice(object@m)), collapse = "\n")

    object@units <- ifelse(!is.na(object@A@units), 
                           paste0(" ", object@A@units), "")
    ### rb calculation (uncapped advice)
    rb_txt <- "RB calculation (r*b*m)"
    rb_val <- paste0(round(object@advice_uncapped), 
                     object@units)
    txt_rb <- paste0(
      format(rb_txt, width = 48), " | ",
      format(rb_val, width = 29, justify = "right"), "\n")
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
                             "   (Ay * r * b * m)")
    catch_adv_val <- paste0(round(object@advice), object@units)
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
      land_val <- paste0(round(object@advice_landings),
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
      txt_A, "\n", txt_r, "\n", txt_b, "\n", txt_m, "\n",
      txt_rb,
      txt_cap,
      txt_catch_adv,
      txt_disc_land,
      txt_change
    )
    cat(txt)
  }
  
)


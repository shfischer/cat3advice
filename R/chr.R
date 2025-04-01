#' @include f.R
#' @include I.R
#' @include F_hr.R
#' @include b.R
#' @include m.R
#' @include A.R
#' @importFrom icesAdvice icesRound
#' @importFrom utils capture.output tail
NULL

### ------------------------------------------------------------------------ ###
### chr class ####
### ------------------------------------------------------------------------ ###

#' @title An S4 class to represent the chr rule.
#'
#' @description This class contains the components of the chr rule 
#' (\code{\link{I}}, \code{\link{F}}, \code{b}, \code{m}).
#'
#' @slot advice The value of the catch advice.
#' @slot advice_dead The dead catch corresponding to the advice.
#' @slot advice_landings Landings corresponding to the catch advice.
#' @slot advice_discards Discards corresponding to the catch advice.
#' @slot advice_discards_dead Dead discards corresponding to the catch advice.
#' @slot advice_discards_surviving Surviving discards corresponding to the catch advice.
#' @slot advice_uncapped The value of the catch advice without the uncertainty cap.
#' @slot units The unit (e.g. tonnes) of the catch advice.
#' @slot advice_metric The advice metric, 'catch' or 'landings'.
#' @slot frequency The advice frequency (annual/biennial).
#' @slot years The years for which the advice is valid.
#' @slot A The reference catch (previous catch advice).
#' @slot I Component I (the biomass index value).
#' @slot F Component F (the relative harvest rate target).
#' @slot b Component b (the biomass safeguard).
#' @slot m Component m (the multiplier).
#' @slot cap Uncertainty cap (stability clause, restricts changes in advice).
#' @slot cap_lower Maximum allowed reduction in advice in \%, e.g. -30.
#' @slot cap_upper Maximum allowed increase in advice in \%, e.g. 20.
#' @slot change Change in advice compared to previous advice.
#' @slot change_uncapped Change in advice compared to previous advice before application of the uncertainty cap.
#' @slot discard_rate Discard rate (\%).
#' @slot discard_survival Discard survival (\%).
#'
#' @rdname chr-class
#' @export
setClass(
  Class = "chr",
  slots = c(
    advice = "numeric",
    advice_dead = "numeric",
    advice_landings = "numeric",
    advice_discards = "numeric",
    advice_discards_dead = "numeric",
    advice_discards_surviving = "numeric",
    advice_uncapped = "numeric",
    units = "character",
    advice_metric = "character",
    frequency = "character",
    years = "numeric",
    A = "A",
    I = "I",
    F = "F",
    b = "b",
    m = "m",
    cap = "logical",
    cap_lower = "numeric",
    cap_upper = "numeric",
    change = "numeric",
    change_uncapped = "numeric",
    discard_rate = "numeric",
    discard_survival = "numeric"
  ),
  prototype = list(
    advice = NA_real_,
    advice_dead = NA_real_,
    advice_landings = NA_real_,
    advice_discards = NA_real_,
    advice_discards_dead = NA_real_,
    advice_discards_surviving = NA_real_,
    advice_uncapped = NA_real_,
    units = NA_character_,
    advice_metric = NA_character_,
    frequency = "biennial",
    years = NA_real_,
    A = new("chr_A"),
    I = new("chr_I"),
    F = new("F"),
    b = new("chr_b"),
    m = new("chr_m"),
    cap = NA,
    cap_lower = -30,
    cap_upper = 20,
    change = NA_real_,
    change_uncapped = NA_real_,
    discard_rate = NA_real_,
    discard_survival = 0
  )
)


### ------------------------------------------------------------------------ ###
### chr methods ####
### ------------------------------------------------------------------------ ###
#' chr rule
#'
#' This function applies the chr rule.
#'
#' This function applies the chr rule following the ICES technical guidelines (ICES, 2025). The function requires the elements of the chr rule: \out{<i>I</i>} (the biomass index, see \code{\link{chr_I}}), \out{<i>F</i><sub>MSYproxy</sub>} (the target harvest rate, see \code{\link{F}}), \out{<i>b</i>} (the biomass safeguard, see \code{\link{chr_b}}) and \out{<i>m</i>} (the multiplier, see \code{\link{chr_m}}). The catch advice is then calculated as
#' 
#' \out{<i>A</i><sub>y+1</sub> = <i>I</i> * <i>F</i><sub>MSYproxy</sub> * <i>b</i> * <i>m</i>}
#' 
#' restricted by the stability clause relative to \out{<i>A</i><sub>y</sub>}.
#' See the help files of the components for their definition (\code{\link{chr_I}}, \code{\link{F}}, \code{\link{chr_b}}, \code{\link{chr_m}})
#' 
#' The cat3advice package vignette includes an example on how to apply the chr rule with custom control parameters, derived from an MSE, and how to include discard survival.
#'
#' @param object Optional. An object of class \code{chr}.
#' @param A \code{\link{A}}. The reference catch (previous catch advice). Required for calculating change in advice and for the application of the stability clause.
#' @param I \code{\link{I}}. The biomass index value.
#' @param F \code{\link{F}}. The harvest rate target.
#' @param b \code{\link{b}}. The biomass safeguard. 
#' @param m \code{\link{m}}. The multiplier.
#' @param cap The uncertainty cap (stability clause). Defaults to 'conditional', i.e. it is only considered when b=1.
#' @param cap_upper Optional. \code{numeric}. The maximum allowed increase in the catch advice in \%. Default to +20.
#' @param cap_lower Optional. \code{numeric}. The maximum allowed decrease in the catch advice in \%. Default to -20.
#' @param years Optional. \code{numeric}. The years for which the advice should be given.
#' @param frequency Optional. The frequency of the advice ('annual'/'biennial'/'triennial'). Defaults to 'annual'.
#' @param  discard_rate Optional. \code{numeric}. The discard rate (in \%) for the advice. If provided, advice values for catch and landings are given.
#' @param  discard_surival Optional. \code{numeric}. The discard survival rate (in \%) for the advice. If provided, Discards are split into dead and surviving discards.
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
#' Fischer, S. H., De Oliveira, J. A. A., Mumford, J. D., and Kell, L. T. 2022. Exploring a relative harvest rate strategy for moderately data-limited fisheries management. ICES Journal of Marine Science, 79: 1730--1741. \url{https://doi.org/10.1093/icesjms/fsac103}.
#'
#' @return An object of class \code{chr}.
#' 
#' @examples 
#' # calculate elements of chr rule for plaice
#' # reference catch
#' data(ple7e_catch)
#' A <- A(object = ple7e_catch, basis = "advice", units = "tonnes", advice_metric = "catch")
#' # biomass index value
#' data(ple7e_idx)
#' I <- I(ple7e_idx)
#' plot(I)
#' # target harvest rate
#' data(ple7e_length)
#' data(ple7e_catch)
#' lc <- Lc(data = ple7e_length, pool = 2017:2021) # length at first capture
#' plot(lc)
#' lmean <- Lmean(data = ple7e_length, Lc = lc, units = "mm") # mean catch length
#' plot(lmean)
#' lref <- Lref(Lc = 264, Linf = 528) # reference length
#' f <- f(Lmean = lmean, Lref = lref, units = "mm") # f indicator
#' plot(f)
#' df <- merge(ple7e_catch, ple7e_idx, all = TRUE) # combine catch & index data
#' hr <- HR(df, units_catch = "tonnes", units_index = "kg/hr") # harvest rate
#' plot(hr)
#' HR <- F(hr, f) # calculate (relative) target harvest rate
#' plot(F)
#' # biomass safeguard
#' b <- b(ple7e_idx)
#' plot(b)
#' # multiplier
#' m <- m(hcr = "chr")
#' 
#' # apply chr rule
#' advice <- chr(A = A, I = I, F = HR, b = b, m = m, discard_rate = 27)
#' advice
#' advice(advice)
#' 
#' # application in following years without updating reference levels
#' A <- A(object = ple7e_catch, basis = "advice", units = "tonnes", advice_metric = "catch")
#' I <- I(ple7e_idx)
#' hr <- HR(merge(ple7e_catch, ple7e_idx, all = TRUE), units_catch = "tonnes", units_index = "kg/hr")
#' HR <- F(hr, yr_ref = c(2016, 2019)) # use reference years to define target
#' b <- b(ple7e_idx, yr_ref = 2007) # use reference year for Itrigger
#' m <- m(0.5) # keep multiplier
#' advice <- chr(A = A, I = I, F = HR, b = b, m = m, discard_rate = 27)
#' advice
#' 
#' # application of custom control parameters and discard survival
#' # see package vignette for more details
#' data("ple7e_WKBPLAICE")
#' A <- chr_A(ple7e_WKBPLAICE, units = "tonnes", 
#'            basis = "advice", advice_metric = "catch", 
#'            discard_survival = 50)
#' I <- chr_I(ple7e_WKBPLAICE, n_yrs = 2, lag = 1, 
#'            units = "kg/(hr m beam)")
#' hr <- HR(ple7e_WKBPLAICE, units_catch = "tonnes", 
#'          units_index = "kg/(hr m beam)", split_discards = TRUE,
#'          discard_survival = 50)
#' HR <- F(hr, yr_ref = 2003:2023, MSE = TRUE, multiplier = 0.66)
#' b <- chr_b(I, ple7e_WKBPLAICE, units = "kg/(hr m beam)", 
#'            yr_ref = 2007, w = 3.7)
#' m <- chr_m(1, MSE = TRUE)
#' advice <- chr(A = A, I = I, F = HR, b = b, m = m,
#'               frequency = "biennial",
#'               discard_rate = 26.43168,
#'               discard_survival = 50,
#'               units = "tonnes", advice_metric = "catch")
#' advice(advice)
#'
#' @name chr
#' @export
NULL

#' @rdname chr
#' @export
setGeneric(name = "chr",
           def = function(object = new("chr"), 
                          A = object@A,
                          I = object@I, F = object@F, 
                          b = object@b, m = object@m,
                          cap = "conditional",
                          cap_upper = 20,
                          cap_lower = -30,
                          years,
                          frequency = "annual",
                          discard_rate = NA,
                          discard_survival = 0,
                          ...)
             standardGeneric("chr"),
           signature = c("object", "A", "I", "F", "b", "m"))
### object = missing, A/I/F/b/m = A/I/F/b/m
#' @rdname chr
#' @usage NULL
#' @export
setMethod(chr,
          signature = c(object = "missing",
                        A = "A", I = "I", F = "F", b = "b",
                        m = "m"),
          function(A, I, F, b, m,
                   cap = "conditional",
                   cap_upper = 20, 
                   cap_lower = -30,
                   years, frequency = "annual",
                   discard_rate = NA,
                   discard_survival = 0,
                   ...) {#browser()
  object <- chr_calc(A = A, I = I, F = F, b = b, m = m,
                     cap = cap, cap_upper = cap_upper, cap_lower = cap_lower,
                     years = years, frequency = frequency,
                     discard_rate = discard_rate, 
                     discard_survival = discard_survival,
                     ... = ...)
  return(object)
})
### object = missing, A/I/F/b/m = numeric
#' @rdname chr
#' @usage NULL
#' @export
setMethod(chr,
          signature = c(object = "missing",
                        A = "numeric", I = "numeric", F = "numeric", 
                        b = "numeric", m = "numeric"),
          function(A, I, F, b, m,
                   cap = "conditional",
                   cap_upper = 20, 
                   cap_lower = -30,
                   years, frequency = "annual",
                   discard_rate = NA,
                   discard_survival = 0,
                   ...) {#browser()
  object <- chr_calc(A = A, I = I, F = F, b = b, m = m,
                     cap = cap, cap_upper = cap_upper, cap_lower = cap_lower,
                     years = years, frequency = frequency,
                     discard_rate = discard_rate, 
                     discard_survival = discard_survival,
                     ... = ...)
  return(object)
})
### object = chr, A/I/F/b/m = missing -> check validity
#' @rdname chr
#' @usage NULL
#' @export
setMethod(chr,
          signature = c(object = "chr",
                        A = "missing", I = "missing", F = "missing",
                        b = "missing", m = "missing"),
          function(object,
                   A = object@A, I = object@I, F = object@F,
                   b = object@b, m = object@m,
                   cap = "conditional",
                   cap_upper = 20, 
                   cap_lower = -30,
                   years, frequency = "annual",
                   discard_rate = NA,
                   discard_survival = 0,
                   ...) {
  ### check validity
  validObject(object)
  ### update object if arguments provided
  object <- chr_calc(object = object,
                     cap = cap, cap_upper = cap_upper, cap_lower = cap_lower,
                     years = years, frequency = frequency,
                     discard_rate = discard_rate, 
                     discard_survival = discard_survival,
                     ... = ...)
  return(object)

})
### object = chr, A/I/F/b/m = A/I/F/b/m -> check validity & update
#' @rdname chr
#' @usage NULL
#' @export
setMethod(chr,
          signature = c(object = "chr",
                        A = "A", I = "I", F = "F",
                        b = "b", m = "m"),
          function(object,
                   A = object@A, I = object@I, F = object@F,
                   b = object@b, m = object@m,
                   cap = "conditional",
                   cap_upper = 20, 
                   cap_lower = -30,
                   years, frequency = "biennial",
                   discard_rate = NA,
                   discard_survival = 0,
                   ...) {
  ### check validity
  validObject(object)
  ### update object
  object <- chr_calc(object = object,
                     A = A, I = I, F = F, b = b, m = m,
                     cap = cap, cap_upper = cap_upper, cap_lower = cap_lower,
                     years = years, frequency = frequency,
                     discard_rate = discard_rate, 
                     discard_survival = discard_survival,
                     ... = ...)
  return(object)
})

### ------------------------------------------------------------------------ ###
### chr calculation ####
### ------------------------------------------------------------------------ ###
chr_calc <- function(object = new("chr"), 
                     A = object@A,
                     I = object@I, F = object@F, 
                     b = object@b, m = object@m,
                     cap = "conditional",
                     cap_upper = 20,
                     cap_lower = -30,
                     years,
                     frequency = "annual",
                     discard_rate = NA,
                     discard_survival = 0,
                     units, advice_metric,
                     ...) {
  #browser()

  ### convert all components into corresponding classes and check validity
  if (!missing(A)) object@A <- chr_A(A)
  if (!missing(I)) object@I <- I(I)
  if (!missing(F)) object@F <- F(F)
  if (!missing(b)) object@b <- chr_b(b)
  if (!missing(m)) {
    object@m <- chr_m(m)
  } else if (is.na(object@m@value)) {
    ### use default multiplier if missing
    object@m <- chr_m()
  }
  if (!missing(cap_upper)) object@cap_upper <- cap_upper
  if (!missing(cap_lower)) object@cap_lower <- cap_lower
  
  ### check arguments
  if (!missing(frequency)) 
    frequency <- match.arg(arg = frequency, 
                           choices = c("biennial", "annual", "triennial"))
  if (!missing(cap)) 
    cap <- match.arg(arg = cap, choices = c("conditional", TRUE, FALSE))
  
  ### units
  if (!missing(units)) object@units <- units
  if (!missing(advice_metric)) object@advice_metric <- advice_metric

  ### calculate I*F*b*m
  chr_value <- object@I@value * object@F@value * object@b@value * object@m@value

  ### calculate new catch advice
  object@advice_uncapped <- object@advice_dead <- chr_value
  
  ### advice change
  object@change <- (object@advice_dead/object@A@value - 1)*100
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
        object@advice_dead <- object@A@value * (100 + object@cap_upper)/100
        object@cap <- TRUE
      } else if (isTRUE(object@change_uncapped < object@cap_lower)) {
        ### use "+" because cap_lower is negative value
        object@advice_dead <- object@A@value * (100 + object@cap_lower)/100
        object@cap <- TRUE
      }
      ### update advice change
      object@change <- (object@advice_dead/object@A@value - 1)*100
    }
    
  }
  
  ### consider discards
  if (!is.na(discard_rate)) {
    
    object@discard_rate <- discard_rate
    
    ### no discard survival (or not considered)
    if (identical(discard_survival, 0L)) {
      
      ### all advised catch is considered dead
      object@advice <- object@advice_dead
      object@advice_landings <- object@advice * (1 - object@discard_rate/100)
      object@advice_discards <- object@advice * (object@discard_rate/100)
    
    ### discard survival > 0
    } else {
      
      object@discard_survival <- discard_survival
      ### get total advice from dead advice
      object@advice <- object@advice_dead/(1 - (discard_rate/100) *
                                             discard_survival/100)
      ### get landings and discards corresponding to advice
      object@advice_landings <- object@advice * (1 - object@discard_rate/100)
      object@advice_discards <- object@advice * (object@discard_rate/100)
      ### split discards into dead and surviving discards
      object@advice_discards_dead <- object@advice_discards * 
        (1 - object@discard_survival/100)
      object@advice_discards_surviving <- object@advice_discards *
        (object@discard_survival/100)
      
      ### update advice change 
      ### -> refers to total advice relative to previous total advice
      object@change <- (object@advice/object@A@value_catch - 1)*100
      
    }
  
  ### no discards
  } else {
    object@advice_landings <- object@advice <- object@advice_dead
    object@advice_discards <- object@advice_discards_dead <-
      object@advice_discards_surviving <- 0
  }
  
  ### advice years
  object@frequency <- frequency
  ### if advice years missing, try to guess from previous advice
  if (missing(years)) {
    if (!is.na(object@A@avg_years)) {
      object@years <- seq(from = tail(sort(object@A@avg_years), 1) + 1, 
                          length.out = switch(object@frequency, 
                                              "annual" = 1, 
                                              "biennial" = 2, 
                                              "triennial" = 3))
    } else {
      object@years <- switch(object@frequency, 
                             "annual" = 1, 
                             "biennial" = 1:2, 
                             "triennial" = 1:3)
    }
  }

  return(object)
  
}

### ------------------------------------------------------------------------ ###
### chr convenience methods ####
### ------------------------------------------------------------------------ ###

### value
#' @rdname value
#' @export
setMethod(f = "value", signature = "chr", 
          definition = function(object) {
            return(object@advice)
})

### print
setMethod(f = "print", signature = "chr", 
          definition = function(x) {
            cat(paste0("An object of class \"", class(x), "\".\n",
                       "Value: ", x@advice, "\n"))
})

### show
setMethod(f = "show", signature = "chr", 
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
  f = "advice", signature = "chr",
  definition = function(object) {
    # browser()
    ### chr rule components
    txt_I <- paste0(capture.output(advice(object@I)), collapse = "\n")
    txt_F <- paste0(capture.output(advice(object@F)), collapse = "\n")
    txt_b <- paste0(capture.output(advice(object@b)), collapse = "\n")
    txt_m <- paste0(capture.output(advice(object@m)), collapse = "\n")

    ### units
    if (is.na(object@units) & !is.na(object@A@units))
      object@units <- object@A@units
    
    ### advice calculation header
    txt_header <- paste0(
      paste(rep("-", 80), collapse = ""), "\n",
      "Catch advice calculations\n",
      paste(rep("-", 80), collapse = ""), "\n"
    )
    
    ### chr calculation (uncapped advice)
    chr_txt <- "chr calculation (I*HR*b*m)"
    chr_val <- paste(round(object@advice_uncapped), object@units)
    txt_chr <- paste0(
      format(chr_txt, width = 48), " | ",
      format(chr_val, width = 29, justify = "right"), "\n")
    
    ### previous advice (for stability clause)
    txt_A <- paste0(capture.output(advice(object@A)), collapse = "\n")
    
    ### stability clause (uncertainty cap)
    cap_txt1 <- paste0("Stability clause (+", object@cap_upper, "%/",
                       object@cap_lower, "%, chr calculation")
    cap_txt2 <- paste0("   compared to Ay, only applied if b=1)")
    cap_val1 <- ifelse(object@cap, "Applied", "Not applied")
    cap_val2 <- ifelse(object@cap, object@advice/object@A@value, "")
    txt_cap <- paste0(format(cap_txt1, width = 48), " | \n",
                      format(cap_txt2, width = 48), " | ",
                      format(cap_val1, width = 13, justify = "right"), " | ",
                      format(cap_val2, width = 13, justfify = "right"), "\n")
    
    ### check if discards are used 
    if (!is.na(object@discard_rate)) {
      if (isTRUE(object@discard_rate > 0)) {
        use_discards <- TRUE
      }
    } else {
      use_discards <- FALSE
    }
    
    ### discard rate
    if (isTRUE(use_discards)) {
      disc_rate_txt <- "Discard rate"
      disc_rate_val <- paste0(icesAdvice::icesRound(object@discard_rate), 
                              "%")
      txt_disc_rate <- paste0(format(disc_rate_txt, width = 48), " | ",
                              format(disc_rate_val, width = 29, 
                                     justify = "right"), "\n")
    } else {
      txt_disc_rate <- ""
    }
    
    ### discard survival
    if (isTRUE(object@discard_survival > 0)) {
      disc_surv_txt <- "Discard survival"
      disc_surv_val <- paste0(icesAdvice::icesRound(object@discard_survival), 
                              "%")
      txt_surv_rate <- paste0(format(disc_surv_txt, width = 48), " | ",
                              format(disc_surv_val, width = 29, 
                                     justify = "right"), "\n")
    } else {
      txt_surv_rate <- ""
    }
    
    ### catch advice
    catch_adv_txt1 <- paste0("Catch advice for ",
                             paste0(object@years, collapse = " and "))
    catch_adv_val <- paste0(round(object@advice), " ", object@units)
    txt_catch_adv <- paste0(format(catch_adv_txt1, width = 48), " |",
                            format(catch_adv_val, width = 29,
                                   justify = "right"), "\n")
    ### add formula for catch advice
    if (isFALSE(object@discard_survival > 0)) {
      catch_adv_txt2 <- ifelse(isTRUE(object@cap),
                               "   (Ay * stability clause)",
                               "   (I * HR * b * m)")
      txt_catch_adv <- paste0(txt_catch_adv,
                              paste0(format(catch_adv_txt2, width = 48), 
                                     " | ", "\n"))
    } else {
      catch_adv_txt2 <- ifelse(isTRUE(object@cap),
                               "   ([Ay * stability clause]/",
                               "   ([I * HR * b * m]/")
      catch_adv_txt3 <- "   [1 - discard rate * discard survival])"
      txt_catch_adv <- paste0(txt_catch_adv,
                              paste0(format(catch_adv_txt2, width = 48), 
                                     " | ", "\n"),
                              paste0(format(catch_adv_txt3, width = 48), 
                                     " | ", "\n"))
    }
 
    ### landings/discards corresponding to advice
    if (isTRUE(use_discards)) {
      
      ### landings
      land_txt <- "Landings corresponding to advice"
      land_val <- paste(round(object@advice_landings),
                        object@units)
      
      ### discards
      disc_txt <- ifelse(isFALSE(object@discard_survival > 0),
                         "Discards corresponding to advice",
                         "Total discards corresponding to advice")
      disc_val <- paste(round(object@advice_discards),
                        object@units)
      
      ### combine text
      txt_disc_land <- paste0(format(land_txt, width = 48), " | ",
                              format(land_val, width = 29,
                                     justify = "right"), "\n",
                              format(disc_txt, width = 48), " | ",
                              format(disc_val, width = 29,
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
      txt_I, "\n", txt_F, "\n", txt_b, "\n", txt_m, "\n",
      txt_header,
      txt_chr,
      txt_A, "\n",
      txt_cap,
      txt_disc_rate,
      txt_surv_rate,
      txt_catch_adv,
      txt_disc_land,
      txt_change
    )
    cat(txt)
  }

)


#' An S4 class to represent component the rb rule.
#' 
#' This class contains 
#' The classes \code{rfb_Ay}, \code{rb_Ay}, and \code{chr_Ay} inherit from 
#' \code{comp_Ay} and their only difference is that the slot \code{catch_rule}
#' is set to the corresponding catch rule name ('rfb', 'rb', or 'chr').
#' 
#' @slot value The value of component m
#' @slot catch_rule The catch rule for which the multiplier is used. One of 'rfb', 'rb', or 'chr'.
#' @slot catches Time series of historical catches
#' @slot avg_years Number of years for calculating average catch
#' @slot basis Basis of Ay. Either "advice" for using previous advice or "average catch" when based on average of historical catch
#' 
#' @rdname rb-class
#' @export
setClass(
  Class = "rb",
  contains = c("comp_Ay", "comp_r", "comp_b", "comp_m", "comp_cap"),
  slots = c(
    value = "numeric",
    catch_rule = "character",
    catches = "vector",
    avg_years = "numeric",
    basis = "character"
  ),
  prototype = list(
    value = NA_real_,
    catch_rule = NA_character_,
    catches = data.frame(matrix(
      ncol = 2, nrow = 0,
      dimnames = list(NULL, c("year", "catch"))
    )),
    avg_years = NA_real_,
    basis = NA_character_
  )
)



rb_calc <- function(Ay, r, b, m,
                    cap = "conditional", 
                    cap_upper = 20,
                    cap_lower = -30,
                    ...) {
  #browser()
  
  ### convert all components into corresponding classes and check validity
  if (!missing(Ay)) Ay <- comp_Ay(Ay)
  if (!missing(r)) r <- rb_r(r)
  if (!missing(b)) b <- rb_b(b)
  if (!missing(m)) {
    m <- comp_m(m)
  } else {
    ### use default multiplier if missing
    m <- rb_m()
  }
  
  ### calculate r*b*m
  rb_factor <- r@value * b@value * m@value
  
  ### calculate new catch advice
  A_new_raw <- A_new <- Ay@value * rb_factor
  
  ### uncertainty cap / catch constraint
  if (!isFALSE(cap)) {
    
    ### check if cap needs to be applied
    cap_consider <- cap
    if (identical(cap, "conditional")) 
      cap_consider <- ifelse(b@value < 1, FALSE, TRUE)
    if (isFALSE(cap_consider %in% c(TRUE, FALSE, "conditional")))
      stop("unknown uncertainty cap condition")
    
    ### apply cap if required
    cap_applied <- FALSE
    if (isTRUE(cap_consider)) {
      ### cap_upper/cap_lower are values in percent
      A_new_change <- (A_new/Ay@value - 1)*100
      if (isTRUE(A_new_change > cap_upper)) {
        A_new <- Ay@value * (100 + cap_upper)/100
        cap_applied <- TRUE
      } else if (isTRUE(A_new_change < cap_lower)) {
        ### use "+" because cap_lower is negative value
        A_new <- Ay@value * (100 + cap_lower)/100
        cap_applied <- TRUE
      }
    }
    
  }
  
  return(A_new)
}



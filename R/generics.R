### ------------------------------------------------------------------------ ###
### advice ####
### ------------------------------------------------------------------------ ###
### advice generic
#' ICES advice sheet-style output table
#'
#' This function returns an advice sheet-style table for the empirical harvest 
#' control rules. The argument passed to the function can either be a single
#' component of any rules (components r, f, b, ...) or the output from applying
#' any of the rules (rfb, rb, chr).
#' 
#' 
#' @param object A component of any of the empirical harvest control rules or the the output from applying the rule.
#'
#'
#' @return NULL. A table is displayed in the R terminal.
#'
#' @examples
#' # multiplier for the rfb rule with known k
#' # advice(rfb_m(k = 0.1))
#' 
#' @name advice
#' @export
NULL
#' @rdname advice
#' @export
setGeneric(
  name = "advice",
  def = function(object) standardGeneric("advice")
)
#' #' @rdname advice
#' #' @export
#' setMethod(f = "advice", signature = "comp_m", definition = function(object) 1)

### ------------------------------------------------------------------------ ###
### value ####
### ------------------------------------------------------------------------ ###
setGeneric(name = "value", 
           def = function(object)  standardGeneric("value"))

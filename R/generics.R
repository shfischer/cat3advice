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
#' @param object A component of any of the empirical harvest control rules or the the output from applying the rule.
#'
#' @return NULL. A table is displayed in the R terminal.
#' 
#' @name advice
#' @export
setGeneric(
  name = "advice",
  def = function(object) standardGeneric("advice")
)


### ------------------------------------------------------------------------ ###
### value ####
### ------------------------------------------------------------------------ ###
#' @title value
#' @description This function returns the value of any component or the advice
#' for the empirical harvest control rules
#' @param object The object from which the value is extracted.
#' @export
setGeneric(name = "value", 
           def = function(object)  standardGeneric("value"))

### ------------------------------------------------------------------------ ###
### show ####
### ------------------------------------------------------------------------ ###
#' @title show 
#' @description Show a summary of the elements of the empirical harvest control rule
#'
#' @name show
NULL
# setGeneric(name = "show",
#            def = function(object)  standardGeneric("show"))

### ------------------------------------------------------------------------ ###
### print ####
### ------------------------------------------------------------------------ ###
# setGeneric(name = "print",
#            def = function(x)  standardGeneric("print"))

### ------------------------------------------------------------------------ ###
### summary ####
### ------------------------------------------------------------------------ ###
#' @title summary
#' @description This function returns a summary of any component or the advice
#' for the empirical harvest control rules
#' @param object The object for which a summary is requested.
#' @param ... Additional arguments. Not used.
#' @export
#' @name summary
NULL
# setGeneric(name = "summary",
#            def = function(object, ...)  standardGeneric("summary"))

### ------------------------------------------------------------------------ ###
### plot ####
### ------------------------------------------------------------------------ ###
# #' @rdname rfb_plot
# #' @usage NULL
# #' @export
# setGeneric(name = "plot",
#            def = function(x, y, y_label, ...)  standardGeneric("plot"))

### ------------------------------------------------------------------------ ###
### indicator ####
### ------------------------------------------------------------------------ ###
#' @title indicator
#' @description This function returns the indicator value(s) for the empirical
#' harvest control rules, e.g. the length indicator.
#' @param object The object from which the indicator is requested.
#' @export
#' @name indicator
#' @rdname chr_indicator
#' @export
setGeneric(name = "indicator",
           def = function(object)  standardGeneric("indicator"))

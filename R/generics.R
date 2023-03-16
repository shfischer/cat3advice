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
#' @export
setGeneric(name = "value", 
           def = function(object)  standardGeneric("value"))

### ------------------------------------------------------------------------ ###
### show ####
### ------------------------------------------------------------------------ ###
setGeneric(name = "show",
           def = function(object)  standardGeneric("show"))

### ------------------------------------------------------------------------ ###
### print ####
### ------------------------------------------------------------------------ ###
setGeneric(name = "print",
           def = function(x)  standardGeneric("print"))

### ------------------------------------------------------------------------ ###
### summary ####
### ------------------------------------------------------------------------ ###
#' @title summary
#' @description This function returns a summary of any component or the advice
#' for the empirical harvest control rules
#' @export
setGeneric(name = "summary",
           def = function(object)  standardGeneric("summary"))

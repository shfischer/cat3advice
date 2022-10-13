#' Plot elements of the rfb/rb/chr rule
#'
#' A convenience function for plotting elements of the rfb, rb, and chr rules
#' using ggplot2 and loosely following ICES style figures.
#' 
#' Combinations of object are possible, e.g. for the rfb rule, it is possible to
#' plot components r (index ratio) and b (biomass safeguard) on the same plot.
#' 
#'
#' @param x An object of class \code{rfb_r}, \code{rfb_b}, ...
#' @param y Optional. An additional object of \code{rfb_b}, ...
#' @param y_label Optional. The y-axis label.
#' @param ... Additional arguments. Not used.
#'  
#' @return An object of class \code{gg}/\code{ggplot} with the plot.
#' Can be manipulated with the usual ggplot2 commands, e.g. \code{ylim()}.
#'
#' @examples
#' # Component b 
#' df_idx <- data.frame(year = 2017:2021,
#'                      index = c(1.33, 1.13, 0.84, 0.60, 1.03))
#' b <- rfb_b(df_idx)
#' plot(b, y = "Biomass index in kg/hr")
#'
#' # Component r
#' r <- rfb_r(df_idx)
#' plot(r, y = "Biomass index in kg/hr")
#'
#' # Components r and b combined
#' plot(r, b)
#'
#' 
#' @export

### rfb_r
#' @rdname rfb_plot
setMethod(f = "plot", signature = c(x = "comp_r"), 
          definition = function(x, y_label, ...) {
            
  object <- x
  
  ### check validity
  . <- validObject(object)
  
  ### get range of years and index values
  yr_min <- min(object@idx$year, na.rm = TRUE)
  yr_max <- max(object@idx$year, na.rm = TRUE)
  idx_min <- min(object@idx$index, na.rm = TRUE)
  idx_max <- max(object@idx$index, na.rm = TRUE)
  
  ### data.frame for horizontal lines
  ### extend 0.5 years before and after
  df_mean <- data.frame(year = c(min(object@n2_yrs) - 0.499,
                                 max(object@n2_yrs) + 0.499,
                                 max(object@n2_yrs) + 0.5,
                                 min(object@n1_yrs) - 0.499,
                                 max(object@n1_yrs) + 0.499),
                        value = c(object@n2_mean,
                                  object@n2_mean,
                                  NA, # separate the two lines
                                  object@n1_mean,
                                  object@n1_mean))
  
  ### index units
  if (missing(y_label)) {
    y_label <- "Biomass index"
    if (!is.na(object@units))
      y_label <- paste0(y_label, " in ", object@units)
  }
  
  
  ### create plot
  p <- ggplot()
  
  ### add shaded area if high/low exist
  if (all(c("low", "high") %in% names(object@idx))) {
    idx_max_high <- max(object@idx$high, na.rm = TRUE)
    if (isTRUE(idx_max_high > idx_max)) idx_max <- idx_max_high
    p <- p +
      geom_ribbon(data = object@idx,
                  aes(x = year, ymin = low, ymax = high),
                  fill = "#077c6c", alpha = 0.7, show.legend = FALSE)
  }
  p <- p +
    geom_line(data = object@idx,
              aes(x = year, y = index),
              color = "#077c6c") +
    geom_line(data = df_mean,
              aes(x = year, y = value),
              colour = "#ed6028") +
    coord_cartesian(ylim = c(0, idx_max * 1.1), 
                    xlim = c(yr_min - 1, yr_max + 1), 
                    expand = FALSE) +
    labs(x = "", y = y_label, 
         title = "Biomass Index") +
    theme_bw(base_size = 8) +
    theme(axis.title.y = element_text(face = "bold"),
          axis.title.x = element_blank(),
          legend.position = "bottom",
          legend.key.height = unit(0.5, "lines"),
          plot.title = element_text(face = "bold", colour = "#097e6e"))
  return(p)
})

### rfb_b
#' @rdname rfb_plot
setMethod(f = "plot", signature = c(x = "comp_b"), 
          definition = function(x, y_label, ...) {
            
  object <- x
  
  ### check validity
  . <- validObject(object)
  
  ### get range of years and index values
  yr_min <- min(object@idx$year, na.rm = TRUE)
  yr_max <- max(object@idx$year, na.rm = TRUE)
  idx_min <- min(object@idx$index, na.rm = TRUE)
  idx_max <- max(object@idx$index, na.rm = TRUE)
  
  ### index units
  if (missing(y_label)) {
    y_label <- "Biomass index"
    if (!is.na(object@units))
      y_label <- paste0(y_label, " in ", object@units)
  }
  
  ### reference lines
  b_refs <- data.frame(name = c("I[loss]", "I[trigger]"),
                       value = c(object@Iloss, object@Itrigger))
  b_refs$name <- factor(b_refs$name)
  
  ### create plot
  p <- ggplot()
  
  ### add shaded area if high/low exist
  if (all(c("low", "high") %in% names(object@idx))) {
    idx_max_high <- max(object@idx$high, na.rm = TRUE)
    if (isTRUE(idx_max_high > idx_max)) idx_max <- idx_max_high
    p <- p +
      geom_ribbon(data = object@idx,
                  aes(x = year, ymin = low, ymax = high),
                  fill = "#077c6c", alpha = 0.7, show.legend = FALSE)
  }
  p <- p +
    geom_line(data = object@idx,
              aes(x = year, y = index),
              color = "#077c6c") +
    geom_hline(data = b_refs, 
               aes(yintercept = value, linetype = name, colour = name)) +
    scale_linetype_manual("", 
                          values = c("I[loss]" = "dotted", 
                                     "I[trigger]" = "solid"),
                          labels = scales::parse_format()) + 
    scale_colour_manual("",
                        values = c("I[loss]" = "#679dfe",
                                   "I[trigger]" = "#679dfe"),
                        labels = scales::parse_format()) +
    coord_cartesian(ylim = c(0, idx_max * 1.1), 
                    xlim = c(yr_min - 1, yr_max + 1), 
                    expand = FALSE) +
    labs(x = "", y = y_label, 
         title = "Biomass Index") +
    theme_bw(base_size = 8) +
    theme(axis.title.y = element_text(face = "bold"),
          axis.title.x = element_blank(),
          legend.position = "bottom",
          legend.key.height = unit(0.5, "lines"),
          plot.title = element_text(face = "bold", colour = "#097e6e"))
  return(p)
})

### comp_r and comp_b
### set S3 plot() as generic so that it can be used with S4 methods
# setGeneric("plot")
#' @rdname rfb_plot
setMethod(f = "plot", signature = c(x = "comp_r", y = "comp_b"), 
          definition = function(x, y, y_label, ...) {
  
  ### check validity
  . <- validObject(x)
  . <- validObject(y)
  if (is(x, "comp_r")) {
    object <- x
    object2 <- y
  } else {
    object <- y
    object2 <- x
  }
  
  ### get range of years and index values
  yr_min <- min(object@idx$year, na.rm = TRUE)
  yr_max <- max(object@idx$year, na.rm = TRUE)
  idx_min <- min(object@idx$index, na.rm = TRUE)
  idx_max <- max(object@idx$index, na.rm = TRUE)
  
  ### data.frame for horizontal lines
  ### extend 0.5 years before and after
  df_mean <- data.frame(year = c(min(object@n2_yrs) - 0.499,
                                 max(object@n2_yrs) + 0.499,
                                 max(object@n2_yrs) + 0.5,
                                 min(object@n1_yrs) - 0.499,
                                 max(object@n1_yrs) + 0.499),
                        value = c(object@n2_mean,
                                  object@n2_mean,
                                  NA, # separate the two lines
                                  object@n1_mean,
                                  object@n1_mean))
  
  ### index units
  if (missing(y_label)) {
    y_label <- "Biomass index"
    if (!is.na(object@units))
      y_label <- paste0(y_label, " in ", object@units)
  }
  
  ### reference lines
  b_refs <- data.frame(name = c("I[loss]", "I[trigger]"),
                       value = c(object2@Iloss, object2@Itrigger))
  b_refs$name <- factor(b_refs$name)
  
  
  ### create plot
  p <- ggplot()
  
  ### add shaded area if high/low exist
  if (all(c("low", "high") %in% names(object@idx))) {
    idx_max_high <- max(object@idx$high, na.rm = TRUE)
    if (isTRUE(idx_max_high > idx_max)) idx_max <- idx_max_high
    p <- p +
      geom_ribbon(data = object@idx,
                  aes(x = year, ymin = low, ymax = high),
                  fill = "#077c6c", alpha = 0.7, show.legend = FALSE)
  }
  p <- p +
    geom_line(data = object@idx,
              aes(x = year, y = index),
              color = "#077c6c") +
    geom_line(data = df_mean,
              aes(x = year, y = value),
              colour = "#ed6028") +
    geom_hline(data = b_refs, 
               aes(yintercept = value, linetype = name, colour = name)) +
    scale_linetype_manual("", 
                          values = c("I[loss]" = "dotted", 
                                     "I[trigger]" = "solid"),
                          labels = scales::parse_format()) + 
    scale_colour_manual("",
                        values = c("I[loss]" = "#679dfe",
                                   "I[trigger]" = "#679dfe"),
                        labels = scales::parse_format()) +
    coord_cartesian(ylim = c(0, idx_max * 1.1), 
                    xlim = c(yr_min - 1, yr_max + 1), 
                    expand = FALSE) +
    labs(x = "", y = y_label, 
         title = "Biomass Index") +
    theme_bw(base_size = 8) +
    theme(axis.title.y = element_text(face = "bold"),
          axis.title.x = element_blank(),
          legend.position = "bottom",
          legend.key.height = unit(0.5, "lines"),
          plot.title = element_text(face = "bold", colour = "#097e6e"))
  return(p)
})

### comp_r and comp_b - but order reversed
#' @rdname rfb_plot
setMethod(f = "plot", signature = c(x = "comp_b", y = "comp_r"), 
          definition = function(x, y, y_label, ...) {
  plot(x = y, y = x, y_label = y_label, ...)
})

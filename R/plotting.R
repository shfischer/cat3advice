#' @include generics.R
#' @importFrom ggplot2 ggplot geom_ribbon geom_line geom_vline aes coord_cartesian labs theme theme_bw element_text element_blank unit scale_linetype_manual scale_colour_manual scale_fill_manual facet_wrap
#' @importFrom scales parse_format
#' @importFrom dplyr select mutate bind_rows filter group_by summarise
#' @importFrom tidyr pivot_longer
#' @importFrom patchwork plot_layout
NULL

### ------------------------------------------------------------------------ ###
### Plot elements of the rfb/rb/chr rule ####
### ------------------------------------------------------------------------ ###

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
#' plot(b, y_label = "Biomass index in kg/hr")
#'
#' # Component r
#' r <- rfb_r(df_idx)
#' plot(r, y_label = "Biomass index in kg/hr")
#'
#' # Components r and b combined
#' plot(r, b)
#' 
#' @export
#' @name rfb_plot
setGeneric(name = "plot",
           def = function(x, y, y_label, ...)  standardGeneric("plot"))

### rfb_r ####
#' @rdname rfb_plot
#' @export
setMethod(f = "plot", signature = c(x = "comp_r", y = "missing"), 
          definition = function(x, y, y_label, ...) {
            
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
  p <- ggplot2::ggplot()
  
  ### add shaded area if high/low exist
  if (all(c("low", "high") %in% names(object@idx))) {
    idx_max_high <- max(object@idx$high, na.rm = TRUE)
    if (isTRUE(idx_max_high > idx_max)) idx_max <- idx_max_high
    p <- p +
      ggplot2::geom_ribbon(data = object@idx,
                  ggplot2::aes(x = year, ymin = low, ymax = high),
                  fill = "#077c6c", alpha = 0.7, show.legend = FALSE)
  }
  p <- p +
    ggplot2::geom_line(data = object@idx,
                       ggplot2::aes(x = year, y = index),
              color = "#077c6c") +
    ggplot2::geom_line(data = df_mean,
                       ggplot2::aes(x = year, y = value),
              colour = "#ed6028") +
    ggplot2::coord_cartesian(ylim = c(0, idx_max * 1.1), 
                    xlim = c(yr_min - 1, yr_max + 1), 
                    expand = FALSE) +
    ggplot2::labs(x = "", y = y_label, 
         title = "Biomass Index") +
    ggplot2::theme_bw(base_size = 8) +
    ggplot2::theme(axis.title.y = ggplot2::element_text(face = "bold"),
          axis.title.x = ggplot2::element_blank(),
          legend.position = "bottom",
          legend.key.height = ggplot2::unit(0.5, "lines"),
          plot.title = ggplot2::element_text(face = "bold", colour = "#097e6e"))
  return(p)
})

### rfb_b ####
#' @rdname rfb_plot
setMethod(f = "plot", signature = c(x = "comp_b", y = "missing"), 
          definition = function(x, y, y_label, ...) {
            
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
  p <- ggplot2::ggplot()
  
  ### add shaded area if high/low exist
  if (all(c("low", "high") %in% names(object@idx))) {
    idx_max_high <- max(object@idx$high, na.rm = TRUE)
    if (isTRUE(idx_max_high > idx_max)) idx_max <- idx_max_high
    p <- p +
      ggplot2::geom_ribbon(data = object@idx,
                           ggplot2::aes(x = year, ymin = low, ymax = high),
                  fill = "#077c6c", alpha = 0.7, show.legend = FALSE)
  }
  p <- p +
    ggplot2::geom_line(data = object@idx,
                       ggplot2::aes(x = year, y = index),
              color = "#077c6c") +
    ggplot2::geom_hline(data = b_refs, 
               ggplot2::aes(yintercept = value, linetype = name, colour = name)) +
    ggplot2::scale_linetype_manual("", 
                          values = c("I[loss]" = "dotted", 
                                     "I[trigger]" = "solid"),
                          labels = scales::parse_format()) + 
    ggplot2::scale_colour_manual("",
                        values = c("I[loss]" = "#679dfe",
                                   "I[trigger]" = "#679dfe"),
                        labels = scales::parse_format()) +
    ggplot2::coord_cartesian(ylim = c(0, idx_max * 1.1), 
                    xlim = c(yr_min - 1, yr_max + 1), 
                    expand = FALSE) +
    ggplot2::labs(x = "", y = y_label, 
         title = "Biomass Index") +
    ggplot2::theme_bw(base_size = 8) +
    ggplot2::theme(axis.title.y = ggplot2::element_text(face = "bold"),
          axis.title.x = ggplot2::element_blank(),
          legend.position = "bottom",
          legend.key.height = ggplot2::unit(0.5, "lines"),
          plot.title = ggplot2::element_text(face = "bold", colour = "#097e6e"))
  return(p)
})

### comp_r and comp_b ####
### set S3 plot() as generic so that it can be used with S4 methods
# setGeneric("plot")
#' @rdname rfb_plot
#' @export
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
  p <- ggplot2::ggplot()
  
  ### add shaded area if high/low exist
  if (all(c("low", "high") %in% names(object@idx))) {
    idx_max_high <- max(object@idx$high, na.rm = TRUE)
    if (isTRUE(idx_max_high > idx_max)) idx_max <- idx_max_high
    p <- p +
      ggplot2::geom_ribbon(data = object@idx,
                           ggplot2::aes(x = year, ymin = low, ymax = high),
                  fill = "#077c6c", alpha = 0.7, show.legend = FALSE)
  }
  p <- p +
    ggplot2::geom_line(data = object@idx,
                       ggplot2::aes(x = year, y = index),
              color = "#077c6c") +
    ggplot2::geom_line(data = df_mean,
              ggplot2::aes(x = year, y = value),
              colour = "#ed6028") +
    ggplot2::geom_hline(data = b_refs, 
               ggplot2::aes(yintercept = value, linetype = name, colour = name)) +
    ggplot2::scale_linetype_manual("", 
                          values = c("I[loss]" = "dotted", 
                                     "I[trigger]" = "solid"),
                          labels = scales::parse_format()) + 
    ggplot2::scale_colour_manual("",
                        values = c("I[loss]" = "#679dfe",
                                   "I[trigger]" = "#679dfe"),
                        labels = scales::parse_format()) +
    ggplot2::coord_cartesian(ylim = c(0, idx_max * 1.1), 
                    xlim = c(yr_min - 1, yr_max + 1), 
                    expand = FALSE) +
    ggplot2::labs(x = "", y = y_label, 
         title = "Biomass Index") +
    ggplot2::theme_bw(base_size = 8) +
    ggplot2::theme(axis.title.y = ggplot2::element_text(face = "bold"),
          axis.title.x = ggplot2::element_blank(),
          legend.position = "bottom",
          legend.key.height = ggplot2::unit(0.5, "lines"),
          plot.title = ggplot2::element_text(face = "bold", colour = "#097e6e"))
  return(p)
})

### comp_r and comp_b - but order reversed ####
#' @rdname rfb_plot
#' @export
setMethod(f = "plot", signature = c(x = "comp_b", y = "comp_r"), 
          definition = function(x, y, y_label, ...) {
  plot(x = y, y = x, y_label = y_label, ...)
})

### rfb_f ####
#' @rdname rfb_plot
setMethod(f = "plot", signature = c(x = "comp_f", y = "missing"), 
          definition = function(x, y, y_label, ...) {
            
  object <- x
  
  ### check validity
  . <- validObject(object)
  
  ### get range of years and index values
  yr_min <- min(object@indicator$year, na.rm = TRUE)
  yr_max <- max(object@indicator$year, na.rm = TRUE)
  idx_min <- min(object@indicator$Lmean, na.rm = TRUE)
  idx_max <- max(object@indicator$Lmean, na.rm = TRUE)
  
  ### index units
  if (missing(y_label)) {
    y_label <- "Mean catch length"
    if (!is.na(object@units))
      y_label <- paste0(y_label, " in ", object@units)
  }
  
  ### data.frame for reference length
  Lref_df <- data.frame(name = "L[F==M]", value = x@Lref@value)
  
  ### create plot
  p <- ggplot2::ggplot()
  
  p <- p +
    ggplot2::geom_line(data = object@indicator,
                       ggplot2::aes(x = year, y = Lmean),
              color = "#ed6028") +
    ggplot2::geom_hline(data = Lref_df, 
                        ggplot2::aes(yintercept = value, colour = name)) +
    ggplot2::scale_colour_manual("",
                        values = c("L[F==M]" = "#679dfe"),
                        labels = scales::parse_format()) +
    ggplot2::scale_x_continuous(breaks = scales::pretty_breaks()) +
    ggplot2::coord_cartesian(ylim = c(0, idx_max * 1.1),
                    xlim = c(yr_min - 1, yr_max + 1),
                    expand = FALSE) +
    ggplot2::labs(x = "", y = y_label, 
         title = "Length indicator") +
    ggplot2::theme_bw(base_size = 8) +
    ggplot2::theme(axis.title.y = ggplot2::element_text(face = "bold"),
          axis.title.x = ggplot2::element_blank(),
          legend.position = "bottom",
          legend.key.height = ggplot2::unit(0.5, "lines"),
          plot.title = ggplot2::element_text(face = "bold", colour = "#ed6028"))
  return(p)
})


### ------------------------------------------------------------------------ ###
### Plot length frequencies ####
### ------------------------------------------------------------------------ ###
#' Plot length frequencies
#'
#' A convenience function for plotting length frequencies and length reference
#' points.
#' 
#' @param x An object of class \code{Lc}, \code{Lmean}, ...
#' @param ... Additional arguments. Not currently used.
#'  
#' @return An object of class \code{gg}/\code{ggplot} with the plot.
#' Can be manipulated with the usual ggplot2 commands, e.g. \code{ylim()}.
#'
#' @examples
#' 
#' @export
#' @rdname length_freq_plot
### Lc
setMethod(f = "plot", signature = c(x = "Lc"), 
          definition = function(x, ...) {
  
  if (all(is.na(x@data$year))) 
    x@data$year <- "pooled data"
  if (all(is.na(x@summary$year))) 
    x@summary$year <- "pooled data"
            
  p <- x@data %>%
    ggplot2::ggplot(aes(x = length, y = numbers)) +
    ggplot2::geom_col() +
    ggplot2::geom_col(
      data = dplyr::bind_rows(
        x@summary |> ### modal length
          dplyr::select(year, L = Lmode, N = Nmode) |>
          dplyr::mutate(source = "mode"),
        x@summary |> ### length at first capture
          dplyr::select(year, L = Lc, N = Nc) |>
          dplyr::mutate(source = "c"),
        ### empty data to ensure bin width is kept
        x@data |>
          dplyr::select(year, L = length) |>
          unique() |>
          dplyr::mutate(N = NA, source = NA)
      ) |>
        dplyr::mutate(source = factor(source,
          levels = c("c", "mode"),
          labels = c("Lc", "mode")
        )),
      ggplot2::aes(x = L, y = N, fill = source)
    ) +
    ggplot2::scale_fill_manual("Length", values = c("Lc" = "red", "mode" = "black")) +
    ggplot2::geom_hline(
      data = x@summary |>
        dplyr::select(year, mode = Nmode) |>
        dplyr::mutate("mode/2" = mode / 2) |>
        tidyr::pivot_longer(-year) |>
        dplyr::mutate(name = factor(name,
          levels = c("mode", "mode/2")
        )),
      ggplot2::aes(yintercept = value, linetype = name),
      size = 0.4
    ) +
    ggplot2::scale_linetype("Numbers") +
    ggplot2::coord_cartesian(xlim = c(0, NA), ylim = c(0, NA)) +
    ggplot2::facet_wrap(~year) +
    ggplot2::labs(x = paste0("Length", 
                    ifelse(length(x@units) > 0, 
                           paste0(" (", x@units, ")"),
                           "")), 
         y = "Numbers") +
    ggplot2::theme_bw()
  
  ### add average Lc line if provided
  if (isTRUE(x@averaged)) {
    p <- p + 
      ggplot2::geom_vline(data = data.frame(length = x@value, colour = "Lc (average)"),
                 aes(xintercept = length, colour = colour),
                 linetype = "dashed") +
      ggplot2::scale_colour_manual("", values = c("Lc (average)" = "red"))
  }
  
  return(p)
  
})

### Lc ####
#' @rdname length_freq_plot
setMethod(
  f = "plot", signature = c(x = "Lmean"),
  definition = function(x, ...) {
    if (all(is.na(x@data$year))) {
      x@data$year <- "pooled data"
    }
    if (all(is.na(x@summary$year))) {
      x@summary$year <- "pooled data"
    }

    p <- x@data %>%
      ggplot2::ggplot(aes(x = length, y = numbers)) +
      ggplot2::geom_col() +
      ggplot2::geom_vline(data = x@summary |>
                   tidyr::pivot_longer(c(Lc, Lmean)) |>
                   dplyr::mutate(name = factor(name, 
                                        levels = c("Lc", "Lmean"),
                                        labels = c("L[c]", "L[mean]"))),
                   ggplot2::aes(xintercept = value, colour = name, linetype = name)) +
      ggplot2::scale_linetype_manual("", 
                            values = c("L[c]" = "dashed", 
                                       "L[mean]" = "solid"), 
                            labels = scales::parse_format()) +
      ggplot2::scale_colour_manual("", 
                          values = c("L[c]" = "grey", 
                                     "L[mean]" = "red"), 
                          labels = scales::parse_format()) +
      ggplot2::coord_cartesian(xlim = c(0, NA), ylim = c(0, NA)) +
      ggplot2::facet_wrap(~year) +
      ggplot2::labs(
        x = paste0(
          "Length",
          ifelse(length(x@units) > 0,
            paste0(" (", x@units, ")"),
            ""
          )
        ),
        y = "Numbers"
      ) +
      ggplot2::theme_bw() +
      ggplot2::theme(legend.text.align = 0)
    
    return(p)
  }
)

### ------------------------------------------------------------------------ ###
### harvest rate ####
### ------------------------------------------------------------------------ ###
### HR ####
#' @rdname HR_plot
setMethod(f = "plot", signature = c(x = "HR"), 
          definition = function(x, y_label, 
                                show.data = TRUE,
                                ...) {
  #browser()
  object <- x
  
  ### check validity
  . <- validObject(object)
  
  ### get range of years and index values
  yr_min <- min(object@data$year[!is.na(object@data$harvest_rate)], 
                na.rm = TRUE)
  yr_max <- max(object@data$year[!is.na(object@data$harvest_rate)], 
                na.rm = TRUE)
  hr_min <- min(object@data$harvest_rate, na.rm = TRUE)
  hr_max <- max(object@data$harvest_rate, na.rm = TRUE)
  
  
  
  
  ### index units
  if (missing(y_label)) {
    y_label <- "Harvest rate"
    if (!is.na(object@units))
      y_label <- paste0(y_label, " in ", object@units)
  }
  # 
  # ### data.frame for reference length
  # Lref_df <- data.frame(name = "L[F==M]", value = x@Lref@value)
  
  ### create plot
  p <- ggplot2::ggplot() +
    ggplot2::geom_line(data = object@data,
                       ggplot2::aes(x = year, y = harvest_rate),
                       color = "#ed6028", 
                       na.rm = TRUE) +
    ggplot2::scale_x_continuous(breaks = scales::pretty_breaks()) +
    ggplot2::coord_cartesian(ylim = c(0, hr_max * 1.1),
                             xlim = c(yr_min - 1, yr_max + 1),
                             expand = FALSE) +
    ggplot2::labs(x = "", y = y_label, 
                  title = "Harvest rate (catches / biomass index)") +
    ggplot2::theme_bw(base_size = 8) +
    ggplot2::theme(axis.title.y = ggplot2::element_text(face = "bold"),
                   axis.title.x = ggplot2::element_blank(),
                   legend.position = "bottom",
                   legend.key.height = ggplot2::unit(0.5, "lines"),
                   plot.title = ggplot2::element_text(face = "bold", 
                                                      colour = "#ed6028"))
  ### add additional plots, if requested
  if (isTRUE(show.data)) {
    yr_min_c <- min(object@data$year[!is.na(object@data$catch) |
                                       !is.na(object@data$index)], 
                    na.rm = TRUE)
    yr_max_c <- max(object@data$year[!is.na(object@data$catch) |
                                       !is.na(object@data$index)], 
                    na.rm = TRUE)
      ### select data columns
      if (all(c("landings", "discards") %in% names(object@data))) {
        cols_c <- c("year", "landings", "discards")
        cols_c_colours <- c(landings = "#002b5f", discards = "#28b3e8")
      } else {
        cols_c <- c("year", "catch")
        cols_c_colours <- c(catch = "#002b5f")
      }
      
      ### format for plotting
      df_catch <- object@data[, cols_c] %>% 
        tidyr::pivot_longer(cols = -year) %>%
        dplyr::filter(!is.na(value))
      ### max catch value
      c_max <- df_catch %>%
        dplyr::group_by(year) %>%
        dplyr::summarise(catch = sum(value)) %>%
        dplyr::select(catch) %>%
        max(na.rm = TRUE)
      ### axis label
      y_label_catch <- paste0("Catches", 
                            ifelse(!is.na(object@units_catch),
                                   paste0(" in ", object@units_catch),
                                   ""))
      p_catch <- ggplot2::ggplot() +
        ggplot2::geom_col(data = df_catch,
                          ggplot2::aes(x = year, y = value, fill = name),
                          na.rm = TRUE) +
        ggplot2::scale_fill_manual("",
                                     values = cols_c_colours) + 
        ggplot2::coord_cartesian(ylim = c(0, c_max * 1.1), 
                                 xlim = c(yr_min_c - 1, yr_max_c + 1), 
                                 expand = FALSE) +
        ggplot2::labs(x = "", y = y_label_catch, 
                      title = "Catches") +
        ggplot2::theme_bw(base_size = 8) +
        ggplot2::theme(axis.title.y = ggplot2::element_text(face = "bold"),
                       axis.title.x = ggplot2::element_blank(),
                       legend.position = "bottom",
                       legend.key.size = ggplot2::unit(0.5, "lines"),
                       plot.title = ggplot2::element_text(face = "bold", 
                                                          colour = "#002b5f"))

      idx_max <- max(object@data$index, na.rm = TRUE)
      y_label_idx <- paste0("Biomass index", 
                            ifelse(!is.na(object@units_index),
                                   paste0(" in ", object@units_index),
                                   ""))
      p_idx <- ggplot2::ggplot() +
        ggplot2::geom_line(data = object@data,
                           ggplot2::aes(x = year, y = index),
                           color = "#077c6c",
                           na.rm = TRUE) +
        ggplot2::coord_cartesian(ylim = c(0, idx_max * 1.1), 
                                 xlim = c(yr_min_c - 1, yr_max_c + 1), 
                                 expand = FALSE) +
        ggplot2::labs(x = "", y = y_label_idx, 
                      title = "Biomass Index") +
        ggplot2::theme_bw(base_size = 8) +
        ggplot2::theme(axis.title.y = ggplot2::element_text(face = "bold"),
                       axis.title.x = ggplot2::element_blank(),
                       legend.position = "bottom",
                       legend.key.height = ggplot2::unit(0.5, "lines"),
                       plot.title = ggplot2::element_text(face = "bold", 
                                                          colour = "#097e6e"))

    ### combine figures
    p <- (p_catch + p_idx)/p + patchwork::plot_layout(heights = c(0.6, 1))
  }
  
  return(p)
})

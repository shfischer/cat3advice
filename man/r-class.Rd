% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/r.R
\docType{class}
\name{r-class}
\alias{r-class}
\alias{rfb_r-class}
\alias{rb_r-class}
\title{r-class}
\description{
An S4 class to represent component r of the rfb and rb rules.

This class (\code{r}) stores the input for component r (the index ratio
) as well as the resulting r value.
}
\section{Slots}{

\describe{
\item{\code{value}}{The value of component r}

\item{\code{n0,n1,n2}}{Parameters for the calculation of the r component. See \code{?rfb_r} for details.}

\item{\code{yr_last}}{\code{numeric}. The last year with index data.}

\item{\code{n1_yrs,n2_yrs}}{\code{numeric}. The years used for \code{n1} and \code{n2}.}

\item{\code{n1_mean,n2_mean}}{\code{numeric}. The mean index values over \code{n1_yrs} and \code{n2_yrs}.}

\item{\code{idx}}{\code{data.frame}. A \code{data.frame} with the index values.}

\item{\code{units}}{\code{character}. The units of the biomass index, e.g. 'kg/hr'.}

\item{\code{hcr}}{\code{character}. The harvest control rule (hcr) for which the biomass safeguard is used. One of 'rfb' or 'rb'.}
}}


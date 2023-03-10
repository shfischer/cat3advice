% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/comp_f.R
\name{comp_f}
\alias{comp_f}
\alias{comp_f,missing,Lmean,Lref-method}
\alias{comp_f,comp_f,missing,missing-method}
\alias{rfb_f}
\alias{rfb_f,missing,ANY,ANY-method}
\alias{rfb_f,ANY,missing,missing-method}
\title{rfb rule - component f (fishing pressure proxy, length indicator)}
\usage{
comp_f(object, Lmean, Lref, units, catch_rule, ...)
}
\arguments{
\item{object}{Optional. An object of class \code{comp_f}.}

\item{Lmean}{The mean catch length. Either a \code{data.frame} with columns
'year' and 'Lmean' or an object of class \code{Lmean}.}

\item{Lref}{The reference length. Either a \code{numeric} with the value or 
            an object of class \code{Lref}.
 
...}

\item{units}{Optional. The units of the length dat, e.g. 'cm'. Only used for plotting.}

\item{catch_rule}{Optional. Defaults to 'rfb'.}

\item{...}{Additional arguments. Not currently used.}
}
\value{
An object of class \code{comp_f} with the length indicator value(s).
}
\description{
This function calculates component f (the fishing pressure proxy, derived from a length indicator ) of the rfb rule.
}
\details{
The value is calculated by comparing the mean catch length (above length of first capture Lc) to a reference length.

\code{rfb_f()} is an alias for
\code{comp_f()} with identical arguments and functionality.
}
\section{Note}{

The reference length Lref should be kept constant for all years unless there 
a substantial changes in the fishery or fishery selectivity.
}

\examples{

}
\references{
ICES. 2022. ICES technical guidance for harvest control rules and stock assessments for stocks in categories 2 and 3. In Report of ICES Advisory Committee, 2022. ICES Advice 2022, Section 16.4.11, 20 pp. \url{https://doi.org/10.17895/ices.advice.19801564}.

Fischer, S. H., De Oliveira, J. A. A., Mumford, J. D., and Kell, L. T. 2023. Risk equivalence in data‐limited and data‐rich fisheries management: An example based on the ICES advice framework. Fish and Fisheries, 24: 231--247. \url{https://doi.org/10.1111/faf.12722}.

Fischer, S. H., De Oliveira, J. A. A., Mumford, J. D., and Kell, L. T. 2021. Application of explicit precautionary principles in data-limited fisheries management. ICES Journal of Marine Science, 78: 2931--2942. \url{https://doi.org/10.1093/icesjms/fsab169}.

Fischer, S. H., De Oliveira, J. A. A., Mumford, J. D., and Kell, L. T. 2021. Using a genetic algorithm to optimize a data-limited catch rule. ICES Journal of Marine Science, 78: 1311--1323. \url{https://doi.org/10.1093/icesjms/fsab018}.

Fischer, S. H., De Oliveira, J. A. A., and Kell, L. T. 2020. Linking the performance of a data-limited empirical catch rule to life-history traits. ICES Journal of Marine Science, 77: 1914--1926. \url{https://doi.org/10.1093/icesjms/fsaa054}.
}

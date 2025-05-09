% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/F_hr.R
\name{F}
\alias{F}
\alias{F,HR,f-method}
\alias{F,HR,missing-method}
\alias{F,numeric,missing-method}
\alias{F,F,missing-method}
\title{Calculation of the (relative) harvest rate target}
\usage{
F(object, indicator, yr_ref, units, MSE, multiplier, ...)
}
\arguments{
\item{object}{The time series with (relative) harvest rate values. See \code{\link{HR}}.}

\item{indicator}{The length based indicator. See \code{\link{f}}.}

\item{yr_ref}{Optional. Allows direct specification of years to include in the calculation instead of using \code{indicator}.}

\item{units}{Optional. The units of the harvest rate. Can be derived automatically from argument \code{HR}.}

\item{MSE}{Optional. \code{TRUE/FALSE}. Is the harvest rate a generic value or was it calculated with stock-specific simulations (MSE)?}

\item{multiplier}{Optional. \code{numeric}. Multiplier to adjust the target harvest rate. Only used if \code{MSE=TRUE}.}

\item{...}{Additional arguments. Not currently used.}
}
\value{
An object of class \code{F} with the target harvest rate and the input data.
}
\description{
This function calculates the target harvest rate for chr rule.
}
\details{
Usually, this functions is used by providing a time series of (relative) harvest rate values (see \code{\link{HR}}) and a length-based indicator based on the mean catch length (see \code{\link{f}}). The functions then finds those years where the indicator values are above 1, indicating that the fishing pressure is likely below Fmsy, extracts the corresponding (relative) harvest rate values for these years, and returns the average of these values as the target harvest rate.

Alternatively, years can directly be specified with the argument \code{yr_ref} and the target harvest rate is then calculated as the average of the (relative) harvest rates for these years. See the ICES technical guidelines (ICES, 2025) for details.

If stock-specific simulations were conducted to derive the target harvest rate, the calculation may differ. Nevertheless, it is good practice to express the target harvest rate relative to the harvest of one or more years. This is useful when historical harvest rates are revised (e.g. because of a revision of historical biomass index values) because the target harvest rate will then be scaled accordingly. If the argument \code{MSE=TRUE}, it is possible to include a multiplier directly in the calculation of the target harvest with the argument \code{multiplier}.

If an object of class \code{F} is provided, its validity is checked.
}
\section{Warning}{

For application in ICES, the target harvest rate should only be calculated in the first year the chr rule is used and the same value used in subsequent years.
}

\examples{

# load harvest rate time series and length-based indicator
data(ple7e_hr)
data(ple7e_f2)
# calculate target harvest rate
HR <- F(ple7e_hr, ple7e_f2)
HR
advice(HR)
plot(HR)

# use reference years when using in following years
F(ple7e_hr, yr_ref = c(2016, 2019))

# full example with ple7e data
data(ple7e_length)
# calculate (pooled) length at first capture first
lc <- Lc(data = ple7e_length, pool = 2017:2021)
# calculate mean catch length
lmean <- Lmean(data = ple7e_length, Lc = lc, units = "mm")
# reference length
lref <- Lref(Lc = 264, Linf = 528)
# calculate component f
f <- f(Lmean = lmean, Lref = lref, units = "mm")
# harvest rate
data(ple7e_idx)
data(ple7e_catch)
df <- merge(ple7e_catch, ple7e_idx, all = TRUE) # combine catch & index data
hr <- HR(df, units_catch = "tonnes", units_index = "kg/hr")
# calculate (relative) target harvest rate
HR <- F(hr, f)
HR
advice(HR)
plot(HR)

# application in following years without updating target harvest rate
HR <- F(hr, yr_ref = c(2016, 2019))

}
\references{
ICES. 2025. ICES Guidelines - Advice rules for stocks in category 2 and 3. Version 3. ICES Guidelines and Policies - Advice Technical Guidelines. 31 pp. \url{https://doi.org/10.17895/ices.pub.28506179}.

Fischer, S. H., De Oliveira, J. A. A., Mumford, J. D., and Kell, L. T. 2023. Risk equivalence in data‐limited and data‐rich fisheries management: An example based on the ICES advice framework. Fish and Fisheries, 24: 231--247. \url{https://doi.org/10.1111/faf.12722}.

Fischer, S. H., De Oliveira, J. A. A., Mumford, J. D., and Kell, L. T. 2022. Exploring a relative harvest rate strategy for moderately data-limited fisheries management. ICES Journal of Marine Science, 79: 1730--1741. \url{https://doi.org/10.1093/icesjms/fsac103}.
}

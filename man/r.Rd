% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/r.R
\name{r}
\alias{r}
\alias{r,data.frame-method}
\alias{r,numeric-method}
\alias{r,r-method}
\alias{rfb_r}
\alias{rfb_r,ANY-method}
\alias{rb_r}
\alias{rb_r,ANY-method}
\title{rfb/rb rule - component r (index ratio)}
\usage{
r(object, n0, n1, n2, units, hcr, ...)

rfb_r(object, n0, n1, n2, units, hcr = "rfb", ...)

rb_r(object, n0, n1, n2, units, hcr = "rb", ...)
}
\arguments{
\item{object}{The biomass index. Can be a \code{data.frame} with columns 'data' and 'index' or an \code{FLQuant} object defined by \code{FLCore}.}

\item{n0}{Optional. Time lag between the last index year and the last year to be used. By default, the last index year is used (\code{n0=0})}

\item{n1}{Optional. Number of years used in the numerator of the r component. Defaults to 2 (i.e. \code{n1} and \code{n2} use a 2 over 3 ratio).}

\item{n2}{Optional. Number of years used in the denominator of the r component. Defaults to 3.}

\item{units}{Optional. The units of the biomass index, e.g. 'kg/hr'. Only used for plotting.}

\item{hcr}{Optional. One of 'rfb' or 'rb'.}

\item{...}{Additional arguments. Not used.}
}
\value{
An object of class \code{r}
}
\description{
This function calculates component r (the index ratio) of the rfb and rb
 rule. The index needs to be a biomass index without age structure.
}
\details{
Usually, this method is used by providing only a biomass index, e.g. 
as a \code{data.frame}. The default index ratio is the average of the last
two index values, divided by the average of the three preceeding index 
values.

The index ratio is identical in the rfb and rb rules. 
\code{rfb_r()} and \code{rb_b()} are aliases for 
\code{r()} with identical arguments and functionality.
}
\section{Warning}{

For application in ICES, do not change the defaults (\code{n0}, \code{n1}, \code{n2}) unless the change is supported by stock-specific simulations.
}

\examples{
# If the value of r is known
rfb_r(0.9)
r(0.9)

# Use a data.frame with index values
df_idx <- data.frame(year = 2017:2021,
                     index = c(1.33, 1.13, 0.84, 0.60, 1.03))
r <- r(df_idx)
r
advice(r)

# plot
plot(r(df_idx, units = "kg/hr"))

}
\references{
ICES. 2025. ICES Guidelines - Advice rules for stocks in category 2 and 3. Version 3. ICES Guidelines and Policies - Advice Technical Guidelines. 31 pp. \url{https://doi.org/10.17895/ices.pub.28506179}.

Fischer, S. H., De Oliveira, J. A. A., Mumford, J. D., and Kell, L. T. 2023. Risk equivalence in data‐limited and data‐rich fisheries management: An example based on the ICES advice framework. Fish and Fisheries, 24: 231--247. \url{https://doi.org/10.1111/faf.12722}.

Fischer, S. H., De Oliveira, J. A. A., Mumford, J. D., and Kell, L. T. 2021. Application of explicit precautionary principles in data-limited fisheries management. ICES Journal of Marine Science, 78: 2931--2942. \url{https://doi.org/10.1093/icesjms/fsab169}.

Fischer, S. H., De Oliveira, J. A. A., Mumford, J. D., and Kell, L. T. 2021. Using a genetic algorithm to optimize a data-limited catch rule. ICES Journal of Marine Science, 78: 1311--1323. \url{https://doi.org/10.1093/icesjms/fsab018}.

Fischer, S. H., De Oliveira, J. A. A., and Kell, L. T. 2020. Linking the performance of a data-limited empirical catch rule to life-history traits. ICES Journal of Marine Science, 77: 1914--1926. \url{https://doi.org/10.1093/icesjms/fsaa054}.
}

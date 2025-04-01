cat3advice
================

`cat3advice` is an R package that allows the application of the ICES
category 3 data-limited harvest control rules (rfb/rb/chr rules) and
follows the ICES Technical Guidelines [ICES,
2025](https://doi.org/10.17895/ices.pub.28506179).

## Documentation

The package documentation contains help files for its functions which
also include code examples. See `help(package = "cat3advice")` for
available functions.

The main functions are `rfb()`, `rb()`, and `chr()`. Each of these
functions has a help file with code examples (see `?rfb`, `?rb` `?chr`).

A detailed vignette with code examples is included in the package
`vignette("cat3advice", package = "cat3advice")`. [The vignette is also
available on
GitHub](https://github.com/shfischer/cat3advice/blob/main/vignettes/cat3advice.md).

## Installation

The easiest way to install the `cat3advice` package is to install it as
a binary package from ICES r-universe:

``` r
install.packages("cat3advice", repos = c("https://ices-tools-prod.r-universe.dev", "https://cran.r-project.org"))
```

It is also possible to install it directly from the GitHub repository.
However, this means building the package locally and requires the
necessary build tools (e.g. on Windows RTools
<https://cran.r-project.org/bin/windows/Rtools/> and the `devtools` R
package).

``` r
library(remotes)
install_github("shfischer/cat3advice")
```

## Updates

See the
[NEWS](https://github.com/shfischer/cat3advice/blob/main/NEWS.md) file
for updates to the package.

## Support

Please use the repository’s [issue
page](https://github.com/shfischer/cat3advice/issues) to report issues,
bugs, and feature requests.

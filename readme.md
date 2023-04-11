cat3advice
================

`cat3advice` is an R package that allows the application of the ICES
category 3 data-limited harvest control rules (rfb/rb/chr rules) and
follows the ICES Technical Guidelines [ICES,
2022](https://doi.org/10.17895/ices.advice.19801564).

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

The latest version of the `cat3advice` R package can be installed from
GitHub with

``` r
library(remotes)
install_github("shfischer/cat3advice")
```

## Support

Please use the repository’s [issue
page](https://github.com/shfischer/cat3advice/issues) to report issues,
bugs, and feature requests.

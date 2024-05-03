NEWS
================

## 03/05/2024 - version 0.0.6

Adapted advice table (`advice()`) to match updated ICES guidance for
completing single-stock advice 2024.

- Text changes (e.g. changed “Fishing Pressure proxy” header to “Fishing
  pressure”, added “multiplier” where needed)

- Added row with “Fishing pressure proxy” (inverted f)

## 22/04/2024 - Version 0.0.5

Fixed bug in `A()` when argument `avg_years` is a vector of years (see
[GitHub issue](https://github.com/shfischer/cat3advice/issues/3)).

## 09/04/2024 - Version 0.0.4

Adapted rounding in `advice()` to match ICES advice sheets:

- Component f of the rfb rule; if units are supplied for the reference
  length and mean catch length, these values are not rounded according
  to ICES rounding rules, but instead to the nearest millimetre
  (e.g. “341 mm” or “34.1 cm”). The indicator value is still rounded
  according to ICES rounding rules.

## 01/03/2024 - Version 0.0.3

Added more functionality for chr rule

- The inverse indicator ratio for component f is now included in
  `indicator(f)` and can be shown with `inverse_indicator(f)`.

- The inverse indicator ratio can now be plotted with
  `plot(f, inverse = TRUE)` and this plots mimics the one shown in ICES
  advice sheets.

## Version 0.0.2

Changes to `advice()` for rfb/rb/chr rule

- Bugfix: the raw values for the rfb/rb/chr calculation in the summary
  table (before application of the stability clause) are now rounded to
  the nearest number (e.g. tonne).

## Version 0.0.1

- First version released for ICES assessment season 2023.

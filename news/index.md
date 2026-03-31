# Changelog

## imagefluency 1.0.0

- redesigned the local Shiny dashboard with multi-image upload,
  integrated results, image comparison, and CSV export
- added optional `collapse`-based speedups for self-similarity,
  symmetry, and typicality computations, while retaining base R
  fallbacks
- made the complexity tests more robust across platforms and updated the
  test suite to modern `testthat` conventions
- fixed
  [`img_complexity()`](https://imagefluency.com/reference/img_complexity.md)
  normalization for array inputs outside the `[0, 1]` range to avoid
  platform-dependent results

## imagefluency 0.2.5

CRAN release: 2024-02-22

- bugfixes in internal functions (incorrect parameter name in
  documentation)

## imagefluency 0.2.4

CRAN release: 2022-08-31

- fix alpha channel throwing error with typicality
- new vignette on analyzing multiple images at once

## imagefluency 0.2.3

CRAN release: 2020-01-09

- fixes shiny app directory issue
- url fixes

## imagefluency 0.2.3

CRAN release: 2020-01-09

- updated unit tests

## imagefluency 0.2.2

CRAN release: 2019-12-12

- under-the-hood-changes in preparation for upcoming R 4.0.0 release

## imagefluency 0.2.1

CRAN release: 2019-09-27

- new package vignette
- new package website
- some under the hood code improvements

## imagefluency 0.2.0

Major update \* new package name: imagefluency \* shorter, more
intuitive function names: img\_\* instead of quantify\_ now works
directly on color images \* improved symmetry score (finds best mirror
axis) \* different compression algorithms for complexity \* interactive
shiny app (alpha)

## imagefluency 0.1.2

- exemplary images included in package as data

## imagefluency 0.1.1

- complete rewrite of complexity function based on `magick` package

## imagefluency 0.1.0

- first version on github

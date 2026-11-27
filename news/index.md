# Changelog

## imagefluency 1.1.0

### New features

- new function
  [`img_fluency()`](https://imagefluency.com/reference/img_fluency.md)
  to compute the scores of several images at once: it takes the file
  paths (or a list of pre-loaded images) and returns a data frame with
  one row per image. Images that cannot be read and scores that cannot
  be computed result in `NA` (with a warning) instead of an error.
  Typicality is not computed by default but can be added via the
  `metrics` argument
- [`img_symmetry()`](https://imagefluency.com/reference/img_symmetry.md)
  is now much faster, especially for large images (e.g., about 18 times
  faster for a 2000 x 1333 color image): the best position of the mirror
  axis is located with a fast Fourier transform, and the symmetry score
  for this position is then computed directly as before. The new
  argument `exact = TRUE` computes the correlation directly for every
  position of the mirror axis (the previous method), which is slower and
  only rarely leads to a (minimally) different result
- [`img_self_similarity()`](https://imagefluency.com/reference/img_self_similarity.md)
  gains a `resize` argument: with `resize = FALSE`, the power spectrum
  of an image that is not square is computed from the original image
  instead of first resizing the image to a square, which avoids the
  smoothing caused by the resizing (default: `resize = TRUE`, i.e.,
  results are unchanged)

### Improvements

- all `img_*` functions now use the same input check: data frames and
  images with missing or non-finite values result in a clear error
  message, grayscale images stored as an array with one layer and
  grayscale images with an alpha channel are accepted, and an alpha
  channel is now also ignored by
  [`img_complexity()`](https://imagefluency.com/reference/img_complexity.md)
  (previously, it was part of the compressed image)
- [`img_symmetry()`](https://imagefluency.com/reference/img_symmetry.md),
  [`img_self_similarity()`](https://imagefluency.com/reference/img_self_similarity.md),
  and
  [`img_typicality()`](https://imagefluency.com/reference/img_typicality.md)
  now return `NA` with a warning instead of an error if the score cannot
  be computed because the image (or an image half or a color channel)
  has no variation, so that a single image no longer stops the analysis
  of many images
- [`img_typicality()`](https://imagefluency.com/reference/img_typicality.md)
  now always returns a plain numeric matrix whose rows are named after
  the names of the input list (previously, the class of the result and
  its printed precision depended on whether the `collapse` package was
  installed), returns an `NA` matrix instead of a list for fewer than
  two images, and accepts lists that contain both grayscale and color
  images
- [`img_self_similarity()`](https://imagefluency.com/reference/img_self_similarity.md)
  now warns if the slope is estimated from fewer than 10 spatial
  frequencies (small images)
- optional arguments that are not `TRUE` or `FALSE` (e.g., `NA`) now
  consistently result in a warning and the default value instead of an
  error
- the `collapse` package is no longer used: the computations now use
  equally fast base R code, so that all users get the same speed and
  results, and the message recommending to install `collapse` is gone
- color images are now converted with the exact Rec. 601 weights (0.299,
  0.587, 0.114) instead of rounded weights that summed to 0.9999. Scores
  for color images change in about the fourth significant digit (e.g., a
  perfectly symmetric color image now has a symmetry of 1 instead of
  0.9999); scores for grayscale images and complexity scores are
  unchanged

### Bug fixes

- fixed
  [`img_complexity()`](https://imagefluency.com/reference/img_complexity.md)
  and
  [`img_simplicity()`](https://imagefluency.com/reference/img_simplicity.md)
  failing for grayscale images (a matrix or the path to a grayscale
  image file resulted in the error “File not found or invalid path”). A
  grayscale image is treated as a color image with three identical color
  channels, so that the scores of grayscale and color images are
  comparable
- fixed
  [`img_typicality()`](https://imagefluency.com/reference/img_typicality.md)
  swapping height and width when resizing images with different image
  sizes or when using `rescale` (images that are not square were resized
  to a distorted aspect ratio)
- fixed
  [`img_contrast()`](https://imagefluency.com/reference/img_contrast.md)
  deciding separately for each color channel whether the values range
  from 0 to 255 (a color image with values between 0 and 255 and one
  color channel with values of at most 1 got a wrong score); this is now
  decided for the image as a whole
- a perfectly symmetric image now always has a symmetry of exactly 1
  (previously, rounding errors could result in values like
  0.9999999999999999)
- fixed [`rotate90()`](https://imagefluency.com/reference/rotate90.md)
  returning a vector instead of a matrix for images with only one row or
  column
- fixed
  [`img_self_similarity()`](https://imagefluency.com/reference/img_self_similarity.md)
  failing with `logplot = TRUE` when the `collapse` package is not
  installed
- dashboard: now works when started with
  [`imagefluency::run_imagefluency()`](https://imagefluency.com/reference/run_imagefluency.md)
  without attaching the package, unticking a single metric after “Select
  all metrics” no longer clears all metrics, an image that cannot be
  read no longer mixes up or stops the results, grayscale images are
  supported, the Compare tab shows typicality and handles identical file
  names, and the description of self-similarity was corrected

### Other changes

- [`img_symmetry()`](https://imagefluency.com/reference/img_symmetry.md):
  corrected the documentation of the shift of the mirror axis (cutting
  off up to 5% of the image width shifts the mirror axis by up to 2.5%,
  not 5%; the computation is unchanged), and `shift_range` is now
  checked
- fixed typos and grammar in the documentation and vignettes
- updated package logo

## imagefluency 1.0.1

- [`run_imagefluency()`](https://imagefluency.com/reference/run_imagefluency.md)
  gains a `max_images` parameter to control the maximum number of images
  that can be uploaded in the Shiny app (default: 100).

## imagefluency 1.0.0

CRAN release: 2026-03-31

- redesigned the Shiny dashboard with multi-image upload, integrated
  results, image comparison, and CSV export
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

- bug fixes in internal functions (incorrect parameter name in
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

- under-the-hood changes in preparation for upcoming R 4.0.0 release

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

- first version on GitHub

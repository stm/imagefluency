# Image self-similarity

`img_self_similarity` returns the self-similarity of an image (i.e., the
degree to which the log-log power spectrum of the image falls with a
slope of -2). Higher values indicate higher image self-similarity.

## Usage

``` r
img_self_similarity(
  img,
  full = FALSE,
  logplot = FALSE,
  raw = FALSE,
  resize = TRUE
)
```

## Arguments

- img:

  An image in form of a matrix or array of numeric values, preferably of
  square size. If the input is not square, bilinear resizing to a square
  size is performed by default using the
  [`OpenImageR`](https://rdrr.io/pkg/OpenImageR/man/resizeImage.html)
  package (see parameter `resize`). Use e.g.
  [`img_read()`](https://imagefluency.com/reference/img_read.md) to read
  an image file into `R`.

- full:

  logical. Should the full frequency range be used for interpolation?
  (default: `FALSE`)

- logplot:

  logical. Should the log-log power spectrum of the image be plotted?
  (default: `FALSE`)

- raw:

  logical. Should the raw value of the regression slope be returned?
  (default: `FALSE`)

- resize:

  logical. Should an image that is not square be resized to a square
  before the power spectrum is computed? If `FALSE`, the power spectrum
  is computed from the original image without resizing (see details).
  (default: `TRUE`)

## Value

a numeric value (self-similarity)

## Details

The function takes a (square) array or matrix of numeric or integer
values representing an image as input and returns the self-similarity of
the image. Self-similarity is computed via the slope of the log-log
power spectrum using OLS. A slope near `-2` indicates fractal-like
properties (see Redies et al., 2007; Simoncelli & Olshausen, 2001).
Thus, the value for self-similarity that is returned by the function is
calculated as `self-similarity = abs(slope + 2) * (-1)`. That is, the
measure reaches its maximum value of 0 for a slope of -2, and any
deviation from -2 results in negative values that are more negative the
higher the deviation from -2. For color images, the weighted average
between each color channel's values is computed (cf. Mayer & Landwehr,
2018).

By default, only the frequency range between 10 and 256 cycles per image
is used for interpolation. Computation for the full range can be set via
the parameter `full = TRUE`. Note that the highest frequency of an image
is half of its (smaller) side length in pixels. For small images, the
slope is therefore estimated from only a few frequencies (e.g., only
from the frequencies 10 and 11 for an image with 22 pixels). The
function throws a warning if the slope is estimated from fewer than 10
frequencies (i.e., for images with a side length of less than 38 pixels
if `full = FALSE`).

Self-similarity cannot be computed for an image without any variation
(e.g., an image with only one color; for color images, this applies to
each color channel). In this case, the function returns `NA` and throws
a warning.

If `logplot` is set to `TRUE` then a log-log plot of the power spectrum
is additionally shown. If the package `ggplot2` is installed the plot
includes the slope of the OLS regression. Note that this option is
currently only implemented for grayscale images.

It is possible to get the raw regression slope (instead of the
transformed value which indicates self-similarity) by using the option
`raw = TRUE`.

The power spectrum is computed for a square image. By default, an image
that is not square is therefore first resized to a square (bilinear
resizing of the longer image side to the length of the shorter side, so
that the complete image content is kept). Note, however, that the
interpolation smooths the image: fine details are attenuated more
strongly than coarse structures, which makes the slope of the power
spectrum steeper. The size of this effect depends on the image size and
on the aspect ratio, and it is larger for small images. With
`resize = FALSE`, the image is not resized. Instead, the power spectrum
is computed from the complete, original image, and the same spatial
frequencies (in cycles per image, in both the horizontal and the
vertical direction) that the resized image would contain are analyzed.
This also uses the complete image content but avoids the smoothing. For
square images, `resize` has no effect.

## Note

The function is inspired by Matlab's sfPlot (by Diederick C.
Niehorster).

## References

Mayer, S. & Landwehr, J. R. (2018). Quantifying Visual Aesthetics Based
on Processing Fluency Theory: Four Algorithmic Measures for Antecedents
of Aesthetic Preferences. *Psychology of Aesthetics, Creativity, and the
Arts*, *12*(4), 399–431.
[doi:10.1037/aca0000187](https://doi.org/10.1037/aca0000187)

Redies, C., Hasenstein, J., & Denzler, J. (2007). Fractal-like image
statistics in visual art: Similarity to natural scenes. *Spatial
Vision*, *21*, 137–148.
[doi:10.1163/156856807782753921](https://doi.org/10.1163/156856807782753921)

Simoncelli, E. P., & Olshausen, B. A. (2001). Natural image statistics
and neural representation. *Annual Review of Neuroscience*, *24*,
1193–1216.
[doi:10.1146/annurev.neuro.24.1.1193](https://doi.org/10.1146/annurev.neuro.24.1.1193)

## See also

[`img_read`](https://imagefluency.com/reference/img_read.md),
[`img_contrast`](https://imagefluency.com/reference/img_contrast.md),
[`img_complexity`](https://imagefluency.com/reference/img_complexity.md),
[`img_simplicity`](https://imagefluency.com/reference/img_simplicity.md),
[`img_symmetry`](https://imagefluency.com/reference/img_symmetry.md),
[`img_typicality`](https://imagefluency.com/reference/img_typicality.md)

## Examples

``` r
# Example image with high self-similarity: romanesco
romanesco <- img_read(system.file("example_images", "romanesco.jpg", package = "imagefluency"))
#
# display image
grid::grid.raster(romanesco)
#
# get self-similarity
img_self_similarity(romanesco)
#> [1] -0.03157893

# Example image with low self-similarity: office
office <- img_read(system.file("example_images", "office.jpg", package = "imagefluency"))
#
# display image
grid::grid.raster(office)

#
# get self-similarity
img_self_similarity(office)
#> [1] -1.217505

# the example images are not square: get self-similarity without resizing
img_self_similarity(office, resize = FALSE)
#> [1] -1.101494
```

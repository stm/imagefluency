# .selfsim

Returns the self-similarity of an image matrix as the degree to which
the slope of the log-log power spectrum falls with power according to
the value of 2.

## Usage

``` r
.selfsim(img, full, raw, logplot, resize = TRUE)
```

## Arguments

- img:

  A matrix of numeric values or integer values, preferably of square
  size.

- full:

  logical. Should the full frequency range be used for interpolation?

- raw:

  logical. Should the raw value of the regression slope be returned?

- logplot:

  logical. Should the log-log power spectrum of the image be plotted?

- resize:

  logical. Should an image that is not square be resized to a square? If
  `FALSE`, the power spectrum of the original image is cropped to a
  square instead.

## Value

a list with the self-similarity (`self_sim`), the number of spatial
frequencies used for the regression (`n_freq`), and, if
`logplot = TRUE`, the values for the log-log plot

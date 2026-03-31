# .selfsim

Returns the self-similarity of an image matrix as the degree to which
the slope of the log-log power spectrum falls with power according to
the value of 2.

## Usage

``` r
.selfsim(img, full, raw, logplot)
```

## Arguments

- img:

  A matrix of numeric values or integer values, preferably by square
  size.

- full:

  logical. Should the full frequency range be used for interpolation?

- raw:

  logical. Should the raw value of the regression slope be returned?

- logplot:

  logical. Should the log-log power spectrum of the image be plotted?

## Value

a numeric value (self-similarity)

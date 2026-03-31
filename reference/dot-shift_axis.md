# .shift_axis

.shift_axis shifts the mirror axis by xrange and returns the symmetries
at each axis position by calling the .sym_mirror function.

## Usage

``` r
.shift_axis(img, imgW, xrange)
```

## Arguments

- img:

  A matrix of numeric values or integer values.

- imgW:

  Image width (numeric).

- xrange:

  Shift range (numeric vector).

## Value

a vector of mirror symmetry values

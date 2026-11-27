# .sym

Calculates the mirror symmetry of an image by correlating image halves.

## Usage

``` r
.sym(img, color, per_channel = TRUE, shift_range = 0.05, exact = FALSE)
```

## Arguments

- img:

  A matrix of numeric values or integer values.

- color:

  logical. Color image?

- per_channel:

  logical. Channel-wise maximum symmetry?

- shift_range:

  numeric. Percentage of axis shift.

- exact:

  logical. Compute the correlation directly for every position of the
  mirror axis?

## Value

one numeric value as the symmetry (`NA` if there is no variation in the
image halves)

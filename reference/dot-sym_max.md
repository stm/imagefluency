# .sym_max

Returns the maximal mirror symmetry over all positions of the mirror
axis. By default, the best position of the mirror axis is located via
`.shift_axis_fft` and its symmetry is then computed directly via
`.sym_mirror`. With `exact = TRUE`, the symmetry is computed directly
for every position of the mirror axis via `.shift_axis`.

## Usage

``` r
.sym_max(channels, imgW, xrange, exact)
```

## Arguments

- channels:

  A list with one matrix (grayscale image) or three matrices (color
  channels that share the position of the mirror axis).

- imgW:

  Image width (numeric).

- xrange:

  Shift range (numeric vector).

- exact:

  logical. Compute the correlation directly for every position of the
  mirror axis?

## Value

one numeric value as the symmetry (`NA` if the symmetry cannot be
computed for any position of the mirror axis)

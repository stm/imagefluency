# .shift_axis_fft

Fast version of `.shift_axis`: returns the symmetries at each axis
position (in the same order), but computes them for all axis positions
at once instead of correlating the image halves separately for each
position. The results equal those of `.shift_axis` up to floating-point
error (about 1e-10).

## Usage

``` r
.shift_axis_fft(img, imgW, xrange)
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

## Details

The correlation between the left image half and the mirrored right image
half only requires the sums of the pixel values, of their squares, and
of the products of each pixel with its mirrored counterpart. The first
two are obtained from cumulative column sums. The sum of the products
for all axis positions is the convolution of each image row with itself,
which is computed via a fast Fourier transform.

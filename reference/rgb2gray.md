# RGB to Gray Conversion

`rgb2gray` transforms colors from RGB space (red/green/blue) into an
matrix of grayscale values.

## Usage

``` r
rgb2gray(img)
```

## Arguments

- img:

  3-dimensional array of numeric or integer values

## Value

A matrix of grayscale values.

## Details

The function takes a 3-dimensional array of numeric or integer values as
input (`img`) and returns a matrix of grayscale values as output. The
grayscale values are computed as
`GRAY = 0.2989 * RED + 0.5870 * GREEN + 0.1140 * BLUE`. If the array has
a fourth dimension (i.e., alpha channel), the fourth dimension is
ignored.

## Examples

``` r
# construct a sample RGB image as array of random integers
imgRed <- matrix(runif(100, min = 0, max = 255), 10, 10)
imgGreen <- matrix(runif(100, min = 0, max = 255), 10, 10)
imgBlue <- matrix(runif(100, min = 0, max = 255), 10, 10)
imgColor <- array(c(imgRed, imgGreen, imgBlue), dim = c(10, 10, 3))

# convert to gray
img <- rgb2gray(imgColor)
```

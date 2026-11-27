# .prepare_img

`.prepare_img` is a helper function of the `imagefluency` package that
checks whether the input is a valid image and returns the image in a
standard form. All `img_*` functions use this function so that the same
input is handled in the same way by every function.

## Usage

``` r
.prepare_img(img)
```

## Arguments

- img:

  An object that needs to be checked.

## Value

A list with the elements `img` (a matrix for grayscale images or a
3-dimensional array with 3 layers for color images) and `type` (`"gray"`
or `"rgb"`). An error is thrown if the check fails.

## Details

The following is checked: (1) the input is a matrix or an array (not,
e.g., a vector or a data frame), (2) the values are numeric or integer,
(3) the input is a matrix (grayscale image) or a 3-dimensional array
with 1 (grayscale), 2 (grayscale with alpha channel), 3 (color), or 4
(color with alpha channel) layers in the third dimension, and (4) the
input contains no missing or non-finite values. An alpha channel is
removed with a warning.

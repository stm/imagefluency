# Image contrast

`img_contrast` returns the RMS contrast of an image `img`. A higher
value indicates higher contrast.

## Usage

``` r
img_contrast(img)
```

## Arguments

- img:

  An image in form of a matrix or array of numeric values. Use e.g.
  [`img_read()`](https://imagefluency.com/reference/img_read.md) to read
  an image file into `R`.

## Value

a numeric value (RMS contrast)

## Details

The function returns the RMS contrast of an image `img`. The RMS
contrast is defined as the standard deviation of the normalized pixel
intensity values. A higher value indicates higher contrast. The image is
automatically normalized if necessary (i.e., normalization into range
\[0, 1\]).

For color images, the weighted average between each color channel's
values is computed.

## References

Peli, E. (1990). Contrast in complex images. *Journal of the Optical
Society of America A*, *7*, 2032–2040.
[doi:10.1364/JOSAA.7.002032](https://doi.org/10.1364/JOSAA.7.002032)

## See also

[`img_read`](https://imagefluency.com/reference/img_read.md),
[`img_complexity`](https://imagefluency.com/reference/img_complexity.md),
[`img_self_similarity`](https://imagefluency.com/reference/img_self_similarity.md),
[`img_simplicity`](https://imagefluency.com/reference/img_simplicity.md),
[`img_symmetry`](https://imagefluency.com/reference/img_symmetry.md),
[`img_typicality`](https://imagefluency.com/reference/img_typicality.md),

## Examples

``` r
# Example image with relatively high contrast: berries
berries <- img_read(system.file("example_images", "berries.jpg", package = "imagefluency"))
#
# display image
grid::grid.raster(berries)
#
# get contrast
img_contrast(berries)
#> [1] 0.2872886

# Example image with relatively low contrast: bike
bike <- img_read(system.file("example_images", "bike.jpg", package = "imagefluency"))
#
# display image
grid::grid.raster(bike)

#
# get contrast
img_contrast(bike)
#> [1] 0.08188639
```

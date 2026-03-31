# Image simplicity

`img_simplicity` returns the simplicity of an image as 1 minus the
complexity of the image. Higher values indicated higher image
simplicity.

## Usage

``` r
img_simplicity(imgfile, algorithm = "zip", rotate = FALSE)
```

## Arguments

- imgfile:

  Either a character string containing the path to the image file (or
  URL) or an an image in form of a matrix (grayscale image) or array
  (color image) of numeric values representing the pre-loaded image
  (e.g. by using
  [`img_read()`](https://imagefluency.com/reference/img_read.md)).

- algorithm:

  Character string that specifies which image compression algorithm to
  use. Currently implemented are `zip` with deflate compression, `jpg`,
  `gif`, and `png`.

- rotate:

  logical. Should the compressed file size of the rotated image also be
  computed? (see details)

## Value

a numeric value: 1 minus the ratio of compressed divided by uncompressed
file size (i.e., the compression rate)

## Details

Image simplicity is calculated as 1 minus the ratio between the
compressed and uncompressed file size (i.e., the compression rate).
Values can range between 0 (no compression possible, thus extremely
complex image) and almost 1 (virtually completely compressed image, thus
extremely simple image). Different compression algorithms are
implemented. For details, see
[`img_complexity`](https://imagefluency.com/reference/img_complexity.md).

## References

Donderi, D. C. (2006). Visual complexity: A Review. *Psychological
Bulletin*, *132*, 73–97.
[doi:10.1037/0033-2909.132.1.73](https://doi.org/10.1037/0033-2909.132.1.73)

Forsythe, A., Nadal, M., Sheehy, N., Cela-Conde, C. J., & Sawey, M.
(2011). Predicting Beauty: Fractal Dimension and Visual Complexity in
Art. *British Journal of Psychology*, *102*, 49–70.
[doi:10.1348/000712610X498958](https://doi.org/10.1348/000712610X498958)

Mayer, S. & Landwehr, J, R. (2018). Quantifying Visual Aesthetics Based
on Processing Fluency Theory: Four Algorithmic Measures for Antecedents
of Aesthetic Preferences. *Psychology of Aesthetics, Creativity, and the
Arts*, *12*(4), 399–431.
[doi:10.1037/aca0000187](https://doi.org/10.1037/aca0000187)

## See also

[`img_read`](https://imagefluency.com/reference/img_read.md),
[`img_complexity`](https://imagefluency.com/reference/img_complexity.md),
[`img_contrast`](https://imagefluency.com/reference/img_contrast.md),
[`img_self_similarity`](https://imagefluency.com/reference/img_self_similarity.md),
[`img_symmetry`](https://imagefluency.com/reference/img_symmetry.md),
[`img_typicality`](https://imagefluency.com/reference/img_typicality.md),

## Examples

``` r
# Example image with low simplicity: trees
trees <- img_read(system.file("example_images", "trees.jpg", package = "imagefluency"))
#
# display image
grid::grid.raster(trees)
#
# get simplicity
img_simplicity(trees)
#> [1] 0.1050314

# Example image with high simplicity: sky
sky <- img_read(system.file("example_images", "sky.jpg", package = "imagefluency"))
#
# display image
grid::grid.raster(sky)

#
# get simplicity
img_simplicity(sky)
#> [1] 0.5800746
```

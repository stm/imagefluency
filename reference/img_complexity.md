# Image complexity

`img_complexity` returns the complexity of an image via image
compression. Higher values indicate higher image complexity.

## Usage

``` r
img_complexity(imgfile, algorithm = "zip", rotate = FALSE)
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
  use. Currently implemented are `zip` with deflate compression
  (default), `jpg`, `gif`, and `png`.

- rotate:

  logical. Should the compressed file size of the rotated image also be
  computed? (see details)

## Value

a numeric value: the ratio of the compressed divided by the uncompressed
image file size

## Details

The function returns the visual complexity of an image. Visual
complexity is calculated as ratio between the compressed and
uncompressed image file size. Preferably, the original image is an
uncompressed image file.

The function takes the file path of an image file (or URL) or a
pre-loaded image as input argument (`imgfile`) and returns the ratio of
the compressed divided by the uncompressed image file size. Values can
range between almost 0 (virtually completely compressed image, thus
extremely simple image) and 1 (no compression possible, thus extremely
complex image).

You can choose between different image compression algorithms. Currently
implemented are `zip` with deflate compression (default), `jpg`, `gif`,
and `png`. See Mayer & Landwehr (2018) for a discussion of different
image compression algorithms for measuring visual complexity.

As most compression algorithms do not depict horizontal and vertical
redundancies equally, the function includes an optional `rotate`
parameter (default: `FALSE`). Setting this parameter to `TRUE` has the
following effects: first, the image is rotated by 90 degrees. Second, a
compressed version of the rotated image is created. Finally, the overall
compressed image's file size is computed as the minimum of the original
image's file size and the file size of the rotated image.

As `R`'s built-in `bmp` device creates (a) indexed instead of True Color
images and (b) creates files with different file sizes depending on the
operating system, the function relies on the `magick` package to write
(and read) images.

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
[`img_contrast`](https://imagefluency.com/reference/img_contrast.md),
[`img_self_similarity`](https://imagefluency.com/reference/img_self_similarity.md),
[`img_simplicity`](https://imagefluency.com/reference/img_simplicity.md),
[`img_symmetry`](https://imagefluency.com/reference/img_symmetry.md),
[`img_typicality`](https://imagefluency.com/reference/img_typicality.md),

## Examples

``` r
# Example image with high complexity: trees
trees <- img_read(system.file("example_images", "trees.jpg", package = "imagefluency"))
#
# display image
grid::grid.raster(trees)
#
# get complexity
img_complexity(trees)
#> [1] 0.8949686


# Example image with low complexity: sky
sky <- img_read(system.file("example_images", "sky.jpg", package = "imagefluency"))
#
# display image
grid::grid.raster(sky)

#
# get complexity
img_complexity(sky)
#> [1] 0.4199254
```

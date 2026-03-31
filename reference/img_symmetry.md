# Image symmetry

`img_symmetry` returns the vertical and horizontal mirror symmetry of an
image. Higher values indicate higher image symmetry.

## Usage

``` r
img_symmetry(img, vertical = TRUE, horizontal = TRUE, ...)
```

## Arguments

- img:

  An image in form of a matrix or array of numeric values. Use e.g.
  [`img_read()`](https://imagefluency.com/reference/img_read.md) to read
  an image file into `R`.

- vertical:

  logical. Should the vertical symmetry be computed? (default: TRUE)

- horizontal:

  logical. Should the horizontal symmetry be computed? (default: TRUE)

- ...:

  Further options: `shift_range` to shift the mirror axis, `per_channel`
  to switch between a maximal per channel vs. per image symmetry (see
  details).

## Value

a named vector of numeric values (vertical and horizontal symmetry)

## Details

The function returns the vertical and horizontal mirror symmetry of an
image `img`. Symmetry values can range between 0 (not symmetrical) and 1
(fully symmetrical). If `vertical` or `horizontal` is set to `FALSE`
then vertical or horizontal symmetry is not computed, respectively.

As the perceptual mirror axis is not necessarily exactly in the middle
of a picture, the function estimates in a first step several symmetry
values with different positions for the mirror axis. To this end, the
mirror axis is automatically shifted up to 5% (default) of the image
width to the left and to the right (in the case of vertical symmetry;
analogously for horizontal symmetry). In the second step, the overall
symmetry score is computed as the maximum of the symmetry scores given
the different mirror axes. See Mayer & Landwehr (2018) for details.

Advanced users can change the shift range with the optional parameter
`shift_range`, which takes a numeric decimal as input. The default
`shift_range = 0.05` (i.e., 5%).

For color images, the default is that first a maximal symmetry score (as
explained above) is obtained per color channel (parameter
`per_channel = TRUE`). Subsequently, a weighted average between each
color channel's maximal score is computed as the image's overall
symmetry. Advanced users can reverse this order by setting
`per_channel = FALSE`. This results in first computing the weighted
averages for each position of the mirror axis separately, and afterwards
finding the maximal overall symmetry score.

## References

Mayer, S. & Landwehr, J, R. (2018). Quantifying Visual Aesthetics Based
on Processing Fluency Theory: Four Algorithmic Measures for Antecedents
of Aesthetic Preferences. *Psychology of Aesthetics, Creativity, and the
Arts*, *12*(4), 399–431.
[doi:10.1037/aca0000187](https://doi.org/10.1037/aca0000187)

## See also

[`img_read`](https://imagefluency.com/reference/img_read.md),
[`img_complexity`](https://imagefluency.com/reference/img_complexity.md),
[`img_contrast`](https://imagefluency.com/reference/img_contrast.md),
[`img_self_similarity`](https://imagefluency.com/reference/img_self_similarity.md)
[`img_simplicity`](https://imagefluency.com/reference/img_simplicity.md),
[`img_typicality`](https://imagefluency.com/reference/img_typicality.md)

## Examples

``` r
# Example image with high vertical symmetry: rails
rails <- img_read(system.file("example_images", "rails.jpg", package = "imagefluency"))
#
# display image
grid::grid.raster(rails)
#
# get symmetry
img_symmetry(rails)
#>   vertical horizontal 
#>  0.9550184  0.1230524 

# Example image with low vertical symmetry: bridge
bridge <- img_read(system.file("example_images", "bridge.jpg", package = "imagefluency"))
#
# display image
grid::grid.raster(bridge)

#
# get symmetry
img_symmetry(bridge)
#>   vertical horizontal 
#>  0.4802899  0.2525909 
```

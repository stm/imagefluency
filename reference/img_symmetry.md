# Image symmetry

`img_symmetry` returns the vertical and horizontal mirror symmetry of an
image. Higher values indicate higher image symmetry.

## Usage

``` r
img_symmetry(img, vertical = TRUE, horizontal = TRUE, exact = FALSE, ...)
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

- exact:

  logical. Should the correlation be computed directly for every
  position of the mirror axis instead of locating the best position with
  a fast Fourier transform? This is considerably slower and only rarely
  leads to a (minimally) different result (see details). (default:
  FALSE)

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
values with different positions for the mirror axis. To this end, up to
5% (default) of the image width is cut off on the left or on the right
side of the image (in the case of vertical symmetry; analogously for
horizontal symmetry), and the symmetry of the remaining image part is
computed. As the mirror axis is always in the middle of the remaining
image part, cutting off 5% of the image width shifts the mirror axis by
up to 2.5% of the image width to the right or to the left. In the second
step, the overall symmetry score is computed as the maximum of the
symmetry scores given the different mirror axes. See Mayer & Landwehr
(2018) for details.

Advanced users can change the shift range with the optional parameter
`shift_range`, which takes a numeric decimal between 0 and 1 as input
(the maximum share of the image width that is cut off). The default is
`shift_range = 0.05` (i.e., 5%).

Symmetry cannot be computed if one of the image halves has no variation
(e.g., an image half that has only one color; for color images, this
applies to each color channel). In this case, the function returns `NA`
for the respective symmetry and throws a warning. For color images with
a color channel that has no variation (e.g., an image that only consists
of black and red), consider converting the image into grayscale first
(see [`rgb2gray`](https://imagefluency.com/reference/rgb2gray.md)).

For color images, the default is that first a maximal symmetry score (as
explained above) is obtained per color channel (parameter
`per_channel = TRUE`). Subsequently, a weighted average between each
color channel's maximal score is computed as the image's overall
symmetry. Advanced users can reverse this order by setting
`per_channel = FALSE`. This results in first computing the weighted
averages for each position of the mirror axis separately, and afterwards
finding the maximal overall symmetry score.

To keep the computation fast, the best position of the mirror axis is
located with a fast Fourier transform (FFT), which yields the symmetry
values of all positions of the mirror axis at once. The symmetry score
for this best position is then computed directly as the correlation
between the two image halves. The result is the same as when the
correlation is computed directly for every position of the mirror axis,
unless the symmetry values of two positions are practically tied
(difference smaller than about 1e-10). In this rare case, the score
might differ by a correspondingly small amount. Setting `exact = TRUE`
computes the correlation directly for every position of the mirror axis
(the method used up to version 1.0.1), which avoids this at the expense
of speed (the computation takes several times longer; for large images,
more than ten times longer).

A perfectly symmetric image (i.e., the two image halves are identical
mirror images of each other) always receives a score of exactly 1.

## References

Mayer, S. & Landwehr, J. R. (2018). Quantifying Visual Aesthetics Based
on Processing Fluency Theory: Four Algorithmic Measures for Antecedents
of Aesthetic Preferences. *Psychology of Aesthetics, Creativity, and the
Arts*, *12*(4), 399–431.
[doi:10.1037/aca0000187](https://doi.org/10.1037/aca0000187)

## See also

[`img_read`](https://imagefluency.com/reference/img_read.md),
[`img_complexity`](https://imagefluency.com/reference/img_complexity.md),
[`img_contrast`](https://imagefluency.com/reference/img_contrast.md),
[`img_self_similarity`](https://imagefluency.com/reference/img_self_similarity.md),
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
#>  0.9551137  0.1230616 

# Example image with low vertical symmetry: bridge
bridge <- img_read(system.file("example_images", "bridge.jpg", package = "imagefluency"))
#
# display image
grid::grid.raster(bridge)

#
# get symmetry
img_symmetry(bridge)
#>   vertical horizontal 
#>  0.4803173  0.2526229 

# compute the correlation directly for every position of the mirror axis
# (slower)
img_symmetry(bridge, exact = TRUE)
#>   vertical horizontal 
#>  0.4803173  0.2526229 
```

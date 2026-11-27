# Image fluency scores for several images at once

`img_fluency` computes the image fluency scores for several images at
once and returns the results as a data frame with one row per image.

## Usage

``` r
img_fluency(
  imgs,
  metrics = c("contrast", "self_similarity", "simplicity", "symmetry"),
  gray = FALSE,
  progress = interactive()
)
```

## Arguments

- imgs:

  Either a character vector with the paths to the image files (e.g.,
  created by [`list.files()`](https://rdrr.io/r/base/list.files.html)
  with `full.names = TRUE`) or a *list* of pre-loaded images (matrices
  or arrays of numeric values, e.g. by using
  [`img_read()`](https://imagefluency.com/reference/img_read.md)).

- metrics:

  Character vector that specifies which scores are computed. One or more
  of `"contrast"`, `"self_similarity"`, `"simplicity"`, `"symmetry"`,
  and `"typicality"`. By default, all scores except typicality are
  computed (see details).

- gray:

  logical. Should color images be converted into grayscale (using
  [`rgb2gray()`](https://imagefluency.com/reference/rgb2gray.md)) before
  the scores are computed? This speeds up the computation. (default:
  `FALSE`)

- progress:

  logical. Should a progress bar be shown? (default: `TRUE` in
  interactive sessions)

## Value

a data frame with one row per image (in the order of `imgs`). The first
column `filename` contains the file names of the images (for a list of
pre-loaded images: the names of the list; `img1`, `img2`, ... for images
without a name). The other columns contain the scores and are named
after `metrics`.

## Details

The function reads one image after the other and computes the requested
scores with the respective functions of the package using their default
settings:

- `"contrast"`:
  [`img_contrast()`](https://imagefluency.com/reference/img_contrast.md)

- `"self_similarity"`:
  [`img_self_similarity()`](https://imagefluency.com/reference/img_self_similarity.md)

- `"simplicity"`:
  [`img_simplicity()`](https://imagefluency.com/reference/img_simplicity.md)

- `"symmetry"`: vertical symmetry, i.e.,
  [`img_symmetry()`](https://imagefluency.com/reference/img_symmetry.md)
  with `horizontal = FALSE`

- `"typicality"`:
  [`img_typicality()`](https://imagefluency.com/reference/img_typicality.md)

Typicality is not computed by default (but can be added to `metrics`)
for two reasons. First, typicality is not a characteristic of a single
image, but of an image relative to all other images that are analyzed.
Thus, the typicality scores change if images are added or removed.
Second, all images have to be kept in memory at the same time to compute
typicality, which might be a problem if you analyze many or large
images. For all other scores, only one image is kept in memory at a
time.

The function does not stop if an image file cannot be read or if a score
cannot be computed for an image (e.g., symmetry for an image without any
variation in one image half). Instead, the respective scores in the
results are `NA`, and a warning at the end lists the images concerned.
Warnings of the individual functions are not shown. Use the individual
functions if you need other than the default settings or want to find
out why a score could not be computed for an image.

## See also

[`img_read`](https://imagefluency.com/reference/img_read.md),
[`img_contrast`](https://imagefluency.com/reference/img_contrast.md),
[`img_self_similarity`](https://imagefluency.com/reference/img_self_similarity.md),
[`img_simplicity`](https://imagefluency.com/reference/img_simplicity.md),
[`img_symmetry`](https://imagefluency.com/reference/img_symmetry.md),
[`img_typicality`](https://imagefluency.com/reference/img_typicality.md)

## Examples

``` r
# paths to some of the example images
files <- system.file("example_images", c("rails.jpg", "sky.jpg", "trees.jpg"),
                     package = "imagefluency")

# contrast and symmetry scores of all images
img_fluency(files, metrics = c("contrast", "symmetry"))
#>    filename   contrast  symmetry
#> 1 rails.jpg 0.36415048 0.9551137
#> 2   sky.jpg 0.08109417 0.6473722
#> 3 trees.jpg 0.18911031 0.1908871

# add typicality (of the images relative to each other)
img_fluency(files, metrics = c("contrast", "symmetry", "typicality"))
#>    filename   contrast  symmetry typicality
#> 1 rails.jpg 0.36415048 0.9551137  0.8910676
#> 2   sky.jpg 0.08109417 0.6473722  0.1035027
#> 3 trees.jpg 0.18911031 0.1908871  0.5195922

# all scores except typicality (default)
img_fluency(files)
#>    filename   contrast self_similarity simplicity  symmetry
#> 1 rails.jpg 0.36415048      -0.5784927  0.5357527 0.9551137
#> 2   sky.jpg 0.08109417      -1.5620177  0.5792614 0.6473722
#> 3 trees.jpg 0.18911031      -0.5505834  0.1045811 0.1908871
```

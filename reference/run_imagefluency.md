# Interactive dashboard for `imagefluency`

Launches a dashboard to interactively use the functions of the
`imagefluency` package. The dashboard is built using the `shiny` package
and provides a user-friendly interface for uploading images, analyzing
them, and exporting the results. The dashboard supports multi-image
uploads and side-by-side comparisons of image fluency scores.

## Usage

``` r
run_imagefluency(max_images = 100)
```

## Arguments

- max_images:

  Integer. Maximum number of images that can be uploaded. Defaults to
  100.

## Examples

``` r
## Only run this example in interactive R sessions
if (interactive()) {
  run_imagefluency()

  # increase the maximum number of uploadable images to 150
  run_imagefluency(max_images = 150)
}
```

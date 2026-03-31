# Read bitmap image (bmp, jpg, png, tiff)

Wrapper for readbitmap's
[`read.bitmap`](https://rdrr.io/pkg/readbitmap/man/read.bitmap.html)
function. The function currently allows reading in images in `bmp`,
`jpg` / `jpeg`, `png`, or `tif` / `tiff` format.

## Usage

``` r
img_read(path, ...)
```

## Arguments

- path:

  Path to the image file.

- ...:

  Additional parameters that are passed to
  [`read.bitmap`](https://rdrr.io/pkg/readbitmap/man/read.bitmap.html)
  and the underlying image reader packages.

## Value

Objects returned by
[`read.bmp`](https://rdrr.io/pkg/bmp/man/read.bmp.html),
[`readJPEG`](https://rdrr.io/pkg/jpeg/man/readJPEG.html),
[`readPNG`](https://rdrr.io/pkg/png/man/readPNG.html), or
[`readTIFF`](https://rdrr.io/pkg/tiff/man/readTIFF.html). See their
documentation for details.

## Details

For details, see the
[`read.bitmap`](https://rdrr.io/pkg/readbitmap/man/read.bitmap.html)
documentation.

## See also

[`read.bitmap`](https://rdrr.io/pkg/readbitmap/man/read.bitmap.html),
[`read.bmp`](https://rdrr.io/pkg/bmp/man/read.bmp.html),
[`readJPEG`](https://rdrr.io/pkg/jpeg/man/readJPEG.html),
[`readPNG`](https://rdrr.io/pkg/png/man/readPNG.html),
[`readTIFF`](https://rdrr.io/pkg/tiff/man/readTIFF.html)

## Examples

``` r
## Example image with high vertical symmetry: rails
rails <- img_read(system.file("example_images", "rails.jpg", package = "imagefluency"))
```

# imagefluency 1.1.0

* color images are now converted with the exact Rec. 601 weights (0.299, 0.587, 0.114) instead of rounded weights that summed to 0.9999. Scores for color images change in about the fourth significant digit (e.g., a perfectly symmetric color image now has a symmetry of 1 instead of 0.9999); scores for grayscale images and complexity scores are unchanged
* fixed `img_complexity()` and `img_simplicity()` failing for grayscale images (a matrix or the path to a grayscale image file resulted in the error "File not found or invalid path"). A grayscale image is treated as a color image with three identical color channels, so that the scores of grayscale and color images are comparable
* all `img_*` functions now use the same input check: data frames and images with missing or non-finite values result in a clear error message, grayscale images stored as an array with one layer and grayscale images with an alpha channel are accepted, and an alpha channel is now also ignored by `img_complexity()` (previously, it was part of the compressed image)
* fixed `img_contrast()` deciding separately for each color channel whether the values range from 0 to 255 (a color image with values between 0 and 255 and one color channel with values of at most 1 got a wrong score); this is now decided for the image as a whole
* optional arguments that are not `TRUE` or `FALSE` (e.g., `NA`) now consistently result in a warning and the default value instead of an error
* fixed `rotate90()` returning a vector instead of a matrix for images with only one row or column
* fixed `img_typicality()` swapping height and width when resizing images with different image sizes or when using `rescale` (images that are not square were resized to a distorted aspect ratio)
* fixed `img_self_similarity()` failing with `logplot = TRUE` when the `collapse` package is not installed
* fixed typos and grammar in the documentation and vignettes

# imagefluency 1.0.1

* `run_imagefluency()` gains a `max_images` parameter to control the maximum number of images that can be uploaded in the Shiny app (default: 100).

# imagefluency 1.0.0

* redesigned the Shiny dashboard with multi-image upload, integrated
  results, image comparison, and CSV export
* added optional `collapse`-based speedups for self-similarity, symmetry, and
  typicality computations, while retaining base R fallbacks
* made the complexity tests more robust across platforms and updated the test
  suite to modern `testthat` conventions
* fixed `img_complexity()` normalization for array inputs outside the `[0, 1]`
  range to avoid platform-dependent results


# imagefluency 0.2.5

* bug fixes in internal functions (incorrect parameter name in documentation)

# imagefluency 0.2.4

* fix alpha channel throwing error with typicality
* new vignette on analyzing multiple images at once

# imagefluency 0.2.3

* fixes shiny app directory issue
* url fixes

# imagefluency 0.2.3

* updated unit tests

# imagefluency 0.2.2

* under-the-hood changes in preparation for upcoming R 4.0.0 release

# imagefluency 0.2.1

* new package vignette
* new package website
* some under the hood code improvements

# imagefluency 0.2.0

Major update
* new package name: imagefluency
* shorter, more intuitive function names: img_* instead of quantify_*
* now works directly on color images
* improved symmetry score (finds best mirror axis)
* different compression algorithms for complexity
* interactive shiny app (alpha)

# imagefluency 0.1.2

* exemplary images included in package as data

# imagefluency 0.1.1

* complete rewrite of complexity function based on `magick` package

# imagefluency 0.1.0

* first version on GitHub

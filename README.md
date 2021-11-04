## imagefluency: Image Statistics Based on Processing Fluency <img src="man/figures/logo.png" align="right" />

<!-- badges: start -->
[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](http://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/)
[![R-CMD-check](https://github.com/stm/imagefluency/workflows/R-CMD-check/badge.svg)](https://github.com/stm/imagefluency/actions)
[![codecov test coverage](https://codecov.io/gh/stm/imagefluency/branch/master/graph/badge.svg)](https://app.codecov.io/gh/stm/imagefluency?branch=main)
[![CRAN status](https://www.r-pkg.org/badges/version/imagefluency)](https://cran.r-project.org/package=imagefluency)
[![CRAN downloads](http://cranlogs.r-pkg.org/badges/imagefluency)](https://cran.r-project.org/package=imagefluency)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.5614666.svg)](https://doi.org/10.5281/zenodo.5614666)
<!-- badges: end -->

## Overview

**imagefluency** is an simple R package for image fluency scores. The
package allows to get scores for several basic aesthetic principles that
facilitate fluent cognitive processing of images. If you want to try it out before installing, you can find an interactive Shiny app [here](https://mayer.shinyapps.io/imagefluency/) (alpha version).
    
The main functions are:

* `img_contrast()`  to get the visual contrast of an image.
* `img_complexity()`  to get the visual complexity of an image (equals
   1 minus image simplicity)
* `img_self_similarity()`  to get the visual self-similarity of an image
* `img_simplicity()`  function to get the visual simplicity of an image (equals
   1 minus image complexity).
* `img_symmetry()`  to get the vertical and horizontal symmetry of an
   image.
* `img_typicality()`  to get the visual typicality of a list of images relative
   to each other

Other helpful functions are:

* `img_read()`  wrapper function to read images into R using `read.bitmap()` from the
  [readbitmap](https://github.com/jefferis/readbitmap) package
* `rgb2gray()`  convert images from RGB into grayscale (might speed up computation)
* `run_imagefluency()`  to launch a Shiny app locally on your computer for an interactive demo of the
   main functions


The main author is [Stefan Mayer](https://github.com/stm/).

## Installation

You can install the current stable version from CRAN.
```r
install.packages('imagefluency')
```

To download the latest development version from Github use the `install_github` function of the `remotes` package.
```r
# install remotes if necessary
if (!require('remotes')) install.packages('remotes')
# install imagefluency from github
remotes::install_github('stm/imagefluency', build_vignettes = TRUE)
```
Use the following link to report bugs/issues: <https://github.com/stm/imagefluency/issues>

## Example usage

```r
# visual contrast
#
# example image file (from package): bike.jpg
bike_location <- system.file('example_images', 'bike.jpg', package = 'imagefluency')
# read image from file
bike <- img_read(bike_location)
# get contrast
img_contrast(bike)

# visual symmetry
#
# read image
rails <- img_read(system.file('example_images', 'rails.jpg', package = 'imagefluency'))
# get only vertical symmetry
img_symmetry(rails, horizontal = FALSE)
```

## Documentation

See the [getting started vignette](https://stm.github.io/imagefluency/articles/getting-started.html) for a detailed introduction and the [reference page](https://stm.github.io/imagefluency/reference/index.html) for details on each function. 

If you are analyzing a larger number of images, make sure to read the tutorial on how to [analyze multiple images at once](https://stm.github.io/imagefluency/articles/batch-processing.html).

## Citation

To cite **imagefluency** in publications use:

> Mayer, S. (2021). *imagefluency: Image Statistics Based on Processing Fluency*. R package version 0.2.4. doi:  [10.5281/zenodo.5614666](https://doi.org/10.5281/zenodo.5614666)

A BibTeX entry is:
```
@software{,
  author       = {Stefan Mayer},
  title        = {imagefluency: Image Statistics Based on Processing Fluency},
  year         = 2021,
  version      = {0.2.4},
  doi          = {10.5281/zenodo.5614666},
  url          = {https://github.com/stm/imagefluency}
}
```
## Dependencies
The `img_complexity` function relies on the packages [R.utils](https://cran.r-project.org/package=R.utils) and [magick](https://github.com/ropensci/magick). The `img_self_similarity` function relies on the packages [OpenImageR](https://github.com/mlampros/OpenImageR), [pracma](https://cran.r-project.org/package=pracma), and [quadprog](https://cran.r-project.org/package=quadprog). The `img_read` function relies on the [readbitmap](https://github.com/jefferis/readbitmap) package. The `run_imagefluency` shiny app depends on [shiny](https://github.com/rstudio/shiny).

## Further references

To learn more about the different image fluency metrics, see the following publications:

* Mayer, S. & Landwehr, J, R. (2018). Quantifying Visual Aesthetics
Based on Processing Fluency Theory: Four Algorithmic Measures for
Antecedents of Aesthetic Preferences. *Psychology of Aesthetics,
Creativity, and the Arts*, *12*(4), 399--431. 
doi: [10.1037/aca0000187](https://doi.org/10.1037/aca0000187)

* Mayer, S. & Landwehr, J. R. (2018). Objective measures of design
typicality. *Design Studies*, *54*, 146--161.
doi: [10.1016/j.destud.2017.09.004](https://doi.org/10.1016/j.destud.2017.09.004)

## Code of Conduct

Please note that this project is released with a [Contributor Code of Conduct](https://stm.github.io/imagefluency/CODE_OF_CONDUCT.html). By contributing to this project, you agree to abide by its terms.

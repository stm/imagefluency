## rquantae: A toolbox for quantified aesthetics in R

The `rquantae` package is an `R` package that provides a toolbox for quantified aesthetics. Five basic aesthetic principles are implemented that can be used to assess the aesthetic properties of an image.

The properties are:

* symmetry: `quantify_symmetry`
* complexity (simplicity): `quantify_complexity`
* contrast: `quantify_contrast`
* typicality: `quantify_typicality`
* self-similarity: `quantify_self_similarity`

The main author is [Stefan Mayer](http://github.com/stm/).

## Installation

To download the lastest version from Github use the `install_github` function of the `devtools` package.
```r
if (!require("devtools")) install.packages("devtools")
devtools::install_github('stm/rquantae')
```
Use the following link to report bugs/issues: <https://github.com/stm/rquantae/issues>

## Dependencies
The `quantify_complexity` function relies on the `magick` package. The `quantify_self_similarity` function relies on the packages `OpenImageR`, `pracma`, and `quadprog`.

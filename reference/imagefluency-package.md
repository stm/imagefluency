# imagefluency: Image Statistics Based on Processing Fluency

Get image statistics based on processing fluency theory. The functions
provide scores for several basic aesthetic principles that facilitate
fluent cognitive processing of images: contrast, complexity /
simplicity, self-similarity, symmetry, and typicality. See Mayer &
Landwehr (2018)
[doi:10.1037/aca0000187](https://doi.org/10.1037/aca0000187) and Mayer &
Landwehr (2018)
[doi:10.31219/osf.io/gtbhw](https://doi.org/10.31219/osf.io/gtbhw) for
the theoretical background of the methods.

## Details

*The main functions are:*

- [`img_contrast`](https://imagefluency.com/reference/img_contrast.md)
  to get the visual contrast of an image

- [`img_complexity`](https://imagefluency.com/reference/img_complexity.md)
  to get the visual complexity of an image (equals 1 minus image
  simplicity)

- [`img_self_similarity`](https://imagefluency.com/reference/img_self_similarity.md)
  to get the visual self-similarity of an image

- [`img_simplicity`](https://imagefluency.com/reference/img_simplicity.md)
  to get the visual simplicity of an image (equals 1 minus image
  complexity)

- [`img_symmetry`](https://imagefluency.com/reference/img_symmetry.md)
  to get the vertical and horizontal symmetry of an image

- [`img_typicality`](https://imagefluency.com/reference/img_typicality.md)
  to get the visual typicality of a list of images relative to each
  other

*Other helpful functions are:*

- [`img_read`](https://imagefluency.com/reference/img_read.md) wrapper
  function to read images using
  [`readbitmap::read.bitmap`](https://rdrr.io/pkg/readbitmap/man/read.bitmap.html)

- [`run_imagefluency`](https://imagefluency.com/reference/run_imagefluency.md)
  to launch a Shiny app for an interactive demo of the main functions

- [`rgb2gray`](https://imagefluency.com/reference/rgb2gray.md) to
  convert images from RGB into grayscale

## References

Mayer, S. & Landwehr, J, R. (2018). Quantifying Visual Aesthetics Based
on Processing Fluency Theory: Four Algorithmic Measures for Antecedents
of Aesthetic Preferences. *Psychology of Aesthetics, Creativity, and the
Arts*, *12*(4), 399–431.
[doi:10.1037/aca0000187](https://doi.org/10.1037/aca0000187)

Mayer, S. & Landwehr, J. R. (2018). Objective measures of design
typicality. *Design Studies*, *54*, 146–161.
[doi:10.1016/j.destud.2017.09.004](https://doi.org/10.1016/j.destud.2017.09.004)

## See also

Useful links:

- <https://imagefluency.com>

- <https://github.com/stm/imagefluency/>

- [doi:10.5281/zenodo.5614665](https://doi.org/10.5281/zenodo.5614665)

- Report bugs at <https://github.com/stm/imagefluency/issues/>

## Author

**Maintainer**: Stefan Mayer <stefan@mayer-de.com>
([ORCID](https://orcid.org/0000-0003-0034-7090))

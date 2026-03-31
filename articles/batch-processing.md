# Analyzing multiple images at once

The [getting started
vignette](https://imagefluency.com/articles/getting-started.md) explains
how to apply the core functions of the `imagefluency` package to an
image. However, often you want to analyze several images at once. The
following tutorial provides a walkthrough of batch-analyzing images with
the `imagefluency` package.

### Reading all images from a folder

Before we can start analyzing the images, we need to load them into R.
In this example, we choose to read all images that are shipped with the
`imagefluency` package. However, you can replace the file path with any
file path on your computer.

Next, we read the file names of all images that satisfy a specific
pattern using [`list.files()`](https://rdrr.io/r/base/list.files.html).
In our case, we want the file names of all files that have `.jpg` as
their file extension. Thus, we specify `pattern = '*.jpg$'`. The `$`
sign at the end ensures that we only find files that *end* with `.jpg`,
a file like `image1.jpg.png` would therefore not be listed. Multiple
patterns could also be specified (e.g., `pattern = '*.jpg$|*.png$')`.
Note that we do *not* actually load the images into our R session here.
Instead, using [`list.files()`](https://rdrr.io/r/base/list.files.html)
we just extract the images’ file paths for later use.

``` r
# path to images (here: example images from the imagefluency package)
# 
# --NOTE: replace with your folder of interest, e.g.
#         mypath <- "C:/Users/NAME/Documents/Project/images/" 
mypath <- system.file("example_images", package = "imagefluency")

# read filenames of images
# --NOTE: change file ending accordingly!
fileNames <- list.files(mypath, pattern = '*.jpg$', full.names = TRUE)
```

### Setting up the analysis

In the next step, we load the `imagefluency` package and create a data
frame to store the results. In this tutorial, we’ll use a simple for
loop to iterate over all images in the folder, get the image fluency
scores, and write the results into the data frame. This is the simplest
approach if you have many images and might run into memory issues when
loading all images at once.

Of course, it is also possible to run the analyses using
[`lapply()`](https://rdrr.io/r/base/lapply.html) or
[`purrr::map()`](https://purrr.tidyverse.org/reference/map.html) or
their parallel counterparts (see [speeding up your
analysis](#speeding-up)).

``` r
# load imagefluency package
library(imagefluency)

# data frame to store results 
results <- data.frame(filename = basename(fileNames),
                      contrast = NA, selfsimilarity = NA, simplicity = NA,
                      symmetry = NA, typicality = NA)
```

### Looping over all images

Now it’s time to calculate the image fluency scores! To this end, we
create a big loop that iterates over all image files. Within the loop,
first, the image is read into R. Next the scores for contrast,
self-similarity, simplicity, and symmetry are computed for each image.
The results are stored in the `results` data frame we created before.

We use [`tryCatch()`](https://rdrr.io/r/base/conditions.html) in the
loop so that the loop does not break if there are any errors with a
specific image. Instead, the result is simply an `NA` if there is an
error. Note that we only compute vertical symmetry in the code below.
Optionally, you can also convert all images to grayscale to speed up the
computation.

``` r
# big loop over all images
for(i in seq_along(fileNames)){
  # print loop info
  cat('** Estimating scores for image',i,'**\n')
  
  # 1. read image into R
  img <- img_read(fileNames[i])
  
  # 2. convert image to grayscale to speed up computation (optional)
  # --NOTE: remove comment sign in the following line to apply grayscale conversion
  # img <- rgb2gray(img)
  
  # 3. estimate image fluency scores (except typicality)
  #    and store the results
  results$contrast[i] <- tryCatch(img_contrast(img), error = function(e) NA)
  results$selfsimilarity[i] <- tryCatch(img_self_similarity(img), error = function(e) NA)
  results$simplicity[i] <- tryCatch(img_simplicity(fileNames[i]), error = function(e) NA)
  results$symmetry[i] <- tryCatch(img_symmetry(img, horizontal = FALSE), error = function(e) NA)
}
```

    > ** Estimating scores for image 1 **
    > ** Estimating scores for image 2 **
    > ** Estimating scores for image 3 **
    > ** Estimating scores for image 4 **
    > ** Estimating scores for image 5 **
    > ** Estimating scores for image 6 **
    > ** Estimating scores for image 7 **
    > ** Estimating scores for image 8 **
    > ** Estimating scores for image 9 **
    > ** Estimating scores for image 10 **
    > ** Estimating scores for image 11 **

### Special case typicality

You might have noticed that we didn’t compute typicality in the code
above. The reason is that typicality is not a characteristic of a single
image itself, but can only be computed relative to a *set* of images.
Thus, we cannot use the loop construct from above. Instead, we read all
images at once into memory and compute typicality. Note that depending
on the number and size of the images, you might run into memory
problems. If that’s the case, you either have to use a computer with a
larger memory or reduce the image set accordingly.

``` r
# 4. get image typicality scores
# --NOTE: image typicality is estimated relative to all other images, hence,
#         the following might take quite a while
# 
# first read all images at once
# -- NOTE: if you have too many images, you might run into memory problems
allImages <- lapply(fileNames, img_read)

# now estimate typicality and store the results (only numeric vector without names)
results$typicality <- as.vector(img_typicality(allImages))
```

### Saving the results

All done! We have now computed all image fluency scores. Let’s have a
look at the results.

``` r
knitr::kable(results, digits = 3)
```

| filename         | contrast | selfsimilarity | simplicity | symmetry | typicality |
|:-----------------|---------:|---------------:|-----------:|---------:|-----------:|
| berries.jpg      |    0.287 |         -1.784 |      0.187 |    0.524 |      0.478 |
| bike.jpg         |    0.082 |         -0.159 |      0.399 |    0.419 |      0.234 |
| bridge.jpg       |    0.256 |         -1.112 |      0.234 |    0.480 |      0.437 |
| fireworks.jpg    |    0.175 |         -1.036 |      0.431 |    0.658 |     -0.130 |
| office.jpg       |    0.227 |         -1.217 |      0.188 |    0.171 |      0.347 |
| rails.jpg        |    0.364 |         -0.578 |      0.536 |    0.955 |      0.797 |
| romanesco.jpg    |    0.292 |         -0.032 |      0.363 |    0.825 |      0.281 |
| sky.jpg          |    0.081 |         -1.562 |      0.580 |    0.647 |      0.053 |
| trees.jpg        |    0.189 |         -0.551 |      0.105 |    0.191 |      0.281 |
| valley_green.jpg |    0.246 |         -0.507 |      0.252 |    0.803 |      0.633 |
| valley_white.jpg |    0.188 |         -0.590 |      0.191 |    0.601 |      0.579 |

If everything worked, we can save the results e.g. into a `.csv` file.

``` r
# save the results into the image folder
write.csv(results, file = paste0(mypath, 'imagefluency_scores.csv'), row.names = FALSE)
```

## Speeding up the analysis

If you plan to include typicality in your image analysis next to other
metrics, there is no need for the big loop from [before](#big-loop). The
reason is that you load all images into memory when computing typicality
anyway. Thus, we can also do this step right at the beginning of our
analysis and compute all image fluency scores from there.

### Reading all images into memory using lapply()

In the example below, I’ll use base R’s
[`lapply()`](https://rdrr.io/r/base/lapply.html) function to read all
images. However, you can of course use
[`purrr::map()`](https://purrr.tidyverse.org/reference/map.html) just as
well.

Using [`lapply()`](https://rdrr.io/r/base/lapply.html) is very simple:
We tell R to apply the
[`img_read()`](https://imagefluency.com/reference/img_read.md) function
to every element in the `fileNames` object. The result is a list where
every list element contains an image matrix. Thus, this object might be
very big!

``` r
# read all images at once into memory
allImages <- lapply(fileNames, img_read)
```

The above code works perfectly fine if every image can be read without
problems (which is usually the case). However, if we want to have a more
robust version, we can also define a custom
[`img_read()`](https://imagefluency.com/reference/img_read.md) function
that includes error handling with `tryCatch`.

``` r
# define custom image read function that catches errors and returns NA
img_read_no_error <- function(img) {
  tryCatch(img_read(img), error=function(e) NA)
}

# read images with custom function
allImages <- lapply(fileNames, img_read_no_error)
```

### Computing image fluency scores using lapply()

Once we have all images read into memory, we can compute the image
fluency scores for all images. To facilitate the process for contrast,
self-similarity, simplicity, and symmetry, we define a function that
computes the scores for a given image, and then apply the function to
all images.

``` r
# define function that computes all image fluency scores except typicality
img_fluency_scores <- function(img) {
  contr <- tryCatch(img_contrast(img), error = function(e) NA)
  selfsim <- tryCatch(img_self_similarity(img), error = function(e) NA)
  simpl <- tryCatch(img_simplicity(img), error = function(e) NA)
  sym <- tryCatch(img_symmetry(img, horizontal = FALSE), error = function(e) NA)
  return(c(contrast = contr, 
              selfsimilarity = selfsim,
              simplicity = simpl,
              symmetry = unname(sym)))
}

# apply function to images
results <- lapply(allImages, img_fluency_scores)
```

Once that’s done, we can convert the results into a data frame and add
the images’ file names.

``` r
# convert results to data frame and add file names
results <- do.call(rbind, results)
results <- data.frame(filename = basename(fileNames), results)
```

In the final step, we add the typicality scores as in the
[previous](#typ) section and look at the results.

``` r
# compute and add typicality to results
results$typicality <- as.vector(img_typicality(allImages))

# print a nicely formatted results table
knitr::kable(results, digits = 3)
```

| filename         | contrast | selfsimilarity | simplicity | symmetry | typicality |
|:-----------------|---------:|---------------:|-----------:|---------:|-----------:|
| berries.jpg      |    0.287 |         -1.784 |      0.187 |    0.524 |      0.478 |
| bike.jpg         |    0.082 |         -0.159 |      0.399 |    0.419 |      0.234 |
| bridge.jpg       |    0.256 |         -1.112 |      0.234 |    0.480 |      0.437 |
| fireworks.jpg    |    0.175 |         -1.036 |      0.431 |    0.658 |     -0.130 |
| office.jpg       |    0.227 |         -1.217 |      0.188 |    0.171 |      0.347 |
| rails.jpg        |    0.364 |         -0.578 |      0.536 |    0.955 |      0.797 |
| romanesco.jpg    |    0.292 |         -0.032 |      0.363 |    0.825 |      0.281 |
| sky.jpg          |    0.081 |         -1.562 |      0.580 |    0.647 |      0.053 |
| trees.jpg        |    0.189 |         -0.551 |      0.105 |    0.191 |      0.281 |
| valley_green.jpg |    0.246 |         -0.507 |      0.252 |    0.803 |      0.633 |
| valley_white.jpg |    0.188 |         -0.590 |      0.192 |    0.601 |      0.579 |

## Parallel processing with multiple cores

Above code using [`lapply()`](https://rdrr.io/r/base/lapply.html) should
be faster than the approach of using first a big loop and then again
reading in all images to compute typicality. However, we might gain a
much larger increase in speed, by using parallelized versions of
[`lapply()`](https://rdrr.io/r/base/lapply.html) or
[`purrr::map()`](https://purrr.tidyverse.org/reference/map.html),
respectively.

The following code example demonstrates shortly how to achieve this.
Essentially, all you have to do is to replace all instances of
[`lapply()`](https://rdrr.io/r/base/lapply.html) from before with
[`parallel::mclapply()`](https://rdrr.io/r/parallel/mclapply.html)
(*m*ulti-*c*ore lapply). In the example below, however, I’ll use
[`pbmcapply::pbmclapply`](https://rdrr.io/pkg/pbmcapply/man/pbmclapply.html)
which adds a progress bar (*pb*) to the multi-core lapply.

``` r
# detect cores for multi-core processing
ncores <- parallel::detectCores()

# read images with multi-core lapply and progress bar
allImages <- pbmcapply::pbmclapply(fileNames, img_read_no_error, mc.cores = ncores)

# compute image fluency scores 
results <- pbmcapply::pbmclapply(allImages, img_fluency_scores, mc.cores = ncores)

# convert results to data frame and add file names
results <- do.call(rbind, results)
results <- data.frame(filename = basename(fileNames), results)

# compute and add typicality to results
results$typicality <- as.vector(img_typicality(allImages))
```

Let’s do a quick comparison of the computing time when computing the
four image fluency scores with and without multi-core processing. Note
that with increasing file size or number of images, this speed bump will
become larger. We ‘simulate’ this by reading in the images 10 times.

``` r
# read images 10 times 
allImages <- lapply(rep(fileNames, 10), img_read_no_error)
cat('Number of images:', length(allImages), '\n')
```

    > Number of images: 110

``` r
# compute imgagefluency scores using lapply() with progress bar
tictoc::tic("lapply")
results_lapply <- pbapply::pblapply(allImages, img_fluency_scores)
tictoc::toc(log=TRUE)
```

```
> lapply: 8.73 sec elapsed
```

``` r
# compute imgagefluency scores using multi-core lapply() with progress bar
tictoc::tic("pbmclapply")
results_pbmclapply <- pbmcapply::pbmclapply(allImages, img_fluency_scores,
                                            mc.cores = ncores)
tictoc::toc(log=TRUE)
```

```
> pbmclapply: 2.44 sec elapsed
```

We can see that the multi-core approach is more than three times faster
(in this single test run) than the single-core approach! Let’s see how
[`furrr::future_map()`](https://furrr.futureverse.org/reference/future_map.html)
compares to this.

``` r
library(dplyr)
future::plan('multisession')

tictoc::tic('future_map')
results_future_map <- allImages %>% furrr::future_map(img_fluency_scores,
                                      .progress = TRUE,
                                      .options = furrr::furrr_options(seed = TRUE))
tictoc::toc(log=TRUE)
```

```
> future_map: 2.98 sec elapsed
```

Finally, we confirm that all results are the same.

``` r
all.equal(results_lapply, results_pbmclapply)
> [1] TRUE
all.equal(results_lapply, results_future_map)
> [1] TRUE
```

# Matrix or Array Rotation by 90 Degrees

Matrix or Array Rotation by 90 Degrees

## Usage

``` r
rotate90(img, direction = "positive")
```

## Arguments

- img:

  an array or a matrix

- direction:

  The direction of rotation by 90 degrees. The value can be `"positive"`
  (default) or `"negative"`. Aliases are `"counterclockwise"` and
  `"clockwise"`, respectively.

## Value

an array or a matrix (rotated by 90 degrees)

## Details

The function takes an array or matrix as input object (`img`) and
returns the object rotated by 90 degrees. Per default, the rotation is
done in the mathematically positive direction (i.e., counterclockwise).
Clockwise rotation (i.e., mathematically negative) can be specified by
passing the value `"negative"` to the `direction` argument.

## Examples

``` r
# sample matrix
img <- matrix(1:6, ncol = 2)
img
#>      [,1] [,2]
#> [1,]    1    4
#> [2,]    2    5
#> [3,]    3    6

rotate90(img) # counterclockwise
#>      [,1] [,2] [,3]
#> [1,]    4    5    6
#> [2,]    1    2    3
rotate90(img, direction = "negative") # clockwise
#>      [,1] [,2] [,3]
#> [1,]    3    2    1
#> [2,]    6    5    4
```

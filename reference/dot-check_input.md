# .check_input

`.check_input` is a helper function of the `rquantae` package that
checks whether the input is a matrix of numeric or integer values. Error
message are thrown if that is not the case.

## Usage

``` r
.check_input(img, f_call = NULL)
```

## Arguments

- img:

  An object that needs to checked.

- f_call:

  The name of the function inside which the `.check_input` is called.

## Value

An error message if the check fails.

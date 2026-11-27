# .check_flag

`.check_flag` is a helper function of the `imagefluency` package that
checks whether an optional parameter is a single logical value (`TRUE`
or `FALSE`). If that is not the case, a warning is thrown and the
default value is returned.

## Usage

``` r
.check_flag(
  x,
  name,
  default = FALSE,
  action = paste0("Automatically set to ", default, " ...")
)
```

## Arguments

- x:

  The value of the parameter.

- name:

  The name of the parameter.

- default:

  The default value that is used if the check fails.

- action:

  Character string. End of the warning message.

## Value

`x` if the check is passed, otherwise `default`.

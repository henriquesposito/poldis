# Extract text matches

Get texts in which certain "matches" occur.

## Usage

``` r
extract_match(v, match, invert = FALSE, ignore.case = TRUE)
```

## Arguments

- v:

  Text vector or annotated data frame.

- match:

  A regex match for a word(s) or expression. For multiple words, please
  use "\|" to divide them.

- invert:

  Do you want texts without certain matches to be returned? By default
  FALSE.

- ignore.case:

  Should case be ignored? By default, TRUE.

## Value

A list the same length as text variable.

## Examples

``` r
# \donttest{
extract_match(c("This function was created on the 29 September 2021",
"Today is October 12, 2021"), "October")
#> [[1]]
#> character(0)
#> 
#> [[2]]
#> [1] "Today is October 12, 2021"
#> 
# }
```

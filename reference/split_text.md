# Split texts

Split texts into structured lists of lists according to a split sign.

## Usage

``` r
split_text(v, splitsign = "\\.")
```

## Arguments

- v:

  Text vector or annotated data frame.

- splitsign:

  Where do you want to split? By default sentences ("."). This can also
  be words, signals or other markers you want. For special characters,
  please use escape sign before (i.e. "\\).

## Value

A list of lists the same length as vector.

## Examples

``` r
# \donttest{
split_text("This is the first sentence. This is the second sentence.")
#> [[1]]
#> [1] "This is the first sentence"   " This is the second sentence"
#> attr(,"Parts")
#> [1] "Number of parts = 2"
#> 
# }
```

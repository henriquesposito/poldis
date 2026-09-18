# Extract a list of possible names of individuals in texts

Extract a list of possible names of individuals in texts

## Usage

``` r
extract_names(v)
```

## Arguments

- v:

  A text vector.

## Value

A data frame of individual names and the number of times they appear.

## Details

The function relies on named entity recognition from NLP models.

## Examples

``` r
#extract_names("This package was created by Jael, James, and I.")
```

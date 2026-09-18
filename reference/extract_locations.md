# Extract locations from strings

Extract locations from strings

## Usage

``` r
extract_locations(v)
```

## Arguments

- v:

  Text vector.

## Value

A data frame of locations and the number of times they appear.

## Details

The function relies on geographical entity detection from NLP models.

## Examples

``` r
#extract_locations(c("This is the United States", "This is Sao Paulo",
#"I was in Rio de Janeiro and Sao Paulo, then back to the United States"))
```

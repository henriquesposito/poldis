# Extract first sentence from text

A lot of information is contained in the first sentence of a text. In
political texts, for example, dates and locations are often contained in
the first sentence of the text.

## Usage

``` r
extract_first_sentence(v)
```

## Arguments

- v:

  Text vector.

## Value

A list of the first sentences in text.

## Examples

``` r
extract_first_sentence("This is the first sentence. This is the second sentence.")
#> [1] "This is the first sentence."
```

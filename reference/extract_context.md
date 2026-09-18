# Extract context for string matches

A function for getting string matches and the context in which they
occur.

## Usage

``` r
extract_context(match, v, level = "sentences", n = 1)
```

## Arguments

- match:

  Character string to be matched. For multiple strings, please use "\|"
  as a separator.

- v:

  Text vector or annotated data frame.

- level:

  At which text level do you want matches to be returned? Defaults to
  "sentences". Options are sentences, words, and paragraph.

- n:

  Number of sentences or words matched before and after string match.
  Defaults to 1. That is, one word or one sentence before, and after,
  string match. For paragraphs, n is always set to one.

## Value

A list of string matches and their context.

## Examples

``` r
# \donttest{
extract_context(match = " war ",
                v = US_inaugural_addresses_1993_2025$text[1],
                level = "sentences", n = 2)
#> [[1]]
#> [1] ",On behalf of our Nation, I salute my predecessor, President Bush, for his half-century of service to America. And I thank the millions of men and women whose steadfastness and sacrifice triumphed over depression, fascism, and communism.,Today, a generation raised in the shadows of the cold war assumes new responsibilities in a world warmed by the sunshine of freedom but threatened still by ancient hatreds and new plagues. Raised in unrivaled prosperity, we inherit an economy that is still the world's strongest but is weakened by business failures, stagnant wages, increasing inequality, and deep divisions among our own people.,When George Washington first took the oath I have just sworn to uphold, news traveled slowly across the land by horseback and across the ocean by boat."
#> 
# }
```

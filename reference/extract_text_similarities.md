# Extract similarities and differences in texts/segments

Extract similarities and differences in texts/segments

## Usage

``` r
extract_text_similarities(v, comparison = "similarities", method)
```

## Arguments

- v:

  Text vector or annotated data frame.

- comparison:

  How would you like to compare texts? Options are "similarities", for
  comparing similarities, or "differences", for comparing differences.
  Defaults to "similarities".

- method:

  A method for checking similarities or differences between texts. For
  similarities, defaults to "correlation" method. Other methods for
  similarities include "cosine", "jaccard", "ejaccard", "dice", "edice",
  "simple matching", and "hamann". For differences, defaults to
  "euclidean". Other methods for differences include "manhattan",
  "maximum", "canberra", and "minkowski". For more information on each
  of these methods and what are the implications in selecting a method,
  please see \`?quanteda.textstats::textstat_simil()\`.

## Value

A matrix of similarity scores between texts.

## Examples

``` r
#extract_text_similarities(US_inaugural_addresses_1993_2025$text)
```

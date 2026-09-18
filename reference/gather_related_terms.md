# Gather terms related to subjects

Gather terms related to subjects

## Usage

``` r
gather_related_terms(v, dictionary)
```

## Arguments

- v:

  Text vector or annotated data frame.

- dictionary:

  The dictionary of 20 major political topics from the Comparative
  Agendas Project (Jones et al., 2023) is used by default. Users can
  also declare a custom dictionary as a vector or a list. If users
  declare a vector, each element is treated as a independent topic. If
  users declare a list of subjects and related terms, function
  understands names as topic and words as terms.

## Value

A list of related terms to each of the topics declared in dictionary.

## Details

This function relies on keyword assisted topic models implemented in the
\`{keyATM}\` package to find related words based on the topics provided
and texts in which they appear.

## References

Eshima S, Imai K, and Sasaki T. 2024. “Keyword-Assisted Topic Models.”
\_American Journal of Political Science\_, 68(2): 730-750.
[doi:10.1111/ajps.12779](https://doi.org/10.1111/ajps.12779)

## Examples

``` r
#gather_related_terms(US_inaugural_addresses_1993_2025$text, dictionary = "CAP")
#gather_related_terms(US_inaugural_addresses_1993_2025$text,
#                     dictionary = list("military" = c("military", "gun", "war"),
#                                       "development" = c("development", "interest rate", "banks")))
```

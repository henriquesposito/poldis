# Annotate text with NLP

This function relies on \`{spacyr}\` NLP parsing to annotate texts.

## Usage

``` r
annotate_text(v, level = "words")
```

## Arguments

- v:

  Text vector

- level:

  At which level would you like to parse the text? Options include
  "words" or "sentences". Defaults to "words".

## Value

A data frame with syntax information by words or sentences in text.

## Examples

``` r
#annotate_text(US_inaugural_addresses_1993_2025$text[2])
```

# Select future priorities from political discourses

Political priorities are statements in which actors express their intent
or commitment to take political action in the future.

## Usage

``` r
select_priorities(v, na.rm = TRUE)
```

## Arguments

- v:

  Text vector or annotated data frame.

- na.rm:

  Would you like political statements that do not contain a political
  action to be removed? By default, TRUE.

## Value

A data frame with syntax information by sentences and a variable
identifying which of these sentences are priorities.

## Examples

``` r
#select_priorities(US_inaugural_addresses_1993_2025$text)
```

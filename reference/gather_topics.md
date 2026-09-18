# Gather topic from political discourses

Gather topic from political discourses

## Usage

``` r
gather_topics(v, dictionary = "CAP")
```

## Arguments

- v:

  Text vector or annotated data frame. If missing, opens the political
  topics codebook.

- dictionary:

  The dictionary of 20 major political topics from the Comparative
  Agendas Project (Jones et al., 2023) is used by default. Users can
  also declare a custom dictionary as a vector or a list. If users
  declare a vector, each element is treated as a independent topic. If
  users declare a list of subjects and related terms, function
  understands names as topic and words as terms. For more information on
  how the CAP topics were adapted, please run \`gather_topics()\` to
  access the political topics codebook.

## Value

A list of topics present in each text separated by comma.

## Examples

``` r
# \donttest{
summary(gather_topics(US_inaugural_addresses_1993_2025$text))
#> 
#>                                          Education 
#>                                                  9 
#>                              Government Operations 
#>                                                  9 
#>                                        Immigration 
#>                                                  9 
#>                               Labor and Employment 
#>                                                  9 
#>                      Law  Crime  and Family Issues 
#>                                                  9 
#> Civil Rights  Minority Issues  and Civil Liberties 
#>                                                  8 
#>                                            Defense 
#>                                                  8 
#>                                             Health 
#>                                                  8 
#>                                     Macroeconomics 
#>                                                  8 
#>              International Affairs and Foreign Aid 
#>                                                  7 
#>            Banking  Finance  and Domestic Commerce 
#>                                                  5 
#>                     Environment and Climate Change 
#>                                                  5 
#>                                     Social Welfare 
#>                                                  5 
#>                                     Transportation 
#>                                                  5 
#>           Community Development and Housing Issues 
#>                                                  4 
#>                                      Foreign Trade 
#>                                                  4 
#>     Space  Science  Technology  and Communications 
#>                                                  4 
#>                                        Agriculture 
#>                                                  3 
#>                  Public Lands and Water Management 
#>                                                  3 
#>                                             Energy 
#>                                                  2 
# }
```

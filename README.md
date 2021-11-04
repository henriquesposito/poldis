
# poldis <img src="inst/poldishexlogo.png" align="right" width="220"/>

<!-- badges: start -->

![GitHub release (latest by
date)](https://img.shields.io/github/v/release/henriquesposito/poldis)
![GitHub
issues](https://img.shields.io/github/issues-raw/henriquesposito/poldis)
<!-- badges: end -->

`{poldis}` provides text data and tools for analysing on political
discourses in time with R.

Political discourse data comes in many types. While it is easier to
analyse data from official speeches, where there is one speaker, with
the available R packages, this only tells a portion of the story of
what, how, and where politics gets done. For most other settings in
which political discourse appears as debates, interviews, news
conferences, or campaign rallies, where there may be multiple speakers,
working with large amounts of text programmatically becomes tricky (to
say the least). For example, large amounts of text might need to be
separated by speakers, or have questions removed for analysis, or only
the context of a string matches is wanted… It is exactly these tasks
`{poldis}` aims at making easier.

A lot more is yet to come, keep your eyes open for it, but I hope the
first few functions available are helpful to you\!

To download the latest version of ´{poldis}´ from GitHub and use it
locally you can use:

``` r
# install.packages("remotes")
remotes::install_github("henriquesposito/poldis")
```

Lastly, if you are using ´{poldis}´, please do not forget to cite us ;)

``` r
citation("poldis")
```

    ## 
    ## To cite poldis in publications use:
    ## 
    ##   H. Sposito. poldis: Tools for analyzing political discourse. 2021.
    ## 
    ## A BibTeX entry for LaTeX users is
    ## 
    ##   @Manual{,
    ##     title = {poldis: Tools for analyzing political discourse},
    ##     author = {Henrique Sposito},
    ##     year = {2021},
    ##     url = {https://github.com/henriquesposito/poldis},
    ##   }

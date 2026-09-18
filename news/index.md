# Changelog

## poldis 0.2.0

2026-09-17

### Package

- Added a US presidential speeches vignette presenting a case study with
  plots
- Improved documentation and examples across exported functions
- Improved README to be shorter and clearer
- Fixed tests to skip gracefully when spaCy (Python) is unavailable,
  rather than erroring
- Removed ORCID field from DESCRIPTION to avoid CRAN submission issues

### Functions

- Renamed `extract_title()` to
  [`extract_first_sentence()`](https://henriquesposito.github.io/poldis/reference/extract_first_sentence.md)
  to be more descriptive
- Removed `extract_date()`
- Removed `sim_urgency()` for now, as it does not work well with the
  updated
  [`get_urgency()`](https://henriquesposito.github.io/poldis/reference/get_urgency.md)
- Improved
  [`annotate_text()`](https://henriquesposito.github.io/poldis/reference/annotate_text.md)
  to parse large text vectors in chunks, avoiding memory issues and R
  crashes
- Improved
  [`annotate_text()`](https://henriquesposito.github.io/poldis/reference/annotate_text.md)
  to initialise the spaCy backend only once, increasing efficiency
- Improved
  [`get_urgency()`](https://henriquesposito.github.io/poldis/reference/get_urgency.md)
  to score from word stems instead of lemmas, increasing accuracy and
  speed
- Improved
  [`get_urgency()`](https://henriquesposito.github.io/poldis/reference/get_urgency.md)
  scoring so that the urgency score is the sum of the other dimensions
  divided by the “must” score
- Added a “min” option to the `summarise` argument of
  [`get_urgency()`](https://henriquesposito.github.io/poldis/reference/get_urgency.md)
- Fixed
  [`get_urgency()`](https://henriquesposito.github.io/poldis/reference/get_urgency.md)
  to return 0s, rather than NAs, when scores are genuinely zero
- Fixed
  [`get_urgency()`](https://henriquesposito.github.io/poldis/reference/get_urgency.md)
  where spacing was being replaced by dots
- Improved
  [`gather_topics()`](https://henriquesposito.github.io/poldis/reference/gather_topics.md)
  to match on word stems instead of lemmas, increasing accuracy and
  speed
- Fixed a bug in priority identification in
  [`select_priorities()`](https://henriquesposito.github.io/poldis/reference/select_priorities.md)
- Improved
  [`read_pdf()`](https://henriquesposito.github.io/poldis/reference/read_pdf.md)
  detection of non-readable PDFs

### Data

- Renamed and replaced `US_News_Conferences_1960_1980` with
  `US_inaugural_addresses_1993_2025`
- Improved internal data to a single dataset with scores from the second
  urgency survey
- Removed the urgency codebook (please refer to the paper for details)
- Moved raw replication data to a separate `data` branch to reduce
  package size

## poldis 0.1.2

CRAN release: 2024-09-04

2024-09-03

### Package

- Updated documentation for
  [`get_urgency()`](https://henriquesposito.github.io/poldis/reference/get_urgency.md)
  to include more details about urgency scores
- Updated internal data and data raw files to increase transparency

### Functions

- Updated
  [`get_urgency()`](https://henriquesposito.github.io/poldis/reference/get_urgency.md)
  and
  [`gather_topics()`](https://henriquesposito.github.io/poldis/reference/gather_topics.md)
  to open codebooks for urgency or topic when no arguments are declared
- Updated urgency scores in
  [`get_urgency()`](https://henriquesposito.github.io/poldis/reference/get_urgency.md)
  according to survey results.
- Added `sim_urgency()` function to simulate urgent priorities
- Updated
  [`select_priorities()`](https://henriquesposito.github.io/poldis/reference/select_priorities.md)
  to be more strict and accurate about selection of priorities

## poldis 0.1.1

CRAN release: 2024-07-21

2024-07-17

### Package

- Removed redundant terms from description for CRAN submission
- Removed commented out code lines and examples in documentation for
  CRAN submission

## poldis 0.1.0

2024-07-15

### Package

- Closed [\#3](https://github.com/henriquesposito/poldis/issues/3) by
  adding code coverage and code factor (and badges) to package
- Closed [\#7](https://github.com/henriquesposito/poldis/issues/7) by
  adding a getting started section in README
- Closed [\#8](https://github.com/henriquesposito/poldis/issues/8) by
  adding a [pkgdown](https://pkgdown.r-lib.org/) website
- Added GitHub workflow actions

### Functions

- Updated text tools
  - Renamed text tools functions to start with “extract\_”
    ([`extract_names()`](https://henriquesposito.github.io/poldis/reference/extract_names.md),
    `extract_title()`,
    [`extract_context()`](https://henriquesposito.github.io/poldis/reference/extract_context.md),
    `extract_date()`,
    [`extract_locations()`](https://henriquesposito.github.io/poldis/reference/extract_locations.md),
    [`extract_match()`](https://henriquesposito.github.io/poldis/reference/extract_match.md))
  - Closed [\#11](https://github.com/henriquesposito/poldis/issues/11)
    by adding `extract_similarities()` function to fuzzy match texts
  - Added
    [`annotate_text()`](https://henriquesposito.github.io/poldis/reference/annotate_text.md)
    function to parse words or sentences using NLP
  - Added
    [`read_pdf()`](https://henriquesposito.github.io/poldis/reference/read_pdf.md)
    function to load readable and non-readable text files from PDFs
- Closed [\#14](https://github.com/henriquesposito/poldis/issues/14) by
  adding `select_promises()` function to extract future promises in
  text  
- Closed [\#15](https://github.com/henriquesposito/poldis/issues/15) by
  adding
  [`gather_topics()`](https://henriquesposito.github.io/poldis/reference/gather_topics.md)
  and
  [`gather_related_terms()`](https://henriquesposito.github.io/poldis/reference/gather_related_terms.md)
  for assigning topics to texts
- Added
  [`get_urgency()`](https://henriquesposito.github.io/poldis/reference/get_urgency.md)
  function for coding urgency from text
- Added summary and plotting methods for “urgency” and “topics” classes

## poldis 0.0.3

2022-09-25

### Package

- Renamed all functions to consistently start with `extract_*`
- Made the package smaller and more concise by removing
  `extract_dates()` and additional commented codes
- Closed [\#6](https://github.com/henriquesposito/poldis/issues/6) by
  updating
  [`extract_context()`](https://henriquesposito.github.io/poldis/reference/extract_context.md)
  function to make it more flexible for users
- Updated `extract_speaker()` to only return a list of speakers for
  texts
- Closed [\#4](https://github.com/henriquesposito/poldis/issues/4) and
  [\#5](https://github.com/henriquesposito/poldis/issues/5) by removing
  `str_translate()` function for translating strings

## poldis 0.0.2

2022-02-07

### Package

- Updated location regex dictionary to improve matching
- Updated package documentation
- Added `str_translate()` function for translating strings

## poldis 0.0.1

2021-11-04

### Package

- Added functions to work with text
  - Added `extract_location()` to extract locations from text variables
  - Added `extract_date()` to extract dates from text variables
  - Added `extract_title()` to extract titles from text variables
  - Added
    [`split_text()`](https://henriquesposito.github.io/poldis/reference/split_text.md)
    to split text variables
  - Added `text_match()` to get matches from text variables
- Added `context()` function to get string matches and return their
  context
- Added `get_speaker()` function for splitting text variables by
  speakers
- Made package public on GitHub

### Data

- Added US News Conference Sample Data to be used with examples and
  teaching

## poldis 0.0.0

2021-03-02

### Package

- Created package and GitHub repo
- Setup package folder structure
  - Added `DESCRIPTION` file
  - Added `R` folder
  - Added `LICENSE` file
  - Added `NAMESPACE` file
  - Added `NEWS` file
  - Added `README` file
  - Added `.github` folder
  - Added `CODE_OF_CONDUCT` file
  - Added `CONTRIBUTING` file
  - Added `pull_request_template` file
  - Added `ISSUE_TEMPLATE` folder
  - Added `bug_report` file
  - Added `feature_request` file
  - Added `tests` folder
  - Added `testthat` folder
  - Added `testthat` file
  - Added `inst` folder
  - Added `data-raw` folder
  - Added `data` folder
- Added package logo

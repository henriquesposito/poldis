# poldis 0.1.1

2024-07-17

## Package

- Removed redundant terms from description for CRAN submission
- Removed commented out code lines and examples in documentation for CRAN submission

# poldis 0.1.0

2024-07-15

## Package

- Closed #3 by adding code coverage and code factor (and badges) to package
- Closed #7 by adding a getting started section in README
- Closed #8 by adding a `{pkgdown}` website
- Added GitHub workflow actions

## Functions

- Updated text tools
  - Renamed text tools functions to start with "extract_" (`extract_names()`, `extract_title()`, `extract_context()`, `extract_date()`, `extract_locations()`, `extract_match()`)
  - Closed #11 by adding `extract_similarities()` function to fuzzy match texts
  - Added `annotate_text()` function to parse words or sentences using NLP
  - Added `read_pdf()` function to load readable and non-readable text files from PDFs
- Closed #14 by adding `select_promises()` function to extract future promises in text  
- Closed #15 by adding `gather_topics()` and `gather_related_terms()` for assigning topics to texts
- Added `get_urgency()` function for coding urgency from text
- Added summary and plotting methods for "urgency" and "topics" classes

# poldis 0.0.3

2022-09-25

## Package

* Renamed all functions to consistently start with `extract_*`
* Made the package smaller and more concise by removing `extract_dates()` and additional commented codes
* Closed #6 by updating `extract_context()` function to make it more flexible for users
* Updated `extract_speaker()` to only return a list of speakers for texts
* Closed #4 and #5 by removing `str_translate()` function for translating strings

# poldis 0.0.2

2022-02-07

## Package

* Updated location regex dictionary to improve matching
* Updated package documentation
* Added `str_translate()` function for translating strings

# poldis 0.0.1

2021-11-04

## Package

* Added functions to work with text
  * Added `extract_location()` to extract locations from text variables
  * Added `extract_date()` to extract dates from text variables
  * Added `extract_title()` to extract titles from text variables
  * Added `split_text()` to split text variables
  * Added `text_match()` to get matches from text variables
* Added `context()` function to get string matches and return their context
* Added `get_speaker()` function for splitting text variables by speakers
* Made package public on GitHub

## Data

* Added US News Conference Sample Data to be used with examples and teaching

# poldis 0.0.0

2021-03-02

## Package

* Created package and GitHub repo
* Setup package folder structure
  * Added `DESCRIPTION` file
  * Added `R` folder
  * Added `LICENSE` file
  * Added `NAMESPACE` file
  * Added `NEWS` file
  * Added `README` file
  * Added `.github` folder
  * Added `CODE_OF_CONDUCT` file
  * Added `CONTRIBUTING` file
  * Added `pull_request_template` file
  * Added `ISSUE_TEMPLATE` folder
  * Added `bug_report` file
  * Added `feature_request` file
  * Added `tests` folder
  * Added `testthat` folder
  * Added `testthat` file
  * Added `inst` folder
  * Added `data-raw` folder
  * Added `data` folder
* Added package logo

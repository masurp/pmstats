# pmstats - Convenient Functions for Statistical Transformations, Result Extraction, and Reporting

This package includes some custom-made functions to facilitate some common statistical procedures as well as extracting and reporting results from various models in RMarkdown articles. Please note that most functions are highly costumized to my own workflow. They may hence break in more general frameworks or when used in a different way...

## Dependencies

Most functions require the following packages:

- For data wrangling procedure: `tidyverse`
- For printing and typesetting: `papaja`
- For SEM procedures: `lavaan` and `semTools`

This packages should be installed prior to using this package. 


## Installation

To install the package and latest developments, type the following commands into the R console:

```r
library(devtools)
devtools::install_github("masurp/pmstats")
```
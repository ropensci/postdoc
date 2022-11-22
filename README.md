# postdoc

> Simple and Uncluttered Package Documentation

## Installation

You can install the development version of postdoc from r-universe:

```r
# Download and install postdoc in R
install.packages('postdoc', 
  repos = c('https://ropensci.r-universe.dev','https://cloud.r-project.org'))
```

## Example

Render the manual for the 'MASS' package:

```r
library(postdoc)
htmlfile <- render_package_manual(package = 'MASS')
utils::browseURL(htmlfile)
```

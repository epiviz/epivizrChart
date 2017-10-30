# epivizrChart

[![Build Status](https://travis-ci.org/epiviz/epivizrChart.svg?branch=travis)](https://travis-ci.org/epiviz/epivizrChart)

Add interactive charts and dashboards for genomic data visualization into RMarkdown and HTML documents using the [epiviz](http://epiviz.org) framework. The `epivizrChart` package provides an API to interactively create and manage web components that encapsulate epiviz charts. Charts can be embedded in R markdown/notebooks to create interactive documents. Epiviz Web components are built using the Google [Polymer](https://www.polymer-project.org/) library. 

## Installation and requirements

```{r}
# using Bioconductor
source("http://bioconductor.org/biocLite.R")
biocLite("epiviz/epivizrChart")

# or using devtools
library(devtools)
install_github("epiviz/epivizrChart")
```

## Generating interactive Markdown Documents

The easiest way to try `epivizrChart` is to knit the package vignette

```
require(epivizrChart)
browseVignettes("epivizrChart")
```

## Resources

- Documentation on the Epiviz web component library can be found here: https://epiviz.github.io/polymer/charts/components/epiviz-charts/

- The repository for the Epiviz web component library itself is here: https://github.com/epiviz/epiviz-chart


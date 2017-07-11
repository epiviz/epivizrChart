# epivizrChart

`EpivizrChart` package provides an API to interactively create and manage epiviz web components. Data objects from R/Bioconductor can be visualized as tracks/plots using `EpivizChart`. Charts can be embedded in R markdown/notebooks to create interactive documents. Epiviz Web components are built using the google polymer library. 

# Installation and requirements

```{r}
# using Bioconductor
source("http://bioconductor.org/biocLite.R")
biocLite("epiviz/epivizrChart")

# or using devtools
library(devtools)
install_github("epiviz/epivizrChart")
```

# Generating interactive Markdown Documents

The easiest way to try `epivizrChart` is to knit the package vignette

```
require(epivizrChart)
browseVignettes("epivizrChart")
```

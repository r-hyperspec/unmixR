# `unmixR`: An R Package for Unmixing of Hyperspectral Images

<!-- badges: start -->
[![R-CMD-check](https://github.com/r-hyperspec/unmixR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/r-hyperspec/unmixR/actions/workflows/R-CMD-check.yaml)
[![Test coverage](https://github.com/r-hyperspec/unmixR/actions/workflows/test-coverage.yaml/badge.svg)](https://github.com/r-hyperspec/unmixR/actions/workflows/test-coverage.yaml)
[![Codecov test coverage](https://codecov.io/gh/r-hyperspec/unmixR/branch/develop/graph/badge.svg)](https://app.codecov.io/gh/r-hyperspec/unmixR?branch=develop)
[![Website (pkgdown)](https://github.com/r-hyperspec/unmixR/actions/workflows/pkgdown.yaml/badge.svg)](https://github.com/r-hyperspec/unmixR/actions/workflows/pkgdown.yaml)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![Project Status: WIP – Initial development is in progress, but there has not yet been a stable, usable release suitable for the public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
<!-- badges: end -->

> [!NOTE]
> This `README` is for GitHub only. `pkgdown` website currently has a separate `README`.

> [!WARNING]
> `unmixR` is **WORK IN PROGRESS**. The fundamental structures & behavior may change. For the time being, use at your own risk.

Hyperspectral data are also called 'imaging spectroscopy' and 'imaging spectrometer data' depending upon the discipline.
Such data consists of spectra collected over an x, y grid.
Data sets like this are found in airborne land imaging studies, biomedical studies and art history investigations.
The spectra are often visible, infrared, near-infrared, raman spectra or mass spectrometer data sets.


## Installation

**Installation:** works easiest using `remotes::install_git()`:

```r
library("remotes")

# Install latest version from GitHub
remotes::install_github("r-hyperspec/unmixR", subdir = "pkg/unmixR")

# Install a specific version from GitHub
remotes::install_github("r-hyperspec/unmixR", subdir = "pkg/unmixR", branch="v2.0")
```

> [!NOTE]
> Version tagged as `v1.0` is the last stable version prior to significant changes. This tag was added for users who prefer to use old version. However, the tag version `v1.0` **does not match the version in DESCRIPION** (see more on this in the [issue](https://github.com/r-hyperspec/unmixR/issues/54)). Starting from `v2.0` the version in DESCRIPTION will match the tag version.


## Example

```r
library(unmixR)

# Load demo data
data("demo_data")

# Reduce data dimensionality with PCA
pca <- prcomp(demo_data)
x <- pca$x[,1:2]

# Perform N-FINDR in reduced space
nf <- nfindr(x, p = 3)

# Get endmembers in original space
ems <- endmembers(nf, demo_data)

# Get endmembers in reduced space
ems_pca <- endmembers(nf, x)

# Calculate abundances using barycentric coordinates
# it works only in the reduced space
ab_bary <- abundances(ems_pca, x, method = "bary")
# Same as: bary(ems_pca, x)

# Calculate abundances using NNLS
# it works in both spaces but it is better to be applied in the original space
ab_nnls <- abundances(ems, demo_data, method = "nnls", normalize = TRUE)
# Same as: nnls(ems, demo_data)/rowSums(nnls(ems, demo_data))
```

## Acknoledgements

Initial development of `unmixR` has been supported by Google Summer of Code 2013 (Conor McManus) and 2016 (Anton Belov). 
Thank you Google!

This project has received funding from the European Union's [Horizon 2020](https://research-and-innovation.ec.europa.eu/funding/funding-opportunities/funding-programmes-and-open-calls/horizon-2020_en) research and innovation programme under the [Marie Sklodowska-Curie Actions](https://marie-sklodowska-curie-actions.ec.europa.eu/) *(Grant Agreement 861122)* as part of [IMAGE-IN](https://image-in-itn.eu/) project.

![GSOC 2016 logo](https://gitlab.com/chemometrics/unmixR/raw/master/GSoC2016Logo.png)
<img src="https://bgsmath.cat/wp-content/uploads/2017/09/marie_curie1-300x160.jpg" alt="Horizon 2020"  style="height: 100px; margin: 10px"/>


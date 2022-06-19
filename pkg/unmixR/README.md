# `unmixR`: An R Package for Unmixing of Hyperspectral Images

<!-- badges: start -->
[![R-CMD-check](https://github.com/r-hyperspec/unmixR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/r-hyperspec/unmixR/actions/workflows/R-CMD-check.yaml)
[![Test coverage](https://github.com/r-hyperspec/unmixR/actions/workflows/test-coverage.yaml/badge.svg)](https://github.com/r-hyperspec/unmixR/actions/workflows/test-coverage.yaml)
[![Codecov test coverage](https://codecov.io/gh/r-hyperspec/unmixR/branch/develop/graph/badge.svg)](https://app.codecov.io/gh/r-hyperspec/unmixR?branch=develop)
[![Website (pkgdown)](https://github.com/r-hyperspec/unmixR/actions/workflows/pkgdown.yaml/badge.svg)](https://github.com/r-hyperspec/unmixR/actions/workflows/pkgdown.yaml)
<!-- badges: end -->



<center>
`unmixR` is **WORK IN PROGRESS**.
</center>
<br>

The fundamental structures & behavior may change.
For the time being, use at your own risk.

Hyperspectral data are also called 'imaging spectroscopy' and 'imaging spectrometer data' depending upon the discipline.
Such data consists of spectra collected over an x, y grid.
Data sets like this are found in airborne land imaging studies, biomedical studies and art history investigations.
The spectra are often visible, infrared, near-infrared, raman spectra or mass spectrometer data sets.


## Installation

**Installation:** works easiest using `remotes::install_git()`:

```r
require("remotes")
install_git("https://gitlab.com/chemometrics/unmixR.git", subdir = "pkg/unmixR")
```

## Acknoledgements

Development of `unmixR` has been supported by Google Summer of Code 2013 (Conor McManus) and 2016 (Anton Belov). 
Thank you Google!

![GSOC 2016 logo](https://gitlab.com/chemometrics/unmixR/raw/master/GSoC2016Logo.png)

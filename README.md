### unmixR
#### An R package for unmixing of hyperspectral images

`unmixR` is **WORK IN PROGRESS**.  The fundamental structures & behavior may change. For the time being, use at your own risk.

Hyperspectral data are also called 'imaging spectroscopy' and 'imaging spectrometer data' depending upon the discipline.  Such data consists of spectra collected over an x, y grid.  Data sets like this are found in airborne land imaging studies, biomedical studies and art history investigations.  The spectra are often visible, infrared, near-infrared, raman spectra or mass spectrometer data sets.

**Installation:** works easiest using `devtools::install_git`:

```r
require ("devtools")
install_git ("https://gitlab.com/chemometrics/unmixR.git", subdir = "pkg/unmixR")
```

Development of `unmixR` has been supported by Google Summer of Code 2013 (Conor McManus) and 2016 (Anton Belov).  Thank you Google!

![GSOC 2016 logo](https://gitlab.com/chemometrics/unmixR/blob/master/GSoC2016Logo.png)

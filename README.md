### unmixR
#### An R package for unmixing of hyperspectral images

`unmixR` is **WORK IN PROGRESS**.  The fundamental structures & behavior may change frequently. Use at your own risk.

**Installation:** works easiest using `devtools::install_git`:

```r
require ("devtools")
install_git ("https://gitlab.com/chemometrics/unmixR.git", subdir = "pkg/unmixR")
```

`unmixR` was supported by [Google Summer of Code 2013](http://www.google-melange.com/gsoc/homepage/google/gsoc2013).  Thank you!

Note: hyperspectral data are also called 'imaging spectroscopy' and 'imaging spectrometer data' depending upon the discipline.  Such data consists of spectra collected over an x, y grid.  Data sets like this are found in airborne land imaging studies, biomedical studies and art history investigations.  The spectra are often visible, infrared, near-infrared, raman spectra or mass spectrometer data sets.

#### Things to do and Things to remember + Misc Notes

An informal list to keep us on track.

##### Top priority
* Review unit tests which currently fail.
* Class definition(s): `nfindr` currently informally defined at end of nfindr.default.  Do we need/want to create an Rd page about it?
* Similar question for class vca.  It is not different from class nfindr, maybe they should both return class 'unmixR'?
* Work on vignettes
* Review all documentation generally.
* Add examples to the documentation.

##### Lower priority

* methods for finding the number of endmembers (recommended in Harsayni2012)
    * hfc (hfcVAR and hfcSAMPLE need discussion and testing)
    * nwhfc
    * sml (second moment linear)
    * Hysime (hyperspectral signal subspace identification by minimum error)
* methods that don't assume a pure pixel is present
    * MVT methods...
    
##### Misc notes



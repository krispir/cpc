# The cpc package

The purpose of the cpc package is primarily to characterize the peaks detected using a separate peak detection method. Currently the only supported preprocessing software is XCMS.

# Installation instructions

To install the package, you need a suitable version of Rtools installed in order to compile the package. For installation instructions for Rtools, please refer to [https://cran.r-project.org/bin/windows/Rtools/](https://cran.r-project.org/bin/windows/Rtools/).

Ensure that the latest version of XCMS (>3.13.5) is installed from  [https://bioconductor.org/packages/release/bioc/html/xcms.html](https://bioconductor.org/packages/release/bioc/html/xcms.html).

The cpc package can be installed from this github using the devtools function install_github(). Run the code in the example below to install the package. Note that it is important that build_vignettes is set to TRUE in order for the vignette to be compiled during installation.

```r
if(!require("devtools")) install.package("devtools")
if(!require("rmarkdown")) install.package("rmarkdown")
if(!require("signal")) install.package("signal")

devtools::install_github(repo = "krispir/cpc", 
                         build_vignettes = TRUE)
```

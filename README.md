# The cpc package

The purpose of the cpc package is primarily to characterize the peaks detected using a separate peak detection method. The peak characteristics can then be used to assess the quality of the reported peaks and filter those that subseed user defined thresholds. The cpc package is fully integrated into a normal XCMS workflow and takes an *XCMSnExp* object containing peak information generated using XCMS as argument.

# Installation instructions

To install the package, you need a suitable version of Rtools installed in order to compile the package. For installation instructions for Rtools, please refer to [https://cran.r-project.org/bin/windows/Rtools/](https://cran.r-project.org/bin/windows/Rtools/).

Ensure that the latest version of XCMS (>3.13.5) is installed from  [https://bioconductor.org/packages/release/bioc/html/xcms.html](https://bioconductor.org/packages/release/bioc/html/xcms.html).

The cpc package can be installed from this github using the devtools function install_github(). Run the code in the example below to install the package. Note that it is important that build_vignettes is set to TRUE in order for the vignette to be compiled during installation.

```r
if(!require("remotes")) install.package("remotes")
if(!require("rmarkdown")) install.package("rmarkdown")
if(!require("signal")) install.package("signal")

remotes::install_github(repo = "krispir/cpc", 
                         build_vignettes = TRUE)
```

# Usage

In addition to the normal documentation that can be accessed using the ? notation in R (e.g. ?filter_xcms_peaklist) there is a vignette included that processes the example data provided with the package. A pre-knitted vignette (cpc.pdf) is available in the inst/ folder or via the link [https://github.com/krispir/cpc/tree/master/inst](https://github.com/krispir/cpc/tree/master/inst). The vignette can also be accessed within R using the command vignette("cpc") if the package was installed with the argument *build_vignette = TRUE*.

The example data included with the package was prepared from real study data available for download via MetaboLights with the unique identifier MTBLS2418 [https://www.ebi.ac.uk/metabolights/MTBLS2418](https://www.ebi.ac.uk/metabolights/MTBLS2418).
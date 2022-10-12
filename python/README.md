# mcceRpy

<!-- badges: start -->
<!-- badges: end -->

mcceRpy is a Python wrapper package for the R-package mcceR. 

## Installation

Being a python wrapper for an R-package, this package requires installation of both R and quite a few R-packages to function.
Below is a full description on how to do all of this within a conda environment without causing conflicts between conda and pip.
We recommend running this package on Linux, as the Python -> R bridge works best there, see https://github.com/rpy2/rpy2.
The below instructions show how to set up a new conda environment with everything you need from a Linux terminal.

### Download and install miniconda (if not already installed)
Download Miniconda from here: https://docs.conda.io/en/latest/miniconda.html#linux-installers

```
bash Miniconda3-latest-Linux-x86_64.sh # install miniconda
```
Check +update conda installation
```
conda --version
conda update conda # Updates conda (Answer y)
```

Set up conda environment with Python version X.Y.Z (3.5.0+ is required, see https://rpy2.github.io/doc/v3.0.x/html/overview.html#requirements)
```
conda create -n "TESTmcceRpy" python=3.10.7 ipython
```
CONTINUE HERE!!!!


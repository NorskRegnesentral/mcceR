# mcceRpy

<!-- badges: start -->
<!-- badges: end -->

mcceRpy is a Python wrapper package for the R-package mcceR, built using the rpy2 python library (https://github.com/rpy2/rpy2)

## Installation

Being a python wrapper for an R-package, this package requires installation of both R and quite a few R-packages to function.

There are several ways of installing R, depending on your system. 
Official instructions can be found her (https://cran.r-project.org/)
R can also be installed with pip as follows:
```
pip install rbase
```
and conda:
```
conda install -c r r
```

Once R is installed, you need to install the ´mcceR´ R-package, in addition to quite a few more common R-packages. From the folder of this file, these can be installed as

```
Rscript install_r_packages.R
```





Below is a brief description.
We recommend running this package on Linux, as the Python -> R bridge works best there, see https://github.com/rpy2/rpy2.
The below instructions show how to set up a new conda environment with everything you need from a Linux terminal.

### Download and install miniconda (if not already installed)
Download Miniconda from here: https://docs.conda.io/en/latest/miniconda.html#latest-miniconda-installer-links

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
conda create -n "TESTmcceRpy" python=3.7.15 ipython
```

Activate the conda environment
```
conda activate TESTmcceRpy
```

Install conda-forge and other necessary python packages
```
conda install -c r r

conda install -c conda-forge rpy2 pandas numpy
```


CONTINUE HERE!!!!


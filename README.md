# Randomized Pedigree Principal Component Analysis

Randomized Pedigree Principal Component Analysis (rpPCA) performs principal component analysis (PCA) of pedigree-based genetic relatedness matrix (GRM) using randomized linear algebra.
[Henderson (1975)](https://doi.org/10.3168/jds.S0022-0302(75)84776-X) developed an efficient way to compute the lower Cholesky factor of the inverse GRM.
rpPCA uses this sparse Cholesky factor to compute the principal components that reveals the underlying population structure of the sample without setting up the GRM.
This approach enables PCA for populations with large pedigrees.


# R package

## Setup
To install the package from GitHub (it's not yet on CRAN), run:
```
devtools::install_github("HighlanderLab/RandPedigreePCA", subdir = "randPedPCA",
                         ref="v0.9.8", build_vignettes = T)
```
## First steps
For a demonstration, check out
```
vignette("pedigree-pca")
```

# Python
An example can be found in `notebook/Example.ipynb`.

# sparsevectors

The **sparsevectors** package provides a simple implementation of sparse
vectors in R.  
A sparse vector is a numeric vector where most entries are zero; instead
of storing all values, this package stores only the non-zero entries and
their indices.  
This saves memory and improves performance for large, mostly-zero
vectors.

## Installation

You can install the development version of sparsevectors from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("mj307/sparsevectors")
```

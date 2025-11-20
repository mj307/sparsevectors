# Element-wise Addition for sparse_numeric Vectors

Defines a generic and method to add two sparse vectors without
converting them to dense format.

## Usage

``` r
sparse_add(x, y)

# S4 method for class 'sparse_numeric,sparse_numeric'
sparse_add(x, y)
```

## Arguments

- x:

  One sparse vector.

- y:

  Another sparse vector.

## Value

A `sparse_numeric` containing the element-wise sum.

## Methods (by class)

- `sparse_add(x = sparse_numeric, y = sparse_numeric)`: Add two sparse
  vectors.

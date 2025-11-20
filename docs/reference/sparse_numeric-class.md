# Sparse Numeric Vector Class

An S4 class used to represent a numeric vector that contains many zeros.
Only the nonzero entries and their locations are stored.

## Value

A new `sparse_numeric` object.

## Slots

- `value`:

  A numeric vector containing all nonzero elements.

- `pos`:

  An integer vector with the positions of nonzero values.

- `length`:

  An integer giving the full length of the vector (including zeros).

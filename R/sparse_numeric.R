#' @importFrom methods new as
#' @importFrom stats sd
#' @importFrom methods show
NULL

#' Sparse Numeric Vector Class
#'
#' An S4 class used to represent a numeric vector that contains many zeros.
#' Only the nonzero entries and their locations are stored.
#'
#' @slot value A numeric vector containing all nonzero elements.
#' @slot pos An integer vector with the positions of nonzero values.
#' @slot length An integer giving the full length of the vector (including zeros).
#'
#' @return A new `sparse_numeric` object.
#' @name sparse_numeric-class
#' @rdname sparse_numeric-class
#' @export
setClass(
  Class = "sparse_numeric",
  slots = c(
    value = "numeric",
    pos = "integer",
    length = "integer"
  )
)

#-------------------------------------------------------------------------------
#' Check Validity of a sparse_numeric Object
#'
#' Ensures that a sparse vector object meets the expected structural rules:
#' matching lengths, valid positions, and no zero entries stored.
#'
#' @param object A `sparse_numeric` instance.
#' @return TRUE if valid, otherwise an informative error.
#' @name validation
#' @rdname validation
setValidity(
  Class = "sparse_numeric",
  method = function(object) {
    if (length(object@value) != length(object@pos))
      return("Number of values does not match number of positions.")
    if (length(object@pos) > object@length)
      return("More stored entries than allowed by the vector length.")
    if (any(object@pos > object@length))
      return("A position index exceeds the vector length.")
    if (any(object@pos < 1))
      return("Position indices must be positive.")
    if (any(object@value == 0))
      return("Zero values should not be stored in a sparse vector.")
    TRUE
  }
)

#-------------------------------------------------------------------------------
#' Convert a Dense Numeric Vector to sparse_numeric
#'
#' Transforms a regular numeric vector into a sparse representation by
#' extracting the nonzero entries and their indices.
#'
#' @param from A standard numeric vector.
#' @return A corresponding `sparse_numeric` object.
#' @name setAs
#' @rdname setAs
setAs(
  from = "numeric",
  to = "sparse_numeric",
  def = function(from) {
    pos <- which(from != 0)
    value <- from[pos]
    vec_length <- as.integer(length(from))
    new("sparse_numeric", value = value, pos = pos, length = vec_length)
  }
)

#' Convert sparse_numeric to a Dense Numeric Vector
#'
#' Expands a sparse vector back into a full numeric vector, filling all
#' non-stored positions with zeros.
#'
#' @param from A `sparse_numeric` object.
#' @return A standard numeric vector.
#' @name setAs
#' @rdname setAs
setAs(
  from = "sparse_numeric",
  to = "numeric",
  def = function(from) {
    vec <- numeric(from@length)
    for (i in seq_along(from@value)) {
      vec[from@pos[i]] <- from@value[i]
    }
    vec
  }
)

#-------------------------------------------------------------------------------
#' Sort a sparse_numeric Object
#'
#' Reorders a sparse vector based on the stored positions.
#'
#' @param x A `sparse_numeric` object.
#' @param decreasing Whether to sort in decreasing order of position.
#' @param ... Additional arguments (unused).
#'
#' @return A new sorted `sparse_numeric` vector.
#' @name sort
#' @rdname sort
#' @aliases sort,sparse_numeric-method
#' @export
setMethod(
  "sort",
  signature(x = "sparse_numeric"),
  function(x, decreasing = FALSE, ...) {
    ord <- order(x@pos, decreasing = decreasing)
    new("sparse_numeric",
        value = x@value[ord],
        pos = x@pos[ord],
        length = x@length)
  }
)

#-------------------------------------------------------------------------------
#' Element-wise Addition for sparse_numeric Vectors
#'
#' Defines a generic and method to add two sparse vectors without
#' converting them to dense format.
#'
#' @param x One sparse vector.
#' @param y Another sparse vector.
#'
#' @return A `sparse_numeric` containing the element-wise sum.
#' @name sparse_add
#' @rdname sparse_add
#' @export
setGeneric("sparse_add", function(x, y) standardGeneric("sparse_add"))

#' @describeIn sparse_add Add two sparse vectors.
setMethod(
  "sparse_add",
  signature(x = "sparse_numeric", y = "sparse_numeric"),
  function(x, y) {
    if (x@length != y@length)
      stop("Vectors must have the same length.")

    shared <- intersect(x@pos, y@pos)

    x_shared_vals <- x@value[x@pos %in% shared]
    y_shared_vals <- y@value[y@pos %in% shared]

    x_only_vals <- x@value[!(x@pos %in% shared)]
    y_only_vals <- y@value[!(y@pos %in% shared)]
    x_only_pos <- x@pos[!(x@pos %in% shared)]
    y_only_pos <- y@pos[!(y@pos %in% shared)]

    combined_vals <- c(x_shared_vals + y_shared_vals, x_only_vals, y_only_vals)
    combined_pos <- c(shared, x_only_pos, y_only_pos)

    out <- new(
      "sparse_numeric",
      value = combined_vals,
      pos = combined_pos,
      length = x@length
    )
    sort(out)
  }
)

#-------------------------------------------------------------------------------
#' Element-wise Multiplication for sparse_numeric Vectors
#'
#' @param x A `sparse_numeric` vector.
#' @param y Another `sparse_numeric` vector.
#'
#' @return A `sparse_numeric` object containing element-wise products.
#' @name sparse_mult
#' @rdname sparse_mult
#' @export
setGeneric("sparse_mult", function(x, y) standardGeneric("sparse_mult"))

#' @describeIn sparse_mult Multiply two sparse vectors.
setMethod(
  "sparse_mult",
  signature(x = "sparse_numeric", y = "sparse_numeric"),
  function(x, y) {
    if (x@length != y@length)
      stop("Vectors must be the same length.")

    shared <- intersect(x@pos, y@pos)

    x_vals <- x@value[x@pos %in% shared]
    y_vals <- y@value[y@pos %in% shared]

    new_sparse <- new(
      "sparse_numeric",
      value = x_vals * y_vals,
      pos = shared,
      length = x@length
    )
    sort(new_sparse)
  }
)

#-------------------------------------------------------------------------------
#' Subtract Two sparse_numeric Vectors
#'
#' @param x First sparse vector.
#' @param y Second sparse vector to subtract.
#'
#' @return A sparse vector representing `x - y`.
#' @name sparse_sub
#' @rdname sparse_sub
#' @export
setGeneric("sparse_sub", function(x, y) standardGeneric("sparse_sub"))

#' @describeIn sparse_sub Subtract two sparse vectors.
setMethod(
  "sparse_sub",
  signature(x = "sparse_numeric", y = "sparse_numeric"),
  function(x, y) {
    shared <- intersect(x@pos, y@pos)

    x_shared_vals <- x@value[x@pos %in% shared]
    y_shared_vals <- y@value[y@pos %in% shared]

    x_only_vals <- x@value[!(x@pos %in% shared)]
    y_only_vals <- -y@value[!(y@pos %in% shared)]
    x_only_pos <- x@pos[!(x@pos %in% shared)]
    y_only_pos <- y@pos[!(y@pos %in% shared)]

    diff_vals <- x_shared_vals - y_shared_vals

    # remove zeros generated by subtraction
    keep <- diff_vals != 0
    diff_vals <- diff_vals[keep]
    shared <- shared[keep]

    new_vals <- c(diff_vals, x_only_vals, y_only_vals)
    new_pos <- c(shared, x_only_pos, y_only_pos)

    out <- new(
      "sparse_numeric",
      value = new_vals,
      pos = new_pos,
      length = x@length
    )
    sort(out)
  }
)

#-------------------------------------------------------------------------------
#' Dot Product of Two sparse_numeric Vectors
#'
#' Computes the sum of pairwise products for overlapping nonzero positions.
#'
#' @param x A sparse vector.
#' @param y A sparse vector.
#'
#' @return A numeric scalar.
#' @name sparse_crossprod
#' @rdname sparse_crossprod
#' @export
setGeneric("sparse_crossprod", function(x, y) standardGeneric("sparse_crossprod"))

#' @describeIn sparse_crossprod Compute sparse dot product.
setMethod(
  "sparse_crossprod",
  signature(x = "sparse_numeric", y = "sparse_numeric"),
  function(x, y) {
    shared <- intersect(x@pos, y@pos)

    x_vals <- x@value[x@pos %in% shared]
    y_vals <- y@value[y@pos %in% shared]

    sum(x_vals * y_vals)
  }
)

#-------------------------------------------------------------------------------
# Arithmetic Operators -----------------------------------------------------------

#' @param e1 First operand (a sparse_numeric object)
#' @param e2 Second operand (a sparse_numeric object)
#'
#' @name sparse_numeric-arithmetic
#' @title Arithmetic Methods for sparse_numeric
#' @rdname sparse_numeric-arithmetic
NULL

#' @describeIn sparse_numeric-arithmetic Addition for sparse vectors.
setMethod("+", signature(e1 = "sparse_numeric", e2 = "sparse_numeric"),
          function(e1, e2) sparse_add(e1, e2))


#' @describeIn sparse_numeric-arithmetic Subtraction for sparse vectors.
setMethod("-", signature(e1 = "sparse_numeric", e2 = "sparse_numeric"),
          function(e1, e2) sparse_sub(e1, e2))


#' @describeIn sparse_numeric-arithmetic Multiplication for sparse vectors.
setMethod("*", signature(e1 = "sparse_numeric", e2 = "sparse_numeric"),
          function(e1, e2) sparse_mult(e1, e2))

#-------------------------------------------------------------------------------
#' Display a sparse_numeric Vector
#'
#' Prints the vector in a readable line-by-line format.
#'
#' @param object A `sparse_numeric` vector.
#' @return Printed output.
#' @name show
#' @rdname show
#' @aliases show,sparse_numeric-method
#' @export
setMethod(
  "show", "sparse_numeric",
  function(object) {
    cat("Sparse vector of length", object@length, "with stored entries:\n")
    for (i in seq_along(object@value)) {
      cat("  - position", object@pos[i], "->", object@value[i], "\n")
    }
  }
)

#-------------------------------------------------------------------------------
#' Plot Two sparse_numeric Vectors
#'
#' Produces a simple plot marking positions where both vectors contain
#' nonzero entries.
#'
#' @param x First sparse vector.
#' @param y Second sparse vector.
#' @return A basic overlap plot.
#' @name plot
#' @rdname plot
#' @aliases plot,sparse_numeric,sparse_numeric-method
#' @export
setMethod(
  "plot",
  signature(x = "sparse_numeric", y = "sparse_numeric"),
  function(x, y) {
    shared <- intersect(x@pos, y@pos)

    indicator <- as.integer(seq_len(x@length) %in% shared)

    plot(
      x = seq_len(x@length),
      y = indicator,
      main = "Overlap of Nonzero Entries",
      ylab = "Overlap (1 = both nonzero)",
      xlab = "Position"
    )
  }
)

#-------------------------------------------------------------------------------
#' Sum of a sparse_numeric Vector
#'
#' Computes the sum of all nonzero stored values.
#'
#' @param object A sparse vector.
#' @return The numeric sum.
#' @name sparse_sum
#' @rdname sparse_sum
#' @export
setGeneric("sparse_sum", function(object) standardGeneric("sparse_sum"))

#' @describeIn sparse_sum Sum stored entries.
setMethod(
  "sparse_sum", "sparse_numeric",
  function(object) {
    total <- 0
    for (v in object@value) total <- total + v
    total
  }
)

#-------------------------------------------------------------------------------
#' Mean of a sparse_numeric Vector
#'
#' Computes the mean including zeros (but without converting to dense form).
#'
#' @param x A sparse vector.
#' @return A numeric mean.
#' @name mean
#' @rdname mean
#' @aliases mean,sparse_numeric-method
#' @export
setMethod(
  "mean", "sparse_numeric",
  function(x) {
    total <- sum(x@value)
    total / x@length
  }
)

#-------------------------------------------------------------------------------
#' Norm of a sparse_numeric Vector
#'
#' Computes the Euclidean (L2) norm using only stored values.
#'
#' @param object A sparse vector.
#' @return A numeric norm.
#' @name norm
#' @rdname norm
#' @export
setGeneric("norm", function(object) standardGeneric("norm"))

#' @describeIn norm Euclidean norm for sparse vectors.
setMethod(
  "norm", "sparse_numeric",
  function(object) {
    acc <- 0
    for (v in object@value) acc <- acc + v^2
    sqrt(acc)
  }
)

#-------------------------------------------------------------------------------
#' Standardize a sparse_numeric Vector
#'
#' Converts the sparse vector to dense form, centers it, scales it, and then
#' converts it back into a sparse representation.
#'
#' @param object A sparse vector.
#' @return A standardized sparse vector.
#' @name standardize
#' @rdname standardize
#' @export
setGeneric("standardize", function(object) standardGeneric("standardize"))

#' @describeIn standardize Standardize a sparse vector.
setMethod(
  "standardize", "sparse_numeric",
  function(object) {

    dense <- as(object, "numeric")

    mu <- mean(dense)
    sigma <- sd(dense)

    if (sigma == 0)
      stop("Cannot standardize a vector with zero standard deviation.")

    standardized <- (dense - mu) / sigma
    as(standardized, "sparse_numeric")
  }
)


library(testthat)

#------------------------
# Validity & coercion
#------------------------
test_that("sparse object with invalid pos gives error", {
  expect_error({
    new("sparse_numeric", value = c(1, 2), pos = c(1L, 4L), length = 3L)
  })
})

test_that("coercion to numeric returns correct vector", {
  x <- new("sparse_numeric", value = c(5, 3), pos = c(2L, 5L), length = 6L)
  dense <- as(x, "numeric")
  expect_equal(dense, c(0, 5, 0, 0, 3, 0))
})

#------------------------
# Arithmetic: sparse_add / +
#------------------------
test_that("sparse_add handles overlapping and non-overlapping positions", {
  x <- as(c(0,2,0,3,0), "sparse_numeric")
  y <- as(c(1,0,4,0,0), "sparse_numeric")
  result <- as(c(1,2,4,3,0), "sparse_numeric")
  expect_equal(sparse_add(x,y), result)
  expect_equal(x+y, result)  # operator
})

#------------------------
# sparse_sub / -
#------------------------
test_that("sparse_sub removes zeros after subtraction", {
  x <- as(c(1,2,0,3,0), "sparse_numeric")
  y <- as(c(1,1,0,4,0), "sparse_numeric")
  result <- as(c(0,1,-1), "sparse_numeric") # positions: 2=1, 4=-1
  diff <- sparse_sub(x, y)
  expect_equal(as(diff, "numeric"), c(0,1,0,-1,0))
  expect_equal(x-y, diff) # operator
})

#------------------------
# sparse_mult / *
#------------------------
test_that("sparse_mult multiplies correctly", {
  x <- as(c(0,2,0,3), "sparse_numeric")
  y <- as(c(1,0,5,2), "sparse_numeric")
  result <- as(c(0,0,0,6), "sparse_numeric")
  expect_equal(sparse_mult(x,y), result)
  expect_equal(x*y, result)  # operator
})

#------------------------
# sparse_crossprod
#------------------------
test_that("sparse_crossprod computes dot product correctly", {
  x <- as(c(1,2,0,3), "sparse_numeric")
  y <- as(c(0,2,5,1), "sparse_numeric")
  expect_equal(sparse_crossprod(x,y), 2*2 + 3*1) # 4 + 3 = 7
})

#------------------------
# sparse_sum
#------------------------
test_that("sparse_sum computes sum of stored values", {
  x <- as(c(1,0,2,0,3), "sparse_numeric")
  expect_equal(sparse_sum(x), 6)
})

#------------------------
# mean
#------------------------
test_that("mean works including zeros", {
  x <- as(c(1,0,2,0,3), "sparse_numeric")
  expect_equal(mean(x), 6/5)
})

#------------------------
# norm
#------------------------
test_that("norm computes Euclidean norm", {
  x <- as(c(3,0,4), "sparse_numeric")
  expect_equal(norm(x), 5)
})

#------------------------
# standardize
#------------------------
test_that("standardize errors on zero SD", {
  x <- as(rep(5,3), "sparse_numeric")
  expect_error(standardize(x))
})

#------------------------
# sort
#------------------------
test_that("sort orders by pos", {
  x <- new("sparse_numeric", value=c(3,1,2), pos=c(5L,2L,4L), length=6L)
  sorted <- sort(x)
  expect_equal(sorted@pos, c(2L,4L,5L))
  expect_equal(sorted@value, c(1,2,3))
})

#------------------------
# show / plot
#------------------------
test_that("show prints without error", {
  x <- as(c(1,0,2), "sparse_numeric")
  expect_output(show(x))
})

test_that("plot runs without error", {
  x <- as(c(1,0,2), "sparse_numeric")
  y <- as(c(0,2,2), "sparse_numeric")
  expect_silent(plot(x,y))
})


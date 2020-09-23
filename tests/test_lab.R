source("../lab.R", chdir = TRUE)
library(testthat)

test_that("1. Assign the value `2L` to `foo`", {
    expect_identical(foo, 2L)
})

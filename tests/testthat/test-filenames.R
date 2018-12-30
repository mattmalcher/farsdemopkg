context("test-filenames")
library(farsdemopkg)

test_that("ints and strings", {
  expect_equal(make_filename(2013),
               "data/accident_2013.csv.bz2")
  expect_equal(make_filename("2013"),
               "data/accident_2013.csv.bz2")
})

test_that("different years", {
  expect_equal(make_filename(2013),
               "data/accident_2013.csv.bz2")
  expect_equal(make_filename(2014),
               "data/accident_2014.csv.bz2")
})

test_that("different years", {
  expect_equal(make_filename(2013),
               file.path("data","accident_2013.csv.bz2"))

})

test_that("type", {
  a <- make_filename(2013)
  expect_type(a,
               "character")

})

test_that("get_ident removes extension, buzzdetect tag, and leading dir", {
  expect_equal(
    get_ident("/data/foo/bar_buzzdetect.csv", "/data"),
    "foo/bar"
  )
  expect_equal(
    get_ident("/data/foo/bar_buzzdetect.rds", "/data"),
    "foo/bar"
  )
})

test_that("get_ident works with no dir_in", {
  expect_equal(get_ident("foo/bar_buzzdetect.csv"), "foo/bar")
  expect_equal(get_ident("bar_buzzdetect.csv"), "bar")
})

test_that("get_ident works on a vector of paths", {
  paths <- c(
    "/data/a/foo_buzzdetect.csv",
    "/data/b/bar_buzzdetect.csv"
  )
  expect_equal(
    get_ident(paths, "/data"),
    c("a/foo", "b/bar")
  )
})

test_that("get_ident uses real example data paths correctly", {
  dir <- system.file("extdata/five_flowers", package = "buzzr")
  skip_if(nchar(dir) == 0, "example data not available")

  path <- file.path(dir, "soybean/9/230809_0000_buzzdetect.csv")
  expect_equal(get_ident(path, dir), "soybean/9/230809_0000")
})

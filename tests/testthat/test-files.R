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

test_that("get_ident strips _buzzpart tag", {
  expect_equal(get_ident("/data/foo/bar_buzzpart.csv", "/data"), "foo/bar")
  expect_equal(get_ident("bar_buzzpart.csv"), "bar")
})


# ── list_results ──────────────────────────────────────────────────────────────

make_results_dir <- function() {
  dir <- file.path(tempdir(), paste0("buzzr_lr_", Sys.getpid()))
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  writeLines("start,activation_ins_buzz\n0,-1.5", file.path(dir, "a_buzzdetect.csv"))
  writeLines("start,activation_ins_buzz\n0,-1.5", file.path(dir, "b_buzzpart.csv"))
  writeLines("start,activation_ins_buzz\n0,-1.5", file.path(dir, "c_other.csv"))
  dir
}

test_that("list_results returns only _buzzdetect files by default", {
  dir <- make_results_dir()
  on.exit(unlink(dir, recursive = TRUE))

  paths <- list_results(dir)
  expect_length(paths, 1)
  expect_true(all(grepl("_buzzdetect\\.csv$", paths)))
})

test_that("list_results includes _buzzpart files when include_partial=TRUE", {
  dir <- make_results_dir()
  on.exit(unlink(dir, recursive = TRUE))

  paths <- list_results(dir, include_partial = TRUE)
  expect_length(paths, 2)
  expect_true(any(grepl("_buzzdetect\\.csv$", paths)))
  expect_true(any(grepl("_buzzpart\\.csv$", paths)))
})

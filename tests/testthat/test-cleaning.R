# Helper: synthetic results with activation columns
make_activation_results <- function() {
  data.table::data.table(
    start_filetime     = c(0, 0.96, 1.92),
    activation_ins_buzz   = c(-1.12345, -0.87654, -1.99999),
    activation_mech_plane = c(-2.22222, -1.99999, -2.55555),
    activation_ins_trill  = c(-1.88888, -1.77777, -2.11111)
  )
}


# ── trim_results ──────────────────────────────────────────────────────────────

test_that("trim_results rounds activation columns to specified digits", {
  results <- make_activation_results()
  out <- trim_results(results, activation_digits = 2)

  expect_equal(out$activation_ins_buzz,   round(c(-1.12345, -0.87654, -1.99999), 2))
  expect_equal(out$activation_mech_plane, round(c(-2.22222, -1.99999, -2.55555), 2))
})

test_that("trim_results does not modify non-activation columns", {
  results <- make_activation_results()
  out <- trim_results(results, activation_digits = 2)

  expect_equal(out$start_filetime, results$start_filetime)
})

test_that("trim_results keeps only specified neurons when neurons_keep given", {
  results <- make_activation_results()
  out <- trim_results(results, activation_digits = 2, neurons_keep = "ins_buzz")

  expect_true("activation_ins_buzz" %in% names(out))
  expect_false("activation_mech_plane" %in% names(out))
  expect_false("activation_ins_trill" %in% names(out))
})

test_that("trim_results accepts neurons_keep with or without 'activation_' prefix", {
  results <- make_activation_results()

  out_plain  <- trim_results(results, activation_digits = 2, neurons_keep = "ins_buzz")
  out_tagged <- trim_results(results, activation_digits = 2, neurons_keep = "activation_ins_buzz")

  expect_equal(names(out_plain), names(out_tagged))
})

test_that("trim_results warns when neurons_keep contains unknown neuron", {
  results <- make_activation_results()
  expect_warning(
    trim_results(results, activation_digits = 2, neurons_keep = c("ins_buzz", "nonexistent")),
    "not found in results"
  )
})

test_that("trim_results does not modify the original data.table", {
  results <- make_activation_results()
  orig_val <- results$activation_ins_buzz[1]
  trim_results(results, activation_digits = 2)

  expect_equal(results$activation_ins_buzz[1], orig_val)
})


# ── trim_directory ────────────────────────────────────────────────────────────

test_that("trim_directory creates .rds output files and returns their paths", {
  dir_in <- system.file("extdata/five_flowers", package = "buzzr")
  skip_if(nchar(dir_in) == 0, "example data not available")

  dir_out <- file.path(tempdir(), paste0("buzzr_trim_", Sys.getpid()))
  on.exit(unlink(dir_out, recursive = TRUE))

  paths <- trim_directory(dir_in, dir_out, activation_digits = 2)

  expect_true(all(file.exists(paths)))
  expect_true(all(endsWith(paths, ".rds")))
})

test_that("trim_directory output files have rounded activations", {
  dir_in <- system.file("extdata/five_flowers", package = "buzzr")
  skip_if(nchar(dir_in) == 0)

  dir_out <- file.path(tempdir(), paste0("buzzr_trim_check_", Sys.getpid()))
  on.exit(unlink(dir_out, recursive = TRUE))

  paths <- trim_directory(dir_in, dir_out, activation_digits = 2)

  # Read one output file and check activation rounding
  df <- readRDS(paths[1])
  act_cols <- grep("^activation_", names(df), value = TRUE)
  for (col in act_cols) {
    expect_equal(df[[col]], round(df[[col]], 2))
  }
})

test_that("trim_directory if_exists='stop' errors when output already exists", {
  dir_in <- system.file("extdata/five_flowers", package = "buzzr")
  skip_if(nchar(dir_in) == 0)

  dir_out <- file.path(tempdir(), paste0("buzzr_trim_stop_", Sys.getpid()))
  on.exit(unlink(dir_out, recursive = TRUE))

  trim_directory(dir_in, dir_out, activation_digits = 2)

  expect_error(
    trim_directory(dir_in, dir_out, activation_digits = 2, if_exists = "stop"),
    "already exist"
  )
})

test_that("trim_directory if_exists='skip' skips existing files with a message", {
  dir_in <- system.file("extdata/five_flowers", package = "buzzr")
  skip_if(nchar(dir_in) == 0)

  dir_out <- file.path(tempdir(), paste0("buzzr_trim_skip_", Sys.getpid()))
  on.exit(unlink(dir_out, recursive = TRUE))

  trim_directory(dir_in, dir_out, activation_digits = 2)

  expect_message(
    trim_directory(dir_in, dir_out, activation_digits = 2, if_exists = "skip"),
    "Skipping"
  )
})

test_that("trim_directory if_exists='overwrite' warns and overwrites", {
  dir_in <- system.file("extdata/five_flowers", package = "buzzr")
  skip_if(nchar(dir_in) == 0)

  dir_out <- file.path(tempdir(), paste0("buzzr_trim_ow_", Sys.getpid()))
  on.exit(unlink(dir_out, recursive = TRUE))

  trim_directory(dir_in, dir_out, activation_digits = 2)

  expect_warning(
    trim_directory(dir_in, dir_out, activation_digits = 2, if_exists = "overwrite"),
    "Overwriting"
  )
})

test_that("trim_directory output_format='csv' creates .csv files without row names", {
  dir_in <- system.file("extdata/five_flowers", package = "buzzr")
  skip_if(nchar(dir_in) == 0, "example data not available")

  dir_out <- file.path(tempdir(), paste0("buzzr_trim_csv_", Sys.getpid()))
  on.exit(unlink(dir_out, recursive = TRUE))

  paths <- trim_directory(dir_in, dir_out, activation_digits = 2, output_format = "csv")

  expect_true(all(file.exists(paths)))
  expect_true(all(endsWith(paths, ".csv")))

  # No row names: first column should not be an unnamed integer index
  df <- read.csv(paths[1])
  expect_false(names(df)[1] == "")
})

test_that("trim_directory with neurons_keep only writes kept neurons", {
  dir_in <- system.file("extdata/five_flowers", package = "buzzr")
  skip_if(nchar(dir_in) == 0)

  dir_out <- file.path(tempdir(), paste0("buzzr_trim_keep_", Sys.getpid()))
  on.exit(unlink(dir_out, recursive = TRUE))

  paths <- trim_directory(
    dir_in, dir_out,
    activation_digits = 2,
    neurons_keep = "ins_buzz"
  )

  df <- readRDS(paths[1])
  act_cols <- grep("^activation_", names(df), value = TRUE)

  expect_true("activation_ins_buzz" %in% act_cols)
  expect_false("activation_mech_plane" %in% act_cols)
})

test_that("trim_directory writes a single .rds file when path_out ends in .rds", {
  dir_in <- system.file("extdata/five_flowers", package = "buzzr")
  skip_if(nchar(dir_in) == 0)

  out_file <- file.path(tempdir(), paste0("buzzr_trim_single_", Sys.getpid(), ".rds"))
  on.exit(unlink(out_file))

  result <- trim_directory(dir_in, out_file, activation_digits = 2, if_exists = "overwrite")

  expect_equal(result, out_file)
  expect_true(file.exists(out_file))
  df <- readRDS(out_file)
  expect_s3_class(df, "data.frame")
  expect_true("ident" %in% names(df))
})

test_that("trim_directory writes a single .csv file when path_out ends in .csv", {
  dir_in <- system.file("extdata/five_flowers", package = "buzzr")
  skip_if(nchar(dir_in) == 0)

  out_file <- file.path(tempdir(), paste0("buzzr_trim_single_", Sys.getpid(), ".csv"))
  on.exit(unlink(out_file))

  result <- trim_directory(dir_in, out_file, activation_digits = 2)

  expect_equal(result, out_file)
  expect_true(file.exists(out_file))
  df <- read.csv(out_file)
  expect_gt(nrow(df), 0)
})

test_that("trim_directory single-file if_exists='stop' errors when file exists", {
  dir_in <- system.file("extdata/five_flowers", package = "buzzr")
  skip_if(nchar(dir_in) == 0)

  out_file <- file.path(tempdir(), paste0("buzzr_trim_single_stop_", Sys.getpid(), ".rds"))
  on.exit(unlink(out_file))

  trim_directory(dir_in, out_file, activation_digits = 2)

  expect_error(
    trim_directory(dir_in, out_file, activation_digits = 2, if_exists = "stop"),
    "already exists"
  )
})

test_that("trim_directory single-file if_exists='skip' returns path with message", {
  dir_in <- system.file("extdata/five_flowers", package = "buzzr")
  skip_if(nchar(dir_in) == 0)

  out_file <- file.path(tempdir(), paste0("buzzr_trim_single_skip_", Sys.getpid(), ".rds"))
  on.exit(unlink(out_file))

  trim_directory(dir_in, out_file, activation_digits = 2)

  expect_message(
    result <- trim_directory(dir_in, out_file, activation_digits = 2, if_exists = "skip"),
    "Skipping"
  )
  expect_equal(result, out_file)
})

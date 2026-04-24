# Helper: synthetic frame-level results with two activation columns
make_frame_results <- function(n = 6) {
  data.table::data.table(
    start_filetime = seq(0, by = 0.96, length.out = n),
    activation_ins_buzz    = c(-1.5, -0.8, -1.3, -2.0, -0.5, -1.1),
    activation_mech_plane  = c(-2.2, -1.9, -2.5, -1.8, -2.0, -2.3)
  )
}

# Helper: results after call_detections (no activation cols)
make_called_results <- function() {
  data.table::data.table(
    start_filetime     = c(0, 0.96, 1.92, 2.88, 300, 300.96),
    detections_ins_buzz = c(FALSE, TRUE, FALSE, TRUE, TRUE, FALSE)
  )
}


# ── read_results ──────────────────────────────────────────────────────────────

test_that("read_results reads CSV and renames 'start' to 'start_filetime'", {
  path <- system.file(
    "extdata/five_flowers/soybean/9/230809_0000_buzzdetect.csv",
    package = "buzzr"
  )
  skip_if(nchar(path) == 0, "example data not available")

  result <- read_results(path)

  expect_s3_class(result, "data.table")
  expect_true("start_filetime" %in% names(result))
  expect_false("start" %in% names(result))
  expect_true(any(startsWith(names(result), "activation_")))
})

test_that("read_results adds start_datetime when posix_formats given", {
  path <- system.file(
    "extdata/five_flowers/soybean/9/230809_0000_buzzdetect.csv",
    package = "buzzr"
  )
  skip_if(nchar(path) == 0)

  result <- read_results(path, posix_formats = "%y%m%d_%H%M", tz = "UTC")

  expect_true("start_datetime" %in% names(result))
  expect_false("start_filetime" %in% names(result))  # dropped by default
  expect_s3_class(result$start_datetime, "POSIXct")
})

test_that("read_results keeps both time cols when drop_filetime=FALSE", {
  path <- system.file(
    "extdata/five_flowers/soybean/9/230809_0000_buzzdetect.csv",
    package = "buzzr"
  )
  skip_if(nchar(path) == 0)

  result <- read_results(
    path,
    posix_formats = "%y%m%d_%H%M",
    tz = "UTC",
    drop_filetime = FALSE
  )

  expect_true("start_datetime" %in% names(result))
  expect_true("start_filetime" %in% names(result))
})

test_that("read_results extracts dir_nesting into columns", {
  path <- system.file(
    "extdata/five_flowers/soybean/9/230809_0000_buzzdetect.csv",
    package = "buzzr"
  )
  skip_if(nchar(path) == 0)

  result <- read_results(path, dir_nesting = c("flower", "recorder"))

  expect_true("flower" %in% names(result))
  expect_true("recorder" %in% names(result))
  expect_equal(result$flower[1], "soybean")
  expect_equal(result$recorder[1], "9")
})

test_that("read_results reads an RDS file", {
  path_csv <- system.file(
    "extdata/five_flowers/soybean/9/230809_0000_buzzdetect.csv",
    package = "buzzr"
  )
  skip_if(nchar(path_csv) == 0)

  # Write a temp RDS to test RDS reading
  tmp <- tempfile(fileext = "_buzzdetect.rds")
  on.exit(unlink(tmp))
  df <- data.table::fread(path_csv)
  saveRDS(df, tmp)

  result <- read_results(tmp)
  expect_s3_class(result, "data.table")
  # 'start' column from the raw CSV gets renamed
  expect_true("start_filetime" %in% names(result))
})


# ── read_directory ────────────────────────────────────────────────────────────

test_that("read_directory returns data.table with all files combined", {
  dir <- system.file("extdata/five_flowers", package = "buzzr")
  skip_if(nchar(dir) == 0)

  result <- read_directory(dir)

  expect_s3_class(result, "data.table")
  expect_gt(nrow(result), 0)
  expect_true("start_filetime" %in% names(result))
})

test_that("read_directory adds dir_nesting columns from all files", {
  dir <- system.file("extdata/five_flowers", package = "buzzr")
  skip_if(nchar(dir) == 0)

  result <- read_directory(dir, dir_nesting = c("flower", "recorder"))

  expect_true("flower" %in% names(result))
  expect_equal(length(unique(result$flower)), 5)  # 5 flowers in dataset
})

test_that("read_directory returns empty data.frame with warning when no files found", {
  dir <- tempdir()
  expect_warning(
    result <- read_directory(dir),
    "No results found"
  )
  expect_equal(nrow(result), 0)
})

test_that("read_directory with return_ident=TRUE adds ident column", {
  dir <- system.file("extdata/five_flowers", package = "buzzr")
  skip_if(nchar(dir) == 0)

  result <- read_directory(dir, return_ident = TRUE)

  expect_true("ident" %in% names(result))
})


# ── call_detections ───────────────────────────────────────────────────────────

test_that("call_detections converts activations to binary detections", {
  results <- make_frame_results()
  out <- call_detections(results, thresholds = c(ins_buzz = -1.2))

  expect_true("detections_ins_buzz" %in% names(out))
  # activation > threshold → TRUE
  # -1.5>-1.2=F, -0.8>-1.2=T, -1.3>-1.2=F, -2.0>-1.2=F, -0.5>-1.2=T, -1.1>-1.2=T
  expect_equal(
    out$detections_ins_buzz,
    c(FALSE, TRUE, FALSE, FALSE, TRUE, TRUE)
  )
})

test_that("call_detections drops all activation_ columns", {
  results <- make_frame_results()
  out <- call_detections(results, thresholds = c(ins_buzz = -1.2))

  expect_false(any(startsWith(names(out), "activation_")))
})

test_that("call_detections handles multiple thresholds", {
  results <- make_frame_results()
  out <- call_detections(
    results,
    thresholds = c(ins_buzz = -1.2, mech_plane = -2.0)
  )

  expect_true("detections_ins_buzz" %in% names(out))
  expect_true("detections_mech_plane" %in% names(out))
  expect_false(any(startsWith(names(out), "activation_")))
})

test_that("call_detections warns on pre-existing detection column", {
  results <- make_frame_results()
  results$detections_ins_buzz <- FALSE

  expect_warning(
    call_detections(results, thresholds = c(ins_buzz = -1.2)),
    "Ignoring existing detection column"
  )
})

test_that("call_detections warns on neuron not found in results", {
  results <- make_frame_results()
  expect_warning(
    call_detections(results, thresholds = c(nonexistent = 0)),
    "not found in results"
  )
})

test_that("call_detections drops activations even when all thresholds are invalid", {
  results <- make_frame_results()
  out <- suppressWarnings(
    call_detections(results, thresholds = c(nonexistent = 0))
  )
  expect_false(any(startsWith(names(out), "activation_")))
})

test_that("call_detections does not modify the original data.table", {
  results <- make_frame_results()
  orig <- data.table::copy(results)
  call_detections(results, thresholds = c(ins_buzz = -1.2))

  expect_equal(results, orig)
})


# ── bin ───────────────────────────────────────────────────────────────────────

test_that("bin creates bin_filetime column and sums detections and frames", {
  results <- make_called_results()
  out <- bin(results, binwidth = 5)  # 5 min = 300 sec

  expect_true("bin_filetime" %in% names(out))
  expect_true("frames" %in% names(out))
  expect_true("detections_ins_buzz" %in% names(out))

  # Two bins: [0, 300) and [300, 600)
  expect_equal(nrow(out), 2)
  expect_equal(out$detections_ins_buzz[out$bin_filetime == 0], 2)
  expect_equal(out$frames[out$bin_filetime == 0], 4)
})

test_that("bin imputes frames = 1 per row when no frames column present", {
  results <- data.table::data.table(
    start_filetime      = c(0, 0.96, 1.92),
    detections_ins_buzz = c(FALSE, TRUE, FALSE)
  )
  out <- bin(results, binwidth = 5)

  expect_equal(out$frames, 3)
})

test_that("bin calculates detection rates when calculate_rate=TRUE", {
  results <- make_called_results()
  out <- bin(results, binwidth = 5, calculate_rate = TRUE)

  expect_true("detectionrate_ins_buzz" %in% names(out))
  # bin 0: 2 detections / 4 frames = 0.5
  expect_equal(out$detectionrate_ins_buzz[out$bin_filetime == 0], 0.5)
})

test_that("bin works with start_datetime column", {
  t0 <- as.POSIXct("2023-08-09 08:00:00", tz = "UTC")
  results <- data.table::data.table(
    start_datetime      = c(t0, t0 + 60, t0 + 120, t0 + 900),
    detections_ins_buzz = c(FALSE, TRUE, FALSE, TRUE)
  )
  out <- bin(results, binwidth = 15)

  expect_true("bin_datetime" %in% names(out))
  expect_equal(nrow(out), 2)
})

test_that("bin can re-bin already-binned results", {
  results_binned <- data.table::data.table(
    bin_filetime        = c(0, 300, 600, 900),   # 5-min bins (300 sec)
    detections_ins_buzz = c(1L, 2L, 0L, 3L),
    frames              = c(312L, 312L, 312L, 312L)
  )
  out <- bin(results_binned, binwidth = 10)  # re-bin to 10 min = 600 sec

  expect_equal(nrow(out), 2)
  # bin at 0: (1+2) detections, (312+312) frames
  expect_equal(out$detections_ins_buzz[out$bin_filetime == 0], 3L)
  expect_equal(out$frames[out$bin_filetime == 0], 624L)
})

test_that("bin groups by extra columns", {
  results <- data.table::data.table(
    start_filetime      = c(0, 0.96, 1.92, 2.88),
    detections_ins_buzz = c(FALSE, TRUE, FALSE, TRUE),
    group               = c("A", "A", "B", "B")
  )
  expect_message(
    out <- bin(results, binwidth = 5),
    "Grouping time bins"
  )
  # Two groups in one time bin → two rows
  expect_equal(nrow(out), 2)
})

test_that("bin warns when no detection columns are present", {
  results <- data.table::data.table(
    start_filetime = c(0, 0.96),
    frames         = c(1L, 1L)
  )
  expect_warning(bin(results, binwidth = 5), "No detection columns")
})

test_that("bin errors when no time column is present", {
  results <- data.table::data.table(
    detections_ins_buzz = c(FALSE, TRUE),
    frames              = c(1L, 1L)
  )
  expect_error(bin(results, binwidth = 5))
})

test_that("bin errors when re-binning data with no frames column", {
  results <- data.table::data.table(
    bin_filetime        = c(0, 300),
    detections_ins_buzz = c(1L, 2L)
    # no frames column — should error
  )
  expect_error(bin(results, binwidth = 10), "frames")
})


# ── bin_directory ─────────────────────────────────────────────────────────────

test_that("bin_directory returns binned data.table with detection columns", {
  dir <- system.file("extdata/five_flowers", package = "buzzr")
  skip_if(nchar(dir) == 0)

  result <- bin_directory(
    dir_results  = dir,
    thresholds   = c(ins_buzz = -1.2),
    posix_formats = "%y%m%d_%H%M",
    tz           = "UTC",
    dir_nesting  = c("flower", "recorder"),
    binwidth     = 20,
    workers     = 1
  )

  expect_s3_class(result, "data.table")
  expect_true("detections_ins_buzz" %in% names(result))
  expect_true("frames" %in% names(result))
  expect_true("flower" %in% names(result))
  expect_equal(length(unique(result$flower)), 5)
})

test_that("bin_directory returns empty data.frame when directory has no results", {
  dir <- tempdir()
  expect_warning(
    result <- bin_directory(dir, thresholds = c(ins_buzz = -1.2)),
    "No results found"
  )
  expect_equal(nrow(result), 0)
})

# ── commontime ────────────────────────────────────────────────────────────────

test_that("commontime coerces date to 2000-01-01, preserving time of day", {
  t <- as.POSIXct("2023-06-15 14:30:00", tz = "UTC")
  result <- commontime(t, tz = "UTC")

  expect_equal(as.Date(result), as.Date("2000-01-01"))
  expect_equal(format(result, "%H:%M:%S"), "14:30:00")
})

test_that("commontime works on a vector of different dates", {
  times <- as.POSIXct(
    c("2023-01-01 08:00:00", "2024-06-15 20:00:00"),
    tz = "UTC"
  )
  result <- commontime(times, tz = "UTC")

  expect_true(all(as.Date(result) == as.Date("2000-01-01")))
  expect_equal(format(result, "%H:%M:%S"), c("08:00:00", "20:00:00"))
})

test_that("commontime returns POSIXct", {
  t <- as.POSIXct("2023-06-15 14:30:00", tz = "UTC")
  expect_s3_class(commontime(t, tz = "UTC"), "POSIXct")
})


# ── time_of_day ───────────────────────────────────────────────────────────────

test_that("time_of_day returns correct proportion", {
  t_midnight <- as.POSIXct("2023-01-01 00:00:00", tz = "UTC")
  t_6am      <- as.POSIXct("2023-01-01 06:00:00", tz = "UTC")
  t_noon     <- as.POSIXct("2023-01-01 12:00:00", tz = "UTC")

  expect_equal(time_of_day(t_midnight), 0)
  expect_equal(time_of_day(t_6am), 0.25)
  expect_equal(time_of_day(t_noon), 0.5)
})

test_that("time_of_day returns correct hours", {
  t_6_30 <- as.POSIXct("2023-01-01 06:30:00", tz = "UTC")
  t_noon  <- as.POSIXct("2023-01-01 12:00:00", tz = "UTC")

  expect_equal(time_of_day(t_6_30, time_format = "hour"), 6.5)
  expect_equal(time_of_day(t_noon, time_format = "hour"), 12)
})

test_that("time_of_day errors on invalid format string", {
  t <- as.POSIXct("2023-01-01 12:00:00", tz = "UTC")
  expect_error(time_of_day(t, time_format = "minutes"), "proportion or hour")
})

test_that("time_of_day works on a vector", {
  times <- as.POSIXct(c("2023-01-01 00:00:00", "2023-01-01 12:00:00"), tz = "UTC")
  result <- time_of_day(times)
  expect_equal(result, c(0, 0.5))
})


# ── file_start_time ───────────────────────────────────────────────────────────

test_that("file_start_time extracts correct datetime from a filename", {
  path <- "path/to/230809_0000_buzzdetect.csv"
  result <- file_start_time(path, posix_formats = "%y%m%d_%H%M", tz = "UTC")

  expect_s3_class(result, "POSIXct")
  expect_equal(format(result, "%Y-%m-%d %H:%M"), "2023-08-09 00:00")
})

test_that("file_start_time works on a vector of paths", {
  paths <- c(
    "path/230809_0000_buzzdetect.csv",
    "path/240727_1430_buzzdetect.csv"
  )
  result <- file_start_time(paths, posix_formats = "%y%m%d_%H%M", tz = "UTC")

  expect_length(result, 2)
  expect_equal(format(result[1], "%Y-%m-%d %H:%M"), "2023-08-09 00:00")
  expect_equal(format(result[2], "%Y-%m-%d %H:%M"), "2024-07-27 14:30")
})

test_that("file_start_time returns NA with warning when no format matches", {
  path <- "path/to/no_date_here_buzzdetect.csv"
  expect_warning(
    result <- file_start_time(path, posix_formats = "%y%m%d_%H%M", tz = "UTC"),
    "No POSIX matches found"
  )
  expect_true(is.na(result))
})

test_that("file_start_time returns NA with warning when formats conflict (first_match=FALSE)", {
  # %y%m%d and %y%d%m both match '230809' but parse to different dates
  path <- "path/230809_buzzdetect.csv"
  expect_warning(
    result <- file_start_time(
      path,
      posix_formats = c("%y%m%d", "%y%d%m"),
      tz = "UTC",
      first_match = FALSE
    ),
    "multiple POSIX matches"
  )
  expect_true(is.na(result))
})

test_that("file_start_time accepts first match when formats conflict and first_match=TRUE", {
  path <- "path/230809_buzzdetect.csv"
  expect_message(
    result <- file_start_time(
      path,
      posix_formats = c("%y%m%d", "%y%d%m"),
      tz = "UTC",
      first_match = TRUE
    ),
    "Multiple POSIXct patterns match"
  )
  expect_false(is.na(result))
})

test_that("file_start_time handles duplicate formats without warning", {
  path <- "path/230809_0000_buzzdetect.csv"
  # Same format twice resolves to the same time → no conflict
  result <- file_start_time(
    path,
    posix_formats = c("%y%m%d_%H%M", "%y%m%d_%H%M"),
    tz = "UTC"
  )
  expect_false(is.na(result))
})

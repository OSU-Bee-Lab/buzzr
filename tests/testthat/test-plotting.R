# ── theme_buzzr ───────────────────────────────────────────────────────────────

test_that("theme_buzzr returns a ggplot2 theme for 'light' mode", {
  t <- theme_buzzr(mode = "light")
  expect_s3_class(t, "theme")
})

test_that("theme_buzzr returns a ggplot2 theme for 'dark' mode", {
  t <- theme_buzzr(mode = "dark")
  expect_s3_class(t, "theme")
})

test_that("theme_buzzr accepts a numeric base_size", {
  t <- theme_buzzr(base_size = 14, mode = "light")
  expect_s3_class(t, "theme")
})

test_that("theme_buzzr errors on an invalid mode", {
  expect_error(theme_buzzr(mode = "invalid"), "light or dark")
})


# ── label_hour ────────────────────────────────────────────────────────────────

test_that("label_hour returns a function", {
  expect_type(label_hour(), "closure")
})

test_that("label_hour function returns one character label per input time", {
  fn <- label_hour(tz = "America/New_York")
  times <- as.POSIXct(
    c("2000-01-01 06:00:00", "2000-01-01 14:00:00"),
    tz = "America/New_York"
  )
  result <- fn(times)

  expect_type(result, "character")
  expect_length(result, 2)
})

test_that("label_hour labels do not have a leading zero", {
  fn <- label_hour(tz = "America/New_York")
  t <- as.POSIXct("2000-01-01 06:00:00", tz = "America/New_York")
  result <- fn(t)

  expect_false(startsWith(result, "0"))
})

test_that("label_hour respects the tz parameter", {
  # 12:00 UTC = 07:00 America/New_York (EST, UTC-5)
  t <- as.POSIXct("2000-01-01 12:00:00", tz = "UTC")

  label_utc <- label_hour(tz = "UTC")(t)
  label_est <- label_hour(tz = "America/New_York")(t)

  # UTC shows noon, EST shows 7 am
  expect_equal(label_utc, "12 pm")
  expect_equal(label_est, "7 am")
})

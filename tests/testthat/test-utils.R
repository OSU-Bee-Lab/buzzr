test_that("frames_expected returns correct calculation", {
  expect_equal(frames_expected(5, 0.96), (5 * 60) / 0.96)
  expect_equal(frames_expected(1, 1), 60)
  expect_equal(frames_expected(0, 1), 0)
  expect_equal(frames_expected(60, 0.96), (60 * 60) / 0.96)
})

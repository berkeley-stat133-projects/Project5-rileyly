test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})
test_that("summaries use only eligible records at the requested station", {
  data <- data.frame(
    station_code = c("BKY", "BKY", "EMY", "BKY", "BKY"),
    status = c(
      "Departed", "Departed", "Departed", "Enroute", "Departed"
    ),
    delay_minutes = c(10, 20, 5, 100, NA_real_)
  )

  result <- summarize_delays(data, " bky ")

  expect_named(
    result,
    c(
      "station_code", "n_records",
      "mean_delay_minutes", "max_delay_minutes"
    )
  )
  expect_equal(nrow(result), 1L)
  expect_identical(result$station_code, "BKY")
  expect_type(result$n_records, "integer")
  expect_type(result$mean_delay_minutes, "double")
  expect_type(result$max_delay_minutes, "double")

  expect_equal(result$n_records, 2L)
  expect_equal(result$mean_delay_minutes, 15)
  expect_equal(result$max_delay_minutes, 20)
})

test_that("no eligible records produce zero count and missing summaries", {
  data <- data.frame(
    station_code = c("BKY", "BKY"),
    status = c("Enroute", "Departed"),
    delay_minutes = c(30, NA_real_)
  )

  for (station in c("BKY", "SAC")) {
    result <- summarize_delays(data, station)

    expect_equal(result$n_records, 0L)
    expect_true(is.na(result$mean_delay_minutes))
    expect_true(is.na(result$max_delay_minutes))
  }
})

test_that("invalid station arguments are rejected", {
  data <- data.frame(
    station_code = "BKY",
    status = "Departed",
    delay_minutes = 10
  )

  for (station in list(123, c("BKY", "EMY"), NA_character_, "")) {
    expect_error(
      summarize_delays(data, station),
      "station must be a single non-empty string.",
      fixed = TRUE
    )
  }
})

test_that("invalid data inputs are rejected", {
  expect_error(
    summarize_delays(123, "BKY"),
    "data must be a data frame.",
    fixed = TRUE
  )

  expect_error(
    summarize_delays(data.frame(x = 1), "BKY"),
    "data must contain station_code, status, and delay_minutes.",
    fixed = TRUE
  )

  data <- data.frame(
    station_code = "BKY",
    status = "Departed",
    delay_minutes = "ten"
  )

  expect_error(
    summarize_delays(data, "BKY"),
    "station_code and status must be character; delay_minutes must be numeric.",
    fixed = TRUE
  )
})
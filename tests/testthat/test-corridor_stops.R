test_that("invalid train numbers give clear errors", {
  for (bad_input in list(
    732,
    c("732", "738"),
    NA_character_,
    "",
    "abc"
  )) {
    expect_error(
      corridor_stops(bad_input),
      "train_num must be a single string containing digits only.",
      fixed = TRUE
    )
  }
})

test_that("API output has the documented columns and types", {
  skip_on_cran()
  skip_if_offline()

  result <- corridor_stops("738")

  expect_s3_class(result, "data.frame")

  expect_named(
    result,
    c(
      "train_id", "train_num", "station", "station_code",
      "scheduled_arrival", "reported_arrival", "delay_minutes",
      "scheduled_departure", "reported_departure",
      "departure_delay_minutes", "status"
    )
  )

  expect_type(result$train_id, "character")
  expect_type(result$train_num, "character")
  expect_type(result$station, "character")
  expect_type(result$station_code, "character")
  expect_type(result$status, "character")

  expect_s3_class(result$scheduled_arrival, "POSIXct")
  expect_s3_class(result$reported_arrival, "POSIXct")
  expect_s3_class(result$scheduled_departure, "POSIXct")
  expect_s3_class(result$reported_departure, "POSIXct")

  expect_type(result$delay_minutes, "double")
  expect_type(result$departure_delay_minutes, "double")

  # An empty response is valid, but cannot verify populated rows
  if (nrow(result) == 0L) {
    skip("No current records for train 738; populated-row checks skipped.")
  }

  # Rule 1: every row belongs to the requested train number
  expect_true(all(result$train_num == "738"))

  # Rule 2: departure delay equals the difference between the times
  expected_delay <- as.numeric(
    difftime(
      result$reported_departure,
      result$scheduled_departure,
      units = "mins"
    )
  )

  expect_equal(result$departure_delay_minutes, expected_delay)
})
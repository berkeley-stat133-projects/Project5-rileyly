test_that("check_train selects the station and explains departure status", {
  fake_stops <- data.frame(
    train_id = rep("738-15", 3),
    train_num = rep("738", 3),
    station = c("Berkeley", "Emeryville", "Berkeley"),
    station_code = c("BKY", "EMY", "BKY"),
    scheduled_departure = as.POSIXct(
      rep("2026-11-15 16:12:00", 3),
      tz = "America/Los_Angeles"
    ),
    reported_departure = as.POSIXct(
      rep("2026-11-15 16:22:00", 3),
      tz = "America/Los_Angeles"
    ),
    departure_delay_minutes = c(10, 10, 10),
    status = c("Departed", "Departed", "Enroute")
  )

  # Replace API-dependent functions only during this test
  local_mocked_bindings(
    find_station = function(station) "BKY",
    corridor_stops = function(train_num) fake_stops
  )

  result <- check_train("738", "Berkeley")

  expect_equal(nrow(result), 2L)
  expect_true(all(result$station_code == "BKY"))

  expect_identical(
    result$boarding_status,
    c(
      "Already departed.",
      "Expected departure is 10 minutes late."
    )
  )

  expect_named(
    result,
    c(
      "train_id", "train_num", "station", "station_code",
      "scheduled_departure", "reported_departure",
      "departure_delay_minutes", "status", "boarding_status"
    )
  )
})

test_that("no matching station returns an empty table with a message", {
  fake_stops <- data.frame(
    train_id = "738-15",
    train_num = "738",
    station = "Emeryville",
    station_code = "EMY",
    scheduled_departure = as.POSIXct(
      "2026-11-15 16:00:00",
      tz = "America/Los_Angeles"
    ),
    reported_departure = as.POSIXct(
      "2026-11-15 16:10:00",
      tz = "America/Los_Angeles"
    ),
    departure_delay_minutes = 10,
    status = "Enroute"
  )

  local_mocked_bindings(
    find_station = function(station) "BKY",
    corridor_stops = function(train_num) fake_stops
  )

  expect_message(
    result <- check_train("738", "Berkeley"),
    "No matching stop data found.",
    fixed = TRUE
  )

  expect_equal(nrow(result), 0L)
  expect_type(result$boarding_status, "character")
  expect_s3_class(result$reported_departure, "POSIXct")
})

test_that("missing departure time is not described as on schedule", {
  fake_stops <- data.frame(
    train_id = "738-15",
    train_num = "738",
    station = "Berkeley",
    station_code = "BKY",
    scheduled_departure = as.POSIXct(
      "2026-11-15 16:12:00",
      tz = "America/Los_Angeles"
    ),
    reported_departure = as.POSIXct(
      NA_character_,
      tz = "America/Los_Angeles"
    ),
    departure_delay_minutes = NA_real_,
    status = "Enroute"
  )

  local_mocked_bindings(
    find_station = function(station) "BKY",
    corridor_stops = function(train_num) fake_stops
  )

  result <- check_train("738", "Berkeley")

  expect_equal(nrow(result), 1L)
  expect_true(is.na(result$reported_departure))
  expect_true(is.na(result$departure_delay_minutes))

  expect_identical(
    result$boarding_status,
    "Departure time unavailable; punctuality unknown."
  )
})

test_that("invalid train input is rejected before station lookup", {
  local_mocked_bindings(
    find_station = function(station) {
      stop("Station lookup should not run.")
    }
  )

  expect_error(
    check_train(738, "Berkeley"),
    "train_num must be a single string containing digits only.",
    fixed = TRUE
  )
})
test_that("find_trains filters direction and departed trains", {

  fake_stations <- list(
    BKY = list(name = "Berkeley", code = "BKY"),
    SAC = list(name = "Sacramento", code = "SAC")
  )

  # Create a small train record for testing
  make_train <- function(number, codes, boarding_status) {
    list(
      routeName = "Capitol Corridor",
      trainID = paste0(number, "-15"),
      trainNum = number,
      destName = if (tail(codes, 1) == "SAC") {
        "Sacramento"
      } else {
        "Berkeley"
      },
      stations = lapply(codes, function(code) {
        list(
          code = code,
          name = if (code == "BKY") "Berkeley" else "Sacramento",
          schDep = "2026-11-15T16:12:00-08:00",
          dep = "2026-11-15T16:22:00-08:00",
          status = if (code == "BKY") boarding_status else "Enroute"
        )
      })
    )
  }

  fake_trains <- list(
    # Correct direction, not departed
    "738" = list(make_train(
      "738", c("BKY", "SAC"), "Enroute"
    )),

    # Wrong direction: Sacramento comes before Berkeley
    "741" = list(make_train(
      "741", c("SAC", "BKY"), "Enroute"
    )),

    # Correct direction, but already departed
    "734" = list(make_train(
      "734", c("BKY", "SAC"), "Departed"
    ))
  )

  # Replace network calls with fixed responses
  response_number <- 0L

  local_mocked_bindings(
    req_perform = function(req, ...) req,
    resp_body_json = function(resp, ...) {
      response_number <<- response_number + 1L

      if (response_number %% 2L == 1L) {
        fake_stations
      } else {
        fake_trains
      }
    },
    .package = "httr2"
  )

  result <- find_trains(
    "Berkeley", destination = "Sacramento"
  )

  expect_equal(nrow(result), 1L)
  expect_identical(result$train_num, "738")
  expect_identical(result$station_code, "BKY")
  expect_equal(result$departure_delay_minutes, 10)
  expect_identical(
    result$boarding_status,
    "Expected departure is 10 minutes late."
  )
  expect_s3_class(result$scheduled_departure, "POSIXct")

  # Explicitly including departed trains should add train 734
  with_departed <- find_trains(
    "Berkeley",
    destination = "Sacramento",
    include_departed = TRUE
  )

  expect_equal(nrow(with_departed), 2L)
  expect_setequal(with_departed$train_num, c("738", "734"))

  expect_identical(
    with_departed$boarding_status[
      with_departed$train_num == "734"
    ],
    "Already departed."
  )
})
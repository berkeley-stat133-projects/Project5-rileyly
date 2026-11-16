# Small station list used only for testing
example_stations <- list(
  BKY = list(name = "Berkeley", code = "BKY"),
  EMY = list(name = "Emeryville", code = "EMY"),
  OAC = list(name = "Oakland Coliseum", code = "OAC"),
  OKJ = list(name = "Oakland-Jack London Square", code = "OKJ")
)

test_that("exact codes and names return the correct station code", {
  expect_identical(
    find_station("BKY", example_stations),
    "BKY"
  )

  expect_identical(
    find_station("berkeley", example_stations),
    "BKY"
  )

  expect_identical(
    find_station("  bky  ", example_stations),
    "BKY"
  )
})

test_that("a unique spelling correction gives a message and correct code", {
  expect_message(
    result <- find_station("Berkley", example_stations),
    "Matched 'Berkley' to Berkeley",
    fixed = TRUE
  )

  expect_identical(result, "BKY")
})

test_that("ambiguous station names require a choice", {
  expect_error(
    find_station("Oakland", example_stations),
    "Multiple possible stations:",
    fixed = TRUE
  )
})

test_that("unrecognized names are not silently matched", {
  expect_error(
    find_station("zzzzzzzz", example_stations),
    "No close station match found.",
    fixed = TRUE
  )
})

test_that("invalid station inputs give clear errors", {
  for (bad_input in list(
    123,
    c("BKY", "EMY"),
    NA_character_,
    "",
    "   "
  )) {
    expect_error(
      find_station(bad_input, example_stations),
      "station must be a single non-empty string.",
      fixed = TRUE
    )
  }
})
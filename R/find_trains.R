#' Find Capitol Corridor Trains at a Station
#'
#' @param station A boarding station name or code.
#' @param destination An optional destination station name or code.
#'   Must occur after the boarding station on the train's route.
#' @param include_departed Logical. Include already departed trains?
#'   Defaults to FALSE.
#' @returns A data frame with one row per matching train at the
#'   boarding station. Includes train_id, train_num, station,
#'   station_code, destination, scheduled_departure,
#'   reported_departure, departure_delay_minutes, status,
#'   and boarding_status (a message explaining departure information).
#'   Times are Pacific date-times. Reported times may be estimates.
#'   This is a current API snapshot, not a complete timetable.
#' @examples
#' \dontrun{
#' find_trains("Berkeley")
#' find_trains("Berkeley", destination = "Sacramento")
#' }
#' @export
find_trains <- function(station, destination = NULL,
                        include_departed = FALSE) {

  if (!is.logical(include_departed) ||
      length(include_departed) != 1L ||
      is.na(include_departed)) {
    stop("include_departed must be TRUE or FALSE.")
  }

  # Download the station list once and reuse it
  station_response <- httr2::request(
    "https://api.amtraker.com/v3/stations"
  ) |>
    httr2::req_user_agent(
      "STAT133-CapitolCorridor-StudentProject"
    ) |>
    httr2::req_perform()

  stations <- httr2::resp_body_json(station_response)

  origin_code <- find_station(station, station_data = stations)

  destination_code <- NULL

  if (!is.null(destination)) {
    destination_code <- find_station(
      destination, station_data = stations
    )

    if (origin_code == destination_code) {
      stop("Boarding station and destination must be different.")
    }
  }

  # Get the current train records
  response <- httr2::request(
    "https://api.amtraker.com/v3/trains"
  ) |>
    httr2::req_user_agent(
      "STAT133-CapitolCorridor-StudentProject"
    ) |>
    httr2::req_perform()

  trains <- httr2::resp_body_json(response)

  output <- data.frame(
    train_id = character(),
    train_num = character(),
    station = character(),
    station_code = character(),
    destination = character(),
    scheduled_departure = as.POSIXct(
      character(), tz = "America/Los_Angeles"
    ),
    reported_departure = as.POSIXct(
      character(), tz = "America/Los_Angeles"
    ),
    departure_delay_minutes = double(),
    status = character()
  )

  # Convert missing JSON values to character NA
  get_text <- function(x) {
    if (is.null(x) || length(x) == 0L) {
      return(NA_character_)
    }
    as.character(x[[1]])
  }

  for (group in trains) {
    for (train in group) {

      if (!identical(train$routeName, "Capitol Corridor")) {
        next
      }

      stops <- train$stations

      codes <- vapply(
        stops, function(x) get_text(x$code), character(1)
      )

      origin_index <- match(origin_code, codes)

      if (is.na(origin_index)) {
        next
      }

      # Check that the destination is farther along this route
      if (!is.null(destination_code)) {
        destination_index <- match(destination_code, codes)

        if (is.na(destination_index) ||
            destination_index <= origin_index) {
          next
        }
      }

      stop_info <- stops[[origin_index]]
      status <- get_text(stop_info$status)

      if (!include_departed && identical(status, "Departed")) {
        next
      }

      scheduled <- lubridate::ymd_hms(
        get_text(stop_info$schDep), tz = "UTC", quiet = TRUE
      ) |>
        lubridate::with_tz("America/Los_Angeles")

      reported <- lubridate::ymd_hms(
        get_text(stop_info$dep), tz = "UTC", quiet = TRUE
      ) |>
        lubridate::with_tz("America/Los_Angeles")

      row <- data.frame(
        train_id = get_text(train$trainID),
        train_num = get_text(train$trainNum),
        station = get_text(stop_info$name),
        station_code = origin_code,
        destination = get_text(train$destName),
        scheduled_departure = scheduled,
        reported_departure = reported,
        departure_delay_minutes = as.numeric(
          difftime(reported, scheduled, units = "mins")
        ),
        status = status
      )

      output <- rbind(output, row)
    }
  }

  # Explain the departure information
  output$boarding_status <- rep(
    "Departure not confirmed; check reported time and status.",
    nrow(output)
  )

  for (i in seq_len(nrow(output))) {

    delay <- output$departure_delay_minutes[i]

    if (identical(output$status[i], "Departed")) {
      output$boarding_status[i] <- "Already departed."

    } else if (is.na(delay)) {
      output$boarding_status[i] <-
        "Departure time unavailable; punctuality unknown."

    } else if (identical(output$status[i], "Enroute")) {

      if (delay > 0) {
        output$boarding_status[i] <- paste0(
          "Expected departure is ",
          round(delay, 1),
          " minutes late."
        )

      } else if (delay == 0) {
        output$boarding_status[i] <-
          "Expected departure is on schedule."

      } else {
        output$boarding_status[i] <- paste0(
          "Reported departure is ",
          round(abs(delay), 1),
          " minutes earlier than scheduled; verify before travel."
        )
      }
    }
  }

  # Sort by reported departure, using schedule when unavailable
  sort_time <- output$reported_departure
  missing_time <- is.na(sort_time)
  sort_time[missing_time] <-
    output$scheduled_departure[missing_time]

  output <- output[order(sort_time, na.last = TRUE), , drop = FALSE]
  rownames(output) <- NULL

  if (nrow(output) == 0L) {
    message(
      "No matching trains in the current API data. ",
      "This is not a complete timetable."
    )
  }

  output
}
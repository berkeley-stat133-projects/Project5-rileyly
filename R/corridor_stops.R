#' Get Capitol Corridor Station Stops
#'
#' @param train_num A single character string identifying a train number,
#'   such as "732".
#' @returns A data frame with one row per train and station stop.
#'   Columns include train_id, train_num, station, station_code,
#'   scheduled_arrival and reported_arrival (Pacific date-times),
#'   delay_minutes (reported minus scheduled arrival), and status.
#'   Additional columns are scheduled_departure and reported_departure
#'   (Pacific date-times), and departure_delay_minutes
#'   (reported minus scheduled departure, in minutes).
#'   Reported times may include estimates for upcoming stops.
#'   Returns zero rows when no Capitol Corridor records are available.
#' @examples
#' \dontrun{
#' corridor_stops("732")
#' }
#' @export
corridor_stops <- function(train_num) {

  # Check the input before sending a request
  if (!is.character(train_num) ||
      length(train_num) != 1L ||
      is.na(train_num) ||
      !grepl("^[0-9]+$", train_num)) {
    stop("train_num must be a single string containing digits only.")
  }

  # Define the output columns, including their types
  empty <- data.frame(
    train_id = character(),
    train_num = character(),
    station = character(),
    station_code = character(),
    scheduled_arrival = as.POSIXct(
      character(), tz = "America/Los_Angeles"
    ),
    reported_arrival = as.POSIXct(
      character(), tz = "America/Los_Angeles"
    ),
   delay_minutes = double(),
  scheduled_departure = as.POSIXct(
  character(), tz = "America/Los_Angeles"
),
  reported_departure = as.POSIXct(
  character(), tz = "America/Los_Angeles"
),
  departure_delay_minutes = double(),
  status = character()
  )

  # Request this train number
  url <- paste0(
    "https://api.amtraker.com/v3/trains/", train_num
  )

  response <- httr2::request(url) |>
    httr2::req_user_agent("STAT133-CapitolCorridor-StudentProject") |>
    httr2::req_perform()

  result <- httr2::resp_body_json(
    response, simplifyVector = TRUE
  )

  trains <- result[[train_num]]

  if (is.null(trains) || NROW(trains) == 0L) {
    return(empty)
  }

  output <- empty

  # Process every record for this train number
  for (i in seq_len(nrow(trains))) {

    if (!identical(trains$routeName[i], "Capitol Corridor")) {
      next
    }

    stops <- trains$stations[[i]]

    if (is.null(stops) || NROW(stops) == 0L) {
      next
    }

    scheduled <- lubridate::ymd_hms(
      stops$schArr, tz = "America/Los_Angeles"
    )

    reported <- lubridate::ymd_hms(
      stops$arr, tz = "America/Los_Angeles"
    )

    scheduled_dep <- lubridate::ymd_hms(
  stops$schDep, tz = "America/Los_Angeles"
)

reported_dep <- lubridate::ymd_hms(
  stops$dep, tz = "America/Los_Angeles"
)

    rows <- data.frame(
      train_id = trains$trainID[i],
      train_num = trains$trainNum[i],
      station = stops$name,
      station_code = stops$code,
      scheduled_arrival = scheduled,
      reported_arrival = reported,
      delay_minutes = as.numeric(
  difftime(reported, scheduled, units = "mins")
),
      scheduled_departure = scheduled_dep,
      reported_departure = reported_dep,
      departure_delay_minutes = as.numeric(
      difftime(reported_dep, scheduled_dep, units = "mins")
),
status = stops$status
    )

    output <- rbind(output, rows)
  }

  rownames(output) <- NULL
  output
}
#' Check a Train at Your Boarding Station
#'
#' @param train_num A single train number as a string, such as "732".
#' @param station A single station name or code, such as
#'   "Berkeley" or "BKY". Unique close name matches are accepted
#'   with a message; ambiguous matches produce an error.
#' @returns A data frame with one row per matching train and station.
#'   Includes train_id, train_num, station, station_code,
#'   scheduled_departure, reported_departure, departure_delay_minutes,
#'   status, and a boarding_status message.
#'   Times are Pacific date-times. Reported times may be estimates.
#'   Returns zero rows with a message if no matching records are found.
#' @examples
#' \dontrun{
#' check_train("732", "BKY")
#' }
#' @export
check_train <- function(train_num, station) {

    # Reject invalid train numbers before any API request
  if (!is.character(train_num) ||
      length(train_num) != 1L ||
      is.na(train_num) ||
      !grepl("^[0-9]+$", train_num)) {
    stop("train_num must be a single string containing digits only.")
  }

  # Convert a station name or code to its official code
  station <- find_station(station)

  # Get stop information using our first function
  stops <- corridor_stops(train_num)

  keep <- !is.na(stops$station_code) &
    stops$station_code == station

  result <- stops[
    keep,
    c(
      "train_id", "train_num", "station", "station_code",
      "scheduled_departure", "reported_departure",
      "departure_delay_minutes", "status"
    ),
    drop = FALSE
  ]

  result$boarding_status <- rep(
    "Check reported time; departure is not confirmed.",
    nrow(result)
  )

  if (nrow(result) == 0L) {
    message(
      "No matching stop data found. ",
      "This does not confirm cancellation or on-time service."
    )
    return(result)
  }

  for (i in seq_len(nrow(result))) {

    delay <- result$departure_delay_minutes[i]

    if (identical(result$status[i], "Departed")) {
      result$boarding_status[i] <- "Already departed."

    } else if (is.na(delay)) {
      result$boarding_status[i] <-
        "Departure time unavailable; punctuality unknown."

    } else if (identical(result$status[i], "Enroute")) {

      if (delay > 0) {
        result$boarding_status[i] <- paste0(
          "Expected departure is ",
          round(delay, 1),
          " minutes late."
        )
      } else if (delay == 0) {
        result$boarding_status[i] <-
          "Expected departure is on schedule."
      } else {
        result$boarding_status[i] <-
          "Reported departure is earlier than scheduled; verify before travel."
      }
    }
  }

  rownames(result) <- NULL
  result
}
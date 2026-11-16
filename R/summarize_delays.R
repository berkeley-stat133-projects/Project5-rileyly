#' Summarize Arrival Delays at a Station
#'
#' @param data A data frame returned by corridor_stops(), or multiple
#'   outputs combined by row. Each train-stop should appear only once.
#' @param station A single station code, such as "BKY".
#' @returns A one-row data frame containing station_code,
#'   n_records, mean_delay_minutes, and max_delay_minutes.
#'   Only Departed records with non-missing delays are included.
#'   If no records qualify, n_records is zero and both summaries are NA.
#' @examples
#' example_stops <- data.frame(
#'   station_code = c("BKY", "BKY"),
#'   status = c("Departed", "Departed"),
#'   delay_minutes = c(10, 20)
#' )
#' summarize_delays(example_stops, "BKY")
#' @export
summarize_delays <- function(data, station) {

  # Check the data
  if (!is.data.frame(data)) {
    stop("data must be a data frame.")
  }

  required <- c("station_code", "status", "delay_minutes")

  if (!all(required %in% names(data))) {
    stop("data must contain station_code, status, and delay_minutes.")
  }

  if (!is.character(data$station_code) ||
      !is.character(data$status) ||
      !is.numeric(data$delay_minutes)) {
    stop("station_code and status must be character; delay_minutes must be numeric.")
  }

  # Check the station argument
  if (!is.character(station) ||
      length(station) != 1L ||
      is.na(station) ||
      !nzchar(trimws(station))) {
    stop("station must be a single non-empty string.")
  }

  station <- toupper(trimws(station))

  # Select usable records for this station
  keep <- !is.na(data$station_code) &
    data$station_code == station &
    !is.na(data$status) &
    data$status == "Departed" &
    !is.na(data$delay_minutes)

  delays <- data$delay_minutes[keep]

  # Handle stations with no usable records
  if (length(delays) == 0L) {
    return(data.frame(
      station_code = station,
      n_records = 0L,
      mean_delay_minutes = NA_real_,
      max_delay_minutes = NA_real_
    ))
  }

  data.frame(
    station_code = station,
    n_records = length(delays),
    mean_delay_minutes = mean(delays),
    max_delay_minutes = max(delays)
  )
}

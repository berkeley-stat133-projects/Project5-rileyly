#' Find a Station Code from a Name or Code
#'
#' @param station A single station name or code.
#' @param station_data A station list from the Amtraker stations endpoint.
#'   If NULL, the function requests the list from the API.
#' @returns A single station code as a character string.
#'   Searches all stations supplied by the API, not only Capitol Corridor.
#'   Stops with suggestions when a match is ambiguous.
#' @examples
#' example_stations <- list(
#'   BKY = list(name = "Berkeley", code = "BKY"),
#'   EMY = list(name = "Emeryville", code = "EMY")
#' )
#' find_station("Berkley", station_data = example_stations)
#' @export
find_station <- function(station, station_data = NULL) {

  if (!is.character(station) ||
      length(station) != 1L ||
      is.na(station) ||
      !nzchar(trimws(station))) {
    stop("station must be a single non-empty string.")
  }

  if (is.null(station_data)) {
    response <- httr2::request(
      "https://api.amtraker.com/v3/stations"
    ) |>
      httr2::req_user_agent(
        "STAT133-CapitolCorridor-StudentProject"
      ) |>
      httr2::req_perform()

    station_data <- httr2::resp_body_json(response)
  }

  if (!is.list(station_data) || length(station_data) == 0L) {
    stop("station_data must be a non-empty list of station records.")
  }

  valid <- vapply(station_data, function(x) {
    is.list(x) &&
      is.character(x$name) && length(x$name) == 1L &&
      !is.na(x$name) && nzchar(trimws(x$name)) &&
      is.character(x$code) && length(x$code) == 1L &&
      !is.na(x$code) && nzchar(trimws(x$code))
  }, logical(1))

  if (!all(valid)) {
    stop("Each station record must have a non-empty name and code.")
  }

  codes <- vapply(station_data, function(x) x$code, character(1))
  station_names <- vapply(
    station_data, function(x) x$name, character(1)
  )

  # Ignore case, extra spaces, and punctuation in station names
  normalize <- function(x) {
    x <- tolower(trimws(x))
    x <- gsub("[[:punct:]]", " ", x)
    trimws(gsub("[[:space:]]+", " ", x))
  }

  query <- normalize(station)
  clean_names <- normalize(station_names)

  if (!nzchar(query)) {
    stop("station must contain letters or numbers.")
  }

  # First try an exact station code
  matches <- which(toupper(codes) == toupper(trimws(station)))

  # Then try an exact station name
  if (length(matches) == 0L) {
    matches <- which(clean_names == query)
  }

  if (length(matches) == 1L) {
    return(unname(codes[matches]))
  }

  # Try part of a name, such as Oakland
  if (length(matches) == 0L && nchar(query) >= 4L) {
    matches <- which(grepl(query, clean_names, fixed = TRUE))
  }

  # Finally try small spelling differences in names
  if (length(matches) == 0L && nchar(query) >= 4L) {
    distances <- as.numeric(utils::adist(query, clean_names))
    limit <- if (nchar(query) <= 7L) 1L else 2L

    if (min(distances) <= limit) {
      matches <- which(distances == min(distances))
    }
  }

  if (length(matches) == 0L) {
    stop("No close station match found. Try a full name or exact code.")
  }

  labels <- paste0(
    station_names[matches], " (", codes[matches], ")"
  )

  if (length(matches) > 1L) {
    stop(
      "Multiple possible stations: ",
      paste(labels, collapse = ", "),
      ". Please use an exact station code."
    )
  }

  message("Matched '", station, "' to ", labels, ".")
  unname(codes[matches])
}